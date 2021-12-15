// Copyright (C) 2021 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

//! Deserialization from XML to Rust types.

use std::fmt::Write;
use std::sync::Arc;

use log::trace;
use xml::{
    common::{Position, TextPosition},
    reader::XmlEvent,
};

use crate::ExpandedNameRef;

/// A single element in the XML stack; see [`Error::stack`].
#[derive(Clone, Debug)]
pub struct StackElement {
    /// The full name of the element, including its namespace and prefix (if any) and local name.
    pub name: xml::name::OwnedName,

    /// The position of this element's `StartElement` event within the underlying document.
    pub pos: TextPosition,
}

/// An error returned by an [`ElementVisitor`] impl or friends to `static-xml`.
///
/// This error is essentially an instruction to propagate an existing [`Error`]
/// (which includes context information) or wrap a user-supplied
/// [`crate::BoxedStdError`] with context.
#[derive(Debug)]
pub enum VisitorError {
    Propagate(Error),
    Wrap(crate::BoxedStdError),
}

impl VisitorError {
    #[doc(hidden)]
    pub fn missing_element(expected: &ExpandedNameRef) -> Self {
        Self::Wrap(Box::new(SimpleError(format!(
            "Missing expected element {}",
            expected
        ))))
    }

    #[doc(hidden)]
    pub fn missing_attribute(expected: &ExpandedNameRef) -> Self {
        Self::Wrap(Box::new(SimpleError(format!(
            "Missing expected attribute {}",
            expected
        ))))
    }

    #[doc(hidden)]
    pub fn duplicate_element(element: &ExpandedNameRef) -> Self {
        Self::Wrap(Box::new(SimpleError(format!(
            "Duplicate element {}",
            element
        ))))
    }

    #[doc(hidden)]
    pub fn unexpected_element(unexpected: &ExpandedNameRef, after: &ExpandedNameRef) -> Self {
        Self::Wrap(Box::new(SimpleError(format!(
            "Got unexpected element {} after {}",
            unexpected, after
        ))))
    }

    #[doc(hidden)]
    pub fn cant_be_empty(ty_: &str) -> Self {
        Self::Wrap(Box::new(SimpleError(format!("{} can't be empty", ty_))))
    }

    fn wrap(self, stack: &[StackElement], pos: TextPosition) -> Error {
        match self {
            VisitorError::Propagate(e) => e,
            VisitorError::Wrap(e) => Error::deserializer(stack, pos, e),
        }
    }
}

impl From<Error> for VisitorError {
    fn from(e: Error) -> Self {
        VisitorError::Propagate(e)
    }
}

/// A simple `Error` impl for use by internal deserializers.
#[derive(Debug)]
struct SimpleError(String);

impl std::fmt::Display for SimpleError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl std::error::Error for SimpleError {}

/// Helper for macros.
#[doc(hidden)]
pub fn union_error(union_type: &str, errors: Vec<crate::BoxedStdError>) -> crate::BoxedStdError {
    let mut msg = format!("all {} union variants failed to parse:", union_type);
    for e in errors {
        let _ = write!(&mut msg, "\n* {}", e);
    }
    Box::new(SimpleError(msg)) as crate::BoxedStdError
}

/// Helper for macros.
#[doc(hidden)]
pub fn no_such_variant(enum_: &str, t: &str) -> crate::BoxedStdError {
    Box::new(SimpleError(format!("no such {} variant {:?}", enum_, t))) as crate::BoxedStdError
}

/// Helper for macros.
#[doc(hidden)]
pub fn cant_be_empty(ty_: &str) -> crate::BoxedStdError {
    Box::new(SimpleError(format!("{} can't be empty", ty_))) as crate::BoxedStdError
}

/// An error encountered while deserializing.
///
/// This type's `Display` impl will show the error encountered and the XML
/// element stack, printing the qname and line:column of each element. E.g.:
///
/// ```text
/// invalid digit found in string @ 14:25
///
/// XML element stack:
///    4: <tt:Hour> @ 14:25
///    3: <tt:Time> @ 13:21
///    2: <tt:UTCDateTime> @ 12:17
///    1: <tds:SystemDateAndTime> @ 6:13
///    0: <tds:GetSystemDateAndTimeResponse> @ 3:9
/// ```
///
/// Cloning an `Error` is cheap.
#[derive(Clone, Debug)]
pub struct Error(Arc<ErrorInner>);

impl Error {
    /// Returns the stack of XML elements as of when this error occurred.
    ///
    /// `stack()[0]` is the root; `stack.last()` is the current element.
    pub fn stack(&self) -> &[StackElement] {
        &self.0.stack
    }

    fn xml(stack: &[StackElement], e: xml::reader::Error) -> Self {
        let pos = e.position();
        Error(Arc::new(ErrorInner {
            kind: ErrorKind::Xml(e),
            stack: stack.to_vec(),
            pos,
        }))
    }

    fn msg(stack: &[StackElement], pos: TextPosition, msg: String) -> Self {
        Error(Arc::new(ErrorInner {
            kind: ErrorKind::Msg(msg),
            stack: stack.to_vec(),
            pos,
        }))
    }

    fn deserializer(stack: &[StackElement], pos: TextPosition, e: crate::BoxedStdError) -> Self {
        Error(Arc::new(ErrorInner {
            kind: ErrorKind::Deserializer(e),
            stack: stack.to_vec(),
            pos,
        }))
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let inner = &*self.0;
        write!(f, "{} @ {}", &inner.kind, &inner.pos)?;
        if !inner.stack.is_empty() {
            write!(f, "\n\nXML element stack:\n")?;
            for (i, element) in inner.stack.iter().enumerate().rev() {
                write!(
                    f,
                    "{:4x}: <{}> @ {}\n",
                    i,
                    element.name.borrow().repr_display(),
                    &element.pos
                )?;
            }
        }
        Ok(())
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.0.kind.source()
    }
}

/// Information about an error, which should be enclosed in an `Arc` to make cloning cheap.
#[derive(Debug)]
struct ErrorInner {
    kind: ErrorKind,
    stack: Vec<StackElement>,
    pos: TextPosition,
}

#[derive(Debug)]
enum ErrorKind {
    /// An error produced by `xml-rs`, including I/O errors and syntax errors.
    Xml(xml::reader::Error),

    /// An error returned by a deserializer.
    Deserializer(crate::BoxedStdError),

    Msg(String),
}

impl std::fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::Xml(e) => e.msg().fmt(f),
            ErrorKind::Deserializer(e) => e.fmt(f),
            ErrorKind::Msg(msg) => msg.fmt(f),
        }
    }
}

impl std::error::Error for ErrorKind {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            // xml::reader::Error doesn't implement source so skip over it when
            // there's an underlying error.
            ErrorKind::Xml(e) => match e.kind() {
                xml::reader::ErrorKind::Syntax(_) => Some(e),
                xml::reader::ErrorKind::Io(io) => Some(io),
                xml::reader::ErrorKind::Utf8(utf) => Some(utf),
                xml::reader::ErrorKind::UnexpectedEof => Some(e),
            },
            ErrorKind::Deserializer(e) => Some(e.as_ref()),
            ErrorKind::Msg(_) => None,
        }
    }
}

/// Reads and returns the root element of the given XML document.
///
/// *TODO:* currently this does no validation of the name of the root element.
/// Maybe we need a `DeserializeRoot` trait to match [`crate::ser::SerializeRoot`].
pub fn read<R: std::io::Read, D: Deserialize>(source: R) -> Result<D, Error> {
    let mut reader = Reader::new(source);
    let v = D::deserialize(reader.root()?).map_err(|e| e.wrap(reader.stack(), reader.pos()))?;
    reader.end()?;
    Ok(v)
}

/// Returns the root element of the given XML document enclosed in a string.
///
/// This is simply `read(source.as_bytes())`; it's common enough to a merit a
/// convenience method.
///
/// See [`read`].
#[inline]
pub fn from_str<D: Deserialize>(source: &str) -> Result<D, Error> {
    read(source.as_bytes())
}

/// Reads XML and tracks the current depth.
struct Reader<R: std::io::Read> {
    inner: xml::reader::EventReader<R>,

    // Invariant: `0 <= depth <= stack.len()`.
    // `depth` and `stack` grow simultaneously. `depth` shrinks first as
    // `ErasedReader::next` returns an `EndElement`, then `stack` follows in
    // `ErasedReader::return_to_depth`. This allows errors thrown while
    // processing an `EndElement` to return the full stack.
    depth: usize,
    stack: Vec<StackElement>,
}

impl<R: std::io::Read> Reader<R> {
    pub fn new(source: R) -> Self {
        Self {
            inner: xml::reader::EventReader::new(source),
            stack: Vec::new(),
            depth: 0,
        }
    }

    /// Returns an [`ElementReader`] for the root element.
    ///
    /// Call only once.
    pub fn root(&mut self) -> Result<ElementReader<'_>, Error> {
        match self.inner.next().map_err(|e| Error::xml(&[], e))? {
            XmlEvent::StartDocument { .. } => {}
            o => {
                return Err(Error::msg(
                    &[],
                    self.inner.position(),
                    format!("expected StartDocument, got {:#?}", o),
                ))
            }
        }
        match self.next()? {
            (
                Event::StartElement {
                    namespace,
                    attributes,
                },
                _pos,
            ) => {
                return Ok(ElementReader {
                    reader: self,
                    attributes,
                    namespace,
                    stack_pos: 0,
                });
            }
            (o, pos) => {
                return Err(Error::msg(
                    &[],
                    pos,
                    format!("expected StartElement, got {:#?}", o),
                ))
            }
        }
    }

    /// Ensures the document ends properly after a successful `root`.
    pub fn end(mut self) -> Result<(), Error> {
        // Like other readers, `root` is not obligated to consume the entire element.
        self.return_to_depth(0)?;

        // Find the `EndDocument`.
        loop {
            match self.inner.next().map_err(|e| Error::xml(&[], e))? {
                XmlEvent::EndDocument => return Ok(()),
                XmlEvent::ProcessingInstruction { .. } | XmlEvent::Comment { .. } => {}
                o => {
                    return Err(Error::msg(
                        &[],
                        self.inner.position(),
                        format!("expected EndDocument, got {:#?}", o),
                    ))
                }
            }
        }
    }

    fn pos(&self) -> TextPosition {
        self.inner.position()
    }
}

/// An event returned by [`ErasedReader::next`].
#[derive(Debug)]
enum Event {
    /// The start of an element.
    ///
    /// Use [`ErasedReader::stack`] to get its name. Not supplying it here
    /// saves an allocation without fighting the borrow checker.
    StartElement {
        attributes: Vec<xml::attribute::OwnedAttribute>,
        namespace: xml::namespace::Namespace,
    },

    /// The end of an element.
    EndElement { name: xml::name::OwnedName },

    /// Characters, whether from `XmlEvent::CData`, `XmlEvent::Characters`, or
    /// (possibly?) `XmlEvent::Whitespace`.
    Characters(String),
}

/// Internal type-erased version of [`Reader`] for use by [`ElementReader`].
///
/// The type erasure reduces monomorphization bloat: there can be only one
/// implementation of `ElementReader` even if there are many supported
/// `std::io::Read` stream types.
trait ErasedReader {
    /// Returns the next element, updating the internal depth.
    ///
    /// On `StartElement`, appends the element to the stack. Does **not** do
    /// the reverse on `EndElement`. That is deferred until `return_to_depth`.
    /// This means that if a deserializer (which calls into `ErasedReader`
    /// indirectly, through `ElementReader`) returns a `VisitorError::Wrap`
    /// while examining the end element, the element in question is included
    /// in the produced `Error`'s stack. Notably, missing element/attribute
    /// errors happen between these calls.
    fn next(&mut self) -> Result<(Event, TextPosition), Error>;

    /// Returns to the given stack depth.
    ///
    /// This has two important responsibilities:
    /// 1.  Skips elements until depth reaches the target. If an `ElementReader`
    ///     is dropped without being processed, all of its elements are
    ///     consumed in this manner.
    /// 2.  Trims the last-ended element from the stack, as noted in `next` doc.
    fn return_to_depth(&mut self, depth: usize) -> Result<(), Error>;

    /// Returns the current stack.
    ///
    /// Initially empty, pushed on start element, popped on `return_to_depth`.
    fn stack(&self) -> &[StackElement];
}

impl<R: std::io::Read> ErasedReader for Reader<R> {
    fn next(&mut self) -> Result<(Event, TextPosition), Error> {
        // On entry, the stack should not have any excess items; any previous EndElements
        // should have been followed by return_to_depth.
        debug_assert_eq!(self.stack.len(), self.depth);

        loop {
            match self.inner.next() {
                Ok(XmlEvent::StartElement {
                    name,
                    attributes,
                    namespace,
                }) => {
                    let pos = self.inner.position();

                    trace!("Starting {}, new depth {}", &name, self.depth + 1);
                    self.stack.push(StackElement { name, pos });
                    self.depth += 1;
                    return Ok((
                        Event::StartElement {
                            attributes,
                            namespace,
                        },
                        pos,
                    ));
                }
                Ok(XmlEvent::EndElement { name }) => {
                    trace!("Ending {}, new depth {}", &name, self.depth - 1);
                    let pos = self.inner.position();
                    self.depth -= 1;
                    debug_assert_eq!(&self.stack[self.depth].name, &name);
                    return Ok((Event::EndElement { name }, pos));
                }
                Ok(XmlEvent::Characters(str)) | Ok(XmlEvent::CData(str)) => {
                    let pos = self.inner.position();
                    return Ok((Event::Characters(str), pos));
                }
                Ok(XmlEvent::Comment(_))
                | Ok(XmlEvent::Whitespace(_))
                | Ok(XmlEvent::ProcessingInstruction { .. })
                | Ok(XmlEvent::StartDocument { .. })
                | Ok(XmlEvent::EndDocument) => continue,
                Err(e) => return Err(Error::xml(&self.stack[..], e)),
            }
        }
    }

    fn return_to_depth(&mut self, depth: usize) -> Result<(), Error> {
        debug_assert!(
            self.depth >= depth,
            "cur depth {} < requested {}",
            self.depth,
            depth
        );
        while self.depth > depth {
            self.next()?;
            self.stack.truncate(self.depth);
        }
        self.stack.truncate(self.depth);
        Ok(())
    }

    fn stack(&self) -> &[StackElement] {
        &self.stack
    }
}

/// Reader for a particular element and its children (attributes, elements, and
/// text).
///
/// Typical flow:
///
/// 1.  Construction: `ElementReader` is constructed within the `static-xml`
///     library and is passed to a [`Deserialize`] impl by [`read`] (the root)
///     or to an [`ElementVisitor`] impl when reading the parent.
/// 2.  Identification: [`ElementReader::name`] returns the name of the element
///     (including its namespace).
/// 3.  Optional reading: see [`ElementReader::read_to`] and
///     [`ElementReader::read_string`].
///
/// If an element reader is dropped before `read_to` or `read_string` is
/// called, its parent will skip over all of its events before proceeding,
/// so the deserialization stream never gets out of sync.
pub struct ElementReader<'a> {
    reader: &'a mut dyn ErasedReader,
    namespace: xml::namespace::Namespace,
    attributes: Vec<xml::attribute::OwnedAttribute>,
    stack_pos: usize,
}

impl<'a> ElementReader<'a> {
    /// Returns the name of this element, including its namespace and prefix (if any).
    #[inline]
    pub fn name(&self) -> ::xml::name::Name {
        self.reader.stack()[self.stack_pos].name.borrow()
    }

    /// Returns the namespace mappings.
    ///
    /// This includes the `xmlns:foo="http://example.com/bar"` mappings on this
    /// element itself and on its parents, unless they have been overridden.
    #[inline]
    pub fn namespace(&self) -> &::xml::namespace::Namespace {
        &self.namespace
    }

    /// Returns the name of this element in a form that can be passed to [`find`].
    ///
    /// Notably, this omits the prefix, which is semantically insignificant.
    #[inline]
    pub fn expanded_name(&self) -> ExpandedNameRef {
        ExpandedNameRef::from_xml_name(&self.name())
    }

    /// Returns the depth of this element within the XML document; the root is depth 1.
    #[inline]
    pub fn depth(&self) -> usize {
        self.stack_pos + 1
    }

    /// Reads all attributes and elements to `visitor`.
    ///
    /// Always returns the `VisitorError::Propagate` form. Callers are expected to return `VisitorError`,
    /// and so `read_to` returning this type avoids them having to append
    /// `.map_err(VisitorError::Propagate)` to every call.
    pub fn read_to(
        mut self,
        visitor: &mut dyn ElementVisitor,
    ) -> Result<
        (
            xml::name::OwnedName,
            xml::namespace::Namespace,
            TextPosition,
        ),
        VisitorError,
    > {
        let stack = self.reader.stack();
        let element = &stack[self.stack_pos];
        for attr in self.attributes.drain(..) {
            let attr_xml_name = attr.name.borrow();
            let attr_name = ExpandedNameRef::from_xml_name(&attr_xml_name);
            visitor
                .attribute(&attr_name, attr.value)
                .map_err(|e| VisitorError::wrap(e, stack, element.pos))?;
        }
        loop {
            debug_assert_eq!(self.stack_pos, self.reader.stack().len() - 1);
            match self.reader.next()? {
                (
                    Event::StartElement {
                        attributes,
                        namespace,
                    },
                    pos,
                ) => {
                    let child = ElementReader {
                        reader: self.reader,
                        namespace,
                        attributes,
                        stack_pos: self.stack_pos + 1,
                    };
                    if let Err(e) = visitor.element(child) {
                        let stack = self.reader.stack();
                        return Err(e.wrap(stack, pos))?;
                    }

                    // `visitor` is not obligated to use `child`. If it
                    // simply ignores it without returning error, skip events
                    // until we return to our proper depth.
                    self.reader
                        .return_to_depth(self.stack_pos + 1)
                        .map_err(VisitorError::Propagate)?;
                }
                (Event::EndElement { name }, pos) => return Ok((name, self.namespace, pos)),
                (Event::Characters(str), pos) => {
                    visitor.characters(str, pos).map_err(|e| {
                        let stack = self.reader.stack();
                        Error::deserializer(stack, pos, e)
                    })?;
                }
            }
        }
    }

    /// Reads all attributes and elements to the given string (or empty).
    ///
    /// Returns an error if there are any child elements.
    ///
    /// *TODO:* should this also return an error if there are attributes?
    /// XML Schema simple types do that, so we should probably match it?
    pub fn read_string(self) -> Result<String, VisitorError> {
        // TODO: consider reimplementing via `impl ElementVisitor for String`.
        // Might decrease this crate's code size.
        let mut out = String::new();
        loop {
            debug_assert_eq!(self.stack_pos, self.reader.stack().len() - 1);
            match self.reader.next().map_err(VisitorError::Propagate)? {
                (Event::StartElement { .. }, pos) => {
                    let stack = self.reader.stack();
                    let child = stack.last().unwrap();
                    return Err(VisitorError::Propagate(Error::msg(
                        stack,
                        pos,
                        format!("Unexpected element {} in string", &child.name),
                    )));
                }
                (Event::EndElement { .. }, _pos) => break,
                (Event::Characters(str), _pos) => {
                    if out.is_empty() {
                        out = str;
                    } else {
                        out.push_str(&str);
                    }
                }
            }
        }
        Ok(out)
    }
}

/// Deserializes the *content* of an element into a new value.
///
/// Content is defined as the element's attributes and child element/text nodes.
/// In XML Schema terms, this deserializes a "complex type".
///
/// This is rarely implemented manually but rather via either of the following:
///
/// 1.  via `static-xml-derive`'s `#[derive(Deserialize)]` macro.
/// 2.  automatically when [`ParseText`] is implemented, which supports
///     deserializing simple elements (ones with only text nodes) and
///     attribute values. That trait in turn is often implemented via
///     `static-xml-derive`'s `#[derive(ParseText)]`.
///
/// It's occasionally useful to implement this manually, as the derive macros
/// are not as flexible as the [`ElementVisitor`] interface. For example, they
/// don't support validating the order of child nodes.
///
/// Manual implementations should place onto the program stack a builder
/// which implements [`ElementVisitor`], then pass that visitor to
/// `ElementReader::read_to`.
///
/// Note that while `deserialize` *can* retrieve the name of the element via
/// `element.name()` or `element.stack()`, it typically *should not*. Checking
/// the name is the parent's responsibility for non-root elements. Currently
/// it's not done at all for root elements, although that should change as
/// mentioned in [`read`].
pub trait Deserialize: Sized {
    fn deserialize(element: ElementReader<'_>) -> Result<Self, VisitorError>;
}

/// Deserializes the *content* of one or more elements into a field.
///
/// Typically used only via derive macros.
///
/// For any `T` that implements [`Deserialize`,, there are three implementations
/// of this trait:
///
/// 1. `T`, for mandatory singleton fields. In XML Schema terms, this
///    is an element with the default `minOccurs="1" maxOccurs="1"`.
/// 2. `Option<T>`, for optional singleton fields. In XML Schema terms,
///    `minOccurs="0" maxOccurs="1".
/// 3. `Vec<T>`, for repeated fields. In XML Schema terms,
///    `minOccurs="0" maxOccurs="unbounded"`.
pub trait DeserializeField: Default {
    /// Handles a single occurrence of this element; called zero or more times.
    fn element<'a>(&mut self, present: &mut bool, element: ElementReader<'_>) -> Result<(), VisitorError>;

    fn finalize(
        present: bool,
        expected: &ExpandedNameRef<'_>,
    ) -> Result<(), VisitorError>;
}

/// Deserializes this type when "flattened" into another.
///
/// With `static-xml-derive`, this can be used via `#[static_xml(flatten)]`.
pub trait DeserializeFlatten: Sized {
    type Builder: ElementVisitor;

    fn init() -> Self::Builder;
    fn finalize(builder: Self::Builder) -> Result<Self, VisitorError>;
}

/// Deserializes text data, whether character nodes or attribute values.
///
/// This matches the XML schema concept of "simple type".
///
/// When `T: ParseText`, the following traits are also implemented:
/// *   [`Deserialize<T>`] allows parsing this type as an element with only text children.
///     In turn this implements [`DeserializeField<T>`], [`DeserializeField<Option<T>>`],
///     and [`DeserializeField<Vec<T>>`].
///     [`DeserializeAttr<T>`] and [`DeserializeAttr<Option<T>>`] allow parsing
///     mandatory and optional attributes, respectively.
pub trait ParseText: Sized {
    /// Parses the given text, which has *not* passed through whitespace normalization.
    ///
    /// The caller can use [`normalize`] as desired.
    fn parse(text: String) -> Result<Self, crate::BoxedStdError>;
}

/// Deserializes an attribute into a field.
///
/// This is implemented via [`ParseText`] as noted there.
pub trait DeserializeAttr: Sized {
    /// May be called zero or one time with the relevant attribute.
    fn attr(
        &mut self,
        present: &mut bool,
        name: &ExpandedNameRef,
        value: String,
    ) -> Result<(), VisitorError>;

    fn finalize(
        present: bool,
        expected: &ExpandedNameRef<'_>,
    ) -> Result<(), VisitorError>;
}

/// Visitor used within [`Deserialize`].
pub trait ElementVisitor {
    /// Processes a given attribute of this element's start tag.
    ///
    /// Implementations which can be used as flattened fields may consume
    /// attributes by returning `Ok(None)`, or "pass" them to later
    /// flattened fields by returning `Ok(Some(value))`. If this `YaDeserialize`
    /// is not being used as a flattened field, this distinction is irrelevant.
    #[allow(unused_variables)]
    fn attribute(
        &mut self,
        name: &ExpandedNameRef<'_>,
        value: String,
    ) -> Result<Option<String>, VisitorError> {
        Ok(Some(value))
    }

    /// Processes a child element.
    ///
    /// Implementations which can be used as flattened fields may consume
    /// elements by returning `Ok(None)`, or "pass" them to later
    /// flattened fields by returning `Ok(Some(child))`. If this `YaDeserialize`
    /// is not being used as a flattened field, this distinction is irrelevant.
    fn element<'a>(
        &mut self,
        child: ElementReader<'a>,
    ) -> Result<Option<ElementReader<'a>>, VisitorError> {
        Ok(Some(child))
    }

    /// Processes character data found directly within this element.
    ///
    /// Implementations which can be used as flattened fields may consume
    /// characters by returning `Ok(None)`, or "pass" them to later
    /// flattened fields by returning `Ok(Some(s))`. If this `YaDeserialize`
    /// is not being used as a flattened field, this distinction is irrelevant.
    #[allow(unused_variables)]
    fn characters(
        &mut self,
        s: String,
        pos: TextPosition,
    ) -> Result<Option<String>, crate::BoxedStdError> {
        Ok(Some(s))
    }
}

/// Type of [white space normalization](https://www.w3.org/TR/xmlschema11-1/#sec-wsnormalization).
#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum WhiteSpace {
    Preserve,
    Replace,
    Collapse,
}

/// Performs white space normalization on a string, destructively.
///
/// ```rust
/// # use static_xml::de::{WhiteSpace, normalize};
/// assert_eq!(normalize("\n foo\t bar\n\n".to_owned(), WhiteSpace::Preserve), "\n foo\t bar\n\n");
/// assert_eq!(normalize("\n foo\t bar\n\n".to_owned(), WhiteSpace::Replace), "  foo  bar  ");
/// assert_eq!(normalize("\n foo\t bar\n\n".to_owned(), WhiteSpace::Collapse), "foo bar");
/// ```
pub fn normalize(s: String, whitespace: WhiteSpace) -> String {
    let is_whitespace = |b: u8| matches!(b, 0x09 | 0x0A | 0x0D | 0x20);
    let collapse = match whitespace {
        WhiteSpace::Preserve => return s,
        WhiteSpace::Replace => false,
        WhiteSpace::Collapse => true,
    };
    let mut v = s.into_bytes();
    let mut outp = 0;
    let mut end = v.len();
    let mut copy = if collapse {
        while end > 0 && is_whitespace(v[end - 1]) {
            end -= 1;
        }
        let start = v
            .iter()
            .take(end)
            .take_while(|&&b| is_whitespace(b))
            .count();
        start..start
    } else {
        0..0
    };
    while copy.end < end {
        let p = copy.end;
        copy.end += 1;
        if is_whitespace(v[p]) {
            v[p] = b' ';
            if collapse {
                let mut next_start = copy.end;
                while next_start < end && is_whitespace(v[next_start]) {
                    next_start += 1;
                }
                if next_start > copy.end {
                    v.copy_within(copy.clone(), outp);
                    outp += copy.len();
                    copy = next_start..next_start;
                    continue;
                }
            }
        }
    }
    if copy.start > outp {
        v.copy_within(copy.clone(), outp);
        end = outp + copy.len();
    }
    v.truncate(end);

    // SAFETY:
    // * v came from s and thus was initially valid UTF-8.
    // * the only transformations are removing ASCII bytes and replacing ASCII
    //   bytes with other ASCII bytes, preserving validity.
    unsafe { String::from_utf8_unchecked(v) }
}

/// Delegates a child element to a list of flattened fields.
///
/// This is `#[doc(hidden)]` because it's a helper for the macros only.
#[doc(hidden)]
pub fn delegate_element<'a>(
    flatten_fields: &mut [&mut dyn ElementVisitor],
    mut child: ElementReader<'a>,
) -> Result<Option<ElementReader<'a>>, VisitorError> {
    for f in flatten_fields {
        match f.element(child) {
            Ok(Some(c)) => child = c,
            r => {
                return r;
            }
        }
    }
    Ok(Some(child))
}

/// Delegates an attribute to a list of flattened fields.
///
/// This is `#[doc(hidden)]` because it's a helper for the macros only.
#[doc(hidden)]
pub fn delegate_attribute(
    flatten_fields: &mut [&mut dyn ElementVisitor],
    name: &ExpandedNameRef,
    mut value: String,
) -> Result<Option<String>, VisitorError> {
    for f in flatten_fields {
        match f.attribute(name, value) {
            Ok(Some(v)) => value = v,
            r => return r,
        }
    }
    Ok(Some(value))
}

/// Delegates characters to a list of flattened fields.
///
/// This is `#[doc(hidden)]` because it's a helper for the macros only.
#[doc(hidden)]
pub fn delegate_characters(
    flatten_fields: &mut [&mut dyn ElementVisitor],
    mut s: String,
    pos: TextPosition,
) -> Result<Option<String>, crate::BoxedStdError> {
    for f in flatten_fields {
        match f.characters(s, pos) {
            Ok(Some(ss)) => s = ss,
            r => return r,
        }
    }
    Ok(Some(s))
}

/// Helper for [`Deserialize`] impls: finds `name` in `sorted_slice`, returning its index or `None`.
///
/// This is simply `sorted_slice.binary_search(name).ok()`, but using this
/// function consistently from generated code should reduce code size.
#[doc(hidden)]
#[inline(never)]
pub fn find(name: &ExpandedNameRef<'_>, sorted_slice: &[ExpandedNameRef<'_>]) -> Option<usize> {
    sorted_slice.binary_search(name).ok()
}

impl<T: Deserialize + Default> DeserializeField for T {
    fn element<'a>(&mut self, present: &mut bool, element: ElementReader<'_>) -> Result<(), VisitorError> {
        if *present {
            return Err(VisitorError::duplicate_element(&element.expanded_name()));
        }
        *self = T::deserialize(element)?;
        *present = true;
        Ok(())
    }

    fn finalize(
        present: bool,
        expected: &ExpandedNameRef<'_>,
    ) -> Result<(), VisitorError> {
        if !present {
            return Err(VisitorError::missing_element(expected));
        }
        Ok(())
    }
}

impl<T: Deserialize> DeserializeField for Option<T> {
    fn element<'a>(&mut self, present: &mut bool, element: ElementReader<'_>) -> Result<(), VisitorError> {
        if *present {
            return Err(VisitorError::duplicate_element(&element.expanded_name()));
        }
        *self = Some(T::deserialize(element)?);
        *present = true;
        Ok(())
    }

    fn finalize(
        _present: bool,
        _expected: &ExpandedNameRef<'_>,
    ) -> Result<(), VisitorError> {
        Ok(())
    }
}

/// Deserializes into a `Vec`, adding an element.
impl<T: Deserialize> DeserializeField for Vec<T> {
    fn element<'a>(&mut self, present: &mut bool, element: ElementReader<'_>) -> Result<(), VisitorError> {
        self.push(T::deserialize(element)?);
        *present = true;
        Ok(())
    }

    fn finalize(
        _present: bool,
        _expected: &ExpandedNameRef<'_>,
    ) -> Result<(), VisitorError> {
        Ok(())
    }
}

impl<T: ParseText> DeserializeAttr for T {
    fn attr(
        &mut self,
        present: &mut bool,
        _name: &ExpandedNameRef,
        value: String,
    ) -> Result<(), VisitorError> {
        *self = T::parse(value).map_err(VisitorError::Wrap)?;
        *present = true;
        Ok(())
    }

    fn finalize(
        present: bool,
        expected: &ExpandedNameRef<'_>,
    ) -> Result<(), VisitorError> {
        if !present {
            return Err(VisitorError::missing_attribute(expected));
        }
        Ok(())
    }
}

impl<T: ParseText> DeserializeAttr for Option<T> {
    fn attr(
        &mut self,
        present: &mut bool,
        _name: &ExpandedNameRef,
        value: String,
    ) -> Result<(), VisitorError> {
        *self = Some(T::parse(value).map_err(VisitorError::Wrap)?);
        *present = true;
        Ok(())
    }

    fn finalize(
        _present: bool,
        _expected: &ExpandedNameRef<'_>,
    ) -> Result<(), VisitorError> {
        Ok(())
    }
}

impl ParseText for bool {
    fn parse(text: String) -> Result<Self, crate::BoxedStdError> {
        // [https://www.w3.org/TR/xmlschema11-2/#boolean] "For all ·atomic· datatypes other than
        // string [...] the value of whiteSpace is collapse and cannot be
        // changed by a schema author". For bool, it's easy to just use trim.

        // https://www.w3.org/TR/xmlschema11-2/#boolean: "booleanRep ::= 'true' | 'false' | '1' | '0'
        match text.trim_matches(&['\x09', '\x0A', '\x0D', '\x20'][..]) {
            "true" | "1" => Ok(true),
            "false" | "0" => Ok(false),
            _ => Err(Box::new(SimpleError(format!("invalid bool {:?}", &text)))),
        }
    }
}

macro_rules! text_for_num {
    ( $t:ident ) => {
        impl ParseText for $t {
            fn parse(text: String) -> Result<Self, crate::BoxedStdError> {
                let text = text.trim_matches(&['\x09', '\x0A', '\x0D', '\x20'][..]);
                <$t as std::str::FromStr>::from_str(text).map_err(|e| Box::new(e).into())
            }
        }
    };
}

text_for_num!(i8);
text_for_num!(u8);
text_for_num!(i16);
text_for_num!(u16);
text_for_num!(i32);
text_for_num!(u32);
text_for_num!(i64);
text_for_num!(u64);
text_for_num!(i128);
text_for_num!(u128);
text_for_num!(f32);
text_for_num!(f64);

impl ParseText for String {
    fn parse(text: String) -> Result<Self, crate::BoxedStdError> {
        Ok(text)
    }
}

impl<T: ParseText> Deserialize for T {
    fn deserialize(element: ElementReader<'_>) -> Result<Self, VisitorError> {
        let str = element.read_string()?;
        T::parse(str).map_err(VisitorError::Wrap)
    }
}

/*#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, Default, Eq, PartialEq)]
    struct Dummy;

    impl ElementVisitor for Dummy {}
    impl Deserialize for Dummy {
        fn deserialize(element: ElementReader<'_>) -> Result<Self, VisitorError> {
            let mut dummy = Dummy::default();
            element.read_to(&mut dummy)?;
            Ok(dummy)
        }
    }

    #[derive(Debug, Default, Eq, PartialEq)]
    struct AttrWrapper<T: DeserializeAttr>(T);

    struct AttrWrapperVisitor<T: DeserializeAttr>(T, bool);

    impl<T: DeserializeAttr> ElementVisitor for AttrWrapperVisitor<T> {
        fn attribute(
            &mut self,
            name: &ExpandedNameRef<'_>,
            value: String,
        ) -> Result<Option<String>, VisitorError> {
            self.0.attr(name, value).map(None)
        }
    }
    impl<T: DeserializeAttr> Deserialize for AttrWrapper<T> {
        fn deserialize(element: ElementReader<'_>) -> Result<Self, VisitorError> {
            let mut visitor = AttrWrapperVisitor::<T>(T::init());
            element.read_to(&mut visitor)?;
            Ok(AttrWrapper(T::finalize(
                visitor.0,
                &ExpandedNameRef {
                    namespace: "",
                    local_name: "attr",
                },
                None,
            )?))
        }
    }

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn bad_xml() {
        init();
        read::<_, Dummy>(&b"argh"[..]).unwrap_err();
    }

    #[test]
    fn empty_element() {
        init();
        read::<_, Dummy>(&br#"<?xml version="1.0"?><root />"#[..]).unwrap();
    }

    #[test]
    fn nested_element() {
        init();
        read::<_, Dummy>(&br#"<?xml version="1.0"?><root><a><b><c /></b></a></root>"#[..]).unwrap();
    }

    #[test]
    fn bool() {
        init();

        // Canonical forms as character data within an element.
        assert!(read::<_, bool>(&br#"<?xml version="1.0"?><root>true</root>"#[..]).unwrap());
        assert!(!read::<_, bool>(&br#"<?xml version="1.0"?><root>false</root>"#[..]).unwrap());
        assert!(read::<_, bool>(&br#"<?xml version="1.0"?><root>1</root>"#[..]).unwrap());
        assert!(!read::<_, bool>(&br#"<?xml version="1.0"?><root>0</root>"#[..]).unwrap());

        // Canonical forms within an attribute.
        assert!(
            read::<_, AttrWrapper<bool>>(&br#"<?xml version="1.0"?><root attr="true"/>"#[..])
                .unwrap()
                .0
        );
        assert!(
            !read::<_, AttrWrapper<bool>>(&br#"<?xml version="1.0"?><root attr="false"/>"#[..])
                .unwrap()
                .0
        );
        assert!(
            read::<_, AttrWrapper<bool>>(&br#"<?xml version="1.0"?><root attr="1"/>"#[..])
                .unwrap()
                .0
        );
        assert!(
            !read::<_, AttrWrapper<bool>>(&br#"<?xml version="1.0"?><root attr="0"/>"#[..])
                .unwrap()
                .0
        );

        // Whitespace.
        assert!(
            read::<_, AttrWrapper<bool>>(&br#"<?xml version="1.0"?><root attr=" true "/>"#[..])
                .unwrap()
                .0
        );
        assert!(
            !read::<_, AttrWrapper<bool>>(&br#"<?xml version="1.0"?><root attr=" false "/>"#[..])
                .unwrap()
                .0
        );
        assert!(
            read::<_, AttrWrapper<bool>>(&br#"<?xml version="1.0"?><root attr=" 1 "/>"#[..])
                .unwrap()
                .0
        );
        assert!(
            !read::<_, AttrWrapper<bool>>(&br#"<?xml version="1.0"?><root attr=" 0 "/>"#[..])
                .unwrap()
                .0
        );
    }

    // TODO: test exercising return_to_depth.
}*/
