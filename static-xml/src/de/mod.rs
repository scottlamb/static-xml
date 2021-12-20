// Copyright (C) 2021 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

//! Deserialization from XML to Rust types.

mod store;

use std::sync::Arc;
use std::{fmt::Write, mem::MaybeUninit};

use log::trace;
use xml::{
    common::{Position, TextPosition},
    reader::XmlEvent,
};

use crate::value::{Field, NamedField, StructVtable, Value};
use crate::ExpandedNameRef;

pub use self::store::ErasedStore;

/// A single element in the XML stack; see [`Error::stack`].
#[derive(Clone, Debug)]
pub struct StackElement {
    /// The full name of the element, including its namespace and prefix (if any) and local name.
    pub name: xml::name::OwnedName,

    /// The position of this element's `StartElement` event within the underlying document.
    pub pos: TextPosition,
}

/// An error returned by an [`Visitor`] impl or friends to `static-xml`.
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

    // xml-rs might detect this anyway, but static-xml-derive shouldn't rely
    // on that for avoiding memory leaks, and it needs an error to return.
    #[doc(hidden)]
    pub fn duplicate_attribute(attribute: &ExpandedNameRef) -> Self {
        Self::Wrap(Box::new(SimpleError(format!(
            "Duplicate attribute {}",
            attribute
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
///     or to an [`Visitor`] impl when reading the parent.
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
        visitor: &mut dyn Visitor,
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
        // TODO: consider reimplementing via `impl Visitor for String`.
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
/// are not as flexible as the [`Visitor`] interface. For example, they
/// don't support validating the order of child nodes.
///
/// Manual implementations should place onto the program stack a builder
/// which implements [`Visitor`], then pass that visitor to
/// `ElementReader::read_to`.
///
/// Note that while `deserialize` *can* retrieve the name of the element via
/// `element.name()` or `element.stack()`, it typically *should not*. Checking
/// the name is the parent's responsibility for non-root elements. Currently
/// it's not done at all for root elements, although that should change as
/// mentioned in [`read`].
pub trait Deserialize: Sized {
    // TODO: deserialize_into(element, field) instead?
    fn deserialize(element: ElementReader<'_>) -> Result<Self, VisitorError>;
}

/// Deserializes the *content* of one or more elements into a field.
///
/// Typically used only via derive macros.
///
/// For any `T` that implements [`Deserialize`], there are three implementations
/// of this trait:
///
/// 1. `T`, for mandatory singleton fields. In XML Schema terms, this
///    is an element with the default `minOccurs="1" maxOccurs="1"`.
/// 2. `Option<T>`, for optional singleton fields. In XML Schema terms,
///    `minOccurs="0" maxOccurs="1".
/// 3. `Vec<T>`, for repeated fields. In XML Schema terms,
///    `minOccurs="0" maxOccurs="unbounded"`.
#[doc(hidden)]
pub trait ElementField: Field {
    #[inline]
    unsafe fn element(
        field: &mut MaybeUninit<Self>,
        initialized: &mut bool,
        child: ElementReader<'_>,
    ) -> Result<(), VisitorError> {
        let field = ErasedStore::new(
            <Self as Field>::Value::VTABLE,
            &mut *(field.as_mut_ptr() as *mut ()),
            <Self as Field>::KIND,
            initialized,
        );

        // TODO: enforce with trait `de` is `Some`, switch to unwrap_unchecked to reduce code size.
        let de_vtable = <Self as Field>::Value::VTABLE.de.as_ref().unwrap();
        de_vtable.kind.deserialize(field, child)
    }

    #[inline]
    unsafe fn finalize<'a>(
        field: &'a mut MaybeUninit<Self>,
        initialized: &'a mut bool,
        expected: &ExpandedNameRef<'_>,
        default: *const (), //Option<fn() -> Self>,
    ) -> Result<(), VisitorError> {
        let field = ErasedStore::new(
            <Self as Field>::Value::VTABLE,
            &mut *(field.as_mut_ptr() as *mut ()),
            <Self as Field>::KIND,
            initialized,
        );

        // TODO: enforce with trait `de` is `Some`, switch to unwrap_unchecked to reduce code size.
        let de_vtable = <Self as Field>::Value::VTABLE
            .de
            .as_ref()
            .unwrap_unchecked();
        (de_vtable.finalize_field)(field, default, &|| VisitorError::missing_element(expected))
    }
}
impl<F, V> ElementField for F
where
    F: Field<Value = V>,
    V: Deserialize,
{
}
//impl<F: FieldValue + Deserialize> ElementField for Field {
//element_field!(T, Direct);
//element_field!(Option<T>, Option);
//element_field!(Vec<T>, Vec);

// TODO: make safe via ErasedStore checking type compatibility.
#[doc(hidden)]
pub type ParseFn =
    unsafe fn(field: ErasedStore<'_>, text: String) -> Result<(), crate::BoxedStdError>;

// TODO: make safe via ErasedStore checking type compatibility.
#[doc(hidden)]
pub type DeserializeFn =
    unsafe fn(field: ErasedStore<'_>, child: ElementReader<'_>) -> Result<(), VisitorError>;

#[doc(hidden)]
pub type FinalizeFieldFn = unsafe fn(
    field: ErasedStore<'_>,
    default_fn: *const (),
    err_fn: &dyn Fn() -> VisitorError,
) -> Result<(), VisitorError>;

/// Portion of [`crate::ValueVtable`] for deserialization.
#[doc(hidden)]
pub struct Vtable {
    pub kind: ValueKind,
    pub finalize_field: FinalizeFieldFn,
}

// TODO: use tagged pointers to save a word per type?
// (there are several bigger improvements we can make to StructVtable first though.)
#[doc(hidden)]
#[derive(Copy, Clone)]
pub enum ValueKind {
    // This goes through a function for two reasons:
    // *   `memoffset::offset_of!` from a `const` context is unstable.
    // *   to break cycles, as described in
    //     [this comment](https://github.com/scottlamb/static-xml/issues/5#issuecomment-998290751).
    StructVisitor(fn() -> &'static StructVtable),

    /// An implementation with a custom visitor type.
    ///
    /// The `Out` and `Scratch` type parameters have been erased.
    /// They must be as expected, and thin pointers.
    CustomVisitor(&'static dyn RawDeserializeImpl<Out = (), Scratch = ()>),
    CustomDeserialize(&'static DeserializeFn),
    Parse(ParseFn),
}

impl ValueKind {
    pub unsafe fn deserialize(
        &self,
        store: ErasedStore,
        child: ElementReader<'_>,
    ) -> Result<(), VisitorError> {
        if store.is_full() {
            return Err(VisitorError::duplicate_element(&child.expanded_name()));
        }
        match self {
            ValueKind::Parse(parse) => {
                parse(store, child.read_string()?).map_err(VisitorError::Wrap)
            }
            ValueKind::CustomDeserialize(deserialize) => deserialize(store, child),
            ValueKind::StructVisitor(s) => (s().deserialize.unwrap())(store, child),
            ValueKind::CustomVisitor(_) => todo!(),
        }
    }

    pub unsafe fn parse(
        &self,
        field: ErasedStore,
        name: &ExpandedNameRef,
        value: String,
    ) -> Result<(), VisitorError> {
        if field.is_full() {
            return Err(VisitorError::duplicate_attribute(name));
        }
        match self {
            ValueKind::Parse(parse) => parse(field, value).map_err(VisitorError::Wrap),
            _ => unreachable!(),
        }
    }
}

#[doc(hidden)]
#[macro_export]
macro_rules! text_vtables {
    ( $t:ty ) => {
        const _: () = {
            unsafe fn parse(
                store: $crate::de::ErasedStore,
                text: String,
            ) -> Result<(), $crate::BoxedStdError> {
                let val: $t = $crate::de::ParseText::parse(text)?;
                store.into_store::<$t>().push(val);
                Ok(())
            }
            unsafe fn finalize_field(
                store: $crate::de::ErasedStore<'_>,
                default_fn: *const (),
                err_fn: &dyn Fn() -> $crate::de::VisitorError,
            ) -> Result<(), $crate::de::VisitorError> {
                store.into_store::<$t>().finalize(default_fn, err_fn)
            }
            unsafe impl $crate::value::Value for $t {
                const VTABLE: &'static $crate::value::ValueVtable = &$crate::value::ValueVtable {
                    type_name: std::any::type_name::<$t>(),
                    de: Some($crate::de::Vtable {
                        kind: $crate::de::ValueKind::Parse(parse),
                        finalize_field,
                    }),
                };
            }
        };
        // TODO: likewise for attr, text.
    };
}

#[doc(hidden)]
#[macro_export]
macro_rules! custom_deserialize_vtable {
    ( $t:ty, $impl:expr ) => {
        const _: () = {
            unsafe fn deserialize(
                erased_store: $crate::de::ErasedStore,
                child: $crate::de::ElementReader<'_>,
            ) -> Result<(), $crate::de::VisitorError> {
                let val: $t = $crate::de::Deserialize::deserialize(child)?;
                erased_store.into_store().push(val);
                Ok(())
            }
            unsafe fn finalize_field(
                erased_store: $crate::de::ErasedStore<'_>,
                default_fn: *const (),
                err_fn: &dyn Fn() -> $crate::de::VisitorError,
            ) -> Result<(), $crate::de::VisitorError> {
                erased_store.into_store::<$t>().finalize(default_fn, err_fn)
            }
            unsafe impl $crate::value::Value for $t {
                const VTABLE: &'static $crate::value::ValueVtable = &$crate::value::ValueVtable {
                    type_name: std::any::type_name::<$t>(),
                    de: Some($crate::de::Vtable {
                        kind: $crate::de::ValueKind::CustomVisitor(unsafe {
                            ::std::mem::transmute::<
                                &'static dyn $crate::de::RawDeserializeImpl<
                                    Scratch = <$t as $crate::de::RawDeserialize>::Scratch,
                                    Out = $t,
                                >,
                                &'static dyn $crate::de::RawDeserializeImpl<Scratch = (), Out = ()>,
                            >($impl)
                        }),
                        finalize_field,
                    }),
                };
            }
        };
    };
}

/// Deserializes an attribute into a field.
///
/// This is implemented via [`ParseText`] as noted there.
#[doc(hidden)]
pub trait AttrField: Field {}
impl<T: Value + ParseText> AttrField for T {}
impl<T: Value + ParseText> AttrField for Option<T> {}

/// Raw deserialization into outer or flattened elements.
///
/// The call site must be `unsafe`, signifying that for soundness
/// the **caller** must guarantee the following:
///
/// 1.  Set aside a `MaybeUninit<Self>`.
/// 2.  Construct a `Self::Buffer` via `Default::default`.
/// 3.  Visit zero or more times via a `visitor` created with a matching
///     `out` and `scratch` which have been untouched except via this
///     method.
/// 4.  Call `finalize` on the same matching `out` and `scratch` which
///     have been untouched except by `visitor`.
///
/// The trait implementation must be `unsafe`, signifying that for
/// soundness the **callee** must guarantee that (assuming the caller
/// follows its contract), `out` is fully initialized when `finalize`
/// returns `Ok`. The caller may then call `out.assume_init()`.
pub unsafe trait RawDeserialize: Deserialize + Value {
    type Scratch: Sized;
}

// TODO: can I make this an actual trait with a singleton? and maybe specific the proper type?
/*pub struct RawDeserializeCustomVtable {
    // scratch layout (size/align)?

    pub init_scratch: unsafe fn(scratch: *mut u8),
    // TODO: drop_scratch_in_place.

    pub attribute: unsafe fn(
        out: *mut u8,
        scratch: *mut u8,
        name: &ExpandedNameRef<'_>,
        value: String
    ) -> Result<Option<String>, VisitorError>,

    pub element: for<'a> unsafe fn(
        out: *mut u8,
        scratch: *mut u8,
        child: ElementReader<'a>,
    ) -> Result<Option<ElementReader<'a>>, VisitorError>,

    pub characters: unsafe fn(
        out: *mut u8,
        scratch: *mut u8,
        s: String,
        pos: TextPosition,
    ) -> Result<Option<String>, crate::BoxedStdError>,

    pub finalize: unsafe fn(
        out: *mut u8,
        scratch: *mut u8,
    ) -> Result<(), VisitorError>,
}*/

#[allow(unused_variables)]
pub unsafe trait RawDeserializeImpl {
    type Out;
    type Scratch;

    fn init_scratch(&self, scratch: &mut MaybeUninit<Self::Scratch>);

    unsafe fn attribute(
        &self,
        out: &mut MaybeUninit<Self::Out>,
        scratch: &mut Self::Scratch,
        name: &ExpandedNameRef<'_>,
        value: String,
    ) -> Result<Option<String>, VisitorError> {
        Ok(Some(value))
    }

    unsafe fn element<'a>(
        &self,
        out: &mut MaybeUninit<Self::Out>,
        scratch: &mut Self::Scratch,
        child: ElementReader<'a>,
    ) -> Result<Option<ElementReader<'a>>, VisitorError> {
        Ok(Some(child))
    }

    unsafe fn characters(
        &self,
        out: &mut MaybeUninit<Self::Out>,
        scratch: &mut Self::Scratch,
        s: String,
        pos: TextPosition,
    ) -> Result<Option<String>, crate::BoxedStdError> {
        Ok(Some(s))
    }

    unsafe fn finalize(
        &self,
        out: &mut MaybeUninit<Self::Out>,
        scratch: &mut Self::Scratch,
    ) -> Result<(), VisitorError>;
}

impl ValueKind {
    unsafe fn init_scratch(&self, scratch: *mut u8) {
        let s = match self {
            ValueKind::StructVisitor(s) => s(),
            ValueKind::CustomVisitor(c) => {
                return c.init_scratch(&mut *(scratch as *mut MaybeUninit<()>))
            }
            ValueKind::Parse(_) => todo!(),
            ValueKind::CustomDeserialize(_) => unreachable!(),
        };

        // You'd think there'd be a simple fill on a MaybeUninit slice? But
        // https://doc.rust-lang.org/reference/types/boolean.html
        // guarantees false is 0 so let's do this:
        std::ptr::write_bytes(
            scratch.add(s.initialized_offset) as *mut bool,
            0,
            s.n_fields(),
        );

        if let Some((text_offset, _)) = s.text {
            std::ptr::write(scratch.add(text_offset) as *mut String, String::new());
        }

        /*for f in s.flattened {
            let f_scratch = scratch.add(f.0);
            f.1.init_scratch(f_scratch);
        }*/
    }

    // TODO: drop_scratch_in_place.
}

pub struct RawDeserializeVisitor<'a> {
    out: &'a mut (),
    scratch: &'a mut (),
    vtable: ValueKind,
}

impl<'a> RawDeserializeVisitor<'a> {
    /// Cheap.
    #[inline]
    pub unsafe fn new<T: RawDeserialize>(
        out: &'a mut MaybeUninit<T>,
        scratch: &'a mut MaybeUninit<T::Scratch>,
        vtable: ValueKind,
    ) -> Self {
        Self {
            out: &mut *(out.as_mut_ptr() as *mut ()),
            scratch: &mut *(scratch.as_mut_ptr() as *mut ()),
            vtable,
        }
    }

    pub fn finalize(mut self) -> Result<(), VisitorError> {
        unsafe {
            match self.vtable {
                ValueKind::StructVisitor(s) => self.struct_visitor(s()).finalize(),
                ValueKind::CustomVisitor(c) => {
                    return c.finalize(
                        &mut *(self.out as *mut () as *mut MaybeUninit<()>),
                        self.scratch,
                    );
                }
                ValueKind::Parse(_) => todo!(),
                ValueKind::CustomDeserialize(_) => unreachable!(),
            }
        }
    }

    #[inline]
    fn struct_visitor<'b>(&'b mut self, vtable: &'b StructVtable) -> StructVisitor<'b> {
        StructVisitor {
            out: self.out as *mut () as *mut u8,
            scratch: self.scratch as *mut () as *mut u8,
            vtable,
        }
    }
}

impl<'a> Visitor for RawDeserializeVisitor<'a> {
    fn attribute(
        &mut self,
        name: &ExpandedNameRef<'_>,
        value: String,
    ) -> Result<Option<String>, VisitorError> {
        unsafe {
            match self.vtable {
                ValueKind::StructVisitor(s) => self.struct_visitor(s()).attribute(name, value),
                ValueKind::CustomVisitor(c) => c.attribute(
                    &mut *(self.out as *mut () as *mut MaybeUninit<()>),
                    self.scratch,
                    name,
                    value,
                ),
                ValueKind::Parse(_) => todo!(),
                ValueKind::CustomDeserialize(_) => unreachable!(),
            }
        }
    }

    fn element<'r>(
        &mut self,
        child: ElementReader<'r>,
    ) -> Result<Option<ElementReader<'r>>, VisitorError> {
        unsafe {
            match self.vtable {
                ValueKind::StructVisitor(s) => self.struct_visitor(s()).element(child),
                ValueKind::CustomVisitor(c) => {
                    return c.element(
                        &mut *(self.out as *mut () as *mut MaybeUninit<()>),
                        self.scratch,
                        child,
                    );
                }
                ValueKind::Parse(_) => todo!(),
                ValueKind::CustomDeserialize(_) => unreachable!(),
            }
        }
    }

    fn characters(
        &mut self,
        text: String,
        pos: TextPosition,
    ) -> Result<Option<String>, crate::BoxedStdError> {
        unsafe {
            match self.vtable {
                ValueKind::StructVisitor(s) => self.struct_visitor(s()).characters(text, pos),
                ValueKind::CustomVisitor(c) => {
                    return c.characters(
                        &mut *(self.out as *mut () as *mut MaybeUninit<()>),
                        self.scratch,
                        text,
                        pos,
                    );
                }
                ValueKind::Parse(_) => todo!(),
                ValueKind::CustomDeserialize(_) => unreachable!(),
            }
        }
    }
}

#[derive(Copy, Clone)]
struct StructVisitor<'a> {
    out: *mut u8,
    scratch: *mut u8,
    vtable: &'a StructVtable,
}

impl<'a> StructVisitor<'a> {
    /// Returns the `initialized` array; caller guarantees `init_scratch` has been called.
    unsafe fn initialized(self) -> &'a mut [bool] {
        std::slice::from_raw_parts_mut(
            self.scratch.add(self.vtable.initialized_offset) as *mut bool,
            self.vtable.n_fields(),
        )
    }

    unsafe fn finalize(self) -> Result<(), VisitorError> {
        let initialized = self.initialized();
        let mut i = 0;
        for nf in self.vtable.elements {
            let field = ErasedStore::new(
                nf.field.vtable,
                &mut *(self.out.add(nf.field.offset as usize) as *mut ()),
                nf.field.field_kind,
                &mut initialized[i],
            );
            (nf.field.vtable.de.as_ref().unwrap().finalize_field)(
                field,
                nf.field.default,
                &|| VisitorError::missing_element(&nf.name),
            )?;
            i += 1;
        }
        for nf in self.vtable.attributes {
            let field = ErasedStore::new(
                nf.field.vtable,
                &mut *(self.out.add(nf.field.offset as usize) as *mut ()),
                nf.field.field_kind,
                &mut initialized[i],
            );
            (nf.field.vtable.de.as_ref().unwrap().finalize_field)(
                field,
                nf.field.default,
                &|| VisitorError::missing_attribute(&nf.name),
            )?;
            i += 1;
        }
        // TODO: text. need to handle buf. empty == absent or not?
        /*if let Some((scratch_offset, text)) = &self.vtable.text {
            let field = ErasedStore {
                value_type: text.vtable,
                ptr: self.out.add(text.offset as usize) as *mut (),
                field_kind: text.field_kind,
                initialized: &mut initialized[i],
            };
            (text.vtable.de.unwrap().finalize_field)(
                field,
                text.default,
                &|| panic!("missing text; does this even make sense?"),
            )?;
        }*/
        // TODO: finalize flatten fields!
        //assert_eq!(self.vtable.flattened.len(), 0);
        Ok(())
    }

    unsafe fn attribute(
        self,
        name: &ExpandedNameRef<'_>,
        value: String,
    ) -> Result<Option<String>, VisitorError> {
        let initialized = self.initialized();
        let field_i = match my_find(name, &self.vtable.attributes) {
            Some(i) => i,
            None => return Ok(Some(value)), // TODO: handle flattened fields.
        };
        let vtable_field = &self.vtable.attributes[field_i].field;
        let store = ErasedStore::new(
            vtable_field.vtable,
            &mut *(self.out.add(vtable_field.offset as usize) as *mut ()),
            vtable_field.field_kind,
            &mut initialized[self.vtable.elements.len() + field_i],
        );
        vtable_field
            .vtable
            .de
            .as_ref()
            .unwrap()
            .kind
            .parse(store, name, value)?;
        Ok(None)
    }

    unsafe fn element<'r>(
        self,
        child: ElementReader<'r>,
    ) -> Result<Option<ElementReader<'r>>, VisitorError> {
        let initialized = self.initialized();
        let field_i = match my_find(&child.expanded_name(), &self.vtable.elements) {
            Some(i) => i,
            None => return Ok(Some(child)), // TODO: handle flattened fields.
        };
        let vtable_field = &self.vtable.elements[field_i].field;
        let store = ErasedStore::new(
            vtable_field.vtable,
            &mut *(self.out.add(vtable_field.offset as usize) as *mut ()),
            vtable_field.field_kind,
            &mut initialized[field_i],
        );
        vtable_field
            .vtable
            .de
            .as_ref()
            .unwrap()
            .kind
            .deserialize(store, child)?;
        Ok(None)
    }

    unsafe fn characters(
        self,
        text: String,
        _pos: TextPosition,
    ) -> Result<Option<String>, crate::BoxedStdError> {
        if let Some((scratch_offset, _)) = self.vtable.text {
            let buf = &mut *(self.scratch.add(scratch_offset) as *mut String);
            if buf.is_empty() {
                // optimization: avoid allocating/copying/deallocating
                *buf = text;
            } else {
                buf.push_str(&text);
            }
            return Ok(None);
        }
        // TODO: handle flattened fields.
        Ok(Some(text))
    }
}

/*
impl<'a, T: RawDeserialize<'a>> Deserialize for T {
    fn deserialize(element: ElementReader<'_>) -> Result<Self, VisitorError> {
        let mut out = ::std::mem::MaybeUninit::uninit();
        let mut visitor = Self::Visitor::new(&mut out);
        element.read_to(&mut visitor)?;
        visitor.finalize()?;
        // SAFETY: finalize()'s contract guarantees this `assume_init` is safe.
        Ok(unsafe {
            out.assume_init()
        })
    }
}
*/

/// Implements [`Deserialize`] via [`RawDeserialize`].
///
/// The type must call this macro, rather than there being an
/// `impl<T: RawDeserialize> Deserialize for T`. That blanket impl
/// doesn't work because `impl<T: RawDeserialize> Deserialize for T` and
/// `impl<T: ParseText> Deserialize for T` would
/// [conflict](https://doc.rust-lang.org/error-index.html#E0119).
// TODO: but what if we impl `RawDeserialize` for `ParseText`? maybe then?
#[doc(hidden)]
#[macro_export]
macro_rules! impl_deserialize_via_raw {
    ( $t:ty ) => {
        impl $crate::de::Deserialize for $t {
            #[inline]
            fn deserialize(
                element: $crate::de::ElementReader<'_>,
            ) -> Result<Self, $crate::de::VisitorError> {
                $crate::de::deserialize_via_raw(element)
            }
        }
    };
}

// TODO: it'd be nice to avoid monomorphizing this...idea:
// * redo the Deserialize interface to deserialize_into a MaybeUninit
// * allocate the scratch space in a Box or via `unsized_locals`.
#[doc(hidden)]
pub fn deserialize_via_raw<T: RawDeserialize>(
    element: ElementReader<'_>,
) -> Result<T, VisitorError> {
    let mut out = MaybeUninit::<T>::uninit();
    let mut scratch = MaybeUninit::<T::Scratch>::uninit();
    // TODO: use unwrap_unchecked to reduce code size?
    let de = <T as Value>::VTABLE.de.as_ref().unwrap();
    unsafe {
        de.kind.init_scratch(out.as_mut_ptr() as *mut u8);
        let mut visitor = RawDeserializeVisitor::new(&mut out, &mut scratch, de.kind);
        element.read_to(&mut visitor)?;
        RawDeserializeVisitor::finalize(visitor)?;
        // SAFETY: finalize's contract guarantees assume_init is safe.
        Ok(out.assume_init())
    }
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

/// Visitor used within [`Deserialize`].
pub trait Visitor {
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

/// Helper for [`Deserialize`] impls: finds `name` in `sorted_slice`, returning its index or `None`.
///
/// This is simply `sorted_slice.binary_search(name).ok()`, but using this
/// function consistently from generated code should reduce code size.
#[doc(hidden)]
#[inline(never)]
pub fn find(name: &ExpandedNameRef<'_>, sorted_slice: &[ExpandedNameRef<'_>]) -> Option<usize> {
    sorted_slice.binary_search(name).ok()
}

fn my_find(name: &ExpandedNameRef<'_>, sorted_slice: &[NamedField]) -> Option<usize> {
    sorted_slice.binary_search_by_key(name, |nf| nf.name).ok()
}

/*
#[doc(hidden)]
pub enum SingleContainerResult<'a, T> {
    Inited(T),
    Pass(ElementReader<'a>),
}

/// A type which is initialized after a single child element.
///
/// `impl SingleChildElement for T` automatically implements [`Deserialize`]
/// for `T`, `Option<T>`, and `Vec<T>`. This is particularly handy for flatten
/// on `enum` types.
#[doc(hidden)]
pub trait SingleElementContainer {
    fn element<'a>(child: ElementReader<'a>) -> Result<SingleContainerResult<Self>, VisitorError>;
    fn missing() -> VisitorError;
}

impl<'out, T: SingleElementContainer> RawDeserialize<'out> for T {
    type Visitor = RequiredVisitor<'out, T>;
}

struct RequiredVisitor<'out, T: SingleElementContainer> {
    out: &'out mut MaybeUninit<T>,
    initialized: bool,
}

// SAFETY: RequiredVisitor fulfills the contract of guaranteeing `out` is
// initialized when `finalize` returns `Ok`.
unsafe impl<'out, T: SingleElementContainer> RawDeserializeVisitor<'out> for RequiredVisitor<'out, T> {
    type Out = T;

    fn new(out: &'out mut MaybeUninit<Self::Out>) -> Self {
        Self {
            out,
            initialized: false,
        }
    }

    fn finalize(self, default: Option<fn() -> Self>) -> Result<(), VisitorError> {
        match (self.initialized, default) {
            (false, Some(d)) => {
                self.out.write(d());
                Ok(())
            }
            (false, None) => Err(T::missing()),
            (true, _) => Ok(())
        }
    }
}

impl<'out, T: SingleElementContainer> Visitor for RequiredVisitor<'out, T> {
    fn element<'a>(
        &mut self,
        child: ElementReader<'a>,
    ) -> Result<Option<ElementReader<'a>>, VisitorError> {
        if self.initialized {
            return Err(VisitorError::duplicate_element(child));
        }
        Ok(match SingleElementContainer::element(child)? {
            SingleContainerResult::Inited(c) => {
                self.out.write(c);
                self.initialized = true;
                None
            }
            SingleContainerResult::Pass(c) => Some(c),
        })
    }
}

impl<'out, T: SingleElementContainer> RawDeserialize<'out> for Option<T> {
    type Visitor = OptionalVisitor<'out, T>;
}

struct OptionalVisitor<'out, T: SingleElementContainer>(&'out mut Option<T>);

// SAFETY: OptionVisitor fulfills the contract of guaranteeing `out` is
// initialized when `finalize` returns `Ok`. I expect writing `None` to be cheap
// (in code size and CPU time), so it just does this immediately.
unsafe impl<'out, T: SingleElementContainer> RawDeserializeVisitor<'out> for OptionalVisitor<'out, T> {
    type Out = Option<T>;

    fn new(out: &'out mut MaybeUninit<Self::Out>) -> Self {
        Self(out.write(None))
    }

    fn finalize(self, default: Option<fn() -> Self>) -> Result<(), VisitorError> {
        if let (None, Some(d)) = (self.out, default) {
            self.out = Some(d());
        }
        Ok(())
    }
}

impl<'out, T: SingleElementContainer> Visitor for OptionalVisitor<'out, T> {
    fn element<'a>(
        &mut self,
        child: ElementReader<'a>,
    ) -> Result<Option<ElementReader<'a>>, VisitorError> {
        if self.out.is_some() {
            return Err(VisitorError::duplicate_element(child.name()));
        }
        *self.out = Some(T::deserialize(child)?);
        Ok(None)
    }
}

impl<'out, T: SingleElementContainer> RawDeserialize<'out> for Vec<T> {
    type Visitor = MultiVisitor<'out, T>;
}

struct MultiVisitor<'out, T: SingleElementContainer>(&'out mut Vec<T>);

// SAFETY: OptionVisitor fulfills the contract of guaranteeing `out` is
// initialized when `finalize` returns `Ok`. I expect writing `None` to be cheap
// (in code size and CPU time), so it just does this immediately.
unsafe impl<'out, T: SingleElementContainer> RawDeserializeVisitor<'out> for MultiVisitor<'out, T> {
    type Out = Vec<T>;

    fn new(out: &'out mut MaybeUninit<Self::Out>) -> Self {
        Self(out.write(Vec::new()))
    }

    fn finalize(self, default: Option<fn() -> Self>) -> Result<(), VisitorError> {
        if let (true, Some(d)) = (self.out.is_empty(), default) {
            *self.out = d();
        }
        Ok(())
    }
}
*/

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
text_vtables!(bool);

macro_rules! text_for_num {
    ( $t:ty ) => {
        impl ParseText for $t {
            fn parse(text: String) -> Result<Self, crate::BoxedStdError> {
                let text = text.trim_matches(&['\x09', '\x0A', '\x0D', '\x20'][..]);
                <$t as std::str::FromStr>::from_str(text).map_err(|e| Box::new(e).into())
            }
        }
        text_vtables!($t);
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
text_vtables!(String);

// TODO: obsolete?
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

    impl Visitor for Dummy {}
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

    impl<T: DeserializeAttr> Visitor for AttrWrapperVisitor<T> {
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
