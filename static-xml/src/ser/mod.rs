// Copyright (C) 2021 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

//! Serialization from Rust types to XML.

use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap},
    io::Write,
};

use xml::writer::XmlEvent;

use crate::ExpandedNameRef;

/// An error while serializing.
///
/// TODO: this currently doesn't do anything fancy but could include the XML
/// element stack as [`crate::de::Error`] does. That would likely require
/// separating out [`ToText`] to use [`crate::BoxedStdError`] instead, as
/// [`crate::de::ParseText`] does.
#[derive(Clone, Debug)]
pub struct Error(pub String);

impl Error {
    pub fn duplicate_element(expected: &ExpandedNameRef) -> Error {
        Error(format!("duplicate element {}", expected))
    }

    pub fn missing_attribute(expected: &ExpandedNameRef) -> Error {
        Error(format!("missing expected attribute {}", expected))
    }

    pub fn missing_element(expected: &ExpandedNameRef) -> Error {
        Error(format!("missing expected element {}", expected))
    }

    pub fn missing_text() -> Error {
        Error(format!("missing expected text"))
    }

    pub fn duplicate_mapping(prefix: &str, old: &str, new: &str) -> Error {
        Error(format!(
            "Prefix {:?} is already mapped to {:?}, so can't map to {:?}",
            prefix, old, new
        ))
    }

    pub fn duplicate_attribute(name: &ExpandedNameRef, old: &str, new: &str) -> Error {
        Error(format!(
            "Attribute {:?} already has value {:?}, so can't set value {:?}",
            name, old, new
        ))
    }
}

impl std::error::Error for Error {}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        self.0.fmt(f)
    }
}

struct WrappedWriter<W: std::io::Write> {
    inner: xml::writer::EventWriter<W>,

    /// When `Some`, all futures writes and the overall operation should fail with this error.
    poison: Option<Error>,
}

/// A type-erased version of [`WrappedWriter`], to avoid monomorphization bloat.
trait ErasedEventWriter {
    /// Writes the element, poisoning the writer on failure.
    fn write(&mut self, event: XmlEvent) -> Result<(), Error>;

    /// Explicitly poison the writer.
    fn poison(&mut self, error: Error);
}

impl<W: Write> ErasedEventWriter for WrappedWriter<W> {
    fn write(&mut self, event: XmlEvent) -> Result<(), Error> {
        if let Some(ref poison) = self.poison {
            return Err(poison.clone());
        }
        if let Err(e) = self.inner.write(event) {
            let wrapped = Error(e.to_string());
            self.poison = Some(wrapped.clone());
            return Err(wrapped);
        }
        Ok(())
    }

    fn poison(&mut self, error: Error) {
        self.poison.get_or_insert(error);
    }
}

/// Builds the start tag for an element: name, namespace mappings, and attributes.
///
/// The element's parent always constructs it with its namespace, local name, and
/// inherited prefix mappings. The type serializing the element may add
/// attributes and name space mappings, then start the element and convert it
/// to an [`ElementWriter`] to add child elements and text.
pub struct ElementBuilder<'a>(Option<ElementBuilderInner<'a>>);

struct ElementBuilderInner<'a> {
    name: ExpandedNameRef<'a>,
    namespaces: NamespacesBuilder<'a>,

    /// Attributes, keyed by the expanded name as that's what must be unique.
    /// The prefix is part of the BTreeMap's value, even though XML considers
    /// it part of the key.
    attributes: Vec<(ExpandedNameRef<'a>, String)>,
    seen_attributes: HashMap<ExpandedNameRef<'a>, ()>,
    writer: &'a mut dyn ErasedEventWriter,
}

impl<'a> ElementBuilder<'a> {
    pub fn namespace(&mut self, requested_prefix: &'a str, url: &'a str) {
        self.0
            .as_mut()
            .unwrap()
            .namespaces
            .insert(requested_prefix, url);
    }

    /// Adds the given attribute.
    ///
    /// `name` must:
    /// *   not specify a namespace if
    pub fn attribute<'b>(
        &'b mut self,
        name: ExpandedNameRef<'a>,
        value: String,
    ) -> Result<(), Error> {
        let inner = self.0.as_mut().unwrap();
        use std::collections::hash_map::Entry;
        match inner.seen_attributes.entry(name) {
            Entry::Vacant(e) => {
                e.insert(());
                inner.attributes.push((name, value));
            }
            Entry::Occupied(_) => {
                // This would be more efficient with indexmap or by seen_attributes values holding
                // attributes indexes, but it's hardly a hot path anyway.
                let old = inner.attributes.iter().find(|(n, _)| *n == name).unwrap();
                return Err(Error::duplicate_attribute(&name, &old.1, &value));
            }
        }
        inner.namespaces.attribute(name.namespace);
        Ok(())
    }

    /// Writes the start event, returning a writer that must be used to finish
    /// the element.
    ///
    /// If `finish` is not called on the returned [`ElementWriter`] before it
    /// is dropped, serialization will fail.
    #[must_use = "must call finish on the returned writer to complete the document"]
    pub fn start(mut self) -> Result<ElementWriter<'a>, Error> {
        let inner = self.0.take().unwrap();
        let namespaces = inner.namespaces.finalize();
        inner
            .writer
            .write(XmlEvent::StartElement {
                name: namespaces.resolve_element(&inner.name),

                // TODO: better to reuse a Vec and amortize this allocation.
                attributes: Cow::Owned(
                    inner
                        .attributes
                        .iter()
                        .map(|(k, v)| xml::attribute::Attribute {
                            name: namespaces.resolve_attribute(k),
                            value: &v,
                        })
                        .collect::<Vec<_>>(),
                ),

                namespace: Cow::Borrowed(&namespaces.by_prefix),
            })
            .map_err(|e| Error(e.to_string()))?;
        Ok(ElementWriter(Some(ElementWriterInner {
            namespaces,
            writer: inner.writer,
        })))
    }
}

impl<'a> Drop for ElementBuilder<'a> {
    fn drop(&mut self) {
        if let Some(inner) = self.0.as_mut() {
            inner
                .writer
                .poison(Error("ElementBuilder dropped before start".to_owned()));
        }
    }
}

/// Builds the body (element and text node children) of an element.
pub struct ElementWriter<'a>(Option<ElementWriterInner<'a>>);

struct ElementWriterInner<'a> {
    namespaces: Namespaces<'a>,
    writer: &'a mut dyn ErasedEventWriter,
}

impl<'a> ElementWriter<'a> {
    /// Returns an [`ElementBuilder`] for a child element.
    ///
    /// If `start` is not called on the returned [`ElementBuilder`] before it
    /// is dropped, serialization will fail.
    #[must_use = "must call start on the returned element to complete the document"]
    pub fn element<'b>(&'b mut self, name: ExpandedNameRef<'b>) -> ElementBuilder<'b>
    where
        'a: 'b,
    {
        let inner = self.0.as_mut().unwrap();
        ElementBuilder(Some(ElementBuilderInner {
            name,
            namespaces: inner.namespaces.child(name.namespace),
            attributes: Vec::default(),
            seen_attributes: HashMap::default(),
            writer: inner.writer,
        }))
    }

    pub fn text(&mut self, text: &str) -> Result<(), Error> {
        self.0
            .as_mut()
            .unwrap()
            .writer
            .write(XmlEvent::Characters(text))
    }

    pub fn finish(mut self) -> Result<(), Error> {
        self.0
            .take()
            .unwrap()
            .writer
            .write(XmlEvent::EndElement { name: None })?;
        Ok(())
    }
}

impl<'a> Drop for ElementWriter<'a> {
    fn drop(&mut self) {
        if let Some(inner) = self.0.take() {
            inner
                .writer
                .poison(Error("ElementWriter dropped before finish".to_owned()));
        }
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
struct NamespacesBuilder<'a>(
    /// A mapping from the URL to the assigned prefix.
    ///
    /// `assigned_prefix` is empty in all of the values.
    BTreeMap<&'a str, Namespace<'a>>,
);

impl<'a> NamespacesBuilder<'a> {
    /// Ensures there's at least one mapping for `url`, preferably called `prefix`.
    ///
    /// An empty `prefix` represents the default namespace. `prefix` must not start
    /// with `xml`.
    fn insert(&mut self, requested_prefix: &'a str, url: &'a str) {
        debug_assert!(
            !requested_prefix.starts_with("xml"),
            "prefix {:?} must not start with xml",
            requested_prefix
        );
        use std::collections::btree_map::Entry;
        match self.0.entry(url) {
            Entry::Vacant(e) => {
                e.insert(Namespace {
                    requested_prefix,
                    required: true,
                    assigned_prefix: None,
                    needs_prefix: false,
                });
            }
            Entry::Occupied(mut e) => {
                let e = e.get_mut();
                if !e.required {
                    e.requested_prefix = requested_prefix;
                }
                e.required = true;
            }
        }
    }

    /// Requests that the existing mapping for `url` be usable by attributes.
    ///
    /// Doesn't panic if `url` already exists; `resolve_attribute` will do that later.
    fn attribute(&mut self, url: &str) {
        if let Some(ns) = self.0.get_mut(url) {
            ns.required = true;
            ns.needs_prefix = true;
        }
    }

    /// Finalizes the chosen prefixes, returning an immutable `Namespaces`.
    fn finalize(mut self) -> Namespaces<'a> {
        let mut by_prefix = xml::namespace::Namespace::empty();

        // Give first dibs to required mappings, and ensure they get some
        // assignment even if their requested prefix is taken (or unsuitable,
        // in the corner case where they requested no prefix but an attribute
        // uses them).
        for (url, ns) in &mut self.0 {
            if !ns.required {
                continue;
            }

            if !(ns.needs_prefix && ns.requested_prefix.is_empty())
                && by_prefix.put(ns.requested_prefix.to_owned(), url.to_owned())
            {
                ns.assigned_prefix = Some(Cow::Borrowed(ns.requested_prefix));
                continue;
            }

            let (prefix, mut i) = match ns.requested_prefix {
                "" => ("ns", 1),
                p => (p, 2),
            };
            while !by_prefix.put(format!("{}{}", prefix, i), url.to_owned()) {
                i += 1;
            }
            ns.assigned_prefix = Some(Cow::Owned(format!("{}{}", prefix, i)));
        }

        // Best-effort assignment for non-required mappings.
        for (url, ns) in &mut self.0 {
            if ns.required {
                continue; // already processed.
            }

            if by_prefix.put(ns.requested_prefix.to_owned(), url.to_owned()) {
                ns.assigned_prefix = Some(Cow::Borrowed(ns.requested_prefix));
            }
        }

        Namespaces {
            by_url: self.0,
            by_prefix,
        }
    }
}

struct Namespaces<'a> {
    /// A mapping from URL to `Namespace` objects with their `requested_prefix` finalized.
    by_url: BTreeMap<&'a str, Namespace<'a>>,

    by_prefix: xml::namespace::Namespace,
}

impl<'a> Namespaces<'a> {
    fn child(&self, namespace: &str) -> NamespacesBuilder {
        let mut child = NamespacesBuilder::default();
        let mut found_namespace = false;
        for (prefix, ns) in &self.by_prefix {
            let required = if !found_namespace && ns == namespace {
                found_namespace = true;
                true
            } else {
                false
            };
            child.0.insert(
                ns,
                Namespace {
                    requested_prefix: prefix,
                    assigned_prefix: None,
                    required,
                    needs_prefix: false,
                },
            );
        }
        child
    }

    fn resolve_element<'b>(&'b self, name: &ExpandedNameRef<'b>) -> xml::name::Name<'b>
    where
        'a: 'b,
    {
        let prefix = match self.by_url.get(name.namespace) {
            Some(Namespace {
                assigned_prefix: Some(p),
                required: true,
                ..
            }) => p,
            None if name.namespace == "http://www.w3.org/XML/1998/namespace" => "xml",
            _ if name.namespace.is_empty() => {
                if self.by_prefix.get("").is_none() {
                    ""
                } else {
                    todo!(
                        "element {} has no namespace, but default namespace is set",
                        name
                    )
                }
            }
            _ => panic!("element {} uses undeclared namespace", name),
        };
        xml::name::Name {
            local_name: name.local_name,
            namespace: None, // unused by xml::writer
            prefix: if prefix.is_empty() {
                None
            } else {
                Some(&*prefix)
            },
        }
    }

    fn resolve_attribute<'b>(&'b self, name: &ExpandedNameRef<'b>) -> xml::name::Name<'b>
    where
        'a: 'b,
    {
        let prefix = if name.namespace.is_empty() {
            ""
        } else {
            match self.by_url.get(name.namespace) {
                Some(Namespace {
                    assigned_prefix: Some(p),
                    ..
                }) if !p.is_empty() => p,
                None if name.namespace == "http://www.w3.org/XML/1998/namespace" => "xml",
                _ => panic!("attribute {} uses undeclared namespace", name),
            }
        };
        xml::name::Name {
            local_name: name.local_name,
            namespace: None, // unused by xml::writer
            prefix: if prefix.is_empty() {
                None
            } else {
                Some(&*prefix)
            },
        }
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
struct Namespace<'a> {
    /// The prefix which was requested; empty for the default namespace.
    ///
    /// Multiple URLs in a [`Namespaces`] may request the same prefix, but
    /// only one will actually get it.
    requested_prefix: &'a str,

    /// The actual prefix assigned by [`Namespaces::finalize`].
    ///
    /// May be `None` iff `!required`.
    assigned_prefix: Option<Cow<'a, str>>,

    /// True iff this is an entry expected by the element or an attribute.
    /// False if it's inherited from a parent and should be dropped if the prefix
    /// is unavailable.
    required: bool,

    /// True if `assigned_prefix` must be non-empty; implies `required`.
    ///
    /// Unprefixed attributes (unlike unprefixed elements) are always
    /// unnamespaced.  Thus, a prefix must always be used for a namespace
    /// attribute.
    needs_prefix: bool,
}

/// Writes an element's contents: namespace mappings, attributes, and
/// element/text node children.
///
/// This matches the XML Schema concept of a "complex type".
///
/// The element name itself is chosen by the parent or in the case of the root,
/// via [`SerializeRoot`] or [`serialize_with_name`].
///
/// `static-xml` always calls `write_attributes` then `write_children` exactly
/// once each (or bails on error).
///
/// This trait is often implemented automatically:
///
/// 1.  via `static-xml-derive`'s `#[static_xml(Serialize)]` impl, or
/// 2.  for [`ToText`] implementations, which also may be derived via
///     `#[static_xml(ToText)]`.
///
/// TODO: test code size of this approach vs giving ownership of the
/// `ElementBuilder` to a single method and expecting ownership of an
/// `ElementWriter` back.
#[allow(unused_variables)]
pub trait Serialize {
    fn write_attributes(&self, element: &mut ElementBuilder) -> Result<(), Error> {
        Ok(())
    }
    fn write_children(&self, element: &mut ElementWriter) -> Result<(), Error> {
        Ok(())
    }
}

/// Writes an element, including its name and contents.
///
/// `static-xml-derive`'s `#[static_xml(Serialize)]` implements this trait
/// as well. The main difference in availability between this trait and
/// [`Serialize`] is that while [`Serialize`] is also implemented for any
/// [`ToText`] type, this trait is not.
pub trait SerializeRoot: Serialize {
    fn root(&self) -> ExpandedNameRef;
}

/// Serializes an element field of a parent element.
///
/// For any `T` that implements [`Serialize`], there are three implementations
/// of this trait:
///
/// 1. `T`, for mandatory singleton fields. In XML Schema terms, this
///    is an element with the default `minOccurs="1" maxOccurs="1"`.
/// 2. `Option<T>`, for optional singleton fields. In XML Schema terms,
///    `minOccurs="0" maxOccurs="1".
/// 3. `Vec<T>`, for repeated fields. In XML Schema terms,
///    `minOccurs="0" maxOccurs="unbounded"`.
pub trait SerializeField {
    fn write<'a>(
        &self,
        parent: &mut ElementWriter<'a>,
        name: ExpandedNameRef<'a>,
    ) -> Result<(), Error>;
}

impl<T: Serialize> SerializeField for T {
    fn write<'a>(
        &self,
        parent: &mut ElementWriter<'a>,
        name: ExpandedNameRef<'a>,
    ) -> Result<(), Error> {
        let mut builder = parent.element(name);
        self.write_attributes(&mut builder)?;
        let mut writer = builder.start()?;
        self.write_children(&mut writer)?;
        writer.finish()
    }
}

impl<T: Serialize> SerializeField for Option<T> {
    fn write<'a>(
        &self,
        parent: &mut ElementWriter<'a>,
        name: ExpandedNameRef<'a>,
    ) -> Result<(), Error> {
        if let Some(t) = self.as_ref() {
            t.write(parent, name)?;
        }
        Ok(())
    }
}

impl<T: Serialize> SerializeField for Vec<T> {
    fn write<'a>(
        &self,
        parent: &mut ElementWriter<'a>,
        name: ExpandedNameRef<'a>,
    ) -> Result<(), Error> {
        for t in self {
            t.write(parent, name)?;
        }
        Ok(())
    }
}

/// Converts to text for use in an attribute or text-only element.
///
/// This matches the XML Schema concept of a "simple type".
///
/// When `T: ToText`, the following traits are also implemented:
/// *   [`Serialize<T>`] allows serializing this type from an element with only
///     text children. In turn this implements [`SerializeField<T>`],
///     [`SerializeField<Option<T>>`], and [`SerializeField<Vec<T>>`].
///     [`SerializeAttr<T>`] and [`SerializeAttr<Option<T>>`] allow serializing
///     mandatory and optional attributes, respectively.
pub trait ToText {
    fn to_text(&self) -> Result<String, Error>;
}

macro_rules! text_from_display {
    ( $t:ident ) => {
        impl ToText for $t {
            fn to_text(&self) -> Result<String, Error> {
                Ok(std::string::ToString::to_string(self))
            }
        }
    };
}

text_from_display!(bool);
text_from_display!(i8);
text_from_display!(u8);
text_from_display!(i16);
text_from_display!(u16);
text_from_display!(i32);
text_from_display!(u32);
text_from_display!(i64);
text_from_display!(u64);
text_from_display!(i128);
text_from_display!(u128);
text_from_display!(f32);
text_from_display!(f64);

impl ToText for &str {
    fn to_text(&self) -> Result<String, Error> {
        Ok((*self).to_owned())
    }
}

impl ToText for String {
    fn to_text(&self) -> Result<String, Error> {
        Ok((*self).clone())
    }
}

impl<T: ToText> Serialize for T {
    fn write_children(&self, writer: &mut ElementWriter) -> Result<(), Error> {
        writer.text(&self.to_text()?)
    }
}

/// Serializes an attribute field of a parent element.
///
/// This is implemented via [`ToText`] as noted there.
pub trait SerializeAttr {
    /// Called zero or one times.
    fn write<'a>(
        &self,
        builder: &mut ElementBuilder<'a>,
        name: ExpandedNameRef<'a>,
    ) -> Result<(), Error>;
}

impl<T: ToText> SerializeAttr for T {
    fn write<'a>(
        &self,
        builder: &mut ElementBuilder<'a>,
        name: ExpandedNameRef<'a>,
    ) -> Result<(), Error> {
        builder.attribute(name, self.to_text()?)?;
        Ok(())
    }
}

impl<T: ToText> SerializeAttr for Option<T> {
    fn write<'a>(
        &self,
        builder: &mut ElementBuilder<'a>,
        name: ExpandedNameRef<'a>,
    ) -> Result<(), Error> {
        if let Some(t) = self {
            builder.attribute(name, t.to_text()?)?;
        }
        Ok(())
    }
}

/// Serializer for element name and content; returned by [`serialize`] or [`serialize_with_name`].
#[derive(Copy, Clone)]
pub struct Serializer<'a> {
    element: &'a dyn Serialize,
    name: ExpandedNameRef<'a>,
    perform_indent: bool,
}

impl<'a> Serializer<'a> {
    /// Sets if the output should be indented; defaults to false.
    #[inline]
    pub fn perform_indent(self, perform_indent: bool) -> Self {
        Self {
            perform_indent,
            ..self
        }
    }

    /// Serializes to any `Write` impl.
    pub fn to<W: Write>(self, writer: W) -> Result<(), Error> {
        let mut writer = WrappedWriter {
            inner: xml::writer::EventWriter::new_with_config(
                writer,
                xml::writer::EmitterConfig {
                    perform_indent: self.perform_indent,
                    ..Default::default()
                },
            ),
            poison: None,
        };
        let mut builder = ElementBuilder(Some(ElementBuilderInner {
            name: self.name,
            namespaces: NamespacesBuilder::default(),
            attributes: Vec::default(),
            seen_attributes: HashMap::default(),
            writer: &mut writer,
        }));
        self.element.write_attributes(&mut builder)?;
        let mut writer = builder.start()?;
        self.element.write_children(&mut writer)?;
        writer.finish()
    }

    /// Serializes to a `String`.
    pub fn to_string(self) -> Result<String, Error> {
        // TODO: refactor xml::writer::EventWriter to take a std::fmt::Write
        // rather than a std::io::Write, to avoid the String::from_utf8 overhead.
        let mut out = Vec::new();
        self.to(&mut out)?;
        Ok(String::from_utf8(out).expect("xml-rs produced invalid UTF-8"))
    }
}

/// Serializes the given element with its default name.
///
/// This requires the supplied value implement [`SerializeRoot`]. If it does
/// not, see [`serialize_with_name`] instead.
#[inline]
pub fn serialize<R: SerializeRoot>(root: &R) -> Serializer {
    Serializer {
        element: root,
        name: root.root(),
        perform_indent: false,
    }
}

/// Serializes the given element with the chosen name.
///
/// Unlike [`serialize`], this doesn't require the supplied value implement
/// [`SerializeRoot`]. It can be used with any [`Serialize`] implementation,
/// including any [`ToText`] implementation.
#[inline]
pub fn serialize_with_name<'a>(
    element: &'a dyn Serialize,
    name: ExpandedNameRef<'a>,
) -> Serializer<'a> {
    Serializer {
        element,
        name,
        perform_indent: false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_matches::assert_matches;

    #[test]
    fn blah() {
        let _ = env_logger::Builder::new().is_test(true).try_init();

        struct Foo;

        impl Serialize for Foo {
            fn write_attributes(&self, element: &mut ElementBuilder) -> Result<(), Error> {
                element.namespace("old", "http://example.com/old");
                element.namespace("new", "http://example.com/new");
                element.attribute(
                    ExpandedNameRef {
                        local_name: "attr",
                        namespace: "http://example.com/old",
                    },
                    "old-value".to_owned(),
                )?;
                element.attribute(
                    ExpandedNameRef {
                        local_name: "attr",
                        namespace: "http://example.com/new",
                    },
                    "new-value".to_owned(),
                )?;
                Ok(())
            }

            fn write_children(&self, element: &mut ElementWriter) -> Result<(), Error> {
                element.text("some text here")?;
                Ok(())
            }
        }
        let out = serialize_with_name(
            &Foo,
            ExpandedNameRef {
                local_name: "foo",
                namespace: "http://example.com/old",
            },
        )
        .to_string()
        .unwrap();
        let reader = xml::reader::EventReader::new(&out.as_bytes()[..]);
        let events: Result<Vec<_>, _> = reader.into_iter().collect();
        let events = events.unwrap();
        use xml::reader::XmlEvent;
        assert_matches!(&events[..], [
            XmlEvent::StartDocument { .. },
            XmlEvent::StartElement { name, attributes, .. },
            XmlEvent::Characters(t),
            XmlEvent::EndElement { .. },
            XmlEvent::EndDocument,
        ] => {
            assert_eq!(name.local_name, "foo");
            assert_eq!(name.namespace.as_ref().unwrap(), "http://example.com/old");
            assert_eq!(attributes[0].name.local_name, "attr");
            assert_eq!(attributes[0].name.namespace.as_ref().unwrap(), "http://example.com/old");
            assert_eq!(attributes[1].name.local_name, "attr");
            assert_eq!(attributes[1].name.namespace.as_ref().unwrap(), "http://example.com/new");
            assert_eq!(t, "some text here");
        });
    }

    /// Tests that dropping an [`ElementBuilder`] without calling `start` will "poison" the writer,
    /// rather than produce a nonsense XML document.
    #[test]
    fn dropped_builder() {
        let _ = env_logger::Builder::new().is_test(true).try_init();

        struct Foo;

        impl Serialize for Foo {
            fn write_children(&self, element: &mut ElementWriter) -> Result<(), Error> {
                let _ = element.element(ExpandedNameRef {
                    local_name: "bar",
                    namespace: "",
                });
                Ok(())
            }
        }

        let e = serialize_with_name(
            &Foo,
            ExpandedNameRef {
                local_name: "foo",
                namespace: "",
            },
        )
        .to_string()
        .unwrap_err();
        assert_eq!(e.0, "ElementBuilder dropped before start");
    }

    #[test]
    fn dropped_writer() {
        let _ = env_logger::Builder::new().is_test(true).try_init();

        struct Foo;

        impl Serialize for Foo {
            fn write_children(&self, element: &mut ElementWriter) -> Result<(), Error> {
                let _ = element
                    .element(ExpandedNameRef {
                        local_name: "bar",
                        namespace: "",
                    })
                    .start();
                Ok(())
            }
        }
        let e = serialize_with_name(
            &Foo,
            ExpandedNameRef {
                local_name: "foo",
                namespace: "",
            },
        )
        .to_string()
        .unwrap_err();
        assert_eq!(e.0, "ElementWriter dropped before finish");
    }

    /// If an attribute's namespace has no requested prefix, a prefix should get used anyway.
    ///
    /// This follows from a weird XML namespacing rule: unprefixed attributes are always
    /// unnamespaced, unlike elements.
    #[test]
    fn unprefixed_attr_namespace() {
        let _ = env_logger::Builder::new().is_test(true).try_init();

        struct Foo;

        impl Serialize for Foo {
            fn write_attributes(&self, element: &mut ElementBuilder) -> Result<(), Error> {
                element.namespace("", "http://example.com/default");
                element.attribute(
                    ExpandedNameRef {
                        local_name: "attr",
                        namespace: "http://example.com/default",
                    },
                    "value".to_owned(),
                )?;
                Ok(())
            }
        }
        let out = serialize_with_name(
            &Foo,
            ExpandedNameRef {
                local_name: "foo",
                namespace: "",
            },
        )
        .to_string()
        .unwrap();
        assert_eq!(
            out,
            r#"<?xml version="1.0" encoding="utf-8"?><foo xmlns:ns1="http://example.com/default" ns1:attr="value" />"#
        );
    }

    /// If a namespace's chosen prefix is unavailable, another should be picked.
    #[test]
    fn namespace_prefix_renamed() {
        let _ = env_logger::Builder::new().is_test(true).try_init();

        struct Foo;

        impl Serialize for Foo {
            fn write_attributes(&self, element: &mut ElementBuilder) -> Result<(), Error> {
                element.namespace("foo", "http://example.com/default");
                element.attribute(
                    ExpandedNameRef {
                        local_name: "attr",
                        namespace: "http://example.com/default",
                    },
                    "value".to_owned(),
                )?;
                Ok(())
            }
            fn write_children(&self, element: &mut ElementWriter) -> Result<(), Error> {
                let mut child = element.element(ExpandedNameRef {
                    local_name: "child",
                    namespace: "http://example.com/default",
                });
                child.namespace("foo", "http://example.com/override");
                child.attribute(
                    ExpandedNameRef {
                        local_name: "attr",
                        namespace: "http://example.com/override",
                    },
                    "value".to_owned(),
                )?;
                child.start()?.finish()?;
                Ok(())
            }
        }
        let out = serialize_with_name(
            &Foo,
            ExpandedNameRef {
                local_name: "root",
                namespace: "",
            },
        )
        .to_string()
        .unwrap();
        assert_eq!(
            out,
            r#"<?xml version="1.0" encoding="utf-8"?><root xmlns:foo="http://example.com/default" foo:attr="value"><foo:child xmlns:foo2="http://example.com/override" foo2:attr="value" /></root>"#
        );
    }
}
