// Copyright (C) 2021 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

//! Serialization and deserialization of XML.
//!
//! This library operates on two classes of types:
//!
//! *   "text" types (corresponding to an XML schema ["simple"
//!     type](https://www.w3.org/TR/xmlschema11-1/#Simple_Type_Definition)) can
//!     represent character data only, as can be contained within an element or
//!     within an attribute value. They are and deserialized with the
//!     [`ser::ToText`] and [`de::ParseText`] traits, respectively.
//! *   "element" types (corresponding to an XML schema ["complex"
//!     type](https://www.w3.org/TR/xmlschema11-1/#Complex_Type_Definition) can
//!     represent the full contents of an element. That is: its attributes,
//!     child elements, and character data. They are serialized and deserialized
//!     with the [`ser::Serialize`] and [`de::Deserialize`] traits,
//!     respectively.
//!
//! All text types are also element types, via these trait impls:
//!
//! *   `impl<T: ToText> Serialize for T { ... }`
//! *   `impl<T: ParseText> Deserialize for T { ... }`.
//!     Currently this `Deserialize` implementation will *ignore* attributes and
//!     child elements. This may change to match XML schema simple types, which
//!     instead produce errors if these exist.
//!
//! The reverse is not true. Supporting attributes and child elements requires
//! implementing `Serialize` and `Deserialize` directly. This would conflict
//! with `ParseText` and `ToText`, so it's not possible to use the same type
//! to represent both complex element values and attribute values.
//!
//! # Implementing traits via the derive macros
//!
//! Often the most convenient wait to implement these traits is via the
//! derive macros in the `static_xml_derive` crate.
//!
//! ## Text types
//!
//! The text type derive macros currently support three modes.
//!
//! ### `mode = "std"`
//!
//! Mode `std` wraps the standard library's traits [`std::string::ToString`] and
//! [`std::str::FromStr`], respectively.
//!
//! It supports normalizing whitespace prior to parsing via the top-level
//! `whitespace` attribute. Supported values are `preserve` (the default),
//! `replace`, or `collapse`. See [`de::normalize`] for details.
//!
//! Example:
//!
//! ```rust
//! use static_xml_derive::{ParseText, ToText};
//! # #[derive(Debug)]
//! # struct Error;
//! # impl std::fmt::Display for Error {
//! #     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
//! #         todo!()
//! #     }
//! # }
//! # impl std::error::Error for Error {}
//!
//! #[derive(ParseText, ToText)]
//! #[static_xml(mode = "std")]
//! struct Foo(/* fields */);
//!
//! impl std::str::FromStr for Foo {
//!     type Err = Error; // must impl std::error::Error + Send + Sync
//!
//!     fn from_str(s: &str) -> Result<Self, Self::Err> {
//!         todo!()
//!     }
//! }
//!
//! // Normally types impl ToString by way of Display.
//! impl std::fmt::Display for Foo {
//!     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
//!         todo!()
//!     }
//! }
//! ```
//!
//! ### `mode = "restriction"`
//!
//! Mode `restriction` on an `enum` offers a *subset* of the functionality
//! offered by the like-named XML schema construct. Currently, it only allows a
//! restriction of strings to a fixed list of possibilities, optionally
//! with an "unknown" variant.
//!
//! It supports one type-level attribute: `whitespace`. It means the same
//! thing as for `mode = "std"`.
//!
//! It supports an enum with zero or more "unit variants" (ones with no fields)
//! and zero or one single-field tuple variants marked with
//! `#[static_xml(unknown)`]. On unit variants, it supports a
//! `static_xml(rename = "...")` to rename the fixed string. When `rename`
//! is unspecified, the fixed string is taken from the Rust variant name,
//! case and all.
//!
//! The following XML schema and Rust snippets are roughly equivalent.
//!
//! <table>
//! <tr>
//! <th>XML schema</th>
//! <th>Rust</th>
//! </tr>
//! <tr>
//! <td>
//!
//! ```xml
//! <xs:simpleType>
//!   <xs:restriction base="xs:string">
//!     <xs:enumeration value="small"/>
//!     <xs:enumeration value="medium"/>
//!     <xs:enumeration value="large"/>
//!   </xs:restriction>
//! </xs:simpleType>
//! ```
//!
//! </td>
//! <td>
//!
//! ```rust
//! # use static_xml_derive::{ParseText, ToText};
//! #[derive(ParseText, ToText)]
//! #[static_xml(mode = "restriction")]
//! enum Size {
//!     #[static_xml(rename = "small")]
//!     Small,
//!     #[static_xml(rename = "medium")]
//!     Medium,
//!     #[static_xml(rename = "large")]
//!     Large,
//! }
//! ```
//!
//! </td>
//! </tr>
//! <tr>
//! <td>
//!
//! ```xml
//! <xs:simpleType>
//!   <xs:union>
//!     <xs:simpleType>
//!       <xs:restriction base="xs:string">
//!         <xs:whiteSpace value="collapse" />
//!         <xs:enumeration value="small"/>
//!         <xs:enumeration value="medium"/>
//!         <xs:enumeration value="large"/>
//!       </xs:restriction>
//!     </xs:simpleType>
//!     <xs:simpleType>
//!       <xs:restriction base="xs:string">
//!         <xs:whiteSpace value="collapse" />
//!       </xs:restriction>
//!     </xs:simpleType>
//!   </xs:union>
//! </xs:simpleType>
//! ```
//!
//! </td>
//! <td>
//!
//! ```rust
//! # use static_xml_derive::{ParseText, ToText};
//! #[derive(ParseText, ToText)]
//! #[static_xml(
//!     mode = "restriction",
//!     whitespace = "collapse",
//! )]
//! enum Size {
//!     #[static_xml(rename = "small")]
//!     Small,
//!     #[static_xml(rename = "medium")]
//!     Medium,
//!     #[static_xml(rename = "large")]
//!     Large,
//!     #[static_xml(unknown)]
//!     Unknown(String),
//! }
//! ```
//!
//! </td>
//! </tr>
//! </table>
//!
//! ### `mode = "union"`
//!
//! Mode `union` on an `enum` tries to parse text using each enum variant in
//! order. It propagates errors only if *all* variants produce an error. When
//! serializing, it simply delegates to the variant in question. Each variant
//! must be a one-field tuple.
//!
//! Example:
//!
//! ```rust
//! # use static_xml_derive::{ParseText, ToText};
//! #[derive(ParseText, ToText)]
//! #[static_xml(mode = "union")]
//! enum MyUnion {
//!   Int(i32),
//!   String(String),
//! }
//! ```
//!
//! ## Element types
//!
//! Currently for elements types, the derive macro's mode is simply determined
//! by whether the Rust type is a `struct` or an `enum`.
//!
//! Both types support the same type-level attributes:
//!
//! *   `rename` renames the element itself, which is otherwise the name of the
//!     Rust type (case and all). Note: this name is *only* used for the root
//!     element, and currently only for serialization, via the
//!     [`ser::SerializeRoot`] trait. Deserialization doesn't check the name at
//!     all (see [#10](https://github.com/scottlamb/static-xml/issues/10).)
//!     Child elements names' are the responsibility of their parents.
//! *   `prefix` chooses one of the prefixes mapped by `namespace` to use for
//!     the element itself. As with `rename`, it's relevant only for the root.
//! *   `namespace` maps a namespace prefix to a URL. This allows this prefix
//!     to be used by the type- and field-level `prefix` attributes to refer
//!     to the given namespace URL. At runtime, parsing is done in terms of the
//!     URL. The generated code doesn't care what prefix name was used in the
//!     source document. Serialization will generally use the given prefix name,
//!     but this is not guaranteed. Below are the *current* cases in which
//!     a different prefix may be used. These may change without a semver break.
//!     *   if the parent has already mapped the given URL with a different
//!         prefix, the parent's will be used.
//!     *   if a flattened field's namespace mappings conflict with its
//!         containing type, the containing type's mappings will be used.
//!     *   if a namespace is mapped both as the default and with a prefix used
//!         by an attribute, the prefixed form will always be used. This
//!         odd-sounding behavior accommodates a quirk of the [XML namespacing
//!         specification](https://www.w3.org/TR/xml-names11/). Unprefixed
//!         elements map to the unprefixed namespace declaration, but unprefixed
//!         attributes are always in the empty namespace.
//! *   `default_namespace` maps the default namespace to a URL, for elements
//!     only. It doesn't affect attributes to match the XML namespace
//!     specification's behavior.
//!
//! ### structs
//!
//! Element structs currently must have named fields, e.g.
//! `struct Type { foo: String, bar: String }` rather than
//! `struct Type(String, String)`.
//!
//! Field-level attributes:
//!
//! *   `attribute` indicates the field represents an XML attribute rather than
//!     the default XML element.
//! *   `default` indicates parsing should use the field's `Default` impl if
//!     the (element or attribute) field is absent in the input. (It does not
//!     yet support passing in a custom fn as `serde` does.)
//! *   `flatten` flattens the field's elements, attributes, and text into
//!     the parent, similar to the like-named `serde` concept. When used with
//!     `#[derive(Deserialize)]`, the field's type must implement
//!     `static_xml::de::DeserializeFlatten` (which happens automatically
//!      if the field was in turn implemented with `#[derive(Deserialize)]`).
//!     During deserialization, unknown fields are offered to each flattened
//!     field in declaration order. Likewise serialization offers each flattened
//!     field a chance to serialize attributes, and then later element/text
//!     nodes.
//! *   `text` indicates the field represents text rather than the default XML
//!     element. There can be at most one text field.
//! *   `prefix` chooses a namespace for the (element or attribute) field, which
//!      must match a type-level `namespace` declaration.
//! *   `rename` chooses a name for the (element or attribute) field, which
//!     otherwise matches the Rust name, case and all.
//!
//! Fields must implement the following traits:
//!
//! | field type | deserialize trait          | serialize trait         | notes                          |
//! |------------|----------------------------|-------------------------|--------------------------------|
//! | element    | [`de::DeserializeField`]   | [`ser::SerializeField`] | `T`, `Option<T>`, and `Vec<T>` |
//! | attribute  | [`de::DeserializeAttr`]    | [`ser::SerializeAttr`]  | `T` and `Option<T>`            |
//! | flatten    | [`de::DeserializeFlatten`] | [`ser::Serialize`]      |                                |
//! | text       | [`de::ParseText`]          | [`ser::ToText`]         |                                |
//!
//! Currently unknown children (attributes, elements, and unexpected text nodes)
//! are ignored. This may become configurable and/or the default may change.
//!
//! Example:
//!
//! ```rust
//! use static_xml_derive::{Deserialize, Serialize};
//!
//! #[derive(Deserialize, Serialize)]
//! #[static_xml(namespace = "ns: http://example.com/", prefix = "ns")]
//! struct MyType {
//!     #[static_xml(prefix = "ns", rename = "Foo")]
//!     element_child: Vec<String>,
//!
//!     #[static_xml(prefix = "ns", rename = "Bar", attribute)]
//!     attribute_child: Option<String>,
//! }
//! ```
//!
//! ### enums
//!
//! Currently an enum represents a complex type that has a single child element
//! matching one of its variants. This is useful with `flatten` to represent a
//! choice as in the example below.
//!
//! We might support this another way in the future; see
//! [#4](https://github.com/scottlamb/static-xml/issues/4).
//!
//! Variant-level attributes:
//!
//! *   `prefix`
//! *   `rename`
//! *   `skip` indicates a variant which is ignored on deserialization
//!     (if an input has this field name, it will be ignored) and produces an
//!     error on serialization.
//!
//! Each variant must have one unnamed field that implements
//! [`de::DeserializeField`] for deserialization and [`ser::SerializeField`] for
//! serialization.
//!
//! Examples:
//!
//! <table>
//!
//! <tr>
//! <th>XML schema</th>
//! <th>Rust</th>
//! </tr>
//!
//! <tr>
//! <td>
//!
//! ```xml
//! <xs:complexType name="Outer">
//!   <xs:sequence>
//!     <xs:element name="FixedField" />
//!     <xs:choice>
//!       <xs:element name="ChoiceA" />
//!       <xs:element name="ChoiceB" />
//!     </xs:choice>
//!   </xs:sequence>
//! </xs:complexType>
//! ```
//!
//! </td>
//! <td>
//!
//! ```rust
//! # use static_xml_derive::{Deserialize, Serialize};
//! #[derive(Deserialize, Serialize)]
//! struct Outer {
//!     #[static_xml(rename = "FixedField")]
//!     fixed_field: String,
//!
//!     #[static_xml(flatten)]
//!     choice: Choice,
//! }
//!
//! #[derive(Deserialize, Serialize)]
//! enum Choice {
//!     ChoiceA(String),
//!     ChoiceB(String),
//! }
//! ```
//!
//! </td>
//! </tr>
//!
//! <tr>
//! <td>
//!
//! ```xml
//! <xs:complexType name="Outer">
//!   <xs:sequence>
//!     <xs:element name="FixedField" />
//!     <xs:choice>
//!       <xs:element
//!         name="ChoiceA"
//!         maxOccurs="unbounded"
//!         />
//!       <xs:element name="ChoiceB" />
//!     </xs:choice>
//!   </xs:sequence>
//! </xs:complexType>
//! ```
//!
//! </td>
//! <td>
//!
//! ```rust
//! # use static_xml_derive::{Deserialize, Serialize};
//! #[derive(Deserialize, Serialize)]
//! struct Outer {
//!     #[static_xml(rename = "FixedField")]
//!     fixed_field: String,
//!
//!     #[static_xml(flatten)]
//!     choice: Choice,
//! }
//!
//! #[derive(Deserialize, Serialize)]
//! enum Choice {
//!     ChoiceA(Vec<String>),
//!     ChoiceB(String),
//! }
//! ```
//!
//! </td>
//! </tr>
//!
//! <tr>
//! <td>
//!
//! ```xml
//! <xs:complexType name="Outer">
//!   <xs:sequence>
//!     <xs:element name="FixedField" />
//!     <xs:choice maxOccurs="unbounded">
//!       <xs:element name="ChoiceA" />
//!       <xs:element name="ChoiceB" />
//!     </xs:choice>
//!   </xs:sequence>
//! </xs:complexType>
//! ```
//!
//! </td>
//! <td>
//! This is not yet possible to represent with the derive macros. The
//! following reasonable-looking code does not compile!
//!
//! ```rust,compile_fail
//! # use static_xml_derive::{Deserialize, Serialize};
//! #[derive(Deserialize, Serialize)]
//! struct Outer {
//!     #[static_xml(rename = "FixedField")]
//!     fixed_field: String,
//!
//!     #[static_xml(flatten)]
//!     choice: Vec<Choice>,
//! }
//!
//! #[derive(Deserialize, Serialize)]
//! enum Choice {
//!     ChoiceA(String),
//!     ChoiceB(String),
//! }
//! ```
//!
//! </td>
//! </tr>
//!
//! </table>
//!

pub mod de;
pub mod ser;

pub use de::read;
pub use ser::{serialize, serialize_with_name};

const XML_NS: &str = "http://www.w3.org/XML/1998/namespace";

pub use xml::common::TextPosition;

/// A reference to an "expanded name": namespace and local name.
///
/// See [Namespaces in XML 1.1 (Second Edition) section 2.1: Basic
/// Concepts](https://www.w3.org/TR/2006/REC-xml-names11-20060816/#concepts).
///
/// The owned version is called [`ExpandedName`].
#[derive(Copy, Clone, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct ExpandedNameRef<'a> {
    pub namespace: &'a str,
    pub local_name: &'a str,
}

impl<'a> ExpandedNameRef<'a> {
    fn from_xml_name(name: &xml::name::Name<'a>) -> Self {
        Self {
            namespace: match name.namespace {
                // Work around xml-rs's erroneous lack of builtin
                // xmlns:xml="http://www.w3.org/XML/1998/namespace" mapping.
                None if name.prefix == Some("xml") => XML_NS,
                None => "",
                Some(ns) => ns,
            },
            local_name: name.local_name,
        }
    }
}

impl<'a> std::fmt::Display for ExpandedNameRef<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.namespace.is_empty() {
            write!(f, "{}", self.local_name)
        } else {
            write!(f, "{{{}}}{}", self.namespace, self.local_name)
        }
    }
}

/// An owned version of an "expanded name": namespace and local name.
///
/// See [Namespaces in XML 1.1 (Second Edition) section 2.1: Basic
/// Concepts](https://www.w3.org/TR/2006/REC-xml-names11-20060816/#concepts).
///
/// The borrowed version is called [`ExpandedNameRef`].
#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
pub struct ExpandedName {
    pub namespace: String,
    pub local_name: String,
}

impl ExpandedName {
    fn as_ref(&self) -> ExpandedNameRef {
        ExpandedNameRef {
            namespace: &self.namespace,
            local_name: &self.local_name,
        }
    }
}

impl std::fmt::Display for ExpandedName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.as_ref().fmt(f)
    }
}

/// Shorthand for `Box<dyn std::error::Error + Send + Sync + 'static>`.
pub type BoxedStdError = Box<dyn std::error::Error + Send + Sync + 'static>;
