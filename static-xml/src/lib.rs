// Copyright (C) 2021 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

pub mod de;
pub mod ser;

pub use de::read;
pub use ser::{serialize, serialize_with_name};

const XML_NS: &str = "http://www.w3.org/XML/1998/namespace";

pub use xml::common::TextPosition;

#[doc(hidden)]
pub use memoffset::offset_of;

#[doc(hidden)]
pub use lazycell::AtomicLazyCell;

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

/// Shorthand for `Box<dyn std::error::Error + 'static>`.
pub type BoxedStdError = Box<dyn std::error::Error + 'static>;
