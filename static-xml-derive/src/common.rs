// Copyright (C) 2021 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

use std::{cell::RefCell, collections::BTreeMap};

use proc_macro2::TokenStream;
use quote::{quote, quote_spanned, ToTokens};
use static_xml::{de::WhiteSpace, ExpandedNameRef};
use syn::{spanned::Spanned, DeriveInput, Fields, Lit, LitStr, Meta, MetaNameValue, NestedMeta};

// See serde/serde_derive/src/internals/attr.rs and yaserde_derive/src/common/field.rs

/// Accumulates compiler errors. Similar to `serde_derive`'s `Ctxt`.
pub(crate) struct Errors(RefCell<Option<Vec<syn::Error>>>);

impl Errors {
    pub(crate) fn new() -> Self {
        Errors(RefCell::new(Some(Vec::new())))
    }

    pub(crate) fn push(&self, err: syn::Error) {
        self.0.borrow_mut().as_mut().unwrap().push(err);
    }

    pub(crate) fn take_compile_errors(&self) -> TokenStream {
        let errors = self
            .0
            .borrow_mut()
            .take()
            .unwrap()
            .into_iter()
            .map(syn::Error::into_compile_error);
        quote! {
            #(#errors)*
        }
    }
}

impl Drop for Errors {
    fn drop(&mut self) {
        if self.0.borrow().is_some() {
            panic!("Errors dropped without take_compile_errors call");
        }
    }
}

/// Common description of an `enum` that represents text, used by `ParseText` and `ToText`.
pub(crate) struct TextEnum<'a> {
    pub(crate) input: &'a DeriveInput,
    pub(crate) whitespace: WhiteSpace,
    pub(crate) mode: TextEnumMode<'a>,
}

pub(crate) enum TextEnumMode<'a> {
    /// Similar to `<xsd:restriction>`: one of several fixed strings, and optionally a catch-all
    /// for unknown values.
    Restriction {
        variants: Vec<RestrictionVariant<'a>>,
        unknown_variant: Option<usize>, // index within variants.
    },

    /// Similar to `<xsd:union`>.
    ///
    /// Parsing attempts each variant in order.
    Union(Vec<UnionVariant<'a>>),
}

pub(crate) struct UnionVariant<'a> {
    pub(crate) inner: &'a syn::Variant,

    /// The type of the single unnamed field.
    pub(crate) ty: &'a syn::Type,
}

impl<'a> TextEnum<'a> {
    pub(crate) fn new(errors: &Errors, input: &'a DeriveInput, enum_: &'a syn::DataEnum) -> Self {
        let mut whitespace = WhiteSpace::Preserve;
        let mut union_mode = false;
        for item in get_meta_items(errors, &input.attrs) {
            match &item {
                NestedMeta::Meta(Meta::NameValue(nv @ MetaNameValue { ref path, .. }))
                    if path.is_ident("whitespace") =>
                {
                    set_whitespace(errors, &mut whitespace, nv);
                }
                NestedMeta::Meta(Meta::NameValue(nv @ MetaNameValue { ref path, .. }))
                    if path.is_ident("mode") =>
                {
                    with_lit_str(errors, nv, &mut |l| match l.value().as_str() {
                        "restriction" => union_mode = false,
                        "union" => union_mode = true,
                        _ => {
                            errors.push(syn::Error::new_spanned(l, "expected restriction or union"))
                        }
                    });
                }
                i => errors.push(syn::Error::new_spanned(i, "item not understood")),
            }
        }
        let mode = match union_mode {
            false => Self::parse_restriction_fields(errors, enum_),
            true => Self::parse_union_fields(errors, enum_),
        };
        TextEnum {
            input,
            mode,
            whitespace,
        }
    }

    /// Helper for `new`.
    fn parse_restriction_fields(errors: &Errors, enum_: &'a syn::DataEnum) -> TextEnumMode<'a> {
        let mut unknown_variant = None;
        let variants = enum_
            .variants
            .iter()
            .enumerate()
            .map(|(i, v)| {
                let mut text = None;
                let mut unknown = false;
                for item in get_meta_items(errors, &v.attrs) {
                    match &item {
                        NestedMeta::Meta(Meta::Path(p)) if p.is_ident("unknown") => unknown = true,
                        NestedMeta::Meta(Meta::NameValue(nv @ MetaNameValue { path, .. }))
                            if path.is_ident("rename") =>
                        {
                            with_lit_str(errors, nv, &mut |l| text = Some(l.value()));
                        }
                        i => errors.push(syn::Error::new_spanned(i, "item not understood")),
                    }
                }
                let mode = if unknown {
                    if text.is_some() {
                        errors.push(syn::Error::new_spanned(
                            &v.ident,
                            "unknown and rename are mutually exclusive",
                        ));
                    }
                    if single_unnamed_field(&v.fields).is_none() {
                        errors.push(syn::Error::new_spanned(
                            &v.ident,
                            "unknown variant should have a single field",
                        ));
                    }
                    if unknown_variant.is_some() {
                        errors.push(syn::Error::new_spanned(
                            &v.ident,
                            "duplicate unknown variant",
                        ));
                    } else {
                        unknown_variant = Some(i);
                    }
                    TextVariantMode::Unknown
                } else {
                    if !matches!(v.fields, syn::Fields::Unit) {
                        errors.push(syn::Error::new_spanned(
                            &v.ident,
                            "known variants should have no fields",
                        ));
                    }
                    TextVariantMode::Known {
                        text: text.unwrap_or_else(|| v.ident.to_string()),
                    }
                };
                RestrictionVariant { inner: v, mode }
            })
            .collect();
        TextEnumMode::Restriction {
            variants,
            unknown_variant,
        }
    }

    /// Helper for `new`.
    fn parse_union_fields(errors: &Errors, enum_: &'a syn::DataEnum) -> TextEnumMode<'a> {
        let variants = enum_
            .variants
            .iter()
            .filter_map(|v| {
                for i in get_meta_items(errors, &v.attrs) {
                    errors.push(syn::Error::new_spanned(i, "item not understood"));
                }
                match single_unnamed_field(&v.fields) {
                    None => {
                        errors.push(syn::Error::new_spanned(
                            &v.ident,
                            "union variant should have a single field",
                        ));
                        None
                    }
                    Some(f) => Some(UnionVariant {
                        inner: v,
                        ty: &f.ty,
                    }),
                }
            })
            .collect();
        TextEnumMode::Union(variants)
    }
}

pub(crate) struct RestrictionVariant<'a> {
    pub(crate) inner: &'a syn::Variant,
    pub(crate) mode: TextVariantMode,
}

pub(crate) enum TextVariantMode {
    Known { text: String },
    Unknown,
}

/// Parsed top-level attributes for an element type (whether `struct` or `enum`).
pub(crate) struct ElementAttr<'a> {
    pub(crate) name: Name<'a>,
    pub(crate) namespaces: Namespaces,
    pub(crate) direct: bool,
    // TODO: rename_all.
}

/// Mapping of prefix to namespace.
#[derive(Default)]
pub(crate) struct Namespaces(pub(crate) BTreeMap<String, String>);

pub(crate) struct Name<'a> {
    pub(crate) ident: &'a syn::Ident,
    pub(crate) local_name: String,
    pub(crate) prefix: Option<String>,
    pub(crate) namespace: Option<String>,
}

impl<'a> Name<'a> {
    fn from_ident(ident: &syn::Ident) -> Name {
        Name {
            ident,
            local_name: ident.to_string(),
            prefix: None,
            namespace: None,
        }
    }

    pub(crate) fn expanded(&self) -> ExpandedNameRef {
        ExpandedNameRef {
            namespace: self.namespace.as_deref().unwrap_or(""),
            local_name: self.local_name.as_str(),
        }
    }

    pub(crate) fn quote_expanded(&self) -> TokenStream {
        let namespace = self.namespace.as_deref().unwrap_or("");
        let local_name = self.local_name.as_str();
        quote_spanned! {
            self.ident.span() =>
            ::static_xml::ExpandedNameRef {
                local_name: #local_name,
                namespace: #namespace,
            }
        }
    }
}

impl Namespaces {
    fn add_namespace(&mut self, errors: &Errors, nv: &MetaNameValue) {
        with_lit_str(errors, nv, &mut |l| {
            let value = l.value();
            if let Some((prefix_untrimmed, url_untrimmed)) = value.split_once(": ") {
                if prefix_untrimmed.starts_with("xml") {
                    let prefix = prefix_untrimmed.trim();
                    let url = url_untrimmed.trim();
                    if prefix == "xml" && url == "http://www.w3.org/XML/1998/namespace" {
                        return;
                    }
                    todo!()
                }
                self.add_internal(errors, nv, prefix_untrimmed.trim(), url_untrimmed);
            } else {
                self.add_internal(errors, nv, "", &value);
            }
        });
    }

    fn add_default_namespace(&mut self, errors: &Errors, nv: &MetaNameValue) {
        with_lit_str(errors, nv, &mut |l| {
            self.add_internal(errors, nv, "", l.value().trim());
        });
    }

    fn add_internal(
        &mut self,
        errors: &Errors,
        nv: &MetaNameValue,
        prefix: &str,
        url_untrimmed: &str,
    ) {
        let url = url_untrimmed.trim();
        match self.0.entry(prefix.to_string()) {
            std::collections::btree_map::Entry::Vacant(e) => {
                e.insert(url.to_owned());
            }
            std::collections::btree_map::Entry::Occupied(e) => {
                errors.push(syn::Error::new_spanned(
                    nv,
                    format!(
                        "won't reassign prefix {:?} from {:?} to {:?}",
                        prefix,
                        e.get(),
                        url
                    ),
                ));
            }
        }
    }

    /// Gets the namespace associated with the `prefix` name-value pair `nv`, adding an error
    /// if it's missing.
    fn process_prefix(&self, errors: &Errors, nv: &MetaNameValue, name: &mut Name) {
        with_lit_str(errors, nv, &mut |l| {
            let prefix = l.value();
            let prefix = prefix.trim();
            if let Some(ref old_prefix) = name.prefix {
                errors.push(syn::Error::new_spanned(
                    nv,
                    format!("duplicate prefix; was {:?}, now {:?}", old_prefix, prefix),
                ));
                return;
            }
            name.prefix = Some(prefix.to_owned());
            match self.0.get(prefix) {
                Some(namespace) => name.namespace = Some(namespace.clone()),
                None if prefix == "xml" => {
                    name.namespace = Some("http://www.w3.org/XML/1998/namespace".to_owned())
                }
                None if !prefix.is_empty() => {
                    errors.push(syn::Error::new_spanned(
                        nv,
                        format!("prefix {:?} not found", prefix),
                    ));
                }
                None => {}
            }
        });
    }
}

impl<'a> ElementAttr<'a> {
    fn new(errors: &Errors, input: &'a DeriveInput) -> Self {
        let mut name = Name::from_ident(&input.ident);
        let mut namespaces = Namespaces::default();
        let mut prefix_nv = None;
        let mut direct = false;
        for item in get_meta_items(errors, &input.attrs) {
            match item {
                NestedMeta::Meta(Meta::NameValue(nv)) => {
                    if let Some(id) = nv.path.get_ident() {
                        if id == "rename" {
                            with_lit_str(errors, &nv, &mut |l| name.local_name = l.value());
                        } else if id == "prefix" {
                            // Defer evaluation until we have all the namespaces.
                            prefix_nv = Some(nv);
                        } else if id == "namespace" {
                            namespaces.add_namespace(errors, &nv);
                        } else if id == "default_namespace" {
                            namespaces.add_default_namespace(errors, &nv);
                        } else {
                            errors.push(syn::Error::new_spanned(nv, "item not understood"));
                        }
                    } else {
                        errors.push(syn::Error::new_spanned(nv, "item not understood"));
                    }
                }
                NestedMeta::Meta(Meta::Path(p)) => {
                    if let Some(id) = p.get_ident() {
                        if id == "direct" {
                            direct = true;
                        } else {
                            errors.push(syn::Error::new_spanned(p, "item not understood"));
                        }
                    } else {
                        errors.push(syn::Error::new_spanned(p, "item not understood"));
                    }
                }
                i => errors.push(syn::Error::new_spanned(i, "item not understood")),
            }
        }
        if let Some(ref nv) = prefix_nv {
            namespaces.process_prefix(errors, nv, &mut name);
        }
        ElementAttr {
            name,
            namespaces,
            direct,
        }
    }
}

/// Common description of a `struct` that represents an element, used by `Deserialize` and
/// `Serialize`.
pub(crate) struct ElementStruct<'a> {
    pub(crate) input: &'a DeriveInput,
    pub(crate) attr: ElementAttr<'a>,
    pub(crate) fields: Vec<ElementField<'a>>,
    pub(crate) sorted_attributes: Vec<usize>, // indices within fields.
    pub(crate) sorted_elements: Vec<usize>,   // indices within fields.
    pub(crate) text_field_pos: Option<usize>, // index within fields.
}

impl<'a> ElementStruct<'a> {
    pub(crate) fn new(
        errors: &Errors,
        input: &'a DeriveInput,
        struct_: &'a syn::DataStruct,
    ) -> Result<Self, ()> {
        let attr = ElementAttr::new(errors, input);
        let mut fields: Vec<_> = match struct_.fields {
            Fields::Named(ref fields) => fields
                .named
                .iter()
                .flat_map(|f| ElementField::new(errors, &attr, f))
                .collect(),
            _ => {
                errors.push(syn::Error::new_spanned(
                    &input.ident,
                    "#[derive(static_xml::Deserialize)] only supports structs with named fields",
                ));
                return Err(());
            }
        };
        let mut sorted_elements = Vec::new();
        let mut sorted_attributes = Vec::new();
        let mut text_field_pos = None;
        for (i, f) in fields.iter().enumerate() {
            match f.mode {
                ElementFieldMode::Attribute { .. } => {
                    sorted_attributes.push(i);
                }
                ElementFieldMode::Element { .. } => {
                    sorted_elements.push(i);
                }
                ElementFieldMode::Text if text_field_pos.is_some() => {
                    errors.push(syn::Error::new_spanned(
                        &f.inner.ident,
                        "more than one field marked with #[static_xml(text)]",
                    ));
                    return Err(());
                }
                ElementFieldMode::Text => text_field_pos = Some(i),
                _ => {}
            }
        }
        sorted_attributes.sort_by_key(|&p| fields[p].name.expanded());
        sorted_elements.sort_by_key(|&p| fields[p].name.expanded());
        for (i, &p) in sorted_attributes.iter().enumerate() {
            debug_assert!(matches!(fields[p].mode, ElementFieldMode::Attribute { .. }));
            fields[p].mode = ElementFieldMode::Attribute {
                sorted_attributes_pos: i,
            };
        }
        for (i, &p) in sorted_elements.iter().enumerate() {
            debug_assert!(matches!(fields[p].mode, ElementFieldMode::Element { .. }));
            fields[p].mode = ElementFieldMode::Element {
                sorted_elements_pos: i,
            };
        }

        Ok(ElementStruct {
            input,
            attr,
            fields,
            sorted_attributes,
            sorted_elements,
            text_field_pos,
        })
    }

    pub(crate) fn quote_flatten_fields(&self) -> Vec<TokenStream> {
        self.fields
            .iter()
            .filter_map(|f| {
                if matches!(f.mode, ElementFieldMode::Flatten) {
                    let ident = &f.inner.ident;
                    Some(quote_spanned! { f.inner.ident.span() => &mut self.#ident })
                } else {
                    None
                }
            })
            .collect()
    }
}

const STATIC_XML: &str = "static_xml";

// Stolen from serde/serde_derive/src/internals/attr.rs.
pub(crate) fn get_meta_items<'a>(errors: &Errors, attrs: &[syn::Attribute]) -> Vec<NestedMeta> {
    let mut out = Vec::new();
    for attr in attrs {
        if !attr.path.is_ident(STATIC_XML) {
            continue;
        }

        match attr.parse_meta() {
            Ok(Meta::List(meta)) => out.extend(meta.nested.into_iter()),
            Ok(other) => errors.push(syn::Error::new_spanned(
                other,
                "expected #[static_xml(...)]",
            )),
            Err(err) => errors.push(err),
        }
    }
    out
}

/// Mode for a field within an `ElementStruct`.
#[derive(Copy, Clone, Debug)]
pub(crate) enum ElementFieldMode {
    Element { sorted_elements_pos: usize },
    Attribute { sorted_attributes_pos: usize },
    Text,
    Flatten,
}

impl ElementFieldMode {
    pub(crate) fn quote_deserialize_trait(self) -> TokenStream {
        match self {
            ElementFieldMode::Element { .. } => quote! { ::static_xml::de::DeserializeField },
            ElementFieldMode::Attribute { .. } => quote! { ::static_xml::de::DeserializeAttr },
            ElementFieldMode::Text => panic!("text is different"),
            ElementFieldMode::Flatten => quote! { ::static_xml::de::DeserializeFlatten },
        }
    }
}

/// Field within an `ElementStruct`.
pub(crate) struct ElementField<'a> {
    pub(crate) inner: &'a syn::Field,
    pub(crate) mode: ElementFieldMode,
    pub(crate) default: bool,
    pub(crate) name: Name<'a>,
}

impl<'a> ElementField<'a> {
    /// For use by `ElementStruct::new` only.
    fn new(errors: &Errors, outer_attr: &ElementAttr, inner: &'a syn::Field) -> Result<Self, ()> {
        let mut attribute = false;
        let mut default = false;
        let mut flatten = false;
        let mut text = false;
        let mut name =
            Name::from_ident(inner.ident.as_ref().expect("struct fields should be named"));
        for item in get_meta_items(errors, &inner.attrs) {
            match &item {
                NestedMeta::Meta(Meta::Path(p)) if p.is_ident("attribute") => attribute = true,
                NestedMeta::Meta(Meta::Path(p)) if p.is_ident("default") => default = true,
                // TODO: also allow explicit defaults.
                NestedMeta::Meta(Meta::Path(p)) if p.is_ident("flatten") => flatten = true,
                NestedMeta::Meta(Meta::Path(p)) if p.is_ident("text") => text = true,
                NestedMeta::Meta(Meta::NameValue(nv @ MetaNameValue { ref path, .. }))
                    if path.is_ident("rename") =>
                {
                    with_lit_str(errors, nv, &mut |l| name.local_name = l.value());
                }
                NestedMeta::Meta(Meta::NameValue(nv @ MetaNameValue { path, .. }))
                    if path.is_ident("prefix") =>
                {
                    outer_attr.namespaces.process_prefix(errors, nv, &mut name);
                }
                i => errors.push(syn::Error::new_spanned(i, "item not understood")),
            }
        }
        let mode = if attribute && !flatten && !text {
            // The position here is just a placeholder; ElementStruct::new fixes it.
            ElementFieldMode::Attribute {
                sorted_attributes_pos: usize::MAX,
            }
        } else if !attribute && !flatten && !text {
            // Likewise.
            ElementFieldMode::Element {
                sorted_elements_pos: usize::MAX,
            }
        } else if !attribute && flatten && !text {
            if default {
                errors.push(syn::Error::new_spanned(
                    inner,
                    "default and flatten are mutually exclusive",
                ));
                return Err(());
            }
            ElementFieldMode::Flatten
        } else if !attribute && !flatten && text {
            ElementFieldMode::Text
        } else {
            errors.push(syn::Error::new_spanned(
                inner,
                "attribute, flatten, and text are mutually exclusive",
            ));
            return Err(());
        };
        Ok(ElementField {
            inner,
            default,
            mode,
            name,
        })
    }
}

pub(crate) struct ElementEnum<'a> {
    pub(crate) input: &'a DeriveInput,
    pub(crate) attr: ElementAttr<'a>,

    /// Non-skipped variants, sorted by `(namespace, local_name)`.
    pub(crate) variants: Vec<ElementVariant<'a>>,
    pub(crate) has_skipped_variants: bool,
}

pub(crate) struct ElementVariant<'a> {
    pub(crate) ident: &'a syn::Ident,
    pub(crate) name: Name<'a>,
    pub(crate) ty: Option<&'a syn::Type>,
}

impl<'a> ElementEnum<'a> {
    pub(crate) fn new(errors: &Errors, input: &'a DeriveInput, enum_: &'a syn::DataEnum) -> Self {
        let attr = ElementAttr::new(errors, input);
        let mut has_skipped_variants = false;
        let mut variants: Vec<_> = enum_
            .variants
            .iter()
            .filter_map(|v| {
                let mut name = Name::from_ident(&v.ident);
                for item in get_meta_items(errors, &v.attrs) {
                    match &item {
                        NestedMeta::Meta(Meta::NameValue(nv @ MetaNameValue { ref path, .. }))
                            if path.is_ident("rename") =>
                        {
                            with_lit_str(errors, nv, &mut |l| name.local_name = l.value());
                        }
                        NestedMeta::Meta(Meta::NameValue(nv @ MetaNameValue { ref path, .. }))
                            if path.is_ident("prefix") =>
                        {
                            attr.namespaces.process_prefix(errors, nv, &mut name);
                        }
                        NestedMeta::Meta(Meta::Path(p)) if p.is_ident("skip") => {
                            has_skipped_variants = true;
                            return None;
                        }
                        i => errors.push(syn::Error::new_spanned(i, "item not understood")),
                    }
                }
                let ty = optional_unnamed_field(errors, &v.ident, &v.fields)
                    .ok()?
                    .map(|f| &f.ty);
                Some(ElementVariant {
                    ident: &v.ident,
                    name,
                    ty,
                })
            })
            .collect();
        variants.sort_by(|a, b| {
            a.name
                .namespace
                .cmp(&b.name.namespace)
                .then_with(|| a.name.local_name.cmp(&b.name.local_name))
        });
        ElementEnum {
            input,
            attr,
            variants,
            has_skipped_variants,
        }
    }
}

pub(crate) fn with_lit_str(
    errors: &Errors,
    name_value: &MetaNameValue,
    f: &mut dyn FnMut(&LitStr),
) {
    if let Lit::Str(s) = &name_value.lit {
        f(s);
    } else {
        errors.push(syn::Error::new_spanned(
            &name_value.lit,
            format!(
                "{:?} expects a string literal",
                name_value.path.to_token_stream()
            ),
        ));
    }
}

pub(crate) fn set_whitespace(
    errors: &Errors,
    whitespace: &mut static_xml::de::WhiteSpace,
    name_value: &MetaNameValue,
) {
    with_lit_str(errors, name_value, &mut |l| match l.value().as_str() {
        "preserve" => *whitespace = WhiteSpace::Preserve,
        "replace" => *whitespace = WhiteSpace::Replace,
        "collapse" => *whitespace = WhiteSpace::Collapse,
        _ => errors.push(syn::Error::new_spanned(
            l,
            "expected preserve, replace, or collapse",
        )),
    });
}

pub(crate) fn single_unnamed_field(fields: &syn::Fields) -> Option<&syn::Field> {
    if let syn::Fields::Unnamed(f) = fields {
        if f.unnamed.len() == 1 {
            return Some(&f.unnamed[0]);
        }
    }
    None
}

pub(crate) fn optional_unnamed_field<'a>(
    errors: &Errors,
    ident: &syn::Ident,
    fields: &'a syn::Fields,
) -> Result<Option<&'a syn::Field>, ()> {
    match fields {
        syn::Fields::Unit => Ok(None),
        syn::Fields::Unnamed(f) if f.unnamed.len() == 1 => Ok(Some(&f.unnamed[0])),
        _ => {
            errors.push(syn::Error::new_spanned(
                &ident,
                "expected zero type or one unnamed field",
            ));
            Err(())
        }
    }
}
