// Copyright (C) 2021 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

//! Logic to derive the [`static_xml::Deserialize`] trait.

use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{spanned::Spanned, Data};

use crate::common::{ElementEnum, ElementField, ElementFieldMode, ElementStruct, Errors};

pub(crate) fn quote_flatten_visitors(struct_: &ElementStruct) -> Vec<TokenStream> {
    struct_
        .fields
        .iter()
        .filter_map(|f| {
            if matches!(f.mode, ElementFieldMode::Flatten) {
                let field_visitor = format_ident!("{}_visitor", f.ident);
                Some(quote_spanned! { f.inner.ident.span() => &mut self.#field_visitor })
            } else {
                None
            }
        })
        .collect()
}

fn field_mut(field: &ElementField) -> TokenStream {
    let ident = field.ident;
    quote! { unsafe { ::std::ptr::addr_of_mut!((*self.out).#ident) } }
}

fn visitor_field_definitions(struct_: &ElementStruct) -> Vec<TokenStream> {
    struct_
        .fields
        .iter()
        .filter_map(|field| {
            let span = field.inner.span();
            let field_present = format_ident!("{}_present", field.ident);
            let field_visitor = format_ident!("{}_visitor", field.ident);
            let ty = &field.inner.ty;
            match field.mode {
                ElementFieldMode::Text => None,
                ElementFieldMode::Flatten => Some(quote_spanned! {span=>
                    #field_visitor: <#ty as ::static_xml::de::RawDeserialize<'out>>::Visitor
                }),
                ElementFieldMode::Element { .. } | ElementFieldMode::Attribute { .. } => {
                    Some(quote_spanned! {span=>
                        #field_present: bool
                    })
                }
            }
        })
        .collect()
}

fn visitor_initializers(struct_: &ElementStruct) -> Vec<TokenStream> {
    struct_
        .fields
        .iter()
        .map(|field| {
            let span = field.inner.span();
            match field.mode {
                ElementFieldMode::Flatten => {
                    let field_visitor = format_ident!("{}_visitor", field.ident);
                    let fident = field.ident;
                    let ty = &field.inner.ty;
                    quote_spanned! {span=>
                        #field_visitor: <#ty as ::static_xml::de::RawDeserialize>::Visitor::new(
                            // SAFETY: out points to valid, uninitialized memory.
                            unsafe {
                                &mut *(
                                    ::std::ptr::addr_of_mut!((*out.as_mut_ptr()).#fident)
                                    as *mut ::std::mem::MaybeUninit<#ty>
                                )
                            }
                        )
                    }
                }
                _ => {
                    let field_present = format_ident!("{}_present", field.ident);
                    quote_spanned! {span=>
                        #field_present: false
                    }
                }
            }
        })
        .collect()
}

fn finalize_visitor_fields(struct_: &ElementStruct) -> Vec<TokenStream> {
    struct_.fields.iter().map(|field| {
        let span = field.inner.span();
        let ty = &field.inner.ty;
        let default = field.default.then(|| {
            quote_spanned! {span=>
                <#ty as ::std::default::Default>::default
            }
        });
        match field.mode {
            ElementFieldMode::Element { sorted_elements_pos: p } => {
                let field_present = format_ident!("{}_present", field.ident);
                let field_mut = field_mut(field);
                let value = match default {
                    Some(d) => quote! { #d() },
                    None => quote_spanned! {span=>
                        <#ty as ::static_xml::de::DeserializeElementField>::missing(&ELEMENTS[#p])?
                    }
                };
                quote_spanned! {span=>
                    if !self.#field_present {
                        // SAFETY: #field_mut is a valid pointer to uninitialized memory.
                        unsafe { ::std::ptr::write(#field_mut as *mut #ty, #value) };
                    }
                }
            }
            ElementFieldMode::Attribute { sorted_attributes_pos: p } => {
                let field_present = format_ident!("{}_present", field.ident);
                let field_mut = field_mut(field);
                let value = match default {
                    Some(d) => quote! { #d() },
                    None => quote_spanned! {span=>
                        <#ty as ::static_xml::de::DeserializeAttrField>::missing(&ATTRIBUTES[#p])?
                    }
                };
                quote_spanned! {span=>
                    if !self.#field_present {
                        // SAFETY: #field_mut is a valid pointer to uninitialized memory.
                        unsafe { ::std::ptr::write(#field_mut /*as *mut #ty*/, #value) };
                    }
                }
            }
            ElementFieldMode::Flatten => {
                let field_visitor = format_ident!("{}_visitor", field.ident);
                let d = if let Some(d) = default {
                    quote! { Some(#d) }
                } else {
                    quote! { None }
                };
                quote_spanned! {span=>
                    <#ty as ::static_xml::de::RawDeserialize>::Visitor::finalize(self.#field_visitor, #d)?;
                }
            }
            ElementFieldMode::Text => todo!("text fields unimplemented"),
        }
    }).collect()
}

fn attributes(struct_: &ElementStruct) -> Vec<TokenStream> {
    struct_
        .sorted_attributes
        .iter()
        .map(|&p| struct_.fields[p].name.quote_expanded())
        .collect()
}

fn elements(struct_: &ElementStruct) -> Vec<TokenStream> {
    struct_
        .sorted_elements
        .iter()
        .map(|&p| struct_.fields[p].name.quote_expanded())
        .collect()
}

fn attribute_match_branches(struct_: &ElementStruct) -> Vec<TokenStream> {
    struct_
        .sorted_attributes
        .iter()
        .enumerate()
        .map(|(i, &p)| {
            let field = &struct_.fields[p];
            let field_present = format_ident!("{}_present", field.ident);
            let field_mut = field_mut(field);
            quote! {
                Some(#i) if self.#field_present => {
                    Err(::static_xml::de::VisitorError::duplicate_attribute(name))
                }
                Some(#i) => {
                    // SAFETY: #field_mut is a valid pointer to uninitialized memory.
                    unsafe {
                        ::std::ptr::write(
                            #field_mut,
                            ::static_xml::de::DeserializeAttrField::init(value)?,
                        );
                    }
                    self.#field_present = true;
                    Ok(None)
                }
            }
        })
        .collect()
}

fn element_match_branches(struct_: &ElementStruct) -> Vec<TokenStream> {
    struct_
        .sorted_elements
        .iter()
        .enumerate()
        .map(|(i, &p)| {
            let field = &struct_.fields[p];
            let span = field.inner.span();
            let field_present = format_ident!("{}_present", &field.ident);
            let field_mut = field_mut(field);
            quote_spanned! {span=>
                Some(#i) if self.#field_present => {
                    ::static_xml::de::DeserializeElementField::update(
                        // SAFETY: the field is initialized when field_present is true.
                        unsafe { &mut *#field_mut },
                        child,
                    )?;
                    Ok(None)
                }
                Some(#i) => unsafe {
                    // SAFETY: #field_mut is a valid pointer to uninitialized memory.
                    ::std::ptr::write(
                        #field_mut,
                        ::static_xml::de::DeserializeElementField::init(child)?,
                    );
                    self.#field_present = true;
                    Ok(None)
                }
            }
        })
        .collect()
}

fn do_struct(struct_: &ElementStruct) -> TokenStream {
    let attributes = attributes(&struct_);
    let attribute_match_branches = attribute_match_branches(&struct_);
    let elements = elements(&struct_);
    let element_match_branches = element_match_branches(&struct_);

    let ident = &struct_.input.ident;
    let visitor_type = format_ident!("{}Visitor", &struct_.input.ident);
    let visitor_field_definitions = visitor_field_definitions(&struct_);
    let visitor_initializers = visitor_initializers(&struct_);
    let finalize_visitor_fields = finalize_visitor_fields(&struct_);
    let flatten_visitors = quote_flatten_visitors(&struct_);
    let (attribute_fallthrough, element_fallthrough);
    if flatten_visitors.is_empty() {
        attribute_fallthrough = quote! { Ok(Some(value)) };
        element_fallthrough = quote! { Ok(Some(child)) };
    } else {
        attribute_fallthrough = quote! {
            ::static_xml::de::delegate_attribute(&mut [#(#flatten_visitors),*], name, value)
        };
        element_fallthrough = quote! {
            ::static_xml::de::delegate_element(&mut [#(#flatten_visitors),*], child)
        };
    }
    let visitor_characters = if struct_.text_field_pos.is_some() {
        quote! {
            fn characters(&mut self, s: String, _p: ::static_xml::TextPosition) -> Result<Option<String>, ::static_xml::de::VisitorError> {
                self._text_buf.push_str(&s);
                Ok(None)
            }
        }
    } else if !flatten_visitors.is_empty() {
        quote! {
            fn characters(&mut self, s: String, p: ::static_xml::TextPosition) -> Result<Option<String>, ::static_xml::BoxedStdError> {
                ::static_xml::de::delegate_characters(&mut [#(#flatten_visitors),*], s, p)
            }
        }
    } else {
        TokenStream::new()
    };
    quote! {
        const ATTRIBUTES: &[::static_xml::ExpandedNameRef] = &[#(#attributes,)*];
        const ELEMENTS: &[::static_xml::ExpandedNameRef] = &[#(#elements,)*];

        // If there's an underlying field named e.g. `foo_`, then there will be
        // a generated field name e.g. `foo__required` or `foo__visitor`. Don't
        // complain about this.
        #[allow(non_snake_case)]
        struct #visitor_type<'out> {
            // This can't be &mut MaybeUninit<#type> because flattened fields
            // (if any) get a &mut MaybeUninit<#type> that aliases it. So
            // use a raw pointer and addr_of! to access individual fields, and
            // PhantomData for the lifetime.
            out: *mut #ident,
            _phantom: ::std::marker::PhantomData<&'out mut #ident>,
            #(#visitor_field_definitions, )*
        }

        impl<'out> ::static_xml::de::ElementVisitor for #visitor_type<'out> {
            fn element<'a>(
                &mut self,
                child: ::static_xml::de::ElementReader<'a>,
            ) -> Result<Option<::static_xml::de::ElementReader<'a>>, ::static_xml::de::VisitorError> {
                match ::static_xml::de::find(&child.expanded_name(), ELEMENTS) {
                    #(#element_match_branches,)*
                    _ => #element_fallthrough
                }
            }

            fn attribute<'a>(
                &mut self,
                name: &::static_xml::ExpandedNameRef,
                value: String,
            ) -> Result<Option<String>, ::static_xml::de::VisitorError> {
                match ::static_xml::de::find(name, ATTRIBUTES) {
                    #(#attribute_match_branches,)*
                    _ => #attribute_fallthrough
                }
            }

            #visitor_characters
        }

        unsafe impl<'out> ::static_xml::de::RawDeserializeVisitor<'out> for #visitor_type<'out> {
            type Out = #ident;

            fn new(out: &'out mut ::std::mem::MaybeUninit<Self::Out>) -> Self {
                Self {
                    out: out.as_mut_ptr(),
                    _phantom: ::std::marker::PhantomData,
                    #(#visitor_initializers, )*
                }
            }

            fn finalize(self, _default: Option<fn() -> Self::Out>) -> Result<(), ::static_xml::de::VisitorError> {
                // Note _default is currently unsupported for structs.

                #(#finalize_visitor_fields)*
                // SAFETY: returning `Ok` guarantees `self.out` is fully initialized.
                // finalize_visitor_fields has guaranteed that each field is fully initialized.
                // The padding doesn't matter. This is similar to this example
                // https://doc.rust-lang.org/stable/std/mem/union.MaybeUninit.html#initializing-a-struct-field-by-field
                Ok(())
            }
        }

        ::static_xml::impl_deserialize_via_raw!(#ident, #visitor_type);
    }
}

fn do_enum(enum_: &ElementEnum) -> TokenStream {
    let ident = &enum_.input.ident;
    let visitor_type = format_ident!("{}Visitor", &enum_.input.ident);
    let elements: Vec<_> = enum_
        .variants
        .iter()
        .map(|v| v.name.quote_expanded())
        .collect();
    let initialized_match_arms: Vec<TokenStream> = enum_
        .variants
        .iter()
        .enumerate()
        .map(|(i, v)| {
            let vident = v.ident;
            let span = vident.span();

            match v.ty {
                None => {
                    quote_spanned! {span=>
                        #ident::#vident if element_i == Some(#i) => {
                            return Ok(None);
                        }
                        #ident::#vident => #i
                    }
                }
                Some(_) => {
                    quote_spanned! {span=>
                        #ident::#vident(f) if element_i == Some(#i) => {
                            ::static_xml::de::DeserializeElementField::update(f, child)?;
                            return Ok(None);
                        }
                        #ident::#vident(_) => #i
                    }
                }
            }
        })
        .collect();
    let uninitialized_match_arms: Vec<TokenStream> = enum_
        .variants
        .iter()
        .enumerate()
        .map(|(i, v)| {
            let vident = v.ident;
            match v.ty {
                None => {
                    quote_spanned! {
                        vident.span() => Some(#i) => { #ident::#vident }
                    }
                }
                Some(_) => {
                    quote_spanned! {
                        vident.span() => Some(#i) => {
                            #ident::#vident(::static_xml::de::DeserializeElementField::init(child)?)
                        }
                    }
                }
            }
        })
        .collect();

    // #ident and #visitor_type's visibilities must exactly match due to a trait reference cycle:
    // * #ident refers to #visitor_type via RawDeserialize::Visitor
    // * #visitor_type refers to #ident via RawDeserializeVisitor::Out
    let vis = &enum_.input.vis;

    quote! {
        const ELEMENTS: &[::static_xml::ExpandedNameRef] = &[#(#elements,)*];

        #vis struct #visitor_type<'out> {
            out: &'out mut ::std::mem::MaybeUninit<#ident>,
            initialized: bool,
        }

        impl<'out> ::static_xml::de::ElementVisitor for #visitor_type<'out> {
            fn element<'a>(
                &mut self,
                mut child: ::static_xml::de::ElementReader<'a>,
            ) -> Result<Option<::static_xml::de::ElementReader<'a>>, ::static_xml::de::VisitorError> {
                let name = child.expanded_name();
                let element_i = ::static_xml::de::find(&name, ELEMENTS);
                if self.initialized {
                    // SAFETY: self.out is initialized when self.initialized is true.
                    let expected_i = match unsafe { self.out.assume_init_mut() } {
                        #(#initialized_match_arms,)*
                    };
                    if let Some(element_i) = element_i {
                        return Err(::static_xml::de::VisitorError::unexpected_element(
                            &name,
                            &ELEMENTS[expected_i],
                        ));
                    }
                    return Ok(None);
                }
                self.out.write(match element_i {
                    #(#uninitialized_match_arms,)*
                    _ => return Ok(Some(child)),
                });
                self.initialized = true;
                Ok(None)
            }
        }

        unsafe impl<'out> ::static_xml::de::RawDeserializeVisitor<'out> for #visitor_type<'out> {
            type Out = #ident;

            fn new(out: &'out mut ::std::mem::MaybeUninit<Self::Out>) -> Self {
                Self {
                    out,
                    initialized: false,
                }
            }

            fn finalize(
                self,
                default: Option<fn() -> Self::Out>,
            ) -> Result<(), ::static_xml::de::VisitorError> {
                if !self.initialized {
                    if let Some(d) = default {
                        self.out.write(d());
                    } else {
                        return Err(static_xml::de::VisitorError::cant_be_empty(stringify!(#ident)));
                    }
                }
                // SAFETY: returning `Ok` guarantees `self.out` is fully initialized.
                Ok(())
            }
        }

        impl<'out> ::static_xml::de::RawDeserialize<'out> for #ident {
            type Visitor = #visitor_type<'out>;
        }

        ::static_xml::impl_deserialize_via_raw!(#ident, #visitor_type);
    }
}

pub(crate) fn derive(errors: &Errors, input: syn::DeriveInput) -> Result<TokenStream, ()> {
    match input.data {
        Data::Enum(ref data) => Ok(do_enum(&ElementEnum::new(&errors, &input, data))),
        Data::Struct(ref data) => ElementStruct::new(&errors, &input, data).map(|s| do_struct(&s)),
        _ => {
            errors.push(syn::Error::new_spanned(
                input.ident,
                "unsupported data type",
            ));
            Err(())
        }
    }
}
