// Copyright (C) 2021 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

//! Logic to derive the [`static_xml::Deserialize`] trait.

use proc_macro2::TokenStream;
use quote::{format_ident, quote, quote_spanned};
use syn::{spanned::Spanned, Data};

use crate::common::{ElementEnum, ElementFieldMode, ElementStruct, Errors};

fn visitor_field_definitions(struct_: &ElementStruct) -> Vec<TokenStream> {
    struct_
        .fields
        .iter()
        .filter_map(|field| {
            let span = field.inner.span();
            let field_present = format_ident!("{}_present", &field.inner.ident.as_ref().unwrap());
            match field.mode {
                ElementFieldMode::Text => None,
                _ => Some(quote_spanned! {span=>
                    #field_present: bool
                }),
            }
        })
        .collect()
}

fn visitor_initializers(struct_: &ElementStruct) -> Vec<TokenStream> {
    struct_
        .fields
        .iter()
        .filter_map(|field| {
            let span = field.inner.span();
            let field_present = format_ident!("{}_present", &field.inner.ident.as_ref().unwrap());
            match field.mode {
                ElementFieldMode::Text => None,
                _ => Some(quote_spanned! {span=>
                    #field_present: false
                }),
            }
        })
        .collect()
}

fn finalize_visitor_fields(struct_: &ElementStruct) -> Vec<TokenStream> {
    struct_.fields.iter().filter_map(|field| {
        let span = field.inner.span();
        let field_present = format_ident!("{}_present", &field.inner.ident.as_ref().unwrap());
        let ty = &field.inner.ty;
        match field.mode {
            ElementFieldMode::Element { sorted_elements_pos: p } if !field.default => {
                Some(quote_spanned! {span=>
                    <#ty as ::static_xml::de::DeserializeField>::finalize(self.#field_present, &ELEMENTS[#p])?;
                })
            }
            ElementFieldMode::Attribute { sorted_attributes_pos: p } if !field.default => {
                Some(quote_spanned! {span=>
                    <#ty as ::static_xml::de::DeserializeAttr>::finalize(self.#field_present, &ATTRIBUTES[#p])?;
                })
            }
            /*ElementFieldMode::Flatten if !field.default => {
                Some(quote_spanned! {span=>
                    <#ty as ::static_xml::de::DeserializeFlatten>::finalize(self.#field_present)?;
                })
            }*/
            _ => None
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
            let field_present = format_ident!("{}_present", &field.inner.ident.as_ref().unwrap());
            let ident = field.inner.ident.as_ref().unwrap();
            quote! {
                Some(#i) => {
                    ::static_xml::de::DeserializeAttr::attr(&mut self.out.#ident, &mut self.#field_present, name, value)?;
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
            let field_present = format_ident!("{}_present", &field.inner.ident.as_ref().unwrap());
            let ident = field.inner.ident.as_ref().unwrap();
            let span = ident.span();
            quote_spanned! {span=>
                Some(#i) => {
                    ::static_xml::de::DeserializeField::element(&mut self.out.#ident, &mut self.#field_present, child)?;
                    return Ok(None)
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
    let (impl_generics, ty_generics, where_clause) = struct_.input.generics.split_for_impl();
    let visitor_type = format_ident!("{}Visitor", &struct_.input.ident);
    let visitor_field_definitions = visitor_field_definitions(&struct_);
    let visitor_initializers = visitor_initializers(&struct_);
    let finalize_visitor_fields = finalize_visitor_fields(&struct_);
    let visitor_defs = quote! {
        pub struct #visitor_type<'out> {
            out: &'out mut #ident,
            #(#visitor_field_definitions, )*
        }

        impl<'out> #visitor_type<'out> {
            fn new(out: &'out mut #ident) -> Self {
                Self {
                    out,
                    #(#visitor_initializers, )*
                }
            }

            fn finalize(self) -> Result<(), ::static_xml::de::VisitorError> {
                #(#finalize_visitor_fields)*
                Ok(())
            }
        }
    };
    let flatten_fields = struct_.quote_flatten_fields();
    let (attribute_fallthrough, element_fallthrough);
    if flatten_fields.is_empty() {
        attribute_fallthrough = quote! { Ok(Some(value)) };
        element_fallthrough = quote! { Ok(Some(child)) };
    } else {
        attribute_fallthrough = quote! {
            ::static_xml::de::delegate_attribute(&mut [#(#flatten_fields),*], name, value)
        };
        element_fallthrough = quote! {
            ::static_xml::de::delegate_element(&mut [#(#flatten_fields),*], child)
        };
    }
    let visitor_characters = if struct_.text_field_pos.is_some() {
        quote! {
            fn characters(&mut self, s: String, _p: ::static_xml::TextPosition) -> Result<Option<String>, ::static_xml::de::VisitorError> {
                self._text_buf.push_str(&s);
                Ok(None)
            }
        }
    } else if !flatten_fields.is_empty() {
        quote! {
            fn characters(&mut self, s: String, p: ::static_xml::TextPosition) -> Result<Option<String>, ::static_xml::BoxedStdError> {
                ::static_xml::de::delegate_characters(&mut [#(#flatten_fields),*], s, p)
            }
        }
    } else {
        TokenStream::new()
    };
    quote! {
        const ATTRIBUTES: &[::static_xml::ExpandedNameRef] = &[#(#attributes,)*];
        const ELEMENTS: &[::static_xml::ExpandedNameRef] = &[#(#elements,)*];

        #visitor_defs

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

        impl #impl_generics ::static_xml::de::Deserialize for #ident #ty_generics
        #where_clause {
            fn deserialize(
                element: ::static_xml::de::ElementReader<'_>,
            ) -> Result<Self, ::static_xml::de::VisitorError> {
                let mut out = <#ident as Default>::default();
                let mut builder = #visitor_type::new(&mut out);
                element.read_to(&mut builder)?;
                builder.finalize()?;
                Ok(out)
            }
        }

        /*impl #impl_generics ::static_xml::de::DeserializeFlatten for #ident #ty_generics
        #where_clause {
            type Builder = #visitor_type;

            fn init() -> Self::Builder {
                #visitor_type::default()
            }

            fn finalize(builder: Self::Builder) -> Result<Self, ::static_xml::de::VisitorError> {
                #finalize
            }
        }*/
    }
}

/*fn do_enum_indirect(enum_: &ElementEnum) -> TokenStream {
    let ident = &enum_.input.ident;
    let elements: Vec<_> = enum_
        .variants
        .iter()
        .map(|v| v.name.quote_expanded())
        .collect();
    let visitor_variants: Vec<TokenStream> = enum_
        .variants
        .iter()
        .map(|v| {
            let vident = v.ident;
            match v.ty {
                None => quote_spanned! {vident.span()=> #vident },
                Some(ty) => quote_spanned! {
                    vident.span() =>
                    #vident(<#ty as ::static_xml::de::DeserializeField>::Builder)
                },
            }
        })
        .collect();
    let finalize_match_arms: Vec<TokenStream> = enum_.variants
        .iter()
        .enumerate()
        .map(|(i, v)| {
            let vident = v.ident;
            match v.ty {
                None => quote_spanned! { vident.span() => VisitorInner::#vident => { Ok(#ident::#vident) } },
                Some(ty) => {
                    quote_spanned! {
                        vident.span() =>
                        VisitorInner::#vident(builder) => {
                            <#ty as ::static_xml::de::DeserializeField>::finalize(
                                builder,
                                &ELEMENTS[#i], // unused?
                                Some(|| unreachable!()),
                            ).map(#ident::#vident)
                        }
                    }
                }
            }
        })
        .collect();
    let element_match_arms: Vec<TokenStream> = enum_
        .variants
        .iter()
        .enumerate()
        .map(|(i, v)| {
            let vident = v.ident;
            match v.ty {
                None => quote_spanned! {vident.span()=>
                    Some(VisitorInner::#vident) if i == Some(#i) => return Ok(None),
                    Some(VisitorInner::#vident) if i.is_some() => #i,
                    None if i == Some(#i) => {
                        self.0 = Some(VisitorInner::#vident);
                        return Ok(None);
                    }
                },
                Some(ty) => quote_spanned! {vident.span()=>
                    Some(VisitorInner::#vident(builder)) if i == Some(#i) => {
                        ::static_xml::de::DeserializeFieldBuilder::element(builder, child)?;
                        return Ok(None);
                    }
                    Some(VisitorInner::#vident(_)) if i.is_some() => #i,
                    None if i == Some(#i) => {
                        let mut builder = <#ty as ::static_xml::de::DeserializeField>::init();
                        ::static_xml::de::DeserializeFieldBuilder::element(&mut builder, child)?;
                        self.0 = Some(VisitorInner::#vident(builder));
                        return Ok(None);
                    }
                },
            }
        })
        .collect();

    quote! {
        const ELEMENTS: &[::static_xml::ExpandedNameRef] = &[#(#elements,)*];

        enum VisitorInner {
            #(#visitor_variants,)*
        }

        impl VisitorInner {
            fn finalize(self) -> Result<#ident, ::static_xml::de::VisitorError> {
                match self {
                    #(#finalize_match_arms,)*
                }
            }
        }

        pub struct Visitor(Option<VisitorInner>);

        impl ::static_xml::de::ElementVisitor for Visitor {
            fn element<'a>(
                &mut self,
                mut child: ::static_xml::de::ElementReader<'a>,
            ) -> Result<Option<::static_xml::de::ElementReader<'a>>, ::static_xml::de::VisitorError> {
                let name = child.expanded_name();
                let i = ::static_xml::de::find(&name, ELEMENTS);
                let expected_i = match &mut self.0 {
                    #(#element_match_arms)*
                    _ => return Ok(Some(child))
                };
                Err(::static_xml::de::VisitorError::unexpected_element(
                    &name,
                    &ELEMENTS[expected_i],
                ))
            }
        }

        impl ::static_xml::de::Deserialize for #ident {
            fn deserialize(element: ::static_xml::de::ElementReader<'_>) -> Result<Self, ::static_xml::de::VisitorError> {
                let mut visitor: Visitor = Visitor(None);
                element.read_to(&mut visitor)?;
                visitor
                    .0
                    .ok_or_else(|| static_xml::de::VisitorError::cant_be_empty(stringify!(#ident)))?
                    .finalize()
            }
        }

        impl ::static_xml::de::DeserializeFlatten for #ident {
            type Builder = Visitor;

            fn init() -> Self::Builder { Visitor(None) }
            fn finalize(builder: Self::Builder) -> Result<Self, ::static_xml::de::VisitorError> {
                builder
                    .0
                    .ok_or_else(|| static_xml::de::VisitorError::cant_be_empty(stringify!(#ident)))?
                    .finalize()
            }
        }

        // can't derive DeserializeFlatten for Option<#ident> and Vec<#ident> because of the
        // orphan rule. :-(
    }
}*/

fn do_enum_direct(enum_: &ElementEnum) -> TokenStream {
    let ident = &enum_.input.ident;
    let elements: Vec<_> = enum_
        .variants
        .iter()
        .map(|v| v.name.quote_expanded())
        .collect();
    let match_arms: Vec<TokenStream> = enum_.variants
        .iter()
        .enumerate()
        .map(|(i, v)| {
            let vident = v.ident;
            match v.ty {
                None => {
                    quote_spanned! {
                        vident.span() => Some(#i) => { *self = #ident::#vident; }
                    }
                }
                Some(ty) => {
                    quote_spanned! {
                        vident.span() => Some(#i) => {
                            let mut builder = <#ty as Default>::default();
                            let mut present = false;
                            ::static_xml::de::DeserializeField::element(&mut builder, &mut present, child)?;
                            *self = #ident::#vident(builder);
                        }
                    }
                }
            }
        })
        .collect();

    quote! {
        const ELEMENTS: &[::static_xml::ExpandedNameRef] = &[#(#elements,)*];

        impl ::static_xml::de::ElementVisitor for #ident {
            fn element<'a>(
                &mut self,
                mut child: ::static_xml::de::ElementReader<'a>,
            ) -> Result<Option<::static_xml::de::ElementReader<'a>>, ::static_xml::de::VisitorError> {
                let name = child.expanded_name();
                match ::static_xml::de::find(&name, ELEMENTS) {
                    #(#match_arms,)*
                    _ => return Ok(Some(child)),
                }
                Ok(None)
            }
        }

        impl ::static_xml::de::Deserialize for #ident {
            fn deserialize(element: ::static_xml::de::ElementReader<'_>) -> Result<Self, ::static_xml::de::VisitorError> {
                let mut visitor: #ident = Default::default();
                element.read_to(&mut visitor)?;
                Ok(visitor)
            }
        }

        impl ::static_xml::de::DeserializeFlatten for #ident {
            type Builder = #ident;

            fn init() -> Self::Builder { Default::default() }
            fn finalize(builder: Self::Builder) -> Result<Self, ::static_xml::de::VisitorError> {
                Ok(builder)
            }
        }

        // can't derive DeserializeFlatten for Option<#ident> and Vec<#ident> because of the
        // orphan rule. :-(
    }
}

pub(crate) fn derive(errors: &Errors, input: syn::DeriveInput) -> Result<TokenStream, ()> {
    match input.data {
        Data::Enum(ref data) => {
            let enum_ = ElementEnum::new(&errors, &input, data);
            //if enum_.attr.direct {
                Ok(do_enum_direct(&enum_))
            //} else {
            //    Ok(do_enum_indirect(&enum_))
            //}
        }
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
