// Copyright (C) 2021 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

//! Logic to derive the [`static_xml::ParseText`] trait.

use proc_macro2::TokenStream;
use quote::{quote, quote_spanned};
use static_xml::de::WhiteSpace;
use syn::Data;

use crate::common::{
    Errors, RestrictionVariant, TextAttr, TextEnum, TextEnumMode, TextMode, TextVariantMode,
    UnionVariant,
};

fn do_enum(enum_: TextEnum) -> TokenStream {
    match &enum_.mode {
        TextEnumMode::Union(v) => do_union(&enum_, &v),
        TextEnumMode::Restriction {
            variants,
            unknown_variant,
        } => do_restriction(&enum_, &variants, *unknown_variant),
    }
}

fn do_union(enum_: &TextEnum, variants: &[UnionVariant]) -> TokenStream {
    let normalize = normalize(enum_.whitespace);
    let ident = &enum_.input.ident;

    // TODO:
    // * use a better Error type which passes back the original String, to avoid the clone.
    // * maybe preserve one of the errors on parse failure?
    // * maybe recognize String and avoid generating error path after it.
    let attempts: Vec<TokenStream> = variants
        .iter()
        .map(|v| {
            let ident = &v.inner.ident;
            let ty = &v.ty;
            quote_spanned! {
                ident.span() =>
                match <#ty as ::static_xml::de::ParseText>::parse(text.clone()) {
                    Ok(r) => return Ok(Self::#ident(r)),
                    Err(e) => errors.push(e),
                }
            }
        })
        .collect();
    quote! {
        impl ::static_xml::de::ParseText for #ident {
            fn parse(text: String) -> Result<Self, ::static_xml::BoxedStdError> {
                #normalize
                let mut errors = Vec::new();
                #(#attempts)*
                Err(::static_xml::de::union_error(stringify!(#ident), errors))
            }
        }
    }
}

fn do_restriction(
    enum_: &TextEnum,
    variants: &[RestrictionVariant],
    unknown_variant: Option<usize>,
) -> TokenStream {
    let ident = &enum_.input.ident;
    let match_arms: Vec<TokenStream> = variants
        .iter()
        .filter_map(|v| {
            let text = match v.mode {
                TextVariantMode::Known { ref text } => text,
                TextVariantMode::Unknown => return None,
            };
            let ident = &v.inner.ident;
            Some(quote_spanned! { ident.span() => #text => Ok(Self::#ident) })
        })
        .collect();
    let fallthrough = if let Some(i) = unknown_variant {
        let vident = &variants[i].inner.ident;
        quote_spanned! { vident.span() => _ => Ok(Self::#vident(text)) }
    } else {
        quote! { t => Err(::static_xml::de::no_such_variant(stringify!(#ident), t)) }
    };
    let ident = &enum_.input.ident;
    let normalize = normalize(enum_.whitespace);
    quote! {
        impl ::static_xml::de::ParseText for #ident {
            fn parse(text: String) -> Result<Self, ::static_xml::BoxedStdError> {
                #normalize
                match text.as_str() {
                    #(#match_arms, )*
                    #fallthrough
                }
            }
        }
    }
}

fn do_std(ident: &proc_macro2::Ident, attr: &TextAttr) -> TokenStream {
    let normalize = normalize(attr.whitespace);
    quote! {
        impl ::static_xml::de::ParseText for #ident {
            fn parse(text: String) -> Result<Self, ::static_xml::BoxedStdError> {
                #normalize
                ::std::str::FromStr::from_str(text.as_str())
                    .map_err(|e| Box::new(e) as ::static_xml::BoxedStdError)
            }
        }
    }
}

fn normalize(whitespace: WhiteSpace) -> TokenStream {
    match whitespace {
        WhiteSpace::Preserve => TokenStream::new(),
        WhiteSpace::Replace => {
            quote! { let text = ::static_xml::de::normalize(text, ::static_xml::de::WhiteSpace::Replace); }
        }
        WhiteSpace::Collapse => {
            quote! { let text = ::static_xml::de::normalize(text, ::static_xml::de::WhiteSpace::Collapse); }
        }
    }
}

pub(crate) fn derive(errors: &Errors, input: syn::DeriveInput) -> Result<TokenStream, ()> {
    let attr = TextAttr::new(errors, &input);
    match attr.mode {
        None => {
            errors.push(syn::Error::new_spanned(
                input.ident,
                "static_xml(mode) must be specified for text types",
            ));
            return Err(());
        }
        Some(TextMode::Std) => {
            return Ok(do_std(&input.ident, &attr));
        }
        _ => {}
    }
    match input.data {
        Data::Enum(ref data) => Ok(do_enum(TextEnum::new(errors, &input, attr, data))),
        _ => {
            errors.push(syn::Error::new_spanned(
                input.ident,
                "unsupported data type",
            ));
            Err(())
        }
    }
}
