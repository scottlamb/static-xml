// Copyright (C) 2021 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

use proc_macro2::TokenStream;
use quote::quote_spanned;
use syn::Data;

use crate::common::{
    Errors, RestrictionVariant, TextEnum, TextEnumMode, TextVariantMode, UnionVariant,
};

fn do_enum(enum_: TextEnum) -> TokenStream {
    match &enum_.mode {
        TextEnumMode::Union(v) => do_union(&enum_, &v),
        TextEnumMode::Restriction { variants, .. } => do_restriction(&enum_, &variants),
    }
}

fn do_union(enum_: &TextEnum, variants: &[UnionVariant]) -> TokenStream {
    let match_arms: Vec<TokenStream> = variants
        .iter()
        .map(|v| {
            let vident = &v.inner.ident;
            quote_spanned! { vident.span() => Self::#vident(ref f) => ::static_xml::ser::ToText::to_text(f) }
        })
        .collect();
    let ident = &enum_.input.ident;
    quote_spanned! {
        ident.span() =>
        impl ::static_xml::ser::ToText for #ident {
            fn to_text(&self) -> Result<String, ::static_xml::ser::Error> {
                match self {
                    #(#match_arms, )*
                }
            }
        }
    }
}

fn do_restriction(enum_: &TextEnum, variants: &[RestrictionVariant]) -> TokenStream {
    let match_arms: Vec<TokenStream> = variants
        .iter()
        .map(|v| {
            let vident = &v.inner.ident;
            match v.mode {
                TextVariantMode::Known { ref text } => {
                    quote_spanned! { vident.span() => Self::#vident => #text }
                },
                TextVariantMode::Unknown => {
                    quote_spanned! { vident.span() => Self::#vident(text) => ::std::convert::AsRef::<str>::as_ref(text) }
                },
            }
        })
        .collect();
    let ident = &enum_.input.ident;
    quote_spanned! {
        ident.span() =>
        impl ::static_xml::ser::ToText for #ident {
            fn to_text(&self) -> Result<String, ::static_xml::ser::Error> {
                Ok(match self {
                    #(#match_arms, )*
                }.to_owned())
            }
        }
    }
}

pub(crate) fn derive(errors: &Errors, input: syn::DeriveInput) -> Result<TokenStream, ()> {
    match input.data {
        Data::Enum(ref data) => Ok(do_enum(TextEnum::new(&errors, &input, data))),
        _ => {
            errors.push(syn::Error::new_spanned(
                input.ident,
                "unsupported data type",
            ));
            Err(())
        }
    }
}
