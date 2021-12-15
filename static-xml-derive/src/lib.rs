// Copyright (C) 2021 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

mod common;
mod deserialize;
mod parse;
mod serialize;
mod to_text;

use common::Errors;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

fn derive(
    input: proc_macro::TokenStream,
    f: fn(&Errors, syn::DeriveInput) -> Result<proc_macro2::TokenStream, ()>,
) -> proc_macro::TokenStream {
    let errors = Errors::new();
    let input = parse_macro_input!(input as DeriveInput);
    let out = f(&errors, input).unwrap_or_default();
    let errors = errors.take_compile_errors();
    proc_macro::TokenStream::from(quote! {
        const _: () = {
            #errors
            #out
        };
    })
}

#[proc_macro_derive(Deserialize, attributes(static_xml))]
pub fn derive_deserialize(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive(input, deserialize::derive)
}

#[proc_macro_derive(Serialize, attributes(static_xml))]
pub fn derive_serialize(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive(input, serialize::derive)
}

#[proc_macro_derive(ParseText, attributes(static_xml))]
pub fn derive_parse_text(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive(input, parse::derive)
}

#[proc_macro_derive(ToText, attributes(static_xml))]
pub fn derive_to_text(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    derive(input, to_text::derive)
}
