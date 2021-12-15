// Copyright (C) 2021 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

//! Logic to derive the [`static_xml::Serialize`] trait.

use proc_macro2::TokenStream;
use quote::{quote, quote_spanned};
use syn::{spanned::Spanned, Data};

use crate::common::{ElementAttr, ElementEnum, ElementFieldMode, ElementStruct, Errors, Name};

fn write_namespaces(attr: &ElementAttr) -> Vec<TokenStream> {
    attr.namespaces
        .0
        .iter()
        .map(|(prefix, ns)| {
            quote! {
                builder.namespace(#prefix, #ns);
            }
        })
        .collect()
}

fn write_struct_attributes(struct_: &ElementStruct) -> Vec<TokenStream> {
    // Attributes, then flattens, so our direct namespaces win.
    let attrs = struct_.fields.iter().filter_map(|f| {
        if !matches!(f.mode, ElementFieldMode::Attribute { .. }) {
            return None;
        }
        let ident = &f.inner.ident;
        let name = f.name.quote_expanded();
        Some(quote_spanned! {
            ident.span() =>
            ::static_xml::ser::SerializeAttr::write(&self.#ident, &mut builder, #name)?;
        })
    });
    let flattens = struct_.fields.iter().filter_map(|f| {
        if !matches!(f.mode, ElementFieldMode::Flatten) {
            return None;
        }
        let ident = &f.inner.ident;
        Some(quote_spanned! {
            ident.span() =>
            ::static_xml::ser::Serialize::write_attributes(&self.#ident, builder)?;
        })
    });
    attrs.chain(flattens).collect()
}

fn write_struct_elements(struct_: &ElementStruct) -> Vec<TokenStream> {
    let elements = struct_.fields.iter().filter_map(|f| {
        if !matches!(f.mode, ElementFieldMode::Element { .. }) {
            return None;
        }
        let ident = &f.inner.ident;
        let name = f.name.quote_expanded();
        Some(quote_spanned! {
            ident.span() =>
            ::static_xml::ser::SerializeField::write(&self.#ident, writer, #name)?;
        })
    });
    let flattens = struct_.fields.iter().filter_map(|f| {
        if !matches!(f.mode, ElementFieldMode::Flatten) {
            return None;
        }
        let ident = &f.inner.ident;
        Some(quote_spanned! {
            ident.span() =>
            ::static_xml::ser::Serialize::write_children(&self.#ident, writer)?;
        })
    });
    elements.chain(flattens).collect()
}

fn impl_root(name: &Name) -> TokenStream {
    let ident = name.ident;
    let expanded = name.quote_expanded();
    quote! {
        impl ::static_xml::ser::SerializeRoot for #ident {
            fn root(&self) -> ::static_xml::ExpandedNameRef {
                #expanded
            }
        }
    }
}

fn do_struct(_errors: &Errors, struct_: &ElementStruct) -> TokenStream {
    let ident = &struct_.input.ident;
    let write_namespaces = write_namespaces(&struct_.attr);
    let write_attributes = write_struct_attributes(struct_);
    let write_elements = write_struct_elements(struct_);
    let impl_root = impl_root(&struct_.attr.name);
    quote! {
        impl ::static_xml::ser::Serialize for #ident {
            fn write_attributes(&self, mut builder: &mut ::static_xml::ser::ElementBuilder) -> Result<(), ::static_xml::ser::Error> {
                #(#write_namespaces)*
                #(#write_attributes)*
                Ok(())
            }

            fn write_children(&self, writer: &mut ::static_xml::ser::ElementWriter) -> Result<(), ::static_xml::ser::Error> {
                #(#write_elements)*
                Ok(())
            }
        }
        #impl_root
    }
}

fn do_enum(_errors: &Errors, enum_: &ElementEnum) -> TokenStream {
    let ident = &enum_.input.ident;
    let mut match_arms: Vec<_> = enum_.variants.iter().map(|v| {
        let vident = &v.ident;
        let name = v.name.quote_expanded();
        match v.ty {
            None => {
                quote_spanned! {
                    vident.span() =>
                    Self::#vident => writer.element(#name).start()?.finish()?
                }
            },
            Some(_) => {
                quote_spanned! {
                    vident.span() =>
                    Self::#vident(ref f) => ::static_xml::ser::SerializeField::write(f, writer, #name)?
                }
            }
        }
    }).collect();
    if enum_.has_skipped_variants {
        match_arms.push(quote! {
            _ => {}
        });
    }
    let write_namespaces = write_namespaces(&enum_.attr);
    let impl_root = impl_root(&enum_.attr.name);
    quote! {
        impl ::static_xml::ser::Serialize for #ident {
            fn write_attributes(&self, builder: &mut ::static_xml::ser::ElementBuilder) -> Result<(), ::static_xml::ser::Error> {
                #(#write_namespaces)*
                Ok(())
            }

            fn write_children(&self, writer: &mut ::static_xml::ser::ElementWriter) -> Result<(), ::static_xml::ser::Error> {
                match self {
                    #(#match_arms,)*
                }
                Ok(())
            }
        }
        #impl_root
    }
}

pub(crate) fn derive(errors: &Errors, input: syn::DeriveInput) -> Result<TokenStream, ()> {
    match input.data {
        Data::Enum(ref data) => Ok(do_enum(errors, &ElementEnum::new(&errors, &input, data))),
        Data::Struct(ref data) => {
            ElementStruct::new(&errors, &input, data).map(|s| do_struct(&errors, &s))
        }
        _ => {
            errors.push(syn::Error::new_spanned(
                input.ident,
                "unsupported data type",
            ));
            Err(())
        }
    }
}
