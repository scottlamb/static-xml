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

fn flattened_field_definitions(struct_: &ElementStruct) -> Vec<TokenStream> {
    struct_
        .fields
        .iter()
        .filter_map(|field| {
            if !matches!(field.mode, ElementFieldMode::Flatten) {
                return None;
            }
            let span = field.inner.span();
            let field_scratch = format_ident!("{}_scratch", field.ident);
            let ty = &field.inner.ty;
            Some(quote_spanned! {span=>
                #field_scratch: <#ty as ::static_xml::de::RawDeserialize>::Scratch
            })
        })
        .collect()
}

fn vtable_field(struct_: &ElementStruct, field: &ElementField) -> TokenStream {
    let ident = &struct_.input.ident;
    let span = field.inner.span();
    let ty = &field.inner.ty;
    let fident = field.ident;
    let default = match field.default {
        true => quote_spanned! {span=>
            unsafe {
                ::std::mem::transmute::<_, *const ()>(
                    <#ty as ::std::default::Default>::default as fn() -> #ty,
                )
            }
        },
        false => quote! { std::ptr::null() },
    };
    quote_spanned! {span=>
        ::static_xml::value::StructVtableField {
            // SAFETY: an assertion checks the struct's size doesn't exceed u32::MAX.
            offset: unsafe { ::static_xml::offset_of!(#ident, #fident) } as u32,
            field_kind: <#ty as ::static_xml::value::Field>::KIND,
            vtable: <<#ty as ::static_xml::value::Field>::Value as ::static_xml::value::Value>::VTABLE,
            default: #default,
        }
    }
}

fn named_fields(struct_: &ElementStruct, sorted: &[usize]) -> Vec<TokenStream> {
    sorted
        .iter()
        .map(|&p| {
            let field = &struct_.fields[p];
            let name = field.name.quote_expanded();
            let vtable_field = vtable_field(struct_, field);
            quote! {
                ::static_xml::value::NamedField {
                    name: #name,
                    field: #vtable_field,
                }
            }
        })
        .collect()
}

fn do_struct(struct_: &ElementStruct) -> TokenStream {
    let attributes = named_fields(&struct_, &struct_.sorted_attributes[..]);
    let elements = named_fields(&struct_, &struct_.sorted_elements[..]);

    let ident = &struct_.input.ident;
    let struct_vtable = format_ident!("STRUCT_VTABLE_FOR_{}", ident);

    let _flattened_field_definitions = flattened_field_definitions(&struct_);
    let _flatten_visitors = quote_flatten_visitors(&struct_);
    let n_fields = elements.len() + attributes.len(); // TODO: text.
    quote! {
        const _: () = assert!(std::mem::size_of::<#ident>() < u32::MAX as usize);

        static VTABLE: &'static ::static_xml::value::StructVtable = &::static_xml::value::StructVtable {
            deserialize: None, // TODO
            elements: &[#(#elements,)*],
            attributes: &[#(#attributes,)*],
            text: None, // TODO
            // TODO: flattened.
            initialized_offset: unsafe { ::static_xml::offset_of!(Scratch, initialized) },
        };
        fn #struct_vtable() -> &'static ::static_xml::value::StructVtable {
            VTABLE
        }

        // If there's an underlying field named e.g. `foo_`, then there will be
        // a generated field name e.g. `foo__required` or `foo__visitor`. Don't
        // complain about this.
        #[allow(non_snake_case)]
        pub struct Scratch {
            // text_buf: String, // TODO: can omit if there's no text field.
            initialized: [bool; #n_fields],
            // #(#flattened_field_definitions, )*
        }

        unsafe impl ::static_xml::de::RawDeserialize for #ident {
            type Scratch = Scratch;
        }
        ::static_xml::impl_deserialize_via_raw!(#ident);

        unsafe fn finalize_field(
            field: ::static_xml::de::ErasedStore<'_>,
            default_fn: *const (),
            err_fn: &dyn Fn() -> ::static_xml::de::VisitorError,
        ) -> Result<(), ::static_xml::de::VisitorError> {
            field.into_store::<#ident>().finalize(default_fn, err_fn)
        }

        unsafe impl ::static_xml::value::Value for #ident {
            const VTABLE: &'static ::static_xml::value::ValueVtable = &::static_xml::value::ValueVtable {
                type_name: concat!(module_path!(), "::", stringify!(#ident)),
                de: Some(::static_xml::de::Vtable {
                    kind: ::static_xml::de::ValueKind::StructVisitor(#struct_vtable as fn() -> &'static ::static_xml::value::StructVtable),
                    finalize_field,
                }),
            };
        }
    }
}

fn do_enum(enum_: &ElementEnum) -> TokenStream {
    let ident = &enum_.input.ident;
    let impl_type = format_ident!("{}VisitorImpl", &enum_.input.ident);
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
                Some(ty) => {
                    quote_spanned! {span=>
                        #ident::#vident(f) if element_i == Some(#i) => {
                            // This is a bit silly when we already know it's
                            // initialized, but it lets us reuse code.
                            let mut initialized = true;
                            ::static_xml::de::ElementField::element(
                                ::std::mem::transmute::<&mut #ty, &mut ::std::mem::MaybeUninit<#ty>>(f),
                                &mut initialized,
                                child,
                            )?;
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
                Some(ty) => {
                    quote_spanned! {
                        vident.span() => Some(#i) => {
                            #ident::#vident({
                                let mut value = ::std::mem::MaybeUninit::<#ty>::uninit();
                                let mut initialized = false;
                                ::static_xml::de::ElementField::element(
                                    &mut value,
                                    &mut initialized,
                                    child
                                )?;
                                assert!(initialized);
                                value.assume_init()
                            })
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

        #vis struct #impl_type {}

        unsafe impl ::static_xml::de::RawDeserializeImpl for #impl_type {
            type Out = #ident;
            type Scratch = bool; // if Out is initialized

            fn init_scratch(&self, scratch: &mut ::std::mem::MaybeUninit<Self::Scratch>) {
                scratch.write(false);
            }

            unsafe fn element<'a>(
                &self,
                out: &mut ::std::mem::MaybeUninit<Self::Out>,
                scratch: &mut Self::Scratch,
                child: ::static_xml::de::ElementReader<'a>,
            ) -> Result<Option<::static_xml::de::ElementReader<'a>>, ::static_xml::de::VisitorError> {
                let name = child.expanded_name();
                let element_i = ::static_xml::de::find(&name, ELEMENTS);
                if *scratch {
                    // SAFETY: self.out is initialized when scratch is true.
                    let expected_i = match out.assume_init_mut() {
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
                out.write(match element_i {
                    #(#uninitialized_match_arms,)*
                    _ => return Ok(Some(child)),
                });
                *scratch = true;
                Ok(None)
            }

            unsafe fn finalize(
                &self,
                out: &mut ::std::mem::MaybeUninit<Self::Out>,
                scratch: &mut Self::Scratch,
                // TODO: default?
            ) -> Result<(), ::static_xml::de::VisitorError> {
                if !*scratch {
                    //if let Some(d) = default {
                    //    self.out.write(d());
                    //} else {
                        return Err(static_xml::de::VisitorError::cant_be_empty(stringify!(#ident)));
                    //}
                }
                // SAFETY: returning `Ok` guarantees `out` is fully initialized.
                Ok(())
            }
        }

        ::static_xml::custom_deserialize_vtable!(#ident, &#impl_type{});

        unsafe impl ::static_xml::de::RawDeserialize for #ident {
            type Scratch = bool;
        }

        ::static_xml::impl_deserialize_via_raw!(#ident);
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
