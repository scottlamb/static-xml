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
            let field_visitor = format_ident!("{}_visitor", field.ident);
            let ty = &field.inner.ty;
            Some(quote_spanned! {span=>
                #field_visitor: <#ty as ::static_xml::de::RawDeserialize<'out>>::Visitor
            })
        })
        .collect()
}

fn flattened_field_initializers(struct_: &ElementStruct) -> Vec<TokenStream> {
    struct_
        .fields
        .iter()
        .filter_map(|field| {
            if !matches!(field.mode, ElementFieldMode::Flatten) {
                return None;
            }
            let span = field.inner.span();
            let field_visitor = format_ident!("{}_visitor", field.ident);
            let fident = field.ident;
            let ty = &field.inner.ty;
            Some(quote_spanned! {span=>
                #field_visitor: <#ty as ::static_xml::de::RawDeserialize>::Visitor::new(
                    // SAFETY: out points to valid, uninitialized memory.
                    unsafe {
                        &mut *(
                            ::std::ptr::addr_of_mut!((*out.as_mut_ptr()).#fident)
                            as *mut ::std::mem::MaybeUninit<#ty>
                        )
                    }
                )
            })
        })
        .collect()
}

fn finalize_flattened_fields(struct_: &ElementStruct) -> Vec<TokenStream> {
    struct_.fields.iter().filter_map(|field| {
        if !matches!(field.mode, ElementFieldMode::Flatten) {
            return None;
        }
        let span = field.inner.span();
        let ty = &field.inner.ty;
        let default = match field.default {
            true => quote_spanned! {span=> Some(<#ty as ::std::default::Default>::default) },
            false => quote! { None }
        };
        let field_visitor = format_ident!("{}_visitor", field.ident);
        Some(quote_spanned! {span=>
            <#ty as ::static_xml::de::RawDeserialize>::Visitor::finalize(self.#field_visitor, #default)?;
        })
    }).collect()
}

fn vtable_field(struct_: &ElementStruct, field: &ElementField, trait_: TokenStream) -> TokenStream {
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
        false => quote! { std::ptr::null() }
    };
    quote_spanned! {span=>
        ::static_xml::de::ElementVtableField {
            // SAFETY: an assertion checks the struct's size doesn't exceed u32::MAX.
            offset: unsafe { ::static_xml::offset_of!(#ident, #fident) } as u32,
            field_type: <#ty as ::static_xml::de::#trait_>::TYPE,
            vtable: <#ty as ::static_xml::de::#trait_>::VTABLE,
            default: #default,
        }
    }
}

fn attributes(struct_: &ElementStruct) -> Vec<TokenStream> {
    struct_.sorted_attributes.iter().map(|&p| {
        let field = &struct_.fields[p];
        let name = field.name.quote_expanded();
        let vtable_field = vtable_field(struct_, field, quote! { AttrField });
        quote! {
            ::static_xml::de::NamedField {
                name: #name,
                field: #vtable_field,
            }
        }
    }).collect()
}

fn elements(struct_: &ElementStruct) -> Vec<TokenStream> {
    struct_.sorted_elements.iter().map(|&p| {
        let field = &struct_.fields[p];
        let name = field.name.quote_expanded();
        let vtable_field = vtable_field(struct_, field, quote! { ElementField });
        quote! {
            ::static_xml::de::NamedField {
                name: #name,
                field: #vtable_field,
            }
        }
    }).collect()
}

fn do_struct(struct_: &ElementStruct) -> TokenStream {
    let attributes = attributes(&struct_);
    let elements = elements(&struct_);

    let ident = &struct_.input.ident;
    let visitor_type = format_ident!("{}Visitor", &struct_.input.ident);
    let flattened_field_definitions = flattened_field_definitions(&struct_);
    let flattened_field_initializers = flattened_field_initializers(&struct_);
    let finalize_flattened_fields = finalize_flattened_fields(&struct_);
    let flatten_visitors = quote_flatten_visitors(&struct_);
    let n_fields = elements.len() + attributes.len();  // TODO: text.
    quote! {
        const _: () = assert!(std::mem::size_of::<#visitor_type>() < u32::MAX as usize);
        const VTABLE: ::static_xml::de::ElementVtable = ::static_xml::de::ElementVtable {
            elements: &[#(#elements,)*],
            attributes: &[#(#attributes,)*],
            text: None, // TODO
        };

        // If there's an underlying field named e.g. `foo_`, then there will be
        // a generated field name e.g. `foo__required` or `foo__visitor`. Don't
        // complain about this.
        #[allow(non_snake_case)]
        struct #visitor_type<'out> {
            // This can't be &mut MaybeUninit<#type> because flattened fields
            // (if any) get a &mut MaybeUninit<#type> that aliases it. So
            // use a raw pointer. u8 is best for ptr::add(offset_of!(...)).
            out: *mut u8,
            _phantom: ::std::marker::PhantomData<&'out mut #ident>,
            text_buf: String, // TODO: can omit if there's no text field.
            initialized: [bool; #n_fields],
            #(#flattened_field_definitions, )*
        }

        impl<'out> #visitor_type<'out> {
            fn vtable_visitor<'a>(&'a mut self) -> ::static_xml::de::VtableVisitor<'a> {
                unsafe {
                    ::static_xml::de::VtableVisitor::new(
                        self.out,
                        &VTABLE,
                        &mut self.initialized[..],
                        &mut self.text_buf,
                        //&mut [#(#flatten_visitors, )*],
                    )
                }
            }
        }

        impl<'out> ::static_xml::de::ElementVisitor for #visitor_type<'out> {
            fn element<'a>(
                &mut self,
                child: ::static_xml::de::ElementReader<'a>,
            ) -> Result<Option<::static_xml::de::ElementReader<'a>>, ::static_xml::de::VisitorError> {
                ::static_xml::de::ElementVisitor::element(&mut self.vtable_visitor(), child)
            }

            fn attribute<'a>(
                &mut self,
                name: &::static_xml::ExpandedNameRef,
                value: String,
            ) -> Result<Option<String>, ::static_xml::de::VisitorError> {
                ::static_xml::de::ElementVisitor::attribute(&mut self.vtable_visitor(), name, value)
            }

            // TODO: characters.
        }

        unsafe impl<'out> ::static_xml::de::RawDeserializeVisitor<'out> for #visitor_type<'out> {
            type Out = #ident;

            fn new(out: &'out mut ::std::mem::MaybeUninit<Self::Out>) -> Self {
                Self {
                    out: out.as_mut_ptr() as *mut u8,
                    initialized: [false; #n_fields],
                    text_buf: String::new(),
                    _phantom: ::std::marker::PhantomData,
                    #(#flattened_field_initializers, )*
                }
            }

            #[warn(unused_mut)] // mut is needed iff there are flattened fields.
            fn finalize(mut self, _default: Option<fn() -> Self::Out>) -> Result<(), ::static_xml::de::VisitorError> {
                // Note _default is currently unsupported for structs.

                #(#finalize_flattened_fields)*
                // SAFETY: returning `Ok` guarantees `self.out` is fully initialized.
                // finalize_visitor_fields has guaranteed that each field is fully initialized.
                // The padding doesn't matter. This is similar to this example
                // https://doc.rust-lang.org/stable/std/mem/union.MaybeUninit.html#initializing-a-struct-field-by-field
                Ok(())
            }
        }

        ::static_xml::impl_deserialize_via_raw!(#ident, #visitor_type);
        ::static_xml::deserialize_vtable!(#ident);
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
                Some(ty) => {
                    quote_spanned! {span=>
                        #ident::#vident(f) if element_i == Some(#i) => unsafe {
                            // This is a bit silly when we already know it's
                            // initialized, but it lets us reuse code.
                            let mut initialized = true;
                            ::static_xml::de::ElementField::element(
                                std::mem::transmute::<&mut #ty, &mut std::mem::MaybeUninit<#ty>>(f),
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
                            #ident::#vident(unsafe {
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
        ::static_xml::deserialize_vtable!(#ident);

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
