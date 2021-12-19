// Copyright (C) 2021 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

/// Stores values in lazily-initialized, type-erased fields.
/// 
/// Currently, stores a Rust type `V` into a field of type `V`, `Option<V>`, or
/// `Vec<V>`. Possible extension: store a Rust enum variant `E::V` into a field
/// of type `E`, `Option<E>`, or `Vec<E>`.
///
/// Fields are lazily initialized so that's it's possible to deserialize
/// required fields without a `Default` impl (particularly enum values).
/// 
/// This is intended for reducing code size of specific deserialization
/// operations:
/// 
/// *   With custom deserializers, storing a value with type known at
///     compile-time into a field of type known at runtime.
/// *   With table-driven struct deserializers, storing a value with type known
///     at runtime into a field of type known at runtime.
/// *   "Finalizing" a field: when uninitialized, store a default value if
///     specified or a following field kind-specific behavior otherwise (`Err`
///     for `FieldKind::T`, `None` for `FieldKind::Option`, empty for
///     `FieldKind::Vec`).
/// 
/// Erasure of field kinds is done by expanding all three possibilities in a
/// `match` clause. It is not easily extensible by callers.
/// 
/// Erasure of value types is done by a macro that defines a vtable trait.

use std::{mem::MaybeUninit, marker::PhantomData};

use crate::value::{Value, ValueVtable, Field, FieldKind};

use super::VisitorError;

/// Stores values of type `V` into a lazily initialized field.
/// 
/// Knows the specific [`Field`] type at runtime and the value type at compile
/// time.
pub struct Store<'a, V: Value> {
    ptr: *mut (),
    _phantom: PhantomData<&'a mut V>,
    initialized: &'a mut bool,
    field_kind: FieldKind,
}

impl<'a, V: Value> Store<'a, V> {
    /// Wraps a store.
    /// 
    /// SAFETY: the caller guarantees `initialized` is accurate.
    #[inline]
    pub unsafe fn wrap<F>(f: &'a mut MaybeUninit<F>, initialized: &'a mut bool,) -> Self
    where F: Field<Value = V> {
        Self {
            ptr: f.as_mut_ptr() as *mut (),
            _phantom: PhantomData,
            initialized,
            field_kind: F::KIND,
        }
    }

    /// Finalizes the store, guaranteeing its initialization or returning `Err`.
    /// 
    /// SAFETY: `default_fn` must be `std::ptr::null()` (for `None`) or
    /// transmuted from a function of type `fn() -> F`. That is, it returns
    /// the *field* type, not the *value* type.
    pub unsafe fn finalize(
        self,
        default_fn: *const (),
        err_fn: &dyn Fn() -> VisitorError,
    ) -> Result<(), VisitorError> {
        if *self.initialized {
            return Ok(());
        }
        if !default_fn.is_null() {
            match self.field_kind {
                FieldKind::Direct => {
                    std::ptr::write(
                        self.ptr as *mut V,
                        std::mem::transmute::<*const (), fn() -> V>(default_fn)(),
                    );
                }
                FieldKind::Option => {
                    std::ptr::write(
                        self.ptr as *mut Option<V>,
                        std::mem::transmute::<*const (), fn() -> Option<V>>(default_fn)(),
                    );
                }
                FieldKind::Vec => {
                    std::ptr::write(
                        self.ptr as *mut Vec<V>,
                        std::mem::transmute::<*const (), fn() -> Vec<V>>(default_fn)(),
                    );
                }
            }
        } else if matches!(self.field_kind, FieldKind::Vec) {
            std::ptr::write(self.ptr as *mut Vec<V>, Vec::new());
        } else {
            return Err(err_fn());
        }
        *self.initialized = true;
        Ok(())
    }

    /// Pushes `val` into the store, panicking if it is full.
    /// 
    /// The caller should call `is_full` to check for this condition *before*
    /// producing a value.
    pub fn push(self, val: V) {
        unsafe {
            match (self.field_kind, *self.initialized) {
                (FieldKind::Direct | FieldKind::Option, true) => unreachable!(),
                (FieldKind::Direct, false) => std::ptr::write(self.ptr as *mut V, val),
                (FieldKind::Option, false) => {
                    std::ptr::write(
                        self.ptr as *mut Option<V>,
                        Some(val),
                    );
                },
                (FieldKind::Vec, false) => {
                    let vec =
                        (&mut *(self.ptr as *mut std::mem::MaybeUninit<Vec<V>>)).write(Vec::new());
                    vec.push(val);
                }
                (FieldKind::Vec, true) => {
                    let vec = &mut *(self.ptr as *mut Vec<V>);
                    vec.push(val);
                }
            }
            *self.initialized = true;
        }
    }

    #[inline]
    pub fn into_erased(self) -> ErasedStore<'a> {
        ErasedStore {
            value_type: V::VTABLE,
            field_kind: self.field_kind,
            ptr: self.ptr,
            initialized: self.initialized,
        }
    }
}

#[cold]
#[track_caller] // TODO: check track_caller's effect on code size.
fn unerase_mismatch(erased_vtable: &'static ValueVtable, store_vtable: &'static ValueVtable) -> ! {
    panic!(
        "Can't convert EraseStore with value type {:?} into Store<{:?}>",
        erased_vtable.type_name,
        store_vtable.type_name,
    )
}

/// Stores values of a type known only at runtime.
/// 
/// This knows the specific [`Field`] and [`Value`] type at runtime.
#[doc(hidden)]
pub struct ErasedStore<'a> {
    value_type: &'static ValueVtable,
    field_kind: FieldKind,
    ptr: *mut (),
    initialized: &'a mut bool,
}
impl<'a> ErasedStore<'a> {
    /// Initializes with caller-supplied values.
    /// 
    /// SAFETY: the caller guarantees accuracy of all arguments.
    #[inline]
    pub unsafe fn new(
        value_type: &'static ValueVtable,
        ptr: &'a mut (),
        field_kind: FieldKind,
        initialized: &'a mut bool,
    ) -> Self {
        Self {
            value_type,
            field_kind,
            ptr: ptr as *mut (),
            initialized,
        }
    }

    #[inline]
    pub fn is_full(&self) -> bool {
        *self.initialized && matches!(self.field_kind, FieldKind::Direct | FieldKind::Option)
    }

    /// Converts to the given store type, or panics on type mismatch.
    #[track_caller] // TODO: check track_caller's effect on code size.
    pub fn into_store<V: Value>(self) -> Store<'a, V> {
        if !std::ptr::eq(self.value_type, V::VTABLE) {
            unerase_mismatch(self.value_type, V::VTABLE);
        }
        Store {
            ptr: self.ptr,
            _phantom: PhantomData,
            initialized: self.initialized,
            field_kind: self.field_kind,
        }
    }
}

// TODO: tests!
