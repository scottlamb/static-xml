use crate::{de, ExpandedNameRef};

/// Type-erased handling of serializable/deserializable values.

/// A value type with a registered vtable.
///
/// The vtable allows type-erased handling of these values, including
/// serialization and/or deserialization.
pub unsafe trait Value: 'static {
    fn vtable() -> &'static ValueVtable;
}

#[derive(Debug)]
pub struct ValueVtable {
    /// A name for this type, which is not guaranteed to be unique.
    ///
    /// See [`std::any::type_name`].
    pub type_name: &'static str,

    pub de: Option<de::Vtable>,
    // TODO: ser.
}

pub unsafe trait Field: Sized {
    type Value: Value;
    const KIND: FieldKind;
}
unsafe impl<T: Value> Field for T {
    type Value = T;
    const KIND: FieldKind = FieldKind::Direct;
}
unsafe impl<T: Value> Field for Option<T> {
    type Value = T;
    const KIND: FieldKind = FieldKind::Option;
}
unsafe impl<T: Value> Field for Vec<T> {
    type Value = T;
    const KIND: FieldKind = FieldKind::Vec;
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
#[repr(u8)]
pub enum FieldKind {
    /// `T`
    Direct,

    /// `Option<T>`
    Option,

    /// `Vec<T>`
    Vec,
}

#[doc(hidden)]
pub struct NamedField {
    pub name: ExpandedNameRef<'static>,
    pub field: StructVtableField,
}

/// Describes field metadata needed for (de)serialization of a `struct`.
///
/// This includes the offsets within the `struct` of the field, its type,
/// attribute/element names, etc.
///
/// It also includes the layout of the "scratch" space used during
/// deserialization. (Booleans describing which fields have been initialized,
/// a `String` buffer for accumulating text nodes, and any scratch space used
/// by flattened field.)
// TODO: restructure:
// * list fields in their original order, as well as the sorted
//   element/attribute order. This will allow serializing in the chosen order,
//   which is important for some schemas. It also may be helpful for
//   deserializing several enum variants into a single field. See
//   [#4](https://github.com/scottlamb/static-xml/issues/4).
// * avoid repeating the two-word namespace &str on each element/attribute.
// *
#[doc(hidden)]
pub struct StructVtable {
    // TODO: finalize.
    pub elements: &'static [NamedField],
    pub attributes: &'static [NamedField],

    /// Offset within scratch and field for text, if any.
    pub text: Option<(usize, StructVtableField)>,

    pub flattened: &'static [FlattenedField],

    /// Offset within scratch of the initialized array.
    pub initialized_offset: usize,
}

impl StructVtable {
    /// Returns number of contained [`StructVtableField`]s (non-flattened fields).
    pub(crate) fn n_fields(&self) -> usize {
        self.elements.len() + self.attributes.len() + self.text.iter().len()
    }
}

pub struct FlattenedField {
    pub out_offset: u32,
    pub scratch_offset: u32,
    pub vtable: fn() -> &'static ValueVtable,
}

pub struct StructVtableField {
    pub offset: u32,
    pub field_kind: FieldKind,
    pub vtable: fn() -> &'static ValueVtable,
    pub default: *const (),
}
unsafe impl Send for StructVtableField {}
unsafe impl Sync for StructVtableField {}
