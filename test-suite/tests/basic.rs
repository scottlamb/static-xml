// Copyright (C) 2021 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

#![feature(const_ptr_offset_from)]

use static_xml_derive::{Deserialize, ParseText, Serialize, ToText};

#[derive(Debug, Default, /*Deserialize,*/ Eq, PartialEq, Serialize)]
#[static_xml(
    namespace = "foo: http://example.com/foo",
    namespace = "bar: http://example.com/bar",
    prefix = "foo"
)]
struct Foo {
    #[static_xml(default, attribute)]
    mybool: bool,

    #[static_xml(prefix = "bar", rename = "blah")]
    string: Vec<String>,

    //#[static_xml(flatten)]
    //bar: Bar,
    text: String,

    constrained: ConstrainedString,

    choice: MyChoice,
}
const _: () = {
    const STRUCT_VTABLE: &'static ::static_xml::value::StructVtable =
        &::static_xml::value::StructVtable {
            deserialize: None,
            elements: &[
                ::static_xml::value::NamedField {
                    name: ::static_xml::ExpandedNameRef {
                        local_name: "choice",
                        namespace: "",
                    },
                    field: ::static_xml::value::StructVtableField {
                        offset: ::static_xml::offset_of!(Foo, choice) as u32,
                        field_kind: <MyChoice as ::static_xml::value::Field>::KIND,
                        vtable: <<MyChoice as ::static_xml::value::Field>::Value as ::static_xml::value::Value>::VTABLE,
                        default: std::ptr::null(),
                    },
                },
                ::static_xml::value::NamedField {
                    name: ::static_xml::ExpandedNameRef {
                        local_name: "constrained",
                        namespace: "",
                    },
                    field: ::static_xml::value::StructVtableField {
                        offset: ::static_xml::offset_of!(Foo, constrained) as u32,
                        field_kind: <ConstrainedString as ::static_xml::value::Field>::KIND,
                        vtable: <ConstrainedString as ::static_xml::value::Value>::VTABLE,
                        default: std::ptr::null(),
                    },
                },
                ::static_xml::value::NamedField {
                    name: ::static_xml::ExpandedNameRef {
                        local_name: "text",
                        namespace: "",
                    },
                    field: ::static_xml::value::StructVtableField {
                        offset: ::static_xml::offset_of!(Foo, text) as u32,
                        field_kind: <String as ::static_xml::value::Field>::KIND,
                        vtable: <String as ::static_xml::value::Value>::VTABLE,
                        default: std::ptr::null(),
                    },
                },
                ::static_xml::value::NamedField {
                    name: ::static_xml::ExpandedNameRef {
                        local_name: "blah",
                        namespace: "http://example.com/bar",
                    },
                    field: ::static_xml::value::StructVtableField {
                        offset: ::static_xml::offset_of!(Foo, string) as u32,
                        field_kind: <Vec<String> as ::static_xml::value::Field>::KIND,
                        vtable: <<Vec<String> as ::static_xml::value::Field>::Value as ::static_xml::value::Value>::VTABLE,
                        default: std::ptr::null(),
                    },
                },
            ],
            attributes: &[::static_xml::value::NamedField {
                name: ::static_xml::ExpandedNameRef {
                    local_name: "mybool",
                    namespace: "",
                },
                field: ::static_xml::value::StructVtableField {
                    offset: ::static_xml::offset_of!(Foo, mybool) as u32,
                    field_kind: <bool as ::static_xml::value::Field>::KIND,
                    vtable: <bool as ::static_xml::value::Value>::VTABLE,
                    default: unsafe {
                        ::std::mem::transmute::<_, *const ()>(
                            <bool as ::std::default::Default>::default as fn() -> bool,
                        )
                    },
                },
            }],
            text: None,
            initialized_offset: ::static_xml::offset_of!(Scratch, initialized),
        };
    #[allow(non_snake_case)]
    struct Scratch {
        initialized: [bool; 5usize],
    }
    unsafe impl ::static_xml::de::RawDeserialize for Foo {
        type Scratch = Scratch;
    }
    unsafe fn finalize_field(
        field: ::static_xml::de::ErasedStore<'_>,
        default_fn: *const (),
        err_fn: &dyn Fn() -> ::static_xml::de::VisitorError,
    ) -> Result<(), ::static_xml::de::VisitorError> {
        field.into_store::<Foo>().finalize(default_fn, err_fn)
    }
    unsafe impl ::static_xml::value::Value for Foo {
        const VTABLE: &'static ::static_xml::value::ValueVtable = &::static_xml::value::ValueVtable {
            type_name: "basic::Foo",
            de: Some(::static_xml::de::Vtable {
                kind: ::static_xml::de::ValueKind::StructVisitor(STRUCT_VTABLE),
                finalize_field,
            }),
        };
    }
};

#[derive(Debug, Eq, PartialEq, ParseText, ToText)]
#[static_xml(whitespace = "collapse")]
enum ConstrainedString {
    Foo,
    Bar,

    #[static_xml(rename = "BAZ")]
    Baz,
}
impl Default for ConstrainedString {
    fn default() -> Self {
        ConstrainedString::Foo
    }
}

#[derive(Debug, ParseText, Eq, PartialEq, ToText)]
#[static_xml(whitespace = "collapse")]
enum UnconstrainedString {
    #[static_xml(rename = "foo")]
    Foo,
    #[static_xml(unknown)]
    Other(String),
}

#[derive(Debug, Eq, PartialEq, Deserialize, Serialize)]
enum MyChoice {
    #[static_xml(rename = "foo")]
    Foo(String),
    #[static_xml(rename = "bar")]
    Bar(String),
    #[static_xml(rename = "baz")]
    Baz(String),
    UnitValue,
}
impl Default for MyChoice {
    fn default() -> Self {
        MyChoice::UnitValue
    }
}

#[derive(Debug, Eq, PartialEq, ParseText, ToText)]
#[static_xml(mode = "union")]
enum MyUnion {
    Int(i32),
    Bool(bool),
    String(String),
}

#[derive(Debug, Default, Deserialize, Eq, PartialEq, Serialize)]
struct Bar {
    more: String,
}

#[test]
fn deserialize() {
    let _ = env_logger::builder().is_test(true).try_init();
    let foo: Foo = static_xml::de::read(
        &br#"
        <?xml version="1.0"?>
        <foo xmlns:b="http://example.com/bar" mybool="true">
            <b:blah>foo</b:blah>
            <b:blah>bar</b:blah>
            <more>more</more>
            <text>asdf</text>
            <constrained>Foo</constrained>
            <choice><foo>blah</foo></choice>
        </foo>
        "#[..],
    )
    .unwrap();
    assert_eq!(
        foo,
        Foo {
            mybool: true,
            string: vec!["foo".to_owned(), "bar".to_owned()],
            //bar: Bar {
            //    more: "more".to_owned()
            //},
            text: "asdf".to_owned(),
            constrained: ConstrainedString::Foo,
            choice: MyChoice::Foo("blah".to_owned()),
        }
    );
}

#[test]
fn round_trip() {
    let _ = env_logger::builder().is_test(true).try_init();
    let original = Foo {
        mybool: true,
        string: vec!["foo".to_owned(), "bar".to_owned()],
        //bar: Bar {
        //    more: "more".to_owned(),
        //},
        text: "asdf".to_owned(),
        constrained: ConstrainedString::Foo,
        choice: MyChoice::Foo("blah".to_owned()),
    };
    let serialized = static_xml::ser::serialize(&original).to_string().unwrap();
    log::info!("serialized: {:?}", serialized);
    let round_tripped: Foo = static_xml::de::read(serialized.as_bytes()).unwrap();
    assert_eq!(original, round_tripped);
}

#[test]
fn parse_str_variants() {
    let _ = env_logger::Builder::new().is_test(true).try_init();
    use static_xml::de::ParseText;
    assert_eq!(
        ConstrainedString::parse("Foo".to_owned()).unwrap(),
        ConstrainedString::Foo
    );
    assert_eq!(
        ConstrainedString::parse("Bar".to_owned()).unwrap(),
        ConstrainedString::Bar
    );
    assert_eq!(
        ConstrainedString::parse("BAZ".to_owned()).unwrap(),
        ConstrainedString::Baz
    );
    assert_eq!(
        ConstrainedString::parse("\n  BAZ  ".to_owned()).unwrap(),
        ConstrainedString::Baz
    );
    ConstrainedString::parse("asdf".to_owned()).unwrap_err();
    ConstrainedString::parse("baz".to_owned()).unwrap_err();

    assert_eq!(
        UnconstrainedString::parse("foo".to_owned()).unwrap(),
        UnconstrainedString::Foo
    );
    assert_eq!(
        UnconstrainedString::parse("bar".to_owned()).unwrap(),
        UnconstrainedString::Other("bar".to_owned())
    );
}
