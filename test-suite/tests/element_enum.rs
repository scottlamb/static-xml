// Copyright (C) 2021 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

use static_xml_derive::{Deserialize, Serialize};

#[derive(Debug, Deserialize, Serialize, PartialEq, Eq)]
struct Holder {
    #[static_xml(flatten)]
    enum_: Enum,
    //#[static_xml(flatten)]
    //other_flatten: OtherFlatten,
}

#[derive(Debug, Deserialize, Serialize, PartialEq, Eq)]
enum Enum {
    Simple(String),
    Vec(Vec<String>),
    Unit,
}

#[derive(Debug, Deserialize, Serialize, PartialEq, Eq)]
struct OtherIndirectFlatten {
    field: Vec<String>,
}

#[test]
fn deserialize_simple() {
    let _ = env_logger::Builder::new().is_test(true).try_init();
    let holder: Holder = static_xml::de::from_str(
        r#"
        <?xml version="1.0"?>
        <foo>
            <field>before</field>
            <Simple>asdf</Simple>
            <field>after</field>
        </foo>
        "#,
    )
    .unwrap();
    assert_eq!(
        holder,
        Holder {
            enum_: Enum::Simple("asdf".to_owned()),
            /*other_flatten: OtherIndirectFlatten {
                field: vec!["before".to_owned(), "after".to_owned()]
            },*/
        }
    );
}

#[test]
fn deserialize_vec() {
    let _ = env_logger::Builder::new().is_test(true).try_init();
    let holder: Holder = static_xml::de::from_str(
        r#"
        <?xml version="1.0"?>
        <foo>
            <field>before</field>
            <Vec>foo</Vec>
            <Vec>bar</Vec>
            <field>after</field>
        </foo>
        "#,
    )
    .unwrap();
    assert_eq!(
        holder,
        Holder {
            enum_: Enum::Vec(vec!["foo".to_owned(), "bar".to_owned()]),
            /*other_flatten: OtherIndirectFlatten {
                field: vec!["before".to_owned(), "after".to_owned()]
            },*/
        }
    );
}

#[test]
fn deserialize_mix_error() {
    let _ = env_logger::Builder::new().is_test(true).try_init();
    let e = static_xml::de::from_str::<Holder>(
        r#"
        <?xml version="1.0"?>
        <foo>
            <Vec>foo</Vec>
            <Simple>bar</Simple>
        </foo>
        "#,
    )
    .unwrap_err();
    let e_str = e.to_string();
    assert!(
        e_str.starts_with("Got unexpected element Simple after Vec"),
        "{}",
        e
    );
    let stack_elements: Vec<_> = e
        .stack()
        .iter()
        .map(|e| e.name.local_name.as_str())
        .collect();
    assert_eq!(&stack_elements[..], &["foo", "Simple"]);
}
