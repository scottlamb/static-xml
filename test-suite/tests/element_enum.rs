// Copyright (C) 2021 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

use static_xml_derive::{Deserialize, Serialize};

#[derive(Default, Debug, Deserialize, Serialize, PartialEq, Eq)]
#[static_xml(direct)]
struct DirectHolder {
    #[static_xml(flatten)]
    direct_enum: DirectEnum,
    #[static_xml(flatten)]
    other_flatten: OtherDirectFlatten,
}

#[derive(Debug, Deserialize, Serialize, PartialEq, Eq)]
#[static_xml(direct)]
enum DirectEnum {
    Simple(String),
    Vec(Vec<String>),
    Unit,
    #[static_xml(skip)]
    Skipped(String),
}
impl Default for DirectEnum {
    fn default() -> Self {
        DirectEnum::Skipped("default".to_owned())
    }
}

#[derive(Debug, Default, Deserialize, Serialize, PartialEq, Eq)]
#[static_xml(direct)]
struct OtherDirectFlatten {
    field: Vec<String>,
}

#[derive(Debug, Deserialize, Serialize, PartialEq, Eq)]
struct IndirectHolder {
    #[static_xml(flatten)]
    indirect_enum: IndirectEnum,
    #[static_xml(flatten)]
    other_flatten: OtherIndirectFlatten,
}

#[derive(Debug, Deserialize, Serialize, PartialEq, Eq)]
enum IndirectEnum {
    Simple(String),
    Vec(Vec<String>),
    Unit,
    #[allow(dead_code)]
    #[static_xml(skip)]
    Skipped,
}

#[derive(Debug, Deserialize, Serialize, PartialEq, Eq)]
struct OtherIndirectFlatten {
    field: Vec<String>,
}

#[test]
fn deserialize_indirect_simple() {
    let _ = env_logger::Builder::new().is_test(true).try_init();
    let indirect: IndirectHolder = static_xml::de::from_str(
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
        indirect,
        IndirectHolder {
            indirect_enum: IndirectEnum::Simple("asdf".to_owned()),
            other_flatten: OtherIndirectFlatten {
                field: vec!["before".to_owned(), "after".to_owned()]
            },
        }
    );
}

#[test]
fn deserialize_indirect_vec() {
    let _ = env_logger::Builder::new().is_test(true).try_init();
    let indirect: IndirectHolder = static_xml::de::from_str(
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
        indirect,
        IndirectHolder {
            indirect_enum: IndirectEnum::Vec(vec!["foo".to_owned(), "bar".to_owned()]),
            other_flatten: OtherIndirectFlatten {
                field: vec!["before".to_owned(), "after".to_owned()]
            },
        }
    );
}

#[test]
fn deserialize_indirect_mix_error() {
    let _ = env_logger::Builder::new().is_test(true).try_init();
    let e = static_xml::de::from_str::<IndirectHolder>(
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
