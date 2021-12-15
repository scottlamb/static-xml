// Copyright (C) 2021 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

use static_xml_derive::{Deserialize, ParseText, Serialize, ToText};

#[derive(Debug, Deserialize, Eq, PartialEq, Serialize)]
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

    #[static_xml(flatten)]
    bar: Bar,

    text: String,

    constrained: ConstrainedString,

    choice: MyChoice,
}

#[derive(Debug, Eq, PartialEq, ParseText, ToText)]
#[static_xml(whitespace = "collapse")]
enum ConstrainedString {
    Foo,
    Bar,

    #[static_xml(rename = "BAZ")]
    Baz,
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

#[derive(Debug, Eq, PartialEq, ParseText, ToText)]
#[static_xml(mode = "union")]
enum MyUnion {
    Int(i32),
    Bool(bool),
    String(String),
}

#[derive(Debug, Deserialize, Eq, PartialEq, Serialize)]
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
            bar: Bar {
                more: "more".to_owned()
            },
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
        bar: Bar {
            more: "more".to_owned(),
        },
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
