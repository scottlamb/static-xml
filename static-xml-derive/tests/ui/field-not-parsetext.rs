use static_xml_derive::Deserialize;

struct Foo;

#[derive(Deserialize)]
struct Outer {
    foo: Foo,
}

fn main() {}
