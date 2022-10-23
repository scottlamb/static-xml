// Copyright (C) 2022 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

use static_xml_derive::Deserialize;

struct Foo;

#[derive(Deserialize)]
struct Outer {
    foo: Foo,
}

fn main() {}
