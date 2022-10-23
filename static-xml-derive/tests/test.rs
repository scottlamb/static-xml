// Copyright (C) 2022 Scott Lamb <slamb@slamb.org>
// SPDX-License-Identifier: MIT OR Apache-2.0

#[test]
fn test() {
    let t = trybuild::TestCases::new();
    t.compile_fail("tests/ui/field-not-parsetext.rs");
}
