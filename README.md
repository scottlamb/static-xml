# static-xml

`static-xml` is a [`serde`](https://serde.rs/)-like serialization and
deserialization library for XML, currently written as a layer on top of
[`xml-rs`](https://crates.io/crates/xml-rs). It is *inspired by* `serde`
but is an entirely separate implementation rather than a `serde` data format.

*Status:* in early development. API is unstable.

## Goals

In order, roughly:

1.  **sound.** `unsafe` is allowed, but never in a way that allows safe calling
    code to violate soundness. It should be carefully reasoned and thoroughly
    tested using tools such as `cargo miri test`.
2.  **correct.** It should have a bug-free mapping of Rust types and XML
    documents with no surprises. The APIs should be misuse-resistant.
3.  **ergonomic.** The derive macros should be easy to use correctly, and they
    should produce good error messages when used incorrectly.
4.  **complete enough.** The `static-xml` crate should support any useful
    format; the `static-xml-derive` macros should support most types.
    See design notes below.
5.  **light-weight.** It should produce reasonably small binaries and compile
    quickly. This is a major concern for large XML schemas such as ONVIF.
6.  **fast enough.** It should be as fast as possible without compromising
    the goals above. The lowest-hanging fruit for improving performance are
    likely in `xml-rs`, not in `static-xml` itself.

## Design notes

This library is divided into two crates:

1.  `static-xml` has the main logic for driving serialization and
    deserialization, the basic traits, and a few helpers for
    `Serialize`/`Deserialize` impls to use to reduce code size.

    `static-xml` is meant to support any useful format. Its current
    implementation has some limitations. E.g. it doesn't handle processing
    instructions. In theory, though, it could be extended to exactly round-trip
    any series of XML events via the underlying XML library.

2.  `static-xml-derive` has macros for automatically deriving `Serialize` and
    `Deserialize` impls on `struct` and `enum` types. It's designed to work
    with simple Rust types that can convey the semantic meaning of typical XML
    types but often will not round-trip to the exact same events or bytes. You
    may need to bypass it for some types. For example, the `struct`s it supports
    don't have any way of conveying order between their fields. To make this
    concrete, when fed an XHTML document, deserialization would lose the
    distinction between `<p>foo<i>bar</i>baz</p>` and `<p>foobaz<i>bar</i></p>`.
    Similarly, it doesn't support validating all the rules that might be
    expressed in an XML schema. It can parse a schema described via
    `<xs:sequence>`, but it won't produce an error if the elements aren't
    written in the stated order.

### `xml-rs` vs `quick-xml` or other alternatives

This library is written on top of `xml-rs`'s stream-of-events interface.
I considered other libraries like `quick-xml` but chose `xml-rs` for a couple
of reasons:

1.  It's the most widely used, and there are companion crates like `xmltree`
    available.
2.  It aims to be standards-compliant, and others don't.

`xml-rs` is not without problems. E.g. its author [wrote](https://github.com/netvl/xml-rs/issues/126#issuecomment-374107131)
that "xml-rs has been first created ages ago, long before the first stable
version of Rust was available. Therefore some details of its API are not really
up-to-date. In particular, xml-rs allocates a lot. Ideally, it should work like
quick-xml does, i.e. reading data to its internal buffer and give out references
to it."

I'm open to porting `static-xml` to another library if there's one that aims for
reasonably good standards compliance, or to a new `xml-rs` version if someone
takes on the task of freshening that crate.

`static-xml` could even support multiple underlying XML crates via feature
flags. The library makes use of `&dyn Trait` indirection internally, so
additional code bloat should be minimal. Some interface choices borrowed from
`xml-rs` would have to change to take the most advantage of an underlying
library that allocates less.

### DOM tree support

It's possible to deserialize Rust types from an in-memory DOM tree rather than
XML events (and vice versa). This was suggested in [this
comment](https://github.com/media-io/yaserde/issues/76#issuecomment-646947783).
I don't believe this simplifies the implementation much: it's beneficial anyway
to use the program stack to represent the types during deserialization.

The streaming interface is strictly more general: just as it's possible for
`static-xml` to support multiple underlying XML streaming libraries, it could
also support traversing a DOM tree.

In the other direction, I plan to add an `xmltree` feature to `static-xml` which
supplies a `Serialize` and `Deserialize` impl on `xmltree::Element`. This would
allow retaining unknown field values easily:

```rust
#[derive(Deserialize, Serialize)]
struct Foo {
    known_field: String,
    #[static_xml(flatten)]
    unknown_fields: xmltree::Element,
}
```

### Future work: table-driven `Visitor` impl.

Currently `static-xml-derive` writes explicit generated code. E.g., the
`Deserialize` impl for `Foo` above looks roughly as follows:

```rust
const ELEMENTS: &[ExpandedNameRef; 1] = &[
    ExpandedNameRef { local_name: "known_field", namespace: "" },
];

impl Deserialize for Foo {
    fn deserialize(element: ElementReader<'_>) -> Result<Self, VisitorError> {
        let mut builder = FooVisitor {
            known_field: <String as DeserializeField>::init(),
            unknown_fields: <XmlTree as DeserializeField>::init(),
        };
        element.read_to(&mut builder)?;
        Self {
            known_field: <String as DeserializeField>::finalize(builder.known_field)?,
            unknown_fields: <XmlTree as DeserializeField>::finalize(builder.unknown_fields)?,
        }
    }
}

pub struct FooVisitor {
    known_field: <String as DeserializeField>::Builder,
    unknown_fields: <xmltree::Element as DeserializeFlatten>::Builder,
}

impl ElementVisitor for FooVisitor {
    fn element<'a>(
        &mut self,
        child: ElementReader<'a>
    ) -> Result<Option<ElementReader<'a>>, VisitorError> {
        match find(&child.expanded_name(), ELEMENTS) {
            Some(0usize) => {
                ::static_xml::de::DeserializeFieldBuilder::element(&mut self.known_field, child)?;
                return Ok(None);
            }
            _ => delegate_element(&mut [&mut self.unknown_fields], child),
        }
    }
}
```

I believe this is close to the minimal size with this approach. Next I'd like
to experiment with a different approach in which the `Visitor` impl is replaced
with a table that holds the offset within `FooVisitor` of each field, and a
pointer to an `element` function. The generated code would use `unsafe`, but
soundness only has to be proved once in the generator, and this seems worthwhile
if it can achieve significant code size reduction. See
[#5](https://github.com/scottlamb/static-xml/issues/5).

## Comparison with other crates

### `static-xml` vs a `serde` data format

There are several XML serialization crates that plug into `serde` as a data
format (`serde::Deserializer` and `serde::Serializer` impls), including:

*   [`serde-xml-rs`](https://github.com/RReverser/serde-xml-rs) (most popular)
*   [`xml_serde`](https://crates.io/crates/xml_serde) (most capable)

This is an attractive idea: take advantage of `serde`'s high-quality derive
macro implementation and maybe even a few existing `#[derive(Serialize)`
annotations in popular crates.

I discarded this approach because I found it frustrating to combine [serde's
generic data model](https://serde.rs/data-model.html) and XML's complex, unique
data model. The challenge is to make it possible to use [serde
attributes](https://serde.rs/attributes.html) to describe an XML data format
easily:

*   on [`serde::de::Deserializer`](`https://docs.serde.rs/serde/de/trait.Deserializer.html`) calls,
    turn ([`xml::reader::XmlEvent`](https://docs.rs/xml-rs/0.8.4/xml/reader/enum.XmlEvent.html))s
    into [`serde::de::Visitor`](https://docs.serde.rs/serde/de/trait.Visitor.html) calls.
*   on [`serde::ser::Serializer`](https://docs.serde.rs/serde/ser/trait.Serializer.html) calls,
    produce [`xml::writer::events::XmlEvent`](https://docs.rs/xml-rs/0.8.4/xml/writer/events/enum.XmlEvent.html)s.

A few examples of the mismatch:

*   XML distinguishes between elements and attributes. `serde-xml-rs`
    [doesn't support attributes](see (https://github.com/RReverser/serde-xml-rs/issues/140)).
    `xml-serde` uses a special `$attr:` rename prefix.
*   XML not only is namespaced but does so indirectly, by assigning prefixes to
    namespaces and referencing prefixes in element and
    attribute names. `serde-xml-rs` [doesn't support
    namespaces](https://github.com/RReverser/serde-xml-rs/issues/50).
    `xml-serde` uses a `{namespace}prefix:element` name for every field, which
    can be verbose both in the struct definition and the generated XML
    (not supporting binding a prefix at a higher level than it is used).
*   Even XML schema's "simple types" (the strings within text nodes
    and attribute values) can be quite complex:
    *   They can represent a list of values separated by spaces. The deserializer
        might be able to hint it's expecting this by calling eg `deserialize_seq`
        rather than `deserialize_string`.
    *   They can represent a "union": any of several possible subtypes. Now we
        need to support accumulating them in some buffer and backtracking.
        The buffer needs to also support these deserializer hints. The caller
        likely needs to request this buffering in some fashion, likely by
        wrapping with a type from this library, dropping the `serde` data format
        abstraction.
    *   They support three modes of [whitespace
        normalization](https://www.w3.org/TR/xmlschema11-1/#sec-wsnormalization).
        There's no way to pass this through serde, other than custom types or
        `#[serde(deserialize_with)]` functions.

These problems can likely be solved, but I find it much easier to understand a
data model specific to XML. It can be extended to support as much of XML as
necessary without wedging a square peg into a round hole.

### `static-xml` vs `yaserde`

[`yaserde`](https://crates.io/crates/yaserde) is conceptually similar to
`static-xml` but suffers from poor implementation quality.

#### Error handling

`yaserde`'s generated code will panic on invalid data, e.g. if a non-digit
is found where an `i32` is expected:

```
thread 'tests::basic_deserialization' panicked at 'called `Result::unwrap()` on an `Err` value: ParseIntError { kind: InvalidDigit }', schema/src/onvif.rs:4030:50
```

`static-xml` instead returns a nicely formatted error:

```
invalid digit found in string @ 14:25

XML element stack:
   4: <tt:Hour> @ 14:25
   3: <tt:Time> @ 13:21
   2: <tt:UTCDateTime> @ 12:17
   1: <tds:SystemDateAndTime> @ 6:13
   0: <tds:GetSystemDateAndTimeResponse> @ 3:9
```

#### Bugs

`yaserde` has several variations of unsolved bugs involving nested elements with
the same name (eg [#76](https://github.com/media-io/yaserde/issues/76)). The
root cause is that it doesn't have a well-defined contract for the
deserialization interface and doesn't track the depth reliably.

`static-xml` is based on the proposal in [#84](https://github.com/media-io/yaserde/issues/84)
which solves these problems systematically, introducing a deserialization
contract which is enforced by Rust's type system.

`yaserde` also has several bugs involving namespaces, eg
[#126](https://github.com/media-io/yaserde/issues/126), and enum element name
comparisons ignoring the namespace entirely. These are believed to be addressed
by `static-xml`, although many tests have yet to be written.

#### Bloat

With large schemas, `yaserde` bloats binaries and compilation time. Using
[`lumeohq/onvif-rs`](https://github.com/lumeohq/onvif-rs) 247b90c and Rust,
look at the code sizes below, particularly for the `schema` crate that contains
yaserde's generated code.

```
$ cargo bloat --release --example camera --crates
...
 File  .text      Size Crate
 7.5%  21.4% 1016.4KiB schema
 4.8%  13.7%  654.1KiB std
 4.1%  11.7%  556.6KiB reqwest
 3.3%   9.3%  443.9KiB yaserde
 2.2%   6.2%  295.5KiB clap
 1.6%   4.4%  211.6KiB h2
 1.2%   3.4%  160.4KiB regex_syntax
 1.1%   3.1%  147.7KiB onvif
 1.0%   2.9%  139.0KiB tokio
 0.9%   2.6%  124.6KiB hyper
 0.9%   2.6%  122.7KiB tracing_subscriber
 0.6%   1.8%   83.7KiB regex_automata
 0.6%   1.7%   80.8KiB xml
 0.5%   1.5%   72.8KiB regex
 0.4%   1.1%   52.4KiB http
 0.4%   1.0%   48.9KiB url
 0.3%   0.9%   43.7KiB num_bigint
 0.3%   0.9%   42.9KiB chrono
 0.3%   0.8%   36.8KiB idna
 0.3%   0.7%   35.4KiB encoding_rs
 2.3%   6.4%  305.0KiB And 62 more crates. Use -n N to show more.
35.2% 100.0%    4.6MiB .text section size, the file size is 13.2MiB

Note: numbers above are a result of guesswork. They are not 100% correct and never will be.
```

Compare to numbers from a WIP branch based on `static-xml` (which are likely to
further improve):

```
 File  .text     Size Crate
 5.0%  17.1% 655.0KiB std
 4.3%  14.5% 557.9KiB reqwest
 2.3%   7.7% 295.5KiB clap
 2.2%   7.4% 282.4KiB schema
 1.7%   5.7% 218.5KiB regex
 1.6%   5.5% 211.6KiB h2
 1.4%   4.8% 185.9KiB regex_syntax
 1.1%   3.7% 142.1KiB tokio
 1.0%   3.3% 125.1KiB tracing_subscriber
 1.0%   3.2% 124.3KiB hyper
 0.8%   2.7% 104.2KiB onvif
 0.6%   2.2%  82.8KiB regex_automata
 0.6%   1.9%  73.8KiB aho_corasick
 0.5%   1.8%  68.7KiB xml
 0.5%   1.7%  65.9KiB static_xml
 0.4%   1.4%  52.0KiB http
 0.4%   1.3%  48.9KiB url
 0.3%   1.1%  43.8KiB num_bigint
 0.3%   1.1%  42.9KiB chrono
 0.3%   1.0%  36.8KiB idna
 2.5%   8.5% 327.0KiB And 63 more crates. Use -n N to show more.
29.6% 100.0%   3.7MiB .text section size, the file size is 12.7MiB
```

On a powerful 12-core/24-thread AMD Ryzen 5900X machine,
`cargo bloat --release --example camera --times` says the `yaserde`-based
`schema` crate takes 97.99s to compile; the `static-xml`-based version takes
33.43s to compile. The difference is even more dramatic on older machines.
On several of my SBC setups, the `yaserde` version fails to compile
without enabling zramfs.

#### Compile-time errors

`yaserde`'s derive macros will panic in some cases with an unhelpful error
message. In others, they emit code that doesn't compile and doesn't have the
proper spans. Eg, if a field doesn't implement the required `YaSerialize`
interface, it describes the problem but doesn't pinpoint the offending line of
code:

```
error[E0277]: the trait bound `Foo: YaSerialize` is not satisfied
   --> schema/src/common.rs:38:37
    |
38  | #[derive(Default, PartialEq, Debug, YaSerialize, YaDeserialize)]
    |                                     ^^^^^^^^^^^ the trait `YaSerialize` is not implemented for `Foo`
    |
note: required by a bound in `yaserde::YaSerialize::serialize`
   --> /home/slamb/.cargo/registry/src/github.com-1ecc6299db9ec823/yaserde-0.7.1/src/lib.rs:106:19
    |
106 |   fn serialize<W: Write>(&self, writer: &mut ser::Serializer<W>) -> Result<(), String>;
    |                   ^^^^^ required by this bound in `yaserde::YaSerialize::serialize`
    = note: this error originates in the derive macro `YaSerialize` (in Nightly builds, run with -Z macro-backtrace for more info)
```

`static-xml`'s derive macros always try to add a relevant span.

```
error[E0277]: the trait bound `Foo: ParseText` is not satisfied
  --> schema/src/common.rs:49:9
   |
49 |     pub foo: Foo,
   |         ^^^ the trait `ParseText` is not implemented for `Foo`
   |
   = note: required because of the requirements on the impl of `Deserialize` for `Foo`
   = note: required because of the requirements on the impl of `DeserializeFieldBuilder` for `Foo`
   = help: see issue #48214
```

#### Flexibility

`yaserde` requires that every deserializable type implement `Default`, which is
particularly awkward for `enum`s. It also doesn't support required fields or
distinguishing between absent fields and ones set to the default value.

`static-xml` avoids this by defining a builder type matching each deserializable
type. Some caveats apply: currently the builders' `finalize` methods are a
significant source of code bloat, so there's a `direct` knob to turn them off.
I'd like to see if I can reduce the bloat without giving up the builders'
advantages.

## License

Your choice of MIT or Apache; see [LICENSE-MIT.txt](LICENSE-MIT.txt) or
[LICENSE-APACHE](LICENSE-APACHE.txt), respectively.
