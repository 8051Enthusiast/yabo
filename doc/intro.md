The Yabo Programming Language
=============================

Welcome to the markdown file that will attempt to teach you the basics of yabo.
How are you doing[?](https://strawpoll.com/Qrgebxr32Zp)

Yabo is a functional programming language intended for parsing binary files using parser combinators.
Yabo has the following goals:

* Being able to parse large structures such as file systems incrementally
* Being usable for exploratory parsing and resilient against errors
* Being able to parse unusual formats
* Being ergonomic without requiring allocations or a garbage collector

On the opposite side, these are some non-goals:
* Being able to parse streams of data
* Reading each byte at most once
* Being usable on changing data or MMIO

Finally, being a good language is not a goal.
The priority is to experiment around with some ideas and see what works.
If it ends up being a good language, it means that I didn't experiment enough.

Table of Contents
-----------------
- [The Yabo Programming Language](#the-yabo-programming-language)
  - [Table of Contents](#table-of-contents)
  - [Installation and Usage](#installation-and-usage)
  - [Parser Definitions](#parser-definitions)
  - [Let Bindings](#let-bindings)
  - [Padding](#padding)
  - [Returning](#returning)
  - [Specifying Types](#specifying-types)
  - [Function Arguments](#function-arguments)
  - [Failing and Patterns](#failing-and-patterns)
  - [Choices](#choices)
    - [Optional Fields](#optional-fields)
    - [Multiple Choices](#multiple-choices)
    - [`then` and `else`](#then-and-else)
  - [Recursive Structures and Thunks](#recursive-structures-and-thunks)
  - [Functions that are not Parsers](#functions-that-are-not-parsers)
  - [Arrays](#arrays)
    - [`parser[..]`](#parser)
    - [Operators working with arrays](#operators-working-with-arrays)
  - [Regexes](#regexes)
  - [The `at` Operator](#the-at-operator)
  - [The `span` Operator](#the-span-operator)
  - [Ranges](#ranges)
  - [Imports](#imports)
  - [Appendix A: Overview of Expressions](#appendix-a-overview-of-expressions)
    - [Precedence](#precedence)

Installation and Usage
----------------------
Right now, it is probably the best just to use the language playground at [yabo-lang.org](https://yabo-lang.org).
The left side is the editor, the upper right corner contains a hex viewer of the file, and the lower right corner contains the output of the parser.

Hex bytes, the scroll bar and the fields of the output are sometimes colored.
The color indicates a contiguous segment of parsed bytes.
Currently, a segment of parsed bytes is considered separate if a pointer points outside the current segment to a struct or array.

If you want to compile the compiler locally, you will have to install the LLVM 18 libraries and the Rust toolchain.
To compile, run `cargo build --release` in the root directory.
Note that the compiler currently only supports x86_64 Linux and WASM as targets.
You will also have to set `YABOC_LIB_PATH` to point to the `lib` directory of this project for the compiler to work.
The compiler can be invoked with `./target/release/yaboc <input file>.yb <output file>.so` to output a shared library.
You can use that shared library using the print tool in `tools/print` or the python bindings in `yabo.py` in the project root.

Finally, the `ylliab` tool in `tools/ylliab` can be used to interactively explore the output of the parser.
You'll need Qt6 installed to compile it using `cmake`.
For it to compile your program, you will also need `yaboc` in your `PATH`.

Parser Definitions
------------------

Let's start with a simple example:
```
def ~u16_pair = {
  fst: u16l
  snd: u16l
}
```
As you might guess this defines a parser `u16_pair` with two fields, both containing `u16l`.
The `u16l` stands for <b>u</b>nsigned <b>16</b>-bit integer, <b>l</b>ittle endian.
We can also use `u16b` for the big endian version, or `i16l` for the signed version.

Now you might ask what the `~` is for.
For that, an explanation of parser combinators is necessary:

Let's say we are parsing an array of bytes with an `u16_pair`.
When starting to parse, we have our initial position in the array.
```
64 12 42 6b 82 93 76 9d
      ^
```
During parsing, we read `42 6b` in order to construct the `u16l` value `0x6b42` in the field `fst`.
In order to parse the value in `snd`, we have to know where `fst` ends, and that information can only be provided by the `u16l` implementation.
This is done by essentially defining a parser to be a function `[u8] -> (type, [u8])` which takes the starting position and returns the parsed value and the new position.
In this case, the `u16l` parser would return `0x6b42` and the new position
```
64 12 42 6b 82 93 76 9d
            ^
```
The case for parsing `snd` works similarly, and the whole block combines the parser to take the initial position and return `{fst: 0x6b42, snd: 0x9382}` and the new position
```
64 12 42 6b 82 93 76 9d
                  ^
```

In yabo, we notate a parser type like `[u8] -> (type, [u8])` as `[u8] ~> type`.
`~type` is a short form for `[u8] ~> type`, because bytes are what we are parsing most of the time.

It is important to recognize that blocks `{...}` are expressions that produce parsers.
For example, these examples are possible as well:
```
def ~nested_block = {
  outer: {
    inner: u16l
  }
}

def ~no_block = u16l
```

There are two special built-in parsers available: `~` and `+`:
* `~` is a parser that returns a single element of the input array and advances the position by one.
* `+` parses zero elements and returns the unit value.

If we want to use a parser from the command line or playground, we have to add an `export` in front of the definition:
```
export
def ~u16_pair = {
  fst: u16l
  snd: u16l
}
```

The playground applies the parser named `main` at offset 0 by default, but we can also apply a different parser to a different address by moving to the position in the hex view, pressing 'a', and entering the name of the parser.

Let Bindings
------------
In some situations, we might have fields that need to have their value calculated from other fields.
For example, say we have two interleaved fields, and we want to access their values as a single field:
```
def ~interleaved = {
  a_lo: u8
  b_lo: u8
  a_hi: u8
  b_hi: u8
  let a = a_lo | (a_hi << 8)
  let b = b_lo | (b_hi << 8)
}
```

The `let` does not influence the position of the following fields, and we cannot rely on `let` to be evaluated in any particular order.
On the other hand, we can insert the `let` anywhere we like regardless of its dependencies.
for example, we can also write
```
def ~interleaved = {
  let a = a_lo | (a_hi << 8)
  a_lo: u8
  b_lo: u8
  let b = b_lo | (b_hi << 8)
  a_hi: u8
  b_hi: u8
}
```
The intent of this is to allow the parser to run in the backwards direction, which will hopefully get implemented sometime in the future.

Remember how blocks are expressions?
This means we can assign it to a `let` binding and then reuse it:
```
def ~u16_quad = {
  let block = {
    fst: u16l
    snd: u16l
  }
  a: block
  b: block
}
```
the value returned from this will be something like `{block: (parser value), a: {fst: (number), snd: (number)}, b: {fst: (number), snd: (number)}}` where `(parser)` and `(number)` are placeholders for the actual values.

Padding
-------
Oftentimes there's space between fields that is unused.
In this case we can specify a parser without specifying a field name:
```
def ~padded = {
  small: u16l
  u8
  u8
  large: u32l
}
```

Because writing a newline every time we want to add a parser takes unnecessary space, we can also separate parsers with commas:
```
def ~padded = {
  small: u16l
  u8, u8
  large: u32l
}
```
or even
```
def ~padded = {small: u16l, u8, u8, large: u32l}
```

When parsing the string `01 02 03 04 05 06 07 08`, the value of `padded` will be `{small: 0x0201, large: 0x08070605}`.


Returning
---------
Sometimes we want to return values that are not structs.
In this case, we can use the `return` keyword, which can be used in positions where normally an identifier would be expected:
```
def ~u16l = {
  lo: u8
  hi: u8
  let return = lo | hi << 8
}
```
All the other fields are ignored and just the value of `return` is returned.

Indeed, this is how `u16l` is implemented in the standard library.
(You might ask how `u8` is implemented and the answer to that is simply `fun ~u8 = ~`.
the difference between `def` and `fun` will be explained later, but they're mostly the same.)

We can also write `return` in place of a field name:
```
def ~skip_four_bytes_u16l = {
  ignored_field: u8
  u8, u8, u8
  return: u16l
}
```

Specifying Types
----------------
Sometimes we want to specify the return type of parser definition or the type of a let binding.
In this case we can write the type after a colon:
```
def ~u16l: int = {
  lo: u8
  hi: u8
  let return: int = lo | hi << 8
}
```
Note that the language currently only has a single signed integer type with 64 bits called `int`.
Here is an overview of the types:

| type            | description                                                         |
| --------------- | ------------------------------------------------------------------- |
| `int`           | signed 64-bit integer                                               |
| `bit`           | single bit (boolean)                                                |
| `u8`            | byte in memory (a byte pointer), is a subtype of `int` by dereferencing into the byte value (not the address!) |
| `'t`            | generic type (can have any identifier)                              |
| `[foo]`         | array of `foo`                                                      |
| `foo ~> bar`    | parser that takes a `foo` and returns an `bar` (advancing `foo`)    |
| `~foo`          | desugars to `[u8] ~> foo`                                           |
| `(bar, baz) -> foo` | function that takes `bar` and `baz` and returns an `foo`, see below |
| identifier      | refers to a parser definition (for example `u16_pair` refers to the value returned by the `u16_pair` parser) |

Failing and Patterns
--------------------
An important task of parsing, other than returning the parsed data, is to recognize whether the input matches the expected format.
The primary way to do this in yabo is the `is` operator:
```
def ~ascii_byte = {
  let byte = u8
  let return = byte is 0x20..0x7e or '\n' or '\r' or '\t'
}
```
The `is` operator takes a value to the left and a pattern to the right.
If the value matches the pattern on the right, the parser fails.

A failure signals that the input does not match the expected format and is forwarded until it is handled or the whole parse fails.
Handling failures will be examined closer in the [Choices](#choices) section.

Integers can be specified as ranges, like `0x20..0x7e`, or as single values, like `42` or `0x64`.
Characters can be specified as single characters, like `'a'`, or as escape sequences, like `'\n'`, and match the corresponding byte value as defined by the ASCII.
Patterns can be combined with `and` and `or`, like `foo is a and c or b and d`.
(Parentheses are not allowed in order to keep the pattern in disjunctive normal form for easier semantic analysis.)

The `is` expression can also be applied directly to parsers, in which case a new parser is created that fails if the original parser does not return a value that matches the pattern:
```
def ~ascii_byte = u8 is 0x20..0x7e or '\n' or '\r' or '\t'
```

On parser specifically, we can also use the `!eof` pattern:
```
def ~rgb_if_not_eof = {r: u8, g:u8, b: u8} is !eof
```
Normally, an `eof` is thrown when the parser reaches the end of the input (in the case of `rgb_if_not_eof`, if there are less than 3 bytes left).
The `!eof` pattern fails if the parser reaches the end of the input prematurely, converting it into a failure condition that can be handled.

If a parser that can fail is called, the `?` operator must be used to mark the call as fallible:
```
def ~tag = u8 is 0x00..0x7f
def ~structure = {
  signature: tag?
  field: u16l
}
```
This is not required if the parser is an `is` operator or it is a block, as the fallibility can already be seen by looking at the current parser definition.

Besides failing, it is also possible for a parser to error.
An error is a condition that is not expected to happen, like a division by zero or an out-of-bounds access, and it cannot be handled.
A fallible parser can be made non-fallible by using the `!` operator:
```
def ~foo = {
  signature: tag?
  # we already expect the signature to match, so if the following
  # does not match, it is an error
  some_other_data: tag!
}
```

To summarize, there are four conditions:
* A parser can succeed, returning a value.
* A parser can throw an `eof` if it reaches the end of input, which can be handled with the `!eof` pattern.
* A parser can fail, which can be converted into an error with the `!` operator.
* A parser can error, which is a condition that is not expected to happen and cannot be handled.

Function Arguments
------------------
Given that this is a parser combinator language, it is important to be able to define higher order functions.
Arguments can be specified in parentheses after the name of the parser:
```
def ~biased_int(int_parser: ~int, bias: int) = {
  parsed_int: int_parser!
  let return = parsed_int + bias
}
```

Note that function arguments are always assumed to be fallible for now (I plan to change this in the future), hence the `!`.

The parser combinator can then be applied by writing the arguments in parentheses after the name, like `biased_int(u16l, 42)`.
We can also partially apply function arguments by writing two dots after the last given argument, like in this (a bit nonsensical) example:
```
def ~apply_with_u16l(applied_int: (int) -> ~int) = {
  val: i16l
  return: applied_int!(val)!
}
def ~runtime_bias_u16l = apply_with_u16l(biased_int(u16l, ..))
```
The `applied_int!(val)!` makes sure that both the function application and parser application is fallible, and I do realize this looks ugly, but again this will hopefully change in the future.

Choices
-------
Barely any binary format is just a fixed sequence of fields.
Say we have an encoding of integers in the range `0x0000`-`0x7FFF` where many are small so we want to save space by encoding them in a single byte:
1. Integers from `0x0000` to `0x007F` are encoded as a single byte of the form `0xxxxxxx`.
2. Integers from `0x0080` to `0x7FFF` are encoded as two bytes of the form `1xxxxxxx xxxxxxxx`, in little endian.

This can be implemented as follows:
```
def ~small_int = {
  lo: u8
  | let return = lo is 0x00..0x7f

  \ hi: u8
    let return = (lo & 0x7f) | (hi << 7)
}
```
The `|` initiates a choice, which means the individual branches are tried in order until one succeeds.
If all branches fail, the whole choice fails.
The last branch uses `\` instead of `|` to indicate that the whole choice ends with it.
Note that this is different to Prolog: `{small_int, u8 is 0}` does not try the other branch in `small_int` if `u8 is 0` fails; it just fails the whole parser.

The extent of one branch is whitespace sensitive and ends at the next dedent, `|`, or end of block.

Let's see how this works with some examples:
1. When parsing `42 55 10 ..`, the `lo` field is first parsed, evaluating to `0x42`.
   Next, the choice is entered and the `is` expression is evaluated.
   `lo` is `0x42`, which matches the pattern `0x00..0x7f`, so the value `0x42` is returned.
   The parser after the `small_int` would continue parsing at `55 10 ..`.
2. When parsing `aa 55 10 ..`, the `lo` field is first parsed, evaluating to `0xaa`.
   Next, the choice is entered and the `is` expression is evaluated.
   `lo` is `0xaa`, which does not match the pattern `0x00..0x7f`, so the expression fails.
   The whole first branch fails, and the next branch is tried.
   In the second branch, the `hi` field is parsed, evaluating to `0x55`.
   The `return` expression is evaluated, which returns `0x2aaa`.
   The parser after the `small_int` would continue parsing at `10 ..`.

If a `return` is in a branch, it must be in every branch, however if there is no `return` in a block then the fields inside each branch become optional (except if they are in every branch):
```
def ~maybe(f: ~'t) = {
  | some: f?
  # remember: `+` parses zero elements and returns the unit value
  \ +
}
```
This returns a structure containing a single `some` field with the output of `f` if it succeeds, or an empty structure if it fails.

### Optional Fields

Optional fields can be accessed either with `block.?field` (which fails) or `block.field` (which errors):
```
def ~small_int = {
  small: maybe(tag)
  | let return = small.?some
  \ return: u16l
}
```

Fields can also be checked with `is`:
```
def ~small_int = {
  | small: maybe(tag) is some
    # this cannot fail as the existence of the `some` field was checked by the `if`
    let return = small.some
  \ return: u16l
}
```

### `then` and `else`
There are two binary operators, `then` and `else`, that allow handling failing within one expression.
`a then b` first evaluates `a` and if it succeeds, it then evaluates and returns the value of `b`.
`a else b` first evaluates `a` and if it succeeds, it returns the value of `a`.
Otherwise, it evaluates and returns the value of `b`.
`then` has lower precedence compared to `else`, so it can essentially be used to replicate the functionality of `is` statements:
`x is 0 then foo else bar` would evaluate `foo` if `x` is `0`, and to `bar` otherwise.

As a convenience, there's also the `when?` function, which fails if the argument is false:
`when?(foo > bar) then foo else bar` would evaluate `foo` if `foo` is greater than `bar`, and to `bar` otherwise.

Recursive Structures and Thunks
-------------------------------
One unusual aspect of yabo is the absence of recursive values.
This makes yabo Turing incomplete as every type can only contain finitely many values.
However, this is precisely what allows yabo to avoid allocations and garbage collection.

However, there is a pointer-like construct that allows indirection, called a thunk.
When we parse a `u16l`, what is returned is not actually the value itself but is a thunk containing all the arguments to the `u16l` parser.
The arguments to the `u16l` consist of just the `[u8]` at the current position, which is represented by a pair of pointers most of the time.
The type of this thunk is `u16l` (which is different from the type of the parser `u16l`, which is actually `[u8] ~> u16l` - one `u16l` is a value, and the other a type).
Values of type `u16l` can be used in any place where an `int` is expected, as `u16l` is a subtype of `int`.

How does one define recursive structures then?
As an example, take a contiguous recursive list:
```
def ~list(f: ~'t) = {
  | head: f?
    tail: list(f)
  \ +
}
```
If we naively tried to construct the resulting value without using thunks, we would end up with arbitrarily deep nested structures like (leaving `head` out), `{tail: {tail: {tail: {}}}}`.
However, since `list` is a thunk, the value is not actually recursive and the `tail` field just contains the `[u8]` slice where the next element starts, and the parser `f`.

For example let's access the thunk returned by the list.
```
def ~byte_3_of_c_string = {
  s: list(u8 is 0x01..0xff)
  # zero terminator
  u8
  let return = s.?tail.?tail.?tail.?head
}
```
If we evaluate `list(u8 is 0x01..0xff)` (a C string) applied to `41 62 63 64 00`, we would only get back the thunk as a value, but it would still evaluate completely as it still has to find out the end of the string.
However, when evaluating the thunk that was returned earlier from the parser, there's no need to recurse.
We do not need the length of the list to get the values of both fields.
For `head`, `u8 is 0x01..0xff` does get evaluated, but the `tail` field is just a thunk and needs no further evaluation.

If you use the `fun` keyword instead of the `def` keyword, no thunk will be returned.
This is required especially if the return type is a type variable (for example, `[u8] ~> 't`).
It is meant more for calculations than data definitions.
For example, the following calculates the sum of all bytes in a list:
```
fun ~sum(f: ~int, acc: int) = {
  | head: f is !eof
    return: sum_list(f, acc + head)
  \ let return = acc
}
```
A `def` would be overkill here since a thunk would be returned.
If we do not immediately evaluate it but instead evaluate the thunk multiple times later whenever accessing it, that would result in unnecessary work.

Functions that are not Parsers
------------------------------
Not everything is a parser.
In order to define a function that is not a parser, we use the `fun` keyword, but leave out the `~`:
```
fun square(n: int) = n * n
fun cube(n: int) = square(n) * n
```
Of course, we also want to use `let` to bind variables, but we cannot use `{` and `}` for this, as those define parsers.
Instead, we use `{|` and `|}`:
```
fun cube(n: int) = {|
  let square = n * n
  let return = square * n
|}
```
We can also define blocks with fields this way, and use choices:
```
fun maybe_zero(n: int) = {|
  | let zero = n is 0
  \ let nonzero = n
|}

fun square(n: int) = maybe_zero(n).?zero else (n * n)
```

Note also that the operator `?` to mark something as backtracking needs to be placed before the application, so if use a backtracking function, we need to write `when?(foo)` instead of `when(foo)?` as with parsers:
```
fun when(condition: bool) = condition is true
fun foo() = when?(1 == 2) then 1 else 2
```

Arrays
------
if the parser has a constant size it can be used inside an array:
```
def ~pascal_string = {
  len: u8
  return: u8[len]
}
```
`parser[len]` takes a parser (`u8`) and the length of the array (`len`), and returns a parser for an array of that length with elements from the parser.
If `parser` is of type `'t ~> 'r`, then `parser[len]` is of type `'t ~> ['r]`.

In order for the compiler to infer that two branches are of the same length (which is a requirement for the length to be constant), it has the ability to reason about polynomial equality.
For example, the following would have a constant size:
```
def ~const_sized(a: int, b: int) = {
  | u8[a * a]
    u8[2 * a * b]
    u8[b * b]
  \ u8[(a + b) * (a + b)]
}
```
as a² + 2ab + b² = (a + b)² (binomial formula).

The following also works:
```
def ~const_sized2(f: ~'t, g: ~'r) = {
  | f, f, g
  | f, g, f
  \ g, f, f
}
```
(Of course, these parsers are useless as they will always take the first branch.)

We can also nest arrays:
```
def ~matrix = {
  width: u8
  height: u8
  return: u8[width][height]
}
```

If the parser has a constant size, we can get the length of the parser without applying it by using `.sizeof`:
```
def ~padded_to_1024 = {
  let parser = const_sized2(u8, u16l)
  return: parser
  u8[1024 - parser.sizeof]
}
```

In many cases, the parser underlying the array will be of the same type as the parsed array (in most cases, `u8`), so instead of using `~[len]` (which frankly does look a bit silly) we can just write `[len]`:
```
def ~padded_to_1024 = {
  let parser = const_sized2(u8, u16l)
  return: parser
  [1024 - parser.sizeof]
}
```

The `.sizeof` operator can also be applied to arrays and will return the length of the array:
```
def ~padded_to_1024 = {
  len: u8
  field: [len]
  u8[1024 - (field.sizeof + 1)]
}
```

### `parser[..]`
Sometimes we do not have a specific length we want the array to be, but instead parse until the end of the current input.
In this case, we can use `parser[..]`, and the shortened version of `~[..]` is `[..]`:
```
fun ~rgb = {
  red: u8
  green: u8
  blue: u8
}

def ~rgb_array = {
  colors: rgb[..]
  rest: [..]
}
```

Each `rgb` value is 3 bytes, and `rgb[..]` makes the array as long as possible.
If the input is 16 bytes, this would mean that the `colors` field of `rgb_array` would contain 5 rgb values (as 5 \* 3 = 15), and the `rest` field would contain just one value, the last byte.

### Operators working with arrays
The `~>` operator allows for the application of a parser to an array:
```
def ~padded_to_1024 = {
  array: u8[1024]
  let return = array ~> const_sized2(u8, u16l)
}
```
Similarly, `|>` allows composition of parsers:
```
def ~padded_to_1024 = u8[1024] |> const_sized2(u8, u16l)
```

`|>` actually just desugars to a call to the following combinator:
```
fun ['a] ~> compose(a: ['a] ~> ['b], b: ['b] ~> 'c) = {
  x: a?, let return = x ~> b?
}
```

We can also index into arrays using the `.[index]` operator:
```
def ~first_byte = {
  array: u8[1024]
  let return = array.[0]
}
```

Regexes
-------
If we want to match a sequence of bytes specified by a regular expression, we can use the `/.../` syntax:
```
def ~c_string = /[^\x00]*\x00/
```
Regex parsers are of type `[u8] ~> [u8]` (i.e., they take a byte array and return a byte array, advancing the input until the end of the match).
Just like with the `is` operator, they may backtrack but do not need a `?` to mark them as such, as regices are fallible in most cases anyway.
The syntax is the same as the regex crate, documented [here](https://docs.rs/regex/latest/regex/).

There is also a regex variant prefixed with `h` that matches hexadecimal strings:
```
def ~elf = {
  magic: h/7f 45 4c 46/
  other_fields: [..]
}
```

The `at` Operator
-----------------

The `at` operator allows parsing at a specific location in the input file, specified via an integer address:
```
fun ~u8ptr(parser: ~'t) = {
  addr: u8
  let return = parser! at addr
}

def ~linked_list = {
  | u8 is 0
  \ next: u8ptr(linked_list)?
  val: u8
}
```
Here in this example, we define a linked list with byte addresses and byte values.
The `u8 is 0` ensures that we do not have a `next` field if the address is 0; otherwise we parse an `u8ptr(linked_list)`.
Upon closer inspection of the `u8ptr` parser, we see how the `at` operator is used — it takes an address to the right side and a parser to the left side and returns the parsed value at the address.

The `at` operator evaluates eagerly.
However, since `linked_list` is defined via `def` and not `fun`, the `linked_list` parser is a thunk, and `parser at addr` only returns the thunk which consists solely of the address.

If the address is out of range, the `at` operator returns an error.

The `span` Operator
-------------------

The `span` operator allows getting an array spanning a range of the input corresponding to a range of fields in the current block.
For example, say we have a function `crc32: int([u8])` for calculating a CRC32 checksum and want to have the expected checksum for a PNG chunk:
```
def ~png_chunk = {
  length: u32l
  type: u32l
  data: u8[length]
  crc: u32l
  let expected_crc = crc32(span type..data)
}
```
The result of `span type..data` would be an array containing the bytes from the start of the `type` field to the end of the `data` field, inclusive.
It can only be used inside blocks and can contain fields of parse statements of the current block that are in the current scope or a parent scope.
For example, the following is not possible because `bar` is not in the top scope of the block:
```
def ~foo = {
  | bar: u8 is 1
  \ bar: u8
  baz: u8
  let spanned = span bar..baz
}
```

`span` can also be useful if you need to pad a structure to a certain alignment:
```
def ~padded_to_1024_align = {
  len: u16l
  field: [len]
  [1023 - (span len..field.sizeof - 1) % 1024]
}
```

Ranges
------
The `foo ..< bar` operator allows creating a range of integers, ranging from `foo` to `bar - 1`.
(Inclusive ranges are not yet implemented).
It can be parsed and indexed as with other kinds of array:
```
fun [int] ~> sum(acc: int) = {
  | val: ~ is !eof
    return: sum(acc + val)
  \ let return = acc
}

fun range_sum(n: int) = 0..<n
                     ~> sum(0)
```

Together with `[..]`, this can be combined to a sort of loop:
```
fun bits_of(n: int, bit_count: int) = 0..<bit_count
  ~> { i: ~, let return = (n >> i) & 1 != 0 }[..]
```
Note what we are doing here:
`{ i: ~, let return = (n >> i) & 1 != 0 }` is a parser of length 1 that takes an integer `i` and returns whether the `i`-th bit of `n` is set.
The `[..]` then maps this parser over each element of the range `0..<bit_count`, returning array containing a boolean for each bit of `n`.
(Hopefully, once lambdas are implemented, this can be written less weirdly using a `map` function).

Imports
-------
The `import` keyword allows importing other files.
It is a top-level statement and takes a single identifier as an argument:
```
import text
def ~num = {
  | /0x/, return: text.basenum(16)?
  \ return: text.basenum(10)?
}
```
The definitions from the imported file can be accessed via `.`, like `text.basenum` in the above example.
Cycles in imports are not allowed.

When importing an identifier `foo`, the following paths are searched, in this order:
1. The directory of the source file is searched for a file named `foo.yb`.
2. The directory of the source file is searched for a directory named `foo`, and then for a file named `mod.yb` inside that directory.
3. The path specified on the `yaboc` command line via `-m foo=path` is used if present.
4. The standard library directory, either at `YABOC_LIB_PATH` or `~/.local/share/yabo/lib`, is searched for a file named `foo.yb`.
5. The standard library directory is searched for a directory named `foo`, and then for a file named `mod.yb` inside that directory.

Appendix A: Overview of Expressions
-----------------------------------

| Operator | Kind          | Description        |
| -------- | ------------- | ------------------ |
| `-`      | Prefix Unary  | Negation           |
| `!`      | Prefix Unary  | Boolean negation   |
| `?`      | Postfix Unary | Mark Fallibility   |
| `!`      | Postfix Unary | Convert into Error |
| `[..]`   | Postfix Unary | Array until end    |
| `.sizeof`| Postfix Unary | Length of array or parser |
| `.field`, `.?field` | Postfix Unary | Access field       |
| `then`   | Control       | Evaluate lhs and return rhs if lhs succeeds |
| `else`   | Control       | Evaluate lhs and return lhs if lhs succeeds, otherwise return rhs |
| `+`, `-` | Integer       | Addition, Subtraction |
| `*`, `/` | Integer       | Multiplication, Division |
| `%`      | Integer       | Modulo             |
| `<<`, `>>` | Integer     | Bit shift          |
| `&`, `\|`, `^` | Integer  | Bitwise and, or, xor |
| `==`, `!=` | Comparison  | Equality, Inequality |
| `<`, `>`, `<=`, `>=` | Comparison | Less than, Greater than, Less or equal, Greater or equal |
| `~>`     | Parser        | Apply parser on rhs to array on lhs |
| `\|>`    | Parser        | Compose parsers    |
| `at`     | Parser        | Parser at lhs at address on rhs  |
| `foo[bar]` | Parser      | Array of length `bar` |
| `foo.[bar]`, `foo.?[bar]` | Array | Index into array |
| `..<`    | Array         | Make range from lhs to rhs - 1 |
| `foo(bar, baz)` | Function Application | Call function `foo` with arguments `bar` and `baz` |
| `foo(bar, ..)` | Function Application | Call function `foo` with arguments `bar`, currying the rest |
| `foo`    | Identifier    | Refer to a value    |
| `true`, `false` | Literal | Boolean literals   |
| `0x..`, `0..` | Literal  | Hexadecimal, Decimal literals |
| `/.../`, `h/.../` | Literal  | Create regex parser |
| `span foo..bar` | Literal | Create span array of fields from `foo` to `bar` |
| `+`      | Literal       | Unit parser        |
| `~`      | Literal       | Single value parser |
| `{...}`  | Block         | Create block parser |
| `{| ... |}`| Inline Block  | Create block that is immediately evaluated |

### Precedence
Generally, postfix operators have higher precedence than prefix operators, and prefix operators have higher precedence than binary operators.
Integer operators have a higher precedence than comparison operators.
Associative operators (`+`, `*`, `&`, `^`, `|`, `then`, `else`, `|>`) don't need parentheses when chained.

In most distributive operator pairs, the multiplicative operation has precedence over the additive one:
* `+`, `-` has lower precedence than `*`, `/`, `%`
* `&`, `^`, `|` has lower precedence than `<<`, `>>`
* `|`, `^` has lower precedence than `&`
* `else` has lower precedence than `than`

Additionally, `else` and `than` have lower precedence than everything else on the rhs, and `..<` has higher precedence than `~>` on the lhs so that writing `0..<n ~> foo` does not need parentheses.