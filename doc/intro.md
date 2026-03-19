The Yabo Programming Language
=============================

Welcome to the markdown file that will attempt to teach you the basics of yabo.
How are you doing[?](https://strawpoll.com/Qrgebxr32Zp)

Yabo is a functional programming language intended for parsing binary files using parser combinators.
Yabo has the following goals:

* Being able to parse large structures such as file systems incrementally
* Being usable for exploratory parsing and resilient against errors
* Being able to parse unusual formats
* Being ergonomic without requiring heap allocations or a garbage collector

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
  - [Cases](#cases)
    - [Optional Fields](#optional-fields)
    - [Multiple Cases](#multiple-cases)
    - [`then` and `else`](#then-and-else)
  - [Generics](#generics)
  - [Recursive Structures and Thunks](#recursive-structures-and-thunks)
  - [Functions that are not Parsers](#functions-that-are-not-parsers)
  - [Arrays](#arrays)
    - [`[..]parser`](#parser)
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

If you want to compile the compiler locally, you will have to install the LLVM 21 libraries with clang and the Rust toolchain.
To compile, run `cargo build --release` in the root directory.
Note that the compiler currently only supports x86_64/aarch64 Linux and WASM as targets.
You will also have to set `YABOC_LIB_PATH` to point to the `lib` directory of this project for the compiler to work.
The compiler can be invoked with `./target/release/yaboc <input file>.yb <output file>.so` to output a shared library.
You can use that shared library using the print tool in `tools/yaboprint` or the python bindings in `yabo.py` in the project root.

Finally, the `ylliab` tool in `tools/ylliab` can be used to interactively explore the output of the parser.
You'll need Qt6 installed to compile it using `cmake`.
For it to compile your program, you will also need `yaboc` in your `PATH`.

Parser Definitions
------------------

Let's start with a simple example:
```
def u16_pair = {
  fst: u16l
  snd: u16l
}
```
As you might guess this defines a parser `u16_pair` with two fields, both containing `u16l`.
The `u16l` stands for <b>u</b>nsigned <b>16</b>-bit integer, <b>l</b>ittle endian.
We can also use `u16b` for the big endian version, or `i16l` for the signed version.

At a first look, this might look like a definition for a type, but we are actually writing a program!
To understand what I mean, an explanation of parser combinators is necessary:

Let's say we are parsing an array of bytes with an `u16_pair`.
When starting to parse, we have our initial position in the array.
```
64 12 42 6b 82 93 76 9d
      ^
```
During parsing, we read `42 6b` in order to construct the `u16l` value `0x6b42` in the field `fst`.
In order to parse the value in `snd`, we have to know where `fst` ends, and that information can only be provided by the `u16l` implementation.
This is done by essentially defining a parser to be a function `[]u8 -> (T, []u8)` which takes the starting position (an area of a byte array) and returns the parsed value and the new position.
In this case, the `u16l` parser would return `0x6b42` and the new position two bytes ahead:
```
64 12 42 6b 82 93 76 9d
            ^
```
The case for parsing `snd` works similarly, and the whole block combines the parser to take the initial position and return `{fst: 0x6b42, snd: 0x9382}` and the new position
```
64 12 42 6b 82 93 76 9d
                  ^
```

In yabo, we notate a parser type like `[]u8 -> (T, []u8)` as `[]u8 ~> T`.
`~T` is a short form for `[]u8 ~> T`, because bytes are what we are parsing most of the time.

It is important to recognize that blocks `{...}` are expressions that produce parsers.
They essentially produce a new parser by sequencing multiple inner parsers.
For illustration purposes, these examples are possible as well:
```
def nested_block = {
  outer: {
    inner: u16l
  }
  other: u16l
}

def no_block = u16l
```

(If you're coming from Haskell, you can see the `{...}` kind of as `do`-Blocks, except the return type is a record with the fields being the variables from the inner context. If you're not, an understanding of monads is not necessary to understand and use this language though.)

There is a special built-in parser `~`: it returns a single element of the input array and advances the position by one.
For example, `def single = ~` has a length of one and just returns the byte of the input itself.

If we want to use a parser from the command line or playground, we have to add an `export` in front of the definition:
```
export
def u16_pair = {
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
def interleaved = {
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
def interleaved = {
  let a = a_lo | (a_hi << 8)
  a_lo: u8
  b_lo: u8
  let b = b_lo | (b_hi << 8)
  a_hi: u8
  b_hi: u8
}
```
The intent of this is to allow the parser to run in the backwards direction, which will hopefully get implemented sometime in the future.

Remember how blocks are expressions that produce a value of parser type?
This means we can assign it to a `let` binding and then reuse it:
```
def u16_quad = {
  let block = {
    fst: u16l
    snd: u16l
  }
  a: block
  b: block
}
```
The value returned from this will be something like `{block: (parser value), a: {fst: (number), snd: (number)}, b: {fst: (number), snd: (number)}}` where `(parser)` and `(number)` are placeholders for the actual values.

Padding
-------
Oftentimes there's space between fields that is unused.
In this case we can specify a parser without specifying a field name:
```
def padded = {
  small: u16l
  u8
  u8
  large: u32l
}
```

Because writing a newline every time we want to add a parser takes unnecessary space, we can also separate parsers with commas:
```
def padded = {
  small: u16l
  u8, u8
  large: u32l
}
```
or even
```
def padded = {small: u16l, u8, u8, large: u32l}
```

When parsing the string `01 02 03 04 05 06 07 08`, the value of `padded` will be `{small: 0x0201, large: 0x08070605}`.


Returning
---------
Sometimes we want to return values that are not structs.
In this case, we can use the `return` keyword, which can be used in positions where an identifier would normally be expected:
```
def u16l = {
  lo: u8
  hi: u8
  let return = lo | hi << 8
}
```
All the other fields are ignored and just the value of `return` is returned.

Indeed, this is how `u16l` is implemented in the standard library.
(You might ask how `u8` is implemented and the answer to that is simply `fun ~u8 = ~`.
the difference between `def` and `fun` will be explained later, but they're mostly the same.)

We can also write `return` in place of a parsed field name:
```
def skip_four_bytes_u16l = {
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
def u16l: int = {
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
| `[]foo`         | array of `foo`                                                      |
| `foo ~> bar`    | parser that takes a `foo` and returns an `bar` (advancing `foo`)    |
| `~foo`          | desugars to `[]u8 ~> foo`                                           |
| `(bar, baz) -> foo` | function that takes two arguments `bar` and `baz` and returns an `foo`, see below |
| identifier      | refers to the type returned by a parser definition (for example `u16_pair` refers to the value returned by the `u16_pair` parser, an `int`) |

Failing and Patterns
--------------------
An important task of parsing, other than returning the parsed data, is to recognize whether the input matches the expected format.
The primary way to do this in yabo is the `if` operator:
```
def ascii_byte = {
  let byte = u8
  let return = byte if 0x20..0x7e or '\n' or '\r' or '\t'
}
```
The `if` operator takes a parser to the left and a pattern to the right.
If the value does not match the pattern on the right, the parser fails.

A failure signals that the input does not match the expected format and is forwarded until it is handled or the whole parse fails.
Handling failures will be examined closer in the [Cases](#cases) section.

Integers can be specified as ranges, like `0x20..0x7e` (inclusive), or as single values, like `42` or `0x64`.
Characters can be specified as single characters, like `'a'`, or as escape sequences, like `'\n'`, and match the corresponding byte value as defined by the ASCII.
Patterns can be combined with `and` and `or`, like `foo is a and c or b and d`.
(Parentheses are not allowed in order to keep the pattern in disjunctive normal form for easier semantic analysis.)

There is also the `is` expression, which can also be applied directly to parsers.
In this case a new parser is created that fails if the original parser does not return a value that matches the pattern:
```
def ascii_byte = if u8 is 0x20..0x7e or '\n' or '\r' or '\t'
```
This time, the `if` is used in prefix form to signify that an application of the parser to a value can fail.
The principle is that each expression that can fail should include either an `if` or a `?` (see more on that later) to signify that it is fallible.
Ideally, marking the expression's fallibility prevents a scenario where any expression may implicitely fail and it is impossible to know which.

On parser specifically, we can also use the `!eof` pattern:
```
def rgb_if_not_eof = if {r: u8, g:u8, b: u8} is !eof
```
Normally, an `eof` is thrown when the parser reaches the end of the input (in the case of `rgb_if_not_eof`, if there are less than 3 bytes left).
The `!eof` pattern fails if the parser reaches the end of the input prematurely, converting it into a failure condition that can be handled.

If a parser that can fail is called, the prefix `if` must also be used to mark the call as fallible:
```
def tag = u8 is 0x00..0x7f
def structure = {
  signature: if tag
  field: u16l
}
```
This is not required if the parser itself is a block that contains fallible expressions, as the fallibility can already be seen by looking inside the block.

Besides failing, it is also possible for a parser to error.
An error is a condition that is not expected to happen, like a division by zero or an out-of-bounds access, and it cannot be handled.
A fallible parser can be called with `expect` instead of `if` to turn any failing into hard errors.
```
def foo = {
  signature: if tag
  # we already expect the signature to match, so if the following
  # does not match, it is an error
  some_other_data: expect tag
}
```

To summarize, there are four conditions:
* A parser can succeed, returning a value.
* A parser can throw an `eof` if it reaches the end of input, which can be handled with the `!eof` pattern.
* A parser can fail, which can be converted into an error with an `expect` call.
* A parser can error, which is a condition that is not expected to happen and cannot be handled.

Function Arguments
------------------
Given that this is a parser combinator language, it is imperative to be able to define higher order functions.
Arguments can be specified in parentheses after the name of the parser:
```
def biased_int(int_parser: ~int, bias: int) = {
  parsed_int: int_parser
  let return = parsed_int + bias
}
```

The parser combinator can then be applied by writing the arguments in parentheses after the name, like `biased_int(u16l, 42)`.
We can also partially apply function arguments by writing two dots after the last given argument, like in this (a bit nonsensical) example:
```
def biased_int(int_parser: ~int, bias: int) = {
  parsed_int: int_parser
  let return = parsed_int + bias
}

def apply_with_u16l(applied_int: (int) -> ~int) = {
  val: i16l
  return: applied_int(val)
}

def runtime_bias_u16l = apply_with_u16l(biased_int(u16l, ..))
```

Cases
-----
Barely any binary format is just a fixed sequence of fields.
Say we have an encoding of integers in the range `0x0000`-`0x7FFF` where many are small so we want to save space by encoding them in a single byte:
1. Integers from `0x0000` to `0x007F` are encoded as a single byte of the form `0xxxxxxx`.
2. Integers from `0x0080` to `0x7FFF` are encoded as two bytes of the form `1xxxxxxx xxxxxxxx`, in little endian.

This can be implemented as follows:
```
def small_int = {
  lo: u8
  case
  | let return = lo if 0x00..0x7f
  | hi: u8
    let return = (lo & 0x7f) | (hi << 7)
  \
}
```
The `case` initiates a case, and each `|` initiates a branch.
The individual branches are tried in order until one succeeds.
If all branches fail, the whole case fails.
After the last branch, a `\` is used to indicate that the case ends.
Note that this is different to Prolog: `{small_int, if u8 is 0}` does not try the other branch in `small_int` if `u8 is 0` fails; it just fails the whole parser.

Let's see how this works with some examples:
1. When parsing `42 55 10 ..`, the `lo` field is first parsed, evaluating to `0x42`.
   Next, the case is entered and the `if` expression is evaluated.
   `lo` is `0x42`, which matches the pattern `0x00..0x7f`, so the value `0x42` is returned.
   The parser after the `small_int` would continue parsing at `55 10 ..`.
2. When parsing `aa 55 10 ..`, the `lo` field is first parsed, evaluating to `0xaa`.
   Next, the case is entered and the `if` expression is evaluated.
   `lo` is `0xaa`, which does not match the pattern `0x00..0x7f`, so the expression fails.
   The whole first branch fails, and the next branch is tried.
   In the second branch, the `hi` field is parsed, evaluating to `0x55`.
   The `return` expression is evaluated, which sets the return value to `0x2aaa`.
   The parser after the `small_int` would continue parsing at `10 ..`.

If a `return` is in a branch, it must be in every branch of that case.
However if there is no `return` in a block then the fields inside each branch become optional (except if they are in every branch):
```
def maybe(f: ~int) = {
  case
  | some: if f
  | {}
  \
}
```
This returns a structure containing a single `some` field with the output of `f` if it succeeds, or an empty structure if it fails.
Note that we are using an empty block here to get a zero-length parser, but we could also use the `nil` parser from the prelude which has the same purpose.

### Optional Fields

Optional fields can be accessed either with `block?.field` (which fails) or `block.field` (which errors):
```
def tag = if u8 is 0x00..0x7f

def small_int = {
  small: maybe(tag)
  case
  | let return = small?.some
  | return: u16l
  \
}
```

Fields can also be checked with `is`/`if`:
```
def small_int = {
  case
  | small: if maybe(tag) is some
    # this cannot fail as the existence of the `some` field was checked by the `if`
    # so we won't get an error
    let return = small.some
  | return: u16l
  \
}
```

### `then` and `else`
There are two binary operators, `then` and `else`, that allow handling failing within one expression.
- `a then b` first evaluates `a` and if it succeeds, it then evaluates and returns the value of `b`.
- `a else b` first evaluates `a` and if it succeeds, it returns the value of `a`.
  Otherwise, it evaluates and returns the value of `b`.

`then` has lower precedence compared to `else`, so it can essentially be used to replicate the functionality of `if` statements:
`x if 0 then foo else bar` would evaluate `foo` if `x` is `0`, and to `bar` otherwise.

As a convenience, there's also the `when` function, which fails if the argument is false:
`when?(foo > bar) then foo else bar` would evaluate `foo` if `foo` is greater than `bar`, and to `bar` otherwise.

Generics
--------

In the previous section, we defined
```
def maybe(f: ~int) = {
  case
  | some: if f
  | {}
  \
}
```
which works for parsers that return an integer.
As soon as we need a parser that returns a record/struct type, we unfortunately cannot reuse that parser combinator.

If we look closer, we see that this function never actually does anything specific with the fact that the returned value is an integer.
To faciliate reuse, yabo allows definitions with generic type parameters:
```
def maybe[T](f: ~T) = {
  case
  | some: if f
  | {}
  \
}

def uses_multiple_types = {
  a: maybe(u8 is 0)
  b: maybe({
    case
    | a: if u8 is 0
    | b: u16l
    } is b
  )
  c: maybe(maybe(u8 is 0) is some)
}
```

We can even be more generic with the definition of `maybe`!
Remember how by default, parser definitions parse bytes?
We annotate the parsed array by prefixing the definition name with a type and a `~>` after, like `def S ~> name = {}`.
This actually works with the `~T` syntax too, so if we want to be explicit that it's a byte parser, we can define it as `def ~name = {}`, but if no annotation is given it defaults to a byte array.
For the `maybe` parser to be generic, it can be defined like this:
```
def []S ~> maybe[T, S](f: []S ~> T) = {
  case
  | some: if f
  | {}
  \
}
```

Notice how we needed to change the argument type of `f` as well to match the fact that it is now used on arrays of type `S`.

Recursive Structures and Thunks
-------------------------------
One unusual aspect of yabo is the absence of recursive values.
This makes yabo Turing incomplete as every type can only contain finitely many values.
However, this is precisely what allows yabo to avoid heap allocations and garbage collection.

To still make it possible to use this language for recursive binary formats, there is a pointer-like construct that allows indirection, called a thunk.
When we parse a `u16l`, what is returned is not actually the value itself but is a thunk containing all the arguments to the `u16l` parser.
The arguments to the `u16l` consist of just the `[]u8` at the current position, which is represented by a pair of pointers (most of the time).
The type of this thunk is still `int`, but the run-time representation is different.
The thunk gets evaluated into the `int` value whenever it is used, which is when an operator like `+` needs it or it is passed as a function argument.

How does one define recursive structures then?
As an example, take a contiguous recursive list:
```
def list[T](f: ~T) = {
  case
  | head: if f
    tail: list(f)
  | {}
  \
}
```
If we naively tried to construct the resulting value without using thunks, we would end up with arbitrarily deep nested structures like (leaving `head` out), `{tail: {tail: {tail: {}}}}`.
However, since `list` is a thunk, the value is not actually recursive and the `tail` field just has the `list` thunk containing a `[]u8` slice that indicates where the next element starts, and the parser `f`.

For example let's access the thunk returned by the list.
```
def byte_3_of_c_string = {
  s: list(u8 is 0x01..0xff)
  # zero terminator
  u8
  let return = s?.tail?.tail?.tail?.head
}
```
If we evaluate `list(u8 is 0x01..0xff)` (a C string) applied to `41 62 63 64 00`, we would only get back the thunk as a value, but it would still evaluate the parts needed for the length of the whole, as it still has to find out the end of the string.
When evaluating the thunk that was returned earlier from the `list` parser, there's no need to recurse.
This is because we do not need the length of the list to get the values of both fields.
For `head`, `u8 is 0x01..0xff` does get evaluated, but the `tail` field is again just a `list` thunk and needs no further evaluation.

You can think of them kind of like pointers in C: they allow not holding the whole list as a single value inside a variable, just an address.
The address holds all the necessary information to calculate/load the current values behind the pointer.

If you use the `fun` keyword instead of the `def` keyword, no thunk will be returned.
It is meant more for calculations than data definitions.
For example, the following calculates the sum of all bytes in a list:
```
fun ~sum(f: ~int, acc: int) = {
  | head: f is !eof
    return: sum_list(f, acc + head)
  \ let return = acc
}
```
A `def` would be unnecessary here since a thunk would be returned.
If we do not immediately evaluate it but instead evaluate the thunk multiple times later whenever accessing it, that would result in unnecessary work.

Note that we are using the `fun ~foo` syntax with an extra `~` here that we are usually not using with `def`s because they're redundant.
In the case of `fun`s they're actually not redundant, as we can write functions that are not parsers.

Functions that are not Parsers
------------------------------
In order to define a function that is not a parser, we use the `fun` keyword, but leave out the `~`:
```
fun square(n: int) = n * n
fun cube(n: int) = square(n) * n
```
Of course, we also want to use `let` to bind variables, but we cannot use `{` and `}` for this, as those define parsers.
Instead, we use `(` and `)` with `let` bindings inside them:
```
fun cube(n: int) = (
  let square = n * n
  square * n
)
```
We can also define blocks with fields this way, and utilize cases:
```
fun maybe_zero(n: int) = (
  case
  | let zero = n if 0
  | let nonzero = n
  \
)

fun square(n: int) = maybe_zero(n)?.zero else (n * n)
```

Note also that the operator `?` to mark something as backtracking needs to be placed before the application if the functions would backtrack:
```
fun when(condition: bit) = condition if true
fun foo() = when?(1 == 2) then 1 else 2
```

Arrays
------
if the parser has a constant size it can be used inside an array:
```
def pascal_string = {
  len: u8
  return: [len]u8
}
```
`[len]foo` takes a parser (`foo`) and the length of the array (`len`), and returns a parser for an array of that length with individual elements from the parser.
If `foo` is of type `T ~> R`, then `[len]foo` is of type `T ~> []R`.

In order for the compiler to infer that two branches are of the same length (which is a requirement for the length to be constant), it has the ability to reason about polynomial equality.
For example, the following would have a constant size:
```
def const_sized(a: int, b: int) = {
  | u8[a * a]
    u8[2 * a * b]
    u8[b * b]
  \ u8[(a + b) * (a + b)]
}
```
as a² + 2ab + b² = (a + b)² (binomial formula).

The following also works:
```
def const_sized2[T, R](f: ~T, g: ~R) = {
  | f, f, g
  | f, g, f
  \ g, f, f
}
```
(Of course, these parsers are useless as they will always take the first branch.)

We can also nest arrays:
```
def matrix = {
  width: u8
  height: u8
  return: [height][width]u8
}
```

If the parser has a constant size, we can get the length of the parser without applying it by using `.sizeof`:
```
def padded_to_1024 = {
  let parser = const_sized2(u8, u16l)
  return: parser
  [1024 - parser.sizeof]u8
}
```

In many cases, the parser underlying the array will be of the same type as the parsed array (in most cases, `u8`), so instead of using `[len]~` (which frankly does look a bit silly) we can just write `[len]`:
```
def padded_to_1024 = {
  let parser = const_sized2(u8, u16l)
  return: parser
  [1024 - parser.sizeof]
}
```

The `.sizeof` operator can also be applied to arrays and will return the length of the array:
```
def padded_to_1024 = {
  len: u8
  field: [len]
  [1024 - (field.sizeof + 1)]u8
}
```

### `[..]parser`
Sometimes we do not have a specific length we want the array to be, but instead parse until the end of the current input.
In this case, we can use `[..]parser`, and the shortened version of `[..]~` is `[..]`:
```
fun ~rgb = {
  red: u8
  green: u8
  blue: u8
}

def rgb_array = {
  colors: [..]rgb
  rest: [..]
}
```

Each `rgb` value is 3 bytes, and `[..]rgb` makes the array as long as possible.
If the input is 16 bytes, this would mean that the `colors` field of `rgb_array` would contain 5 rgb values (as 5 \* 3 = 15), and the `rest` field would contain just one value, the last byte.

### Operators working with arrays
The `~>` operator allows for the application of a parser to an array:
```
def padded_to_1024 = {
  array: [1024]u8
  let return = array ~> const_sized2(u8, u16l)
}
```
Similarly, `|>` allows composition of parsers:
```
def padded_to_1024 = [1024]u8 |> const_sized2(u8, u16l)
```

`|>` actually just desugars to a call to the following combinator:
```
fun []A ~> compose[A](a: []A ~> []B, b: []B ~> C) = {
  x: if a, let return = x ~> if b
}
```

We can also index into arrays using the `.[index]` operator:
```
def first_byte = {
  array: [1024]u8
  let return = array.[0]
}
```

Regexes
-------
If we want to match a sequence of bytes specified by a regular expression, we can use the `/.../` syntax:
```
def c_string = if /[^\x00]*\x00/
```
Regex parsers are of type `[]u8 ~> []u8` (i.e., they take a byte array and return a byte array, advancing the input until the end of the match).
Just like with the `is` operator, they may backtrack so they generally need an `if`/`expect` when called directly.
The syntax is the same as the regex crate, documented [here](https://docs.rs/regex/latest/regex/).

There is also a regex variant prefixed with `h` that matches hexadecimal strings:
```
def elf = {
  magic: if h/7f 45 4c 46/
  other_fields: [..]
}
```

The `at` Operator
-----------------

The `at` operator allows parsing at a specific location in the input file, specified via an integer address:
```
fun ~u8ptr[T](parser: ~T) = {
  addr: u8
  let return = parser at addr
}

def linked_list = {
  case
  | if u8 is 0
  | next: if u8ptr(linked_list)
  \
  val: u8
}
```
Here in this example, we define a linked list with byte addresses and byte values.
The `u8 is 0` ensures that we do not have a `next` field if the address is 0; otherwise we parse an `u8ptr(linked_list)`.
Upon closer inspection of the `u8ptr` parser, we see how the `at` operator is used — it takes an address to the right side and a parser to the left side and returns the parsed value at the address.

The `at` operator evaluates eagerly.
However, since `linked_list` is defined via `def` and not `fun`, the `linked_list` parser is a thunk, and `parser at addr` only returns the thunk which consists solely of the address.

If the address is out of range, the `at` operator returns an error.

It's also possible to use the `at` operator with fallible parsers by writing `parser if at addr` or `parser expect at addr`.

The `span` Operator
-------------------

The `span` operator allows getting an array spanning a range of the input corresponding to a range of fields in the current block.
For example, say we have a function `fun crc32(bytes: []u8): int = ...` for calculating a CRC32 checksum and want to have the expected checksum for a PNG chunk:
```
def png_chunk = {
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
def foo = {
  case
  | bar: u8 is 1
  | bar: u8
  \
  baz: u8
  let spanned = span bar..baz
}
```

`span` can also be useful if you need to pad a structure to a certain alignment:
```
def padded_to_1024_align = {
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
fun []int ~> sum(acc: int) = {
  case
  | val: if ~ is !eof
    return: sum(acc + val)
  | let return = acc
  \
}

fun range_sum(n: int) = 0..<n
                     ~> sum(0)
```

Together with `[..]`, this can be combined to a sort of loop:
```
fun bits_of(n: int, bit_count: int) = (0..<bit_count) ~> [..]{
  i: ~
  let return = (n >> i) & 1 != 0
}
```
Note what we are doing here:
The block is a parser of length 1 that takes an integer `i` and returns whether the `i`-th bit of `n` is set.
The `[..]` then maps this parser over each element of the range `0..<bit_count`, returning array containing a boolean for each bit of `n`.

More comfortably, once may use lambdas (which are written as `<arg1, arg2> expr`) and the map function from the core library:
```
fun bits_of(n: int, bit_count: int) = (0..<bit_count) ~> map(<i> (n >> i) & 1 != 0)
```

Imports
-------
The `import` keyword allows importing other files.
It is a top-level statement and takes a single identifier as an argument:
```
import text
def num = {
  case
  | if /0x/, return: text.basenum(16)?
  | return: text.basenum(10)?
  \
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
| `.field`, `?.field` | Postfix Unary | Access field       |
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
| `[bar]foo` | Parser      | Array of length `bar` |
| `foo.[bar]`, `foo?.[bar]` | Array | Index into array |
| `..<`    | Array         | Make range from lhs to rhs - 1 |
| `foo(bar, baz)` | Function Application | Call function `foo` with arguments `bar` and `baz` |
| `foo(bar, ..)` | Function Application | Call function `foo` with arguments `bar`, currying the rest |
| `foo`    | Identifier    | Refer to a value    |
| `true`, `false` | Literal | Boolean literals   |
| `0x..`, `0..` | Literal  | Hexadecimal, Decimal literals |
| `/.../`, `h/.../` | Literal  | Create regex parser |
| `span foo..bar` | Literal | Create span array of fields from `foo` to `bar` |
| `~`      | Literal       | Single value parser |
| `{...}`  | Block         | Create block parser |
| `( ... )`| Inline Block  | Create block that is immediately evaluated |

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
