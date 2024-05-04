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
The goal is to fuck around and find out.
If this ends up a good language, then i didn't fuck around enough.

Parser Definitions
------------------

Let's start with a simple example:
```
def *u16_pair = {
  fst: u16l
  snd: u16l
}
```
As you might guess this defines a parser `u16_pair` with two fields, both containing `u16l`.
The `u16l` stands for <b>u</b>nsigned <b>16</b>-bit integer, <b>l</b>ittle endian.
We can also use `u16b` for the big endian version, or `i16l` for the signed version.

Now you might ask what the `*` is for.
For that, an explanation of what parser combinators are is necessary:

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

In yabo, we notate a parser type like `[u8] -> (type, [u8])` as `[u8] *> type`.
`*type` is a short form for `[u8] *> type`, because bytes are what we are parsing most of the time.

It is important to recognize that blocks `{...}` are expressions that produce parsers.
For example, these examples are possible as well:
```
def *nested_block = {
  outer: {
    inner: u16l
  }
}

def *no_block = u16l
```

There are two speical built-in parsers available: `~` and `+`:
* `~` is a parser that returns a single element of the input array and advances the position by one.
* `+` parses zero elements and returns the unit value.

Let Bindings
------------
In some situations, we might have fields that need to have their value calculated from other fields.
For example, say we have two interleaved fields, and we want to access their values as a single field:
```
def *interleaved = {
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
def *interleaved = {
  let a = a_lo | (a_hi << 8)
  a_lo: u8
  b_lo: u8
  let b = b_lo | (b_hi << 8)
  a_hi: u8
  b_hi: u8
}
```
The intent of this is to allow the parser to run in the backwords direction, which will hopefully get implemented sometime in the future.

Remember how blocks are expressions?
This means we can assign it to a `let` binding and then reuse it:
```
def *u16_quad = {
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
In this case we can specify a pase without specifying a field name:
```
def *padded = {
  small: u16l
  u8
  u8
  large: u32l
}
```

Because writing a new line every time we want to add a parser takes unnecessary space, we can also separate parsers with commas:
```
def *padded = {
  small: u16l
  u8, u8
  large: u32l
}
```
or even
```
def *padded = {small: u16l, u8, u8, large: u32l}
```

When parsing the string `01 02 03 04 05 06 07 08`, the value of `padded` will be `{small: 0x0201, large: 0x08070605}`.


Return
------
Sometimes we want to return values that are not structs.
In this case, we can use the `return` keyword, which can be used in positions where normally an identifier would be expected:
```
def *u16l = {
  lo: u8
  hi: u8
  let return = lo | hi << 8
}
```
All the other fields are ignored and just the value of `return` is returned.

Indeed, this is how `u16l` is implemented in the standard library.
(You might ask how `u8` is implemented and the answer to that is simply `fun *u8 = ~`.
the difference between `def` and `fun` will be explained later, but they're mostly the same.)

We can also write return in place of a field name:
```
def *skip_four_bytes_u16l = {
  ignored_field: u8
  u8, u8, u8
  return: u16l
}
```

Specifying Types
----------------
Sometimes we want to specific the return type of parser definition or the type of a let binding.
In this case we can write the type after a colon:
```
def *u16l: int = {
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
| `foo *> bar`    | parser that takes a `foo` and returns an `bar` (advancing `foo`)    |
| `*foo`          | desugars to `[u8] *> foo`                                           |
| `foo(bar, baz)` | function that takes `bar` and `baz` and returns an `foo`, see below |
| identifier      | refers to a parser definition (for example `u16_pair` refers to the value returned by the `u16_pair` parser) |

Function Arguments
------------------
Given that this is a parser combinator language, it is important to be able to define higher order functions.
Arguments can be specified in parens after the name of the parser:
```
def *biased_int(int_parser: *int, bias: int) = {
  parsed_int: int_parser
  let return = parsed_int + bias
}
```

The parser combinator can then be applied by writing the arguments in parens after the name, like `biased_int(u16l, 42)`.
We can also partially apply function arguments by writing two dots after the last given argument, like in this (a bit non-sensical) example:
```
def *apply_with_u16l(applied_int: *int(int)) = {
  val: i16l
  return: applied_int(val)
}
def *runtime_bias_u16l = apply_with_u16l(biased_int(u16l, ..))
```
Note the `*` before `int` in the type of `bias_int`:
The `*` binds thighter than the function argument types, so `*int(int)` takes an integer, and returns a byte parser returning an integer.



Choices and Failing
-------------------
Barely any binary format is just a fixed sequence of fields.
Say we have an encoding of integers in the range `0x0000`-`0x7FFF` where many are small so we want to save space by encoding them in a single byte:
1. Integers from `0x0000` to `0x007F` are encoded as a single byte of the form `0xxxxxxx`.
2. Integers from `0x0080` to `0x7FFF` are encoded as two bytes of the form `1xxxxxxx xxxxxxxx`, in little endian.

This can be implemented as follows:
```
def *small_int = {
  lo: u8
  | let return = lo if 0x00..0x7f

  | hi: u8
    let return = (lo & 0x7f) | (hi << 7)
}
```
There are two new things here:
The `|` initiates a choice, which means the invididual branches are tried in order until one succeeds.
If all branches fail, the whole choice fails.
The extend of one branch is whitespce sensitive and ends at the next dedent, `|`, or end of block.

How does a branch fail?
One way is the `if` expression:

The value to the left side is checked against the pattern on the right side and if it matches, the value is returned.
If it does not match, the expression fails, which means the whole branch fails.

Let's see how this works with some examples:
1. When parsing `42 55 10 ..`, the `lo` field is first parsed, evluating to `0x42`.
   Next, the choice is entered and the `if` expression is evaluated.
   `lo` is `0x42`, which matches the pattern `0x00..0x7f`, so the value `0x42` is returned.
   The parser after the `small_int` would continue parsing at `55 10 ..`.
2. When parsing `aa 55 10 ..`, the `lo` field is first parsed, evluating to `0xaa`.
   Next, the choice is entered and the `if` expression is evaluated.
   `lo` is `0xaa`, which does not match the pattern `0x00..0x7f`, so the expression fails.
   The first whole branch fails, and the next branch is tried.
   In the second branch, the `hi` field is parsed, evluating to `0x55`.
   The `return` expression is evaluated, which returns `0x2aaa`.
   The parser after the `small_int` would continue parsing at `10 ..`.

The `if` expression can also be applied directly to parsers, in which case a new parser is created that fails if the original parser does not return a value that matches the pattern:
```
def *small_int = {
  | return: u8 if 0x00..0x7f
  | lo: u8
    hi: u8
    let return = (lo & 0x7f) | (hi << 7)
}
```

On parser specifically, we can also use the `!eof` condition:
```
def *default_value_if_eof = {
  | return: u8 if !eof
  | let return = 0
}
```
Normally, `eof` is an error condition, but with `if !eof` this can be used to backtrack.
Here, the `return` field is only parsed if there is still data left, otherwise it returns `0`.

If a parser as the whole can fail, the caller is required to mark it with the `?` operator:
```
def *tag = u8 if 0x00..0x7f
def *big_structure = {
  | tag?
    return: some_complex_parser
  | return: some_other_complex_parser
}
```
As we can see from the example before that, this is not required if the parser is an `if` expression or it is a block, as the fallibility can already be seen by looking at the current parser definition.
The reasoning behind this is that we want to avoid checking that `some_complex_parser` fails when determining the branch, as that would require parsing the whole structure.

A fallible parser can be made non-fallible by using the `!` operator:
```
def *foo = {
  | u8 if 0..0x7f
    # assume we already know this is the right branch
    # so we want `tag` to fail if it does not match
    return: tag!
  | return: u16l
}
```

If a `return` is in a branch it must be in every branch, however if there is no `return` in a block then the fields inside branches essentially become optional (except if they are in every branch):
```
def *maybe(f: *'t) = {
  | some: f?
  # remember: `+` parses zero elements and returns the unit value
  | +
}
```
This returns a structure containing a single `some` field with the output of `f` if it succeeds, or an empty structure if it fails.

### Optional Fields

Optional fields can be accessed either with `block.?field` (which fails) or `block.!field` (which errors):
```
def *small_int = {
  small: maybe(tag?)
  | let return = small.?some
  | return: u16l
}
```

Fields can also be checked with `if`:
```
def *small_int = {
  | small: maybe(tag?) if some
    # this cannot fail as the existence of the `some` field was checked by the `if`,
    # so we just use `!` for the sake of this example
    let return = small.!some
  | return: u16l
}
```
### Multiple Choices

If we have multiple choices one after another, we need to insert something that separates them so they are not parsed as one big choice.
The `+` parser is very handy for this:
```
def *multiple_choice = {
  | a: foo?
  | b: bar
  +
  | c: baz?
  | d: qux
}
```
As `+` has a length of zero, the two choices are right after one other.

`if` patterns can be combined with `and` and `or`, like `multiple_choice if a and c or b and d`.
Parens are not allowed to keep it in disjunctive normal form for easier semantic analysis.

### `then` and `else`
There are two binary operators, `then` and `else`, that allow handling backtracking within one expression.
`a then b` evaluates `a` and if it doesn't fail, evaluates `b` and returns the value of `b`.
`a else b` evaluates `a` and if it succeeds, returns the value of `a`, otherwise evaluates `b` and returns the value of `b`.
`then` has lower precedence than `else`, so it can essentially be used to replicate `if` statements:
`x if 0 then foo else bar` would evaluate `foo` if `x` is `0`, and `bar` otherwise.

Recursive Structures and Thunks
-------------------------------
One unusual aspect of yabo is the absence of recursive values.
This makes yabo turing incomplete as every type can only contain finitely many values.
However this is precisely what allows yabo to avoid allocations and garbage collection.

There is however a pointer-like construct that allows indirection, called a thunk.
When we parse a `u16l` what is returned is not actually the value itself but is a thunk containing all the arguments to the `u16l` parser.
The arguments to the `u16l` consist of just the `[u8]` at the current position, which is represented by a pair of pointers most of the time.
The type of this thunk is `u16l` (which is different to the type of the parser `u16l`, which is actually `[u8] *> u16l` - one `u16l` is a value, and the other a type).
Values of type `u16l` can be used in any place where an `int` is expected, as `u16l` is a subtype of `int`.

How does one define recursive structures then?
As an example, take a contiguous recursive list:
```
def *list(f: *'t) = {
  | head: f?
    tail: list(f?)
  | +
}
```
If we naively tried to construct the resulting value without using thunks, we would end up with arbitrarily deep nested structures like (leaving `head` out) `{tail: {tail: {tail: {}}}}`.
However since `list` is a thunk, the value is not actually recursive and the `tail` field just contains the `[u8]` slice where the next element starts, and the parser `f`.

For example let's access the thunk returned by the list.
```
def *byte_3_of_c_string = {
  s: list(u8 if 0x01..0xff)
  # zero terminator
  u8
  let return = s.?tail.?tail.?tail.?head
}
```
If we evaluate `list(u8 if 0x01..0xff)` (a C string) applied to `41 62 63 64 00`, we would only get back the thunk as a value, but it would still evaluate all the way as it still has to find out the end of the string.
However when evaluating the thunk that was returned earlier from the parser, there's no need to recurse.
We do not need the length of the list to get the values of both fields.
For `head`, `u8 if 0x01..0xff` does get evaluated, but the `tail` field is just a thunk and needs no further evaluation.

If you use the `fun` keyword instead of the `def` keyword, then no thunk will be returned.
This is required especially if the return type is a type variable (for example, `[u8] *> 't`).
It is meant more for calculations than data definitions.

Arrays
------
if the parser has a constant size it can be used inside an array:
```
def *pascal_string = {
  len: u8
  return: u8[len]
}
```
`parser[len]` takes a parser (`u8`) and the length of the array (`len`) and returns a parser for an array of that length with elements from the parser.
If `parser` is of type `'t *> 'r`, then `parser[len]` is of type `'t *> ['r]`.

In order for the compiler to infer that two branches are of the same length (which is required for the length to be constant), it has the ability to reason about polynomial equality.
For example, the following would be constant size
```
def *const_sized(a: int, b: int) = {
  | u8[a * a]
    u8[2 * a * b]
    u8[b * b]
  | u8[(a + b) * (a + b)]
}
```
as a² + 2ab + b² = (a + b)² (binomial formula).

The following works as well:
```
def *const_sized2(f: *'t, g: *'r) = {
  | f, f, g
  | f, g, f
  | g, f, f
}
```
(Of course, these parsers are useless as they will always take the first branch.)

We can also nest arrays:
```
def *matrix = {
  width: u8
  height: u8
  return: u8[width][height]
}
```

If the parser is constant-sized, we can get the length of the parser without applying it by using `.sizeof`:
```
def *padded_to_1024 = {
  let parser = const_sized2(u8, u16l)
  return: parser
  u8[1024 - parser.sizeof]
}
```

In many cases, the parser underlying the array will be of the same type as the parsed array (for most cases, `u8`), so instead of using `~[len]` (which frankly does look a bit silly) we can just write `[len]`:
```
def *padded_to_1024 = {
  let parser = const_sized2(u8, u16l)
  return: parser
  [1024 - parser.sizeof]
}
```

### `parser[..]`
Sometimes we do not have a specific length we want the array to be, but instead parse until the end of the current input.
In this case, we can use `parser[..]`, and the shortened version of `~[..]` is `[..]`:
```
fun *rgb = {
  red: u8
  green: u8
  blue: u8
}

def *rgb_array = {
  colors: rgb[..]
  rest: [..]
}
```

Each rgb value is 3 bytes, and `rgb[..]` makes the array as long as possible.
In case the input is 16 bytes, this would mean that the `colors` field of `rgb_array` would contain 5 rgb values (as 5 \* 3 = 15), and the `rest` field would contain just one value, the last byte.

### Operators woking with arrays
The `*>` operator allows applying a parser to an array:
```
def *padded_to_1024 = {
  array: u8[1024]
  let return = array *> const_sized2(u8, u16l)
}
```
Similarly, `|>` allows composition of parsers:
```
def *padded_to_1024 = u8[1024] |> const_sized2(u8, u16l)
```
`|>` actually just desugars to a call to this combinator:
```
fun ['a] *> compose(a: ['a] *> ['b], b: ['b] *> 'c) = {
  x: a, let return = x *> b
}
```

We can also index into arrays with `.[index]`:
```
def *first_byte = {
  array: u8[1024]
  let return = array.[0]
}
```

The `at` Operator
-----------------

The `at` operator allows parsing at a specific location in the input specified via an integer address:
```
fun *u8ptr(parser: *'t) = {
  addr: u8
  let return = parser at addr
}

export
def *linked_list = {
  | u8 if 0
  | next: u8ptr(linked_list)?
  val: u8
}
```
Here in this example, we define a linked list with byte addresses and byte values.
The `u8 if 0` makes sure that we do not have a `next` field if the address is 0, and otherwise we parse an `u8ptr(linked_list)`.
If we look closer at the `u8ptr` parser, we see how the `at` operator is used - it takes an address to the right side and a parser to the left side and returns the parsed value at the address.

The `at` operator evaluates eagerly, but since `linked_list` is defined via `def` and not `fun`, the `linked_list` parser is a thunk and `parser at addr` only returns the thunk, which consists of just the address.

If the address is out of range, the `at` operator returns an error.

The `span` Operator
-------------------

The `span` operator allows getting an array spanning a range of the input corresponding to a range of fields in the current block.
For example, say we have a function `crc32: int([u8])` for calculating a CRC32 checksum and want to have the expected checksum for a PNG chunk:
```
def *png_chunk = {
  length: u32l
  type: u32l
  data: u8[length]
  crc: u32l
  let expected_crc = crc32(span type..data)
}
```
The result of `span type..data` would be an array containing the bytes from the start of the `type` field until the end of the `data` field, meaning it is inclusive.
It can only be used inside blocks and contain fields of parse statements of the current block that are in the current scope or a parent scope.
For example, the following is not possible because `bar` is not in the top scope of the block:
```
def *foo = {
  | bar: u8 if 1
  | bar: u8
  baz: u8
  let spanned = span bar..baz
}
```