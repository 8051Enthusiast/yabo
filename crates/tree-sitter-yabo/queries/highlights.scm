[
    "["
    "]"
    (block_open)
    (block_close)
    (parser_block_open)
    (parser_block_close)
    "("
    ")"
] @punctuation.bracket

"," @punctuation.delimiter

(comment) @comment.line

[
    "bit"
    "int"
    "char"
] @type.builtin

[
    "import"
    "export"
] @keyword.control.import

"sizeof" @keyword.operator

[
    "if"
    "else"
    "then"
] @keyword.control.conditional

"try" @keyword.control.exception

(retvrn) @variable.builtin

[
    "or"
    "and"
    "at"
] @keyword.operator

[
    "fun"
    "def"
    "static"
] @keyword.function

[
    "true"
    "false"
    (not_eof)
] @constant.builtin

[
    "="
    "+"
    "-"
    "*"
    "/"
    "%"
    "^"
    ">"
    "<"
    "=="
    "<="
    ">="
    "!="
    "&"
    "|"
    "^"
    ">>"
    "<<"
    "*>"
    "|>"
    "&>"
    "."
    ".."
    ".["
    ".?"
    ":"
    "?"
    "!"
    "~"
] @operator

(char_literal) @constant.character

(number_literal) @constant.numeric.integer

(regex_literal) @string.regexp

(type_var) @type
(parserdef_ref
    name: (identifier) @type)

(val_dot
    right: (identifier) @variable.other.member)

(parser_definition
    name: (identifier) @function)

(arg_definition
    name: (identifier) @variable.parameter)

(import
    name: (identifier) @namespace)

((identifier) @function.method
  (#is-not? local))

(identifier) @variable

"let" @keyword.storage.type
