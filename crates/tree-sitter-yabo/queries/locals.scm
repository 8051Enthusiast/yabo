; scopes
(parser_definition) @local.scope

(parser_block) @local.scope


; definitions
(arg_definition
    name: (identifier) @local.definition)

(generic_param_list
    args: (identifier) @local.definition)

(let_statement
    name: (identifier) @local.definition)

(parse_statement
    name: (identifier) @local.definition)

(import
    name: (identifier) @local.definition)


; references
(identifier) @local.reference