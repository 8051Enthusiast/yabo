const PREC = {
  DEFAULT: 0,
  COMPARE: 1,
  OR: 2,
  XOR: 3,
  AND: 4,
  SHIFT: 5,
  ADD: 6,
  MULTIPLY: 7,
  PARSE: 8,
  UNARY: 9,
  WIGGLE: 10,
  ELSE: 11,
  DOT: 12,
};
module.exports = grammar({
  name: 'yabo',

  rules: {
    source_file: $ => repeat($._definition),
    _definition: $ => choice(
      $.parser_definition
    ),
    parser_definition: $ => seq(
      'def',
      field('from', $._type_expression),
      '*>',
      field('name', $.identifier),
      '=',
      field('to', $._expression),
    ),
    _type_expression: $ => choice(
      $.binary_type_expression,
      $.unary_type_expression,
      $.type_array,
      $._type_atom,
      seq('(', $._type_expression, ')'),
    ),
    binary_type_expression: $ => choice(
      prec.left(PREC.PARSE, seq(
        field('left', $._type_expression),
        field('op', '*>'),
        field('right', $._type_expression),
      )),
      prec.left(PREC.WIGGLE, seq(
        field('left', $._type_expression),
        field('op', '~'),
        field('right', $._constraint_expression),
      )),
    ),
    unary_type_expression: $ => prec(PREC.UNARY, seq(
      field('op', choice('*>')),
      field('right', $._type_expression)
    )),

    _expression: $ => choice(
      $.binary_expression,
      $.prefix_expression,
      seq('(', $._expression, ')'),
      $._atom,
    ),
    parser_block: $ => seq(
      '{',
      optional(
        field('content', $._parser_block_content),
      ),
      '}',
    ),
    _parser_block_content: $ => choice(
      $.parser_choice,
      $.parser_sequence,
    ),
    parser_sequence: $ => repeat1(choice(
      $._statement,
      seq('(', $._parser_block_content, ')'),
    )),
    parser_choice: $ => prec.left(1, seq(
      field('left', $._parser_block_content),
      '|',
      field('right', $._parser_block_content),
    )),
    type_array: $ => seq(
      field('direction', choice('for', 'each')),
      '[',
      field('expr', $._type_expression),
      ']',
    ),
    parser_array: $ => seq(
      field('direction', choice('for', 'each')),
      '[',
      field('expr', $._expression),
      ']',
    ),
    _statement: $ => choice(
      $.parse_statement,
      $.let_statement,
    ),
    parse_statement: $ => seq(
      optional(
        seq(
          field('name', $.identifier),
          ':',
        )
      ),
      field('parser', $._expression),
      ',',
    ),
    let_statement: $ => seq(
      'let',
      field('name', $.identifier),
      ':',
      field('ty', $._type_expression),
      '=',
      field('expr', $._expression),
      ',',
    ),
    _constraint_expression: $ => choice(
      $.binary_constraint_expression,
      $.unary_constraint_expression,
      $._atom,
    ),
    binary_constraint_expression: $ => choice(
      prec.left(PREC.OR, seq(
        field('left', $._constraint_expression),
        field('op', 'or'),
        field('right', $._constraint_expression),
      )),
      prec.left(PREC.AND, seq(
        field('left', $._constraint_expression),
        field('op', 'and'),
        field('right', $._constraint_expression),
      )),
    ),
    unary_constraint_expression: $ => seq('!', $._atom),
    _expression: $ => choice(
      $.binary_expression,
      $.unary_expression,
      $.parser_array,
      $.parser_block,
      seq('(', $._expression, ')'),
      $._atom,
    ),
    fun_application: $ => seq(
      $._expression,
      '(',
      $._expression,
      ')',
    ),
    binary_expression: $ => {
      const table = [
        ['.', PREC.DOT],
        ['else', PREC.ELSE],
        ['~', PREC.WIGGLE],
        ['*>', PREC.PARSE],
        ['|>', PREC.PARSE],
        ['+', PREC.ADD],
        ['-', PREC.ADD],
        ['*', PREC.MULTIPLY],
        ['/', PREC.MULTIPLY],
        ['%', PREC.MULTIPLY],
        ['<<', PREC.SHIFT],
        ['>>', PREC.SHIFT],
        ['==', PREC.COMPARE],
        ['!=', PREC.COMPARE],
        ['>', PREC.COMPARE],
        ['>=', PREC.COMPARE],
        ['<=', PREC.COMPARE],
        ['<', PREC.COMPARE],
        ['|', PREC.OR],
        ['^', PREC.XOR],
        ['&', PREC.AND],
      ];

      return choice(...table.map(([operator, precedence]) => {
        return prec.left(precedence, seq(
          field('left', $._expression),
          field('op', operator),
          field('right', operator == "~" ? $._constraint_expression : $._expression)
        ))
      }));
    },
    unary_expression: $ => prec(PREC.UNARY, seq(
      field('op', choice('-', '!', '+', 'if')),
      field('right', $._expression)
    )),
    _type_atom: $ => choice(
      $.primitive_type,
      $.typevar,
      $.parserdef_ref,
    ),
    parserdef_ref: $ => seq(
      optional(
        field('from', $._type_expression),
      ),
      field('name', $.identifier),
      optional(
        seq(
          '[',
          repeat(
            seq(
              field('args', $._type_expression),
              ',',
            )
          ),
          ']',
        )
      ),
    ),
    _atom: $ => choice(
      $.identifier,
      $._literal,
    ),
    _literal: $ => choice(
      $.number_literal,
      $.char_literal,
    ),
    primitive_type: $ => choice(
      'int',
      'bit',
      'char',
      'mem',
    ),
    typevar: $ => /\'[A-Za-z_][A-Za-z_0-9]*/,
    identifier: $ => /[A-Za-z_][A-Za-z_0-9]*/,
    number_literal: $ => /[0-9]+|0x[0-9a-fA-F]+|0b[01]+|0o[0-7]+/,
    char_literal: $ => seq(
      '\'',
      choice(
        token.immediate(/[^\n']/)
      ),
      '\'',
    ),
  }
});
