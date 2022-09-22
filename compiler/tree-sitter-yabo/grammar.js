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
  ARGS: 13,
};
module.exports = grammar({
  name: 'yabo',

  extras: $ => [
    /\s/,
    $._comment,
  ],

  word: $ => $.identifier,

  externals: $ => [
    $._newline,
    $._indent,
    $._dedent,
    $._block_open,
    $._block_close,
    $._lexer_error,
    // unused
    $._notoken,
  ],

  rules: {
    source_file: $ => repeat($._definition),
    _definition: $ => choice(
      $.parser_definition
    ),
    _comment: $ => token(seq('#', /[^\n]*/)),
    parser_definition: $ => seq(
      optional(field('qualifier', 'export')),
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
      $.type_constraint,
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
    ),
    unary_type_expression: $ => prec(PREC.UNARY, seq(
      field('op', choice('*>')),
      field('right', $._type_expression)
    )),
    parser_block: $ => seq(
      $._block_open,
      optional(seq(
        $._indent,
        field('content', $.parser_sequence),
        $._dedent,
      )),
      $._block_close,
    ),
    parser_sequence: $ => seq(
      $._parser_sequence_element,
      repeat(seq(
        choice($._newline, ','),
        $._parser_sequence_element,
      ))
    ),
    _parser_sequence_element: $=> choice(
      $._statement,
      $.parser_choice,
    ),
    parser_choice: $ => seq(
      '|',
      $._indent,
      field('content', $.parser_sequence),
      $._dedent,
    ),
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
    constraint_apply: $ => prec.left(PREC.WIGGLE, seq(
      field('left', $._expression),
      field('op', choice('~', 'if', 'try')),
      field('right', $._constraint_expression),
    )),
    type_constraint: $ => prec.left(PREC.WIGGLE, seq(
      field('left', $._type_expression),
      field('op', '~'),
      field('right', $._constraint_expression),
    )),
    _statement: $ => choice(
      $.parse_statement,
      $.let_statement,
    ),
    parse_statement: $ => seq(
      optional(
        seq(
          field('name', $._field_name),
          ':',
        )
      ),
      field('parser', $._expression),
    ),
    let_statement: $ => seq(
      'let',
      field('name', $._field_name),
      ':',
      field('ty', $._type_expression),
      '=',
      field('expr', $._expression),
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
      $.val_dot,
      $.constraint_apply,
      $.parser_array,
      $.parser_block,
      $.single,
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
        ['else', PREC.ELSE],
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
          field('right', $._expression)
        ))
      }));
    },
    unary_expression: $ => prec(PREC.UNARY, seq(
      field('op', choice('-', '!', 'if')),
      field('right', $._expression)
    )),
    _type_atom: $ => choice(
      $.primitive_type,
      $.type_var,
      $.parserdef_ref,
    ),
    val_dot: $ => prec.left(PREC.DOT, seq(
      field('left', $._expression),
      field('op', '.'),
      field('right', $._atom),
    )),
    parserdef_ref: $ => prec.left(PREC.ARGS, seq(
      optional(
        seq(
          field('from', $._type_expression),
          '&>'
        )
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
    )),
    _atom: $ => choice(
      $._field_name,
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
    single: $ => '~',
    type_var: $ => /\'[A-Za-z_][A-Za-z_0-9]*/,
    _field_name: $ => choice(
      $.identifier,
      $.retvrn,
    ),
    retvrn: $ => 'return',
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
