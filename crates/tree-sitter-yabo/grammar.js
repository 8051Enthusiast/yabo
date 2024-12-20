const PREC = {
  DEFAULT: 0,
  ELSE: 1,
  THEN: 2,
  COMPARE: 3,
  OR: 4,
  XOR: 5,
  AND: 6,
  SHIFT: 7,
  ADD: 8,
  MULTIPLY: 9,
  PARSE: 10,
  RANGE: 11,
  CONSTRAINT: 12,
  LPOSTFIX: 13,
  PREFIX: 14,
  POSTFIX: 15,
  ARGS: 16,
  PARSERTYPE: 17,
  PARSERDEF: 18,
  BYTE_SLICE: 19,
};
module.exports = grammar({
  name: 'yabo',

  extras: $ => [
    /\s/,
    $.comment,
  ],

  word: $ => $.identifier,

  externals: $ => [
    $._newline,
    $._indent,
    $._dedent,
    $.block_open,
    $.block_close,
    $.parser_block_open,
    $.parser_block_close,
    $._lexer_error,
    // unused
    $._notoken,
  ],

  rules: {
    source_file: $ => repeat($._top_level_statement),
    _top_level_statement: $ => choice(
      $.parser_definition,
      $.import
    ),
    comment: $ => token(seq('#', /[^\n]*/)),
    parser_definition: $ => seq(
      optional(field('qualifier', 'export')),
      field('kind', choice('def', 'fun', 'static')),
      prec(PREC.PARSERDEF, seq(
        optional(
          choice(
            seq(
              field('from', $._pd_from_type_expression),
              '~>',
            ),
            field('from', $.byte_slice),
          ),
        ),
        field('name', $.identifier),
        optional(field('type_params', $.generic_param_list)),
        optional(field('argdefs', $.arg_def_list)),
      )),
      optional(
        seq(
          ':',
          field('ret_ty', $._type_expression),
        )
      ),
      '=',
      field('to', $._expression),
    ),
    import: $ => seq(
      'import',
      field('name', $.identifier),
    ),
    _type_expression: $ => choice(
      $.type_fun_application,
      $.binary_type_expression,
      $.unary_type_expression,
      $.type_constraint,
      $.type_array,
      $._type_atom,
      seq('(', $._type_expression, ')'),
    ),
    _pd_from_type_expression: $ => choice(
      seq('(', $._type_expression, ')'),
      $.type_array,
    ),
    binary_type_expression: $ => choice(
      prec.right(PREC.PARSERTYPE, seq(
        field('left', $._type_expression),
        field('op', '~>'),
        field('right', $._type_expression),
      )),
    ),
    unary_type_expression: $ => prec(PREC.PARSERTYPE + 1, seq(
      field('op', '~'),
      field('right', $._type_expression)
    )),
    block: $ => seq(
      $.block_open,
      optional(seq(
        $._indent,
        field('content', $.parser_sequence),
        $._dedent,
      )),
      $.block_close
    ),
    parser_block: $ => seq(
      $.parser_block_open,
      optional(seq(
        $._indent,
        field('content', $.parser_sequence),
        $._dedent,
      )),
      $.parser_block_close,
    ),
    parser_sequence: $ => seq(
      field('content', $._parser_sequence_element),
      repeat(seq(
        choice($._newline, ','),
        field('content', $._parser_sequence_element),
      ))
    ),
    _parser_sequence_element: $ => choice(
      $._statement,
      $.parser_choice,
    ),
    parser_choice: $ => seq(
      repeat(seq(
        '|',
        $._indent,
        field('content', $.parser_sequence),
        $._dedent,
      )),
      seq(
        '\\',
        $._indent,
        field('content', $.parser_sequence),
        $._dedent,
      )
    ),
    type_array: $ => seq(
      '[',
      field('expr', $._type_expression),
      ']',
    ),
    constraint_apply: $ => prec.left(PREC.LPOSTFIX, seq(
      field('left', $._expression),
      field('op', choice('~', 'is', 'try')),
      field('right', $._constraint_expression),
    )),
    type_constraint: $ => prec.left(PREC.LPOSTFIX, seq(
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
      optional(seq(
        ':',
        field('ty', $._type_expression),
      )),
      '=',
      field('expr', $._expression),
    ),
    _constraint_expression: $ => choice(
      $.binary_constraint_expression,
      $.unary_constraint_expression,
      $._constraint_atom,
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
      $.fun_application,
      $.val_dot,
      $.bt_mark,
      $.constraint_apply,
      $.parser_block,
      $.block,
      $.lambda,
      $.single,
      $.parser_span,
      $.array_fill,
      $.single_constraint_apply,
      seq('(', $._expression, ')'),
      $._atom,
    ),
    _arg_list: $ => seq(
      '(',
      optional(seq(
        field('args', $._expression),
        repeat(seq(
          ',',
          field('args', $._expression),
        )),
        optional(field('partial', $.partial_indicator))
      )),
      ')'
    ),
    partial_indicator: $ => seq(
      ',', '..'
    ),
    arg_definition: $ => seq(
      field('name', $.identifier),
      optional(seq(
        ':',
        field('ty', $._type_expression),
      ))
    ),
    generic_param_list: $ => seq(
      '[',
      optional(seq(
        field('args', $.identifier),
        repeat(seq(
          ',',
          field('args', $.identifier),
        )),
      )),
      ']'
    ),
    arg_def_list: $ => seq(
      '(',
      optional(seq(
        field('args', $.arg_definition),
        repeat(seq(
          ',',
          field('args', $.arg_definition),
        )),
      )),
      ')'
    ),
    lambda_arg_def_list: $ => seq(
      '<',
      optional(seq(
        field('args', $.arg_definition),
        repeat(seq(
          ',',
          field('args', $.arg_definition),
        )),
      )),
      '>'
    ),
    lambda: $ => seq(
      field('argdefs', $.lambda_arg_def_list),
      field('expr', $._expression),
    ),
    fun_application: $ => prec(PREC.ARGS, seq(
      field('applicant', $._expression),
      $._arg_list,
    )),
    _type_arg_list: $ => seq(
      '(',
      optional(seq(
        field('args', $._type_expression),
        repeat(seq(
          ',',
          field('args', $._type_expression),
        )),
      )),
      ')'
    ),
    type_fun_application: $ => prec.right(PREC.PARSERTYPE, seq(
      $._type_arg_list,
      '->',
      field('result', $._type_expression),
    )),
    binary_expression: $ => {
      const table = [
        ['..<', PREC.RANGE],
        ['then', PREC.THEN],
        ['else', PREC.ELSE],
        ['~>', PREC.PARSE],
        ['|>', PREC.PARSE],
        ['at', PREC.PARSE],
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

      const regular_ops = choice(...table.map(([operator, precedence]) => {
        return prec.left(precedence, seq(
          field('left', $._expression),
          field('op', operator),
          field('right', $._expression)
        ))
      }));
      return choice(
        regular_ops,
        prec.left(PREC.POSTFIX,
          seq(
            field('left', $._expression),
            field('op', choice('.[', '.?[', '[')),
            field('right', $._expression),
            ']'
          )
        ),
      )
    },
    unary_expression: $ => choice(
      prec(PREC.PREFIX,
        seq(
          field('op', choice('-', '!', '>')),
          field('right', $._expression)
        ),
      ),
      prec(PREC.POSTFIX, seq(
        field('right', $._expression),
        field('op', $.array_fill)
      )),
      prec(PREC.POSTFIX, seq(
        field('right', $._expression),
        '.',
        field('op', 'sizeof')
      )),
      prec(PREC.POSTFIX, seq(
        field('op', '['),
        field('right', $._expression),
        ']'
      )),
    ),
    single_constraint_apply: $ => seq(
      field('op', choice('is', 'try')),
      field('constraint', $._constraint_expression),
    ),
    _type_atom: $ => choice(
      $.primitive_type,
      $.parserdef_ref,
      $.placeholder,
    ),
    range: $ => prec(PREC.CONSTRAINT, seq(
      field('start', $._int_literal),
      '..',
      field('end', $._int_literal),
    )),
    parser_span: $ => seq(
      'span',
      field('start', $._field_name),
      optional(seq(
        '..',
        field('end', $._field_name),
      ))
    ),
    _constraint_atom: $ => choice(
      $._atom,
      $.range,
      $.not_eof,
    ),
    val_dot: $ => prec.left(PREC.POSTFIX, seq(
      field('left', $._expression),
      field('op', choice('.', '.?')),
      field('right', $._atom),
    )),
    bt_mark: $ => prec.left(PREC.POSTFIX, seq(
      field('left', $._expression),
      field('op', choice('!', '?')),
    )),
    parserdef_ref: $ => prec.left(PREC.ARGS, seq(
      seq(
        field('name', $.identifier),
        repeat(seq(
          '.',
          field('name', $.identifier),
        )),
      ),
      optional(
        seq(
          '[',
          seq(
            field('args', $._type_expression),
            repeat(seq(
              ',',
              field('args', $._type_expression),
            )),
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
      $._int_literal,
      $.bool_literal,
      $.regex_literal,
      $.string_literal,
    ),
    _int_literal: $ => choice(
      $.number_literal,
      $.char_literal,
    ),
    primitive_type: $ => choice(
      'int',
      'bit',
      'char',
    ),
    byte_slice: $ => prec(PREC.BYTE_SLICE, '~'),
    single: $ => '~',
    rule: $ => /\\|\|/,
    placeholder: $ => '_',
    array_fill: $ => '[..]',
    not_eof: $ => '!eof',
    _field_name: $ => choice(
      $.identifier,
      $.retvrn,
    ),
    retvrn: $ => 'return',
    identifier: $ => /[A-Za-z_][A-Za-z_0-9]*/,
    number_literal: $ => /-?([0-9]+|0x[0-9a-fA-F]+|0b[01]+|0o[0-7]+)/,
    char_literal: $ => /'([^'\\\n]|\\.)'/,
    string_literal: $ => /"([^"\\\n]|\\.)*"/,
    bool_literal: $ => choice(
      'true',
      'false',
    ),
    regex_literal: $ => /h?\/([^\/\\\n]|\\.)*\//,
  }
});
