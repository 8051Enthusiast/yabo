const PREC = {
  DEFAULT: 0,
  COMPARE: 3,
  OR: 4,
  XOR: 5,
  AND: 6,
  SHIFT: 7,
  ADD: 8,
  MULTIPLY: 9,
  PARSE: 10,
  UNARY: 11,
  WIGGLE: 12,
  ELSE: 15,
  DOT: 16,
};
module.exports = grammar({
  name: 'yabo',

  rules: {
    source_file: $ => repeat($._definition),
    _definition: $ => choice(
      $.parser_definition
    ),
    parser_definition: $ => seq(
      'parser',
      field('name', $.identifier),
      '=',
      field('from', $._parse_expression),
      '*>',
      field('to', $._parse_expression),
    ),
    _parse_expression: $ => choice(
      $.binary_parse_expression,
      $.unary_parse_expression,
      $.parser_array,
      $.parser_block,
      $._atom,
      seq('(', $._parse_expression, ')'),
    ),

    binary_parse_expression: $ => choice(
      prec.left(PREC.PARSE, seq(
        field('left', $._parse_expression),
        field('op', '|>'),
        field('right', $._parse_expression),
      )),
      prec.left(PREC.WIGGLE, seq(
        field('left', $._parse_expression),
        field('op', '~'),
        field('right', $._constraint_expression),
      )),
    ),
    unary_parse_expression: $ => choice(
      seq(
        field('op', 'if'),
        field('right', $._parse_expression)
      ),
    ),
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
    parser_array: $ => seq(
      field('direction', choice('for', 'each', 'rof')),
      '[',
      field('expr', $._parse_expression),
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
      field('parser', $._parse_expression),
      ';',
    ),
    let_statement: $ => seq(
      'let',
      field('name', $.identifier),
      ':',
      field('ty', $._parse_expression),
      '=',
      field('expr', $._expression),
      ';',
    ),
    _constraint_expression: $ => choice(
      $.binary_constraint_expression,
      $.unary_constraint_expression,
      $._atom,
    ),
    binary_constraint_expression: $ => choice(
      prec.left(PREC.OR, seq(
        field('left', $._constraint_expression),
        field('op', '||'),
        field('right', $._constraint_expression),
      )),
      prec.left(PREC.AND, seq(
        field('left', $._constraint_expression),
        field('op', '&&'),
        field('right', $._constraint_expression),
      )),
    ),
    unary_constraint_expression: $ => seq('!', $._atom),
    _expression: $ => choice(
      $.binary_expression,
      $.unary_expression,
      seq('(', $._expression, ')'),
      $._atom,
    ),
    binary_expression: $ => {
      const table = [
        ['.', PREC.DOT],
        ['else', PREC.ELSE],
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
      field('op', choice('-', '!', '+')),
      field('right', $._expression)
    )),
    _atom: $ => choice(
      $.identifier,
      $._literal,
    ),
    _literal: $ => choice(
      $.number_literal,
      $.char_literal,
    ),
    identifier: $ => /[A-Za-z_][A-Za-z_0-9]*/,
    number_literal: $ => /[0-9]+|0x[0-9a-fA-F]+|0b[01]+|0o[0-7]+/,
    char_literal: $ => seq(
      '\'',
      choice(
        token.immediate(/[^\n']/)
      ),
      '\'',
    ),
    string_literal: $ => seq(
      '\"',
      choice(
        token.immediate(/[^\n"]/)
      ),
      '\"'
    ),
  }
});
