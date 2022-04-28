#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 13
#define STATE_COUNT 105
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 85
#define ALIAS_COUNT 0
#define TOKEN_COUNT 51
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 12
#define MAX_ALIAS_SEQUENCE_LENGTH 7
#define PRODUCTION_ID_COUNT 16

enum {
  anon_sym_def = 1,
  anon_sym_STAR_GT = 2,
  anon_sym_EQ = 3,
  anon_sym_LPAREN = 4,
  anon_sym_RPAREN = 5,
  anon_sym_LBRACE = 6,
  anon_sym_RBRACE = 7,
  anon_sym_SEMI = 8,
  anon_sym_for = 9,
  anon_sym_each = 10,
  anon_sym_LBRACK = 11,
  anon_sym_RBRACK = 12,
  anon_sym_TILDE = 13,
  anon_sym_if = 14,
  anon_sym_try = 15,
  anon_sym_COLON = 16,
  anon_sym_COMMA = 17,
  anon_sym_let = 18,
  anon_sym_or = 19,
  anon_sym_and = 20,
  anon_sym_BANG = 21,
  anon_sym_else = 22,
  anon_sym_PIPE_GT = 23,
  anon_sym_PLUS = 24,
  anon_sym_DASH = 25,
  anon_sym_STAR = 26,
  anon_sym_SLASH = 27,
  anon_sym_PERCENT = 28,
  anon_sym_LT_LT = 29,
  anon_sym_GT_GT = 30,
  anon_sym_EQ_EQ = 31,
  anon_sym_BANG_EQ = 32,
  anon_sym_GT = 33,
  anon_sym_GT_EQ = 34,
  anon_sym_LT_EQ = 35,
  anon_sym_LT = 36,
  anon_sym_PIPE = 37,
  anon_sym_CARET = 38,
  anon_sym_AMP = 39,
  anon_sym_DOT = 40,
  anon_sym_AMP_GT = 41,
  anon_sym_int = 42,
  anon_sym_bit = 43,
  anon_sym_char = 44,
  anon_sym_mem = 45,
  sym_type_var = 46,
  sym_identifier = 47,
  sym_number_literal = 48,
  anon_sym_SQUOTE = 49,
  aux_sym_char_literal_token1 = 50,
  sym_source_file = 51,
  sym__definition = 52,
  sym_parser_definition = 53,
  sym__type_expression = 54,
  sym_binary_type_expression = 55,
  sym_unary_type_expression = 56,
  sym_parser_block = 57,
  sym__parser_block_content = 58,
  sym_parser_sequence = 59,
  sym_parser_choice = 60,
  sym_type_array = 61,
  sym_parser_array = 62,
  sym_constraint_apply = 63,
  sym_type_constraint = 64,
  sym__statement = 65,
  sym_parse_statement = 66,
  sym_let_statement = 67,
  sym__constraint_expression = 68,
  sym_binary_constraint_expression = 69,
  sym_unary_constraint_expression = 70,
  sym__expression = 71,
  sym_binary_expression = 72,
  sym_unary_expression = 73,
  sym__type_atom = 74,
  sym_val_dot = 75,
  sym_parserdef_ref = 76,
  sym__atom = 77,
  sym__literal = 78,
  sym_primitive_type = 79,
  sym_single = 80,
  sym_char_literal = 81,
  aux_sym_source_file_repeat1 = 82,
  aux_sym_parser_sequence_repeat1 = 83,
  aux_sym_parserdef_ref_repeat1 = 84,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_def] = "def",
  [anon_sym_STAR_GT] = "*>",
  [anon_sym_EQ] = "=",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [anon_sym_LBRACE] = "{",
  [anon_sym_RBRACE] = "}",
  [anon_sym_SEMI] = ";",
  [anon_sym_for] = "for",
  [anon_sym_each] = "each",
  [anon_sym_LBRACK] = "[",
  [anon_sym_RBRACK] = "]",
  [anon_sym_TILDE] = "~",
  [anon_sym_if] = "if",
  [anon_sym_try] = "try",
  [anon_sym_COLON] = ":",
  [anon_sym_COMMA] = ",",
  [anon_sym_let] = "let",
  [anon_sym_or] = "or",
  [anon_sym_and] = "and",
  [anon_sym_BANG] = "!",
  [anon_sym_else] = "else",
  [anon_sym_PIPE_GT] = "|>",
  [anon_sym_PLUS] = "+",
  [anon_sym_DASH] = "-",
  [anon_sym_STAR] = "*",
  [anon_sym_SLASH] = "/",
  [anon_sym_PERCENT] = "%",
  [anon_sym_LT_LT] = "<<",
  [anon_sym_GT_GT] = ">>",
  [anon_sym_EQ_EQ] = "==",
  [anon_sym_BANG_EQ] = "!=",
  [anon_sym_GT] = ">",
  [anon_sym_GT_EQ] = ">=",
  [anon_sym_LT_EQ] = "<=",
  [anon_sym_LT] = "<",
  [anon_sym_PIPE] = "|",
  [anon_sym_CARET] = "^",
  [anon_sym_AMP] = "&",
  [anon_sym_DOT] = ".",
  [anon_sym_AMP_GT] = "&>",
  [anon_sym_int] = "int",
  [anon_sym_bit] = "bit",
  [anon_sym_char] = "char",
  [anon_sym_mem] = "mem",
  [sym_type_var] = "type_var",
  [sym_identifier] = "identifier",
  [sym_number_literal] = "number_literal",
  [anon_sym_SQUOTE] = "'",
  [aux_sym_char_literal_token1] = "char_literal_token1",
  [sym_source_file] = "source_file",
  [sym__definition] = "_definition",
  [sym_parser_definition] = "parser_definition",
  [sym__type_expression] = "_type_expression",
  [sym_binary_type_expression] = "binary_type_expression",
  [sym_unary_type_expression] = "unary_type_expression",
  [sym_parser_block] = "parser_block",
  [sym__parser_block_content] = "_parser_block_content",
  [sym_parser_sequence] = "parser_sequence",
  [sym_parser_choice] = "parser_choice",
  [sym_type_array] = "type_array",
  [sym_parser_array] = "parser_array",
  [sym_constraint_apply] = "constraint_apply",
  [sym_type_constraint] = "type_constraint",
  [sym__statement] = "_statement",
  [sym_parse_statement] = "parse_statement",
  [sym_let_statement] = "let_statement",
  [sym__constraint_expression] = "_constraint_expression",
  [sym_binary_constraint_expression] = "binary_constraint_expression",
  [sym_unary_constraint_expression] = "unary_constraint_expression",
  [sym__expression] = "_expression",
  [sym_binary_expression] = "binary_expression",
  [sym_unary_expression] = "unary_expression",
  [sym__type_atom] = "_type_atom",
  [sym_val_dot] = "val_dot",
  [sym_parserdef_ref] = "parserdef_ref",
  [sym__atom] = "_atom",
  [sym__literal] = "_literal",
  [sym_primitive_type] = "primitive_type",
  [sym_single] = "single",
  [sym_char_literal] = "char_literal",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
  [aux_sym_parser_sequence_repeat1] = "parser_sequence_repeat1",
  [aux_sym_parserdef_ref_repeat1] = "parserdef_ref_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_def] = anon_sym_def,
  [anon_sym_STAR_GT] = anon_sym_STAR_GT,
  [anon_sym_EQ] = anon_sym_EQ,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_LBRACE] = anon_sym_LBRACE,
  [anon_sym_RBRACE] = anon_sym_RBRACE,
  [anon_sym_SEMI] = anon_sym_SEMI,
  [anon_sym_for] = anon_sym_for,
  [anon_sym_each] = anon_sym_each,
  [anon_sym_LBRACK] = anon_sym_LBRACK,
  [anon_sym_RBRACK] = anon_sym_RBRACK,
  [anon_sym_TILDE] = anon_sym_TILDE,
  [anon_sym_if] = anon_sym_if,
  [anon_sym_try] = anon_sym_try,
  [anon_sym_COLON] = anon_sym_COLON,
  [anon_sym_COMMA] = anon_sym_COMMA,
  [anon_sym_let] = anon_sym_let,
  [anon_sym_or] = anon_sym_or,
  [anon_sym_and] = anon_sym_and,
  [anon_sym_BANG] = anon_sym_BANG,
  [anon_sym_else] = anon_sym_else,
  [anon_sym_PIPE_GT] = anon_sym_PIPE_GT,
  [anon_sym_PLUS] = anon_sym_PLUS,
  [anon_sym_DASH] = anon_sym_DASH,
  [anon_sym_STAR] = anon_sym_STAR,
  [anon_sym_SLASH] = anon_sym_SLASH,
  [anon_sym_PERCENT] = anon_sym_PERCENT,
  [anon_sym_LT_LT] = anon_sym_LT_LT,
  [anon_sym_GT_GT] = anon_sym_GT_GT,
  [anon_sym_EQ_EQ] = anon_sym_EQ_EQ,
  [anon_sym_BANG_EQ] = anon_sym_BANG_EQ,
  [anon_sym_GT] = anon_sym_GT,
  [anon_sym_GT_EQ] = anon_sym_GT_EQ,
  [anon_sym_LT_EQ] = anon_sym_LT_EQ,
  [anon_sym_LT] = anon_sym_LT,
  [anon_sym_PIPE] = anon_sym_PIPE,
  [anon_sym_CARET] = anon_sym_CARET,
  [anon_sym_AMP] = anon_sym_AMP,
  [anon_sym_DOT] = anon_sym_DOT,
  [anon_sym_AMP_GT] = anon_sym_AMP_GT,
  [anon_sym_int] = anon_sym_int,
  [anon_sym_bit] = anon_sym_bit,
  [anon_sym_char] = anon_sym_char,
  [anon_sym_mem] = anon_sym_mem,
  [sym_type_var] = sym_type_var,
  [sym_identifier] = sym_identifier,
  [sym_number_literal] = sym_number_literal,
  [anon_sym_SQUOTE] = anon_sym_SQUOTE,
  [aux_sym_char_literal_token1] = aux_sym_char_literal_token1,
  [sym_source_file] = sym_source_file,
  [sym__definition] = sym__definition,
  [sym_parser_definition] = sym_parser_definition,
  [sym__type_expression] = sym__type_expression,
  [sym_binary_type_expression] = sym_binary_type_expression,
  [sym_unary_type_expression] = sym_unary_type_expression,
  [sym_parser_block] = sym_parser_block,
  [sym__parser_block_content] = sym__parser_block_content,
  [sym_parser_sequence] = sym_parser_sequence,
  [sym_parser_choice] = sym_parser_choice,
  [sym_type_array] = sym_type_array,
  [sym_parser_array] = sym_parser_array,
  [sym_constraint_apply] = sym_constraint_apply,
  [sym_type_constraint] = sym_type_constraint,
  [sym__statement] = sym__statement,
  [sym_parse_statement] = sym_parse_statement,
  [sym_let_statement] = sym_let_statement,
  [sym__constraint_expression] = sym__constraint_expression,
  [sym_binary_constraint_expression] = sym_binary_constraint_expression,
  [sym_unary_constraint_expression] = sym_unary_constraint_expression,
  [sym__expression] = sym__expression,
  [sym_binary_expression] = sym_binary_expression,
  [sym_unary_expression] = sym_unary_expression,
  [sym__type_atom] = sym__type_atom,
  [sym_val_dot] = sym_val_dot,
  [sym_parserdef_ref] = sym_parserdef_ref,
  [sym__atom] = sym__atom,
  [sym__literal] = sym__literal,
  [sym_primitive_type] = sym_primitive_type,
  [sym_single] = sym_single,
  [sym_char_literal] = sym_char_literal,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
  [aux_sym_parser_sequence_repeat1] = aux_sym_parser_sequence_repeat1,
  [aux_sym_parserdef_ref_repeat1] = aux_sym_parserdef_ref_repeat1,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [anon_sym_def] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_STAR_GT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LBRACE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RBRACE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SEMI] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_for] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_each] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LBRACK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RBRACK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_TILDE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_if] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_try] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COLON] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COMMA] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_let] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_or] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_and] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_BANG] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_else] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PIPE_GT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PLUS] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DASH] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_STAR] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SLASH] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PERCENT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LT_LT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_GT_GT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_BANG_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_GT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_GT_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LT_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PIPE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_CARET] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_AMP] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_AMP_GT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_int] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_bit] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_char] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_mem] = {
    .visible = true,
    .named = false,
  },
  [sym_type_var] = {
    .visible = true,
    .named = true,
  },
  [sym_identifier] = {
    .visible = true,
    .named = true,
  },
  [sym_number_literal] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_SQUOTE] = {
    .visible = true,
    .named = false,
  },
  [aux_sym_char_literal_token1] = {
    .visible = false,
    .named = false,
  },
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym__definition] = {
    .visible = false,
    .named = true,
  },
  [sym_parser_definition] = {
    .visible = true,
    .named = true,
  },
  [sym__type_expression] = {
    .visible = false,
    .named = true,
  },
  [sym_binary_type_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_unary_type_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_parser_block] = {
    .visible = true,
    .named = true,
  },
  [sym__parser_block_content] = {
    .visible = false,
    .named = true,
  },
  [sym_parser_sequence] = {
    .visible = true,
    .named = true,
  },
  [sym_parser_choice] = {
    .visible = true,
    .named = true,
  },
  [sym_type_array] = {
    .visible = true,
    .named = true,
  },
  [sym_parser_array] = {
    .visible = true,
    .named = true,
  },
  [sym_constraint_apply] = {
    .visible = true,
    .named = true,
  },
  [sym_type_constraint] = {
    .visible = true,
    .named = true,
  },
  [sym__statement] = {
    .visible = false,
    .named = true,
  },
  [sym_parse_statement] = {
    .visible = true,
    .named = true,
  },
  [sym_let_statement] = {
    .visible = true,
    .named = true,
  },
  [sym__constraint_expression] = {
    .visible = false,
    .named = true,
  },
  [sym_binary_constraint_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_unary_constraint_expression] = {
    .visible = true,
    .named = true,
  },
  [sym__expression] = {
    .visible = false,
    .named = true,
  },
  [sym_binary_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_unary_expression] = {
    .visible = true,
    .named = true,
  },
  [sym__type_atom] = {
    .visible = false,
    .named = true,
  },
  [sym_val_dot] = {
    .visible = true,
    .named = true,
  },
  [sym_parserdef_ref] = {
    .visible = true,
    .named = true,
  },
  [sym__atom] = {
    .visible = false,
    .named = true,
  },
  [sym__literal] = {
    .visible = false,
    .named = true,
  },
  [sym_primitive_type] = {
    .visible = true,
    .named = true,
  },
  [sym_single] = {
    .visible = true,
    .named = true,
  },
  [sym_char_literal] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_source_file_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_parser_sequence_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_parserdef_ref_repeat1] = {
    .visible = false,
    .named = false,
  },
};

enum {
  field_args = 1,
  field_content = 2,
  field_direction = 3,
  field_expr = 4,
  field_from = 5,
  field_left = 6,
  field_name = 7,
  field_op = 8,
  field_parser = 9,
  field_right = 10,
  field_to = 11,
  field_ty = 12,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
  [field_args] = "args",
  [field_content] = "content",
  [field_direction] = "direction",
  [field_expr] = "expr",
  [field_from] = "from",
  [field_left] = "left",
  [field_name] = "name",
  [field_op] = "op",
  [field_parser] = "parser",
  [field_right] = "right",
  [field_to] = "to",
  [field_ty] = "ty",
};

static const TSFieldMapSlice ts_field_map_slices[PRODUCTION_ID_COUNT] = {
  [1] = {.index = 0, .length = 1},
  [2] = {.index = 1, .length = 2},
  [3] = {.index = 3, .length = 3},
  [4] = {.index = 6, .length = 2},
  [5] = {.index = 8, .length = 2},
  [6] = {.index = 10, .length = 1},
  [7] = {.index = 11, .length = 2},
  [8] = {.index = 13, .length = 2},
  [9] = {.index = 15, .length = 3},
  [10] = {.index = 18, .length = 3},
  [11] = {.index = 21, .length = 1},
  [12] = {.index = 22, .length = 1},
  [13] = {.index = 23, .length = 2},
  [14] = {.index = 25, .length = 2},
  [15] = {.index = 27, .length = 3},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_name, 0},
  [1] =
    {field_op, 0},
    {field_right, 1},
  [3] =
    {field_left, 0},
    {field_op, 1},
    {field_right, 2},
  [6] =
    {field_from, 0},
    {field_name, 2},
  [8] =
    {field_direction, 0},
    {field_expr, 2},
  [10] =
    {field_args, 0},
  [11] =
    {field_args, 2, .inherited = true},
    {field_name, 0},
  [13] =
    {field_args, 0, .inherited = true},
    {field_args, 1, .inherited = true},
  [15] =
    {field_from, 1},
    {field_name, 3},
    {field_to, 5},
  [18] =
    {field_args, 4, .inherited = true},
    {field_from, 0},
    {field_name, 2},
  [21] =
    {field_content, 1},
  [22] =
    {field_parser, 0},
  [23] =
    {field_left, 0},
    {field_right, 2},
  [25] =
    {field_name, 0},
    {field_parser, 2},
  [27] =
    {field_expr, 5},
    {field_name, 1},
    {field_ty, 3},
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
};

static const uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(44);
      if (lookahead == '!') ADVANCE(71);
      if (lookahead == '%') ADVANCE(78);
      if (lookahead == '&') ADVANCE(90);
      if (lookahead == '\'') ADVANCE(125);
      if (lookahead == '(') ADVANCE(49);
      if (lookahead == ')') ADVANCE(50);
      if (lookahead == '*') ADVANCE(76);
      if (lookahead == '+') ADVANCE(74);
      if (lookahead == ',') ADVANCE(65);
      if (lookahead == '-') ADVANCE(75);
      if (lookahead == '.') ADVANCE(91);
      if (lookahead == '/') ADVANCE(77);
      if (lookahead == '0') ADVANCE(120);
      if (lookahead == ':') ADVANCE(64);
      if (lookahead == ';') ADVANCE(53);
      if (lookahead == '<') ADVANCE(86);
      if (lookahead == '=') ADVANCE(48);
      if (lookahead == '>') ADVANCE(83);
      if (lookahead == '[') ADVANCE(58);
      if (lookahead == ']') ADVANCE(59);
      if (lookahead == '^') ADVANCE(88);
      if (lookahead == 'a') ADVANCE(26);
      if (lookahead == 'b') ADVANCE(23);
      if (lookahead == 'c') ADVANCE(21);
      if (lookahead == 'd') ADVANCE(14);
      if (lookahead == 'e') ADVANCE(10);
      if (lookahead == 'f') ADVANCE(27);
      if (lookahead == 'i') ADVANCE(19);
      if (lookahead == 'l') ADVANCE(17);
      if (lookahead == 'm') ADVANCE(15);
      if (lookahead == 'o') ADVANCE(28);
      if (lookahead == 't') ADVANCE(29);
      if (lookahead == '{') ADVANCE(51);
      if (lookahead == '|') ADVANCE(87);
      if (lookahead == '}') ADVANCE(52);
      if (lookahead == '~') ADVANCE(60);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(123);
      END_STATE();
    case 1:
      if (lookahead == '!') ADVANCE(70);
      if (lookahead == '&') ADVANCE(8);
      if (lookahead == '\'') ADVANCE(125);
      if (lookahead == '(') ADVANCE(49);
      if (lookahead == ')') ADVANCE(50);
      if (lookahead == '*') ADVANCE(9);
      if (lookahead == '+') ADVANCE(74);
      if (lookahead == ',') ADVANCE(65);
      if (lookahead == '-') ADVANCE(75);
      if (lookahead == '0') ADVANCE(120);
      if (lookahead == ';') ADVANCE(53);
      if (lookahead == '=') ADVANCE(47);
      if (lookahead == '[') ADVANCE(58);
      if (lookahead == ']') ADVANCE(59);
      if (lookahead == 'e') ADVANCE(102);
      if (lookahead == 'f') ADVANCE(113);
      if (lookahead == 'i') ADVANCE(107);
      if (lookahead == 'l') ADVANCE(105);
      if (lookahead == '{') ADVANCE(51);
      if (lookahead == '}') ADVANCE(52);
      if (lookahead == '~') ADVANCE(60);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(1)
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(123);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 2:
      if (lookahead == '!') ADVANCE(70);
      if (lookahead == '\'') ADVANCE(125);
      if (lookahead == '(') ADVANCE(49);
      if (lookahead == '+') ADVANCE(74);
      if (lookahead == '-') ADVANCE(75);
      if (lookahead == '0') ADVANCE(120);
      if (lookahead == 'e') ADVANCE(102);
      if (lookahead == 'f') ADVANCE(113);
      if (lookahead == 'i') ADVANCE(107);
      if (lookahead == '{') ADVANCE(51);
      if (lookahead == '~') ADVANCE(60);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(2)
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(123);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 3:
      if (lookahead == '!') ADVANCE(70);
      if (lookahead == '\'') ADVANCE(125);
      if (lookahead == '0') ADVANCE(120);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(3)
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(123);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 4:
      if (lookahead == '&') ADVANCE(8);
      if (lookahead == ')') ADVANCE(50);
      if (lookahead == '*') ADVANCE(9);
      if (lookahead == ',') ADVANCE(65);
      if (lookahead == '=') ADVANCE(47);
      if (lookahead == ']') ADVANCE(59);
      if (lookahead == 'a') ADVANCE(26);
      if (lookahead == 'o') ADVANCE(28);
      if (lookahead == '~') ADVANCE(60);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(4)
      END_STATE();
    case 5:
      if (lookahead == '\'') ADVANCE(40);
      if (lookahead == '(') ADVANCE(49);
      if (lookahead == '*') ADVANCE(9);
      if (lookahead == ']') ADVANCE(59);
      if (lookahead == 'b') ADVANCE(110);
      if (lookahead == 'c') ADVANCE(109);
      if (lookahead == 'e') ADVANCE(102);
      if (lookahead == 'f') ADVANCE(113);
      if (lookahead == 'i') ADVANCE(112);
      if (lookahead == 'm') ADVANCE(106);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(5)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 6:
      if (lookahead == '=') ADVANCE(82);
      END_STATE();
    case 7:
      if (lookahead == '=') ADVANCE(81);
      END_STATE();
    case 8:
      if (lookahead == '>') ADVANCE(92);
      END_STATE();
    case 9:
      if (lookahead == '>') ADVANCE(46);
      END_STATE();
    case 10:
      if (lookahead == 'a') ADVANCE(12);
      if (lookahead == 'l') ADVANCE(32);
      END_STATE();
    case 11:
      if (lookahead == 'a') ADVANCE(31);
      END_STATE();
    case 12:
      if (lookahead == 'c') ADVANCE(22);
      END_STATE();
    case 13:
      if (lookahead == 'd') ADVANCE(69);
      END_STATE();
    case 14:
      if (lookahead == 'e') ADVANCE(20);
      END_STATE();
    case 15:
      if (lookahead == 'e') ADVANCE(25);
      END_STATE();
    case 16:
      if (lookahead == 'e') ADVANCE(72);
      END_STATE();
    case 17:
      if (lookahead == 'e') ADVANCE(35);
      END_STATE();
    case 18:
      if (lookahead == 'f') ADVANCE(61);
      END_STATE();
    case 19:
      if (lookahead == 'f') ADVANCE(61);
      if (lookahead == 'n') ADVANCE(34);
      END_STATE();
    case 20:
      if (lookahead == 'f') ADVANCE(45);
      END_STATE();
    case 21:
      if (lookahead == 'h') ADVANCE(11);
      END_STATE();
    case 22:
      if (lookahead == 'h') ADVANCE(56);
      END_STATE();
    case 23:
      if (lookahead == 'i') ADVANCE(33);
      END_STATE();
    case 24:
      if (lookahead == 'l') ADVANCE(32);
      END_STATE();
    case 25:
      if (lookahead == 'm') ADVANCE(99);
      END_STATE();
    case 26:
      if (lookahead == 'n') ADVANCE(13);
      END_STATE();
    case 27:
      if (lookahead == 'o') ADVANCE(30);
      END_STATE();
    case 28:
      if (lookahead == 'r') ADVANCE(68);
      END_STATE();
    case 29:
      if (lookahead == 'r') ADVANCE(36);
      END_STATE();
    case 30:
      if (lookahead == 'r') ADVANCE(54);
      END_STATE();
    case 31:
      if (lookahead == 'r') ADVANCE(97);
      END_STATE();
    case 32:
      if (lookahead == 's') ADVANCE(16);
      END_STATE();
    case 33:
      if (lookahead == 't') ADVANCE(95);
      END_STATE();
    case 34:
      if (lookahead == 't') ADVANCE(93);
      END_STATE();
    case 35:
      if (lookahead == 't') ADVANCE(66);
      END_STATE();
    case 36:
      if (lookahead == 'y') ADVANCE(63);
      END_STATE();
    case 37:
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(121);
      END_STATE();
    case 38:
      if (('0' <= lookahead && lookahead <= '7')) ADVANCE(122);
      END_STATE();
    case 39:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(124);
      END_STATE();
    case 40:
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(101);
      END_STATE();
    case 41:
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '\'') ADVANCE(126);
      END_STATE();
    case 42:
      if (eof) ADVANCE(44);
      if (lookahead == '!') ADVANCE(6);
      if (lookahead == '%') ADVANCE(78);
      if (lookahead == '&') ADVANCE(90);
      if (lookahead == ')') ADVANCE(50);
      if (lookahead == '*') ADVANCE(76);
      if (lookahead == '+') ADVANCE(74);
      if (lookahead == ',') ADVANCE(65);
      if (lookahead == '-') ADVANCE(75);
      if (lookahead == '.') ADVANCE(91);
      if (lookahead == '/') ADVANCE(77);
      if (lookahead == '<') ADVANCE(86);
      if (lookahead == '=') ADVANCE(48);
      if (lookahead == '>') ADVANCE(83);
      if (lookahead == ']') ADVANCE(59);
      if (lookahead == '^') ADVANCE(88);
      if (lookahead == 'a') ADVANCE(26);
      if (lookahead == 'd') ADVANCE(14);
      if (lookahead == 'e') ADVANCE(24);
      if (lookahead == 'i') ADVANCE(18);
      if (lookahead == 'o') ADVANCE(28);
      if (lookahead == 't') ADVANCE(29);
      if (lookahead == '|') ADVANCE(87);
      if (lookahead == '~') ADVANCE(60);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(42)
      END_STATE();
    case 43:
      if (eof) ADVANCE(44);
      if (lookahead == '!') ADVANCE(6);
      if (lookahead == '%') ADVANCE(78);
      if (lookahead == '&') ADVANCE(89);
      if (lookahead == ')') ADVANCE(50);
      if (lookahead == '*') ADVANCE(76);
      if (lookahead == '+') ADVANCE(74);
      if (lookahead == ',') ADVANCE(65);
      if (lookahead == '-') ADVANCE(75);
      if (lookahead == '.') ADVANCE(91);
      if (lookahead == '/') ADVANCE(77);
      if (lookahead == ':') ADVANCE(64);
      if (lookahead == '<') ADVANCE(86);
      if (lookahead == '=') ADVANCE(7);
      if (lookahead == '>') ADVANCE(83);
      if (lookahead == ']') ADVANCE(59);
      if (lookahead == '^') ADVANCE(88);
      if (lookahead == 'a') ADVANCE(26);
      if (lookahead == 'd') ADVANCE(14);
      if (lookahead == 'e') ADVANCE(24);
      if (lookahead == 'i') ADVANCE(18);
      if (lookahead == 'o') ADVANCE(28);
      if (lookahead == 't') ADVANCE(29);
      if (lookahead == '|') ADVANCE(87);
      if (lookahead == '~') ADVANCE(60);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(43)
      END_STATE();
    case 44:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(anon_sym_def);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(anon_sym_STAR_GT);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(anon_sym_EQ);
      if (lookahead == '=') ADVANCE(81);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(anon_sym_LBRACE);
      END_STATE();
    case 52:
      ACCEPT_TOKEN(anon_sym_RBRACE);
      END_STATE();
    case 53:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 54:
      ACCEPT_TOKEN(anon_sym_for);
      END_STATE();
    case 55:
      ACCEPT_TOKEN(anon_sym_for);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 56:
      ACCEPT_TOKEN(anon_sym_each);
      END_STATE();
    case 57:
      ACCEPT_TOKEN(anon_sym_each);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 58:
      ACCEPT_TOKEN(anon_sym_LBRACK);
      END_STATE();
    case 59:
      ACCEPT_TOKEN(anon_sym_RBRACK);
      END_STATE();
    case 60:
      ACCEPT_TOKEN(anon_sym_TILDE);
      END_STATE();
    case 61:
      ACCEPT_TOKEN(anon_sym_if);
      END_STATE();
    case 62:
      ACCEPT_TOKEN(anon_sym_if);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 63:
      ACCEPT_TOKEN(anon_sym_try);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(anon_sym_let);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(anon_sym_let);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(anon_sym_or);
      END_STATE();
    case 69:
      ACCEPT_TOKEN(anon_sym_and);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(anon_sym_BANG);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(anon_sym_BANG);
      if (lookahead == '=') ADVANCE(82);
      END_STATE();
    case 72:
      ACCEPT_TOKEN(anon_sym_else);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(anon_sym_PIPE_GT);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 75:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(anon_sym_STAR);
      if (lookahead == '>') ADVANCE(46);
      END_STATE();
    case 77:
      ACCEPT_TOKEN(anon_sym_SLASH);
      END_STATE();
    case 78:
      ACCEPT_TOKEN(anon_sym_PERCENT);
      END_STATE();
    case 79:
      ACCEPT_TOKEN(anon_sym_LT_LT);
      END_STATE();
    case 80:
      ACCEPT_TOKEN(anon_sym_GT_GT);
      END_STATE();
    case 81:
      ACCEPT_TOKEN(anon_sym_EQ_EQ);
      END_STATE();
    case 82:
      ACCEPT_TOKEN(anon_sym_BANG_EQ);
      END_STATE();
    case 83:
      ACCEPT_TOKEN(anon_sym_GT);
      if (lookahead == '=') ADVANCE(84);
      if (lookahead == '>') ADVANCE(80);
      END_STATE();
    case 84:
      ACCEPT_TOKEN(anon_sym_GT_EQ);
      END_STATE();
    case 85:
      ACCEPT_TOKEN(anon_sym_LT_EQ);
      END_STATE();
    case 86:
      ACCEPT_TOKEN(anon_sym_LT);
      if (lookahead == '<') ADVANCE(79);
      if (lookahead == '=') ADVANCE(85);
      END_STATE();
    case 87:
      ACCEPT_TOKEN(anon_sym_PIPE);
      if (lookahead == '>') ADVANCE(73);
      END_STATE();
    case 88:
      ACCEPT_TOKEN(anon_sym_CARET);
      END_STATE();
    case 89:
      ACCEPT_TOKEN(anon_sym_AMP);
      END_STATE();
    case 90:
      ACCEPT_TOKEN(anon_sym_AMP);
      if (lookahead == '>') ADVANCE(92);
      END_STATE();
    case 91:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 92:
      ACCEPT_TOKEN(anon_sym_AMP_GT);
      END_STATE();
    case 93:
      ACCEPT_TOKEN(anon_sym_int);
      END_STATE();
    case 94:
      ACCEPT_TOKEN(anon_sym_int);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 95:
      ACCEPT_TOKEN(anon_sym_bit);
      END_STATE();
    case 96:
      ACCEPT_TOKEN(anon_sym_bit);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 97:
      ACCEPT_TOKEN(anon_sym_char);
      END_STATE();
    case 98:
      ACCEPT_TOKEN(anon_sym_char);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 99:
      ACCEPT_TOKEN(anon_sym_mem);
      END_STATE();
    case 100:
      ACCEPT_TOKEN(anon_sym_mem);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 101:
      ACCEPT_TOKEN(sym_type_var);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(101);
      END_STATE();
    case 102:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(104);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 103:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(115);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 104:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'c') ADVANCE(108);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 105:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(116);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 106:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(111);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 107:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'f') ADVANCE(62);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 108:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'h') ADVANCE(57);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 109:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'h') ADVANCE(103);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 110:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'i') ADVANCE(117);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 111:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'm') ADVANCE(100);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 112:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'n') ADVANCE(118);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 113:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(114);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 114:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(55);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 115:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(98);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 116:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(67);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 117:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(96);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 118:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(94);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 119:
      ACCEPT_TOKEN(sym_identifier);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(119);
      END_STATE();
    case 120:
      ACCEPT_TOKEN(sym_number_literal);
      if (lookahead == 'b') ADVANCE(37);
      if (lookahead == 'o') ADVANCE(38);
      if (lookahead == 'x') ADVANCE(39);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(123);
      END_STATE();
    case 121:
      ACCEPT_TOKEN(sym_number_literal);
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(121);
      END_STATE();
    case 122:
      ACCEPT_TOKEN(sym_number_literal);
      if (('0' <= lookahead && lookahead <= '7')) ADVANCE(122);
      END_STATE();
    case 123:
      ACCEPT_TOKEN(sym_number_literal);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(123);
      END_STATE();
    case 124:
      ACCEPT_TOKEN(sym_number_literal);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(124);
      END_STATE();
    case 125:
      ACCEPT_TOKEN(anon_sym_SQUOTE);
      END_STATE();
    case 126:
      ACCEPT_TOKEN(aux_sym_char_literal_token1);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 0},
  [2] = {.lex_state = 42},
  [3] = {.lex_state = 1},
  [4] = {.lex_state = 42},
  [5] = {.lex_state = 42},
  [6] = {.lex_state = 42},
  [7] = {.lex_state = 1},
  [8] = {.lex_state = 1},
  [9] = {.lex_state = 1},
  [10] = {.lex_state = 1},
  [11] = {.lex_state = 43},
  [12] = {.lex_state = 43},
  [13] = {.lex_state = 43},
  [14] = {.lex_state = 43},
  [15] = {.lex_state = 43},
  [16] = {.lex_state = 43},
  [17] = {.lex_state = 43},
  [18] = {.lex_state = 43},
  [19] = {.lex_state = 43},
  [20] = {.lex_state = 43},
  [21] = {.lex_state = 43},
  [22] = {.lex_state = 43},
  [23] = {.lex_state = 43},
  [24] = {.lex_state = 43},
  [25] = {.lex_state = 43},
  [26] = {.lex_state = 43},
  [27] = {.lex_state = 43},
  [28] = {.lex_state = 43},
  [29] = {.lex_state = 43},
  [30] = {.lex_state = 43},
  [31] = {.lex_state = 43},
  [32] = {.lex_state = 43},
  [33] = {.lex_state = 43},
  [34] = {.lex_state = 43},
  [35] = {.lex_state = 43},
  [36] = {.lex_state = 2},
  [37] = {.lex_state = 2},
  [38] = {.lex_state = 2},
  [39] = {.lex_state = 2},
  [40] = {.lex_state = 2},
  [41] = {.lex_state = 2},
  [42] = {.lex_state = 2},
  [43] = {.lex_state = 2},
  [44] = {.lex_state = 2},
  [45] = {.lex_state = 2},
  [46] = {.lex_state = 2},
  [47] = {.lex_state = 2},
  [48] = {.lex_state = 2},
  [49] = {.lex_state = 2},
  [50] = {.lex_state = 2},
  [51] = {.lex_state = 5},
  [52] = {.lex_state = 5},
  [53] = {.lex_state = 5},
  [54] = {.lex_state = 5},
  [55] = {.lex_state = 5},
  [56] = {.lex_state = 5},
  [57] = {.lex_state = 5},
  [58] = {.lex_state = 5},
  [59] = {.lex_state = 5},
  [60] = {.lex_state = 5},
  [61] = {.lex_state = 5},
  [62] = {.lex_state = 5},
  [63] = {.lex_state = 1},
  [64] = {.lex_state = 1},
  [65] = {.lex_state = 1},
  [66] = {.lex_state = 1},
  [67] = {.lex_state = 5},
  [68] = {.lex_state = 3},
  [69] = {.lex_state = 3},
  [70] = {.lex_state = 3},
  [71] = {.lex_state = 3},
  [72] = {.lex_state = 4},
  [73] = {.lex_state = 1},
  [74] = {.lex_state = 1},
  [75] = {.lex_state = 1},
  [76] = {.lex_state = 1},
  [77] = {.lex_state = 1},
  [78] = {.lex_state = 1},
  [79] = {.lex_state = 1},
  [80] = {.lex_state = 1},
  [81] = {.lex_state = 1},
  [82] = {.lex_state = 1},
  [83] = {.lex_state = 1},
  [84] = {.lex_state = 3},
  [85] = {.lex_state = 3},
  [86] = {.lex_state = 0},
  [87] = {.lex_state = 1},
  [88] = {.lex_state = 0},
  [89] = {.lex_state = 1},
  [90] = {.lex_state = 1},
  [91] = {.lex_state = 1},
  [92] = {.lex_state = 1},
  [93] = {.lex_state = 1},
  [94] = {.lex_state = 0},
  [95] = {.lex_state = 0},
  [96] = {.lex_state = 0},
  [97] = {.lex_state = 3},
  [98] = {.lex_state = 0},
  [99] = {.lex_state = 0},
  [100] = {.lex_state = 0},
  [101] = {.lex_state = 3},
  [102] = {.lex_state = 41},
  [103] = {.lex_state = 0},
  [104] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_def] = ACTIONS(1),
    [anon_sym_STAR_GT] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_LBRACE] = ACTIONS(1),
    [anon_sym_RBRACE] = ACTIONS(1),
    [anon_sym_SEMI] = ACTIONS(1),
    [anon_sym_for] = ACTIONS(1),
    [anon_sym_each] = ACTIONS(1),
    [anon_sym_LBRACK] = ACTIONS(1),
    [anon_sym_RBRACK] = ACTIONS(1),
    [anon_sym_TILDE] = ACTIONS(1),
    [anon_sym_if] = ACTIONS(1),
    [anon_sym_try] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
    [anon_sym_let] = ACTIONS(1),
    [anon_sym_or] = ACTIONS(1),
    [anon_sym_and] = ACTIONS(1),
    [anon_sym_BANG] = ACTIONS(1),
    [anon_sym_else] = ACTIONS(1),
    [anon_sym_PIPE_GT] = ACTIONS(1),
    [anon_sym_PLUS] = ACTIONS(1),
    [anon_sym_DASH] = ACTIONS(1),
    [anon_sym_STAR] = ACTIONS(1),
    [anon_sym_SLASH] = ACTIONS(1),
    [anon_sym_PERCENT] = ACTIONS(1),
    [anon_sym_LT_LT] = ACTIONS(1),
    [anon_sym_GT_GT] = ACTIONS(1),
    [anon_sym_EQ_EQ] = ACTIONS(1),
    [anon_sym_BANG_EQ] = ACTIONS(1),
    [anon_sym_GT] = ACTIONS(1),
    [anon_sym_GT_EQ] = ACTIONS(1),
    [anon_sym_LT_EQ] = ACTIONS(1),
    [anon_sym_LT] = ACTIONS(1),
    [anon_sym_PIPE] = ACTIONS(1),
    [anon_sym_CARET] = ACTIONS(1),
    [anon_sym_AMP] = ACTIONS(1),
    [anon_sym_DOT] = ACTIONS(1),
    [anon_sym_AMP_GT] = ACTIONS(1),
    [anon_sym_int] = ACTIONS(1),
    [anon_sym_bit] = ACTIONS(1),
    [anon_sym_char] = ACTIONS(1),
    [anon_sym_mem] = ACTIONS(1),
    [sym_number_literal] = ACTIONS(1),
    [anon_sym_SQUOTE] = ACTIONS(1),
  },
  [1] = {
    [sym_source_file] = STATE(103),
    [sym__definition] = STATE(88),
    [sym_parser_definition] = STATE(88),
    [aux_sym_source_file_repeat1] = STATE(88),
    [ts_builtin_sym_end] = ACTIONS(3),
    [anon_sym_def] = ACTIONS(5),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 2,
    ACTIONS(9), 6,
      anon_sym_EQ,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
      anon_sym_AMP,
    ACTIONS(7), 26,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_or,
      anon_sym_and,
      anon_sym_else,
      anon_sym_PIPE_GT,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_DOT,
      anon_sym_AMP_GT,
  [37] = 14,
    ACTIONS(11), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_LBRACE,
    ACTIONS(15), 1,
      anon_sym_RBRACE,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_if,
    ACTIONS(23), 1,
      anon_sym_let,
    ACTIONS(27), 1,
      sym_identifier,
    ACTIONS(29), 1,
      sym_number_literal,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(17), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(25), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(96), 3,
      sym__parser_block_content,
      sym_parser_sequence,
      sym_parser_choice,
    STATE(9), 4,
      sym__statement,
      sym_parse_statement,
      sym_let_statement,
      aux_sym_parser_sequence_repeat1,
    STATE(31), 11,
      sym_parser_block,
      sym_parser_array,
      sym_constraint_apply,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_val_dot,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [98] = 3,
    ACTIONS(33), 1,
      anon_sym_and,
    ACTIONS(9), 6,
      anon_sym_EQ,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
      anon_sym_AMP,
    ACTIONS(7), 25,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_or,
      anon_sym_else,
      anon_sym_PIPE_GT,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_DOT,
      anon_sym_AMP_GT,
  [137] = 2,
    ACTIONS(37), 6,
      anon_sym_EQ,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
      anon_sym_AMP,
    ACTIONS(35), 26,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_or,
      anon_sym_and,
      anon_sym_else,
      anon_sym_PIPE_GT,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_DOT,
      anon_sym_AMP_GT,
  [174] = 2,
    ACTIONS(41), 6,
      anon_sym_EQ,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
      anon_sym_AMP,
    ACTIONS(39), 26,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_or,
      anon_sym_and,
      anon_sym_else,
      anon_sym_PIPE_GT,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_DOT,
      anon_sym_AMP_GT,
  [211] = 13,
    ACTIONS(43), 1,
      anon_sym_LPAREN,
    ACTIONS(48), 1,
      anon_sym_LBRACE,
    ACTIONS(54), 1,
      anon_sym_TILDE,
    ACTIONS(57), 1,
      anon_sym_if,
    ACTIONS(60), 1,
      anon_sym_let,
    ACTIONS(66), 1,
      sym_identifier,
    ACTIONS(69), 1,
      sym_number_literal,
    ACTIONS(72), 1,
      anon_sym_SQUOTE,
    ACTIONS(51), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(46), 3,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_SEMI,
    ACTIONS(63), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(7), 4,
      sym__statement,
      sym_parse_statement,
      sym_let_statement,
      aux_sym_parser_sequence_repeat1,
    STATE(31), 11,
      sym_parser_block,
      sym_parser_array,
      sym_constraint_apply,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_val_dot,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [269] = 13,
    ACTIONS(11), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_LBRACE,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_if,
    ACTIONS(23), 1,
      anon_sym_let,
    ACTIONS(27), 1,
      sym_identifier,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(75), 1,
      sym_number_literal,
    ACTIONS(17), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(25), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(95), 3,
      sym__parser_block_content,
      sym_parser_sequence,
      sym_parser_choice,
    STATE(9), 4,
      sym__statement,
      sym_parse_statement,
      sym_let_statement,
      aux_sym_parser_sequence_repeat1,
    STATE(30), 11,
      sym_parser_block,
      sym_parser_array,
      sym_constraint_apply,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_val_dot,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [327] = 13,
    ACTIONS(11), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_LBRACE,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_if,
    ACTIONS(23), 1,
      anon_sym_let,
    ACTIONS(27), 1,
      sym_identifier,
    ACTIONS(29), 1,
      sym_number_literal,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(17), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(25), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(77), 3,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_SEMI,
    STATE(7), 4,
      sym__statement,
      sym_parse_statement,
      sym_let_statement,
      aux_sym_parser_sequence_repeat1,
    STATE(31), 11,
      sym_parser_block,
      sym_parser_array,
      sym_constraint_apply,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_val_dot,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [385] = 13,
    ACTIONS(11), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_LBRACE,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_if,
    ACTIONS(23), 1,
      anon_sym_let,
    ACTIONS(27), 1,
      sym_identifier,
    ACTIONS(29), 1,
      sym_number_literal,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(17), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(25), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(94), 3,
      sym__parser_block_content,
      sym_parser_sequence,
      sym_parser_choice,
    STATE(9), 4,
      sym__statement,
      sym_parse_statement,
      sym_let_statement,
      aux_sym_parser_sequence_repeat1,
    STATE(31), 11,
      sym_parser_block,
      sym_parser_array,
      sym_constraint_apply,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_val_dot,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [443] = 4,
    ACTIONS(33), 1,
      anon_sym_and,
    ACTIONS(81), 1,
      anon_sym_or,
    ACTIONS(83), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(79), 24,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_else,
      anon_sym_PIPE_GT,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_AMP,
      anon_sym_DOT,
  [482] = 12,
    ACTIONS(89), 1,
      anon_sym_else,
    ACTIONS(93), 1,
      anon_sym_STAR,
    ACTIONS(101), 1,
      anon_sym_PIPE,
    ACTIONS(103), 1,
      anon_sym_CARET,
    ACTIONS(105), 1,
      anon_sym_AMP,
    ACTIONS(107), 1,
      anon_sym_DOT,
    ACTIONS(87), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(91), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(95), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(97), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(99), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(85), 12,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [535] = 10,
    ACTIONS(89), 1,
      anon_sym_else,
    ACTIONS(93), 1,
      anon_sym_STAR,
    ACTIONS(105), 1,
      anon_sym_AMP,
    ACTIONS(107), 1,
      anon_sym_DOT,
    ACTIONS(87), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(91), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(95), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(97), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(99), 3,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(85), 13,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
  [584] = 5,
    ACTIONS(89), 1,
      anon_sym_else,
    ACTIONS(107), 1,
      anon_sym_DOT,
    ACTIONS(87), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(99), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(85), 20,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_AMP,
  [623] = 7,
    ACTIONS(89), 1,
      anon_sym_else,
    ACTIONS(93), 1,
      anon_sym_STAR,
    ACTIONS(107), 1,
      anon_sym_DOT,
    ACTIONS(87), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(95), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(99), 3,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(85), 18,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_AMP,
  [666] = 3,
    ACTIONS(107), 1,
      anon_sym_DOT,
    ACTIONS(99), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(85), 23,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_else,
      anon_sym_PIPE_GT,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_AMP,
  [701] = 4,
    ACTIONS(89), 1,
      anon_sym_else,
    ACTIONS(107), 1,
      anon_sym_DOT,
    ACTIONS(99), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(85), 22,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_PIPE_GT,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_AMP,
  [738] = 2,
    ACTIONS(111), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(109), 24,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_else,
      anon_sym_PIPE_GT,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_AMP,
      anon_sym_DOT,
  [771] = 2,
    ACTIONS(115), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(113), 24,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_else,
      anon_sym_PIPE_GT,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_AMP,
      anon_sym_DOT,
  [804] = 2,
    ACTIONS(119), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(117), 24,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_else,
      anon_sym_PIPE_GT,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_AMP,
      anon_sym_DOT,
  [837] = 9,
    ACTIONS(89), 1,
      anon_sym_else,
    ACTIONS(93), 1,
      anon_sym_STAR,
    ACTIONS(107), 1,
      anon_sym_DOT,
    ACTIONS(87), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(91), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(95), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(97), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(99), 3,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(85), 14,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_AMP,
  [884] = 2,
    ACTIONS(123), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(121), 24,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_else,
      anon_sym_PIPE_GT,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_AMP,
      anon_sym_DOT,
  [917] = 4,
    ACTIONS(89), 1,
      anon_sym_else,
    ACTIONS(107), 1,
      anon_sym_DOT,
    ACTIONS(127), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(125), 22,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_PIPE_GT,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_AMP,
  [954] = 11,
    ACTIONS(89), 1,
      anon_sym_else,
    ACTIONS(93), 1,
      anon_sym_STAR,
    ACTIONS(103), 1,
      anon_sym_CARET,
    ACTIONS(105), 1,
      anon_sym_AMP,
    ACTIONS(107), 1,
      anon_sym_DOT,
    ACTIONS(87), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(91), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(95), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(97), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(99), 3,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(85), 12,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1005] = 2,
    ACTIONS(131), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(129), 24,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_else,
      anon_sym_PIPE_GT,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_AMP,
      anon_sym_DOT,
  [1038] = 8,
    ACTIONS(89), 1,
      anon_sym_else,
    ACTIONS(93), 1,
      anon_sym_STAR,
    ACTIONS(107), 1,
      anon_sym_DOT,
    ACTIONS(87), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(91), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(95), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(99), 3,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(85), 16,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_AMP,
  [1083] = 2,
    ACTIONS(135), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(133), 24,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_else,
      anon_sym_PIPE_GT,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_AMP,
      anon_sym_DOT,
  [1116] = 3,
    ACTIONS(139), 1,
      anon_sym_COLON,
    ACTIONS(141), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(137), 21,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
      anon_sym_COMMA,
      anon_sym_else,
      anon_sym_PIPE_GT,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_SLASH,
      anon_sym_PERCENT,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_AMP,
      anon_sym_DOT,
  [1149] = 14,
    ACTIONS(89), 1,
      anon_sym_else,
    ACTIONS(93), 1,
      anon_sym_STAR,
    ACTIONS(101), 1,
      anon_sym_PIPE,
    ACTIONS(103), 1,
      anon_sym_CARET,
    ACTIONS(105), 1,
      anon_sym_AMP,
    ACTIONS(107), 1,
      anon_sym_DOT,
    ACTIONS(87), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(91), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(95), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(97), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(143), 2,
      ts_builtin_sym_end,
      anon_sym_def,
    ACTIONS(149), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(145), 3,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
    ACTIONS(147), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1203] = 15,
    ACTIONS(89), 1,
      anon_sym_else,
    ACTIONS(93), 1,
      anon_sym_STAR,
    ACTIONS(101), 1,
      anon_sym_PIPE,
    ACTIONS(103), 1,
      anon_sym_CARET,
    ACTIONS(105), 1,
      anon_sym_AMP,
    ACTIONS(107), 1,
      anon_sym_DOT,
    ACTIONS(151), 1,
      anon_sym_RPAREN,
    ACTIONS(153), 1,
      anon_sym_COMMA,
    ACTIONS(87), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(91), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(95), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(97), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(149), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(145), 3,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
    ACTIONS(147), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1259] = 14,
    ACTIONS(89), 1,
      anon_sym_else,
    ACTIONS(93), 1,
      anon_sym_STAR,
    ACTIONS(101), 1,
      anon_sym_PIPE,
    ACTIONS(103), 1,
      anon_sym_CARET,
    ACTIONS(105), 1,
      anon_sym_AMP,
    ACTIONS(107), 1,
      anon_sym_DOT,
    ACTIONS(153), 1,
      anon_sym_COMMA,
    ACTIONS(87), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(91), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(95), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(97), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(149), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(145), 3,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
    ACTIONS(147), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1312] = 14,
    ACTIONS(89), 1,
      anon_sym_else,
    ACTIONS(93), 1,
      anon_sym_STAR,
    ACTIONS(101), 1,
      anon_sym_PIPE,
    ACTIONS(103), 1,
      anon_sym_CARET,
    ACTIONS(105), 1,
      anon_sym_AMP,
    ACTIONS(107), 1,
      anon_sym_DOT,
    ACTIONS(151), 1,
      anon_sym_RPAREN,
    ACTIONS(87), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(91), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(95), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(97), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(149), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(145), 3,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
    ACTIONS(147), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1365] = 14,
    ACTIONS(89), 1,
      anon_sym_else,
    ACTIONS(93), 1,
      anon_sym_STAR,
    ACTIONS(101), 1,
      anon_sym_PIPE,
    ACTIONS(103), 1,
      anon_sym_CARET,
    ACTIONS(105), 1,
      anon_sym_AMP,
    ACTIONS(107), 1,
      anon_sym_DOT,
    ACTIONS(155), 1,
      anon_sym_COMMA,
    ACTIONS(87), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(91), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(95), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(97), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(149), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(145), 3,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
    ACTIONS(147), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1418] = 14,
    ACTIONS(89), 1,
      anon_sym_else,
    ACTIONS(93), 1,
      anon_sym_STAR,
    ACTIONS(101), 1,
      anon_sym_PIPE,
    ACTIONS(103), 1,
      anon_sym_CARET,
    ACTIONS(105), 1,
      anon_sym_AMP,
    ACTIONS(107), 1,
      anon_sym_DOT,
    ACTIONS(157), 1,
      anon_sym_COMMA,
    ACTIONS(87), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(91), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(95), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(97), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(149), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(145), 3,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
    ACTIONS(147), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1471] = 14,
    ACTIONS(89), 1,
      anon_sym_else,
    ACTIONS(93), 1,
      anon_sym_STAR,
    ACTIONS(101), 1,
      anon_sym_PIPE,
    ACTIONS(103), 1,
      anon_sym_CARET,
    ACTIONS(105), 1,
      anon_sym_AMP,
    ACTIONS(107), 1,
      anon_sym_DOT,
    ACTIONS(159), 1,
      anon_sym_RBRACK,
    ACTIONS(87), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(91), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(95), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(97), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(149), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(145), 3,
      anon_sym_TILDE,
      anon_sym_if,
      anon_sym_try,
    ACTIONS(147), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1524] = 10,
    ACTIONS(13), 1,
      anon_sym_LBRACE,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_if,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(161), 1,
      anon_sym_LPAREN,
    ACTIONS(163), 1,
      sym_identifier,
    ACTIONS(165), 1,
      sym_number_literal,
    ACTIONS(17), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(25), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(34), 11,
      sym_parser_block,
      sym_parser_array,
      sym_constraint_apply,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_val_dot,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1568] = 10,
    ACTIONS(13), 1,
      anon_sym_LBRACE,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_if,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(161), 1,
      anon_sym_LPAREN,
    ACTIONS(167), 1,
      sym_identifier,
    ACTIONS(169), 1,
      sym_number_literal,
    ACTIONS(17), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(25), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(35), 11,
      sym_parser_block,
      sym_parser_array,
      sym_constraint_apply,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_val_dot,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1612] = 10,
    ACTIONS(13), 1,
      anon_sym_LBRACE,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_if,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(161), 1,
      anon_sym_LPAREN,
    ACTIONS(171), 1,
      sym_identifier,
    ACTIONS(173), 1,
      sym_number_literal,
    ACTIONS(17), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(25), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(21), 11,
      sym_parser_block,
      sym_parser_array,
      sym_constraint_apply,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_val_dot,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1656] = 10,
    ACTIONS(13), 1,
      anon_sym_LBRACE,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_if,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(161), 1,
      anon_sym_LPAREN,
    ACTIONS(175), 1,
      sym_identifier,
    ACTIONS(177), 1,
      sym_number_literal,
    ACTIONS(17), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(25), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(13), 11,
      sym_parser_block,
      sym_parser_array,
      sym_constraint_apply,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_val_dot,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1700] = 10,
    ACTIONS(13), 1,
      anon_sym_LBRACE,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_if,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(161), 1,
      anon_sym_LPAREN,
    ACTIONS(179), 1,
      sym_identifier,
    ACTIONS(181), 1,
      sym_number_literal,
    ACTIONS(17), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(25), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(24), 11,
      sym_parser_block,
      sym_parser_array,
      sym_constraint_apply,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_val_dot,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1744] = 10,
    ACTIONS(13), 1,
      anon_sym_LBRACE,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_if,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(161), 1,
      anon_sym_LPAREN,
    ACTIONS(183), 1,
      sym_identifier,
    ACTIONS(185), 1,
      sym_number_literal,
    ACTIONS(17), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(25), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(12), 11,
      sym_parser_block,
      sym_parser_array,
      sym_constraint_apply,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_val_dot,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1788] = 10,
    ACTIONS(13), 1,
      anon_sym_LBRACE,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_if,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(161), 1,
      anon_sym_LPAREN,
    ACTIONS(187), 1,
      sym_identifier,
    ACTIONS(189), 1,
      sym_number_literal,
    ACTIONS(17), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(25), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(26), 11,
      sym_parser_block,
      sym_parser_array,
      sym_constraint_apply,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_val_dot,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1832] = 10,
    ACTIONS(13), 1,
      anon_sym_LBRACE,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_if,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(161), 1,
      anon_sym_LPAREN,
    ACTIONS(191), 1,
      sym_identifier,
    ACTIONS(193), 1,
      sym_number_literal,
    ACTIONS(17), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(25), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(29), 11,
      sym_parser_block,
      sym_parser_array,
      sym_constraint_apply,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_val_dot,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1876] = 10,
    ACTIONS(13), 1,
      anon_sym_LBRACE,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_if,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(161), 1,
      anon_sym_LPAREN,
    ACTIONS(195), 1,
      sym_identifier,
    ACTIONS(197), 1,
      sym_number_literal,
    ACTIONS(17), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(25), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(14), 11,
      sym_parser_block,
      sym_parser_array,
      sym_constraint_apply,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_val_dot,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1920] = 10,
    ACTIONS(13), 1,
      anon_sym_LBRACE,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_if,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(161), 1,
      anon_sym_LPAREN,
    ACTIONS(199), 1,
      sym_identifier,
    ACTIONS(201), 1,
      sym_number_literal,
    ACTIONS(17), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(25), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(15), 11,
      sym_parser_block,
      sym_parser_array,
      sym_constraint_apply,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_val_dot,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1964] = 10,
    ACTIONS(13), 1,
      anon_sym_LBRACE,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_if,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(161), 1,
      anon_sym_LPAREN,
    ACTIONS(203), 1,
      sym_identifier,
    ACTIONS(205), 1,
      sym_number_literal,
    ACTIONS(17), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(25), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(16), 11,
      sym_parser_block,
      sym_parser_array,
      sym_constraint_apply,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_val_dot,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [2008] = 10,
    ACTIONS(13), 1,
      anon_sym_LBRACE,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_if,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(161), 1,
      anon_sym_LPAREN,
    ACTIONS(207), 1,
      sym_identifier,
    ACTIONS(209), 1,
      sym_number_literal,
    ACTIONS(17), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(25), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(17), 11,
      sym_parser_block,
      sym_parser_array,
      sym_constraint_apply,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_val_dot,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [2052] = 10,
    ACTIONS(13), 1,
      anon_sym_LBRACE,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_if,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(161), 1,
      anon_sym_LPAREN,
    ACTIONS(211), 1,
      sym_identifier,
    ACTIONS(213), 1,
      sym_number_literal,
    ACTIONS(17), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(25), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(32), 11,
      sym_parser_block,
      sym_parser_array,
      sym_constraint_apply,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_val_dot,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [2096] = 10,
    ACTIONS(13), 1,
      anon_sym_LBRACE,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_if,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(161), 1,
      anon_sym_LPAREN,
    ACTIONS(215), 1,
      sym_identifier,
    ACTIONS(217), 1,
      sym_number_literal,
    ACTIONS(17), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(25), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(33), 11,
      sym_parser_block,
      sym_parser_array,
      sym_constraint_apply,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_val_dot,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [2140] = 10,
    ACTIONS(13), 1,
      anon_sym_LBRACE,
    ACTIONS(19), 1,
      anon_sym_TILDE,
    ACTIONS(21), 1,
      anon_sym_if,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(161), 1,
      anon_sym_LPAREN,
    ACTIONS(219), 1,
      sym_identifier,
    ACTIONS(221), 1,
      sym_number_literal,
    ACTIONS(17), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(25), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(23), 11,
      sym_parser_block,
      sym_parser_array,
      sym_constraint_apply,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym_val_dot,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [2184] = 9,
    ACTIONS(223), 1,
      anon_sym_STAR_GT,
    ACTIONS(225), 1,
      anon_sym_LPAREN,
    ACTIONS(229), 1,
      anon_sym_RBRACK,
    ACTIONS(233), 1,
      sym_type_var,
    ACTIONS(235), 1,
      sym_identifier,
    STATE(55), 1,
      aux_sym_parserdef_ref_repeat1,
    ACTIONS(227), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(231), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(91), 8,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym_type_constraint,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2223] = 9,
    ACTIONS(223), 1,
      anon_sym_STAR_GT,
    ACTIONS(225), 1,
      anon_sym_LPAREN,
    ACTIONS(233), 1,
      sym_type_var,
    ACTIONS(235), 1,
      sym_identifier,
    ACTIONS(237), 1,
      anon_sym_RBRACK,
    STATE(54), 1,
      aux_sym_parserdef_ref_repeat1,
    ACTIONS(227), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(231), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(91), 8,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym_type_constraint,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2262] = 9,
    ACTIONS(223), 1,
      anon_sym_STAR_GT,
    ACTIONS(225), 1,
      anon_sym_LPAREN,
    ACTIONS(233), 1,
      sym_type_var,
    ACTIONS(235), 1,
      sym_identifier,
    ACTIONS(239), 1,
      anon_sym_RBRACK,
    STATE(51), 1,
      aux_sym_parserdef_ref_repeat1,
    ACTIONS(227), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(231), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(91), 8,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym_type_constraint,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2301] = 9,
    ACTIONS(223), 1,
      anon_sym_STAR_GT,
    ACTIONS(225), 1,
      anon_sym_LPAREN,
    ACTIONS(233), 1,
      sym_type_var,
    ACTIONS(235), 1,
      sym_identifier,
    ACTIONS(241), 1,
      anon_sym_RBRACK,
    STATE(55), 1,
      aux_sym_parserdef_ref_repeat1,
    ACTIONS(227), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(231), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(91), 8,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym_type_constraint,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2340] = 9,
    ACTIONS(243), 1,
      anon_sym_STAR_GT,
    ACTIONS(246), 1,
      anon_sym_LPAREN,
    ACTIONS(252), 1,
      anon_sym_RBRACK,
    ACTIONS(257), 1,
      sym_type_var,
    ACTIONS(260), 1,
      sym_identifier,
    STATE(55), 1,
      aux_sym_parserdef_ref_repeat1,
    ACTIONS(249), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(254), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(91), 8,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym_type_constraint,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2379] = 7,
    ACTIONS(223), 1,
      anon_sym_STAR_GT,
    ACTIONS(225), 1,
      anon_sym_LPAREN,
    ACTIONS(235), 1,
      sym_identifier,
    ACTIONS(263), 1,
      sym_type_var,
    ACTIONS(227), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(231), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(76), 8,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym_type_constraint,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2412] = 7,
    ACTIONS(223), 1,
      anon_sym_STAR_GT,
    ACTIONS(225), 1,
      anon_sym_LPAREN,
    ACTIONS(235), 1,
      sym_identifier,
    ACTIONS(265), 1,
      sym_type_var,
    ACTIONS(227), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(231), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(93), 8,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym_type_constraint,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2445] = 7,
    ACTIONS(223), 1,
      anon_sym_STAR_GT,
    ACTIONS(225), 1,
      anon_sym_LPAREN,
    ACTIONS(235), 1,
      sym_identifier,
    ACTIONS(267), 1,
      sym_type_var,
    ACTIONS(227), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(231), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(92), 8,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym_type_constraint,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2478] = 7,
    ACTIONS(223), 1,
      anon_sym_STAR_GT,
    ACTIONS(225), 1,
      anon_sym_LPAREN,
    ACTIONS(235), 1,
      sym_identifier,
    ACTIONS(269), 1,
      sym_type_var,
    ACTIONS(227), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(231), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(90), 8,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym_type_constraint,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2511] = 7,
    ACTIONS(223), 1,
      anon_sym_STAR_GT,
    ACTIONS(225), 1,
      anon_sym_LPAREN,
    ACTIONS(235), 1,
      sym_identifier,
    ACTIONS(271), 1,
      sym_type_var,
    ACTIONS(227), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(231), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(89), 8,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym_type_constraint,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2544] = 7,
    ACTIONS(223), 1,
      anon_sym_STAR_GT,
    ACTIONS(225), 1,
      anon_sym_LPAREN,
    ACTIONS(263), 1,
      sym_type_var,
    ACTIONS(273), 1,
      sym_identifier,
    ACTIONS(227), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(231), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(76), 8,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym_type_constraint,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2577] = 7,
    ACTIONS(223), 1,
      anon_sym_STAR_GT,
    ACTIONS(225), 1,
      anon_sym_LPAREN,
    ACTIONS(235), 1,
      sym_identifier,
    ACTIONS(275), 1,
      sym_type_var,
    ACTIONS(227), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(231), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(82), 8,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym_type_constraint,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2610] = 2,
    ACTIONS(279), 5,
      anon_sym_for,
      anon_sym_each,
      anon_sym_if,
      anon_sym_let,
      sym_identifier,
    ACTIONS(277), 11,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_SEMI,
      anon_sym_TILDE,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_number_literal,
      anon_sym_SQUOTE,
  [2631] = 2,
    ACTIONS(283), 5,
      anon_sym_for,
      anon_sym_each,
      anon_sym_if,
      anon_sym_let,
      sym_identifier,
    ACTIONS(281), 11,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_SEMI,
      anon_sym_TILDE,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_number_literal,
      anon_sym_SQUOTE,
  [2652] = 2,
    ACTIONS(287), 5,
      anon_sym_for,
      anon_sym_each,
      anon_sym_if,
      anon_sym_let,
      sym_identifier,
    ACTIONS(285), 11,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_SEMI,
      anon_sym_TILDE,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_number_literal,
      anon_sym_SQUOTE,
  [2673] = 2,
    ACTIONS(291), 5,
      anon_sym_for,
      anon_sym_each,
      anon_sym_if,
      anon_sym_let,
      sym_identifier,
    ACTIONS(289), 11,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_SEMI,
      anon_sym_TILDE,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_number_literal,
      anon_sym_SQUOTE,
  [2694] = 2,
    ACTIONS(293), 4,
      anon_sym_STAR_GT,
      anon_sym_LPAREN,
      anon_sym_RBRACK,
      sym_type_var,
    ACTIONS(295), 7,
      anon_sym_for,
      anon_sym_each,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
      sym_identifier,
  [2710] = 4,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(297), 1,
      anon_sym_BANG,
    ACTIONS(299), 2,
      sym_identifier,
      sym_number_literal,
    STATE(72), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2729] = 4,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(297), 1,
      anon_sym_BANG,
    ACTIONS(301), 2,
      sym_identifier,
      sym_number_literal,
    STATE(4), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2748] = 4,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(297), 1,
      anon_sym_BANG,
    ACTIONS(303), 2,
      sym_identifier,
      sym_number_literal,
    STATE(2), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2767] = 4,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(297), 1,
      anon_sym_BANG,
    ACTIONS(305), 2,
      sym_identifier,
      sym_number_literal,
    STATE(11), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2786] = 3,
    ACTIONS(33), 1,
      anon_sym_and,
    ACTIONS(81), 1,
      anon_sym_or,
    ACTIONS(307), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2802] = 2,
    ACTIONS(311), 1,
      anon_sym_LBRACK,
    ACTIONS(309), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2815] = 2,
    ACTIONS(315), 1,
      anon_sym_LBRACK,
    ACTIONS(313), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2828] = 1,
    ACTIONS(317), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2838] = 1,
    ACTIONS(319), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2848] = 1,
    ACTIONS(321), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2858] = 1,
    ACTIONS(323), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2868] = 1,
    ACTIONS(325), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2878] = 1,
    ACTIONS(327), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2888] = 1,
    ACTIONS(329), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2898] = 1,
    ACTIONS(331), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2908] = 1,
    ACTIONS(333), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_TILDE,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2918] = 3,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(335), 2,
      sym_identifier,
      sym_number_literal,
    STATE(20), 3,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2931] = 3,
    ACTIONS(31), 1,
      anon_sym_SQUOTE,
    ACTIONS(337), 2,
      sym_identifier,
      sym_number_literal,
    STATE(6), 3,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2944] = 3,
    ACTIONS(339), 1,
      ts_builtin_sym_end,
    ACTIONS(341), 1,
      anon_sym_def,
    STATE(86), 3,
      sym__definition,
      sym_parser_definition,
      aux_sym_source_file_repeat1,
  [2956] = 3,
    ACTIONS(315), 1,
      anon_sym_LBRACK,
    ACTIONS(344), 1,
      anon_sym_EQ,
    ACTIONS(313), 3,
      anon_sym_STAR_GT,
      anon_sym_TILDE,
      anon_sym_AMP_GT,
  [2968] = 3,
    ACTIONS(5), 1,
      anon_sym_def,
    ACTIONS(346), 1,
      ts_builtin_sym_end,
    STATE(86), 3,
      sym__definition,
      sym_parser_definition,
      aux_sym_source_file_repeat1,
  [2980] = 4,
    ACTIONS(348), 1,
      anon_sym_STAR_GT,
    ACTIONS(350), 1,
      anon_sym_RPAREN,
    ACTIONS(352), 1,
      anon_sym_TILDE,
    ACTIONS(354), 1,
      anon_sym_AMP_GT,
  [2993] = 4,
    ACTIONS(348), 1,
      anon_sym_STAR_GT,
    ACTIONS(352), 1,
      anon_sym_TILDE,
    ACTIONS(354), 1,
      anon_sym_AMP_GT,
    ACTIONS(356), 1,
      anon_sym_RBRACK,
  [3006] = 4,
    ACTIONS(348), 1,
      anon_sym_STAR_GT,
    ACTIONS(352), 1,
      anon_sym_TILDE,
    ACTIONS(354), 1,
      anon_sym_AMP_GT,
    ACTIONS(358), 1,
      anon_sym_COMMA,
  [3019] = 4,
    ACTIONS(348), 1,
      anon_sym_STAR_GT,
    ACTIONS(352), 1,
      anon_sym_TILDE,
    ACTIONS(354), 1,
      anon_sym_AMP_GT,
    ACTIONS(360), 1,
      anon_sym_EQ,
  [3032] = 3,
    ACTIONS(352), 1,
      anon_sym_TILDE,
    ACTIONS(354), 1,
      anon_sym_AMP_GT,
    ACTIONS(362), 1,
      anon_sym_STAR_GT,
  [3042] = 1,
    ACTIONS(364), 3,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_SEMI,
  [3048] = 2,
    ACTIONS(366), 1,
      anon_sym_RPAREN,
    ACTIONS(368), 1,
      anon_sym_SEMI,
  [3055] = 2,
    ACTIONS(368), 1,
      anon_sym_SEMI,
    ACTIONS(370), 1,
      anon_sym_RBRACE,
  [3062] = 1,
    ACTIONS(372), 1,
      sym_identifier,
  [3066] = 1,
    ACTIONS(374), 1,
      anon_sym_LBRACK,
  [3070] = 1,
    ACTIONS(376), 1,
      anon_sym_COLON,
  [3074] = 1,
    ACTIONS(378), 1,
      anon_sym_SQUOTE,
  [3078] = 1,
    ACTIONS(380), 1,
      sym_identifier,
  [3082] = 1,
    ACTIONS(382), 1,
      aux_sym_char_literal_token1,
  [3086] = 1,
    ACTIONS(384), 1,
      ts_builtin_sym_end,
  [3090] = 1,
    ACTIONS(386), 1,
      anon_sym_LBRACK,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 37,
  [SMALL_STATE(4)] = 98,
  [SMALL_STATE(5)] = 137,
  [SMALL_STATE(6)] = 174,
  [SMALL_STATE(7)] = 211,
  [SMALL_STATE(8)] = 269,
  [SMALL_STATE(9)] = 327,
  [SMALL_STATE(10)] = 385,
  [SMALL_STATE(11)] = 443,
  [SMALL_STATE(12)] = 482,
  [SMALL_STATE(13)] = 535,
  [SMALL_STATE(14)] = 584,
  [SMALL_STATE(15)] = 623,
  [SMALL_STATE(16)] = 666,
  [SMALL_STATE(17)] = 701,
  [SMALL_STATE(18)] = 738,
  [SMALL_STATE(19)] = 771,
  [SMALL_STATE(20)] = 804,
  [SMALL_STATE(21)] = 837,
  [SMALL_STATE(22)] = 884,
  [SMALL_STATE(23)] = 917,
  [SMALL_STATE(24)] = 954,
  [SMALL_STATE(25)] = 1005,
  [SMALL_STATE(26)] = 1038,
  [SMALL_STATE(27)] = 1083,
  [SMALL_STATE(28)] = 1116,
  [SMALL_STATE(29)] = 1149,
  [SMALL_STATE(30)] = 1203,
  [SMALL_STATE(31)] = 1259,
  [SMALL_STATE(32)] = 1312,
  [SMALL_STATE(33)] = 1365,
  [SMALL_STATE(34)] = 1418,
  [SMALL_STATE(35)] = 1471,
  [SMALL_STATE(36)] = 1524,
  [SMALL_STATE(37)] = 1568,
  [SMALL_STATE(38)] = 1612,
  [SMALL_STATE(39)] = 1656,
  [SMALL_STATE(40)] = 1700,
  [SMALL_STATE(41)] = 1744,
  [SMALL_STATE(42)] = 1788,
  [SMALL_STATE(43)] = 1832,
  [SMALL_STATE(44)] = 1876,
  [SMALL_STATE(45)] = 1920,
  [SMALL_STATE(46)] = 1964,
  [SMALL_STATE(47)] = 2008,
  [SMALL_STATE(48)] = 2052,
  [SMALL_STATE(49)] = 2096,
  [SMALL_STATE(50)] = 2140,
  [SMALL_STATE(51)] = 2184,
  [SMALL_STATE(52)] = 2223,
  [SMALL_STATE(53)] = 2262,
  [SMALL_STATE(54)] = 2301,
  [SMALL_STATE(55)] = 2340,
  [SMALL_STATE(56)] = 2379,
  [SMALL_STATE(57)] = 2412,
  [SMALL_STATE(58)] = 2445,
  [SMALL_STATE(59)] = 2478,
  [SMALL_STATE(60)] = 2511,
  [SMALL_STATE(61)] = 2544,
  [SMALL_STATE(62)] = 2577,
  [SMALL_STATE(63)] = 2610,
  [SMALL_STATE(64)] = 2631,
  [SMALL_STATE(65)] = 2652,
  [SMALL_STATE(66)] = 2673,
  [SMALL_STATE(67)] = 2694,
  [SMALL_STATE(68)] = 2710,
  [SMALL_STATE(69)] = 2729,
  [SMALL_STATE(70)] = 2748,
  [SMALL_STATE(71)] = 2767,
  [SMALL_STATE(72)] = 2786,
  [SMALL_STATE(73)] = 2802,
  [SMALL_STATE(74)] = 2815,
  [SMALL_STATE(75)] = 2828,
  [SMALL_STATE(76)] = 2838,
  [SMALL_STATE(77)] = 2848,
  [SMALL_STATE(78)] = 2858,
  [SMALL_STATE(79)] = 2868,
  [SMALL_STATE(80)] = 2878,
  [SMALL_STATE(81)] = 2888,
  [SMALL_STATE(82)] = 2898,
  [SMALL_STATE(83)] = 2908,
  [SMALL_STATE(84)] = 2918,
  [SMALL_STATE(85)] = 2931,
  [SMALL_STATE(86)] = 2944,
  [SMALL_STATE(87)] = 2956,
  [SMALL_STATE(88)] = 2968,
  [SMALL_STATE(89)] = 2980,
  [SMALL_STATE(90)] = 2993,
  [SMALL_STATE(91)] = 3006,
  [SMALL_STATE(92)] = 3019,
  [SMALL_STATE(93)] = 3032,
  [SMALL_STATE(94)] = 3042,
  [SMALL_STATE(95)] = 3048,
  [SMALL_STATE(96)] = 3055,
  [SMALL_STATE(97)] = 3062,
  [SMALL_STATE(98)] = 3066,
  [SMALL_STATE(99)] = 3070,
  [SMALL_STATE(100)] = 3074,
  [SMALL_STATE(101)] = 3078,
  [SMALL_STATE(102)] = 3082,
  [SMALL_STATE(103)] = 3086,
  [SMALL_STATE(104)] = 3090,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(57),
  [7] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_constraint_expression, 3, .production_id = 3),
  [9] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_binary_constraint_expression, 3, .production_id = 3),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [17] = {.entry = {.count = 1, .reusable = false}}, SHIFT(104),
  [19] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [21] = {.entry = {.count = 1, .reusable = false}}, SHIFT(50),
  [23] = {.entry = {.count = 1, .reusable = false}}, SHIFT(101),
  [25] = {.entry = {.count = 1, .reusable = true}}, SHIFT(50),
  [27] = {.entry = {.count = 1, .reusable = false}}, SHIFT(28),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [31] = {.entry = {.count = 1, .reusable = true}}, SHIFT(102),
  [33] = {.entry = {.count = 1, .reusable = true}}, SHIFT(70),
  [35] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_char_literal, 3),
  [37] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_char_literal, 3),
  [39] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_constraint_expression, 2),
  [41] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unary_constraint_expression, 2),
  [43] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(8),
  [46] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2),
  [48] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(3),
  [51] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(104),
  [54] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(25),
  [57] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(50),
  [60] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(101),
  [63] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(50),
  [66] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(28),
  [69] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(31),
  [72] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(102),
  [75] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [77] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_sequence, 1),
  [79] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_constraint_apply, 3, .production_id = 3),
  [81] = {.entry = {.count = 1, .reusable = true}}, SHIFT(69),
  [83] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_constraint_apply, 3, .production_id = 3),
  [85] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_expression, 3, .production_id = 3),
  [87] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [89] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [91] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [93] = {.entry = {.count = 1, .reusable = false}}, SHIFT(44),
  [95] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
  [97] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
  [99] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_binary_expression, 3, .production_id = 3),
  [101] = {.entry = {.count = 1, .reusable = false}}, SHIFT(40),
  [103] = {.entry = {.count = 1, .reusable = true}}, SHIFT(39),
  [105] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [107] = {.entry = {.count = 1, .reusable = true}}, SHIFT(84),
  [109] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_array, 4, .production_id = 5),
  [111] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parser_array, 4, .production_id = 5),
  [113] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_block, 3, .production_id = 11),
  [115] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parser_block, 3, .production_id = 11),
  [117] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_val_dot, 3, .production_id = 3),
  [119] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_val_dot, 3, .production_id = 3),
  [121] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__expression, 3),
  [123] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__expression, 3),
  [125] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_expression, 2, .production_id = 2),
  [127] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unary_expression, 2, .production_id = 2),
  [129] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_single, 1),
  [131] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_single, 1),
  [133] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_block, 2),
  [135] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parser_block, 2),
  [137] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__atom, 1),
  [139] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
  [141] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__atom, 1),
  [143] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_definition, 6, .production_id = 9),
  [145] = {.entry = {.count = 1, .reusable = true}}, SHIFT(71),
  [147] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [149] = {.entry = {.count = 1, .reusable = false}}, SHIFT(41),
  [151] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [153] = {.entry = {.count = 1, .reusable = true}}, SHIFT(65),
  [155] = {.entry = {.count = 1, .reusable = true}}, SHIFT(66),
  [157] = {.entry = {.count = 1, .reusable = true}}, SHIFT(64),
  [159] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [161] = {.entry = {.count = 1, .reusable = true}}, SHIFT(48),
  [163] = {.entry = {.count = 1, .reusable = false}}, SHIFT(34),
  [165] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [167] = {.entry = {.count = 1, .reusable = false}}, SHIFT(35),
  [169] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [171] = {.entry = {.count = 1, .reusable = false}}, SHIFT(21),
  [173] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [175] = {.entry = {.count = 1, .reusable = false}}, SHIFT(13),
  [177] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [179] = {.entry = {.count = 1, .reusable = false}}, SHIFT(24),
  [181] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [183] = {.entry = {.count = 1, .reusable = false}}, SHIFT(12),
  [185] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [187] = {.entry = {.count = 1, .reusable = false}}, SHIFT(26),
  [189] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [191] = {.entry = {.count = 1, .reusable = false}}, SHIFT(29),
  [193] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [195] = {.entry = {.count = 1, .reusable = false}}, SHIFT(14),
  [197] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [199] = {.entry = {.count = 1, .reusable = false}}, SHIFT(15),
  [201] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [203] = {.entry = {.count = 1, .reusable = false}}, SHIFT(16),
  [205] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [207] = {.entry = {.count = 1, .reusable = false}}, SHIFT(17),
  [209] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [211] = {.entry = {.count = 1, .reusable = false}}, SHIFT(32),
  [213] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [215] = {.entry = {.count = 1, .reusable = false}}, SHIFT(33),
  [217] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [219] = {.entry = {.count = 1, .reusable = false}}, SHIFT(23),
  [221] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [223] = {.entry = {.count = 1, .reusable = true}}, SHIFT(62),
  [225] = {.entry = {.count = 1, .reusable = true}}, SHIFT(60),
  [227] = {.entry = {.count = 1, .reusable = false}}, SHIFT(98),
  [229] = {.entry = {.count = 1, .reusable = true}}, SHIFT(75),
  [231] = {.entry = {.count = 1, .reusable = false}}, SHIFT(83),
  [233] = {.entry = {.count = 1, .reusable = true}}, SHIFT(91),
  [235] = {.entry = {.count = 1, .reusable = false}}, SHIFT(74),
  [237] = {.entry = {.count = 1, .reusable = true}}, SHIFT(81),
  [239] = {.entry = {.count = 1, .reusable = true}}, SHIFT(79),
  [241] = {.entry = {.count = 1, .reusable = true}}, SHIFT(78),
  [243] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(62),
  [246] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(60),
  [249] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(98),
  [252] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8),
  [254] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(83),
  [257] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(91),
  [260] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(74),
  [263] = {.entry = {.count = 1, .reusable = true}}, SHIFT(76),
  [265] = {.entry = {.count = 1, .reusable = true}}, SHIFT(93),
  [267] = {.entry = {.count = 1, .reusable = true}}, SHIFT(92),
  [269] = {.entry = {.count = 1, .reusable = true}}, SHIFT(90),
  [271] = {.entry = {.count = 1, .reusable = true}}, SHIFT(89),
  [273] = {.entry = {.count = 1, .reusable = false}}, SHIFT(87),
  [275] = {.entry = {.count = 1, .reusable = true}}, SHIFT(82),
  [277] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 3),
  [279] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 3),
  [281] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parse_statement, 4, .production_id = 14),
  [283] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parse_statement, 4, .production_id = 14),
  [285] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parse_statement, 2, .production_id = 12),
  [287] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parse_statement, 2, .production_id = 12),
  [289] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_let_statement, 7, .production_id = 15),
  [291] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_let_statement, 7, .production_id = 15),
  [293] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 6),
  [295] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 6),
  [297] = {.entry = {.count = 1, .reusable = true}}, SHIFT(85),
  [299] = {.entry = {.count = 1, .reusable = true}}, SHIFT(72),
  [301] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [303] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [305] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [307] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type_constraint, 3, .production_id = 3),
  [309] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 3, .production_id = 4),
  [311] = {.entry = {.count = 1, .reusable = true}}, SHIFT(52),
  [313] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 1, .production_id = 1),
  [315] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [317] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 4, .production_id = 7),
  [319] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_type_expression, 3, .production_id = 3),
  [321] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type_array, 4, .production_id = 5),
  [323] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 6, .production_id = 10),
  [325] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 3, .production_id = 1),
  [327] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__type_expression, 3),
  [329] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 5, .production_id = 4),
  [331] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_type_expression, 2, .production_id = 2),
  [333] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_primitive_type, 1),
  [335] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [337] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [339] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [341] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(57),
  [344] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [346] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [348] = {.entry = {.count = 1, .reusable = true}}, SHIFT(56),
  [350] = {.entry = {.count = 1, .reusable = true}}, SHIFT(80),
  [352] = {.entry = {.count = 1, .reusable = true}}, SHIFT(68),
  [354] = {.entry = {.count = 1, .reusable = true}}, SHIFT(97),
  [356] = {.entry = {.count = 1, .reusable = true}}, SHIFT(77),
  [358] = {.entry = {.count = 1, .reusable = true}}, SHIFT(67),
  [360] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [362] = {.entry = {.count = 1, .reusable = true}}, SHIFT(61),
  [364] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_choice, 3, .production_id = 13),
  [366] = {.entry = {.count = 1, .reusable = true}}, SHIFT(63),
  [368] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [370] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [372] = {.entry = {.count = 1, .reusable = true}}, SHIFT(73),
  [374] = {.entry = {.count = 1, .reusable = true}}, SHIFT(59),
  [376] = {.entry = {.count = 1, .reusable = true}}, SHIFT(58),
  [378] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [380] = {.entry = {.count = 1, .reusable = true}}, SHIFT(99),
  [382] = {.entry = {.count = 1, .reusable = true}}, SHIFT(100),
  [384] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [386] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
};

#ifdef __cplusplus
extern "C" {
#endif
#ifdef _WIN32
#define extern __declspec(dllexport)
#endif

extern const TSLanguage *tree_sitter_yabo(void) {
  static const TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .state_count = STATE_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .production_id_count = PRODUCTION_ID_COUNT,
    .field_count = FIELD_COUNT,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .parse_table = &ts_parse_table[0][0],
    .small_parse_table = ts_small_parse_table,
    .small_parse_table_map = ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .symbol_names = ts_symbol_names,
    .field_names = ts_field_names,
    .field_map_slices = ts_field_map_slices,
    .field_map_entries = ts_field_map_entries,
    .symbol_metadata = ts_symbol_metadata,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .alias_sequences = &ts_alias_sequences[0][0],
    .lex_modes = ts_lex_modes,
    .lex_fn = ts_lex,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
