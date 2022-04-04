#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 13
#define STATE_COUNT 105
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 81
#define ALIAS_COUNT 0
#define TOKEN_COUNT 50
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
  anon_sym_TILDE = 6,
  anon_sym_LBRACE = 7,
  anon_sym_RBRACE = 8,
  anon_sym_SEMI = 9,
  anon_sym_for = 10,
  anon_sym_each = 11,
  anon_sym_LBRACK = 12,
  anon_sym_RBRACK = 13,
  anon_sym_COLON = 14,
  anon_sym_COMMA = 15,
  anon_sym_let = 16,
  anon_sym_or = 17,
  anon_sym_and = 18,
  anon_sym_BANG = 19,
  anon_sym_DOT = 20,
  anon_sym_else = 21,
  anon_sym_PIPE_GT = 22,
  anon_sym_PLUS = 23,
  anon_sym_DASH = 24,
  anon_sym_STAR = 25,
  anon_sym_SLASH = 26,
  anon_sym_PERCENT = 27,
  anon_sym_LT_LT = 28,
  anon_sym_GT_GT = 29,
  anon_sym_EQ_EQ = 30,
  anon_sym_BANG_EQ = 31,
  anon_sym_GT = 32,
  anon_sym_GT_EQ = 33,
  anon_sym_LT_EQ = 34,
  anon_sym_LT = 35,
  anon_sym_PIPE = 36,
  anon_sym_CARET = 37,
  anon_sym_AMP = 38,
  anon_sym_if = 39,
  anon_sym_AMP_GT = 40,
  anon_sym_int = 41,
  anon_sym_bit = 42,
  anon_sym_char = 43,
  anon_sym_mem = 44,
  sym_type_var = 45,
  sym_identifier = 46,
  sym_number_literal = 47,
  anon_sym_SQUOTE = 48,
  aux_sym_char_literal_token1 = 49,
  sym_source_file = 50,
  sym__definition = 51,
  sym_parser_definition = 52,
  sym__type_expression = 53,
  sym_binary_type_expression = 54,
  sym_unary_type_expression = 55,
  sym__expression = 56,
  sym_parser_block = 57,
  sym__parser_block_content = 58,
  sym_parser_sequence = 59,
  sym_parser_choice = 60,
  sym_type_array = 61,
  sym_parser_array = 62,
  sym__statement = 63,
  sym_parse_statement = 64,
  sym_let_statement = 65,
  sym__constraint_expression = 66,
  sym_binary_constraint_expression = 67,
  sym_unary_constraint_expression = 68,
  sym_binary_expression = 69,
  sym_unary_expression = 70,
  sym__type_atom = 71,
  sym_parserdef_ref = 72,
  sym__atom = 73,
  sym__literal = 74,
  sym_primitive_type = 75,
  sym_single = 76,
  sym_char_literal = 77,
  aux_sym_source_file_repeat1 = 78,
  aux_sym_parser_sequence_repeat1 = 79,
  aux_sym_parserdef_ref_repeat1 = 80,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_def] = "def",
  [anon_sym_STAR_GT] = "*>",
  [anon_sym_EQ] = "=",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [anon_sym_TILDE] = "~",
  [anon_sym_LBRACE] = "{",
  [anon_sym_RBRACE] = "}",
  [anon_sym_SEMI] = ";",
  [anon_sym_for] = "for",
  [anon_sym_each] = "each",
  [anon_sym_LBRACK] = "[",
  [anon_sym_RBRACK] = "]",
  [anon_sym_COLON] = ":",
  [anon_sym_COMMA] = ",",
  [anon_sym_let] = "let",
  [anon_sym_or] = "or",
  [anon_sym_and] = "and",
  [anon_sym_BANG] = "!",
  [anon_sym_DOT] = ".",
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
  [anon_sym_if] = "if",
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
  [sym__expression] = "_expression",
  [sym_parser_block] = "parser_block",
  [sym__parser_block_content] = "_parser_block_content",
  [sym_parser_sequence] = "parser_sequence",
  [sym_parser_choice] = "parser_choice",
  [sym_type_array] = "type_array",
  [sym_parser_array] = "parser_array",
  [sym__statement] = "_statement",
  [sym_parse_statement] = "parse_statement",
  [sym_let_statement] = "let_statement",
  [sym__constraint_expression] = "_constraint_expression",
  [sym_binary_constraint_expression] = "binary_constraint_expression",
  [sym_unary_constraint_expression] = "unary_constraint_expression",
  [sym_binary_expression] = "binary_expression",
  [sym_unary_expression] = "unary_expression",
  [sym__type_atom] = "_type_atom",
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
  [anon_sym_TILDE] = anon_sym_TILDE,
  [anon_sym_LBRACE] = anon_sym_LBRACE,
  [anon_sym_RBRACE] = anon_sym_RBRACE,
  [anon_sym_SEMI] = anon_sym_SEMI,
  [anon_sym_for] = anon_sym_for,
  [anon_sym_each] = anon_sym_each,
  [anon_sym_LBRACK] = anon_sym_LBRACK,
  [anon_sym_RBRACK] = anon_sym_RBRACK,
  [anon_sym_COLON] = anon_sym_COLON,
  [anon_sym_COMMA] = anon_sym_COMMA,
  [anon_sym_let] = anon_sym_let,
  [anon_sym_or] = anon_sym_or,
  [anon_sym_and] = anon_sym_and,
  [anon_sym_BANG] = anon_sym_BANG,
  [anon_sym_DOT] = anon_sym_DOT,
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
  [anon_sym_if] = anon_sym_if,
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
  [sym__expression] = sym__expression,
  [sym_parser_block] = sym_parser_block,
  [sym__parser_block_content] = sym__parser_block_content,
  [sym_parser_sequence] = sym_parser_sequence,
  [sym_parser_choice] = sym_parser_choice,
  [sym_type_array] = sym_type_array,
  [sym_parser_array] = sym_parser_array,
  [sym__statement] = sym__statement,
  [sym_parse_statement] = sym_parse_statement,
  [sym_let_statement] = sym_let_statement,
  [sym__constraint_expression] = sym__constraint_expression,
  [sym_binary_constraint_expression] = sym_binary_constraint_expression,
  [sym_unary_constraint_expression] = sym_unary_constraint_expression,
  [sym_binary_expression] = sym_binary_expression,
  [sym_unary_expression] = sym_unary_expression,
  [sym__type_atom] = sym__type_atom,
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
  [anon_sym_TILDE] = {
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
  [anon_sym_DOT] = {
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
  [anon_sym_if] = {
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
  [sym__expression] = {
    .visible = false,
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
    {field_parser, 0},
  [22] =
    {field_content, 1},
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
      if (eof) ADVANCE(41);
      if (lookahead == '!') ADVANCE(65);
      if (lookahead == '%') ADVANCE(73);
      if (lookahead == '&') ADVANCE(85);
      if (lookahead == '\'') ADVANCE(121);
      if (lookahead == '(') ADVANCE(46);
      if (lookahead == ')') ADVANCE(47);
      if (lookahead == '*') ADVANCE(71);
      if (lookahead == '+') ADVANCE(69);
      if (lookahead == ',') ADVANCE(59);
      if (lookahead == '-') ADVANCE(70);
      if (lookahead == '.') ADVANCE(66);
      if (lookahead == '/') ADVANCE(72);
      if (lookahead == '0') ADVANCE(116);
      if (lookahead == ':') ADVANCE(58);
      if (lookahead == ';') ADVANCE(51);
      if (lookahead == '<') ADVANCE(81);
      if (lookahead == '=') ADVANCE(45);
      if (lookahead == '>') ADVANCE(78);
      if (lookahead == '[') ADVANCE(56);
      if (lookahead == ']') ADVANCE(57);
      if (lookahead == '^') ADVANCE(83);
      if (lookahead == 'a') ADVANCE(25);
      if (lookahead == 'b') ADVANCE(22);
      if (lookahead == 'c') ADVANCE(20);
      if (lookahead == 'd') ADVANCE(14);
      if (lookahead == 'e') ADVANCE(10);
      if (lookahead == 'f') ADVANCE(26);
      if (lookahead == 'i') ADVANCE(18);
      if (lookahead == 'l') ADVANCE(17);
      if (lookahead == 'm') ADVANCE(15);
      if (lookahead == 'o') ADVANCE(27);
      if (lookahead == '{') ADVANCE(49);
      if (lookahead == '|') ADVANCE(82);
      if (lookahead == '}') ADVANCE(50);
      if (lookahead == '~') ADVANCE(48);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(119);
      END_STATE();
    case 1:
      if (lookahead == '!') ADVANCE(64);
      if (lookahead == '&') ADVANCE(8);
      if (lookahead == '\'') ADVANCE(121);
      if (lookahead == '(') ADVANCE(46);
      if (lookahead == ')') ADVANCE(47);
      if (lookahead == '*') ADVANCE(9);
      if (lookahead == '+') ADVANCE(69);
      if (lookahead == ',') ADVANCE(59);
      if (lookahead == '-') ADVANCE(70);
      if (lookahead == '0') ADVANCE(116);
      if (lookahead == ';') ADVANCE(51);
      if (lookahead == '=') ADVANCE(44);
      if (lookahead == '[') ADVANCE(56);
      if (lookahead == ']') ADVANCE(57);
      if (lookahead == 'e') ADVANCE(98);
      if (lookahead == 'f') ADVANCE(109);
      if (lookahead == 'i') ADVANCE(103);
      if (lookahead == 'l') ADVANCE(101);
      if (lookahead == '{') ADVANCE(49);
      if (lookahead == '}') ADVANCE(50);
      if (lookahead == '~') ADVANCE(48);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(1)
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(119);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 2:
      if (lookahead == '!') ADVANCE(64);
      if (lookahead == '\'') ADVANCE(121);
      if (lookahead == '(') ADVANCE(46);
      if (lookahead == '+') ADVANCE(69);
      if (lookahead == '-') ADVANCE(70);
      if (lookahead == '0') ADVANCE(116);
      if (lookahead == 'e') ADVANCE(98);
      if (lookahead == 'f') ADVANCE(109);
      if (lookahead == 'i') ADVANCE(103);
      if (lookahead == '{') ADVANCE(49);
      if (lookahead == '~') ADVANCE(48);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(2)
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(119);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 3:
      if (lookahead == '!') ADVANCE(64);
      if (lookahead == '\'') ADVANCE(121);
      if (lookahead == '0') ADVANCE(116);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(3)
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(119);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 4:
      if (lookahead == '&') ADVANCE(8);
      if (lookahead == ')') ADVANCE(47);
      if (lookahead == '*') ADVANCE(9);
      if (lookahead == ',') ADVANCE(59);
      if (lookahead == '=') ADVANCE(44);
      if (lookahead == ']') ADVANCE(57);
      if (lookahead == 'a') ADVANCE(25);
      if (lookahead == 'o') ADVANCE(27);
      if (lookahead == '~') ADVANCE(48);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(4)
      END_STATE();
    case 5:
      if (lookahead == '\'') ADVANCE(37);
      if (lookahead == '(') ADVANCE(46);
      if (lookahead == '*') ADVANCE(9);
      if (lookahead == ']') ADVANCE(57);
      if (lookahead == 'b') ADVANCE(106);
      if (lookahead == 'c') ADVANCE(105);
      if (lookahead == 'e') ADVANCE(98);
      if (lookahead == 'f') ADVANCE(109);
      if (lookahead == 'i') ADVANCE(108);
      if (lookahead == 'm') ADVANCE(102);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(5)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 6:
      if (lookahead == '=') ADVANCE(77);
      END_STATE();
    case 7:
      if (lookahead == '=') ADVANCE(76);
      END_STATE();
    case 8:
      if (lookahead == '>') ADVANCE(88);
      END_STATE();
    case 9:
      if (lookahead == '>') ADVANCE(43);
      END_STATE();
    case 10:
      if (lookahead == 'a') ADVANCE(12);
      if (lookahead == 'l') ADVANCE(30);
      END_STATE();
    case 11:
      if (lookahead == 'a') ADVANCE(29);
      END_STATE();
    case 12:
      if (lookahead == 'c') ADVANCE(21);
      END_STATE();
    case 13:
      if (lookahead == 'd') ADVANCE(63);
      END_STATE();
    case 14:
      if (lookahead == 'e') ADVANCE(19);
      END_STATE();
    case 15:
      if (lookahead == 'e') ADVANCE(24);
      END_STATE();
    case 16:
      if (lookahead == 'e') ADVANCE(67);
      END_STATE();
    case 17:
      if (lookahead == 'e') ADVANCE(33);
      END_STATE();
    case 18:
      if (lookahead == 'f') ADVANCE(86);
      if (lookahead == 'n') ADVANCE(32);
      END_STATE();
    case 19:
      if (lookahead == 'f') ADVANCE(42);
      END_STATE();
    case 20:
      if (lookahead == 'h') ADVANCE(11);
      END_STATE();
    case 21:
      if (lookahead == 'h') ADVANCE(54);
      END_STATE();
    case 22:
      if (lookahead == 'i') ADVANCE(31);
      END_STATE();
    case 23:
      if (lookahead == 'l') ADVANCE(30);
      END_STATE();
    case 24:
      if (lookahead == 'm') ADVANCE(95);
      END_STATE();
    case 25:
      if (lookahead == 'n') ADVANCE(13);
      END_STATE();
    case 26:
      if (lookahead == 'o') ADVANCE(28);
      END_STATE();
    case 27:
      if (lookahead == 'r') ADVANCE(62);
      END_STATE();
    case 28:
      if (lookahead == 'r') ADVANCE(52);
      END_STATE();
    case 29:
      if (lookahead == 'r') ADVANCE(93);
      END_STATE();
    case 30:
      if (lookahead == 's') ADVANCE(16);
      END_STATE();
    case 31:
      if (lookahead == 't') ADVANCE(91);
      END_STATE();
    case 32:
      if (lookahead == 't') ADVANCE(89);
      END_STATE();
    case 33:
      if (lookahead == 't') ADVANCE(60);
      END_STATE();
    case 34:
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(117);
      END_STATE();
    case 35:
      if (('0' <= lookahead && lookahead <= '7')) ADVANCE(118);
      END_STATE();
    case 36:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(120);
      END_STATE();
    case 37:
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 38:
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '\'') ADVANCE(122);
      END_STATE();
    case 39:
      if (eof) ADVANCE(41);
      if (lookahead == '!') ADVANCE(6);
      if (lookahead == '%') ADVANCE(73);
      if (lookahead == '&') ADVANCE(85);
      if (lookahead == ')') ADVANCE(47);
      if (lookahead == '*') ADVANCE(71);
      if (lookahead == '+') ADVANCE(69);
      if (lookahead == ',') ADVANCE(59);
      if (lookahead == '-') ADVANCE(70);
      if (lookahead == '.') ADVANCE(66);
      if (lookahead == '/') ADVANCE(72);
      if (lookahead == '<') ADVANCE(81);
      if (lookahead == '=') ADVANCE(45);
      if (lookahead == '>') ADVANCE(78);
      if (lookahead == ']') ADVANCE(57);
      if (lookahead == '^') ADVANCE(83);
      if (lookahead == 'a') ADVANCE(25);
      if (lookahead == 'd') ADVANCE(14);
      if (lookahead == 'e') ADVANCE(23);
      if (lookahead == 'o') ADVANCE(27);
      if (lookahead == '|') ADVANCE(82);
      if (lookahead == '~') ADVANCE(48);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(39)
      END_STATE();
    case 40:
      if (eof) ADVANCE(41);
      if (lookahead == '!') ADVANCE(6);
      if (lookahead == '%') ADVANCE(73);
      if (lookahead == '&') ADVANCE(84);
      if (lookahead == ')') ADVANCE(47);
      if (lookahead == '*') ADVANCE(71);
      if (lookahead == '+') ADVANCE(69);
      if (lookahead == ',') ADVANCE(59);
      if (lookahead == '-') ADVANCE(70);
      if (lookahead == '.') ADVANCE(66);
      if (lookahead == '/') ADVANCE(72);
      if (lookahead == ':') ADVANCE(58);
      if (lookahead == '<') ADVANCE(81);
      if (lookahead == '=') ADVANCE(7);
      if (lookahead == '>') ADVANCE(78);
      if (lookahead == ']') ADVANCE(57);
      if (lookahead == '^') ADVANCE(83);
      if (lookahead == 'a') ADVANCE(25);
      if (lookahead == 'd') ADVANCE(14);
      if (lookahead == 'e') ADVANCE(23);
      if (lookahead == 'o') ADVANCE(27);
      if (lookahead == '|') ADVANCE(82);
      if (lookahead == '~') ADVANCE(48);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(40)
      END_STATE();
    case 41:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(anon_sym_def);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(anon_sym_STAR_GT);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(anon_sym_EQ);
      if (lookahead == '=') ADVANCE(76);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(anon_sym_TILDE);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(anon_sym_LBRACE);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(anon_sym_RBRACE);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 52:
      ACCEPT_TOKEN(anon_sym_for);
      END_STATE();
    case 53:
      ACCEPT_TOKEN(anon_sym_for);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 54:
      ACCEPT_TOKEN(anon_sym_each);
      END_STATE();
    case 55:
      ACCEPT_TOKEN(anon_sym_each);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 56:
      ACCEPT_TOKEN(anon_sym_LBRACK);
      END_STATE();
    case 57:
      ACCEPT_TOKEN(anon_sym_RBRACK);
      END_STATE();
    case 58:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 59:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 60:
      ACCEPT_TOKEN(anon_sym_let);
      END_STATE();
    case 61:
      ACCEPT_TOKEN(anon_sym_let);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 62:
      ACCEPT_TOKEN(anon_sym_or);
      END_STATE();
    case 63:
      ACCEPT_TOKEN(anon_sym_and);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(anon_sym_BANG);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(anon_sym_BANG);
      if (lookahead == '=') ADVANCE(77);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(anon_sym_else);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(anon_sym_PIPE_GT);
      END_STATE();
    case 69:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(anon_sym_STAR);
      if (lookahead == '>') ADVANCE(43);
      END_STATE();
    case 72:
      ACCEPT_TOKEN(anon_sym_SLASH);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(anon_sym_PERCENT);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(anon_sym_LT_LT);
      END_STATE();
    case 75:
      ACCEPT_TOKEN(anon_sym_GT_GT);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(anon_sym_EQ_EQ);
      END_STATE();
    case 77:
      ACCEPT_TOKEN(anon_sym_BANG_EQ);
      END_STATE();
    case 78:
      ACCEPT_TOKEN(anon_sym_GT);
      if (lookahead == '=') ADVANCE(79);
      if (lookahead == '>') ADVANCE(75);
      END_STATE();
    case 79:
      ACCEPT_TOKEN(anon_sym_GT_EQ);
      END_STATE();
    case 80:
      ACCEPT_TOKEN(anon_sym_LT_EQ);
      END_STATE();
    case 81:
      ACCEPT_TOKEN(anon_sym_LT);
      if (lookahead == '<') ADVANCE(74);
      if (lookahead == '=') ADVANCE(80);
      END_STATE();
    case 82:
      ACCEPT_TOKEN(anon_sym_PIPE);
      if (lookahead == '>') ADVANCE(68);
      END_STATE();
    case 83:
      ACCEPT_TOKEN(anon_sym_CARET);
      END_STATE();
    case 84:
      ACCEPT_TOKEN(anon_sym_AMP);
      END_STATE();
    case 85:
      ACCEPT_TOKEN(anon_sym_AMP);
      if (lookahead == '>') ADVANCE(88);
      END_STATE();
    case 86:
      ACCEPT_TOKEN(anon_sym_if);
      END_STATE();
    case 87:
      ACCEPT_TOKEN(anon_sym_if);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 88:
      ACCEPT_TOKEN(anon_sym_AMP_GT);
      END_STATE();
    case 89:
      ACCEPT_TOKEN(anon_sym_int);
      END_STATE();
    case 90:
      ACCEPT_TOKEN(anon_sym_int);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 91:
      ACCEPT_TOKEN(anon_sym_bit);
      END_STATE();
    case 92:
      ACCEPT_TOKEN(anon_sym_bit);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 93:
      ACCEPT_TOKEN(anon_sym_char);
      END_STATE();
    case 94:
      ACCEPT_TOKEN(anon_sym_char);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 95:
      ACCEPT_TOKEN(anon_sym_mem);
      END_STATE();
    case 96:
      ACCEPT_TOKEN(anon_sym_mem);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 97:
      ACCEPT_TOKEN(sym_type_var);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(97);
      END_STATE();
    case 98:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(100);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 99:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(111);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 100:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'c') ADVANCE(104);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 101:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(112);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 102:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(107);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 103:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'f') ADVANCE(87);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 104:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'h') ADVANCE(55);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 105:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'h') ADVANCE(99);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 106:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'i') ADVANCE(113);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 107:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'm') ADVANCE(96);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 108:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'n') ADVANCE(114);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 109:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(110);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 110:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(53);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 111:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(94);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 112:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(61);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 113:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(92);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 114:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(90);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 115:
      ACCEPT_TOKEN(sym_identifier);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(115);
      END_STATE();
    case 116:
      ACCEPT_TOKEN(sym_number_literal);
      if (lookahead == 'b') ADVANCE(34);
      if (lookahead == 'o') ADVANCE(35);
      if (lookahead == 'x') ADVANCE(36);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(119);
      END_STATE();
    case 117:
      ACCEPT_TOKEN(sym_number_literal);
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(117);
      END_STATE();
    case 118:
      ACCEPT_TOKEN(sym_number_literal);
      if (('0' <= lookahead && lookahead <= '7')) ADVANCE(118);
      END_STATE();
    case 119:
      ACCEPT_TOKEN(sym_number_literal);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(119);
      END_STATE();
    case 120:
      ACCEPT_TOKEN(sym_number_literal);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(120);
      END_STATE();
    case 121:
      ACCEPT_TOKEN(anon_sym_SQUOTE);
      END_STATE();
    case 122:
      ACCEPT_TOKEN(aux_sym_char_literal_token1);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 0},
  [2] = {.lex_state = 39},
  [3] = {.lex_state = 39},
  [4] = {.lex_state = 39},
  [5] = {.lex_state = 39},
  [6] = {.lex_state = 1},
  [7] = {.lex_state = 1},
  [8] = {.lex_state = 1},
  [9] = {.lex_state = 1},
  [10] = {.lex_state = 1},
  [11] = {.lex_state = 40},
  [12] = {.lex_state = 40},
  [13] = {.lex_state = 40},
  [14] = {.lex_state = 40},
  [15] = {.lex_state = 40},
  [16] = {.lex_state = 40},
  [17] = {.lex_state = 40},
  [18] = {.lex_state = 40},
  [19] = {.lex_state = 40},
  [20] = {.lex_state = 40},
  [21] = {.lex_state = 40},
  [22] = {.lex_state = 40},
  [23] = {.lex_state = 40},
  [24] = {.lex_state = 40},
  [25] = {.lex_state = 40},
  [26] = {.lex_state = 40},
  [27] = {.lex_state = 40},
  [28] = {.lex_state = 40},
  [29] = {.lex_state = 40},
  [30] = {.lex_state = 40},
  [31] = {.lex_state = 40},
  [32] = {.lex_state = 40},
  [33] = {.lex_state = 40},
  [34] = {.lex_state = 40},
  [35] = {.lex_state = 40},
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
  [51] = {.lex_state = 2},
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
  [63] = {.lex_state = 5},
  [64] = {.lex_state = 1},
  [65] = {.lex_state = 1},
  [66] = {.lex_state = 1},
  [67] = {.lex_state = 1},
  [68] = {.lex_state = 5},
  [69] = {.lex_state = 3},
  [70] = {.lex_state = 3},
  [71] = {.lex_state = 3},
  [72] = {.lex_state = 3},
  [73] = {.lex_state = 4},
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
  [84] = {.lex_state = 1},
  [85] = {.lex_state = 3},
  [86] = {.lex_state = 0},
  [87] = {.lex_state = 0},
  [88] = {.lex_state = 1},
  [89] = {.lex_state = 1},
  [90] = {.lex_state = 1},
  [91] = {.lex_state = 1},
  [92] = {.lex_state = 1},
  [93] = {.lex_state = 0},
  [94] = {.lex_state = 1},
  [95] = {.lex_state = 0},
  [96] = {.lex_state = 0},
  [97] = {.lex_state = 3},
  [98] = {.lex_state = 0},
  [99] = {.lex_state = 0},
  [100] = {.lex_state = 3},
  [101] = {.lex_state = 38},
  [102] = {.lex_state = 0},
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
    [anon_sym_TILDE] = ACTIONS(1),
    [anon_sym_LBRACE] = ACTIONS(1),
    [anon_sym_RBRACE] = ACTIONS(1),
    [anon_sym_SEMI] = ACTIONS(1),
    [anon_sym_for] = ACTIONS(1),
    [anon_sym_each] = ACTIONS(1),
    [anon_sym_LBRACK] = ACTIONS(1),
    [anon_sym_RBRACK] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
    [anon_sym_let] = ACTIONS(1),
    [anon_sym_or] = ACTIONS(1),
    [anon_sym_and] = ACTIONS(1),
    [anon_sym_BANG] = ACTIONS(1),
    [anon_sym_DOT] = ACTIONS(1),
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
    [anon_sym_if] = ACTIONS(1),
    [anon_sym_AMP_GT] = ACTIONS(1),
    [anon_sym_int] = ACTIONS(1),
    [anon_sym_bit] = ACTIONS(1),
    [anon_sym_char] = ACTIONS(1),
    [anon_sym_mem] = ACTIONS(1),
    [sym_number_literal] = ACTIONS(1),
    [anon_sym_SQUOTE] = ACTIONS(1),
  },
  [1] = {
    [sym_source_file] = STATE(104),
    [sym__definition] = STATE(87),
    [sym_parser_definition] = STATE(87),
    [aux_sym_source_file_repeat1] = STATE(87),
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
    ACTIONS(7), 24,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_or,
      anon_sym_and,
      anon_sym_DOT,
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
      anon_sym_AMP_GT,
  [35] = 2,
    ACTIONS(13), 6,
      anon_sym_EQ,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
      anon_sym_AMP,
    ACTIONS(11), 24,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_or,
      anon_sym_and,
      anon_sym_DOT,
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
      anon_sym_AMP_GT,
  [70] = 3,
    ACTIONS(15), 1,
      anon_sym_and,
    ACTIONS(13), 6,
      anon_sym_EQ,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
      anon_sym_AMP,
    ACTIONS(11), 23,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_or,
      anon_sym_DOT,
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
      anon_sym_AMP_GT,
  [107] = 2,
    ACTIONS(19), 6,
      anon_sym_EQ,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
      anon_sym_AMP,
    ACTIONS(17), 24,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_or,
      anon_sym_and,
      anon_sym_DOT,
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
      anon_sym_AMP_GT,
  [142] = 14,
    ACTIONS(21), 1,
      anon_sym_LPAREN,
    ACTIONS(23), 1,
      anon_sym_TILDE,
    ACTIONS(25), 1,
      anon_sym_LBRACE,
    ACTIONS(27), 1,
      anon_sym_RBRACE,
    ACTIONS(31), 1,
      anon_sym_let,
    ACTIONS(35), 1,
      anon_sym_if,
    ACTIONS(37), 1,
      sym_identifier,
    ACTIONS(39), 1,
      sym_number_literal,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(29), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(33), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(95), 3,
      sym__parser_block_content,
      sym_parser_sequence,
      sym_parser_choice,
    STATE(8), 4,
      sym__statement,
      sym_parse_statement,
      sym_let_statement,
      aux_sym_parser_sequence_repeat1,
    STATE(33), 9,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [201] = 13,
    ACTIONS(21), 1,
      anon_sym_LPAREN,
    ACTIONS(23), 1,
      anon_sym_TILDE,
    ACTIONS(25), 1,
      anon_sym_LBRACE,
    ACTIONS(31), 1,
      anon_sym_let,
    ACTIONS(35), 1,
      anon_sym_if,
    ACTIONS(37), 1,
      sym_identifier,
    ACTIONS(39), 1,
      sym_number_literal,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(29), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(33), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(93), 3,
      sym__parser_block_content,
      sym_parser_sequence,
      sym_parser_choice,
    STATE(8), 4,
      sym__statement,
      sym_parse_statement,
      sym_let_statement,
      aux_sym_parser_sequence_repeat1,
    STATE(33), 9,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [257] = 13,
    ACTIONS(21), 1,
      anon_sym_LPAREN,
    ACTIONS(23), 1,
      anon_sym_TILDE,
    ACTIONS(25), 1,
      anon_sym_LBRACE,
    ACTIONS(31), 1,
      anon_sym_let,
    ACTIONS(35), 1,
      anon_sym_if,
    ACTIONS(37), 1,
      sym_identifier,
    ACTIONS(39), 1,
      sym_number_literal,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(29), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(33), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(43), 3,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_SEMI,
    STATE(9), 4,
      sym__statement,
      sym_parse_statement,
      sym_let_statement,
      aux_sym_parser_sequence_repeat1,
    STATE(33), 9,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [313] = 13,
    ACTIONS(45), 1,
      anon_sym_LPAREN,
    ACTIONS(50), 1,
      anon_sym_TILDE,
    ACTIONS(53), 1,
      anon_sym_LBRACE,
    ACTIONS(59), 1,
      anon_sym_let,
    ACTIONS(65), 1,
      anon_sym_if,
    ACTIONS(68), 1,
      sym_identifier,
    ACTIONS(71), 1,
      sym_number_literal,
    ACTIONS(74), 1,
      anon_sym_SQUOTE,
    ACTIONS(56), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(48), 3,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_SEMI,
    ACTIONS(62), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(9), 4,
      sym__statement,
      sym_parse_statement,
      sym_let_statement,
      aux_sym_parser_sequence_repeat1,
    STATE(33), 9,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [369] = 13,
    ACTIONS(21), 1,
      anon_sym_LPAREN,
    ACTIONS(23), 1,
      anon_sym_TILDE,
    ACTIONS(25), 1,
      anon_sym_LBRACE,
    ACTIONS(31), 1,
      anon_sym_let,
    ACTIONS(35), 1,
      anon_sym_if,
    ACTIONS(37), 1,
      sym_identifier,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(77), 1,
      sym_number_literal,
    ACTIONS(29), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(33), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(96), 3,
      sym__parser_block_content,
      sym_parser_sequence,
      sym_parser_choice,
    STATE(8), 4,
      sym__statement,
      sym_parse_statement,
      sym_let_statement,
      aux_sym_parser_sequence_repeat1,
    STATE(29), 9,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [425] = 4,
    ACTIONS(15), 1,
      anon_sym_and,
    ACTIONS(81), 1,
      anon_sym_or,
    ACTIONS(83), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(79), 22,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_DOT,
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
  [462] = 2,
    ACTIONS(87), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(85), 22,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_DOT,
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
  [493] = 12,
    ACTIONS(91), 1,
      anon_sym_TILDE,
    ACTIONS(93), 1,
      anon_sym_DOT,
    ACTIONS(95), 1,
      anon_sym_else,
    ACTIONS(99), 1,
      anon_sym_STAR,
    ACTIONS(105), 1,
      anon_sym_CARET,
    ACTIONS(107), 1,
      anon_sym_AMP,
    ACTIONS(89), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(97), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(101), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(103), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(83), 3,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(79), 9,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [544] = 13,
    ACTIONS(91), 1,
      anon_sym_TILDE,
    ACTIONS(93), 1,
      anon_sym_DOT,
    ACTIONS(95), 1,
      anon_sym_else,
    ACTIONS(99), 1,
      anon_sym_STAR,
    ACTIONS(105), 1,
      anon_sym_CARET,
    ACTIONS(107), 1,
      anon_sym_AMP,
    ACTIONS(109), 1,
      anon_sym_PIPE,
    ACTIONS(83), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(89), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(97), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(101), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(103), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(79), 9,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [597] = 5,
    ACTIONS(91), 1,
      anon_sym_TILDE,
    ACTIONS(93), 1,
      anon_sym_DOT,
    ACTIONS(95), 1,
      anon_sym_else,
    ACTIONS(113), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(111), 19,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
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
  [634] = 2,
    ACTIONS(117), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(115), 22,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_DOT,
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
  [665] = 9,
    ACTIONS(91), 1,
      anon_sym_TILDE,
    ACTIONS(93), 1,
      anon_sym_DOT,
    ACTIONS(95), 1,
      anon_sym_else,
    ACTIONS(99), 1,
      anon_sym_STAR,
    ACTIONS(89), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(97), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(101), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(83), 3,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(79), 13,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_AMP,
  [710] = 6,
    ACTIONS(91), 1,
      anon_sym_TILDE,
    ACTIONS(93), 1,
      anon_sym_DOT,
    ACTIONS(95), 1,
      anon_sym_else,
    ACTIONS(89), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(83), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(79), 17,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
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
  [749] = 11,
    ACTIONS(91), 1,
      anon_sym_TILDE,
    ACTIONS(93), 1,
      anon_sym_DOT,
    ACTIONS(95), 1,
      anon_sym_else,
    ACTIONS(99), 1,
      anon_sym_STAR,
    ACTIONS(107), 1,
      anon_sym_AMP,
    ACTIONS(89), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(97), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(101), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(103), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(83), 3,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(79), 10,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
  [798] = 8,
    ACTIONS(91), 1,
      anon_sym_TILDE,
    ACTIONS(93), 1,
      anon_sym_DOT,
    ACTIONS(95), 1,
      anon_sym_else,
    ACTIONS(99), 1,
      anon_sym_STAR,
    ACTIONS(89), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(101), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(83), 3,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(79), 15,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
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
  [841] = 3,
    ACTIONS(93), 1,
      anon_sym_DOT,
    ACTIONS(83), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(79), 21,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
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
  [874] = 2,
    ACTIONS(83), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(79), 22,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_DOT,
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
  [905] = 10,
    ACTIONS(91), 1,
      anon_sym_TILDE,
    ACTIONS(93), 1,
      anon_sym_DOT,
    ACTIONS(95), 1,
      anon_sym_else,
    ACTIONS(99), 1,
      anon_sym_STAR,
    ACTIONS(89), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(97), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(101), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(103), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(83), 3,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(79), 11,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_AMP,
  [952] = 2,
    ACTIONS(121), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(119), 22,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_DOT,
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
  [983] = 5,
    ACTIONS(91), 1,
      anon_sym_TILDE,
    ACTIONS(93), 1,
      anon_sym_DOT,
    ACTIONS(95), 1,
      anon_sym_else,
    ACTIONS(83), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(79), 19,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
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
  [1020] = 2,
    ACTIONS(125), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(123), 22,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_DOT,
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
  [1051] = 2,
    ACTIONS(129), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(127), 22,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_DOT,
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
  [1082] = 3,
    ACTIONS(133), 1,
      anon_sym_COLON,
    ACTIONS(135), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(131), 19,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_COMMA,
      anon_sym_DOT,
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
  [1113] = 15,
    ACTIONS(91), 1,
      anon_sym_TILDE,
    ACTIONS(93), 1,
      anon_sym_DOT,
    ACTIONS(95), 1,
      anon_sym_else,
    ACTIONS(99), 1,
      anon_sym_STAR,
    ACTIONS(105), 1,
      anon_sym_CARET,
    ACTIONS(107), 1,
      anon_sym_AMP,
    ACTIONS(109), 1,
      anon_sym_PIPE,
    ACTIONS(137), 1,
      anon_sym_RPAREN,
    ACTIONS(139), 1,
      anon_sym_COMMA,
    ACTIONS(89), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(97), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(101), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(103), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(143), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(141), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1167] = 14,
    ACTIONS(91), 1,
      anon_sym_TILDE,
    ACTIONS(93), 1,
      anon_sym_DOT,
    ACTIONS(95), 1,
      anon_sym_else,
    ACTIONS(99), 1,
      anon_sym_STAR,
    ACTIONS(105), 1,
      anon_sym_CARET,
    ACTIONS(107), 1,
      anon_sym_AMP,
    ACTIONS(109), 1,
      anon_sym_PIPE,
    ACTIONS(89), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(97), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(101), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(103), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(143), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(145), 2,
      ts_builtin_sym_end,
      anon_sym_def,
    ACTIONS(141), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1219] = 14,
    ACTIONS(91), 1,
      anon_sym_TILDE,
    ACTIONS(93), 1,
      anon_sym_DOT,
    ACTIONS(95), 1,
      anon_sym_else,
    ACTIONS(99), 1,
      anon_sym_STAR,
    ACTIONS(105), 1,
      anon_sym_CARET,
    ACTIONS(107), 1,
      anon_sym_AMP,
    ACTIONS(109), 1,
      anon_sym_PIPE,
    ACTIONS(147), 1,
      anon_sym_RBRACK,
    ACTIONS(89), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(97), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(101), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(103), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(143), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(141), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1270] = 14,
    ACTIONS(91), 1,
      anon_sym_TILDE,
    ACTIONS(93), 1,
      anon_sym_DOT,
    ACTIONS(95), 1,
      anon_sym_else,
    ACTIONS(99), 1,
      anon_sym_STAR,
    ACTIONS(105), 1,
      anon_sym_CARET,
    ACTIONS(107), 1,
      anon_sym_AMP,
    ACTIONS(109), 1,
      anon_sym_PIPE,
    ACTIONS(149), 1,
      anon_sym_COMMA,
    ACTIONS(89), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(97), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(101), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(103), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(143), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(141), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1321] = 14,
    ACTIONS(91), 1,
      anon_sym_TILDE,
    ACTIONS(93), 1,
      anon_sym_DOT,
    ACTIONS(95), 1,
      anon_sym_else,
    ACTIONS(99), 1,
      anon_sym_STAR,
    ACTIONS(105), 1,
      anon_sym_CARET,
    ACTIONS(107), 1,
      anon_sym_AMP,
    ACTIONS(109), 1,
      anon_sym_PIPE,
    ACTIONS(139), 1,
      anon_sym_COMMA,
    ACTIONS(89), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(97), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(101), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(103), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(143), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(141), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1372] = 14,
    ACTIONS(91), 1,
      anon_sym_TILDE,
    ACTIONS(93), 1,
      anon_sym_DOT,
    ACTIONS(95), 1,
      anon_sym_else,
    ACTIONS(99), 1,
      anon_sym_STAR,
    ACTIONS(105), 1,
      anon_sym_CARET,
    ACTIONS(107), 1,
      anon_sym_AMP,
    ACTIONS(109), 1,
      anon_sym_PIPE,
    ACTIONS(151), 1,
      anon_sym_COMMA,
    ACTIONS(89), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(97), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(101), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(103), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(143), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(141), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1423] = 14,
    ACTIONS(91), 1,
      anon_sym_TILDE,
    ACTIONS(93), 1,
      anon_sym_DOT,
    ACTIONS(95), 1,
      anon_sym_else,
    ACTIONS(99), 1,
      anon_sym_STAR,
    ACTIONS(105), 1,
      anon_sym_CARET,
    ACTIONS(107), 1,
      anon_sym_AMP,
    ACTIONS(109), 1,
      anon_sym_PIPE,
    ACTIONS(137), 1,
      anon_sym_RPAREN,
    ACTIONS(89), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(97), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(101), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(103), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(143), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(141), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1474] = 10,
    ACTIONS(23), 1,
      anon_sym_TILDE,
    ACTIONS(25), 1,
      anon_sym_LBRACE,
    ACTIONS(35), 1,
      anon_sym_if,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(153), 1,
      anon_sym_LPAREN,
    ACTIONS(155), 1,
      sym_identifier,
    ACTIONS(157), 1,
      sym_number_literal,
    ACTIONS(29), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(33), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(19), 9,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1516] = 10,
    ACTIONS(23), 1,
      anon_sym_TILDE,
    ACTIONS(25), 1,
      anon_sym_LBRACE,
    ACTIONS(35), 1,
      anon_sym_if,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(153), 1,
      anon_sym_LPAREN,
    ACTIONS(159), 1,
      sym_identifier,
    ACTIONS(161), 1,
      sym_number_literal,
    ACTIONS(29), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(33), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(25), 9,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1558] = 10,
    ACTIONS(23), 1,
      anon_sym_TILDE,
    ACTIONS(25), 1,
      anon_sym_LBRACE,
    ACTIONS(35), 1,
      anon_sym_if,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(153), 1,
      anon_sym_LPAREN,
    ACTIONS(163), 1,
      sym_identifier,
    ACTIONS(165), 1,
      sym_number_literal,
    ACTIONS(29), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(33), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(23), 9,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1600] = 10,
    ACTIONS(23), 1,
      anon_sym_TILDE,
    ACTIONS(25), 1,
      anon_sym_LBRACE,
    ACTIONS(35), 1,
      anon_sym_if,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(153), 1,
      anon_sym_LPAREN,
    ACTIONS(167), 1,
      sym_identifier,
    ACTIONS(169), 1,
      sym_number_literal,
    ACTIONS(29), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(33), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(13), 9,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1642] = 10,
    ACTIONS(23), 1,
      anon_sym_TILDE,
    ACTIONS(25), 1,
      anon_sym_LBRACE,
    ACTIONS(35), 1,
      anon_sym_if,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(153), 1,
      anon_sym_LPAREN,
    ACTIONS(171), 1,
      sym_identifier,
    ACTIONS(173), 1,
      sym_number_literal,
    ACTIONS(29), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(33), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(35), 9,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1684] = 10,
    ACTIONS(23), 1,
      anon_sym_TILDE,
    ACTIONS(25), 1,
      anon_sym_LBRACE,
    ACTIONS(35), 1,
      anon_sym_if,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(153), 1,
      anon_sym_LPAREN,
    ACTIONS(175), 1,
      sym_identifier,
    ACTIONS(177), 1,
      sym_number_literal,
    ACTIONS(29), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(33), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(20), 9,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1726] = 10,
    ACTIONS(23), 1,
      anon_sym_TILDE,
    ACTIONS(25), 1,
      anon_sym_LBRACE,
    ACTIONS(35), 1,
      anon_sym_if,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(153), 1,
      anon_sym_LPAREN,
    ACTIONS(179), 1,
      sym_identifier,
    ACTIONS(181), 1,
      sym_number_literal,
    ACTIONS(29), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(33), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(21), 9,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1768] = 10,
    ACTIONS(23), 1,
      anon_sym_TILDE,
    ACTIONS(25), 1,
      anon_sym_LBRACE,
    ACTIONS(35), 1,
      anon_sym_if,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(153), 1,
      anon_sym_LPAREN,
    ACTIONS(183), 1,
      sym_identifier,
    ACTIONS(185), 1,
      sym_number_literal,
    ACTIONS(29), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(33), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(14), 9,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1810] = 10,
    ACTIONS(23), 1,
      anon_sym_TILDE,
    ACTIONS(25), 1,
      anon_sym_LBRACE,
    ACTIONS(35), 1,
      anon_sym_if,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(153), 1,
      anon_sym_LPAREN,
    ACTIONS(187), 1,
      sym_identifier,
    ACTIONS(189), 1,
      sym_number_literal,
    ACTIONS(29), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(33), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(15), 9,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1852] = 10,
    ACTIONS(23), 1,
      anon_sym_TILDE,
    ACTIONS(25), 1,
      anon_sym_LBRACE,
    ACTIONS(35), 1,
      anon_sym_if,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(153), 1,
      anon_sym_LPAREN,
    ACTIONS(191), 1,
      sym_identifier,
    ACTIONS(193), 1,
      sym_number_literal,
    ACTIONS(29), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(33), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(34), 9,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1894] = 10,
    ACTIONS(23), 1,
      anon_sym_TILDE,
    ACTIONS(25), 1,
      anon_sym_LBRACE,
    ACTIONS(35), 1,
      anon_sym_if,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(153), 1,
      anon_sym_LPAREN,
    ACTIONS(195), 1,
      sym_identifier,
    ACTIONS(197), 1,
      sym_number_literal,
    ACTIONS(29), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(33), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(22), 9,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1936] = 10,
    ACTIONS(23), 1,
      anon_sym_TILDE,
    ACTIONS(25), 1,
      anon_sym_LBRACE,
    ACTIONS(35), 1,
      anon_sym_if,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(153), 1,
      anon_sym_LPAREN,
    ACTIONS(199), 1,
      sym_identifier,
    ACTIONS(201), 1,
      sym_number_literal,
    ACTIONS(29), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(33), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(17), 9,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [1978] = 10,
    ACTIONS(23), 1,
      anon_sym_TILDE,
    ACTIONS(25), 1,
      anon_sym_LBRACE,
    ACTIONS(35), 1,
      anon_sym_if,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(153), 1,
      anon_sym_LPAREN,
    ACTIONS(203), 1,
      sym_identifier,
    ACTIONS(205), 1,
      sym_number_literal,
    ACTIONS(29), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(33), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(30), 9,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [2020] = 10,
    ACTIONS(23), 1,
      anon_sym_TILDE,
    ACTIONS(25), 1,
      anon_sym_LBRACE,
    ACTIONS(35), 1,
      anon_sym_if,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(153), 1,
      anon_sym_LPAREN,
    ACTIONS(207), 1,
      sym_identifier,
    ACTIONS(209), 1,
      sym_number_literal,
    ACTIONS(29), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(33), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(32), 9,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [2062] = 10,
    ACTIONS(23), 1,
      anon_sym_TILDE,
    ACTIONS(25), 1,
      anon_sym_LBRACE,
    ACTIONS(35), 1,
      anon_sym_if,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(153), 1,
      anon_sym_LPAREN,
    ACTIONS(211), 1,
      sym_identifier,
    ACTIONS(213), 1,
      sym_number_literal,
    ACTIONS(29), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(33), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(31), 9,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [2104] = 10,
    ACTIONS(23), 1,
      anon_sym_TILDE,
    ACTIONS(25), 1,
      anon_sym_LBRACE,
    ACTIONS(35), 1,
      anon_sym_if,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(153), 1,
      anon_sym_LPAREN,
    ACTIONS(215), 1,
      sym_identifier,
    ACTIONS(217), 1,
      sym_number_literal,
    ACTIONS(29), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(33), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(18), 9,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_single,
      sym_char_literal,
  [2146] = 9,
    ACTIONS(219), 1,
      anon_sym_STAR_GT,
    ACTIONS(221), 1,
      anon_sym_LPAREN,
    ACTIONS(225), 1,
      anon_sym_RBRACK,
    ACTIONS(229), 1,
      sym_type_var,
    ACTIONS(231), 1,
      sym_identifier,
    STATE(55), 1,
      aux_sym_parserdef_ref_repeat1,
    ACTIONS(223), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(227), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(90), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2184] = 9,
    ACTIONS(219), 1,
      anon_sym_STAR_GT,
    ACTIONS(221), 1,
      anon_sym_LPAREN,
    ACTIONS(229), 1,
      sym_type_var,
    ACTIONS(231), 1,
      sym_identifier,
    ACTIONS(233), 1,
      anon_sym_RBRACK,
    STATE(54), 1,
      aux_sym_parserdef_ref_repeat1,
    ACTIONS(223), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(227), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(90), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2222] = 9,
    ACTIONS(219), 1,
      anon_sym_STAR_GT,
    ACTIONS(221), 1,
      anon_sym_LPAREN,
    ACTIONS(229), 1,
      sym_type_var,
    ACTIONS(231), 1,
      sym_identifier,
    ACTIONS(235), 1,
      anon_sym_RBRACK,
    STATE(56), 1,
      aux_sym_parserdef_ref_repeat1,
    ACTIONS(223), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(227), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(90), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2260] = 9,
    ACTIONS(219), 1,
      anon_sym_STAR_GT,
    ACTIONS(221), 1,
      anon_sym_LPAREN,
    ACTIONS(229), 1,
      sym_type_var,
    ACTIONS(231), 1,
      sym_identifier,
    ACTIONS(237), 1,
      anon_sym_RBRACK,
    STATE(56), 1,
      aux_sym_parserdef_ref_repeat1,
    ACTIONS(223), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(227), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(90), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2298] = 9,
    ACTIONS(239), 1,
      anon_sym_STAR_GT,
    ACTIONS(242), 1,
      anon_sym_LPAREN,
    ACTIONS(248), 1,
      anon_sym_RBRACK,
    ACTIONS(253), 1,
      sym_type_var,
    ACTIONS(256), 1,
      sym_identifier,
    STATE(56), 1,
      aux_sym_parserdef_ref_repeat1,
    ACTIONS(245), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(250), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(90), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2336] = 7,
    ACTIONS(219), 1,
      anon_sym_STAR_GT,
    ACTIONS(221), 1,
      anon_sym_LPAREN,
    ACTIONS(231), 1,
      sym_identifier,
    ACTIONS(259), 1,
      sym_type_var,
    ACTIONS(223), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(227), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(89), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2368] = 7,
    ACTIONS(219), 1,
      anon_sym_STAR_GT,
    ACTIONS(221), 1,
      anon_sym_LPAREN,
    ACTIONS(231), 1,
      sym_identifier,
    ACTIONS(261), 1,
      sym_type_var,
    ACTIONS(223), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(227), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(84), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2400] = 7,
    ACTIONS(219), 1,
      anon_sym_STAR_GT,
    ACTIONS(221), 1,
      anon_sym_LPAREN,
    ACTIONS(231), 1,
      sym_identifier,
    ACTIONS(263), 1,
      sym_type_var,
    ACTIONS(223), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(227), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(94), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2432] = 7,
    ACTIONS(219), 1,
      anon_sym_STAR_GT,
    ACTIONS(221), 1,
      anon_sym_LPAREN,
    ACTIONS(231), 1,
      sym_identifier,
    ACTIONS(265), 1,
      sym_type_var,
    ACTIONS(223), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(227), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(92), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2464] = 7,
    ACTIONS(219), 1,
      anon_sym_STAR_GT,
    ACTIONS(221), 1,
      anon_sym_LPAREN,
    ACTIONS(231), 1,
      sym_identifier,
    ACTIONS(267), 1,
      sym_type_var,
    ACTIONS(223), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(227), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(80), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2496] = 7,
    ACTIONS(219), 1,
      anon_sym_STAR_GT,
    ACTIONS(221), 1,
      anon_sym_LPAREN,
    ACTIONS(231), 1,
      sym_identifier,
    ACTIONS(269), 1,
      sym_type_var,
    ACTIONS(223), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(227), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(91), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2528] = 7,
    ACTIONS(219), 1,
      anon_sym_STAR_GT,
    ACTIONS(221), 1,
      anon_sym_LPAREN,
    ACTIONS(267), 1,
      sym_type_var,
    ACTIONS(271), 1,
      sym_identifier,
    ACTIONS(223), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(227), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(80), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2560] = 2,
    ACTIONS(275), 5,
      anon_sym_for,
      anon_sym_each,
      anon_sym_let,
      anon_sym_if,
      sym_identifier,
    ACTIONS(273), 11,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_SEMI,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_number_literal,
      anon_sym_SQUOTE,
  [2581] = 2,
    ACTIONS(279), 5,
      anon_sym_for,
      anon_sym_each,
      anon_sym_let,
      anon_sym_if,
      sym_identifier,
    ACTIONS(277), 11,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_SEMI,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_number_literal,
      anon_sym_SQUOTE,
  [2602] = 2,
    ACTIONS(283), 5,
      anon_sym_for,
      anon_sym_each,
      anon_sym_let,
      anon_sym_if,
      sym_identifier,
    ACTIONS(281), 11,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_SEMI,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_number_literal,
      anon_sym_SQUOTE,
  [2623] = 2,
    ACTIONS(287), 5,
      anon_sym_for,
      anon_sym_each,
      anon_sym_let,
      anon_sym_if,
      sym_identifier,
    ACTIONS(285), 11,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_SEMI,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_number_literal,
      anon_sym_SQUOTE,
  [2644] = 2,
    ACTIONS(289), 4,
      anon_sym_STAR_GT,
      anon_sym_LPAREN,
      anon_sym_RBRACK,
      sym_type_var,
    ACTIONS(291), 7,
      anon_sym_for,
      anon_sym_each,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
      sym_identifier,
  [2660] = 4,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(293), 1,
      anon_sym_BANG,
    ACTIONS(295), 2,
      sym_identifier,
      sym_number_literal,
    STATE(11), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2679] = 4,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(293), 1,
      anon_sym_BANG,
    ACTIONS(297), 2,
      sym_identifier,
      sym_number_literal,
    STATE(3), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2698] = 4,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(293), 1,
      anon_sym_BANG,
    ACTIONS(299), 2,
      sym_identifier,
      sym_number_literal,
    STATE(4), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2717] = 4,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(293), 1,
      anon_sym_BANG,
    ACTIONS(301), 2,
      sym_identifier,
      sym_number_literal,
    STATE(73), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2736] = 3,
    ACTIONS(15), 1,
      anon_sym_and,
    ACTIONS(81), 1,
      anon_sym_or,
    ACTIONS(303), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2752] = 2,
    ACTIONS(307), 1,
      anon_sym_LBRACK,
    ACTIONS(305), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2765] = 2,
    ACTIONS(311), 1,
      anon_sym_LBRACK,
    ACTIONS(309), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2778] = 1,
    ACTIONS(313), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2788] = 1,
    ACTIONS(315), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2798] = 1,
    ACTIONS(317), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2808] = 1,
    ACTIONS(319), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2818] = 2,
    ACTIONS(321), 1,
      anon_sym_TILDE,
    ACTIONS(303), 6,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2830] = 1,
    ACTIONS(323), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2840] = 1,
    ACTIONS(325), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2850] = 1,
    ACTIONS(327), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2860] = 2,
    ACTIONS(321), 1,
      anon_sym_TILDE,
    ACTIONS(329), 6,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2872] = 3,
    ACTIONS(41), 1,
      anon_sym_SQUOTE,
    ACTIONS(331), 2,
      sym_identifier,
      sym_number_literal,
    STATE(5), 3,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2885] = 3,
    ACTIONS(333), 1,
      ts_builtin_sym_end,
    ACTIONS(335), 1,
      anon_sym_def,
    STATE(86), 3,
      sym__definition,
      sym_parser_definition,
      aux_sym_source_file_repeat1,
  [2897] = 3,
    ACTIONS(5), 1,
      anon_sym_def,
    ACTIONS(338), 1,
      ts_builtin_sym_end,
    STATE(86), 3,
      sym__definition,
      sym_parser_definition,
      aux_sym_source_file_repeat1,
  [2909] = 3,
    ACTIONS(307), 1,
      anon_sym_LBRACK,
    ACTIONS(340), 1,
      anon_sym_EQ,
    ACTIONS(305), 3,
      anon_sym_STAR_GT,
      anon_sym_TILDE,
      anon_sym_AMP_GT,
  [2921] = 4,
    ACTIONS(321), 1,
      anon_sym_TILDE,
    ACTIONS(342), 1,
      anon_sym_STAR_GT,
    ACTIONS(344), 1,
      anon_sym_EQ,
    ACTIONS(346), 1,
      anon_sym_AMP_GT,
  [2934] = 4,
    ACTIONS(321), 1,
      anon_sym_TILDE,
    ACTIONS(342), 1,
      anon_sym_STAR_GT,
    ACTIONS(346), 1,
      anon_sym_AMP_GT,
    ACTIONS(348), 1,
      anon_sym_COMMA,
  [2947] = 4,
    ACTIONS(321), 1,
      anon_sym_TILDE,
    ACTIONS(342), 1,
      anon_sym_STAR_GT,
    ACTIONS(346), 1,
      anon_sym_AMP_GT,
    ACTIONS(350), 1,
      anon_sym_RBRACK,
  [2960] = 4,
    ACTIONS(321), 1,
      anon_sym_TILDE,
    ACTIONS(342), 1,
      anon_sym_STAR_GT,
    ACTIONS(346), 1,
      anon_sym_AMP_GT,
    ACTIONS(352), 1,
      anon_sym_RPAREN,
  [2973] = 1,
    ACTIONS(354), 3,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_SEMI,
  [2979] = 3,
    ACTIONS(321), 1,
      anon_sym_TILDE,
    ACTIONS(346), 1,
      anon_sym_AMP_GT,
    ACTIONS(356), 1,
      anon_sym_STAR_GT,
  [2989] = 2,
    ACTIONS(358), 1,
      anon_sym_RBRACE,
    ACTIONS(360), 1,
      anon_sym_SEMI,
  [2996] = 2,
    ACTIONS(360), 1,
      anon_sym_SEMI,
    ACTIONS(362), 1,
      anon_sym_RPAREN,
  [3003] = 1,
    ACTIONS(364), 1,
      sym_identifier,
  [3007] = 1,
    ACTIONS(366), 1,
      anon_sym_LBRACK,
  [3011] = 1,
    ACTIONS(368), 1,
      anon_sym_SQUOTE,
  [3015] = 1,
    ACTIONS(370), 1,
      sym_identifier,
  [3019] = 1,
    ACTIONS(372), 1,
      aux_sym_char_literal_token1,
  [3023] = 1,
    ACTIONS(374), 1,
      anon_sym_COLON,
  [3027] = 1,
    ACTIONS(376), 1,
      anon_sym_LBRACK,
  [3031] = 1,
    ACTIONS(378), 1,
      ts_builtin_sym_end,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 35,
  [SMALL_STATE(4)] = 70,
  [SMALL_STATE(5)] = 107,
  [SMALL_STATE(6)] = 142,
  [SMALL_STATE(7)] = 201,
  [SMALL_STATE(8)] = 257,
  [SMALL_STATE(9)] = 313,
  [SMALL_STATE(10)] = 369,
  [SMALL_STATE(11)] = 425,
  [SMALL_STATE(12)] = 462,
  [SMALL_STATE(13)] = 493,
  [SMALL_STATE(14)] = 544,
  [SMALL_STATE(15)] = 597,
  [SMALL_STATE(16)] = 634,
  [SMALL_STATE(17)] = 665,
  [SMALL_STATE(18)] = 710,
  [SMALL_STATE(19)] = 749,
  [SMALL_STATE(20)] = 798,
  [SMALL_STATE(21)] = 841,
  [SMALL_STATE(22)] = 874,
  [SMALL_STATE(23)] = 905,
  [SMALL_STATE(24)] = 952,
  [SMALL_STATE(25)] = 983,
  [SMALL_STATE(26)] = 1020,
  [SMALL_STATE(27)] = 1051,
  [SMALL_STATE(28)] = 1082,
  [SMALL_STATE(29)] = 1113,
  [SMALL_STATE(30)] = 1167,
  [SMALL_STATE(31)] = 1219,
  [SMALL_STATE(32)] = 1270,
  [SMALL_STATE(33)] = 1321,
  [SMALL_STATE(34)] = 1372,
  [SMALL_STATE(35)] = 1423,
  [SMALL_STATE(36)] = 1474,
  [SMALL_STATE(37)] = 1516,
  [SMALL_STATE(38)] = 1558,
  [SMALL_STATE(39)] = 1600,
  [SMALL_STATE(40)] = 1642,
  [SMALL_STATE(41)] = 1684,
  [SMALL_STATE(42)] = 1726,
  [SMALL_STATE(43)] = 1768,
  [SMALL_STATE(44)] = 1810,
  [SMALL_STATE(45)] = 1852,
  [SMALL_STATE(46)] = 1894,
  [SMALL_STATE(47)] = 1936,
  [SMALL_STATE(48)] = 1978,
  [SMALL_STATE(49)] = 2020,
  [SMALL_STATE(50)] = 2062,
  [SMALL_STATE(51)] = 2104,
  [SMALL_STATE(52)] = 2146,
  [SMALL_STATE(53)] = 2184,
  [SMALL_STATE(54)] = 2222,
  [SMALL_STATE(55)] = 2260,
  [SMALL_STATE(56)] = 2298,
  [SMALL_STATE(57)] = 2336,
  [SMALL_STATE(58)] = 2368,
  [SMALL_STATE(59)] = 2400,
  [SMALL_STATE(60)] = 2432,
  [SMALL_STATE(61)] = 2464,
  [SMALL_STATE(62)] = 2496,
  [SMALL_STATE(63)] = 2528,
  [SMALL_STATE(64)] = 2560,
  [SMALL_STATE(65)] = 2581,
  [SMALL_STATE(66)] = 2602,
  [SMALL_STATE(67)] = 2623,
  [SMALL_STATE(68)] = 2644,
  [SMALL_STATE(69)] = 2660,
  [SMALL_STATE(70)] = 2679,
  [SMALL_STATE(71)] = 2698,
  [SMALL_STATE(72)] = 2717,
  [SMALL_STATE(73)] = 2736,
  [SMALL_STATE(74)] = 2752,
  [SMALL_STATE(75)] = 2765,
  [SMALL_STATE(76)] = 2778,
  [SMALL_STATE(77)] = 2788,
  [SMALL_STATE(78)] = 2798,
  [SMALL_STATE(79)] = 2808,
  [SMALL_STATE(80)] = 2818,
  [SMALL_STATE(81)] = 2830,
  [SMALL_STATE(82)] = 2840,
  [SMALL_STATE(83)] = 2850,
  [SMALL_STATE(84)] = 2860,
  [SMALL_STATE(85)] = 2872,
  [SMALL_STATE(86)] = 2885,
  [SMALL_STATE(87)] = 2897,
  [SMALL_STATE(88)] = 2909,
  [SMALL_STATE(89)] = 2921,
  [SMALL_STATE(90)] = 2934,
  [SMALL_STATE(91)] = 2947,
  [SMALL_STATE(92)] = 2960,
  [SMALL_STATE(93)] = 2973,
  [SMALL_STATE(94)] = 2979,
  [SMALL_STATE(95)] = 2989,
  [SMALL_STATE(96)] = 2996,
  [SMALL_STATE(97)] = 3003,
  [SMALL_STATE(98)] = 3007,
  [SMALL_STATE(99)] = 3011,
  [SMALL_STATE(100)] = 3015,
  [SMALL_STATE(101)] = 3019,
  [SMALL_STATE(102)] = 3023,
  [SMALL_STATE(103)] = 3027,
  [SMALL_STATE(104)] = 3031,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(59),
  [7] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_char_literal, 3),
  [9] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_char_literal, 3),
  [11] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_constraint_expression, 3, .production_id = 3),
  [13] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_binary_constraint_expression, 3, .production_id = 3),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(70),
  [17] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_constraint_expression, 2),
  [19] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unary_constraint_expression, 2),
  [21] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [23] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [25] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [29] = {.entry = {.count = 1, .reusable = false}}, SHIFT(98),
  [31] = {.entry = {.count = 1, .reusable = false}}, SHIFT(97),
  [33] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
  [35] = {.entry = {.count = 1, .reusable = false}}, SHIFT(44),
  [37] = {.entry = {.count = 1, .reusable = false}}, SHIFT(28),
  [39] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [41] = {.entry = {.count = 1, .reusable = true}}, SHIFT(101),
  [43] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_sequence, 1),
  [45] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(10),
  [48] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2),
  [50] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(16),
  [53] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(6),
  [56] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(98),
  [59] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(97),
  [62] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(44),
  [65] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(44),
  [68] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(28),
  [71] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(33),
  [74] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(101),
  [77] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [79] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_expression, 3, .production_id = 3),
  [81] = {.entry = {.count = 1, .reusable = true}}, SHIFT(71),
  [83] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_binary_expression, 3, .production_id = 3),
  [85] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_array, 4, .production_id = 5),
  [87] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parser_array, 4, .production_id = 5),
  [89] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
  [91] = {.entry = {.count = 1, .reusable = true}}, SHIFT(69),
  [93] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [95] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
  [97] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [99] = {.entry = {.count = 1, .reusable = false}}, SHIFT(51),
  [101] = {.entry = {.count = 1, .reusable = true}}, SHIFT(51),
  [103] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [105] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
  [107] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [109] = {.entry = {.count = 1, .reusable = false}}, SHIFT(39),
  [111] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_expression, 2, .production_id = 2),
  [113] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unary_expression, 2, .production_id = 2),
  [115] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_single, 1),
  [117] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_single, 1),
  [119] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__expression, 3),
  [121] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__expression, 3),
  [123] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_block, 3, .production_id = 12),
  [125] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parser_block, 3, .production_id = 12),
  [127] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_block, 2),
  [129] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parser_block, 2),
  [131] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__atom, 1),
  [133] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [135] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__atom, 1),
  [137] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [139] = {.entry = {.count = 1, .reusable = true}}, SHIFT(64),
  [141] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [143] = {.entry = {.count = 1, .reusable = false}}, SHIFT(43),
  [145] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_definition, 6, .production_id = 9),
  [147] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [149] = {.entry = {.count = 1, .reusable = true}}, SHIFT(65),
  [151] = {.entry = {.count = 1, .reusable = true}}, SHIFT(66),
  [153] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [155] = {.entry = {.count = 1, .reusable = false}}, SHIFT(19),
  [157] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [159] = {.entry = {.count = 1, .reusable = false}}, SHIFT(25),
  [161] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [163] = {.entry = {.count = 1, .reusable = false}}, SHIFT(23),
  [165] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [167] = {.entry = {.count = 1, .reusable = false}}, SHIFT(13),
  [169] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [171] = {.entry = {.count = 1, .reusable = false}}, SHIFT(35),
  [173] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [175] = {.entry = {.count = 1, .reusable = false}}, SHIFT(20),
  [177] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [179] = {.entry = {.count = 1, .reusable = false}}, SHIFT(21),
  [181] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [183] = {.entry = {.count = 1, .reusable = false}}, SHIFT(14),
  [185] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [187] = {.entry = {.count = 1, .reusable = false}}, SHIFT(15),
  [189] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [191] = {.entry = {.count = 1, .reusable = false}}, SHIFT(34),
  [193] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [195] = {.entry = {.count = 1, .reusable = false}}, SHIFT(22),
  [197] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [199] = {.entry = {.count = 1, .reusable = false}}, SHIFT(17),
  [201] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [203] = {.entry = {.count = 1, .reusable = false}}, SHIFT(30),
  [205] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [207] = {.entry = {.count = 1, .reusable = false}}, SHIFT(32),
  [209] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [211] = {.entry = {.count = 1, .reusable = false}}, SHIFT(31),
  [213] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [215] = {.entry = {.count = 1, .reusable = false}}, SHIFT(18),
  [217] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [219] = {.entry = {.count = 1, .reusable = true}}, SHIFT(58),
  [221] = {.entry = {.count = 1, .reusable = true}}, SHIFT(60),
  [223] = {.entry = {.count = 1, .reusable = false}}, SHIFT(103),
  [225] = {.entry = {.count = 1, .reusable = true}}, SHIFT(77),
  [227] = {.entry = {.count = 1, .reusable = false}}, SHIFT(79),
  [229] = {.entry = {.count = 1, .reusable = true}}, SHIFT(90),
  [231] = {.entry = {.count = 1, .reusable = false}}, SHIFT(74),
  [233] = {.entry = {.count = 1, .reusable = true}}, SHIFT(81),
  [235] = {.entry = {.count = 1, .reusable = true}}, SHIFT(76),
  [237] = {.entry = {.count = 1, .reusable = true}}, SHIFT(83),
  [239] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(58),
  [242] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(60),
  [245] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(103),
  [248] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8),
  [250] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(79),
  [253] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(90),
  [256] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(74),
  [259] = {.entry = {.count = 1, .reusable = true}}, SHIFT(89),
  [261] = {.entry = {.count = 1, .reusable = true}}, SHIFT(84),
  [263] = {.entry = {.count = 1, .reusable = true}}, SHIFT(94),
  [265] = {.entry = {.count = 1, .reusable = true}}, SHIFT(92),
  [267] = {.entry = {.count = 1, .reusable = true}}, SHIFT(80),
  [269] = {.entry = {.count = 1, .reusable = true}}, SHIFT(91),
  [271] = {.entry = {.count = 1, .reusable = false}}, SHIFT(88),
  [273] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parse_statement, 2, .production_id = 11),
  [275] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parse_statement, 2, .production_id = 11),
  [277] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_let_statement, 7, .production_id = 15),
  [279] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_let_statement, 7, .production_id = 15),
  [281] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parse_statement, 4, .production_id = 14),
  [283] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parse_statement, 4, .production_id = 14),
  [285] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 3),
  [287] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 3),
  [289] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 6),
  [291] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 6),
  [293] = {.entry = {.count = 1, .reusable = true}}, SHIFT(85),
  [295] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [297] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [299] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [301] = {.entry = {.count = 1, .reusable = true}}, SHIFT(73),
  [303] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_type_expression, 3, .production_id = 3),
  [305] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 1, .production_id = 1),
  [307] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [309] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 3, .production_id = 4),
  [311] = {.entry = {.count = 1, .reusable = true}}, SHIFT(52),
  [313] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 4, .production_id = 7),
  [315] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 5, .production_id = 4),
  [317] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type_array, 4, .production_id = 5),
  [319] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_primitive_type, 1),
  [321] = {.entry = {.count = 1, .reusable = true}}, SHIFT(72),
  [323] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 3, .production_id = 1),
  [325] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__type_expression, 3),
  [327] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 6, .production_id = 10),
  [329] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_type_expression, 2, .production_id = 2),
  [331] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [333] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [335] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(59),
  [338] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [340] = {.entry = {.count = 1, .reusable = true}}, SHIFT(48),
  [342] = {.entry = {.count = 1, .reusable = true}}, SHIFT(61),
  [344] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [346] = {.entry = {.count = 1, .reusable = true}}, SHIFT(100),
  [348] = {.entry = {.count = 1, .reusable = true}}, SHIFT(68),
  [350] = {.entry = {.count = 1, .reusable = true}}, SHIFT(78),
  [352] = {.entry = {.count = 1, .reusable = true}}, SHIFT(82),
  [354] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_choice, 3, .production_id = 13),
  [356] = {.entry = {.count = 1, .reusable = true}}, SHIFT(63),
  [358] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [360] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [362] = {.entry = {.count = 1, .reusable = true}}, SHIFT(67),
  [364] = {.entry = {.count = 1, .reusable = true}}, SHIFT(102),
  [366] = {.entry = {.count = 1, .reusable = true}}, SHIFT(50),
  [368] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [370] = {.entry = {.count = 1, .reusable = true}}, SHIFT(75),
  [372] = {.entry = {.count = 1, .reusable = true}}, SHIFT(99),
  [374] = {.entry = {.count = 1, .reusable = true}}, SHIFT(57),
  [376] = {.entry = {.count = 1, .reusable = true}}, SHIFT(62),
  [378] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
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
