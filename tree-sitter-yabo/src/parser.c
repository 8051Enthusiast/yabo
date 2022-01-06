#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 13
#define STATE_COUNT 91
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 78
#define ALIAS_COUNT 0
#define TOKEN_COUNT 50
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 11
#define MAX_ALIAS_SEQUENCE_LENGTH 7
#define PRODUCTION_ID_COUNT 10

enum {
  anon_sym_def = 1,
  anon_sym_STAR_GT = 2,
  anon_sym_COLON = 3,
  anon_sym_LPAREN = 4,
  anon_sym_RPAREN = 5,
  anon_sym_AMP_GT = 6,
  anon_sym_TILDE = 7,
  anon_sym_AMP = 8,
  anon_sym_LBRACE = 9,
  anon_sym_RBRACE = 10,
  anon_sym_PIPE = 11,
  anon_sym_for = 12,
  anon_sym_each = 13,
  anon_sym_LBRACK = 14,
  anon_sym_RBRACK = 15,
  anon_sym_COMMA = 16,
  anon_sym_let = 17,
  anon_sym_EQ = 18,
  anon_sym_or = 19,
  anon_sym_and = 20,
  anon_sym_BANG = 21,
  anon_sym_DOT = 22,
  anon_sym_else = 23,
  anon_sym_PIPE_GT = 24,
  anon_sym_PLUS = 25,
  anon_sym_DASH = 26,
  anon_sym_STAR = 27,
  anon_sym_SLASH = 28,
  anon_sym_PERCENT = 29,
  anon_sym_LT_LT = 30,
  anon_sym_GT_GT = 31,
  anon_sym_EQ_EQ = 32,
  anon_sym_BANG_EQ = 33,
  anon_sym_GT = 34,
  anon_sym_GT_EQ = 35,
  anon_sym_LT_EQ = 36,
  anon_sym_LT = 37,
  anon_sym_CARET = 38,
  anon_sym_if = 39,
  anon_sym_int = 40,
  anon_sym_bit = 41,
  anon_sym_char = 42,
  anon_sym_mem = 43,
  sym_substidentifier = 44,
  sym_identifier = 45,
  sym_number_literal = 46,
  anon_sym_SQUOTE = 47,
  aux_sym_char_literal_token1 = 48,
  anon_sym_DQUOTE = 49,
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
  sym__atom = 72,
  sym__literal = 73,
  sym_primitive_type = 74,
  sym_char_literal = 75,
  aux_sym_source_file_repeat1 = 76,
  aux_sym_parser_sequence_repeat1 = 77,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_def] = "def",
  [anon_sym_STAR_GT] = "*>",
  [anon_sym_COLON] = ":",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [anon_sym_AMP_GT] = "&>",
  [anon_sym_TILDE] = "~",
  [anon_sym_AMP] = "&",
  [anon_sym_LBRACE] = "{",
  [anon_sym_RBRACE] = "}",
  [anon_sym_PIPE] = "|",
  [anon_sym_for] = "for",
  [anon_sym_each] = "each",
  [anon_sym_LBRACK] = "[",
  [anon_sym_RBRACK] = "]",
  [anon_sym_COMMA] = ",",
  [anon_sym_let] = "let",
  [anon_sym_EQ] = "=",
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
  [anon_sym_CARET] = "^",
  [anon_sym_if] = "if",
  [anon_sym_int] = "int",
  [anon_sym_bit] = "bit",
  [anon_sym_char] = "char",
  [anon_sym_mem] = "mem",
  [sym_substidentifier] = "substidentifier",
  [sym_identifier] = "identifier",
  [sym_number_literal] = "number_literal",
  [anon_sym_SQUOTE] = "'",
  [aux_sym_char_literal_token1] = "char_literal_token1",
  [anon_sym_DQUOTE] = "\"",
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
  [sym__atom] = "_atom",
  [sym__literal] = "_literal",
  [sym_primitive_type] = "primitive_type",
  [sym_char_literal] = "char_literal",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
  [aux_sym_parser_sequence_repeat1] = "parser_sequence_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_def] = anon_sym_def,
  [anon_sym_STAR_GT] = anon_sym_STAR_GT,
  [anon_sym_COLON] = anon_sym_COLON,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_AMP_GT] = anon_sym_AMP_GT,
  [anon_sym_TILDE] = anon_sym_TILDE,
  [anon_sym_AMP] = anon_sym_AMP,
  [anon_sym_LBRACE] = anon_sym_LBRACE,
  [anon_sym_RBRACE] = anon_sym_RBRACE,
  [anon_sym_PIPE] = anon_sym_PIPE,
  [anon_sym_for] = anon_sym_for,
  [anon_sym_each] = anon_sym_each,
  [anon_sym_LBRACK] = anon_sym_LBRACK,
  [anon_sym_RBRACK] = anon_sym_RBRACK,
  [anon_sym_COMMA] = anon_sym_COMMA,
  [anon_sym_let] = anon_sym_let,
  [anon_sym_EQ] = anon_sym_EQ,
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
  [anon_sym_CARET] = anon_sym_CARET,
  [anon_sym_if] = anon_sym_if,
  [anon_sym_int] = anon_sym_int,
  [anon_sym_bit] = anon_sym_bit,
  [anon_sym_char] = anon_sym_char,
  [anon_sym_mem] = anon_sym_mem,
  [sym_substidentifier] = sym_substidentifier,
  [sym_identifier] = sym_identifier,
  [sym_number_literal] = sym_number_literal,
  [anon_sym_SQUOTE] = anon_sym_SQUOTE,
  [aux_sym_char_literal_token1] = aux_sym_char_literal_token1,
  [anon_sym_DQUOTE] = anon_sym_DQUOTE,
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
  [sym__atom] = sym__atom,
  [sym__literal] = sym__literal,
  [sym_primitive_type] = sym_primitive_type,
  [sym_char_literal] = sym_char_literal,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
  [aux_sym_parser_sequence_repeat1] = aux_sym_parser_sequence_repeat1,
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
  [anon_sym_COLON] = {
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
  [anon_sym_AMP_GT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_TILDE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_AMP] = {
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
  [anon_sym_PIPE] = {
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
  [anon_sym_COMMA] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_let] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ] = {
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
  [anon_sym_CARET] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_if] = {
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
  [sym_substidentifier] = {
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
  [anon_sym_DQUOTE] = {
    .visible = true,
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
};

enum {
  field_content = 1,
  field_direction = 2,
  field_expr = 3,
  field_from = 4,
  field_left = 5,
  field_name = 6,
  field_op = 7,
  field_parser = 8,
  field_right = 9,
  field_to = 10,
  field_ty = 11,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
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
  [1] = {.index = 0, .length = 2},
  [2] = {.index = 2, .length = 3},
  [3] = {.index = 5, .length = 2},
  [4] = {.index = 7, .length = 3},
  [5] = {.index = 10, .length = 1},
  [6] = {.index = 11, .length = 1},
  [7] = {.index = 12, .length = 2},
  [8] = {.index = 14, .length = 2},
  [9] = {.index = 16, .length = 3},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_op, 0},
    {field_right, 1},
  [2] =
    {field_left, 0},
    {field_op, 1},
    {field_right, 2},
  [5] =
    {field_direction, 0},
    {field_expr, 2},
  [7] =
    {field_from, 1},
    {field_name, 3},
    {field_to, 5},
  [10] =
    {field_parser, 0},
  [11] =
    {field_content, 1},
  [12] =
    {field_left, 0},
    {field_right, 2},
  [14] =
    {field_name, 0},
    {field_parser, 2},
  [16] =
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
      if (eof) ADVANCE(42);
      if (lookahead == '!') ADVANCE(70);
      if (lookahead == '"') ADVANCE(124);
      if (lookahead == '$') ADVANCE(38);
      if (lookahead == '%') ADVANCE(78);
      if (lookahead == '&') ADVANCE(51);
      if (lookahead == '\'') ADVANCE(122);
      if (lookahead == '(') ADVANCE(46);
      if (lookahead == ')') ADVANCE(47);
      if (lookahead == '*') ADVANCE(76);
      if (lookahead == '+') ADVANCE(74);
      if (lookahead == ',') ADVANCE(62);
      if (lookahead == '-') ADVANCE(75);
      if (lookahead == '.') ADVANCE(71);
      if (lookahead == '/') ADVANCE(77);
      if (lookahead == '0') ADVANCE(117);
      if (lookahead == ':') ADVANCE(45);
      if (lookahead == '<') ADVANCE(86);
      if (lookahead == '=') ADVANCE(66);
      if (lookahead == '>') ADVANCE(83);
      if (lookahead == '[') ADVANCE(60);
      if (lookahead == ']') ADVANCE(61);
      if (lookahead == '^') ADVANCE(87);
      if (lookahead == 'a') ADVANCE(26);
      if (lookahead == 'b') ADVANCE(23);
      if (lookahead == 'c') ADVANCE(21);
      if (lookahead == 'd') ADVANCE(15);
      if (lookahead == 'e') ADVANCE(11);
      if (lookahead == 'f') ADVANCE(27);
      if (lookahead == 'i') ADVANCE(19);
      if (lookahead == 'l') ADVANCE(18);
      if (lookahead == 'm') ADVANCE(16);
      if (lookahead == 'o') ADVANCE(28);
      if (lookahead == '{') ADVANCE(52);
      if (lookahead == '|') ADVANCE(55);
      if (lookahead == '}') ADVANCE(53);
      if (lookahead == '~') ADVANCE(49);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(120);
      END_STATE();
    case 1:
      if (lookahead == '!') ADVANCE(69);
      if (lookahead == '&') ADVANCE(9);
      if (lookahead == '\'') ADVANCE(122);
      if (lookahead == '(') ADVANCE(46);
      if (lookahead == ')') ADVANCE(47);
      if (lookahead == '*') ADVANCE(10);
      if (lookahead == '+') ADVANCE(74);
      if (lookahead == '-') ADVANCE(75);
      if (lookahead == '0') ADVANCE(117);
      if (lookahead == ':') ADVANCE(45);
      if (lookahead == '=') ADVANCE(65);
      if (lookahead == ']') ADVANCE(61);
      if (lookahead == 'e') ADVANCE(99);
      if (lookahead == 'f') ADVANCE(110);
      if (lookahead == 'i') ADVANCE(104);
      if (lookahead == 'l') ADVANCE(102);
      if (lookahead == '{') ADVANCE(52);
      if (lookahead == '|') ADVANCE(54);
      if (lookahead == '}') ADVANCE(53);
      if (lookahead == '~') ADVANCE(49);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(1)
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(120);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 2:
      if (lookahead == '!') ADVANCE(69);
      if (lookahead == '\'') ADVANCE(122);
      if (lookahead == '(') ADVANCE(46);
      if (lookahead == '+') ADVANCE(74);
      if (lookahead == '-') ADVANCE(75);
      if (lookahead == '0') ADVANCE(117);
      if (lookahead == 'e') ADVANCE(99);
      if (lookahead == 'f') ADVANCE(110);
      if (lookahead == 'i') ADVANCE(104);
      if (lookahead == '{') ADVANCE(52);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(2)
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(120);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 3:
      if (lookahead == '!') ADVANCE(69);
      if (lookahead == '\'') ADVANCE(122);
      if (lookahead == '0') ADVANCE(117);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(3)
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(120);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 4:
      if (lookahead == '&') ADVANCE(50);
      if (lookahead == '(') ADVANCE(46);
      if (lookahead == 'b') ADVANCE(107);
      if (lookahead == 'c') ADVANCE(106);
      if (lookahead == 'e') ADVANCE(99);
      if (lookahead == 'f') ADVANCE(110);
      if (lookahead == 'i') ADVANCE(109);
      if (lookahead == 'm') ADVANCE(103);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(4)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 5:
      if (lookahead == '&') ADVANCE(50);
      if (lookahead == '(') ADVANCE(46);
      if (lookahead == 'b') ADVANCE(107);
      if (lookahead == 'c') ADVANCE(106);
      if (lookahead == 'e') ADVANCE(99);
      if (lookahead == 'f') ADVANCE(110);
      if (lookahead == 'i') ADVANCE(109);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(5)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 6:
      if (lookahead == '&') ADVANCE(9);
      if (lookahead == ')') ADVANCE(47);
      if (lookahead == '*') ADVANCE(10);
      if (lookahead == '=') ADVANCE(65);
      if (lookahead == ']') ADVANCE(61);
      if (lookahead == 'a') ADVANCE(26);
      if (lookahead == 'o') ADVANCE(28);
      if (lookahead == '~') ADVANCE(49);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(6)
      END_STATE();
    case 7:
      if (lookahead == '=') ADVANCE(82);
      END_STATE();
    case 8:
      if (lookahead == '=') ADVANCE(81);
      END_STATE();
    case 9:
      if (lookahead == '>') ADVANCE(48);
      END_STATE();
    case 10:
      if (lookahead == '>') ADVANCE(44);
      END_STATE();
    case 11:
      if (lookahead == 'a') ADVANCE(13);
      if (lookahead == 'l') ADVANCE(31);
      END_STATE();
    case 12:
      if (lookahead == 'a') ADVANCE(30);
      END_STATE();
    case 13:
      if (lookahead == 'c') ADVANCE(22);
      END_STATE();
    case 14:
      if (lookahead == 'd') ADVANCE(68);
      END_STATE();
    case 15:
      if (lookahead == 'e') ADVANCE(20);
      END_STATE();
    case 16:
      if (lookahead == 'e') ADVANCE(25);
      END_STATE();
    case 17:
      if (lookahead == 'e') ADVANCE(72);
      END_STATE();
    case 18:
      if (lookahead == 'e') ADVANCE(34);
      END_STATE();
    case 19:
      if (lookahead == 'f') ADVANCE(88);
      if (lookahead == 'n') ADVANCE(33);
      END_STATE();
    case 20:
      if (lookahead == 'f') ADVANCE(43);
      END_STATE();
    case 21:
      if (lookahead == 'h') ADVANCE(12);
      END_STATE();
    case 22:
      if (lookahead == 'h') ADVANCE(58);
      END_STATE();
    case 23:
      if (lookahead == 'i') ADVANCE(32);
      END_STATE();
    case 24:
      if (lookahead == 'l') ADVANCE(31);
      END_STATE();
    case 25:
      if (lookahead == 'm') ADVANCE(96);
      END_STATE();
    case 26:
      if (lookahead == 'n') ADVANCE(14);
      END_STATE();
    case 27:
      if (lookahead == 'o') ADVANCE(29);
      END_STATE();
    case 28:
      if (lookahead == 'r') ADVANCE(67);
      END_STATE();
    case 29:
      if (lookahead == 'r') ADVANCE(56);
      END_STATE();
    case 30:
      if (lookahead == 'r') ADVANCE(94);
      END_STATE();
    case 31:
      if (lookahead == 's') ADVANCE(17);
      END_STATE();
    case 32:
      if (lookahead == 't') ADVANCE(92);
      END_STATE();
    case 33:
      if (lookahead == 't') ADVANCE(90);
      END_STATE();
    case 34:
      if (lookahead == 't') ADVANCE(63);
      END_STATE();
    case 35:
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(118);
      END_STATE();
    case 36:
      if (('0' <= lookahead && lookahead <= '7')) ADVANCE(119);
      END_STATE();
    case 37:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(121);
      END_STATE();
    case 38:
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 39:
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '\'') ADVANCE(123);
      END_STATE();
    case 40:
      if (eof) ADVANCE(42);
      if (lookahead == '!') ADVANCE(7);
      if (lookahead == '%') ADVANCE(78);
      if (lookahead == '&') ADVANCE(51);
      if (lookahead == ')') ADVANCE(47);
      if (lookahead == '*') ADVANCE(76);
      if (lookahead == '+') ADVANCE(74);
      if (lookahead == ',') ADVANCE(62);
      if (lookahead == '-') ADVANCE(75);
      if (lookahead == '.') ADVANCE(71);
      if (lookahead == '/') ADVANCE(77);
      if (lookahead == '<') ADVANCE(86);
      if (lookahead == '=') ADVANCE(66);
      if (lookahead == '>') ADVANCE(83);
      if (lookahead == ']') ADVANCE(61);
      if (lookahead == '^') ADVANCE(87);
      if (lookahead == 'a') ADVANCE(26);
      if (lookahead == 'd') ADVANCE(15);
      if (lookahead == 'e') ADVANCE(24);
      if (lookahead == 'o') ADVANCE(28);
      if (lookahead == '|') ADVANCE(55);
      if (lookahead == '~') ADVANCE(49);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(40)
      END_STATE();
    case 41:
      if (eof) ADVANCE(42);
      if (lookahead == '!') ADVANCE(7);
      if (lookahead == '%') ADVANCE(78);
      if (lookahead == '&') ADVANCE(50);
      if (lookahead == ')') ADVANCE(47);
      if (lookahead == '*') ADVANCE(76);
      if (lookahead == '+') ADVANCE(74);
      if (lookahead == ',') ADVANCE(62);
      if (lookahead == '-') ADVANCE(75);
      if (lookahead == '.') ADVANCE(71);
      if (lookahead == '/') ADVANCE(77);
      if (lookahead == ':') ADVANCE(45);
      if (lookahead == '<') ADVANCE(86);
      if (lookahead == '=') ADVANCE(8);
      if (lookahead == '>') ADVANCE(83);
      if (lookahead == ']') ADVANCE(61);
      if (lookahead == '^') ADVANCE(87);
      if (lookahead == 'a') ADVANCE(26);
      if (lookahead == 'd') ADVANCE(15);
      if (lookahead == 'e') ADVANCE(24);
      if (lookahead == 'o') ADVANCE(28);
      if (lookahead == '|') ADVANCE(55);
      if (lookahead == '~') ADVANCE(49);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(41)
      END_STATE();
    case 42:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(anon_sym_def);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(anon_sym_STAR_GT);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(anon_sym_AMP_GT);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(anon_sym_TILDE);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(anon_sym_AMP);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(anon_sym_AMP);
      if (lookahead == '>') ADVANCE(48);
      END_STATE();
    case 52:
      ACCEPT_TOKEN(anon_sym_LBRACE);
      END_STATE();
    case 53:
      ACCEPT_TOKEN(anon_sym_RBRACE);
      END_STATE();
    case 54:
      ACCEPT_TOKEN(anon_sym_PIPE);
      END_STATE();
    case 55:
      ACCEPT_TOKEN(anon_sym_PIPE);
      if (lookahead == '>') ADVANCE(73);
      END_STATE();
    case 56:
      ACCEPT_TOKEN(anon_sym_for);
      END_STATE();
    case 57:
      ACCEPT_TOKEN(anon_sym_for);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 58:
      ACCEPT_TOKEN(anon_sym_each);
      END_STATE();
    case 59:
      ACCEPT_TOKEN(anon_sym_each);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 60:
      ACCEPT_TOKEN(anon_sym_LBRACK);
      END_STATE();
    case 61:
      ACCEPT_TOKEN(anon_sym_RBRACK);
      END_STATE();
    case 62:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 63:
      ACCEPT_TOKEN(anon_sym_let);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(anon_sym_let);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(anon_sym_EQ);
      if (lookahead == '=') ADVANCE(81);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(anon_sym_or);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(anon_sym_and);
      END_STATE();
    case 69:
      ACCEPT_TOKEN(anon_sym_BANG);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(anon_sym_BANG);
      if (lookahead == '=') ADVANCE(82);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(anon_sym_DOT);
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
      if (lookahead == '>') ADVANCE(44);
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
      ACCEPT_TOKEN(anon_sym_CARET);
      END_STATE();
    case 88:
      ACCEPT_TOKEN(anon_sym_if);
      END_STATE();
    case 89:
      ACCEPT_TOKEN(anon_sym_if);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 90:
      ACCEPT_TOKEN(anon_sym_int);
      END_STATE();
    case 91:
      ACCEPT_TOKEN(anon_sym_int);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 92:
      ACCEPT_TOKEN(anon_sym_bit);
      END_STATE();
    case 93:
      ACCEPT_TOKEN(anon_sym_bit);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 94:
      ACCEPT_TOKEN(anon_sym_char);
      END_STATE();
    case 95:
      ACCEPT_TOKEN(anon_sym_char);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 96:
      ACCEPT_TOKEN(anon_sym_mem);
      END_STATE();
    case 97:
      ACCEPT_TOKEN(anon_sym_mem);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 98:
      ACCEPT_TOKEN(sym_substidentifier);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(98);
      END_STATE();
    case 99:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(101);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 100:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(112);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 101:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'c') ADVANCE(105);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 102:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(113);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 103:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(108);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 104:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'f') ADVANCE(89);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 105:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'h') ADVANCE(59);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 106:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'h') ADVANCE(100);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 107:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'i') ADVANCE(114);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 108:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'm') ADVANCE(97);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 109:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'n') ADVANCE(115);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 110:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(111);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 111:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(57);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 112:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(95);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 113:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(64);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 114:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(93);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 115:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(91);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 116:
      ACCEPT_TOKEN(sym_identifier);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 117:
      ACCEPT_TOKEN(sym_number_literal);
      if (lookahead == 'b') ADVANCE(35);
      if (lookahead == 'o') ADVANCE(36);
      if (lookahead == 'x') ADVANCE(37);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(120);
      END_STATE();
    case 118:
      ACCEPT_TOKEN(sym_number_literal);
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(118);
      END_STATE();
    case 119:
      ACCEPT_TOKEN(sym_number_literal);
      if (('0' <= lookahead && lookahead <= '7')) ADVANCE(119);
      END_STATE();
    case 120:
      ACCEPT_TOKEN(sym_number_literal);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(120);
      END_STATE();
    case 121:
      ACCEPT_TOKEN(sym_number_literal);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(121);
      END_STATE();
    case 122:
      ACCEPT_TOKEN(anon_sym_SQUOTE);
      END_STATE();
    case 123:
      ACCEPT_TOKEN(aux_sym_char_literal_token1);
      END_STATE();
    case 124:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 0},
  [2] = {.lex_state = 40},
  [3] = {.lex_state = 40},
  [4] = {.lex_state = 40},
  [5] = {.lex_state = 40},
  [6] = {.lex_state = 41},
  [7] = {.lex_state = 1},
  [8] = {.lex_state = 1},
  [9] = {.lex_state = 1},
  [10] = {.lex_state = 1},
  [11] = {.lex_state = 1},
  [12] = {.lex_state = 41},
  [13] = {.lex_state = 41},
  [14] = {.lex_state = 41},
  [15] = {.lex_state = 41},
  [16] = {.lex_state = 41},
  [17] = {.lex_state = 41},
  [18] = {.lex_state = 41},
  [19] = {.lex_state = 41},
  [20] = {.lex_state = 41},
  [21] = {.lex_state = 41},
  [22] = {.lex_state = 41},
  [23] = {.lex_state = 41},
  [24] = {.lex_state = 41},
  [25] = {.lex_state = 41},
  [26] = {.lex_state = 41},
  [27] = {.lex_state = 41},
  [28] = {.lex_state = 41},
  [29] = {.lex_state = 41},
  [30] = {.lex_state = 41},
  [31] = {.lex_state = 41},
  [32] = {.lex_state = 41},
  [33] = {.lex_state = 41},
  [34] = {.lex_state = 41},
  [35] = {.lex_state = 2},
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
  [51] = {.lex_state = 4},
  [52] = {.lex_state = 1},
  [53] = {.lex_state = 1},
  [54] = {.lex_state = 1},
  [55] = {.lex_state = 1},
  [56] = {.lex_state = 5},
  [57] = {.lex_state = 5},
  [58] = {.lex_state = 5},
  [59] = {.lex_state = 5},
  [60] = {.lex_state = 5},
  [61] = {.lex_state = 5},
  [62] = {.lex_state = 3},
  [63] = {.lex_state = 3},
  [64] = {.lex_state = 3},
  [65] = {.lex_state = 3},
  [66] = {.lex_state = 6},
  [67] = {.lex_state = 1},
  [68] = {.lex_state = 3},
  [69] = {.lex_state = 1},
  [70] = {.lex_state = 1},
  [71] = {.lex_state = 1},
  [72] = {.lex_state = 1},
  [73] = {.lex_state = 1},
  [74] = {.lex_state = 0},
  [75] = {.lex_state = 0},
  [76] = {.lex_state = 1},
  [77] = {.lex_state = 1},
  [78] = {.lex_state = 1},
  [79] = {.lex_state = 1},
  [80] = {.lex_state = 1},
  [81] = {.lex_state = 0},
  [82] = {.lex_state = 0},
  [83] = {.lex_state = 0},
  [84] = {.lex_state = 0},
  [85] = {.lex_state = 0},
  [86] = {.lex_state = 39},
  [87] = {.lex_state = 0},
  [88] = {.lex_state = 3},
  [89] = {.lex_state = 0},
  [90] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_def] = ACTIONS(1),
    [anon_sym_STAR_GT] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_AMP_GT] = ACTIONS(1),
    [anon_sym_TILDE] = ACTIONS(1),
    [anon_sym_AMP] = ACTIONS(1),
    [anon_sym_LBRACE] = ACTIONS(1),
    [anon_sym_RBRACE] = ACTIONS(1),
    [anon_sym_PIPE] = ACTIONS(1),
    [anon_sym_for] = ACTIONS(1),
    [anon_sym_each] = ACTIONS(1),
    [anon_sym_LBRACK] = ACTIONS(1),
    [anon_sym_RBRACK] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
    [anon_sym_let] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
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
    [anon_sym_CARET] = ACTIONS(1),
    [anon_sym_if] = ACTIONS(1),
    [anon_sym_int] = ACTIONS(1),
    [anon_sym_bit] = ACTIONS(1),
    [anon_sym_char] = ACTIONS(1),
    [anon_sym_mem] = ACTIONS(1),
    [sym_substidentifier] = ACTIONS(1),
    [sym_number_literal] = ACTIONS(1),
    [anon_sym_SQUOTE] = ACTIONS(1),
    [anon_sym_DQUOTE] = ACTIONS(1),
  },
  [1] = {
    [sym_source_file] = STATE(87),
    [sym__definition] = STATE(75),
    [sym_parser_definition] = STATE(75),
    [aux_sym_source_file_repeat1] = STATE(75),
    [ts_builtin_sym_end] = ACTIONS(3),
    [anon_sym_def] = ACTIONS(5),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 3,
    ACTIONS(11), 1,
      anon_sym_and,
    ACTIONS(9), 6,
      anon_sym_AMP,
      anon_sym_PIPE,
      anon_sym_EQ,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(7), 23,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_AMP_GT,
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
  [37] = 2,
    ACTIONS(15), 6,
      anon_sym_AMP,
      anon_sym_PIPE,
      anon_sym_EQ,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(13), 24,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_AMP_GT,
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
  [72] = 2,
    ACTIONS(19), 6,
      anon_sym_AMP,
      anon_sym_PIPE,
      anon_sym_EQ,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(17), 24,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_AMP_GT,
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
  [107] = 2,
    ACTIONS(9), 6,
      anon_sym_AMP,
      anon_sym_PIPE,
      anon_sym_EQ,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(7), 24,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_AMP_GT,
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
  [142] = 4,
    ACTIONS(11), 1,
      anon_sym_and,
    ACTIONS(25), 1,
      anon_sym_or,
    ACTIONS(23), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(21), 22,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_AMP,
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
  [179] = 13,
    ACTIONS(27), 1,
      anon_sym_LPAREN,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(31), 1,
      anon_sym_RBRACE,
    ACTIONS(35), 1,
      anon_sym_let,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(41), 1,
      sym_identifier,
    ACTIONS(43), 1,
      sym_number_literal,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(83), 3,
      sym__parser_block_content,
      sym_parser_sequence,
      sym_parser_choice,
    STATE(11), 4,
      sym__statement,
      sym_parse_statement,
      sym_let_statement,
      aux_sym_parser_sequence_repeat1,
    STATE(30), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [234] = 12,
    ACTIONS(47), 1,
      anon_sym_LPAREN,
    ACTIONS(52), 1,
      anon_sym_LBRACE,
    ACTIONS(58), 1,
      anon_sym_let,
    ACTIONS(64), 1,
      anon_sym_if,
    ACTIONS(67), 1,
      sym_identifier,
    ACTIONS(70), 1,
      sym_number_literal,
    ACTIONS(73), 1,
      anon_sym_SQUOTE,
    ACTIONS(55), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(50), 3,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_PIPE,
    ACTIONS(61), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(8), 4,
      sym__statement,
      sym_parse_statement,
      sym_let_statement,
      aux_sym_parser_sequence_repeat1,
    STATE(30), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [286] = 12,
    ACTIONS(27), 1,
      anon_sym_LPAREN,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(35), 1,
      anon_sym_let,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(41), 1,
      sym_identifier,
    ACTIONS(43), 1,
      sym_number_literal,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(81), 3,
      sym__parser_block_content,
      sym_parser_sequence,
      sym_parser_choice,
    STATE(11), 4,
      sym__statement,
      sym_parse_statement,
      sym_let_statement,
      aux_sym_parser_sequence_repeat1,
    STATE(30), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [338] = 12,
    ACTIONS(27), 1,
      anon_sym_LPAREN,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(35), 1,
      anon_sym_let,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(41), 1,
      sym_identifier,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(76), 1,
      sym_number_literal,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(82), 3,
      sym__parser_block_content,
      sym_parser_sequence,
      sym_parser_choice,
    STATE(11), 4,
      sym__statement,
      sym_parse_statement,
      sym_let_statement,
      aux_sym_parser_sequence_repeat1,
    STATE(28), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [390] = 12,
    ACTIONS(27), 1,
      anon_sym_LPAREN,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(35), 1,
      anon_sym_let,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(41), 1,
      sym_identifier,
    ACTIONS(43), 1,
      sym_number_literal,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(78), 3,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_PIPE,
    STATE(8), 4,
      sym__statement,
      sym_parse_statement,
      sym_let_statement,
      aux_sym_parser_sequence_repeat1,
    STATE(30), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [442] = 5,
    ACTIONS(80), 1,
      anon_sym_TILDE,
    ACTIONS(82), 1,
      anon_sym_DOT,
    ACTIONS(84), 1,
      anon_sym_else,
    ACTIONS(23), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(21), 19,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_AMP,
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
  [479] = 12,
    ACTIONS(80), 1,
      anon_sym_TILDE,
    ACTIONS(82), 1,
      anon_sym_DOT,
    ACTIONS(84), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_AMP,
    ACTIONS(92), 1,
      anon_sym_STAR,
    ACTIONS(98), 1,
      anon_sym_CARET,
    ACTIONS(86), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(96), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(23), 3,
      anon_sym_PIPE,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(21), 9,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [530] = 10,
    ACTIONS(80), 1,
      anon_sym_TILDE,
    ACTIONS(82), 1,
      anon_sym_DOT,
    ACTIONS(84), 1,
      anon_sym_else,
    ACTIONS(92), 1,
      anon_sym_STAR,
    ACTIONS(86), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(96), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(23), 3,
      anon_sym_PIPE,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(21), 11,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_AMP,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
  [577] = 8,
    ACTIONS(80), 1,
      anon_sym_TILDE,
    ACTIONS(82), 1,
      anon_sym_DOT,
    ACTIONS(84), 1,
      anon_sym_else,
    ACTIONS(92), 1,
      anon_sym_STAR,
    ACTIONS(86), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(94), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(23), 3,
      anon_sym_PIPE,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(21), 15,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_AMP,
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
  [620] = 2,
    ACTIONS(102), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(100), 22,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_AMP,
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
  [651] = 2,
    ACTIONS(106), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(104), 22,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_AMP,
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
  [682] = 5,
    ACTIONS(80), 1,
      anon_sym_TILDE,
    ACTIONS(82), 1,
      anon_sym_DOT,
    ACTIONS(84), 1,
      anon_sym_else,
    ACTIONS(110), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(108), 19,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_AMP,
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
  [719] = 3,
    ACTIONS(82), 1,
      anon_sym_DOT,
    ACTIONS(23), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(21), 21,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_AMP,
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
  [752] = 2,
    ACTIONS(114), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(112), 22,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_AMP,
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
  [783] = 6,
    ACTIONS(80), 1,
      anon_sym_TILDE,
    ACTIONS(82), 1,
      anon_sym_DOT,
    ACTIONS(84), 1,
      anon_sym_else,
    ACTIONS(86), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(23), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(21), 17,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_AMP,
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
  [822] = 2,
    ACTIONS(23), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(21), 22,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_AMP,
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
  [853] = 2,
    ACTIONS(118), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(116), 22,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_AMP,
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
  [884] = 9,
    ACTIONS(80), 1,
      anon_sym_TILDE,
    ACTIONS(82), 1,
      anon_sym_DOT,
    ACTIONS(84), 1,
      anon_sym_else,
    ACTIONS(92), 1,
      anon_sym_STAR,
    ACTIONS(86), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(23), 3,
      anon_sym_PIPE,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(21), 13,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_AMP,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
  [929] = 13,
    ACTIONS(80), 1,
      anon_sym_TILDE,
    ACTIONS(82), 1,
      anon_sym_DOT,
    ACTIONS(84), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_AMP,
    ACTIONS(92), 1,
      anon_sym_STAR,
    ACTIONS(98), 1,
      anon_sym_CARET,
    ACTIONS(120), 1,
      anon_sym_PIPE,
    ACTIONS(23), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(86), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(96), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(21), 9,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [982] = 11,
    ACTIONS(80), 1,
      anon_sym_TILDE,
    ACTIONS(82), 1,
      anon_sym_DOT,
    ACTIONS(84), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_AMP,
    ACTIONS(92), 1,
      anon_sym_STAR,
    ACTIONS(86), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(96), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(23), 3,
      anon_sym_PIPE,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(21), 10,
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
  [1031] = 3,
    ACTIONS(124), 1,
      anon_sym_COLON,
    ACTIONS(126), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(122), 19,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_AMP,
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
  [1062] = 15,
    ACTIONS(80), 1,
      anon_sym_TILDE,
    ACTIONS(82), 1,
      anon_sym_DOT,
    ACTIONS(84), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_AMP,
    ACTIONS(92), 1,
      anon_sym_STAR,
    ACTIONS(98), 1,
      anon_sym_CARET,
    ACTIONS(120), 1,
      anon_sym_PIPE,
    ACTIONS(128), 1,
      anon_sym_RPAREN,
    ACTIONS(130), 1,
      anon_sym_COMMA,
    ACTIONS(86), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(96), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(134), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(132), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1116] = 14,
    ACTIONS(80), 1,
      anon_sym_TILDE,
    ACTIONS(82), 1,
      anon_sym_DOT,
    ACTIONS(84), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_AMP,
    ACTIONS(92), 1,
      anon_sym_STAR,
    ACTIONS(98), 1,
      anon_sym_CARET,
    ACTIONS(120), 1,
      anon_sym_PIPE,
    ACTIONS(86), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(96), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(134), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(136), 2,
      ts_builtin_sym_end,
      anon_sym_def,
    ACTIONS(132), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1168] = 14,
    ACTIONS(80), 1,
      anon_sym_TILDE,
    ACTIONS(82), 1,
      anon_sym_DOT,
    ACTIONS(84), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_AMP,
    ACTIONS(92), 1,
      anon_sym_STAR,
    ACTIONS(98), 1,
      anon_sym_CARET,
    ACTIONS(120), 1,
      anon_sym_PIPE,
    ACTIONS(130), 1,
      anon_sym_COMMA,
    ACTIONS(86), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(96), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(134), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(132), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1219] = 14,
    ACTIONS(80), 1,
      anon_sym_TILDE,
    ACTIONS(82), 1,
      anon_sym_DOT,
    ACTIONS(84), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_AMP,
    ACTIONS(92), 1,
      anon_sym_STAR,
    ACTIONS(98), 1,
      anon_sym_CARET,
    ACTIONS(120), 1,
      anon_sym_PIPE,
    ACTIONS(128), 1,
      anon_sym_RPAREN,
    ACTIONS(86), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(96), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(134), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(132), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1270] = 14,
    ACTIONS(80), 1,
      anon_sym_TILDE,
    ACTIONS(82), 1,
      anon_sym_DOT,
    ACTIONS(84), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_AMP,
    ACTIONS(92), 1,
      anon_sym_STAR,
    ACTIONS(98), 1,
      anon_sym_CARET,
    ACTIONS(120), 1,
      anon_sym_PIPE,
    ACTIONS(138), 1,
      anon_sym_RBRACK,
    ACTIONS(86), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(96), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(134), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(132), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1321] = 14,
    ACTIONS(80), 1,
      anon_sym_TILDE,
    ACTIONS(82), 1,
      anon_sym_DOT,
    ACTIONS(84), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_AMP,
    ACTIONS(92), 1,
      anon_sym_STAR,
    ACTIONS(98), 1,
      anon_sym_CARET,
    ACTIONS(120), 1,
      anon_sym_PIPE,
    ACTIONS(140), 1,
      anon_sym_COMMA,
    ACTIONS(86), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(96), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(134), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(132), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1372] = 14,
    ACTIONS(80), 1,
      anon_sym_TILDE,
    ACTIONS(82), 1,
      anon_sym_DOT,
    ACTIONS(84), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_AMP,
    ACTIONS(92), 1,
      anon_sym_STAR,
    ACTIONS(98), 1,
      anon_sym_CARET,
    ACTIONS(120), 1,
      anon_sym_PIPE,
    ACTIONS(142), 1,
      anon_sym_COMMA,
    ACTIONS(86), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(96), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(134), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(132), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1423] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(146), 1,
      sym_identifier,
    ACTIONS(148), 1,
      sym_number_literal,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(22), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1461] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(150), 1,
      sym_identifier,
    ACTIONS(152), 1,
      sym_number_literal,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(15), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1499] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(154), 1,
      sym_identifier,
    ACTIONS(156), 1,
      sym_number_literal,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(29), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1537] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(158), 1,
      sym_identifier,
    ACTIONS(160), 1,
      sym_number_literal,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(31), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1575] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(162), 1,
      sym_identifier,
    ACTIONS(164), 1,
      sym_number_literal,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(21), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1613] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(166), 1,
      sym_identifier,
    ACTIONS(168), 1,
      sym_number_literal,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(18), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1651] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(170), 1,
      sym_identifier,
    ACTIONS(172), 1,
      sym_number_literal,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(34), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1689] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(174), 1,
      sym_identifier,
    ACTIONS(176), 1,
      sym_number_literal,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(26), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1727] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(178), 1,
      sym_identifier,
    ACTIONS(180), 1,
      sym_number_literal,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(32), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1765] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(182), 1,
      sym_identifier,
    ACTIONS(184), 1,
      sym_number_literal,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(25), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1803] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(186), 1,
      sym_identifier,
    ACTIONS(188), 1,
      sym_number_literal,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(12), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1841] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(190), 1,
      sym_identifier,
    ACTIONS(192), 1,
      sym_number_literal,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(24), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1879] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(194), 1,
      sym_identifier,
    ACTIONS(196), 1,
      sym_number_literal,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(14), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1917] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(198), 1,
      sym_identifier,
    ACTIONS(200), 1,
      sym_number_literal,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(13), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1955] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(202), 1,
      sym_identifier,
    ACTIONS(204), 1,
      sym_number_literal,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(19), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1993] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(206), 1,
      sym_identifier,
    ACTIONS(208), 1,
      sym_number_literal,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(33), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2031] = 7,
    ACTIONS(210), 1,
      anon_sym_LPAREN,
    ACTIONS(212), 1,
      anon_sym_AMP,
    ACTIONS(218), 1,
      anon_sym_mem,
    ACTIONS(220), 1,
      sym_identifier,
    ACTIONS(214), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(216), 3,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
    STATE(71), 6,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_primitive_type,
  [2061] = 2,
    ACTIONS(224), 5,
      anon_sym_for,
      anon_sym_each,
      anon_sym_let,
      anon_sym_if,
      sym_identifier,
    ACTIONS(222), 10,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_PIPE,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_number_literal,
      anon_sym_SQUOTE,
  [2081] = 2,
    ACTIONS(228), 5,
      anon_sym_for,
      anon_sym_each,
      anon_sym_let,
      anon_sym_if,
      sym_identifier,
    ACTIONS(226), 10,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_PIPE,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_number_literal,
      anon_sym_SQUOTE,
  [2101] = 2,
    ACTIONS(232), 5,
      anon_sym_for,
      anon_sym_each,
      anon_sym_let,
      anon_sym_if,
      sym_identifier,
    ACTIONS(230), 10,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_PIPE,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_number_literal,
      anon_sym_SQUOTE,
  [2121] = 2,
    ACTIONS(236), 5,
      anon_sym_for,
      anon_sym_each,
      anon_sym_let,
      anon_sym_if,
      sym_identifier,
    ACTIONS(234), 10,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_PIPE,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_number_literal,
      anon_sym_SQUOTE,
  [2141] = 6,
    ACTIONS(210), 1,
      anon_sym_LPAREN,
    ACTIONS(212), 1,
      anon_sym_AMP,
    ACTIONS(238), 1,
      sym_identifier,
    ACTIONS(214), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(216), 3,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
    STATE(70), 6,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_primitive_type,
  [2168] = 6,
    ACTIONS(210), 1,
      anon_sym_LPAREN,
    ACTIONS(212), 1,
      anon_sym_AMP,
    ACTIONS(240), 1,
      sym_identifier,
    ACTIONS(214), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(216), 3,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
    STATE(79), 6,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_primitive_type,
  [2195] = 6,
    ACTIONS(210), 1,
      anon_sym_LPAREN,
    ACTIONS(212), 1,
      anon_sym_AMP,
    ACTIONS(242), 1,
      sym_identifier,
    ACTIONS(214), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(216), 3,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
    STATE(80), 6,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_primitive_type,
  [2222] = 6,
    ACTIONS(210), 1,
      anon_sym_LPAREN,
    ACTIONS(212), 1,
      anon_sym_AMP,
    ACTIONS(244), 1,
      sym_identifier,
    ACTIONS(214), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(216), 3,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
    STATE(70), 6,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_primitive_type,
  [2249] = 6,
    ACTIONS(210), 1,
      anon_sym_LPAREN,
    ACTIONS(212), 1,
      anon_sym_AMP,
    ACTIONS(246), 1,
      sym_identifier,
    ACTIONS(214), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(216), 3,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
    STATE(76), 6,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_primitive_type,
  [2276] = 6,
    ACTIONS(210), 1,
      anon_sym_LPAREN,
    ACTIONS(212), 1,
      anon_sym_AMP,
    ACTIONS(248), 1,
      sym_identifier,
    ACTIONS(214), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(216), 3,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
    STATE(77), 6,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_primitive_type,
  [2303] = 4,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(250), 1,
      anon_sym_BANG,
    ACTIONS(252), 2,
      sym_identifier,
      sym_number_literal,
    STATE(6), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2322] = 4,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(250), 1,
      anon_sym_BANG,
    ACTIONS(254), 2,
      sym_identifier,
      sym_number_literal,
    STATE(66), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2341] = 4,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(250), 1,
      anon_sym_BANG,
    ACTIONS(256), 2,
      sym_identifier,
      sym_number_literal,
    STATE(5), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2360] = 4,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(250), 1,
      anon_sym_BANG,
    ACTIONS(258), 2,
      sym_identifier,
      sym_number_literal,
    STATE(2), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2379] = 3,
    ACTIONS(11), 1,
      anon_sym_and,
    ACTIONS(25), 1,
      anon_sym_or,
    ACTIONS(260), 6,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_AMP_GT,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_EQ,
  [2394] = 1,
    ACTIONS(262), 6,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_AMP_GT,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_EQ,
  [2403] = 3,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(264), 2,
      sym_identifier,
      sym_number_literal,
    STATE(4), 3,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2416] = 1,
    ACTIONS(266), 6,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_AMP_GT,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_EQ,
  [2425] = 2,
    ACTIONS(268), 1,
      anon_sym_TILDE,
    ACTIONS(260), 5,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_AMP_GT,
      anon_sym_RBRACK,
      anon_sym_EQ,
  [2436] = 2,
    ACTIONS(268), 1,
      anon_sym_TILDE,
    ACTIONS(270), 5,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_AMP_GT,
      anon_sym_RBRACK,
      anon_sym_EQ,
  [2447] = 1,
    ACTIONS(272), 6,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_AMP_GT,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_EQ,
  [2456] = 1,
    ACTIONS(274), 6,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_AMP_GT,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_EQ,
  [2465] = 3,
    ACTIONS(276), 1,
      ts_builtin_sym_end,
    ACTIONS(278), 1,
      anon_sym_def,
    STATE(74), 3,
      sym__definition,
      sym_parser_definition,
      aux_sym_source_file_repeat1,
  [2477] = 3,
    ACTIONS(5), 1,
      anon_sym_def,
    ACTIONS(281), 1,
      ts_builtin_sym_end,
    STATE(74), 3,
      sym__definition,
      sym_parser_definition,
      aux_sym_source_file_repeat1,
  [2489] = 3,
    ACTIONS(268), 1,
      anon_sym_TILDE,
    ACTIONS(285), 1,
      anon_sym_RBRACK,
    ACTIONS(283), 2,
      anon_sym_STAR_GT,
      anon_sym_AMP_GT,
  [2500] = 3,
    ACTIONS(268), 1,
      anon_sym_TILDE,
    ACTIONS(287), 1,
      anon_sym_RPAREN,
    ACTIONS(283), 2,
      anon_sym_STAR_GT,
      anon_sym_AMP_GT,
  [2511] = 2,
    ACTIONS(291), 1,
      anon_sym_COLON,
    ACTIONS(289), 3,
      anon_sym_STAR_GT,
      anon_sym_AMP_GT,
      anon_sym_TILDE,
  [2520] = 3,
    ACTIONS(268), 1,
      anon_sym_TILDE,
    ACTIONS(293), 1,
      anon_sym_EQ,
    ACTIONS(283), 2,
      anon_sym_STAR_GT,
      anon_sym_AMP_GT,
  [2531] = 3,
    ACTIONS(268), 1,
      anon_sym_TILDE,
    ACTIONS(283), 1,
      anon_sym_AMP_GT,
    ACTIONS(295), 1,
      anon_sym_STAR_GT,
  [2541] = 1,
    ACTIONS(297), 3,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_PIPE,
  [2547] = 2,
    ACTIONS(299), 1,
      anon_sym_RPAREN,
    ACTIONS(301), 1,
      anon_sym_PIPE,
  [2554] = 2,
    ACTIONS(301), 1,
      anon_sym_PIPE,
    ACTIONS(303), 1,
      anon_sym_RBRACE,
  [2561] = 1,
    ACTIONS(305), 1,
      anon_sym_LBRACK,
  [2565] = 1,
    ACTIONS(307), 1,
      anon_sym_COLON,
  [2569] = 1,
    ACTIONS(309), 1,
      aux_sym_char_literal_token1,
  [2573] = 1,
    ACTIONS(311), 1,
      ts_builtin_sym_end,
  [2577] = 1,
    ACTIONS(313), 1,
      sym_identifier,
  [2581] = 1,
    ACTIONS(315), 1,
      anon_sym_LBRACK,
  [2585] = 1,
    ACTIONS(317), 1,
      anon_sym_SQUOTE,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 37,
  [SMALL_STATE(4)] = 72,
  [SMALL_STATE(5)] = 107,
  [SMALL_STATE(6)] = 142,
  [SMALL_STATE(7)] = 179,
  [SMALL_STATE(8)] = 234,
  [SMALL_STATE(9)] = 286,
  [SMALL_STATE(10)] = 338,
  [SMALL_STATE(11)] = 390,
  [SMALL_STATE(12)] = 442,
  [SMALL_STATE(13)] = 479,
  [SMALL_STATE(14)] = 530,
  [SMALL_STATE(15)] = 577,
  [SMALL_STATE(16)] = 620,
  [SMALL_STATE(17)] = 651,
  [SMALL_STATE(18)] = 682,
  [SMALL_STATE(19)] = 719,
  [SMALL_STATE(20)] = 752,
  [SMALL_STATE(21)] = 783,
  [SMALL_STATE(22)] = 822,
  [SMALL_STATE(23)] = 853,
  [SMALL_STATE(24)] = 884,
  [SMALL_STATE(25)] = 929,
  [SMALL_STATE(26)] = 982,
  [SMALL_STATE(27)] = 1031,
  [SMALL_STATE(28)] = 1062,
  [SMALL_STATE(29)] = 1116,
  [SMALL_STATE(30)] = 1168,
  [SMALL_STATE(31)] = 1219,
  [SMALL_STATE(32)] = 1270,
  [SMALL_STATE(33)] = 1321,
  [SMALL_STATE(34)] = 1372,
  [SMALL_STATE(35)] = 1423,
  [SMALL_STATE(36)] = 1461,
  [SMALL_STATE(37)] = 1499,
  [SMALL_STATE(38)] = 1537,
  [SMALL_STATE(39)] = 1575,
  [SMALL_STATE(40)] = 1613,
  [SMALL_STATE(41)] = 1651,
  [SMALL_STATE(42)] = 1689,
  [SMALL_STATE(43)] = 1727,
  [SMALL_STATE(44)] = 1765,
  [SMALL_STATE(45)] = 1803,
  [SMALL_STATE(46)] = 1841,
  [SMALL_STATE(47)] = 1879,
  [SMALL_STATE(48)] = 1917,
  [SMALL_STATE(49)] = 1955,
  [SMALL_STATE(50)] = 1993,
  [SMALL_STATE(51)] = 2031,
  [SMALL_STATE(52)] = 2061,
  [SMALL_STATE(53)] = 2081,
  [SMALL_STATE(54)] = 2101,
  [SMALL_STATE(55)] = 2121,
  [SMALL_STATE(56)] = 2141,
  [SMALL_STATE(57)] = 2168,
  [SMALL_STATE(58)] = 2195,
  [SMALL_STATE(59)] = 2222,
  [SMALL_STATE(60)] = 2249,
  [SMALL_STATE(61)] = 2276,
  [SMALL_STATE(62)] = 2303,
  [SMALL_STATE(63)] = 2322,
  [SMALL_STATE(64)] = 2341,
  [SMALL_STATE(65)] = 2360,
  [SMALL_STATE(66)] = 2379,
  [SMALL_STATE(67)] = 2394,
  [SMALL_STATE(68)] = 2403,
  [SMALL_STATE(69)] = 2416,
  [SMALL_STATE(70)] = 2425,
  [SMALL_STATE(71)] = 2436,
  [SMALL_STATE(72)] = 2447,
  [SMALL_STATE(73)] = 2456,
  [SMALL_STATE(74)] = 2465,
  [SMALL_STATE(75)] = 2477,
  [SMALL_STATE(76)] = 2489,
  [SMALL_STATE(77)] = 2500,
  [SMALL_STATE(78)] = 2511,
  [SMALL_STATE(79)] = 2520,
  [SMALL_STATE(80)] = 2531,
  [SMALL_STATE(81)] = 2541,
  [SMALL_STATE(82)] = 2547,
  [SMALL_STATE(83)] = 2554,
  [SMALL_STATE(84)] = 2561,
  [SMALL_STATE(85)] = 2565,
  [SMALL_STATE(86)] = 2569,
  [SMALL_STATE(87)] = 2573,
  [SMALL_STATE(88)] = 2577,
  [SMALL_STATE(89)] = 2581,
  [SMALL_STATE(90)] = 2585,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(58),
  [7] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_constraint_expression, 3, .production_id = 2),
  [9] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_binary_constraint_expression, 3, .production_id = 2),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(64),
  [13] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_char_literal, 3),
  [15] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_char_literal, 3),
  [17] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_constraint_expression, 2),
  [19] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unary_constraint_expression, 2),
  [21] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_expression, 3, .production_id = 2),
  [23] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_binary_expression, 3, .production_id = 2),
  [25] = {.entry = {.count = 1, .reusable = true}}, SHIFT(65),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [31] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [33] = {.entry = {.count = 1, .reusable = false}}, SHIFT(89),
  [35] = {.entry = {.count = 1, .reusable = false}}, SHIFT(88),
  [37] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [39] = {.entry = {.count = 1, .reusable = false}}, SHIFT(40),
  [41] = {.entry = {.count = 1, .reusable = false}}, SHIFT(27),
  [43] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [45] = {.entry = {.count = 1, .reusable = true}}, SHIFT(86),
  [47] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(10),
  [50] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2),
  [52] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(7),
  [55] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(89),
  [58] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(88),
  [61] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(40),
  [64] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(40),
  [67] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(27),
  [70] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(30),
  [73] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(86),
  [76] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [78] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_sequence, 1),
  [80] = {.entry = {.count = 1, .reusable = true}}, SHIFT(62),
  [82] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [84] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [86] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [88] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [90] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
  [92] = {.entry = {.count = 1, .reusable = false}}, SHIFT(39),
  [94] = {.entry = {.count = 1, .reusable = true}}, SHIFT(39),
  [96] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [98] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
  [100] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_block, 3, .production_id = 6),
  [102] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parser_block, 3, .production_id = 6),
  [104] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__expression, 3),
  [106] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__expression, 3),
  [108] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_expression, 2, .production_id = 1),
  [110] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unary_expression, 2, .production_id = 1),
  [112] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_array, 4, .production_id = 3),
  [114] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parser_array, 4, .production_id = 3),
  [116] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_block, 2),
  [118] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parser_block, 2),
  [120] = {.entry = {.count = 1, .reusable = false}}, SHIFT(48),
  [122] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__atom, 1),
  [124] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [126] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__atom, 1),
  [128] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [130] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [132] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
  [134] = {.entry = {.count = 1, .reusable = false}}, SHIFT(44),
  [136] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_definition, 6, .production_id = 4),
  [138] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [140] = {.entry = {.count = 1, .reusable = true}}, SHIFT(52),
  [142] = {.entry = {.count = 1, .reusable = true}}, SHIFT(54),
  [144] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [146] = {.entry = {.count = 1, .reusable = false}}, SHIFT(22),
  [148] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [150] = {.entry = {.count = 1, .reusable = false}}, SHIFT(15),
  [152] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [154] = {.entry = {.count = 1, .reusable = false}}, SHIFT(29),
  [156] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [158] = {.entry = {.count = 1, .reusable = false}}, SHIFT(31),
  [160] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [162] = {.entry = {.count = 1, .reusable = false}}, SHIFT(21),
  [164] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [166] = {.entry = {.count = 1, .reusable = false}}, SHIFT(18),
  [168] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [170] = {.entry = {.count = 1, .reusable = false}}, SHIFT(34),
  [172] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [174] = {.entry = {.count = 1, .reusable = false}}, SHIFT(26),
  [176] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [178] = {.entry = {.count = 1, .reusable = false}}, SHIFT(32),
  [180] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [182] = {.entry = {.count = 1, .reusable = false}}, SHIFT(25),
  [184] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [186] = {.entry = {.count = 1, .reusable = false}}, SHIFT(12),
  [188] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [190] = {.entry = {.count = 1, .reusable = false}}, SHIFT(24),
  [192] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [194] = {.entry = {.count = 1, .reusable = false}}, SHIFT(14),
  [196] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [198] = {.entry = {.count = 1, .reusable = false}}, SHIFT(13),
  [200] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [202] = {.entry = {.count = 1, .reusable = false}}, SHIFT(19),
  [204] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [206] = {.entry = {.count = 1, .reusable = false}}, SHIFT(33),
  [208] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [210] = {.entry = {.count = 1, .reusable = true}}, SHIFT(61),
  [212] = {.entry = {.count = 1, .reusable = true}}, SHIFT(51),
  [214] = {.entry = {.count = 1, .reusable = false}}, SHIFT(84),
  [216] = {.entry = {.count = 1, .reusable = false}}, SHIFT(73),
  [218] = {.entry = {.count = 1, .reusable = false}}, SHIFT(72),
  [220] = {.entry = {.count = 1, .reusable = false}}, SHIFT(71),
  [222] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_let_statement, 7, .production_id = 9),
  [224] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_let_statement, 7, .production_id = 9),
  [226] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parse_statement, 2, .production_id = 5),
  [228] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parse_statement, 2, .production_id = 5),
  [230] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parse_statement, 4, .production_id = 8),
  [232] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parse_statement, 4, .production_id = 8),
  [234] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 3),
  [236] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 3),
  [238] = {.entry = {.count = 1, .reusable = false}}, SHIFT(78),
  [240] = {.entry = {.count = 1, .reusable = false}}, SHIFT(79),
  [242] = {.entry = {.count = 1, .reusable = false}}, SHIFT(80),
  [244] = {.entry = {.count = 1, .reusable = false}}, SHIFT(70),
  [246] = {.entry = {.count = 1, .reusable = false}}, SHIFT(76),
  [248] = {.entry = {.count = 1, .reusable = false}}, SHIFT(77),
  [250] = {.entry = {.count = 1, .reusable = true}}, SHIFT(68),
  [252] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [254] = {.entry = {.count = 1, .reusable = true}}, SHIFT(66),
  [256] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [258] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [260] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_type_expression, 3, .production_id = 2),
  [262] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type_array, 4, .production_id = 3),
  [264] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [266] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__type_expression, 3),
  [268] = {.entry = {.count = 1, .reusable = true}}, SHIFT(63),
  [270] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_type_expression, 2, .production_id = 1),
  [272] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_primitive_type, 2),
  [274] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_primitive_type, 1),
  [276] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [278] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(58),
  [281] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [283] = {.entry = {.count = 1, .reusable = true}}, SHIFT(59),
  [285] = {.entry = {.count = 1, .reusable = true}}, SHIFT(67),
  [287] = {.entry = {.count = 1, .reusable = true}}, SHIFT(69),
  [289] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__type_atom, 1),
  [291] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
  [293] = {.entry = {.count = 1, .reusable = true}}, SHIFT(50),
  [295] = {.entry = {.count = 1, .reusable = true}}, SHIFT(56),
  [297] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_choice, 3, .production_id = 7),
  [299] = {.entry = {.count = 1, .reusable = true}}, SHIFT(55),
  [301] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [303] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [305] = {.entry = {.count = 1, .reusable = true}}, SHIFT(60),
  [307] = {.entry = {.count = 1, .reusable = true}}, SHIFT(57),
  [309] = {.entry = {.count = 1, .reusable = true}}, SHIFT(90),
  [311] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [313] = {.entry = {.count = 1, .reusable = true}}, SHIFT(85),
  [315] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [317] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
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
