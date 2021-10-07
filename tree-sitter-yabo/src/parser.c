#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 13
#define STATE_COUNT 83
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 70
#define ALIAS_COUNT 0
#define TOKEN_COUNT 45
#define EXTERNAL_TOKEN_COUNT 0
#define FIELD_COUNT 11
#define MAX_ALIAS_SEQUENCE_LENGTH 7
#define PRODUCTION_ID_COUNT 10

enum {
  anon_sym_parser = 1,
  anon_sym_EQ = 2,
  anon_sym_STAR_GT = 3,
  anon_sym_LPAREN = 4,
  anon_sym_RPAREN = 5,
  anon_sym_PIPE_GT = 6,
  anon_sym_TILDE = 7,
  anon_sym_if = 8,
  anon_sym_LBRACE = 9,
  anon_sym_RBRACE = 10,
  anon_sym_PIPE = 11,
  anon_sym_for = 12,
  anon_sym_each = 13,
  anon_sym_rof = 14,
  anon_sym_LBRACK = 15,
  anon_sym_RBRACK = 16,
  anon_sym_COLON = 17,
  anon_sym_SEMI = 18,
  anon_sym_let = 19,
  anon_sym_PIPE_PIPE = 20,
  anon_sym_AMP_AMP = 21,
  anon_sym_BANG = 22,
  anon_sym_DOT = 23,
  anon_sym_else = 24,
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
  anon_sym_AMP = 39,
  sym_identifier = 40,
  sym_number_literal = 41,
  anon_sym_SQUOTE = 42,
  aux_sym_char_literal_token1 = 43,
  anon_sym_DQUOTE = 44,
  sym_source_file = 45,
  sym__definition = 46,
  sym_parser_definition = 47,
  sym__parse_expression = 48,
  sym_binary_parse_expression = 49,
  sym_unary_parse_expression = 50,
  sym__expression = 51,
  sym_parser_block = 52,
  sym__parser_block_content = 53,
  sym_parser_sequence = 54,
  sym_parser_choice = 55,
  sym_parser_array = 56,
  sym__statement = 57,
  sym_parse_statement = 58,
  sym_let_statement = 59,
  sym__constraint_expression = 60,
  sym_binary_constraint_expression = 61,
  sym_unary_constraint_expression = 62,
  sym_binary_expression = 63,
  sym_unary_expression = 64,
  sym__atom = 65,
  sym__literal = 66,
  sym_char_literal = 67,
  aux_sym_source_file_repeat1 = 68,
  aux_sym_parser_sequence_repeat1 = 69,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_parser] = "parser",
  [anon_sym_EQ] = "=",
  [anon_sym_STAR_GT] = "*>",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [anon_sym_PIPE_GT] = "|>",
  [anon_sym_TILDE] = "~",
  [anon_sym_if] = "if",
  [anon_sym_LBRACE] = "{",
  [anon_sym_RBRACE] = "}",
  [anon_sym_PIPE] = "|",
  [anon_sym_for] = "for",
  [anon_sym_each] = "each",
  [anon_sym_rof] = "rof",
  [anon_sym_LBRACK] = "[",
  [anon_sym_RBRACK] = "]",
  [anon_sym_COLON] = ":",
  [anon_sym_SEMI] = ";",
  [anon_sym_let] = "let",
  [anon_sym_PIPE_PIPE] = "||",
  [anon_sym_AMP_AMP] = "&&",
  [anon_sym_BANG] = "!",
  [anon_sym_DOT] = ".",
  [anon_sym_else] = "else",
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
  [anon_sym_AMP] = "&",
  [sym_identifier] = "identifier",
  [sym_number_literal] = "number_literal",
  [anon_sym_SQUOTE] = "'",
  [aux_sym_char_literal_token1] = "char_literal_token1",
  [anon_sym_DQUOTE] = "\"",
  [sym_source_file] = "source_file",
  [sym__definition] = "_definition",
  [sym_parser_definition] = "parser_definition",
  [sym__parse_expression] = "_parse_expression",
  [sym_binary_parse_expression] = "binary_parse_expression",
  [sym_unary_parse_expression] = "unary_parse_expression",
  [sym__expression] = "_expression",
  [sym_parser_block] = "parser_block",
  [sym__parser_block_content] = "_parser_block_content",
  [sym_parser_sequence] = "parser_sequence",
  [sym_parser_choice] = "parser_choice",
  [sym_parser_array] = "parser_array",
  [sym__statement] = "_statement",
  [sym_parse_statement] = "parse_statement",
  [sym_let_statement] = "let_statement",
  [sym__constraint_expression] = "_constraint_expression",
  [sym_binary_constraint_expression] = "binary_constraint_expression",
  [sym_unary_constraint_expression] = "unary_constraint_expression",
  [sym_binary_expression] = "binary_expression",
  [sym_unary_expression] = "unary_expression",
  [sym__atom] = "_atom",
  [sym__literal] = "_literal",
  [sym_char_literal] = "char_literal",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
  [aux_sym_parser_sequence_repeat1] = "parser_sequence_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_parser] = anon_sym_parser,
  [anon_sym_EQ] = anon_sym_EQ,
  [anon_sym_STAR_GT] = anon_sym_STAR_GT,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_PIPE_GT] = anon_sym_PIPE_GT,
  [anon_sym_TILDE] = anon_sym_TILDE,
  [anon_sym_if] = anon_sym_if,
  [anon_sym_LBRACE] = anon_sym_LBRACE,
  [anon_sym_RBRACE] = anon_sym_RBRACE,
  [anon_sym_PIPE] = anon_sym_PIPE,
  [anon_sym_for] = anon_sym_for,
  [anon_sym_each] = anon_sym_each,
  [anon_sym_rof] = anon_sym_rof,
  [anon_sym_LBRACK] = anon_sym_LBRACK,
  [anon_sym_RBRACK] = anon_sym_RBRACK,
  [anon_sym_COLON] = anon_sym_COLON,
  [anon_sym_SEMI] = anon_sym_SEMI,
  [anon_sym_let] = anon_sym_let,
  [anon_sym_PIPE_PIPE] = anon_sym_PIPE_PIPE,
  [anon_sym_AMP_AMP] = anon_sym_AMP_AMP,
  [anon_sym_BANG] = anon_sym_BANG,
  [anon_sym_DOT] = anon_sym_DOT,
  [anon_sym_else] = anon_sym_else,
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
  [anon_sym_AMP] = anon_sym_AMP,
  [sym_identifier] = sym_identifier,
  [sym_number_literal] = sym_number_literal,
  [anon_sym_SQUOTE] = anon_sym_SQUOTE,
  [aux_sym_char_literal_token1] = aux_sym_char_literal_token1,
  [anon_sym_DQUOTE] = anon_sym_DQUOTE,
  [sym_source_file] = sym_source_file,
  [sym__definition] = sym__definition,
  [sym_parser_definition] = sym_parser_definition,
  [sym__parse_expression] = sym__parse_expression,
  [sym_binary_parse_expression] = sym_binary_parse_expression,
  [sym_unary_parse_expression] = sym_unary_parse_expression,
  [sym__expression] = sym__expression,
  [sym_parser_block] = sym_parser_block,
  [sym__parser_block_content] = sym__parser_block_content,
  [sym_parser_sequence] = sym_parser_sequence,
  [sym_parser_choice] = sym_parser_choice,
  [sym_parser_array] = sym_parser_array,
  [sym__statement] = sym__statement,
  [sym_parse_statement] = sym_parse_statement,
  [sym_let_statement] = sym_let_statement,
  [sym__constraint_expression] = sym__constraint_expression,
  [sym_binary_constraint_expression] = sym_binary_constraint_expression,
  [sym_unary_constraint_expression] = sym_unary_constraint_expression,
  [sym_binary_expression] = sym_binary_expression,
  [sym_unary_expression] = sym_unary_expression,
  [sym__atom] = sym__atom,
  [sym__literal] = sym__literal,
  [sym_char_literal] = sym_char_literal,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
  [aux_sym_parser_sequence_repeat1] = aux_sym_parser_sequence_repeat1,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [anon_sym_parser] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_STAR_GT] = {
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
  [anon_sym_PIPE_GT] = {
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
  [anon_sym_rof] = {
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
  [anon_sym_SEMI] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_let] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_PIPE_PIPE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_AMP_AMP] = {
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
  [anon_sym_AMP] = {
    .visible = true,
    .named = false,
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
  [sym__parse_expression] = {
    .visible = false,
    .named = true,
  },
  [sym_binary_parse_expression] = {
    .visible = true,
    .named = true,
  },
  [sym_unary_parse_expression] = {
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
  [sym__atom] = {
    .visible = false,
    .named = true,
  },
  [sym__literal] = {
    .visible = false,
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
  [2] = {.index = 2, .length = 1},
  [3] = {.index = 3, .length = 1},
  [4] = {.index = 4, .length = 3},
  [5] = {.index = 7, .length = 3},
  [6] = {.index = 10, .length = 2},
  [7] = {.index = 12, .length = 2},
  [8] = {.index = 14, .length = 2},
  [9] = {.index = 16, .length = 3},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_op, 0},
    {field_right, 1},
  [2] =
    {field_parser, 0},
  [3] =
    {field_content, 1},
  [4] =
    {field_from, 3},
    {field_name, 1},
    {field_to, 5},
  [7] =
    {field_left, 0},
    {field_op, 1},
    {field_right, 2},
  [10] =
    {field_left, 0},
    {field_right, 2},
  [12] =
    {field_direction, 0},
    {field_expr, 2},
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
      if (eof) ADVANCE(34);
      if (lookahead == '!') ADVANCE(64);
      if (lookahead == '"') ADVANCE(102);
      if (lookahead == '%') ADVANCE(72);
      if (lookahead == '&') ADVANCE(83);
      if (lookahead == '\'') ADVANCE(100);
      if (lookahead == '(') ADVANCE(39);
      if (lookahead == ')') ADVANCE(40);
      if (lookahead == '*') ADVANCE(70);
      if (lookahead == '+') ADVANCE(67);
      if (lookahead == '-') ADVANCE(68);
      if (lookahead == '.') ADVANCE(65);
      if (lookahead == '/') ADVANCE(71);
      if (lookahead == '0') ADVANCE(95);
      if (lookahead == ':') ADVANCE(57);
      if (lookahead == ';') ADVANCE(58);
      if (lookahead == '<') ADVANCE(80);
      if (lookahead == '=') ADVANCE(37);
      if (lookahead == '>') ADVANCE(77);
      if (lookahead == '[') ADVANCE(55);
      if (lookahead == ']') ADVANCE(56);
      if (lookahead == '^') ADVANCE(81);
      if (lookahead == 'e') ADVANCE(10);
      if (lookahead == 'f') ADVANCE(20);
      if (lookahead == 'i') ADVANCE(16);
      if (lookahead == 'l') ADVANCE(13);
      if (lookahead == 'p') ADVANCE(11);
      if (lookahead == 'r') ADVANCE(21);
      if (lookahead == '{') ADVANCE(45);
      if (lookahead == '|') ADVANCE(48);
      if (lookahead == '}') ADVANCE(46);
      if (lookahead == '~') ADVANCE(42);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(98);
      END_STATE();
    case 1:
      if (lookahead == '!') ADVANCE(6);
      if (lookahead == '%') ADVANCE(72);
      if (lookahead == '&') ADVANCE(82);
      if (lookahead == ')') ADVANCE(40);
      if (lookahead == '*') ADVANCE(69);
      if (lookahead == '+') ADVANCE(67);
      if (lookahead == '-') ADVANCE(68);
      if (lookahead == '.') ADVANCE(65);
      if (lookahead == '/') ADVANCE(71);
      if (lookahead == ';') ADVANCE(58);
      if (lookahead == '<') ADVANCE(80);
      if (lookahead == '=') ADVANCE(7);
      if (lookahead == '>') ADVANCE(77);
      if (lookahead == '^') ADVANCE(81);
      if (lookahead == 'e') ADVANCE(19);
      if (lookahead == '|') ADVANCE(47);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(1)
      END_STATE();
    case 2:
      if (lookahead == '!') ADVANCE(63);
      if (lookahead == '\'') ADVANCE(100);
      if (lookahead == '(') ADVANCE(39);
      if (lookahead == '+') ADVANCE(67);
      if (lookahead == '-') ADVANCE(68);
      if (lookahead == '0') ADVANCE(95);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(2)
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(98);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(94);
      END_STATE();
    case 3:
      if (lookahead == '&') ADVANCE(62);
      END_STATE();
    case 4:
      if (lookahead == '\'') ADVANCE(100);
      if (lookahead == '(') ADVANCE(39);
      if (lookahead == ')') ADVANCE(40);
      if (lookahead == '*') ADVANCE(8);
      if (lookahead == '0') ADVANCE(95);
      if (lookahead == '=') ADVANCE(36);
      if (lookahead == 'e') ADVANCE(84);
      if (lookahead == 'f') ADVANCE(90);
      if (lookahead == 'i') ADVANCE(87);
      if (lookahead == 'l') ADVANCE(86);
      if (lookahead == 'r') ADVANCE(91);
      if (lookahead == '{') ADVANCE(45);
      if (lookahead == '|') ADVANCE(47);
      if (lookahead == '}') ADVANCE(46);
      if (lookahead == '~') ADVANCE(42);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(4)
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(98);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(94);
      END_STATE();
    case 5:
      if (lookahead == '\'') ADVANCE(100);
      if (lookahead == '(') ADVANCE(39);
      if (lookahead == '0') ADVANCE(95);
      if (lookahead == 'e') ADVANCE(84);
      if (lookahead == 'f') ADVANCE(90);
      if (lookahead == 'i') ADVANCE(87);
      if (lookahead == 'r') ADVANCE(91);
      if (lookahead == '{') ADVANCE(45);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(5)
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(98);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(94);
      END_STATE();
    case 6:
      if (lookahead == '=') ADVANCE(76);
      END_STATE();
    case 7:
      if (lookahead == '=') ADVANCE(75);
      END_STATE();
    case 8:
      if (lookahead == '>') ADVANCE(38);
      END_STATE();
    case 9:
      if (lookahead == '>') ADVANCE(41);
      if (lookahead == '|') ADVANCE(61);
      END_STATE();
    case 10:
      if (lookahead == 'a') ADVANCE(12);
      if (lookahead == 'l') ADVANCE(25);
      END_STATE();
    case 11:
      if (lookahead == 'a') ADVANCE(24);
      END_STATE();
    case 12:
      if (lookahead == 'c') ADVANCE(18);
      END_STATE();
    case 13:
      if (lookahead == 'e') ADVANCE(27);
      END_STATE();
    case 14:
      if (lookahead == 'e') ADVANCE(66);
      END_STATE();
    case 15:
      if (lookahead == 'e') ADVANCE(23);
      END_STATE();
    case 16:
      if (lookahead == 'f') ADVANCE(43);
      END_STATE();
    case 17:
      if (lookahead == 'f') ADVANCE(53);
      END_STATE();
    case 18:
      if (lookahead == 'h') ADVANCE(51);
      END_STATE();
    case 19:
      if (lookahead == 'l') ADVANCE(25);
      END_STATE();
    case 20:
      if (lookahead == 'o') ADVANCE(22);
      END_STATE();
    case 21:
      if (lookahead == 'o') ADVANCE(17);
      END_STATE();
    case 22:
      if (lookahead == 'r') ADVANCE(49);
      END_STATE();
    case 23:
      if (lookahead == 'r') ADVANCE(35);
      END_STATE();
    case 24:
      if (lookahead == 'r') ADVANCE(26);
      END_STATE();
    case 25:
      if (lookahead == 's') ADVANCE(14);
      END_STATE();
    case 26:
      if (lookahead == 's') ADVANCE(15);
      END_STATE();
    case 27:
      if (lookahead == 't') ADVANCE(59);
      END_STATE();
    case 28:
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(96);
      END_STATE();
    case 29:
      if (('0' <= lookahead && lookahead <= '7')) ADVANCE(97);
      END_STATE();
    case 30:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(99);
      END_STATE();
    case 31:
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '\'') ADVANCE(101);
      END_STATE();
    case 32:
      if (eof) ADVANCE(34);
      if (lookahead == '!') ADVANCE(6);
      if (lookahead == '%') ADVANCE(72);
      if (lookahead == '&') ADVANCE(83);
      if (lookahead == ')') ADVANCE(40);
      if (lookahead == '*') ADVANCE(70);
      if (lookahead == '+') ADVANCE(67);
      if (lookahead == '-') ADVANCE(68);
      if (lookahead == '.') ADVANCE(65);
      if (lookahead == '/') ADVANCE(71);
      if (lookahead == ';') ADVANCE(58);
      if (lookahead == '<') ADVANCE(80);
      if (lookahead == '=') ADVANCE(37);
      if (lookahead == '>') ADVANCE(77);
      if (lookahead == ']') ADVANCE(56);
      if (lookahead == '^') ADVANCE(81);
      if (lookahead == 'e') ADVANCE(19);
      if (lookahead == 'p') ADVANCE(11);
      if (lookahead == '|') ADVANCE(48);
      if (lookahead == '~') ADVANCE(42);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(32)
      END_STATE();
    case 33:
      if (eof) ADVANCE(34);
      if (lookahead == '&') ADVANCE(3);
      if (lookahead == ')') ADVANCE(40);
      if (lookahead == '*') ADVANCE(8);
      if (lookahead == ';') ADVANCE(58);
      if (lookahead == '=') ADVANCE(36);
      if (lookahead == ']') ADVANCE(56);
      if (lookahead == 'p') ADVANCE(11);
      if (lookahead == '|') ADVANCE(9);
      if (lookahead == '~') ADVANCE(42);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(33)
      END_STATE();
    case 34:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(anon_sym_parser);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(anon_sym_EQ);
      if (lookahead == '=') ADVANCE(75);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(anon_sym_STAR_GT);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(anon_sym_PIPE_GT);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(anon_sym_TILDE);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(anon_sym_if);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(anon_sym_if);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(94);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(anon_sym_LBRACE);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(anon_sym_RBRACE);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(anon_sym_PIPE);
      if (lookahead == '>') ADVANCE(41);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(anon_sym_PIPE);
      if (lookahead == '>') ADVANCE(41);
      if (lookahead == '|') ADVANCE(61);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(anon_sym_for);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(anon_sym_for);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(94);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(anon_sym_each);
      END_STATE();
    case 52:
      ACCEPT_TOKEN(anon_sym_each);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(94);
      END_STATE();
    case 53:
      ACCEPT_TOKEN(anon_sym_rof);
      END_STATE();
    case 54:
      ACCEPT_TOKEN(anon_sym_rof);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(94);
      END_STATE();
    case 55:
      ACCEPT_TOKEN(anon_sym_LBRACK);
      END_STATE();
    case 56:
      ACCEPT_TOKEN(anon_sym_RBRACK);
      END_STATE();
    case 57:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 58:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 59:
      ACCEPT_TOKEN(anon_sym_let);
      END_STATE();
    case 60:
      ACCEPT_TOKEN(anon_sym_let);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(94);
      END_STATE();
    case 61:
      ACCEPT_TOKEN(anon_sym_PIPE_PIPE);
      END_STATE();
    case 62:
      ACCEPT_TOKEN(anon_sym_AMP_AMP);
      END_STATE();
    case 63:
      ACCEPT_TOKEN(anon_sym_BANG);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(anon_sym_BANG);
      if (lookahead == '=') ADVANCE(76);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(anon_sym_else);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 69:
      ACCEPT_TOKEN(anon_sym_STAR);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(anon_sym_STAR);
      if (lookahead == '>') ADVANCE(38);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(anon_sym_SLASH);
      END_STATE();
    case 72:
      ACCEPT_TOKEN(anon_sym_PERCENT);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(anon_sym_LT_LT);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(anon_sym_GT_GT);
      END_STATE();
    case 75:
      ACCEPT_TOKEN(anon_sym_EQ_EQ);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(anon_sym_BANG_EQ);
      END_STATE();
    case 77:
      ACCEPT_TOKEN(anon_sym_GT);
      if (lookahead == '=') ADVANCE(78);
      if (lookahead == '>') ADVANCE(74);
      END_STATE();
    case 78:
      ACCEPT_TOKEN(anon_sym_GT_EQ);
      END_STATE();
    case 79:
      ACCEPT_TOKEN(anon_sym_LT_EQ);
      END_STATE();
    case 80:
      ACCEPT_TOKEN(anon_sym_LT);
      if (lookahead == '<') ADVANCE(73);
      if (lookahead == '=') ADVANCE(79);
      END_STATE();
    case 81:
      ACCEPT_TOKEN(anon_sym_CARET);
      END_STATE();
    case 82:
      ACCEPT_TOKEN(anon_sym_AMP);
      END_STATE();
    case 83:
      ACCEPT_TOKEN(anon_sym_AMP);
      if (lookahead == '&') ADVANCE(62);
      END_STATE();
    case 84:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(85);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(94);
      END_STATE();
    case 85:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'c') ADVANCE(89);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(94);
      END_STATE();
    case 86:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(93);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(94);
      END_STATE();
    case 87:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'f') ADVANCE(44);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(94);
      END_STATE();
    case 88:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'f') ADVANCE(54);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(94);
      END_STATE();
    case 89:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'h') ADVANCE(52);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(94);
      END_STATE();
    case 90:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(92);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(94);
      END_STATE();
    case 91:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(88);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(94);
      END_STATE();
    case 92:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(50);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(94);
      END_STATE();
    case 93:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(60);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(94);
      END_STATE();
    case 94:
      ACCEPT_TOKEN(sym_identifier);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(94);
      END_STATE();
    case 95:
      ACCEPT_TOKEN(sym_number_literal);
      if (lookahead == 'b') ADVANCE(28);
      if (lookahead == 'o') ADVANCE(29);
      if (lookahead == 'x') ADVANCE(30);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(98);
      END_STATE();
    case 96:
      ACCEPT_TOKEN(sym_number_literal);
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(96);
      END_STATE();
    case 97:
      ACCEPT_TOKEN(sym_number_literal);
      if (('0' <= lookahead && lookahead <= '7')) ADVANCE(97);
      END_STATE();
    case 98:
      ACCEPT_TOKEN(sym_number_literal);
      if (('0' <= lookahead && lookahead <= '9')) ADVANCE(98);
      END_STATE();
    case 99:
      ACCEPT_TOKEN(sym_number_literal);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(99);
      END_STATE();
    case 100:
      ACCEPT_TOKEN(anon_sym_SQUOTE);
      END_STATE();
    case 101:
      ACCEPT_TOKEN(aux_sym_char_literal_token1);
      END_STATE();
    case 102:
      ACCEPT_TOKEN(anon_sym_DQUOTE);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 0},
  [2] = {.lex_state = 32},
  [3] = {.lex_state = 4},
  [4] = {.lex_state = 4},
  [5] = {.lex_state = 4},
  [6] = {.lex_state = 4},
  [7] = {.lex_state = 4},
  [8] = {.lex_state = 1},
  [9] = {.lex_state = 1},
  [10] = {.lex_state = 1},
  [11] = {.lex_state = 1},
  [12] = {.lex_state = 1},
  [13] = {.lex_state = 1},
  [14] = {.lex_state = 1},
  [15] = {.lex_state = 1},
  [16] = {.lex_state = 1},
  [17] = {.lex_state = 1},
  [18] = {.lex_state = 1},
  [19] = {.lex_state = 1},
  [20] = {.lex_state = 1},
  [21] = {.lex_state = 1},
  [22] = {.lex_state = 5},
  [23] = {.lex_state = 5},
  [24] = {.lex_state = 5},
  [25] = {.lex_state = 5},
  [26] = {.lex_state = 5},
  [27] = {.lex_state = 5},
  [28] = {.lex_state = 5},
  [29] = {.lex_state = 5},
  [30] = {.lex_state = 2},
  [31] = {.lex_state = 4},
  [32] = {.lex_state = 4},
  [33] = {.lex_state = 2},
  [34] = {.lex_state = 4},
  [35] = {.lex_state = 2},
  [36] = {.lex_state = 2},
  [37] = {.lex_state = 2},
  [38] = {.lex_state = 2},
  [39] = {.lex_state = 2},
  [40] = {.lex_state = 2},
  [41] = {.lex_state = 2},
  [42] = {.lex_state = 2},
  [43] = {.lex_state = 4},
  [44] = {.lex_state = 2},
  [45] = {.lex_state = 2},
  [46] = {.lex_state = 2},
  [47] = {.lex_state = 33},
  [48] = {.lex_state = 33},
  [49] = {.lex_state = 33},
  [50] = {.lex_state = 33},
  [51] = {.lex_state = 2},
  [52] = {.lex_state = 2},
  [53] = {.lex_state = 2},
  [54] = {.lex_state = 33},
  [55] = {.lex_state = 33},
  [56] = {.lex_state = 33},
  [57] = {.lex_state = 33},
  [58] = {.lex_state = 33},
  [59] = {.lex_state = 33},
  [60] = {.lex_state = 2},
  [61] = {.lex_state = 0},
  [62] = {.lex_state = 0},
  [63] = {.lex_state = 0},
  [64] = {.lex_state = 0},
  [65] = {.lex_state = 0},
  [66] = {.lex_state = 4},
  [67] = {.lex_state = 0},
  [68] = {.lex_state = 0},
  [69] = {.lex_state = 4},
  [70] = {.lex_state = 0},
  [71] = {.lex_state = 0},
  [72] = {.lex_state = 4},
  [73] = {.lex_state = 4},
  [74] = {.lex_state = 4},
  [75] = {.lex_state = 0},
  [76] = {.lex_state = 0},
  [77] = {.lex_state = 2},
  [78] = {.lex_state = 31},
  [79] = {.lex_state = 0},
  [80] = {.lex_state = 2},
  [81] = {.lex_state = 4},
  [82] = {.lex_state = 0},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_parser] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
    [anon_sym_STAR_GT] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_PIPE_GT] = ACTIONS(1),
    [anon_sym_TILDE] = ACTIONS(1),
    [anon_sym_if] = ACTIONS(1),
    [anon_sym_LBRACE] = ACTIONS(1),
    [anon_sym_RBRACE] = ACTIONS(1),
    [anon_sym_PIPE] = ACTIONS(1),
    [anon_sym_for] = ACTIONS(1),
    [anon_sym_each] = ACTIONS(1),
    [anon_sym_rof] = ACTIONS(1),
    [anon_sym_LBRACK] = ACTIONS(1),
    [anon_sym_RBRACK] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
    [anon_sym_SEMI] = ACTIONS(1),
    [anon_sym_let] = ACTIONS(1),
    [anon_sym_PIPE_PIPE] = ACTIONS(1),
    [anon_sym_AMP_AMP] = ACTIONS(1),
    [anon_sym_BANG] = ACTIONS(1),
    [anon_sym_DOT] = ACTIONS(1),
    [anon_sym_else] = ACTIONS(1),
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
    [anon_sym_AMP] = ACTIONS(1),
    [sym_number_literal] = ACTIONS(1),
    [anon_sym_SQUOTE] = ACTIONS(1),
    [anon_sym_DQUOTE] = ACTIONS(1),
  },
  [1] = {
    [sym_source_file] = STATE(82),
    [sym__definition] = STATE(61),
    [sym_parser_definition] = STATE(61),
    [aux_sym_source_file_repeat1] = STATE(61),
    [ts_builtin_sym_end] = ACTIONS(3),
    [anon_sym_parser] = ACTIONS(5),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 2,
    ACTIONS(9), 6,
      anon_sym_EQ,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_AMP,
    ACTIONS(7), 23,
      ts_builtin_sym_end,
      anon_sym_parser,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_PIPE_GT,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_SEMI,
      anon_sym_PIPE_PIPE,
      anon_sym_AMP_AMP,
      anon_sym_DOT,
      anon_sym_else,
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
  [34] = 12,
    ACTIONS(11), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_if,
    ACTIONS(15), 1,
      anon_sym_LBRACE,
    ACTIONS(17), 1,
      anon_sym_RBRACE,
    ACTIONS(21), 1,
      anon_sym_let,
    ACTIONS(23), 1,
      sym_identifier,
    ACTIONS(25), 1,
      sym_number_literal,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(19), 3,
      anon_sym_for,
      anon_sym_each,
      anon_sym_rof,
    STATE(74), 3,
      sym__parser_block_content,
      sym_parser_sequence,
      sym_parser_choice,
    STATE(5), 4,
      sym__statement,
      sym_parse_statement,
      sym_let_statement,
      aux_sym_parser_sequence_repeat1,
    STATE(68), 8,
      sym__parse_expression,
      sym_binary_parse_expression,
      sym_unary_parse_expression,
      sym_parser_block,
      sym_parser_array,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [85] = 11,
    ACTIONS(29), 1,
      anon_sym_LPAREN,
    ACTIONS(34), 1,
      anon_sym_if,
    ACTIONS(37), 1,
      anon_sym_LBRACE,
    ACTIONS(43), 1,
      anon_sym_let,
    ACTIONS(46), 1,
      sym_identifier,
    ACTIONS(49), 1,
      sym_number_literal,
    ACTIONS(52), 1,
      anon_sym_SQUOTE,
    ACTIONS(32), 3,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_PIPE,
    ACTIONS(40), 3,
      anon_sym_for,
      anon_sym_each,
      anon_sym_rof,
    STATE(4), 4,
      sym__statement,
      sym_parse_statement,
      sym_let_statement,
      aux_sym_parser_sequence_repeat1,
    STATE(68), 8,
      sym__parse_expression,
      sym_binary_parse_expression,
      sym_unary_parse_expression,
      sym_parser_block,
      sym_parser_array,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [133] = 11,
    ACTIONS(11), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_if,
    ACTIONS(15), 1,
      anon_sym_LBRACE,
    ACTIONS(21), 1,
      anon_sym_let,
    ACTIONS(23), 1,
      sym_identifier,
    ACTIONS(25), 1,
      sym_number_literal,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(19), 3,
      anon_sym_for,
      anon_sym_each,
      anon_sym_rof,
    ACTIONS(55), 3,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_PIPE,
    STATE(4), 4,
      sym__statement,
      sym_parse_statement,
      sym_let_statement,
      aux_sym_parser_sequence_repeat1,
    STATE(68), 8,
      sym__parse_expression,
      sym_binary_parse_expression,
      sym_unary_parse_expression,
      sym_parser_block,
      sym_parser_array,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [181] = 11,
    ACTIONS(11), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_if,
    ACTIONS(15), 1,
      anon_sym_LBRACE,
    ACTIONS(21), 1,
      anon_sym_let,
    ACTIONS(23), 1,
      sym_identifier,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(57), 1,
      sym_number_literal,
    ACTIONS(19), 3,
      anon_sym_for,
      anon_sym_each,
      anon_sym_rof,
    STATE(73), 3,
      sym__parser_block_content,
      sym_parser_sequence,
      sym_parser_choice,
    STATE(5), 4,
      sym__statement,
      sym_parse_statement,
      sym_let_statement,
      aux_sym_parser_sequence_repeat1,
    STATE(64), 8,
      sym__parse_expression,
      sym_binary_parse_expression,
      sym_unary_parse_expression,
      sym_parser_block,
      sym_parser_array,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [229] = 11,
    ACTIONS(11), 1,
      anon_sym_LPAREN,
    ACTIONS(13), 1,
      anon_sym_if,
    ACTIONS(15), 1,
      anon_sym_LBRACE,
    ACTIONS(21), 1,
      anon_sym_let,
    ACTIONS(23), 1,
      sym_identifier,
    ACTIONS(25), 1,
      sym_number_literal,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(19), 3,
      anon_sym_for,
      anon_sym_each,
      anon_sym_rof,
    STATE(69), 3,
      sym__parser_block_content,
      sym_parser_sequence,
      sym_parser_choice,
    STATE(5), 4,
      sym__statement,
      sym_parse_statement,
      sym_let_statement,
      aux_sym_parser_sequence_repeat1,
    STATE(68), 8,
      sym__parse_expression,
      sym_binary_parse_expression,
      sym_unary_parse_expression,
      sym_parser_block,
      sym_parser_array,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [277] = 7,
    ACTIONS(61), 1,
      anon_sym_PIPE_GT,
    ACTIONS(65), 1,
      anon_sym_DOT,
    ACTIONS(67), 1,
      anon_sym_else,
    ACTIONS(69), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(63), 3,
      anon_sym_PIPE,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(71), 3,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(59), 10,
      anon_sym_RPAREN,
      anon_sym_SEMI,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_AMP,
  [313] = 11,
    ACTIONS(61), 1,
      anon_sym_PIPE_GT,
    ACTIONS(65), 1,
      anon_sym_DOT,
    ACTIONS(67), 1,
      anon_sym_else,
    ACTIONS(73), 1,
      anon_sym_PIPE,
    ACTIONS(77), 1,
      anon_sym_CARET,
    ACTIONS(79), 1,
      anon_sym_AMP,
    ACTIONS(63), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(69), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(75), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(71), 3,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(59), 6,
      anon_sym_RPAREN,
      anon_sym_SEMI,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [357] = 5,
    ACTIONS(61), 1,
      anon_sym_PIPE_GT,
    ACTIONS(65), 1,
      anon_sym_DOT,
    ACTIONS(67), 1,
      anon_sym_else,
    ACTIONS(63), 3,
      anon_sym_PIPE,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(59), 15,
      anon_sym_RPAREN,
      anon_sym_SEMI,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_STAR,
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
  [389] = 6,
    ACTIONS(61), 1,
      anon_sym_PIPE_GT,
    ACTIONS(65), 1,
      anon_sym_DOT,
    ACTIONS(67), 1,
      anon_sym_else,
    ACTIONS(63), 3,
      anon_sym_PIPE,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(71), 3,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(59), 12,
      anon_sym_RPAREN,
      anon_sym_SEMI,
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
  [423] = 3,
    ACTIONS(65), 1,
      anon_sym_DOT,
    ACTIONS(63), 3,
      anon_sym_PIPE,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(59), 17,
      anon_sym_RPAREN,
      anon_sym_PIPE_GT,
      anon_sym_SEMI,
      anon_sym_else,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_STAR,
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
  [451] = 2,
    ACTIONS(63), 3,
      anon_sym_PIPE,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(59), 18,
      anon_sym_RPAREN,
      anon_sym_PIPE_GT,
      anon_sym_SEMI,
      anon_sym_DOT,
      anon_sym_else,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_STAR,
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
  [477] = 10,
    ACTIONS(61), 1,
      anon_sym_PIPE_GT,
    ACTIONS(65), 1,
      anon_sym_DOT,
    ACTIONS(67), 1,
      anon_sym_else,
    ACTIONS(77), 1,
      anon_sym_CARET,
    ACTIONS(79), 1,
      anon_sym_AMP,
    ACTIONS(69), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(75), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(63), 3,
      anon_sym_PIPE,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(71), 3,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(59), 6,
      anon_sym_RPAREN,
      anon_sym_SEMI,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [519] = 4,
    ACTIONS(65), 1,
      anon_sym_DOT,
    ACTIONS(67), 1,
      anon_sym_else,
    ACTIONS(83), 3,
      anon_sym_PIPE,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(81), 16,
      anon_sym_RPAREN,
      anon_sym_PIPE_GT,
      anon_sym_SEMI,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_STAR,
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
  [549] = 8,
    ACTIONS(61), 1,
      anon_sym_PIPE_GT,
    ACTIONS(65), 1,
      anon_sym_DOT,
    ACTIONS(67), 1,
      anon_sym_else,
    ACTIONS(69), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(75), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(63), 3,
      anon_sym_PIPE,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(71), 3,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(59), 8,
      anon_sym_RPAREN,
      anon_sym_SEMI,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
      anon_sym_AMP,
  [587] = 4,
    ACTIONS(65), 1,
      anon_sym_DOT,
    ACTIONS(67), 1,
      anon_sym_else,
    ACTIONS(63), 3,
      anon_sym_PIPE,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(59), 16,
      anon_sym_RPAREN,
      anon_sym_PIPE_GT,
      anon_sym_SEMI,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_STAR,
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
  [617] = 2,
    ACTIONS(87), 3,
      anon_sym_PIPE,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(85), 18,
      anon_sym_RPAREN,
      anon_sym_PIPE_GT,
      anon_sym_SEMI,
      anon_sym_DOT,
      anon_sym_else,
      anon_sym_PLUS,
      anon_sym_DASH,
      anon_sym_STAR,
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
  [643] = 9,
    ACTIONS(61), 1,
      anon_sym_PIPE_GT,
    ACTIONS(65), 1,
      anon_sym_DOT,
    ACTIONS(67), 1,
      anon_sym_else,
    ACTIONS(79), 1,
      anon_sym_AMP,
    ACTIONS(69), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(75), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(63), 3,
      anon_sym_PIPE,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(71), 3,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(59), 7,
      anon_sym_RPAREN,
      anon_sym_SEMI,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
      anon_sym_CARET,
  [683] = 12,
    ACTIONS(61), 1,
      anon_sym_PIPE_GT,
    ACTIONS(65), 1,
      anon_sym_DOT,
    ACTIONS(67), 1,
      anon_sym_else,
    ACTIONS(73), 1,
      anon_sym_PIPE,
    ACTIONS(77), 1,
      anon_sym_CARET,
    ACTIONS(79), 1,
      anon_sym_AMP,
    ACTIONS(89), 1,
      anon_sym_RPAREN,
    ACTIONS(69), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(75), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(93), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(71), 3,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(91), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [728] = 12,
    ACTIONS(61), 1,
      anon_sym_PIPE_GT,
    ACTIONS(65), 1,
      anon_sym_DOT,
    ACTIONS(67), 1,
      anon_sym_else,
    ACTIONS(73), 1,
      anon_sym_PIPE,
    ACTIONS(77), 1,
      anon_sym_CARET,
    ACTIONS(79), 1,
      anon_sym_AMP,
    ACTIONS(95), 1,
      anon_sym_SEMI,
    ACTIONS(69), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(75), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(93), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(71), 3,
      anon_sym_STAR,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(91), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [773] = 8,
    ACTIONS(13), 1,
      anon_sym_if,
    ACTIONS(15), 1,
      anon_sym_LBRACE,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(97), 1,
      anon_sym_LPAREN,
    ACTIONS(99), 1,
      sym_identifier,
    ACTIONS(101), 1,
      sym_number_literal,
    ACTIONS(19), 3,
      anon_sym_for,
      anon_sym_each,
      anon_sym_rof,
    STATE(66), 8,
      sym__parse_expression,
      sym_binary_parse_expression,
      sym_unary_parse_expression,
      sym_parser_block,
      sym_parser_array,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [807] = 8,
    ACTIONS(13), 1,
      anon_sym_if,
    ACTIONS(15), 1,
      anon_sym_LBRACE,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(97), 1,
      anon_sym_LPAREN,
    ACTIONS(103), 1,
      sym_identifier,
    ACTIONS(105), 1,
      sym_number_literal,
    ACTIONS(19), 3,
      anon_sym_for,
      anon_sym_each,
      anon_sym_rof,
    STATE(67), 8,
      sym__parse_expression,
      sym_binary_parse_expression,
      sym_unary_parse_expression,
      sym_parser_block,
      sym_parser_array,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [841] = 8,
    ACTIONS(13), 1,
      anon_sym_if,
    ACTIONS(15), 1,
      anon_sym_LBRACE,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(97), 1,
      anon_sym_LPAREN,
    ACTIONS(107), 1,
      sym_identifier,
    ACTIONS(109), 1,
      sym_number_literal,
    ACTIONS(19), 3,
      anon_sym_for,
      anon_sym_each,
      anon_sym_rof,
    STATE(72), 8,
      sym__parse_expression,
      sym_binary_parse_expression,
      sym_unary_parse_expression,
      sym_parser_block,
      sym_parser_array,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [875] = 8,
    ACTIONS(13), 1,
      anon_sym_if,
    ACTIONS(15), 1,
      anon_sym_LBRACE,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(97), 1,
      anon_sym_LPAREN,
    ACTIONS(111), 1,
      sym_identifier,
    ACTIONS(113), 1,
      sym_number_literal,
    ACTIONS(19), 3,
      anon_sym_for,
      anon_sym_each,
      anon_sym_rof,
    STATE(65), 8,
      sym__parse_expression,
      sym_binary_parse_expression,
      sym_unary_parse_expression,
      sym_parser_block,
      sym_parser_array,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [909] = 8,
    ACTIONS(13), 1,
      anon_sym_if,
    ACTIONS(15), 1,
      anon_sym_LBRACE,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(97), 1,
      anon_sym_LPAREN,
    ACTIONS(115), 1,
      sym_identifier,
    ACTIONS(117), 1,
      sym_number_literal,
    ACTIONS(19), 3,
      anon_sym_for,
      anon_sym_each,
      anon_sym_rof,
    STATE(58), 8,
      sym__parse_expression,
      sym_binary_parse_expression,
      sym_unary_parse_expression,
      sym_parser_block,
      sym_parser_array,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [943] = 8,
    ACTIONS(13), 1,
      anon_sym_if,
    ACTIONS(15), 1,
      anon_sym_LBRACE,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(97), 1,
      anon_sym_LPAREN,
    ACTIONS(119), 1,
      sym_identifier,
    ACTIONS(121), 1,
      sym_number_literal,
    ACTIONS(19), 3,
      anon_sym_for,
      anon_sym_each,
      anon_sym_rof,
    STATE(71), 8,
      sym__parse_expression,
      sym_binary_parse_expression,
      sym_unary_parse_expression,
      sym_parser_block,
      sym_parser_array,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [977] = 8,
    ACTIONS(13), 1,
      anon_sym_if,
    ACTIONS(15), 1,
      anon_sym_LBRACE,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(97), 1,
      anon_sym_LPAREN,
    ACTIONS(123), 1,
      sym_identifier,
    ACTIONS(125), 1,
      sym_number_literal,
    ACTIONS(19), 3,
      anon_sym_for,
      anon_sym_each,
      anon_sym_rof,
    STATE(59), 8,
      sym__parse_expression,
      sym_binary_parse_expression,
      sym_unary_parse_expression,
      sym_parser_block,
      sym_parser_array,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1011] = 8,
    ACTIONS(13), 1,
      anon_sym_if,
    ACTIONS(15), 1,
      anon_sym_LBRACE,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(97), 1,
      anon_sym_LPAREN,
    ACTIONS(127), 1,
      sym_identifier,
    ACTIONS(129), 1,
      sym_number_literal,
    ACTIONS(19), 3,
      anon_sym_for,
      anon_sym_each,
      anon_sym_rof,
    STATE(70), 8,
      sym__parse_expression,
      sym_binary_parse_expression,
      sym_unary_parse_expression,
      sym_parser_block,
      sym_parser_array,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1045] = 5,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(131), 1,
      anon_sym_LPAREN,
    ACTIONS(135), 2,
      sym_identifier,
      sym_number_literal,
    ACTIONS(133), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(10), 6,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1069] = 2,
    ACTIONS(139), 6,
      anon_sym_if,
      anon_sym_for,
      anon_sym_each,
      anon_sym_rof,
      anon_sym_let,
      sym_identifier,
    ACTIONS(137), 7,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_PIPE,
      sym_number_literal,
      anon_sym_SQUOTE,
  [1087] = 2,
    ACTIONS(143), 6,
      anon_sym_if,
      anon_sym_for,
      anon_sym_each,
      anon_sym_rof,
      anon_sym_let,
      sym_identifier,
    ACTIONS(141), 7,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_PIPE,
      sym_number_literal,
      anon_sym_SQUOTE,
  [1105] = 5,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(131), 1,
      anon_sym_LPAREN,
    ACTIONS(145), 2,
      sym_identifier,
      sym_number_literal,
    ACTIONS(133), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(11), 6,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1129] = 2,
    ACTIONS(149), 6,
      anon_sym_if,
      anon_sym_for,
      anon_sym_each,
      anon_sym_rof,
      anon_sym_let,
      sym_identifier,
    ACTIONS(147), 7,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_PIPE,
      sym_number_literal,
      anon_sym_SQUOTE,
  [1147] = 5,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(131), 1,
      anon_sym_LPAREN,
    ACTIONS(151), 2,
      sym_identifier,
      sym_number_literal,
    ACTIONS(133), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(14), 6,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1171] = 5,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(131), 1,
      anon_sym_LPAREN,
    ACTIONS(153), 2,
      sym_identifier,
      sym_number_literal,
    ACTIONS(133), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(17), 6,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1195] = 5,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(131), 1,
      anon_sym_LPAREN,
    ACTIONS(155), 2,
      sym_identifier,
      sym_number_literal,
    ACTIONS(133), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(12), 6,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1219] = 5,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(131), 1,
      anon_sym_LPAREN,
    ACTIONS(157), 2,
      sym_identifier,
      sym_number_literal,
    ACTIONS(133), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(8), 6,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1243] = 5,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(131), 1,
      anon_sym_LPAREN,
    ACTIONS(159), 2,
      sym_identifier,
      sym_number_literal,
    ACTIONS(133), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(13), 6,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1267] = 5,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(131), 1,
      anon_sym_LPAREN,
    ACTIONS(161), 2,
      sym_identifier,
      sym_number_literal,
    ACTIONS(133), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(16), 6,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1291] = 5,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(131), 1,
      anon_sym_LPAREN,
    ACTIONS(163), 2,
      sym_identifier,
      sym_number_literal,
    ACTIONS(133), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(20), 6,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1315] = 5,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(131), 1,
      anon_sym_LPAREN,
    ACTIONS(165), 2,
      sym_identifier,
      sym_number_literal,
    ACTIONS(133), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(21), 6,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1339] = 2,
    ACTIONS(169), 6,
      anon_sym_if,
      anon_sym_for,
      anon_sym_each,
      anon_sym_rof,
      anon_sym_let,
      sym_identifier,
    ACTIONS(167), 7,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_PIPE,
      sym_number_literal,
      anon_sym_SQUOTE,
  [1357] = 5,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(131), 1,
      anon_sym_LPAREN,
    ACTIONS(171), 2,
      sym_identifier,
      sym_number_literal,
    ACTIONS(133), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(9), 6,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1381] = 5,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(131), 1,
      anon_sym_LPAREN,
    ACTIONS(173), 2,
      sym_identifier,
      sym_number_literal,
    ACTIONS(133), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(19), 6,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1405] = 5,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(131), 1,
      anon_sym_LPAREN,
    ACTIONS(175), 2,
      sym_identifier,
      sym_number_literal,
    ACTIONS(133), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(15), 6,
      sym__expression,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1429] = 1,
    ACTIONS(177), 11,
      ts_builtin_sym_end,
      anon_sym_parser,
      anon_sym_EQ,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_PIPE_GT,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_SEMI,
      anon_sym_PIPE_PIPE,
      anon_sym_AMP_AMP,
  [1443] = 2,
    ACTIONS(181), 1,
      anon_sym_AMP_AMP,
    ACTIONS(179), 10,
      ts_builtin_sym_end,
      anon_sym_parser,
      anon_sym_EQ,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_PIPE_GT,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_SEMI,
      anon_sym_PIPE_PIPE,
  [1459] = 1,
    ACTIONS(179), 11,
      ts_builtin_sym_end,
      anon_sym_parser,
      anon_sym_EQ,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_PIPE_GT,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_SEMI,
      anon_sym_PIPE_PIPE,
      anon_sym_AMP_AMP,
  [1473] = 3,
    ACTIONS(181), 1,
      anon_sym_AMP_AMP,
    ACTIONS(185), 1,
      anon_sym_PIPE_PIPE,
    ACTIONS(183), 9,
      ts_builtin_sym_end,
      anon_sym_parser,
      anon_sym_EQ,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_PIPE_GT,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_SEMI,
  [1491] = 4,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(187), 1,
      anon_sym_BANG,
    ACTIONS(189), 2,
      sym_identifier,
      sym_number_literal,
    STATE(49), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1510] = 4,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(187), 1,
      anon_sym_BANG,
    ACTIONS(191), 2,
      sym_identifier,
      sym_number_literal,
    STATE(48), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1529] = 4,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(187), 1,
      anon_sym_BANG,
    ACTIONS(193), 2,
      sym_identifier,
      sym_number_literal,
    STATE(50), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1548] = 1,
    ACTIONS(195), 9,
      ts_builtin_sym_end,
      anon_sym_parser,
      anon_sym_EQ,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_PIPE_GT,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_SEMI,
  [1560] = 1,
    ACTIONS(197), 9,
      ts_builtin_sym_end,
      anon_sym_parser,
      anon_sym_EQ,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_PIPE_GT,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_SEMI,
  [1572] = 1,
    ACTIONS(199), 9,
      ts_builtin_sym_end,
      anon_sym_parser,
      anon_sym_EQ,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_PIPE_GT,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_SEMI,
  [1584] = 1,
    ACTIONS(201), 9,
      ts_builtin_sym_end,
      anon_sym_parser,
      anon_sym_EQ,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_PIPE_GT,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_SEMI,
  [1596] = 2,
    ACTIONS(203), 1,
      anon_sym_TILDE,
    ACTIONS(183), 8,
      ts_builtin_sym_end,
      anon_sym_parser,
      anon_sym_EQ,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_PIPE_GT,
      anon_sym_RBRACK,
      anon_sym_SEMI,
  [1610] = 3,
    ACTIONS(203), 1,
      anon_sym_TILDE,
    ACTIONS(207), 1,
      anon_sym_PIPE_GT,
    ACTIONS(205), 7,
      ts_builtin_sym_end,
      anon_sym_parser,
      anon_sym_EQ,
      anon_sym_STAR_GT,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_SEMI,
  [1626] = 3,
    ACTIONS(27), 1,
      anon_sym_SQUOTE,
    ACTIONS(209), 2,
      sym_identifier,
      sym_number_literal,
    STATE(47), 3,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1639] = 3,
    ACTIONS(5), 1,
      anon_sym_parser,
    ACTIONS(211), 1,
      ts_builtin_sym_end,
    STATE(62), 3,
      sym__definition,
      sym_parser_definition,
      aux_sym_source_file_repeat1,
  [1651] = 3,
    ACTIONS(213), 1,
      ts_builtin_sym_end,
    ACTIONS(215), 1,
      anon_sym_parser,
    STATE(62), 3,
      sym__definition,
      sym_parser_definition,
      aux_sym_source_file_repeat1,
  [1663] = 2,
    ACTIONS(220), 1,
      anon_sym_COLON,
    ACTIONS(218), 4,
      anon_sym_RPAREN,
      anon_sym_PIPE_GT,
      anon_sym_TILDE,
      anon_sym_SEMI,
  [1673] = 4,
    ACTIONS(203), 1,
      anon_sym_TILDE,
    ACTIONS(207), 1,
      anon_sym_PIPE_GT,
    ACTIONS(222), 1,
      anon_sym_RPAREN,
    ACTIONS(224), 1,
      anon_sym_SEMI,
  [1686] = 3,
    ACTIONS(203), 1,
      anon_sym_TILDE,
    ACTIONS(207), 1,
      anon_sym_PIPE_GT,
    ACTIONS(226), 2,
      ts_builtin_sym_end,
      anon_sym_parser,
  [1697] = 3,
    ACTIONS(203), 1,
      anon_sym_TILDE,
    ACTIONS(207), 1,
      anon_sym_PIPE_GT,
    ACTIONS(228), 1,
      anon_sym_STAR_GT,
  [1707] = 3,
    ACTIONS(203), 1,
      anon_sym_TILDE,
    ACTIONS(207), 1,
      anon_sym_PIPE_GT,
    ACTIONS(230), 1,
      anon_sym_RBRACK,
  [1717] = 3,
    ACTIONS(203), 1,
      anon_sym_TILDE,
    ACTIONS(207), 1,
      anon_sym_PIPE_GT,
    ACTIONS(224), 1,
      anon_sym_SEMI,
  [1727] = 1,
    ACTIONS(232), 3,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_PIPE,
  [1733] = 3,
    ACTIONS(203), 1,
      anon_sym_TILDE,
    ACTIONS(207), 1,
      anon_sym_PIPE_GT,
    ACTIONS(222), 1,
      anon_sym_RPAREN,
  [1743] = 3,
    ACTIONS(203), 1,
      anon_sym_TILDE,
    ACTIONS(207), 1,
      anon_sym_PIPE_GT,
    ACTIONS(234), 1,
      anon_sym_SEMI,
  [1753] = 3,
    ACTIONS(203), 1,
      anon_sym_TILDE,
    ACTIONS(207), 1,
      anon_sym_PIPE_GT,
    ACTIONS(236), 1,
      anon_sym_EQ,
  [1763] = 2,
    ACTIONS(238), 1,
      anon_sym_RPAREN,
    ACTIONS(240), 1,
      anon_sym_PIPE,
  [1770] = 2,
    ACTIONS(240), 1,
      anon_sym_PIPE,
    ACTIONS(242), 1,
      anon_sym_RBRACE,
  [1777] = 1,
    ACTIONS(244), 1,
      anon_sym_COLON,
  [1781] = 1,
    ACTIONS(246), 1,
      anon_sym_SQUOTE,
  [1785] = 1,
    ACTIONS(248), 1,
      sym_identifier,
  [1789] = 1,
    ACTIONS(250), 1,
      aux_sym_char_literal_token1,
  [1793] = 1,
    ACTIONS(252), 1,
      anon_sym_LBRACK,
  [1797] = 1,
    ACTIONS(254), 1,
      sym_identifier,
  [1801] = 1,
    ACTIONS(256), 1,
      anon_sym_EQ,
  [1805] = 1,
    ACTIONS(258), 1,
      ts_builtin_sym_end,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 34,
  [SMALL_STATE(4)] = 85,
  [SMALL_STATE(5)] = 133,
  [SMALL_STATE(6)] = 181,
  [SMALL_STATE(7)] = 229,
  [SMALL_STATE(8)] = 277,
  [SMALL_STATE(9)] = 313,
  [SMALL_STATE(10)] = 357,
  [SMALL_STATE(11)] = 389,
  [SMALL_STATE(12)] = 423,
  [SMALL_STATE(13)] = 451,
  [SMALL_STATE(14)] = 477,
  [SMALL_STATE(15)] = 519,
  [SMALL_STATE(16)] = 549,
  [SMALL_STATE(17)] = 587,
  [SMALL_STATE(18)] = 617,
  [SMALL_STATE(19)] = 643,
  [SMALL_STATE(20)] = 683,
  [SMALL_STATE(21)] = 728,
  [SMALL_STATE(22)] = 773,
  [SMALL_STATE(23)] = 807,
  [SMALL_STATE(24)] = 841,
  [SMALL_STATE(25)] = 875,
  [SMALL_STATE(26)] = 909,
  [SMALL_STATE(27)] = 943,
  [SMALL_STATE(28)] = 977,
  [SMALL_STATE(29)] = 1011,
  [SMALL_STATE(30)] = 1045,
  [SMALL_STATE(31)] = 1069,
  [SMALL_STATE(32)] = 1087,
  [SMALL_STATE(33)] = 1105,
  [SMALL_STATE(34)] = 1129,
  [SMALL_STATE(35)] = 1147,
  [SMALL_STATE(36)] = 1171,
  [SMALL_STATE(37)] = 1195,
  [SMALL_STATE(38)] = 1219,
  [SMALL_STATE(39)] = 1243,
  [SMALL_STATE(40)] = 1267,
  [SMALL_STATE(41)] = 1291,
  [SMALL_STATE(42)] = 1315,
  [SMALL_STATE(43)] = 1339,
  [SMALL_STATE(44)] = 1357,
  [SMALL_STATE(45)] = 1381,
  [SMALL_STATE(46)] = 1405,
  [SMALL_STATE(47)] = 1429,
  [SMALL_STATE(48)] = 1443,
  [SMALL_STATE(49)] = 1459,
  [SMALL_STATE(50)] = 1473,
  [SMALL_STATE(51)] = 1491,
  [SMALL_STATE(52)] = 1510,
  [SMALL_STATE(53)] = 1529,
  [SMALL_STATE(54)] = 1548,
  [SMALL_STATE(55)] = 1560,
  [SMALL_STATE(56)] = 1572,
  [SMALL_STATE(57)] = 1584,
  [SMALL_STATE(58)] = 1596,
  [SMALL_STATE(59)] = 1610,
  [SMALL_STATE(60)] = 1626,
  [SMALL_STATE(61)] = 1639,
  [SMALL_STATE(62)] = 1651,
  [SMALL_STATE(63)] = 1663,
  [SMALL_STATE(64)] = 1673,
  [SMALL_STATE(65)] = 1686,
  [SMALL_STATE(66)] = 1697,
  [SMALL_STATE(67)] = 1707,
  [SMALL_STATE(68)] = 1717,
  [SMALL_STATE(69)] = 1727,
  [SMALL_STATE(70)] = 1733,
  [SMALL_STATE(71)] = 1743,
  [SMALL_STATE(72)] = 1753,
  [SMALL_STATE(73)] = 1763,
  [SMALL_STATE(74)] = 1770,
  [SMALL_STATE(75)] = 1777,
  [SMALL_STATE(76)] = 1781,
  [SMALL_STATE(77)] = 1785,
  [SMALL_STATE(78)] = 1789,
  [SMALL_STATE(79)] = 1793,
  [SMALL_STATE(80)] = 1797,
  [SMALL_STATE(81)] = 1801,
  [SMALL_STATE(82)] = 1805,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(80),
  [7] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_char_literal, 3),
  [9] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_char_literal, 3),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [13] = {.entry = {.count = 1, .reusable = false}}, SHIFT(28),
  [15] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [17] = {.entry = {.count = 1, .reusable = true}}, SHIFT(57),
  [19] = {.entry = {.count = 1, .reusable = false}}, SHIFT(79),
  [21] = {.entry = {.count = 1, .reusable = false}}, SHIFT(77),
  [23] = {.entry = {.count = 1, .reusable = false}}, SHIFT(63),
  [25] = {.entry = {.count = 1, .reusable = true}}, SHIFT(68),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(78),
  [29] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(6),
  [32] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2),
  [34] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(28),
  [37] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(3),
  [40] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(79),
  [43] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(77),
  [46] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(63),
  [49] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(68),
  [52] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(78),
  [55] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_sequence, 1),
  [57] = {.entry = {.count = 1, .reusable = true}}, SHIFT(64),
  [59] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_expression, 3, .production_id = 5),
  [61] = {.entry = {.count = 1, .reusable = true}}, SHIFT(36),
  [63] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_binary_expression, 3, .production_id = 5),
  [65] = {.entry = {.count = 1, .reusable = true}}, SHIFT(39),
  [67] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
  [69] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [71] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [73] = {.entry = {.count = 1, .reusable = false}}, SHIFT(35),
  [75] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [77] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [79] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [81] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_expression, 2, .production_id = 1),
  [83] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unary_expression, 2, .production_id = 1),
  [85] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__expression, 3),
  [87] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__expression, 3),
  [89] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [91] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
  [93] = {.entry = {.count = 1, .reusable = false}}, SHIFT(44),
  [95] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [97] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [99] = {.entry = {.count = 1, .reusable = false}}, SHIFT(66),
  [101] = {.entry = {.count = 1, .reusable = true}}, SHIFT(66),
  [103] = {.entry = {.count = 1, .reusable = false}}, SHIFT(67),
  [105] = {.entry = {.count = 1, .reusable = true}}, SHIFT(67),
  [107] = {.entry = {.count = 1, .reusable = false}}, SHIFT(72),
  [109] = {.entry = {.count = 1, .reusable = true}}, SHIFT(72),
  [111] = {.entry = {.count = 1, .reusable = false}}, SHIFT(65),
  [113] = {.entry = {.count = 1, .reusable = true}}, SHIFT(65),
  [115] = {.entry = {.count = 1, .reusable = false}}, SHIFT(58),
  [117] = {.entry = {.count = 1, .reusable = true}}, SHIFT(58),
  [119] = {.entry = {.count = 1, .reusable = false}}, SHIFT(71),
  [121] = {.entry = {.count = 1, .reusable = true}}, SHIFT(71),
  [123] = {.entry = {.count = 1, .reusable = false}}, SHIFT(59),
  [125] = {.entry = {.count = 1, .reusable = true}}, SHIFT(59),
  [127] = {.entry = {.count = 1, .reusable = false}}, SHIFT(70),
  [129] = {.entry = {.count = 1, .reusable = true}}, SHIFT(70),
  [131] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [133] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [135] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [137] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parse_statement, 4, .production_id = 8),
  [139] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parse_statement, 4, .production_id = 8),
  [141] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_let_statement, 7, .production_id = 9),
  [143] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_let_statement, 7, .production_id = 9),
  [145] = {.entry = {.count = 1, .reusable = true}}, SHIFT(11),
  [147] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parse_statement, 2, .production_id = 2),
  [149] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parse_statement, 2, .production_id = 2),
  [151] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [153] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [155] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [157] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [159] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [161] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [163] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [165] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [167] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 3),
  [169] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 3),
  [171] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [173] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [175] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [177] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_constraint_expression, 2),
  [179] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_constraint_expression, 3, .production_id = 5),
  [181] = {.entry = {.count = 1, .reusable = true}}, SHIFT(51),
  [183] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_parse_expression, 3, .production_id = 5),
  [185] = {.entry = {.count = 1, .reusable = true}}, SHIFT(52),
  [187] = {.entry = {.count = 1, .reusable = true}}, SHIFT(60),
  [189] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [191] = {.entry = {.count = 1, .reusable = true}}, SHIFT(48),
  [193] = {.entry = {.count = 1, .reusable = true}}, SHIFT(50),
  [195] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__parse_expression, 3),
  [197] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_block, 3, .production_id = 3),
  [199] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_array, 4, .production_id = 7),
  [201] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_block, 2),
  [203] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [205] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_parse_expression, 2, .production_id = 1),
  [207] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [209] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [211] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [213] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [215] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(80),
  [218] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__atom, 1),
  [220] = {.entry = {.count = 1, .reusable = true}}, SHIFT(27),
  [222] = {.entry = {.count = 1, .reusable = true}}, SHIFT(54),
  [224] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [226] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_definition, 6, .production_id = 4),
  [228] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [230] = {.entry = {.count = 1, .reusable = true}}, SHIFT(56),
  [232] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_choice, 3, .production_id = 6),
  [234] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [236] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
  [238] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [240] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [242] = {.entry = {.count = 1, .reusable = true}}, SHIFT(55),
  [244] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [246] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [248] = {.entry = {.count = 1, .reusable = true}}, SHIFT(75),
  [250] = {.entry = {.count = 1, .reusable = true}}, SHIFT(76),
  [252] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [254] = {.entry = {.count = 1, .reusable = true}}, SHIFT(81),
  [256] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [258] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
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
