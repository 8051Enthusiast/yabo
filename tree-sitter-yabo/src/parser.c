#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 13
#define STATE_COUNT 104
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 80
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
  sym_char_literal = 76,
  aux_sym_source_file_repeat1 = 77,
  aux_sym_parser_sequence_repeat1 = 78,
  aux_sym_parserdef_ref_repeat1 = 79,
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
  [6] = {.lex_state = 40},
  [7] = {.lex_state = 1},
  [8] = {.lex_state = 1},
  [9] = {.lex_state = 1},
  [10] = {.lex_state = 1},
  [11] = {.lex_state = 1},
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
  [35] = {.lex_state = 2},
  [36] = {.lex_state = 5},
  [37] = {.lex_state = 5},
  [38] = {.lex_state = 2},
  [39] = {.lex_state = 2},
  [40] = {.lex_state = 2},
  [41] = {.lex_state = 5},
  [42] = {.lex_state = 2},
  [43] = {.lex_state = 2},
  [44] = {.lex_state = 2},
  [45] = {.lex_state = 2},
  [46] = {.lex_state = 5},
  [47] = {.lex_state = 2},
  [48] = {.lex_state = 2},
  [49] = {.lex_state = 2},
  [50] = {.lex_state = 2},
  [51] = {.lex_state = 5},
  [52] = {.lex_state = 2},
  [53] = {.lex_state = 2},
  [54] = {.lex_state = 2},
  [55] = {.lex_state = 2},
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
  [85] = {.lex_state = 0},
  [86] = {.lex_state = 1},
  [87] = {.lex_state = 0},
  [88] = {.lex_state = 1},
  [89] = {.lex_state = 1},
  [90] = {.lex_state = 1},
  [91] = {.lex_state = 1},
  [92] = {.lex_state = 1},
  [93] = {.lex_state = 0},
  [94] = {.lex_state = 0},
  [95] = {.lex_state = 0},
  [96] = {.lex_state = 3},
  [97] = {.lex_state = 0},
  [98] = {.lex_state = 0},
  [99] = {.lex_state = 0},
  [100] = {.lex_state = 3},
  [101] = {.lex_state = 38},
  [102] = {.lex_state = 0},
  [103] = {.lex_state = 0},
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
    [sym_source_file] = STATE(102),
    [sym__definition] = STATE(87),
    [sym_parser_definition] = STATE(87),
    [aux_sym_source_file_repeat1] = STATE(87),
    [ts_builtin_sym_end] = ACTIONS(3),
    [anon_sym_def] = ACTIONS(5),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 3,
    ACTIONS(11), 1,
      anon_sym_and,
    ACTIONS(9), 6,
      anon_sym_EQ,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
      anon_sym_AMP,
    ACTIONS(7), 23,
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
  [37] = 2,
    ACTIONS(15), 6,
      anon_sym_EQ,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
      anon_sym_AMP,
    ACTIONS(13), 24,
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
  [72] = 2,
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
  [142] = 4,
    ACTIONS(11), 1,
      anon_sym_and,
    ACTIONS(23), 1,
      anon_sym_or,
    ACTIONS(25), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(21), 22,
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
    STATE(95), 3,
      sym__parser_block_content,
      sym_parser_sequence,
      sym_parser_choice,
    STATE(9), 4,
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
    ACTIONS(47), 1,
      sym_number_literal,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
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
    STATE(28), 8,
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
    ACTIONS(49), 3,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_SEMI,
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
    STATE(93), 3,
      sym__parser_block_content,
      sym_parser_sequence,
      sym_parser_choice,
    STATE(9), 4,
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
  [390] = 12,
    ACTIONS(51), 1,
      anon_sym_LPAREN,
    ACTIONS(56), 1,
      anon_sym_LBRACE,
    ACTIONS(62), 1,
      anon_sym_let,
    ACTIONS(68), 1,
      anon_sym_if,
    ACTIONS(71), 1,
      sym_identifier,
    ACTIONS(74), 1,
      sym_number_literal,
    ACTIONS(77), 1,
      anon_sym_SQUOTE,
    ACTIONS(59), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(54), 3,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_SEMI,
    ACTIONS(65), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
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
  [442] = 13,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(90), 1,
      anon_sym_STAR,
    ACTIONS(96), 1,
      anon_sym_PIPE,
    ACTIONS(98), 1,
      anon_sym_CARET,
    ACTIONS(100), 1,
      anon_sym_AMP,
    ACTIONS(25), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(88), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(92), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(94), 2,
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
  [495] = 12,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(90), 1,
      anon_sym_STAR,
    ACTIONS(98), 1,
      anon_sym_CARET,
    ACTIONS(100), 1,
      anon_sym_AMP,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(88), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(92), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(94), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(25), 3,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
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
  [546] = 3,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(25), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(21), 21,
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
  [579] = 2,
    ACTIONS(25), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(21), 22,
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
  [610] = 5,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(25), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(21), 19,
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
  [647] = 2,
    ACTIONS(104), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(102), 22,
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
  [678] = 10,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(90), 1,
      anon_sym_STAR,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(88), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(92), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(94), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(25), 3,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(21), 11,
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
  [725] = 2,
    ACTIONS(108), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(106), 22,
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
  [756] = 6,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(25), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(21), 17,
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
  [795] = 11,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(90), 1,
      anon_sym_STAR,
    ACTIONS(100), 1,
      anon_sym_AMP,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(88), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(92), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(94), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(25), 3,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
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
  [844] = 2,
    ACTIONS(112), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(110), 22,
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
  [875] = 5,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(116), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(114), 19,
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
  [912] = 9,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(90), 1,
      anon_sym_STAR,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(88), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(92), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(25), 3,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(21), 13,
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
  [957] = 8,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(90), 1,
      anon_sym_STAR,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(92), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(25), 3,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(21), 15,
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
  [1000] = 2,
    ACTIONS(120), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(118), 22,
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
  [1031] = 3,
    ACTIONS(124), 1,
      anon_sym_COLON,
    ACTIONS(126), 4,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
      anon_sym_PIPE,
    ACTIONS(122), 19,
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
  [1062] = 15,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(90), 1,
      anon_sym_STAR,
    ACTIONS(96), 1,
      anon_sym_PIPE,
    ACTIONS(98), 1,
      anon_sym_CARET,
    ACTIONS(100), 1,
      anon_sym_AMP,
    ACTIONS(128), 1,
      anon_sym_RPAREN,
    ACTIONS(130), 1,
      anon_sym_COMMA,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(88), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(92), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(94), 2,
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
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(90), 1,
      anon_sym_STAR,
    ACTIONS(96), 1,
      anon_sym_PIPE,
    ACTIONS(98), 1,
      anon_sym_CARET,
    ACTIONS(100), 1,
      anon_sym_AMP,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(88), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(92), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(94), 2,
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
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(90), 1,
      anon_sym_STAR,
    ACTIONS(96), 1,
      anon_sym_PIPE,
    ACTIONS(98), 1,
      anon_sym_CARET,
    ACTIONS(100), 1,
      anon_sym_AMP,
    ACTIONS(130), 1,
      anon_sym_COMMA,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(88), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(92), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(94), 2,
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
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(90), 1,
      anon_sym_STAR,
    ACTIONS(96), 1,
      anon_sym_PIPE,
    ACTIONS(98), 1,
      anon_sym_CARET,
    ACTIONS(100), 1,
      anon_sym_AMP,
    ACTIONS(138), 1,
      anon_sym_COMMA,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(88), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(92), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(94), 2,
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
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(90), 1,
      anon_sym_STAR,
    ACTIONS(96), 1,
      anon_sym_PIPE,
    ACTIONS(98), 1,
      anon_sym_CARET,
    ACTIONS(100), 1,
      anon_sym_AMP,
    ACTIONS(128), 1,
      anon_sym_RPAREN,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(88), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(92), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(94), 2,
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
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(90), 1,
      anon_sym_STAR,
    ACTIONS(96), 1,
      anon_sym_PIPE,
    ACTIONS(98), 1,
      anon_sym_CARET,
    ACTIONS(100), 1,
      anon_sym_AMP,
    ACTIONS(140), 1,
      anon_sym_COMMA,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(88), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(92), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(94), 2,
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
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(90), 1,
      anon_sym_STAR,
    ACTIONS(96), 1,
      anon_sym_PIPE,
    ACTIONS(98), 1,
      anon_sym_CARET,
    ACTIONS(100), 1,
      anon_sym_AMP,
    ACTIONS(142), 1,
      anon_sym_RBRACK,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(88), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(92), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(94), 2,
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
    STATE(33), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1461] = 9,
    ACTIONS(150), 1,
      anon_sym_STAR_GT,
    ACTIONS(152), 1,
      anon_sym_LPAREN,
    ACTIONS(156), 1,
      anon_sym_RBRACK,
    ACTIONS(160), 1,
      sym_type_var,
    ACTIONS(162), 1,
      sym_identifier,
    STATE(41), 1,
      aux_sym_parserdef_ref_repeat1,
    ACTIONS(154), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(158), 4,
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
  [1499] = 9,
    ACTIONS(150), 1,
      anon_sym_STAR_GT,
    ACTIONS(152), 1,
      anon_sym_LPAREN,
    ACTIONS(160), 1,
      sym_type_var,
    ACTIONS(162), 1,
      sym_identifier,
    ACTIONS(164), 1,
      anon_sym_RBRACK,
    STATE(36), 1,
      aux_sym_parserdef_ref_repeat1,
    ACTIONS(154), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(158), 4,
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
  [1537] = 9,
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
  [1575] = 9,
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
    STATE(13), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1651] = 9,
    ACTIONS(178), 1,
      anon_sym_STAR_GT,
    ACTIONS(181), 1,
      anon_sym_LPAREN,
    ACTIONS(187), 1,
      anon_sym_RBRACK,
    ACTIONS(192), 1,
      sym_type_var,
    ACTIONS(195), 1,
      sym_identifier,
    STATE(41), 1,
      aux_sym_parserdef_ref_repeat1,
    ACTIONS(184), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(189), 4,
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
  [1689] = 9,
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
    STATE(29), 8,
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
    STATE(12), 8,
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
    STATE(24), 8,
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
    ACTIONS(210), 1,
      sym_identifier,
    ACTIONS(212), 1,
      sym_number_literal,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(20), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1841] = 9,
    ACTIONS(150), 1,
      anon_sym_STAR_GT,
    ACTIONS(152), 1,
      anon_sym_LPAREN,
    ACTIONS(160), 1,
      sym_type_var,
    ACTIONS(162), 1,
      sym_identifier,
    ACTIONS(214), 1,
      anon_sym_RBRACK,
    STATE(51), 1,
      aux_sym_parserdef_ref_repeat1,
    ACTIONS(154), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(158), 4,
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
  [1879] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(216), 1,
      sym_identifier,
    ACTIONS(218), 1,
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
  [1917] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(220), 1,
      sym_identifier,
    ACTIONS(222), 1,
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
  [1955] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(224), 1,
      sym_identifier,
    ACTIONS(226), 1,
      sym_number_literal,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(23), 8,
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
    ACTIONS(228), 1,
      sym_identifier,
    ACTIONS(230), 1,
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
  [2031] = 9,
    ACTIONS(150), 1,
      anon_sym_STAR_GT,
    ACTIONS(152), 1,
      anon_sym_LPAREN,
    ACTIONS(160), 1,
      sym_type_var,
    ACTIONS(162), 1,
      sym_identifier,
    ACTIONS(232), 1,
      anon_sym_RBRACK,
    STATE(41), 1,
      aux_sym_parserdef_ref_repeat1,
    ACTIONS(154), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(158), 4,
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
  [2069] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(234), 1,
      sym_identifier,
    ACTIONS(236), 1,
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
  [2107] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(238), 1,
      sym_identifier,
    ACTIONS(240), 1,
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
  [2145] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(242), 1,
      sym_identifier,
    ACTIONS(244), 1,
      sym_number_literal,
    ACTIONS(33), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(37), 3,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
    STATE(16), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2183] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(246), 1,
      sym_identifier,
    ACTIONS(248), 1,
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
  [2221] = 7,
    ACTIONS(150), 1,
      anon_sym_STAR_GT,
    ACTIONS(152), 1,
      anon_sym_LPAREN,
    ACTIONS(162), 1,
      sym_identifier,
    ACTIONS(250), 1,
      sym_type_var,
    ACTIONS(154), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(158), 4,
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
  [2253] = 7,
    ACTIONS(150), 1,
      anon_sym_STAR_GT,
    ACTIONS(152), 1,
      anon_sym_LPAREN,
    ACTIONS(162), 1,
      sym_identifier,
    ACTIONS(252), 1,
      sym_type_var,
    ACTIONS(154), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(158), 4,
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
  [2285] = 7,
    ACTIONS(150), 1,
      anon_sym_STAR_GT,
    ACTIONS(152), 1,
      anon_sym_LPAREN,
    ACTIONS(162), 1,
      sym_identifier,
    ACTIONS(254), 1,
      sym_type_var,
    ACTIONS(154), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(158), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(88), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2317] = 7,
    ACTIONS(150), 1,
      anon_sym_STAR_GT,
    ACTIONS(152), 1,
      anon_sym_LPAREN,
    ACTIONS(162), 1,
      sym_identifier,
    ACTIONS(256), 1,
      sym_type_var,
    ACTIONS(154), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(158), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(82), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2349] = 7,
    ACTIONS(150), 1,
      anon_sym_STAR_GT,
    ACTIONS(152), 1,
      anon_sym_LPAREN,
    ACTIONS(258), 1,
      sym_type_var,
    ACTIONS(260), 1,
      sym_identifier,
    ACTIONS(154), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(158), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(76), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2381] = 7,
    ACTIONS(150), 1,
      anon_sym_STAR_GT,
    ACTIONS(152), 1,
      anon_sym_LPAREN,
    ACTIONS(162), 1,
      sym_identifier,
    ACTIONS(262), 1,
      sym_type_var,
    ACTIONS(154), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(158), 4,
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
  [2413] = 7,
    ACTIONS(150), 1,
      anon_sym_STAR_GT,
    ACTIONS(152), 1,
      anon_sym_LPAREN,
    ACTIONS(162), 1,
      sym_identifier,
    ACTIONS(258), 1,
      sym_type_var,
    ACTIONS(154), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(158), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(76), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2445] = 2,
    ACTIONS(266), 5,
      anon_sym_for,
      anon_sym_each,
      anon_sym_let,
      anon_sym_if,
      sym_identifier,
    ACTIONS(264), 10,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_SEMI,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_number_literal,
      anon_sym_SQUOTE,
  [2465] = 2,
    ACTIONS(270), 5,
      anon_sym_for,
      anon_sym_each,
      anon_sym_let,
      anon_sym_if,
      sym_identifier,
    ACTIONS(268), 10,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_SEMI,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_number_literal,
      anon_sym_SQUOTE,
  [2485] = 2,
    ACTIONS(274), 5,
      anon_sym_for,
      anon_sym_each,
      anon_sym_let,
      anon_sym_if,
      sym_identifier,
    ACTIONS(272), 10,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_SEMI,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_number_literal,
      anon_sym_SQUOTE,
  [2505] = 2,
    ACTIONS(278), 5,
      anon_sym_for,
      anon_sym_each,
      anon_sym_let,
      anon_sym_if,
      sym_identifier,
    ACTIONS(276), 10,
      anon_sym_LPAREN,
      anon_sym_RPAREN,
      anon_sym_LBRACE,
      anon_sym_RBRACE,
      anon_sym_SEMI,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_number_literal,
      anon_sym_SQUOTE,
  [2525] = 2,
    ACTIONS(280), 4,
      anon_sym_STAR_GT,
      anon_sym_LPAREN,
      anon_sym_RBRACK,
      sym_type_var,
    ACTIONS(282), 7,
      anon_sym_for,
      anon_sym_each,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
      sym_identifier,
  [2541] = 4,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(284), 1,
      anon_sym_BANG,
    ACTIONS(286), 2,
      sym_identifier,
      sym_number_literal,
    STATE(2), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2560] = 4,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(284), 1,
      anon_sym_BANG,
    ACTIONS(288), 2,
      sym_identifier,
      sym_number_literal,
    STATE(4), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2579] = 4,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(284), 1,
      anon_sym_BANG,
    ACTIONS(290), 2,
      sym_identifier,
      sym_number_literal,
    STATE(72), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2598] = 4,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(284), 1,
      anon_sym_BANG,
    ACTIONS(292), 2,
      sym_identifier,
      sym_number_literal,
    STATE(6), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2617] = 3,
    ACTIONS(11), 1,
      anon_sym_and,
    ACTIONS(23), 1,
      anon_sym_or,
    ACTIONS(294), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2633] = 2,
    ACTIONS(298), 1,
      anon_sym_LBRACK,
    ACTIONS(296), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2646] = 2,
    ACTIONS(302), 1,
      anon_sym_LBRACK,
    ACTIONS(300), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2659] = 1,
    ACTIONS(304), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2669] = 2,
    ACTIONS(306), 1,
      anon_sym_TILDE,
    ACTIONS(294), 6,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2681] = 1,
    ACTIONS(308), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2691] = 1,
    ACTIONS(310), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2701] = 1,
    ACTIONS(312), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2711] = 1,
    ACTIONS(314), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2721] = 1,
    ACTIONS(316), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2731] = 2,
    ACTIONS(306), 1,
      anon_sym_TILDE,
    ACTIONS(318), 6,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2743] = 1,
    ACTIONS(320), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_AMP_GT,
  [2753] = 3,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(322), 2,
      sym_identifier,
      sym_number_literal,
    STATE(5), 3,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2766] = 3,
    ACTIONS(324), 1,
      ts_builtin_sym_end,
    ACTIONS(326), 1,
      anon_sym_def,
    STATE(85), 3,
      sym__definition,
      sym_parser_definition,
      aux_sym_source_file_repeat1,
  [2778] = 3,
    ACTIONS(302), 1,
      anon_sym_LBRACK,
    ACTIONS(329), 1,
      anon_sym_EQ,
    ACTIONS(300), 3,
      anon_sym_STAR_GT,
      anon_sym_TILDE,
      anon_sym_AMP_GT,
  [2790] = 3,
    ACTIONS(5), 1,
      anon_sym_def,
    ACTIONS(331), 1,
      ts_builtin_sym_end,
    STATE(85), 3,
      sym__definition,
      sym_parser_definition,
      aux_sym_source_file_repeat1,
  [2802] = 4,
    ACTIONS(306), 1,
      anon_sym_TILDE,
    ACTIONS(333), 1,
      anon_sym_STAR_GT,
    ACTIONS(335), 1,
      anon_sym_RPAREN,
    ACTIONS(337), 1,
      anon_sym_AMP_GT,
  [2815] = 4,
    ACTIONS(306), 1,
      anon_sym_TILDE,
    ACTIONS(333), 1,
      anon_sym_STAR_GT,
    ACTIONS(337), 1,
      anon_sym_AMP_GT,
    ACTIONS(339), 1,
      anon_sym_RBRACK,
  [2828] = 4,
    ACTIONS(306), 1,
      anon_sym_TILDE,
    ACTIONS(333), 1,
      anon_sym_STAR_GT,
    ACTIONS(337), 1,
      anon_sym_AMP_GT,
    ACTIONS(341), 1,
      anon_sym_COMMA,
  [2841] = 4,
    ACTIONS(306), 1,
      anon_sym_TILDE,
    ACTIONS(333), 1,
      anon_sym_STAR_GT,
    ACTIONS(337), 1,
      anon_sym_AMP_GT,
    ACTIONS(343), 1,
      anon_sym_EQ,
  [2854] = 3,
    ACTIONS(306), 1,
      anon_sym_TILDE,
    ACTIONS(337), 1,
      anon_sym_AMP_GT,
    ACTIONS(345), 1,
      anon_sym_STAR_GT,
  [2864] = 1,
    ACTIONS(347), 3,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_SEMI,
  [2870] = 2,
    ACTIONS(349), 1,
      anon_sym_RPAREN,
    ACTIONS(351), 1,
      anon_sym_SEMI,
  [2877] = 2,
    ACTIONS(351), 1,
      anon_sym_SEMI,
    ACTIONS(353), 1,
      anon_sym_RBRACE,
  [2884] = 1,
    ACTIONS(355), 1,
      sym_identifier,
  [2888] = 1,
    ACTIONS(357), 1,
      anon_sym_LBRACK,
  [2892] = 1,
    ACTIONS(359), 1,
      anon_sym_COLON,
  [2896] = 1,
    ACTIONS(361), 1,
      anon_sym_SQUOTE,
  [2900] = 1,
    ACTIONS(363), 1,
      sym_identifier,
  [2904] = 1,
    ACTIONS(365), 1,
      aux_sym_char_literal_token1,
  [2908] = 1,
    ACTIONS(367), 1,
      ts_builtin_sym_end,
  [2912] = 1,
    ACTIONS(369), 1,
      anon_sym_LBRACK,
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
  [SMALL_STATE(13)] = 495,
  [SMALL_STATE(14)] = 546,
  [SMALL_STATE(15)] = 579,
  [SMALL_STATE(16)] = 610,
  [SMALL_STATE(17)] = 647,
  [SMALL_STATE(18)] = 678,
  [SMALL_STATE(19)] = 725,
  [SMALL_STATE(20)] = 756,
  [SMALL_STATE(21)] = 795,
  [SMALL_STATE(22)] = 844,
  [SMALL_STATE(23)] = 875,
  [SMALL_STATE(24)] = 912,
  [SMALL_STATE(25)] = 957,
  [SMALL_STATE(26)] = 1000,
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
  [SMALL_STATE(52)] = 2069,
  [SMALL_STATE(53)] = 2107,
  [SMALL_STATE(54)] = 2145,
  [SMALL_STATE(55)] = 2183,
  [SMALL_STATE(56)] = 2221,
  [SMALL_STATE(57)] = 2253,
  [SMALL_STATE(58)] = 2285,
  [SMALL_STATE(59)] = 2317,
  [SMALL_STATE(60)] = 2349,
  [SMALL_STATE(61)] = 2381,
  [SMALL_STATE(62)] = 2413,
  [SMALL_STATE(63)] = 2445,
  [SMALL_STATE(64)] = 2465,
  [SMALL_STATE(65)] = 2485,
  [SMALL_STATE(66)] = 2505,
  [SMALL_STATE(67)] = 2525,
  [SMALL_STATE(68)] = 2541,
  [SMALL_STATE(69)] = 2560,
  [SMALL_STATE(70)] = 2579,
  [SMALL_STATE(71)] = 2598,
  [SMALL_STATE(72)] = 2617,
  [SMALL_STATE(73)] = 2633,
  [SMALL_STATE(74)] = 2646,
  [SMALL_STATE(75)] = 2659,
  [SMALL_STATE(76)] = 2669,
  [SMALL_STATE(77)] = 2681,
  [SMALL_STATE(78)] = 2691,
  [SMALL_STATE(79)] = 2701,
  [SMALL_STATE(80)] = 2711,
  [SMALL_STATE(81)] = 2721,
  [SMALL_STATE(82)] = 2731,
  [SMALL_STATE(83)] = 2743,
  [SMALL_STATE(84)] = 2753,
  [SMALL_STATE(85)] = 2766,
  [SMALL_STATE(86)] = 2778,
  [SMALL_STATE(87)] = 2790,
  [SMALL_STATE(88)] = 2802,
  [SMALL_STATE(89)] = 2815,
  [SMALL_STATE(90)] = 2828,
  [SMALL_STATE(91)] = 2841,
  [SMALL_STATE(92)] = 2854,
  [SMALL_STATE(93)] = 2864,
  [SMALL_STATE(94)] = 2870,
  [SMALL_STATE(95)] = 2877,
  [SMALL_STATE(96)] = 2884,
  [SMALL_STATE(97)] = 2888,
  [SMALL_STATE(98)] = 2892,
  [SMALL_STATE(99)] = 2896,
  [SMALL_STATE(100)] = 2900,
  [SMALL_STATE(101)] = 2904,
  [SMALL_STATE(102)] = 2908,
  [SMALL_STATE(103)] = 2912,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(61),
  [7] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_constraint_expression, 3, .production_id = 3),
  [9] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_binary_constraint_expression, 3, .production_id = 3),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(69),
  [13] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_char_literal, 3),
  [15] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_char_literal, 3),
  [17] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_constraint_expression, 2),
  [19] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unary_constraint_expression, 2),
  [21] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_expression, 3, .production_id = 3),
  [23] = {.entry = {.count = 1, .reusable = true}}, SHIFT(68),
  [25] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_binary_expression, 3, .production_id = 3),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [31] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [33] = {.entry = {.count = 1, .reusable = false}}, SHIFT(103),
  [35] = {.entry = {.count = 1, .reusable = false}}, SHIFT(100),
  [37] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [39] = {.entry = {.count = 1, .reusable = false}}, SHIFT(49),
  [41] = {.entry = {.count = 1, .reusable = false}}, SHIFT(27),
  [43] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [45] = {.entry = {.count = 1, .reusable = true}}, SHIFT(101),
  [47] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [49] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_sequence, 1),
  [51] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(8),
  [54] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2),
  [56] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(7),
  [59] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(103),
  [62] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(100),
  [65] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(49),
  [68] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(49),
  [71] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(27),
  [74] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(30),
  [77] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(101),
  [80] = {.entry = {.count = 1, .reusable = true}}, SHIFT(54),
  [82] = {.entry = {.count = 1, .reusable = true}}, SHIFT(71),
  [84] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [86] = {.entry = {.count = 1, .reusable = true}}, SHIFT(52),
  [88] = {.entry = {.count = 1, .reusable = true}}, SHIFT(50),
  [90] = {.entry = {.count = 1, .reusable = false}}, SHIFT(45),
  [92] = {.entry = {.count = 1, .reusable = true}}, SHIFT(45),
  [94] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
  [96] = {.entry = {.count = 1, .reusable = false}}, SHIFT(40),
  [98] = {.entry = {.count = 1, .reusable = true}}, SHIFT(39),
  [100] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [102] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_array, 4, .production_id = 5),
  [104] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parser_array, 4, .production_id = 5),
  [106] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_block, 3, .production_id = 12),
  [108] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parser_block, 3, .production_id = 12),
  [110] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__expression, 3),
  [112] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__expression, 3),
  [114] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_expression, 2, .production_id = 2),
  [116] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unary_expression, 2, .production_id = 2),
  [118] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_block, 2),
  [120] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parser_block, 2),
  [122] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__atom, 1),
  [124] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [126] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__atom, 1),
  [128] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [130] = {.entry = {.count = 1, .reusable = true}}, SHIFT(66),
  [132] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [134] = {.entry = {.count = 1, .reusable = false}}, SHIFT(43),
  [136] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_definition, 6, .production_id = 9),
  [138] = {.entry = {.count = 1, .reusable = true}}, SHIFT(63),
  [140] = {.entry = {.count = 1, .reusable = true}}, SHIFT(65),
  [142] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [144] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [146] = {.entry = {.count = 1, .reusable = false}}, SHIFT(33),
  [148] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [150] = {.entry = {.count = 1, .reusable = true}}, SHIFT(59),
  [152] = {.entry = {.count = 1, .reusable = true}}, SHIFT(58),
  [154] = {.entry = {.count = 1, .reusable = false}}, SHIFT(97),
  [156] = {.entry = {.count = 1, .reusable = true}}, SHIFT(75),
  [158] = {.entry = {.count = 1, .reusable = false}}, SHIFT(83),
  [160] = {.entry = {.count = 1, .reusable = true}}, SHIFT(90),
  [162] = {.entry = {.count = 1, .reusable = false}}, SHIFT(74),
  [164] = {.entry = {.count = 1, .reusable = true}}, SHIFT(79),
  [166] = {.entry = {.count = 1, .reusable = false}}, SHIFT(18),
  [168] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [170] = {.entry = {.count = 1, .reusable = false}}, SHIFT(21),
  [172] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [174] = {.entry = {.count = 1, .reusable = false}}, SHIFT(13),
  [176] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [178] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(59),
  [181] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(58),
  [184] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(97),
  [187] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8),
  [189] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(83),
  [192] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(90),
  [195] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(74),
  [198] = {.entry = {.count = 1, .reusable = false}}, SHIFT(29),
  [200] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [202] = {.entry = {.count = 1, .reusable = false}}, SHIFT(12),
  [204] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [206] = {.entry = {.count = 1, .reusable = false}}, SHIFT(24),
  [208] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [210] = {.entry = {.count = 1, .reusable = false}}, SHIFT(20),
  [212] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [214] = {.entry = {.count = 1, .reusable = true}}, SHIFT(81),
  [216] = {.entry = {.count = 1, .reusable = false}}, SHIFT(32),
  [218] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [220] = {.entry = {.count = 1, .reusable = false}}, SHIFT(31),
  [222] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [224] = {.entry = {.count = 1, .reusable = false}}, SHIFT(23),
  [226] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [228] = {.entry = {.count = 1, .reusable = false}}, SHIFT(25),
  [230] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [232] = {.entry = {.count = 1, .reusable = true}}, SHIFT(78),
  [234] = {.entry = {.count = 1, .reusable = false}}, SHIFT(14),
  [236] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [238] = {.entry = {.count = 1, .reusable = false}}, SHIFT(15),
  [240] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [242] = {.entry = {.count = 1, .reusable = false}}, SHIFT(16),
  [244] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [246] = {.entry = {.count = 1, .reusable = false}}, SHIFT(34),
  [248] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [250] = {.entry = {.count = 1, .reusable = true}}, SHIFT(91),
  [252] = {.entry = {.count = 1, .reusable = true}}, SHIFT(89),
  [254] = {.entry = {.count = 1, .reusable = true}}, SHIFT(88),
  [256] = {.entry = {.count = 1, .reusable = true}}, SHIFT(82),
  [258] = {.entry = {.count = 1, .reusable = true}}, SHIFT(76),
  [260] = {.entry = {.count = 1, .reusable = false}}, SHIFT(86),
  [262] = {.entry = {.count = 1, .reusable = true}}, SHIFT(92),
  [264] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_let_statement, 7, .production_id = 15),
  [266] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_let_statement, 7, .production_id = 15),
  [268] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 3),
  [270] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 3),
  [272] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parse_statement, 4, .production_id = 14),
  [274] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parse_statement, 4, .production_id = 14),
  [276] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parse_statement, 2, .production_id = 11),
  [278] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parse_statement, 2, .production_id = 11),
  [280] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 6),
  [282] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 6),
  [284] = {.entry = {.count = 1, .reusable = true}}, SHIFT(84),
  [286] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [288] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [290] = {.entry = {.count = 1, .reusable = true}}, SHIFT(72),
  [292] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [294] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_type_expression, 3, .production_id = 3),
  [296] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 3, .production_id = 4),
  [298] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [300] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 1, .production_id = 1),
  [302] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
  [304] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 4, .production_id = 7),
  [306] = {.entry = {.count = 1, .reusable = true}}, SHIFT(70),
  [308] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type_array, 4, .production_id = 5),
  [310] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 6, .production_id = 10),
  [312] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 3, .production_id = 1),
  [314] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__type_expression, 3),
  [316] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 5, .production_id = 4),
  [318] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_type_expression, 2, .production_id = 2),
  [320] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_primitive_type, 1),
  [322] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [324] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [326] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(61),
  [329] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
  [331] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [333] = {.entry = {.count = 1, .reusable = true}}, SHIFT(62),
  [335] = {.entry = {.count = 1, .reusable = true}}, SHIFT(80),
  [337] = {.entry = {.count = 1, .reusable = true}}, SHIFT(96),
  [339] = {.entry = {.count = 1, .reusable = true}}, SHIFT(77),
  [341] = {.entry = {.count = 1, .reusable = true}}, SHIFT(67),
  [343] = {.entry = {.count = 1, .reusable = true}}, SHIFT(48),
  [345] = {.entry = {.count = 1, .reusable = true}}, SHIFT(60),
  [347] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_choice, 3, .production_id = 13),
  [349] = {.entry = {.count = 1, .reusable = true}}, SHIFT(64),
  [351] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [353] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [355] = {.entry = {.count = 1, .reusable = true}}, SHIFT(73),
  [357] = {.entry = {.count = 1, .reusable = true}}, SHIFT(57),
  [359] = {.entry = {.count = 1, .reusable = true}}, SHIFT(56),
  [361] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [363] = {.entry = {.count = 1, .reusable = true}}, SHIFT(98),
  [365] = {.entry = {.count = 1, .reusable = true}}, SHIFT(99),
  [367] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [369] = {.entry = {.count = 1, .reusable = true}}, SHIFT(55),
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
