#include <tree_sitter/parser.h>

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 13
#define STATE_COUNT 112
#define LARGE_STATE_COUNT 2
#define SYMBOL_COUNT 78
#define ALIAS_COUNT 0
#define TOKEN_COUNT 48
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
  anon_sym_PIPE = 9,
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
  anon_sym_CARET = 36,
  anon_sym_AMP = 37,
  anon_sym_if = 38,
  anon_sym_int = 39,
  anon_sym_bit = 40,
  anon_sym_char = 41,
  anon_sym_mem = 42,
  sym_type_var = 43,
  sym_identifier = 44,
  sym_number_literal = 45,
  anon_sym_SQUOTE = 46,
  aux_sym_char_literal_token1 = 47,
  sym_source_file = 48,
  sym__definition = 49,
  sym_parser_definition = 50,
  sym__type_expression = 51,
  sym_binary_type_expression = 52,
  sym_unary_type_expression = 53,
  sym__expression = 54,
  sym_parser_block = 55,
  sym__parser_block_content = 56,
  sym_parser_sequence = 57,
  sym_parser_choice = 58,
  sym_type_array = 59,
  sym_parser_array = 60,
  sym__statement = 61,
  sym_parse_statement = 62,
  sym_let_statement = 63,
  sym__constraint_expression = 64,
  sym_binary_constraint_expression = 65,
  sym_unary_constraint_expression = 66,
  sym_binary_expression = 67,
  sym_unary_expression = 68,
  sym__type_atom = 69,
  sym_parserdef_ref = 70,
  sym__atom = 71,
  sym__literal = 72,
  sym_primitive_type = 73,
  sym_char_literal = 74,
  aux_sym_source_file_repeat1 = 75,
  aux_sym_parser_sequence_repeat1 = 76,
  aux_sym_parserdef_ref_repeat1 = 77,
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
  [anon_sym_PIPE] = "|",
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
  [anon_sym_CARET] = "^",
  [anon_sym_AMP] = "&",
  [anon_sym_if] = "if",
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
  [anon_sym_PIPE] = anon_sym_PIPE,
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
  [anon_sym_CARET] = anon_sym_CARET,
  [anon_sym_AMP] = anon_sym_AMP,
  [anon_sym_if] = anon_sym_if,
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
  [3] = {.index = 3, .length = 2},
  [4] = {.index = 5, .length = 3},
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
    {field_from, 0},
    {field_name, 1},
  [5] =
    {field_left, 0},
    {field_op, 1},
    {field_right, 2},
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
    {field_args, 3, .inherited = true},
    {field_from, 0},
    {field_name, 1},
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
      if (eof) ADVANCE(39);
      if (lookahead == '!') ADVANCE(66);
      if (lookahead == '%') ADVANCE(74);
      if (lookahead == '&') ADVANCE(84);
      if (lookahead == '\'') ADVANCE(122);
      if (lookahead == '(') ADVANCE(44);
      if (lookahead == ')') ADVANCE(45);
      if (lookahead == '*') ADVANCE(72);
      if (lookahead == '+') ADVANCE(70);
      if (lookahead == ',') ADVANCE(58);
      if (lookahead == '-') ADVANCE(71);
      if (lookahead == '.') ADVANCE(67);
      if (lookahead == '/') ADVANCE(73);
      if (lookahead == '0') ADVANCE(117);
      if (lookahead == ':') ADVANCE(57);
      if (lookahead == '<') ADVANCE(82);
      if (lookahead == '=') ADVANCE(43);
      if (lookahead == '>') ADVANCE(79);
      if (lookahead == '[') ADVANCE(55);
      if (lookahead == ']') ADVANCE(56);
      if (lookahead == '^') ADVANCE(83);
      if (lookahead == 'a') ADVANCE(24);
      if (lookahead == 'b') ADVANCE(21);
      if (lookahead == 'c') ADVANCE(19);
      if (lookahead == 'd') ADVANCE(13);
      if (lookahead == 'e') ADVANCE(9);
      if (lookahead == 'f') ADVANCE(25);
      if (lookahead == 'i') ADVANCE(17);
      if (lookahead == 'l') ADVANCE(16);
      if (lookahead == 'm') ADVANCE(14);
      if (lookahead == 'o') ADVANCE(26);
      if (lookahead == '{') ADVANCE(47);
      if (lookahead == '|') ADVANCE(50);
      if (lookahead == '}') ADVANCE(48);
      if (lookahead == '~') ADVANCE(46);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0)
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(120);
      END_STATE();
    case 1:
      if (lookahead == '!') ADVANCE(65);
      if (lookahead == '\'') ADVANCE(122);
      if (lookahead == '(') ADVANCE(44);
      if (lookahead == ')') ADVANCE(45);
      if (lookahead == '+') ADVANCE(70);
      if (lookahead == '-') ADVANCE(71);
      if (lookahead == '0') ADVANCE(117);
      if (lookahead == 'e') ADVANCE(96);
      if (lookahead == 'f') ADVANCE(109);
      if (lookahead == 'i') ADVANCE(102);
      if (lookahead == 'l') ADVANCE(100);
      if (lookahead == '{') ADVANCE(47);
      if (lookahead == '|') ADVANCE(49);
      if (lookahead == '}') ADVANCE(48);
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
      if (lookahead == '!') ADVANCE(65);
      if (lookahead == '\'') ADVANCE(122);
      if (lookahead == '(') ADVANCE(44);
      if (lookahead == '+') ADVANCE(70);
      if (lookahead == '-') ADVANCE(71);
      if (lookahead == '0') ADVANCE(117);
      if (lookahead == 'e') ADVANCE(96);
      if (lookahead == 'f') ADVANCE(109);
      if (lookahead == 'i') ADVANCE(102);
      if (lookahead == '{') ADVANCE(47);
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
      if (lookahead == '!') ADVANCE(65);
      if (lookahead == '\'') ADVANCE(122);
      if (lookahead == ')') ADVANCE(45);
      if (lookahead == '*') ADVANCE(8);
      if (lookahead == ',') ADVANCE(58);
      if (lookahead == '0') ADVANCE(117);
      if (lookahead == '=') ADVANCE(42);
      if (lookahead == '[') ADVANCE(55);
      if (lookahead == ']') ADVANCE(56);
      if (lookahead == '~') ADVANCE(46);
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
      if (lookahead == '\'') ADVANCE(36);
      if (lookahead == '(') ADVANCE(44);
      if (lookahead == '*') ADVANCE(8);
      if (lookahead == ']') ADVANCE(56);
      if (lookahead == 'b') ADVANCE(105);
      if (lookahead == 'c') ADVANCE(104);
      if (lookahead == 'e') ADVANCE(96);
      if (lookahead == 'f') ADVANCE(109);
      if (lookahead == 'i') ADVANCE(108);
      if (lookahead == 'm') ADVANCE(101);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(4)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 5:
      if (lookahead == ')') ADVANCE(45);
      if (lookahead == '*') ADVANCE(8);
      if (lookahead == ',') ADVANCE(58);
      if (lookahead == '=') ADVANCE(42);
      if (lookahead == ']') ADVANCE(56);
      if (lookahead == 'a') ADVANCE(107);
      if (lookahead == 'o') ADVANCE(112);
      if (lookahead == '~') ADVANCE(46);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(5)
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 6:
      if (lookahead == '=') ADVANCE(78);
      END_STATE();
    case 7:
      if (lookahead == '=') ADVANCE(77);
      END_STATE();
    case 8:
      if (lookahead == '>') ADVANCE(41);
      END_STATE();
    case 9:
      if (lookahead == 'a') ADVANCE(11);
      if (lookahead == 'l') ADVANCE(29);
      END_STATE();
    case 10:
      if (lookahead == 'a') ADVANCE(28);
      END_STATE();
    case 11:
      if (lookahead == 'c') ADVANCE(20);
      END_STATE();
    case 12:
      if (lookahead == 'd') ADVANCE(63);
      END_STATE();
    case 13:
      if (lookahead == 'e') ADVANCE(18);
      END_STATE();
    case 14:
      if (lookahead == 'e') ADVANCE(23);
      END_STATE();
    case 15:
      if (lookahead == 'e') ADVANCE(68);
      END_STATE();
    case 16:
      if (lookahead == 'e') ADVANCE(32);
      END_STATE();
    case 17:
      if (lookahead == 'f') ADVANCE(85);
      if (lookahead == 'n') ADVANCE(31);
      END_STATE();
    case 18:
      if (lookahead == 'f') ADVANCE(40);
      END_STATE();
    case 19:
      if (lookahead == 'h') ADVANCE(10);
      END_STATE();
    case 20:
      if (lookahead == 'h') ADVANCE(53);
      END_STATE();
    case 21:
      if (lookahead == 'i') ADVANCE(30);
      END_STATE();
    case 22:
      if (lookahead == 'l') ADVANCE(29);
      END_STATE();
    case 23:
      if (lookahead == 'm') ADVANCE(93);
      END_STATE();
    case 24:
      if (lookahead == 'n') ADVANCE(12);
      END_STATE();
    case 25:
      if (lookahead == 'o') ADVANCE(27);
      END_STATE();
    case 26:
      if (lookahead == 'r') ADVANCE(61);
      END_STATE();
    case 27:
      if (lookahead == 'r') ADVANCE(51);
      END_STATE();
    case 28:
      if (lookahead == 'r') ADVANCE(91);
      END_STATE();
    case 29:
      if (lookahead == 's') ADVANCE(15);
      END_STATE();
    case 30:
      if (lookahead == 't') ADVANCE(89);
      END_STATE();
    case 31:
      if (lookahead == 't') ADVANCE(87);
      END_STATE();
    case 32:
      if (lookahead == 't') ADVANCE(59);
      END_STATE();
    case 33:
      if (lookahead == '0' ||
          lookahead == '1') ADVANCE(118);
      END_STATE();
    case 34:
      if (('0' <= lookahead && lookahead <= '7')) ADVANCE(119);
      END_STATE();
    case 35:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(121);
      END_STATE();
    case 36:
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(95);
      END_STATE();
    case 37:
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '\'') ADVANCE(123);
      END_STATE();
    case 38:
      if (eof) ADVANCE(39);
      if (lookahead == '!') ADVANCE(6);
      if (lookahead == '%') ADVANCE(74);
      if (lookahead == '&') ADVANCE(84);
      if (lookahead == ')') ADVANCE(45);
      if (lookahead == '*') ADVANCE(72);
      if (lookahead == '+') ADVANCE(70);
      if (lookahead == ',') ADVANCE(58);
      if (lookahead == '-') ADVANCE(71);
      if (lookahead == '.') ADVANCE(67);
      if (lookahead == '/') ADVANCE(73);
      if (lookahead == ':') ADVANCE(57);
      if (lookahead == '<') ADVANCE(82);
      if (lookahead == '=') ADVANCE(7);
      if (lookahead == '>') ADVANCE(79);
      if (lookahead == ']') ADVANCE(56);
      if (lookahead == '^') ADVANCE(83);
      if (lookahead == 'a') ADVANCE(24);
      if (lookahead == 'd') ADVANCE(13);
      if (lookahead == 'e') ADVANCE(22);
      if (lookahead == 'o') ADVANCE(26);
      if (lookahead == '|') ADVANCE(50);
      if (lookahead == '~') ADVANCE(46);
      if (lookahead == '\t' ||
          lookahead == '\n' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(38)
      END_STATE();
    case 39:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(anon_sym_def);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(anon_sym_STAR_GT);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(anon_sym_EQ);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(anon_sym_EQ);
      if (lookahead == '=') ADVANCE(77);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(anon_sym_TILDE);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(anon_sym_LBRACE);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(anon_sym_RBRACE);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(anon_sym_PIPE);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(anon_sym_PIPE);
      if (lookahead == '>') ADVANCE(69);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(anon_sym_for);
      END_STATE();
    case 52:
      ACCEPT_TOKEN(anon_sym_for);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 53:
      ACCEPT_TOKEN(anon_sym_each);
      END_STATE();
    case 54:
      ACCEPT_TOKEN(anon_sym_each);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
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
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 59:
      ACCEPT_TOKEN(anon_sym_let);
      END_STATE();
    case 60:
      ACCEPT_TOKEN(anon_sym_let);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 61:
      ACCEPT_TOKEN(anon_sym_or);
      END_STATE();
    case 62:
      ACCEPT_TOKEN(anon_sym_or);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 63:
      ACCEPT_TOKEN(anon_sym_and);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(anon_sym_and);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(anon_sym_BANG);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(anon_sym_BANG);
      if (lookahead == '=') ADVANCE(78);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(anon_sym_DOT);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(anon_sym_else);
      END_STATE();
    case 69:
      ACCEPT_TOKEN(anon_sym_PIPE_GT);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(anon_sym_PLUS);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(anon_sym_DASH);
      END_STATE();
    case 72:
      ACCEPT_TOKEN(anon_sym_STAR);
      if (lookahead == '>') ADVANCE(41);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(anon_sym_SLASH);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(anon_sym_PERCENT);
      END_STATE();
    case 75:
      ACCEPT_TOKEN(anon_sym_LT_LT);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(anon_sym_GT_GT);
      END_STATE();
    case 77:
      ACCEPT_TOKEN(anon_sym_EQ_EQ);
      END_STATE();
    case 78:
      ACCEPT_TOKEN(anon_sym_BANG_EQ);
      END_STATE();
    case 79:
      ACCEPT_TOKEN(anon_sym_GT);
      if (lookahead == '=') ADVANCE(80);
      if (lookahead == '>') ADVANCE(76);
      END_STATE();
    case 80:
      ACCEPT_TOKEN(anon_sym_GT_EQ);
      END_STATE();
    case 81:
      ACCEPT_TOKEN(anon_sym_LT_EQ);
      END_STATE();
    case 82:
      ACCEPT_TOKEN(anon_sym_LT);
      if (lookahead == '<') ADVANCE(75);
      if (lookahead == '=') ADVANCE(81);
      END_STATE();
    case 83:
      ACCEPT_TOKEN(anon_sym_CARET);
      END_STATE();
    case 84:
      ACCEPT_TOKEN(anon_sym_AMP);
      END_STATE();
    case 85:
      ACCEPT_TOKEN(anon_sym_if);
      END_STATE();
    case 86:
      ACCEPT_TOKEN(anon_sym_if);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 87:
      ACCEPT_TOKEN(anon_sym_int);
      END_STATE();
    case 88:
      ACCEPT_TOKEN(anon_sym_int);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 89:
      ACCEPT_TOKEN(anon_sym_bit);
      END_STATE();
    case 90:
      ACCEPT_TOKEN(anon_sym_bit);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 91:
      ACCEPT_TOKEN(anon_sym_char);
      END_STATE();
    case 92:
      ACCEPT_TOKEN(anon_sym_char);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 93:
      ACCEPT_TOKEN(anon_sym_mem);
      END_STATE();
    case 94:
      ACCEPT_TOKEN(anon_sym_mem);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 95:
      ACCEPT_TOKEN(sym_type_var);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(95);
      END_STATE();
    case 96:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(98);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 97:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(111);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 98:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'c') ADVANCE(103);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 99:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'd') ADVANCE(64);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 100:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(113);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 101:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(106);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 102:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'f') ADVANCE(86);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 103:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'h') ADVANCE(54);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 104:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'h') ADVANCE(97);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 105:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'i') ADVANCE(114);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 106:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'm') ADVANCE(94);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 107:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'n') ADVANCE(99);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 108:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'n') ADVANCE(115);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 109:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(110);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 110:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(52);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 111:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(92);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 112:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(62);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 113:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(60);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 114:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(90);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(116);
      END_STATE();
    case 115:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(88);
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
      if (lookahead == 'b') ADVANCE(33);
      if (lookahead == 'o') ADVANCE(34);
      if (lookahead == 'x') ADVANCE(35);
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
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0},
  [1] = {.lex_state = 0},
  [2] = {.lex_state = 38},
  [3] = {.lex_state = 38},
  [4] = {.lex_state = 38},
  [5] = {.lex_state = 38},
  [6] = {.lex_state = 38},
  [7] = {.lex_state = 1},
  [8] = {.lex_state = 1},
  [9] = {.lex_state = 1},
  [10] = {.lex_state = 1},
  [11] = {.lex_state = 1},
  [12] = {.lex_state = 38},
  [13] = {.lex_state = 38},
  [14] = {.lex_state = 38},
  [15] = {.lex_state = 38},
  [16] = {.lex_state = 38},
  [17] = {.lex_state = 38},
  [18] = {.lex_state = 38},
  [19] = {.lex_state = 38},
  [20] = {.lex_state = 38},
  [21] = {.lex_state = 38},
  [22] = {.lex_state = 38},
  [23] = {.lex_state = 38},
  [24] = {.lex_state = 38},
  [25] = {.lex_state = 38},
  [26] = {.lex_state = 38},
  [27] = {.lex_state = 38},
  [28] = {.lex_state = 38},
  [29] = {.lex_state = 38},
  [30] = {.lex_state = 38},
  [31] = {.lex_state = 38},
  [32] = {.lex_state = 38},
  [33] = {.lex_state = 38},
  [34] = {.lex_state = 38},
  [35] = {.lex_state = 2},
  [36] = {.lex_state = 2},
  [37] = {.lex_state = 2},
  [38] = {.lex_state = 2},
  [39] = {.lex_state = 4},
  [40] = {.lex_state = 2},
  [41] = {.lex_state = 2},
  [42] = {.lex_state = 2},
  [43] = {.lex_state = 4},
  [44] = {.lex_state = 2},
  [45] = {.lex_state = 4},
  [46] = {.lex_state = 2},
  [47] = {.lex_state = 2},
  [48] = {.lex_state = 2},
  [49] = {.lex_state = 2},
  [50] = {.lex_state = 2},
  [51] = {.lex_state = 4},
  [52] = {.lex_state = 4},
  [53] = {.lex_state = 2},
  [54] = {.lex_state = 2},
  [55] = {.lex_state = 2},
  [56] = {.lex_state = 4},
  [57] = {.lex_state = 4},
  [58] = {.lex_state = 4},
  [59] = {.lex_state = 4},
  [60] = {.lex_state = 4},
  [61] = {.lex_state = 4},
  [62] = {.lex_state = 4},
  [63] = {.lex_state = 1},
  [64] = {.lex_state = 1},
  [65] = {.lex_state = 1},
  [66] = {.lex_state = 1},
  [67] = {.lex_state = 4},
  [68] = {.lex_state = 3},
  [69] = {.lex_state = 3},
  [70] = {.lex_state = 3},
  [71] = {.lex_state = 3},
  [72] = {.lex_state = 3},
  [73] = {.lex_state = 3},
  [74] = {.lex_state = 5},
  [75] = {.lex_state = 5},
  [76] = {.lex_state = 5},
  [77] = {.lex_state = 5},
  [78] = {.lex_state = 5},
  [79] = {.lex_state = 3},
  [80] = {.lex_state = 3},
  [81] = {.lex_state = 3},
  [82] = {.lex_state = 3},
  [83] = {.lex_state = 3},
  [84] = {.lex_state = 3},
  [85] = {.lex_state = 3},
  [86] = {.lex_state = 3},
  [87] = {.lex_state = 3},
  [88] = {.lex_state = 3},
  [89] = {.lex_state = 3},
  [90] = {.lex_state = 3},
  [91] = {.lex_state = 3},
  [92] = {.lex_state = 0},
  [93] = {.lex_state = 0},
  [94] = {.lex_state = 3},
  [95] = {.lex_state = 3},
  [96] = {.lex_state = 3},
  [97] = {.lex_state = 3},
  [98] = {.lex_state = 3},
  [99] = {.lex_state = 3},
  [100] = {.lex_state = 0},
  [101] = {.lex_state = 0},
  [102] = {.lex_state = 0},
  [103] = {.lex_state = 0},
  [104] = {.lex_state = 0},
  [105] = {.lex_state = 0},
  [106] = {.lex_state = 3},
  [107] = {.lex_state = 0},
  [108] = {.lex_state = 0},
  [109] = {.lex_state = 37},
  [110] = {.lex_state = 0},
  [111] = {.lex_state = 37},
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
    [anon_sym_PIPE] = ACTIONS(1),
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
    [anon_sym_CARET] = ACTIONS(1),
    [anon_sym_AMP] = ACTIONS(1),
    [anon_sym_if] = ACTIONS(1),
    [anon_sym_int] = ACTIONS(1),
    [anon_sym_bit] = ACTIONS(1),
    [anon_sym_char] = ACTIONS(1),
    [anon_sym_mem] = ACTIONS(1),
    [sym_number_literal] = ACTIONS(1),
    [anon_sym_SQUOTE] = ACTIONS(1),
  },
  [1] = {
    [sym_source_file] = STATE(108),
    [sym__definition] = STATE(92),
    [sym_parser_definition] = STATE(92),
    [aux_sym_source_file_repeat1] = STATE(92),
    [ts_builtin_sym_end] = ACTIONS(3),
    [anon_sym_def] = ACTIONS(5),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 4,
    ACTIONS(11), 1,
      anon_sym_or,
    ACTIONS(13), 1,
      anon_sym_and,
    ACTIONS(9), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(7), 22,
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
  [37] = 2,
    ACTIONS(17), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(15), 24,
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
      anon_sym_AMP,
  [70] = 3,
    ACTIONS(13), 1,
      anon_sym_and,
    ACTIONS(17), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(15), 23,
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
      anon_sym_AMP,
  [105] = 2,
    ACTIONS(21), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(19), 24,
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
      anon_sym_AMP,
  [138] = 2,
    ACTIONS(25), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(23), 24,
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
      anon_sym_AMP,
  [171] = 13,
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
    STATE(101), 3,
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
  [226] = 12,
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
    STATE(102), 3,
      sym__parser_block_content,
      sym_parser_sequence,
      sym_parser_choice,
    STATE(9), 4,
      sym__statement,
      sym_parse_statement,
      sym_let_statement,
      aux_sym_parser_sequence_repeat1,
    STATE(29), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [278] = 12,
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
      anon_sym_PIPE,
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
  [330] = 12,
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
    STATE(100), 3,
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
  [382] = 12,
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
      anon_sym_PIPE,
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
  [434] = 2,
    ACTIONS(9), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(7), 22,
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
  [465] = 8,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_STAR,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(9), 3,
      anon_sym_PIPE,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(7), 15,
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
  [508] = 3,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(9), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(7), 21,
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
  [541] = 12,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_STAR,
    ACTIONS(96), 1,
      anon_sym_CARET,
    ACTIONS(98), 1,
      anon_sym_AMP,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(92), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(9), 3,
      anon_sym_PIPE,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(7), 9,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [592] = 5,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(9), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(7), 19,
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
  [629] = 2,
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
  [660] = 6,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(9), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(7), 17,
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
  [699] = 2,
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
  [730] = 2,
    ACTIONS(110), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(108), 22,
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
  [761] = 5,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(114), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(112), 19,
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
  [798] = 9,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_STAR,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(92), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(9), 3,
      anon_sym_PIPE,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(7), 13,
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
  [843] = 13,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_STAR,
    ACTIONS(96), 1,
      anon_sym_CARET,
    ACTIONS(98), 1,
      anon_sym_AMP,
    ACTIONS(116), 1,
      anon_sym_PIPE,
    ACTIONS(9), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(92), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(7), 9,
      ts_builtin_sym_end,
      anon_sym_def,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [896] = 11,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_STAR,
    ACTIONS(98), 1,
      anon_sym_AMP,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(92), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(9), 3,
      anon_sym_PIPE,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(7), 10,
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
  [945] = 2,
    ACTIONS(120), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
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
  [976] = 10,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_STAR,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(92), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(9), 3,
      anon_sym_PIPE,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(7), 11,
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
  [1023] = 3,
    ACTIONS(126), 1,
      anon_sym_COLON,
    ACTIONS(124), 4,
      anon_sym_PIPE,
      anon_sym_STAR,
      anon_sym_GT,
      anon_sym_LT,
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
  [1054] = 14,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_STAR,
    ACTIONS(96), 1,
      anon_sym_CARET,
    ACTIONS(98), 1,
      anon_sym_AMP,
    ACTIONS(116), 1,
      anon_sym_PIPE,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(92), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(128), 2,
      ts_builtin_sym_end,
      anon_sym_def,
    ACTIONS(132), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(130), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1106] = 15,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_STAR,
    ACTIONS(96), 1,
      anon_sym_CARET,
    ACTIONS(98), 1,
      anon_sym_AMP,
    ACTIONS(116), 1,
      anon_sym_PIPE,
    ACTIONS(134), 1,
      anon_sym_RPAREN,
    ACTIONS(136), 1,
      anon_sym_COMMA,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(92), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(132), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(130), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1160] = 14,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_STAR,
    ACTIONS(96), 1,
      anon_sym_CARET,
    ACTIONS(98), 1,
      anon_sym_AMP,
    ACTIONS(116), 1,
      anon_sym_PIPE,
    ACTIONS(136), 1,
      anon_sym_COMMA,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(92), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(132), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(130), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1211] = 14,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_STAR,
    ACTIONS(96), 1,
      anon_sym_CARET,
    ACTIONS(98), 1,
      anon_sym_AMP,
    ACTIONS(116), 1,
      anon_sym_PIPE,
    ACTIONS(138), 1,
      anon_sym_COMMA,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(92), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(132), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(130), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1262] = 14,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_STAR,
    ACTIONS(96), 1,
      anon_sym_CARET,
    ACTIONS(98), 1,
      anon_sym_AMP,
    ACTIONS(116), 1,
      anon_sym_PIPE,
    ACTIONS(140), 1,
      anon_sym_RBRACK,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(92), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(132), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(130), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1313] = 14,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_STAR,
    ACTIONS(96), 1,
      anon_sym_CARET,
    ACTIONS(98), 1,
      anon_sym_AMP,
    ACTIONS(116), 1,
      anon_sym_PIPE,
    ACTIONS(134), 1,
      anon_sym_RPAREN,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(92), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(132), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(130), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1364] = 14,
    ACTIONS(82), 1,
      anon_sym_TILDE,
    ACTIONS(84), 1,
      anon_sym_DOT,
    ACTIONS(86), 1,
      anon_sym_else,
    ACTIONS(88), 1,
      anon_sym_STAR,
    ACTIONS(96), 1,
      anon_sym_CARET,
    ACTIONS(98), 1,
      anon_sym_AMP,
    ACTIONS(116), 1,
      anon_sym_PIPE,
    ACTIONS(142), 1,
      anon_sym_COMMA,
    ACTIONS(80), 2,
      anon_sym_STAR_GT,
      anon_sym_PIPE_GT,
    ACTIONS(90), 2,
      anon_sym_SLASH,
      anon_sym_PERCENT,
    ACTIONS(92), 2,
      anon_sym_PLUS,
      anon_sym_DASH,
    ACTIONS(94), 2,
      anon_sym_LT_LT,
      anon_sym_GT_GT,
    ACTIONS(132), 2,
      anon_sym_GT,
      anon_sym_LT,
    ACTIONS(130), 4,
      anon_sym_EQ_EQ,
      anon_sym_BANG_EQ,
      anon_sym_GT_EQ,
      anon_sym_LT_EQ,
  [1415] = 9,
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
    STATE(16), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1453] = 9,
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
  [1491] = 9,
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
    STATE(34), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1529] = 9,
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
    STATE(26), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1567] = 9,
    ACTIONS(162), 1,
      anon_sym_STAR_GT,
    ACTIONS(165), 1,
      anon_sym_LPAREN,
    ACTIONS(171), 1,
      anon_sym_RBRACK,
    ACTIONS(176), 1,
      sym_type_var,
    ACTIONS(179), 1,
      sym_identifier,
    STATE(39), 1,
      aux_sym_parserdef_ref_repeat1,
    ACTIONS(168), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(173), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(95), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [1605] = 9,
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
    STATE(28), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1643] = 9,
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
    STATE(24), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1681] = 9,
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
    STATE(23), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [1719] = 9,
    ACTIONS(194), 1,
      anon_sym_STAR_GT,
    ACTIONS(196), 1,
      anon_sym_LPAREN,
    ACTIONS(200), 1,
      anon_sym_RBRACK,
    ACTIONS(204), 1,
      sym_type_var,
    ACTIONS(206), 1,
      sym_identifier,
    STATE(45), 1,
      aux_sym_parserdef_ref_repeat1,
    ACTIONS(198), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(202), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(95), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [1757] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(208), 1,
      sym_identifier,
    ACTIONS(210), 1,
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
  [1795] = 9,
    ACTIONS(194), 1,
      anon_sym_STAR_GT,
    ACTIONS(196), 1,
      anon_sym_LPAREN,
    ACTIONS(204), 1,
      sym_type_var,
    ACTIONS(206), 1,
      sym_identifier,
    ACTIONS(212), 1,
      anon_sym_RBRACK,
    STATE(39), 1,
      aux_sym_parserdef_ref_repeat1,
    ACTIONS(198), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(202), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(95), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [1833] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(214), 1,
      sym_identifier,
    ACTIONS(216), 1,
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
  [1871] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(218), 1,
      sym_identifier,
    ACTIONS(220), 1,
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
  [1909] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(222), 1,
      sym_identifier,
    ACTIONS(224), 1,
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
  [1947] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(226), 1,
      sym_identifier,
    ACTIONS(228), 1,
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
  [1985] = 9,
    ACTIONS(29), 1,
      anon_sym_LBRACE,
    ACTIONS(39), 1,
      anon_sym_if,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(144), 1,
      anon_sym_LPAREN,
    ACTIONS(230), 1,
      sym_identifier,
    ACTIONS(232), 1,
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
  [2023] = 9,
    ACTIONS(194), 1,
      anon_sym_STAR_GT,
    ACTIONS(196), 1,
      anon_sym_LPAREN,
    ACTIONS(204), 1,
      sym_type_var,
    ACTIONS(206), 1,
      sym_identifier,
    ACTIONS(234), 1,
      anon_sym_RBRACK,
    STATE(39), 1,
      aux_sym_parserdef_ref_repeat1,
    ACTIONS(198), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(202), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(95), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2061] = 9,
    ACTIONS(194), 1,
      anon_sym_STAR_GT,
    ACTIONS(196), 1,
      anon_sym_LPAREN,
    ACTIONS(204), 1,
      sym_type_var,
    ACTIONS(206), 1,
      sym_identifier,
    ACTIONS(236), 1,
      anon_sym_RBRACK,
    STATE(51), 1,
      aux_sym_parserdef_ref_repeat1,
    ACTIONS(198), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(202), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(95), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2099] = 9,
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
    STATE(22), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2137] = 9,
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
    STATE(32), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2175] = 9,
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
    STATE(14), 8,
      sym__expression,
      sym_parser_block,
      sym_parser_array,
      sym_binary_expression,
      sym_unary_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2213] = 7,
    ACTIONS(194), 1,
      anon_sym_STAR_GT,
    ACTIONS(196), 1,
      anon_sym_LPAREN,
    ACTIONS(250), 1,
      sym_type_var,
    ACTIONS(252), 1,
      sym_identifier,
    ACTIONS(198), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(202), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(85), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2245] = 7,
    ACTIONS(194), 1,
      anon_sym_STAR_GT,
    ACTIONS(196), 1,
      anon_sym_LPAREN,
    ACTIONS(206), 1,
      sym_identifier,
    ACTIONS(254), 1,
      sym_type_var,
    ACTIONS(198), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(202), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(98), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2277] = 7,
    ACTIONS(194), 1,
      anon_sym_STAR_GT,
    ACTIONS(196), 1,
      anon_sym_LPAREN,
    ACTIONS(206), 1,
      sym_identifier,
    ACTIONS(250), 1,
      sym_type_var,
    ACTIONS(198), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(202), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(85), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2309] = 7,
    ACTIONS(194), 1,
      anon_sym_STAR_GT,
    ACTIONS(196), 1,
      anon_sym_LPAREN,
    ACTIONS(206), 1,
      sym_identifier,
    ACTIONS(256), 1,
      sym_type_var,
    ACTIONS(198), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(202), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(96), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2341] = 7,
    ACTIONS(194), 1,
      anon_sym_STAR_GT,
    ACTIONS(196), 1,
      anon_sym_LPAREN,
    ACTIONS(206), 1,
      sym_identifier,
    ACTIONS(258), 1,
      sym_type_var,
    ACTIONS(198), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(202), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(99), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2373] = 7,
    ACTIONS(194), 1,
      anon_sym_STAR_GT,
    ACTIONS(196), 1,
      anon_sym_LPAREN,
    ACTIONS(206), 1,
      sym_identifier,
    ACTIONS(260), 1,
      sym_type_var,
    ACTIONS(198), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(202), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(87), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2405] = 7,
    ACTIONS(194), 1,
      anon_sym_STAR_GT,
    ACTIONS(196), 1,
      anon_sym_LPAREN,
    ACTIONS(206), 1,
      sym_identifier,
    ACTIONS(262), 1,
      sym_type_var,
    ACTIONS(198), 2,
      anon_sym_for,
      anon_sym_each,
    ACTIONS(202), 4,
      anon_sym_int,
      anon_sym_bit,
      anon_sym_char,
      anon_sym_mem,
    STATE(97), 7,
      sym__type_expression,
      sym_binary_type_expression,
      sym_unary_type_expression,
      sym_type_array,
      sym__type_atom,
      sym_parserdef_ref,
      sym_primitive_type,
  [2437] = 2,
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
      anon_sym_PIPE,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_number_literal,
      anon_sym_SQUOTE,
  [2457] = 2,
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
      anon_sym_PIPE,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_number_literal,
      anon_sym_SQUOTE,
  [2477] = 2,
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
      anon_sym_PIPE,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_number_literal,
      anon_sym_SQUOTE,
  [2497] = 2,
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
      anon_sym_PIPE,
      anon_sym_BANG,
      anon_sym_PLUS,
      anon_sym_DASH,
      sym_number_literal,
      anon_sym_SQUOTE,
  [2517] = 2,
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
  [2533] = 4,
    ACTIONS(284), 1,
      anon_sym_BANG,
    ACTIONS(288), 1,
      anon_sym_SQUOTE,
    ACTIONS(286), 2,
      sym_identifier,
      sym_number_literal,
    STATE(77), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2552] = 4,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(290), 1,
      anon_sym_BANG,
    ACTIONS(292), 2,
      sym_identifier,
      sym_number_literal,
    STATE(3), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2571] = 4,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(290), 1,
      anon_sym_BANG,
    ACTIONS(294), 2,
      sym_identifier,
      sym_number_literal,
    STATE(2), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2590] = 4,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(290), 1,
      anon_sym_BANG,
    ACTIONS(296), 2,
      sym_identifier,
      sym_number_literal,
    STATE(4), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2609] = 4,
    ACTIONS(284), 1,
      anon_sym_BANG,
    ACTIONS(288), 1,
      anon_sym_SQUOTE,
    ACTIONS(298), 2,
      sym_identifier,
      sym_number_literal,
    STATE(74), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2628] = 4,
    ACTIONS(284), 1,
      anon_sym_BANG,
    ACTIONS(288), 1,
      anon_sym_SQUOTE,
    ACTIONS(300), 2,
      sym_identifier,
      sym_number_literal,
    STATE(78), 6,
      sym__constraint_expression,
      sym_binary_constraint_expression,
      sym_unary_constraint_expression,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2647] = 2,
    ACTIONS(17), 3,
      anon_sym_or,
      anon_sym_and,
      sym_identifier,
    ACTIONS(15), 6,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
  [2661] = 2,
    ACTIONS(25), 3,
      anon_sym_or,
      anon_sym_and,
      sym_identifier,
    ACTIONS(23), 6,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
  [2675] = 2,
    ACTIONS(21), 3,
      anon_sym_or,
      anon_sym_and,
      sym_identifier,
    ACTIONS(19), 6,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
  [2689] = 4,
    ACTIONS(304), 1,
      anon_sym_or,
    ACTIONS(306), 1,
      anon_sym_and,
    ACTIONS(308), 1,
      sym_identifier,
    ACTIONS(302), 6,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
  [2707] = 3,
    ACTIONS(306), 1,
      anon_sym_and,
    ACTIONS(17), 2,
      anon_sym_or,
      sym_identifier,
    ACTIONS(15), 6,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
  [2723] = 2,
    ACTIONS(312), 1,
      anon_sym_LBRACK,
    ACTIONS(310), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      sym_identifier,
  [2736] = 2,
    ACTIONS(316), 1,
      anon_sym_LBRACK,
    ACTIONS(314), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      sym_identifier,
  [2749] = 1,
    ACTIONS(318), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      sym_identifier,
  [2759] = 1,
    ACTIONS(320), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      sym_identifier,
  [2769] = 1,
    ACTIONS(322), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      sym_identifier,
  [2779] = 1,
    ACTIONS(324), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      sym_identifier,
  [2789] = 2,
    ACTIONS(326), 1,
      anon_sym_TILDE,
    ACTIONS(302), 6,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      sym_identifier,
  [2801] = 1,
    ACTIONS(328), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      sym_identifier,
  [2811] = 2,
    ACTIONS(326), 1,
      anon_sym_TILDE,
    ACTIONS(330), 6,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      sym_identifier,
  [2823] = 1,
    ACTIONS(332), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      sym_identifier,
  [2833] = 1,
    ACTIONS(334), 7,
      anon_sym_STAR_GT,
      anon_sym_EQ,
      anon_sym_RPAREN,
      anon_sym_TILDE,
      anon_sym_RBRACK,
      anon_sym_COMMA,
      sym_identifier,
  [2843] = 3,
    ACTIONS(288), 1,
      anon_sym_SQUOTE,
    ACTIONS(336), 2,
      sym_identifier,
      sym_number_literal,
    STATE(75), 3,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2856] = 3,
    ACTIONS(45), 1,
      anon_sym_SQUOTE,
    ACTIONS(338), 2,
      sym_identifier,
      sym_number_literal,
    STATE(6), 3,
      sym__atom,
      sym__literal,
      sym_char_literal,
  [2869] = 3,
    ACTIONS(5), 1,
      anon_sym_def,
    ACTIONS(340), 1,
      ts_builtin_sym_end,
    STATE(93), 3,
      sym__definition,
      sym_parser_definition,
      aux_sym_source_file_repeat1,
  [2881] = 3,
    ACTIONS(342), 1,
      ts_builtin_sym_end,
    ACTIONS(344), 1,
      anon_sym_def,
    STATE(93), 3,
      sym__definition,
      sym_parser_definition,
      aux_sym_source_file_repeat1,
  [2893] = 3,
    ACTIONS(316), 1,
      anon_sym_LBRACK,
    ACTIONS(347), 1,
      anon_sym_EQ,
    ACTIONS(314), 3,
      anon_sym_STAR_GT,
      anon_sym_TILDE,
      sym_identifier,
  [2905] = 4,
    ACTIONS(326), 1,
      anon_sym_TILDE,
    ACTIONS(349), 1,
      anon_sym_STAR_GT,
    ACTIONS(351), 1,
      anon_sym_COMMA,
    ACTIONS(353), 1,
      sym_identifier,
  [2918] = 4,
    ACTIONS(326), 1,
      anon_sym_TILDE,
    ACTIONS(349), 1,
      anon_sym_STAR_GT,
    ACTIONS(353), 1,
      sym_identifier,
    ACTIONS(355), 1,
      anon_sym_EQ,
  [2931] = 4,
    ACTIONS(326), 1,
      anon_sym_TILDE,
    ACTIONS(349), 1,
      anon_sym_STAR_GT,
    ACTIONS(353), 1,
      sym_identifier,
    ACTIONS(357), 1,
      anon_sym_RPAREN,
  [2944] = 4,
    ACTIONS(326), 1,
      anon_sym_TILDE,
    ACTIONS(349), 1,
      anon_sym_STAR_GT,
    ACTIONS(353), 1,
      sym_identifier,
    ACTIONS(359), 1,
      anon_sym_RBRACK,
  [2957] = 3,
    ACTIONS(326), 1,
      anon_sym_TILDE,
    ACTIONS(353), 1,
      sym_identifier,
    ACTIONS(361), 1,
      anon_sym_STAR_GT,
  [2967] = 1,
    ACTIONS(363), 3,
      anon_sym_RPAREN,
      anon_sym_RBRACE,
      anon_sym_PIPE,
  [2973] = 2,
    ACTIONS(365), 1,
      anon_sym_RBRACE,
    ACTIONS(367), 1,
      anon_sym_PIPE,
  [2980] = 2,
    ACTIONS(367), 1,
      anon_sym_PIPE,
    ACTIONS(369), 1,
      anon_sym_RPAREN,
  [2987] = 1,
    ACTIONS(371), 1,
      anon_sym_SQUOTE,
  [2991] = 1,
    ACTIONS(373), 1,
      anon_sym_LBRACK,
  [2995] = 1,
    ACTIONS(375), 1,
      anon_sym_LBRACK,
  [2999] = 1,
    ACTIONS(377), 1,
      sym_identifier,
  [3003] = 1,
    ACTIONS(379), 1,
      anon_sym_COLON,
  [3007] = 1,
    ACTIONS(381), 1,
      ts_builtin_sym_end,
  [3011] = 1,
    ACTIONS(383), 1,
      aux_sym_char_literal_token1,
  [3015] = 1,
    ACTIONS(385), 1,
      anon_sym_SQUOTE,
  [3019] = 1,
    ACTIONS(387), 1,
      aux_sym_char_literal_token1,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(2)] = 0,
  [SMALL_STATE(3)] = 37,
  [SMALL_STATE(4)] = 70,
  [SMALL_STATE(5)] = 105,
  [SMALL_STATE(6)] = 138,
  [SMALL_STATE(7)] = 171,
  [SMALL_STATE(8)] = 226,
  [SMALL_STATE(9)] = 278,
  [SMALL_STATE(10)] = 330,
  [SMALL_STATE(11)] = 382,
  [SMALL_STATE(12)] = 434,
  [SMALL_STATE(13)] = 465,
  [SMALL_STATE(14)] = 508,
  [SMALL_STATE(15)] = 541,
  [SMALL_STATE(16)] = 592,
  [SMALL_STATE(17)] = 629,
  [SMALL_STATE(18)] = 660,
  [SMALL_STATE(19)] = 699,
  [SMALL_STATE(20)] = 730,
  [SMALL_STATE(21)] = 761,
  [SMALL_STATE(22)] = 798,
  [SMALL_STATE(23)] = 843,
  [SMALL_STATE(24)] = 896,
  [SMALL_STATE(25)] = 945,
  [SMALL_STATE(26)] = 976,
  [SMALL_STATE(27)] = 1023,
  [SMALL_STATE(28)] = 1054,
  [SMALL_STATE(29)] = 1106,
  [SMALL_STATE(30)] = 1160,
  [SMALL_STATE(31)] = 1211,
  [SMALL_STATE(32)] = 1262,
  [SMALL_STATE(33)] = 1313,
  [SMALL_STATE(34)] = 1364,
  [SMALL_STATE(35)] = 1415,
  [SMALL_STATE(36)] = 1453,
  [SMALL_STATE(37)] = 1491,
  [SMALL_STATE(38)] = 1529,
  [SMALL_STATE(39)] = 1567,
  [SMALL_STATE(40)] = 1605,
  [SMALL_STATE(41)] = 1643,
  [SMALL_STATE(42)] = 1681,
  [SMALL_STATE(43)] = 1719,
  [SMALL_STATE(44)] = 1757,
  [SMALL_STATE(45)] = 1795,
  [SMALL_STATE(46)] = 1833,
  [SMALL_STATE(47)] = 1871,
  [SMALL_STATE(48)] = 1909,
  [SMALL_STATE(49)] = 1947,
  [SMALL_STATE(50)] = 1985,
  [SMALL_STATE(51)] = 2023,
  [SMALL_STATE(52)] = 2061,
  [SMALL_STATE(53)] = 2099,
  [SMALL_STATE(54)] = 2137,
  [SMALL_STATE(55)] = 2175,
  [SMALL_STATE(56)] = 2213,
  [SMALL_STATE(57)] = 2245,
  [SMALL_STATE(58)] = 2277,
  [SMALL_STATE(59)] = 2309,
  [SMALL_STATE(60)] = 2341,
  [SMALL_STATE(61)] = 2373,
  [SMALL_STATE(62)] = 2405,
  [SMALL_STATE(63)] = 2437,
  [SMALL_STATE(64)] = 2457,
  [SMALL_STATE(65)] = 2477,
  [SMALL_STATE(66)] = 2497,
  [SMALL_STATE(67)] = 2517,
  [SMALL_STATE(68)] = 2533,
  [SMALL_STATE(69)] = 2552,
  [SMALL_STATE(70)] = 2571,
  [SMALL_STATE(71)] = 2590,
  [SMALL_STATE(72)] = 2609,
  [SMALL_STATE(73)] = 2628,
  [SMALL_STATE(74)] = 2647,
  [SMALL_STATE(75)] = 2661,
  [SMALL_STATE(76)] = 2675,
  [SMALL_STATE(77)] = 2689,
  [SMALL_STATE(78)] = 2707,
  [SMALL_STATE(79)] = 2723,
  [SMALL_STATE(80)] = 2736,
  [SMALL_STATE(81)] = 2749,
  [SMALL_STATE(82)] = 2759,
  [SMALL_STATE(83)] = 2769,
  [SMALL_STATE(84)] = 2779,
  [SMALL_STATE(85)] = 2789,
  [SMALL_STATE(86)] = 2801,
  [SMALL_STATE(87)] = 2811,
  [SMALL_STATE(88)] = 2823,
  [SMALL_STATE(89)] = 2833,
  [SMALL_STATE(90)] = 2843,
  [SMALL_STATE(91)] = 2856,
  [SMALL_STATE(92)] = 2869,
  [SMALL_STATE(93)] = 2881,
  [SMALL_STATE(94)] = 2893,
  [SMALL_STATE(95)] = 2905,
  [SMALL_STATE(96)] = 2918,
  [SMALL_STATE(97)] = 2931,
  [SMALL_STATE(98)] = 2944,
  [SMALL_STATE(99)] = 2957,
  [SMALL_STATE(100)] = 2967,
  [SMALL_STATE(101)] = 2973,
  [SMALL_STATE(102)] = 2980,
  [SMALL_STATE(103)] = 2987,
  [SMALL_STATE(104)] = 2991,
  [SMALL_STATE(105)] = 2995,
  [SMALL_STATE(106)] = 2999,
  [SMALL_STATE(107)] = 3003,
  [SMALL_STATE(108)] = 3007,
  [SMALL_STATE(109)] = 3011,
  [SMALL_STATE(110)] = 3015,
  [SMALL_STATE(111)] = 3019,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0),
  [5] = {.entry = {.count = 1, .reusable = true}}, SHIFT(60),
  [7] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_expression, 3, .production_id = 4),
  [9] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_binary_expression, 3, .production_id = 4),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(71),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(69),
  [15] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_constraint_expression, 3, .production_id = 4),
  [17] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_binary_constraint_expression, 3, .production_id = 4),
  [19] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_char_literal, 3),
  [21] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_char_literal, 3),
  [23] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_constraint_expression, 2),
  [25] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unary_constraint_expression, 2),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [29] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [31] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [33] = {.entry = {.count = 1, .reusable = false}}, SHIFT(104),
  [35] = {.entry = {.count = 1, .reusable = false}}, SHIFT(106),
  [37] = {.entry = {.count = 1, .reusable = true}}, SHIFT(48),
  [39] = {.entry = {.count = 1, .reusable = false}}, SHIFT(48),
  [41] = {.entry = {.count = 1, .reusable = false}}, SHIFT(27),
  [43] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [45] = {.entry = {.count = 1, .reusable = true}}, SHIFT(111),
  [47] = {.entry = {.count = 1, .reusable = true}}, SHIFT(29),
  [49] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_sequence, 1),
  [51] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(8),
  [54] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2),
  [56] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(7),
  [59] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(104),
  [62] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(106),
  [65] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(48),
  [68] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(48),
  [71] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(27),
  [74] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(30),
  [77] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 2), SHIFT_REPEAT(111),
  [80] = {.entry = {.count = 1, .reusable = true}}, SHIFT(35),
  [82] = {.entry = {.count = 1, .reusable = true}}, SHIFT(70),
  [84] = {.entry = {.count = 1, .reusable = true}}, SHIFT(50),
  [86] = {.entry = {.count = 1, .reusable = true}}, SHIFT(55),
  [88] = {.entry = {.count = 1, .reusable = false}}, SHIFT(47),
  [90] = {.entry = {.count = 1, .reusable = true}}, SHIFT(47),
  [92] = {.entry = {.count = 1, .reusable = true}}, SHIFT(49),
  [94] = {.entry = {.count = 1, .reusable = true}}, SHIFT(53),
  [96] = {.entry = {.count = 1, .reusable = true}}, SHIFT(41),
  [98] = {.entry = {.count = 1, .reusable = true}}, SHIFT(38),
  [100] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_array, 4, .production_id = 5),
  [102] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parser_array, 4, .production_id = 5),
  [104] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_block, 3, .production_id = 12),
  [106] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parser_block, 3, .production_id = 12),
  [108] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__expression, 3),
  [110] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__expression, 3),
  [112] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_expression, 2, .production_id = 2),
  [114] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_unary_expression, 2, .production_id = 2),
  [116] = {.entry = {.count = 1, .reusable = false}}, SHIFT(36),
  [118] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_block, 2),
  [120] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parser_block, 2),
  [122] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__atom, 1),
  [124] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym__atom, 1),
  [126] = {.entry = {.count = 1, .reusable = true}}, SHIFT(37),
  [128] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_definition, 6, .production_id = 9),
  [130] = {.entry = {.count = 1, .reusable = true}}, SHIFT(42),
  [132] = {.entry = {.count = 1, .reusable = false}}, SHIFT(42),
  [134] = {.entry = {.count = 1, .reusable = true}}, SHIFT(20),
  [136] = {.entry = {.count = 1, .reusable = true}}, SHIFT(66),
  [138] = {.entry = {.count = 1, .reusable = true}}, SHIFT(65),
  [140] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [142] = {.entry = {.count = 1, .reusable = true}}, SHIFT(64),
  [144] = {.entry = {.count = 1, .reusable = true}}, SHIFT(46),
  [146] = {.entry = {.count = 1, .reusable = false}}, SHIFT(16),
  [148] = {.entry = {.count = 1, .reusable = true}}, SHIFT(16),
  [150] = {.entry = {.count = 1, .reusable = false}}, SHIFT(15),
  [152] = {.entry = {.count = 1, .reusable = true}}, SHIFT(15),
  [154] = {.entry = {.count = 1, .reusable = false}}, SHIFT(34),
  [156] = {.entry = {.count = 1, .reusable = true}}, SHIFT(34),
  [158] = {.entry = {.count = 1, .reusable = false}}, SHIFT(26),
  [160] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [162] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(61),
  [165] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(62),
  [168] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(105),
  [171] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8),
  [173] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(88),
  [176] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(95),
  [179] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 8), SHIFT_REPEAT(80),
  [182] = {.entry = {.count = 1, .reusable = false}}, SHIFT(28),
  [184] = {.entry = {.count = 1, .reusable = true}}, SHIFT(28),
  [186] = {.entry = {.count = 1, .reusable = false}}, SHIFT(24),
  [188] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [190] = {.entry = {.count = 1, .reusable = false}}, SHIFT(23),
  [192] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [194] = {.entry = {.count = 1, .reusable = true}}, SHIFT(61),
  [196] = {.entry = {.count = 1, .reusable = true}}, SHIFT(62),
  [198] = {.entry = {.count = 1, .reusable = false}}, SHIFT(105),
  [200] = {.entry = {.count = 1, .reusable = true}}, SHIFT(82),
  [202] = {.entry = {.count = 1, .reusable = false}}, SHIFT(88),
  [204] = {.entry = {.count = 1, .reusable = true}}, SHIFT(95),
  [206] = {.entry = {.count = 1, .reusable = false}}, SHIFT(80),
  [208] = {.entry = {.count = 1, .reusable = false}}, SHIFT(31),
  [210] = {.entry = {.count = 1, .reusable = true}}, SHIFT(31),
  [212] = {.entry = {.count = 1, .reusable = true}}, SHIFT(89),
  [214] = {.entry = {.count = 1, .reusable = false}}, SHIFT(33),
  [216] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [218] = {.entry = {.count = 1, .reusable = false}}, SHIFT(18),
  [220] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [222] = {.entry = {.count = 1, .reusable = false}}, SHIFT(21),
  [224] = {.entry = {.count = 1, .reusable = true}}, SHIFT(21),
  [226] = {.entry = {.count = 1, .reusable = false}}, SHIFT(13),
  [228] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [230] = {.entry = {.count = 1, .reusable = false}}, SHIFT(12),
  [232] = {.entry = {.count = 1, .reusable = true}}, SHIFT(12),
  [234] = {.entry = {.count = 1, .reusable = true}}, SHIFT(84),
  [236] = {.entry = {.count = 1, .reusable = true}}, SHIFT(81),
  [238] = {.entry = {.count = 1, .reusable = false}}, SHIFT(22),
  [240] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [242] = {.entry = {.count = 1, .reusable = false}}, SHIFT(32),
  [244] = {.entry = {.count = 1, .reusable = true}}, SHIFT(32),
  [246] = {.entry = {.count = 1, .reusable = false}}, SHIFT(14),
  [248] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
  [250] = {.entry = {.count = 1, .reusable = true}}, SHIFT(85),
  [252] = {.entry = {.count = 1, .reusable = false}}, SHIFT(94),
  [254] = {.entry = {.count = 1, .reusable = true}}, SHIFT(98),
  [256] = {.entry = {.count = 1, .reusable = true}}, SHIFT(96),
  [258] = {.entry = {.count = 1, .reusable = true}}, SHIFT(99),
  [260] = {.entry = {.count = 1, .reusable = true}}, SHIFT(87),
  [262] = {.entry = {.count = 1, .reusable = true}}, SHIFT(97),
  [264] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_parser_sequence_repeat1, 3),
  [266] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_parser_sequence_repeat1, 3),
  [268] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parse_statement, 4, .production_id = 14),
  [270] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parse_statement, 4, .production_id = 14),
  [272] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_let_statement, 7, .production_id = 15),
  [274] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_let_statement, 7, .production_id = 15),
  [276] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parse_statement, 2, .production_id = 11),
  [278] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_parse_statement, 2, .production_id = 11),
  [280] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 6),
  [282] = {.entry = {.count = 1, .reusable = false}}, REDUCE(aux_sym_parserdef_ref_repeat1, 2, .production_id = 6),
  [284] = {.entry = {.count = 1, .reusable = true}}, SHIFT(90),
  [286] = {.entry = {.count = 1, .reusable = true}}, SHIFT(77),
  [288] = {.entry = {.count = 1, .reusable = true}}, SHIFT(109),
  [290] = {.entry = {.count = 1, .reusable = true}}, SHIFT(91),
  [292] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [294] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [296] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [298] = {.entry = {.count = 1, .reusable = true}}, SHIFT(74),
  [300] = {.entry = {.count = 1, .reusable = true}}, SHIFT(78),
  [302] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_binary_type_expression, 3, .production_id = 4),
  [304] = {.entry = {.count = 1, .reusable = false}}, SHIFT(73),
  [306] = {.entry = {.count = 1, .reusable = false}}, SHIFT(72),
  [308] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_binary_type_expression, 3, .production_id = 4),
  [310] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 2, .production_id = 3),
  [312] = {.entry = {.count = 1, .reusable = true}}, SHIFT(43),
  [314] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 1, .production_id = 1),
  [316] = {.entry = {.count = 1, .reusable = true}}, SHIFT(52),
  [318] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 3, .production_id = 1),
  [320] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 4, .production_id = 3),
  [322] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym__type_expression, 3),
  [324] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 4, .production_id = 7),
  [326] = {.entry = {.count = 1, .reusable = true}}, SHIFT(68),
  [328] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_type_array, 4, .production_id = 5),
  [330] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_unary_type_expression, 2, .production_id = 2),
  [332] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_primitive_type, 1),
  [334] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parserdef_ref, 5, .production_id = 10),
  [336] = {.entry = {.count = 1, .reusable = true}}, SHIFT(75),
  [338] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [340] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1),
  [342] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2),
  [344] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2), SHIFT_REPEAT(60),
  [347] = {.entry = {.count = 1, .reusable = true}}, SHIFT(40),
  [349] = {.entry = {.count = 1, .reusable = true}}, SHIFT(58),
  [351] = {.entry = {.count = 1, .reusable = true}}, SHIFT(67),
  [353] = {.entry = {.count = 1, .reusable = true}}, SHIFT(79),
  [355] = {.entry = {.count = 1, .reusable = true}}, SHIFT(44),
  [357] = {.entry = {.count = 1, .reusable = true}}, SHIFT(83),
  [359] = {.entry = {.count = 1, .reusable = true}}, SHIFT(86),
  [361] = {.entry = {.count = 1, .reusable = true}}, SHIFT(56),
  [363] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_parser_choice, 3, .production_id = 13),
  [365] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [367] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [369] = {.entry = {.count = 1, .reusable = true}}, SHIFT(63),
  [371] = {.entry = {.count = 1, .reusable = true}}, SHIFT(76),
  [373] = {.entry = {.count = 1, .reusable = true}}, SHIFT(54),
  [375] = {.entry = {.count = 1, .reusable = true}}, SHIFT(57),
  [377] = {.entry = {.count = 1, .reusable = true}}, SHIFT(107),
  [379] = {.entry = {.count = 1, .reusable = true}}, SHIFT(59),
  [381] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [383] = {.entry = {.count = 1, .reusable = true}}, SHIFT(103),
  [385] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [387] = {.entry = {.count = 1, .reusable = true}}, SHIFT(110),
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
