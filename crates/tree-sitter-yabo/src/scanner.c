#include "tree_sitter/parser.h"
#include <stddef.h>
#include <assert.h>
#include <string.h>
#include <wctype.h>
#include <stdio.h>

enum TokenType {
    NEWLINE,
    INDENT,
    DEDENT,
    BLOCK_OPEN,
    BLOCK_CLOSE,
    PARSER_BLOCK_OPEN,
    PARSER_BLOCK_CLOSE,
    LEXER_ERROR,
    NOTOKEN,
};

struct Scanner {
    uint16_t size;
    char buf[];
};

typedef enum {
    NONE,
    BRACE,
    NEW_INDENT,
    REPEAT_DEDENT,
} State;


#define MAX_BUFFER_SIZE TREE_SITTER_SERIALIZATION_BUFFER_SIZE - sizeof(struct Scanner)

#define INVALID_INDENT ((uint16_t)-1)

#define INDENT_STATE_SIZE ((size_t)1 + sizeof(uint16_t))
#define REPEAT_DEDENT_STATE_SIZE ((size_t)1 + sizeof(uint16_t))

static inline size_t scanner_size(uint16_t size) {
    return (size_t)size + sizeof(struct Scanner);
}

static inline char *current_buf_end(struct Scanner *scanner) {
    return scanner->buf + scanner->size;
}

static void add_indent(struct Scanner *scanner, uint32_t indent) {
    uint16_t truncated_indent = (uint16_t)indent;
    if (indent >= UINT16_MAX) {
        truncated_indent = UINT16_MAX - 1;
    }
    if ((size_t)scanner->size + INDENT_STATE_SIZE > MAX_BUFFER_SIZE) {
        return;
    }
    char *buf_end = current_buf_end(scanner);
    memcpy(buf_end, &truncated_indent, sizeof(uint16_t));
    buf_end[sizeof(uint16_t)] = NEW_INDENT;
    scanner->size += INDENT_STATE_SIZE;
}

static void add_repeat_dedent(struct Scanner *scanner, uint16_t dedent_count) {
    if ((size_t)scanner->size + REPEAT_DEDENT_STATE_SIZE > MAX_BUFFER_SIZE) {
        return;
    }
    char *buf_end = current_buf_end(scanner);
    memcpy(buf_end, &dedent_count, sizeof(uint16_t));
    buf_end[sizeof(uint16_t)] = REPEAT_DEDENT;
    scanner->size += REPEAT_DEDENT_STATE_SIZE;
}

static inline void add_simple_state(struct Scanner *scanner, State state) {
    if (scanner->size >= MAX_BUFFER_SIZE) {
        return;
    }
    char *buf_end = current_buf_end(scanner);
    *buf_end = state;
    scanner->size++;
}

static inline State top_state(struct Scanner *scanner) {
    if (scanner->size == 0) {
        return NONE;
    }
    return current_buf_end(scanner)[-1];
}

static uint16_t top_indent(struct Scanner *scanner) {
    if (top_state(scanner) != NEW_INDENT) {
        return INVALID_INDENT;
    }
    char *buf_end = current_buf_end(scanner);
    uint16_t indent;
    memcpy(&indent, buf_end - INDENT_STATE_SIZE, sizeof(uint16_t));
    return indent;
}

static inline uint16_t state_size(State state) {
    if (state == NEW_INDENT) {
        return INDENT_STATE_SIZE;
    } else if (state == REPEAT_DEDENT) {
        return REPEAT_DEDENT_STATE_SIZE;
    } else if (state == NONE) {
        return 0;
    } else {
        return 1;
    }
}

static State pop_state(struct Scanner *scanner) {
    State state = top_state(scanner);
    scanner->size -= state_size(state);
    return state;
}

static bool repeat_dedent(struct Scanner *scanner) {
    if (top_state(scanner) != REPEAT_DEDENT) {
        return false;
    }
    uint16_t repeat_count;
    memcpy(&repeat_count, current_buf_end(scanner) - REPEAT_DEDENT_STATE_SIZE, sizeof(uint16_t));
    if (repeat_count == 0) {
        pop_state(scanner);
        return false;
    }
    repeat_count--;
    memcpy(current_buf_end(scanner) - REPEAT_DEDENT_STATE_SIZE, &repeat_count, sizeof(uint16_t));
    return true;
}

static enum TokenType set_indent(struct Scanner *scanner, uint32_t indent) {
    State top = top_state(scanner);
    if (top == NONE) {
        // we are not in a brace context, so there should be no dedent/indent
        return NOTOKEN;
    }
    if (top == BRACE) {
        // the first non-whitespace character after a brace should determine the indent
        add_indent(scanner, indent);
        return INDENT;
    }
    uint16_t old_indent = top_indent(scanner);
    if (old_indent == INVALID_INDENT) {
        // signal an error and remove the invalid indent state
        pop_state(scanner);
        return LEXER_ERROR;
    }
    if (old_indent == indent) {
        // when the indent stays the same, that's a newline
        return NEWLINE;
    }
    if (old_indent < indent) {
        // indent increases, so add an indent state and return the indent token
        add_indent(scanner, indent);
        return INDENT;
    }
    // otherwise we have a count of dedents
    uint16_t dedent_count = 0;
    for (;;) {
        pop_state(scanner);
        State top = top_state(scanner);
        if (top == NONE || top == BRACE) {
            // we would need to dedent more than the top indent, so signal an error
            return LEXER_ERROR;
        }
        uint16_t old_indent = top_indent(scanner);
        if (old_indent == INVALID_INDENT) {
            return LEXER_ERROR;
        }
        if (old_indent < indent) {
            return LEXER_ERROR;
        }
        dedent_count++;
        if (old_indent == indent) {
            break;
        }
    }
    add_repeat_dedent(scanner, dedent_count - 1);
    return DEDENT;
}

static inline void skip(TSLexer *lexer) {
    lexer->advance(lexer, true);
}

void *tree_sitter_yabo_external_scanner_create() {
    struct Scanner *scanner = malloc(TREE_SITTER_SERIALIZATION_BUFFER_SIZE);
    scanner->size = 0;
    return (void *)scanner;
}

void tree_sitter_yabo_external_scanner_destroy(void *payload) {
    free(payload);
}

void tree_sitter_yabo_external_scanner_reset(void *payload) {
    struct Scanner *scanner = (struct Scanner *)payload;
    scanner->size = 0;
}

unsigned tree_sitter_yabo_external_scanner_serialize(void *payload, char *buffer) {
    struct Scanner *scanner = (struct Scanner *)payload;
    size_t size = scanner_size(scanner->size);
    memcpy(buffer, scanner, size);
    return size;
}

void tree_sitter_yabo_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
    memcpy(payload, buffer, length);
}

bool tree_sitter_yabo_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
    struct Scanner *scanner = (struct Scanner *)payload;
    //fprintf(stderr, "valid symbols: INDENT %d, DEDENT %d, NEWLINE %d, LEXER_ERROR %d\n",
    //    valid_symbols[INDENT], valid_symbols[DEDENT], valid_symbols[NEWLINE], valid_symbols[LEXER_ERROR]);
    
    if (top_state(scanner) == REPEAT_DEDENT) {
        if (!valid_symbols[DEDENT]) {
            // remove repeat dedent from stack
            scanner->size -= 3;
        } else if (repeat_dedent(scanner)) {
            lexer->result_symbol = DEDENT;
            return true;
        } else {
            lexer->result_symbol = NEWLINE;
            return true;
        }
    }
    bool has_newline = false;
    while (iswspace(lexer->lookahead)) {
        if (lexer->lookahead == '\n' || lexer->lookahead == '\r') {
            has_newline = true;
        }
        skip(lexer);
    }
    bool whitespace_sensitive = valid_symbols[INDENT] | valid_symbols[DEDENT] | valid_symbols[NEWLINE];
    // we are now beyond the whitespace and save the current indent level in case we need to
    // look ahead
    uint32_t new_indent;
    int32_t lookahead = lexer->lookahead;
    lexer->mark_end(lexer);

    // we ignore comments
    if (lookahead == '#') {
        // we can directly return that we have not recognized a token
        // even when we have seen newlines, because there will always
        // be a newline token after a comment and the parser will
        // just parse the comment in the meanwhile, which will be
        // ignored as it is in `extras`
        return false;
    }
    if (whitespace_sensitive || has_newline) {
        new_indent = lexer->get_column(lexer);
    }
    bool is_closing = false;
    if ((valid_symbols[PARSER_BLOCK_CLOSE] | valid_symbols[DEDENT]) && lookahead == '}') {
        is_closing = true;
        lexer->result_symbol = PARSER_BLOCK_CLOSE;
    }
    if ((valid_symbols[BLOCK_CLOSE] | valid_symbols[DEDENT]) && lookahead == '|') {
        lexer->advance(lexer, false);
        if (lexer->lookahead == '}') {
            is_closing = true;
            lexer->result_symbol = BLOCK_CLOSE;
        }
    }
    if (is_closing) {
        State state = pop_state(scanner);
        if (state == NEW_INDENT) {
            lexer->result_symbol = DEDENT;
            return true;
        }
        lexer->advance(lexer, false);
        lexer->mark_end(lexer);
        return true;
    }
    // an opening brace might still indicate indent/dedent or might be after a newline
    // so we need to emit our indent/dedent/newline tokens first and if we are finished
    // with that, we can advance the lexer
    if (whitespace_sensitive || has_newline) {
        lexer->result_symbol = set_indent(scanner, new_indent);
        if (lexer->result_symbol == INDENT && valid_symbols[INDENT]) {
            return true;
        }
        if (lexer->result_symbol == NEWLINE && valid_symbols[NEWLINE]) {
            return true;
        }
        if (lexer->result_symbol == DEDENT) {
            return true;
        }
        if (lexer->result_symbol == LEXER_ERROR) {
            return true;
        }
    }
    if ((valid_symbols[PARSER_BLOCK_OPEN] | valid_symbols[BLOCK_OPEN]) && lookahead == '{') {
        add_simple_state(scanner, BRACE);
        lexer->advance(lexer, false);
        if (lexer->lookahead == '|') {
            lexer->advance(lexer, false);
            lexer->result_symbol = BLOCK_OPEN;
        } else {
            lexer->result_symbol = PARSER_BLOCK_OPEN;
        }
        lexer->mark_end(lexer);
        return true;
    }
    return false;
}