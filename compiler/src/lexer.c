#include "../include/lexer.h"

#include "../include/defines.h"
#include "../lib/containers/dynarray.h"
#include "../lib/containers/hashmap.h"

typedef struct Keyword {
    char *literal;
    enum TokenKind kind;
} Keyword;

static struct hashmap *map = NULL;

int keyword_compare(const void *a, const void *b, void *udata) {
    const struct Keyword *ka = a;
    const struct Keyword *kb = b;
    return strcmp(ka->literal, kb->literal);
}

uint64_t keyword_hash(const void *item, uint64_t seed0, uint64_t seed1) {
    const struct Keyword *keyword = item;
    return hashmap_sip(keyword->literal, strlen(keyword->literal), seed0, seed1);
}

Token *new_token(enum TokenKind kind, char *lexeme, void *literal, int line) {
    Token *t = (Token *)malloc(sizeof(Token));
    t->kind = kind;
    t->lexeme = lexeme;
    t->literal = literal;
    t->line = line;
    return t;
}

Lexer *new_lexer(char *input) {
    Lexer *l = (Lexer *)malloc(sizeof(Lexer));
    l->tokens = dynarray_create(Token);
    l->source = input;
    l->start = 0;
    l->current = 0;
    l->line = 1;

    // generate the keyword map
    map = hashmap_new(sizeof(struct Keyword), 0, 0, 0, keyword_hash, keyword_compare, NULL, NULL);
    hashmap_set(map, &(struct Keyword){.literal = "fn", .kind = TOKEN_FN_});
    hashmap_set(map, &(struct Keyword){.literal = "let", .kind = TOKEN_LET});
    hashmap_set(map, &(struct Keyword){.literal = "if", .kind = TOKEN_IF});
    hashmap_set(map, &(struct Keyword){.literal = "else", .kind = TOKEN_ELSE});
    hashmap_set(map, &(struct Keyword){.literal = "match", .kind = TOKEN_MATCH});
    hashmap_set(map, &(struct Keyword){.literal = "enum", .kind = TOKEN_ENUM});
    hashmap_set(map, &(struct Keyword){.literal = "struct", .kind = TOKEN_STRUCT});
    hashmap_set(map, &(struct Keyword){.literal = "type", .kind = TOKEN_TYPE});
    hashmap_set(map, &(struct Keyword){.literal = "interface", .kind = TOKEN_INTERFACE});
    hashmap_set(map, &(struct Keyword){.literal = "impl", .kind = TOKEN_IMPL});
    hashmap_set(map, &(struct Keyword){.literal = "const", .kind = TOKEN_CONST});
    hashmap_set(map, &(struct Keyword){.literal = "return", .kind = TOKEN_RETURN});
    hashmap_set(map, &(struct Keyword){.literal = "defer", .kind = TOKEN_DEFER});
    hashmap_set(map, &(struct Keyword){.literal = "use", .kind = TOKEN_USE});
    hashmap_set(map, &(struct Keyword){.literal = "spawn", .kind = TOKEN_SPAWN});
    hashmap_set(map, &(struct Keyword){.literal = "mut", .kind = TOKEN_MUT});
    hashmap_set(map, &(struct Keyword){.literal = "for", .kind = TOKEN_FOR});
    hashmap_set(map, &(struct Keyword){.literal = "in", .kind = TOKEN_IN_});
    hashmap_set(map, &(struct Keyword){.literal = "while", .kind = TOKEN_WHILE});
    hashmap_set(map, &(struct Keyword){.literal = "loop", .kind = TOKEN_LOOP});
    hashmap_set(map, &(struct Keyword){.literal = "break", .kind = TOKEN_BREAK});
    hashmap_set(map, &(struct Keyword){.literal = "continue", .kind = TOKEN_CONTINUE});
    hashmap_set(map, &(struct Keyword){.literal = "select", .kind = TOKEN_SELECT});
    hashmap_set(map, &(struct Keyword){.literal = "support", .kind = TOKEN_SUPPORT});

    return l;
}

void free_lexer(Lexer *l) {
    if (l->source != NULL) {
        free(l->source);
    }
    free(l);
}

boolean is_eof(Lexer *l) {
    return l->current >= (int)strlen(l->source);
}

char advance(Lexer *l) {
    return l->source[l->current++];
}

void add_token(Lexer *l, enum TokenKind kind) {
    char *lexeme = (char *)malloc(l->current - l->start + 1);
    memcpy(lexeme, &l->source[l->start], l->current - l->start);
    // Null-terminate the string
    lexeme[l->current - l->start] = '\0';
    Token *token = new_token(kind, lexeme, NULL, l->line);
    dynarray_push(l->tokens, token);
}

void add_token_literal(Lexer *l, enum TokenKind kind, void *literal) {
    char *lexeme = (char *)malloc(l->current - l->start + 1);
    memcpy(lexeme, &l->source[l->start], l->current - l->start);
    // Null-terminate the string
    lexeme[l->current - l->start] = '\0';
    Token *token = new_token(kind, lexeme, literal, l->line);
    dynarray_push(l->tokens, token);
}

boolean is_alpha(char c) {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

boolean is_digit(char c) {
    return c >= '0' && c <= '9';
}

boolean is_alphanumeric(char c) {
    return is_alpha(c) || is_digit(c);
}

boolean match(Lexer *l, char expected) {
    if (is_eof(l)) {
        return false;
    }
    if (l->source[l->current] != expected) {
        return false;
    }
    l->current++;
    return true;
}

char peek(Lexer *l) {
    if (is_eof(l)) {
        return '\0';
    }
    return l->source[l->current];
}

char peek_next(Lexer *l) {
    if (l->current + 1 >= (int)strlen(l->source)) {
        return '\0';
    }
    return l->source[l->current + 1];
}

void string(Lexer *l) {
    while (peek(l) != '"' && !is_eof(l)) {
        if (peek(l) == '\n') {
            l->line++;
        }
        advance(l);
    }
    if (is_eof(l)) {
        add_token(l, TOKEN_ERROR);
        debug_printf("ERROR\n");
        return;
    }
    // closing the string
    advance(l);

    // Trim the surrounding quotes and create a new string
    char *value = (char *)malloc(l->current - l->start - 2);
    memcpy(value, &l->source[l->start + 1], l->current - l->start - 2);
    value[l->current - l->start - 2] = '\0';

    add_token_literal(l, TOKEN_STRING, value);
    debug_printf("STRING\n");
}

void number(Lexer *l) {
    boolean is_float = false;
    while (is_digit(peek(l))) {
        advance(l);
    }
    if (peek(l) == '.' && is_digit(peek_next(l))) {
        advance(l);
        while (is_digit(peek(l))) {
            advance(l);
        }
        is_float = true;
    }

    char *value = (char *)malloc(l->current - l->start + 1);
    memcpy(value, &l->source[l->start], l->current - l->start);
    value[l->current - l->start] = '\0';

    if (is_float) {
        add_token_literal(l, TOKEN_FLOAT, value);
        return;
    }
    add_token_literal(l, TOKEN_INT, value);
}

void identifier(Lexer *l) {
    while (is_alphanumeric(peek(l))) {
        advance(l);
    }

    char *value = (char *)malloc(l->current - l->start + 1);
    memcpy(value, &l->source[l->start], l->current - l->start);
    value[l->current - l->start] = '\0';

    const struct Keyword *keyword = hashmap_get(map, &(struct Keyword){.literal = value});
    if (keyword != NULL) {
        add_token(l, keyword->kind);
        return;
    }
    add_token(l, TOKEN_IDENTIFIER);
}

void scan_token(Lexer *l) {
    char c = l->source[l->current];
    l->current++;
    switch (c) {
        case '(':
            add_token(l, TOKEN_LPAREN);
            debug_printf("LPAREN\n");
            break;
        case ')':
            add_token(l, TOKEN_RPAREN);
            debug_printf("RPAREN\n");
            break;
        case '{':
            add_token(l, TOKEN_LBRACE);
            debug_printf("LBRACE\n");
            break;
        case '}':
            add_token(l, TOKEN_RBRACE);
            debug_printf("RBRACE\n");
            break;
        case '[':
            add_token(l, TOKEN_LBRACKET);
            debug_printf("LBRACKET\n");
            break;
        case ']':
            add_token(l, TOKEN_RBRACKET);
            debug_printf("RBRACKET\n");
            break;
        case ',':
            add_token(l, TOKEN_COMMA);
            debug_printf("COMMA\n");
            break;
        case '.':
            add_token(l, TOKEN_DOT);
            debug_printf("DOT\n");
            break;
        case '-':
            add_token(l, TOKEN_MINUS);
            debug_printf("MINUS\n");
            break;
        case '+':
            add_token(l, TOKEN_PLUS);
            debug_printf("PLUS\n");
            break;
        case ';':
            add_token(l, TOKEN_SEMICOLON);
            debug_printf("SEMICOLON\n");
            break;
        case '*':
            add_token(l, TOKEN_STAR);
            debug_printf("STAR\n");
            break;
        case '%':
            add_token(l, TOKEN_PERCENT);
            debug_printf("PERCENT\n");
            break;
        case '!':
            add_token(l, TOKEN_BANG);
            debug_printf("BANG\n");
            break;
        case '=':
            add_token(l, TOKEN_EQ);
            debug_printf("EQ\n");
            break;
        case '<':
            add_token(l, TOKEN_LANGLE);
            debug_printf("LANGLE\n");
            break;
        case '>':
            add_token(l, TOKEN_RANGLE);
            debug_printf("RANGLE\n");
            break;
        case '/':
            if (match(l, '/')) {
                while (peek(l) != '\n' && !is_eof(l)) {
                    advance(l);
                }
                debug_printf("COMMENT\n");
            } else if (match(l, '*')) {
                while (peek(l) != '*' && peek_next(l) != '/' && !is_eof(l)) {
                    if (peek(l) == '\n') {
                        l->line++;
                    }
                    advance(l);
                }
                if (is_eof(l)) {
                    add_token(l, TOKEN_ERROR);
                    debug_printf("ERROR\n");
                    return;
                }
                advance(l);
                advance(l);
                debug_printf("MULTILINE COMMENT\n");
            } else {
                add_token(l, TOKEN_SLASH);
                debug_printf("SLASH\n");
            }
            break;
        case ' ':
        case '\r':
        case '\t':
            break;
        case '\n':
            l->line++;
            break;
        case '"':
            string(l);
            break;
        case '|':
            add_token(l, match(l, '|') ? TOKEN_PIPE_OR : TOKEN_PIPE);
            debug_printf("PIPE\n");
            break;
        case '&':
            add_token(l, match(l, '&') ? TOKEN_AMPERSAND_AND : TOKEN_AMPERSAND);
            debug_printf("AMPERSAND\n");
            break;
        default:
            if (is_digit(c)) {
                number(l);
                debug_printf("NUMBER\n");
            } else if (isalpha(c)) {
                identifier(l);
                debug_printf("IDENTIFIER\n");
            } else {
                add_token(l, TOKEN_ERROR);
                debug_printf("ERROR\n");
            }
            break;
    }
}

void scan_tokens(Lexer *l) {
    while (!is_eof(l)) {
        l->start = l->current;
        scan_token(l);
    }
    add_token(l, TOKEN_EOF);
    debug_printf("EOF\n");
}