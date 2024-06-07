#include "../include/lexer.h"

#include "../include/defines.h"
#include "../lib/containers/hashmap.h"
#include "../lib/containers/list.h"

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

Token *new_token(enum TokenKind kind, char *lexeme, void *literal, int line, char *string) {
    Token *t = (Token *)malloc(sizeof(Token));
    t->kind = kind;
    t->line = line;
    t->lexeme = NULL;
    t->literal = NULL;
    t->string = NULL;

    if (lexeme != NULL) {
        t->lexeme = malloc(strlen(lexeme) + 1);
        strcpy(t->lexeme, lexeme);
    }

    if (literal != NULL) {
        t->literal = malloc(strlen(literal) + 1);
        strcpy(t->literal, literal);
    }

    if (string != NULL) {
        t->string = malloc(strlen(string) + 1);
        strcpy(t->string, string);
    }
    return t;
}

Lexer *new_lexer(char *input) {
    Lexer *l = (Lexer *)malloc(sizeof(Lexer));
    l->tokens = list_new();
    l->source = input;
    l->start = 0;
    l->current = 0;
    l->line = 1;
    l->token_scanned = 0;

    // generate the keyword map
    map = hashmap_new(sizeof(struct Keyword), 0, 0, 0, keyword_hash, keyword_compare, NULL, NULL);
    hashmap_set(map, &(struct Keyword){.literal = "package", .kind = TOKEN_PACKAGE});
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

void add_token(Lexer *l, enum TokenKind kind, char *string) {
    char *lexeme = (char *)malloc(l->current - l->start + 1);
    memcpy(lexeme, &l->source[l->start], l->current - l->start);
    // Null-terminate the string
    lexeme[l->current - l->start] = '\0';
    Token *token = new_token(kind, lexeme, NULL, l->line, string);

    list_rpush(l->tokens, list_node_new(token));
    l->token_scanned++;
}

void add_token_literal(Lexer *l, enum TokenKind kind, char *string, void *literal) {
    char *lexeme = (char *)malloc(l->current - l->start + 1);
    memcpy(lexeme, &l->source[l->start], l->current - l->start);
    // Null-terminate the string
    lexeme[l->current - l->start] = '\0';
    Token *token = new_token(kind, lexeme, literal, l->line, string);

    list_rpush(l->tokens, list_node_new(token));
    l->token_scanned++;
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
        add_token(l, TOKEN_ERROR, "!!!error!!!");
        return;
    }
    // closing the string
    advance(l);

    // Trim the surrounding quotes and create a new string
    char *value = (char *)malloc(l->current - l->start - 2);
    memcpy(value, &l->source[l->start + 1], l->current - l->start - 2);
    value[l->current - l->start - 2] = '\0';

    add_token_literal(l, TOKEN_STRING, value, value);
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
        add_token_literal(l, TOKEN_FLOAT, value, value);
        return;
    }
    add_token_literal(l, TOKEN_INT, value, value);
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
        add_token(l, keyword->kind, value);
        return;
    }
    add_token(l, TOKEN_IDENTIFIER, value);
}

void scan_token(Lexer *l) {
    char c;
scanner:
    c = l->source[l->current];
    l->current++;
    switch (c) {
        case '(':
            add_token(l, TOKEN_LPAREN, "(");
            break;
        case ')':
            add_token(l, TOKEN_RPAREN, ")");
            break;
        case '{':
            add_token(l, TOKEN_LBRACE, "{");
            break;
        case '}':
            add_token(l, TOKEN_RBRACE, "}");
            break;
        case '[':
            add_token(l, TOKEN_LBRACKET, "[");
            break;
        case ']':
            add_token(l, TOKEN_RBRACKET, "]");
            break;
        case ',':
            add_token(l, TOKEN_COMMA, ",");
            break;
        case '.':
            add_token(l, TOKEN_DOT, ".");
            break;
        case '-':
            if (match(l, '>')) {
                add_token(l, TOKEN_ARROW, "->");
            } else if (match(l, '=')) {
                add_token(l, TOKEN_SUB_ASSIGN, "-=");
            } else if (match(l, '-')) {
                add_token(l, TOKEN_DECREMENT, "--");
            } else {
                add_token(l, TOKEN_MINUS, "-");
            }
            break;
        case '+':
            if (match(l, '=')) {
                add_token(l, TOKEN_ADD_ASSIGN, "+=");
            } else if (match(l, '+')) {
                add_token(l, TOKEN_INCREMENT, "++");
            } else {
                add_token(l, TOKEN_PLUS, "+");
            }
            break;
        case ';':
            add_token(l, TOKEN_SEMICOLON, ";");
            break;
        case '*':
            if (match(l, '=')) {
                add_token(l, TOKEN_MUL_ASSIGN, "*=");
            } else {
                add_token(l, TOKEN_STAR, "*");
            }
            break;
        case '%':
            if (match(l, '=')) {
                add_token(l, TOKEN_MOD_ASSIGN, "%=");
            } else {
                add_token(l, TOKEN_PERCENT, "%");
            }
            break;
        case '!':
            if (match(l, '=')) {
                add_token(l, TOKEN_NOTEQ, "!=");
            } else {
                add_token(l, TOKEN_BANG, "!");
            }
            break;
        case '=':
            if (match(l, '=')) {
                add_token(l, TOKEN_EQ, "==");
            } else {
                add_token(l, TOKEN_ASSIGN, "=");
            }
            break;
        case '<':
            if (match(l, '=')) {
                add_token(l, TOKEN_LTEQ, "<=");
            } else if (match(l, '<')) {
                if (peek(l) == '=') {
                    advance(l);
                    add_token(l, TOKEN_SHL_ASSIGN, "<<=");
                } else {
                    add_token(l, TOKEN_SHIFTLEFT, "<<");
                }
            } else {
                add_token(l, TOKEN_LANGLE, "<");
            }
            break;
        case '>':
            if (match(l, '=')) {
                add_token(l, TOKEN_GTEQ, ">=");
            } else if (match(l, '>')) {
                if (peek(l) == '=') {
                    advance(l);
                    add_token(l, TOKEN_SHR_ASSIGN, ">>=");
                } else {
                    add_token(l, TOKEN_SHIFTRIGHT, ">>");
                }
            } else {
                add_token(l, TOKEN_RANGLE, ">");
            }
            break;
        case '^':
            if (match(l, '=')) {
                add_token(l, TOKEN_XOR_ASSIGN, "^=");
            } else {
                add_token(l, TOKEN_CARET, "^");
            }
            break;
        case ':':
            add_token(l, TOKEN_COLON, ":");
            break;
        case '_':
            add_token(l, TOKEN_UNDERSCORE, "_");
            break;
        case '/':
            if (match(l, '/')) {
                while (peek(l) != '\n' && !is_eof(l)) {
                    advance(l);
                }
            } else if (match(l, '*')) {
                while (peek(l) != '*' && peek_next(l) != '/' && !is_eof(l)) {
                    if (peek(l) == '\n') {
                        l->line++;
                    }
                    advance(l);
                }
                if (is_eof(l)) {
                    add_token(l, TOKEN_ERROR, "!!!error!!!");
                    return;
                }
                advance(l);
                advance(l);
            } else if (match(l, '=')) {
                add_token(l, TOKEN_DIV_ASSIGN, "/=");
            } else {
                add_token(l, TOKEN_SLASH, "/");
            }
            break;
        case ' ':
        case '\r':
        case '\t':
            l->current++;
            l->start++;
            goto scanner;
        case '\n':
            l->line++;
            l->current++;
            l->start++;
            goto scanner;
        case '"':
            string(l);
            break;
        case '|':
            if (match(l, '|')) {
                add_token(l, TOKEN_PIPE_OR, "||");
            } else if (match(l, '=')) {
                add_token(l, TOKEN_OR_ASSIGN, "|=");
            } else {
                add_token(l, TOKEN_PIPE, "|");
            }
            break;
        case '&':
            if (match(l, '&')) {
                add_token(l, TOKEN_AMPERSAND_AND, "&&");
            } else if (match(l, '=')) {
                add_token(l, TOKEN_AND_ASSIGN, "&=");
            } else {
                add_token(l, TOKEN_AMPERSAND, "&");
            }
            break;
        default:
            if (is_digit(c)) {
                number(l);
            } else if (isalpha(c)) {
                identifier(l);
            } else {
                add_token(l, TOKEN_ERROR, "!!!error!!!");
            }
            break;
    }
}

Token *next_token(Lexer *l) {
    if (!is_eof(l)) {
        l->start = l->current;
        scan_token(l);
    } else {
        add_token(l, TOKEN_EOF, "EOF");
    }
    list_node_t *token = list_at(l->tokens, l->token_scanned - 1);
    return (Token *)token->val;
}

char *token_kind_to_string(enum TokenKind kind) {
    switch (kind) {
        case TOKEN_INT:
            return "TOKEN_INT\0";
        case TOKEN_STRING:
            return "TOKEN_STRING\0";
        case TOKEN_CHAR:
            return "TOKEN_CHAR\0";
        case TOKEN_BOOL:
            return "TOKEN_BOOL\0";
        case TOKEN_FLOAT:
            return "TOKEN_FLOAT\0";
        case TOKEN_IDENTIFIER:
            return "TOKEN_IDENTIFIER\0";
        case TOKEN_COMMENT:
            return "TOKEN_COMMENT\0";
        case TOKEN_MULTISTRING:
            return "TOKEN_MULTISTRING\0";
        case TOKEN_SEMICOLON:
            return "TOKEN_SEMICOLON\0";
        case TOKEN_LPAREN:
            return "TOKEN_LPAREN\0";
        case TOKEN_RPAREN:
            return "TOKEN_RPAREN\0";
        case TOKEN_LBRACKET:
            return "TOKEN_LBRACKET\0";
        case TOKEN_RBRACKET:
            return "TOKEN_RBRACKET\0";
        case TOKEN_LBRACE:
            return "TOKEN_LBRACE\0";
        case TOKEN_RBRACE:
            return "TOKEN_RBRACE\0";
        case TOKEN_LANGLE:
            return "TOKEN_LANGLE\0";
        case TOKEN_RANGLE:
            return "TOKEN_RANGLE\0";
        case TOKEN_ARROW:
            return "TOKEN_ARROW\0";
        case TOKEN_FATARROW:
            return "TOKEN_FATARROW\0";
        case TOKEN_EQ:
            return "TOKEN_EQ\0";
        case TOKEN_ASSIGN:
            return "TOKEN_ASSIGN\0";
        case TOKEN_NOTEQ:
            return "TOKEN_NOTEQ\0";
        case TOKEN_GTEQ:
            return "TOKEN_GTEQ\0";
        case TOKEN_LTEQ:
            return "TOKEN_LTEQ\0";
        case TOKEN_COLON:
            return "TOKEN_COLON\0";
        case TOKEN_PIPE:
            return "TOKEN_PIPE\0";
        case TOKEN_PIPE_OR:
            return "TOKEN_PIPE_OR\0";
        case TOKEN_AMPERSAND:
            return "TOKEN_AMPERSAND\0";
        case TOKEN_AMPERSAND_AND:
            return "TOKEN_AMPERSAND_AND\0";
        case TOKEN_PLUS:
            return "TOKEN_PLUS\0";
        case TOKEN_MINUS:
            return "TOKEN_MINUS\0";
        case TOKEN_STAR:
            return "TOKEN_STAR\0";
        case TOKEN_SLASH:
            return "TOKEN_SLASH\0";
        case TOKEN_BACKSLASH:
            return "TOKEN_BACKSLASH\0";
        case TOKEN_CARET:
            return "TOKEN_CARET\0";
        case TOKEN_PERCENT:
            return "TOKEN_PERCENT\0";
        case TOKEN_BANG:
            return "TOKEN_BANG\0";
        case TOKEN_QUESTION:
            return "TOKEN_QUESTION\0";
        case TOKEN_DOT:
            return "TOKEN_DOT\0";
        case TOKEN_COMMA:
            return "TOKEN_COMMA\0";
        case TOKEN_UNDERSCORE:
            return "TOKEN_UNDERSCORE\0";
        case TOKEN_FN_:
            return "TOKEN_FN_\0";
        case TOKEN_LET:
            return "TOKEN_LET\0";
        case TOKEN_IF:
            return "TOKEN_IF\0";
        case TOKEN_ELSE:
            return "TOKEN_ELSE\0";
        case TOKEN_MATCH:
            return "TOKEN_MATCH\0";
        case TOKEN_ENUM:
            return "TOKEN_ENUM\0";
        case TOKEN_STRUCT:
            return "TOKEN_STRUCT\0";
        case TOKEN_TYPE:
            return "TOKEN_TYPE\0";
        case TOKEN_INTERFACE:
            return "TOKEN_INTERFACE\0";
        case TOKEN_IMPL:
            return "TOKEN_IMPL\0";
        case TOKEN_CONST:
            return "TOKEN_CONST\0";
        case TOKEN_RETURN:
            return "TOKEN_RETURN\0";
        case TOKEN_DEFER:
            return "TOKEN_DEFER\0";
        case TOKEN_USE:
            return "TOKEN_USE\0";
        case TOKEN_SPAWN:
            return "TOKEN_SPAWN\0";
        case TOKEN_MUT:
            return "TOKEN_MUT\0";
        case TOKEN_FOR:
            return "TOKEN_FOR\0";
        case TOKEN_IN_:
            return "TOKEN_IN_\0";
        case TOKEN_WHILE:
            return "TOKEN_WHILE\0";
        case TOKEN_LOOP:
            return "TOKEN_LOOP\0";
        case TOKEN_BREAK:
            return "TOKEN_BREAK\0";
        case TOKEN_CONTINUE:
            return "TOKEN_CONTINUE\0";
        case TOKEN_SELECT:
            return "TOKEN_SELECT\0";
        case TOKEN_ERROR:
            return "TOKEN_ERROR\0";
        case TOKEN_NULL:
            return "TOKEN_NULL\0";
        case TOKEN_EOF:
            return "TOKEN_EOF\0";
        default:
            return "UNKNOWN_TOKEN\0";
    }
}

char *token_to_string(Token *token) {
    if (token != NULL) {
        return token->string;
    }
    return "Empty Token";
}

void scan_tokens(Lexer *l) {
    while (!is_eof(l)) {
        l->start = l->current;
        scan_token(l);
    }
    add_token(l, TOKEN_EOF, "TOKEN_EOF");
}