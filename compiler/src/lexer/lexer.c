#include "lexer.h"

#include <ctype.h>
#include <stdlib.h>
#include <string.h>

#include "containers/dynarray.h"

enum TokenKind get_keyword(char *s) {
    if (strcmp(s, "fn") == 0) {
        return TOKEN_FN_;
    }
    if (strcmp(s, "let") == 0) {
        return TOKEN_LET;
    }
    if (strcmp(s, "if") == 0) {
        return TOKEN_IF;
    }
    if (strcmp(s, "else") == 0) {
        return TOKEN_ELSE;
    }
    if (strcmp(s, "match") == 0) {
        return TOKEN_MATCH;
    }
    if (strcmp(s, "enum") == 0) {
        return TOKEN_ENUM;
    }
    if (strcmp(s, "struct") == 0) {
        return TOKEN_STRUCT;
    }
    if (strcmp(s, "type") == 0) {
        return TOKEN_TYPE;
    }
    if (strcmp(s, "interface") == 0) {
        return TOKEN_INTERFACE;
    }
    if (strcmp(s, "impl") == 0) {
        return TOKEN_IMPL;
    }
    if (strcmp(s, "const") == 0) {
        return TOKEN_CONST;
    }
    if (strcmp(s, "return") == 0) {
        return TOKEN_RETURN;
    }
    if (strcmp(s, "defer") == 0) {
        return TOKEN_DEFER;
    }
    if (strcmp(s, "use") == 0) {
        return TOKEN_IMPORT;
    }
    if (strcmp(s, "spawn") == 0) {
        return TOKEN_SPAWN;
    }
    if (strcmp(s, "mut") == 0) {
        return TOKEN_MUT;
    }
    if (strcmp(s, "for") == 0) {
        return TOKEN_FOR;
    }
    if (strcmp(s, "in") == 0) {
        return TOKEN_IN_;
    }
    if (strcmp(s, "while") == 0) {
        return TOKEN_WHILE;
    }
    if (strcmp(s, "loop") == 0) {
        return TOKEN_LOOP;
    }
    if (strcmp(s, "break") == 0) {
        return TOKEN_BREAK;
    }
    if (strcmp(s, "continue") == 0) {
        return TOKEN_CONTINUE;
    }
    if (strcmp(s, "select") == 0) {
        return TOKEN_SELECT;
    }
    return TOKEN_NULL;
}

TokenKind get_syntax(char *s) {
    if (strcmp(s, "+") == 0) {
        return TOKEN_PLUS;
    }
    if (strcmp(s, "-") == 0) {
        return TOKEN_MINUS;
    }
    if (strcmp(s, "*") == 0) {
        return TOKEN_STAR;
    }
    if (strcmp(s, "/") == 0) {
        return TOKEN_SLASH;
    }
    if (strcmp(s, "%") == 0) {
        return TOKEN_PERCENT;
    }
    if (strcmp(s, "=") == 0) {
        return TOKEN_EQ;
    }
    if (strcmp(s, "==") == 0) {
        return TOKEN_EQ2;
    }
    if (strcmp(s, "!=") == 0) {
        return TOKEN_NOTEQ;
    }
    if (strcmp(s, "<") == 0) {
        return TOKEN_LANGLE;
    }
    if (strcmp(s, "<=") == 0) {
        return TOKEN_LTEQ;
    }
    if (strcmp(s, ">") == 0) {
        return TOKEN_RANGLE;
    }
    if (strcmp(s, ">=") == 0) {
        return TOKEN_GTEQ;
    }
    if (strcmp(s, "(") == 0) {
        return TOKEN_LPAREN;
    }
    if (strcmp(s, ")") == 0) {
        return TOKEN_RPAREN;
    }
    if (strcmp(s, "{") == 0) {
        return TOKEN_LCURLY;
    }
    if (strcmp(s, "}") == 0) {
        return TOKEN_RCURLY;
    }
    if (strcmp(s, "[") == 0) {
        return TOKEN_LBRACK;
    }
    if (strcmp(s, "]") == 0) {
        return TOKEN_RBRACK;
    }
    if (strcmp(s, ",") == 0) {
        return TOKEN_COMMA;
    }
    if (strcmp(s, ".") == 0) {
        return TOKEN_DOT;
    }
    if (strcmp(s, ":") == 0) {
        return TOKEN_COLON;
    }
    if (strcmp(s, ";") == 0) {
        return TOKEN_SEMICOLON;
    }
    if (strcmp(s, "->") == 0) {
        return TOKEN_ARROW;
    }
    if (strcmp(s, "=>") == 0) {
        return TOKEN_FATARROW;
    }
    if (strcmp(s, "|") == 0) {
        return TOKEN_PIPE;
    }
    if (strcmp(s, "||") == 0) {
        return TOKEN_PIPE2;
    }
    if (strcmp(s, "&") == 0) {
        return TOKEN_AMPERSAND;
    }
    if (strcmp(s, "&&") == 0) {
        return TOKEN_AMPERSAND2;
    }
    if (strcmp(s, "^") == 0) {
        return TOKEN_CARET;
    }
    if (strcmp(s, "%") == 0) {
        return TOKEN_PERCENT;
    }
    if (strcmp(s, "!") == 0) {
        return TOKEN_BANG;
    }
    if (strcmp(s, "?") == 0) {
        return TOKEN_QUESTION;
    }
    return TOKEN_NULL;
}

boolean insert_semicolon(TokenKind kind) {
    return kind == TOKEN_IDENT ||
           kind == TOKEN_INT ||
           kind == TOKEN_FLOAT ||
           kind == TOKEN_STRING ||
           kind == TOKEN_CHAR ||
           kind == TOKEN_BOOL ||
           kind == TOKEN_RPAREN ||
           kind == TOKEN_RBRACK ||
           kind == TOKEN_MULTISTRING ||
           kind == TOKEN_BREAK ||
           kind == TOKEN_CONTINUE ||
           kind == TOKEN_RETURN ||
           kind == TOKEN_RANGLE ||
           kind == TOKEN_QUESTION ||
           kind == TOKEN_RCURLY;
}

Lexer *new_lexer(char *input) {
    Lexer *l = malloc(sizeof(Lexer));
    l->input = input;
    l->ch = '\0';
    l->current_position = 0;
    l->prev_token = TOKEN_ERROR;
    l->line = 1;
    l->column = 1;

    // explicitly read the first character
    read_char_at(l, 0);

    return l;
}

void free_lexer(Lexer *l) {
    free(l);
}

int start(Lexer *l) {
    return l->column;
}

Pos *make_pos(Lexer *l, int start) {
    Pos *p = malloc(sizeof(Pos));
    p->line = l->line;
    p->column = start;
    p->len = l->column - start;
    return p;
}

char read_char_at(Lexer *l, int offset) {
    return l->input[offset];
}

// Returns the original position before advancing
int next(Lexer *l) {
    int start = l->column;
    l->column += 1;
    l->current_position += 1;
    l->ch = read_char_at(l, l->current_position);
    return start;
}

char peek_char(Lexer *l) {
    return read_char_at(l, l->current_position + 1);
}

int skip(Lexer *l, int count) {
    int st = start(l);
    while (count > 0) {
        next(l);
        count -= 1;
    }
    return st;
}

Token *once(Lexer *l, TokenKind kind) {
    char text = l->ch;
    int start = next(l);

    Token *t = malloc(sizeof(Token));
    t->kind = kind;
    t->literal = &text;
    t->pos = make_pos(l, start);

    return t;
}

Token *get_tokens(Lexer *l) {
    Token *tokens = dynarray_create(Token);
    Token *token;

    while (is_eof(l)) {
        token = scan(l);
        dynarray_push(tokens, token);
    }

    token = dynarray_last(tokens);
    if (token->kind != TOKEN_EOF) {
        dynarray_push(tokens, *once(l, TOKEN_EOF));
    }
    return tokens;
}

Token *error(Lexer *l, int start, char *msg) {
    Token *t = malloc(sizeof(Token));
    t->kind = TOKEN_ERROR;
    t->literal = msg;
    t->pos = make_pos(l, start);

    return t;
}

boolean is_eof(Lexer *l) {
    return l->current_position >= strlen(l->input);
}

boolean is_end_of_line(Lexer *l) {
    return l->ch == '\n' || is_eof(l);
}

void bump_line(Lexer *l) {
    l->line += 1;
    l->column = 1;
}

boolean next_token_is_dot_or_multistr(Lexer *l) {
    int pos = l->current_position + 1;
    for (;;) {
        char ch = read_char_at(l, pos);
        if (ch == '\0') {
            // 0 is false
            return 0;
        }
        if (ch == ' ' || ch == '\t' || ch == '\n') {
            pos += 1;
            continue;
        }
        if (ch == '/' && read_char_at(l, pos + 1) == '/') {
            // consume '//'
            pos += 2;

            while (read_char_at(l, pos) != '\n' && pos < strlen(l->input)) {
                pos += 1;
            }
            pos += 1;
            continue;
        }
        return ch == '.' || (ch == '\\' && read_char_at(l, pos + 1) == '\\');
    }
}

char *digits(Lexer *l) {
    char *text = dynarray_create(char *);
    while (l->ch != '\0') {
        if (isdigit(l->ch)) {
            dynarray_push(text, l->ch);
            next(l);
        }
    }
    return text;
}

Token *scan_number(Lexer *l) {
    char *text = dynarray_create(char *);
    TokenKind kind = TOKEN_INT;

    if (l->ch == '.') {
        dynarray_push(text, l->ch);
        next(l);

        char *decimals = digits(l);
        if (decimals == NULL) {
            kind = TOKEN_ERROR;
        } else {
            dynarray_push(text, decimals);
            kind = TOKEN_FLOAT;
        }
    }

    Token *token = malloc(sizeof(Token));
    token->kind = kind;
    token->literal = text;
    token->pos = make_pos(l, start(l));

    return token;
}

Token *scan_ident(Lexer *l) {
    char *text = dynarray_create(char *);

    while (isalnum(l->ch) || l->ch == '_') {
        dynarray_push(text, l->ch);
        next(l);
    }

    TokenKind kind = get_keyword(text);
    if (kind == TOKEN_NULL) {
        kind = TOKEN_IDENT;
    }

    for (int i = 0; i < dynarray_length(text); ++i) {
        if (strcmp(text[i], "true") == 0 || strcmp(text[i], "false") == 0) {
            kind = TOKEN_BOOL;
            break;
        }
    }

    Token *token = malloc(sizeof(Token));
    token->kind = kind;
    token->literal = text;
    token->pos = make_pos(l, start);

    return token;
}

Token *scan_string(Lexer *l) {
    char *text = dynarray_create(char *);
    int start = next(l);
    boolean seen_closing = 0;  // false

    for (;;) {
        if (l->ch == '"') {
            next(l);
            seen_closing = 1;
            break;
        }
        if (is_end_of_line(l)) {
            break;
        }
        dynarray_push(text, l->ch);
        next(l);
    }
    if (!seen_closing) {
        return error(l, start, text);
    }

    Token *token = malloc(sizeof(Token));
    token->kind = TOKEN_STRING;
    token->literal = text;
    token->pos = make_pos(l, start);
}

Token *scan_char(Lexer *l) {
    char *text = dynarray_create(char *);
    int start = next(l);

    while (l->ch != '\'') {
        dynarray_push(text, l->ch);
        next(l);
    }

    next(l);

    Token *token = malloc(sizeof(Token));
    token->kind = TOKEN_CHAR;
    token->literal = text;
    token->pos = make_pos(l, start);
}

Token *scan_slash_comment(Lexer *l) {
    // potentially a division operator
    if (peek_char(l) == '/') {
        int start = next(l);
        Token *token = malloc(sizeof(Token));
        token->kind = TOKEN_SLASH;
        token->literal = l->ch;
        token->pos = make_pos(l, start);
        return token;
    }
    // it's a comment otherwise
    int start = skip(l, 2);
    char *text = dynarray_create(char *);
    while (!is_end_of_line(l)) {
        dynarray_push(text, l->ch);
        next(l);
    }

    Token *token = malloc(sizeof(Token));
    token->kind = TOKEN_COMMENT;
    token->literal = text;
    token->pos = make_pos(l, start);

    return token;
}

Token *scan_multistring(Lexer *l) {
    if (peek_char(l) != '\\') {
        int start = next(l);
        Token *token = malloc(sizeof(Token));
        token->kind = TOKEN_BACKSLASH;
        token->literal = l->ch;
        token->pos = make_pos(l, start);
    }
    // it's a multi-line string
    int start = skip(l, 2);
    char *text = dynarray_create(char *);

    while (is_end_of_line(l)) {
        dynarray_push(text, l->ch);
        next(l);
    }

    Token *token = malloc(sizeof(Token));
    token->kind = TOKEN_MULTISTRING;
    token->literal = text;
    token->pos = make_pos(l, start);

    return token;
}

Token *scan_support_tokens(Lexer *l) {
    // consume '@'
    int start = next(l);
    int ident = scan_ident(l);

    Token *token = malloc(sizeof(Token));
    token->kind = TOKEN_SUPPORT;
    token->literal = ident;
    token->pos = make_pos(l, start);

    return token;
}

Token *scan(Lexer *l) {
    Token *tok = scan_actual(l);
    // update previous token.
    // if we just scanned a comment, keep the previous one around so that the rules for
    // semicolon insertion apply to it
    if (tok->kind == TOKEN_COMMENT) {
        l->prev_token = l->prev_token;
    } else {
        l->prev_token = tok->kind;
    }
    return tok;
}

Token *scan_single_token(Lexer *l) {
    char token_str[256];
    int col = l->column;
    Token *token;

    sprintf(token_str, "%c%c", l->ch, peek_char(l));
    Pos *pos = make_pos(l, col);
    return check_token(token_str, &pos);
}

Token *check_token(char *token_str, Pos *pos) {
    enum TokenType *kind = get_syntax(token_str);
    if (kind != TOKEN_ERROR) {
        Token *token = malloc(sizeof(Token));
        token->kind = kind;
        strcpy(token->literal, token_str);
        token->pos = pos;
    }
    return NULL;
}

Token *scan_actual(Lexer *l) {
    Token *token = skip_whitespace(l);
    if (token != NULL) {
        return token;
    }

    if (is_eof(l)) {
        return once(l, TOKEN_EOF);
    }

    token = scan_single_token(l);
    if (token != NULL) {
        skip(l, strlen(token->literal));
        return token;
    }

    if (isdigit(l->ch)) {
        return scan_number(l);
    }
    if (isalpha(l->ch) || l->ch == '_') {
        return scan_ident(l);
    }
    if (l->ch == '"') {
        return scan_string(l);
    }
    if (l->ch == '\'') {
        return scan_char(l);
    }
    if (l->ch == '/') {
        return scan_slash_comment(l);
    }
    if (l->ch == '\\') {
        return scan_multistring(l);
    }
    if (l->ch == ';') {
        return once(l, TOKEN_SEMICOLON);
    }
    if (l->ch == '@') {
        return scan_support_tokens(l);
    }
    return error(l, start(l), sprintf("unexpected character: %c", l->ch));
}

Token *skip_whitespace(Lexer *l) {
    while (l->ch == ' ' || l->ch == '\t' || l->ch == '\n' || l->ch == '\r') {
        if (l->ch == '\n') {
            next(l);
            continue;
        }
        // We're on a newline, trigger automatic semicolon insertion.
        // This is similar to what the Go scanner does.
        // One (big) difference is that we want to allow DOTs on a newline,
        // whereas Go wants you to put a trailing DOT in a chain of method calls.
        // ie.
        //      fmt.
        //          Println()
        // not:
        //      fmt
        //          .Println()
        //
        // Ideally, we support the latter, although it complicates things a bit.
        // We need to scan (indefinitely) ahead to find the next non whitespace token
        // and check if it's a DOT: in that case, we don't insert the semicolon.
        // Note that other postfix operators like '[index]', '?' and '(call)'
        // need to be on the same line to parse correctly.
        if (insert_semicolor(l->prev_token) && !next_token_is_dot_or_multistr(l)) {
            int start = next(l);

            // consume '\n'
            Token *t = malloc(sizeof(Token));
            t->kind = TOKEN_SEMICOLON;
            t->literal = '\n';
            t->pos = make_pos(l, start);

            bump_line(l);
            return t;
        }
        // consume '\n'
        next(l);
        // And reset line + col
        bump_line(l);
    }
    return NULL;
}