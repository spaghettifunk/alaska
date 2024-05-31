#pragma once

#ifndef LEXER_H
#define LEXER_H

#include "../defines.h"

typedef enum TokenKind {
    TOKEN_INT,
    TOKEN_STRING,
    TOKEN_CHAR,
    TOKEN_BOOL,
    TOKEN_FLOAT,
    TOKEN_IDENT,
    TOKEN_COMMENT,
    TOKEN_MULTISTRING,

    TOKEN_SEMICOLON,

    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACK,
    TOKEN_RBRACK,
    TOKEN_LCURLY,
    TOKEN_RCURLY,
    TOKEN_LANGLE,
    TOKEN_RANGLE,

    TOKEN_ARROW,
    TOKEN_FATARROW,

    TOKEN_EQ,
    TOKEN_EQ2,
    TOKEN_NOTEQ,
    TOKEN_GTEQ,
    TOKEN_LTEQ,

    TOKEN_COLON,
    TOKEN_PIPE,
    TOKEN_PIPE2,
    TOKEN_AMPERSAND,
    TOKEN_AMPERSAND2,

    TOKEN_PLUS,
    TOKEN_MINUS,
    TOKEN_STAR,
    TOKEN_SLASH,
    TOKEN_BACKSLASH,

    TOKEN_CARET,
    TOKEN_PERCENT,
    TOKEN_BANG,
    TOKEN_QUESTION,
    TOKEN_DOT,
    TOKEN_COMMA,

    TOKEN_FN_,
    TOKEN_LET,
    TOKEN_IF,
    TOKEN_ELSE,
    TOKEN_MATCH,
    TOKEN_ENUM,
    TOKEN_STRUCT,
    TOKEN_TYPE,
    TOKEN_INTERFACE,
    TOKEN_IMPL,
    TOKEN_CONST,
    TOKEN_RETURN,
    TOKEN_DEFER,
    TOKEN_IMPORT,
    TOKEN_SPAWN,
    TOKEN_MUT,
    TOKEN_FOR,
    TOKEN_IN_,
    TOKEN_WHILE,
    TOKEN_LOOP,
    TOKEN_BREAK,
    TOKEN_CONTINUE,
    TOKEN_SELECT,

    TOKEN_SUPPORT,  // @ENSURE, @RAWGO
    TOKEN_ERROR,
    TOKEN_NULL,
    TOKEN_EOF,
} TokenKind;

typedef struct
{
    char *input;
    char ch;
    int current_position;
    enum TokenKind prev_token;
    int line;
    int column;
} Lexer;

typedef struct
{
    int line;
    int column;
    int len;
} Pos;

typedef struct
{
    char *literal;
    enum TokenKind kind;
    Pos *pos;
} Token;

Lexer *new_lexer(char *input);
void free_lexer(Lexer *l);
int start(Lexer *l);
Pos *make_pos(Lexer *l, int start);
char read_char_at(Lexer *l, int offset);

int start(Lexer *l);
int next(Lexer *l);
int skip(Lexer *l, int n);

Token *once(Lexer *l, TokenKind kind);
Token *get_tokens(Lexer *l);
Token *error(Lexer *l, int start, char *msg);
Token *scan_single_token(Lexer *l);
TokenKind get_keyword(char *s);
TokenKind get_syntax(char *s);

boolean insert_semicolon(TokenKind kind);

boolean is_eof(Lexer *l);
boolean is_end_of_line(Lexer *l);

void bump_line(Lexer *l);
boolean next_token_is_dot_or_multistr(Lexer *l);

char *digits(Lexer *l);
Token *scan_number(Lexer *l);
Token *scan_ident(Lexer *l);
Token *scan_string(Lexer *l);
Token *scan_char(Lexer *l);
Token *scan_slash_comment(Lexer *l);
Token *scan_multistring(Lexer *l);
Token *scan_support_tokens(Lexer *l);
Token *scan(Lexer *l);
Token *scan_actual(Lexer *l);

char peek_char(Lexer *l);
Token *skip_whitespace(Lexer *l);

#endif  // LEXER_H