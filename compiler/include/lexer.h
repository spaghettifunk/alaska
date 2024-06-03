#include "defines.h"

typedef enum TokenKind {
    // Literals
    TOKEN_INT,
    TOKEN_STRING,
    TOKEN_CHAR,
    TOKEN_BOOL,
    TOKEN_FLOAT,
    TOKEN_IDENTIFIER,
    TOKEN_COMMENT,
    TOKEN_MULTISTRING,
    TOKEN_SEMICOLON,
    TOKEN_LPAREN,
    TOKEN_RPAREN,
    TOKEN_LBRACKET,
    TOKEN_RBRACKET,
    TOKEN_LBRACE,
    TOKEN_RBRACE,
    // Operators
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
    TOKEN_PIPE_OR,
    TOKEN_AMPERSAND,
    TOKEN_AMPERSAND_AND,
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
    TOKEN_UNDERSCORE,
    // Keywords
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
    TOKEN_USE,
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
    char *lexeme;
    enum TokenKind kind;
    void *literal;
    int line;
} Token;

typedef struct
{
    char *source;
    Token *tokens;
    int start;
    int current;
    int line;
} Lexer;

Lexer *new_lexer(char *source);
void free_lexer(Lexer *l);
void scan_tokens(Lexer *l);
