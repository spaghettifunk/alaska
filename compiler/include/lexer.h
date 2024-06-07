#include "../lib/containers/list.h"
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
    TOKEN_ASSIGN,
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
    TOKEN_SHIFTRIGHT,
    TOKEN_SHIFTLEFT,
    TOKEN_DECREMENT,
    TOKEN_INCREMENT,
    TOKEN_ADD_ASSIGN,
    TOKEN_SUB_ASSIGN,
    TOKEN_MUL_ASSIGN,
    TOKEN_DIV_ASSIGN,
    TOKEN_MOD_ASSIGN,
    TOKEN_AND_ASSIGN,
    TOKEN_OR_ASSIGN,
    TOKEN_XOR_ASSIGN,
    TOKEN_SHL_ASSIGN,
    TOKEN_SHR_ASSIGN,
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
    TOKEN_ERROR,
    TOKEN_NULL,
    TOKEN_PACKAGE,
    TOKEN_EOF,
} TokenKind;

typedef struct
{
    char *lexeme;
    enum TokenKind kind;
    void *literal;
    int line;
    char *string;  // this is only for debugging purposes
} Token;

typedef struct
{
    char *source;
    list_t *tokens;
    int start;
    int current;
    int line;
    int token_scanned;
} Lexer;

Lexer *new_lexer(char *source);
void free_lexer(Lexer *l);
Token *next_token(Lexer *l);
char *token_to_string(Token *token);
char *token_kind_to_string(enum TokenKind kind);

// debugging purposes
void scan_tokens(Lexer *l);
