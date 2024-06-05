#include "ast.h"
#include "defines.h"
#include "lexer.h"

typedef struct {
    Lexer *lexer;
    Token *current_token;
} Parser;

Parser *new_parser(Lexer *lexer);
Token *parser_consume(Parser *parser, enum TokenKind kind);
AST *parser_parse(Parser *parser);
AST *parser_parse_syntax(Parser *parser);