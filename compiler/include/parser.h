#include "ast.h"
#include "defines.h"
#include "lexer.h"

typedef struct {
    Lexer *lexer;
    Token *current_token;
} Parser;

Parser *new_parser(Lexer *lexer);
Token *parser_consume(Parser *parser, enum TokenKind kind);
AST *parser_begin(Parser *parser);
AST *parse_sourcefile(Parser *parser);
AST_PACKAGE_CLAUSE *parse_package_clause(Parser *parser);
AST_USE_DECL *parse_use_declaration(Parser *parser);