#include "../include/parser.h"

#include "../lib/containers/list.h"

Parser *new_parser(Lexer *lexer) {
    Parser *parser = malloc(sizeof(Parser));
    parser->lexer = lexer;
    parser->current_token = next_token(lexer);
    return parser;
}

Token *parser_consume(Parser *parser, enum TokenKind kind) {
    if (parser->current_token->kind != kind) {
        printf("Unexpected token: `%s`, was expecting: `%s`\n", parser->current_token->lexeme, token_kind_to_string(kind));
        exit(1);
    }
    parser->current_token = next_token(parser->lexer);
    return parser->current_token;
}

AST *parser_parse(Parser *parser) {
    return new_ast(NODE_SYNTAX);
}

AST *parser_parse_syntax(Parser *parser) {
    AST *ast = new_ast(NODE_SYNTAX);
    while (parser->current_token->kind != TOKEN_EOF) {
        AST *child = parser_parse(parser);
        list_rpush(ast->productions, list_node_new(child));
    }
    return ast;
}