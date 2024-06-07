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

AST_USE_DECL *parse_use_declaration(Parser *parser) {
    AST_USE_DECL *ast_use_decl = (AST_USE_DECL *)malloc(sizeof(AST_USE_DECL));
    parser_consume(parser, TOKEN_USE);
    parser_consume(parser, TOKEN_IDENTIFIER);
    return ast_use_decl;
}

AST_PACKAGE_CLAUSE *parse_package_clause(Parser *parser) {
    AST_PACKAGE_CLAUSE *ast_pkg_clause = (AST_PACKAGE_CLAUSE *)malloc(sizeof(AST_PACKAGE_CLAUSE));

    parser_consume(parser, TOKEN_PACKAGE);
    parser_consume(parser, TOKEN_IDENTIFIER);

    return ast_pkg_clause;
}

AST *parse_sourcefile(Parser *parser) {
    AST *ast = new_ast(NODE_SOURCEFILE);
    ast->sourcefile = (AST_SOURCEFILE *)malloc(sizeof(AST_SOURCEFILE));
    AST_PACKAGE_CLAUSE *pkg_clause = parse_package_clause(parser);
    ast->sourcefile->package_clause = pkg_clause;
    // ast->sourcefile->use_declarations = parse_use_declarations(parser);
    return ast;
}

AST *parser_begin(Parser *parser) {
    return parse_sourcefile(parser);
}