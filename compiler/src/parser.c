#include "../include/parser.h"

#include "../lib/containers/dynarray.h"

Token *advance_token(Parser *parser) {
    if (parser->lexer->current < (int)strlen(parser->lexer->source)) {
        parser->previous_token = parser->current_token;
        parser->current_token = &parser->lexer->tokens[parser->lexer->current++];
    }
    return parser->current_token;
}

boolean match_token(Parser *parser, enum TokenKind kind) {
    if (parser->current_token->kind == kind) {
        advance_token(parser);
        return true;
    }
    return false;
}

boolean check(Parser *parser, enum TokenKind kind) {
    return parser->current_token->kind == kind;
}

Parser *new_parser(Lexer *lexer) {
    Parser *parser = (Parser *)malloc(sizeof(Parser));
    if (parser == NULL) {
        fprintf(stderr, "Not enough memory to create parser.\n");
        exit(74);
    }

    parser->lexer = lexer;
    parser->current_token = &lexer->tokens[0];
    parser->previous_token = NULL;

    return parser;
}