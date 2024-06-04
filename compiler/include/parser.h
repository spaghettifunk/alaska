#include "defines.h"
#include "lexer.h"

typedef struct {
    Lexer *lexer;
    Token *current_token;
    Token *previous_token;
} Parser;

Parser *new_parser(Lexer *lexer);