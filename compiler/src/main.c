#include <stdio.h>
#include <stdlib.h>

#include "lexer/lexer.h"

char *readFile(char *fileName) {
    FILE *file = fopen(fileName, "r");
    char *code;
    size_t n = 0;
    int c;

    if (file == NULL) {
        return NULL;  // could not open file
    }

    code = malloc(1000);
    while ((c = fgetc(file)) != EOF) {
        code[n++] = (char)c;
    }
    // don't forget to terminate with the null character
    code[n] = '\0';

    return code;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("No arguments\n");
        return 1;
    }

    char *fileName = argv[1];
    char *code = readFile(fileName);
    if (code == NULL) {
        printf("Could not open file\n");
        return 1;
    }

    Lexer *l = new_lexer(code);
    l->input = code;

    Token *tokens = get_tokens(l);
    while (tokens->kind != TOKEN_EOF) {
        printf("Token: %s\n", tokens->literal);
        tokens++;
    }
}