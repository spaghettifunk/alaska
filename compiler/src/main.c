#include "../include/ast.h"
#include "../include/defines.h"
#include "../include/parser.h"
#include "../lib/containers/list.h"

char *readFile(const char *path) {
    FILE *file = fopen(path, "rb");
    if (file == NULL) {
        fprintf(stderr, "Could not open file \"%s\".\n", path);
        exit(74);
    }

    fseek(file, 0L, SEEK_END);
    size_t fileSize = ftell(file);
    rewind(file);

    char *buffer = (char *)malloc(fileSize + 1);
    if (buffer == NULL) {
        fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
        exit(74);
    }

    size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
    if (bytesRead < fileSize) {
        fprintf(stderr, "Could not read file \"%s\".\n", path);
        exit(74);
    }

    buffer[bytesRead] = '\0';

    fclose(file);
    return buffer;
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
        return 64;
    }

    Lexer *l = new_lexer(code);
    Parser *p = new_parser(l);
    AST *root = parser_begin(p);

    printf("Root node type: %p\n", root);

    // scan_tokens(l);

    // list_node_t *node;
    // list_iterator_t *it = list_iterator_new(l->tokens, LIST_HEAD);
    // while ((node = list_iterator_next(it))) {
    //     Token *tok = (Token *)node->val;
    //     puts(tok->string);
    // }

    return 0;
}