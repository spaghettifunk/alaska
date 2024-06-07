#include "../include/ast.h"

#include "../include/defines.h"

AST *new_ast(enum NodeType type) {
    AST *ast = (AST *)malloc(sizeof(AST));
    ast->type = type;
    return ast;
}