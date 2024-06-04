#include "../include/ast.h"

#include "../include/defines.h"

ASTNode *create_syntax_node(ASTNode **productions, size_t production_count) {
    // allocate memory for the syntax node
    ASTSyntax *syntax_node = (ASTSyntax *)malloc(sizeof(ASTSyntax));
    if (syntax_node == NULL) {
        fprintf(stderr, "Not enough memory to create syntax node.\n");
        exit(74);
    }

    // allocate memory for the productions array
    syntax_node->productions = (ASTNode **)malloc(production_count * sizeof(ASTNode *));
    if (syntax_node->productions == NULL) {
        fprintf(stderr, "Not enough memory to create productions array.\n");
        free(syntax_node);
        exit(74);
    }

    // copy the productions into the productions array
    for (size_t i = 0; i < production_count; i++) {
        syntax_node->productions[i] = productions[i];
    }

    syntax_node->production_count = production_count;

    // allocate memory for the ASTNode wrapper
    ASTNode *node = (ASTNode *)malloc(sizeof(ASTNode));
    if (node == NULL) {
        fprintf(stderr, "Not enough memory to create ASTNode.\n");
        free(syntax_node->productions);
        free(syntax_node);
        exit(74);
    }

    // initialize the ASTNode wrapper
    node->type = (struct ASTType *)malloc(sizeof(struct ASTType *));
    node->type->type = NODE_SYNTAX;
    node->syntax = syntax_node;

    return node;
}

ASTNode *create_production_node(char *name, ASTExpression *expression) {
    return NULL;
}

ASTNode *create_expression_node(ASTTerm **terms, size_t term_count) {
    return NULL;
}

ASTNode *create_term_node(ASTFactor **factors, size_t factor_count) {
    return NULL;
}

ASTNode *create_factor_node(NodeType type, void *data) {
    return NULL;
}

ASTNode *create_group_node(ASTExpression *expression) {
    return NULL;
}

ASTNode *create_option_node(ASTExpression *expression) {
    return NULL;
}

ASTNode *create_repetition_node(ASTExpression *expression) {
    return NULL;
}

// ASTNode *create_statement_node(NodeType type, void *data);
// ASTNode *create_declaration_node(NodeType type, void *data);
// ASTNode *create_literal_node(NodeType type, void *data);
// ASTNode *create_block_node(ASTStatement **statements, size_t statement_count);
// ASTNode *create_if_stmt_node(ASTExpression *condition, ASTBlock *then_block, ASTBlock *else_block);
// ASTNode *create_for_stmt_node(ASTExpression *condition, ASTBlock *body);
// ASTNode *create_return_stmt_node(ASTExpression **expressions, size_t expression_count);
// ASTNode *create_expression_stmt_node(ASTExpression *expression);
// ASTNode *create_assignment_node(ASTExpression **lhs, size_t lhs_count, ASTExpression **rhs, size_t rhs_count, char *assign_op);
// ASTNode *create_type_node(char *type_name);
// ASTNode *create_function_decl_node(char *name, ASTType *return_type, ASTParameter **parameters, size_t parameter_count, ASTBlock *body);
// ASTNode *create_package_clause_node(char *package_name);
// ASTNode *create_use_decl_node(char *path);

// ASTNode *create_primary_expr_node(NodeType type, void *data);
// ASTNode *create_unary_expr_node(char unary_op, ASTExpression *operand);
// ASTNode *create_binary_expr_node(ASTExpression *left, char *binary_op, ASTExpression *right);
// ASTNode *create_function_lit_node(ASTFunctionDecl *function_decl);
// ASTNode *create_parameter_node(char *name, ASTType *type);
// ASTNode *create_parameter_list_node(ASTParameter **parameters, size_t parameter_count);
// ASTNode *create_type_assertion_node(ASTExpression *expression, ASTType *type);
// ASTNode *create_selector_node(ASTExpression *operand, char *field);
// ASTNode *create_index_node(ASTExpression *operand, ASTExpression *index);
// ASTNode *create_slice_node(ASTExpression *operand, ASTExpression *low, ASTExpression *high, ASTExpression *max);
// ASTNode *create_arguments_node(ASTExpression **arguments, size_t argument_count);
// ASTNode *create_conversion_node(ASTType *type, ASTExpression *expression);
// ASTNode *create_receiver_type_node(ASTType *type);

// ASTNode *create_type_name_node(char *identifier);
// ASTNode *create_type_args_node(ASTTypeList *type_list);
// ASTNode *create_type_list_node(ASTType **types, size_t type_count);
// ASTNode *create_array_type_node(ASTExpression *length, ASTType *element_type);
// ASTNode *create_struct_type_node(ASTFieldDecl **fields, size_t field_count);
// ASTNode *create_pointer_type_node(ASTType *base_type);
// ASTNode *create_function_type_node(ASTParameterList *parameters, ASTType *return_type);
// ASTNode *create_interface_type_node(ASTMethod **methods, size_t method_count);
// ASTNode *create_slice_type_node(ASTType *element_type);
// ASTNode *create_map_type_node(ASTType *key_type, ASTType *element_type);

// ASTNode *create_defer_stmt_node(ASTExpression *expression);
// ASTNode *create_labeled_stmt_node(char *label, ASTStatement *statement);
// ASTNode *create_simple_stmt_node(NodeType type, void *data);
// ASTNode *create_const_decl_node(ASTIdentifierList *identifiers, ASTType *type, ASTExpressionList *expression_list);
// ASTNode *create_type_decl_node(ASTTypeSpec **type_specs, size_t type_spec_count);
// ASTNode *create_let_decl_node(ASTIdentifierList *identifiers, ASTType *type, ASTExpressionList *expression_list);
// ASTNode *create_top_level_decl_node(NodeType type, void *data);

// ASTNode *create_package_clause_node(char *package_name);
// ASTNode *create_use_decl_node(char *path);

void ast_print(ASTNode *ast) {
    // Implement printing of the AST
    printf("AST\n");
}