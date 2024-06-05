// AST implementation of the Alaska grammar for the compiler
#pragma once
#include <stdio.h>
#include <stdlib.h>

#include "../lib/containers/list.h"

// Enumeration for different node types
typedef enum NodeType {
    NODE_SYNTAX,
    NODE_PRODUCTION,
    NODE_EXPRESSION,
    NODE_TERM,
    NODE_FACTOR,
    NODE_GROUP,
    NODE_OPTION,
    NODE_REPETITION,
    NODE_IDENTIFIER,
    NODE_TOKEN,
    NODE_STATEMENT,
    NODE_DECLARATION,
    NODE_LITERAL,
    NODE_PARAMETER,
    NODE_BLOCK,
    NODE_IF_STMT,
    NODE_FOR_STMT,
    NODE_RETURN_STMT,
    NODE_EXPRESSION_STMT,
    NODE_ASSIGNMENT,
    NODE_TYPE,
    NODE_FUNCTION_DECL,
    NODE_PACKAGE_CLAUSE,
    NODE_USE_DECL,
    NODE_PRIMARY_EXPR,
    NODE_UNARY_EXPR,
    NODE_BINARY_EXPR,
    NODE_FUNCTION_LIT,
    NODE_PARAMETER_LIST,
    NODE_TYPE_ASSERTION,
    NODE_SELECTOR,
    NODE_INDEX,
    NODE_SLICE,
    NODE_ARGUMENTS,
    NODE_CONVERSION,
    NODE_RECEIVER_TYPE,
    NODE_TYPE_NAME,
    NODE_TYPE_ARGS,
    NODE_TYPE_LIST,
    NODE_ARRAY_TYPE,
    NODE_STRUCT_TYPE,
    NODE_POINTER_TYPE,
    NODE_FUNCTION_TYPE,
    NODE_INTERFACE_TYPE,
    NODE_SLICE_TYPE,
    NODE_MAP_TYPE,
} NodeType;

// Forward declarations of AST nodes
typedef struct ASTNode ASTNode;
typedef struct ASTProduction ASTProduction;
typedef struct ASTExpression ASTExpression;
typedef struct ASTTerm ASTTerm;
typedef struct ASTFactor ASTFactor;
typedef struct ASTGroup ASTGroup;
typedef struct ASTOption ASTOption;
typedef struct ASTRepetition ASTRepetition;
typedef struct ASTStatement ASTStatement;
typedef struct ASTDeclaration ASTDeclaration;
typedef struct ASTLiteral ASTLiteral;
typedef struct ASTBlock ASTBlock;
typedef struct ASTIfStmt ASTIfStmt;
typedef struct ASTForStmt ASTForStmt;
typedef struct ASTReturnStmt ASTReturnStmt;
typedef struct ASTExpressionStmt ASTExpressionStmt;
typedef struct ASTAssignment ASTAssignment;
typedef struct ASTType ASTType;
typedef struct ASTFunctionDecl ASTFunctionDecl;
typedef struct ASTPackageClause ASTPackageClause;
typedef struct ASTUseDecl ASTUseDecl;
typedef struct ASTParameter ASTParameter;
typedef struct ASTPrimaryExpr ASTPrimaryExpr;
typedef struct ASTUnaryExpr ASTUnaryExpr;
typedef struct ASTBinaryExpr ASTBinaryExpr;
typedef struct ASTFunctionLit ASTFunctionLit;
typedef struct ASTParameterList ASTParameterList;
typedef struct ASTTypeAssertion ASTTypeAssertion;
typedef struct ASTSelector ASTSelector;
typedef struct ASTIndex ASTIndex;
typedef struct ASTSlice ASTSlice;
typedef struct ASTArguments ASTArguments;
typedef struct ASTConversion ASTConversion;
typedef struct ASTReceiverType ASTReceiverType;
typedef struct ASTTypeName ASTTypeName;
typedef struct ASTTypeArgs ASTTypeArgs;
typedef struct ASTTypeList ASTTypeList;
typedef struct ASTArrayType ASTArrayType;
typedef struct ASTStructType ASTStructType;
typedef struct ASTPointerType ASTPointerType;
typedef struct ASTFunctionType ASTFunctionType;
typedef struct ASTInterfaceType ASTInterfaceType;
typedef struct ASTSliceType ASTSliceType;
typedef struct ASTMapType ASTMapType;
typedef struct ASTMethod ASTMethod;
typedef struct ASTFieldDecl ASTFieldDecl;
typedef struct ASTConstDecl ASTConstDecl;
typedef struct ASTIdentifierList ASTIdentifierList;
typedef struct ASTExpressionList ASTExpressionList;
typedef struct ASTTypeSpec ASTTypeSpec;
typedef struct ASTMethodDecl ASTMethodDecl;

// Structure for syntax node
typedef struct AST {
    list_t *productions;  // Array of productions
    enum NodeType type;
} AST;

// Structure for production node
typedef struct ASTProduction {
    char *name;  // Production name
    ASTExpression *expression;
} ASTProduction;

// Structure for term node
typedef struct ASTTerm {
    ASTFactor **factors;  // Array of factors
    size_t factor_count;
} ASTTerm;

// Structure for factor node
typedef struct ASTFactor {
    NodeType type;
    union {
        char *production_name;
        struct {
            char *token_start;
            char *token_end;
        } token_range;
        ASTGroup *group;
        ASTOption *option;
        ASTRepetition *repetition;
    };
} ASTFactor;

// Structure for group node
typedef struct ASTGroup {
    ASTExpression *expression;
} ASTGroup;

// Structure for option node
typedef struct ASTOption {
    ASTExpression *expression;
} ASTOption;

// Structure for repetition node
typedef struct ASTRepetition {
    ASTExpression *expression;
} ASTRepetition;

typedef struct ASTStatement {
    NodeType type;
    union {
        ASTBlock *block;
        ASTIfStmt *if_stmt;
        ASTForStmt *for_stmt;
        ASTReturnStmt *return_stmt;
        ASTExpressionStmt *expression_stmt;
        ASTAssignment *assignment;
        // Add more as necessary
    };
} ASTStatement;

// Structure for declaration node
typedef struct ASTDeclaration {
    NodeType type;
    union {
        ASTFunctionDecl *function_decl;
        // Add more as necessary
    };
} ASTDeclaration;

// Structure for literal node
typedef struct ASTLiteral {
    NodeType type;
    union {
        int int_lit;
        double float_lit;
        char *string_lit;
        char rune_lit;
        // Add more as necessary
    };
} ASTLiteral;

// Structure for block node
typedef struct ASTBlock {
    ASTStatement **statements;  // Array of statements
    size_t statement_count;
} ASTBlock;

// Structure for if statement node
typedef struct ASTIfStmt {
    ASTExpression *condition;
    ASTBlock *then_block;
    ASTBlock *else_block;  // Optional
} ASTIfStmt;

// Structure for for statement node
typedef struct ASTForStmt {
    ASTExpression *condition;
    ASTBlock *body;
} ASTForStmt;

// Structure for return statement node
typedef struct ASTReturnStmt {
    ASTExpression **expressions;  // Array of expressions
    size_t expression_count;
} ASTReturnStmt;

// Structure for expression statement node
typedef struct ASTExpressionStmt {
    ASTExpression *expression;
} ASTExpressionStmt;

// Structure for assignment node
typedef struct ASTAssignment {
    ASTExpression **lhs;  // Array of left-hand side expressions
    size_t lhs_count;
    ASTExpression **rhs;  // Array of right-hand side expressions
    size_t rhs_count;
    char *assign_op;  // Assignment operator (e.g., '=', '+=', etc.)
} ASTAssignment;

// Structure for type node
typedef struct ASTType {
    NodeType type;
    char *type_name;  // Type name (e.g., int, float)
} ASTType;

// Structure for function declaration node
typedef struct ASTFunctionDecl {
    char *name;
    ASTType *return_type;
    ASTParameter **parameters;  // Array of parameters
    size_t parameter_count;
    ASTBlock *body;  // Function body
} ASTFunctionDecl;

// Structure for use declaration node
typedef struct ASTUseDecl {
    char *path;
} ASTUseDecl;

// Structure for primary expression node
typedef struct ASTPrimaryExpr {
    NodeType type;
    union {
        ASTLiteral *literal;
        char *operand_name;
        ASTExpression *expression;
    };
} ASTPrimaryExpr;

// Structure for unary expression node
typedef struct ASTUnaryExpr {
    char unary_op;  // Unary operator (e.g., '+', '-', '!')
    ASTExpression *operand;
} ASTUnaryExpr;

// Structure for binary expression node
typedef struct ASTBinaryExpr {
    ASTExpression *left;
    char *binary_op;  // Binary operator (e.g., '||', '&&', '==', etc.)
    ASTExpression *right;
} ASTBinaryExpr;

// Structure for function literal node
typedef struct ASTFunctionLit {
    ASTFunctionDecl *function_decl;
} ASTFunctionLit;

// Structure for parameter node
typedef struct ASTParameter {
    char *name;
    ASTType *type;
} ASTParameter;

// Structure for parameter list node
typedef struct ASTParameterList {
    ASTParameter **parameters;  // Array of parameters
    size_t parameter_count;
} ASTParameterList;

// Structure for type assertion node
typedef struct ASTTypeAssertion {
    ASTExpression *expression;
    ASTType *type;
} ASTTypeAssertion;

// Structure for selector node
typedef struct ASTSelector {
    ASTExpression *operand;
    char *field;
} ASTSelector;

// Structure for index node
typedef struct ASTIndex {
    ASTExpression *operand;
    ASTExpression *index;
} ASTIndex;

// Structure for slice node
typedef struct ASTSlice {
    ASTExpression *operand;
    ASTExpression *low;   // Optional
    ASTExpression *high;  // Optional
    ASTExpression *max;   // Optional
} ASTSlice;

// Structure for arguments node
typedef struct ASTArguments {
    ASTExpression **arguments;  // Array of arguments
    size_t argument_count;
} ASTArguments;

// Structure for conversion node
typedef struct ASTConversion {
    ASTType *type;
    ASTExpression *expression;
} ASTConversion;

// Structure for receiver type node
typedef struct ASTReceiverType {
    ASTType *type;
} ASTReceiverType;

// Structure for type name node
typedef struct ASTTypeName {
    char *identifier;
} ASTTypeName;

// Structure for type arguments node
typedef struct ASTTypeArgs {
    ASTTypeList *type_list;
} ASTTypeArgs;

// Structure for type list node
typedef struct ASTTypeList {
    ASTType **types;  // Array of types
    size_t type_count;
} ASTTypeList;

// Structure for array type node
typedef struct ASTArrayType {
    ASTExpression *length;
    ASTType *element_type;
} ASTArrayType;

// Structure for struct type node
typedef struct ASTStructType {
    ASTFieldDecl **fields;  // Array of field declarations
    size_t field_count;
} ASTStructType;

// Structure for pointer type node
typedef struct ASTPointerType {
    ASTType *base_type;
} ASTPointerType;

// Structure for function type node
typedef struct ASTFunctionType {
    ASTParameterList *parameters;
    ASTType *return_type;
} ASTFunctionType;

// Structure for interface type node
typedef struct ASTInterfaceType {
    ASTMethod **methods;  // Array of methods
    size_t method_count;
} ASTInterfaceType;

// Structure for slice type node
typedef struct ASTSliceType {
    ASTType *element_type;
} ASTSliceType;

// Structure for map type node
typedef struct ASTMapType {
    ASTType *key_type;
    ASTType *element_type;
} ASTMapType;

// Structure for defer statement node
typedef struct ASTDeferStmt {
    ASTExpression *expression;
} ASTDeferStmt;

// Structure for labeled statement node
typedef struct ASTLabeledStmt {
    char *label;
    ASTStatement *statement;
} ASTLabeledStmt;

// Structure for simple statement node
typedef struct ASTSimpleStmt {
    NodeType type;
    union {
        ASTExpressionStmt *expression_stmt;
        ASTAssignment *assignment;
        // Add more as necessary
    };
} ASTSimpleStmt;

// Structure for const declaration node
typedef struct ASTConstDecl {
    ASTIdentifierList *identifiers;
    ASTType *type;
    ASTExpressionList *expression_list;
} ASTConstDecl;

// Structure for type declaration node
typedef struct ASTTypeDecl {
    ASTTypeSpec **type_specs;  // Array of type specifications
    size_t type_spec_count;
} ASTTypeDecl;

// Structure for let declaration node
typedef struct ASTLetDecl {
    ASTIdentifierList *identifiers;
    ASTType *type;
    ASTExpressionList *expression_list;
} ASTLetDecl;

typedef struct ASTPackageClause {
    char *package_name;
} ASTPackageClause;

typedef struct ASTExpression {
    ASTTerm **terms;  // Array of terms
    size_t term_count;
    NodeType type;
    union {
        ASTPrimaryExpr *primary_expr;
        ASTUnaryExpr *unary_expr;
        ASTBinaryExpr *binary_expr;
        ASTFunctionLit *function_lit;
        ASTTypeAssertion *type_assertion;
        ASTSelector *selector;
        ASTIndex *index;
        ASTSlice *slice;
        ASTArguments *arguments;
        ASTConversion *conversion;
    };
} ASTExpression;

// Structure for top-level declaration node
typedef struct ASTTopLevelDecl {
    NodeType type;
    union {
        ASTConstDecl *const_decl;
        ASTTypeDecl *type_decl;
        ASTLetDecl *let_decl;
        ASTFunctionDecl *function_decl;
        ASTMethodDecl *method_decl;
    };
} ASTTopLevelDecl;

// Main AST node structure for extended nodes
typedef struct ASTNode {
    NodeType ext_type;
    union {
        ASTTopLevelDecl *top_level_decl;
        AST *syntax;
        ASTProduction *production;
        ASTExpression *expression;
        ASTTerm *term;
        ASTFactor *factor;
        ASTGroup *group;
        ASTOption *option;
        ASTRepetition *repetition;
        ASTStatement *statement;
        ASTDeclaration *declaration;
        ASTLiteral *literal;
        ASTBlock *block;
        ASTIfStmt *if_stmt;
        ASTForStmt *for_stmt;
        ASTReturnStmt *return_stmt;
        ASTExpressionStmt *expression_stmt;
        ASTAssignment *assignment;
        ASTType *type;
        ASTFunctionDecl *function_decl;
        ASTPackageClause *package_clause;
        ASTUseDecl *use_decl;
    };
} ASTNode;

AST *new_ast(enum NodeType type);
ASTNode *create_production_node(char *name, ASTExpression *expression);
ASTNode *create_expression_node(ASTTerm **terms, size_t term_count);
ASTNode *create_term_node(ASTFactor **factors, size_t factor_count);
ASTNode *create_factor_node(NodeType type, void *data);
ASTNode *create_group_node(ASTExpression *expression);
ASTNode *create_option_node(ASTExpression *expression);
ASTNode *create_repetition_node(ASTExpression *expression);

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