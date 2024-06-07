// AST implementation of the Alaska grammar for the compiler
#pragma once
#include <stdio.h>
#include <stdlib.h>

#include "../lib/containers/list.h"

// Enumeration for different node types
typedef enum NodeType {
    NODE_SOURCEFILE,
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
    NODE_PACKAGENAME,
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

/*
identifier ::= letter ( letter | decimal_digit )*
*/
typedef struct AST_IDENTIFIER {
    char *name;
} AST_IDENTIFIER;

/*
unary_op ::= '+' | '-' | '!' | '^' | '*' | '&'
*/
typedef struct AST_UNARY_OPERATOR {
    char *operator;
} AST_UNARY_OPERATOR;

/*
rel_op ::= '==' | '!=' | '<' | '<=' | '>' | '>='
*/
typedef struct AST_REL_OPERATOR {
    char *operator;
} AST_REL_OPERATOR;

/*
add_op ::= '+' | '-' | '|' | '^'
*/
typedef struct AST_ADD_OPERATOR {
    char *operator;
} AST_ADD_OPERATOR;

/*
mul_op   ::= '*' | '/' | '%' | '<<' | '>>' | '&'
*/
typedef struct AST_MUL_OPERATOR {
    char *operator;
} AST_MUL_OPERATOR;

/*
binary_op ::= '||' | '&&' | rel_op | add_op | mul_op
*/
typedef struct AST_BINARY_OPERATOR {
    char *operator;
    AST_REL_OPERATOR *rel_operator;
    AST_ADD_OPERATOR *add_operator;
    AST_MUL_OPERATOR *mul_operator;
} AST_BINARY_OPERATOR;

/*
packageName ::= identifier
*/
typedef struct AST_PACKAGENAME {
    AST_IDENTIFIER *identifier;
} AST_PACKAGENAME;

/*
packageClause ::= 'package' packageName
*/
typedef struct AST_PACKAGE_CLAUSE {
    AST_PACKAGENAME *package_name;
} AST_PACKAGE_CLAUSE;

/*
usePath ::= string_lit
*/
typedef struct AST_USE_PATH {
    AST_IDENTIFIER *identifier;
} AST_USE_PATH;

/*
useDecl ::= 'use' usePath
*/
typedef struct AST_USE_DECL {
    AST_USE_PATH *use_path;
} AST_USE_DECL;

/*
sourceFile ::= packageClause ';' ( useDecl ';' )* ( topLevelDecl ';' )*
*/
typedef struct AST_SOURCEFILE {
    AST_PACKAGE_CLAUSE *package_clause;
    AST_USE_DECL *use_declarations;
    // top level declarations
    // here...
} AST_SOURCEFILE;

typedef struct AST {
    enum NodeType type;
    AST_SOURCEFILE *sourcefile;
} AST;

AST *new_ast(enum NodeType type);