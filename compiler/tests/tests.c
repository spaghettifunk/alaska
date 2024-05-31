#include <stdbool.h>
#include <stdio.h>

#include "../src/lexer/lexer.h"

void test_get_keyword() {
    char *keywords[] = {
        "fn", "let", "if", "else", "match", "enum", "struct", "type", "interface",
        "impl", "const", "return", "defer", "use", "spawn", "mut", "for", "in",
        "while", "loop", "break", "continue", "select"};
    int numKeywords = sizeof(keywords) / sizeof(keywords[0]);

    for (int i = 0; i < numKeywords; i++) {
        TokenKind kind = get_keyword(keywords[i]);
        printf("Keyword: %s, Kind: %d\n", keywords[i], kind);
    }
}

void test_get_syntax() {
    char *syntax[] = {
        "+", "-", "*", "/", "%", "=", "==", "!=", "<", "<=", ">", ">=",
        "(", ")", "{", "}", "[", "]", ",", ".", ":", ";", "->", "=>",
        "|", "||", "&", "&&", "^", "%", "!", "?"};
    int numSyntax = sizeof(syntax) / sizeof(syntax[0]);

    for (int i = 0; i < numSyntax; i++) {
        TokenKind kind = get_syntax(syntax[i]);
        printf("Syntax: %s, Kind: %d\n", syntax[i], kind);
    }
}

void test_insert_semicolon() {
    TokenKind kinds[] = {
        TOKEN_IDENT, TOKEN_INT, TOKEN_FLOAT, TOKEN_STRING, TOKEN_CHAR,
        TOKEN_BOOL, TOKEN_RPAREN, TOKEN_RBRACK, TOKEN_MULTISTRING,
        TOKEN_BREAK, TOKEN_CONTINUE, TOKEN_RETURN, TOKEN_RANGLE,
        TOKEN_QUESTION, TOKEN_RCURLY};
    int numKinds = sizeof(kinds) / sizeof(kinds[0]);

    for (int i = 0; i < numKinds; i++) {
        bool insert = insert_semicolon(kinds[i]);
        printf("Kind: %d, Insert Semicolon: %s\n", kinds[i], insert ? "true" : "false");
    }
}

int main() {
    test_get_keyword();
    test_get_syntax();
    test_insert_semicolon();

    return 0;
}