# Alaska Context-Free Grammar

This is the CFG represetation of this new language. It is in Wirth syntax language, a variant of BNF.

```cfg
syntax      = { production } .
production  = production_name "=" [ expression ] "." .
expression  = term { "|" term } .
term        = factor { factor } .
factor      = production_name | token [ "..." token ] | group | option | repetition .
group       = "(" expression ")" .
option      = "[" expression "]" .
repetition  = "{" expression "}" .

newline       = "\n" .
letter        = "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" | "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" | "y" | "z" |
         "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" | "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" | "Y" | "Z" .
decimal_digit = "0" ... "9" .
binary_digit  = "0" | "1" .
octal_digit   = "0" ... "7" .
hex_digit     = "0" ... "9" | "A" ... "F" | "a" ... "f" .

int_lit        = decimal_lit | binary_lit | octal_lit | hex_lit .
decimal_lit    = "0" | ( "1" ... "9" ) [ [ "_" ] decimal_digits ] .
binary_lit     = "0" ( "b" | "B" ) [ "_" ] binary_digits .
octal_lit      = "0" [ "o" | "O" ] [ "_" ] octal_digits .
hex_lit        = "0" ( "x" | "X" ) [ "_" ] hex_digits .

decimal_digits = decimal_digit { [ "_" ] decimal_digit } .
binary_digits  = binary_digit { [ "_" ] binary_digit } .
octal_digits   = octal_digit { [ "_" ] octal_digit } .
hex_digits     = hex_digit { [ "_" ] hex_digit } .

float_lit         = decimal_float_lit | hex_float_lit .
decimal_float_lit = decimal_digits "." [ decimal_digits ] [ decimal_exponent ] |
                    decimal_digits decimal_exponent |
                    "." decimal_digits [ decimal_exponent ] .
decimal_exponent  = ( "e" | "E" ) [ "+" | "-" ] decimal_digits .
hex_float_lit     = "0" ( "x" | "X" ) hex_mantissa hex_exponent .
hex_mantissa      = [ "_" ] hex_digits "." [ hex_digits ] |
                    [ "_" ] hex_digits |
                    "." hex_digits .
hex_exponent      = ( "p" | "P" ) [ "+" | "-" ] decimal_digits .

rune_lit         = "'" ( unicode_value | byte_value ) "'" .
unicode_value    = letter | little_u_value | big_u_value | escaped_char .
byte_value       = octal_byte_value | hex_byte_value .
octal_byte_value = `\` octal_digit octal_digit octal_digit .
hex_byte_value   = `\` "x" hex_digit hex_digit .
little_u_value   = `\` "u" hex_digit hex_digit hex_digit hex_digit .
big_u_value      = `\` "U" hex_digit hex_digit hex_digit hex_digit
                           hex_digit hex_digit hex_digit hex_digit .
escaped_char     = `\` ( "a" | "b" | "f" | "n" | "r" | "t" | "v" | `\` | "'" | `"` ) .

string_lit             = raw_string_lit | interpreted_string_lit .
raw_string_lit         = "`" { letter | newline } "`" .
interpreted_string_lit = `"` { unicode_value | byte_value } `"` .

identifier     = letter { letter | decimal_digit } .

expression = unaryExpr | expression binary_op expression .
unaryExpr  = PrimaryExpr | unary_op unaryExpr .

binary_op  = "||" | "&&" | rel_op | add_op | mul_op .
rel_op     = "==" | "!=" | "<" | "<=" | ">" | ">=" .
add_op     = "+" | "-" | "|" | "^" .
mul_op     = "*" | "/" | "%" | "<<" | ">>" | "&" | "&^" .
unary_op   = "+" | "-" | "!" | "^" | "*" | "&" | "<-" .

type      = typeName [ typeArgs ] | typeLit | "(" type ")" .
typeName  = identifier | qualifiedIdent .
typeArgs  = "[" typeList [ "," ] "]" .
typeList  = type { "," type } .
typeLit   = arrayType | structType | pointerType | functionType | interfaceType |
            sliceType | mapType .
arrayType   = "[" arrayLength "]" elementType .
arrayLength = expression .
elementType = type .
sliceType = "[" "]" elementType .

structType    = "struct" "{" { fieldDecl ";" } "}" .
fieldDecl     = (identifierList type) .

pointerType = "*" baseType .
baseType    = type .

functionType   = "fn" signature .
signature      = parameters [ result ] .
result         = "->" parameters | type .
parameters     = "(" [ parameterList [ "," ] ] ")" .
parameterList  = parameterDecl { "," parameterDecl } .
parameterDecl  = [ identifierList ] [ "..." ] type .

interfaceType  = "interface" "{" { interfaceElem ";" } "}" .
interfaceElem  = methodElem | type .
methodElem     = methodName signature .
methodName     = identifier .

mapType     = "map" "[" keyType "]" elementType .
keyType     = type .

block = "{" statementList "}" .
statementList = { statement ";" } .

declaration   = constDecl | typeDecl | letDecl .
topLevelDecl  = declaration | functionDecl | methodDecl .

constDecl      = "const" ( constSpec | "(" { constSpec ";" } ")" ) .
constSpec      = identifierList [ [ type ] "=" expressionList ] .
identifierList = identifier { "," identifier } .
expressionList = expression { "," expression } .

typeDecl = "type" ( typeSpec | "(" { typeSpec ";" } ")" ) .
typeSpec = aliasDecl | typeDef .
aliasDecl = identifier "=" type .
typeDef = identifier type .

letDeclaration    = 'let' ( letSpec | "(" { letSpec ";" } ")" ) .
letSpec     = identifierList ( type [ "=" expressionList ] | "=" expressionList ) .

functionDecl = "fn" functionName signature [ functionBody ] .
functionName = identifier .
functionBody = block .

qualifiedIdent = packageName "." identifier .

methodDecl = "fn" receiver methodName signature [ functionBody ] .
receiver   = parameters .

operand     = literal | operandName | "(" expression ")" .
literal     = basicLit | compositeLit | functionLit .
basicLit    = int_lit | float_lit | imaginary_lit | rune_lit | string_lit .
operandName = identifier | qualifiedIdent .

functionLit = "fn" signature functionBody .

primaryExpr =
	operand |
	conversion |
	methodExpr |
	primaryExpr selector |
	primaryExpr index |
	primaryExpr slice |
	primaryExpr typeAssertion |
	primaryExpr arguments .

selector       = "." identifier .
index          = "[" expression [ "," ] "]" .
slice          = "[" [ expression ] ":" [ expression ] "]" |
                 "[" [ expression ] ":" expression ":" expression "]" .
typeAssertion  = "." "(" Type ")" .
arguments      = "(" [ ( expressionList | Type [ "," expressionList ] ) [ "..." ] [ "," ] ] ")" .

methodExpr    = receiverType "." methodName .
receiverType  = type .

conversion = type "(" expression [ "," ] ")" .

statement =
	declaration | labeledStmt | simpleStmt |
	returnStmt | breakStmt | continueStmt | gotoStmt |
    block | ifStmt | forStmt |
	deferStmt . // matchStmt

// TODO: match statement
// matchStmt  = "match" scrutinee "{" { innerAttribute* matchArms? } "}" .
// scrutinee  = expression .
// matchArms  = "(" matchArm "=>" "(" expression , | expression block , ? ")" ")"* matchArm "=>" expression , ?
// matchArm   = outerAttribute* pattern matchArmGuard?
// matchArmGuard = "if" expression

simpleStmt = emptyStmt | expressionStmt | sendStmt | incDecStmt | assignment .
emptyStmt = .
labeledStmt = label ":" statement .
label       = identifier .
expressionStmt = expression .
incDecStmt = expression ( "++" | "--" ) .

assignment = expressionList assign_op expressionList .
assign_op = [ add_op | mul_op ] "=" .

ifStmt = "if" "("[ simpleStmt ";" ] expression ")" block [ "else" ( ifStmt | block ) ] .

forStmt = "for" "("[ condition | forClause | rangeClause ] ")" block .
condition = expression .

forClause = [ initStmt ] ";" [ condition ] ";" [ postStmt ] .
initStmt = simpleStmt .
postStmt = simpleStmt .

rangeClause = [ expressionList "=" ] "range" expression .

returnStmt = "return" [ expressionList ] .
breakStmt = "break" [ label ] .
continueStmt = "continue" [ label ] .
gotoStmt = "goto" label .
deferStmt = "defer" expression .

sourceFile = packageClause ";" { useDecl ";" } { topLevelDecl ";" } .

packageClause  = "package" packageName .
packageName    = identifier .

useDecl       = "use" ( useSpec | "(" { useSpec ";" } ")" ) .
useSpec       = [ "." | PackageName ] usePath .
usePath       = string_lit .
```
