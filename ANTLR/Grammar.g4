grammar Grammar;

// Parser rules
program: (statement)* EOF;

statement:
    declaration
    | assignment
    | ifStatement
    | whileStatement
    | printStatement
    | threadCreate
    | threadStart
    | threadJoin
    | globalDecl
    | lockCreate
    | lockFree
    | lockGet
    | scopeBlock
    ;

declaration:
    typeIdentifier ID COLON_PAREN
    ;

assignment:
    ID ASSIGN expr COLON_PAREN
    ;

ifStatement:
    SI expr INICIAMOS block CERRAMOS COLON_PAREN
    ;

whileStatement:
    DURANTE expr INICIAMOS block CERRAMOS COLON_PAREN
    ;

printStatement:
    IMPRIMIR LPAREN expr RPAREN COLON_PAREN
    ;

threadCreate:
    HILO ID ASSIGN INICIAMOS block CERRAMOS COLON_PAREN
    ;

threadStart:
    EMPZAMOS ID COLON_PAREN
    ;

threadJoin:
    ESPERAMOS ID COLON_PAREN
    ;

globalDecl:
    GLOBAL ID COLON_PAREN
    ;

lockCreate:
    ESCLUSA ID COLON_PAREN
    ;

lockFree:
    LIBERAR ID COLON_PAREN
    ;

lockGet:
    OBTENER ID COLON_PAREN
    ;

scopeBlock:
    INICIAMOS block CERRAMOS COLON_PAREN
    ;

block: (statement)*;

typeIdentifier:
    ENTERO | BOOLEANA | ARRAY
    ;

expr: orExpr;
orExpr: andExpr (OR andExpr)*;
andExpr: equalityExpr (AND equalityExpr)*;
equalityExpr: relExpr ((EQUAL | NOT_EQUAL) relExpr)*;
relExpr: addExpr ((LT | LE | GT | GE) addExpr)*;
addExpr: multExpr ((PLUS | MINUS) multExpr)*;
multExpr: unaryExpr (MULT unaryExpr)*;
unaryExpr:
    NOT unaryExpr
    | MINUS unaryExpr
    | primaryExpr
    ;

primaryExpr:
    INT
    | VERDAD
    | MENTIRA
    | ID
    | arrayLiteral
    | LPAREN expr RPAREN
    ;

arrayLiteral:
    LBRACKET (expr (COMMA expr)*)? RBRACKET
    ;

// Lexer rules
SINGLE_LINE_COMMENT: '//' ~[\r\n]* -> skip;
MULTI_LINE_COMMENT: '/*' .*? '*/' -> skip;
WS: [ \t\r\n] -> skip;

ENTERO: 'entero';
BOOLEANA: 'booleana';
ARRAY: 'array';
SI: 'si';
DURANTE: 'durante';
IMPRIMIR: 'imprimir';
HILO: 'hilo';
EMPZAMOS: 'empezamos';
ESPERAMOS: 'esperamos';
GLOBAL: 'global';
ESCLUSA: 'esclusa';
LIBERAR: 'liberar';
OBTENER: 'obtener';
INICIAMOS: 'iniciamos';
CERRAMOS: 'cerramos';

VERDAD: 'verdad';
MENTIRA: 'mentira';

PLUS: '+';
MINUS: '-';
MULT: '*';
EQUAL: '==';
NOT_EQUAL: '!=';
LT: '<';
LE: '<=';
GT: '>';
GE: '>=';
AND: 'Y';
OR: 'O';
NOT: '~:(';
ASSIGN: '=';
COLON_PAREN: ':)';
COMMA: ',';
LBRACKET: '[';
RBRACKET: ']';
LPAREN: '¡';
RPAREN: '!';

ID: [a-zA-Z_][a-zA-Z0-9_]*;
INT: [0-9]+;
