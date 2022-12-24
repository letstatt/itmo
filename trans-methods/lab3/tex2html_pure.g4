grammar tex2html_pure;

@header {
package com.letstatt.antlr;
}

// PARSER RULES

// TeX routines

tex2html:
    header packages body
    EOF;

header:
    DOCUMENTCLASS ARTICLE;

packages:
    (USEPACKAGE LEFT_BRACE WORD RIGHT_BRACE)*;

body:
    BEGIN DOCUMENT
    (text | math)+
    END DOCUMENT;

text:
    (number | word)+;

math:
    DOLLAR expr DOLLAR;

// expressions

expr
    : expr (plus | minus | eq) term
    | term;

term
    : term (mul | div) factor
    | factor;

factor
    : factor sub group sup group
    | factor sup group sub group
    | factor (sup | sub) group
    | group;

group
    : number        // 12345
    | word          // a
    | number word   // 2ab
    | sqrt
    | frac
    | LEFT_PARENTHESES expr RIGHT_PARENTHESES
    | LEFT_BRACE expr RIGHT_BRACE;

plus: PLUS;
minus: MINUS;
eq: EQ;
mul: MULTIPLICATION;
div: DIVISION;
sup: CARET;
sub: UNDERSCORE;
number: NUMBER;
word: WORD;

sqrt: SQRT sqrt_base? LEFT_BRACE expr RIGHT_BRACE;
sqrt_base : LEFT_SQAURE_BRACKET expr RIGHT_SQAURE_BRACKET;

frac: FRAC LEFT_BRACE expr RIGHT_BRACE LEFT_BRACE expr RIGHT_BRACE;

// LEXER RULES

// common tokens
DOLLAR: '$';
UNDERSCORE: '_';
CARET: '^';
LEFT_PARENTHESES: '(';
RIGHT_PARENTHESES: ')';
LEFT_SQAURE_BRACKET: '[';
RIGHT_SQAURE_BRACKET: ']';
LEFT_BRACE: '{';
RIGHT_BRACE: '}';

// arithmetic operators
PLUS: '+';
MINUS: '-';
MULTIPLICATION: '*';
DIVISION: '/';
EQ: '=';

// TeX functions
SQRT: '\\sqrt';
FRAC: '\\frac';

// TeX definitions
DOCUMENTCLASS: '\\documentclass';
ARTICLE: LEFT_BRACE 'article' RIGHT_BRACE;
USEPACKAGE: '\\usepackage';
DOCUMENT: LEFT_BRACE 'document' RIGHT_BRACE;
BEGIN: '\\begin';
END: '\\end';

// others
SPACE: [ \t\r\n]+ -> skip;
WORD: [a-zA-Z][0-9a-zA-Z]*;
NUMBER: '-'? ('0'|[1-9][0-9]*);