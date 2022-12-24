grammar tex2html;

@header {
package com.letstatt.antlr;
}

// PARSER RULES

// TeX routines

tex2html returns [StringBuilder res] @init {
StringBuilder res = new StringBuilder();
$res = res;
}:
    header packages body[res]
    EOF;

header:
    DOCUMENTCLASS ARTICLE;

packages:
    (USEPACKAGE LEFT_BRACE WORD RIGHT_BRACE)*;

body[StringBuilder res]:
    BEGIN DOCUMENT
    {$res.append("<!DOCTYPE html>\n<html lang=\"ru\"><head><title>ANTLR4 example</title></head><body>\n");}
    (
        text[res] |
        math[res]
    )+
    {$res.append("</body></html>\n");}
    END DOCUMENT;

text[StringBuilder res] locals [StringBuilder t] @init {
$t = new StringBuilder();
}
    : {$res.append("<div><p>");}
    (word {$t.append(($t.length() > 0 ? " " : "") + $word.text);}
    | number {$t.append(($t.length() > 0 ? " " : "") + $number.text);})+
    {$res.append($t);
    $res.append("</p></div>\n");};

math[StringBuilder res]:
    DOLLAR {$res.append("<math display=\"block\">");}
    expr[res]
    DOLLAR {$res.append("</math>\n");};

// expressions

expr[StringBuilder res]
    : term[res] expr1[res];

expr1[StringBuilder res]
    : plus {$res.append("<mo>+</mo>");} term[res] expr1[res]
    | eq {$res.append("<mo>=</mo>");} term[res] expr1[res]
    | minus {$res.append("<mo>-</mo>");} term[res] expr1[res]
    | /* epsilon */;

term[StringBuilder res]
    : factor[res] term1[res];

term1[StringBuilder res]
    : mul {$res.append("<mo>*</mo>");} factor[res] term1[res]
    | div {$res.append("<mo>/</mo>");} factor[res] term1[res]
    | /* epsilon */;

factor[StringBuilder res]
    : {$res.append("<mrow>");} group[res] {$res.append("</mrow>");}
    | subsup[res]
    | {$res.append("<msup><mrow>");} group[res] {$res.append("</mrow>");} sup factor[res] {$res.append("</msup>");}
    | {$res.append("<msub><mrow>");} group[res] {$res.append("</mrow>");} sub factor[res] {$res.append("</msub>");};

group[StringBuilder res]
    : number        {$res.append("<mn>" + $number.text + "</mn>");}                           // 12345
    | word          {$res.append("<mi>" + $word.text + "</mi>");}                             // abc
    | number word   {$res.append("<mn>" + $number.text + "</mn><mi>" + $word.text + "<mi>");} // 2ab
    | sqrt[res]
    | frac[res]
    | LEFT_PARENTHESES {$res.append("<mo>(</mo>");} expr[res] RIGHT_PARENTHESES {$res.append("<mo>)</mo>");}
    | LEFT_BRACE expr[res] RIGHT_BRACE;

subsup[StringBuilder res] locals [StringBuilder left, StringBuilder right] @init {
$left = new StringBuilder();
$right = new StringBuilder();
}
    : {$res.append("<msubsup><mrow>");}
      group[res]
      {$res.append("</mrow>");}
      sub
      factor[res]
      sup
      factor[res]
      {$res.append("</msubsup>");}
    | {$res.append("<msubsup><mrow>");}
      group[res]
      {$res.append("</mrow>");}
      sup
      factor[$right]
      sub
      factor[$left]
      {$res.append($left);
      $res.append($right);
      $res.append("</msubsup>");};


// arithmetic operations

plus: PLUS;
minus: MINUS;
eq: EQ;
mul: MULTIPLICATION;
div: DIVISION;
sup: CARET;
sub: UNDERSCORE;
number: NUMBER;
word: WORD;

sqrt[StringBuilder res]
    : SQRT sqrt_base
      {$res.append("<mroot><mrow>");}
      LEFT_BRACE expr[res] RIGHT_BRACE
      {$res.append("</mrow><mrow>");
      $res.append($sqrt_base.e);
      $res.append("</mrow></mroot>");}
    | SQRT {$res.append("<msqrt><mrow>");}
      LEFT_BRACE expr[res] RIGHT_BRACE
      {$res.append("</mrow></msqrt>");};

sqrt_base returns [StringBuilder e] @init {
StringBuilder e = new StringBuilder();
$e = e;
}
    : LEFT_SQAURE_BRACKET expr[e] RIGHT_SQAURE_BRACKET;

frac[StringBuilder res]
    : FRAC
    {$res.append("<mfrac><mrow>");}
    LEFT_BRACE expr[res] RIGHT_BRACE
    {$res.append("</mrow><mrow>");}
    LEFT_BRACE expr[res] RIGHT_BRACE
    {$res.append("</mrow></mfrac>");};

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