FUN: 'fun';
IDENT: [a-zA-Z]('_' | [a-zA-Z0-9])*;
LP: '(';
RP: ')';
COLON: ':';
COMMA: ',';
EPS: '';
SPACE!: ' ';

start: FUN IDENT LP argsList RP tail EOF;
arg: IDENT COLON IDENT;
argsList: arg argsRest;
argsList: EPS;
argsRest: COMMA argsList;
argsRest: EPS;
tail: COLON IDENT;
tail: EPS;
