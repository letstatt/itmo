#pragma once

#include "node.h"
#include "utils.h"
#include <fstream>
#include <sstream>
#include <string>

/*
Gramar for grammars. Spaces are ignored.

START -> ^ GRAMMAR $

GRAMMAR -> SECTION GRAMMAR
GRAMMAR -> TOKEN GRAMMAR
GRAMMAR -> RULE GRAMMAR
GRAMMAR -> EMBEEDDED GRAMMAR
GRAMMAR -> $eps$

Sections:

SNAME -> '@'[a-zA-Z0-9]+ # section_name
SBLOCK1 -> \{ ([^ \{ \} ]* SBLOCK1)+ \}
SBLOCK1 -> $eps$

SBLOCK -> \{ ([^ \{ \} ]* SBLOCK1)+ \} # block
SECTION -> SNAME SBLOCK;

Terminals:

TNAME -> [A-Z][a-zA-Z0-9]+ # token_name
TCHRS -> [a-zA-Z0-9]+ # alphanum_seq
TENUM -> [a-z]'-'[a-z]
TENUM -> [A-Z]'-'[A-Z]
TENUM -> [0-9]'-'[0-9]

# root
TREGEX -> TREGEX3 TALTER
TALTER  -> '|' TREGEX3
TALTER  -> $eps$

TREGEX3 -> TREGEX2 TREGEX0
TREGEX0 -> TREGEX
TREGEX0 -> $eps$

TREGEX2 -> TREGEX1 TMOD
TMOD    -> '?'
TMOD    -> '+'
TMOD    -> '*'
TMOD    -> $eps$

TREGEX1 -> \( TREGEX \)
TREGEX1 -> \[ TENUM+ TCHRS? \]
TREGEX1 -> \' .*? \'
TREGEX1 -> TCHRS

TSKIP -> '!'
TSKIP -> $eps$
TOKEN -> TNAME TSKIP: REGEX;

Attributes:

ANAME -> '$' [a-z][a-zA-Z0-9]+ # attr_name
ATYPE -> [^=\{\}]+
AINIT -> = [^\{\}]+
AINIT -> $eps$
ASPEC -> \{ ATYPE AINIT? \}
ATTRREQ -> ANAME ASPEC
ASPECOPT -> ASPEC
ASPECOPT -> $eps$
ATTR -> ANAME ASPECOPT

ENAME -> '$' [A-Z][a-zA-Z0-9]+ # embed_name
EMBEEDDED -> ENAME SBLOCK;

Rules:

RNAME -> [a-z][a-zA-Z0-9]+ # rule_name
RBODY -> (RNAME | TNAME | ENAME)+

RARGS -> ATTR RARGS
RARGS -> $eps$
RRET -> '->' ATTRREQ
RRET -> $eps$
RULE -> RNAME RARGS RRET: RBODY;
*/

#include <iostream>

struct grammar {
    grammar(const char*);
    
    std::string graphviz();

    utils::checker build();

private:
    tokenizer tokenizer;
    parser::parser_tree tree;
};
