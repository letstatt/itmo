#include "user_nodes.h"

start::start(tokenizer& tr) {
    auto t = tr.curToken();
    ensure<$, Char>(t);

    if (safeEnsure<$, Char>::ensure(t)) {
        auto* _parseChar = expectNonTerminal<parseChar>(tr);
        auto _$ = expectTerminal<$>(tr);
        $length = _parseChar->$acc;

    }
}

parseChar::parseChar(tokenizer& tr) {
    auto t = tr.curToken();
    ensure<Char, $>(t);

    if (safeEnsure<Char>::ensure(t)) {
        auto _Char = expectTerminal<Char>(tr);
        auto* _parseChar = expectNonTerminal<parseChar>(tr);
        $acc = 1 + _parseChar->$acc;

    }
}

