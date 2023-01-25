#include "user_nodes.h"

arg::arg(tokenizer& tr) {
    auto t = tr.curToken();
    ensure<IDENT>(t);

    if (safeEnsure<IDENT>::ensure(t)) {
        auto _IDENT = expectTerminal<IDENT>(tr);
        auto _COLON = expectTerminal<COLON>(tr);
        _IDENT = expectTerminal<IDENT>(tr);
    }
}

start::start(tokenizer& tr) {
    auto t = tr.curToken();
    ensure<FUN>(t);

    if (safeEnsure<FUN>::ensure(t)) {
        auto _FUN = expectTerminal<FUN>(tr);
        auto _IDENT = expectTerminal<IDENT>(tr);
        auto _LP = expectTerminal<LP>(tr);
        auto* _argsList = expectNonTerminal<argsList>(tr);
        auto _RP = expectTerminal<RP>(tr);
        auto* _tail = expectNonTerminal<tail>(tr);
        auto _$ = expectTerminal<$>(tr);
    }
}

argsRest::argsRest(tokenizer& tr) {
    auto t = tr.curToken();
    ensure<COMMA, RP>(t);

    if (safeEnsure<COMMA>::ensure(t)) {
        auto _COMMA = expectTerminal<COMMA>(tr);
        auto* _argsList = expectNonTerminal<argsList>(tr);
    }
}

tail::tail(tokenizer& tr) {
    auto t = tr.curToken();
    ensure<COLON, $>(t);

    if (safeEnsure<COLON>::ensure(t)) {
        auto _COLON = expectTerminal<COLON>(tr);
        auto _IDENT = expectTerminal<IDENT>(tr);
    }
}

argsList::argsList(tokenizer& tr) {
    auto t = tr.curToken();
    ensure<IDENT, RP>(t);

    if (safeEnsure<IDENT>::ensure(t)) {
        auto* _arg = expectNonTerminal<arg>(tr);
        auto* _argsRest = expectNonTerminal<argsRest>(tr);
    }
}

