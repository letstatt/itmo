#include "nodes.h"

namespace nodes {
    
    G::G(tokenizer & tr) {
        token t = tr.curToken();
        ensure<LBRACKET, RBRACKET, RPAREN, COMMA, $>(t);

        std::visit([this, &tr](auto&& arg) {
            using T1 = std::decay_t<decltype(arg)>;

            if constexpr(std::is_same_v<T1, LBRACKET>) {
                expectTerminal<LBRACKET>(tr);
                expectNonTerminal<T>(tr);
                expectTerminal<RBRACKET>(tr);
            }
        }, t);
    }
}
