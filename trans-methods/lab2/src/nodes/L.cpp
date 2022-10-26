#include "nodes.h"

namespace nodes {
    
    L::L(tokenizer & tr) {
        token t = tr.curToken();
        ensure<NAME, RPAREN>(t);

        std::visit([this, &tr](auto&& arg) {
            using T = std::decay_t<decltype(arg)>;

            if constexpr(std::is_same_v<T, NAME>) {
                expectNonTerminal<A>(tr);
                expectNonTerminal<B>(tr);
            } else if constexpr (std::is_same_v<T, RPAREN>) {
                return;
            }
        }, t);
    }
}
