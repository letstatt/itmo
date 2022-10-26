#include "nodes.h"

namespace nodes {
    
    B::B(tokenizer & tr) {
        token t = tr.curToken();
        ensure<COMMA, RPAREN>(t);

        std::visit([this, &tr](auto&& arg) {
            using T = std::decay_t<decltype(arg)>;

            if constexpr(std::is_same_v<T, COMMA>) {
                expectTerminal<COMMA>(tr);
                expectNonTerminal<L>(tr);
            } else if constexpr (std::is_same_v<T, RPAREN>) {
                return;
            }
        }, t);
    }
}
