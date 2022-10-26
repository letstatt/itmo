#include "nodes.h"

namespace nodes {
    
    E::E(tokenizer & tr) {
        token t = tr.curToken();
        ensure<COLON, $>(t);

        std::visit([this, &tr](auto&& arg) {
            using T1 = std::decay_t<decltype(arg)>;

            if constexpr(std::is_same_v<T1, COLON>) {
                expectTerminal<COLON>(tr);
                expectNonTerminal<T>(tr);
            } else if constexpr (std::is_same_v<T1, $>) {
                return;
            }
        }, t);
    }
}
