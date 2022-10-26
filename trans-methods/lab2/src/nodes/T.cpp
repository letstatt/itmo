#include "nodes.h"

namespace nodes {
    
    T::T(tokenizer & tr) {
        token t = tr.curToken();
        expectNonTerminal<N>(tr);
        expectNonTerminal<G>(tr);
    }
}
