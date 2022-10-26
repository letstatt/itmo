#include "nodes.h"

namespace nodes {
    
    A::A(tokenizer & tr) {
        expectNonTerminal<N>(tr);
        expectTerminal<COLON>(tr);
        expectNonTerminal<T>(tr);
    }
}
