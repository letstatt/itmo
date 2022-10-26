#include "nodes.h"

namespace nodes {
    
    N::N(tokenizer & tr) {
        expectTerminal<NAME>(tr);
    }
}
