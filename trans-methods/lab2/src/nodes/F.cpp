#include "nodes.h"

namespace nodes {
    
    F::F(tokenizer & tr) {
        expectTerminal<FUNC_DECL>(tr);
    }
}
