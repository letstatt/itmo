#include "nodes.h"

namespace nodes {

    S::S(tokenizer &tr) {
        expectNonTerminal<F>(tr);
        expectTerminal<SPACE>(tr);
        expectNonTerminal<N>(tr);
        expectTerminal<LPAREN>(tr);
        expectNonTerminal<L>(tr);
        expectTerminal<RPAREN>(tr);
        expectNonTerminal<E>(tr);
    }
}
