#pragma once
#include "node.h"

namespace nodes {

    #define NODE(A) struct A: node {        \
        A(tokenizer &);                     \
        virtual std::string to_string() {   \
            return #A;                      \
        }                                   \
    }

    NODE(A);
    NODE(B);
    NODE(E);
    NODE(L);
    NODE(F);
    NODE(N);
    NODE(S);
    NODE(T);
    NODE(G);
}
