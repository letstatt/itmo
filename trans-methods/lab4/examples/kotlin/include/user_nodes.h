#include "node.h"

struct arg : nodes::node {
    arg(tokenizer& tr);
};

struct start : nodes::node {
    start(tokenizer& tr);
};

struct argsRest : nodes::node {
    argsRest(tokenizer& tr);
};

struct tail : nodes::node {
    tail(tokenizer& tr);
};

struct argsList : nodes::node {
    argsList(tokenizer& tr);
};

