#include "node.h"

struct start : nodes::node {
    start(tokenizer& tr);
    int $length = 0;
};

struct parseChar : nodes::node {
    parseChar(tokenizer& tr);
    int $acc = 0;
};

