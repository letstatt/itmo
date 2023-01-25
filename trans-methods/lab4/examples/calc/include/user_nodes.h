#include "node.h"

#include <string>
#include <ostream>
#include <cmath>
#include <algorithm>

using Num = float;

struct term2 : nodes::node {
    term2(tokenizer& tr);
    Num $val = 1;
};

struct factor : nodes::node {
    factor(tokenizer& tr);
    Num $val;
};

struct exprPack : nodes::node {
    exprPack(tokenizer& tr);
    std::vector<Num> $vec;
};

struct expr2 : nodes::node {
    expr2(tokenizer& tr);
    Num $val = 0;
};

struct powRule : nodes::node {
    powRule(tokenizer& tr);
    Num $val = 1;
};

struct start : nodes::node {
    start(tokenizer& tr, std::ostream& $log);
};

struct expr : nodes::node {
    expr(tokenizer& tr);
    Num $val;
};

struct term : nodes::node {
    term(tokenizer& tr);
    Num $val;
};

