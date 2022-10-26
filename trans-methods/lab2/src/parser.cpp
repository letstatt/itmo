#include "nodes.h"

nodes::S parse(const char* filepath) {
    tokenizer tr(filepath);
    tr.nextToken();

    // do stuff
    nodes::S s(tr);
    nodes::protonode::ensure<$>(tr.curToken());
    return s;
}