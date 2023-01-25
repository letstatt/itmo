#include "user_nodes.h"
#include <iostream>

int main(int argc, char** argv) {
    init();

    auto tr = tokenizer(argv[1]);
    tr.nextToken();
    start(tr, std::cout);
}