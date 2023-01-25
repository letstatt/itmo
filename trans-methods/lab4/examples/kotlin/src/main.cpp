#include "user_nodes.h"
#include <iostream>

int main(int argc, char** argv) {
    init();

    try {
        auto tr = tokenizer(argv[1]);
        tr.nextToken();
        start{tr};
        std::cout << "ok" << std::endl;
        return 0;
        
    } catch (const std::exception& e) {
        std::cout << e.what() << std::endl;
        return 1;
    }
}