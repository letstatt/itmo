#include "user_nodes.h"
#include <iostream>

int main(int argc, char** argv) {
    init();

    {
        // demonstrate tokenizer
        auto tr = tokenizer(argv[1]);
        std::vector<token> tokens;

        do {
            token t = tr.nextToken();
            tokens.push_back(t);
        } while (!std::holds_alternative<$>(tokens.back()));

        std::cout << "Tokens: \n";
        for (auto &t: tokens) {
            std::cout << std::visit(nodes::protonode::repr, t) << std::endl;
        }
    }
    {
        // calculate length of the string
        auto tr = tokenizer(argv[1]);
        tr.nextToken();
        auto s = start(tr);
        std::cout << "Length: " << s.$length << std::endl;
    }
}