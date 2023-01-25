#include "grammar.h"


grammar::grammar(const char* file) : tokenizer(file), tree(tokenizer) {
    tree.parse();
}

extern std::string visualize_graphviz(nodes::protonode*);

std::string grammar::graphviz() {
    return visualize_graphviz(tree.get_tree());
}

utils::checker grammar::build() {
    return utils::checker(
        tree.sections,
        tree.tokens,
        tree.attributes,
        tree.embeddings,
        tree.rules
    );
}
