#pragma once
#include "tokenizer.h"
#include "graph.h"
#include "node.h"
#include <vector>

namespace parser {

    struct section {
        std::string name;
        std::string block;
    };

    struct token {
        std::string name;
        NFA graph;
        bool skip;
    };

    struct embedding {
        std::string name;
        std::string embed;
    };

    struct attribute {
        std::string name;
        std::string type;
        std::string initializer;

        bool operator!=(const attribute&) const;
    };

    struct rule {
        std::string name;
        std::vector<attribute> args;
        std::optional<attribute> ret;
        std::vector<std::string> sequence;
    };

    struct parser_tree {
        parser_tree(tokenizer&);

        void parse();
        nodes::protonode* get_tree();

        section& new_section();
        token& new_token();
        attribute& new_attribute();
        embedding& new_embedding();
        rule& new_rule();

        std::vector<section> sections;
        std::vector<token> tokens;
        std::vector<attribute> attributes;
        std::vector<embedding> embeddings;
        std::vector<rule> rules;

    private:
        std::optional<nodes::protonode*> tree;
        tokenizer& tokenizer;
    };

}
