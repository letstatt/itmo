#pragma once
#include "parser.h"
#include <functional>

namespace utils {

    bool isToken(const std::string&);
    bool isRule(const std::string&);
    bool isEmbedding(const std::string&);
    bool isArgument(const std::string&);

    struct token {
        token(const parser::token&);

        const bool skip;
        const bool accepts_eps;
        const std::string name;
        const std::reference_wrapper<const parser::token> t;
    };

    struct argument {
        argument(const parser::attribute&);

        const std::string name;
        const std::string type;
        const std::string default_value;
    };

    struct rule {
        rule(const parser::rule&);

        mutable bool accepts_eps = false;
        mutable std::vector<std::string> sequence;
        mutable std::unordered_set<std::string> first;
        mutable std::reference_wrapper<const parser::rule> r;
    };

    struct bunch_of_rules {
        bunch_of_rules(std::vector<rule>&&);

        bool accepts_eps = false;
        mutable std::vector<rule> rules;
        const std::optional<argument> ret;
        const std::vector<argument> args;
        std::unordered_set<std::string> first;
        std::unordered_set<std::string> follow;
    };

    struct checker {
        checker(
            const std::vector<parser::section>& sections,
            const std::vector<parser::token>& tokens,
            const std::vector<parser::attribute>& attrs,
            const std::vector<parser::embedding>& embeds,
            const std::vector<parser::rule>& rules
        );

        bool isLL1;
        std::unordered_map<std::string, bunch_of_rules> rules;
        std::unordered_map<std::string, token> tokens;
        std::vector<std::string> tokens_order;
        std::unordered_map<std::string, std::reference_wrapper<const parser::embedding>> embeds;
        std::unordered_map<std::string, std::reference_wrapper<const parser::section>> sections;
    
    private:
        void first();
        void follow();
        bool checkLL1();
    };
}
