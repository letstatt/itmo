#include "parser.h"
#include "node.h"

namespace {
    using namespace parser;
    using ::token;

    #define NODE(A, ...) struct A: nodes::node { \
        A(tokenizer &, __VA_ARGS__);       \
        virtual std::string to_string() {   \
            return #A;                      \
        }                                   \
    }

    NODE(GRAMMAR, parser_tree&);
    // section
    NODE(SECTION, section&);
    // embedding
    NODE(EMBEDDING, embedding&);
    // terminal
    NODE(TOKEN, parser::token&);
    NODE(TSKIP, parser::token&);
    NODE(TREGEX, NFA&, size_t, size_t);
    NODE(TREGEX3, NFA&, size_t, size_t);
    NODE(TREGEX2, NFA&, size_t, size_t);
    NODE(TREGEX1, NFA&, size_t, size_t);
    // attribute
    NODE(ATTR, attribute&);
    NODE(ATTRREQ, attribute&);
    NODE(ASPEC, attribute&);
    // rule
    NODE(RULE, rule&);
    NODE(RBODY, rule&);

    GRAMMAR::GRAMMAR(tokenizer& tr, parser_tree& tree) {
        token t = tr.curToken();

        while (!std::holds_alternative<$>(t)) {
            ensure<SECTION_NAME, TOKEN_NAME, EMBED_NAME, RULE_NAME>(t);

            if (std::holds_alternative<SECTION_NAME>(t)) {
                auto& section = tree.new_section();
                expectNonTerminal<SECTION>(tr, section);
            } else if (std::holds_alternative<TOKEN_NAME>(t)) {
                auto& token = tree.new_token();
                expectNonTerminal<TOKEN>(tr, token);
            } else if (std::holds_alternative<EMBED_NAME>(t)) {
                auto& embedding = tree.new_embedding();
                expectNonTerminal<EMBEDDING>(tr, embedding);
            } else if (std::holds_alternative<RULE_NAME>(t)) {
                auto& rule = tree.new_rule();
                expectNonTerminal<RULE>(tr, rule);
            }
            t = tr.curToken();
        }
        ensure<$>(t);
    }

    SECTION::SECTION(tokenizer& tr, section& section) {
        token t = tr.curToken();
        ensure<SECTION_NAME>(t);

        std::visit([this, &tr, &section](auto&& arg) {
            using T = std::decay_t<decltype(arg)>;

            auto name = expectTerminal<SECTION_NAME>(tr); // @...
            section.name = name.value.substr(1);
            expectTerminal<LBRACE>(tr);
            auto block = expectTerminal<BLOCK>(tr);
            section.block = block.value;
            expectTerminal<RBRACE>(tr);
            expectTerminal<SEMICOLON>(tr);
        }, t);
    }

    EMBEDDING::EMBEDDING(tokenizer& tr, embedding& embedding) {
        token t = tr.curToken();
        ensure<EMBED_NAME>(t);

        std::visit([this, &tr, &embedding](auto&& arg) {
            using T = std::decay_t<decltype(arg)>;

            auto name = expectTerminal<EMBED_NAME>(tr);
            embedding.name = name.value; // $...
            expectTerminal<LBRACE>(tr);
            auto block = expectTerminal<BLOCK>(tr);
            embedding.embed = block.value;
            expectTerminal<RBRACE>(tr);
            expectTerminal<SEMICOLON>(tr);
        }, t);
    }

    TOKEN::TOKEN(tokenizer& tr, parser::token& token) {
        auto name = expectTerminal<TOKEN_NAME>(tr);
        token.name = name.value;
        expectNonTerminal<TSKIP>(tr, token);
        expectTerminal<COLON>(tr);
        expectNonTerminal<TREGEX>(tr, token.graph, 0, 1);
        expectTerminal<SEMICOLON>(tr);
    }

    TSKIP::TSKIP(tokenizer& tr, parser::token& token) {
        ::token t = tr.curToken();
        if (std::holds_alternative<EXCLAMATION_MARK>(t)) {
            expectTerminal<EXCLAMATION_MARK>(tr);
            token.skip = true;
        } else {
            token.skip = false;
        }
    }

    TREGEX::TREGEX(tokenizer& tr, NFA& g, size_t a, size_t b) {
        size_t x = g.add_vertex();
        g.add_edge(a, x, "");

        expectNonTerminal<TREGEX3>(tr, g, x, b);
        token t = tr.curToken();

        if (std::holds_alternative<BAR>(t)) {
            expectTerminal<BAR>(tr);
            size_t y = g.add_vertex();
            g.add_edge(a, y, "");
            expectNonTerminal<TREGEX3>(tr, g, y, b);
        } else {
            ensure<ALPHANUM_SEQ, RBRACKET, LBRACKET, LSQBRACKET, QUOTATION_MARK, SEMICOLON>(t);
        }
    }

    TREGEX3::TREGEX3(tokenizer& tr, NFA& g, size_t a, size_t b) {
        auto v = g.add_vertex();
        expectNonTerminal<TREGEX2>(tr, g, a, v);

        token t = tr.curToken();
        ensure<ALPHANUM_SEQ, LBRACKET, RBRACKET, LSQBRACKET, QUOTATION_MARK, BAR, SEMICOLON>(t);

        if (std::holds_alternative<SEMICOLON>(t)
        || std::holds_alternative<BAR>(t)
        || std::holds_alternative<RBRACKET>(t)) {
            g.add_edge(v, b, "");
        } else {
            expectNonTerminal<TREGEX>(tr, g, v, b);
        }
    }

    TREGEX2::TREGEX2(tokenizer& tr, NFA& g, size_t a, size_t b) {
        token t = tr.curToken();

        size_t x = g.add_vertex();
        size_t y = g.add_vertex();

        g.add_edge(a, x, "");
        bool split = std::holds_alternative<ALPHANUM_SEQ>(t);
        
        expectNonTerminal<TREGEX1>(tr, g, x, y);
        char c = 0;

        if (split) {
            c = g.split_last_edge();
            g.add_edge(y, b, {c});
        } else {
            g.add_edge(y, b, "");
        }

        t = tr.curToken();
        ensure<QUESTION_MARK, PLUS_SIGN, MUL_SIGN, BAR,
            ALPHANUM_SEQ, LBRACKET, RBRACKET, LSQBRACKET, QUOTATION_MARK, SEMICOLON>(t);

        if (std::holds_alternative<QUESTION_MARK>(t)) {
            if (split) {
                g.add_edge(y, b, "");
            } else {
                g.add_edge(a, b, "");
            }
            expectTerminal<QUESTION_MARK>(tr);
        } else if (std::holds_alternative<PLUS_SIGN>(t)) {
            if (split) {
                g.add_edge(b, y, "");
            } else {
                g.add_edge(b, a, "");
            }
            expectTerminal<PLUS_SIGN>(tr);
        } else if (std::holds_alternative<MUL_SIGN>(t)) {
            if (split) {
                g.add_edge(b, y, "");
                g.add_edge(y, b, "");
            } else {
                g.add_edge(b, a, "");
                g.add_edge(a, b, "");
            }
            expectTerminal<MUL_SIGN>(tr);
        }
    }

    TREGEX1::TREGEX1(tokenizer& tr, NFA& g, size_t a, size_t b) {
        token t = tr.curToken();
        ensure<ALPHANUM_SEQ, LBRACKET, LSQBRACKET, QUOTATION_MARK>(t);

        if (std::holds_alternative<ALPHANUM_SEQ>(t)) {
            auto seq = expectTerminal<ALPHANUM_SEQ>(tr);
            int u = a;
            int v = g.add_vertex();
            for (int i = 0; i + 1 < seq.value.size(); ++i) {
                g.add_edge(u, v, {seq.value[i]});
                u = v;
                v = g.add_vertex();
            }
            g.add_edge(u, b, {seq.value.back()});

        } else if (std::holds_alternative<QUOTATION_MARK>(t)) {
            expectTerminal<QUOTATION_MARK>(tr);
            auto esc = expectTerminal<ESCAPED>(tr);
            if (esc.value.empty()) {
                g.add_edge(a, b, "");
            } else {
                int u = a;
                int v = g.add_vertex();
                for (int i = 0; i + 1 < esc.value.size(); ++i) {
                    g.add_edge(u, v, {esc.value[i]});
                    u = v;
                    v = g.add_vertex();
                }
                g.add_edge(u, b, {esc.value.back()});
            }
            expectTerminal<QUOTATION_MARK>(tr);

        } else if (std::holds_alternative<LBRACKET>(t)) {
            expectTerminal<LBRACKET>(tr);
            expectNonTerminal<TREGEX>(tr, g, a, b);
            expectTerminal<RBRACKET>(tr);

        } else if (std::holds_alternative<LSQBRACKET>(t)) {
            expectTerminal<LSQBRACKET>(tr);
            std::string last_seq;
            bool minus = false;

            do {
                token t = tr.curToken();
                if (last_seq.empty() | minus) {
                    ensure<ALPHANUM_SEQ>(t);
                } else {
                    ensure<ALPHANUM_SEQ, MINUS, RSQBRACKET>(t);
                }

                if (std::holds_alternative<ALPHANUM_SEQ>(t)) {
                    auto seq = std::get<ALPHANUM_SEQ>(t);

                    if (minus) {
                        char from = last_seq.back();
                        char to = seq.value.front();

                        if (to < from) std::swap(from, to);
                        while (from < to) {
                            g.add_edge(a, b, {from++});
                        }
                        g.add_edge(a, b, {from}); // from == to
                    }

                    for (auto c: seq.value) {
                        g.add_edge(a, b, {c});
                    }
                    last_seq = seq.value;
                    minus = false;
                    expectTerminal<ALPHANUM_SEQ>(tr);
                } else if (std::holds_alternative<MINUS>(t)) {
                    expectTerminal<MINUS>(tr);
                    minus = true;
                } else {
                    expectTerminal<RSQBRACKET>(tr);
                    break;
                }

            } while (true);
        }
    }

    ATTR::ATTR(tokenizer& tr, attribute& a) {
        auto name = expectTerminal<ATTR_NAME>(tr);
        a.name = name.value;
        
        token t = tr.curToken();
        if (std::holds_alternative<ATTR_NAME>(t)
        || std::holds_alternative<COLON>(t)) {
            return;
        } else {
            expectNonTerminal<ASPEC>(tr, a);
        }
    }

    ATTRREQ::ATTRREQ(tokenizer& tr, attribute& a) {
        auto name = expectTerminal<ATTR_NAME>(tr);
        a.name = name.value;
        expectNonTerminal<ASPEC>(tr, a);
    }

    ASPEC::ASPEC(tokenizer& tr, attribute& a) {
        expectTerminal<LBRACE>(tr);
        auto type = expectTerminal<ATTR_TYPE>(tr);
        a.type = type.value;

        token t = tr.curToken();
        if (std::holds_alternative<EQ>(t)) {
            expectTerminal<EQ>(tr);
            auto init = expectTerminal<ATTR_INITIALIZER>(tr);
            a.initializer = init.value;
        }

        expectTerminal<RBRACE>(tr);
    }

    RULE::RULE(tokenizer& tr, rule& r) {
        auto name = expectTerminal<RULE_NAME>(tr);
        r.name = name.value;

        token t = tr.curToken();
        while (std::holds_alternative<ATTR_NAME>(t)) {
            attribute a{};
            expectNonTerminal<ATTR>(tr, a);
            r.args.emplace_back(std::move(a));
            t = tr.curToken();
        }
        
        t = tr.curToken();
        if (std::holds_alternative<RETURN_ARROW>(t)) {
            expectTerminal<RETURN_ARROW>(tr);
            attribute a{};
            expectNonTerminal<ATTRREQ>(tr, a);
            r.ret = std::move(a);
        }

        expectTerminal<COLON>(tr);
        expectNonTerminal<RBODY>(tr, r);
        expectTerminal<SEMICOLON>(tr);
    }

    RBODY::RBODY(tokenizer& tr, rule& r) {
        token t = tr.curToken();
        while (!std::holds_alternative<SEMICOLON>(t)) {
            ensure<RULE_NAME, TOKEN_NAME, EMBED_NAME, ATTR_NAME>(t);

            if (std::holds_alternative<RULE_NAME>(t)) {
                auto name = expectTerminal<RULE_NAME>(tr);
                r.sequence.push_back(name.value);
            } else if (std::holds_alternative<TOKEN_NAME>(t)) {
                auto name = expectTerminal<TOKEN_NAME>(tr);
                r.sequence.push_back(name.value);
            } else if (std::holds_alternative<EMBED_NAME>(t)) {
                auto name = expectTerminal<EMBED_NAME>(tr);
                r.sequence.push_back(name.value);
            } else if (std::holds_alternative<ATTR_NAME>(t)) {
                auto name = expectTerminal<ATTR_NAME>(tr);
                r.sequence.push_back(name.value);
            }
            t = tr.curToken();
        }
    }

}

namespace parser {

    bool attribute::operator!=(const attribute& a) const {
        return a.name != name || a.type != type || a.initializer != initializer;
    }

    parser_tree::parser_tree(::tokenizer& tokenizer) : tokenizer(tokenizer) {}

    void parser_tree::parse() {
        tokenizer.nextToken();
        tree = new GRAMMAR(tokenizer, *this);
    }

    nodes::protonode* parser_tree::get_tree() {
        if (!tree) {
            throw std::runtime_error("tree is uninitialized");
        }
        return *tree;
    }

    section& parser_tree::new_section() {
        sections.push_back({});
        return sections.back();
    }

    token& parser_tree::new_token() {
        tokens.push_back({});
        return tokens.back();
    }

    attribute& parser_tree::new_attribute() {
        attributes.push_back({});
        return attributes.back();
    }

    embedding& parser_tree::new_embedding() {
        embeddings.push_back({});
        return embeddings.back();
    }

    rule& parser_tree::new_rule() {
        rules.push_back({});
        return rules.back();
    }
}
