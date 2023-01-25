#include "utils.h"
#include <iostream>

namespace {
    parser::token eof {.name = "EOF", .skip = false};
}

namespace utils {

    bool isToken(const std::string& s) {
        if (s.empty()) {
            return false;
        }
        return ('A' <= s[0] && s[0] <= 'Z');
    }

    bool isRule(const std::string& s) {
        if (s.empty()) {
            return false;
        }
        return ('a' <= s[0] && s[0] <= 'z');
    }

    bool isEmbedding(const std::string& s) {
        if (s.size() < 2 || s[0] != '$') {
            return false;
        }
        return ('A' <= s[1] && s[1] <= 'Z');
    }

    bool isArgument(const std::string& s) {
        if (s.size() < 2 || s[0] != '$') {
            return false;
        }
        return ('a' <= s[1] && s[1] <= 'z');
    }

    // token
    token::token(const parser::token& t)
        : accepts_eps(t.graph.accepts(0))
        , skip(t.skip)
        , name(t.name)
        , t(std::cref(t)) {}
    
    // argument
    argument::argument(const parser::attribute& a)
        : name(a.name)
        , type(a.type)
        , default_value(a.initializer) {}

    // rule
    rule::rule(const parser::rule& r)
        : sequence(r.sequence)
        , r(std::cref(r)) {}
    
    // bunch_of_rules
    bunch_of_rules::bunch_of_rules(std::vector<rule>&& r)
        : rules(std::move(r))
        , ret(rules[0].r.get().ret)
        , args(rules[0].r.get().args.begin(), rules[0].r.get().args.end()) {}
    
    // checker
    checker::checker(
        const std::vector<parser::section>& sections,
        const std::vector<parser::token>& tokens,
        const std::vector<parser::attribute>& attrs,
        const std::vector<parser::embedding>& embeds,
        const std::vector<parser::rule>& rules)
    {
        // tokens check
        // note: token EOF must be replaced to the default one later
        std::unordered_map<std::string, std::reference_wrapper<const parser::token>> t;
        for (auto& token: tokens) {
            if (t.count(token.name)) {
                std::stringstream str;
                str << "Error: token \"" << token.name;
                str << "\" encountered twice";
                throw std::runtime_error(str.str());
            }
            t.insert({token.name, token});
        }

        // sections check
        for (auto& section: sections) {
            if (this->sections.count(section.name)) {
                std::stringstream str;
                str << "Error: section \"" << section.name;
                str << "\" encountered twice";
                throw std::runtime_error(str.str());
            }
            this->sections.insert({section.name, std::cref(section)});
        }

        // embeddings check
        for (auto& embedding: embeds) {
            if (this->embeds.count(embedding.name)) {
                std::stringstream str;
                str << "Error: embedding \"" << embedding.name;
                str << "\" encountered twice";
                throw std::runtime_error(str.str());
            }
            this->embeds.insert({embedding.name, std::cref(embedding)});
        }

        // rules precheck
        std::unordered_map<std::string, std::vector<std::reference_wrapper<const parser::rule>>> r;
        for (auto& rule: rules) {
            r[rule.name].push_back(std::cref(rule));
        }
        for (auto& rule: rules) {
            if (rule.sequence.empty()) {
                std::stringstream stream;
                stream << "Error: empty rules prohibited";
                stream << " (see rule \"" << rule.name << "\")";
                throw std::runtime_error(stream.str());
            }
            for (auto& str: rule.sequence) {
                if (isRule(str) && !r.count(str)) {
                    std::stringstream stream;
                    stream << "Error: referenced rule \"" << str;
                    stream << "\" not found (see rule \"" << rule.name << "\")";
                    throw std::runtime_error(stream.str());
                }
                if (isToken(str) && !t.count(str)) {
                    if (str == "EOF") {
                        t.insert({str, std::cref(eof)});
                        continue;
                    }
                    std::stringstream stream;
                    stream << "Error: token \"" << str;
                    stream << "\" not found (see rule \"" << rule.name << "\")";
                    throw std::runtime_error(stream.str());
                }
                if (isEmbedding(str) && !this->embeds.count(str)) {
                    std::stringstream stream;
                    stream << "Error: embedding \"" << str;
                    stream << "\" not found (see rule \"" << rule.name << "\")";
                    throw std::runtime_error(stream.str());
                }
            }
        }
        // check args equality
        for (auto&[name, vec]: r) {
            std::unordered_map<std::string, int> args;
            for (auto &rule: vec) {
                for (auto& attr: rule.get().args) {
                    args[attr.name] += 1;
                }
            }
            int cnt = (args.size() > 0 ? args.begin()->second : 0);
            for (auto &[i, j]: args) {
                if (j != cnt) {
                    std::stringstream stream;
                    stream << "Error: inconsistent arguments";
                    stream << " (see rule \"" << name << "\")";
                    throw std::runtime_error(stream.str());
                }
            }
        }
        // check return types equality
        for (auto&[name, vec]: r) {
            std::optional<parser::attribute> ret;
            for (auto& rule: vec) {
                auto& opt = rule.get().ret;
                if (opt.has_value() && !ret.has_value()) {
                    ret = opt;
                } else if (ret != opt) {
                    std::stringstream stream;
                    stream << "Error: inconsistent arguments";
                    stream << " (see rule \"" << name << "\")";
                    throw std::runtime_error(stream.str());
                }
            }
        }

        // tokens graphs determinization and finalizing
        for (auto&[name, token]: t) {
            if (name == "EOF") continue;
            token.get().graph.noeps();
            token.get().graph.toDFA();
            this->tokens.insert({name, token.get()});
        }

        // insert EOF
        if (t.count("EOF")) {
            this->tokens.insert({"EOF", eof});
        }

        // insert rules
        for (auto&[name, vec]: r) {
            std::vector<rule> rs;
            for (auto wrapper: vec) {
                rs.emplace_back(wrapper.get());
            }
            this->rules.emplace(name, std::move(rs));
        }

        // check if all args are in place
        for (auto&[name, bunch]: this->rules) {
            for (auto& rule: bunch.rules) {
                for (int i = 0; i < rule.sequence.size(); ++i) {
                    if (isRule(rule.sequence[i])) {
                        int j = i + 1;
                        while (j < rule.sequence.size() && isArgument(rule.sequence[j])) j++;
                        auto cnt = j - i - 1;
                        auto require = this->rules.at(rule.sequence[i]).args.size();
                        if (cnt == require) {
                            i = j - 1;
                            continue;
                        }
                        std::stringstream stream;
                        stream << "Error: calling \"" << rule.sequence[i];
                        stream << "\" requires " << require << "arguments,";
                        stream << "but " << cnt << "given";
                        stream << " (see rule \"" << name << "\")";
                        throw std::runtime_error(stream.str());
                    }
                }
            }
        }

        // build FIRST
        first();

        // build FOLLOW
        follow();

        // set LL1-belonging bit
        isLL1 = checkLL1();

        // remove pure-eps tokens
        std::unordered_set<std::string> pure_eps;
        while (true) {
            bool changed = false;
            for (auto& [name, t]: this->tokens) {
                if (t.accepts_eps) {
                    auto& g = t.t.get().graph.get_graph();
                    if (g.size() < 2) {
                        changed = true;
                        pure_eps.insert(name);
                        this->tokens.erase(name);
                        break;
                    }
                }
            }
            if (!changed) {
                break;
            }
        }

        // remove pure-eps tokens in rules
        for (auto& [name, bunch]: this->rules) {
            for (int i = 0; i < bunch.rules.size(); ++i) {
                auto& rule = bunch.rules[i];
                auto& seq = rule.sequence;
                for (int j = 0; j < seq.size(); ++j) {
                    if (pure_eps.count(seq[j])) {
                        seq.erase(seq.begin() + j);
                        j -= 1;
                    }
                }
                if (seq.empty()) {
                    bunch.rules.erase(bunch.rules.begin() + i);
                    i -= 1;
                }
            }

            if (bunch.rules.empty()) {
                std::stringstream stream;
                stream << "Error: redundant pure-eps rule";
                stream << " (see rule \"" << name << "\")";
                throw std::runtime_error(stream.str());
            }
        }

        // remove pure-eps in first and follow
        for (auto& [name, bunch]: this->rules) {
            for (auto& rule: bunch.rules) {
                for (auto& eps: pure_eps) {
                    auto it = rule.first.find(eps);
                    if (it != rule.first.end()) {
                        rule.first.erase(it);
                    }
                }
            }
            for (auto& eps: pure_eps) {
                auto it = bunch.first.find(eps);
                if (it != bunch.first.end()) {
                    bunch.first.erase(it);
                }
                it = bunch.follow.find(eps);
                if (it != bunch.follow.end()) {
                    bunch.follow.erase(it);
                }
            }
        }

        // save tokens order
        for (auto& token: tokens) {
            if (this->tokens.count(token.name)) {
                tokens_order.push_back(token.name);
            }
        }
    }

    void checker::first() {
        bool changed = true;
        while (changed) {
            changed = false;
            for (auto&[name, bunch]: rules) {
                auto& f1 = bunch.first;
                for (auto& r: bunch.rules) {
                    auto& f2 = r.first;
                    bool add_eps = true;

                    for (const auto& str: r.sequence) {
                        if (isToken(str)) {
                            f2.insert(str);
                            if (!tokens.at(str).accepts_eps) {
                                add_eps = false;
                                break;
                            }
                        } else if (isRule(str)) {
                            const auto& bunch2 = rules.at(str);
                            f2.insert(bunch2.first.begin(), bunch2.first.end());
                            if (!bunch2.accepts_eps) {
                                add_eps = false;
                                break;
                            }
                        }
                    }

                    r.accepts_eps |= add_eps;
                    bunch.accepts_eps |= r.accepts_eps;

                    // is f2 a subset of f1?
                    for (auto i: f2) {
                        if (!f1.count(i)) {
                            f1.insert(i);
                            changed = true;
                        }
                    }
                }
            }
        }
    }

    void checker::follow() {
        bool changed = true;
        while (changed) {
            changed = false;
            for (auto&[name, bunch]: rules) {
                for (auto& rule: bunch.rules) {
                    for (int i = 0; i < rule.sequence.size(); ++i) {
                        auto& str = rule.sequence[i];
                        if (!isRule(str)) {
                            continue;
                        }

                        auto& follow_b = rules.at(str).follow;
                        std::unordered_set<std::string> first1;
                        bool add_eps = true;

                        for (int j = i + 1; j < rule.sequence.size(); ++j) {
                            auto& str2 = rule.sequence[j];
                            if (isRule(str2)) {
                                auto& r = rules.at(str2);
                                first1.insert(r.first.begin(), r.first.end());
                                if (!r.accepts_eps) {
                                    add_eps = false;
                                    break;
                                }
                            } else if (isToken(str2)) {
                                first1.insert(str2);
                                if (!tokens.at(str2).accepts_eps) {
                                    add_eps = false;
                                    break;
                                }
                            }
                        }

                        // is first1 a subset of follow?
                        for (auto i: first1) {
                            if (!follow_b.count(i)) {
                                follow_b.insert(i);
                                changed = true;
                            }
                        }

                        if (add_eps) {
                            // copy! it's important.
                            auto follow2 = bunch.follow;

                            // is follow2 a subset of follow?
                            for (auto i: follow2) {
                                if (!follow_b.count(i)) {
                                    follow_b.insert(i);
                                    changed = true;
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    bool checker::checkLL1() {
        for (auto&[name, bunch]: rules) {
            for (int i = 0; i < bunch.rules.size(); ++i) {
                for (int j = i + 1; j < bunch.rules.size(); ++j) {
                    const auto& f1 = bunch.rules[i].first;
                    const auto& f2 = bunch.rules[j].first;
                    bool eps1 = bunch.rules[i].accepts_eps;
                    bool eps2 = bunch.rules[j].accepts_eps;

                    // does first1 and first2 intersect?
                    for (const auto& str: f1) {
                        if (f2.count(str)) {
                            std::cout << "[*] Warning: LL(1)-check failed, rule \"" << name << "\"" << std::endl;
                            return false;
                        }
                    }

                    if (eps1) {
                        const auto& follow_a = bunch.follow;

                        // does follow_a and first2 intersect?
                        for (const auto& str: f2) {
                            if (follow_a.count(str)) {
                                std::cout << "[*] Warning: LL(1)-check failed, rule \"" << name << "\"" << std::endl;
                                return false;
                            }
                        }
                    }
                }
            }
        }
        return true;
    }
}
