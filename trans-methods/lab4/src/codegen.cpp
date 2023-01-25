#include "codegen.h"
#include <iostream>
#include <fstream>

bool isWhitespace(char c) {
    return c == '\t' || c == '\r' || c == '\n' || c == ' ';
}

std::string trim(const std::string& s) {
    int i = 0;
    while (i < s.size() && isWhitespace(s[i])) i++;
    if (i == s.size()) {
        return "";
    }
    int j = s.size() - 1;
    while (j >= 0 && isWhitespace(s[j])) j--;
    // "  abc   ", i = 2, j = 4, len = j - i + 1
    return s.substr(i, j - i + 1);
}

template <typename... Args>
void print(std::ostream& s, size_t depth, Args&&... args) {
    s << std::string(depth, ' ');
    (s << ... << args);
}

#define P0(s, ...) print(s, 0, __VA_ARGS__);
#define P1(s, ...) print(s, 4, __VA_ARGS__);
#define P2(s, ...) print(s, 8, __VA_ARGS__);
#define P3(s, ...) print(s, 12, __VA_ARGS__);

void write_user_tokens(
    const char* dir,
    const utils::checker& checker) {
    const auto& tokens = checker.tokens;

    std::stringstream user_header;
    std::stringstream user_cpp;

    user_header << dir << "/" << "user_tokens.h";
    user_cpp << dir << "/" << "user_tokens.cpp";

    std::ofstream cpp(user_cpp.str());
    std::ofstream h(user_header.str());

    if (!cpp.is_open() || !h.is_open()) {
        std::stringstream err;
        err << "Error: can't create file \"";
        err << (!cpp.is_open() ? user_cpp : user_header).str();
        err << "\"";
        throw std::runtime_error(err.str());
    }

    P0(h, "#include \"tokens.h\"\n\n");
    P0(h, "extern std::vector<DFA> graphs;\n\n");

    std::vector<std::string> names;
    for (auto& name: checker.tokens_order) {
        if (name != "EOF") {
            names.push_back(name);
            if (tokens.at(name).skip) {
                P0(h, "SKIPPED_TOKEN(", name, ", \"<", name, ">\");\n");
            } else {
                P0(h, "STRING_TOKEN(", name, ", \"<", name, ">\");\n");
            }
        }
    }
    P0(h, "\n");

    P0(h, "// type alias\n");
    P0(h, "typedef std::variant<\n");
    for (auto& name: names) {
        P1(h, name, ",\n");
    }
    P1(h, "$\n");
    P0(h, "> token;\n\n");

    P0(h, "std::optional<std::pair<size_t, token>> try_tokenize(const std::string& word);\n\n");
    P0(h, "void init();\n");

    P0(cpp, "#include \"user_tokens.h\"\n");
    P0(cpp, "#include <tuple>\n");
    P0(cpp, "#include <set>\n");
    P0(cpp, "std::vector<DFA> graphs;\n\n");

    cpp <<
    "namespace {\n"
    "    using record = std::tuple<size_t, size_t, token>;\n"
    "    auto cmp = [](const record& a, const record& b) {\n"
    "        return std::get<0>(a) != std::get<0>(b) ?\n"
    "            std::get<0>(a) > std::get<0>(b) : std::get<1>(a) < std::get<1>(b);\n"
    "    };\n"
    "}\n\n";

    P0(cpp, "std::optional<std::pair<size_t, token>> try_tokenize(const std::string& word) {\n");
    P1(cpp, "std::set<record, decltype(cmp)> s(cmp);\n");
    P1(cpp, "size_t len;\n");
    for (int i = 0; i < names.size(); ++i) {
        auto& name = names[i];
        P1(cpp, "len = graphs[", i, "].greed_search(word);\n");
        P1(cpp, "if (len != 0) {\n");
        P2(cpp, "s.emplace(len, ", i, ", ", name);
        P0(cpp, "{", 0, ", word.substr(0, len)});\n");
        P1(cpp, "}\n");
    }
    P1(cpp, "if (s.empty()) {\n");
    P2(cpp, "return std::nullopt;\n")
    P1(cpp, "} else {\n");
    P2(cpp, "auto&[len, order, token] = *s.begin();\n");
    P2(cpp, "return std::make_pair(len, token);\n");
    P1(cpp, "}\n}\n\n");

    P0(cpp, "void init() {\n");
    for (auto& name: checker.tokens_order) {
        auto& token = tokens.at(name);
        if (name != "EOF") {
            P1(cpp, "{ // ", name, "\n");
            P2(cpp, "DFA::graph gr;\n");
            auto& gr = token.t.get().graph.get_graph();
            auto& ac = token.t.get().graph.get_acceptable();

            for (auto&[i, edges]: gr) {
                P2(cpp, "gr[", i, "] = std::set<std::pair<std::string, size_t>>({");
                if (edges.empty()) {
                    P0(cpp, "});\n");
                } else {
                    P0(cpp, "\n");
                    for (auto&[label, j]: edges) {
                        P3(cpp, "std::make_pair(\"", label, "\", ", j, "),\n");
                    }
                    P2(cpp, "});\n");
                }
            }
            P2(cpp, "std::unordered_set<size_t> ac;\n");
            for (auto& i: ac) {
                if (i != 0) { // do not allow zero-length tokens
                    P2(cpp, "ac.insert(", i, ");\n");
                }
            }
            P2(cpp, "graphs.emplace_back(std::move(gr), std::move(ac));\n");
            P1(cpp, "}\n");
        }
    }
    P0(cpp, "}\n");

    if (cpp.fail() || h.fail()) {
        std::stringstream err;
        err << "Error: failed to write \"";
        err << (cpp.fail() ? user_cpp : user_header).str();
        err << "\"";
        throw std::runtime_error(err.str());
    }
}

void write_user_nodes(
    const char* dir,
    const utils::checker& checker) {
    
    const auto& sections = checker.sections;
    const auto& embeddings = checker.embeds;
    const auto& tokens = checker.tokens;
    const auto& rules = checker.rules;
    
    std::stringstream user_header;
    std::stringstream user_cpp;

    user_header << dir << "/" << "user_nodes.h";
    user_cpp << dir << "/" << "user_nodes.cpp";

    std::ofstream cpp(user_cpp.str());
    std::ofstream h(user_header.str());

    if (!cpp.is_open() || !h.is_open()) {
        std::stringstream err;
        err << "Error: can't create file \"";
        err << (!cpp.is_open() ? user_cpp : user_header).str();
        err << "\"";
        throw std::runtime_error(err.str());
    }

    P0(h, "#include \"node.h\"\n\n");
    P0(cpp, "#include \"user_nodes.h\"\n\n");

    for (auto&[name, section]: sections) {
        if (name == "header") {
            P0(h, section.get().block, "\n");
        }
    }

    for (auto&[name, bunch]: rules) {
        P0(h, "struct ", name, " : nodes::node {\n");
        size_t args = bunch.args.size();
        size_t templates = 0;
        for (auto& arg: bunch.args) {
            if (trim(arg.type).empty()) {
                templates++;
            }
        }
        if (templates > 0) {
            P1(h, "template <");
            for (int i = 1; i <= templates; ++i) {
                h << "typename T" << i;
                if (i != templates) {
                    h << ", ";
                }
            }
            h << ">\n";
        }
        P1(h, name, "(tokenizer& tr");
        if (args > 0) {
            for (int i = 0, j = 1; i < args; ++i) {
                auto& arg = bunch.args[i];
                if (trim(arg.type).empty()) {
                    h << ", " << "const T" << j++ << "&";
                } else {
                    h << ", " << trim(arg.type);
                }
                h << " " << trim(arg.name);
            }
        }
        h << ");\n";

        if (bunch.ret.has_value()) {
            auto& ret = *bunch.ret;
            P1(h, trim(ret.type), " ", ret.name);
            if (!trim(ret.default_value).empty()) {
                h << " = " << trim(ret.default_value);
            }
            h << ";\n";
        }

        h << "};\n\n";
    }

    for (auto&[name, bunch]: rules) {
        size_t args = bunch.args.size();
        size_t templates = 0;
        for (auto& arg: bunch.args) {
            if (trim(arg.type).empty()) {
                templates++;
            }
        }
        if (templates > 0) {
            cpp << "template <";
            for (int i = 1; i <= templates; ++i) {
                cpp << "typename T" << i;
                if (i != templates) {
                    cpp << ", ";
                }
            }
            cpp << ">\n";
        }
        cpp << name << "::" << name << "(tokenizer& tr";
        if (args > 0) {
            for (int i = 0, j = 1; i < args; ++i) {
                auto& arg = bunch.args[i];
                if (trim(arg.type).empty()) {
                    cpp << ", " << "const T" << j << "&";
                } else {
                    cpp << ", " << trim(arg.type);
                }
                cpp << " " << trim(arg.name);
                if (!trim(arg.default_value).empty()) {
                    cpp << " = " << trim(arg.default_value);
                }
                if (trim(arg.type).empty()) {
                    j++;
                }
            }
        }
        cpp << ") {\n";
        P1(cpp, "auto t = tr.curToken();\n");
        
        #define esc(str) (str == "EOF" ? "$" : str)

        P1(cpp, "ensure<");
        bool started = false;
        for (auto& str: bunch.first) {
            if (utils::isToken(str)) {
                cpp << (started ? ", " : "") << esc(str);
                started = true;
            }
        }
        if (bunch.accepts_eps) {
            for (auto& str: bunch.follow) {
                if (utils::isToken(str)) {
                    cpp << (started ? ", " : "") << esc(str);
                    started = true;
                }
            }
        }
        cpp << ">(t);\n\n";

        started = false;
        for (auto& rule: bunch.rules) {
            if (!started) {
                P1(cpp, "if (");
            } else {
                cpp << " else if (";
            }
            bool started2 = false;
            cpp << "safeEnsure<";
            for (auto& str: rule.first) {
                cpp << (started2 ? ", " : "") << esc(str);
                started2 = true;
            }
            cpp << ">::ensure(t)) {\n";

            std::unordered_set<std::string> identifiers;

            bool write_arguments = false;
            for (auto& str: rule.sequence) {
                if (utils::isArgument(str)) {
                    cpp << ", " << str;

                } else {
                    if (write_arguments) {
                        cpp << ");\n";
                        write_arguments = false;
                    }

                    if (utils::isToken(str)) {
                        P2(cpp, !identifiers.count(str) ? "auto " : "");
                        P0(cpp, "_", esc(str), " = expectTerminal<", esc(str), ">(tr);\n");
                        identifiers.insert(str);
                    } else if (utils::isRule(str)) {
                        P2(cpp, !identifiers.count(str) ? "auto* " : "");
                        P0(cpp, "_", str, " = expectNonTerminal<", str, ">(tr");
                        identifiers.insert(str);
                        write_arguments = true;
                    } else {
                        P2(cpp, embeddings.at(str).get().embed, "\n");
                    }
                }
            }
            if (write_arguments) {
                cpp << ");\n";
            }

            P1(cpp, "}");
            started = true;
        }
        cpp << "\n";

        cpp << "}\n\n";
    }

    if (cpp.fail() || h.fail()) {
        std::stringstream err;
        err << "Error: failed to write \"";
        err << (cpp.fail() ? user_cpp : user_header).str();
        err << "\"";
        throw std::runtime_error(err.str());
    }
}