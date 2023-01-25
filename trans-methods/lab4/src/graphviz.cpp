#include "node.h"

namespace {
    int free_id = 0;
    nodes::terminal eps("eps");

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
}

std::string dfs(std::ostream &stream, nodes::protonode* p, std::string const& parent) {
    std::vector<nodes::protonode*>* p_ch = p->children();

    std::string tag = "V";
    tag += std::to_string(free_id++);

    stream << tag << " [label=\"" << trim(p->to_string()) << "\"];\n";

    if (p_ch) {
        if (p_ch->empty()) {
            auto ch_tag = dfs(stream, &eps, tag);
            stream << tag << " -> " << ch_tag << "\n";
        } else {
            for (auto i: *p_ch) {
                auto ch_tag = dfs(stream, i, tag);
                stream << tag << " -> " << ch_tag << "\n";
            }
        }
    }
    return tag;
}

std::string visualize_graphviz(nodes::protonode* p) {
    //std::fstream file(filepath, std::ios_base::out);
    std::stringstream s;

    s << "digraph G {\n";
    dfs(s, p, "");
    s << "}\n";
    return s.str();
}
