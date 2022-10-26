#include "node.h"

namespace {
    int free_id = 0;
    nodes::terminal eps("eps");
}

std::string dfs(std::ostream &stream, nodes::protonode* p, std::string const& parent) {
    std::vector<nodes::protonode*>* p_ch = p->children();

    std::string tag = "V";
    tag += std::to_string(free_id++);

    stream << tag << " [label=\"" << p->to_string() << "\"];\n";

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

void visualize_graphviz(const char* filepath, nodes::protonode* p) {
    std::fstream file(filepath, std::ios_base::out);

    file << "digraph G {\n";
    dfs(file, p, "");
    file << "}\n";
}
