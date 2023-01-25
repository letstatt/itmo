#include <unordered_set>
#include <unordered_map>
#include <sstream>
#include <vector>
#include <queue>
#include <set>
#include <map>

struct NFA {
    using graph = std::unordered_map<
        size_t,
        std::set<std::pair<std::string, size_t>>
    >;

    NFA() {
        g[0] = {};
        g[1] = {};
        acceptable.insert(1);
    }

    size_t add_vertex() {
        size_t v = g.size();
        g[v] = {};
        return v;
    }

    void add_edge(size_t a, size_t b, std::string str) {
        if (a == b && str.empty()) return; // no e-loops
        g[a].insert({str, b});
        last_edge = {a, b};
    }

    bool accepts(size_t v) const {
        return acceptable.count(v);
    }

    const graph& get_graph() const {
        return g;
    }

    const auto& get_acceptable() const {
        return acceptable;
    }

    char split_last_edge() {
        std::optional<std::pair<std::string, size_t>> edge;
        for (auto &i: g[last_edge.first]) {
            if (i.second == last_edge.second) {
                if (!edge) {
                    edge = i;
                } else {
                    throw std::runtime_error("edge splitting assertion (ambiguous)");
                }
            }
        }
        if (!edge) {
            throw std::runtime_error("edge splitting assertion (no edges)");
        }
        auto& pair = *edge;
        if (pair.first.empty()) {
            throw std::runtime_error("edge splitting assertion (eps-edge)");
        }
        g[last_edge.first].erase(pair);
        char last_char = pair.first.back();
        pair.first.pop_back();
        g[last_edge.first].insert(pair);
        return last_char;
    }

    void noeps() const {
        std::unordered_map<size_t, std::unordered_set<size_t>> m;
        
        // add extra eps-edges
        for (auto &[i, edges]: g) {
            for (auto&[label, j]: edges) {
                if (label.empty()) {
                    m[i].insert(j);
                }
            }
        }
        for (int k = 0; k < g.size(); ++k) {
            for (int i = 0; i < g.size(); ++i) {
                for (int j = 0; j < g.size(); ++j) {
                    if (m[i].count(j) || (m[i].count(k) && m[k].count(j))) {
                        g[i].emplace("", j);
                        m[i].insert(j);
                    }
                }
            }
        }
        m.clear();

        // add extra acceptable states
        for (auto &[i, edges]: g) {
            for (auto&[label, j]: edges) {
                if (label.empty()) {
                    m[j].insert(i); // reverse eps-edges
                }
            }
        }
        std::queue<size_t> q;
        q.push(1);
        while (!q.empty()) {
            int v = q.front();
            q.pop();
            if (m.count(v) && acceptable.count(v)) {
                for (auto& u: m[v]) {
                    acceptable.insert(u);
                    if (m.count(u)) {
                        q.push(u);
                    }
                }
            }
            m.erase(v);
        }

        // add extra non-eps edges
        std::unordered_map<size_t, std::vector<std::pair<std::string, size_t>>> m2;
        for (auto &[i, edges]: g) {
            for (auto&[label, j]: edges) {
                if (label.empty()) {
                    for (auto&[label, k]: g[j]) {
                        if (!label.empty()) {
                            m2[i].push_back(make_pair(label, k));
                        }
                    }
                }
            }
        }
        for (auto&[i, vec]: m2) {
            for (auto& pair: vec) {
                g[i].insert(pair);
            }
        }
        m2.clear();

        // eps-edges elimination
        for (auto &[i, edges]: g) {
            for (auto&[label, j]: edges) {
                if (label.empty()) {
                    m2[i].push_back(make_pair(label, j));
                }
            }
        }
        for (auto&[i, vec]: m2) {
            for (auto& pair: vec) {
                g[i].erase(pair);
            }
        }
        m2.clear();

        // cleaning
        std::unordered_set<size_t> to_kill;
        for (auto&[i, edges]: g) {
            to_kill.insert(i);
        }
        q.push(0);
        while(!q.empty()) {
            int v = q.front();
            q.pop();

            to_kill.erase(v);
            for (auto&[label, u]: g[v]) {
                if (!to_kill.count(u)) {
                    continue;
                }
                q.push(u);
            }
        }
        for (auto i: to_kill) {
            g.erase(i);
        }
    }

    void toDFA() const {
        graph dfa;
        std::unordered_set<size_t> acceptable2;
        std::map<std::set<int>, int> dict;
        std::queue<std::set<int>> q;
        q.push({0});
        dict[q.front()] = 0;
        if (acceptable.count(0)) {
            acceptable2.insert(0);
        }

        /*static const std::vector<char> alphabet = {
            'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
            'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
            's', 't', 'u', 'v', 'w', 'x', 'y', 'z', 'A',
            'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J',
            'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S',
            'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '0', '1',
            '2', '3', '4', '5', '6', '7', '8', '9'
        };*/

        while (!q.empty()) {
            auto s = q.front();
            q.pop();

            for (char c = 32; c < 127; c++) {
                if (c == 39) continue;
                std::set<int> t;
                bool accepts = false;

                for (auto &i: s) {
                    for (auto &[label, j]: g[i]) {
                        if (label[0] == c) {
                            t.insert(j);
                            accepts |= bool(acceptable.count(j));
                        }
                    }
                }
                if (!t.empty()) {
                    if (dict.find(t) == dict.end()) {
                        if (accepts) {
                            acceptable2.insert(dict.size());
                        }
                        dict[t] = dict.size();
                        q.push(t);
                    } else if (accepts) {
                        acceptable2.insert(dict[t]);
                    }
                    dfa[dict[s]].insert(make_pair(std::string{c}, dict[t]));
                    dfa[dict[t]];
                }
            }
        }
        std::unordered_set<size_t> to_kill;
        for (auto&[i, edges]: dfa) {
            to_kill.insert(i);
        }
        std::queue<size_t> q2;
        q2.push(0);
        while(!q2.empty()) {
            int v = q2.front();
            q2.pop();

            to_kill.erase(v);
            for (auto&[label, u]: dfa[v]) {
                if (!to_kill.count(u)) {
                    continue;
                }
                q2.push(u);
            }
        }
        for (auto i: to_kill) {
            dfa.erase(i);
        }
        swap(g, dfa);
        swap(acceptable, acceptable2);
    }

    std::string graphviz() const {
        std::stringstream str;
        str << "digraph G {\n";
        for (auto &[i, edges]: g) {
            if (acceptable.count(i)) {
                str << "node [shape = doublecircle]; V" << i << ";\n";
            } else {
                str << "node [shape = circle];\n";
                str << 'V' << i << " [label=\"" << 'V' << i  << "\"];\n";
            }
        }

        for (auto &[i, edges]: g) {
            for (auto&[label, j]: edges) {
                str << 'V' << i << " -> " << 'V' << j << "[label = \"" << label << "\"];\n";
            }
        }
        str << "}\n";
        return str.str();
    }

    size_t greed_search(const std::string& input) {
        // run only after determinization
        size_t v = 0;
        
        for (int i = 0; i < input.size(); ++i) {
            bool found = false;
            for (auto&[label, u]: g[v]) {
                if (label == std::string{input[i]}) {
                    v = u;
                    found = true;
                    break;
                }
            }
            if (!found) {
                return 0;
            }
            if (acceptable.count(v)) {
                return i + 1;
            }
        }
        return 0;
    }


private:
    mutable graph g;
    std::pair<size_t, size_t> last_edge;
    mutable std::unordered_set<size_t> acceptable;
};