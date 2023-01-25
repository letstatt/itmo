#include <unordered_set>
#include <unordered_map>
#include <vector>
#include <set>

struct DFA {
    using graph = std::unordered_map<
        size_t,
        std::set<std::pair<std::string, size_t>>
    >;

    DFA(graph&& g, std::unordered_set<size_t>&& acceptable)
    : g(std::move(g)), acceptable(std::move(acceptable)) {}

    size_t greed_search(const std::string& input) {
        size_t v = 0;
        size_t len = 0;
        
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
                return len;
            }
            if (acceptable.count(v)) {
                len = i + 1;
                
            }
        }
        return len;
    }

private:
    graph g;
    std::unordered_set<size_t> acceptable;
};
