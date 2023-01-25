#include "user_tokens.h"
#include <tuple>
#include <set>
std::vector<DFA> graphs;

namespace {
    using record = std::tuple<size_t, size_t, token>;
    auto cmp = [](const record& a, const record& b) {
        return std::get<0>(a) != std::get<0>(b) ?
            std::get<0>(a) > std::get<0>(b) : std::get<1>(a) < std::get<1>(b);
    };
}

std::optional<std::pair<size_t, token>> try_tokenize(const std::string& word) {
    std::set<record, decltype(cmp)> s(cmp);
    size_t len;
    len = graphs[0].greed_search(word);
    if (len != 0) {
        s.emplace(len, 0, Char{0, word.substr(0, len)});
    }
    if (s.empty()) {
        return std::nullopt;
    } else {
        auto&[len, order, token] = *s.begin();
        return std::make_pair(len, token);
    }
}

void init() {
    { // Char
        DFA::graph gr;
        gr[2] = std::set<std::pair<std::string, size_t>>({});
        gr[0] = std::set<std::pair<std::string, size_t>>({
            std::make_pair(" ", 1),
            std::make_pair("0", 2),
            std::make_pair("1", 2),
            std::make_pair("2", 2),
            std::make_pair("3", 2),
            std::make_pair("4", 2),
            std::make_pair("5", 2),
            std::make_pair("6", 2),
            std::make_pair("7", 2),
            std::make_pair("8", 2),
            std::make_pair("9", 2),
            std::make_pair("A", 2),
            std::make_pair("B", 2),
            std::make_pair("C", 2),
            std::make_pair("D", 2),
            std::make_pair("E", 2),
            std::make_pair("F", 2),
            std::make_pair("G", 2),
            std::make_pair("H", 2),
            std::make_pair("I", 2),
            std::make_pair("J", 2),
            std::make_pair("K", 2),
            std::make_pair("L", 2),
            std::make_pair("M", 2),
            std::make_pair("N", 2),
            std::make_pair("O", 2),
            std::make_pair("P", 2),
            std::make_pair("Q", 2),
            std::make_pair("R", 2),
            std::make_pair("S", 2),
            std::make_pair("T", 2),
            std::make_pair("U", 2),
            std::make_pair("V", 2),
            std::make_pair("W", 2),
            std::make_pair("X", 2),
            std::make_pair("Y", 2),
            std::make_pair("Z", 2),
            std::make_pair("a", 2),
            std::make_pair("b", 2),
            std::make_pair("c", 2),
            std::make_pair("d", 2),
            std::make_pair("e", 2),
            std::make_pair("f", 2),
            std::make_pair("g", 2),
            std::make_pair("h", 2),
            std::make_pair("i", 2),
            std::make_pair("j", 2),
            std::make_pair("k", 2),
            std::make_pair("l", 2),
            std::make_pair("m", 2),
            std::make_pair("n", 2),
            std::make_pair("o", 2),
            std::make_pair("p", 2),
            std::make_pair("q", 2),
            std::make_pair("r", 2),
            std::make_pair("s", 2),
            std::make_pair("t", 2),
            std::make_pair("u", 2),
            std::make_pair("v", 2),
            std::make_pair("w", 2),
            std::make_pair("x", 2),
            std::make_pair("y", 2),
            std::make_pair("z", 2),
        });
        gr[1] = std::set<std::pair<std::string, size_t>>({});
        std::unordered_set<size_t> ac;
        ac.insert(2);
        ac.insert(1);
        graphs.emplace_back(std::move(gr), std::move(ac));
    }
}
