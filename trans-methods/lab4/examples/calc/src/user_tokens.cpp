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
        s.emplace(len, 0, Plus{0, word.substr(0, len)});
    }
    len = graphs[1].greed_search(word);
    if (len != 0) {
        s.emplace(len, 1, Minus{0, word.substr(0, len)});
    }
    len = graphs[2].greed_search(word);
    if (len != 0) {
        s.emplace(len, 2, Pow{0, word.substr(0, len)});
    }
    len = graphs[3].greed_search(word);
    if (len != 0) {
        s.emplace(len, 3, Mul{0, word.substr(0, len)});
    }
    len = graphs[4].greed_search(word);
    if (len != 0) {
        s.emplace(len, 4, Div{0, word.substr(0, len)});
    }
    len = graphs[5].greed_search(word);
    if (len != 0) {
        s.emplace(len, 5, LP{0, word.substr(0, len)});
    }
    len = graphs[6].greed_search(word);
    if (len != 0) {
        s.emplace(len, 6, RP{0, word.substr(0, len)});
    }
    len = graphs[7].greed_search(word);
    if (len != 0) {
        s.emplace(len, 7, Int{0, word.substr(0, len)});
    }
    len = graphs[8].greed_search(word);
    if (len != 0) {
        s.emplace(len, 8, Space{0, word.substr(0, len)});
    }
    len = graphs[9].greed_search(word);
    if (len != 0) {
        s.emplace(len, 9, LSB{0, word.substr(0, len)});
    }
    len = graphs[10].greed_search(word);
    if (len != 0) {
        s.emplace(len, 10, RSB{0, word.substr(0, len)});
    }
    len = graphs[11].greed_search(word);
    if (len != 0) {
        s.emplace(len, 11, Comma{0, word.substr(0, len)});
    }
    if (s.empty()) {
        return std::nullopt;
    } else {
        auto&[len, order, token] = *s.begin();
        return std::make_pair(len, token);
    }
}

void init() {
    { // Plus
        DFA::graph gr;
        gr[1] = std::set<std::pair<std::string, size_t>>({});
        gr[0] = std::set<std::pair<std::string, size_t>>({
            std::make_pair("+", 1),
        });
        std::unordered_set<size_t> ac;
        ac.insert(1);
        graphs.emplace_back(std::move(gr), std::move(ac));
    }
    { // Minus
        DFA::graph gr;
        gr[1] = std::set<std::pair<std::string, size_t>>({});
        gr[0] = std::set<std::pair<std::string, size_t>>({
            std::make_pair("-", 1),
        });
        std::unordered_set<size_t> ac;
        ac.insert(1);
        graphs.emplace_back(std::move(gr), std::move(ac));
    }
    { // Pow
        DFA::graph gr;
        gr[2] = std::set<std::pair<std::string, size_t>>({});
        gr[0] = std::set<std::pair<std::string, size_t>>({
            std::make_pair("*", 1),
        });
        gr[1] = std::set<std::pair<std::string, size_t>>({
            std::make_pair("*", 2),
        });
        std::unordered_set<size_t> ac;
        ac.insert(2);
        graphs.emplace_back(std::move(gr), std::move(ac));
    }
    { // Mul
        DFA::graph gr;
        gr[1] = std::set<std::pair<std::string, size_t>>({});
        gr[0] = std::set<std::pair<std::string, size_t>>({
            std::make_pair("*", 1),
        });
        std::unordered_set<size_t> ac;
        ac.insert(1);
        graphs.emplace_back(std::move(gr), std::move(ac));
    }
    { // Div
        DFA::graph gr;
        gr[1] = std::set<std::pair<std::string, size_t>>({});
        gr[0] = std::set<std::pair<std::string, size_t>>({
            std::make_pair("/", 1),
        });
        std::unordered_set<size_t> ac;
        ac.insert(1);
        graphs.emplace_back(std::move(gr), std::move(ac));
    }
    { // LP
        DFA::graph gr;
        gr[1] = std::set<std::pair<std::string, size_t>>({});
        gr[0] = std::set<std::pair<std::string, size_t>>({
            std::make_pair("(", 1),
        });
        std::unordered_set<size_t> ac;
        ac.insert(1);
        graphs.emplace_back(std::move(gr), std::move(ac));
    }
    { // RP
        DFA::graph gr;
        gr[1] = std::set<std::pair<std::string, size_t>>({});
        gr[0] = std::set<std::pair<std::string, size_t>>({
            std::make_pair(")", 1),
        });
        std::unordered_set<size_t> ac;
        ac.insert(1);
        graphs.emplace_back(std::move(gr), std::move(ac));
    }
    { // Int
        DFA::graph gr;
        gr[3] = std::set<std::pair<std::string, size_t>>({
            std::make_pair("0", 3),
            std::make_pair("1", 3),
            std::make_pair("2", 3),
            std::make_pair("3", 3),
            std::make_pair("4", 3),
            std::make_pair("5", 3),
            std::make_pair("6", 3),
            std::make_pair("7", 3),
            std::make_pair("8", 3),
            std::make_pair("9", 3),
        });
        gr[2] = std::set<std::pair<std::string, size_t>>({
            std::make_pair("0", 3),
            std::make_pair("1", 3),
            std::make_pair("2", 3),
            std::make_pair("3", 3),
            std::make_pair("4", 3),
            std::make_pair("5", 3),
            std::make_pair("6", 3),
            std::make_pair("7", 3),
            std::make_pair("8", 3),
            std::make_pair("9", 3),
        });
        gr[0] = std::set<std::pair<std::string, size_t>>({
            std::make_pair("0", 1),
            std::make_pair("1", 2),
            std::make_pair("2", 2),
            std::make_pair("3", 2),
            std::make_pair("4", 2),
            std::make_pair("5", 2),
            std::make_pair("6", 2),
            std::make_pair("7", 2),
            std::make_pair("8", 2),
            std::make_pair("9", 2),
        });
        gr[1] = std::set<std::pair<std::string, size_t>>({});
        std::unordered_set<size_t> ac;
        ac.insert(3);
        ac.insert(1);
        ac.insert(2);
        graphs.emplace_back(std::move(gr), std::move(ac));
    }
    { // Space
        DFA::graph gr;
        gr[1] = std::set<std::pair<std::string, size_t>>({});
        gr[0] = std::set<std::pair<std::string, size_t>>({
            std::make_pair(" ", 1),
        });
        std::unordered_set<size_t> ac;
        ac.insert(1);
        graphs.emplace_back(std::move(gr), std::move(ac));
    }
    { // LSB
        DFA::graph gr;
        gr[1] = std::set<std::pair<std::string, size_t>>({});
        gr[0] = std::set<std::pair<std::string, size_t>>({
            std::make_pair("[", 1),
        });
        std::unordered_set<size_t> ac;
        ac.insert(1);
        graphs.emplace_back(std::move(gr), std::move(ac));
    }
    { // RSB
        DFA::graph gr;
        gr[1] = std::set<std::pair<std::string, size_t>>({});
        gr[0] = std::set<std::pair<std::string, size_t>>({
            std::make_pair("]", 1),
        });
        std::unordered_set<size_t> ac;
        ac.insert(1);
        graphs.emplace_back(std::move(gr), std::move(ac));
    }
    { // Comma
        DFA::graph gr;
        gr[1] = std::set<std::pair<std::string, size_t>>({});
        gr[0] = std::set<std::pair<std::string, size_t>>({
            std::make_pair(",", 1),
        });
        std::unordered_set<size_t> ac;
        ac.insert(1);
        graphs.emplace_back(std::move(gr), std::move(ac));
    }
}
