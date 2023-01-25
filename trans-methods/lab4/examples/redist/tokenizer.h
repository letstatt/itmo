#include "user_tokens.h"
#include <fstream>
#include <optional>
#include <sstream>

namespace {
    bool is_whitespace(char c) {
        return c == '\r' || c == '\n' || c == ' ' || c == '\t';
    }

    bool is_allowed_whitespace(char c) {
        return c == ' ' || c == '\t';
    }
}

struct tokenizer {
    tokenizer(const char* filepath) {
        std::ifstream file(filepath);
        file.unsetf(std::ios_base::skipws);
        if (!file.is_open()) {
            throw std::runtime_error("Unable to open the file");
        }
        std::vector<std::string> splitted;
        std::string word;
        char c;
        while (file >> c) {
            if (is_whitespace(c) && !is_allowed_whitespace(c)) {
                if (!word.empty()) {
                    splitted.emplace_back(std::move(word));
                    word = {};
                }
            } else {
                word.push_back(c);
            }
        }
        if (!word.empty()) {
            splitted.emplace_back(std::move(word));
        }
        if (!file.eof() && file.bad()) {
            throw std::runtime_error("Reading error");
        }
        file.close();

        auto skipped_check = [](auto&& arg) -> bool {
            return arg.skip;
        };

        for (auto &s: splitted) {
            size_t skip = 0;
            while (skip < s.size()) {
                std::string word = s.substr(skip);
                std::optional<std::pair<size_t, token>> t;
                t = try_tokenize(word);

                if (!t) {
                    throw std::runtime_error("Error: token parsing failed");
                }

                auto& p = *t;
                skip += p.first;
                if (!std::visit(skipped_check, p.second)) {
                    tokens.push_back(p.second);
                }
            }
        }
    }

    token nextToken() {
        if (prevToken && std::holds_alternative<$>(*prevToken)) {
            throw std::runtime_error("Already reached the end");
        }

        if (pos == tokens.size()) {
            prevToken = ${0, "<end of input>"};
            return *prevToken;
        } else {
            prevToken = tokens[pos];
            pos++;
            return *prevToken;
        }
    }

    token curToken() {
        if (!prevToken) {
            throw std::runtime_error("Tokenizer is not initialized");
        }
        return *prevToken;
    }
    
    std::string content;
    std::optional<token> prevToken;
    std::vector<token> tokens;
    size_t pos = 0;
};
