#include "tokenizer.h"
#include "exception.h"
#include <stdexcept>
#include <iostream>

namespace {
    bool is_whitespace(char c) {
        return c == '\t' || c == ' ';
    }

    bool is_head(char c) {
        return (c >= 'a' && c <= 'z')
            || (c >= 'A' && c <= 'Z')
            || (c == '_');
    }

    bool is_tail(char c) {
        return is_head(c)
            || (c >= '0' && c <= '9');
    }
}

tokenizer::tokenizer(const char* filepath) : file(filepath) {
    file.unsetf(std::ios_base::skipws);
    if (!file.is_open()) {
        throw std::runtime_error("Unable to open the file");
    }
}

token tokenizer::nextToken() {
    if (queued) {
        prevToken = *queued;
        queued.reset();
        return *prevToken;
    }

    if (prevToken && std::holds_alternative<$>(*prevToken)) {
        throw std::runtime_error("Already reached the end");
    }

    std::optional<token> result;
    char c;

    while (file >> c) {
        pos++;

        // trim spaces
        if (is_whitespace(c)) {
            continue;

        // keywords and identifiers
        } else if (is_head(c)) {
            std::string word{c};
            while ((file >> c) && is_tail(c)) {
                word.push_back(c);
                pos++;
            }
            if (!file.eof()) {
                file.unget();
            }

            if (word == "fun") {
                result = FUNC_DECL{};
            } else {
                result = NAME{word};
            }

            if (prevToken &&
                (std::holds_alternative<FUNC_DECL>(*prevToken)
                || std::holds_alternative<NAME>(*prevToken))) {
                queued = result;
                result = SPACE{};
            }
            break;
        
        // special symbols
        } else {
            switch (c) {
                case '(':
                    result = LPAREN{};
                    break;
                case ')':
                    result = RPAREN{};
                    break;
                case '<':
                    result = LBRACKET{};
                    break;
                case '>':
                    result = RBRACKET{};
                    break;
                case ':':
                    result = COLON{};
                    break;
                case ',':
                    result = COMMA{};
                    break;
                default:
                    throw parse_exception(c);
            }
            break;
        }
    }

    if (!result && file.eof()) {
        prevToken.emplace(${});
        return ${};

    } else if (file.bad()) {
        throw std::runtime_error("Reading error");

    } else if (result) {
        prevToken = result;
        return *result;

    } else {
        throw std::runtime_error("Tokenizer got inconsistent state");
    }
}

token tokenizer::curToken() {
    if (!prevToken) {
        throw std::runtime_error("Tokenizer is not initialized");
    }
    return *prevToken;
}
