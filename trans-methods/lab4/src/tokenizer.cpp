#include "tokenizer.h"

namespace {
    // routines
    bool is_whitespace(char c) {
        return c == '\t' || c == '\r' || c == '\n' || c == ' ';
    }

    bool is_allowed_whitespace(char c) {
        return c == '\t' || c == ' ';
    }

    bool is_lowercase(char c) {
        return (c >= 'a' && c <= 'z');
    }

    bool is_uppercase(char c) {
        return (c >= 'A' && c <= 'Z');
    }

    bool is_digit(char c) {
        return (c >= '0' && c <= '9');
    }

    bool is_alphanum(char c) {
        return is_lowercase(c) || is_uppercase(c) || is_digit(c);
    }

    bool is_section_name(const std::string& s) {
        if (s.size() < 2 || s[0] != '@') {
            return false;
        }
        for (int i = 1; i < s.size(); ++i) {
            if (!is_alphanum(s[i])) {
                return false;
            }
        }
        return true;
    }

    bool is_token_name(const std::string& s) {
        if (s.size() < 1 || !is_uppercase(s[0])) {
            return false;
        }
        for (int i = 1; i < s.size(); ++i) {
            if (!is_alphanum(s[i])) {
                return false;
            }
        }
        return true;
    }

    bool is_rule_name(const std::string& s) {
        if (s.size() < 1 || !is_lowercase(s[0])) {
            return false;
        }
        for (int i = 1; i < s.size(); ++i) {
            if (!is_alphanum(s[i])) {
                return false;
            }
        }
        return true;
    }

    bool is_attr_name(const std::string& s) {
        if (s.size() < 2 || s[0] != '$' || !is_lowercase(s[1])) {
            return false;
        }
        for (int i = 2; i < s.size(); ++i) {
            if (!is_alphanum(s[i])) {
                return false;
            }
        }
        return true;
    }

    bool is_embed_name(const std::string& s) {
        if (s.size() < 2 || s[0] != '$' || !is_uppercase(s[1])) {
            return false;
        }
        for (int i = 2; i < s.size(); ++i) {
            if (!is_alphanum(s[i])) {
                return false;
            }
        }
        return true;
    }
}

// tokenizer impl

tokenizer::tokenizer(const char* filepath) : file(filepath) {
    file.unsetf(std::ios_base::skipws);
    if (!file.is_open()) {
        throw std::runtime_error("Unable to open the file");
    }
}

#include <iostream>
#include "node.h"

token tokenizer::nextToken() {
    if (prevToken && std::holds_alternative<$>(*prevToken)) {
        throw std::runtime_error("Already reached the end");
    }

    std::optional<token> result;
    char c;

    while (file >> c) {
        pos++;

        // trim spaces
        if (is_whitespace(c) && context != QUOTE) {
            continue;

        // top-level
        } else if (context == OUTER) {
            std::string word{c};

            while ((file >> c) && is_alphanum(c)) {
                word.push_back(c);
            }
            if (!file.eof()) {
                file.unget();
            }

            if (is_section_name(word)) {
                result = SECTION_NAME(pos, word);
                context = SECTION;

            } else if (is_token_name(word)) {
                result = TOKEN_NAME(pos, word);
                context = TOKEN;

            } else if (is_rule_name(word)) {
                result = RULE_NAME(pos, word);
                context = RULE;

            } else if (is_embed_name(word)) {
                result = EMBED_NAME(pos, word);
                context = EMBEDDING;

            } else {
                throw parse_exception(c, pos);
            }

            pos += word.size() - 1;
            break;
        
        // section-level or embedding-level
        } else if (context == SECTION || context == EMBEDDING) {
            if (c == '{') {
                if (!prevToken.has_value() ||
                    (!std::holds_alternative<SECTION_NAME>(*prevToken)
                    && !std::holds_alternative<EMBED_NAME>(*prevToken))) {
                    throw parse_exception(c, pos);
                }
                result = LBRACE(pos);
                context = BLOCK;
            } else if (c == '}') {
                result = RBRACE(pos);
            } else if (c == ';') {
                result = SEMICOLON(pos);
                context = OUTER;
            } else {
                throw parse_exception(c, pos);
            }
            break;
        
        // block-level
        } else if (context == BLOCK) {
            std::string block{c};
            int balance = 1;

            while (file >> c) {
                if (c == '{') {
                    balance++;
                } else if (c == '}') {
                    if (--balance == 0) {
                        context = SECTION; // = EMBEDDING
                        break;
                    }
                }
                block.push_back(c);
                pos++;
            }
            if (!file.eof()) {
                file.unget();
            }
            result = ::BLOCK(pos - block.size(), block);
            break;
        
        // token-level
        } else if (context == TOKEN) {
            if (c == ':') {
                result = COLON(pos);
            } else if (c == '!') {
                result = EXCLAMATION_MARK(pos);
            } else if (is_alphanum(c)) {
                std::string word{c};
                while ((file >> c) && is_alphanum(c)) {
                    word.push_back(c);
                    pos++;
                }
                if (!file.eof()) {
                    file.unget();
                }
                result = ALPHANUM_SEQ(pos, word);
            } else if (c == '-') {
                result = MINUS(pos);
            } else if (c == '|') {
                result = BAR(pos);
            } else if (c == '?') {
                result = QUESTION_MARK(pos);
            } else if (c == '+') {
                result = PLUS_SIGN(pos);
            } else if (c == '*') {
                result = MUL_SIGN(pos);
            } else if (c == '(') {
                result = LBRACKET(pos);
            } else if (c == ')') {
                result = RBRACKET(pos);
            } else if (c == '[') {
                result = LSQBRACKET(pos);
            } else if (c == ']') {
                result = RSQBRACKET(pos);
            } else if (c == '\'') {
                result = QUOTATION_MARK(pos);
                context = QUOTE;
            } else if (c == ';') {
                result = SEMICOLON(pos);
                context = OUTER;
            } else {
                throw parse_exception(c, pos);
            }
            break;

        // escaped sequence
        } else if (context == QUOTE) {
            if (c == '\'') {
                if (std::holds_alternative<QUOTATION_MARK>(*prevToken)) {
                    file.unget();
                    pos--;
                    result = ESCAPED(pos, "");
                } else {
                    result = QUOTATION_MARK(pos);
                    context = TOKEN;
                }
            } else {
                std::string escaped{c};
                while ((file >> c) && c != '\'') {
                    if (is_whitespace(c) && !is_allowed_whitespace(c)) {
                        throw std::runtime_error(
                            "Error: escaped sequence contains prohibited whitespace"
                        );
                    }
                    escaped.push_back(c);
                    pos++;
                }
                if (!file.eof()) {
                    file.unget();
                }
                result = ESCAPED(pos, escaped);
            }
            break;

        // rule-level
        } else if (context == RULE) {
            if (c == '$') {
                std::string word{c};
                size_t old_pos = pos;
                while ((file >> c) && is_alphanum(c)) {
                    word.push_back(c);
                    pos++;
                }
                if (!file.eof()) {
                    file.unget();
                }
                if (word.size() == 1) {
                    throw parse_exception(c, pos + 1); // next to '$'
                } else if (is_attr_name(word)) {
                    if (!prevToken.has_value() || !(std::holds_alternative<ATTR_NAME>(*prevToken)
                    || std::holds_alternative<RULE_NAME>(*prevToken)
                    || std::holds_alternative<RETURN_ARROW>(*prevToken)
                    || std::holds_alternative<RBRACE>(*prevToken))) {
                        throw parse_exception(c, pos);
                    }
                    result = ATTR_NAME(pos, word);
                } else if (is_embed_name(word)) {
                    result = EMBED_NAME(pos, word);
                } else {
                    throw parse_exception(c, old_pos);
                }
            } else if (c == '{') {
                if (!prevToken.has_value() || !std::holds_alternative<ATTR_NAME>(*prevToken)) {
                    throw parse_exception(c, pos);
                }
                result = LBRACE(pos);
                context = ATTRIBUTE;
            } else if (c == '-') {
                if ((file >> c) && c == '>') {
                    result = RETURN_ARROW(pos);
                    pos++;

                } else {
                    throw parse_exception(c, pos);
                }
            } else if (c == ':') {
                result = COLON(pos);
            } else if (is_uppercase(c) || is_lowercase(c)) {
                std::string word{c};
                while ((file >> c) && is_alphanum(c)) {
                    word.push_back(c);
                }
                if (!file.eof()) {
                    file.unget();
                }

                if (is_uppercase(word[0])) {
                    result = TOKEN_NAME(pos, word);
                } else {
                    result = RULE_NAME(pos, word);
                }
                pos += word.size() - 1;
            } else if (c == ';') {
                result = SEMICOLON(pos);
                context = OUTER;
            } else {
                throw parse_exception(c, pos);
            }
            break;
        
        // attribute type and initializer
        } else if (context == ATTRIBUTE) {
            if (c == '=') {
                result = EQ(pos);
            } else if (c == '}') {
                result = RBRACE('}');
                context = RULE;
            } else {
                std::string word{c};
                while ((file >> c) && c != '=' && c != '}') {
                    word.push_back(c);
                    pos++;
                }
                if (!file.eof()) {
                    file.unget();
                }
                if (std::holds_alternative<EQ>(*prevToken)) {
                    result = ATTR_INITIALIZER(pos, word);
                } else {
                    result = ATTR_TYPE(pos, word);
                }
            }
            break;
        }
    }

    if (!result && file.eof()) {
        prevToken.emplace(${pos});
        return ${pos};

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
