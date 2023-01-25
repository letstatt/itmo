#pragma once
#include <fstream>
#include <optional>
#include "tokens.h"
#include "exception.h"

struct tokenizer {
    tokenizer(const char*);

    token nextToken();

    token curToken();

private:
    std::ifstream file;
    std::optional<token> prevToken;
    enum {
        OUTER,
        BLOCK,
        SECTION,
        TOKEN,
        QUOTE,
        ATTRIBUTE,
        EMBEDDING,
        RULE
    } context = OUTER;
    size_t pos = 0;
};
