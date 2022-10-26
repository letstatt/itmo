#pragma once
#include <fstream>
#include <optional>
#include "structs.h"

struct tokenizer {
    tokenizer(const char*);

    token nextToken();

    token curToken();

private:
    std::ifstream file;
    std::optional<token> prevToken;
    std::optional<token> queued;
    int pos = 0;
};
