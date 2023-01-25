#pragma once
#include "graph.h"
#include <variant>
#include <string>
#include <vector>

template <typename T>
constexpr static const char* GET_TOKEN_REPR = T::token_repr;

struct token_base {
    explicit token_base(size_t pos) : pos(pos) {}
    size_t pos;
};

// macro
#define STRING_TOKEN(name, _repr) \
struct name : token_base { \
    explicit name(size_t pos, std::string&& s) : token_base(pos), text(std::move(s)) {} \
    explicit name(size_t pos, const std::string& s) : token_base(pos), text(std::move(s)) {} \
    constexpr static const char* token_repr = _repr; \
    std::string text; \
    bool skip = false; \
    const char* repr() const { \
        return text.c_str();\
    }\
};

#define SKIPPED_TOKEN(name, _repr) \
struct name : token_base { \
    explicit name(size_t pos, std::string&& s) : token_base(pos), text(std::move(s)) {} \
    explicit name(size_t pos, const std::string& s) : token_base(pos), text(std::move(s)) {} \
    constexpr static const char* token_repr = _repr; \
    std::string text; \
    bool skip = true; \
    const char* repr() const { \
        return text.c_str();\
    }\
};

// tokens

STRING_TOKEN($, "<end of input>");
