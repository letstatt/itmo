#pragma once
#include <variant>
#include <string>

template <typename T>
constexpr static const char* GET_TOKEN_REPR = T::token_repr;

struct token_base {
    explicit token_base(size_t pos) : pos(pos) {}
    size_t pos;
};

// macro
#define SIMPLE_TOKEN(name, repr) \
struct name : token_base { \
    explicit name(size_t pos) : token_base(pos) {} \
    constexpr static const char* token_repr = repr; \
};

#define STRING_TOKEN(name, _repr) \
struct name : token_base { \
    explicit name(size_t pos, std::string&& s) : token_base(pos), value(std::move(s)) {} \
    explicit name(size_t pos, const std::string& s) : token_base(pos), value(std::move(s)) {} \
    constexpr static const char* token_repr = _repr; \
    std::string value; \
    const char* repr() const { \
        return value.c_str();\
    }\
};

// tokens
STRING_TOKEN(SECTION_NAME, "<section name>");
STRING_TOKEN(BLOCK, "<block>");
STRING_TOKEN(TOKEN_NAME, "<token name>");
STRING_TOKEN(ALPHANUM_SEQ, "<alphanumerical sequence>");
STRING_TOKEN(ESCAPED, "<escaped sequence>");
STRING_TOKEN(ATTR_NAME, "<attribute name>");
STRING_TOKEN(ATTR_TYPE, "<attribute type>");
STRING_TOKEN(ATTR_INITIALIZER, "<attribute initializer>");
STRING_TOKEN(EMBED_NAME, "<embedded name>");
STRING_TOKEN(RULE_NAME, "<rule name>");

SIMPLE_TOKEN(RETURN_ARROW, "->");
SIMPLE_TOKEN(QUOTATION_MARK, "'");
SIMPLE_TOKEN(LBRACE, "{");
SIMPLE_TOKEN(RBRACE, "}");
SIMPLE_TOKEN(LSQBRACKET, "[");
SIMPLE_TOKEN(RSQBRACKET, "]");
SIMPLE_TOKEN(LBRACKET, "(");
SIMPLE_TOKEN(RBRACKET, ")");
SIMPLE_TOKEN(MINUS, "-");
SIMPLE_TOKEN(BAR, "|");
SIMPLE_TOKEN(QUESTION_MARK, "?");
SIMPLE_TOKEN(PLUS_SIGN, "+");
SIMPLE_TOKEN(MUL_SIGN, "*");
SIMPLE_TOKEN(EXCLAMATION_MARK, "!");
SIMPLE_TOKEN(COLON, ":");
SIMPLE_TOKEN(SEMICOLON, ";");
SIMPLE_TOKEN(EQ, "=");
SIMPLE_TOKEN($, "<end of input>");

// type alias
typedef std::variant<
    SECTION_NAME,
    BLOCK,
    TOKEN_NAME,
    ALPHANUM_SEQ,
    ESCAPED,
    ATTR_NAME,
    ATTR_TYPE,
    ATTR_INITIALIZER,
    EMBED_NAME,
    RULE_NAME,
    RETURN_ARROW,
    QUOTATION_MARK,
    LBRACE,
    RBRACE,
    LSQBRACKET,
    RSQBRACKET,
    LBRACKET,
    RBRACKET,
    MINUS,
    BAR,
    QUESTION_MARK,
    PLUS_SIGN,
    MUL_SIGN,
    EXCLAMATION_MARK,
    COLON,
    SEMICOLON,
    EQ,
    $
> token;
