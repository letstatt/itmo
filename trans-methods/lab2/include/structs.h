#pragma once
#include <variant>
#include <string>

#define TOKEN_REPR(str) \
    constexpr static const char* token_repr = str;

namespace {
    template <class>
    inline constexpr bool always_false_v = false;
}

template <typename T>
constexpr static const char* GET_TOKEN_REPR = T::token_repr;

// tokens
struct FUNC_DECL {
    TOKEN_REPR("fun")
};
struct NAME {
    TOKEN_REPR("<identifier>")
    std::string value;
};
struct LPAREN {
    TOKEN_REPR("(")
};
struct RPAREN {
    TOKEN_REPR(")")
};
struct LBRACKET {
    TOKEN_REPR("<")
};
struct RBRACKET {
    TOKEN_REPR(">")
};
struct COLON {
    TOKEN_REPR(":")
};
struct COMMA {
    TOKEN_REPR(",")
};
struct SPACE {
    TOKEN_REPR(" ")
};
struct $ {
    TOKEN_REPR("<end of input>")
};

// printer
constexpr auto printer = [](auto&& arg) -> const char* {
    using T = std::decay_t<decltype(arg)>;

    if constexpr (std::is_same_v<T, NAME>) {
        return ((NAME&) arg).value.c_str();
    } else {
        return GET_TOKEN_REPR<T>;
    }
};

// type alias
typedef std::variant<
    FUNC_DECL,
    NAME,
    LPAREN,
    RPAREN,
    LBRACKET,
    RBRACKET,
    COLON,
    COMMA,
    SPACE,
    $
> token;
