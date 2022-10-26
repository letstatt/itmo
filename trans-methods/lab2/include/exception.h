#pragma once
#include <stdexcept>

struct parse_exception : std::exception {
    std::string msg;

    parse_exception(char got)
        : std::exception() {
        msg.append("Unexpected token: \"");
        msg.push_back(got);
        msg.append("\"");
    }

    parse_exception(const char* got)
        : std::exception() {
        msg.append("Unexpected token: \"");
        msg.append(got);
        msg.append("\"");
    }

    parse_exception(const char* expected, const char* got)
        : std::exception() {
        msg.append("Expected token: \"");
        msg.append(expected);
        msg.append("\", got: \"");
        msg.append(got);
        msg.append("\"");
    }
    
    virtual const char* what() const noexcept {
        return msg.c_str();
    }
};
