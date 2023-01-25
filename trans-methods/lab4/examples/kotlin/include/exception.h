#pragma once
#include <stdexcept>
#include <sstream>

struct parse_exception : std::exception {
    std::string msg;

    parse_exception(char got, size_t pos)
        : std::exception() {
        std::stringstream s;
        s << pos;
        msg.append("Unexpected token: \"");
        msg.push_back(got);
        msg.append("\" at pos ");
        msg.append(s.str());
    }

    parse_exception(const char* got, size_t pos)
        : std::exception() {
        std::stringstream s;
        s << pos;
        msg.append("Unexpected token: \"");
        msg.append(got);
        msg.append("\" at pos ");
        msg.append(s.str());
    }

    parse_exception(const char* expected, const char* got, size_t pos)
        : std::exception() {
        std::stringstream s;
        s << pos;
        msg.append("Expected token: \"");
        msg.append(expected);
        msg.append("\", got: \"");
        msg.append(got);
        msg.append("\" at pos ");
        msg.append(s.str());
    }
    
    virtual const char* what() const noexcept {
        return msg.c_str();
    }
};
