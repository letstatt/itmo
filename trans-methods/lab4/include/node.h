#pragma once
#include "tokenizer.h"
#include "exception.h"
#include <vector>

namespace nodes {

    struct protonode {
        template <typename T, typename = const char*>
        struct has_custom_repr {
            constexpr static bool value = false;
        };

        template <typename T>
        struct has_custom_repr<T, decltype(std::declval<T>().repr())> {
            constexpr static bool value = true;
        };

        static constexpr auto repr = [](auto&& arg) -> const char* {
            using T = std::decay_t<decltype(arg)>;

            if constexpr(protonode::has_custom_repr<T>::value) {
                return arg.repr();
            } else {
                return GET_TOKEN_REPR<T>;
            }
        };

        static constexpr auto pos = [](auto&& arg) -> size_t {
            return arg.pos;
        };

        template <typename T>
        static void ensure(const token& t) {
            if (!std::holds_alternative<T>(t)) {
                throw parse_exception(
                    GET_TOKEN_REPR<T>,
                    std::visit(repr, t),
                    std::visit(pos, t)
                );
            }
        }

        template <typename T, typename T2, typename... U>
        static void ensure(const token& t) {
            if (!safeEnsure<T, T2, U...>::ensure(t)) {
                throw parse_exception(
                    std::visit(repr, t),
                    std::visit(pos, t)
                );
            }
        }

        virtual std::vector<protonode*>* children() = 0;

        virtual std::string to_string() = 0;

    protected:
        template <typename T, typename... U>
        struct safeEnsure {
            constexpr static bool ensure(const token& t) {
                return safeEnsure<T>::ensure(t)
                    || safeEnsure<U...>::ensure(t);
            }
        };

        template <typename T>
        struct safeEnsure<T> {
            constexpr static bool ensure(const token& t) {
                return std::holds_alternative<T>(t);
            }
        };
    };

    struct terminal : protonode {
        std::string val;

        terminal(const char* val) : val(val) {}

        virtual std::vector<protonode*>* children() {
            return nullptr;
        }

        virtual std::string to_string() {
            return val;
        }
    };

    struct node : protonode {
        std::vector<protonode*> ch;

        template <typename T>
        T expectTerminal(tokenizer & tr) {
            ensure<T>(tr.curToken());
            ch.push_back(new terminal{
                std::visit(repr, tr.curToken())
            });
            auto backup = tr.curToken();
            tr.nextToken();
            return std::get<T>(backup);
        }

        template <typename N>
        void expectNonTerminal(tokenizer & tr) {
            ch.push_back(new N(tr));
        }

        template <typename N, typename... Args>
        void expectNonTerminal(tokenizer & tr, Args&& ...args) {
            ch.push_back(new N(tr, std::forward<Args>(args)...));
        }

        virtual std::vector<protonode*>* children() {
            return &ch;
        }
    };
}
