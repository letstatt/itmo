#pragma once
#include "tokenizer.h"
#include "exception.h"
#include <vector>

namespace nodes {

    struct protonode {
        template <typename T>
        static void ensure(const token& t) {
            if (!std::holds_alternative<T>(t)) {
                throw parse_exception(
                    GET_TOKEN_REPR<T>,
                    std::visit(printer, t)
                    );
            }
        }

        template <typename T, typename T2, typename... U>
        static void ensure(const token& t) {
            if (!safeEnsure<T, T2, U...>::ensure(t)) {
                throw parse_exception(std::visit(printer, t));
            }
        }

        virtual std::vector<protonode*>* children() = 0;

        virtual std::string to_string() = 0;

    private:
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
        void expectTerminal(tokenizer & tr) {
            ensure<T>(tr.curToken());
            ch.push_back(new terminal{
                std::visit(printer, tr.curToken())
            });
            tr.nextToken();
        }

        template <typename N>
        void expectNonTerminal(tokenizer & tr) {
            ch.push_back(new N(tr));
        }

        virtual std::vector<protonode*>* children() {
            return &ch;
        }
    };

}
