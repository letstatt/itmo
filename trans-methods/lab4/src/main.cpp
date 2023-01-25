#include "codegen.h"
#include "grammar.h"
#include <iostream>
#include <string.h>

template <typename... Args>
[[noreturn]] void usage(Args...);
[[noreturn]] void usage();

void compile(const char** args);
void graphviz(const char** args);

int main(int argc, char **argv) {
    if (argc <= 1) {
        usage();
    }

    char* action = argv[1];

    try {
        if (!strcmp(action, "compile")) {
            compile(const_cast<const char**>(argv + 2));
        } else if (!strcmp(action, "graphviz")) {
            graphviz(const_cast<const char**>(argv + 2));
        } else {
            usage("unknown action \"", action, "\"");
        }
    } catch (const std::runtime_error& e) {
        std::cout << e.what() << std::endl;
        return 1;
    }
    return 0;
}

static const char *msg =
    "trm by letstatt.\n"
    "Usage:\n\n"
    "trm compile  <file.g> [options]         Compiles grammar into lexer and parser\n"
    "trm graphviz <file.g>                   Prints graphviz representation\n\n"
    "Options:\n"
    "-o      folder    [default=output]     Output folder\n";

template <typename... Args>
void usage(Args... args) {
    std::cout << "Error: ";
    (std::cout << ... << args);
    std::cout << std::endl;
    exit(1);
}

void usage() {
    std::cout << msg;
    exit(0);
}

#define SETOPTION(name, arg, var) \
    if (!arg) { \
        usage(name " option expected");       \
    } else if (var) { \
        usage("-" name " option was encountered twice");        \
    } else {         \
        var = arg;            \
    }

#define SETFILE(arg, var) \
    if (var) { \
        usage("only one grammar file could be passed");        \
    } else {         \
        var = arg;            \
    }

void graphviz(const char** args) {
    const char* file = nullptr;
    for (size_t i = 0; args[i]; i++) {
        if (!strcmp(args[i], "-o")) {
        } else {
            SETFILE(args[i], file);
        }
    }
    if (!file) {
        usage("no grammar file given");
    }
    auto gram = grammar(file);
    std::cout << gram.graphviz();
}

void compile(const char** args) {
    const char* file = nullptr;
    const char* output_folder = nullptr;

    for (size_t i = 0; args[i]; i++) {
        if (!strcmp(args[i], "-o")) {
            SETOPTION("o", args[i + 1], output_folder);
            i += 1;
        } else {
            SETFILE(args[i], file);
        }
    }

    if (!file) {
        usage("no grammar file given");
    }
    if (!output_folder) {
        output_folder = "output";
    }

    std::cout << "[*] Parsing..." << std::endl;
    auto gram = grammar(file);

    std::cout << "[*] Build grammar..." << std::endl;
    auto checker = gram.build();

    if (!checker.isLL1) {
        throw std::runtime_error("Error: grammar is not LL(1)");
    }

    write_user_tokens(output_folder, checker);
    write_user_nodes(output_folder, checker);
    std::cout << "[*] Done!" << std::endl;
}
