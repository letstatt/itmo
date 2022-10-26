#include <iostream>
#include <iomanip>
#include <optional>
#include <fstream>
#include "exception.h"
#include "nodes.h"

void usage();

nodes::S parse(const char*);

void visualize_graphviz(const char* filepath, nodes::protonode* p);

int main(int argc, char* argv[]) {
    if (argc != 2 && argc != 3) {
        usage();
        return 0;
    }

    try {
        auto s = parse(argv[1]);

        if (argc == 3) {
            visualize_graphviz(argv[2], &s);
        } else {
            std::cout << "OK" << std::endl;
        }

    } catch (parse_exception const& e) {
        std::cout << e.what() << std::endl;
        return 1;

    } catch (std::exception const& e) {
        std::cout << "[!] Unhandled exception: ";
        std::cout << e.what() << std::endl;
        return 2;
    }
    return 0;
}

void usage() {
    std::cout << "Kotlin-like function declaration parser\n";
    std::cout << "Author: letstatt\n\n";
    std::cout << "Usage: parser input [output]\n\n";
}