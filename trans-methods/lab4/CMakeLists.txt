cmake_minimum_required(VERSION 3.15)
project(parser)

set(CMAKE_CXX_STANDARD 20)

include_directories(include)
set(sources)

LIST(APPEND sources
    src/main.cpp
    src/grammar.cpp
    src/tokenizer.cpp
    src/parser.cpp
    src/graphviz.cpp
    src/codegen.cpp
    src/utils.cpp
    )

add_executable(parser ${sources})
