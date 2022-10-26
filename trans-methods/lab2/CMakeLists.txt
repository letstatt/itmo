cmake_minimum_required(VERSION 3.15)
project(parser)

set(CMAKE_CXX_STANDARD 20)

include_directories(include)
set(sources)

LIST(APPEND sources
    "src/main.cpp"
    "src/parser.cpp"
    "src/tokenizer.cpp"
    "src/graphviz.cpp"
    "src/nodes/A.cpp"
    "src/nodes/B.cpp"
    "src/nodes/E.cpp"
    "src/nodes/F.cpp"
    "src/nodes/L.cpp"
    "src/nodes/N.cpp"
    "src/nodes/S.cpp"
    "src/nodes/T.cpp"
    "src/nodes/G.cpp"
    )

add_executable(parser ${sources})
