cmake_minimum_required(VERSION 3.15)
project(kotlin)

set(CMAKE_CXX_STANDARD 20)

include_directories(include)
set(sources)

LIST(APPEND sources
    src/user_nodes.cpp
    src/user_tokens.cpp
    src/main.cpp
    )

add_executable(kotlin ${sources})
