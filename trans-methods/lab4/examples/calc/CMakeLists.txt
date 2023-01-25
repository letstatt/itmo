cmake_minimum_required(VERSION 3.15)
project(calc)

set(CMAKE_CXX_STANDARD 20)

include_directories(include)
set(sources)

LIST(APPEND sources
    src/main.cpp
    src/user_nodes.cpp
    src/user_tokens.cpp
    )

add_executable(calc ${sources})
