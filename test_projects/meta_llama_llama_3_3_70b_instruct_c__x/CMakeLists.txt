
cmake_minimum_required(VERSION 3.10)
project(complang)

set(CMAKE_CXX_STANDARD 14)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(PQXX REQUIRED)

add_executable(main main.cpp)

target_link_libraries(main pqxx pq)
