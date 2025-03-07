cmake_minimum_required(VERSION 3.10)
project(crud_api)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find required packages
find_package(PostgreSQL REQUIRED)
find_package(Threads REQUIRED)
find_package(Boost REQUIRED COMPONENTS system filesystem)

# Include directories
include_directories(${PostgreSQL_INCLUDE_DIRS})
include_directories(${Boost_INCLUDE_DIRS})

# Add the executable
add_executable(crud_api main.cpp)

# Link libraries
target_link_libraries(crud_api 
    ${PostgreSQL_LIBRARIES}
    pqxx
    pthread
    ${Boost_LIBRARIES}
)
