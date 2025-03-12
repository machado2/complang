
cmake_minimum_required(VERSION 3.14)
project(crud_api)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Include FetchContent for dependency management
include(FetchContent)

# Fetch and configure Crow
FetchContent_Declare(
    crow
    GIT_REPOSITORY https://github.com/crowcpp/crow.git
    GIT_TAG v1.0+5
)
FetchContent_MakeAvailable(crow)

# Fetch and configure nlohmann_json
FetchContent_Declare(
    nlohmann_json
    GIT_REPOSITORY https://github.com/nlohmann/json.git
    GIT_TAG v3.11.2
)
FetchContent_MakeAvailable(nlohmann_json)

# Fetch and configure libpqxx
FetchContent_Declare(
    libpqxx
    GIT_REPOSITORY https://github.com/jtv/libpqxx.git
    GIT_TAG 7.7.4
)
# Configure libpqxx build options
set(BUILD_TEST OFF CACHE BOOL "")
FetchContent_MakeAvailable(libpqxx)

# Create the executable
add_executable(crud_api main.cpp)

# Link libraries
target_link_libraries(crud_api PRIVATE crow::crow nlohmann_json::nlohmann_json pqxx)
