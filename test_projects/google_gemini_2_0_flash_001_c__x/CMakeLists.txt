
cmake_minimum_required(VERSION 3.0)
project(user_api)

set(CMAKE_CXX_STANDARD 11)

find_package(Boost REQUIRED COMPONENTS system thread)
find_package(cpprestsdk REQUIRED)
find_package(PostgreSQL REQUIRED)

include_directories(${Boost_INCLUDE_DIRS} ${CPPRESTSDK_INCLUDE_DIRS} ${PostgreSQL_INCLUDE_DIRS} /usr/include/cpprest)

add_executable(user_api main.cpp)
target_link_libraries(user_api ${Boost_LIBRARIES} ${CPPRESTSDK_LIBRARIES} ${PostgreSQL_LIBRARIES})
