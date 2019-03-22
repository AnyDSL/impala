cmake_minimum_required(VERSION 3.4.3 FATAL_ERROR)

project(Impala)

set(PACKAGE_VERSION "0.3.9")
set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "limited config" FORCE)
set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS 1)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

option(BUILD_SHARED_LIBS "Build shared libraries" ON)

if(CMAKE_BUILD_TYPE STREQUAL "")
    set(CMAKE_BUILD_TYPE Debug CACHE STRING "Debug or Release" FORCE)
endif()

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

add_subdirectory(src)

message(STATUS "Using Debug flags: ${CMAKE_CXX_FLAGS_DEBUG}")
message(STATUS "Using Release flags: ${CMAKE_CXX_FLAGS_RELEASE}")
if(DEFINED CMAKE_BUILD_TYPE)
    message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")
endif()

export(TARGETS libimpala impala FILE ${CMAKE_BINARY_DIR}/share/anydsl/cmake/impala-exports.cmake)
configure_file(cmake/impala-config.cmake.in ${CMAKE_BINARY_DIR}/share/anydsl/cmake/impala-config.cmake @ONLY)

if (BUILD_TESTING)
    enable_testing()
    add_subdirectory(test)
endif(BUILD_TESTING)
