cmake_minimum_required(VERSION 3.1 FATAL_ERROR)

find_package(PythonInterp 3.0 REQUIRED)
set(PYTHON_BIN ${PYTHON_EXECUTABLE})

find_package(LLVM QUIET)
find_program(CLANG_BIN clang clang++ PATHS ${LLVM_TOOLS_BINARY_DIR} PATH_SUFFIXES Release Debug)

add_library(rtmock STATIC rtmock.cpp)

set(TEST_SCRIPT perform.py)
set(TEST_ARGS --impala $<TARGET_FILE:impala> --clang ${CLANG_BIN} --temp ${CMAKE_CURRENT_BINARY_DIR} --rtmock $<TARGET_FILE:rtmock>)

file(GLOB_RECURSE _testcases RELATIVE ${CMAKE_CURRENT_SOURCE_DIR} "*.impala")

foreach(_test ${_testcases})
    add_test(NAME ${_test} COMMAND ${PYTHON_BIN} ${TEST_SCRIPT} ${TEST_ARGS} ${_test} WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endforeach()

set(_content
    "CONFIGURATION = \"$<CONFIG>\"\nIMPALA_BIN = \"$<TARGET_FILE:impala>\"\nCLANG_BIN = \"${CLANG_BIN}\"\nLIBRTMOCK = \"$<TARGET_FILE:rtmock>\"\nTEMP_DIR = \"${CMAKE_CURRENT_BINARY_DIR}\"\n")
file(GENERATE OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/config$<CONFIG>.py CONTENT ${_content})

