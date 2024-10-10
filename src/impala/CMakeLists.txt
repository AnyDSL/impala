set(IMPALA_SOURCES
    ast.cpp
    ast.h
    ast_stream.cpp
    cgen.cpp
    cgen.h
    emit.cpp
    impala.cpp
    impala.h
    lexer.cpp
    lexer.h
    parser.cpp
    stream.cpp
    stream.h
    symbol.cpp
    symbol.h
    token.cpp
    token.h
    tokenlist.h
    sema/infersema.cpp
    sema/namesema.cpp
    sema/type.cpp
    sema/type.h
    sema/type_table.h
    sema/typesema.cpp
)

set(IMPALA_SOURCES
    ast.cpp
    ast.h
    ast_stream.cpp
    cgen.cpp
    cgen.h
    emit.cpp
    impala.cpp
    impala.h
    lexer.cpp
    lexer.h
    parser.cpp
    stream.cpp
    stream.h
    symbol.cpp
    symbol.h
    token.cpp
    token.h
    tokenlist.h
    sema/infersema.cpp
    sema/namesema.cpp
    sema/type.cpp
    sema/type.h
    sema/type_table.h
    sema/typesema.cpp
)

add_library(libimpala ${IMPALA_SOURCES})
set_target_properties(libimpala PROPERTIES PREFIX "")
target_link_libraries(libimpala ${MIM_TARGET_NAMESPACE}libmim)
target_include_directories(libimpala PUBLIC
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/..>
    $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/include>
    $<INSTALL_INTERFACE:include>
)
add_dependencies(libimpala ${MIM_TARGET_NAMESPACE}mim_core ${MIM_TARGET_NAMESPACE}mim_math ${MIM_TARGET_NAMESPACE}mim_mem)

add_executable(impala main.cpp)
target_link_libraries(impala ${MIM_TARGET_NAMESPACE}libmim libimpala)
set_target_properties(impala PROPERTIES INSTALL_RPATH "${CMAKE_INSTALL_PREFIX}/lib")
install(TARGETS impala EXPORT impala-targets)
