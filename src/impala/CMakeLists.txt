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
    loc.cpp
    loc.h
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
target_link_libraries(libimpala libthorin)
target_include_directories(libimpala PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/..)

add_executable(impala main.cpp)
target_link_libraries(impala libthorin libimpala)
add_dependencies(impala thorin_mem thorin_core thorin_affine)
