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
target_link_libraries(libimpala PRIVATE ${Thorin_LIBRARIES})
set_target_properties(libimpala PROPERTIES PREFIX "")

add_executable(impala main.cpp)
target_link_libraries(impala ${Thorin_LIBRARIES} libimpala)
if(MSVC)
    set_target_properties(impala PROPERTIES LINK_FLAGS /STACK:8388608)
endif(MSVC)
