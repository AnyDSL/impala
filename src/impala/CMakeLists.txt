# get source files
set(IMPALA_SOURCES
    ast.cpp
    ast.h
    cgen.cpp
    cgen.h
    emit.cpp
    impala.cpp
    impala.h
    lexer.cpp
    lexer.h
    parser.cpp
    sema/infersema.cpp
    sema/namesema.cpp
    sema/type.cpp
    sema/type.h
    sema/typesema.cpp
    sema/typetable.cpp
    sema/typetable.h
    stream.cpp
    symbol.cpp
    symbol.h
    token.cpp
    token.h
    tokenlist.h
)

add_library(libimpala ${IMPALA_SOURCES})
target_link_libraries(libimpala ${Thorin_LIBRARIES})
set_target_properties(libimpala PROPERTIES PREFIX "")

add_executable(impala main.cpp)
target_link_libraries(impala ${Thorin_LIBRARIES} libimpala)
if(MSVC)
    set_target_properties(impala PROPERTIES LINK_FLAGS /STACK:8388608)
endif(MSVC)
