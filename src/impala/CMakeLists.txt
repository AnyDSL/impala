SET ( EXECUTABLE_NAME exe_impala )
SET ( LIBRARY_NAME lib_impala )

# get source files

SET ( SOURCES
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
    prec.cpp
    prec.h
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

ADD_LIBRARY ( ${LIBRARY_NAME} ${SOURCES} )
TARGET_LINK_LIBRARIES ( ${LIBRARY_NAME} ${THORIN_LIBRARIES} )

ADD_EXECUTABLE( ${EXECUTABLE_NAME} main.cpp )
TARGET_LINK_LIBRARIES ( ${EXECUTABLE_NAME} ${THORIN_LIBRARIES} ${LIBRARY_NAME} )
SET_TARGET_PROPERTIES ( ${EXECUTABLE_NAME} PROPERTIES CXX_STANDARD 14 CXX_STANDARD_REQUIRED ON )
ADD_DEPENDENCIES ( ${EXECUTABLE_NAME} ${LIBRARY_NAME} )

SET_TARGET_PROPERTIES( ${EXECUTABLE_NAME} PROPERTIES OUTPUT_NAME impala)
SET_TARGET_PROPERTIES( ${LIBRARY_NAME} PROPERTIES OUTPUT_NAME impala)
SET_TARGET_PROPERTIES( ${LIBRARY_NAME} PROPERTIES CXX_STANDARD 14 CXX_STANDARD_REQUIRED ON)
