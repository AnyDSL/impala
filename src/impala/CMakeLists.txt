SET ( EXECUTABLE_NAME exe_impala )

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

ADD_LIBRARY ( libimpala ${SOURCES} )
TARGET_LINK_LIBRARIES ( libimpala ${THORIN_LIBRARIES} )
SET_TARGET_PROPERTIES( libimpala PROPERTIES PREFIX "")

ADD_EXECUTABLE( ${EXECUTABLE_NAME} main.cpp )

SET_TARGET_PROPERTIES( ${EXECUTABLE_NAME} PROPERTIES OUTPUT_NAME ${IMPALA_BINARY} )
TARGET_LINK_LIBRARIES ( ${EXECUTABLE_NAME} ${THORIN_LIBRARIES} libimpala )
