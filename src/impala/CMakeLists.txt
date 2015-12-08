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
    sema/borrowsema.cpp
    sema/infersema.cpp
    sema/namesema.cpp
    sema/typesema.cpp
    sema/typetable.cpp
    sema/typetable.h
    sema/unifiable.cpp
    sema/unifiable.h
    stream.cpp
    symbol.cpp
    symbol.h
    token.cpp
    token.h
    tokenlist.h
)


IF ( BUILD_STATIC )
    MESSAGE ( "Building impala as static library ..." )
    ADD_LIBRARY ( ${LIBRARY_NAME} STATIC ${SOURCES} )
ELSE ()
    MESSAGE ( "Building impala as shared library ..." )
    ADD_LIBRARY ( ${LIBRARY_NAME} SHARED ${SOURCES} )
ENDIF ()
TARGET_LINK_LIBRARIES ( ${LIBRARY_NAME} ${THORIN_LIBRARIES} )

ADD_EXECUTABLE( ${EXECUTABLE_NAME} main.cpp )
TARGET_LINK_LIBRARIES ( ${EXECUTABLE_NAME} ${THORIN_LIBRARIES} lib_impala )
ADD_DEPENDENCIES ( ${EXECUTABLE_NAME} lib_impala )

SET_TARGET_PROPERTIES(exe_impala PROPERTIES OUTPUT_NAME impala)
SET_TARGET_PROPERTIES(lib_impala PROPERTIES OUTPUT_NAME impala)
