set(IMPALA_SOURCES
    args.h
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
    sema/infersema.cpp
    sema/namesema.cpp
    sema/type.cpp
    sema/type.h
    sema/typesema.cpp
    token.cpp
    token.h
    tokenlist.h
)

add_library(libimpala ${IMPALA_SOURCES})
target_link_libraries(libimpala PRIVATE ${Thorin_LIBRARIES})
set_target_properties(libimpala PROPERTIES PREFIX "")

add_executable(impala main.cpp)
target_link_libraries(impala PRIVATE ${Thorin_LIBRARIES} libimpala)
if(Thorin_HAS_LLVM_SUPPORT)
    set(Impala_LLVM_COMPONENTS support)
    llvm_config(impala ${AnyDSL_LLVM_LINK_SHARED} ${Impala_LLVM_COMPONENTS})
endif()
if(MSVC)
    set_target_properties(impala PROPERTIES LINK_FLAGS /STACK:8388608)
endif(MSVC)
