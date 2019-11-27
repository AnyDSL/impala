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
    stream.cpp
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
    if(AnyDSL_LLVM_LINK_SHARED)
        set(_specify_use_shared "USE_SHARED")
    else()
        unset(_specify_use_shared)
    endif()
    llvm_config(impala ${_specify_use_shared} ${Impala_LLVM_COMPONENTS})
endif()
if(MSVC)
    set_target_properties(impala PROPERTIES LINK_FLAGS /STACK:8388608)
endif(MSVC)
