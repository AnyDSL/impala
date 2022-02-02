if(MSVC)
    add_definitions(-D_SCL_SECURE_NO_WARNINGS)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS)
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /wd4800 /wd4520")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /wd4800 /wd4520")
else()
    add_definitions("-Wall -Wextra")
endif()

# HACK
include_directories(
    ${CMAKE_CURRENT_SOURCE_DIR} 
    ${PROJECT_SOURCE_DIR}/../thorin/src/ 
    ${PROJECT_SOURCE_DIR}/../thorin/build/include/
    ${PROJECT_SOURCE_DIR}/../thorin/half/include/)

add_subdirectory(impala)
