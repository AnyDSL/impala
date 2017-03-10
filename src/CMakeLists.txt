if(MSVC)
    add_definitions(-D_SCL_SECURE_NO_WARNINGS)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS)
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /wd4800 /wd4520")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /wd4800 /wd4520")
else()
    add_definitions("-Wall -Wextra")
endif()

find_package(Thorin REQUIRED)
include_directories(${Thorin_INCLUDE_DIRS})

# check for Half library
find_package(Half REQUIRED)
if(Half_FOUND)
    message(STATUS "Building with Half library from ${Half_INCLUDE_DIRS}.")
    include_directories(${Half_INCLUDE_DIRS})
endif()

if(LLVM_FOUND)
    add_definitions(${LLVM_DEFINITIONS})
    add_definitions("-DLLVM_SUPPORT")
    include_directories(${LLVM_INCLUDE_DIRS})
endif()

include_directories(${CMAKE_CURRENT_SOURCE_DIR})
add_subdirectory(impala)
add_subdirectory(intrinsicgen)
