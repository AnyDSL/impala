cmake_minimum_required(VERSION 3.4.3 FATAL_ERROR)

project(Impala)

set(PACKAGE_VERSION "0.3.9")
set(CMAKE_CONFIGURATION_TYPES "Debug;Release" CACHE STRING "limited config" FORCE)
set(CMAKE_WINDOWS_EXPORT_ALL_SYMBOLS 1)
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(CMakePackageConfigHelpers)
include(GNUInstallDirs)

option(BUILD_SHARED_LIBS "Build shared libraries" ON)

if(CMAKE_BUILD_TYPE STREQUAL "")
    set(CMAKE_BUILD_TYPE Debug CACHE STRING "Debug or Release" FORCE)
endif()

set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

if(MSVC)
    add_definitions(-D_SCL_SECURE_NO_WARNINGS)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS)
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} /wd4800 /wd4520")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /wd4800 /wd4520")
else()
    add_definitions("-Wall -Wextra")
endif()

if(NOT DEFINED Mim_DIR OR Mim_DIR STREQUAL "")
    add_subdirectory(MimIR)
else()
    find_package(Mim REQUIRED CONFIG)
endif()

if(NOT MIM_TARGET_NAMESPACE)
    set(MIM_TARGET_NAMESPACE "")
endif()

add_subdirectory(src)


write_basic_package_version_file(
    "${CMAKE_CURRENT_BINARY_DIR}/impala-config-version.cmake"
    VERSION ${PACKAGE_VERSION}
    COMPATIBILITY SameMajorVersion
)
configure_package_config_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/cmake/impala-config.cmake.in"
    "${CMAKE_CURRENT_BINARY_DIR}/impala-config.cmake"
    INSTALL_DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/impala"
)
install(
    TARGETS libimpala
    EXPORT impala-targets
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)
install(
    FILES
        "${CMAKE_CURRENT_BINARY_DIR}/impala-config.cmake"
        "${CMAKE_CURRENT_BINARY_DIR}/impala-config-version.cmake"
    DESTINATION
        ${CMAKE_INSTALL_LIBDIR}/cmake/impala
)
install(
    EXPORT impala-targets
    NAMESPACE impala::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/impala
)
