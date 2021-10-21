add_executable(intrinsicgen main.cpp)
target_link_libraries(intrinsicgen PRIVATE ${Thorin_LIBRARIES} libimpala)
target_include_directories(intrinsicgen PRIVATE ${Thorin_INCLUDE_DIRS} ${Impala_ROOT_DIR}/src)
llvm_config(intrinsicgen ${AnyDSL_LLVM_LINK_SHARED} core support)
