add_executable(intrinsicgen main.cpp)
target_link_libraries(intrinsicgen PRIVATE ${Thorin_LIBRARIES} libimpala)
llvm_config(intrinsicgen ${AnyDSL_LLVM_LINK_SHARED} core passes support)
