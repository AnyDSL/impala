add_executable(intrinsicgen main.cpp)
target_link_libraries(intrinsicgen PRIVATE ${Thorin_LIBRARIES} libimpala)
llvm_config(intrinsicgen USE_SHARED all)

