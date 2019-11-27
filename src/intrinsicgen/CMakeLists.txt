add_executable(intrinsicgen main.cpp)
target_link_libraries(intrinsicgen PRIVATE ${Thorin_LIBRARIES} libimpala)

if(AnyDSL_LLVM_LINK_SHARED)
	set(_specify_use_shared "USE_SHARED")
else()
	unset(_specify_use_shared)
endif()
llvm_config(intrinsicgen ${_specify_use_shared} core support)
