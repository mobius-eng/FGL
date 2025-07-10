add_executable(hash_test HashTest.f90)

# target_include_directories(hash_test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../build/debug/FGL/)
target_link_libraries(hash_test PRIVATE FGL)
set_target_properties(hash_test PROPERTIES LINKER_LANGUAGE Fortran)
add_dependencies(hash_test FGL)
