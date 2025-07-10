add_executable(bitstring_test BitStringDemo.f90)

# target_include_directories(bitstring_test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../build/debug/FGL/)
# target_include_directories(bitstring_test PRIVATE ${CMAKE_BINARY_DIR}/mod)
target_link_libraries(bitstring_test PRIVATE FGL)
set_target_properties(bitstring_test PROPERTIES LINKER_LANGUAGE Fortran)
add_dependencies(bitstring_test FGL)