add_executable(list_test ListTest.f90)

# target_include_directories(list_test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../build/debug/FGL/)
target_link_libraries(list_test PRIVATE FGL)
set_target_properties(list_test PROPERTIES LINKER_LANGUAGE Fortran)
add_dependencies(list_test FGL)
