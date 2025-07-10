add_executable(sort_test SortTest.f90)

# target_include_directories(sort_test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../build/debug/FGL/ ${CMAKE_CURRENT_SOURCE_DIR}/../FGL/)
target_link_libraries(sort_test PRIVATE FGL)
set_target_properties(sort_test PROPERTIES LINKER_LANGUAGE Fortran)
add_dependencies(sort_test FGL)
