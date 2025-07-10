add_executable(ustorage_test UStorageTest.f90)

# target_include_directories(ustorage_test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../build/debug/FGL/)
target_link_libraries(ustorage_test PRIVATE FGL cjson)
set_target_properties(ustorage_test PROPERTIES LINKER_LANGUAGE Fortran)
add_dependencies(ustorage_test FGL)
