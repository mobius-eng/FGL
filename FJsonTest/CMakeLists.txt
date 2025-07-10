add_executable(fjson_test fjsontest.f90)

# target_include_directories(fjson_test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../build/debug/FGL/)
target_link_libraries(fjson_test PRIVATE FGL cjson)
set_target_properties(fjson_test PROPERTIES LINKER_LANGUAGE Fortran)
add_dependencies(fjson_test FGL)
