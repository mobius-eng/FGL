add_executable(string_test StringTest.f90)

# target_include_directories(string_test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../build/debug/FGL/ ${CMAKE_CURRENT_SOURCE_DIR}/../FGL/)
target_link_libraries(string_test PRIVATE FGL)
set_target_properties(string_test PROPERTIES LINKER_LANGUAGE Fortran)
add_dependencies(string_test FGL)
# Ninja will preprocess macros before honoring dependencies
# Hence, need to specify directory for include files
target_include_directories(string_test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../FGL/)
