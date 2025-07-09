add_executable(error_test ErrorHandlingTest.f90)

target_include_directories(error_test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../build/debug/FGL/ ${CMAKE_CURRENT_SOURCE_DIR}/../FGL/)
target_link_libraries(error_test PRIVATE FGL)
set_target_properties(error_test PROPERTIES LINKER_LANGUAGE Fortran)
