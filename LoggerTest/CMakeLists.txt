add_executable(logger_test LoggerTest.f90)

# target_include_directories(logger_test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../build/debug/FGL/ ${CMAKE_CURRENT_SOURCE_DIR}/../FGL/)
target_link_libraries(logger_test PRIVATE FGL)
set_target_properties(logger_test PROPERTIES LINKER_LANGUAGE Fortran)
add_dependencies(logger_test FGL)
# Ninja will preprocess macros before honoring dependencies
# Hence, need to specify directory for include files
target_include_directories(logger_test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../FGL/)
