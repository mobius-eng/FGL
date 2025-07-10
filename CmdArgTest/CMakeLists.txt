add_executable(cmdarg_test CmdArgTest.f90)

add_dependencies(cmdarg_test FGL)
# Ninja will preprocess macros before honoring dependencies
# Hence, need to specify directory for include files
target_include_directories(cmdarg_test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../FGL/)
target_link_libraries(cmdarg_test PRIVATE FGL)
set_target_properties(cmdarg_test PROPERTIES LINKER_LANGUAGE Fortran)
