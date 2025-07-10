add_executable(queue_test QueueTest.f90)

# target_include_directories(queue_test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../build/debug/FGL/ ${CMAKE_CURRENT_SOURCE_DIR}/../FGL/)
target_link_libraries(queue_test PRIVATE FGL)
set_target_properties(queue_test PROPERTIES LINKER_LANGUAGE Fortran)
add_dependencies(queue_test FGL)
