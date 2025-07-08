add_executable(fjson_test fjsontest.f90)

target_link_libraries(fjson_test PRIVATE FGL cjson)
target_include_directories(fjson_test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../build/debug/FGL/)