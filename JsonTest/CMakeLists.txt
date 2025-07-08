add_executable(json_test json_test.c)

target_link_libraries(json_test PUBLIC cjson)
target_include_directories(json_test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/../cjson/)
