add_library(
    FGL
    STATIC
    BitString.f90
    Error.f90
    String.f90
    UStorage.f90
    sort_int.f90
    sort_real_sp.f90
    sort_real_dp.f90
    Sort.f90
    Optional.f90
    Logger.f90
    List.f90
    Queue.f90
    Hash.f90
    HashTable.f90
    FJSON.f90
    UTest.f90
)

target_include_directories(FGL PUBLIC ${CMAKE_BINARY_DIR}/mod)

add_custom_command(
    TARGET FGL PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy
            ${CMAKE_SOURCE_DIR}/FGL/get_cmd_arg_inc.f90
            ${CMAKE_BINARY_DIR}/mod/get_cmd_arg_inc.f90
)

add_custom_command(
    TARGET FGL PRE_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy
            ${CMAKE_SOURCE_DIR}/FGL/logger_inc.f90
            ${CMAKE_BINARY_DIR}/mod/logger_inc.f90
)

add_dependencies(FGL cjson)