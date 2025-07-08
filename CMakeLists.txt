cmake_minimum_required(VERSION 3.28)

project(ftestproj VERSION 0.1
        DESCRIPTION "My Fortran program"
        LANGUAGES Fortran CXX C)

enable_language(Fortran)

set(CMAKE_Fortran_COMPILER "ifx")
set(CMAKE_Fortran_FLAGS_DEBUG_INIT "-g -O0 -cxxlib -shared-intel")
set(CMAKE_Fortran_FLAGS_RELEASE_INIT "-O3")


# add_subdirectory(code)

add_subdirectory(cjson)
add_subdirectory(JsonTest)
add_subdirectory(FGL)
add_subdirectory(FJsonTest)