cmake_minimum_required(VERSION 3.30)

project(hello_world VERSION 0.1
        DESCRIPTION "Hello world program and tests")
enable_language(C Fortran)
# Currently setting the Fortran compiler to use -std=gnu, change this if you
# want a specific standard
# TODO enable strict warning flags
set(CMAKE_Fortran_FLAGS "-O0 -g -stand f2018 -warn all,errors")
set(CMAKE_Fortran_COMPILER "ifx")
set(CMAKE_INSTALL_PREFIX "./")
# Source code
add_subdirectory(src)
add_subdirectory(test)
