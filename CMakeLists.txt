cmake_minimum_required(VERSION 3.30)

project(hello_world VERSION 0.1
        DESCRIPTION "Hello world")
enable_language(Fortran)

# Currently setting the Fortran compiler to use -std=gnu, change this if you
# want a specific standard
set(CMAKE_Fortran_FLAGS "-g -stand f2018 -warn all,errors")
set(CMAKE_Fortran_COMPILER "ifx")
set(CMAKE_INSTALL_PREFIX "./")

# Source code
set(MY_MODULES
        src/hello.f90
        src/world.f90)
add_library(hello_world_lib "${MY_MODULES}")
add_executable(hello_world src/hello_world.f90)
target_link_libraries(hello_world PRIVATE hello_world_lib)

enable_testing()
set(TEST_LIST
        test_success
        test_fail
)
#execute_process(COMMAND "grep --no-filename -o -r ${CMAKE_SOURCE_DIR}}/test -e 'end function test_.*' | sed 's/end function //'" OUTPUT_VARIABLE TEST_LIST)

# The test runner is written in C and has its own main, we should not let the compiler add another "Fortran main" function
set(CMAKE_Fortran_FLAGS "-nofor-main")

create_test_sourcelist(_ my_tests_main.c ${TEST_LIST})
add_executable(my_tests my_tests_main.c test/my_tests.f90)
target_link_libraries(my_tests hello_world_lib)

foreach (test ${TEST_LIST})
    add_test(NAME ${test} COMMAND my_tests ${test})
endforeach ()
