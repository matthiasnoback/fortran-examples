cmake_minimum_required(VERSION 3.30)

project(hello_world_tests VERSION 0.1
        DESCRIPTION "Hello world tests")
enable_language(C Fortran)

enable_testing()
set(TEST_LIST
        my_test_success
        my_test_fail
)
create_test_sourcelist(_ my_tests_main.c ${TEST_LIST})
add_executable(my_tests my_tests_main.c my_tests.f90)

foreach (test ${TEST_LIST})
    add_test(NAME ${test} COMMAND my_tests ${test})
endforeach ()
