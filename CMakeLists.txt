cmake_minimum_required(VERSION 3.30)

project(hello_world VERSION 0.1
    LANGUAGES C Fortran
    DESCRIPTION "Hello world")

add_library(all_compiler_warnings INTERFACE)
set(intel_fortran_windows_all_warning_flags /fpp /stand:f18 "SHELL:/warn:all$<COMMA>errors$<COMMA>stderrors" /check:all)
set(intel_fortran_linux_all_warning_flags -fpp -stand f18 -check all "SHELL:-warn all$<COMMA>errors$<COMMA>stderrors")
set(gcc_all_warning_flags -Wall -pedantic)
target_compile_options(all_compiler_warnings INTERFACE
    "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<Fortran_COMPILER_ID:Intel,IntelLLVM>>:$<IF:$<BOOL:${WIN32}>,${intel_fortran_windows_all_warning_flags},${intel_fortran_linux_all_warning_flags}>>"
    "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<Fortran_COMPILER_ID:GNU>>:${gcc_all_warning_flags}>"
)

add_library(compiler_warnings_as_errors INTERFACE)
set(intel_windows_warning_error_flag /warn:errors)
set(intel_linux_warning_error_flag "SHELL:-warn errors")
set(gcc_warning_error_flag -Werror)
target_compile_options(compiler_warnings_as_errors INTERFACE
    "$<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<Fortran_COMPILER_ID:GNU>>:${gcc_warning_error_flag}>"
)


add_library("benchmark" STATIC "src/benchmark/diagnostics.f90" "src/benchmark/facade.f90" "src/common/to_string.f90")
target_include_directories("benchmark" INTERFACE ${CMAKE_CURRENT_BINARY_DIR})

add_subdirectory(src/common)

add_subdirectory(src/hello_world)
add_executable(hello_world src/hello_world.f90)
target_link_libraries(hello_world PRIVATE
    hello_world-lib
    common-lib
    all_compiler_warnings
    compiler_warnings_as_errors
)

add_subdirectory(src/ode)
add_executable(solve_some_ode src/solve_some_ode.f90)
target_link_libraries(solve_some_ode PRIVATE
    ode-lib
    common-lib
    all_compiler_warnings
    compiler_warnings_as_errors
)

add_subdirectory(src/integration)
add_executable(integration src/integration.f90)
target_link_libraries(integration PRIVATE
    integration-lib
    common-lib
    all_compiler_warnings
    compiler_warnings_as_errors
)

add_subdirectory(src/tsunami)
add_executable(tsunami src/tsunami.f90)
target_link_libraries(tsunami PRIVATE
    tsunami-lib
    all_compiler_warnings
    compiler_warnings_as_errors
)

add_subdirectory(src/error_handling)
add_executable(error_handling src/error_handling.f90)
target_link_libraries(error_handling PRIVATE
    error-handling-lib
    all_compiler_warnings
    compiler_warnings_as_errors
)

add_subdirectory(src/logging)
add_executable(logging src/logging.f90)
target_link_libraries(logging PRIVATE
    logging-lib
    all_compiler_warnings
    compiler_warnings_as_errors
)

add_subdirectory(src/stopwatch)
add_executable(performance src/performance.f90)
target_link_libraries(performance PRIVATE
    stopwatch-lib
    all_compiler_warnings
    compiler_warnings_as_errors
)

add_subdirectory(src/examples)

include(cmake/python_dependencies.cmake)

# # This will download fassert, and we patch the project by copying a custom CMakeLists.txt file to its source directory
# # CMake will consider this a completely separate project, and build it only once
# include(ExternalProject)
# set(FASSERT_MOD_DIR ${CMAKE_CURRENT_BINARY_DIR}/fassert-lib-bin)
# set(patch_command ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/ctest/cmake/fassert/CMakeLists.txt <SOURCE_DIR>/CMakeLists.txt)
# ExternalProject_Add(fassert-lib-external-project
#   GIT_REPOSITORY    https://github.com/degawa/fassert
#   GIT_TAG           master
#   CMAKE_GENERATOR "Visual Studio 17 2022"
#   CMAKE_GENERATOR_TOOLSET "fortran=ifort"
#   PATCH_COMMAND ${patch_command}
#   CMAKE_ARGS "-DCMAKE_INSTALL_PREFIX=<INSTALL_DIR>"
#   INSTALL_DIR ${CMAKE_BINARY_DIR}/external
#   BINARY_DIR ${FASSERT_MOD_DIR}
#   UPDATE_DISCONNECTED TRUE
# )
# include_directories(${FASSERT_MOD_DIR})
# link_directories(${CMAKE_BINARY_DIR}/external/lib)

# enable_testing()
# include(CTest)

# include(ctest/define-tests)

# set(TEST_FILES)
# set(TEST_FUNCTIONS)

# function(append_unique list_var item)
#     list(FIND ${list_var} ${item} index)
#     if(index EQUAL -1)
#         list(APPEND ${list_var} ${item})
#         set(${list_var} "${${list_var}}" PARENT_SCOPE)
#     endif()
# endfunction()

# foreach(FILE_AND_TEST IN LISTS FILES_AND_TESTS)
#     string(FIND "${FILE_AND_TEST}" "::" pos)
#     if(pos GREATER -1)
#         string(SUBSTRING "${FILE_AND_TEST}" 0 ${pos} FILE)
#         math(EXPR pos "${pos} + 2")
#         string(SUBSTRING "${FILE_AND_TEST}" ${pos} -1 TEST)
#         append_unique(TEST_FILES "ctest/${FILE}")
#         list(APPEND TEST_FUNCTIONS "${TEST}")
#         # message(INFO "File: ${FILE}")
#         # message(INFO "Test: ${TEST}")
#         add_test(NAME ${FILE_AND_TEST} COMMAND my_tests ${TEST})
#     endif()
# endforeach()

# create_test_sourcelist(_ my_tests_main.c ${TEST_FUNCTIONS})

# add_library(my_tests_fortran ${TEST_FILES})
# # add_dependencies(my_tests_fortran fassert-lib-external-project)
# target_link_libraries(my_tests_fortran PRIVATE hello_world-lib)

# add_executable(my_tests my_tests_main.c)
# target_link_libraries(my_tests PRIVATE my_tests_fortran)

# add_subdirectory(test)

# Disable deprecation warnings for test-drive package
set(CMAKE_WARN_DEPRECATED OFF CACHE BOOL "" FORCE)

# Disable testing by default
set(TEST_DRIVE_BUILD_TESTING OFF CACHE BOOL "Enable testing for the test-drive project")

include(test/cmake/find-test-drive)

if (NOT TARGET "test-drive::test-drive")
    find_package("test-drive" REQUIRED)
endif ()

# Unit testing
set(
        tests
        "hello_world_module"
)
set(
        test-srcs
        "test/main.f90"
)
foreach (t IN LISTS tests)
    string(MAKE_C_IDENTIFIER ${t} t)
    list(APPEND test-srcs "test/test_${t}.f90")
endforeach ()

add_executable(
        "hello-world-tester"
        "${test-srcs}"
)
target_link_libraries(
        "hello-world-tester"
        PRIVATE
        "hello_world-lib"
        "test-drive::test-drive"
)

target_include_directories("hello-world-tester" INTERFACE ${CMAKE_CURRENT_BINARY_DIR})

enable_testing()
include(CTest)
foreach (t IN LISTS tests)
    add_test("${t}" "hello-world-tester" "${t}")
endforeach ()
