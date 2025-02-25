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

enable_testing()

# include(FetchContent)
# FetchContent_Declare(
#     FAssertProject
#     GIT_REPOSITORY https://github.com/degawa/fassert
#     GIT_TAG        master
#     SOURCE_SUBDIR ${CMAKE_CURRENT_SOURCE_DIR}/ctest/cmake/fassert
# )
# FetchContent_MakeAvailable(FAssertProject)

# This will download fassert, and we patch the project by copying a custom CMakeLists.txt file to its source directory
# CMake will consider this a completely separate project, and build it only once
include(ExternalProject)
set(patch_command ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/ctest/cmake/fassert/CMakeLists.txt <SOURCE_DIR>/CMakeLists.txt)
ExternalProject_Add(fassert-lib-external-project
  GIT_REPOSITORY    https://github.com/degawa/fassert
  GIT_TAG           master
  CMAKE_GENERATOR "Visual Studio 17 2022"
  CMAKE_GENERATOR_TOOLSET "fortran=ifort"
  PATCH_COMMAND ${patch_command}
  CMAKE_ARGS "-DCMAKE_INSTALL_PREFIX=<INSTALL_DIR>"
  INSTALL_DIR ${CMAKE_BINARY_DIR}/external
  BINARY_DIR ${CMAKE_CURRENT_BINARY_DIR}/fassert-lib-bin
  INSTALL_BYPRODUCTS ${CMAKE_BINARY_DIR}/external/lib/fassert-lib.lib
)

# Now we can use fassert as a library in our CTest project
enable_testing()
include(CTest)

set(TEST_LIST
    my_test_success
    test_hello_anyone
    my_test_fail
)

create_test_sourcelist(_ my_tests_main.c ${TEST_LIST})

foreach(test ${TEST_LIST})
    add_test(NAME ${test} COMMAND my_tests ${test})
endforeach()

add_library(my_tests_fortran ctest/my_tests.f90)

add_dependencies(my_tests_fortran fassert-lib-external-project)
# find_library(FASSERT_LIB NAMES fassert-lib PATHS ${CMAKE_BINARY_DIR}/external/lib)

include_directories(my_tests_fortran ${CMAKE_CURRENT_BINARY_DIR}/fassert-lib-bin)
link_directories(${CMAKE_BINARY_DIR}/external/lib)

target_link_libraries(my_tests_fortran PRIVATE hello_world-lib fassert-lib)

add_executable(my_tests my_tests_main.c)
target_link_libraries(my_tests PRIVATE my_tests_fortran)

find_package(Python COMPONENTS Interpreter REQUIRED)
if(NOT Python_FOUND)
    message(SEND_ERROR "Please install Python")
endif()
