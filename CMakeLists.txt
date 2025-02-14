cmake_minimum_required(VERSION 3.30)

project(hello_world VERSION 0.1
        LANGUAGES Fortran
        DESCRIPTION "Hello world")
        
add_library(all_compiler_warnings INTERFACE)
set(intel_fortran_windows_all_warning_flags /stand:f18 /warn:all)
set(intel_fortran_linux_all_warning_flags -stand f18 "SHELL:-warn all")
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

add_subdirectory(test)

find_package(Python COMPONENTS Interpreter REQUIRED)
if (NOT Python_FOUND)
    message(SEND_ERROR "Please install Python")
endif()

execute_process(COMMAND ${Python_EXECUTABLE} -m venv ${PROJECT_SOURCE_DIR}/venv)
execute_process(COMMAND ${PROJECT_SOURCE_DIR}/venv/Scripts/python.exe -m pip install -q --upgrade pip)
execute_process(COMMAND ${PROJECT_SOURCE_DIR}/venv/Scripts/python.exe -m pip install -q -r ${PROJECT_SOURCE_DIR}/requirements.txt)
