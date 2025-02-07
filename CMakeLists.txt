cmake_minimum_required(VERSION 3.30)

project(hello_world VERSION 0.1
        LANGUAGES Fortran
        DESCRIPTION "Hello world")

set(CMAKE_Fortran_FLAGS "-g2 -stand f2018 -warn all,errors")

add_subdirectory(src/common)

add_subdirectory(src/hello_world)
add_executable(hello_world src/hello_world.f90)
target_link_libraries(hello_world PUBLIC hello_world-lib common-lib)

add_subdirectory(src/ode)
add_executable(solve_some_ode src/solve_some_ode.f90)
target_link_libraries(solve_some_ode PUBLIC ode-lib common-lib)

add_subdirectory(src/integration)
add_executable(integration src/integration.f90)
target_link_libraries(integration PUBLIC integration-lib common-lib)

add_subdirectory(src/tsunami)
add_executable(tsunami src/tsunami.f90)
target_link_libraries(tsunami PUBLIC tsunami-lib)

add_subdirectory(test)
