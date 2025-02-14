set(library_name ode-lib)
add_library(ode-lib STATIC
        ode_solvers.f90
        some_ode.f90
)

target_link_libraries(ode-lib PRIVATE 
        common-lib
        all_compiler_warnings
        compiler_warnings_as_errors
)

# Allows main project to find .mod files for used modules
target_include_directories(ode-lib INTERFACE ${CMAKE_CURRENT_BINARY_DIR})
