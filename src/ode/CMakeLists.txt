add_library(ode-lib STATIC
        ode_solvers.f90
        some_ode.f90
)

# Allows main project to find .mod files for used modules
target_include_directories(ode-lib INTERFACE ${CMAKE_CURRENT_BINARY_DIR})
