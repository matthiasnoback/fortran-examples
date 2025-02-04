add_library(hello_world-lib STATIC
        hello.f90
        world.f90
)

# Allows main project to find .mod files for used modules
target_include_directories(hello_world-lib INTERFACE ${CMAKE_CURRENT_BINARY_DIR})
