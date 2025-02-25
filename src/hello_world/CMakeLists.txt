set(library_name hello_world-lib)

add_library(${library_name} STATIC
    hello.f90
    world.f90
)

# Allows main project to find .mod files for used modules
target_include_directories(${library_name} INTERFACE ${CMAKE_CURRENT_BINARY_DIR})

target_link_libraries(${library_name} PRIVATE
    all_compiler_warnings
    compiler_warnings_as_errors
)
