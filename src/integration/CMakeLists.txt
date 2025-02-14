set(library_name integration-lib)

add_library(${library_name} STATIC
        functions.f90
        integration_library.f90
)
target_link_libraries(${library_name} PRIVATE
        common-lib 
        all_compiler_warnings
        compiler_warnings_as_errors
)

# Allows main project to find .mod files for used modules
target_include_directories(${library_name} INTERFACE ${CMAKE_CURRENT_BINARY_DIR})
