set(library_name common-lib)

add_library(${library_name} STATIC
    precision.f90
    m_to_string.f90
)

# Allows main project to find .mod files for used modules
target_include_directories(common-lib INTERFACE ${CMAKE_CURRENT_BINARY_DIR})

target_link_libraries(${library_name} PRIVATE
    all_compiler_warnings
    compiler_warnings_as_errors
)
