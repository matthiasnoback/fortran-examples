add_library(tsunami-lib STATIC
    m_config.f90
    m_diff.f90
    m_field.f90
    m_init.f90
    m_observer.f90
)

target_link_libraries(tsunami-lib PRIVATE
    common-lib
    all_compiler_warnings
    compiler_warnings_as_errors
)

# Allows main project to find .mod files for used modules
target_include_directories(tsunami-lib INTERFACE ${CMAKE_CURRENT_BINARY_DIR})
