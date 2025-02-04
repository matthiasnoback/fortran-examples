add_library(integration-lib STATIC
        functions.f90
        integration_library.f90
)

# Allows main project to find .mod files for used modules
target_include_directories(integration-lib INTERFACE ${CMAKE_CURRENT_BINARY_DIR})
