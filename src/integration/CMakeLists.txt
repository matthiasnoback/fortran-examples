add_library(integration-lib STATIC
        functions.f90
        integration_library.f90
)
target_link_libraries(integration-lib common-lib)

# Allows main project to find .mod files for used modules
target_include_directories(integration-lib INTERFACE ${CMAKE_CURRENT_BINARY_DIR})
