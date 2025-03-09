set(library_name stopwatch-lib)

add_library(${library_name} STATIC
    m_stopwatch.f90
    m_stopwatch_facade.f90
    m_started_timer_map.f90
    m_ended_timer_list.f90
)

# Allows main project to find .mod files for used modules
target_include_directories(${library_name} INTERFACE ${CMAKE_CURRENT_BINARY_DIR})

target_link_libraries(${library_name} PRIVATE
    common-lib
    all_compiler_warnings
    compiler_warnings_as_errors
)

add_custom_command(
    TARGET ${library_name}
    COMMENT "Run Fortitude"
    POST_BUILD
    WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    COMMAND ./venv/Scripts/fortitude check --fix ${CMAKE_CURRENT_SOURCE_DIR}
)
