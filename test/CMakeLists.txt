# Disable deprecation warnings for test-drive package
set(CMAKE_WARN_DEPRECATED OFF CACHE BOOL "" FORCE)

include(cmake/find-test-drive)

# Disable testing by default
set(TEST_DRIVE_BUILD_TESTING OFF CACHE BOOL "Enable testing for the test-drive project")

if (NOT TARGET "test-drive::test-drive")
    find_package("test-drive" REQUIRED)
endif ()

# Unit testing
set(
        tests
        "hello_world_module"
)
set(
        test-srcs
        "main.f90"
)
foreach (t IN LISTS tests)
    string(MAKE_C_IDENTIFIER ${t} t)
    list(APPEND test-srcs "test_${t}.f90")
endforeach ()

add_executable(
        "hello-world-tester"
        "${test-srcs}"
)
target_link_libraries(
        "hello-world-tester"
        PRIVATE
        "hello_world-lib"
        "test-drive::test-drive"
)

target_include_directories("hello-world-tester" INTERFACE ${CMAKE_CURRENT_BINARY_DIR})
