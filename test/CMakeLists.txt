include(cmake/find-test-drive)

# Disable testing by default
set(TEST_DRIVE_BUILD_TESTING OFF CACHE BOOL "Enable testing for the test-drive project")

if (NOT TARGET "test-drive::test-drive")
    find_package("test-drive" REQUIRED)
endif ()

# Unit testing
set(
        tests
        "suite1"
        "tsunami"
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
        "${PROJECT_NAME}-tester"
        "${test-srcs}"
)
target_link_libraries(
        "${PROJECT_NAME}-tester"
        PRIVATE
        "${PROJECT_NAME}-lib"
        "ode-lib"
        "tsunami-lib"
        "test-drive::test-drive"
)

target_include_directories("${PROJECT_NAME}-tester" INTERFACE ${CMAKE_CURRENT_BINARY_DIR})

foreach (t IN LISTS tests)
    add_test("${PROJECT_NAME}/${t}" "${PROJECT_NAME}-tester" "${t}")
endforeach ()
