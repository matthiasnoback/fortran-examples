set(MY_MODULES
        hello.f90
        world.f90
        CACHE INTERNAL "")
add_library(internal_modules "${MY_MODULES}")
add_executable(hello_world hello_world.f90)
target_link_libraries(hello_world PRIVATE internal_modules)
