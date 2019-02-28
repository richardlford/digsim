cmake_minimum_required(VERSION 3.10)
project(task01)

set(cmake_export_compile_commands on)
#set(CMAKE_VERBOSE_MAKEFILE ON)

IF (CMAKE_BUILD_TYPE MATCHES Debug)
    set(CMAKE_C_COMPILER clang)
    message("\nDebug build.")
ELSEIF (CMAKE_BUILD_TYPE MATCHES Release)
    set(CMAKE_C_COMPILER ccomp)
    add_definitions(-fstruct-passing)
    message("\nRelease build.")
ELSE ()
    message("\nSome other build type.")
ENDIF ()
message("Compiler -> ${CMAKE_C_COMPILER}")

include_directories(
        ${MY_SOURCE_DIR}
)

add_executable(
        ${PROJECT_NAME}
        build1.c
        diffeq.c
        onestep.c
        state.h
)

target_link_libraries(${PROJECT_NAME} m)
