cmake_minimum_required(VERSION 3.10.0)
project(bridge_c_sparsesuite VERSION 0.1.0 LANGUAGES C Fortran)
#include(CMakePrintHelpers)

set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -Wextra -Wno-unused-but-set-variable -Wno-unused-function")
set (CMAKE_C_FLAGS "${CMAKE_Fortran_FLAGS} -Wall -Wextra -Wno-unused-but-set-variable -Wno-unused-function -std=c11")

#set (BUILD_STATIC_LIBS OFF CACHE BOOL "disable static")
#set (BUILD_SHARED_LIBS ON CACHE BOOL "enable shared")
set (GRAPHBLAS_USE_JIT OFF)
add_subdirectory(GraphBLAS)
add_subdirectory(src_c)
add_subdirectory(src_fortran)

add_executable(main.exe bridge_fortran_c.F90)
include_directories("${CMAKE_CURRENT_SOURCE_DIR}/src_fortran/include")
target_link_libraries(main.exe GraphBLAS)
target_link_libraries(main.exe mylib_c)
target_link_libraries(main.exe mylib_f)

get_property(moduleDir GLOBAL PROPERTY mylib_f_module_dir)
#cmake_print_variables(moduleDir)
target_include_directories(main.exe PUBLIC ${moduleDir})

install(TARGETS main.exe     DESTINATION "bin")