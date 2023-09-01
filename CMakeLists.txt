#
# CMake project file for Radioss Starter
# --------------------------------------
 
cmake_minimum_required (VERSION 2.8)
project (OpenRadioss)

set (actual_directory ${CMAKE_CURRENT_SOURCE_DIR})


message (STATUS "source directory : ${actual_directory} ")
message (STATUS "VS_BUILD : ${VS_BUILD} ")

if (NOT DEFINED build)
   set (build "both")
endif ()

if ( ${build} STREQUAL "starter" OR ${build} STREQUAL "both" )
message (STATUS "Building Starter ")
set (EXEC_NAME ${starter})
add_subdirectory(${actual_directory}/starter)
message (STATUS "starter name : ${EXEC_NAME} ")
endif()

if ( ${build} STREQUAL "engine" OR ${build} STREQUAL "both" )
message (STATUS "Building Engine ")
set (EXEC_NAME ${engine})
add_subdirectory(${actual_directory}/engine)
message (STATUS "engine name : ${EXEC_NAME} ")
endif()

