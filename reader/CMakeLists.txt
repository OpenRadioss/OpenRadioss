# 
# CMake Project for open_reader
#

cmake_minimum_required (VERSION 3.6)
project(open_reader)

set (open_reader_root_dir ${CMAKE_CURRENT_SOURCE_DIR})

include ( CMake_arch/CMake_${arch}.cmake )

# Print configuration
message( " ")
message( "OpenReader build")
message( "----------------")
message( " ")
message(STATUS "arch: ${arch}")
message(STATUS "Project root directory: ${open_reader_root_dir}")
message( " ")
message( "Configuration:")
message(STATUS "C++ Compiler: ${CMAKE_CXX_COMPILER}")
message(STATUS "C Compiler: ${CMAKE_C_COMPILER}")
message(STATUS "Debug: ${CMAKE_BUILD_TYPE}")
message( " ")

# Global includes directory list
# -------------------------------
set (include_directory "${CMAKE_CURRENT_SOURCE_DIR}/source" )
file(GLOB_RECURSE include_files ${include_directory}/*.h)

set (include_dir_list "")
foreach (incfile ${include_files})
    get_filename_component(dir ${incfile} PATH)
    list (APPEND include_dir_list ${dir})
endforeach()
list(REMOVE_DUPLICATES include_dir_list)


# Build submodules
#----------------

# Dyna2Rad module
add_subdirectory(./source/dyna2rad dyna2rad_${arch})

# cfgkernel module
add_subdirectory(./source/cfgkernel cfgkernel_${arch})

# io module
add_subdirectory(./source/io io_${arch})

# sdi module
add_subdirectory(./source/sdi sdi_${arch})

# cfgio module
add_subdirectory(./source/cfgio cfgio_${arch})


# Build solver_interface 
# link the open_reader library
#-----------------------------
# Solver interface module + Linking
include(./source/solver_interface/CMakeLists.txt)
