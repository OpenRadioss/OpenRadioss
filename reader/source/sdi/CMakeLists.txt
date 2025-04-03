# CMake project file for CMake

cmake_minimum_required (VERSION 3.6)
project (sdi) 
enable_language (CXX)

set (library_name "sdi_${arch}" )

#Include the CMake configuration file
include ( CMake_arch/CMake_${arch}.cmake )


#set source directory
set (source_directory ${CMAKE_CURRENT_SOURCE_DIR})

set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${open_reader_root_dir}/cbuild_${arch}/libopen_reader/${arch}" )
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${open_reader_root_dir}/cbuild_${arch}/libopen_reader/${arch}" )

# Add includes for project
include ( ${open_reader_root_dir}/CMake_arch/CMake_includes.cmake )


#CPP source files
file(GLOB_RECURSE cpp_source_files ${source_directory}/*.cpp  ${source_directory}/*.cxx )


#
# compiler Flags
# --------------
set (CMAKE_Fortran_FLAGS_DEBUG " " )
set (CMAKE_Fortran_FLAGS_RELEASE " " )
set (CMAKE_Fortran_FLAGS " " )

set (CMAKE_C_FLAGS_DEBUG " " )
set (CMAKE_C_FLAGS_RELEASE " " )
set (CMAKE_C_FLAGS " " )

set (CMAKE_CPP_FLAGS_DEBUG " " )
set (CMAKE_CPP_FLAGS_RELEASE " " )
set (CMAKE_CPP_FLAGS " " )

set (CMAKE_CXX_FLAGS_DEBUG " " )
set (CMAKE_CXX_FLAGS_RELEASE " " )
set (CMAKE_CXX_FLAGS " " )


#Windows Porting modify here the Makefiles
set ( cpp_flags "${CXX_FLAGS} ${PRECOM_DEFINITION}" )
set_source_files_properties(${cpp_source_files} PROPERTIES COMPILE_FLAGS "${cpp_flags}")


message ( STATUS "sdi module: ${libp}${library_name}.${lib}")

# Create the module library
add_library(${library_name} STATIC ${cpp_source_files} ${c_source_files})
target_include_directories(${library_name} PRIVATE ${include_dir_list}  )



