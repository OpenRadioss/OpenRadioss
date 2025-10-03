# CMake project file for CMake

cmake_minimum_required (VERSION 3.15)
project (solver_interface)
enable_language (CXX)

set (library_name "solver_interface_${arch}" )

#set source directory
set (source_directory ${CMAKE_CURRENT_SOURCE_DIR})

#Include the CMake configuration file
if (com STREQUAL "0")
   include ( CMake_arch/CMake_${arch}.cmake )
else()
    include ( CMake_arch_c/CMake_${arch}.cmake )
endif()



set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${open_reader_root_dir}/${build_dir}/libopen_reader/${arch}" )
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${open_reader_root_dir}/${build_dir}/libopen_reader/${arch}" )

# Add includes for project
include ( ${open_reader_root_dir}/CMake_arch/CMake_includes.cmake )

# Include for Build_tag
set ( Build_includes_directory ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/includes_${arch} )
file(MAKE_DIRECTORY ${Build_includes_directory} )

#CPP source files
file(GLOB_RECURSE cpp_source_files ${source_directory}/source/*.cpp ${source_directory}/source/*.cxx)


#set source directory
set (source_directory ${CMAKE_CURRENT_SOURCE_DIR})

list (APPEND include_dir_list ${Build_includes_directory} )

add_custom_target(    
            chglt_id ALL
            COMMAND python ${open_reader_root_dir}/../Compiling_tools/script/reader_info.py -outfile=${Build_includes_directory}/chglt_id.h
)


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

set ( cpp_flags "${CXX_FLAGS} ${PRECOM_DEFINITION}" )
set_source_files_properties(${cpp_source_files} PROPERTIES COMPILE_FLAGS "${cpp_flags}")


message ( STATUS "solver_interface module: ${libp}${library_name}.${lib}")

# Create the module library
add_library(${library_name} STATIC ${cpp_source_files} ${c_source_files})
target_include_directories(${library_name} PRIVATE ${include_dir_list}  )
add_dependencies(${library_name}  chglt_id)

add_custom_command(TARGET ${library_name}
                   PRE_BUILD
                   COMMAND python ${open_reader_root_dir}/../Compiling_tools/script/reader_info.py -outfile=${Build_includes_directory}/chglt_id.h
                   )


