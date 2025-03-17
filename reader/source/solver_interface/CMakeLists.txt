# CMake project file for CMake

enable_language (CXX)

set (library_name "open_reader_${arch}" )


# Add includes for project
set ( Build_includes_directory ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/includes_${arch} )
file(MAKE_DIRECTORY ${Build_includes_directory} )


#set source directory
set (source_directory ${CMAKE_CURRENT_SOURCE_DIR}/source/solver_interface)

#Include the CMake configuration file
include ( ${source_directory}/CMake_arch/CMake_${arch}.cmake )


set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${open_reader_root_dir}/../exec" )
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${open_reader_root_dir}/../exec/" )

include ( ${open_reader_root_dir}/CMake_arch/CMake_includes.cmake )
list (APPEND include_dir_list ${Build_includes_directory} )


add_custom_target(    
            chglt_id ALL
            COMMAND python ${open_reader_root_dir}/../Compiling_tools/script/reader_info.py -outfile=${Build_includes_directory}/chglt_id.h
)


#CPP source files
file(GLOB_RECURSE cpp_source_files ${source_directory}/source/*.cpp ${source_directory}/source/*.cxx)



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


message (" " )
message (STATUS "Build Solver Interface")
message (STATUS "C++ Compiler: ${CMAKE_CXX_COMPILER}")
message (STATUS "C Compiler: ${CMAKE_C_COMPILER}")
message (STATUS "Library name: ${library_name} ")
message (STATUS "Source directory: ${source_directory} ")
message (STATUS "Build directory:  ${open_reader_root_dir}/cbuild_win64/libopen_reader/")
message (STATUS "Include directories: ${Build_includes_directory}")
message (STATUS " ${CMAKE_CURRENT_BINARY_DIR}")
message (STATUS " ${open_reader_root_dir}")
message (" " )


#add_library(hm_reader/libs_${arch}/libhm_reader_rd_${arch} STATIC ${cpp_source_files} )
add_library(${library_name} SHARED ${cpp_source_files} )
target_include_directories(${library_name} PRIVATE ${include_dir_list}  )

add_dependencies(${library_name}  chglt_id)

target_link_libraries(${library_name} 
                      dyna2rad_${arch}
                      io_${arch}
                      sdi_${arch}
                      cfgio_${arch}
		              cfgkernel_${arch}
                      ${open_reader_root_dir}/../extlib/boost/boost_1_70_0/static/lib/${arch}/libboost_filesystem.${lib})

add_custom_command(TARGET ${library_name}
                   POST_BUILD
                   COMMAND ${CMAKE_COMMAND} -E make_directory ${open_reader_root_dir}/../exec
                   COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${library_name}> ${open_reader_root_dir}/../exec
                   )

add_custom_command(TARGET ${library_name}
                   PRE_BUILD
                   COMMAND python ${open_reader_root_dir}/../Compiling_tools/script/reader_info.py -outfile=${Build_includes_directory}/chglt_id.h
                   )

