# 
# CMake Project for open_reader
#

cmake_minimum_required (VERSION 3.15)
project(open_reader)

# Windows specific settings
# --------------------------
if (arch STREQUAL "win64")
   set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
endif()

set (open_reader_root_dir ${CMAKE_CURRENT_SOURCE_DIR})

include ( CMake_arch/CMake_${arch}.cmake )

if (com STREQUAL "1")
    set (build_dir "cbuild_${arch}_c")
    set (DNC "-DDNC")
else()
    set (build_dir "cbuild_${arch}")
endif()

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

# Radioss interface module
add_subdirectory(./source/solver_interface solver_interface_${arch})


# Create the library
# -------------------
if (com STREQUAL "0")

   set (link_library_name "open_reader_${arch}" )
   add_library(${link_library_name} SHARED $<TARGET_OBJECTS:solver_interface_${arch}> )

   target_link_libraries(${link_library_name}
                      solver_interface_${arch}
                      dyna2rad_${arch}
                      io_${arch}
                      sdi_${arch}
                      cfgio_${arch}
		              cfgkernel_${arch}
                      ${open_reader_root_dir}/../extlib/boost/boost_1_70_0/static/lib/${arch}/libboost_filesystem.${lib})



#
   if (arch STREQUAL "win64")
        add_custom_command(TARGET ${link_library_name}
                   POST_BUILD
                   COMMAND ${CMAKE_COMMAND} -E make_directory ${open_reader_root_dir}/../exec
                   COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${link_library_name}> ${open_reader_root_dir}/../exec
                   COMMAND ${CMAKE_COMMAND} -E copy ${link_library_name}.lib ${open_reader_root_dir}/../exec
                   COMMAND ${CMAKE_COMMAND} -E copy ${link_library_name}.exp ${open_reader_root_dir}/../exec
                   )
    else()
        add_custom_command(TARGET ${link_library_name}
                   POST_BUILD
                   COMMAND ${CMAKE_COMMAND} -E make_directory ${open_reader_root_dir}/../exec
                   COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${link_library_name}> ${open_reader_root_dir}/../exec
                   )
    endif()

else()
   set (link_library_name "com_reader_${arch}" )
   add_library(${link_library_name} STATIC $<TARGET_OBJECTS:solver_interface_${arch}> 
                                           $<TARGET_OBJECTS:dyna2rad_${arch}>
                                           $<TARGET_OBJECTS:io_${arch}>
                                           $<TARGET_OBJECTS:sdi_${arch}>
                                           $<TARGET_OBJECTS:cfgio_${arch}>
                                           $<TARGET_OBJECTS:cfgkernel_${arch}> )

   add_custom_command(TARGET ${link_library_name}
                   POST_BUILD
                   COMMAND ${CMAKE_COMMAND} -E make_directory ${open_reader_root_dir}/../exec
                   COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${link_library_name}> ${open_reader_root_dir}/../exec
                   )
endif()







