#
# CMake project file for Radioss Starter
# --------------------------------------
 
cmake_minimum_required (VERSION 2.8)
project (Radioss_Starter)

# Verbose
# -------
# set (CMAKE_VERBOSE_MAKEFILE on)


# Language settings & Compiler Settings
# -------------------------------------
enable_language (Fortran)
enable_language (C)
enable_language (CXX)

if ( NOT DEFINED COM) 
set (cdir "" )
else()
set (cdir "_c" )
endif()


# source files & include directories
# -------------------------------------

set (source_directory ${CMAKE_CURRENT_SOURCE_DIR})
set (include_directory "${CMAKE_CURRENT_SOURCE_DIR}/share" )

# Source file settings
# --------------------

if ( NOT DEFINED COM) 

#Fortran source files
file(GLOB_RECURSE source_files  ${source_directory}/source/*.F ${source_directory}/stub/*.F ${source_directory}/share/*.F  ${source_directory}/../common_source/*.F ${source_directory}/source/*.F90 ${source_directory}/stub/*.F90 ${source_directory}/share/*.F90  ${source_directory}/../common_source/*.F90 ${source_directory}/source/*.F90 ${source_directory}/stub/*.F90 ${source_directory}/share/*.F90  ${source_directory}/../common_source/*.F90)

#C source files
file(GLOB_RECURSE c_source_files ${source_directory}/source/*.c ${source_directory}/stub/*.c ${source_directory}/share/*.c ${source_directory}/../common_source/*.c )

#C++ source files
file(GLOB_RECURSE cpp_source_files ${source_directory}/source/*.cpp ${source_directory}/stub/*.cpp ${source_directory}/share/*.cpp ${source_directory}/../common_source/*.cpp )

file(GLOB_RECURSE include_files ${include_directory}/*.inc ${include_directory}/*.h ${source_directory}/../common_source/*.inc ${source_directory}/../common_source/*.h )

# starter name
set (ex_version "starter")

else()

#Fortran source files
file(GLOB_RECURSE source_files  ${source_directory}/source/*.F ${source_directory}/com/*.F ${source_directory}/share/*.F  ${source_directory}/../common_source/*.F  ${source_directory}/source/*.F90 ${source_directory}/stub/*.F90 ${source_directory}/share/*.F90  ${source_directory}/../common_source/*.F90 ${source_directory}/../common_source/*.F90 ${source_directory}/source/*.F90 ${source_directory}/stub/*.F90 ${source_directory}/share/*.F90  ${source_directory}/../common_source/*.F90)

#Fortran90 Free source files
file(GLOB_RECURSE f90_source_files  ${source_directory}/source/*.F90 ${source_directory}/stub/*.F90 ${source_directory}/share/*.F90  ${source_directory}/../common_source/*.F90 )

#C source files
file(GLOB_RECURSE c_source_files ${source_directory}/source/*.c ${source_directory}/com/*.c ${source_directory}/share/*.c ${source_directory}/../common_source/*.c )

#C++ source files
file(GLOB_RECURSE cpp_source_files ${source_directory}/source/*.cpp ${source_directory}/com/*.cpp ${source_directory}/share/*.cpp ${source_directory}/../common_source/*.cpp )

file(GLOB_RECURSE include_files ${include_directory}/*.inc ${include_directory}/*.h  ${include_directory}/com/*.inc ${include_directory}/com/*.h ${source_directory}/../common_source/*.inc ${source_directory}/../common_source/*.h ../thid_party_com/*.h )

include (CMake_Compilers_c/cmake_st_version.txt)
set (ex_version "s_${version}")

endif()


# includes directory list
# -----------------------

set (include_dir_list "")

foreach (incfile ${include_files})
    get_filename_component(dir ${incfile} PATH)
    list (APPEND include_dir_list ${dir})
endforeach()

list(REMOVE_DUPLICATES include_dir_list)

# compatibility with previous Makefile system / remove old _inc_xxxx directory
file(GLOB_RECURSE delete_old_inc ${source_directory}/share/_inc_*/*.inc)
foreach (incfile ${delete_old_inc})
    get_filename_component(dir ${incfile} PATH)
    list (REMOVE_ITEM include_dir_list ${dir})
endforeach()


# Precision Settings
# ------------------

if (precision STREQUAL "sp")
  set (precision_dir "sp")
  set (precision_include_remove ${source_directory}/share/r8 )
  set (exec_prec "_sp")

else ()
  set (precision "dp")
  set (precision_dir "dp")
  set (precision_include_remove ${source_directory}/share/r4 )
  set (exec_prec "")
endif ()

if (debug STREQUAL "1")
  set (exec_deb "_db")
else ()
  set (exec_deb "")
endif ()

list(REMOVE_ITEM include_dir_list ${precision_include_remove})


#
# Configure Compilers 
# Compiler, link Flags, ...
# 
# using "arch" Variable 
# ----------------------------------------------------------

#
# additional build flags from command line
# -----------------------------------------
if (DEFINED  ENV{ADFL})
  set ( ADF "$ENV{ADFL}"  )
endif()

if (NOT DEFINED arch)
   set (arch "linux64_gf")
endif ()

if ( NOT EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/CMake_Compilers${cdir}/cmake_${arch}.txt )
   message(STATUS "CMake_Compilers/cmake_${arch}.txt  file not found ")
   set (arch "none")
endif ( NOT EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/CMake_Compilers${cdir}/cmake_${arch}.txt )


if ( NOT (arch STREQUAL "none") )
   message(STATUS "Loading ${arch} settings")
   include (${CMAKE_CURRENT_SOURCE_DIR}/CMake_Compilers${cdir}/cmake_${arch}.txt)
endif ()

#
# Print some configuration messages
# ---------------------------------
message (" " )
message (STATUS "Fortran Compiler: ${CMAKE_Fortran_COMPILER}")
message (STATUS "C Compiler: ${CMAKE_C_COMPILER}")
message (STATUS "CPP Compiler: ${CMAKE_CPP_COMPILER}")
message (STATUS "Exec name: ${EXEC_NAME} ")
message (STATUS "Flags release: ${CMAKE_Fortran_FLAGS_RELEASE} ")
message (STATUS "Precision: ${precision} ")
message (STATUS "Runtime Static link : ${static_link} ")
message (STATUS "debug: ${debug} ")
if ( DEFINED ADF )
   message (STATUS "Addflag : ${ADF} ")
endif()

if ( NOT DEFINED COM )
   message (STATUS "Build_type : o")
else()
   message (STATUS "Build_type : c")
endif()


message (" " )


#
# Adding some dependencies for files which are generated before compilation
# Create preprocessed files starter_message_description.inc
# --------------------------------------------------------------------------
  
#create subdirectory to store created includes
set ( Build_includes_directory ${CMAKE_CURRENT_BINARY_DIR}/CMakeFiles/includes_${EXEC_NAME} )
file(MAKE_DIRECTORY ${Build_includes_directory} )
list(APPEND include_dir_list ${Build_includes_directory} )
message (" dir : ${Build_includes_directory} added")



#include generated with a text files
add_custom_command(    
         OUTPUT 
            ${Build_includes_directory}/starter_message_description.inc
         COMMAND 
            python ${source_directory}/../Compiling_tools/script/merge_files.py -file1=${source_directory}/source/output/message/starter_message_description.txt -file2=${source_directory}/../hm_cfg_files/messages/CONFIG/msg_hw_radioss_reader.txt -outfile=${Build_includes_directory}/fullstarter_message_description.txt
         COMMAND
            python ${source_directory}/../Compiling_tools/script/build_message.py -inputfile=${Build_includes_directory}/fullstarter_message_description.txt -outfile=${Build_includes_directory}/starter_message_description.inc
         DEPENDS 
              ${source_directory}/source/output/message/starter_message_description.txt ${source_directory}/../hm_cfg_files/messages/CONFIG/msg_hw_radioss_reader.txt
         )


if ( NOT DEFINED COM )
#include always rebuild 
add_custom_command(    
         OUTPUT 
         ${Build_includes_directory}/__foo.inc
         ${Build_includes_directory}/build_info.inc
         COMMAND
            python ${source_directory}/../Compiling_tools/script/or_build_info.py -arch=${arch} -outfile=${Build_includes_directory}/build_info.inc
         DEPENDS 
         )
else()
#include always rebuild 
add_custom_command(    
         OUTPUT 
            ${Build_includes_directory}/__foo.inc 
            ${Build_includes_directory}/build_info.inc
         COMMAND 
            python ${source_directory}/../Compiling_tools/script/build_info.py -version=${version} -titlevers=${titlevers} -arch=${arch} -solver=S -outfile=${Build_includes_directory}/build_info.inc
         DEPENDS 
         )
endif()



# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: None Debug Release."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)




add_executable (${EXEC_NAME} ${source_files} ${c_source_files} ${cpp_source_files} ${Build_includes_directory}/starter_message_description.inc ${Build_includes_directory}/__foo.inc)

add_custom_command(TARGET ${EXEC_NAME}
                   POST_BUILD
                   COMMAND ${CMAKE_COMMAND} -E make_directory ${source_directory}/../exec
                   COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${EXEC_NAME}> ${source_directory}/../exec
                  )


target_include_directories(${EXEC_NAME} PRIVATE ${include_dir_list}  )
target_link_libraries(${EXEC_NAME} ${LINK} )

# Fortran to link
set_property(TARGET ${EXEC_NAME} PROPERTY LINKER_LANGUAGE Fortran)

# install executables and scripts
install (TARGETS ${EXECUTABLES}
         RUNTIME DESTINATION "bin")
install (PROGRAMS ${SCRIPTS}
         DESTINATION "bin")

