#
# CMake project file for Radioss Starter
# --------------------------------------
 
cmake_minimum_required (VERSION 2.8)
project (Radioss_Engine)


# Verbose
# -------
# set (CMAKE_VERBOSE_MAKEFILE on)

# Language settings & Compiler Settings
# -------------------------------------
enable_language (Fortran)
enable_language (C)
enable_language (CXX)


# Load Compiler flags
# --------------------
if ( NOT DEFINED COM) 
# Define the URL and extraction directory
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
file(GLOB_RECURSE source_files  ${source_directory}/source/*.F ${source_directory}/stub/*.F ${source_directory}/share/*.F  ${source_directory}/../common_source/*.F ${source_directory}/source/*.F90 ${source_directory}/stub/*.F90 ${source_directory}/share/*.F90  ${source_directory}/../common_source/*.F90 ${source_directory}/source/*.f90 ${source_directory}/stub/*.f90 ${source_directory}/share/*.f90  ${source_directory}/../common_source/*.f90 )
#C source files
file(GLOB_RECURSE c_source_files ${source_directory}/source/*.c ${source_directory}/stub/*.c ${source_directory}/share/*.c ${source_directory}/../common_source/*.c )
#C++ source files
file(GLOB_RECURSE cpp_source_files ${source_directory}/source/*.cpp ${source_directory}/stub/*.cpp ${source_directory}/share/*.cpp ${source_directory}/../common_source/*.cpp )
file(GLOB_RECURSE include_files ${include_directory}/*.inc ${include_directory}/*.h ${source_directory}/../common_source/*.inc ${source_directory}/../common_source/*.h )

# Get the Fortran compiler name
get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

# Check if the Fortran compiler name contains "ifx" or "ifort"
string(FIND "${Fortran_COMPILER_NAME}" "ifx" Fortran_COMPILER_CONTAINS_IFX)
string(FIND "${Fortran_COMPILER_NAME}" "ifort" Fortran_COMPILER_CONTAINS_IFORT)

set(MUMPS_VERSION "5.5.1")

# Double precision & Intel compiler & MPI => automatic build of MUMPS if found in extlib
if((Fortran_COMPILER_CONTAINS_IFX GREATER -1 OR Fortran_COMPILER_CONTAINS_IFORT GREATER -1) AND NOT "${MPI}" STREQUAL "smp" AND "${precision}" STREQUAL "dp")
    set(MUMPS_PATH "${CMAKE_CURRENT_LIST_DIR}/extlib/MUMPS_${MUMPS_VERSION}")
    
    # Check if the MUMPS folder exists
    if(EXISTS "${MUMPS_PATH}")
        set(mumps_root "${MUMPS_PATH}")
        message(STATUS "mumps_root: ${mumps_root}")
    else()
        message(STATUS "MUMPS folder does not exist at ${MUMPS_PATH}")
        set(mumps_root "")
    endif()
    
    if(DEFINED mumps_root AND NOT "${mumps_root}" STREQUAL "")
       message(STATUS "MUMPS is enabled")
       #print the value of mumps_root
       set(MUMPS_INCLUDE_DIR "${mumps_root}/include")
       file(WRITE ${MUMPS_INCLUDE_DIR}/mumps_int_def.h "#define MUMPS_INTSIZE32\n")
       file(GLOB_RECURSE mumps_fortran  ${mumps_root}/src/*.F)
       list(FILTER mumps_fortran EXCLUDE REGEX ".*/[csz][^/]*\\.F$|.*\\\\[csz][^\\\\]*\\.F$")
       list(FILTER mumps_fortran EXCLUDE REGEX ".*/src/ve/src/.*")
       # Print out the contents of mumps_fortran after filtering for verification
       list(APPEND mumps_fortran ${mumps_root}/src/sol_common.F)
       file(GLOB_RECURSE mumps_c  ${mumps_root}/src/*.c  ${mumps_root}/PORD/lib/*.c )
       list(FILTER mumps_c EXCLUDE REGEX ".*/src/ve/src/.*")
       file(GLOB_RECURSE mumps_include ${mumps_root}/include/*.h  ${mumps_root}/PORD/include/*.h)
       list(FILTER mumps_include EXCLUDE REGEX ".*/src/ve/src/.*")
       list(APPEND source_files ${mumps_fortran})
       #append mumps_c to c_source_files
       list(APPEND c_source_files ${mumps_c})
       # append mumps_include to include_files
       list(APPEND include_files ${mumps_include})
    endif()
endif()





# Engine name
set (ex_version "engine")

else()

#Fortran source files
file(GLOB_RECURSE source_files  ${source_directory}/source/*.F ${source_directory}/com/*.F ${source_directory}/share/*.F  ${source_directory}/../common_source/*.F ${source_directory}/source/*.F90 ${source_directory}/com/*.F90 ${source_directory}/share/*.F90  ${source_directory}/../common_source/*.F90 ${source_directory}/source/*.f90 ${source_directory}/com/*.f90 ${source_directory}/share/*.f90  ${source_directory}/../common_source/*.f90)

if ( NOT DEFINED WITH_LINEAR_ALGEBRA OR precision STREQUAL "sp") 
# remove files related to linear algebra
file(GLOB_RECURSE remove_files ${source_directory}/com/eig/*.F  )
list(REMOVE_ITEM source_files ${remove_files})
endif()


#C source files
file(GLOB_RECURSE c_source_files ${source_directory}/source/*.c ${source_directory}/com/*.c ${source_directory}/share/*.c ${source_directory}/../common_source/*.c )

#C++ source files
file(GLOB_RECURSE cpp_source_files ${source_directory}/source/*.cpp ${source_directory}/com/*.cpp ${source_directory}/share/*.cpp ${source_directory}/../common_source/*.cpp )

file(GLOB_RECURSE include_files ${include_directory}/*.inc ${include_directory}/*.h  ${include_directory}/com/*.inc ${include_directory}/com/*.h ${source_directory}/../common_source/*.inc ${source_directory}/../common_source/*.h ../thid_party_com/*.h )

include (CMake_Compilers_c/cmake_eng_version.txt)
set (ex_version "e_${version}")

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
   message(STATUS "CMake_Compilers${cdir}/cmake_${arch}.txt  file not found ")
   set (arch "none")
endif ( NOT EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/CMake_Compilers${cdir}/cmake_${arch}.txt )


if ( NOT (arch STREQUAL "none") )
   message(STATUS "Loading ${arch} settings")
   include (${CMAKE_CURRENT_SOURCE_DIR}/CMake_Compilers${cdir}/cmake_${arch}.txt)

else ()
   if(UNIX)
     include (CMake_Compilers${cdir}/cmake_configure_linux.txt)
   endif (UNIX)
endif ()

#
# Print some configuration messages
# ---------------------------------
message (" " )
message (STATUS "Fortran Compiler: ${CMAKE_Fortran_COMPILER}")
message (STATUS "C Compiler: ${CMAKE_C_COMPILER}")
message (STATUS "CPP Compiler: ${CMAKE_CPP_COMPILER}")
message (STATUS "Exec name: ${EXEC_NAME} ")
message (STATUS "MPI: ${mpiver} ")
if ( mpi_os STREQUAL "1")
   message (STATUS "MPI in default OS installation : ${mpi_os} ")
endif()
message (STATUS "MPI libraries : ${mpi_lib} ")
message (STATUS "MPI include directory : ${mpi_inc} ")

message (STATUS "Flags release: ${CMAKE_Fortran_FLAGS_RELEASE} ")
message (STATUS "Precision: ${precision} ")
message (STATUS "Runtime Static link : ${static_link} ")
message (STATUS "debug: ${debug} ")
message (STATUS "PreCICE coupling: ${precice} ")
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

# Load external libraries
add_custom_target(
    extlib ALL
    COMMAND python ${source_directory}/../Compiling_tools/script/load_extlib.py
    COMMENT "Loading extlib libraries"
)

if ( NOT DEFINED COM )
#include generated with a text files
add_custom_command(    
         OUTPUT 
            ${Build_includes_directory}/engine_message_description.inc
         COMMAND 
         python ${source_directory}/../Compiling_tools/script/build_message.py -inputfile=${source_directory}/source/output/message/engine_message_description.txt -outfile=${Build_includes_directory}/engine_message_description.inc
         DEPENDS 
              ${source_directory}/source/output/message/engine_message_description.txt
         )

#include always rebuild 
add_custom_command(    
         OUTPUT 
            ${Build_includes_directory}/__foo.inc 
            ${Build_includes_directory}/build_info.inc
         COMMAND 
            python ${source_directory}/../Compiling_tools/script/or_build_info.py -arch=${arch} -outfile=${Build_includes_directory}/build_info.inc
         )
else()
#include generated with a text files
add_custom_command(    
         OUTPUT 
            ${Build_includes_directory}/engine_message_description.inc
         COMMAND 
            python ${source_directory}/../Compiling_tools/script/build_message.py -inputfile=${source_directory}/source/output/message/engine_message_description.txt -outfile=${Build_includes_directory}/engine_message_description.inc
         DEPENDS 
              ${source_directory}/source/output/message/engine_message_description.txt
         )

#include always rebuild 
add_custom_command(    
         OUTPUT 
            ${Build_includes_directory}/__foo.inc 
            ${Build_includes_directory}/build_info.inc
         COMMAND 
            python ${source_directory}/../Compiling_tools/script/build_info.py -version=${version} -titlevers=${titlevers} -arch=${arch} -solver=E -outfile=${Build_includes_directory}/build_info.inc
         )
endif()



# make sure that the default is a RELEASE
if (NOT CMAKE_BUILD_TYPE)
  set (CMAKE_BUILD_TYPE RELEASE CACHE STRING
      "Choose the type of build, options are: Non Debug Release."
      FORCE)
endif (NOT CMAKE_BUILD_TYPE)

if(precice STREQUAL "1")
  message(STATUS "Coupling with preCICE")
  find_package(precice REQUIRED CONFIG)
#else if cwipi == 1
elseif(cwipi STREQUAL "1")
  message(STATUS "Coupling with cwipi")
# find_package(cwipi REQUIRED CONFIG)
else()
  message(STATUS "No coupling library selected")
endif()


add_executable (${EXEC_NAME} ${source_files} ${source_files_common} ${c_source_files} ${cpp_source_files} ${Build_includes_directory}/engine_message_description.inc ${Build_includes_directory}/__foo.inc )

add_dependencies(${EXEC_NAME} extlib)

add_custom_command(TARGET ${EXEC_NAME}
                   POST_BUILD
                   COMMAND ${CMAKE_COMMAND} -E make_directory ${source_directory}/../exec
                   COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${EXEC_NAME}> ${source_directory}/../exec
                  )

target_include_directories(${EXEC_NAME} PRIVATE ${include_dir_list}  )

if(precice STREQUAL "1")
  message(STATUS "Linking with preCICE libraries")
  target_link_libraries(${EXEC_NAME} PRIVATE precice::precice ${LINK} )
else()
  target_link_libraries(${EXEC_NAME} ${LINK} )
endif()


# Fortran to link
set_property(TARGET ${EXEC_NAME} PROPERTY LINKER_LANGUAGE Fortran)

# install executables and scripts
install (TARGETS ${EXECUTABLES}
         RUNTIME DESTINATION "bin")
install (PROGRAMS ${SCRIPTS}
         DESTINATION "bin")

