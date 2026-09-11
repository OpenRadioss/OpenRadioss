#
# CMake project file for Radioss Starter
# --------------------------------------
 
cmake_minimum_required (VERSION 3.15)
project (Radioss_Engine)

# Windows specific settings
# --------------------------
if (arch STREQUAL "win64")
   set(CMAKE_MSVC_RUNTIME_LIBRARY "MultiThreaded$<$<CONFIG:Debug>:Debug>")
   set(PYTHON_EXEC "python")
else()
   set(PYTHON_EXEC "python3")
endif()


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
#CUDA source files (.cu) - only compiled when NVIDIA HPC SDK is used
file(GLOB_RECURSE cuda_source_files ${source_directory}/source/*.cu ${source_directory}/stub/*.cu ${source_directory}/share/*.cu ${source_directory}/../common_source/*.cu )
file(GLOB_RECURSE include_files ${include_directory}/*.inc ${include_directory}/*.h ${source_directory}/../common_source/*.inc ${source_directory}/../common_source/*.h )

# Get the Fortran compiler name
get_filename_component(Fortran_COMPILER_NAME ${CMAKE_Fortran_COMPILER} NAME)

# Check if the Fortran compiler name contains "ifx" or "ifort"
string(FIND "${Fortran_COMPILER_NAME}" "ifx" Fortran_COMPILER_CONTAINS_IFX)
string(FIND "${Fortran_COMPILER_NAME}" "ifort" Fortran_COMPILER_CONTAINS_IFORT)
# Check if the Fortran compiler name contains "nvfortran"
string(FIND "${Fortran_COMPILER_NAME}" "nvfortran" Fortran_COMPILER_CONTAINS_NVFORTRAN)

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
#CUDA source files (.cu) - only compiled when NVIDIA HPC SDK is used
file(GLOB_RECURSE cuda_source_files ${source_directory}/source/*.cu ${source_directory}/com/*.cu ${source_directory}/share/*.cu ${source_directory}/../common_source/*.cu )

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
    COMMAND ${PYTHON_EXEC} ${source_directory}/../Compiling_tools/script/load_extlib.py
    COMMENT "Loading extlib libraries"
)

if ( NOT DEFINED COM )
#include generated with a text files
add_custom_command(    
         OUTPUT 
            ${Build_includes_directory}/engine_message_description.inc
         COMMAND 
         ${PYTHON_EXEC} ${source_directory}/../Compiling_tools/script/build_message.py -inputfile=${source_directory}/source/output/message/engine_message_description.txt -outfile=${Build_includes_directory}/engine_message_description.inc
         DEPENDS 
              ${source_directory}/source/output/message/engine_message_description.txt
         )

#include always rebuild 
add_custom_command(    
         OUTPUT 
            ${Build_includes_directory}/__foo.inc 
            ${Build_includes_directory}/build_info.inc
         COMMAND 
            ${PYTHON_EXEC} ${source_directory}/../Compiling_tools/script/or_build_info.py -arch=${arch} -outfile=${Build_includes_directory}/build_info.inc
         )
else()
#include generated with a text files
add_custom_command(    
         OUTPUT 
            ${Build_includes_directory}/engine_message_description.inc
         COMMAND 
            ${PYTHON_EXEC} ${source_directory}/../Compiling_tools/script/build_message.py -inputfile=${source_directory}/source/output/message/engine_message_description.txt -outfile=${Build_includes_directory}/engine_message_description.inc
         DEPENDS 
              ${source_directory}/source/output/message/engine_message_description.txt
         )

#include always rebuild 
add_custom_command(    
         OUTPUT 
            ${Build_includes_directory}/__foo.inc 
            ${Build_includes_directory}/build_info.inc
         COMMAND 
            ${PYTHON_EXEC} ${source_directory}/../Compiling_tools/script/build_info.py -version=${version} -titlevers=${titlevers} -arch=${arch} -solver=E -outfile=${Build_includes_directory}/build_info.inc
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

# CUDA source files (.cu) — requires gpu_cc to be defined (NVIDIA HPC SDK builds).
# For non-GPU builds, discard any .cu files found during globbing;
# the Fortran module shell_gpu_mod provides stub implementations via #ifdef WITH_CUDA.
if (NOT DEFINED gpu_cc OR gpu_cc STREQUAL "")
  set(cuda_source_files "")
endif()

# Always exclude the C stub; Fortran module stubs handle the non-CUDA case
list(FILTER c_source_files EXCLUDE REGEX "shell_gpu_stub\\.c$")

if (cuda_source_files)
  message(STATUS "CUDA source files: ${cuda_source_files}")
endif()


add_executable (${EXEC_NAME} ${source_files} ${source_files_common} ${c_source_files} ${cpp_source_files} ${cuda_source_files} ${Build_includes_directory}/engine_message_description.inc ${Build_includes_directory}/__foo.inc )

add_dependencies(${EXEC_NAME} extlib)

add_custom_command(TARGET ${EXEC_NAME}
                   POST_BUILD
                   COMMAND ${CMAKE_COMMAND} -E make_directory ${source_directory}/../exec
                   COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${EXEC_NAME}> ${source_directory}/../exec
                  )

target_include_directories(${EXEC_NAME} PRIVATE ${include_dir_list}  )

# Define WITH_CUDA preprocessor macro when CUDA source files are compiled
if (cuda_source_files)
  target_compile_definitions(${EXEC_NAME} PRIVATE WITH_CUDA)
endif()

if(precice STREQUAL "1")
  message(STATUS "Linking with preCICE libraries")
  target_link_libraries(${EXEC_NAME} PRIVATE precice::precice ${LINK} )
else()
  target_link_libraries(${EXEC_NAME} ${LINK} )
endif()

option(OPENRADIOSS_EIG_SLEPC
       "Enable the optional SLEPc/PETSc public /EIG backend" OFF)

# The optional distributed eigensolver uses a small MPI assembly layer. Engine C
# sources do not otherwise inherit the Fortran MPI preprocessor/include
# flags, so apply them explicitly and only to the public /EIG adapter.
set(PUBLIC_EIG_C_SOURCES
    ${source_directory}/source/implicit/eig_solver_mpi.c
    ${source_directory}/source/implicit/eig_solver_slepc.c)
if(OPENRADIOSS_EIG_SLEPC AND DEFINED MPI AND NOT MPI STREQUAL "smp")
  set_property(SOURCE ${PUBLIC_EIG_C_SOURCES}
               APPEND PROPERTY COMPILE_DEFINITIONS PUBLIC_EIG_MPI)
  if(NOT "${mpi_inc}" STREQUAL "")
    separate_arguments(PUBLIC_EIG_MPI_COMPILE_OPTIONS NATIVE_COMMAND
                       "${mpi_inc}")
    set_property(SOURCE ${PUBLIC_EIG_C_SOURCES}
                 APPEND PROPERTY COMPILE_OPTIONS
                 ${PUBLIC_EIG_MPI_COMPILE_OPTIONS})
  endif()
endif()

set(SLEPC_ROOT "" CACHE PATH "SLEPc installation root")
set(PETSC_ROOT "" CACHE PATH "PETSc installation root")
if(OPENRADIOSS_EIG_SLEPC)
  if(NOT precision STREQUAL "dp")
    message(FATAL_ERROR
            "OPENRADIOSS_EIG_SLEPC currently requires precision=dp")
  endif()
  if(NOT DEFINED MPI OR MPI STREQUAL "smp")
    message(FATAL_ERROR
            "OPENRADIOSS_EIG_SLEPC requires an MPI Engine build")
  endif()
  find_path(SLEPC_INCLUDE_DIR slepceps.h
            HINTS ${SLEPC_ROOT}
            PATH_SUFFIXES include)
  find_library(SLEPC_LIBRARY slepc
               HINTS ${SLEPC_ROOT}
               PATH_SUFFIXES lib lib64)
  find_path(PETSC_INCLUDE_DIR petsc.h
            HINTS ${PETSC_ROOT}
            PATH_SUFFIXES include)
  find_library(PETSC_LIBRARY petsc
               HINTS ${PETSC_ROOT}
               PATH_SUFFIXES lib lib64)
  if(NOT SLEPC_INCLUDE_DIR OR NOT SLEPC_LIBRARY OR
     NOT PETSC_INCLUDE_DIR OR NOT PETSC_LIBRARY)
    message(FATAL_ERROR
            "OPENRADIOSS_EIG_SLEPC=ON requires SLEPc and PETSc")
  endif()
  if(NOT EXISTS "${PETSC_INCLUDE_DIR}/petscconf.h")
    message(FATAL_ERROR
            "Cannot inspect PETSc capabilities: petscconf.h was not found")
  endif()
  file(STRINGS "${PETSC_INCLUDE_DIR}/petscconf.h"
       PETSC_MUMPS_DEFINITION REGEX
       "^[ \t]*#define[ \t]+PETSC_HAVE_MUMPS[ \t]+1")
  if(NOT PETSC_MUMPS_DEFINITION)
    message(FATAL_ERROR
            "SLEPc /EIG requires PETSc built with MUMPS")
  endif()
  file(STRINGS "${PETSC_INCLUDE_DIR}/petscconf.h"
       PETSC_REAL_DOUBLE_DEFINITION REGEX
       "^[ \t]*#define[ \t]+PETSC_USE_REAL_DOUBLE[ \t]+1")
  file(STRINGS "${PETSC_INCLUDE_DIR}/petscconf.h"
       PETSC_COMPLEX_DEFINITION REGEX
       "^[ \t]*#define[ \t]+PETSC_USE_COMPLEX[ \t]+1")
  if(NOT PETSC_REAL_DOUBLE_DEFINITION OR PETSC_COMPLEX_DEFINITION)
    message(FATAL_ERROR
            "SLEPc /EIG requires PETSc built with real double-precision scalars")
  endif()
  # PETSc records the MPI implementation used to build it and diagnoses
  # incompatible mpi.h headers.  Exercise that check during configuration so
  # an MPI ABI mismatch fails immediately instead of part-way through the
  # several-thousand-file Engine build.
  include(CheckCSourceCompiles)
  include(CMakePushCheckState)
  cmake_push_check_state(RESET)
  set(CMAKE_REQUIRED_INCLUDES ${SLEPC_INCLUDE_DIR} ${PETSC_INCLUDE_DIR})
  set(CMAKE_REQUIRED_FLAGS "${mpi_inc}")
  unset(PUBLIC_EIG_PETSC_MPI_HEADERS_COMPATIBLE CACHE)
  check_c_source_compiles(
      "#include <mpi.h>\n#include <slepceps.h>\nint main(void) { return 0; }"
      PUBLIC_EIG_PETSC_MPI_HEADERS_COMPATIBLE)
  cmake_pop_check_state()
  if(NOT PUBLIC_EIG_PETSC_MPI_HEADERS_COMPATIBLE)
    message(FATAL_ERROR
            "SLEPc /EIG requires Engine and PETSc to use compatible MPI headers; set mpi_root or mpi_incdir to the MPI used to build PETSc")
  endif()
  message(STATUS "SLEPc /EIG factorizer: MUMPS")
  set_property(SOURCE
               ${source_directory}/source/implicit/eig_matrix_export.F
               ${source_directory}/source/implicit/eig_solver_backend.F
               ${source_directory}/source/implicit/eig_solver_slepc.c
               APPEND PROPERTY COMPILE_DEFINITIONS PUBLIC_EIG_SLEPC)
  # S8SLKE3 only needs the public BLAS entry points DSYMM and DGEMM, which
  # are supplied by the SLEPc/PETSc dependency stack.  Enable that element
  # kernel without unsetting WITHOUT_LINALG globally (doing so would also
  # expose unrelated legacy/proprietary implicit-solver paths).
  set_property(SOURCE
               ${source_directory}/source/elements/solid/solide8s/s8slke3.F
               APPEND PROPERTY COMPILE_DEFINITIONS PUBLIC_EIG_BLAS)
  set_property(SOURCE
               ${source_directory}/source/implicit/eig_solver_slepc.c
               APPEND PROPERTY INCLUDE_DIRECTORIES
               ${SLEPC_INCLUDE_DIR} ${PETSC_INCLUDE_DIR})
  # Retain source locations and frame chains in the two diagnostic boundaries
  # that can report a native /EIG failure.  This does not lower optimization
  # for the numerical kernels and only affects SLEPc-enabled Engine builds.
  if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    set_property(SOURCE
                 ${source_directory}/source/implicit/eig_solver_slepc.c
                 ${source_directory}/source/system/traceback_handler.c
                 APPEND PROPERTY COMPILE_OPTIONS
                 -g -fno-omit-frame-pointer)
  endif()
  target_link_libraries(${EXEC_NAME} ${SLEPC_LIBRARY} ${PETSC_LIBRARY})
  get_filename_component(SLEPC_LIBRARY_DIR "${SLEPC_LIBRARY}" DIRECTORY)
  get_filename_component(PETSC_LIBRARY_DIR "${PETSC_LIBRARY}" DIRECTORY)
  set(PETSC_RUNTIME_PATHS ${SLEPC_LIBRARY_DIR} ${PETSC_LIBRARY_DIR})
  set(PETSC_MPI_RUNTIME_PATHS "")
  separate_arguments(PETSC_MPI_LINK_OPTIONS NATIVE_COMMAND "${mpi_lib}")
  foreach(PETSC_MPI_LINK_OPTION ${PETSC_MPI_LINK_OPTIONS})
    if(PETSC_MPI_LINK_OPTION MATCHES "^-L(.+)$")
      list(APPEND PETSC_RUNTIME_PATHS "${CMAKE_MATCH_1}")
      list(APPEND PETSC_MPI_RUNTIME_PATHS "${CMAKE_MATCH_1}")
    endif()
  endforeach()
  list(REMOVE_DUPLICATES PETSC_RUNTIME_PATHS)
  list(REMOVE_DUPLICATES PETSC_MPI_RUNTIME_PATHS)
  set_property(TARGET ${EXEC_NAME} APPEND PROPERTY BUILD_RPATH
               "${PETSC_RUNTIME_PATHS}")
  set_property(TARGET ${EXEC_NAME} APPEND PROPERTY INSTALL_RPATH
               "\$ORIGIN/../lib" "${PETSC_MPI_RUNTIME_PATHS}")
  message(STATUS "Public /EIG SLEPc backend enabled: ${SLEPC_LIBRARY}")
endif()

# The distributed public eigensolvers reuse OpenRadioss's implicit MPI graph
# exchanges, but those communication-only routines were historically compiled
# only as a side effect of enabling the legacy Engine MUMPS solver.  Keep the
# exchange capability independent from either factorizer implementation.
if(OPENRADIOSS_EIG_SLEPC AND
   DEFINED MPI AND NOT MPI STREQUAL "smp")
  set_property(SOURCE
               ${source_directory}/source/mpi/implicit/imp_spmd.F
               APPEND PROPERTY COMPILE_DEFINITIONS PUBLIC_EIG_MPI)
endif()

# The public /EIG reference solver deliberately remains independent of
# MUMPS/PARPACK.  Double-precision builds use the small dense verification
# solver from the LAPACK archives already distributed with OpenRadioss.  This
# does not unset WITHOUT_LINALG and therefore does not enable the proprietary
# sparse-solver call paths.
set(PUBLIC_EIG_LAPACK_LIBS "")
if(NOT DEFINED COM AND precision STREQUAL "dp")
  if(arch STREQUAL "linux64_gf")
    set(PUBLIC_EIG_LAPACK_LIBS
        ${source_directory}/../extlib/lapack-3.10.0/lib_linux64_gf/liblapack.a
        ${source_directory}/../extlib/lapack-3.10.0/lib_linux64_gf/librefblas.a)
  elseif(arch STREQUAL "linuxa64_gf")
    set(PUBLIC_EIG_LAPACK_LIBS
        ${source_directory}/../extlib/lapack-3.10.0/lib_linuxa64/liblapack_gcc.a
        ${source_directory}/../extlib/lapack-3.10.0/lib_linuxa64/librefblas_gcc.a)
  elseif(arch STREQUAL "linux64_AOCC")
    set(PUBLIC_EIG_LAPACK_LIBS
        ${source_directory}/../extlib/lapack-3.10.0/lib_linux64_AOCC/liblapack.a
        ${source_directory}/../extlib/lapack-3.10.0/lib_linux64_AOCC/libblas.a)
  endif()
endif()

if(PUBLIC_EIG_LAPACK_LIBS)
  set_property(SOURCE
               ${source_directory}/source/implicit/eig_solver_backend.F
               APPEND PROPERTY COMPILE_DEFINITIONS PUBLIC_EIG_LAPACK)
  set_property(SOURCE
               ${source_directory}/source/elements/solid/solide8s/s8slke3.F
               APPEND PROPERTY COMPILE_DEFINITIONS PUBLIC_EIG_BLAS)
  target_link_libraries(${EXEC_NAME} ${PUBLIC_EIG_LAPACK_LIBS})
  message(STATUS "Public /EIG dense LAPACK solver enabled")
endif()


# Fortran to link
set_property(TARGET ${EXEC_NAME} PROPERTY LINKER_LANGUAGE Fortran)

# install executables and scripts
install (TARGETS ${EXEC_NAME}
         RUNTIME DESTINATION "bin")
install (PROGRAMS ${SCRIPTS}
         DESTINATION "bin")
