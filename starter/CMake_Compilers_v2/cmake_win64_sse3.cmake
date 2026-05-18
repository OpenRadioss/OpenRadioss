########################################
# STARTER - Intel Compiler / win64 (SSE3)
# Uses the Ninja-compatible FPP workaround.
########################################

set(BUILD_SHARED_LIBS FALSE)
set(RELNAME ${arch})
set(cppmach "-DCPP_mach=CPP_p4win64")
set(cpprel  "-DCPP_rel=00")

# Third-party libraries
include(${CMAKE_CURRENT_LIST_DIR}/common/reader_win64.cmake)
set(metis_lib "${source_directory}/../extlib/metis/win64/libmetis_win64_i2018_1_vs2015.lib")
set(MKL_Inc   "-DMKL")
set(MKL_Lib   "mkl_intel_lp64_dll.lib mkl_intel_thread_dll.lib mkl_core_dll.lib")
include(${CMAKE_CURRENT_LIST_DIR}/common/libs_win64.cmake)

# Common setup
include(${CMAKE_CURRENT_LIST_DIR}/common/precision.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/common/flags_reset_full.cmake)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles/${EXEC_NAME}/modules)
set(CMAKE_Fortran_MODDIR_FLAG "/module:")
include(${CMAKE_CURRENT_LIST_DIR}/common/module_dir.cmake)

# Ninja preprocesses files before compilation; /fpp would double-process them.
message(STATUS "CMake Generator: ${CMAKE_GENERATOR}")
if (CMAKE_GENERATOR STREQUAL "Ninja")
  set(FPP_FLAG "/nofpp")
else()
  set(FPP_FLAG "/fpp")
endif()
# VECT_OPT without /Qfma- (SSE3 target)
set(VECT_OPT "/nologo /Qaxsse3 /O2 /fp:precise /Qftz /Qopenmp")

# Compilation flags (debug/chkb/release)
include(${CMAKE_CURRENT_LIST_DIR}/compiler/intel_win64.cmake)

set(CMAKE_EXE_LINKER_FLAGS "${debug_link} /F1500000000 /STACK:1500000000 ${reader_lib} ${MKL_Lib} ${metis_lib} ${zlib_lib} ${md5_lib} svml_dispmd.lib")
set(LINK "advapi32.lib")

# Per-file flag overrides
set(F_O1_compiler_flags        "${FPP_FLAG} ${precision_flag} -DMETIS5 ${cppmach} ${cpprel} /extend-source /O1 /fp:precise /Qopenmp /Qftz")
set(F_O1_compiler_flags_no_omp "${FPP_FLAG} ${precision_flag} -DMETIS5 ${cppmach} ${cpprel} /extend-source /O1 /Qopenmp /fp:precise /Qftz")
set(F_O2_compiler_flags_no_omp "${FPP_FLAG} ${precision_flag} -DMETIS5 ${cppmach} ${cpprel} /extend-source /O2 /Qopenmp /fp:precise /Qftz")
if (NOT debug OR debug STREQUAL "0")
  set_source_files_properties(${source_directory}/source/starter/lectur.F        PROPERTIES COMPILE_FLAGS "${F_O1_compiler_flags}")
  set_source_files_properties(${source_directory}/source/tools/sect/prelecsec.F  PROPERTIES COMPILE_FLAGS "${F_O2_compiler_flags_no_omp}")
  set_source_files_properties(${source_directory}/source/elements/sph/nbsph.F   PROPERTIES COMPILE_FLAGS "${F_O1_compiler_flags_no_omp}")
endif()

include(${CMAKE_CURRENT_LIST_DIR}/common/no_python.cmake)
