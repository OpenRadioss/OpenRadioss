######################################
# STARTER - Intel ifx Compiler / linux64
######################################

set(RELNAME ${arch})
set(cppmach "-DCPP_mach=CPP_p4linux964")
set(cpprel  "-DCPP_rel=00")

# Third-party libraries
include(${CMAKE_CURRENT_LIST_DIR}/common/reader_linux64.cmake)
set(metis_lib "${source_directory}/../extlib/metis/linux64/libmetis_linux64_gcc.a")
include(${CMAKE_CURRENT_LIST_DIR}/common/libs_linux64.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/common/mkl.cmake)

# Common setup
include(${CMAKE_CURRENT_LIST_DIR}/common/precision.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/common/flags_reset_full.cmake)
set(CMAKE_Fortran_MODDIR_FLAG "-module ")
include(${CMAKE_CURRENT_LIST_DIR}/common/module_dir.cmake)

# Compilation flags (debug/chkb/asan/release)
include(${CMAKE_CURRENT_LIST_DIR}/compiler/intel_linux.cmake)

# Link libraries
set(LINK "dl ${xml_lib} ${metis_lib} ${reader_lib} ${MKL_lib} ${Tet_mesher_lib} ${zlib_lib} ${md5_lib} ${asan_link} -ldl -rdynamic -lstdc++")

# Per-file flag overrides
set(F_O0_compiler_flags "${precision_flag} ${cppmach} ${cpprel} -axsse3 -extend-source -O0 -ftz -qopenmp -fp-model precise ${ADF}")
set(F_O1_compiler_flags "${precision_flag} ${cppmach} ${cpprel} -axsse3 -extend-source -O1 -ftz -qopenmp -fp-model precise ${ADF}")

if (NOT debug OR debug STREQUAL "0")
  set_source_files_properties(${source_directory}/source/elements/sph/nbsph.F
    PROPERTIES COMPILE_FLAGS "${F_O1_compiler_flags}")
  set_source_files_properties(${source_directory}/source/starter/lectur.F
    PROPERTIES COMPILE_FLAGS "${F_O1_compiler_flags}")
  set(C_BASIC "${cppmach} ${cpprel}")
  set_source_files_properties(${source_directory}/source/output/tools/ieee.cpp
    PROPERTIES COMPILE_FLAGS "-O1 ${C_BASIC}")
endif()

set_source_files_properties(${source_directory}/source/system/rad_sys_call.c
  PROPERTIES COMPILE_FLAGS "${precision_flag} -std=c90 ${cppmach} ${cpprel}")

# Compiler bug fix: ani_fasolfr.F must be compiled at O0
set_source_files_properties(${source_directory}/source/output/anim/ani_fasolfr.F
  PROPERTIES COMPILE_FLAGS "${F_O0_compiler_flags}")

include(${CMAKE_CURRENT_LIST_DIR}/common/no_python.cmake)
