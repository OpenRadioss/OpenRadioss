########################################
# STARTER - AMD AOCC Compiler / linux64
########################################
# Note: advanced arch flags (e.g. -march=zen4) can be passed via -addflag.

set(RELNAME ${arch})
set(cppmach "-DCPP_mach=CPP_p4linux964 -DCOMP_AOCC")
set(cpprel  "-DCPP_rel=96")

# Third-party libraries
include(${CMAKE_CURRENT_LIST_DIR}/common/reader_linux64.cmake)
set(lapack_lib "${source_directory}/../extlib/lapack-3.10.0/lib_linux64_AOCC/liblapack.a ${source_directory}/../extlib/lapack-3.10.0/lib_linux64_AOCC/libblas.a ${source_directory}/../extlib/lapack-3.10.0/lib_linux64_gf/libtmglib.a")
set(metis_lib  "${source_directory}/../extlib/metis/linux64/libmetis_linux64_gcc.a")
include(${CMAKE_CURRENT_LIST_DIR}/common/libs_linux64.cmake)

# Common setup
include(${CMAKE_CURRENT_LIST_DIR}/common/precision.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/common/flags_reset_minimal.cmake)
set(CMAKE_Fortran_MODDIR_FLAG "-J")
include(${CMAKE_CURRENT_LIST_DIR}/common/module_dir.cmake)

# Compilation flags (debug/release)
include(${CMAKE_CURRENT_LIST_DIR}/compiler/aocc_linux.cmake)

# Link libraries
if (static_link STREQUAL "1")
  set(LINK "dl ${flexpipe_lib} ${metis_lib} ${reader_lib} ${lapack_lib} ${zlib_lib} ${md5_lib} -ldl -static-libgfortran -static-libstdc++ -static-libgcc -Wunused-function")
else()
  set(LINK "dl ${flexpipe_lib} ${metis_lib} ${reader_lib} ${lapack_lib} ${zlib_lib} ${md5_lib} -ldl -Wunused-function")
endif()

# Per-file flag overrides
set(F_O0_compiler_flags "-O0 ${precision_flag} -DMETIS5 ${cppmach} ${cpprel} -Mextend -Mbackslash")
if (NOT debug OR debug STREQUAL "0")
  set_source_files_properties(${source_directory}/source/output/message/inimsg.F
    PROPERTIES COMPILE_FLAGS "${F_O0_compiler_flags}")
endif()

include(${CMAKE_CURRENT_LIST_DIR}/common/no_python.cmake)
