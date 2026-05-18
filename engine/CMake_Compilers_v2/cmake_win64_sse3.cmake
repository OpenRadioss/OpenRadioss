########################################
# ENGINE - Intel ifort / win64 (SSE3 variant)
########################################

set(BUILD_SHARED_LIBS FALSE)
set(cppmach "-DCPP_mach=CPP_p4win64")

# MPI (smp / impi) — also sets cpprel and MKL_libraries
include(${CMAKE_CURRENT_LIST_DIR}/common/mpi_win64.cmake)
set(RELNAME ${arch}${mpi_suf})

# Third-party libraries
include(${CMAKE_CURRENT_LIST_DIR}/common/libs_win64.cmake)

# Optional: MUMPS (Windows Intel style)
include(${CMAKE_CURRENT_LIST_DIR}/common/mumps_win64.cmake)

# Common setup
include(${CMAKE_CURRENT_LIST_DIR}/common/precision.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/common/flags_reset_full.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/common/wo_linalg.cmake)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles/${EXEC_NAME}/modules)
set(CMAKE_Fortran_MODDIR_FLAG "/MODULE:")
include(${CMAKE_CURRENT_LIST_DIR}/common/module_dir.cmake)

# FPP flag (Ninja workaround)
if (CMAKE_GENERATOR STREQUAL "Ninja")
  set(FPP_FLAG "/nofpp")
else()
  set(FPP_FLAG "/fpp")
endif()

# Vectorization flags for win64_sse3 (SSE3)
set(VECT_OPT "/Qaxsse3,COMMON-AVX512 /Qfma- /Qimf-use-svml:true /align:array64byte")

# Build opt_flag (includes wo_linalg and MUMPS)
set(opt_flag  "${wo_linalg} ${VECT_OPT}")
set(optc_flag "${wo_linalg} /Qfma- /Qimf-use-svml:true")
if (DEFINED mumps_root AND NOT "${mumps_root}" STREQUAL "")
  set(opt_flag  "${opt_flag} ${mumps_flag} ${mumps_inc}")
  set(optc_flag "${optc_flag} ${mumps_flag} ${mumps_inc}")
endif()

# Compiler flags
include(${CMAKE_CURRENT_LIST_DIR}/compiler/intel_win64.cmake)

# Linker and libraries
set(CMAKE_EXE_LINKER_FLAGS "/F1500000000 /STACK:1500000000 /manifest:no ${MKL_libraries} ${mpi_lib} ${zlib_lib} ${md5_lib} libifport.lib svml_dispmt.lib libifcoremt.lib libmmt.lib Psapi.lib libvcruntime.lib")
set(LINK "advapi32.lib")

# Per-file overrides
set(C_BASIC "${cppmach} ${cpprel}")
set_source_files_properties(${source_directory}/source/output/tools/ieee.cpp  PROPERTIES COMPILE_FLAGS "${C_BASIC}")
set_source_files_properties(${source_directory}/source/coupling/rad2rad/rad2rad_c.c PROPERTIES COMPILE_FLAGS "${C_BASIC}")

set(Fortran_base "${FPP_FLAG} /extend-source /assume:buffered_io")
set(F_O1_AXSSE3 "/nologo /Qaxsse3 /Qopenmp /O1 /fp:precise /Qftz /Qimf-use-svml:true /align:array64byte ${FPP_FLAG} ${precision_flag} ${Fortran_base} -DMETIS5 ${mpi_flag} ${mumps_flag} ${cppmach} ${cpprel}")
set(F_O2_AXSSE3 "/nologo /Qaxsse3 /Qopenmp /O2 /fp:precise /Qftz /Qimf-use-svml:true /align:array64byte ${FPP_FLAG} ${precision_flag} ${Fortran_base} -DMETIS5 ${mpi_flag} ${mumps_flag} ${cppmach} ${cpprel}")
set(F_O3_AXSSE3 "/nologo /Qaxsse3 /Qopenmp /O3 /fp:precise /Qftz /Qimf-use-svml:true /align:array64byte ${FPP_FLAG} ${precision_flag} ${Fortran_base} -DMETIS5 ${mpi_flag} ${mumps_flag} ${cppmach} ${cpprel}")

if (NOT debug OR debug STREQUAL "0")
  set_source_files_properties(${source_directory}/source/elements/forintp.F        PROPERTIES COMPILE_FLAGS ${F_O1_AXSSE3})
  set_source_files_properties(${source_directory}/source/engine/resol_init.F       PROPERTIES COMPILE_FLAGS ${F_O1_AXSSE3})
  set_source_files_properties(${source_directory}/source/elements/shell/coqueba/cbasumg3.F PROPERTIES COMPILE_FLAGS ${F_O2_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/int21/i21mainf.F PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/input/redkey0.F           PROPERTIES COMPILE_FLAGS ${F_O1_AXSSE3})
  set_source_files_properties(${source_directory}/source/engine/resol.F            PROPERTIES COMPILE_FLAGS ${F_O2_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/inter3d/i3fri3.F PROPERTIES COMPILE_FLAGS ${F_O1_AXSSE3})
  set_source_files_properties(${source_directory}/source/output/restart/arralloc.F PROPERTIES COMPILE_FLAGS ${F_O2_AXSSE3})
  set_source_files_properties(${source_directory}/source/elements/sph/spbuc3.F     PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/elements/sph/spclasv.F    PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/intsort/i24cor3t.F PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/intsort/i25cor3t.F PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/int11/i11cor3.F PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/intsort/i11pen3.F PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/int07/i7cor3t.F PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/int20/i20cor3t.F PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/int23/i23cor3t.F PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/intsort/i20sto.F PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/materials/mat/mat044/sigeps44c.F PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/intsort/i25pen3_e2s.F PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/mpi/interfaces/spmd_i7tool.F PROPERTIES COMPILE_FLAGS ${F_O1_AXSSE3})
  set_source_files_properties(${source_directory}/source/model/remesh/admmap4.F    PROPERTIES COMPILE_FLAGS ${F_O2_AXSSE3})
  set_source_files_properties(${source_directory}/source/airbag/fv_up_switch.F     PROPERTIES COMPILE_FLAGS ${F_O1_AXSSE3})
  set_source_files_properties(${source_directory}/source/airbag/fvbag.F            PROPERTIES COMPILE_FLAGS ${F_O1_AXSSE3})
  set_source_files_properties(${source_directory}/source/airbag/fvbag0.F           PROPERTIES COMPILE_FLAGS ${F_O1_AXSSE3})
  set_source_files_properties(${source_directory}/source/airbag/fvbag1.F           PROPERTIES COMPILE_FLAGS ${F_O1_AXSSE3})
  set_source_files_properties(${source_directory}/source/airbag/fvbag2.F           PROPERTIES COMPILE_FLAGS ${F_O1_AXSSE3})
  set_source_files_properties(${source_directory}/source/elements/elbuf/w_elbuf_str.F PROPERTIES COMPILE_FLAGS ${F_O2_AXSSE3})
endif()

include(${CMAKE_CURRENT_LIST_DIR}/common/no_python.cmake)
