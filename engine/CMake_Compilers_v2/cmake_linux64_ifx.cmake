########################################
# ENGINE - Intel ifx / linux64
########################################

set(cppmach "-DCPP_mach=CPP_p4linux964")
set(cpprel  "-DCPP_rel=00")

# MPI (smp / impi / ompi)
# Note: ifx sets cpprel=40 for ompi
include(${CMAKE_CURRENT_LIST_DIR}/common/mpi_linux_ompi_impi.cmake)
if (DEFINED mpiver AND mpiver STREQUAL "ompi")
  set(cpprel "-DCPP_rel=40")
endif()
set(RELNAME ${arch}${mpi_suf})

# Third-party libraries
include(${CMAKE_CURRENT_LIST_DIR}/common/libs_linux64.cmake)

# Optional: MUMPS (Intel Linux style)
include(${CMAKE_CURRENT_LIST_DIR}/common/mumps_intel_linux.cmake)

# Common setup
include(${CMAKE_CURRENT_LIST_DIR}/common/precision.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/common/flags_reset_full.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/common/wo_linalg.cmake)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles/modules_${EXEC_NAME})
set(CMAKE_Fortran_MODDIR_FLAG "-module ")
include(${CMAKE_CURRENT_LIST_DIR}/common/module_dir.cmake)

# Release optimization flags (ifx-specific)
set(opt_flag_release  "-axCORE-AVX2,COMMON-AVX512 -qopt-mem-layout-trans=4 -no-fma -O3 -fp-model precise -fimf-use-svml=true -qopenmp")
set(optc_flag_release "-axCORE-AVX2,COMMON-AVX512 -qopt-mem-layout-trans=4 -no-fma -O3 -fp-model precise -fimf-use-svml=true -qopenmp")

# MUMPS linear algebra flags
set(linalg_flags  "${mumps_flag} ${mumps_inc}")
set(linalg_cflags "${mumps_flag} ${mumps_inc}")
set(Fortran       "${wo_linalg} -ftz -extend-source -assume buffered_io -align array64byte")

# Compilation flags
include(${CMAKE_CURRENT_LIST_DIR}/compiler/intel_linux.cmake)

# Linker and libraries
set(LINK "dl ${mumps_libs} ${MKL_lib} ${mpi_lib} ${zlib_lib} ${md5_lib} ${asan_link} -ldl -lrt -lstdc++")
string(STRIP "${LINK}" LINK)

# Per-file overrides
set(C_BASIC "${cppmach} ${cpprel}")
set_source_files_properties(${source_directory}/source/coupling/rad2rad/rad2rad_c.c  PROPERTIES COMPILE_FLAGS "${C_BASIC}")
set_source_files_properties(${source_directory}/source/coupling/rad2rad/sys_pipes_c.c PROPERTIES COMPILE_FLAGS "${C_BASIC} -std=c90")

set(F_O1_AXSSE3 "-O1 -axsse3 -no-fma -qopenmp -fp-model precise ${precision_flag} ${Fortran} ${mpi_flag} ${cppmach} ${cpprel}")
set(F_O2_AXSSE3 "-O2 -axsse3 -no-fma -qopenmp -fp-model precise ${precision_flag} ${Fortran} ${mpi_flag} ${cppmach} ${cpprel}")
set(F_O3_AXSSE3 "-O3 -axsse3 -no-fma -qopenmp -fp-model precise ${precision_flag} ${Fortran} ${mpi_flag} ${cppmach} ${cpprel}")

if (NOT debug OR debug STREQUAL "0")
  set_source_files_properties(${source_directory}/source/sty/outp_c_s.F  PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/sty/outp_c_t.F  PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/sty/outp_mt.F   PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/sty/outp_no.F   PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/sty/outp_n_v.F  PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/sty/outp_r_s.F  PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/sty/outp_r_t.F  PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/sty/outp_sp_s.F PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/sty/outp_sp_t.F PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/sty/outp_s_s.F  PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/sty/outp_s_t.F  PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/engine/resol_init.F  PROPERTIES COMPILE_FLAGS ${F_O1_AXSSE3})
  set_source_files_properties(${source_directory}/source/elements/shell/coqueba/cbasumg3.F PROPERTIES COMPILE_FLAGS ${F_O2_AXSSE3})
  set_source_files_properties(${source_directory}/source/input/redkey0.F      PROPERTIES COMPILE_FLAGS ${F_O1_AXSSE3})
  set_source_files_properties(${source_directory}/source/engine/resol.F       PROPERTIES COMPILE_FLAGS ${F_O2_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/inter3d/i3fri3.F PROPERTIES COMPILE_FLAGS ${F_O1_AXSSE3})
  set_source_files_properties(${source_directory}/source/output/restart/arralloc.F  PROPERTIES COMPILE_FLAGS ${F_O2_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/int11/i11cor3.F PROPERTIES COMPILE_FLAGS ${F_O2_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/intsort/i11pen3.F PROPERTIES COMPILE_FLAGS ${F_O2_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/int07/i7cor3t.F  PROPERTIES COMPILE_FLAGS ${F_O2_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/int20/i20cor3t.F PROPERTIES COMPILE_FLAGS ${F_O2_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/int23/i23cor3t.F PROPERTIES COMPILE_FLAGS ${F_O2_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/intsort/i20sto.F PROPERTIES COMPILE_FLAGS ${F_O2_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/intsort/i24cor3t.F PROPERTIES COMPILE_FLAGS ${F_O2_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/intsort/i25cor3t.F PROPERTIES COMPILE_FLAGS ${F_O2_AXSSE3})
  set_source_files_properties(${source_directory}/source/elements/sph/spbuc3.F  PROPERTIES COMPILE_FLAGS ${F_O2_AXSSE3})
  set_source_files_properties(${source_directory}/source/elements/sph/spclasv.F PROPERTIES COMPILE_FLAGS ${F_O2_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/intsort/i7buce_crit.F PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/elements/shell/coque/cupdt3.F   PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/elements/shell/coque/cupdtn3.F  PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/elements/shell/coqueba/cbafori.F PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/materials/mat/mat044/sigeps44c.F PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/interfaces/intsort/i25pen3_e2s.F PROPERTIES COMPILE_FLAGS ${F_O3_AXSSE3})
  set_source_files_properties(${source_directory}/source/model/remesh/admmap4.F  PROPERTIES COMPILE_FLAGS ${F_O2_AXSSE3})
  set_source_files_properties(${source_directory}/source/elements/elbuf/w_elbuf_str.F PROPERTIES COMPILE_FLAGS ${F_O2_AXSSE3})
  set_source_files_properties(${source_directory}/source/airbag/fv_up_switch.F  PROPERTIES COMPILE_FLAGS ${F_O1_AXSSE3})
  set_source_files_properties(${source_directory}/source/airbag/fvbag.F         PROPERTIES COMPILE_FLAGS ${F_O1_AXSSE3})
  set_source_files_properties(${source_directory}/source/airbag/fvbag0.F        PROPERTIES COMPILE_FLAGS ${F_O1_AXSSE3})
  set_source_files_properties(${source_directory}/source/airbag/fvbag1.F        PROPERTIES COMPILE_FLAGS ${F_O1_AXSSE3})
  set_source_files_properties(${source_directory}/source/airbag/fvbag2.F        PROPERTIES COMPILE_FLAGS ${F_O1_AXSSE3})
  set_source_files_properties(${source_directory}/source/output/tools/ieee.cpp  PROPERTIES COMPILE_FLAGS "-O1 ${C_BASIC}")
endif()

include(${CMAKE_CURRENT_LIST_DIR}/common/no_python.cmake)
