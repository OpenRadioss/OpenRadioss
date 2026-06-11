########################################
# ENGINE - NVIDIA HPC SDK / linux64
########################################

set(cppmach "-DCPP_mach=CPP_p4linux964")
set(cpprel  "-DCPP_rel=80")

# MPI (smp / ompi with NVHPC HPC-X discovery)
include(${CMAKE_CURRENT_LIST_DIR}/common/mpi_linux_ompi_nvhpc.cmake)
set(RELNAME ${arch}${mpi_suf})

# Third-party libraries
include(${CMAKE_CURRENT_LIST_DIR}/common/libs_linux64.cmake)

# Note: MUMPS is not supported with the NVIDIA compiler
set(wo_linalg "-DWITHOUT_LINALG")

# Common setup
include(${CMAKE_CURRENT_LIST_DIR}/common/precision.cmake)
include(${CMAKE_CURRENT_LIST_DIR}/common/flags_reset_full.cmake)
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles/modules)
set(CMAKE_Fortran_MODDIR_FLAG "-module ")
include(${CMAKE_CURRENT_LIST_DIR}/common/module_dir.cmake)

# GPU compute capability (cc80=A100, cc90=H100, cc120=B200); override with -Dgpu_cc=ccXX
if (NOT DEFINED gpu_cc)
  set(gpu_cc "cc80")
endif()

# CUDA toolkit path; override with NVHPC_CUDA_HOME or CUDA_HOME env vars
set(cuda_home "/usr/local/cuda")
if (DEFINED ENV{NVHPC_CUDA_HOME})
  set(cuda_home "$ENV{NVHPC_CUDA_HOME}")
elseif (DEFINED ENV{CUDA_HOME})
  set(cuda_home "$ENV{CUDA_HOME}")
endif()

# OpenACC GPU offloading (enable with -Dopenacc=1)
# Note: -cuda/-cudaforlibs omitted — they cause a deadlock with mem:managed on Blackwell GPUs
if (openacc STREQUAL "1")
  message(STATUS "OpenACC enabled – GPU target: ${gpu_cc}")
  message(STATUS "CUDA toolkit   : ${cuda_home}")
  set(acc_fort_flags "-acc -gpu=${gpu_cc} -Minfo=accel -Mdetail")
  set(acc_link_flags "-acc -gpu=${gpu_cc}")
  set(acc_link_libs  "-L${cuda_home}/lib64 -lnvToolsExt")
else()
  set(acc_fort_flags "")
  set(acc_link_flags "")
  set(acc_link_libs  "")
endif()

# NVIDIA HPC SDK intrinsic modules path
set(nv_sdk_inc "/opt/nvidia/hpc_sdk/Linux_x86_64/26.3/compilers/include")
if (DEFINED ENV{NVHPC})
  set(nv_sdk_inc "$ENV{NVHPC}/compilers/include")
endif()

set(fort_flags "-Mnofma -mp -traceback -Mextend -Munroll -Mvect=simd -Minform=warn -Mlarge_arrays -I${nv_sdk_inc} ${acc_fort_flags}")

# Compiler flags (debug/release)
include(${CMAKE_CURRENT_LIST_DIR}/compiler/nvhpc_linux.cmake)

# CUDA source files (.cu) compiled as C++ via nvc++
if (cuda_source_files)
  set_source_files_properties(${cuda_source_files} PROPERTIES LANGUAGE CXX)
  if (debug STREQUAL "1")
    set_source_files_properties(${cuda_source_files} PROPERTIES COMPILE_FLAGS
      "-g -O0 -cuda -gpu=${gpu_cc} -mp -Minfo=accel --diag_suppress cuda_compile ${h3d_inc} ${zlib_inc} ${md5_inc} ${precision_flag} ${cppmach} ${cpprel} -std=c++17 ${mpi_flag}")
  else()
    set_source_files_properties(${cuda_source_files} PROPERTIES COMPILE_FLAGS
      "-w -O3 -cuda -gpu=${gpu_cc} -mp -Minfo=accel --diag_suppress cuda_compile ${h3d_inc} ${zlib_inc} ${md5_inc} ${precision_flag} ${cppmach} ${cpprel} -std=c++17 ${mpi_flag}")
  endif()
  set(cuda_link_flags "-cuda -gpu=${gpu_cc}")
  set(cuda_link_libs  "-L${cuda_home}/lib64 -lcudart")
  message(STATUS "CUDA sources detected – GPU target: ${gpu_cc}")
  message(STATUS "CUDA toolkit: ${cuda_home}")
else()
  set(cuda_link_flags "")
  set(cuda_link_libs  "")
endif()

# Linker flags (override what nvhpc_linux.cmake set to add acc/cuda)
set(CMAKE_EXE_LINKER_FLAGS "-Wl,--export-dynamic -Wl,-no-pie -mp ${acc_link_flags} ${cuda_link_flags}")
string(STRIP "${CMAKE_EXE_LINKER_FLAGS}" CMAKE_EXE_LINKER_FLAGS)

# Libraries
if (static_link STREQUAL "1")
  set(LINK "rt ${zlib_lib} ${md5_lib} ${mpi_lib} ${acc_link_libs} ${cuda_link_libs} -ldl -Bstatic_pgi -Bstatic_c++libs")
else()
  set(LINK "rt ${zlib_lib} ${md5_lib} ${mpi_lib} ${acc_link_libs} ${cuda_link_libs} -ldl")
endif()
string(STRIP "${LINK}" LINK)

# Per-file overrides (release only)
set(F_O0_compiler_flags "-O0 ${fort_flags} ${wo_linalg} ${precision_flag} ${cppmach} ${cpprel} -DCPP_comp=f90 -DCOMP_NVFORTRAN=1 ${mpi_flag} ${ADF}")
set(F_O1_compiler_flags "-O1 ${fort_flags} ${wo_linalg} ${precision_flag} ${cppmach} ${cpprel} -DCPP_comp=f90 -DCOMP_NVFORTRAN=1 ${mpi_flag} ${ADF}")
set(F_O2_compiler_flags "-O2 ${fort_flags} ${wo_linalg} ${precision_flag} ${cppmach} ${cpprel} -DCPP_comp=f90 -DCOMP_NVFORTRAN=1 ${mpi_flag} ${ADF}")
set(C_O1_compiler_flags "-O1 -mp ${precision_flag} ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} ${mpi_flag} ${ADF}")

if (NOT debug OR debug STREQUAL "0")
  set_source_files_properties(${source_directory}/source/engine/resol_init.F        PROPERTIES COMPILE_FLAGS ${F_O1_compiler_flags})
  set_source_files_properties(${source_directory}/source/engine/resol.F             PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/output/restart/arralloc.F  PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/materials/mat/mat001/m1lawp.F PROPERTIES COMPILE_FLAGS ${F_O1_compiler_flags})
  set_source_files_properties(${source_directory}/source/elements/shell/coqueba/cbasumg3.F PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/input/redkey0.F            PROPERTIES COMPILE_FLAGS ${F_O1_compiler_flags})
  set_source_files_properties(${source_directory}/source/interfaces/inter3d/i3fri3.F PROPERTIES COMPILE_FLAGS ${F_O1_compiler_flags})
  set_source_files_properties(${source_directory}/source/implicit/dsolve/dsgri7.F   PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/interfaces/int11/i11cor3.F PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/interfaces/intsort/i11pen3.F PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/interfaces/int07/i7cor3t.F PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/interfaces/int20/i20cor3.F PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/interfaces/intsort/i20sto.F PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/interfaces/int23/i23cort3.F PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/interfaces/int24/i24cort3.F PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/interfaces/int25/i25cort3.F PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/elements/sph/spbuc3.F      PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/elements/sph/spclasv.F     PROPERTIES COMPILE_FLAGS ${F_O2_compiler_flags})
  set_source_files_properties(${source_directory}/source/coupling/rad2rad/rad2rad_c.c PROPERTIES COMPILE_FLAGS ${C_O1_compiler_flags})
endif()

include(${CMAKE_CURRENT_LIST_DIR}/common/no_python.cmake)
