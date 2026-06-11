## MUMPS linear algebra solver setup for Intel compilers on Windows.
## Sets: MKL_lib, mumps_flag, mumps_inc, WITH_LINEAR_ALGEBRA
if (DEFINED mumps_root AND NOT "${mumps_root}" STREQUAL "")
  message(STATUS "MUMPS root directory: ${mumps_root}")
  if (mpiver STREQUAL "smp")
    message(FATAL_ERROR "\n ERROR: cannot use MUMPS without -mpi=impi")
  endif()
  if (DEFINED ENV{MKLROOT})
    set(MKL_IDIR "$ENV{MKLROOT}/include")
    set(MKL_LDIR "$ENV{MKLROOT}/lib/intel64")
  else()
    set(MKL_IDIR "C:/Program Files (x86)/Intel/oneAPI/mkl/latest/include")
    set(MKL_LDIR "C:/Program Files (x86)/Intel/oneAPI/mkl/latest/lib/intel64")
  endif()
  set(MKL_lib
    "${MKL_LDIR}/mkl_scalapack_lp64.lib"
    "${MKL_LDIR}/mkl_intel_lp64.lib"
    "${MKL_LDIR}/mkl_intel_thread.lib"
    "${MKL_LDIR}/mkl_core.lib"
    "${MKL_LDIR}/mkl_blacs_intelmpi_lp64.lib")
  set(MUMPSLIB  "${mumps_root}/lib")
  set(mumps_inc  "-I${mumps_root}/include")
  set(mumps_flag "-Dpord -Dthr_all -DMUMPS5 -DAdd_ -DMUMPS_ARITH=MUMPS_ARITH_d")
  set(WITH_LINEAR_ALGEBRA "yes")
endif()
