## GFortran Linux compilation flags.
## Required variables (caller sets before include):
##   cppmach, cpprel, precision_flag, zlib_inc, md5_inc, h3d_inc
##   ARCH_FLAGS      e.g. "" (x86-64) or "-march=armv8-a -DCOMP_GFORTRAN=1 -fdec-math -DARCH_CPU=ARM"
##   OPENMP          e.g. "-fopenmp"
##   opt_flag        base optimization flags (wo_linalg + COMP_GFORTRAN + OPENMP + mumps + coupling)
## Optional:
##   portability     e.g. "-fallow-argument-mismatch -fallow-invalid-boz -std=legacy"
##   strict          diagnostic error flags
##   strict_wo_mpi   strict without MPI unused-dummy-arg
##   error_uninitialized
##   FSANITIZE       pre-set by sanitize=1 check
##   unused_dummy_arg e.g. "-Werror=unused-dummy-argument" when using ompi
##   ADF             additional flags from -addflag
##   coupling        coupling flags (-DWITH_PRECICE etc.)

if (sanitize STREQUAL "1")
  set(FSANITIZE "-fsanitize=address -fsanitize=undefined -fsanitize=bounds-strict -DSANITIZE")
endif()

if (debug STREQUAL "analysis")
  set(plugin "-fplugin=../../scripts/gcc_plugin/fortran_signatures.so")
endif()

if (debug STREQUAL "asan")
  set(FSANITIZE "-fsanitize=address -fsanitize=undefined -fsanitize=bounds-strict -fsanitize=integer-divide-by-zero -fsanitize=object-size -fsanitize=float-divide-by-zero -DSANITIZE ${coupling}")
endif()

if (debug STREQUAL "1" OR debug STREQUAL "analysis" OR debug STREQUAL "asan")
  set(fortran_flags             "${plugin} ${FSANITIZE} ${ARCH_FLAGS} -Wall -Warray-temporaries -O0 -g -fdec-math -fbacktrace ${strict} ${opt_flag} ${precision_flag} ${cppmach} ${cpprel} -DCPP_comp=f90 -ffixed-line-length-none ${mpi_flag} ${ADF} ${coupling}")
  set(legacy_flags              "${plugin} ${FSANITIZE} -Wall -O0 -g -fdec-math -fbacktrace ${opt_flag} ${precision_flag} ${cppmach} ${cpprel} -DCPP_comp=f90 -ffixed-line-length-none ${portability} ${ARCH_FLAGS} ${mpi_flag} ${ADF} ${coupling}")
  set(unused_dummy_arg_strict_flags "${plugin} ${FSANITIZE} ${ARCH_FLAGS} -Wall -Warray-temporaries -O0 -g -fdec-math -fbacktrace ${strict_wo_mpi} ${opt_flag} ${precision_flag} ${cppmach} ${cpprel} -DCPP_comp=f90 -ffixed-line-length-none ${mpi_flag} ${ADF} ${coupling}")
  set(c_flags   "${FSANITIZE} ${h3d_inc} ${zlib_inc} ${md5_inc} ${precision_flag} ${cppmach} ${cpprel} ${ARCH_FLAGS} -O0 -g ${OPENMP} ${coupling}")
  set(cpp_flags "${FSANITIZE} ${h3d_inc} ${zlib_inc} ${md5_inc} ${precision_flag} ${cppmach} ${cpprel} ${ARCH_FLAGS} -O0 -g ${OPENMP} -std=c++14 ${coupling} ${mpi_flag}")
else()
  set(fortran_flags             "${ARCH_FLAGS} -nostdinc -O3 -fdec-math ${strict} ${opt_flag} ${precision_flag} ${cppmach} ${cpprel} -DCPP_comp=f90 -ffixed-line-length-none ${mpi_flag} ${ADF} ${coupling}")
  set(legacy_flags              "-nostdinc -w -O3 -fdec-math ${opt_flag} ${precision_flag} ${cppmach} ${cpprel} -DCPP_comp=f90 -ffixed-line-length-none ${portability} ${ARCH_FLAGS} ${mpi_flag} ${ADF} ${coupling}")
  set(unused_dummy_arg_strict_flags "${ARCH_FLAGS} -nostdinc -O3 -fdec-math ${strict_wo_mpi} ${opt_flag} ${precision_flag} ${cppmach} ${cpprel} -DCPP_comp=f90 -ffixed-line-length-none ${mpi_flag} ${ADF} ${coupling}")
  set(c_flags   "-w -O2 ${h3d_inc} ${zlib_inc} ${md5_inc} ${opt_flag} ${precision_flag} ${cppmach} ${cpprel} ${ARCH_FLAGS} ${coupling}")
  set(cpp_flags "-w -O2 ${h3d_inc} ${zlib_inc} ${md5_inc} ${opt_flag} ${precision_flag} ${cppmach} ${cpprel} ${ARCH_FLAGS} -std=c++14 ${coupling} ${mpi_flag}")
endif()

set_source_files_properties(${source_files}     PROPERTIES COMPILE_FLAGS "${fortran_flags}")
set_source_files_properties(${c_source_files}   PROPERTIES COMPILE_FLAGS "${c_flags}")
set_source_files_properties(${cpp_source_files} PROPERTIES COMPILE_FLAGS "${cpp_flags}")

set(CMAKE_EXE_LINKER_FLAGS "-rdynamic ${OPENMP} -ldl -lrt -lstdc++ ${FSANITIZE} ${CWIPI_FLAG}")
