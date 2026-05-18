## GFortran Linux compilation flags — shared by linux64_gf and linuxa64_gf.
##
## Required variables (must be set by the calling platform file):
##   cppmach          e.g. "-DCPP_mach=CPP_p4linux964"
##   cpprel           e.g. "-DCPP_rel=80"
##   precision_flag   set by common/precision.cmake
##   zlib_inc         set by common/libs_linux64.cmake or libs_linuxa64.cmake
##   md5_inc          set by common/libs_linux64.cmake or libs_linuxa64.cmake
##   source_files     Fortran source file list
##   c_source_files   C source file list
##   cpp_source_files C++ source file list
##   ARCH_FLAGS       Architecture-specific flags, e.g. "" or "-march=armv8-a -DARCH_CPU=ARM"
##   OPENMP           OpenMP flag, e.g. "-fopenmp"
##
## Optional variables (set by calling platform file before this include):
##   portability      e.g. "-fallow-argument-mismatch -fallow-invalid-boz -std=legacy" (GCC>10)
##   strict           Fortran diagnostic flags
##   error_uninitialized  e.g. "-Werror=maybe-uninitialized" (GCC 11/15/16)
##   ADF              Additional flags from build_script.sh -addflag option
##
## Sets: CMAKE_EXE_LINKER_FLAGS, FSANITIZE (for use in LINK by caller)

if (sanitize STREQUAL "1")
  set(FSANITIZE "-fsanitize=address -fsanitize=undefined -fsanitize=bounds-strict -DSANITIZE")
endif()

if (debug STREQUAL "analysis")
  set(plugin "-fplugin=../../scripts/gcc_plugin/fortran_signatures.so")
endif()

if (debug STREQUAL "asan")
  set(FSANITIZE "-fsanitize=address -fsanitize=undefined -fsanitize=bounds-strict -DSANITIZE")
endif()

if (debug STREQUAL "1" OR debug STREQUAL "analysis" OR debug STREQUAL "asan")
  set(legacy_flags  "${plugin} -Wall ${FSANITIZE} ${precision_flag} -DMETIS5 ${cppmach} ${cpprel} -DCPP_comp=f90 -DCOMP_GFORTRAN=1 -fdec-math -g -O0 -ffp-contract=off -frounding-math ${OPENMP} -fbacktrace -ffixed-line-length-none -Wdo-subscript ${ARCH_FLAGS} ${ADF} ${portability}")
  set(fortran_flags "${plugin} ${FSANITIZE} -Wall ${strict} ${precision_flag} -DMETIS5 ${cppmach} ${cpprel} -DCPP_comp=f90 -DCOMP_GFORTRAN=1 -fdec-math -g -O0 -ffp-contract=off -frounding-math ${OPENMP} -fbacktrace -ffixed-line-length-none ${ARCH_FLAGS} ${ADF}")
  set(c_flags       "${FSANITIZE} ${precision_flag} -DMETIS5 ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} -O0 -g ${OPENMP} ${ARCH_FLAGS}")
  set(cpp_flags     "${FSANITIZE} ${precision_flag} -DMETIS5 ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} -O0 -g ${OPENMP} -std=c++14 ${ARCH_FLAGS}")
else()
  set(legacy_flags  "-w ${precision_flag} -DMETIS5 ${cppmach} ${cpprel} -DCPP_comp=f90 -DCOMP_GFORTRAN=1 -fdec-math -O2 -ffp-contract=off -frounding-math ${OPENMP} -ffixed-line-length-none ${ARCH_FLAGS} ${ADF} ${portability}")
  set(fortran_flags "${precision_flag} ${strict} -DMETIS5 ${cppmach} ${cpprel} -DCPP_comp=f90 -DCOMP_GFORTRAN=1 -fdec-math -O2 -ffp-contract=off -frounding-math ${OPENMP} -ffixed-line-length-none ${ARCH_FLAGS} ${ADF}")
  set(c_flags       "-w ${precision_flag} -DMETIS5 ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} -O2 ${OPENMP} ${ARCH_FLAGS}")
  set(cpp_flags     "-w ${precision_flag} -DMETIS5 ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} -O2 ${OPENMP} -std=c++14 ${ARCH_FLAGS}")
endif()

set_source_files_properties(${source_files}     PROPERTIES COMPILE_FLAGS "${fortran_flags}")
set_source_files_properties(${c_source_files}   PROPERTIES COMPILE_FLAGS "${c_flags}")
set_source_files_properties(${cpp_source_files} PROPERTIES COMPILE_FLAGS "${cpp_flags}")

set(CMAKE_EXE_LINKER_FLAGS "-rdynamic -no-pie -O2 ${OPENMP} ${FSANITIZE}")
