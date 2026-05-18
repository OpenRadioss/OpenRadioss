## ARM Flang (armflang/armflang++) compilation flags for linuxa64.
##
## Required variables (set by calling platform file):
##   cppmach, cpprel, precision_flag
##   zlib_inc, md5_inc
##   ARCH_OPT       e.g. "-O2 -march=armv8-a -DCOMP_ARMFLANG=1 -DARCH_CPU=ARM -nofma -fopenmp -fveclib=none"
##   FLAG_PRECISE   e.g. "-ffp-contract=off -fno-unsafe-math-optimizations -fno-fast-math"
##   Fortran_base   e.g. "-DCPP_comp=f90 -ffixed-line-length-none -fno-backslash"
##   source_files, c_source_files, cpp_source_files
##
## Optional variables:
##   Tet_mesher_inc, ADF
##
## Sets: CMAKE_EXE_LINKER_FLAGS

if (debug STREQUAL "1" OR debug STREQUAL "asan")
  # Address sanitizer is not available on Linux/ARM — asan falls back to debug=1
  set(ARCH_OPT_DBG "-O0 -march=armv8-a -DCOMP_ARMFLANG=1 -DARCH_CPU=ARM -nofma -fopenmp")
  set(fortran_flags "-g ${precision_flag} ${Tet_mesher_inc} -DMETIS5 ${cppmach} ${cpprel} ${ARCH_OPT_DBG} ${FLAG_PRECISE} ${Fortran_base} ${ADF}")
  set(c_flags       "-g ${precision_flag} ${Tet_mesher_inc} -DMETIS5 ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} ${ARCH_OPT_DBG}")
  set(cpp_flags     "-g ${precision_flag} ${Tet_mesher_inc} -DMETIS5 ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} ${ARCH_OPT_DBG}")
else()
  set(fortran_flags "${precision_flag} ${Tet_mesher_inc} -DMETIS5 ${cppmach} ${cpprel} ${ARCH_OPT} ${FLAG_PRECISE} ${Fortran_base} ${ADF}")
  set(c_flags       "${precision_flag} ${Tet_mesher_inc} -DMETIS5 ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} ${ARCH_OPT}")
  set(cpp_flags     "${precision_flag} ${Tet_mesher_inc} -DMETIS5 ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} ${ARCH_OPT}")
endif()

set_source_files_properties(${source_files}     PROPERTIES COMPILE_FLAGS "${fortran_flags}")
set_source_files_properties(${c_source_files}   PROPERTIES COMPILE_FLAGS "${c_flags}")
set_source_files_properties(${cpp_source_files} PROPERTIES COMPILE_FLAGS "${cpp_flags}")

set(CMAKE_EXE_LINKER_FLAGS "-march=armv8-a -nofma -DCOMP_ARMFLANG=1 -ffp-contract=off -fno-unsafe-math-optimizations -fno-fast-math -O2 -fopenmp -ffixed-line-length-none -fno-backslash -rdynamic -armpl")
