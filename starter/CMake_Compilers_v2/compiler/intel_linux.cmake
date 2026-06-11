## Intel Fortran Linux compilation flags — shared by linux64_ifort and linux64_ifx.
##
## Required variables (must be set by the calling platform file):
##   cppmach          e.g. "-DCPP_mach=CPP_p4linux964"
##   cpprel           e.g. "-DCPP_rel=00"
##   precision_flag   set by common/precision.cmake
##   zlib_inc         set by common/libs_linux64.cmake
##   md5_inc          set by common/libs_linux64.cmake
##   source_files, c_source_files, cpp_source_files
##
## Optional variables:
##   Tet_mesher_inc   Tet mesher include flags (commercial variant only)
##   ADF              Additional flags from build_script.sh -addflag option
##
## Sets: CMAKE_EXE_LINKER_FLAGS, asan_link (for use in LINK by caller)

if (debug STREQUAL "1")

  set(fortran_flags "${precision_flag} ${Tet_mesher_inc} ${cppmach} ${cpprel} -DCPP_comp=f90 -g -O0 -fp-model precise -qopenmp -ftz -extend-source -fpe0 -ftrapuv -traceback ${ADF}")
  set(c_flags       "${precision_flag} ${Tet_mesher_inc} ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} -O0 -g -traceback -openmp")
  set(cpp_flags     "${precision_flag} ${Tet_mesher_inc} ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} -O0 -g -traceback -std=c++11")

elseif (debug STREQUAL "chkb")

  set(fortran_flags "${precision_flag} ${Tet_mesher_inc} ${cppmach} ${cpprel} -DCPP_comp=f90 -g -check bounds -O0 -fp-model precise -qopenmp -ftz -extend-source -fpe0 -ftrapuv -traceback ${ADF}")
  set(c_flags       "${precision_flag} ${Tet_mesher_inc} ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} -O0 -g -traceback -openmp")
  set(cpp_flags     "${precision_flag} ${Tet_mesher_inc} ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} -O0 -g -traceback -std=c++11")

elseif (debug STREQUAL "asan")

  set(fortran_flags "${precision_flag} ${Tet_mesher_inc} ${cppmach} ${cpprel} -DCPP_comp=f90 -g -fsanitize=address -DSANITIZE -O0 -fp-model precise -qopenmp -ftz -extend-source -fpe0 -ftrapuv -traceback ${ADF}")
  set(c_flags       "${precision_flag} ${Tet_mesher_inc} ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} -fsanitize=address -DSANITIZE -O0 -g -traceback -openmp")
  set(cpp_flags     "${precision_flag} ${Tet_mesher_inc} ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} -fsanitize=address -DSANITIZE -O0 -g -traceback -std=c++11")
  set(asan_link     "-fsanitize=address")

else()

  set(fortran_flags "${precision_flag} ${Tet_mesher_inc} ${cppmach} ${cpprel} -DCPP_comp=f90 -axsse3 -O2 -fp-model precise -ftz -extend-source -qopenmp -assume buffered_io ${ADF}")
  set(c_flags       "${precision_flag} ${Tet_mesher_inc} ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} -qopenmp")
  set(cpp_flags     "${precision_flag} ${Tet_mesher_inc} ${cppmach} ${cpprel} ${zlib_inc} ${md5_inc} -axsse3 -O2 -fopenmp -Wno-deprecated -fpermissive -std=c++11 -fPIC")

endif()

set_source_files_properties(${source_files}     PROPERTIES COMPILE_FLAGS "${fortran_flags}")
set_source_files_properties(${c_source_files}   PROPERTIES COMPILE_FLAGS "${c_flags}")
set_source_files_properties(${cpp_source_files} PROPERTIES COMPILE_FLAGS "${cpp_flags}")

set(CMAKE_EXE_LINKER_FLAGS "-O2 -qopenmp -static-intel -qopenmp-link=static ${asan_link}")
