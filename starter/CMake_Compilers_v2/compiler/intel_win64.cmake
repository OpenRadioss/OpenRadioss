## Intel Fortran Windows compilation flags — shared by win64, win64_ifort, win64_sse3.
##
## Required variables (must be set by the calling platform file):
##   cppmach       e.g. "-DCPP_mach=CPP_p4win64"
##   cpprel        e.g. "-DCPP_rel=00"
##   precision_flag  set by common/precision.cmake
##   zlib_inc      set by the platform file (win64 uses a different zlib path structure)
##   source_files, c_source_files, cpp_source_files
##   VECT_OPT      Vectorisation/optimisation flags, e.g. "/nologo /Qaxsse3 /O2 /fp:precise /Qftz /Qopenmp"
##                 (may or may not include /Qfma- depending on the target)
##   FPP_FLAG      Preprocessor flag: "/fpp" normally, "/nofpp" under Ninja to avoid double-processing
##
## Optional variables:
##   Tet_mesher_inc, md5_inc
##   ADF
##
## Sets: debug_link (for use in CMAKE_EXE_LINKER_FLAGS by caller)

set(Fortran_base "${FPP_FLAG} /extend-source /assume:buffered_io")

if (debug STREQUAL "1")

  set(fortran_flags "/nologo ${precision_flag} -DMETIS5 ${cppmach} ${cpprel} -DCPP_comp=f90 ${FPP_FLAG} /Od /debug:all /Zi /fp:precise /Qfma- /Qftz /extend-source /assume:buffered_io /Qopenmp ${ADF} /traceback")
  set(c_flags       "${precision_flag} -DMETIS5 ${cppmach} ${cpprel} /nologo /Od /debug:all /Zi /Qftz /Qopenmp /traceback ${zlib_inc} ${md5_inc}")
  set(cpp_flags     "${precision_flag} -DMETIS5 ${cppmach} ${cpprel} /nologo /Od /debug:all /Zi /Qftz /Qopenmp /traceback ${zlib_inc} ${md5_inc}")
  set(debug_link    "/DEBUG:FULL")

elseif (debug STREQUAL "chkb")

  set(fortran_flags "/nologo ${precision_flag} -DMETIS5 ${cppmach} ${cpprel} -DCPP_comp=f90 ${FPP_FLAG} /Od /debug:all /Zi /fp:precise /Qfma- /Qftz /extend-source /assume:buffered_io /Qopenmp ${ADF} /check:bounds /check:uninit /traceback")
  set(c_flags       "${precision_flag} -DMETIS5 ${cppmach} ${cpprel} /nologo /Od /debug:all /Zi /Qftz /Qopenmp /traceback ${zlib_inc} ${md5_inc}")
  set(cpp_flags     "${precision_flag} -DMETIS5 ${cppmach} ${cpprel} /nologo /Od /debug:all /Zi /Qftz /Qopenmp /traceback ${zlib_inc} ${md5_inc}")
  set(debug_link    "/DEBUG:FULL")

else()

  set(fortran_flags "${precision_flag} -DMETIS5 ${cppmach} ${cpprel} -DCPP_comp=f90 ${VECT_OPT} ${Fortran_base} ${ADF}")
  set(c_flags       "${precision_flag} ${Tet_mesher_inc} -DMETIS5 ${cppmach} ${cpprel} ${VECT_OPT} ${zlib_inc} ${md5_inc}")
  set(cpp_flags     "${precision_flag} ${Tet_mesher_inc} -DMETIS5 ${cppmach} ${cpprel} ${VECT_OPT} ${zlib_inc} ${md5_inc}")

endif()

set_source_files_properties(${source_files}     PROPERTIES COMPILE_FLAGS "${fortran_flags}")
set_source_files_properties(${c_source_files}   PROPERTIES COMPILE_FLAGS "${c_flags}")
set_source_files_properties(${cpp_source_files} PROPERTIES COMPILE_FLAGS "${cpp_flags}")
