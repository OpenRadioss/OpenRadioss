## Intel Windows compilation flags.
## Required variables set by caller:
##   FPP_FLAG        /fpp or /nofpp (Ninja workaround)
##   VECT_OPT        vectorisation flags (differs between win64 and win64_ifort/sse3)
##   opt_flag        Fortran optimizer flags (includes wo_linalg, mumps)
##   optc_flag       C optimizer flags
##   precision_flag, cppmach, cpprel, h3d_inc, zlib_inc, md5_inc, mpi_flag, mumps_flag, ADF

set(Vect_precise "/Qopenmp /fp:precise /Qfma- /Qftz")
set(Fortran_base "${FPP_FLAG} /extend-source /assume:buffered_io")

if (debug STREQUAL "1")
  set(fortran_flags "/nologo /Qopenmp ${opt_flag} /Od /debug:all /traceback /Zi /Qftz ${Fortran_base} ${precision_flag} ${mpi_flag} ${mumps_flag} -DMETIS5 ${cppmach} ${cpprel} ${ADF}")
  set(c_flags       "/nologo /Qopenmp ${optc_flag} /Od /debug:all /Zi /Qftz ${precision_flag} -DMETIS5 ${h3d_inc} ${md5_inc} ${mpi_flag} ${mumps_flag} ${cppmach} ${cpprel} ${zlib_inc}")
  set(cpp_flags     "/nologo /Qopenmp ${optc_flag} /Od /debug:all /Zi /Qftz ${precision_flag} -DMETIS5 ${h3d_inc} ${md5_inc} ${mpi_flag} ${mumps_flag} ${cppmach} ${cpprel} ${zlib_inc} /Qstd=c++11")
elseif (debug STREQUAL "chkb")
  set(fortran_flags "/nologo /Qopenmp ${opt_flag} /Od /debug:all /traceback /Zi /Qftz ${Fortran_base} ${precision_flag} ${mpi_flag} ${mumps_flag} -DMETIS5 ${cppmach} ${cpprel} /check:bounds /check:uninit ${ADF}")
  set(c_flags       "/nologo /Qopenmp ${optc_flag} /Od /debug:all /Qftz /Zi ${precision_flag} -DMETIS5 ${h3d_inc} ${md5_inc} ${mpi_flag} ${mumps_flag} ${cppmach} ${cpprel} ${zlib_inc} /check:bounds /check:uninit")
  set(cpp_flags     "/nologo /Qopenmp ${optc_flag} /Od /debug:all /Qftz /Zi ${precision_flag} -DMETIS5 ${h3d_inc} ${md5_inc} ${mpi_flag} ${mumps_flag} ${cppmach} ${cpprel} ${zlib_inc} /Qstd=c++11 /check:bounds /check:uninit")
else()
  set(fortran_flags "/nologo /O3 ${opt_flag} ${VECT_OPT} ${precision_flag} ${Vect_precise} ${Fortran_base} ${mpi_flag} ${h3d_inc} ${mumps_flag} -DMETIS5 ${cppmach} ${cpprel} ${ADF}")
  set(c_flags       "/nologo /O2 ${optc_flag} ${precision_flag} ${Vect_precise} ${mpi_flag} ${h3d_inc} ${md5_inc} -DMETIS5 ${mumps_flag} ${cppmach} ${cpprel} ${zlib_inc}")
  set(cpp_flags     "/nologo /O2 ${optc_flag} ${precision_flag} ${Vect_precise} ${mpi_flag} ${h3d_inc} ${md5_inc} -DMETIS5 ${mumps_flag} ${cppmach} ${cpprel} ${zlib_inc}")
endif()

set_source_files_properties(${source_files}     PROPERTIES COMPILE_FLAGS "${fortran_flags}")
set_source_files_properties(${c_source_files}   PROPERTIES COMPILE_FLAGS "${c_flags}")
set_source_files_properties(${cpp_source_files} PROPERTIES COMPILE_FLAGS "${cpp_flags}")
