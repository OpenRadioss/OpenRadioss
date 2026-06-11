## Intel MKL discovery for Linux Intel compilers (ifort, ifx).
## Sets: MKL_IDIR, MKL_LDIR, MKL_lib
if (DEFINED ENV{MKLROOT})
  set(MKL_IDIR "$ENV{MKLROOT}/include")
  set(MKL_LDIR "$ENV{MKLROOT}/lib/intel64")
else()
  set(MKL_IDIR "/opt/intel/oneapi/mkl/latest/include")
  set(MKL_LDIR "/opt/intel/oneapi/mkl/latest/lib/intel64")
endif()
set(MKL_lib "-L${MKL_LDIR} -I${MKL_IDIR} -Wl,--start-group ${MKL_LDIR}/libmkl_intel_lp64.a ${MKL_LDIR}/libmkl_intel_thread.a ${MKL_LDIR}/libmkl_core.a -Wl,--end-group")
