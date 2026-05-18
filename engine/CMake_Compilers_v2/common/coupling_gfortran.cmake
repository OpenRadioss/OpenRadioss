## Optional coupling library support (preCICE or CWIPI).
## Appends to opt_flag; sets coupling and CWIPI_FLAG for use in linker flags.
if (precice STREQUAL "1")
  set(opt_flag "${opt_flag} -DWITH_PRECICE")
  set(coupling "-DWITH_PRECICE -I/usr/include/precice/")
elseif (cwipi STREQUAL "1")
  set(coupling  "-DWITH_CWIPI -I${cwipi_path}/include/")
  set(CWIPI_FLAG "-Wl,-rpath,${cwipi_path}: -lm ${cwipi_path}/lib/libcwp.so.1.3.0 -lm -lstdc++ ")
endif()
