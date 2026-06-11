## Third-party library paths for linuxa64 (AArch64 Linux).
## Sets: h3d_inc, zlib_inc, zlib_lib, md5_inc, md5_lib
set(h3d_inc  "-I${source_directory}/../extlib/h3d/includes")
set(zlib_inc "-I${source_directory}/../extlib/zlib/linuxa64/include")
set(zlib_lib "${source_directory}/../extlib/zlib/linuxa64/lib/libz.a")
set(md5_inc  "-I${source_directory}/../extlib/md5/include")
set(md5_lib  "${source_directory}/../extlib/md5/linuxa64/libmd5.a")
