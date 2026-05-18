## Third-party library paths for linuxa64 (AArch64 / ARM Linux).
## Sets: zlib_inc, zlib_lib, md5_inc, md5_lib
set(zlib_inc "-I${source_directory}/../extlib/zlib/linuxa64/include")
set(zlib_lib "${source_directory}/../extlib/zlib/linuxa64/lib/libz.a")
set(md5_inc  "-I${source_directory}/../extlib/md5/include")
set(md5_lib  "-L${source_directory}/../extlib/md5/linuxa64/ -lmd5")
