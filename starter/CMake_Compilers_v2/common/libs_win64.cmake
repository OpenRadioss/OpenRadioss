## Third-party library paths for win64 (Windows x86-64).
## Sets: zlib_inc, zlib_lib, md5_inc, md5_lib
set(zlib_inc "-I${source_directory}/../extlib/zlib/win64_mt/include")
set(zlib_lib "${source_directory}/../extlib/zlib/win64_mt/zlib1.lib")
set(md5_inc  "-I${source_directory}/../extlib/md5/include")
set(md5_lib  "${source_directory}/../extlib/md5/win64/md5.lib")
