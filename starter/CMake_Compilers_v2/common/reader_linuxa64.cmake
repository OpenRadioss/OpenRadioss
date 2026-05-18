## HM reader / open_reader library selection for linuxa64 (AArch64 / ARM Linux).
## Sets: reader_lib
if (USE_OPEN_READER STREQUAL 1)
  set(reader_lib "-L${source_directory}/../exec -lopen_reader_linuxa64")
else()
  set(reader_lib "-L${source_directory}/../extlib/hm_reader/linuxa64/ -lhm_reader_linuxa64 -lapr-1")
endif()
