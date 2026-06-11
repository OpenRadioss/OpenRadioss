## HM reader / open_reader library selection for linux64.
## Sets: reader_lib
if (USE_OPEN_READER STREQUAL 1)
  set(reader_lib "-L${source_directory}/../exec -lopen_reader_linux64")
else()
  set(reader_lib "-L${source_directory}/../extlib/hm_reader/linux64/ -lhm_reader_linux64 -lapr-1")
endif()
