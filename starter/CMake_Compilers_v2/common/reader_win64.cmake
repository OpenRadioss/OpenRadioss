## HM reader / open_reader library selection for win64.
## Sets: reader_lib
if (USE_OPEN_READER STREQUAL 1)
  set(reader_lib "${source_directory}/../exec/open_reader_win64.lib")
else()
  set(reader_lib "${source_directory}/../extlib/hm_reader/win64/hm_reader_win64.lib")
endif()
