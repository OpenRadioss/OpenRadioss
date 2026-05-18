## Single / double precision selector.
if (precision STREQUAL "sp")
  set(precision_flag "-DMYREAL4")
else()
  set(precision_flag "-DMYREAL8")
endif()
