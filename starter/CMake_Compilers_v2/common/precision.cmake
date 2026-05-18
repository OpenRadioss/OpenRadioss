## Set precision preprocessor flag.
## Caller must define the `precision` variable (set by build_script.sh via -precision=sp).
if (precision STREQUAL "sp")
  set(precision_flag "-DMYREAL4")
else()
  set(precision_flag "-DMYREAL8")
endif()
