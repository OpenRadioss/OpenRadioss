## Disable Python embedding when the build is configured with no_python=1.
## Appends -DPYTHON_DISABLED to the existing compile flags of cpp_python_funct.cpp.
if (no_python STREQUAL "1")
  get_source_file_property(existing_flags
    ${source_directory}/../common_source/modules/cpp_python_funct.cpp COMPILE_FLAGS)
  set_source_files_properties(
    ${source_directory}/../common_source/modules/cpp_python_funct.cpp
    PROPERTIES COMPILE_FLAGS "${existing_flags} -DPYTHON_DISABLED")
endif()
