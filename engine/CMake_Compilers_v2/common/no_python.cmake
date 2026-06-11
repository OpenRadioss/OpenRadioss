## Disable Python bindings when no_python=1.
if (no_python STREQUAL "1")
  get_source_file_property(existing_flags
    ${source_directory}/../common_source/modules/cpp_python_funct.cpp COMPILE_FLAGS)
  set_source_files_properties(
    ${source_directory}/../common_source/modules/cpp_python_funct.cpp
    PROPERTIES COMPILE_FLAGS "${existing_flags} -DPYTHON_DISABLED")
endif()
