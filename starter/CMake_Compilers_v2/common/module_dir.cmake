## Set Fortran module output directory.
## Caller must set CMAKE_Fortran_MODDIR_FLAG before including this file:
##   -J           (GFortran)
##   -module      (Intel, NVHPC, ARM Flang)
##   /module:     (Intel Windows)
##
## Caller may also pre-set CMAKE_Fortran_MODULE_DIRECTORY to override the
## default path (e.g. win64 uses CMakeFiles/{EXEC_NAME}/modules).
if (NOT DEFINED CMAKE_Fortran_MODULE_DIRECTORY)
  set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/CMakeFiles/modules)
endif()
message(STATUS "modules: ${CMAKE_Fortran_MODULE_DIRECTORY}")
