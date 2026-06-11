## Print the Fortran module output directory.
## The calling platform file must set CMAKE_Fortran_MODULE_DIRECTORY
## and CMAKE_Fortran_MODDIR_FLAG before including this snippet.
message(STATUS "modules: ${CMAKE_Fortran_MODULE_DIRECTORY}")
