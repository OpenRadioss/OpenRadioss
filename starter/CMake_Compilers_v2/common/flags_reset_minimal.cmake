## Reset CMake default DEBUG/RELEASE flag variables so that only the flags set
## explicitly in the platform file are used (no hidden CMake defaults).
## Used by: GFortran, AOCC, NVHPC (compilers that do not need the base-flag reset).
set(CMAKE_Fortran_FLAGS_DEBUG   " ")
set(CMAKE_Fortran_FLAGS_RELEASE " ")
set(CMAKE_C_FLAGS_DEBUG         " ")
set(CMAKE_C_FLAGS_RELEASE       " ")
set(CMAKE_CPP_FLAGS_DEBUG       " ")
set(CMAKE_CPP_FLAGS_RELEASE     " ")
