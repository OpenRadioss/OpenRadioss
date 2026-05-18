## Reset all CMake compiler flag variables including the base (non-variant) ones.
## Used by: Intel Linux and Intel Windows compilers.
set(CMAKE_Fortran_FLAGS " ")
set(CMAKE_C_FLAGS       " ")
set(CMAKE_CPP_FLAGS     " ")
set(CMAKE_CXX_FLAGS     " ")
include(${CMAKE_CURRENT_LIST_DIR}/flags_reset_minimal.cmake)
set(CMAKE_CXX_FLAGS_DEBUG   " ")
set(CMAKE_CXX_FLAGS_RELEASE " ")
