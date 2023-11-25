# CTEST For OpenRadioss test suite
# ---------------------------------
# ctest -C Release --output-on-failure --timeout 600
# set minimum cmake version

cmake_minimum_required(VERSION 2.7)

# project name and language
project(OpenRadioss_test_suite LANGUAGES NONE)

# enable testing functionality
enable_testing()

# detect python
# --------------
find_package(Python REQUIRED)



message(" ")
message(" OpenRadioss Test Suite ") 
message("------------------------")
message(" ")
message(" arch=        : ${arch}")
message(" Precision    : ${PREC}")
message(" Debug        : ${DEBUG}")
message(" mpi          : ${MPI}")
message(" #mpi domains : ${NP}")
message(" Keep_results : ${KEEP}")
message(" ")
if (STDOUT STREQUAL 1)
   message(" Print output files in Stdout")
endif()




set (Test_script "${CMAKE_CURRENT_SOURCE_DIR}/qa_tools/or_execute.py")

include (qa_tools/miniqa.txt)

