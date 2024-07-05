# CTEST For OpenRadioss test suite
# ---------------------------------
# ctest -C Release --output-on-failure --timeout 600
# set minimum cmake version

cmake_minimum_required(VERSION 3.5)

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
message(" QA Type=     : ${type}")
message(" arch=        : ${arch}")
message(" Precision    : ${PREC}")
message(" Debug        : ${DEBUG}")
message(" mpi          : ${MPI}")
message(" #mpi domains : ${NP}")
message(" #threads     : ${NT}")
message(" Keep_results : ${KEEP}")
message(" ")
if (STDOUT STREQUAL 1)
   message(" Print output files in Stdout")
endif()

if ( NOT ${DEBUG} STREQUAL "optimized")
  set (debug_option "-debug;${DEBUG}")
endif()


set (Test_script "${CMAKE_CURRENT_SOURCE_DIR}/qa_tools/or_execute.py")


set (arguments -arch;${arch};-mpi;${MPI};-np;${NP};-nt;${NT};-stdout;${STDOUT};-prec;${PREC};-keep;${KEEP};-qa_type;${type};-pon_string;${pon_run};${debug_option})

include (qa_tools/miniqa.txt)

