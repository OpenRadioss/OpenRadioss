#!/bin/bash

function my_help()
{
  echo " " 
  echo " test_script"
  echo " ------------"
  echo " Run Test suite and verify the results"
  echo " " 
  echo " Use with arguments : "
  echo " -help              : print this help & exit"
  echo " "
  echo " -type=[default,pon] : test type"
  echo "                       -type=default : test suite with numerical results verification"
  echo "                       -type=pon     : check parallel arithmetic"
  echo " -pon_run="MPIxThreds,..." : komma separated list of #mpix#threads"
  echo " " 
  echo " -arch=arch         : Set the executable architecture. "
  echo "                      -arch=built_in (Default) : "
  echo "                               linux64_gf for linux"
  echo "                               win64 for windows "
  echo "                               linuxa64 for Linux/Arm"
  echo " "
  echo " -mpi=[smp|impi|ompi]"
  echo "         -mpi=smp  : Engine is SMP only executable (default)"
  echo "         -mpi=ompi : engine is using OpenMPI"
  echo "         -mpi=ompi : engine is using Intel MPI"
  echo " " 
  echo "-debug=[0,1,asan]   : Debug flag for executable : 0 (default) 1 debug (_db),asan (gfortran address sanitizer)"
  echo " "
  echo " -np=#MPI Domains    : Set # MPI Domains thest will run through"
  echo " -nt=#Threads        : Set # Threads"
  echo " -prec=[dp|sp]       : set executable precision - dp (default) |sp "
  echo " -stdout             : print Test output"
  echo " -tests=\"Test list\"  : Run specific tests"
  echo "                       use CTest regular expression form"
  echo "                       -tests=\"test1|test2|...\"       "
  echo " " 
  echo " -keep_results       : Keep computation results"
  echo " -clean              : Clean execution directory"
  echo " " 
}

arch=built_in
mpi=smp
prec=dp
np=1
nt=1
stdout="-DSTDOUT=0"
keep_results=0
tests=
clean=0
debug=0
ddebug=optimized
verbose=''
qa_type=default
pon_run="4x1,1x4"


for var in "$@"
do
    arg=`echo $var|awk -F '=' '{print $1}'`

    if [ "$arg" == "-help" ]
    then
         my_help
         exit 0
    fi

    if [ "$arg" == "-type" ]
    then
         qa_type=`echo $var|awk -F '=' '{print $2}'`
    fi

    if [ "$arg" == "-pon_run" ]
    then
         pon_run=`echo $var|awk -F '=' '{print $2}'`
         qa_type=pon
    fi

    if [ "$arg" == "-arch" ]
    then
         arch=`echo $var|awk -F '=' '{print $2}'`
    fi

    if [ "$arg" == "-mpi" ]
    then
         mpi=`echo $var|awk -F '=' '{print $2}'`
    fi

    if [ "$arg" == "-np" ]
    then
        np=`echo $var|awk -F '=' '{print $2}'`
    fi

    if [ "$arg" == "-nt" ]
    then
        nt=`echo $var|awk -F '=' '{print $2}'`
    fi

    if [ "$arg" == "-debug" ]
    then
        ddebug=`echo $var|awk -F '=' '{print $2}'`
    fi

    if [ "$arg" == "-prec" ]
    then
        prec=`echo $var|awk -F '=' '{print $2}'`
    fi

    if [ "$arg" == "-tests" ]
    then
        tlist=`echo $var|awk -F '=' '{print $2}'`
        tests="-R $tlist"
    fi

    if [ "$arg" == "-stdout" ]
    then
        stdout="-DSTDOUT=1"
        verbose="--verbose"
    fi

    if [ "$arg" == "-keep_results" ]
    then
        keep_results=1
    fi

    if [ "$arg" == "-clean" ]
    then
        clean=1
    fi

   done

# As this is a bash script it is intended to be executed on Linux Plarforms.
test_directory=ctest_suite_linux

echo " " 
echo " test_script"
echo " ------------"
echo " " 

# Clean & Exit
if [ $clean = 1 ]
then
   echo "Clean ${test_directory}"
   echo " "    
   if [ -d ${test_directory} ]
   then
     rm -rf $test_directory
   fi
   exit 0
fi

# create build directory & enter
if [ ! -d ${test_directory} ] 
then
   mkdir ${test_directory}
fi
cd ${test_directory}


# MPI=smp,impi,ompi : depending on the flavors

#rem cmake -DMPI=impi -DNP=4 ..

cmake -Darch=$arch -DPREC=$prec -DMPI=$mpi -DNP=$np -DNT=$nt $stdout -DKEEP=$keep_results -DDEBUG=$ddebug -Dtype=$qa_type -Dpon_run=$pon_run  ..
echo " " 
ctest -C Release --output-on-failure --timeout 600 $tests $verbose

cd ..
