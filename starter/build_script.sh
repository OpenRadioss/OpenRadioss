#!/bin/bash

function my_help()
{
  echo " " 
  echo " build_script"
  echo " ------------"
  echo " " 
  echo " Use with arguments : "
  echo " -arch=[build architecture]"
  if  [ -f CMake_Compilers/platforms.txt ]
  then
       cat CMake_Compilers/platforms.txt 
  fi
  if  [ -f CMake_Compilers_c/platforms.txt ]
  then
       cat CMake_Compilers_c/platforms.txt 
  fi
  echo " -prec=[dp|sp]                       : set precision - dp (default) |sp "
  echo " -static-link                        : Fortran, C & C++ runtime are linked in binary"
  echo " -debug=[0|1|asan]                   : debug version for gfortran"
  echo "                                          0 : no debug flags (default)"
  echo "                                          1 : usual debug flag"
  echo "                                          asan : gfortran address sanitizer"
  echo " -open_reader                        : link with open_reader"
  echo " -release                            : Set build for release (optimized)"
  echo " "
  echo " -addflag=\"list of additional flags\" : add compiler flags to usual set"
  echo " "
  echo " Execution control "
  echo " -nt=[threads]      : number of threads for build "
  echo " -verbose           : Verbose build"
  echo " -clean             : clean build directory"
  echo " " 
  echo " -no-python : do not link with python"
  echo " "
  echo " -cmake_ver=legacy : use legacy CMake_Compilers/ build system (default: v2)"
  echo " " 
}

# Variable initialization
# -----------------------
OTHER_MAKE_ARGS=" " 
# -----------------------------
# Parse command line arguments
# -----------------------------
arch=none
prec=dp
threads=1
got_arch=0
debug=0
ddebug=""
sanitize=0
jenkins_release=0
no_rr_clean=0
no_python=0
changelist=00000
cf=""
dc=""
qd=""
ADF=""
static_link=0
cmake_ver=v2
dcmake_ver="-Dcmake_ver=v2"
number_of_arguments=$#
clean=0
verbose=""
st_vers="starter"
com=0
release=0
ad=none
use_openreader=0
orb=""

if [ "$(uname -m)" = "x86_64" ]
then
  built_in_arch=linux64
  default_arch=linux64_gf
else
  built_in_arch=linuxa64
  default_arch=linuxa64_gf
fi


if [ $number_of_arguments = 0 ]
then
  echo "No arguments — building with default: -arch=${default_arch} -cmake_ver=v2"
  echo "Run './build_script.sh -help' for all options."
fi

   for var in "$@"
   do
       arg=`echo $var|awk -F '=' '{print $1}'`

       if [ "$arg" == "-help" ] || [ "$var" == "--help" ] || [ "$var" == "-h" ]
       then
         my_help
         exit 0
       fi

       if [ "$arg" == "-arch" ]
       then
         arch=`echo $var|awk -F '=' '{print $2}'`
         got_arch=1
       fi

       if [ "$arg" == "-prec" ]
       then
         prec=`echo $var|awk -F '=' '{print $2}'`
         if [ ${prec} = 'sp' ]
         then
           suffix=_sp
         fi
       fi

       if [ "$arg" == "-addflag" ]
       then
         ad=`echo $var|awk -F '-addflag=' '{ print $2}'`
         export ADFL=${ad}
       fi

       if [ "$arg" == "-debug" ]
       then
         debug=`echo $var|awk -F '=' '{print $2}'`
         ddebug=_${debug}

         if [ $debug == 0 ]
         then
           ddebug=""
         fi

         if [ $debug == 1 ]
         then
           ddebug="_db"
         fi
         if [ $debug == 2 ]
         then
           debug=1
           sanitize=1
           ddebug="_db2"
         fi 
       fi

       if [ "$arg" == "-nt" ]
       then
         threads=`echo $var|awk -F '=' '{print $2}'`
       fi

       if [ "$arg" == "-open_reader" ]
       then
         use_openreader=1
       fi

       if [ "$arg" == "-static-link" ]
       then
         static_link=1
       fi

       if [ "$arg" == "-no-python" ]
       then
         no_python=1
       fi

       if [ "$arg" == "-cmake_ver" ]
       then
         cmake_ver=`echo $var|awk -F '=' '{print $2}'`
         if [ "$cmake_ver" = "legacy" ]; then
           dcmake_ver=""
         else
           dcmake_ver="-Dcmake_ver=${cmake_ver}"
         fi
       fi

       if [ "$arg" == "-release" ]
       then
         release=1
       fi

       if [ "$arg" == "-verbose" ]
       then
         verbose="VERBOSE=1"
       fi

       if [ "$arg" == "-clean" ]
       then
         clean=1
       fi

       if [ "$arg" == "-c" ]
       then
         com=1
         dc="-DCOM=1"
         cf="_c"
         orb="-c"
         vers=`grep version CMake_Compilers_c/cmake_st_version.txt | awk -F '\"' '{print $2}' `
         st_vers="s_${vers}"
       fi

   done

   if [ $got_arch == 0 ]
   then
     arch=${default_arch}
   fi

   if [ $release == 1 ]
   then
     debug=0
     ddebug=""
   fi 

starter_exec=${st_vers}_${arch}${dmpi}${suffix}${ddebug}
if [ "$cmake_ver" = "legacy" ]; then
  build_directory=cbuild_${starter_exec}${cf}
else
  build_directory=cbuild${cmake_ver}_${starter_exec}${cf}
fi

   echo " " 
   echo " Build OpenRadioss Starter "
   echo " --------------------------"
   echo " Build Arguments :"
   echo " arch =                 : " $arch
   echo " precision =            : " $prec
   echo " debug =                : " $debug
   echo " static_link =          : " $static_link
   echo " cmake_ver =            : " $cmake_ver
   if [ $use_openreader == 1 ]
   then
       echo " "
       echo " linking with open_reader"
   fi

   echo " " 
   echo " Executable name        : " ${starter_exec}
   if [ "$ad" != "none" ]  
   then
      echo " Addflag                : \"$ad\" "
   fi
   echo " "
   echo " #threads for Makefile : " $threads
   echo " "

if [ $clean = 1 ]
then
   if [ -d ${build_directory} ]
   then
     echo "Clean ${build_directory} directory"
     rm -rf ./${build_directory}
   else
     echo "Clean ${build_directory} directory requested but not found"
   fi
   echo " " 
   exit 0
fi

#
# OpenReader if -open_reader was set 
# Build open_reader 
#
if [ $use_openreader == 1 ]
then
    echo " "
    echo "Build open_reader: ${built_in_arch} "
    echo "----------------"
    cd ../reader
    ./build_script.bash -arch=${built_in_arch} -nt=${threads} ${orb}
    return_value=$?
    if [ $return_value -ne 0 ]
    then
       echo " " 
       echo " " 
       echo "-- Errors in Build found"
       cd ..
       exit 1
    fi
    cd ../starter
fi

# create build directory
if [ ! -d ../exec ] 
then
   mkdir ../exec
fi

# create build directory
if [ ! -d ${build_directory} ] 
then
   mkdir ${build_directory}
fi


if [ -f ${build_directory}/${starter_exec} ]
then
  echo " -- Remove executable in build_script "
  rm ${build_directory}/${starter_exec}
fi

if [ -f ../exec/${starter_exec} ]
then
  echo " -- Remove executable in exec "
 
  rm ../exec/${starter_exec}
fi
echo " "

cd ${build_directory}

# Get compiler settings
if [ $com = 1 ]
then
    if [ -f ../CMake_Compilers_c/cmake_${arch}_compilers.sh ]
    then
      source ../CMake_Compilers_c/cmake_${arch}_compilers.sh
    else
      echo "-- Error: -arch=${arch} does not exist in CMake_Compilers_c/"
      cd .. && exit 1
    fi
elif [ "$cmake_ver" = "legacy" ]
then
    if [ -f ../CMake_Compilers/cmake_${arch}_compilers.sh ]
    then
      source ../CMake_Compilers/cmake_${arch}_compilers.sh
    else
      echo "-- Error: -arch=${arch} does not exist in CMake_Compilers/"
      cd .. && exit 1
    fi
else
    # v2 mode: embedded compiler lookup (no external files needed)
    case "$arch" in
      linux64_gf)        Fortran_comp=gfortran;  C_comp=gcc;       CXX_comp=g++         ;;
      linux64_ifort)     Fortran_comp=ifort;     C_comp=icc;       CXX_comp=icpc        ;;
      linux64_ifx)       Fortran_comp=ifx;       C_comp=icx;       CXX_comp=icpx        ;;
      linux64_AOCC)      Fortran_comp=flang;     C_comp=clang;     CXX_comp=clang++     ;;
      linux64_nv)        Fortran_comp=nvfortran; C_comp=nvc;       CXX_comp=nvc++       ;;
      linuxa64|linuxa64_armflang) Fortran_comp=armflang; C_comp=armclang; CXX_comp=armclang++ ;;
      linuxa64_gf)       Fortran_comp=gfortran;  C_comp=gcc;       CXX_comp=g++         ;;
      win64|win64_ifx|win64_sse3) Fortran_comp=ifx; C_comp=icx;   CXX_comp=icpx        ;;
      win64_ifort)       Fortran_comp=ifort;     C_comp=icl;       CXX_comp=icl         ;;
      *)
        echo "-- Error: Unknown arch '${arch}' for v2 build."
        echo "-- Available architectures:"
        cat ../CMake_Compilers_v2/platforms.txt
        cd .. && exit 1
        ;;
    esac
    CPP_comp=$CXX_comp
fi

Fortran_path=`which $Fortran_comp 2>/dev/null`
C_path=`which $C_comp 2>/dev/null`
CPP_path=`which $CPP_comp 2>/dev/null`
CXX_path=`which $CXX_comp 2>/dev/null`

# Validate compilers are found
if [ -z "$Fortran_path" ]; then
  echo "-- Error: Fortran compiler '$Fortran_comp' not found in PATH"
  cd .. && exit 1
fi
if [ -z "$C_path" ]; then
  echo "-- Error: C compiler '$C_comp' not found in PATH"
  cd .. && exit 1
fi
if [ -z "$CXX_path" ]; then
  echo "-- Error: C++ compiler '$CXX_comp' not found in PATH"
  cd .. && exit 1
fi


# Apply cmake

if [ ${arch} = "win64" ]
then
  Fortran_path_w=`cygpath.exe -m "${Fortran_path}"`
  C_path_w=`cygpath.exe -m "${C_path}"`
  CPP_path_w=`cygpath.exe -m "${CPP_path}"`
  CXX_path_w=`cygpath.exe -m "${CXX_path}"`
  cmake.exe -G "Unix Makefiles" -Darch=${arch} -Dprecision=${prec} ${DAD} -Ddebug=${debug} -DEXEC_NAME=${starter_exec} ${dc} -Dno_python=${no_python} -Dstatic_link=$static_link ${dcmake_ver} -DCMAKE_BUILD_TYPE=Release -DCMAKE_Fortran_COMPILER="${Fortran_path_w}" -DCMAKE_C_COMPILER="${C_path_w}" -DCMAKE_CPP_COMPILER="${CPP_path_w}" -DCMAKE_CXX_COMPILER="${CXX_path_w}" .. 
else
  cmake -Darch=${arch} -Dprecision=${prec} ${DAD} -Ddebug=${debug} -DEXEC_NAME=${starter_exec} -Dstatic_link=$static_link -Dno_python=${no_python} ${dc} -Dsanitize=${sanitize} ${dcmake_ver} -DCMAKE_Fortran_COMPILER=${Fortran_path} -DCMAKE_C_COMPILER=${C_path} -DCMAKE_CPP_COMPILER=${CPP_path} -DCMAKE_CXX_COMPILER=${CXX_path} -DUSE_OPEN_READER=${use_openreader} ..
fi

return_value=$?
if [ $return_value -ne 0 ]
then
   echo " " 
   echo " " 
   echo "-- Errors in Cmake found"
   cd ..
   if [ -d ${build_directory} ]
   then
     echo "-- Cleaning ${build_directory} directory"
     rm -rf ./${build_directory}
   fi
   echo " " 
   exit 1
fi

make -j ${threads} ${verbose}
#ninja -v -j ${threads} -d explain
return_value=$?

if [ $debug == 'asan' ]
then
    echo " "
    echo "Warning:"
    echo "--------"
    echo "Build was made with debug configuration."
    echo "To enable optimization, add -release flag."
    echo " "
fi

if [ $debug == 'analysis' ]
then
if [ $return_value -eq 0 ]
then
    pwd
    cd ../../scripts
    python3 ./static_analysis.py starter
    return_value=$?
fi
fi



if [ $return_value -ne 0 ]
then
   echo " " 
   echo " " 
   echo "-- Errors in Build found"
   cd ..
   exit 1
fi

cd ..
echo " "

