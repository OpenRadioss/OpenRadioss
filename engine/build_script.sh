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
  echo " "
  echo " MPI libraries"
  echo " -mpi=[mpi]"
  echo "         not set   : SMP (default)"
  echo "         -mpi=ompi : OpenMPI"
  if  [ -f CMake_Compilers_c/mpi.txt ]
  then
       cat CMake_Compilers_c/mpi.txt 
  fi
  echo " " 
  echo "         Controlling MPI Libraries - if need choose one of the 3 Option Set"
  echo "                                    If no options set, recommended OpenMPI directories are used (default)"
  echo "           1. -mpi-os                             : link with default MPI version installed on system"
  echo "                                                    libraries are in default installation "
  echo "           2. -mpi-root=[directory]               : set rootname to link with specific MPI installation"
  echo "           3. -mpi-include=[directory]            : set include directory where to find mpif.h and mpi.h"
  echo "              -mpi-libdir=[directory]             : set library directory where to find mpi libraries"  
  echo " "
  echo " -prec=[dp|sp]                       : set precision - dp (default) |sp "
  echo " -static-link                        : Fortran, C & C++ runtime are linked in binary"
  echo " -debug=[0|1|asan]                   : debug version for gfortran"
  echo "                                          0 : no debug flags (default)"
  echo "                                          1 : usual debug flag"
  echo "                                          asan : gfortran address sanitizer"
  echo " -release                            : Set build for release (optimized)"
  echo " "
  echo " -addflag=\"list of additional flags\" : add compiler flags to usual set"
  echo " "
  echo " Execution control "
  echo " -nt=[threads]      : number of threads for build "
  echo " -verbose           : Verbose build"
  echo " -clean             : clean build directory"
  echo " " 
  echo "  MUMPS linear solver: available only for dp, with mpi" 
  echo " -mumps_root=[path_to_mumps]          : path_to_mumps/lib/libdmumps.a must exist"
  echo " -scalapack_root=[path to scalapack]  : path_to_scalapack/libscalapack.a must exist" 
  echo " -lapack_root=[path to lapack]  : path_to_lapack/liblapack.a must exist" 
  echo " "
  echo " -no-python : do not link with python"
  echo " " 
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
MPI="-DMPI=smp"
pmpi="SMP Only"
clean=0
mpi_os=0
mpi_root=""
mpi_libdir=""
mpi_incdir=""
number_of_arguments=$#
verbose=""
eng_vers="engine"
mumps_root=""
scalapack_root=""
lapack_root=""
com=0
release=0
ad=none

if [ $number_of_arguments = 0 ]
then
  my_help
  exit 1
 
else

   for var in "$@"
   do
       arg=`echo $var|awk -F '=' '{print $1}'`

       if [ "$arg" == "-arch" ]
       then
         arch=`echo $var|awk -F '=' '{print $2}'`
         got_arch=1
       fi

       if [ "$arg" == "-mpi" ]
       then
         pmpi=`echo $var|awk -F '=' '{print $2}'`
         dmpi=_${pmpi}
         MPI=-DMPI=${pmpi}

         if [ "$pmpi" != "smp" ]
         then
            dmpi=_${pmpi}
            MPI=-DMPI=${pmpi}
         else 
            pmpi="SMP Only"
         fi

       fi

       if [ "$arg" == "-mpi-os" ]
       then
         mpi_os=1
       fi

       if [ "$arg" == "-mpi-root" ]
       then
        mpir=`echo $var|awk -F '=' '{print $2}'`
        mpi_root="-Dmpi_root=${mpir}"
       fi

       if [ "$arg" == "-mpi-include" ]
       then
        mpii=`echo $var|awk -F '=' '{print $2}'`
        mpi_incdir="-Dmpi_incdir=${mpii}"
       fi

       if [ "$arg" == "-mpi-libdir" ]
       then
        mpil=`echo $var|awk -F '=' '{print $2}'`
        mpi_libdir="-Dmpi_libdir=${mpil}"
       fi


       if [ "$arg" == "-prec" ]
       then
         prec=`echo $var|awk -F '=' '{print $2}'`
         if [ ${prec} = 'sp' ]
         then
           suffix=_sp
         fi
       fi

       if [ "$arg" == "-mumps_root" ]
       then
        mumps_root=`echo $var|awk -F '=' '{print $2}'`
        mumps_root="-Dmumps_root=${mumps_root}"
       fi
       if [ "$arg" == "-lapack_root" ]
       then
        lapack_root=`echo $var|awk -F '=' '{print $2}'`
        lapack_root="-Dlapack_root=${lapack_root}"
       fi
       if [ "$arg" == "-scalapack_root" ]
       then
        scalapack_root=`echo $var|awk -F '=' '{print $2}'`
        scalapack_root="-Dscalapack_root=${scalapack_root}"
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

       if [ "$arg" == "-static-link" ]
       then
         static_link=1
       fi

       if [ "$arg" == "-no-python" ]
       then
         no_python=1
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
         vers=`grep version CMake_Compilers_c/cmake_eng_version.txt | awk -F '\"' '{print $2}' `
         eng_vers="e_${vers}"
       fi

   done

   if [ $got_arch == 0 ] 
   then
     echo " " 
     echo " --- Error "
     echo " No architecture flag set ! "
     echo " -arch=[architecture]" 
     echo "       Available arch:"
     my_help
     exit 1
   fi

   if [ $release == 1 ]
   then
     debug=0
     ddebug=""
   fi 

engine_exec=${eng_vers}_${arch}${dmpi}${suffix}${ddebug}
build_directory=cbuild_${engine_exec}${cf}


   echo " " 
   echo " Build OpenRadioss Engine "
   echo " -------------------------"
   echo " Build Arguments :"
   echo " arch =                 : " $arch
   echo " mpi =                  : " $pmpi
   echo " mpi_os =               : " $mpi_os
   echo " precision =            : " $prec
   echo " debug =                : " $debug
   echo " static_link =          : " $static_link
   echo " " 
   echo " Executable name        : " ${engine_exec}
   if [ "$ad" != "none" ]  
   then
      echo " Addflag               : \"$ad\" "
   fi
   echo " "
   echo " #threads for Makefile : " $threads
   echo " "
fi

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


if [ -f ${build_directory}/${engine_exec} ]
then
  echo " -- Remove executable in build_script "
  rm ${build_directory}/${engine_exec}
fi

if [ -f ../exec/${engine_exec} ]
then
  echo " -- Remove executable in exec "
 
  rm ../exec/${engine_exec}
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
      echo "-- Error: -arch=${arch} does not exist"
      echo "-- See help bellow"
      echo " " 
      my_help
      exit 1
    fi
else
    if [ -f ../CMake_Compilers/cmake_${arch}_compilers.sh ]
    then
      source ../CMake_Compilers/cmake_${arch}_compilers.sh
    else
      echo "-- Error: -arch=${arch} does not exist"
      echo "-- See help bellow"
      echo " " 
      my_help
      exit 1
    fi
fi

Fortran_path=`which $Fortran_comp`
C_path=`which $C_comp`
CPP_path=`which $CPP_comp`
CXX_path=`which $CXX_comp`


# Apply cmake

if [ ${arch} = "win64" ]
then
  Fortran_path_w=`cygpath.exe -m "${Fortran_path}"`
  C_path_w=`cygpath.exe -m "${C_path}"`
  CPP_path_w=`cygpath.exe -m "${CPP_path}"`
  CXX_path_w=`cygpath.exe -m "${CXX_path}"`
  cmake.exe .. -G "Unix Makefiles" -Darch=${arch} -Dprecision=${prec} ${MPI} -Ddebug=${debug} -DEXEC_NAME=${engine_exec} -Dstatic_link=$static_link -Dmpi_os=${mpi_os} ${mpi_root} ${mpi_libdir} ${mpi_incdir} ${dc} ${mumps_root} ${scalapack_root} ${lapack_root} -DCMAKE_BUILD_TYPE=Release -Dno_python=${no_python}  -Dstatic_link=$static_link -DCMAKE_BUILD_TYPE=Release -DCMAKE_Fortran_COMPILER="${Fortran_path_w}" -DCMAKE_C_COMPILER="${C_path_w}" -DCMAKE_CPP_COMPILER="${CPP_path_w}" -DCMAKE_CXX_COMPILER="${CXX_path_w}" ${la}
else
  cmake .. -Darch=${arch} -Dprecision=${prec} ${MPI} -Ddebug=${debug} -DEXEC_NAME=${engine_exec} -Dstatic_link=$static_link -Dmpi_os=${mpi_os} -Dsanitize=${sanitize} ${mpi_root} ${mpi_libdir} ${mpi_incdir} ${dc} ${mumps_root} ${scalapack_root} ${lapack_root} -Dno_python=${no_python} -Dstatic_link=$static_link -DCMAKE_BUILD_TYPE=Release -DCMAKE_Fortran_COMPILER=${Fortran_path} -DCMAKE_C_COMPILER=${C_path} -DCMAKE_CPP_COMPILER=${CPP_path} -DCMAKE_CXX_COMPILER=${CXX_path}  ${la}

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

