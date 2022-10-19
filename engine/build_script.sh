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
  echo "                                    If no options set, recommended OpenMPI directories are uses (default)"
  echo "           1. -mpi-os                             : link with default MPI version installed on system"
  echo "                                                    libraries are in default installation "
  echo "           2. -mpi-root=[directory]               : set rootname to link with specific MPI installation"
  echo "           3. -mpi-include=[directory]            : set include directory where to find mpif.h and mpi.h"
  echo "              -mpi-libdir=[directory]             : set library directory where to find mpi libraries"  
  echo " " 
  echo " -prec=[dp|sp]                        : set precision - dp (default) |sp "
  echo " -static-link                         : Fortran, C & C++ runtime are linked in binary"
  echo " -debug=[0|1]                         : debug version 0 no debug flags (default), 1 usual debug flag )"
  echo " -addflag=\"list of additional flags\" : add compiler flags to usual set"
  echo " -nt=[threads]      : number of threads for build "
  echo " -verbose           : Verbose build"
  echo " -clean             : clean build directory"
  echo "        MUMPS linear solver: available only for dp, with mpi" 
  echo " -mumps_root=[path_to_mumps]          : path_to_mumps/lib/libdmumps.a must exist"
  echo " -scalapack_root=[path to scalapack]  : path_to_scalapack/libscalapack.a must exist" 
  echo " -lapack_root=[path to lapack]  : path_to_lapack/liblapack.a must exist" 
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
sanitize=0
jenkins_release=0
no_rr_clean=0
changelist=00000
cf=""
dc=""
qd=""
ADF=""
MPI="-DMPI=smp"
pmpi="SMP Only"
debug=0
clean=0
mpi_os=0
mpi_root=""
mpi_libdir=""
mpi_incdir=""
ddebug=""
number_of_arguments=$#
eng_vers="engine"
verbose=""
mumps_root=""
scalapack_root=""
lapack_root=""

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
        scalapack_root="-Dsclapack_root=${scalapack_root}"
       fi

       if [ "$arg" == "-addflag" ]
       then
         ad=`echo $var|awk -F '-addflag=' '{ print $2}'`
         export ADFL=${ad}
       fi


       if [ "$arg" == "-debug" ]
       then
         debug=`echo $var|awk -F '=' '{print $2}'`
         if [ $debug == 2 ]
         then
           debug=1
           sanitize=1
         fi 
         if [ $debug == 1 ]
         then
           ddebug="_db"
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
         dc="-DCOM=1"
         cf="_c"
         vers=`cat CMake_Compilers_c/cmake_eng_version.txt | awk -F '\"' '{print $2}' `
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
   if [[ -v ad ]]  
   then
      echo " Addflag               : \""$ad "\" "
   fi
   echo " "
   echo " #threads for Makefile : " $threads
   echo " "
fi


build_directory=cbuild_${arch}${dmpi}${suffix}${cf}${ddebug}

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

engine_exec=${eng_vers}_${arch}${dmpi}${suffix}${ddebug}
echo " " 

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

# Apply cmake

if [ ${arch} = "win64" ]
then
  cmake.exe -G "Unix Makefiles"  -Darch=${arch} -Dprecision=${prec} ${MPI} -Ddebug=${debug} -Dstatic_link=$static_link -Dmpi_os=${mpi_os} ${mpi_root} ${mpi_libdir} ${mpi_incdir} ${dc} ${mumps_root} ${scalapack_root} ${lapack_root} -DCMAKE_BUILD_TYPE=Release .. 
else
  cmake -Darch=${arch} -Dprecision=${prec} ${MPI} -Ddebug=${debug} -Dstatic_link=$static_link -Dmpi_os=${mpi_os} -Dsanitize=${sanitize} ${mpi_root} ${mpi_libdir} ${mpi_incdir} ${dc} ${mumps_root} ${scalapack_root} ${lapack_root} .. 
fi

make -j ${threads} ${verbose}

echo " "
if [ -f ${engine_exec} ]
then
  echo " -- Copy ${engine_exec} in ../exec "
  cp  ${engine_exec} ../../exec
fi

cd ..
echo " "

