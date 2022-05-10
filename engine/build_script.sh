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
  echo " -mpi=[mpi]"
  echo "         not set   : SMP (default)"
  echo "         -mpi=ompi : OpenMPI"
  if  [ -f CMake_Compilers_c/mpi.txt ]
  then
       cat CMake_Compilers_c/mpi.txt 
  fi
  echo " -prec=[dp|sp]                        : set precision - dp (default) |sp "
  echo " -debug=[0|1]                         : debug version 0 no debug flags (default), 1 usual debug flag )"
  echo " -addflag=\"list of additionnal flags\" : add compiler flags to usual set"
  echo " " 
  echo " Execution control "
  echo " -nt=[threads]      : number of threads for build "
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
ddebug=""
number_of_arguments=$#
eng_vers="engine"

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
         ad=`echo $var|awk -F '=' '{print $2}'`
         DAD="-DADF=\" ${ad} \" "
         echo $DAD
       fi

       if [ "$arg" == "-debug" ]
       then
         debug=`echo $var|awk -F '=' '{print $2}'`
         if [ $debug == 1 ]
         then
           ddebug="_db"
         fi
       fi

       if [ "$arg" == "-nt" ]
       then
         threads=`echo $var|awk -F '=' '{print $2}'`
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
     echo "       Available archh:"
     my_help
     exit 1
   fi


   echo " " 
   echo " Build OpenRadioss Engine "
   echo " -------------------------"
   echo " Build Arguments :"
   echo " arch =                 : " $arch
   echo " mpi =                  : " $pmpi
   echo " precision =            : " $prec
   echo " debug =                : " $debug
   if [[ -v ad ]]  
   then
      echo " Addflag               : \""$ad "\" "
   fi
   echo " "
   echo " #threads for Makefile : " $threads
   echo " "
fi




build_directory=cbuild_${arch}${dmpi}${suffix}${cf}${ddebug}

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
cmake -Darch=${arch} -Dprecision=${prec} ${MPI} ${AD} -Ddebug=${debug} ${dc} .. 
make -j ${threads} 

echo " "
if [ -f ${engine_exec} ]
then
  echo " -- Copy ${engine_exec} in ../exec "
  cp  ${engine_exec} ../../exec
fi

cd ..
echo " "

