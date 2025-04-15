#!/bin/bash

function my_help()
{
  echo " " 
  echo " open_reader build_script"
  echo " ------------------------"
  echo " " 
  echo " Use with arguments : "
  echo " -arch=[build architecture]"
  echo "          linux64  : X86_64 Linux"
  echo "          linuxa64 : ARM64" 
  echo " " 
  echo " Execution control"
  echo " -nt=[threads]      : number of threads for build "
  echo " -verbose           : Verbose build"
  echo " -clean             : clean build directory"
  echo " " 
  echo " "
}

# Script Variables 
threads=1
verbose=""
clean=0
arch=none

number_of_arguments=$#
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
       fi

       if [ "$arg" == "-nt" ]
       then
         threads=`echo $var|awk -F '=' '{print $2}'`
       fi

       if [ "$arg" == "-verbose" ]
       then
         verbose="VERBOSE=1"
       fi

         if [ "$arg" == "-clean" ]
         then
            clean=1
         fi

   done

fi


if [ $arch ==  "none" ]
then
   echo " "
   echo "Error: No architecture specified !"
   echo " "
   my_help
   exit 1
fi


build_directory=cbuild_${arch}${db}


if [ $clean = 1 ]
then
   echo " "
   echo "Cleaning build directory: ${build_directory}"
   echo " "
   if [ -d ${build_directory} ] 
   then
      rm -rf ${build_directory}
   fi
   exit 0
fi

echo " " 
echo " Build OpenReader "
echo " -----------------"
echo " Build Arguments :"
echo " arch =                 : " $arch
echo " " 
echo " threads =              : " $threads
echo " " 

# Load external libraries
echo "Load external libraries"
python ../Compiling_tools/script/load_extlib.py


# create build directory
if [ ! -d ${build_directory} ] 
then
   mkdir ${build_directory}
fi

cd ${build_directory}

cmake -DCMAKE_CXX_COMPILER=g++ -DCMAKE_C_COMPILER=gcc -Darch=${arch} ..
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
if [ $return_value -ne 0 ]
then
   echo " " 
   echo " " 
   echo "-- Errors in build found"
   echo " " 
   exit 1
fi

echo " "
echo " "
echo "Build completed successfully"
echo " "
echo " "
exit 0
