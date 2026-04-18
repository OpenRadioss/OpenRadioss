#!/bin/bash 
if [ ! -d exec ]; then
  mkdir exec
fi

if [ -f exec/element_linux_x64_gfortran ]; then
  rm exec/element_linux_x64_gfortran
fi

echo "building with gfortran - exec/element_linux_x64_gfortran"
gfortran -O3 -fdec-math -fstack-arrays -fopenmp -frounding-math -g -fbacktrace -ffixed-line-length-none -D COMP_GFORTRAN -I source/include source/*.F -o exec/element_linux_x64_gfortran
#
echo "done"