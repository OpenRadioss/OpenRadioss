#!/bin/bash 
if [ ! -d exec ]; then
  mkdir exec
fi

if [ -f exec/element_linux_arm64_gfortran_sve]; then
  rm exec/element_linux_arm64_gfortran_sve
fi

gfortran -O3 -march=armv8-a+sve -fdec-math -fstack-arrays -fopenmp -frounding-math -g -fbacktrace -ffixed-line-length-none  -D COMP_GFORTRAN -I source/include source/*.F -o exec/element_linux_arm64_gfortran_sve
