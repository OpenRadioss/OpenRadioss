#!/bin/bash 
if [ ! -d exec ]; then
  mkdir exec
fi

if [ -f exec/element_linux_arm64_armflang_sve ]; then
  rm exec/element_linux_arm64_armflang_sve
fi

armflang -Ofast -march=armv8.2-a+sve -fno-stack-arrays -mcpu=a64fx -fopenmp -g -ffixed-line-length-none -armpl=sve  -static-arm-libs -D COMP_ARMFLANG -I source/include source/*.F -o exec/element_linux_arm64_armflang_sve

