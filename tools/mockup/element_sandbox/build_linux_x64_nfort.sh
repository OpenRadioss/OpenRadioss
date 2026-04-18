#!/bin/bash 
if [ ! -d exec ]; then
  mkdir exec
fi

if [ -f exec/element_linux_x64_nfort ]; then
  rm exec/element_linux_x64_nfort
fi

nfort -O3 -fopenmp -fextend-source -fassume-contiguous -fno-matrix-multiply -mno-vector-fma -fno-associative-math -fno-outerloop-unroll -Isource/include source/*.F -o exec/element_linux_x64_nfort
