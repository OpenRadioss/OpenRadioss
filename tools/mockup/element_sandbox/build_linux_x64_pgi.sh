#!/bin/bash 
if [ ! -d exec ]; then
  mkdir exec
fi

if [ -f exec/element_linux_x64_pgi ]; then
  rm exec/element_linux_x64_pgi
fi
pgfortran -O3 -Mextend -mp -I source/include -o exec/element_linux_x64_pgi source/*.F
