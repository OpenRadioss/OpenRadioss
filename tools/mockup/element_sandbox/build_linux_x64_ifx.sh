#!/bin/bash 
if [ ! -d exec ]; then
  mkdir exec
fi

if [ -f exec/element_linux_x64_ifx ]; then
  rm exec/element_linux_x64_ifx
fi
ifx  -O3 -fp-model precise -align array64byte -fimf-use-svml=true -assume buffered_io -extend-source -qopenmp -traceback -g -qopenmp-link=static -I source/include -o exec/element_linux_x64_ifx source/*.F
