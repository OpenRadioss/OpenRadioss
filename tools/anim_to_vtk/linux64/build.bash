#!/bin/bash

#
# check if exec directory exists, create if not
#
if [ ! -d ../../../exec ]
then
   mkdir ../../../exec
fi


 g++ -g3 -O0  -DLINUX -o ../../../exec/anim_to_vtk_linux64_gf ../src/anim_to_vtk.cpp

