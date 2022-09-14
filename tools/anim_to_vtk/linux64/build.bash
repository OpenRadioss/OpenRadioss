#!/bin/bash

#
# check if exec directory exists, create if not
#
if [ ! -d ../../../exec ]
then
   mkdir ../../../exec
fi


 g++ -DLINUX -o ../../../exec/anim_to_vtk_linux64_gf ../src/anim_to_vtk.cpp

