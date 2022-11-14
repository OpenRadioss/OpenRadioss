# anim_to_vtk

anim_to_vtk is an external tool to convert OpenRadioss animation files to legacy vtk ascii format.

# How to build

## Linux
gcc compiler instalaltion requested.

Enter the platform directory : anim_to_vtk/linux64
Apply the build script : ./build.bash

Executable will be copied in [OpenRadioss]/exec directory

## Windows
Visual Studio Community, Enterprise or Professional Edition installation is required.
Launch Visual Studio Shell for X86-64 Native tools.

Enter the platform directory : anim_to_vtk/win64
Apply the script : build.bat

Executable is copied in [OpenRadioss]/exec

# How to use

Apply anim_to_vtk_linux64_gf to each animation file :

        ./anim_to_vtk_linux64_gf  [Deck Rootname]A001 > [Deck Rootname]_001.vtk
        ./anim_to_vtk_linux64_gf  [Deck Rootname]A002 > [Deck Rootname]_002.vtk
        ...
        ./anim_to_vtk_linux64_gf  [Deck Rootname]A002 > [Deck Rootname]_002.vtk


In Paraview, the vtk files are bundled and can be loaded in one step.


