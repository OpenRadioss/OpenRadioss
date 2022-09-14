# anim_to_vtk

anim_to_vtk is an external tool to convert OpenRadioss animation files to legacy vtk ascii format.

# How to build

Enter the platform directory.
Apply the build script

Executable will be copied in [OpenRadioss]/exec directory

# How to use

Apply anim_to_vtk_linux64_gf to each animation file :

        ./anim_to_vtk_linux64_gf  [Deck Rootname]A001 > [Deck Rootname]_001.vtk
        ./anim_to_vtk_linux64_gf  [Deck Rootname]A002 > [Deck Rootname]_002.vtk
        ...
        ./anim_to_vtk_linux64_gf  [Deck Rootname]A002 > [Deck Rootname]_002.vtk


In Paraview, the vtk files are bundled and can be loaded in one step.


