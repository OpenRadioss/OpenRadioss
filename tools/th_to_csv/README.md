# th_to_csv

anim_to_vtk is an external tool to convert OpenRadioss time history files to csv format

## How to build

### Linux

gcc installation is required

Enter the platform directory : linux64
Apply the build script : ./build.bash

Executable will be copied in [OpenRadioss]/exec directory

### Linux ARM64

gcc installation is required

Enter the platform directory : linuxa64
Apply the build script : ./build.bash

Executable will be copied in [OpenRadioss]/exec directory

### Windows

Visual Studio Community, Enterprise or Professional Edition installation is required.
Launch Visual Studio Shell for X86-64 Native tools.

Enter the platform directory : win64
Apply the build script : ./build.bat

Executable will be copied in [OpenRadioss]/exec directory

## How to use

Launch the converter after the simulation :

        ./th_to_csv [TimeHistory_File]

## Note

To have full variable names in .csv file, add /TH/TITLE in 1.rad file when running engine :
/TH/TITLE write some _TITLES file that contains additional information allowing to have full titles in writen .csv file

OpenRadioss time history files do not contain any forces curves , but only impulses
In addition to titles , /TH/TITLE write some information that allows the converter to derivate impulses and write forces
