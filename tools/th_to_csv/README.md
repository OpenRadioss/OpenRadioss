# th_to_csv

th_to_csv is an external tool to convert OpenRadioss time history files to CSV format.

## How to build

### Linux

GCC installation is required.

Enter the platform directory: linux64
Apply the build script: ./build.bash

The executable will be copied to the [OpenRadioss]/exec directory.

### Linux ARM64

GCC installation is required.

Enter the platform directory: linuxa64
Apply the build script: ./build.bash

The executable will be copied to the [OpenRadioss]/exec directory.

### Windows

Visual Studio Community, Enterprise, or Professional Edition installation is required.
Launch Visual Studio Shell for X86-64 Native tools.

Enter the platform directory: win64
Apply the build script: ./build.bat

The executable will be copied to the [OpenRadioss]/exec directory.

## How to use

Launch the converter after the simulation:

        ./th_to_csv [TimeHistory_File]

## Note

To have full variable names in the .csv file, add /TH/TITLE in 1.rad file when running the engine:
/TH/TITLE writes some _TITLES file that contains additional information allowing to have full titles in the written .csv file.

OpenRadioss time history files do not contain any force curves, but only impulses.
In addition to titles, /TH/TITLE writes some information that allows the converter to derive impulses and write forces.
