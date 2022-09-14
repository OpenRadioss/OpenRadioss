# th_to_csv

anim_to_vtk is an external tool to convert OpenRadioss time history files to csv format

# How to build

Enter the platform directory.
Apply the build script

Executable will be copied in [OpenRadioss]/exec directory

# How to use



Launch the converter after the simulation :

        ./th_to_csv [TimeHistory_File]

## Note
To have full variable names in .csv file, add /TH/TITLE in 1.rad file when running engine : 
/TH/TITLE write some _TITLES file that contains additional information allowing to have full titles in writen .csv file

