# How to Build OpenRadioss 

## System and compiler installation

### Linux
Linux system with glibc version 2.17 or higher : 
* CentOS/RHEL 7, CentOS Stream 8, RHEL 8
* Ubuntu 20.0.4 or higher
* Works with WSL/WSL2 Ubuntu 20.04 LTS, WSL2 Ubuntu 22.x

### Compiler and development tools

Compiler is gcc/gfortran version 8 or higher
cmake version 2.8 or higher and make are need

Install as sudo or root

* RHEL 7

            yum install devtoolset-8
            yum install make
            yum install cmake
            yum install perl

* RHEL 8, CentOS Stream 8


           dnf install gcc
           dnf install gcc-gfortran
           dnf install make
           dnf install cmake
           dnf install perl

* Ubuntu

           apt-get update
           apt-get install build-essential
           apt-get install gfortran
           apt-get install cmake
           apt-get install perl

### OpenMPI

OpenMPI is need to build OpenRadioss with OpenMPI support, compilers 

1. Download OpenMPI tarball from  [www.openmpi.org](https://www.open-mpi.org/software/ompi/v4.1)
   prefered version is OpenMPI v4.1.2

2. untar file in Home directory & enter the directory

3. use configure, make & make install

   CARE : to install **you need root or sudo rights on your computer**.

        ./configure --prefix=/opt/openmpi
        make
        make install


## How to build OpenRadioss

### Building OpenRadioss Starter

* Enter the OpenRadioss/starter directory

    cd OpenRadioss/starter

* Launch `build_script.sh` to proceed to the compilation

  Usual build is make with:

            ./build_script.sh -arch=linux64_gf


* OpenRadioss Starter : **starter_linux64_gf** binary will be copied in **OpenRadioss/exec** directory


* Advanced script flags can be used to build OpenRadioss  : launch ./build_script without arguments :

        []$ ./build_script.sh

         build_script
         ------------
 
         Use with arguments : 
         -arch=[build architecture]
             -arch=linux64_gf  (SMP executable / Gfortran compiler)
         -prec=[dp|sp]                        : set precision - dp (default) |sp 
         -static-link                         : Fortran, C & C++ runtime are linked in binary
         -debug=[0|1]                         : debug version 0 no debug flags (default), 1 usual debug flag )
         -addflag="list of additionnal flags" : add compiler flags to usual set
 
         Execution control 
         -nt=[threads]      : number of threads for build 
         -verbose           : Verbose build
         -clean             : clean build directory
 

- `-arch`: you will find the list of possible architectures
- `-prec`: controls the OpenRadioss Floating Point Precision : dp : double Precision - Floats in 64 bits (default),  sp activates the Extended Single Precision Version (32bit)
- `-static-link`: Runtime librairies are statically linked in Executable (easier when executable is used on different computers).
- `-debug=1`: activates debug build (-O0 + usual debug flags).
- `-addflag="list of additional flags"`: add compiler flags to usual set for all files 

Execution Control

- `-nt=N` use N threads to fasten build
- `-verbose`: compilation process is in Verbose mode
- `-clean`: deletes compilation files and execution.


### Building OpenRadioss Engine
 
* Enter the OpenRadioss/engine directory

* Launch `build_script.sh` to proceed to the compilation
  To build OpenRadioss Engine with OpenMPI support
            
            ./build_script.sh -arch=linux64_gf -mpi=ompi
  

  To build OpenRadioss without OpenMPI support (SMP parallelism) :

            ./build_script.sh -arch=linux64_gf 


* OpenRadioss Engine : **engine_linux64_gf** or **engine_linux64_gf_ompi** binary will be copied in **OpenRadioss/exec** directory


* Advanced script flags can be used to build OpenRadioss Engine : launch ./build_script without arguments :


        []$ ./build_script.sh 
         
         build_script
         ------------
 
         Use with arguments : 
         -arch=[build architecture]

                 -arch=linux64_gf               (SMP executable / Gfortran compiler)
                 -arch=linux64_gf -mpi=ompi     (OpenMPI executable / Gfortran compiler)
 
         MPI libraries
         -mpi=[mpi]
                 not set   : SMP (default)
                 -mpi=ompi : OpenMPI
 
                  Controling MPI Libraries - if need choose one of the 3 Option Set
                                             If no options set, recommended OpenMPI directories are uses (default)
                    1. -mpi-os                             : link with default MPI version installed on system
                                                    libraries are in default installation 
                    2. -mpi-root=[directory]               : set rootname to link with specific MPI installation
                    3. -mpi-include=[directory]            : set include directory where to find mpif.h and mpi.h
                       -mpi-libdir=[directory]             : set library directory where to find mpi libraries
 
         Other control
         -prec=[dp|sp]                        : set precision - dp (default) |sp 
         -static-link                         : Fortran, C & C++ runtime are linked in binary
         -debug=[0|1]                         : debug version 0 no debug flags (default), 1 usual debug flag )
         -addflag="list of additionnal flags" : add compiler flags to usual set
 
         Build control 
         -nt=[threads]      : number of threads for build 
         -verbose           : Verbose build
         -clean             : clean build directory


- `-arch`: you will find the list of possible architectures

MPI libraries

- `-mpi` controls the MPI flavor 

More Flags to control the MPI installation. Per default OpenMPI is installed in /opt/openmpi. 
3 additionnal ways are possible : 
 1. `-mpi-os`: `mpif.h` is found in default system installation, as well as the libraries
 2. `-mpi-root`: set this flag to set a new root directory where OpenMPI can be found.
 3. `-mpi-include`: set the directory where OpenMPI can be found
    `-mpi-libdir`: set the Directory where OpenMPI can be found

Other controls

- `-prec=[dp|sp]`: controls the OpenRadioss Floating Point Precision 
            - `dp`: double Precision - Floats in 64 bits (default)
            - `sp`: activates the Extended Single Precision Version (32bit)
- `-static-link`: Runtime librairies are statically linked in Executable (easier when executable is used on different computers).
- `-debug=1`: activates debug build (-O0 + usual debug flags).
- `-addflag="list of additionnal flags"`: add compiler flags to usual set for all files 

Execution Control

- `-nt=N` use N threads to fasten build
- `-verbose`: compilation process is in Verbose mode
- `-clean`: deletes compilation files and execution.


