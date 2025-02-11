# Building OpenRadioss

* [Build environment on Linux](#build-environment-on-linux)
  * [System prerequisites](#system-prerequisites)
  * [Compiler and development tools](#compiler-and-development-tools)
  * [OpenMPI installation](#openmpi-installation)
* [Build environment on Linux ARM64](#build-environment-on-linux-arm64)
  * [System prerequisites](#system-prerequisites-for-linux-arm64)
  * [Compiler and development tools](#compiler-and-development-tools-for-linux-arm64)
  * [OpenMPI installation](#openmpi-installation-for-linux-arm64)
* [Build environment on Windows](#build-environment-on-windows)
  * [Compiler environment](#compiler-environment)
  * [Build environment using cmd DOS shell](#build-environment-using-cmd-dos-shell)
  * [Build environment using Visual Studio](#build-environment-using-visual-studio-2022)
* [How to build OpenRadioss](#how-to-build-openradioss)
  * [Build defaults](#build-defaults)
  * [Get the source](#get-the-source)
  * [Building on Linux](#building-on-linux)
  * [Building on Linux Arm64](#building-on-linux-arm64)
  * [Build OpenRadioss on Windows with cmd Shell](#build-openradioss-on-windows-with-cmd-shell)
  * [Build OpenRadioss with Visual Studio](#build-openradioss-with-visual-studio)
* [How to build OpenRadioss on Linux with Container using Apptainer](#how-to-build-openradioss-on-linux-with-container-using-apptainer)
* [How to debug with Visual Studio](./doc/Visual_Studio_Debugger.md)
* [Notes on third party libraries](#notes-on-third-party-libraries)

## Build environment on Linux

### System prerequisites

Linux system with glibc version 2.17 or higher:

* CentOS Stream 8, RHEL 8, Rocky Linux 8, Rocky Linux 9
* Ubuntu 20.0.4 or higher
* [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install): OpenRadioss works with WSL/WSL2 Ubuntu 20.04 LTS, WSL2 Ubuntu 22.x

### Compiler and development tools

You will need GCC/Gfortran version 11 or higher,
[Cmake](https://cmake.org/) version 2.8 or higher, and GNU make.

Install as sudo or root

* RHEL 8, CentOS Stream 8, Rocky Linux 8

           dnf install gcc-toolset-11-toolchain
           dnf install gcc-toolset-11-libasan-devel
           dnf install gcc-toolset-11-libubsan-devel
           dnf install libasan
           dnf install libasan6
           dnf install libubsan
           dnf install make
           dnf install cmake
           dnf install python
           dnf install perl
           dnf install git-lfs

  Installed python is Python3. To create the link from python3 to python,

  type:

           alternatives --config python

           Select python3.

  To enable the devtoolset-11, you can run `source /opt/rh/gcc-toolset-11/enable`

* Ubuntu

           apt-get update
           apt-get upgrade
           apt-get install build-essential
           apt-get install libasan6
           apt-get install libubsan1
           apt-get install gfortran
           apt-get install cmake
           apt-get install perl
           apt-get install python3
           apt-get install python-is-python3
           apt-get install git-lfs

### OpenMPI installation

OpenMPI is needed to build OpenRadioss with OpenMPI support.
It is recommended to build and install OpenMPI from OpenMPI website using gcc compiler.

1. Download OpenMPI tarball from  [www.openmpi.org](https://www.open-mpi.org/software/ompi/v4.1)
   preferred version is [OpenMPI v4.1.2](https://download.open-mpi.org/release/open-mpi/v4.1/openmpi-4.1.2.tar.gz)

            wget https://download.open-mpi.org/release/open-mpi/v4.1/openmpi-4.1.2.tar.gz

2. Decompress and enter the folder:

            tar -xvzf openmpi-4.1.2.tar.gz
            cd openmpi-4.1.2

3. Build and install OpenMPI

**you need root or sudo rights on your computer**.

        ./configure --prefix=/opt/openmpi
        make
        make install

## Build environment on Linux ARM64

### System prerequisites for linux ARM64

This version works on ARMv8-A and higher architectures using Linux OS:

For the supported Hardware list, visit :

<https://developer.arm.com/Tools%20and%20Software/Arm%20Compiler%20for%20Linux#Supported-Devices>

The Linux ARM64 version is built using armflang and armclang compiler.
They are available for following Linux version :

* RHEL 7, RHEL 8, RHEL 9 for ARM64
* Suse SLES 15 for ARM64
* Ubuntu 20.04, Ubuntu 22.04 for ARM64

### Compiler and development tools for Linux Arm64

#### Development environment

##### Install Development tools on your machine

As root or sudo user:

On RHEL 8, Rocky Linux8, Suse...

     dnf install cmake
     dnf install python
     dnf install perl
     dnf install git
     dnf install git-lfs
     dnf install environment-modules

On Ubuntu 20.x, 22.x, 23.x...

     apt-get update
     apt-get upgrade
     apt-get install build-essential
     apt-get install gfortran
     apt-get install cmake
     apt-get install perl
     apt-get install python3
     apt-get install python-is-python3
     apt-get install git
     apt-get install git-lfs
     apt-get install environment-modules

##### Install ArmFlang compiler

ARM compilers and ARM PErformance libraries are used to build OpenRadioss.
ArmFlang 24.04 is recommended to build OpenRadioss. It uses the module system to setup the compiler.

* ArmFlang compilers can be downloaded at:
<https://developer.arm.com/downloads/-/arm-compiler-for-linux>
* Follow ArmFlang installation instructions: <https://developer.arm.com/documentation/102621/0100>

* To see the list of available modules and load the compiler settings type:

      module list
      module load acfl/24.04

**Note:**

* The module system was found to be improperly configured on some systems. If the module command is not found, add in your  .bashrc shell file:

      source /etc/profile 

* If the compiler could not be found with *module list*, add */opt/arm/modulefiles* in the *MODULEPATH* variable. Add this setting in your .bashrc file to have it permanent.

      export MODULEPATH=$MODULEPATH:/opt/arm/modulefiles

### OpenMPI installation for Linux Arm64

OpenMPI is needed to build OpenRadioss with OpenMPI support.
It is recommended to build and install OpenMPI from OpenMPI website using gcc compiler.

1. Download OpenMPI tarball from  [www.openmpi.org](https://www.open-mpi.org/software/ompi/v4.1)
   preferred version is [OpenMPI v4.1.2](https://download.open-mpi.org/release/open-mpi/v4.1/openmpi-4.1.2.tar.gz)

            wget https://download.open-mpi.org/release/open-mpi/v4.1/openmpi-4.1.2.tar.gz

2. Decompress and enter the folder:

            tar -xvzf openmpi-4.1.2.tar.gz
            cd openmpi-4.1.2

3. Build and install OpenMPI

Load the gcc/gfortran compiler module from ArmFlang installation using module environment tool:

* Find the available GNU compiler with:

        module avail

* Load the gnu compiler with module environment tool: here an example with gnu 11.2.0

        module load gnu/11.2.0

**you need root or sudo rights on your computer**.

        ./configure --prefix=/opt/openmpi
        make
        make install

## Build environment on Windows

OpenRadioss was tested with OneAPI 2025.0.1 + Visual Studio 2022. It is recommended to use
this compiler or more recent to build OpenRadioss.

This chapter explains how to setup Windows on different build configuration

* Compiler environment
* OpenRadioss build environment using cmd.exe
* OpenRadioss build using cygwin
* OpenRadioss build environment using Visual Studio.

### Compiler environment

1. Intel OneAPI requires Visual Studio Community, Enterprise or Professional Edition installed.
   For all prerequisites, visit : <https://www.intel.com/content/www/us/en/developer/articles/system-requirements/intel-oneapi-base-toolkit-system-requirements.html>
   **It is recommended to upgrade Visual Studio to the latest available one.**

2. Download one API Base Toolkit and one API HPC Toolkit

    * Visit one API Base Toolkit Download page: [oneAPI Base Toolkit](https://www.intel.com/content/www/us/en/developer/tools/oneapi/base-toolkit-download.html)
    * Visit one API HPC Toolkit Download page: [oneAPI HPC Toolkit](https://www.intel.com/content/www/us/en/developer/tools/oneapi/hpc-toolkit-download.html)

3. Install Toolkits

   Minimum required packages are

   * In the Base Toolkit: Intel DPC++/C++, Intel Math Kernel Library, Intel distribution for Python.
   * In the HPC Toolkit: Intel Intel® oneAPI DPC++/C++ Compiler, Intel® Fortran Compiler, Intel® MPI Library

   **Notes:**

   * Intel OneAPI plugin for Visual Studio is recommended to use Intel OneAPI in Visual Studio 2019
   * Choose the default directory to install Intel oneAPI

4. Install Git

   * Install Git for Windows from: [https://git-scm.com/downloads](https://git-scm.com/downloads).
The Git Bash tool is not needed, but can be installed.

5. Post installation tasks with git

* Install git-lfs

            git lfs install

* Add in Git global environment the autocrlf flag

            git config --global core.autocrlf true

* Create the ssh key & set it in GitHub

            ssh-keygen -t rsa
  
  **Note: Accept all defaults, Standard directory, no passphrase**

* Set your git parameters as in [CONTRIBUTING.md](./CONTRIBUTING.md)

### Build environment using cmd DOS shell

Building using cmd.exe is using cmake.exe and ninja.exe
Both are shipped with Visual Studio 2019.

1. Setup the compiler
   Load compiler settings in cmd.exe using following command :

         call "c:\Program Files (x86)\Intel\oneAPI\setvars.bat"

   cmd.exe can be launched using a batch script to ease usage

         @echo off
         call "c:\Program Files (x86)\Intel\oneAPI\setvars.bat"
         cmd.exe

### Build environment using Visual Studio 2022

**Notes:**

* Following procedure was tested on Visual Studio 2022

* Visual Studio Graphical environment must be installed.
* Visual Studio using cmake and ninja for compilation.
* It is recommended to update Visual Studio to most recent release.
* Cmake + Builders must be installed in Visual Studio : Visual Studio is using Cmake and ninja builder (available with cmake package)
* Intel OneAPI plugin for VS2022 must be installed and running. Otherwise Compiler is not found.

## How to build OpenRadioss

### Get the source

* Activate LFS: `git lfs install`
* Run `git clone git@github.com:OpenRadioss/OpenRadioss.git`.

See [here](./CONTRIBUTING.md) if you want to contribute to OpenRadioss.

### Build defaults

The default for Radioss builds are:

* Linux with with Gfortran : Optimized for release usage
* Windows with Intel Compiler : Optimized for release usage

Recommendations for developers are to build OpenRadioss with :

* Address Sanitizer for Linux / Gfortran : -debug=asan
* Check Bounds executable for Windows users : -debug=chkb

### Building on Linux

#### OpenRadioss Starter on Linux

* Enter the OpenRadioss/starter directory

            cd OpenRadioss/starter

* Launch `build_script.sh` to proceed to the compilation

  Usual build is make with:

            ./build_script.sh -arch=linux64_gf -release

* OpenRadioss Starter: **starter_linux64_gf** binary will be copied in **OpenRadioss/exec** directory

* Advanced script flags can be used to build OpenRadioss: run `./build_script.sh` without arguments:

       []$ ./build_script.sh

       Use with arguments :
       -arch=[build architecture]
                -arch=linux64_gf  (SMP executable / Gfortran compiler)
                -arch=linux64_AOCC (SMP executable / AOCC compiler)
                -arch=linuxa64    (SMP executable / ARM Linux - Armflang compiler)
                -arch=win64       (SMP executable / Windows X86-64 - Intel OneAPI)
       
        -prec=[dp|sp]                       : set precision - dp (default) |sp
        -static-link                        : Fortran, C & C++ runtime are linked in binary
        -debug=[0|1|asan]                   : debug version for gfortran
                                                 0 : no debug flags (default)
                                                 1 : usual debug flag
                                                 asan : gfortran address sanitizer (default)
        -release                            : Set build for release (optimized)

        -addflag="list of additional flags" : add compiler flags to usual set

        Execution control
        -nt=[threads]      : number of threads for build
        -verbose           : Verbose build
        -clean             : clean build directory

        -no-python : do not link with python

  * `-arch`: lists the available architectures
  * `-prec`: controls the OpenRadioss Floating Point Precision : dp : double Precision - Floats in 64 bits (default),  sp activates the Extended Single Precision Version (32bit)
  * `-static-link`: Runtime libraries are statically linked in Executable (easier when executable is used on different computers).
  * `-debug=[0|1|asan]`: activates debug build (-O0 + usual debug flags).
  * `-release`:  Set build for release (optimized)
  * `-addflag="list of additional flags"`: add compiler flags to usual set for all files

  Execution Control

  * `-nt=N` use N threads to fasten build
  * `-verbose`: compilation process is in Verbose mode
  * `-clean`: deletes compilation files and execution.

#### Building OpenRadioss Engine on Linux

* Enter the OpenRadioss/engine directory

* Launch `build_script.sh` to proceed to the compilation
  To build OpenRadioss Engine with OpenMPI support

            ./build_script.sh -arch=linux64_gf -mpi=ompi -release
  
  To build OpenRadioss without OpenMPI support (SMP parallelism):

            ./build_script.sh -arch=linux64_gf -release 

* OpenRadioss Engine: **engine_linux64_gf** or **engine_linux64_gf_ompi** binary will be copied in **OpenRadioss/exec** directory

* Advanced script flags can be used to build OpenRadioss Engine: launch `./build_script.sh` without arguments:

        []$ ./build_script.sh
        
        
         build_script
         ------------

         Use with arguments :
         -arch=[build architecture]
        
                 -arch=linux64_gf                (SMP executable / Gfortran compiler / Linux X86-64)
                 -arch=linux64_gf  -mpi=ompi     (OpenMPI executable / Gfortran compiler / Linux X86-64)
                 -arch=linux64_AOCC  -mpi=ompi     (OpenMPI executable / AOCC compiler / Linux X86-64)
                 -arch=linux64_intel                (SMP executable / Intel compiler / Linux X86-64)
                 -arch=linux64_intel  -mpi=impi     (MPI executable / Intel compiler / Linux X86-64)
                 -arch=linuxa64_gf               (SMP executable / Armflang compiler / Linux ARM64)
                 -arch=linuxa64_gf -mpi=ompi     (OpenMPI executable / Armflang compiler / Linux X86-64)
                 -arch=win64                     (SMP executable / Intel OneAPI / Windows X86-64)
                 -arch=win64       -mpi=impi     (Intel MPI OneAPI executable / Intel OneAPI / Windows X86-64)
        
         MPI libraries
         -mpi=[mpi]
                 not set   : SMP (default)
                 -mpi=ompi : OpenMPI
        
                 Controlling MPI Libraries - if need choose one of the 3 Option Set
                                            If no options set, recommended OpenMPI directories         are used (default)
                   1. -mpi-os                             : link with default MPI version         installed on system
                                                    libraries are in default installation
                   2. -mpi-root=[directory]               : set rootname to link with specific MPI installation
                   3. -mpi-include=[directory]            : set include directory where to find mpif.h and mpi.h
                      -mpi-libdir=[directory]             : set library directory where to find mpi libraries
        
         -prec=[dp|sp]                       : set precision - dp (default) |sp
         -static-link                        : Fortran, C & C++ runtime are linked in binary
         -debug=[0|1|asan]                   : debug version for gfortran
                                                  0 : no debug flags
                                                  1 : usual debug flag
                                                  asan : gfortran address sanitizer (default)
         -release                            : Set build for release (optimized)
        
         -addflag="list of additional flags" : add compiler flags to usual set
        
         Execution control
         -nt=[threads]      : number of threads for build
         -verbose           : Verbose build
         -clean             : clean build directory
        
          MUMPS linear solver: available only for dp, with mpi
         -mumps_root=[path_to_mumps]          : path_to_mumps/lib/libdmumps.a must exist
         -scalapack_root=[path to scalapack]  : path_to_scalapack/libscalapack.a must exist
         -lapack_root=[path to lapack]  : path_to_lapack/liblapack.a must exist
        
         -no-python : do not link with python

  * `-arch`: lists the available architectures

  MPI libraries

  * `-mpi` controls the MPI flavor

  More Flags to control the MPI installation. Per default OpenMPI is installed in /opt/openmpi.

  Additional ways are possible:

  * `-mpi-os`: `mpif.h` is found in default system installation, as well as the libraries
  * `-mpi-root`: set this flag to set a new root directory where OpenMPI can be found.
  * `-mpi-include`: set the directory where OpenMPI includes can be found
  * `-mpi-libdir`: set the Directory where OpenMPI libraries can be found

  Other controls

  * `-prec=[dp|sp]`: controls the OpenRadioss Floating Point Precision
    * `dp`: double Precision - Floats in 64 bits (default)
    * `sp`: activates the Extended Single Precision Version (32bit)
  * `-static-link`: Runtime libraries are statically linked in Executable (easier when executable is used on different computers).  
  * `-debug=1`: activates debug build (-O0 + usual debug flags).
  * `-release`: Set build for release (optimized)
  * `-addflag="list of additional flags"`: add compiler flags to usual set for all files

  Execution Control

  * `-nt=N` use N threads to fasten build
  * `-verbose`: compilation process is in Verbose mode
  * `-clean`: deletes compilation files and execution.

### Building on Linux Arm64

#### OpenRadioss Starter on Linux Arm64

* Enter the OpenRadioss/starter directory

            cd OpenRadioss/starter

* Launch `build_script.sh` to proceed to the compilation

  Usual build is make with:

            ./build_script.sh -arch=linuxa64 -release

* OpenRadioss Starter: **starter_linuxa64** binary will be copied in **OpenRadioss/exec** directory

* Advanced script flags can be used to build OpenRadioss: run `./build_script.sh` without arguments:

       []$ ./build_script.sh

       Use with arguments :
       -arch=[build architecture]
                -arch=linux64_gf  (SMP executable / Gfortran compiler)
                -arch=linux64_AOCC (SMP executable / AOCC compiler)
                -arch=linuxa64    (SMP executable / ARM Linux - Armflang compiler)
                -arch=win64       (SMP executable / Windows X86-64 - Intel OneAPI)
       
        -prec=[dp|sp]                       : set precision - dp (default) |sp
        -static-link                        : Fortran, C & C++ runtime are linked in binary
        -debug=[0|1|asan]                   : debug version for gfortran
                                                 0 : no debug flags (default)
                                                 1 : usual debug flag
                                                 asan : gfortran address sanitizer (default)
        -release                            : Set build for release (optimized)

        -addflag="list of additional flags" : add compiler flags to usual set

        Execution control
        -nt=[threads]      : number of threads for build
        -verbose           : Verbose build
        -clean             : clean build directory

        -no-python : do not link with python

  * `-arch`: lists the available architectures
  * `-prec`: controls the OpenRadioss Floating Point Precision : dp : double Precision - Floats in 64 bits (default),  sp activates the Extended Single Precision Version (32bit)
  * `-static-link`: Runtime libraries are statically linked in Executable (easier when executable is used on different computers).
  * `-debug=[0|1|asan]`: activates debug build (-O0 + usual debug flags).
  * `-release`:  Set build for release (optimized)
  * `-addflag="list of additional flags"`: add compiler flags to usual set for all files

  Execution Control

  * `-nt=N` use N threads to fasten build
  * `-verbose`: compilation process is in Verbose mode
  * `-clean`: deletes compilation files and execution.

#### Building OpenRadioss Engine on Linux Arm64

* Enter the OpenRadioss/engine directory

* Launch `build_script.sh` to proceed to the compilation
  To build OpenRadioss Engine with OpenMPI support

            ./build_script.sh -arch=linuxa64 -mpi=ompi -release
  
  To build OpenRadioss without OpenMPI support (SMP parallelism):

            ./build_script.sh -arch=linuxa64 -release 

* OpenRadioss Engine: **engine_linuxa64** or **engine_linuxa64_ompi** binary will be copied in **OpenRadioss/exec** directory

* Advanced script flags can be used to build OpenRadioss Engine: launch `./build_script.sh` without arguments:

        []$ ./build_script.sh
        
        
         build_script
         ------------

         Use with arguments :
         -arch=[build architecture]
        
                 -arch=linux64_gf                (SMP executable / Gfortran compiler / Linux X86-64)
                 -arch=linux64_gf  -mpi=ompi     (OpenMPI executable / Gfortran compiler / Linux X86-64)
                 -arch=linux64_AOCC  -mpi=ompi     (OpenMPI executable / AOCC compiler / Linux X86-64)
                 -arch=linux64_intel                (SMP executable / Intel compiler / Linux X86-64)
                 -arch=linux64_intel  -mpi=impi     (MPI executable / Intel compiler / Linux X86-64)
                 -arch=linuxa64_gf               (SMP executable / Armflang compiler / Linux ARM64)
                 -arch=linuxa64_gf -mpi=ompi     (OpenMPI executable / Armflang compiler / Linux X86-64)
                 -arch=win64                     (SMP executable / Intel OneAPI / Windows X86-64)
                 -arch=win64       -mpi=impi     (Intel MPI OneAPI executable / Intel OneAPI / Windows X86-64)
        
         MPI libraries
         -mpi=[mpi]
                 not set   : SMP (default)
                 -mpi=ompi : OpenMPI
        
                 Controlling MPI Libraries - if need choose one of the 3 Option Set
                                            If no options set, recommended OpenMPI directories         are used (default)
                   1. -mpi-os                             : link with default MPI version         installed on system
                                                    libraries are in default installation
                   2. -mpi-root=[directory]               : set rootname to link with specific MPI installation
                   3. -mpi-include=[directory]            : set include directory where to find mpif.h and mpi.h
                      -mpi-libdir=[directory]             : set library directory where to find mpi libraries
        
         -prec=[dp|sp]                       : set precision - dp (default) |sp
         -static-link                        : Fortran, C & C++ runtime are linked in binary
         -debug=[0|1|asan]                   : debug version for gfortran
                                                  0 : no debug flags
                                                  1 : usual debug flag
                                                  asan : gfortran address sanitizer (default)
         -release                            : Set build for release (optimized)
        
         -addflag="list of additional flags" : add compiler flags to usual set
        
         Execution control
         -nt=[threads]      : number of threads for build
         -verbose           : Verbose build
         -clean             : clean build directory
        
          MUMPS linear solver: available only for dp, with mpi
         -mumps_root=[path_to_mumps]          : path_to_mumps/lib/libdmumps.a must exist
         -scalapack_root=[path to scalapack]  : path_to_scalapack/libscalapack.a must exist
         -lapack_root=[path to lapack]  : path_to_lapack/liblapack.a must exist
        
         -no-python : do not link with python

  * `-arch`: lists the available architectures

  MPI libraries

  * `-mpi` controls the MPI flavor

  More Flags to control the MPI installation. Per default OpenMPI is installed in /opt/openmpi.

  Additional ways are possible:

  * `-mpi-os`: `mpif.h` is found in default system installation, as well as the libraries
  * `-mpi-root`: set this flag to set a new root directory where OpenMPI can be found.
  * `-mpi-include`: set the directory where OpenMPI includes can be found
  * `-mpi-libdir`: set the Directory where OpenMPI libraries can be found

  Other controls

  * `-prec=[dp|sp]`: controls the OpenRadioss Floating Point Precision
    * `dp`: double Precision - Floats in 64 bits (default)
    * `sp`: activates the Extended Single Precision Version (32bit)
  * `-static-link`: Runtime libraries are statically linked in Executable (easier when executable is used on different computers).  
  * `-debug=1`: activates debug build (-O0 + usual debug flags).
  * `-release`: Set build for release (optimized)
  * `-addflag="list of additional flags"`: add compiler flags to usual set for all files

  Execution Control

  * `-nt=N` use N threads to fasten build
  * `-verbose`: compilation process is in Verbose mode
  * `-clean`: deletes compilation files and execution.

### Build OpenRadioss on Windows with cmd Shell

#### OpenRadioss Starter on Windows

* Enter the OpenRadioss/starter directory

            cd OpenRadioss/starter

* Launch `build_windows.bat` to proceed with compilation
  Usual build is made with:
  
           build_windows.bat -arch=win64 -release

* OpenRadioss Starter: **starter_win64.exe** binary is copied in **OpenRadioss/exec** directory

* Different builds are possible : launch build_windows.bat without argument to see the possible options:

       Use with arguments : 
           -arch=[build architecture]          : set architecture : default  Windows 64 bit
           -prec=[dp,sp]                       : set precision - dp (default),sp
           -static-link                        : Compiler runtime is linked in binary
           -debug=[0,1,chkb]                   : debug version 
                                                 0: no debug flags
                                                 1: usual debug flags
                                                 chkb: Check bounds build (default)

           -release                            : set build for release (optimized)

       Execution control
           -nt [N,all]        : Run build with N Threads, all : takes all resources of machine
           -verbose           : Verbose build
           -clean             : clean build directory

#### OpenRadioss Engine on Windows

* Enter the OpenRadioss/engine directory

* Launch `build_windows.bat` to proceed to the compilation
  To build OpenRadioss Engine with Intel MPI support

            ./build_windows.bat -arch=win64 -mpi=impi

  To build OpenRadioss without Intel MPI support (SMP parallelism):

            ../build_windows.bat -arch=win64 -release

* OpenRadioss Engine: **engine_win64_impi.exe** or **engine_win64.exe** binary are copied in **OpenRadioss/exec** directory

* Different builds are possible : launch build_windows.bat without argument to see the possible options:

       Use with arguments :
          -arch=[build architecture]          : set architecture : default  Windows 64 bit
          -mpi=[smp,impi]                     : set MPI version
          -prec=[dp,sp]                       : set precision - dp (default),sp
          -static-link                        : Compiler runtime is linked in binary
          -debug=[0,1,chkb]                   : debug version 
                                                0: no debug flags
                                                1: usual debug flag
                                                chkb: check bounds build (default)

          -release                            : set build for release (optimized)
          
       Execution control
          -nt [N,all]        : Run build with N Threads, all : consumes all resources of machine
          -verbose           : Verbose build
          -clean             : clean build directory

### Build OpenRadioss with Visual Studio

This sections assumes, that Intel OneAPI Compiler was successfully installed.
Procedure was tested on Visual Studio 2019 and Visual Studio 2022

* Launch Visual Studio

* Choose `Open Local Folder` option and select the OpenRadioss directory from your clone.

![image](/doc/vs_start.png)

* Visual Studio starts and read the configuration files CMakeLists.txt and CMakeSettings.json.

* Select build configuration in the `Configuration` menu :

![image](/doc/vs_studio_in.png)

* Choose the wanted configuration from Starter/Engine release and debug configuration.

* Launch in Menu : [Build]:[Build All]

* OpenRadioss binaries are copied in **OpenRadioss/exec** directory

## How to build OpenRadioss on Linux with Container using Apptainer

### Linux

Linux system with [Apptainer](https://apptainer.org/docs/admin/main/installation.html):

* CentOS Stream 8, RHEL 8, Rocky Linux 8, Rocky Linux 9
* Ubuntu 20.0.4 or higher
* [Windows Subsystem for Linux](https://docs.microsoft.com/en-us/windows/wsl/install): Apptainer works with WSL/WSL2 Ubuntu 20.04 LTS, WSL2 Ubuntu 22.x

### Build OpenRadioss Container

* Enter the OpenRadioss/Apptainer directory

            cd OpenRadioss/Apptainer

* Build OpenRadioss container using Apptainer

            sudo apptainer build openradioss.sif openradioss.def

* Copy OpenRadioss container to the directory which is in your `$PATH`

            sudo cp openradioss.sif /usr/local/bin

## Notes on third party libraries

OpenRadioss Starter and Engine use third party libraries in build process.
Those third party libraries are stored in [OpenRadioss/OpenRadioss_extlib](https://github.com/OpenRadioss/OpenRadioss_extlib) repository.

During compilation process, the needed "Release asset" is downloaded and installed if not already in the repository.
