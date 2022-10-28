# How to run OpenRadioss

OpenRadioss is made of:

* OpenRadioss Starter that checks the model and splits the mesh
* OpenRadioss Engine that runs the simulation in parallel
* Few [libraries](https://github.com/OpenRadioss/OpenRadioss/tree/main/extlib)
* A set of [configuration files](https://github.com/OpenRadioss/OpenRadioss/tree/main/hm_cfg_files) that describes the input


## Prerequisites

Download and [build OpenRadioss](https://github.com/OpenRadioss/OpenRadioss/blob/main/HOWTO.md), or download the [binaries](https://github.com/OpenRadioss/OpenRadioss/releases)

### Environment variables settings under Linux

Set the following environment variables:

        export OPENRADIOSS_PATH=[Path to OpenRadioss root directory]
        export RAD_CFG_PATH=$OPENRADIOSS_PATH/hm_cfg_files
        export OMP_STACKSIZE=400m
        export LD_LIBRARY_PATH=$OPENRADIOSS_PATH/extlib/hm_reader/linux64/:$OPENRADIOSS_PATH/extlib/h3d/lib/linux64/:$LD_LIBRARY_PATH

### Environment variables settings under Windows cmd shell

Set the following environment variables:

        set OPENRADIOSS_PATH=[Path to OpenRadioss root directory / Windows Style]
        set RAD_CFG_PATH=%OPENRADIOSS_PATH%\hm_cfg_files
        set KMP_STACKSIZE=400m
        set PATH=%OPENRADIOSS_PATH%\extlib\hm_reader\win64;%OPENRADIOSS_PATH%\extlib\h3d\lib\win64;%PATH% 


### Environment variables settings under Windows cygwin shell

Cygwin is translating PATH variable into DOS path to execute OpenRadioss, but not other variables.
To be understand by OpenRadioss, some variables need specific syntax understood by cygwin & DOS.

        export OPENRADIOSS_PATH=[Path to OpenRadioss root directory / Cygwin Style]
        export RAD_CFG_PATH=[Path to OpenRadioss root directory / mixed DOS/Cygwin Style]/hm_cfg_files
        export KMP_STACKSIZE=400m
        export PATH=$OPENRADIOSS_PATH/extlib/hm_reader/win64:$OPENRADIOSS_PATH/extlib/h3d/lib/win64;$PATH

Example : If OpenRadioss is placed in C:\OpenRadioss

        export OPENRADIOSS_PATH=/cygdrive/c/OpenRadioss
        export RAD_CFG_PATH=c:/OpenRadioss/hm_cfg_files
        export KMP_STACKSIZE=400m
        export PATH=$OPENRADIOSS_PATH/extlib/hm_reader/win64:$OPENRADIOSS_PATH/extlib/h3d/lib/win64;$PATH



## Runing OpenRadioss


### Running OpenRadioss without MPI (OpenMP only)

* Define number of OpenMP threads

   * **Under Linux and Windows Cygwin**

         export OMP_NUM_THREADS=[N]

   * **Under Windows**

         set OMP_NUM_THREADS=[N]

* Run OpenRadioss Starter and Engine from the directory that contains the binaries

   * **Under Linux**

         ./starter_linux64_gf -i [Starter input file] -np 1
         ./engine_linux64_gf -i [Engine input file]

  * **Under Windows**

        starter_win64.exe -i [Starter input file] -np 1
        engine_win64.exe  -i [Engine input file]


### Running OpenRadioss with MPI+OpenMP

#### Under Linux 

* Set up environment variables, assuming that OpenMPI is installed in `/opt/openmpi`

        export LD_LIBRARY_PATH=/opt/openmpi/lib:$LD_LIBRARY_PATH
        export PATH=/opt/openmpi/bin:$PATH

* Run OpenRadioss with P MPI processes and N threads per domain from the directory that contains the binaries

        export OMP_STACKSIZE=400m
        export OMP_NUM_THREADS=[N]
        ./starter_linux64_gf -i [Starter input file] -np [P]
        mpiexec -n [P]  --map-by socket:PE=$OMP_NUM_THREADS --bind-to core ./engine_linux64_gf_ompi -i [Engine input file]


#### Under Windows in cmd.exe shell

* Intel OneAPI MPI must be installed and setup. Variables can le load separately : 

        call [Path to Intel OneAPI]\env\vars.bat

* Run OpenRadioss with P MPI processes and N threads per domain from the directory that contains the binaries

        set OMP_NUM_THREADS=[N]
        starter_win64.exe -i [Starter input file] -np [P]
        mpirun -delegate -np [P]  engine_win64.exe  -i [Engine input file]

### Running OpenRadioss container using Apptainer under Linux

#### Running OpenRadioss container without MPI (OpenMP only)

* Define number of OpenMP threads

        export OMP_NUM_THREADS=[N]

* Run OpenRadioss Starter and Engine from the directory that contains the input file

        openradioss.sif starter_linux64_gf -i [Starter input file] -np 1
        openradioss.sif engine_linux64_gf -i [Engine input file]

#### Running OpenRadioss container with MPI+OpenMP

* Set up environment variables, assuming that OpenMPI is installed in `/opt/openmpi`

        export LD_LIBRARY_PATH=/opt/openmpi/lib:$LD_LIBRARY_PATH
        export PATH=/opt/openmpi/bin:$PATH

* Run OpenRadioss with P MPI processes and N threads per domain from the directory that contains the input file

        export OMP_NUM_THREADS=[N]
        export OMP_STACKSIZE=400m
        openradioss.sif starter_linux64_gf -i [Starter input file] -np [P]
        mpiexec  --map-by socket:PE=$OMP_NUM_THREADS --bind-to core -n [P] openradioss.sif engine_linux64_gf_ompi -i [Engine input file]

## Running OpenRadioss test suite from the source code

### Under Linux

* Go to the `qa_test/scripts` directory

        cd $OPENRADIOSS_PATH/qa-tests/scripts

#### Running without MPI (OpenMP only)

        perl ./or_qa_script ../../exec/engine_linux64_gf 1.0

#### Running with MPI (MPI+OpenMP)

* Set up environment variables, assuming that OpenMPI is installed in `/opt/openmpi`

        export LD_LIBRARY_PATH=/opt/openmpi/lib:$LD_LIBRARY_PATH
        export PATH=/opt/openmpi/bin:$PATH

* Run the test suite with P MPI processes, and N threads per MPI process

        export OMP_NUM_THREADS=[N]
        perl ./or_qa_script ../../exec/engine_linux64_gf_ompi --exec_script_args="mpiexec -np [P]"  1.0

### Under Windows

The QA test case can be executed under Cygwin.
Set the variables like running under Cygwin.

* Go to the `qa_test/scripts` directory

        cd $OPENRADIOSS_PATH/qa-tests/scripts

#### Running without MPI (OpenMP only)

        perl ./or_qa_script ../../exec/engine_win64.exe 1.0


#### Running with MPI (MPI+OpenMP)

* Set up environment variables assuming Intel OneAPI / MPI is installed & variables loaded.

* Run the test suite with P MPI processes, and N threads per MPI process

        export OMP_NUM_THREADS=[N]
        perl ./or_qa_script ../../exec/engine_win64_impi.exe --exec_script_args="mpiexec -delegate -np [P]"  1.0

