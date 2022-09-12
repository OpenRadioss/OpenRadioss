# How to run OpenRadioss

OpenRadioss is made of:

* OpenRadioss Starter that checks the model and splits the mesh
* OpenRadioss Engine that runs the simulation in parallel
* Few [libraries](https://github.com/OpenRadioss/OpenRadioss/tree/main/extlib)
* A set of [configuration files](https://github.com/OpenRadioss/OpenRadioss/tree/main/hm_cfg_files) that describes the input


## Prerequisites
* Download and [build OpenRadioss](https://github.com/OpenRadioss/OpenRadioss/blob/main/HOWTO.md), or download the [binaries](https://github.com/OpenRadioss/OpenRadioss/releases)
* Set the following environment variables:

        export OPENRADIOSS_PATH=[Path to OpenRadioss root directory]
        export RAD_CFG_PATH=$OPENRADIOSS_PATH/hm_cfg_files
        export OMP_STACKSIZE=400m
        export LD_LIBRARY_PATH=$OPENRADIOSS_PATH/extlib/hm_reader/linux64/:$OPENRADIOSS_PATH/extlib/h3d/lib/linux64/:$LD_LIBRARY_PATH

## Runing OpenRadioss
### Running OpenRadioss without MPI (OpenMP only)

* Define number of OpenMP threads

        export OMP_NUM_THREADS=[N]

* Run OpenRadioss Starter and Engine from the directory that contains the binaries

        ./starter_linux64_gf -i [Starter input file] -np 1
        ./engine_linux64_gf -i [Engine input file]

### Running OpenRadioss with MPI+OpenMP

* Set up environment variables, assuming that OpenMPI is installed in `/opt/openmpi`

        export LD_LIBRARY_PATH=/opt/openmpi/lib:$LD_LIBRARY_PATH
        export PATH=/opt/openmpi/bin:$PATH

* Run OpenRadioss with P MPI processes and N threads per domain from the directory that contains the binaries

        export OMP_NUM_THREADS=[N]
        ./starter_linux64_gf -i [Starter input file] -np [P]
        mpiexec -n [P] ./engine_linux64_gf_ompi -i [Engine input file]


## Running OpenRadioss test suite from the source code

* Go to the `qa_test/scripts` directory

        cd $OPENRADIOSS_PATH/qa-tests/scripts

### Running without MPI (OpenMP only)

        perl ./or_qa_script ../../exec/engine_linux64_gf 1.0

### Running with MPI (MPI+OpenMP)

* Set up environment variables, assuming that OpenMPI is installed in `/opt/openmpi`

        export LD_LIBRARY_PATH=/opt/openmpi/lib:$LD_LIBRARY_PATH
        export PATH=/opt/openmpi/bin:$PATH

* Run the test suite with P MPI processes, and N threads per MPI process

        export OMP_NUM_THREADS=[N]
        perl ./or_qa_script ../../exec/engine_linux64_gf_ompi --exec_script_args="mpiexec -np [P]"  1.0
