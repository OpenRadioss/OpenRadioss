# How to run OpenRadioss and How to run the MiniQA

OpenRadioss is made of :

* OpenRadioss Starter
* OpenRadioss Engine
* 3 dynamic libraries
* A set of configuration files to Read input : in hm_cfg_files directory

Usually there is a Starter and Engine input deck.

## Running OpenRadioss from OpenRadioss source directory

#### Environment variable setting for Radioss

        export OPENRADIOSS_PATH=[OpenRadioss Root directory]
        export RAD_CFG_PATH=$OPENRADIOSS_PATH/hm_cfg_files
        export LD_LIBRARY_PATH=$OPENRADIOSS_PATH/extlib/hm_reader/linux64/:$OPENRADIOSS_PATH/extlib/h3d/lib/linux64/:$LD_LIBRARY_PATH
        export OMP_STACKSIZE=400m

#### Running Radioss from OpenRadioss directories in Radioss SMP

* Define number of cores to execute Radioss With N threads

        export OMP_NUM_THREADS=N

* Execute Radioss Starter and Engine

        $OPENRADIOSS_PATH/exec/starter_linux64_gf -i [Starter input file] -np 1       
        $OPENRADIOSS_PATH/exec/engine_linux64_gf -i [Engine input file]
 
#### Running Radioss from OpenRadioss directories in Radioss Hybrid SMP / SPMD

* Environment variables to setup OpenMPI built and installed in /opt/openmpi

        export LD_LIBRARY_PATH=/opt/openmpi/lib:$LD_LIBRARY_PATH
        export PATH=/opt/openmpi/bin:$PATH

* Execute Radioss with P MPI process and N threads per domain 

        export OMP_NUM_THREADS=N
        $OPENRADIOSS_PATH/exec/starter_linux64_gf -i [Starter input file] -np P        
        mpiexec -n P $OPENRADIOSS_PATH/exec/engine_linux64_gf_ompi -i [Engine input file]


## Running OpenRadioss download from GIT releases

#### Environment variable setting for Radioss

        export OPENRADIOSS_PATH=[OpenRadioss directory]
        export RAD_CFG_PATH=$OPENRADIOSS_PATH/hm_cfg_files
        export LD_LIBRARY_PATH=$OPENRADIOSS_PATH/lib/:$LD_LIBRARY_PATH
        export PATH=$OPENRADIOSS_PATH/bin:$PATH
        export OMP_STACKSIZE=400m

#### Launching OpenRadioss SMP executable
     
        starter_linux64_gf -i [Starter input file] -np 1       
        engine_linux64_gf -i [Engine input file]

#### Launching OpenRadioss in Radioss Hybrid SMP / SPMD
* Environment variables to setup OpenMPI built and installed in /opt/openmpi

        export LD_LIBRARY_PATH=/opt/openmpi/lib:$LD_LIBRARY_PATH
        export PATH=/opt/openmpi/bin:$PATH

* Execute Radioss with P MPI process and N threads per domain 

        export OMP_NUM_THREADS=N
        starter_linux64_gf -i [Starter input file] -np P        
        mpiexec -n P engine_linux64_gf_ompi -i [Engine input file]


## Running OpenRadioss QA 

* Set OpenRadioss Environment variables

        export OPENRADIOSS_SOURCE_PATH=[OpenRadioss Root directory]
        export RAD_CFG_PATH=$OPENRADIOSS_SOURCE_PATH/hm_cfg_files
        export LD_LIBRARY_PATH=$OPENRADIOSS_SOURCE_PATH/extlib/hm_reader/linux64/:$OPENRADIOSS_SOURCE_PATH/extlib/h3d/lib/linux64/:$LD_LIBRARY_PATH
        export OMP_STACKSIZE=400m

* Enter qa_test/scripts directory

        cd $OPENRADIOSS_SOURCE_PATH/qa-tests/scripts

#### Running with SMP binary

        perl ./or_qa_script ../../exec/engine_linux64_gf 1.0

#### Running with OpenMPI binary


* Environment variables to setup OpenMPI built and installed in /opt/openmpi

        export LD_LIBRARY_PATH=/opt/openmpi/lib:$LD_LIBRARY_PATH
        export PATH=/opt/openmpi/bin:$PATH

* Executing the QA with P MPI domains and N threads per domain

        export OMP_NUM_THREADS=N
        perl ./or_qa_script ../../exec/engine_linux64_gf_ompi --exec_script_args="mpiexec -np P"  1.0





