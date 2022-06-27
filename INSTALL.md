# How to run OpenRadioss and How to run the MiniQA

OpenRadioss is made of :

* OpenRadioss Starter
* OpenRadioss Engine
* 3 dynamic libraries
* A set of configuration files to Read input : in hm_cfg_files directory

Usually there is a Starter and Engine input deck.

## Running OpenRadioss from OpenRadioss source directory

#### Environment variable setting for Radioss

        export OpenRadioss_path=[OpenRadioss Root directory]
        export RAD_CFG_PATH=$OpenRadioss_path/hm_cfg_files
        export LD_LIBRARY_PATH=$OpenRadioss_path/extlib/hm_reader/linux64/:$OpenRadioss_path/extlib/h3d/lib/linux64/:$LD_LIBRARY_PATH
        export OMP_STACKSIZE=400m

#### Running Radioss from OpenRadioss directories in Radioss SMP

* Define number of cores to execute Radioss With N threads

        export OMP_NUM_THREADS=N

* Execute Radioss Starter and Engine

        $OpenRadioss_path/exec/starter_linux64_gf -i [Starter input file] -np 1       
        $OpenRadioss_path/exec/engine_linux64_gf -i [Engine input file]
 
#### Running Radioss from OpenRadioss directories in Radioss Hybrid SMP / SPMD

* Environment variables to setup OpenMPI built and installed in /opt/openmpi

        export LD_LIBRARY_PATH=/opt/openmpi/lib:$LD_LIBRARY_PATH
        export PATH=/opt/openmpi/bin:$PATH

* Execute Radioss with P MPI process and N threads per domain 

        export OMP_NUM_THREADS=N
        $OpenRadioss_path/exec/starter_linux64_gf -i [Starter input file] -np P        
        mpiexec -n P $OpenRadioss_path/exec/engine_linux64_gf_ompi -i [Engine input file]


## Running OpenRadioss download from GIT releases

#### Environment variable setting for Radioss

        export OpenRadioss_path=[OpenRadioss directory]
        export RAD_CFG_PATH=$OpenRadioss_path/hm_cfg_files
        export LD_LIBRARY_PATH=$OpenRadioss_path/lib/:$LD_LIBRARY_PATH
        export LD_LIBRARY_PATH=$OpenRadioss_path/bin:$PATH
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

        export OpenRadioss_path=[OpenRadioss Root directory]
        export RAD_CFG_PATH=$OpenRadioss_path/hm_cfg_files
        export LD_LIBRARY_PATH=$OpenRadioss_path/extlib/hm_reader/linux64/:$OpenRadioss_path/extlib/h3d/lib/linux64/:$LD_LIBRARY_PATH
        export OMP_STACKSIZE=400m

* Enter qa_test/scripts directory
        cd $OpenRadioss_path/qa-tests/scripts

#### Running with SMP binary

        perl ./or_qa_script ../../exec/engine_linux64_gf 1.0

#### Running with OpenMPI binary


* Environment variables to setup OpenMPI built and installed in /opt/openmpi

        export LD_LIBRARY_PATH=/opt/openmpi/lib:$LD_LIBRARY_PATH
        export PATH=/opt/openmpi/bin:$PATH

* Executing the QA with P MPI domains
        perl ./or_qa_script ../../exec/engine_linux64_gf_ompi --exec_script_args="mpiexec -np P"  1.0





