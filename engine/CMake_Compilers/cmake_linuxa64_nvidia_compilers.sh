# Fixed compiler switches for linuxa64 NVIDIA HPC SDK binary
# Priority:
#   1) NVHPC environment variable
#   2) local install in ~/nvidia
#   3) system install in /opt
NVHPC_ROOT=${NVHPC:-$HOME/nvidia/Linux_aarch64/26.3}
if [ ! -x "${NVHPC_ROOT}/compilers/bin/nvfortran" ]
then
	NVHPC_ROOT=/opt/nvidia/hpc_sdk/Linux_aarch64/26.3
fi

# Avoid header contamination from other compiler environments (e.g. Arm Compiler for Linux)
unset CPATH
unset C_INCLUDE_PATH
unset CPLUS_INCLUDE_PATH
unset OBJC_INCLUDE_PATH
unset INCLUDE

Fortran_comp=${NVHPC_ROOT}/compilers/bin/nvfortran
C_comp=${NVHPC_ROOT}/compilers/bin/nvc
CXX_comp=${NVHPC_ROOT}/compilers/bin/nvc++
CPP_comp=${NVHPC_ROOT}/compilers/bin/nvc++
