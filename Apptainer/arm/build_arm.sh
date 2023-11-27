#!/bin/bash

function build_OpenRadioss(){
  apptainer build openradioss_arm.sif openradioss_arm.def
}

function build_dev_environment(){
  apptainer build arm_buildEnv.sif arm_buildEnv.def
}

# Check if file filename.sif exists
if [ -f "arm_buildEnv.sif" ]; then
    # Run command 2
    echo "Build environment exists. Building OpenRadioss image..."
    build_OpenRadioss

else
    # Build development environment image first
    echo "Build environment image not found. Building first..."
    build_dev_environment
    
    if [[ $? -ne 0 ]]; then
      # Build failed, exit with an error
      echo "Error creating image. Review console output."
      exit 1
    fi
    build_OpenRadioss
fi
