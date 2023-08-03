#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys
import os
import shutil
import json
import subprocess

##### TO RUN THIS SCRIPT please define the following environement variables:
# OMP_NUM_THREADS
# QA_NB_PROC
# LD_LIBRARY_PATH (incl hm reader lib and mpiexec lib path)
# PATH (incl mpiexec path)
# OMP_STACKSIZE
# RAD_CFG_PATH
# Optional : ASAN_OPTIONS

### AS an example:
# export OMP_NUM_THREADS=2; export QA_NB_PROC=4; export LD_LIBRARY_PATH="/work/mquinzin/GIT_Workspaces/GITHUB/mquinzin/OpenRadioss/extlib/hm_reader/linux64:/opt/openmpi/lib:${LD_LIBRARY_PATH}"; export PATH=/opt/openmpi/bin:${PATH} export OMP_STACKSIZE="400m"; export RAD_CFG_PATH="/work/mquinzin/GIT_Workspaces/GITHUB/mquinzin/OpenRadioss/hm_cfg_files"; export DISABLE_COPY_ENGINE_AT_STARTER_EXEC=1

# ===========================================
# Global variables definition
# ===========================================
scriptdir = os.path.dirname(sys.argv[0])
data_dir_name = 'data'

mpirun = 'mpiexec'
nbprocs = os.environ['QA_NB_PROC']

# ===========================================
# Functions definition
# ===========================================

def remove_directory(directory_path):
    try:
        # Use shutil.rmtree() to remove the directory and its contents
        shutil.rmtree(directory_path)
        # print(f"Directory '{directory_path}' and its contents have been removed successfully.")
    except OSError as e:
        print(f"Error: {e}")

def copy_directory(source_directory, destination_directory):
    try:
        # Use shutil.copytree() to recursively copy the directory and its contents
        shutil.copytree(source_directory, destination_directory)
        # print(f"Directory '{source_directory}' and its contents have been copied to '{destination_directory}'.")
    except OSError as e:
        print(f"Error: {e}")

def run_shell_command(command):
    try:
        # Use subprocess.run() to run the shell command
        result = subprocess.run(command, shell=True, capture_output=True, text=True)

        # Get the standard output of the command as a string
        output = result.stdout.strip()

        # Get the return code of the command
        return_code = result.returncode

        return output, return_code
    except subprocess.CalledProcessError as e:
        print(f"Error: {e}")
        return None, None


def read_input_file_datas():
    try:
        # Open the JSON file and load its contents into a Python dictionary
        with open('test_files.json', 'r') as json_file:
            return json.load(json_file)
    except IOError as e:
        print(f"Error: {e}")

# ===========================================
# Main entry point
# ===========================================

if __name__ == "__main__":

    # Prepare a temporary directory with input files to run the test
    qa_scripts_dir = os.getcwd() + '/scripts'
    exec_dir = os.getcwd() + '/../exec'
    data_dir = scriptdir + '/'  + data_dir_name
    temp_data_dir = data_dir + '_tmp'
    if os.path.exists(temp_data_dir):
        remove_directory(temp_data_dir)
    copy_directory(data_dir,temp_data_dir)

    # Go in it
    try:
        # Use os.chdir() to change the current working directory
        os.chdir(temp_data_dir)
    except OSError as e:
        print(f"Error: {e}")

    # Find starter and engine from a json file 
    input_files = read_input_file_datas()

    # Run starter
    command_to_run = qa_scripts_dir + '/or_radioss.pl 0 ' + exec_dir + '/engine_linux64_gf_ompi ' + mpirun + ' -np ' + str(nbprocs) + ' ' + input_files['starter'] + ' -starter --ignore_check_errors=0 last_go=1'
    output, return_code = run_shell_command(command_to_run)
    print(output)

    if return_code:
        print ("STARTER FAILED (code " + str(return_code) + ")")
        sys.exit(return_code)

    # Run engine
    if 'engines' in input_files:
        cpt = 1
        nb_engine = len(input_files['engines'])
        for engine in input_files['engines']:
            last_go = 1
            print ("Compare " + str(cpt) + " vs " + str(nb_engine))
            if cpt == nb_engine:
                last_go = 0
            command_to_run = qa_scripts_dir + '/or_radioss.pl 0 ' + exec_dir + '/engine_linux64_gf_ompi ' + mpirun + ' -np ' + str(nbprocs) + ' ' + engine + ' -engine --ignore_check_errors=0 last_go=' + str(last_go)
            output, return_code = run_shell_command(command_to_run)
            print (output)

            if return_code:
                print ("ENGINE #" + str(cpt) + " FAILED (code " + str(return_code) + ")")
                sys.exit(return_code)
            
            cpt += 1

    if os.path.exists(temp_data_dir):
        remove_directory(temp_data_dir)