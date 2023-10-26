# Copyright>        OpenRadioss
# Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
# Copyright>    
# Copyright>        This program is free software: you can redistribute it and/or modify
# Copyright>        it under the terms of the GNU Affero General Public License as published by
# Copyright>        the Free Software Foundation, either version 3 of the License, or
# Copyright>        (at your option) any later version.
# Copyright>    
# Copyright>        This program is distributed in the hope that it will be useful,
# Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
# Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# Copyright>        GNU Affero General Public License for more details.
# Copyright>    
# Copyright>        You should have received a copy of the GNU Affero General Public License
# Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
# Copyright>    
# Copyright>    
# Copyright>        Commercial Alternative: Altair Radioss Software 
# Copyright>    
# Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss 
# Copyright>        software under a commercial license.  Contact Altair to discuss further if the 
# Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    
#

# -----------------------------------------------------------------------
# functions to determine Starter & Engine command lines to launch a test
# Execute Starter & Engine
# -----------------------------------------------------------------------

import os
import json
# qa_tools libraries
import qa_system

arch,default_arch,slash,sep=qa_system.get_arch()

def load_default_tolerances():
# -----------------------------------------------------------
# Output : 
#   constants.json  : default constants file
# -----------------------------------------------------------
# Read constants.json file : default tolerances, Zero values...
# returns constants.json file.
# -----------------------------------------------------------
    file=os.getcwd()+slash+".."+slash+"qa_tools"+slash+'constants.json'
    try:
        # Open the JSON file and load its contents into a Python dictionary
        with open(file, 'r') as json_file:
            return json.load(json_file)
    except IOError as e:
            return {}

def read_input_file_datas(data_directory):
# ---------------------------------------------------
# Input :
#   data_directory : directory of Data test file
# Output :
#   1|0 : file found or not.
#   test_files.json file or []
# ---------------------------------------------------
# Read Json file : test_files.json in data directory
# If exists : returns 1 + json structure
# If not    : returns 0 + empty json 
# ---------------------------------------------------
    file=data_directory+slash+'test_files.json'
    try:
        # Open the JSON file and load its contents into a Python dictionary
        with open(file, 'r') as json_file:
            return 1,json.load(json_file)
    except IOError as e:
            return 0,[]

def find_deck_names_in_json(test_file):
  entry=test_file
  starter_deck=entry['starter']
  engine_decks=entry['engines']
  return starter_deck,engine_decks


def find_deck_names(data_directory):
# ---------------------------------------------------
# Input :
#   data_directory : directory of Data test file
# Output :
#   Array with Starter_input_deck, Engine input Decks
# ---------------------------------------------------
# Get input deck names by reading the directory
# Applied if no json file was found.
# ----------------------------------------------
  got_starter_deck=0
  got_engine_deck=0

  file_list=os.listdir(data_directory)
  for file in file_list:
    part=file.split('_') 
    len_part=len(part)

    if (part[len_part-1] == '0000.rad'):
      got_starter_deck=1
      starter_deck=file

    if (part[len_part-1] == '0001.rad'):
      got_engine_deck=1
      engine_deck=file

  if (got_starter_deck == 0):
    result=['Error','Error']
  else:
    result=[starter_deck]
    if (got_engine_deck==1):
      result.append(engine_deck)
    else:
      # No Engine input file - Probably common Starter/Engine input file
      result.append(starter_deck)

  return result

def get_version_name(dir_exec):
# ------------------------------------------------------------------
# Input : 
#   Executable with its Path.
# Output :
#   Executable name
# ------------------------------------------------------------------
# When Path to Starter  Engine is given : find the executable names
# Get version name
# Extract from engine path+name the version for directory name
# eG: ..\..\exec\engine_win64_impi becomes engine_win64_impi
# --------------------------------------------------------------
  fields=dir_exec.split(slash)
  size=len(fields)
  return fields[size-1]


def get_executables(mpi,prec,exec_arch):
# ------------------------------------------------------------------
# Input : 
#   mpi : which mpi was defined in or_execute.py command line
#   prec : precision defined in or_execute.py command line
#   exec_arch : result of -arch from or_execute.py or built_in 
# Output :
#   Executable names Array
# -------------------------------
# Get built_in executables names
# Return executables array :
# executable[0] = starter
# executable[1] = engine
# -------------------------------
  executable=''
  sp=''
  if prec == 'sp':
     sp='_sp'

  # grab arch
  # ---------
  if exec_arch == 'built_in':
     parch=default_arch
  else:
     parch=exec_arch

  # name according to mpi
  # ---------------------
  if mpi=='smp':
     pmpi=''
  else:
     pmpi='_'+mpi

  starter='..'+slash+'..'+slash+'..'+slash+'exec'+slash+'starter_'+parch+sp
  engine='..'+slash+'..'+slash+'..'+slash+'exec'+slash+'engine_'+parch+pmpi+sp
  executable=[starter]
  executable.append(engine)
  
  return executable

def get_starter_command(starter,starter_input,np,starter_arg):
# ------------------------------------------------------------------
# Input : 
#   starter : executable name
#   starter_input : Starter Input Deck
# ------------------------------------------------------------------
# Create Starter Command line
# ----------------------------
# starter: starter executable name
# starter_input: Starter input deckname
# np: Number of MPI domains
#
  if (arch == 'win64'):
    redirect=' > starter_output.log 2>&1'
  else:
     redirect=' > starter_output.log 2>&1'
  
  starter_command=starter+' -i '+ starter_input +' -np '+str(np) + ' ' + starter_arg + redirect
  return starter_command

def get_engine_command(engine,mpi,engine_input,np):
# ------------------------------------------------------------------
# Input : 
#   engine : executable name
#   mpi    : smp|impi|ompi
#   engine_input : Starter Input Deck
#   np     : Number of MPI Domains
# ------------------------------------------------------------------
# Create Engine command line
# --------------------
# Create engine command line :
# engine:        engine executable name
# mpi:           'smp', 'impi', 'ompi'
# engine_input:  input deckname
# np:            Number MPI domains
#
  if (arch == 'win64'):
    redirect=' > engine_output.log  2>&1'
    if (mpi =='impi' ):
      command='mpiexec -delegate -np '+str(np)+' ' + engine + ' -i '+engine_input + redirect
    else:
      command=engine + ' -i '+engine_input + redirect
  else:
    redirect=' > engine_output.log 2>&1'
    if (mpi == 'smp' ):
      command=engine + ' -i '+engine_input + redirect
    else:
      command='mpirun -np '+str(np) + ' '+engine + ' -i '+engine_input  + redirect
  return command


def exec_openradioss(starter,starter_arg,run_starter,engine,run_engine,starter_deck,engine_decks,mpi,np,nt,stdout):
# ------------------------------------------------------------------
# Input : 
#   starter      : Starter executable name
#   run_starter  : 1|0 - flag if Starter must be run
#   engine       : Engine executable name
#   run_engine   : 1|0 - flag if Engine must be run
#   starter_deck : Starter Input Deck
#   Engine_deck  : Engine input Deck
#   mpi          : smp|impi|ompi
#   np           : Number of MPI Domains
#   nt           : Number of Threads
#   stdout       : -stdout / Verbose mode in or_execute.py
# ------------------------------------------------------------------
# Execute OpenRadioss Starter & Engine
# ------------------------------------

  # Environment Variable setting
  #-----------------------------
  # OMP_NUM_THREADS
  qa_system.set_variable('OMP_NUM_THREADS',str(nt))
  #
  # RAD_CFG_PATH
  #
  rad_cfg_path=os.getcwd()+slash+'..'+slash+'..'+slash+'..'+slash+'hm_cfg_files'
  qa_system.set_variable('RAD_CFG_PATH',rad_cfg_path)

  #
  # RAD_H3D_PATH
  #
  rad_h3d_path=os.getcwd()+slash+'..'+slash+'..'+slash+'..'+slash+'extlib'+slash+'h3d'+slash+'lib'+arch
  qa_system.set_variable('RAD_H3D_PATH',rad_h3d_path)

  #
  # PATH/LD_LIBRARY_PATH : add parth to extlib in PATH
  #
  if arch == 'win64':
    path=os.getenv('PATH')
    wd=os.getcwd()
    hm_reader_path=wd+slash+'..'+slash+'..'+slash+'..'+slash+'extlib'+slash+'hm_reader'+slash+arch
    new_path=hm_reader_path+sep+path
    os.environ['PATH']=new_path
  else:
    wd=os.getcwd()
    hm_reader_path=wd+slash+'..'+slash+'..'+slash+'..'+slash+'extlib'+slash+'hm_reader'+slash+arch
    try:
      path=os.getenv('LD_LIBRARY_PATH')
      new_path=hm_reader_path+sep+path
    except:
      new_path=hm_reader_path
    os.environ['LD_LIBRARY_PATH']=new_path

  print("")
  if run_starter == 1:
     print("--- Executing Starter")
     starter_command=get_starter_command(starter,starter_deck,np,starter_arg)
     p_starter_command='    '+starter_command
     print(p_starter_command)
     os.system(starter_command)

     if stdout == 1:
        print('')
        with open('starter_output.log') as outfile:
          while True:
           line=outfile.readline()
           if not line:
             break
           print(line[:-1])

  else:
     print("--- Skip Starter")
  print("")

  if run_engine == 1:
     print("--- Executing Engine")
     for deck in engine_decks:
        engine_command=get_engine_command(engine,mpi,deck,np)
        p_engine_command='    '+ engine_command
        print(p_engine_command)
        os.system(engine_command)

        if stdout == 1:
           print('')
           with open('engine_output.log') as outfile:
             while True:
               line=outfile.readline()
               if not line:
                 break
               print(line[:-1])
           outfile.close()

  else:
     print("--- Skip Engine")  

  

  

