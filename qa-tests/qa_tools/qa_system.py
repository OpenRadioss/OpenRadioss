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
# qa_systems.py
# ----------------------------------------------------------------
# Some Global system routines
# ----------------------------------------------------------------
import os
import platform

def get_arch():
# -----------------------------------------------------------------------
# Output :
#   get_arch :
#      arch=linux64,linuxa64 or win64
#      default_arch : built_in executable name
#      slash='/' (linux) or '\' (Windows) : Slash for PATH creation
#      sep=':' (linux) or ';' (Windows) : PATH/LD_LIBRARY_PATH separator 
# -----------------------------------------------------------------------
  op_system=platform.system()
  cpu=platform.machine()

  if (cpu=='aarch64'):
     arch='linuxa64'
     default_arch='linuxa64'
     slash='/'
     separator=':'
  else:
     if(op_system=='Linux'):
       arch='linux64'
       default_arch='linux64_gf'
       slash='/'
       separator=':'
     else:
       arch='win64'
       default_arch='win64'
       slash='\\'
       separator=';'

  return arch,default_arch,slash,separator

def set_variable(variable,value):
# --------------------------------------------------
# Input:
#    variable : variable name
#    value : variable contents
# --------------------------------------------------
# Sets environment variable if not already existing
# --------------------------------------------------
  check_value=os.getenv(variable)
  if (check_value is None):
      os.environ[variable]=value


def print_log_files(starter_log_file,engine_log_file):
# -------------------------------------------------------
# Input:
#    starter_log_file : Starter Stdout+Stderr file
#    engine_log_file  : Engine  Stdout+Stderr file
# -------------------------------------------------------
# Print log files in Stdout
# -------------------------------------------------------
  print('------------------------------ Starter Output ------------------------------')
  try:
      with open(starter_log_file) as log_file:
        while True:
           line=log_file.readline()

           if not line:
            break
           print (line[:-1])
  except:
     print("    *** ERROR : No Starter Log")

  print('')
  print('------------------------------ Engine Output -------------------------------')

  try:
      with open(engine_log_file) as log_file:
        while True:
           line=log_file.readline()

           if not line:
            break
           print (line[:-1])
  except:
     print("    *** ERROR : No Engine Log")
  print('----------------------------------------------------------------------------')


def set_or_variables():
# -------------------------------------------------------
# Input:
#    threads : Set Threads for execution
# -------------------------------------------------------
# set environment variables for Radioss.
# -------------------------------------------------------
  arch,default_arch,slash,sep=get_arch()

  #
  # OMP_STACKSIZE
  set_variable('OMP_STACKSIZE','400m')

  #
  # RAD_CFG_PATH
  #
  rad_cfg_path=os.getcwd()+slash+'..'+slash+'..'+slash+'..'+slash+'hm_cfg_files'
  set_variable('RAD_CFG_PATH',rad_cfg_path)

  #
  # RAD_H3D_PATH
  #
  rad_h3d_path=os.getcwd()+slash+'..'+slash+'..'+slash+'..'+slash+'extlib'+slash+'h3d'+slash+'lib'+arch
  set_variable('RAD_H3D_PATH',rad_h3d_path)

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


def get_rootname(input_file):
#
# Input file : 
# Get the input format : rad or dyn
# The input deck rootname  
#
   deck_type='unknown'
   rootname='-'

   deck_suf=input_file.split('.')
   len_deck=len(deck_suf)
   if len_deck >= 2:
       suff=deck_suf[len_deck-1]
       if suff == 'rad':
           deck_type='rad'
           rootname=input_file[0:len(input_file)-9]

       if suff == 'k' or suff == 'key':
           deck_type='dyn'
           rootname=deck_suf[0]
           
   return rootname

def add_checksum_option(engine_file):
    #rename file
    tmp_file=engine_file+'_tmp'
    try:
       os.rename(engine_file,tmp_file)
    except:
       print('--- Error: '+engine_file+' not found' )

    with open (engine_file,"w") as write_file:
         write_file.write('/DEBUG/CHKSM/10\n')
         with open(tmp_file,"r") as read_file:     
           while True:
             line=read_file.readline()
             if not line:
               break 
             write_file.write(line)

    write_file.close()
    read_file.close()

   
      
