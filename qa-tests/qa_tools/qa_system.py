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


 