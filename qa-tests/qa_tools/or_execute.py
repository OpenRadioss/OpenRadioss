
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

# Main Entry : Executes Radioss Starter & Engine with a QA test
# -------------------------------------------------

import os
import sys
import shutil

# qa_tools_library
import qa_system
import execute_solver
import verify_results

arch,default_arch,slash,sep=qa_system.get_arch()

# ===========================================
# Main entry point
# ===========================================
if __name__ == "__main__":

  starter=""
  got_starter=0
  engine=""
  got_engine=0
  deck_dir=""
  got_qa_name=0
  got_starter_deck=0
  got_engine_deck=0
  qa_name=""
  test_id=""
  mpi='smp'
  prec='dp'
  exec_arch='built_in'
  run_starter=1
  run_engine=1
  np=1
  stdout = 0
  keep=0
  qa_type='default'

  # Parse command line arguments
  # -----------------------------
  arguments=sys.argv
  count = 0
  for arg  in arguments:
    count=count+1
    if(arg == '-starter'):
      starter=arguments[count]
      got_starter=1

    if(arg == '-engine'):
      engine=arguments[count]
      got_engine=1

    if(arg == '-deck_dir'):
      deck_dir=arguments[count]

    if(arg == '-qa_name'):
      qa_name=arguments[count]
      got_qa_name=1

    if(arg == '-starter_deck'):
      starter_deck=arguments[count]
      got_starter_deck=1

    if(arg == '-engine_deck'):
      engine_deck=arguments[count]
      got_engine_deck=1

    if(arg == '-test_id'):
      id=arguments[count]
      test_id='_'+arguments[count]

    if(arg == '-mpi'):
      mpi=arguments[count]

    if(arg == '-np'):
      np=int(arguments[count])

    if(arg == '-prec'):
      prec=arguments[count] 

    if(arg == '-arch'):
      exec_arch=arguments[count] 

    if(arg == '-stdout'):
      stdout=int(arguments[count])

    if(arg == '-qa_type'):
      qa_type=int(arguments[count])

    if(arg == '-keep'):
      keep=int(arguments[count])



  # ---------------------------------
 
  print("")
  print("QA Script")
  print("----------")
  print("")
  print('--- Run Number: ',id)
  Deck_mes='--- Deck Directory: '+deck_dir
  if prec == 'sp':
     print('--- Single Precision version tested')
  print(Deck_mes)

# Find Json File in data directory & read it
# -------------------------------------------
  deck_data='../'+deck_dir+'/data/'
  json_found,test_json=execute_solver.read_input_file_datas(deck_data)


  if (json_found ==1):
     
  # Some settings can be done in the Json:
  # search for them & apply the settings.

    if 'enabled' in test_json:
    # enabled=0 skip the test.
       enabled = test_json['enabled']
       if enabled == 0:
          print('')
          print('--- Test case disabled ! ')
          print('    Skiping') 
          print('')
          sys.exit(0)

    if 'enabled_sp' in test_json:
    # enabled_sp=0 skip the test when QA is single Precision.
       enabled = test_json['enabled_sp']
       if enabled == 0 and prec == 'sp':
          print('')
          print('--- Test case disabled in Single Precision ! ')
          print('    Skiping') 
          print('')
          sys.exit(0)

    if 'np' in  test_json:
    # Force -np to np value
       np=int( test_json['np'])
       print('--- # MPI Domains set to ',np)

    if 'run_starter' in test_json:
    # No Starter run
       run_starter=int( test_json['run_starter'])

    if 'run_engine' in test_json:
    # No Engine run
       run_engine=int( test_json['run_engine'])

  # Set -np 1 when run in SMP mode.
  if mpi == 'smp':
    np=1

# Load Default tolerances file
# -----------------------------
  default_tol = execute_solver.load_default_tolerances()

# Executable Starter & Engine
#-----------------------------
  default_execs = execute_solver.get_executables(mpi,prec,exec_arch)

  if (got_starter==0):
      starter = default_execs[0]

  if (got_engine==0):
      engine = default_execs[1]

  print('')
  print('--- Executables')
  Starter_mes='    Starter: ' + starter
  Engine_mes='    Engine:  ' + engine

  print(Starter_mes)
  print(Engine_mes)  
  print("")

# Check if executable exists
#----------------------------
  Errors=0
  if arch=='win64':
     exe_suf='.exe'
  else:
     exe_suf=''

  starter_check='..'+slash+'..'+slash+'exec'+slash+execute_solver.get_version_name(starter)+exe_suf
  engine_check='..'+slash+'..'+slash+'exec'+slash+execute_solver.get_version_name(engine)+exe_suf

  if not (os.path.isfile(starter_check) ):
      Errors=Errors+1
      print("--- Error :",execute_solver.get_version_name(starter)+exe_suf,' not found')
  if not (os.path.isfile(engine_check)):
      Errors=Errors+1
      print("--- Error :",execute_solver.get_version_name(engine)+exe_suf ,' not found')

  if Errors >0:
       sys.exit(Errors)

# Define Execution directory 
# --------------------------
  if (got_qa_name == 1):
    exec_directory='run_'+qa_name+'_'+test_id
  else:
    exec_directory='run_'+execute_solver.get_version_name(engine)+test_id
 
  Exec_mes='--- Execution directory: '+exec_directory
  print(Exec_mes)
  print("")


# Create execution directory & copy data in
# -----------------------------------------
  print("--- Create Execution directory & copy data")

  if os.path.isdir(exec_directory):
    print("    ** Exec directory exists. Deleting it")
    shutil.rmtree(exec_directory)
  
  deck_data='../'+deck_dir+'/data/'

  shutil.copytree(deck_data,exec_directory) 

  print("")
  print("--- Input Decks")


# Find Starter & Engine input deck
# --------------------------------
  engine_deck=[]

  # Either it is in [data_directory]/test_files.json
  # -------------------------------------------------
  if (json_found == 1):
    starter_deck,engine_deck=execute_solver.find_deck_names_in_json(test_json)
  else:

  # Or found it in data directories
  # -------------------------------
    decks = execute_solver.find_deck_names(deck_data)
    starter_deck=decks[0]
    engine_deck.append(decks[1])

  starter_deck_mess='    Starter Input deck: '+starter_deck
  engine_deck_mess='    Engine Input deck:  '
  print(starter_deck_mess)
  print(engine_deck_mess,engine_deck)

# Execute the Solver
# ------------------
  os.chdir(exec_directory)
  execute_solver.exec_openradioss(starter,run_starter,engine,run_engine,starter_deck,engine_deck,mpi,np,1,stdout)
  print('')

# Verify the results
# ------------------
  print('--- Verify Results')
  number_of_runs = len(engine_deck)
  Errors = 0

  if qa_type == 'default':
  # Default Radioss QA Check
  # Compare last cycle line with Reference
  # --------------------------------------

     print('    Listing Engine output check')
     # Get the last line infos from last engine output file
     # ---------------------------------------------------
     results,Normal_termination = verify_results.extract_engine_results(engine_deck[0],number_of_runs)
     if Normal_termination == 0:
        Errors = Errors + 1
     
     if Normal_termination == -1:
        Errors = Errors + 1
        qa_system.print_log_files('starter_output.log','engine_output.log') 

     else:
        # Get the lines from ref.extract
        # ------------------------------
        words,reference = verify_results.extract_reference('ref.extract')

        # Compare the results - get #Errors in return
        # --------------------------------------------
        Errors =  Errors + verify_results.compare(words,reference,results,test_json,default_tol,prec)
  
  if qa_type == 'qa_print':
     print('--- QA PRINT Verification TBD')
     Errors = Errors + 1

  os.chdir('..')

  if keep == 0:
    print('')
    print('--- clean up')
    print('')
    shutil.rmtree(exec_directory)
  

  sys.exit(Errors)
 