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
# Verify_results.py
# ----------------------------------------------------------------
# Library dedicated to extract the results from Engine output file
# and compare the results with the tolerance mechanism
# ----------------------------------------------------------------


def extract_engine_results(input_deck,run_number):
# ----------------------------------------------------------------
# Input:
#   input_deck : Input Deck name
#   run_number : Output run number
# Output:
#   Result dictionary of last explicit cycle
#   Result of "Normal Termination" check in file
# ----------------------------------------------------------------------------------
# Parse the Engine output file
# Get the last explicit cycle print & fill the results dictionary
# Seek for 'Normal Termination' in file
# ----------------------------------------------------------------
  results={}
  l_deck=len(input_deck)
  result=[]
  Normal_termination=0
  rootname=input_deck[0:l_deck-9]
  engine_output=rootname+'_'+str(run_number).zfill(4)+'.out'

  print("    Engine output file: ",engine_output)
  try :
    with open(engine_output) as outfile:
     while True:
      line=outfile.readline()
      if not line:
        break
      line=' '.join(line.split())
      fields=line.split(' ')

      if (len(fields) == 13 ):
        # Explicit cycle line has 13 fields, Error with % is on field 5
        error=fields[5]
        if (error[len(error)-1]=='%'):
          # Append to explicit cycle array
          result.append(line)

      if len(fields) == 2:
        if fields[0] == 'NORMAL' and fields[1] == 'TERMINATION':
          Normal_termination = 1
    outfile.close()  
  except:
    # Engine output file was not found
    print("")
    print("    *** ERROR Engine output file not found")
    return[],-1

  last=len(result)
  if last == 0:
     # Engine started but no cycles were made
     print("    *** ERROR No Engine Results found")
     return[],-1

  line = result[last-1]
  fields=line.split(' ')

  results['CYCLE']     = fields[0]
  results['TIME']      = fields[1]
  results['TSTEP']     = fields[2]
  results['ELEMENT']   = '0'
  results['ELTID']     = fields[4]
  results['ERROR']     = fields[5][:-1]     # Remove the '%' character
  results['IENERGY']   = fields[6] 
  results['KENERGYT']  = fields[7]
  results['KENERGYR']  = fields[8]
  results['EXTWORK']   = fields[9]
  results['MASERR']    = fields[10]
  results['TOTALMASS'] = fields[11]  
  results['ADDEDMASS'] = fields[12]  

  if Normal_termination==0:
    print('*** Normal Termination missing in ',engine_output)

  return results,Normal_termination

def extract_reference(ref):
# ----------------------------------------------------------------
# Input:
#   ref       : ref : name of reference file (ref_extract) 
# Output :
#   word : dictionary of all Keyworks in Red_extract
#   results: dictionany Keyword,result
# ----------------------------------------------------------------
# Read the Ref_extract file 
# fill the Reference dictionary
# ----------------------------------------------------------------
  results={}
  word=[]
  with open(ref) as ref_file:
    while True:
      line=ref_file.readline()
      if not line:
        break
      line=' '.join(line.split())
      fields=line.split(' ')

      if (fields[1]!='Start' and fields[1]!='Stop'):
        results[fields[1]]=fields[2]
        word.append(fields[1])
  return word,results


def compute_tolerance(ref,res,tolerance):
# ----------------------------------------------------------------------------------
# Input:
#   ref       : reference result
#   res       : run Result
#   tolerance : Simple dictionary with Tolerance name & tolerance value
# Output:
#   1|0 check Ok, Nok
#   string : with Tolerance info when check is Nok
# ----------------------------------------------------------------------------------
# Compute the tolerance for float results with Tolerance mechanism
# To formulas are supported
#    Diff_tolerance : Relative tolerance of Absolute values
#    Abs_tolerance  : Absolute Tolerance diff between Result & run must be in Range 
# ----------------------------------------------------------------------------------
  if ( tolerance[0] == 'Diff_tolerance'):
    # Diff_tolerance : relative tolerance : ABS (ref - res) / ABS(ref)  ) < tolerance
    tol = float(tolerance[1])
    diff = abs (ref-res)
    abs_ref = abs(ref)
    compute = diff / abs_ref
    string=''
    if ( compute < tol):
      return 1,string
    else:
      string=' ---- Tol : abs(Ref-Run)/abs(Ref) < Tol : '+str(compute)+' < '+str(tol)
      return 0,string

  if ( tolerance[0] == 'Abs_tolerance'):
    # Abs_tolerance : Absolute Tolerance : ABS (ref - res) < tolerance
    tol = float(tolerance[1])
    diff = abs (ref-res)
    string=''
    if ( diff < tol):
       return 1,string
    else:
       string=' ---- Tol : abs(Ref-Run) < Tol : '+str(diff)+' < '+str(tol)
       return 0,string


# In case tolerance is not well set / do a simple diff 
  string=''
  diff = abs(ref-res)
  if diff == 0.0:
    return 0,string
  else:
    string=' ---- Tol : abs(res-ref)==0' 

  return 0,string


def verify_integer_tolerance(word,ref,res,test_json):
# ----------------------------------------------------------------------------------
# word      : Name of Tolerance being checked (Integer = NCYCLE)
# ref       : reference result
# res       : run Result
# test_json : Json file with Tolerance if present in data directory
# ----------------------------------------------------------------------------------
# Compute the tolerance for Intger results with Tolerance mechanism
# Supported Tolerance
#    Abs_tolerance  : Absolute Tolerance diff between Result & run must be in Range 
# ----------------------------------------------------------------------------------
  if (test_json == []):
    return 0
  
  tol_word='TOL_'+word
  entry=test_json

  if ( tol_word in entry ):

    string=''
    tolerance = entry[tol_word]
    tolerance_type=tolerance[0]
    if (tolerance_type=="Abs_tolerance"):
       itol = int(tolerance[1])
       iref=int(ref)
       ires=int(res)
       diff = abs(iref-ires)
       if (diff < itol):
         return 1,string
       else:
         string=' ---- Tol : abs(Ref-Run)<Tol : ',str(diff),' > ',str(itol)
         return 0,string
    else:
      string=' ---- Tol : abs(Ref-Run)==0'
      return 0,string
    
  else:
    string=' ---- Tol : abs(Ref-Run)==0'
    return 0,string

def compare(words,reference,results,test_json,default_tol,prec):
# ----------------------------------------------------------------------------------
# compare:
# words       : Explicit cycke keywords from Engine output file
# reference   : ref_extract file in form of Dictionnary
# results     : last explicit cycle result in form of Dictionary
# test.json   : Json test file with Tolerance information if exist in directory
# default_tol : Default tolerance as in constants.json file
# prec        : run Precision : 'dp' for double Precision, 'sp' for single Precision
# ----------------------------
# Main comparision fonction 
# Compare the results / Compute Tolerances
# Print the Errors.
# ----------------------------------------------------------------------------------
  Error=''
  Error_found=0
  for word in words:
  
    if (word == 'CYCLE'):
       ref_cycle=int(reference[word])
       cycle=int(results[word])

       if (ref_cycle != cycle):
         result,tol_info = verify_integer_tolerance(word,ref_cycle,cycle,test_json)
         if (result == 0):
            cycle_error='    '+word.ljust(10,' ')+'  Ref: '+str(ref_cycle).ljust(12,' ')+' ----  Run: '+str(cycle)+tol_info+'\n'
            Error=Error+cycle_error
            Error_found = Error_found+1

    else:
      if (word=='ELEMENT' or word=='ELTID'):
         Error_found = Error_found+0
         #No checks on Element Type or EltID giving the timestep

      else:

        result = 0
        ref = float(reference[word])
        res = float(results[word])
        diff = abs(ref-res)


        # In single Precision Zerp_tol is set to a lower value closer
        # to the machine precision.
        if prec == 'sp':
          Zero_tol = float(default_tol['Zero_tolerance_SP'])
        else:
          Zero_tol = float(default_tol['Zero_tolerance'])

        # Zero_tol is close to the precision in Single or Double Precision.
        # Bellow this value digits are no more meaningfull.
        # skip if Both are bellow the precision or if the diff is also bellow.
        #
        # Magic : diff < 2* Zero_tol for very small numbers
        if not( (abs(ref) < Zero_tol and abs(res) < Zero_tol ) or diff < 2*Zero_tol ):

           if word=='IENERGY' or word=='KENERGYT' or word=='KENERGYR' or word=='EXTWORK':
           # --------------------------------------------------------------------------------------------   
           # Magic : 
           # An Energy value smaller than the MAX of Energies can be neglected: 
           #    When Reference value less than (MAX of Energies)*(Max_value_scale_factor) : 2%
           #    When difference is less than (MAX of Energies)*(Max_value_scale_factor_diff ) : 3%
           #    last check avoid reference to increase a lot.
              
              max_value=max(abs(float(reference['IENERGY'])),abs(float(reference['KENERGYT'])),abs(float(reference['KENERGYR'])),abs(float(reference['EXTWORK'])))
              scale=float(default_tol['Max_value_scale_factor'])
              diff_scale=float(default_tol['Delta_scale_factor'])

              if (abs(ref) < max_value * scale):
                if (diff < max_value * diff_scale):
                  result = 1
                else:
                  result = 0
           # --------------------------------------------------------------------------------------------
   
           if result == 0:
                # verify if a Tolerance is applied in the command file: test_file.json
                tol_word='TOL_'+word
                tol_word_sp='TOL_'+word+'_SP'
                entry=test_json

                if prec == 'sp' and (tol_word_sp in entry ):
                  # Specific Tolerance in single precision
                  # Found tolerance in json file copy it & give to compute_tolerance
                  # ---------------------------------------------------------------
                  tolerance=entry[tol_word_sp]
                  result,tol_info = compute_tolerance(ref,res,tolerance)

                elif ( tol_word in entry ):
                  # Tolerance common to Single & Double Precision
                  # Found tolerance in json file copy it & give to compute_tolerance
                  # ---------------------------------------------------------------
                  tolerance=entry[tol_word]
                  result,tol_info = compute_tolerance(ref,res,tolerance)
           
                else:
                  # Grab default Diff_tolerance from default_tolerances files
                  # ---------------------------------------------------------
                  if prec == 'sp':
                    tol = default_tol["Diff_tolerance_SP"]
                  else:
                    tol = default_tol["Diff_tolerance"]
                  
                  tolerance=["Diff_tolerance",tol]
                  result,tol_info = compute_tolerance(ref,res,tolerance)

           # Result = 0 -> difference detected
           # Result = 1 -> Within tolerance
           if (result == 0):
             Error_found = Error_found+1
             # Append Error line to Error print
             word_error='    '+word.ljust(10,' ')+'  Ref: '+reference[word].ljust(12,' ')+' ----  Run: '+str(res).ljust(12,' ')+tol_info+'\n'
             Error=Error+word_error

  if ( Error_found > 0):
    print(" ")
    print("    Differences found")
    print("    -----------------")
    print(Error)
  else:
    print(" ")
    print("    No differences found")
    print("    --------------------")

  return Error_found
