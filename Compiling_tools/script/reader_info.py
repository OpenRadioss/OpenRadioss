import subprocess
import os
import sys
from datetime import datetime

def get_sha():
  my_repository = os.getcwd()
  try:
     sha1 = subprocess.check_output(['git', 'log', '--no-merges','-1','--format=%H' ], cwd=my_repository).decode('ascii').strip()
  except : 
     sha1 = "00000000"
  return sha1[0:8]

# ===========================================
# Main entry point
# ===========================================

if __name__ == "__main__":

  for arg in sys.argv:
    argument=arg.split('=')
    if(argument[0] == '-outfile'):
      size=len(argument)
      if (size == 2):
        outfile=argument[1]

  #Git Sha in shor format
  git_sha=get_sha()

  # Date and time in format YYMMDDHHMM 
  now = datetime.now()
  dt_string = now.strftime("%Y%m%d%H%M%S")

  build_id=dt_string+'_'+git_sha

  with open(outfile,'w') as include_file:
    include_file.write("// Library ID - generated during build process\n")
    include_file.write("#define BUILD_ID \"or_"+build_id+"\"\n")




