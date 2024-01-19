import sys
import os
# ===========================================
# Main entry point
# ===========================================

if __name__ == "__main__":

  # initialization
  arch="none"
  git_sha=""
  msg=" "
  outfile="build_info.inc"

  
  # Parse argument lists
  for arg in sys.argv:

    argument=arg.split('=')

    if(argument[0] == '-outfile'):
      size=len(argument)
      if (size == 2):
        outfile=argument[1]


    if (argument[0] == '-arch'):

      size=len(argument)
      if (size == 2):
        arch=argument[1]

    if(argument[0] == '-msg'):
      size=len(argument)
      if (size == 2):
        msg=argument[1]

  arch_len=len(arch)
  line='       DATA BNAME/\"'+arch+'\"/\n'
  line_len='       DATA LENBNAM/'+str(arch_len)+'/\n'
  msg_line='       DATA  MSGO/\''+msg+'\'/\n'

  #grep GITHUB_SHA variable
  git_sha=os.getenv('GITHUB_SHA','')
  if ( len(git_sha) > 0): 
      btag='       DATA BTAG/\'CommitID: '+git_sha+'\'/\n'
  else:
      btag='       DATA BTAG/\' \'/\n'

  print("")
  print("Create:",outfile)
  print("")

  with open(outfile,'w') as include_file:
    include_file.write("       CHARACTER VERS*32, BDATE*32, BTIME*32,BTAG*80, MSGO*68,BNAME*32\n")
    include_file.write("       CHARACTER YEARSTRING*11\n")
    include_file.write("       INTEGER LEN_VERS, LEN_BDATE, LEN_BTIME, PMSG,LEN_MSG\n")
    include_file.write("       INTEGER LENBNAM\n")
    include_file.write("\n")
    include_file.write("       DATA VERS/\"OpenRadioss\"/\n")
    include_file.write("       PARAMETER (LEN_VERS=11)\n")
    include_file.write("\n")
    include_file.write("       DATA BDATE/__DATE__/\n")
    include_file.write("\n")
    include_file.write("       DATA BTIME/__TIME__/\n")
    include_file.write("\n")
    include_file.write(line)
    include_file.write(line_len)
    include_file.write("\n")
    include_file.write(btag)
    include_file.write("       PARAMETER (PMSG=0)\n")
    include_file.write(msg_line)
    include_file.write("       PARAMETER (LEN_MSG=1)\n")
