import sys
import os

# Creates the message include from text message file
# -------------------------------------------------------------------
# The include is an array : each non empty line is an entry in array
# ===========================================
# Main entry point
# ===========================================
if __name__ == "__main__":

# initialization
  inputfile='infile.txt'
  outfile='outfile.txt'

  # Parse argument lists
  # --------------------
  for arg in sys.argv:

    argument=arg.split('=')

    if(argument[0] == '-outfile'):
      size=len(argument)
      if (size == 2):
        outfile=argument[1]

    if (argument[0] == '-inputfile'):

      size=len(argument)
      if (size == 2):
        inputfile=argument[1]

print(" ")
print(" Create include file from Message Text files ")
print("---------------------------------------------")
print("InputFile:",inputfile)
print("OutputFile:",outfile)
print("---------------------------------------------")

f_out = open(outfile,'w')
f_in  = open(inputfile,'r')

count=0
# First count the number of entries in array
for line in f_in:
   line_s=line.rstrip()
   line_len=len(line_s)
   if (line[0]!='#' and line_len>1):
     count=count+1

# write number of lines in array
headline='      CHARACTER(LEN=ncharline),DIMENSION(:),ALLOCATABLE :: MESSAGESDATA\n'
f_out.write(headline)
headline='      ALLOCATE(MESSAGESDATA('+str(count)+'))\n'
f_out.write(headline)


#rewind iput file and proceed with writing
f_in.seek(0)
count=0
for line in f_in:
   
   #remove spaces and \n at end of string
   line_s=line.rstrip()
   line_len=len(line_s)
   if (line[0]!='#' and line_len>1):
     count=count+1
     #character ' is treated as ''
     line_s=line_s.replace('\'','\'\'')
     write_line1='      MESSAGESDATA('+str(count)+')=\n'
     write_line2='     .\''+ line_s +'\'\n'
     f_out.write(write_line1)
     f_out.write(write_line2)
sizeline='      SMESSAGESFILE='+str(count)+'\n'
f_out.write(sizeline)

f_out.close()
f_in.close
