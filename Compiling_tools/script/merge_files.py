import sys
import os
# ===========================================
# Main entry point
# ===========================================
if __name__ == "__main__":

# initialization
  file1="none_1"
  file2="none_2"
  outfile='outfile.txt'

  # Parse argument lists
  for arg in sys.argv:

    argument=arg.split('=')

    if(argument[0] == '-outfile'):
      size=len(argument)
      if (size == 2):
        outfile=argument[1]

    if (argument[0] == '-file1'):

      size=len(argument)
      if (size == 2):
        file1=argument[1]

    if(argument[0] == '-file2'):
      size=len(argument)
      if (size == 2):
        file2=argument[1]

print('')
print('Merging\n',file1,'\nand\n',file2,'\ninto\n',outfile)

with open(outfile,'w') as f_outfile:
    with open(file1,'r') as f_file1:
        for  line in f_file1:
            f_outfile.write(line)
    f_file1.close()

    with open(file2,'r') as f_file2:
        for  line in f_file2:
            f_outfile.write(line)
    f_file2.close()

f_outfile.close()

    
         

           