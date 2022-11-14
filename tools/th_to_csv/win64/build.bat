echo off

if not exist ..\..\..\exec (
  echo "--- Creating exec directory"
  mkdir ..\..\..\exec
)

cl /Fe..\..\..\exec\th_to_csv_win64.exe ..\src\th_to_csv.c

del *.obj
