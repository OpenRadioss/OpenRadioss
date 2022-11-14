echo off

if not exist ..\..\..\exec (
  echo "--- Creating exec directory"
  mkdir ..\..\..\exec
)

cl -DWIN32 /Fe..\..\..\exec\anim_to_vtk_win64.exe ..\src\anim_to_vtk.cpp Ws2_32.lib

del *.obj
