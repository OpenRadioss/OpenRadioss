@echo OFF

setlocal

:: Variable setting
set arch=win64
set verbose=
set threads=1
set clean=0


:: Loop over arguments

:ARG_LOOP
IF (%1) == () GOTO END_ARG_LOOP

   IF %1==-arch (
       set arch=%2
    )

   IF %1==-verbose (
       set verbose=-v
   )

   IF %1==-nt (
      set threads=%2
   )

   IF %1==-clean (
       set clean=1
   )

    IF %1==-help (
        GOTO HELP
    )

SHIFT
GOTO ARG_LOOP

:END_ARG_LOOP

:: Load external libraries
echo Load external libraries
python ..\Compiling_tools\script\load_extlib.py

:: build directory
set build_directory=cbuild_%arch%

:: clean
if %clean% == 1 (
  echo.
  echo Cleaning %build_directory%
  RMDIR /S /Q %build_directory%
  goto END
)


:: Create build directory
if exist %build_directory% (
  cd  %build_directory%
) else (
  mkdir %build_directory%
  cd  %build_directory%
)


echo.
echo  Build OpenReader 
echo  -----------------
echo  Build Arguments :
echo  arch =                 : %arch%
echo. 
echo  threads =              : %threads%
echo.


set build_type=Release 
cmake  -G Ninja -DCMAKE_CXX_COMPILER=cl -DCMAKE_C_COMPILER=cl -DCMAKE_BUILD_TYPE=%build_type% -Darch=%arch%  ..
if errorLevel=1 (
  echo.
  echo.
  echo Errors in CMAKE configuration !!!
  echo.
  cd ..
  endlocal
  exit /b 1
)

ninja -j %threads% %verbose%
if errorLevel=1 (
  echo.
  echo.
  echo Errors in CMAKE configuration !!!
  echo.
  cd ..
  endlocal
  exit /b 1
)

GOTO END

:HELP

  echo.
  echo  open_reader build_script
  echo  ------------------------
  echo.
  echo  Use with arguments :
  echo  -arch=[build architecture]
  echo           win64  : X86_64 Windows
  echo.
  echo  Execution control 
  echo  -nt=[threads]      : number of threads for build
  echo  -verbose           : Verbose build
  echo  -clean             : clean build directory
  echo. 
  echo  -help              : print this help
  echo.
  echo.

:END
endlocal