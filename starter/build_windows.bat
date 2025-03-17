@echo OFF

setlocal
:: Variable setting
set arch=win64
set dc=
set dc_suf=
set prec=dp
set debug=0
set release=0
set static=0
set verbose=
set clean=0
set jobs=1
set jobsv=1
set debug_suffix=
set build_type=
set cbuild=0
set use_openreader=0

IF (%1) == () GOTO ERROR

:ARG_LOOP
IF (%1) == () GOTO END_ARG_LOOP

   IF %1==-arch (
       set arch=%2
    )

   IF %1==-prec (
       set prec=%2 
    )

   IF %1==-debug (
       set debug=%2
   )

   IF %1==-release (
       set release=1
   )

   IF %1==-static-link (
       set static=1
   )

   IF %1==-verbose (
       set verbose=-v
   )

   IF %1==-clean (
       set clean=1
   )

   IF %1==-c (
       set dc="-DCOM=1"
       set dc_suf=_c
       set cbuild=1
   )

   IF %1==-nt (
       set jobs=%2
       set jobsv=%2
       )

   IF %1==-open_reader (
       set use_openreader=1
   )

SHIFT
GOTO ARG_LOOP

:END_ARG_LOOP


if %jobsv%==all ( set jobs=0)


:: Starter name
if %prec%==sp ( set sp_suffix=_sp)

if %debug%==0 (
    set debug_suffix=
) else (

   if %debug%==1 (
    set debug_suffix=_db
   ) else (
    set debug_suffix=_%debug%
   )

)

:: if release is set, set debug to zero and no suffix
if  %release%==1 (
    set debug_suffix=
    set debug=0
)

if %cbuild%==0 (
  set starter=starter_%arch%%sp_suffix%%debug_suffix%
) 

if %cbuild%==1 (
  call CMake_Compilers_c\cmake_st_version.bat
)

if %cbuild%==1 (
  set starter=s_%st_version%_%arch%%sp_suffix%%debug_suffix%
)

:: Create build directory
set build_directory=cbuild_%starter%_ninja%dc_suf%

:: clean
if %clean%==1 (
  echo.
  echo Cleaning %build_directory%
  RMDIR /S /Q %build_directory%
  goto END_STARTER
)

echo.
echo Build OpenRadioss Starter
echo --------------------------
echo.
echo  Build Arguments :
echo  arch =                      : %arch%
echo  precision =                 : %prec%
echo  debug =                     : %debug%
echo  static_link =               : %static_link%
if %use_openreader%==1 (
echo.
echo  Using open_reader
)

echo.
echo  Running on             : %jobsv% Threads
echo.
echo  verbose=               : %verbose%
echo.
echo  Build directory        : %build_directory%
echo.

:: OpenReader Build
if %use_openreader%==1 (
  echo.
  echo Build OpenReader
  echo ----------------
  echo.
  cd ..\reader
  call build_windows.bat -arch=%arch% -nt=%jobsv% -debug=%debug% 
  if errorLevel=1 (
    echo.
    echo.
    echo Errors in OpenReader build !!!
    echo.
    exit /b 1
  )
  echo OpenReader build done
  cd ..\starter
)

if exist %build_directory% (

  cd  %build_directory%

) else (

  mkdir %build_directory%
  cd  %build_directory%
)

:: Load Compiler settings
if %cbuild%==0 (
    call ..\CMake_Compilers\cmake_%arch%_compilers.bat
) else (
    call ..\CMake_Compilers_c\cmake_%arch%_compilers.bat
)
:: define Build type

if %debug%==0 (
    set build_type=Release 
) else (
    set build_type=Debug
)

cmake -G Ninja -DVS_BUILD=1 %dc% -DEXEC_NAME=%starter% -Darch=%arch% -Dprecision=%prec% -Ddebug=%debug%  -Dstatic_link=%static% -DCMAKE_BUILD_TYPE=%build_type% -DCMAKE_Fortran_COMPILER=%Fortran_comp% -DCMAKE_C_COMPILER=%C_comp% -DCMAKE_CPP_COMPILER=%CPP_comp% -DCMAKE_CXX_COMPILER=%CXX_comp% -DUSE_OPEN_READER=%use_openreader% ..

if errorLevel=1 (
  echo.
  echo.
  echo Errors in CMAKE configuration !!!
  echo.
  cd ..
  endlocal
  exit /b 1
)

ninja %verbose% -j %jobs%

if %debug%==chkb (
    echo.
    echo Warning:
    echo --------
    echo Build was made with debug configuration.
    echo To enable optimization, add -release flag.
    echo.
)

if errorLevel=1 (

  echo.
  echo.
  echo Errors in build encontered !!!
  echo.
  cd ..
  endlocal
  exit /b 1
)
   

cd ..

GOTO END_STARTER

:ERROR
  echo.
  echo Windows build_script
  echo --------------------
  echo.
  echo Use with arguments : 
  echo     -arch=[build architecture]          : set architecture : default  Windows 64 bit
  echo            -arch=win64       (SMP executable / Windows X86-64 / Intel OneAPI / Intel ifx)
  echo            -arch=win64_sse3  (SMP executable / Windows X86-64 / Intel OneAPI / Intel ifx / Support older CPUs)  
  echo            -arch=win64_ifort (SMP executable / Windows X86-64 / Intel OneAPI / Legay Fortran Compiler)
  echo.
  echo     -prec=[dp,sp]                       : set precision - dp (default),sp
  echo     -static-link                        : Compiler runtime is linked in binary
  echo     -debug=[0,1,chkb]                   : debug version
  echo                                              0 no debug flags (default)
  echo                                              1 usual debug flags
  echo                                              chkb Check bounds build
  echo     -open_reader                        : link with open_reader
  echo     -release                            : set build for release (optimized)
  echo.
  echo Execution control 
  echo     -nt=[N,all]        : Run build with N Threads, all : consumes all resources of machine
  echo     -verbose           : Verbose build
  echo     -clean             : clean build directory
  echo.

:END_STARTER
echo.

:: clean used variables
set arch=
set prec=
set debug=
set static=
set verbose=
set clean=
set jobs=
set jobsv=
set debug_suffix=
set build_type=
set dc=
set dc_suf=
set cbuild=

echo Terminating
endlocal
echo.

