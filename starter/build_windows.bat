echo OFF

REM Variable setting
set arch=win64
set dc=
set dc_suf=
set prec=dp
set debug=0
set static=0
set verbose=
set clean=0
set jobs=1
set jobsv=1
set debug_suffix=
set build_type=

IF (%1) == () GOTO ERROR

:ARG_LOOP
IF (%1) == () GOTO END_ARG_LOOP

   IF %1==-prec (
       set prec=%2 
    )

   IF %1==-debug (
       set debug=%2
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
   )

   IF %1==-nt (
       set jobs=%2
       set jobsv=%2
       )
   )

SHIFT
GOTO ARG_LOOP

:END_ARG_LOOP


if %jobsv%==all ( set jobs=0)


Rem Starter name
if %prec%==sp ( set sp_suffix=_sp)
if %debug%==1 ( set debug_suffix=_db)
if %debug%==2 ( set debug_suffix=_db2)

set starter=starter_%arch%%sp_suffix%%debug_suffix%%dc_suf%

Rem Create build directory


set build_directory=cbuild_%starter%_ninja

Ren clean
if %clean%==1 (
  echo.
  echo Cleaning %build_directory%
  RMDIR /S /Q %build_directory%
  goto END
)

echo.
echo Build OpenRadioss Starter
echo --------------------------
echo.
echo  Build Arguments :
echo  arch =                 : %arch%
echo  precision =            : %prec%
echo  debug =                : %debug%
echo  static_link =          : %static_link%
echo.
echo  Running on             : %jobsv% Threads
echo.
echo  verbose=               : %verbose%
echo.
echo  Build directory:  %build_directory%
echo.

if exist %build_directory% (

  cd  %build_directory%

) else (

  mkdir %build_directory%
  cd  %build_directory%
)

Rem Load Compiler settings
call ..\CMake_Compilers\cmake_%arch%_compilers.bat

REM define Build type

if %debug%==0 (
    set build_type=Release 
) else (
    set build_type=Debug
)

cmake -G Ninja -DVS_BUILD=1 %dc% -DEXEC_NAME=%starter% -Darch=%arch% -Dprecision=%prec% -Ddebug=%debug%  -Dstatic_link=%static% -DCMAKE_BUILD_TYPE=%build_type% -DCMAKE_Fortran_COMPILER=%Fortran_comp% -DCMAKE_C_COMPILER=%C_comp% -DCMAKE_CPP_COMPILER=%CPP_comp% -DCMAKE_CXX_COMPILER=%CXX_comp% ..
ninja %verbose% -j %jobs%

cd ..

GOTO END

:ERROR

  echo Use with arguments : 
  echo     -arch=[build architecture]          : set architecture : default  Windows 64 bit
  echo     -prec=[dp,sp]                       : set precision - dp (default),sp
  echo     -static-link                        : Compiler runtime is linked in binary
  echo     -debug=[0,1]                        : debug version 0 no debug flags (default), 1 usual debug flag )
  echo.
  echo Execution control 
  echo     -nt [N,all]        : Run build with N Threads, all : takes all ressources of machine
  echo     -verbose           : Verbose build
  echo     -clean             : clean build directory
  echo.

:END
echo.

REM clean used variables
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

echo Terminating
echo.

