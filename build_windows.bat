@echo OFF

REM Variable setting
set arch=none
set my_arch=
set prec=dp
set debug=0
set static=0
set MPI="-DMPI=smp"
set pmpi=SMP
set got_mpi=0
set sp_suffix=
set mpi_suffix=
set verbose=
set clean=0
set jobs=1
set jobsv=1
set build=both
set buildv=both
set debug_suffix=
set build_type=

IF (%1) == () GOTO ERROR

:ARG_LOOP
IF (%1) == () GOTO END_ARG_LOOP

   IF %1==-arch (
       set my_arch=%2
    )

   IF %1==-prec (
       set prec=%2
    )

   IF %1==-debug (
       set debug=%2
   )

   IF %1==-static-link (
       set static=1
   )

   IF %1==-mpi (
       set MPI="-DMPI=%2"
       set pmi=%2
       set got_mpi=1
   )

   IF %1==-verbose (
       set verbose=-v
   )

   IF %1==-clean (
       set clean=1
   )

   IF %1==-nt (
       set jobs=%2
       set jobsv=%2
       )

   IF %1==-build (
       set buildv=%2
       )
   )

SHIFT
GOTO ARG_LOOP

:END_ARG_LOOP


if %buildv%==starter ( set build=starter )
if %buildv%==engine  ( set build=engine )
if %jobsv%==all      ( set jobs=0 )

Rem Engine name
if %prec%==sp   ( set sp_suffix=_sp)
if %debug%==1   ( set debug_suffix=_db)
if %debug%==2   ( set debug_suffix=_db)
if %got_mpi%==1 ( set mpi_suffix=_%pmi%)

set arch=%my_arch%

set engine=engine_%arch%%mpi_suffix%%sp_suffix%%debug_suffix%.exe
set starter=starter_%arch%%sp_suffix%%debug_suffix%.exe

Rem Create build directory

set build_directory=cbuild_%arch%%sp_suffix%%debug_suffix%_ninja

Ren clean
if %clean%==1 (
  echo.
  echo Cleaning %build_directory%
  RMDIR /S /Q %build_directory%
  goto END
)

echo.
echo Build OpenRadioss Engine
echo -------------------------
echo.
echo  Build Arguments :
echo  arch =                 : %arch%
echo  build =                : %build%
echo  MPI =                  : %pmpi%
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


REM define Build type

if %debug%==0 (
    set build_type=Release 
) else (
    set build_type=Debug
)


Rem Load Compiler settings
call ..\CMake_Compilers\cmake_%arch%_compilers.bat



cmake -G Ninja -Darch=%arch% -Dbuild=%build% -DVS_BUILD=1 -Dprecision=%prec% %MPI% -Ddebug=%debug%  -Dstatic_link=%static% -DCMAKE_BUILD_TYPE=%build_type% -DCMAKE_Fortran_COMPILER=%Fortran_comp% -DCMAKE_C_COMPILER=%C_comp% -DCMAKE_CPP_COMPILER=%CPP_comp% -DCMAKE_CXX_COMPILER=%CXX_comp% ..
ninja %verbose% -j %jobs%


cd ..

GOTO END

:ERROR

  echo Use with arguments : 
  echo     -arch=[build architecture]          : set architecture : default  Windows 64 bit
  echo     -build=[starter,engine,both]        : set binary to build : default 'both'
  echo     -mpi=[smp,impi]                     : set MPI version
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
Rem clean up used variables

set arch=
set my_arch=
set prec=
set debug=
set static=
set MPI=
set pmpi=
set got_mpi=
set sp_suffix=
set mpi_suffix=
set verbose=
set clean=
set jobs=
set jobsv=
set build=
set buildv=
set debug_suffix=
set build_type=

echo.
echo Terminating
echo.

