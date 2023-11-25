echo OFF

REM Variable setting
set arch=win64
set dc=
set dc_suf=
set prec=dp
set debug=chkb
set release=0
set static=0
set MPI="-DMPI=smp"
set pmpi=SMP
set got_mpi=0
set sp_suffix=
set mpi_suffix=
set debug_suffix=
set verbose=
set clean=0
set jobs=1
set jobsv=1
set debug_suffix=
set build_type=
set cbuild=0
set la=

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

   IF %1==-c (
       set dc="-DCOM=1"
       set dc_suf=_c
       set cbuild=1
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


Rem Engine name
if %prec%==sp   ( set sp_suffix=_sp)

if %debug%==0 (
    set debug_suffix=''
) else (

   if %debug%==1 (
    set debug_suffix='_db'
   ) else (
    set debug_suffix=_%debug%
   )

)

Rem if release is set, set debug to zero and no suffix
if  %release%==1 (
    set debug_suffix=
    set debug=0
)

if %got_mpi%==1 ( set mpi_suffix=_%pmi%)

if %cbuild%==0 (
   set engine=engine_%arch%%mpi_suffix%%sp_suffix%%debug_suffix%
)

if %cbuild%==1 (
   call CMake_Compilers_c\cmake_eng_version.bat
)

if %cbuild%==1 (
   set engine=e_%eng_version%_%arch%%mpi_suffix%%sp_suffix%%debug_suffix%
)

Rem Create build directory

set build_directory=cbuild_%engine%_ninja%dc_suf%

Rem clean
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

Rem Load Compiler settings
if %cbuild%==0 (
    call ..\CMake_Compilers\cmake_%arch%_compilers.bat
) else (
    call ..\CMake_Compilers_c\cmake_%arch%_compilers.bat
)
REM define Build type

if %debug%==0 (
    set build_type=Release 
) else (
    set build_type=Debug
)

cmake -G Ninja -DVS_BUILD=1 %dc% -DEXEC_NAME=%engine% -Darch=%arch% -Dprecision=%prec% %MPI% -Ddebug=%debug%  -Dstatic_link=%static% -DCMAKE_BUILD_TYPE=%build_type% -DCMAKE_Fortran_COMPILER=%Fortran_comp% -DCMAKE_C_COMPILER=%C_comp% -DCMAKE_CPP_COMPILER=%CPP_comp% -DCMAKE_CXX_COMPILER=%CXX_comp% %la% ..

if errorLevel=1 (

  echo.
  echo.
  echo Errors in CMAKE configuration !!!
  echo.
  cd ..
  exit /b 1
)

ninja %verbose% -j %jobs%

if errorLevel=1 (

  echo.
  echo.
  echo Errors in build encontered !!!
  echo.
  cd ..
  exit /b 1
)


cd ..

GOTO END

:ERROR

  echo.
  echo Windows build_script
  echo --------------------
  echo.
  echo Use with arguments : 
  echo     -arch=[build architecture]          : set architecture : default  Windows 64 bit
  echo           -arch=win64                     (SMP executable / Intel OneAPI / Windows X86-64)
  echo           -arch=win64       -mpi=impi     (Intel MPI OneAPI executable / Intel OneAPI / Fortran legacy compiler / Intel MPI / Windows X86-64)
  echo           -arch=win64_ifx                 (Intel MPI OneAPI executable / Intel OneAPI / Ifx compiler / Windows X86-64)
  echo           -arch=win64_ifx   -mpi=impi     (Intel MPI OneAPI executable / Intel OneAPI / Ifx compiler  / Intel MPI / Windows X86-64)
  echo .
  echo     -mpi=[smp,impi]                     : set MPI version
  echo     -prec=[dp,sp]                       : set precision - dp (default),sp
  echo     -static-link                        : Compiler runtime is linked in binary
  echo     -debug=[0,1,chkb]                   : debug version 
  echo                                              0: no debug flags
  echo                                              1: usual debug flag
  echo                                              chkb: check bounds build (default)
  echo     -release                            : set build for release (optimized)
  echo.
  echo Execution control 
  echo     -nt [N,all]        : Run build with N Threads, all : takes all ressources of machine
  echo     -verbose           : Verbose build
  echo     -clean             : clean build directory
  echo.

:END
echo.
echo Terminating

REM unset any variable used
set arch=
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
set debug_suffix=
set build_type=
set dc=
set dc_suf=
set cbuild=
set la=

echo Terminating
echo.
