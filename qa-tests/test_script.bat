echo OFF

set arch=built_in
set mpi=smp
set np=1
set prec=dp
set test_cases=""
set verbose=""
set stdout="-DSTDOUT=0"
set keep_results=0
set clean=0

rem Argument loop
:ARG_LOOP
IF (%1) == () GOTO END_ARG_LOOP

   IF %1==-help (
       GOTO HELP
   )

   IF %1==-arch (
       set arch=%2
   )

   IF %1==-mpi (
       set mpi=%2
   )

   IF %1==-np (
       set np=%2
   )

   IF %1==-prec (
       set prec=%2 
    )

   IF %1==-tests (
       set tests=-R %2

   )

   IF %1==-stdout (
       set stdout="-DSTDOUT=1"
       set verbose="--verbose"
   )

   IF %1==-keep_results (
       set keep_results=1
   )

   IF %1==-clean (
       set clean=1
   )

SHIFT
GOTO ARG_LOOP

:END_ARG_LOOP



REM As this is a bash script it is intended to be executed on Linux Plarforms.
set test_directory=ctest_suite_win64

echo.
echo test_script
echo ------------
echo.

REM clean
if %clean%==1 (
  echo.
  echo Cleaning %test_directory%
  RMDIR /S /Q %test_directory%
  goto END
)

REM Create and enter directory
if exist %test_directory% (
  cd  %test_directory%
) else (
  mkdir %test_directory%
  cd  %test_directory%
)

rem MPI=smp,impi,ompi : depending on the flavors
rem cmake -DMPI=impi -DNP=4 ..

cmake -Darch=%arch% -DPREC=%prec% -DMPI=%mpi%  -DNP=%np% %stdout% -DKEEP=%keep_results% ..
ctest -C Release --output-on-failure --timeout 600 %tests% %verbose%

cd ..

GOTO END

:HELP

  echo.
  echo test_script
  echo ------------
  echo Run Test suite and verify the results
  echo. 
  echo Use with arguments :
  echo -help              : print this help exit
  echo. 
  echo -arch=arch         : Set the executable architecture.
  echo                      -arch=built_in (Default) :
  echo                               linux64_gf for linux
  echo                               win64 for windows
  echo                               linuxa64 for Linux/Arm
  echo.
  echo -mpi=[smp,ompi,impi]
  echo         -mpi=smp  : Engine is SMP only executable (default)
  echo         -mpi=ompi : engine is using OpenMPI
  echo         -mpi=impi : engine is using Intel MPI
  echo.
  echo -np=#MPI Domains    : Set # MPI Domains thest will run through
  echo -prec=[dp,sp]       : set executable precision - dp (default) ,sp
  echo -stdout             : print Test output
  echo -tests="Test list"  : Run specific tests
  echo                       use CTest regular expression form
  echo                       -tests="test1|test2|..."       
  echo. 
  echo -keep_results       : Keep computation results
  echo -clean              : Clean execution directory
  echo. 

:END

rem uninitialize variables
set arch=
set mpi=
set np=
set prec=
set tests=
set verbose=
set stdout=
set keep_results=
set clean=
set test_directory=

echo.


