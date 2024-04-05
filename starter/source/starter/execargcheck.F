Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
Chd|====================================================================
Chd|  EXECARGCHECK                  source/starter/execargcheck.F 
Chd|-- called by -----------
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|        BUILD_MSG                     source/output/message/build_msg.F
Chd|        GET_IBUILTIN_ARCH             source/system/get_ibuiltin_arch.c
Chd|        ISANARGUMENT                  source/starter/execargcheck.F 
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        PEXECINFO                     source/starter/execargcheck.F 
Chd|        PHELPINFO                     source/starter/execargcheck.F 
Chd|        PRHELPINFO                    source/starter/execargcheck.F 
Chd|        READ_MSGFILE                  source/output/message/read_msgfile.F
Chd|        UPCASE                        source/starter/execargcheck.F 
Chd|        CHECK_MOD                     ../common_source/modules/check_mod.F
Chd|        GRP_SIZE_MOD                  share/modules1/grp_size_mod.F 
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        RANDOM_MOD                    share/modules1/random_mod.F   
Chd|====================================================================
      SUBROUTINE EXECARGCHECK()
      USE CHECK_MOD
      USE INOUTFILE_MOD
      USE RANDOM_MOD
      USE GRP_SIZE_MOD
      USE PYTHON_FUNCT_MOD 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "commandline.inc"
#include      "warn_c.inc"
#include      "execinp.inc"
#include      "userlib.inc"
#include      "tablen_c.inc"
#include      "ddspmd_c.inc"
#include      "debug_rst.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER ARGN
       INTEGER PHELPI,PEXECI,PINPUTI,PNCPUI, PNTHI, PTIMER, PUSERLNAMI,MDS_PATHI
       INTEGER :: MDS_DIRI            !< Flag for -mdsdir - stores Iteration in command line
       INTEGER I,STRL,STRLN,ERR,LENLIST,ISIN,STRLNA,BEGIN
       INTEGER IDUM
       LOGICAL :: CONDITION
       CHARACTER C
       INTEGER GLOBAL_ERROR
       CHARACTER*2096 INPUTR,INPUTC,STRING,ARGP,ARGS
       CHARACTER*4096 ULIBC
       character(len=2096) ARGS2,ARGS_REDUCE
       INTEGER :: LEN_DOMDEC_CPU_TYPE
       CHARACTER(LEN=15) :: DOMDEC_CPU_TYPE
       INTEGER IARGC,IERRMSG,CDL_CASE
       CHARACTER :: LAST_LETTER,SEPARATOR
       INTEGER :: RANDM_SEED,RANDM_ALEA
       REAL(kind=8) :: RANDM_SEED_NBR,RANDM_ALEA_NBR
       INTEGER :: GOT_GRP_SIZE
       PARAMETER (LENLIST=31)
       CHARACTER (LEN=12) ::  ARGLIST(LENLIST)
       EXTERNAL IARGC
       DATA ARGLIST/
     .    '-VERSION',  '-V',
     .    '-HELP'   ,  '-H',
     .    '-INPUT'  ,  '-I',
     .    '-NSPMD'  ,  '-NP',
     .    '-NTHREAD' ,  '-NT',
     .    '-ERROR_MSG','-EM',
     .    '-NOTRAP' ,  '-TIMER',
     .    '-DYNAMIC_LIB', '-DYLIB',
     .    '-MDS_LIBPATH', '-MDSDIR',
     .    '-MEM-MAP' ,  '-INSPIRE', '-DD_TUNING',
     .    '-INSPIRE_ALM' , '-FLUSH_RST', '-CHECK',
     .    '-HSTP_READ' , '-HSTP_WRITE', '-RXALEA', '-RSEED',
     .    '-PREVIEW',
     .    '-GRP_SIZE' , '-PYTHON' /

C-----------------------------------------------
       IDUM=-1
       ITRACE=1
       IERRMSG=0
       GLOBAL_ERROR = 0
       PYTHON_ERROR = 1

       GOT_INPUT = 0
       GOT_NCPU = 0
       GOT_NTH = 0
       GOT_TIMER = 0
       GOT_USERL_ALTNAME=0
       GOT_MEM_MAP=0
       GOT_INSPIRE=0
       GOT_INSPIRE_ALM=0
       GOT_HSTP_READ = 0
       GOT_HSTP_WRITE = 0
       MDS_PATH_LEN = 0
       FLUSH_RST_TO_TXT = .FALSE.
c
       INPUT=' '
       LENI=0

       GOT_PATH=0
       LENP=0
       PATH=' '

       PHELPI = 0 
       PEXECI = 0
       PINPUTI = 0
       PNCPUI= 0
       PNTHI = 0
       PTIMER = 0
       PUSERLNAMI = 0
       MDS_PATHI = 0
       MDS_DIRI = 0
!       ------------------------
!      domdec optimization
       DOMDEC_TUNING = 0
       DD_OPTIMIZATION = 0
       !    /RFILE/OFF or -check command line 
       !    --> avoid to write restart files at the end of the starter
       !    default = write restart files
       RESTART_FILE = 1
!       ------------------------
!      outfile / infile option
       INOUT_BOOL = .FALSE.
       OUTFILE_NAME_LEN = 0
       OUTFILE_BOOL = .FALSE.
       OUTFILE_NAME(1:OUTFILE_CHAR_LEN) =''
       INFILE_NAME_LEN = 0
       INFILE_BOOL = .FALSE.
       INFILE_NAME(1:INFILE_CHAR_LEN) =''
#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
        SEPARATOR='\'
#elif 1
        SEPARATOR='/'
#endif
!       ------------------------
!       -rxalea or -rseed options
        RAND_STRUCT%CMD=.FALSE.
        RAND_STRUCT%ALEA_NBR=0.d+00
        RAND_STRUCT%SEED_NBR=0.d+00
        RAND_STRUCT%ALEA=.FALSE.
        RAND_STRUCT%SEED=.FALSE.

        RANDM_SEED=0
        RANDM_ALEA=0
!       ------------------------
!       -GRP_SIZE hidden option
        GRP_SIZE_BOOL=.FALSE.
        GOT_GRP_SIZE = 0
        GRP_SIZE=128     
!       ------------------------

       ARGS2(1:2096) = ''

       ARGN = COMMAND_ARGUMENT_COUNT()!IARGC()

       DO I=1,ARGN
        CALL GET_COMMAND_ARGUMENT(I,ARGS)      !GETARG(I,ARGS)
        strl=LEN_TRIM(ARGS) 
        ARGS2(1:2096) = ''
        ARGS2(1:strl) = ARGS(1:strl)
        CALL UPCASE(ARGS)

        ARGS_REDUCE(1:9) = ARGS(1:9)
        CDL_CASE = 0
C        IF(ARGS_REDUCE(1:7)=='-CHECK=') CDL_CASE=1       ! remove -CHECK= to -CHECK
        IF(ARGS_REDUCE(1:9)=='-OUTFILE=') CDL_CASE = 2
        IF(ARGS_REDUCE(1:8)=='-INFILE=') CDL_CASE = 3

        IF(CDL_CASE==0) THEN

            SELECT CASE (ARGS)
C------------------------------------------------
C -VERSION, -V
            CASE ( '-VERSION')
                PEXECI = 1
            CASE ( '-V')
                PEXECI = 1
C------------------------------------------------
C -HELP, -H
            CASE ( '-HELP')
                PHELPI = 1
            CASE ( '-H')
                PHELPI = 1
C------------------------------------------------
C -NODEBUG
            CASE ( '-NOTRAP')
                ITRACE = 0
C------------------------------------------------
C -ERROR_MSG, -EM
            CASE ( '-ERROR_MSG')
                IERRMSG = I
            CASE ( '-EM')
                IERRMSG = I
C------------------------------------------------
C -INPUT, -I
            CASE ( '-INPUT')
                IF (PINPUTI==0) PINPUTI = I
            CASE ( '-I')
                IF (PINPUTI==0) PINPUTI = I 
C------------------------------------------------
C -NSPMD, -NP
            CASE ( '-NSPMD')
                IF (PNCPUI==0) PNCPUI = I
            CASE ( '-NP')
                IF (PNCPUI==0) PNCPUI = I 
C------------------------------------------------
C -NTHREAD, -NT
            CASE ( '-NTHREAD')
                IF (PNTHI==0) PNTHI = I
            CASE ( '-NT')
                IF (PNTHI==0) PNTHI = I 
C------------------------------------------------
C -TIMER
            CASE ( '-TIMER')
                IF (PTIMER==0) PTIMER = I
C------------------------------------------------
C -DYNAMIC_LIB, -DYLIB
            CASE ( '-DYNAMIC_LIB')
                IF (PUSERLNAMI==0) PUSERLNAMI=I
            CASE ( '-DYLIB')
                IF (PUSERLNAMI==0) PUSERLNAMI=I
C------------------------------------------------
C -MDS_LIBPATH
            CASE ( '-MDS_LIBPATH')
                IF (MDS_PATHI==0) MDS_PATHI=I
C------------------------------------------------
C -MDSDIR
            CASE ( '-MDSDIR')
                IF (MDS_DIRI==0) MDS_DIRI=I
C------------------------------------------------
C -MEM-MAP
            CASE ( '-MEM-MAP')
                GOT_MEM_MAP=1
C------------------------------------------------
C -INSPIRE
            CASE ( '-INSPIRE')
                GOT_INSPIRE=1
C------------------------------------------------
! -DD_TUNING
            CASE('-DD_TUNING')
                IF(DOMDEC_TUNING==0) DOMDEC_TUNING=I
C------------------------------------------------
C -INSPIRE_ALM
            CASE ( '-INSPIRE_ALM')
                GOT_INSPIRE_ALM=1
C------------------------------------------------
! -HSTP_READ
            CASE ( '-HSTP_READ')
                GOT_HSTP_READ = 1
C------------------------------------------------
! -HSTP_READ
            CASE ( '-HSTP_WRITE')
                GOT_HSTP_WRITE = 1
C------------------------------------------------
#ifdef DEBUG_RST
            CASE ( '-FLUSH_RST')
                FLUSH_RST_TO_TXT = .TRUE.
#endif
C------------------------------------------------
! -CHECK
            CASE('-CHECK')
                RESTART_FILE = 0
            CASE('-RXALEA')
                RANDM_ALEA = I
            CASE('-RSEED')
                RANDM_SEED = I
            CASE('-PYTHON')
                PYTHON_ERROR = 0
            CASE('-PREVIEW')
C------------------------------------------------
            CASE ( '-GRP_SIZE')
                GOT_GRP_SIZE = I
C------------------------------------------------
C Lignes de commande qui ne sont pas definies
            CASE DEFAULT
            ERR = 0
            IF (I == 1)THEN
               ERR = 1
            ELSE
               CALL GET_COMMAND_ARGUMENT(I-1,ARGP)       !GETARG(I-1,ARGP)
               CALL UPCASE(ARGP)
               STRLN=LEN_TRIM(ARGP)
C Check if argument string is a variable to an argument
               IF (ARGP == '-I' .OR. ARGP =='-INPUT'  .OR.
     *             ARGP == '-NP' .OR. ARGP =='-NSPMD' .OR.
     *             ARGP == '-NT' .OR. ARGP =='-NTHREAD'.OR.
     *             ARGP == '-TIMER' .OR. ARGP =='-DYLIB'.OR.
     *             ARGP == '-DYNAMIC_LIB' .OR. ARGP == '-DD_TUNING'.OR.
     *             ARGP == '-RSEED' .OR. ARGP == '-RXALEA'.OR.
     *             ARGP == '-GRP_SIZE' .OR. ARGP == '-MDS_LIBPATH' .OR. ARGP == '-MDSDIR'  )THEN 
                 ERR = 0
               ELSE
                 ERR = 1
               ENDIF
            ENDIF
            IF (ERR == 1)THEN
               CALL GET_COMMAND_ARGUMENT(I,ARGP)       !GETARG(I,ARGP)
               STRLN=LEN_TRIM(ARGP) 
               WRITE(6,'(A,A)') ' '
               WRITE(6,'(A,A)') 
     *           '***  ERROR : Unknown command line argument: ',
     *            ARGP(1:STRLN)
                  WRITE(6,'(A,A)') ' '
               CALL PRHELPINFO()
               CALL MY_EXIT(2)
            ENDIF
C------------------------------------------------
            END SELECT

        ELSE
C------------------------------------------------
         SELECT CASE (CDL_CASE)
         ! -----------------------------
         !       CHECK=... options
         ! -----------------------------
!   ------------------------------------------------
            CASE(1)
                ARGS2(:)=''
                ARGS2(1:len_trim(ARGS)-7) = ARGS(8:len_trim(ARGS))
                SELECT CASE ( ARGS2(1:len_trim(ARGS2)) )
                ! -----------------------------
                !       -CHECK=NORST option
                ! -----------------------------
                    CASE ( 'NORST')
                    !   -CHECK=NORST : no restart file command
                        RESTART_FILE = 0
                END SELECT
         ! -----------------------------
!   ------------------------------------------------
         ! -----------------------------
         !       -OUTFILE=... option
         ! -----------------------------
            CASE(2)
!            IF(ARGS_REDUCE(1:9)=='-OUTFILE=') THEN
                OUTFILE_NAME_LEN = LEN_TRIM(ARGS2) - 9
                OUTFILE_NAME(1:OUTFILE_NAME_LEN) = ARGS2(10:LEN_TRIM(ARGS2))
                OUTFILE_BOOL=.TRUE.
                LAST_LETTER = OUTFILE_NAME(OUTFILE_NAME_LEN:OUTFILE_NAME_LEN)
                IF(LAST_LETTER/=SEPARATOR) THEN
                    OUTFILE_NAME(1:OUTFILE_NAME_LEN+1) = 
     .                       OUTFILE_NAME(1:OUTFILE_NAME_LEN)//SEPARATOR
                    OUTFILE_NAME_LEN = OUTFILE_NAME_LEN + 1
                ENDIF
!   ------------------------------------------------
         ! -----------------------------
         !       -INFILE=... option
         ! -----------------------------
            CASE(3)
!            ELSEIF(ARGS_REDUCE(1:8)=='-INFILE=') THEN
            !   -INFILE=... option
                INFILE_NAME_LEN = LEN_TRIM(ARGS2) - 8
                INFILE_NAME(1:INFILE_NAME_LEN) = ARGS2(9:LEN_TRIM(ARGS2))
                INFILE_BOOL=.TRUE.
                LAST_LETTER = INFILE_NAME(OUTFILE_NAME_LEN:OUTFILE_NAME_LEN)
                IF(LAST_LETTER/=SEPARATOR) THEN
                    INFILE_NAME(1:INFILE_NAME_LEN+1) = 
     .                       INFILE_NAME(1:INFILE_NAME_LEN)//SEPARATOR
                    INFILE_NAME_LEN = INFILE_NAME_LEN + 1
                ENDIF
!            ENDIF
            END SELECT
!   ------------------------------------------------
        ENDIF
       ENDDO

       GLOBAL_ERROR = 0
C------------------------------------------------
C -VERSION SET
C------------------------------------------------
       IF (PEXECI==1) THEN
          CALL PEXECINFO(IDUM)
       ENDIF

C------------------------------------------------
C -HELP SET
C------------------------------------------------
       IF (PHELPI==1) THEN
          CALL PHELPINFO()
       ENDIF

C------------------------------------------------
C -ERROR_MSG SET
C------------------------------------------------
       IF (IERRMSG /= 0)THEN
         IF (IERRMSG+1  > ARGN) THEN

C          Case -I is last argument in the list.

           CALL GET_COMMAND_ARGUMENT(IERRMSG,ARGP)       !GETARG(IERRMSG,ARGP)
           STRLN=LEN_TRIM(ARGP) 
           WRITE(6,'(A)') ' '
           WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',
     *                    ARGP(1:STRLN)
!            MSG = ARGP
!            CALL PHELPINFO()

           CALL MY_EXIT(2)
         ELSE
           CALL GET_COMMAND_ARGUMENT(IERRMSG+1,INPUTR)       !GETARG(IERRMSG+1,INPUTR)
           LENI=LEN_TRIM(INPUTR)

C check if -i has got an argument or if the next string is an input command

           INPUTC = INPUTR
           CALL UPCASE(INPUTC)
           ISIN = 0
           CALL ISANARGUMENT(ARGLIST,LENLIST,INPUTC,ISIN)
           IF ( ISIN==1 )THEN
               CALL GET_COMMAND_ARGUMENT(PINPUTI,ARGP)       !GETARG(PINPUTI,ARGP)
               STRLN=LEN_TRIM(ARGP) 
!                MSG=ARGP
               WRITE(6,'(A)') ' '
               WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',
     *                         ARGP(1:STRLN)
               CALL MY_EXIT(2)
           ENDIF 
           CALL READ_MSGFILE(LENI,INPUTR)
           CALL BUILD_MSG()
         ENDIF
       ENDIF
C------------------------------------------------
C -INPUT SET
C------------------------------------------------
       IF (PINPUTI /= 0)THEN
         IF (PINPUTI+1  > ARGN) THEN

C          Case -I is last argument in the list.

           CALL GET_COMMAND_ARGUMENT(PINPUTI,ARGP)       !GETARG(PINPUTI,ARGP)
           STRLN=LEN_TRIM(ARGP) 
C           CALL PRHELPINFO()
           WRITE(6,'(A)') ' '
           WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',
     *                    ARGP(1:STRLN)
           GLOBAL_ERROR = 1
           GOTO 100
 
         ELSE
           CALL GET_COMMAND_ARGUMENT(PINPUTI+1,INPUTR)       !GETARG(PINPUTI+1,INPUTR)
           LENI=LEN_TRIM(INPUTR)
           GOT_INPUT = 1

C check if -i has got an argument or if the next string is an input command

           INPUTC = INPUTR
           CALL UPCASE(INPUTC)
           ISIN = 0
           CALL ISANARGUMENT(ARGLIST,LENLIST,INPUTC,ISIN)
           IF ( ISIN==1 )THEN
               CALL GET_COMMAND_ARGUMENT(PINPUTI,ARGP)       !GETARG(PINPUTI,ARGP)
               STRLN=LEN_TRIM(ARGP) 
C               CALL PRHELPINFO()
               WRITE(6,'(A)') ' '
               WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',
     *                         ARGP(1:STRLN)
               GLOBAL_ERROR = 1
               GOTO 100
           ENDIF 

           BEGIN=LEN_TRIM(INPUTR)
           CONDITION = .FALSE.
           DO WHILE (BEGIN > 0 .AND. .NOT.CONDITION )
             C = INPUTR(BEGIN:BEGIN)
             IF (ICHAR(C)==47 .OR. ichar(C)==92) THEN
                  CONDITION=.TRUE.
                  GOTO 150
             ENDIF
             BEGIN=BEGIN-1
           ENDDO
 150       CONTINUE
           LENI = LEN_TRIM(INPUTR) - BEGIN
           BEGIN=BEGIN+1
           INPUT(1:LENI) = INPUTR(BEGIN:LEN_TRIM(INPUTR))

           IF (BEGIN > 1)THEN 
             GOT_PATH=1
             LENP=BEGIN-1
             PATH(1:LENP)=INPUTR(1:LENP)
           ENDIF
         ENDIF
       ELSE
           ! PINPUTI is 0, which means -input/-i was not set
           WRITE(6,'(A)') ' '
           WRITE(6,'(A)') '*** ERROR : No input deck set. Use -input [Starter input file] '
           WRITE(6,'(A)') ' '
           CALL PHELPINFO()
       ENDIF
 100    CONTINUE
C------------------------------------------------
C -NSPMD SET
C------------------------------------------------
       IF (PNCPUI /= 0)THEN

         IF (PNCPUI+1  > ARGN) THEN

C          Case -np is last argument in the list.

           CALL GET_COMMAND_ARGUMENT(PNCPUI,ARGP)       !GETARG(PNCPUI,ARGP)
           STRLN=LEN_TRIM(ARGP) 
C           CALL PRHELPINFO()
           WRITE(6,'(A)') ' '
           WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',
     *                    ARGP(1:STRLN)
           GLOBAL_ERROR=1
           GOTO 200
          
         ELSE
           CALL GET_COMMAND_ARGUMENT(PNCPUI+1,STRING)       !GETARG(PNCPUI+1,STRING)

C check if -np has got an argument or if the next string is an input command
           CALL UPCASE(STRING)
           ISIN = 0
           CALL ISANARGUMENT(ARGLIST,LENLIST,STRING,ISIN)
           IF ( ISIN==1 )THEN
               CALL GET_COMMAND_ARGUMENT(PNCPUI,ARGP)       !GETARG(PNCPUI,ARGP)
               STRLN=LEN_TRIM(ARGP) 
C               CALL PRHELPINFO()
               WRITE(6,'(A)') ' '
               WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',
     *                         ARGP(1:STRLN)
               GLOBAL_ERROR=1
               GOTO 200
           ENDIF 
C convert the argument to an Integer
           GOT_NCPU = 1
           CALL GET_COMMAND_ARGUMENT(PNCPUI+1,STRING)       !GETARG(PNCPUI+1,STRING)
           READ(STRING,'(I10)',ERR=999) NCPU
C Read converting was OK goto 1000
           GOTO 1000


 999       CONTINUE
C Error was found during Character to Integer translation
           STRLN=LEN_TRIM(STRING) 
           CALL GET_COMMAND_ARGUMENT(PNCPUI,ARGP)       !GETARG(PNCPUI,ARGP)
           STRLNA=LEN_TRIM(ARGP) 
           WRITE(6,'(A)') ' '
           WRITE(6,'(A,A,A,A,A)') 
     *         '*** ERROR in "',ARGP(1:STRLNA),'" argument : "',
     *          STRING(1:STRLN),'" is not an integer value'
           GLOBAL_ERROR=1
           GOTO 200

 1000      CONTINUE
         ENDIF
       ENDIF

C------------------------------------------------
C -NTHREAD SET
C------------------------------------------------
       IF (PNTHI /= 0)THEN

         IF (PNTHI+1  > ARGN) THEN

C          Case -nt is last argument in the list.

           CALL GET_COMMAND_ARGUMENT(PNTHI,ARGP)       !GETARG(PNTHI,ARGP)
           STRLN=LEN_TRIM(ARGP) 
C           CALL PRHELPINFO()
           WRITE(6,'(A)') ' '
           WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',
     *                    ARGP(1:STRLN)
           GLOBAL_ERROR=1
           GOTO 200
          
         ELSE
           CALL GET_COMMAND_ARGUMENT(PNTHI+1,STRING)       !GETARG(PNTHI+1,STRING)

C check if -nt has got an argument or if the next string is an input command
           CALL UPCASE(STRING)
           ISIN = 0
           CALL ISANARGUMENT(ARGLIST,LENLIST,STRING,ISIN)
           IF ( ISIN==1 )THEN
               CALL GET_COMMAND_ARGUMENT(PNTHI,ARGP)       !GETARG(PNTHI,ARGP)
               STRLN=LEN_TRIM(ARGP) 
C               CALL PRHELPINFO()
               WRITE(6,'(A)') ' '
               WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',
     *                         ARGP(1:STRLN)
               GLOBAL_ERROR=1
               GOTO 200
           ENDIF 
C convert the argument to an Integer
           GOT_NTH = 1
           CALL GET_COMMAND_ARGUMENT(PNTHI+1,STRING)       !GETARG(PNTHI+1,STRING)
           READ(STRING,'(I10)',ERR=1999) NTH
C Read converting was OK goto 2000
           GOTO 2000


 1999      CONTINUE
C Error was found during Character to Integer translation
           STRLN=LEN_TRIM(STRING) 
           CALL GET_COMMAND_ARGUMENT(PNTHI,ARGP)       !GETARG(PNTHI,ARGP)
           STRLNA=LEN_TRIM(ARGP) 
           WRITE(6,'(A)') ' '
           WRITE(6,'(A,A,A,A,A)') 
     *         '*** ERROR in "',ARGP(1:STRLNA),'" argument : "',
     *          STRING(1:STRLN),'" is not an integer value'
           GLOBAL_ERROR=1
           GOTO 200

 2000      CONTINUE
         ENDIF
       ENDIF
C------------------------------------------------
C -DYLIB SET
C------------------------------------------------
       IF (PUSERLNAMI /= 0)THEN
         IF (PUSERLNAMI+1  > ARGN) THEN

C          Case -DYLIB is last argument in the list.

           CALL GET_COMMAND_ARGUMENT(PUSERLNAMI,ARGP)       !GETARG(PUSERLNAMI,ARGP)
           STRLN=LEN_TRIM(ARGP) 
C           CALL PRHELPINFO()
           WRITE(6,'(A)') ' '
           WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',ARGP(1:STRLN)
           GLOBAL_ERROR = 1
 
         ELSE
           CALL GET_COMMAND_ARGUMENT(PUSERLNAMI+1,USERL_ALTNAME)       !GETARG(PUSERLNAMI+1,USERL_ALTNAME)
           LEN_USERL_ALTNAME=LEN_TRIM(USERL_ALTNAME)
           GOT_USERL_ALTNAME = 1
C check if -dylib has got an argument or if the next string is an input command

           ULIBC=''
           ULIBC(1:LEN_USERL_ALTNAME) = USERL_ALTNAME(1:LEN_USERL_ALTNAME)
           CALL UPCASE(ULIBC)
           ISIN = 0
           CALL ISANARGUMENT(ARGLIST,LENLIST,ULIBC,ISIN)
           IF ( ISIN==1 )THEN
               CALL GET_COMMAND_ARGUMENT(PUSERLNAMI,ARGP)       !GETARG(PUSERLNAMI,ARGP)
               STRLN=LEN_TRIM(ARGP) 
C               CALL PRHELPINFO()
               WRITE(6,'(A)') ' '
               WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',
     *                         ARGP(1:STRLN)
               GLOBAL_ERROR = 1
               GOTO 3000
           ENDIF 

         ENDIF     !         IF (PUSERLNAMI+1  > ARGN) THEN
       ENDIF
 3000  CONTINUE
C------------------------------------------------
C -MDS_LIBPATH
C------------------------------------------------
       IF (MDS_PATHI /= 0)THEN

         IF (MDS_PATHI+1  > ARGN) THEN

C          Case -DYLIB is last argument in the list.

           CALL GET_COMMAND_ARGUMENT(MDS_PATHI,ARGP)       
           STRLN=LEN_TRIM(ARGP) 
C           CALL PRHELPINFO()
           WRITE(6,'(A)') ' '
           WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',ARGP(1:STRLN)
           GLOBAL_ERROR = 1
 
         ELSE
           CALL GET_COMMAND_ARGUMENT(MDS_PATHI+1,MDS_PATH)       
           MDS_PATH_LEN=LEN_TRIM(MDS_PATH)
C check if -mds has got an argument or if the next string is an input command

           ULIBC=''
           ULIBC(1:MDS_PATH_LEN) = MDS_PATH(1:MDS_PATH_LEN)
           CALL UPCASE(ULIBC)
           ISIN = 0
           CALL ISANARGUMENT(ARGLIST,LENLIST,ULIBC,ISIN)
           IF ( ISIN==1 )THEN
               CALL GET_COMMAND_ARGUMENT(MDS_PATHI,ARGP)       !GETARG(PUSERLNAMI,ARGP)
               STRLN=LEN_TRIM(ARGP) 
C               CALL PRHELPINFO()
               WRITE(6,'(A)') ' '
               WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',
     *                         ARGP(1:STRLN)
               GLOBAL_ERROR = 1
               GOTO 4000
           ENDIF 

         ENDIF     !         IF (MDS_PATHI+1  > ARGN) THEN
       ENDIF
 4000  CONTINUE
C------------------------------------------------
C -MDSDIR
C------------------------------------------------
       IF (MDS_DIRI /= 0)THEN

        IF (MDS_DIRI+1  > ARGN) THEN

C          Case -DYLIB is last argument in the list.

           CALL GET_COMMAND_ARGUMENT(MDS_DIRI,ARGP)       
           STRLN=LEN_TRIM(ARGP) 
C           CALL PRHELPINFO()
           WRITE(6,'(A)') ' '
           WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',ARGP(1:STRLN)
           GLOBAL_ERROR = 1

        ELSE

            CALL GET_COMMAND_ARGUMENT(MDS_DIRI+1,MDS_PATH)       
            MDS_PATH_LEN=LEN_TRIM(MDS_PATH)
            ! check if -mdsdir has got an argument or if the next string is an input command
            ULIBC=''
            ULIBC(1:MDS_PATH_LEN) = MDS_PATH(1:MDS_PATH_LEN)
            CALL UPCASE(ULIBC)
            ISIN = 0
            CALL ISANARGUMENT(ARGLIST,LENLIST,ULIBC,ISIN)
            IF ( ISIN==1 )THEN
                  CALL GET_COMMAND_ARGUMENT(MDS_DIRI,ARGP)       !GETARG(PUSERLNAMI,ARGP)
                  STRLN=LEN_TRIM(ARGP) 
                  WRITE(6,'(A)') ' '
                  WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',
     *                         ARGP(1:STRLN)
                  GLOBAL_ERROR = 1

            ENDIF 

        ENDIF 
       ENDIF

C------------------------------------------------
C -TIME SET (hidden option)
C------------------------------------------------
       IF (PTIMER>0) THEN
          GOT_TIMER = 1
       ENDIF


!       ------------------------------------------------
!        Domain decomposition tuning : hidden option
!       ------------------------------------------------
!       DD_OPTIMIZATION = 0 --> default case, DD optimized for Broadwell processor      - AVX-2
!       DD_OPTIMIZATION = 1 --> DD optimized for Skylake processor                      - AVX-512
!       DD_OPTIMIZATION = 2 --> DD optimized for Sandy Bridge processor                 - SSE3
!       DD_OPTIMIZATION = 3 -->  DD optimized for ThunderX2 processor                   - ARM
       IF (DOMDEC_TUNING /= 0)THEN
         IF (DOMDEC_TUNING+1  > ARGN) THEN

C          Case -DYLIB is last argument in the list.

           CALL GET_COMMAND_ARGUMENT(DOMDEC_TUNING,ARGP)       !GETARG(DOMDEC_TUNING,ARGP)
           STRLN=LEN_TRIM(ARGP) 
C           CALL PRHELPINFO()
           WRITE(6,'(A)') ' '
           WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',
     *                    ARGP(1:STRLN)
           GLOBAL_ERROR = 1
 
         ELSE
           CALL GET_COMMAND_ARGUMENT(DOMDEC_TUNING+1,DOMDEC_CPU_TYPE)       !GETARG(DOMDEC_TUNING+1,DOMDEC_CPU_TYPE)
           LEN_DOMDEC_CPU_TYPE=LEN_TRIM(DOMDEC_CPU_TYPE)
C check if -DD_TUNING has got an argument or if the next string is an input command

           ULIBC(1:LEN_DOMDEC_CPU_TYPE) = DOMDEC_CPU_TYPE(1:LEN_DOMDEC_CPU_TYPE)
           CALL UPCASE(ULIBC)
           ISIN = 0
           CALL ISANARGUMENT(ARGLIST,LENLIST,ULIBC,ISIN)
           IF ( ISIN==1 )THEN
               CALL GET_COMMAND_ARGUMENT(DOMDEC_TUNING,ARGP)       !GETARG(DOMDEC_TUNING,ARGP)
               STRLN=LEN_TRIM(ARGP) 
C               CALL PRHELPINFO()
               WRITE(6,'(A)') ' '
               WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',
     *                         ARGP(1:STRLN)
               GLOBAL_ERROR = 1
               GOTO 3010
           ENDIF
           
           IF(DOMDEC_CPU_TYPE(1:LEN_DOMDEC_CPU_TYPE)=='AVX512') THEN
            DD_OPTIMIZATION = 1
           ELSEIF(DOMDEC_CPU_TYPE(1:LEN_DOMDEC_CPU_TYPE)=='SSE3') THEN
            DD_OPTIMIZATION = 2
           ELSEIF(DOMDEC_CPU_TYPE(1:LEN_DOMDEC_CPU_TYPE)=='ARMV8.0') THEN
            DD_OPTIMIZATION = 3
           ELSEIF(DOMDEC_CPU_TYPE(1:LEN_DOMDEC_CPU_TYPE)=='AVX2') THEN
            DD_OPTIMIZATION = 0
           ENDIF
         ENDIF     !         IF (DOMDEC_TUNING+1  > ARGN) THEN
       ELSE
!       Default case : check the os/cpu in order to use the best choice of element costs
        CALL GET_IBUILTIN_ARCH(DD_OPTIMIZATION)
!       in GET_IBUILTIN_ARCH :
!       0 - X86-64 Linux AVX-2
!       1 - X86-64 Linux AVX-512
!       2 - X86-64 Linux SSE3
!       3 - ARM64 Linux
!       4 - X86-64 Windows AVX-2 --> default case AVX-2 ; DD_OPTIMIZATION is set to 0 in grid2mat
       ENDIF
!       ------------------------------------------------
!        -RXALEA option
!       ------------------------------------------------
       IF (RANDM_ALEA/=0)THEN
         IF (RANDM_ALEA+1  > ARGN) THEN

C          Case -rxalea is last argument in the list.

           CALL GET_COMMAND_ARGUMENT(RANDM_ALEA,ARGP)       !GETARG(PNTHI,ARGP)
           STRLN=LEN_TRIM(ARGP) 

           WRITE(6,'(A)') ' '
           WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',
     *                    ARGP(1:STRLN)
           GLOBAL_ERROR=1
           GOTO 200
          
         ELSE
           CALL GET_COMMAND_ARGUMENT(RANDM_ALEA+1,STRING)

C check if -rxalea has got an argument or if the next string is an input command
           CALL UPCASE(STRING)
           ISIN = 0
           CALL ISANARGUMENT(ARGLIST,LENLIST,STRING,ISIN)
           IF ( ISIN==1 )THEN
               CALL GET_COMMAND_ARGUMENT(RANDM_ALEA,ARGP)
               STRLN=LEN_TRIM(ARGP) 

               WRITE(6,'(A)') ' '
               WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',ARGP(1:STRLN)
               GLOBAL_ERROR=1
               GOTO 200
           ENDIF 
C convert the argument to a real

           CALL GET_COMMAND_ARGUMENT(RANDM_ALEA+1,STRING)
           READ(STRING,'(F20.0)',ERR=5999) RANDM_ALEA_NBR
           RAND_STRUCT%CMD=.TRUE.
           RAND_STRUCT%ALEA_NBR=RANDM_ALEA_NBR
           RAND_STRUCT%ALEA=.TRUE.
C Read converting was OK goto 2000
           GOTO 5000

 5999      CONTINUE
C Error was found during Character to real translation
           STRLN=LEN_TRIM(STRING) 
           CALL GET_COMMAND_ARGUMENT(RANDM_ALEA,ARGP)
           STRLNA=LEN_TRIM(ARGP) 
           WRITE(6,'(A)') ' '
           WRITE(6,'(A,A,A,A,A)') 
     *         '*** ERROR in "',ARGP(1:STRLNA),'" argument : "',
     *          STRING(1:STRLN),'" is not an real value'
           GLOBAL_ERROR=1
           GOTO 200

 5000      CONTINUE
         ENDIF
       ENDIF
!       ------------------------------------------------
!        -RSEED option
!       ------------------------------------------------
       IF (RANDM_SEED/=0)THEN
         IF (RANDM_SEED+1  > ARGN) THEN

C          Case -rseed is last argument in the list.

           CALL GET_COMMAND_ARGUMENT(RANDM_SEED,ARGP)
           STRLN=LEN_TRIM(ARGP) 

           WRITE(6,'(A)') ' '
           WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',
     *                    ARGP(1:STRLN)
           GLOBAL_ERROR=1
           GOTO 200
          
         ELSE
           CALL GET_COMMAND_ARGUMENT(RANDM_SEED+1,STRING)

C check if -rseed has got an argument or if the next string is an input command
           CALL UPCASE(STRING)
           ISIN = 0
           CALL ISANARGUMENT(ARGLIST,LENLIST,STRING,ISIN)
           IF ( ISIN==1 )THEN
               CALL GET_COMMAND_ARGUMENT(RANDM_SEED,ARGP)
               STRLN=LEN_TRIM(ARGP) 

               WRITE(6,'(A)') ' '
               WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',ARGP(1:STRLN)
               GLOBAL_ERROR=1
               GOTO 200
           ENDIF 
C convert the argument to a real

           CALL GET_COMMAND_ARGUMENT(RANDM_SEED+1,STRING)
           READ(STRING,'(F20.0)',ERR=6999) RANDM_SEED_NBR
           RAND_STRUCT%CMD=.TRUE.
           RAND_STRUCT%SEED_NBR=RANDM_SEED_NBR
           RAND_STRUCT%SEED=.TRUE.
C Read converting was OK goto 2000
           GOTO 6000

 6999      CONTINUE
C Error was found during Character to real translation
           STRLN=LEN_TRIM(STRING) 
           CALL GET_COMMAND_ARGUMENT(RANDM_ALEA,ARGP)
           STRLNA=LEN_TRIM(ARGP) 
           WRITE(6,'(A)') ' '
           WRITE(6,'(A,A,A,A,A)') 
     *         '*** ERROR in "',ARGP(1:STRLNA),'" argument : "',
     *          STRING(1:STRLN),'" is not an real value'
           GLOBAL_ERROR=1
           GOTO 200

 6000      CONTINUE
         ENDIF
       ENDIF


C------------------------------------------------
C -GRP_SIZE hidden option
C------------------------------------------------
       IF (GOT_GRP_SIZE /= 0)THEN
         IF (GOT_GRP_SIZE+1  > ARGN) THEN
C          Case -grp_size is last argument in the list.

           CALL GET_COMMAND_ARGUMENT(GOT_GRP_SIZE,ARGP)
           STRLN=LEN_TRIM(ARGP) 
           WRITE(6,'(A)') ' '
           WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',
     *                    ARGP(1:STRLN)
           GLOBAL_ERROR=1
           GOTO 200          
         ELSE
           CALL GET_COMMAND_ARGUMENT(GOT_GRP_SIZE+1,STRING)

C check if -grp_size has got an argument or if the next string is an input command
           CALL UPCASE(STRING)
           ISIN = 0
           CALL ISANARGUMENT(ARGLIST,LENLIST,STRING,ISIN)
           IF ( ISIN==1 )THEN
               CALL GET_COMMAND_ARGUMENT(GOT_GRP_SIZE,ARGP)
               STRLN=LEN_TRIM(ARGP) 
C               CALL PRHELPINFO()
               WRITE(6,'(A)') ' '
               WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',
     *                         ARGP(1:STRLN)
               GLOBAL_ERROR=1
               GOTO 200
           ENDIF 
C convert the argument to an Integer
           GOT_NTH = 1
           CALL GET_COMMAND_ARGUMENT(GOT_GRP_SIZE+1,STRING)
           READ(STRING,'(I10)',ERR=3999) GRP_SIZE
           GRP_SIZE_BOOL = .TRUE.
C Read converting was OK goto 2000
           GOTO 2123


 3999      CONTINUE
C Error was found during Character to Integer translation
           STRLN=LEN_TRIM(STRING) 
           CALL GET_COMMAND_ARGUMENT(GOT_GRP_SIZE,ARGP)
           STRLNA=LEN_TRIM(ARGP) 
           WRITE(6,'(A)') ' '
           WRITE(6,'(A,A,A,A,A)') 
     *         '*** ERROR in "',ARGP(1:STRLNA),'" argument : "',
     *          STRING(1:STRLN),'" is not an integer value'
           GLOBAL_ERROR=1
           GOTO 200

 2123      CONTINUE
         ENDIF
       ENDIF



!       ------------------------------------------------
 3010  CONTINUE



 200   CONTINUE
       IF (GLOBAL_ERROR ==1)THEN
         WRITE(6,'(A)') ' '
         CALL PRHELPINFO() 
         CALL MY_EXIT(2)
       ENDIF

       IF(INFILE_BOOL.or.OUTFILE_BOOL) INOUT_BOOL = .TRUE. 


       RETURN
       END SUBROUTINE



Chd|====================================================================
Chd|  UPCASE                        source/starter/execargcheck.F 
Chd|-- called by -----------
Chd|        EXECARGCHECK                  source/starter/execargcheck.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE UPCASE (STRING) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       CHARACTER*(*) STRING
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER  LS,LC
C
       LS = LEN (STRING)
C
       DO LC = 1,LS
          IF (LGE(STRING(LC:LC),'a') .AND. 
     1        LLE(STRING(LC:LC),'z')) THEN
C
              STRING (LC:LC) = CHAR (ICHAR (STRING(LC:LC)) - 32)
            ELSE
            ENDIF
       END DO
       RETURN
       END
Chd|====================================================================
Chd|  ISANARGUMENT                  source/starter/execargcheck.F 
Chd|-- called by -----------
Chd|        EXECARGCHECK                  source/starter/execargcheck.F 
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE ISANARGUMENT(ARGLIST,LENLIST,ARG,ISIN)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       CHARACTER (LEN=12),INTENT(IN) :: ARGLIST(LENLIST)
       CHARACTER ARG*2096
       INTEGER LENLIST,ISIN
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER I
C-----------------------------------------------
       ISIN = 0
       DO I=1,LENLIST
        IF (TRIM(ARGLIST(I))==TRIM(ARG)) ISIN=1
       ENDDO 
       END



Chd|====================================================================
Chd|  PEXECINFO                     source/starter/execargcheck.F 
Chd|-- called by -----------
Chd|        EXECARGCHECK                  source/starter/execargcheck.F 
Chd|-- calls ---------------
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        PREXECINFO                    source/starter/execargcheck.F 
Chd|====================================================================
        SUBROUTINE PEXECINFO(IDUM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER IDUM
C-----------------------------------------------
        CALL PREXECINFO(IDUM)

        CALL MY_EXIT(0)
        END

Chd|====================================================================
Chd|  PREXECINFO                    source/starter/execargcheck.F 
Chd|-- called by -----------
Chd|        PEXECINFO                     source/starter/execargcheck.F 
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE PREXECINFO(IDUM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "build_info.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER IDUM
        CHARACTER*256 RFLEXNAM
        INTEGER LEN
C machine.inc include
        CHARACTER CPUNAM*20,ARCHTITLE*66
        INTEGER RDFLEXCOMP

C variables inutiles mais definies dans machine.inc
        INTEGER IBUILTIN,NSPMD,IEXPM,ICRAY,IRESP,IMACH,IRFORM,LENBT,READER_ID
        INTEGER READER_COMMIT_ID_SIZE,READER_COMMIT_ID_LEN
        CHARACTER(LEN=128):: READER_COMMIT_ID
        my_real :: BMUL0
C-----------------------------------------------
#include "machine.inc"
C-----------------------------------------------
         ! Grab Build ID from hm_reader library
         READER_COMMIT_ID_SIZE=128
         CALL HM_BUILD_ID(READER_COMMIT_ID,READER_COMMIT_ID_SIZE,READER_COMMIT_ID_LEN)


         WRITE(6,'(A,A)') ' '
         WRITE(6,'(A,A)') 'OpenRadioss Starter '
         WRITE(6,'(A,A)') ' '
         WRITE(6,'(A,A)') 'Platform release : ',CPUNAM
         WRITE(6,'(A,A)') 'Platform info : ',ARCHTITLE
         IF (IR4R8==1)THEN
           WRITE(6,'(A,A)') 
     *         '                Extended Single Precision Version'
         ENDIF
         WRITE(6,'(A,A)')   ' '
         WRITE(6,'(A,A)')   'Time of build : ',BTIME
         WRITE(6,'(A,A)')   'Date of build : ',BDATE
         WRITE(6,'(A,A)') ' '
         IF( BTAG(1:1) == ' ') THEN
           WRITE(6,'(A)')   'CommitID : User Build'
         ELSE
           WRITE(6,'(A)')     BTAG(1:LEN_TRIM(BTAG))
         ENDIF
         WRITE(6,'(A,A)') 'Hm_reader : ',READER_COMMIT_ID(1:READER_COMMIT_ID_LEN)
         WRITE(6,'(A,A)')   ' '

        END

Chd|====================================================================
Chd|  PHELPINFO                     source/starter/execargcheck.F 
Chd|-- called by -----------
Chd|        EXECARGCHECK                  source/starter/execargcheck.F 
Chd|-- calls ---------------
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        PRHELPINFO                    source/starter/execargcheck.F 
Chd|====================================================================
        SUBROUTINE PHELPINFO()
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        CALL PRHELPINFO()

        CALL MY_EXIT(0)
        END

Chd|====================================================================
Chd|  PRHELPINFO                    source/starter/execargcheck.F 
Chd|-- called by -----------
Chd|        EXECARGCHECK                  source/starter/execargcheck.F 
Chd|        GET_FILE_NAME_INFO            source/system/get_file_name_info.F
Chd|        PHELPINFO                     source/starter/execargcheck.F 
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE PRHELPINFO()
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "build_info.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       WRITE(6,'(A)') ' '
       WRITE(6,'(A,A)') 'RADIOSS Starter version ',VERS(1:LEN_VERS)
       WRITE(6,'(A)') ' '
       WRITE(6,'(A)')   'Command line arguments help: '
       WRITE(6,'(A,A)') '  -help           / -h                 : ',
     *                  'Print this message'
       WRITE(6,'(A,A)') '  -version        / -v                 : ',
     *                  'Print RADIOSS release information'
       WRITE(6,'(A,A)') '  -input [FILE]   / -i [FILE]          : ',
     *                  'Set RADIOSS Starter input file'
       WRITE(6,'(A,A)') '  -nspmd [INTEGER] / -np [INTEGER]     : ',
     *                  'Set Number of RADIOSS SPMD domains '
       WRITE(6,'(A,A)') '  -nthread [INTEGER] / -nt [INTEGER]   : ',
     *                  'Set Number of SMP threads per SPMD domain'
       WRITE(6,'(A,A)') '  -check                               : ',
     *                  'Check run - no restart file written'
       WRITE(6,'(A,A)') '  -notrap                              : ',
     *                  'Disable error trapping'
       WRITE(6,'(A,A)') '  -dynamic_lib [FILE] / -dylib [FILE]  : ',
     *                  'Set name to the dynamic library for Radioss User Interface'
       WRITE(6,'(A,A)') '  -mdsdir [PATH] / -mds_libpath [PATH] : ',
#ifdef _WIN32
     *                  'Set directory to MDS Library (default is %ALTAIR_HOME%/hwsolver/MultiscaleDesigner/%arch%)'
#else
     *                  'Set directory to MDS Library (default is $ALTAIR_HOME/hwsolver/MultiscaleDesigner/$arch)'
#endif
       WRITE(6,'(A,A)') '  -outfile=[PATH]                      : ',
     *                  'Set output file directory for all output and created files'
       WRITE(6,'(A,A)') '  -hstp_write                          : ',
     *                  'Write the xml file hst_input.hstp with the parameters for HyperStudy'
       WRITE(6,'(A,A)') '  -hstp_read                           : ',
     *                  'Read the xml file hst_input.hstp with the parameters from HyperStudy'
       WRITE(6,'(A,A)') '  -rxalea [real value]                 : ',
     *                  'Activation of nodal random noise with a [value] alea'
       WRITE(6,'(A,A)') '  -rseed [real value]                  : ',
     *                  'Optional value, set the seed value for nodal random noise'
       WRITE(6,'(A,A)') '  -python                              : ',
     *                  'Python function in model is safe and can be used'

       WRITE(6,'(A)') ' '
       WRITE(6,'(A)') ' '
       END

