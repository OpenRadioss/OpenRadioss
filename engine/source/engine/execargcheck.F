Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  EXECARGCHECK                  source/engine/execargcheck.F  
Chd|-- called by -----------
Chd|        RADIOSS0                      source/engine/radioss0.F      
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|        BUILD_MSG                     source/output/message/build_msg.F
Chd|        ISANARGUMENT                  source/engine/execargcheck.F  
Chd|        PEXECINFO                     source/engine/execargcheck.F  
Chd|        PHELPINFO                     source/engine/execargcheck.F  
Chd|        READ_MSGFILE                  source/output/message/read_msgfile.F
Chd|        UPCASE                        source/engine/execargcheck.F  
Chd|        CHECK_MOD                     ../common_source/modules/check_mod.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE EXECARGCHECK(GOT_INPUT,INPUT,LENI, 
     *                        GOT_PATH,  PATH,  LENP)
      USE MESSAGE_MOD
      USE CHECK_MOD
      USE INOUTFILE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "warn_c.inc"
#include      "commandline.inc"
#include      "userlib.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       INTEGER GOT_INPUT,LENI,GOT_PATH,LENP
       CHARACTER*256 INPUT
       CHARACTER*2048 PATH
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER ARGN,J,K,L
       INTEGER PHELPI,PEXECI,PINPUTI, PNTHI,PUSERLNAMI,MDS_PATHI
       INTEGER I,STRL,STRLN,ERR,LENLIST,ISIN,BEGIN,IERRMSG
       INTEGER IDUM
       CHARACTER*2096  ARGS,INPUTC,INPUTR,ARGP,MSG,STRING,INPUTE,
     .                 ARGREAD
       LOGICAL :: CONDITION
       CHARACTER C
       INTEGER IARGC,CDL_CASE,IJK
       CHARACTER :: LAST_LETTER,SEPARATOR
       character(len=2096) ARGS2,ARGS_REDUCE
       CHARACTER(len=4096) ULIBC
       PARAMETER (LENLIST=18)
       CHARACTER(LEN=12):: ARGLIST(LENLIST)
       EXTERNAL IARGC
       DATA ARGLIST/
     .    '-VERSION',  '-V',
     .    '-HELP'   ,  '-H',
     .    '-INPUT'  ,  '-I',
     .    '-NTHREAD' ,  '-NT',
     .    '-ERROR_MSG','-EM',
     .    '-NOTRAP',
     .    '-DYNAMIC_LIB', 
     .    '-DYLIB',
     .    '-MEM-MAP' ,
     .    '-INSPIRE',
     .    '-INSPIRE_ALM','-NORST',
     .    '-MDS_LIBPATH'/
C-----------------------------------------------
       IDUM=-1
       GOT_INPUT = 0
       GOT_NTH = 0
       INPUT=' '
       LENI=0
       PHELPI = 0 
       PEXECI = 0
       PINPUTI = 0
       PNTHI= 0
       IERRMSG=0
       PUSERLNAMI = 0
       GOT_USERL_ALTNAME=0
       GOT_MEM_MAP=0
       GOT_INSPIRE=0
       GOT_INSPIRE_ALM=0
       MDS_PATH_LEN = 0
       MDS_PATHI = 0

       ! Initialize MDS_PATH Array
       MDS_PATH=''

       !    /RFILE/OFF or -norst command line --> avoid to write restart files during a run
       !    default = write restart files
       RESTART_FILE = 1
       ! ---------------------
       ! infile / outfile cdl line
       INFILE_NAME_LEN = 0
       INFILE_NAME(1:INFILE_CHAR_LEN) = ''
       INFILE_BOOL=.FALSE.
       OUTFILE_NAME_LEN = 0
       OUTFILE_NAME(1:OUTFILE_CHAR_LEN) = ''
       OUTFILE_BOOL=.FALSE.
       INOUT_BOOL=.FALSE.

#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32
        SEPARATOR='\'
#elif 1
        SEPARATOR='/'
#endif

       ARGN = COMMAND_ARGUMENT_COUNT()!IARGC()

       DO I=1,ARGN
        CALL GETARG(I,ARGS)
        STRL=LEN_TRIM(ARGS)

        ARGS2(1:2096) = ''
        ARGS2(1:strl) = ARGS(1:strl)
        CALL UPCASE(ARGS)

        ARGS_REDUCE(1:9) = ARGS(1:9)
        CDL_CASE = 0
        IF(ARGS_REDUCE(1:9)=='-OUTFILE=') CDL_CASE = 1
        IF(ARGS_REDUCE(1:8)=='-INFILE=') CDL_CASE = 2
        IF(CDL_CASE==0) THEN
          SELECT CASE (ARGS)
C------------------------------------------------
C -NOTRAP
          CASE ( '-NOTRAP')
             ITRACE = -1
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
C -RST / MADYMO RESTART
          CASE ( '-RST')
             ERR = 0
C------------------------------------------------
C Madymo Option
          CASE ( '--')
             ERR = 0
C -NTHREAD, -NT
          CASE ( '-NTHREAD')
             IF (PNTHI==0) PNTHI = I
          CASE ( '-NT')
             IF (PNTHI==0) PNTHI = I 
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
C -MEM-MAP
          CASE ( '-MEM-MAP')
             GOT_MEM_MAP=1
          CASE ( '-INSPIRE_ALM')
             GOT_INSPIRE_ALM=1
          CASE ( '-INSPIRE')
             GOT_INSPIRE=1
C------------------------------------------------
C -NORST : no restart file command
          CASE ( '-NORST')
             RESTART_FILE = 0
C------------------------------------------------
C Lignes de commande qui ne sont pas definies
          CASE DEFAULT
             ERR = 0
             IF (I == 1)THEN
               ERR = 1
             ELSE
               CALL GETARG(I-1,ARGP)
               CALL UPCASE(ARGP)
               STRLN=LEN_TRIM(ARGP)
C Check if argument string is a variable to an argument
               IF (ARGP == '-I'      .OR. ARGP =='-INPUT' .OR.
     *             ARGP == '-RST'    .OR. ARGP =='-NT'    .OR.
     *             ARGP == '-NTHREAD'.OR. 
     *             ARGP == '-DYLIB'  .OR. ARGP =='-DYNAMIC_LIB'.OR.
     *             ARGP == '-MDS_LIBPATH')THEN
                 ERR = 0
               ELSE
                 ERR = 1
               ENDIF
             ENDIF
C
C Specificife Radioss Madymo
             IF (ERR == 1.AND.strl>4)THEN
               IF (ARGS(strl-3:strl)=='.XML')THEN
                 ERR = 0
               ENDIF
             ENDIF
             IF (ERR == 1)THEN
               CALL GETARG(I,ARGP)
               STRLN=LEN_TRIM(ARGP) 
               CALL PHELPINFO(2,ARGP,STRLN)
               CALL ARRET(7)
             ENDIF
C------------------------------------------------
         END SELECT
        ELSE
!   ------------------------------------------------
            SELECT CASE(CDL_CASE)
         ! -----------------------------
         !       -OUTFILE=... option
         ! -----------------------------
            CASE(1)

                OUTFILE_NAME_LEN = LEN_TRIM(ARGS2) - 9
                OUTFILE_NAME(1:OUTFILE_NAME_LEN) = ARGS2(10:LEN_TRIM(ARGS2))
                OUTFILE_BOOL=.TRUE.
                DO IJK=1,OUTFILE_NAME_LEN
                    LAST_LETTER = OUTFILE_NAME(IJK:IJK)
                ENDDO
                IF(LAST_LETTER/=SEPARATOR) THEN
                    OUTFILE_NAME(1:OUTFILE_NAME_LEN+1) = 
     .                       OUTFILE_NAME(1:OUTFILE_NAME_LEN)//SEPARATOR
                    OUTFILE_NAME_LEN = OUTFILE_NAME_LEN + 1
                ENDIF
         ! -----------------------------
         !       -INFILE=... option
         ! -----------------------------
            CASE(2)
                INFILE_NAME_LEN = LEN_TRIM(ARGS2) - 8
                INFILE_NAME(1:INFILE_NAME_LEN) = ARGS2(9:LEN_TRIM(ARGS2))
                INFILE_BOOL=.TRUE.
                DO IJK=1,INFILE_NAME_LEN
                    LAST_LETTER = INFILE_NAME(IJK:IJK)
                ENDDO
                IF(LAST_LETTER/=SEPARATOR) THEN
                    INFILE_NAME(1:INFILE_NAME_LEN+1) = 
     .                       INFILE_NAME(1:INFILE_NAME_LEN)//SEPARATOR
                    INFILE_NAME_LEN = INFILE_NAME_LEN + 1
                ENDIF
            END SELECT
!   ------------------------------------------------
        ENDIF
       ENDDO
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
          MSG= ' ' 
          CALL PHELPINFO(0,MSG,0) 
       ENDIF

C------------------------------------------------
C -ERROR_MSG SET
C------------------------------------------------
       IF (IERRMSG /= 0)THEN
         IF (IERRMSG+1  > ARGN) THEN

C          Case -I is last argument in the list.

           CALL GETARG(IERRMSG,ARGP)
           STRLN=LEN_TRIM(ARGP) 
           MSG = ARGP
           CALL PHELPINFO(1,MSG,STRLN)
           CALL ARRET(7)
         ELSE
           CALL GETARG(IERRMSG+1,INPUTR)
           LENI=LEN_TRIM(INPUTR)

C check if -i has got an argument or if the next string is an input command

           INPUTC = INPUTR
           CALL UPCASE(INPUTC)
           ISIN = 0
           CALL ISANARGUMENT(ARGLIST,LENLIST,INPUTC,ISIN)
           IF ( ISIN==1 )THEN
               CALL GETARG(PINPUTI,ARGP)
               STRLN=LEN_TRIM(ARGP) 
               MSG=ARGP
               CALL PHELPINFO(1,MSG,STRLN)
               CALL ARRET(7)
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

           CALL GETARG(PINPUTI,ARGP)
           STRLN=LEN_TRIM(ARGP) 
           MSG = ARGP
           CALL PHELPINFO(1,MSG,STRLN)
           CALL ARRET(7)
         ELSE
           CALL GETARG(PINPUTI+1,INPUTR)
           LENI=LEN_TRIM(INPUTR)
           GOT_INPUT = 1

C check if -i has got an argument or if the next string is an input command

           INPUTC = INPUTR
           CALL UPCASE(INPUTC)
           ISIN = 0
           CALL ISANARGUMENT(ARGLIST,LENLIST,INPUTC,ISIN)
           IF ( ISIN==1 )THEN
               CALL GETARG(PINPUTI,ARGP)
               STRLN=LEN_TRIM(ARGP) 
               MSG=ARGP
               CALL PHELPINFO(1,MSG,STRLN)
               CALL ARRET(7)
           ENDIF 

           BEGIN=LEN_TRIM(INPUTR)
           CONDITION = .FALSE.
           DO WHILE (BEGIN > 0 .AND. .NOT.CONDITION)
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
           ! -input/-i was not set. Exiting with error message.
           MSG= ' ' 
           CALL PHELPINFO(6,MSG,0)
           CALL ARRET(7)
       ENDIF

C------------------------------------------------
C -NTHREAD SET
C------------------------------------------------
       IF (PNTHI /= 0)THEN
         IF (PNTHI+1  > ARGN) THEN
C case -nt is last argument in the list.
           CALL GETARG(PNTHI,ARGP)
           STRLN=LEN_TRIM(ARGP)
           MSG = ARGP
           CALL PHELPINFO(1,MSG,STRLN)
           CALL ARRET(7)
         ELSE
           CALL GETARG(PNTHI+1,STRING)
C check if -nt has got an argument or if the next string is an input command
           CALL UPCASE(STRING)
           ISIN = 0
           CALL ISANARGUMENT(ARGLIST,LENLIST,STRING,ISIN)
           IF ( ISIN==1 )THEN
             CALL GETARG(PNTHI,ARGP)
             STRLN=LEN_TRIM(ARGP) 
             MSG=ARGP
             CALL PHELPINFO(1,MSG,STRLN)
             CALL ARRET(7)
           END IF
           GOT_NTH = 1
C convert the argument to an Integer
           CALL GETARG(PNTHI+1,STRING)
           READ(STRING,'(I10)',ERR=1999) NTH
C Read converting was OK goto 2000
           GOTO 2000
 1999      CONTINUE
C Error was found during Character to Integer translation
           STRLN=LEN_TRIM(STRING) 
c           CALL GETARG(PNTHI,ARGP)
           MSG=STRING
           CALL PHELPINFO(4,MSG,STRLN)
           CALL ARRET(7)
 2000      CONTINUE
         ENDIF
       ENDIF 
C------------------------------------------------
C -DYLIB SET
C------------------------------------------------
       IF (PUSERLNAMI /= 0)THEN
         IF (PUSERLNAMI+1  > ARGN) THEN

C          Case -DYLIB is last argument in the list.

           CALL GETARG(PUSERLNAMI,ARGP)
           STRLN=LEN_TRIM(ARGP) 
           CALL PHELPINFO(1,ARGP,STRLN)
 
         ELSE
           CALL GETARG(PUSERLNAMI+1,USERL_ALTNAME)
           LEN_USERL_ALTNAME=LEN_TRIM(USERL_ALTNAME)
           GOT_USERL_ALTNAME = 1
C check if -dylib has got an argument or if the next string is an input command
           ULIBC=''
           ULIBC(1:LEN_USERL_ALTNAME) = USERL_ALTNAME(1:LEN_USERL_ALTNAME)
           CALL UPCASE(ULIBC)
           ISIN = 0
           CALL ISANARGUMENT(ARGLIST,LENLIST,ULIBC,ISIN)
           IF ( ISIN==1 )THEN
               CALL GETARG(PUSERLNAMI,ARGP)
               STRLN=LEN_TRIM(ARGP) 
               CALL PHELPINFO(1,ARGP,STRLN)
               CALL ARRET(7)
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
           CALL PHELPINFO(1,ARGP,STRLN)
 
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
               CALL GETARG(MDS_PATHI,ARGP)
               STRLN=LEN_TRIM(ARGP) 
               CALL PHELPINFO(1,ARGP,STRLN)
               CALL ARRET(7)
           ENDIF 

         ENDIF     !         IF (MDS_PATHI+1  > ARGN) THEN
       ENDIF
 4000  CONTINUE
C------------------------------------------------

       END



Chd|====================================================================
Chd|  UPCASE                        source/engine/execargcheck.F  
Chd|-- called by -----------
Chd|        EXECARGCHECK                  source/engine/execargcheck.F  
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
       INTEGER  STLEN,LS,LC
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
Chd|  ISANARGUMENT                  source/engine/execargcheck.F  
Chd|-- called by -----------
Chd|        EXECARGCHECK                  source/engine/execargcheck.F  
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
       INTEGER I,STRLEN
       CHARACTER ARGC*2096
C-----------------------------------------------
       STRLEN=LEN_TRIM(ARG)
       ARGC(1:2096)=''
       I=1
       DO WHILE (I<=STRLEN .AND. ARG(I:I)/='=')
          ARGC(I:I)=ARG(I:I)
          I=I+1
          IF(I>STRLEN)EXIT ! check bounds issue for ARG (I:I) if I=STRLEN+1
       ENDDO

       ISIN = 0
       DO I=1,LENLIST
        IF (TRIM(ARGLIST(I))==TRIM(ARGC)) ISIN=1
       ENDDO 
       END



	SUBROUTINE PEXECINFO(IDUM)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,ICODE
      INTEGER MYRANK, NNODES
      INTEGER KEY, IERR
#endif
      INTEGER IDUM
C-----------------------------------------------
#ifdef MPI
        KEY = 0
        call MPI_INITIALIZED(KEY, IERR)
        CALL MPI_INIT(IERROR)
        CALL MPI_COMM_SIZE(MPI_COMM_WORLD, NNODES, IERROR)
        CALL MPI_COMM_RANK(MPI_COMM_WORLD, MYRANK, IERROR)

        if (MYRANK==0) CALL PREXECINFO(IDUM)
        
        CALL MPI_BARRIER(MPI_COMM_WORLD,IERROR)

        CALL MPI_FINALIZE(IERROR)

#elif 1
        CALL PREXECINFO(IDUM)
#endif
        CALL MY_EXIT(0)
        END

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
#include      "titr_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        CHARACTER*256 RFLEXNAM,GPUTITLE,ARCHTIT*66
        INTEGER LEN,LBT
C machine.inc include
        INTEGER RDFLEXCOMP
        INTEGER IDUM

C-----------------------------------------------

         LBT = LEN_TRIM(BTAG)
         WRITE(6,'(A,A)') ' '
         WRITE(6,'(A,A)') 'OpenRadios Engine '
         WRITE(6,'(A,A)') ' '
         WRITE(6,'(A,A)') 'Platform release : ',BNAME(1:LENBNAM)
         WRITE(6,'(A,A)') 'Platform info : ',ARCHTITLE

         IF (IR4R8==1)THEN
           WRITE(6,'(A,A)') 
     *         '                Extended Single Precision Version'
         ENDIF
         WRITE(6,'(A,A)')   ' '
         WRITE(6,'(A,A)')   'Time of build : ',BTIME
         WRITE(6,'(A,A)')   'Date of build : ',BDATE
         WRITE(6,'(A,A)')   ' '


        END

	SUBROUTINE PHELPINFO(ERRN,EMSG,SMSG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ERRN,STRLN,SMSG
      CHARACTER*256 ARGP,EMSG*256
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,ICODE
      INTEGER MYRANK, NNODES
      INTEGER KEY, IERR
#endif
C-----------------------------------------------
#ifdef MPI
        KEY = 0
        call MPI_INITIALIZED(KEY, IERR)
        CALL MPI_INIT(IERROR)
        CALL MPI_COMM_SIZE(MPI_COMM_WORLD, NNODES, IERROR)
        CALL MPI_COMM_RANK(MPI_COMM_WORLD, MYRANK, IERROR)

        if (MYRANK==0) CALL PRHELPINFO(ERRN,EMSG,SMSG)
        
        CALL MPI_BARRIER(MPI_COMM_WORLD,IERROR)

        CALL MPI_FINALIZE(IERROR)

#elif 1
        CALL PRHELPINFO(ERRN,EMSG,SMSG)
#endif
        CALL ARRET(7)
        END

Chd|====================================================================
Chd|  PRHELPINFO                    source/engine/execargcheck.F  
Chd|-- called by -----------
Chd|        PHELPINFO                     source/engine/execargcheck.F  
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE PRHELPINFO(ERRN,EMSG,SMSG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "build_info.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ERRN,SMSG,STRLNA
      CHARACTER EMSG*256, ARGP*256
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       IF (ERRN == 1)THEN
         WRITE(6,'(A)') ' '
         WRITE(6,'(A,A)') '*** ERROR : Missing argument to ',
     *                    EMSG(1:SMSG)
         WRITE(6,'(A)') ' '         
       ENDIF

       IF (ERRN == 2)THEN
         WRITE(6,'(A)') ' '
         WRITE(6,'(A,A)') '***  ERROR : Unknown command line argument:',
     *                    EMSG(1:SMSG)
         WRITE(6,'(A)') ' '         
       ENDIF

       IF (ERRN == 3)THEN
         WRITE(6,'(A)') ' ' 
         WRITE(6,'(A,A)') 'WRONG RADIOSS INPUT FILE NAME: ',EMSG(1:SMSG)
         WRITE(6,'(A)') ' ' 
       ENDIF

       IF (ERRN == 4)THEN
         WRITE(6,'(A)') ' '
         WRITE(6,'(A,A,A,A,A)') 
     *     '*** ERROR : "',EMSG(1:SMSG),'" is not an integer value'
       ENDIF

       IF (ERRN == 5)THEN
         WRITE(6,'(A)') ' '
         WRITE(6,'(A,A,A,A,A)') 
     *     '*** ERROR : Wrong "',EMSG(1:SMSG),'" option'
       ENDIF

       IF (ERRN == 6)THEN
         WRITE(6,'(A)') ' '
         WRITE(6,'(A)') '*** ERROR : No input deck set. Use -input [Engine input file] '
       ENDIF

       WRITE(6,'(A)') ' '
       WRITE(6,'(A,A)') 'RADIOSS Engine version ',VERS(1:LEN_VERS)
       WRITE(6,'(A)') ' '
       WRITE(6,'(A)')   'Command line arguments help: '
       WRITE(6,'(A,A)') '  -help         / -h                  : ',
     *                  'Print this message'
       WRITE(6,'(A,A)') '  -version      / -v                  : ',
     *                  'Print RADIOSS release information'
       WRITE(6,'(A,A)') '  -input [FILE] / -i [FILE]           : ',
     *                  'Set RADIOSS Engine input file'
       WRITE(6,'(A,A)') '  -nthread [INTEGER] / -nt [INTEGER]  : ',
     *                  'Set Number of SMP threads per SPMD domain'
C       WRITE(6,'(A,A)') '  -gpuid [INTEGER]                    : ',
C    *                  'Set GPU device number'
       WRITE(6,'(A,A)') '  -notrap                             : ',
     *                  'Disable error trapping'
       WRITE(6,'(A,A)') '  -norst                              : ',
     *                  'Do not write restart files'
       WRITE(6,'(A,A)') '  -dynamic_lib [FILE] / -dylib [FILE] : ',
     *                  'Set name to the dynamic library for Radioss User Interface'
       WRITE(6,'(A,A)') '  -mds_libpath [PATH]                 : ',
#ifdef _WIN32
     *                  'Set directory to MDS Library (default is %ALTAIR_HOME%/hwsolver/MultiscaleDesigner/%arch%)'
#else
     *                  'Set directory to MDS Library (default is $ALTAIR_HOME/hwsolver/MultiscaleDesigner/$arch)'
#endif

       WRITE(6,'(A,A)') '  -outfile=[PATH]                     : ',
     *                  'Set output file directory for all output and created files'
       WRITE(6,'(A)') ' '


      END
