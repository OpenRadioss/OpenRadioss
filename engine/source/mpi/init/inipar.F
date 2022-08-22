Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
#ifdef MPI
Chd|====================================================================
Chd|  INIPAR                        source/mpi/init/inipar.F      
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|====================================================================
      SUBROUTINE INIPAR(ITID,ICAS,NNODES,INPUT,GOT_INPUT,NBTASK)
C-----------------------------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "my_real.inc" 
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITID(*),ICAS, NNODES,GOT_INPUT,NBTASK(*)
      CHARACTER*100 INPUT
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include  "com01_c.inc"
#include  "scr03_c.inc"
#include  "task_c.inc"
#include  "scr05_c.inc"
#include  "units_c.inc"
#include  "commandline.inc"
      INTEGER KEY, IERR
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,ICODE
      INTEGER MYRANK, JRANK, I, J, NTHREAD1, LOCAL, RLEN
      CHARACTER (LEN=255) :: STR
      CHARACTER (LEN=MPI_MAX_PROCESSOR_NAME) :: HOSTNAME(NSPMD),
     *     HSTNAM,HOST
      INTEGER PERM(NSPMD),HOST_NUMBERS(NSPMD),HOST_NUMBER
      INTEGER BUFFER(NSPMD*2),NSPMDx2
#if defined(_OPENMP)
      INTEGER OMP_GET_MAX_THREADS
#endif
C--------------------------------------------------
C     CONVENTIONS:
C                  ispmd 0...NSPMD-1
C--------------------------------------------------
      IF(ICAS.EQ.1) THEN
        REAL = MPI_REAL
        IF(IR4R8.EQ.2) REAL = MPI_DOUBLE_PRECISION
C
        IF(ICRAY.EQ.1) THEN
          INTSIZE = 8
          IFLSIZE = 8
        ELSE
          INTSIZE = 4
          IFLSIZE = 4*IR4R8
        ENDIF
C
        KEY = 0
        CALL MPI_INITIALIZED(KEY, IERR)

        CALL MPI_INIT(IERROR)
        CALL MPI_COMM_SIZE(MPI_COMM_WORLD, NNODES, IERROR)
        CALL MPI_COMM_RANK(MPI_COMM_WORLD, MYRANK, IERROR)
C
        DO JRANK=1,NNODES
          IT_SPMD(JRANK) = JRANK-1
        END DO
C
        IF (MYRANK.EQ.0) THEN
C         I AM THE FIRST ONE
          ISPMD = 0
C
C Calcul du nombre de threads
C
#if defined(_OPENMP)
          STR = ' '
          CALL  GETENV('OMP_NUM_THREADS',STR)
          NTHREAD1=0
C nthread1 : nombre de threads fixe par la variable environnememt
          READ(STR,'(I10)',ERR=999)NTHREAD1
          GOTO 1000
 999      CONTINUE
          NTHREAD1 = -1
 1000     CONTINUE
          IF(NTHREAD1>0)THEN
            NTHREAD=NTHREAD1
          ELSE
            NTHREAD=NTHREAD0 ! defaut run precedent
          END IF
          IF (GOT_NTH == 1) THEN
            IF (NTHREAD>1 .AND. (NTH/= NTHREAD .OR. (NTH==NTHREAD .AND.
     +          NTHREAD1>0.AND.NTHREAD0>1.AND.NTHREAD1/=NTHREAD0)))THEN
              WRITE(IOUT,*)
     +      '** WARNING : -nthread OPTION USED, DEFAULT SETTING IGNORED'
              WRITE(IOUT,*)' '
            END IF
            NTHREAD = NTH
          ELSE
            IF(NTHREAD1>0.AND.NTHREAD0>1.AND.NTHREAD1/=NTHREAD0)THEN
                WRITE(IOUT,*)
     +      '** WARNING : OMP_NUM_THREADS SET, DEFAULT SETTING IGNORED'
                WRITE(IOUT,*)' '
            END IF
          ENDIF
          IF(NTHREAD.LE.0)  NTHREAD=1
          IF(NTHREAD.GT.NTHMAX) NTHREAD=NTHMAX
#elif 1
          NTHREAD = 1
#endif
        ELSE
          CALL MPI_BCAST(
     1      NSPMD,1,MPI_INTEGER,IT_SPMD(1),MPI_COMM_WORLD,
     2      IERROR )
C recuperation de NTHREAD depuis process 0
          CALL MPI_BCAST(
     1      NTHREAD,1,MPI_INTEGER,IT_SPMD(1),MPI_COMM_WORLD,
     2      IERROR )
C
          IF (GOT_INPUT == 0)THEN
            CALL MPI_BCAST(INPUT,100,MPI_CHARACTER,IT_SPMD(1),
     .                     MPI_COMM_WORLD,IERROR)
          ENDIF
C
          DO J=2,NSPMD
            IF(MYRANK.EQ.IT_SPMD(J))THEN
               ISPMD = J-1
            END IF
          END DO
cccc
C code special pour MIC
#if defined(__MIC__)
          STR = ' '
          CALL  GETENV('OMP_NUM_THREADS',STR)
          NTHREAD1=0
C nthread1 : nombre de threads fixe par la variable environnement sur le MIC
          READ(STR,'(I10)',ERR=1999)NTHREAD1
          GOTO 2000
 1999     CONTINUE
          NTHREAD1 = -1
 2000     CONTINUE
          IF(NTHREAD1>0 .AND. NTHREAD1 /= NTHREAD)THEN
            NTHREAD=NTHREAD1
            WRITE(ISTDO,'(A,I4,A,I4)')
     .        '** WARNING: RESET OMP_NUM_THREADS TO:',
     .        NTHREAD,' FOR MPI PROCESS',ISPMD+1 
          ENDIF
#endif
cccc               
C control of nthread per MPI process
          CALL MPI_GATHER(NTHREAD,1,MPI_INTEGER,NBTASK,1,MPI_INTEGER,
     .                    IT_SPMD(1),MPI_COMM_WORLD,IERROR)            
           
            END IF
      ELSE IF(ICAS.EQ.3) THEN
        IF(ISPMD == 0) THEN
C
C test cooherence NSPMD et nombre de proccesses MPI demandes (-np)
C
          IF (NSPMD .NE. NNODES) THEN
            WRITE(IOUT,*)
     .      'THE REQUIRED NUMBER OF MPI PROCESSES DOES NOT MATCH MPIRUN'
            WRITE(IOUT,*)
     .      'PLEASE, RUN WITH THE PROPER NUMBER OF MPI PROCESSES'
            WRITE(IOUT,*)
     .      'REQUIRED (number of .rst files) NSPMD =',NSPMD
            WRITE(IOUT,*)
     .      'AVAILABLE (-np argument of mpirun)    =',NNODES
            WRITE(IOUT,*)' '
            WRITE(ISTDO,*)
     .      'THE REQUIRED NUMBER OF MPI PROCESSES DOES NOT MATCH MPIRUN'
            WRITE(ISTDO,*)
     .      'PLEASE, RUN WITH THE PROPER NUMBER OF MPI PROCESSES'
            WRITE(ISTDO,*)' '
            WRITE(IOUT,*)
     .             ' E R R O R     T E R M I N A T I O N'
            WRITE(ISTDO,*)
     .             ' E R R O R     T E R M I N A T I O N'
            WRITE(IOUT,*)
     .             ' TOTAL NUMBER OF CYCLES  :', NCYCLE
            WRITE(ISTDO,*)
     .             ' TOTAL NUMBER OF CYCLES  :', NCYCLE
            ICODE=2
            CALL MPI_ABORT(MPI_COMM_WORLD,ICODE,ierror)
            CALL ARRET(7)
          END IF
          IF(NSPMD > 1) THEN
C envoi NSPMD de process0 vers les autres processes
            CALL MPI_BCAST(NSPMD,1,MPI_INTEGER,IT_SPMD(1),
     .                     MPI_COMM_WORLD,IERROR)
C envoi de NTHREAD de process0 vers les autres processes
            CALL MPI_BCAST(NTHREAD,1,MPI_INTEGER,IT_SPMD(1),
     .                     MPI_COMM_WORLD,IERROR)
C
            IF (GOT_INPUT == 0)THEN
              CALL MPI_BCAST(INPUT,100,MPI_CHARACTER,IT_SPMD(1),
     .                       MPI_COMM_WORLD,IERROR)
            ENDIF
            
C control of nthread per MPI process
            CALL MPI_GATHER(NTHREAD,1,MPI_INTEGER,NBTASK,1,MPI_INTEGER,
     .                      IT_SPMD(1),MPI_COMM_WORLD,IERROR)            
            IF(ISPMD==0)THEN
              NBTASK(NSPMD+1)=0
              DO I=1,NSPMD
                NBTASK(NSPMD+1)=NBTASK(NSPMD+1)+NBTASK(I)
              END DO
          END IF
          ELSE
            NBTASK(1)=NTHREAD
            NBTASK(2)=NTHREAD
          END IF
C
        END IF
C------------------------------------------------------------
C     Starting from now, ALL TASK ARE EQUAL ===> SPMD program
C------------------------------------------------------------
C
C Init OpenMP
C
#if defined(_OPENMP)
        CALL OMP_SET_NUM_THREADS(NTHREAD)
        DO I = 1, 2*INTSEG
          CALL OMP_INIT_LOCK(LLOCK(1,I))
        ENDDO
#endif
C
C Init local SPMD numbering on a node (L_SPMD)
C
        CALL MPI_GET_PROCESSOR_NAME(HSTNAM,RLEN,IERROR)
        CALL MPI_GATHER(
     .     HSTNAM,MPI_MAX_PROCESSOR_NAME,MPI_CHARACTER,
     .     HOSTNAME   ,MPI_MAX_PROCESSOR_NAME,MPI_CHARACTER,
     .     IT_SPMD(1),MPI_COMM_WORLD,IERROR)

        IF(ISPMD==0)THEN
C permutations during sort are saved
          DO I = 1,NSPMD
            PERM(I) = I
          END DO
C tri bulle basic
          DO I = 1, NSPMD-1
            DO J = I, NSPMD
              IF(HOSTNAME(I) > HOSTNAME(J))THEN
                HOST = HOSTNAME(I)
                HOSTNAME(I) = HOSTNAME(J)
                PERM(I) = J
                HOSTNAME(J) = HOST
                PERM(J) = I
              END IF
            END DO
          END DO
          HOST = ' '
          LOCAL=0
          HOST_NUMBER = 0
          DO I = 1, NSPMD
            IF(HOST .NE. HOSTNAME(I))THEN
              HOST = HOSTNAME(I)
              LOCAL = 1
              HOST_NUMBER = HOST_NUMBER + 1
            ELSE
              LOCAL = LOCAL + 1
            ENDIF
            L_SPMD(PERM(I))=LOCAL-1
            HOST_NUMBERS(PERM(I))=HOST_NUMBER
          ENDDO
C         preparing MPI buffer 
C         Local rank in the node
          BUFFER(1:NSPMD) = L_SPMD(1:NSPMD) 
C         host numbers
          BUFFER(NSPMD+1:2*NSPMD) = HOST_NUMBERS(1:NSPMD)
        ENDIF
        NSPMDx2 = NSPMD*2
        CALL MPI_BCAST(BUFFER,NSPMDx2,MPI_INTEGER,IT_SPMD(1),
     .                 MPI_COMM_WORLD,IERROR)

        L_SPMD(1:NSPMD) = BUFFER(1:NSPMD)         
        HOST_NUMBERS(1:NSPMD) = BUFFER(NSPMD+1:2*NSPMD)
    
        NSPMD_PER_NODE=0 
C       Count the number of SPMD on the local host
        DO I = 1,NSPMD
          IF(HOST_NUMBERS(I) == HOST_NUMBERS(ISPMD+1)) THEN
            NSPMD_PER_NODE = NSPMD_PER_NODE + 1 
          ENDIF
        END DO  


      ELSE IF(ICAS.EQ.2) THEN
C
        CALL MPI_BARRIER(MPI_COMM_WORLD,IERROR)
C
        CALL MPI_FINALIZE(IERROR)
      ENDIF
C
      RETURN
      END
C MPI

#elif 1
C
C routines simplifiees non mpi
C
Chd|====================================================================
Chd|  INIPAR                        source/mpi/init/inipar.F      
Chd|-- called by -----------
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|-- calls ---------------
Chd|        ARRET                         source/system/arret.F         
Chd|====================================================================
      SUBROUTINE INIPAR(ITID,ICAS,NNODES,INPUT,GOT_INPUT,NBTASK)
C-----------------------------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
#include      "my_real.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITID(*),ICAS, NNODES,GOT_INPUT,NBTASK(*)
      CHARACTER*100 INPUT
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include  "com01_c.inc"
#include  "scr03_c.inc"
#include  "task_c.inc"
#include  "scr05_c.inc"
#include  "units_c.inc"
#include  "commandline.inc"
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
      INTEGER ICODE, I, NTHREAD1
#if defined(_OPENMP)
      INTEGER OMP_GET_MAX_THREADS
#endif
      CHARACTER (LEN=255) :: STR
C--------------------------------------------------
C     CONVENTIONS:
C                  ispmd 0...NSPMD-1
C--------------------------------------------------
      IF(ICAS.EQ.1) THEN
C
        REAL = 4
        IF(IR4R8.EQ.2) REAL = 8
C
        IF(ICRAY.EQ.1) THEN
          INTSIZE = 8
          IFLSIZE = 8
        ELSE
          INTSIZE = 4
          IFLSIZE = 4*IR4R8
        ENDIF
C
        NNODES= 1
        ISPMD = 0
#if defined(_OPENMP)
        STR = ' '
        CALL  GETENV('OMP_NUM_THREADS',STR)
        NTHREAD1=0
C nthread1 : nombre de threads fixe par la variable environnememt
        READ(STR,'(I10)',ERR=999)NTHREAD1
        GOTO 1000
 999    CONTINUE
        NTHREAD1 = -1
 1000   CONTINUE
        IF(NTHREAD1>0)THEN
          NTHREAD=NTHREAD1
        ELSE
          NTHREAD=NTHREAD0 ! defaut run precedent
        END IF
        IF (GOT_NTH == 1) THEN
          IF (NTHREAD>1 .AND. (NTH/= NTHREAD .OR. (NTH==NTHREAD .AND.
     +        NTHREAD1>0.AND.NTHREAD0>1.AND.NTHREAD1/=NTHREAD0)))THEN
            WRITE(IOUT,*)
     +    '** WARNING : -nthread OPTION USED, DEFAULT SETTING IGNORED'
            WRITE(IOUT,*)' '
          END IF
          NTHREAD = NTH
        ELSE
          IF(NTHREAD1>0.AND.NTHREAD0>1.AND.NTHREAD1/=NTHREAD0)THEN
              WRITE(IOUT,*)
     +    '** WARNING : OMP_NUM_THREADS SET, DEFAULT SETTING IGNORED'
              WRITE(IOUT,*)' '
          END IF
        ENDIF
        IF(NTHREAD.LE.0)  NTHREAD=1
        IF(NTHREAD.GT.NTHMAX) NTHREAD=NTHMAX
C
#elif 1
        NTHREAD = 1
#endif
      ELSE IF(ICAS.EQ.3) THEN
C
        IF (NSPMD .NE. NNODES) THEN
          WRITE(IOUT,*)
     .    'NON HYBRID EXECUTABLE ONLY SUPPORTS ONE SPMD DOMAIN'
          WRITE(IOUT,*)
     .    'PLEASE, RUN STARTER WITH -nspmd 1 OR USE HMPP EXECUTABLE'
          WRITE(IOUT,*)
     .        'REQUIRED (number of .rst files) NSPMD =',NSPMD
          WRITE(IOUT,*)
     .        'AVAILABLE                             =',NNODES
          WRITE(IOUT,*)' '
          WRITE(ISTDO,*)
     .    'NON HYBRID EXECUTABLE ONLY SUPPORTS ONE SPMD DOMAIN'
          WRITE(ISTDO,*)
     .    'PLEASE, RUN STARTER WITH -nspmd 1 OR USE HMPP EXECUTABLE'
          WRITE(ISTDO,*)' '
          WRITE(IOUT,*)
     .             ' E R R O R     T E R M I N A T I O N'
          WRITE(ISTDO,*)
     .             ' E R R O R     T E R M I N A T I O N'
          WRITE(IOUT,*)
     .             ' TOTAL NUMBER OF CYCLES  :', NCYCLE
          WRITE(ISTDO,*)
     .             ' TOTAL NUMBER OF CYCLES  :', NCYCLE
          ICODE=2
          CALL ARRET(7)
C
        ENDIF
        NBTASK(1)=NTHREAD
        NBTASK(2)=NTHREAD
#if defined(_OPENMP)
        CALL OMP_SET_NUM_THREADS(NTHREAD)
        DO I = 1, 2*INTSEG
          CALL OMP_INIT_LOCK(LLOCK(1,I))
        ENDDO
#endif
      ELSE IF(ICAS.EQ.2) THEN
#if defined(_OPENMP)
#endif
      ENDIF
      RETURN
      END

#endif
