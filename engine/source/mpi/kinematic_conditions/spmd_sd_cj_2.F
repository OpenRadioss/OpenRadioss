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
C
Chd|====================================================================
Chd|  SPMD_SD_CJ_2                  source/mpi/kinematic_conditions/spmd_sd_cj_2.F
Chd|-- called by -----------
Chd|        SMS_BUILD_DIAG                source/ams/sms_build_diag.F   
Chd|        SMS_INI_KIN_1                 source/ams/sms_init.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_SD_CJ_2(NOD2ADD,LJOINT,FR_CJ,IADCJ,ICSIZE,
     2                      TAG_LNK_SMS,NODNX_SMS,INDX1_SMS)
C envoi de p0 vers les autres procs de A, AR, V, VR
C apres traitement des joints cylindriques
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "sms_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ICSIZE, NOD2ADD(*),LJOINT(*),FR_CJ(*),
     .        IADCJ(NSPMD+1,*), TAG_LNK_SMS(*), NODNX_SMS(*),
     .        INDX1_SMS(*)
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,MSGOFF,IERROR,LOC_PROC,NOD,L,I,J,N,
     .        IDEB,IAD,SIZ,A_AR,NBINDEX,
     .        INDEXI(NSPMD),REQ_S(NSPMD),STATUS(MPI_STATUS_SIZE),
     .        LEN(NSPMD),IBUF(ICSIZE)
      DATA MSGOFF/104/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      IF (LOC_PROC.EQ.1) THEN
        L = 0
        IDEB = 1

        DO I = 2, NSPMD
          LEN(I)=0
          DO N=1,NJOINT
           IF(TAG_LNK_SMS(N)/=0)
     .       LEN(I) = LEN(I)+IADCJ(I+1,N)-IADCJ(I,N)
          END DO
          IF(LEN(I).GT.0) THEN
            L=L+1
            INDEXI(L)=I
            DO N=1,NJOINT
              IF(TAG_LNK_SMS(N)==0)CYCLE
              DO J = IADCJ(I,N), IADCJ(I+1,N)-1
                NOD = FR_CJ(J)
                IBUF(IDEB  )  = NOD2ADD(NOD)
                IDEB = IDEB + 1
              END DO
            END DO
          END IF
        END DO
        NBINDEX = L
C
        IDEB = 1

        DO L=1,NBINDEX
         I = INDEXI(L)
         SIZ = LEN(I)
         MSGTYP = MSGOFF
         CALL MPI_ISEND(
     S     IBUF(IDEB),SIZ,MPI_INTEGER,IT_SPMD(I),MSGTYP,
     G     MPI_COMM_WORLD,REQ_S(L),IERROR)
         IDEB = IDEB + SIZ
        END DO
C
        DO L=1,NBINDEX
          CALL MPI_WAITANY(NBINDEX,REQ_S,I,STATUS,IERROR)
        ENDDO
      ELSEIF(ICSIZE.GT.0) THEN
C proc <> 0
        IDEB = 1
        SIZ = ICSIZE
        MSGTYP = MSGOFF
        CALL MPI_RECV(
     S    IBUF,SIZ,MPI_INTEGER,IT_SPMD(1),MSGTYP,
     G    MPI_COMM_WORLD,STATUS,IERROR)
        DO N = 1, NJOINT
          IF(TAG_LNK_SMS(N)==0)CYCLE
          DO J = IADCJ(1,N),IADCJ(NSPMD+1,N)-1
            NOD = FR_CJ(J)
            IF(NODNX_SMS(NOD)==0.AND.NOD2ADD(NOD)==0.AND.
     .         IBUF(IDEB)/=0)THEN
              NINDX1_SMS=NINDX1_SMS+1
              INDX1_SMS(NINDX1_SMS)=NOD
              NODNX_SMS(NOD)=NINDX1_SMS
              NOD2ADD(NOD)=1
            END IF
            IDEB = IDEB + 1
          END DO
        END DO
      END IF
C
#endif
      RETURN
      END
