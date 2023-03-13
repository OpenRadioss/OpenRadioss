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
Chd|  SPMD_EXCH_SMS6                source/mpi/ams/spmd_exch_sms6.F
Chd|-- called by -----------
Chd|        SMS_BUILD_DIAG                source/ams/sms_build_diag.F   
Chd|        SMS_MAV_LT                    source/ams/sms_pcg.F          
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_SMS6(V,NODNX_SMS,IAD_ELEM,FR_ELEM,
     .                           SIZE,LENR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
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
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODNX_SMS(*), IAD_ELEM(2,*), FR_ELEM(*), SIZE, LENR
      DOUBLE PRECISION 
     .        V(6,SIZE,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,MSGOFF,I,NOD,LOC_PROC,IERROR,
     .        SIZ,J,L,NB_NOD,K,M,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD)
      DOUBLE PRECISION RBUF(6*SIZE*LENR), SBUF(6*SIZE*LENR)
      DATA MSGOFF/214/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      L = 1
      IAD_RECV(1) = 1
      DO I=1,NSPMD

        SIZ = 0
        DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
          NOD = FR_ELEM(J)
          IF(NODNX_SMS(NOD)/=0)THEN
            SIZ = SIZ + 6*SIZE
          END IF
        END DO

        IF(SIZ/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,MPI_DOUBLE_PRECISION,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
      END DO
      L = 1
      IAD_SEND(1) = 1
      DO I=1,NSPMD
#include      "vectorize.inc"
        DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
          NOD = FR_ELEM(J)
          IF(NODNX_SMS(NOD)/=0)THEN
            DO M=1,6
             DO K=1,SIZE
              SBUF(L+K-1) =  V(M,K,NOD)
             END DO
             L = L + SIZE
            END DO
          END IF
        END DO
        IAD_SEND(I+1) = L
      ENDDO
C
C   echange messages
C
      DO I=1,NSPMD
C--------------------------------------------------------------------
ccc       IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
       IF(IAD_SEND(I+1)-IAD_SEND(I)>0)THEN
          MSGTYP = MSGOFF
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,MPI_DOUBLE_PRECISION,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF
C--------------------------------------------------------------------
      ENDDO
C
      DO I = 1, NSPMD
ccc        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
        IF(IAD_RECV(I+1)-IAD_RECV(I)>0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L = IAD_RECV(I)
#include      "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            NOD = FR_ELEM(J)
            IF(NODNX_SMS(NOD)/=0) THEN
             DO M=1,6
               DO K=1,SIZE
                V(M,K,NOD) = V(M,K,NOD)+RBUF(L+K-1)
               END DO
               L = L + SIZE
             END DO
            END IF
          END DO
        ENDIF
      END DO
C
      DO I = 1, NSPMD
ccc        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
        IF(IAD_SEND(I+1)-IAD_SEND(I)>0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
#endif
      RETURN
      END
