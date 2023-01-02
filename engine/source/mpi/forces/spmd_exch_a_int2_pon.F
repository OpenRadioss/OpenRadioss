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
C
Chd|====================================================================
Chd|  SPMD_EXCH_A_INT2_PON          source/mpi/forces/spmd_exch_a_int2_pon.F
Chd|-- called by -----------
Chd|        INTTI1                        source/interfaces/interf/intti1.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_A_INT2_PON(
     1   FR_I2M,IAD_I2M,ADDCNI2,PROCNI2,FR_NBCCI2,
     2   I2SIZE,LENR   ,LENS   ,FSKYI2,INTTH2,
     3   FTHESKYI2,CONDNSKYI2, I2SIZEMEC )
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
#include      "parit_c.inc"
#include      "spmd_c.inc"
#include      "units_c.inc"
#include      "timeri_c.inc"
#include      "warn_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IAD_I2M(*),FR_I2M(*),FR_NBCCI2(2,*),
     .        ADDCNI2(*), PROCNI2(*),
     .        I2SIZE ,LENR ,LENS,INTTH2,I2SIZEMEC
      my_real
     .        FSKYI2(I2SIZEMEC,*),FTHESKYI2(*),CONDNSKYI2(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR, INDEXI, NISKYF, N, IDEB,
     .        SIZ, J, L, CC, NBIRECV, NBISEND, II, MSGOFF,
     .        IAD_RECV(NSPMD+1), INDEXR(NSPMD),INDEXS(NSPMD),
     .        REQ_R(NSPMD),REQ_S(NSPMD),
     .        STATUS(MPI_STATUS_SIZE)
      my_real
     .        RBUF(LENR*I2SIZE),
     .        SBUF(LENS*I2SIZE)
      DATA MSGOFF/171/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
      NBIRECV = 0
      NBISEND = 0
      L = 1
      IAD_RECV(1) = 1
      DO I = 1, NSPMD
        IF(FR_NBCCI2(2,I)>0) THEN
          SIZ = (I2SIZE)*FR_NBCCI2(2,I)
          MSGTYP = MSGOFF
          NBIRECV = NBIRECV + 1
          INDEXR(NBIRECV) = I
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L
        IF(FR_NBCCI2(1,I)>0) THEN
          NBISEND = NBISEND + 1
          INDEXS(NBISEND) = I
        ENDIF
      ENDDO
C
C preparation envoi
C
      L = 1
      DO II=1, NBISEND
        I = INDEXS(II)
        IF (INTTH2 == 1) THEN
         IF(IDT_THERM == 1) THEN             
          IF(IRODDL/=0) THEN
           DO J=IAD_I2M(I),IAD_I2M(I+1)-1
            NOD = FR_I2M(J)
            DO CC = ADDCNI2(NOD),ADDCNI2(NOD+1)-1
              IF(PROCNI2(CC)==LOC_PROC) THEN
                SBUF(L)   = FSKYI2(1,CC)
                SBUF(L+1) = FSKYI2(2,CC)
                SBUF(L+2) = FSKYI2(3,CC)
                SBUF(L+3) = FSKYI2(4,CC)
                SBUF(L+4) = FSKYI2(5,CC)
                SBUF(L+5) = FSKYI2(6,CC)
                SBUF(L+6) = FSKYI2(7,CC)
                SBUF(L+7) = FSKYI2(8,CC)
                SBUF(L+8) = FSKYI2(9,CC)
                SBUF(L+9) = FSKYI2(10,CC)
                SBUF(L+10)= FTHESKYI2(CC)
                SBUF(L+11)= CONDNSKYI2(CC)
                L = L + I2SIZE
              ENDIF
            ENDDO
           END DO
          ELSE
           DO J=IAD_I2M(I),IAD_I2M(I+1)-1
            NOD = FR_I2M(J)
            DO CC = ADDCNI2(NOD),ADDCNI2(NOD+1)-1
              IF(PROCNI2(CC)==LOC_PROC) THEN
                SBUF(L)   = FSKYI2(1,CC)
                SBUF(L+1) = FSKYI2(2,CC)
                SBUF(L+2) = FSKYI2(3,CC)
                SBUF(L+3) = FSKYI2(4,CC)
                SBUF(L+4) = FSKYI2(5,CC)
                SBUF(L+5) = FTHESKYI2(CC)
                SBUF(L+6) = CONDNSKYI2(CC)
                L = L + I2SIZE
              ENDIF
            ENDDO
           END DO
          ENDIF
         ELSE
          IF(IRODDL/=0) THEN
           DO J=IAD_I2M(I),IAD_I2M(I+1)-1
            NOD = FR_I2M(J)
            DO CC = ADDCNI2(NOD),ADDCNI2(NOD+1)-1
              IF(PROCNI2(CC)==LOC_PROC) THEN
                SBUF(L)   = FSKYI2(1,CC)
                SBUF(L+1) = FSKYI2(2,CC)
                SBUF(L+2) = FSKYI2(3,CC)
                SBUF(L+3) = FSKYI2(4,CC)
                SBUF(L+4) = FSKYI2(5,CC)
                SBUF(L+5) = FSKYI2(6,CC)
                SBUF(L+6) = FSKYI2(7,CC)
                SBUF(L+7) = FSKYI2(8,CC)
                SBUF(L+8) = FSKYI2(9,CC)
                SBUF(L+9) = FSKYI2(10,CC)
                SBUF(L+10)= FTHESKYI2(CC)
                L = L + I2SIZE
              ENDIF
            ENDDO
           END DO
          ELSE
           DO J=IAD_I2M(I),IAD_I2M(I+1)-1
            NOD = FR_I2M(J)
            DO CC = ADDCNI2(NOD),ADDCNI2(NOD+1)-1
              IF(PROCNI2(CC)==LOC_PROC) THEN
                SBUF(L)   = FSKYI2(1,CC)
                SBUF(L+1) = FSKYI2(2,CC)
                SBUF(L+2) = FSKYI2(3,CC)
                SBUF(L+3) = FSKYI2(4,CC)
                SBUF(L+4) = FSKYI2(5,CC)
                SBUF(L+5) = FTHESKYI2(CC)
                L = L + I2SIZE
              ENDIF
            ENDDO
           END DO
          ENDIF
         ENDIF
       ELSE
         IF(IRODDL/=0) THEN
          DO J=IAD_I2M(I),IAD_I2M(I+1)-1
            NOD = FR_I2M(J)
            DO CC = ADDCNI2(NOD),ADDCNI2(NOD+1)-1
              IF(PROCNI2(CC)==LOC_PROC) THEN
                SBUF(L)   = FSKYI2(1,CC)
                SBUF(L+1) = FSKYI2(2,CC)
                SBUF(L+2) = FSKYI2(3,CC)
                SBUF(L+3) = FSKYI2(4,CC)
                SBUF(L+4) = FSKYI2(5,CC)
                SBUF(L+5) = FSKYI2(6,CC)
                SBUF(L+6) = FSKYI2(7,CC)
                SBUF(L+7) = FSKYI2(8,CC)
                SBUF(L+8) = FSKYI2(9,CC)
                SBUF(L+9) = FSKYI2(10,CC)
                L = L + I2SIZE
              ENDIF
            ENDDO
          END DO
         ELSE
          DO J=IAD_I2M(I),IAD_I2M(I+1)-1
            NOD = FR_I2M(J)
            DO CC = ADDCNI2(NOD),ADDCNI2(NOD+1)-1
              IF(PROCNI2(CC)==LOC_PROC) THEN
                SBUF(L)   = FSKYI2(1,CC)
                SBUF(L+1) = FSKYI2(2,CC)
                SBUF(L+2) = FSKYI2(3,CC)
                SBUF(L+3) = FSKYI2(4,CC)
                SBUF(L+4) = FSKYI2(5,CC)
                L = L + I2SIZE
              ENDIF
            ENDDO
          END DO
         ENDIF
       ENDIF
C
      ENDDO
C
C   echange messages
C
      L = 1
      DO II=1,NBISEND
        I = INDEXS(II)
        SIZ = I2SIZE*FR_NBCCI2(1,I)                         
        MSGTYP = MSGOFF 
        CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(II),IERROR)
        L = L + SIZ
      ENDDO
C
C decompactage
C
      DO II=1,NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEXI,STATUS,IERROR)
        I = INDEXR(INDEXI)
        L = IAD_RECV(I)
        IF (INTTH2 == 1) THEN
         IF(IDT_THERM == 1) THEN                    
          IF(IRODDL/=0) THEN
           DO J=IAD_I2M(I),IAD_I2M(I+1)-1
            NOD = FR_I2M(J)
#include "scalar.inc"
            DO CC = ADDCNI2(NOD), ADDCNI2(NOD+1)-1
              IF(PROCNI2(CC)==I) THEN
                FSKYI2(1,CC) = RBUF(L)
                FSKYI2(2,CC) = RBUF(L+1)
                FSKYI2(3,CC) = RBUF(L+2)
                FSKYI2(4,CC) = RBUF(L+3)
                FSKYI2(5,CC) = RBUF(L+4)
                FSKYI2(6,CC) = RBUF(L+5)
                FSKYI2(7,CC) = RBUF(L+6)
                FSKYI2(8,CC) = RBUF(L+7)
                FSKYI2(9,CC) = RBUF(L+8)
                FSKYI2(10,CC)= RBUF(L+9)
                FTHESKYI2(CC)= RBUF(L+10)
                CONDNSKYI2(CC)= RBUF(L+11)
                L = L + I2SIZE
              ENDIF
            ENDDO
           END DO
          ELSE
           DO J=IAD_I2M(I),IAD_I2M(I+1)-1
            NOD = FR_I2M(J)
#include "scalar.inc"
            DO CC = ADDCNI2(NOD), ADDCNI2(NOD+1)-1
              IF(PROCNI2(CC)==I) THEN
                FSKYI2(1,CC) = RBUF(L)
                FSKYI2(2,CC) = RBUF(L+1)
                FSKYI2(3,CC) = RBUF(L+2)
                FSKYI2(4,CC) = RBUF(L+3)
                FSKYI2(5,CC) = RBUF(L+4)
                FTHESKYI2(CC)= RBUF(L+5)
                CONDNSKYI2(CC)= RBUF(L+6)
                L = L + I2SIZE
              END IF
            END DO
           END DO
          END IF
         ELSE
          IF(IRODDL/=0) THEN
           DO J=IAD_I2M(I),IAD_I2M(I+1)-1
            NOD = FR_I2M(J)
#include "scalar.inc"
            DO CC = ADDCNI2(NOD), ADDCNI2(NOD+1)-1
              IF(PROCNI2(CC)==I) THEN
                FSKYI2(1,CC) = RBUF(L)
                FSKYI2(2,CC) = RBUF(L+1)
                FSKYI2(3,CC) = RBUF(L+2)
                FSKYI2(4,CC) = RBUF(L+3)
                FSKYI2(5,CC) = RBUF(L+4)
                FSKYI2(6,CC) = RBUF(L+5)
                FSKYI2(7,CC) = RBUF(L+6)
                FSKYI2(8,CC) = RBUF(L+7)
                FSKYI2(9,CC) = RBUF(L+8)
                FSKYI2(10,CC)= RBUF(L+9)
                FTHESKYI2(CC)= RBUF(L+10)
                L = L + I2SIZE
              ENDIF
            ENDDO
           END DO
          ELSE
           DO J=IAD_I2M(I),IAD_I2M(I+1)-1
            NOD = FR_I2M(J)
#include "scalar.inc"
            DO CC = ADDCNI2(NOD), ADDCNI2(NOD+1)-1
              IF(PROCNI2(CC)==I) THEN
                FSKYI2(1,CC) = RBUF(L)
                FSKYI2(2,CC) = RBUF(L+1)
                FSKYI2(3,CC) = RBUF(L+2)
                FSKYI2(4,CC) = RBUF(L+3)
                FSKYI2(5,CC) = RBUF(L+4)
                FTHESKYI2(CC)= RBUF(L+5)
                L = L + I2SIZE
              END IF
            END DO
           END DO
          END IF
         ENDIF
       ELSE
         IF(IRODDL/=0) THEN
          DO J=IAD_I2M(I),IAD_I2M(I+1)-1
            NOD = FR_I2M(J)
#include "scalar.inc"
            DO CC = ADDCNI2(NOD), ADDCNI2(NOD+1)-1
              IF(PROCNI2(CC)==I) THEN
                FSKYI2(1,CC) = RBUF(L)
                FSKYI2(2,CC) = RBUF(L+1)
                FSKYI2(3,CC) = RBUF(L+2)
                FSKYI2(4,CC) = RBUF(L+3)
                FSKYI2(5,CC) = RBUF(L+4)
                FSKYI2(6,CC) = RBUF(L+5)
                FSKYI2(7,CC) = RBUF(L+6)
                FSKYI2(8,CC) = RBUF(L+7)
                FSKYI2(9,CC) = RBUF(L+8)
                FSKYI2(10,CC)= RBUF(L+9)
                L = L + I2SIZE
              ENDIF
            ENDDO
           END DO
         ELSE
           DO J=IAD_I2M(I),IAD_I2M(I+1)-1
            NOD = FR_I2M(J)
#include "scalar.inc"
            DO CC = ADDCNI2(NOD), ADDCNI2(NOD+1)-1
              IF(PROCNI2(CC)==I) THEN
                FSKYI2(1,CC) = RBUF(L)
                FSKYI2(2,CC) = RBUF(L+1)
                FSKYI2(3,CC) = RBUF(L+2)
                FSKYI2(4,CC) = RBUF(L+3)
                FSKYI2(5,CC) = RBUF(L+4)
                L = L + I2SIZE
              END IF
            END DO
           END DO
         END IF
       ENDIF  
      END DO
C
Cel wait terminaison isend
C
      DO L=1,NBISEND
        CALL MPI_WAITANY(NBISEND,REQ_S,INDEXI,STATUS,IERROR)
      ENDDO
C
#endif
      RETURN
      END
