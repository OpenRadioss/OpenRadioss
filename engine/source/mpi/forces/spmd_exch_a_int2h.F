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
Chd|  SPMD_EXCH_A_INT2H             source/mpi/forces/spmd_exch_a_int2h.F
Chd|-- called by -----------
Chd|        INTTI1                        source/interfaces/interf/intti1.F
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_A_INT2H(
     1   A    ,AR     ,MS    ,IN     ,STIFN,
     2   STIFR,FR_I2M,IAD_I2M,LCOMI2M,ISIZE,
     3   TAGNOD,INTTH2,FTHE,CONDN,FNCONT  ,
     4   FNCONTP,FTCONTP,H3D_DATA )
C-----------------------------------------------
       USE H3D_MOD
C-----------------------------------------------
C realise le cumul des acc et masses aux noeuds main d'int2,
C cas ou il existe une hierarchie d'interfaces ds le modele
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
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
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LCOMI2M, ISIZE, INTTH2,
     .        FR_I2M(*), IAD_I2M(*),TAGNOD(*)
      my_real
     .        A(3,*), AR(3,*), MS(*), IN(*),
     .        STIFN(*), STIFR(*),FTHE(*),CONDN(*)
      my_real , INTENT(INOUT) :: FNCONT(3,NUMNOD),
     .      FNCONTP(3,NUMNOD),FTCONTP(3,NUMNOD)
      TYPE(H3D_DATABASE) :: H3D_DATA
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,LOC_PROC,NOD,I,J,L,IDEB,IAD,LEN,
     .        NBINDEX,INDEX,MSGOFF,SIZ,IERROR,ISIZE2,
     .        STATUS(MPI_STATUS_SIZE),
     .        REQ_S(NSPMD),REQ_R(NSPMD),INDEXI(NSPMD)
      DATA MSGOFF/119/
      my_real,
     .  DIMENSION (:),ALLOCATABLE :: SBUF,RBUF
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
      ISIZE2=ISIZE
      IF (H3D_DATA%N_VECT_CONT2_MAX > 0.OR.H3D_DATA%N_VECT_CONT2_MIN > 0) THEN
         ISIZE2 = ISIZE2 + 3
      ENDIF
      IF (H3D_DATA%N_VECT_PCONT2_MAX > 0.OR.H3D_DATA%N_VECT_PCONT2_MIN > 0) THEN
         ISIZE2 = ISIZE2 + 6
      ENDIF 
      ALLOCATE(SBUF(LCOMI2M*ISIZE2))    
      ALLOCATE(RBUF(LCOMI2M*ISIZE2))  
C
      IDEB = 1
      L = 0
      DO I = 1, NSPMD
        LEN = IAD_I2M(I+1)-IAD_I2M(I)
        IF(LEN>0) THEN
          SIZ = LEN*ISIZE2
          L=L+1
          INDEXI(L)=I
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(IDEB),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(L),IERROR)
          IDEB = IDEB + SIZ
        ENDIF
      ENDDO
      NBINDEX = L
C
      IDEB = 1
      DO L = 1, NBINDEX
        I = INDEXI(L)
        LEN = IAD_I2M(I+1) - IAD_I2M(I)
        IAD = IAD_I2M(I)-1
        IF(INTTH2==1) THEN
         IF (IDT_THERM== 1) THEN
          IF (IRODDL==0) THEN
#include      "vectorize.inc"
           DO J = 1, LEN
            NOD = FR_I2M(IAD+J)
            SBUF(IDEB)   = A(1,NOD)*TAGNOD(NOD)
            SBUF(IDEB+1) = A(2,NOD)*TAGNOD(NOD)
            SBUF(IDEB+2) = A(3,NOD)*TAGNOD(NOD)
            SBUF(IDEB+3) = MS(NOD) *TAGNOD(NOD)
            SBUF(IDEB+4) = STIFN(NOD)*TAGNOD(NOD)
            SBUF(IDEB+5) = FTHE(NOD)*TAGNOD(NOD)
            SBUF(IDEB+6) = CONDN(NOD)*TAGNOD(NOD)
            IDEB = IDEB + ISIZE
           ENDDO
          ELSE
#include      "vectorize.inc"
           DO J = 1, LEN
            NOD = FR_I2M(IAD+J)
            SBUF(IDEB)   = A(1,NOD)*TAGNOD(NOD)
            SBUF(IDEB+1) = A(2,NOD)*TAGNOD(NOD)
            SBUF(IDEB+2) = A(3,NOD)*TAGNOD(NOD)
            SBUF(IDEB+3) = AR(1,NOD)*TAGNOD(NOD)
            SBUF(IDEB+4) = AR(2,NOD)*TAGNOD(NOD)
            SBUF(IDEB+5) = AR(3,NOD)*TAGNOD(NOD)
            SBUF(IDEB+6) = MS(NOD)*TAGNOD(NOD)
            SBUF(IDEB+7) = IN(NOD)*TAGNOD(NOD)
            SBUF(IDEB+8) = STIFN(NOD)*TAGNOD(NOD)
            SBUF(IDEB+9) = STIFR(NOD)*TAGNOD(NOD)
            SBUF(IDEB+10)= FTHE(NOD)*TAGNOD(NOD)
            SBUF(IDEB+11)= CONDN(NOD)*TAGNOD(NOD)
            IDEB = IDEB + ISIZE
           ENDDO
          ENDIF
         ELSE
          IF (IRODDL==0) THEN
#include      "vectorize.inc"
           DO J = 1, LEN
            NOD = FR_I2M(IAD+J)
            SBUF(IDEB)   = A(1,NOD)*TAGNOD(NOD)
            SBUF(IDEB+1) = A(2,NOD)*TAGNOD(NOD)
            SBUF(IDEB+2) = A(3,NOD)*TAGNOD(NOD)
            SBUF(IDEB+3) = MS(NOD) *TAGNOD(NOD)
            SBUF(IDEB+4) = STIFN(NOD)*TAGNOD(NOD)
            SBUF(IDEB+5) = FTHE(NOD)*TAGNOD(NOD)
            IDEB = IDEB + ISIZE
           ENDDO
          ELSE
#include      "vectorize.inc"
           DO J = 1, LEN
            NOD = FR_I2M(IAD+J)
            SBUF(IDEB)   = A(1,NOD)*TAGNOD(NOD)
            SBUF(IDEB+1) = A(2,NOD)*TAGNOD(NOD)
            SBUF(IDEB+2) = A(3,NOD)*TAGNOD(NOD)
            SBUF(IDEB+3) = AR(1,NOD)*TAGNOD(NOD)
            SBUF(IDEB+4) = AR(2,NOD)*TAGNOD(NOD)
            SBUF(IDEB+5) = AR(3,NOD)*TAGNOD(NOD)
            SBUF(IDEB+6) = MS(NOD)*TAGNOD(NOD)
            SBUF(IDEB+7) = IN(NOD)*TAGNOD(NOD)
            SBUF(IDEB+8) = STIFN(NOD)*TAGNOD(NOD)
            SBUF(IDEB+9) = STIFR(NOD)*TAGNOD(NOD)
            SBUF(IDEB+10)= FTHE(NOD)*TAGNOD(NOD)
            IDEB = IDEB + ISIZE
           ENDDO
          ENDIF
         ENDIF
        ELSE
         IF (IRODDL==0) THEN
#include      "vectorize.inc"
          DO J = 1, LEN
            NOD = FR_I2M(IAD+J)
            SBUF(IDEB)   = A(1,NOD)*TAGNOD(NOD)
            SBUF(IDEB+1) = A(2,NOD)*TAGNOD(NOD)
            SBUF(IDEB+2) = A(3,NOD)*TAGNOD(NOD)
            SBUF(IDEB+3) = MS(NOD) *TAGNOD(NOD)
            SBUF(IDEB+4) = STIFN(NOD)*TAGNOD(NOD)
            IDEB = IDEB + ISIZE
          ENDDO
         ELSE
#include      "vectorize.inc"
          DO J = 1, LEN
            NOD = FR_I2M(IAD+J)
            SBUF(IDEB)   = A(1,NOD)*TAGNOD(NOD)
            SBUF(IDEB+1) = A(2,NOD)*TAGNOD(NOD)
            SBUF(IDEB+2) = A(3,NOD)*TAGNOD(NOD)
            SBUF(IDEB+3) = AR(1,NOD)*TAGNOD(NOD)
            SBUF(IDEB+4) = AR(2,NOD)*TAGNOD(NOD)
            SBUF(IDEB+5) = AR(3,NOD)*TAGNOD(NOD)
            SBUF(IDEB+6) = MS(NOD)*TAGNOD(NOD)
            SBUF(IDEB+7) = IN(NOD)*TAGNOD(NOD)
            SBUF(IDEB+8) = STIFN(NOD)*TAGNOD(NOD)
            SBUF(IDEB+9) = STIFR(NOD)*TAGNOD(NOD)
            IDEB = IDEB + ISIZE
          ENDDO
         ENDIF
        ENDIF
C
       IF (H3D_DATA%N_VECT_CONT2_MAX > 0.OR.H3D_DATA%N_VECT_CONT2_MIN > 0) THEN
#include      "vectorize.inc"
          DO J = 1, LEN
            NOD = FR_I2M(IAD+J)
            SBUF(IDEB)   = FNCONT(1,NOD)
            SBUF(IDEB+1) = FNCONT(2,NOD)
            SBUF(IDEB+2) = FNCONT(3,NOD)
            IDEB = IDEB + 3
          ENDDO
       ENDIF
       IF (H3D_DATA%N_VECT_PCONT2_MAX > 0.OR.H3D_DATA%N_VECT_PCONT2_MIN > 0) THEN
#include      "vectorize.inc"
          DO J = 1, LEN
            NOD = FR_I2M(IAD+J)
            SBUF(IDEB)   = FNCONTP(1,NOD)
            SBUF(IDEB+1) = FNCONTP(2,NOD)
            SBUF(IDEB+2) = FNCONTP(3,NOD)
            SBUF(IDEB+3) = FTCONTP(1,NOD)
            SBUF(IDEB+4) = FTCONTP(2,NOD)
            SBUF(IDEB+5) = FTCONTP(3,NOD)
            IDEB = IDEB + 6
          ENDDO
       ENDIF 
C 
      ENDDO
C
      IDEB = 1
      DO L=1,NBINDEX
        I = INDEXI(L)
        LEN = IAD_I2M(I+1)-IAD_I2M(I)
        SIZ = LEN*ISIZE2
        MSGTYP = MSGOFF
        CALL MPI_ISEND(
     S    SBUF(IDEB),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G    MPI_COMM_WORLD,REQ_S(L),IERROR)
        IDEB = IDEB + SIZ
      ENDDO
C
      DO L=1,NBINDEX
        CALL MPI_WAITANY(NBINDEX,REQ_R,INDEX,STATUS,IERROR)
        I = INDEXI(INDEX)
        IDEB = 1+(IAD_I2M(I)-1)*ISIZE2
        LEN = IAD_I2M(I+1)-IAD_I2M(I)
        IAD = IAD_I2M(I)-1
        IF(INTTH2 == 1) THEN 
         IF (IDT_THERM== 1) THEN
          IF (IRODDL==0) THEN
#include      "vectorize.inc"
           DO J = 1, LEN
            NOD = FR_I2M(IAD+J)
            A(1,NOD)   = A(1,NOD) + RBUF(IDEB)
            A(2,NOD)   = A(2,NOD) + RBUF(IDEB+1)
            A(3,NOD)   = A(3,NOD) + RBUF(IDEB+2)
            MS(NOD)    = MS(NOD)  + RBUF(IDEB+3)
            STIFN(NOD) = STIFN(NOD)+RBUF(IDEB+4)
            FTHE(NOD)  = FTHE(NOD) +RBUF(IDEB+5)
            CONDN(NOD) = CONDN(NOD)+CONDN(IDEB+6)
            IDEB = IDEB + ISIZE
           ENDDO
         ELSE
#include      "vectorize.inc"
          DO J = 1, LEN
            NOD = FR_I2M(IAD+J)
            A(1,NOD)   = A(1,NOD) + RBUF(IDEB)
            A(2,NOD)   = A(2,NOD) + RBUF(IDEB+1)
            A(3,NOD)   = A(3,NOD) + RBUF(IDEB+2)
            AR(1,NOD)  = AR(1,NOD)+ RBUF(IDEB+3)
            AR(2,NOD)  = AR(2,NOD)+ RBUF(IDEB+4)
            AR(3,NOD)  = AR(3,NOD)+ RBUF(IDEB+5)
            MS(NOD)    = MS(NOD)  + RBUF(IDEB+6)
            IN(NOD)    = IN(NOD)  + RBUF(IDEB+7)
            STIFN(NOD) = STIFN(NOD)+RBUF(IDEB+8)
            STIFR(NOD) = STIFR(NOD)+RBUF(IDEB+9)
            FTHE(NOD)  = FTHE(NOD) +RBUF(IDEB+10)
            CONDN(NOD) = CONDN(NOD)+CONDN(IDEB+11)
            IDEB = IDEB + ISIZE
          END DO
         END IF
        ELSE
          IF (IRODDL==0) THEN
#include      "vectorize.inc"
           DO J = 1, LEN
            NOD = FR_I2M(IAD+J)
            A(1,NOD)   = A(1,NOD) + RBUF(IDEB)
            A(2,NOD)   = A(2,NOD) + RBUF(IDEB+1)
            A(3,NOD)   = A(3,NOD) + RBUF(IDEB+2)
            MS(NOD)    = MS(NOD)  + RBUF(IDEB+3)
            STIFN(NOD) = STIFN(NOD)+RBUF(IDEB+4)
            FTHE(NOD)  = FTHE(NOD) +RBUF(IDEB+5)
            IDEB = IDEB + ISIZE
           ENDDO
         ELSE
#include      "vectorize.inc"
          DO J = 1, LEN
            NOD = FR_I2M(IAD+J)
            A(1,NOD)   = A(1,NOD) + RBUF(IDEB)
            A(2,NOD)   = A(2,NOD) + RBUF(IDEB+1)
            A(3,NOD)   = A(3,NOD) + RBUF(IDEB+2)
            AR(1,NOD)  = AR(1,NOD)+ RBUF(IDEB+3)
            AR(2,NOD)  = AR(2,NOD)+ RBUF(IDEB+4)
            AR(3,NOD)  = AR(3,NOD)+ RBUF(IDEB+5)
            MS(NOD)    = MS(NOD)  + RBUF(IDEB+6)
            IN(NOD)    = IN(NOD)  + RBUF(IDEB+7)
            STIFN(NOD) = STIFN(NOD)+RBUF(IDEB+8)
            STIFR(NOD) = STIFR(NOD)+RBUF(IDEB+9)
            FTHE(NOD)  = FTHE(NOD) +RBUF(IDEB+10)
            IDEB = IDEB + ISIZE
          END DO
         END IF
        ENDIF
       ELSE
         IF (IRODDL==0) THEN
#include      "vectorize.inc"
          DO J = 1, LEN
            NOD = FR_I2M(IAD+J)
            A(1,NOD)   = A(1,NOD) + RBUF(IDEB)
            A(2,NOD)   = A(2,NOD) + RBUF(IDEB+1)
            A(3,NOD)   = A(3,NOD) + RBUF(IDEB+2)
            MS(NOD)    = MS(NOD)  + RBUF(IDEB+3)
            STIFN(NOD) = STIFN(NOD)+RBUF(IDEB+4)
            IDEB = IDEB + ISIZE
          ENDDO
        ELSE
#include      "vectorize.inc"
          DO J = 1, LEN
            NOD = FR_I2M(IAD+J)
            A(1,NOD)   = A(1,NOD) + RBUF(IDEB)
            A(2,NOD)   = A(2,NOD) + RBUF(IDEB+1)
            A(3,NOD)   = A(3,NOD) + RBUF(IDEB+2)
            AR(1,NOD)  = AR(1,NOD)+ RBUF(IDEB+3)
            AR(2,NOD)  = AR(2,NOD)+ RBUF(IDEB+4)
            AR(3,NOD)  = AR(3,NOD)+ RBUF(IDEB+5)
            MS(NOD)    = MS(NOD)  + RBUF(IDEB+6)
            IN(NOD)    = IN(NOD)  + RBUF(IDEB+7)
            STIFN(NOD) = STIFN(NOD)+RBUF(IDEB+8)
            STIFR(NOD) = STIFR(NOD)+RBUF(IDEB+9)
            IDEB = IDEB + ISIZE
          END DO
         END IF
       ENDIF
C
       IF (H3D_DATA%N_VECT_CONT2_MAX > 0.OR.H3D_DATA%N_VECT_CONT2_MIN > 0) THEN
#include      "vectorize.inc"
          DO J = 1, LEN
            NOD = FR_I2M(IAD+J)
            FNCONT(1,NOD)   = FNCONT(1,NOD) + RBUF(IDEB)
            FNCONT(2,NOD)   = FNCONT(2,NOD) + RBUF(IDEB+1)
            FNCONT(3,NOD)   = FNCONT(3,NOD) + RBUF(IDEB+2)
            IDEB = IDEB + 3
          ENDDO
       ENDIF
       IF (H3D_DATA%N_VECT_PCONT2_MAX > 0.OR.H3D_DATA%N_VECT_PCONT2_MIN > 0) THEN
#include      "vectorize.inc"
          DO J = 1, LEN
            NOD = FR_I2M(IAD+J)
            FNCONTP(1,NOD)   = FNCONTP(1,NOD) + RBUF(IDEB)
            FNCONTP(2,NOD)   = FNCONTP(2,NOD) + RBUF(IDEB+1)
            FNCONTP(3,NOD)   = FNCONTP(3,NOD) + RBUF(IDEB+2)
            FTCONTP(1,NOD)   = FTCONTP(1,NOD) + RBUF(IDEB+3)
            FTCONTP(2,NOD)   = FTCONTP(2,NOD) + RBUF(IDEB+4)
            FTCONTP(3,NOD)   = FTCONTP(3,NOD) + RBUF(IDEB+5)
            IDEB = IDEB + 6
          ENDDO
       ENDIF 
C
      ENDDO
C
      DO L=1,NBINDEX
        CALL MPI_WAITANY(NBINDEX,REQ_S,INDEX,STATUS,IERROR)
      ENDDO
C
      DEALLOCATE(RBUF)
      DEALLOCATE(SBUF)
C
#endif
      RETURN
      END
