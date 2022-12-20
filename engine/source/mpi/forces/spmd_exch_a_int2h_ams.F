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
Chd|  SPMD_EXCH_A_INT2H_AMS         source/mpi/forces/spmd_exch_a_int2h_ams.F
Chd|-- called by -----------
Chd|        INTTI1                        source/interfaces/interf/intti1.F
Chd|-- calls ---------------
Chd|        H3D_MOD                       share/modules/h3d_mod.F       
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_A_INT2H_AMS(
     1   A    ,AR     ,MS    ,IN     ,STIFN,
     2   STIFR,FR_I2M,IAD_I2M,LCOMI2M,ISIZE,
     3   NB_FRI2M,FR_LOCI2M,TAGNOD,INTTH2,FTHE,
     4   CONDN)
C realise le cumul des acc et masses aux noeuds main d'int2
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
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
#include      "param_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LCOMI2M, ISIZE, NB_FRI2M,INTTH2,
     .        FR_I2M(*), IAD_I2M(*),FR_LOCI2M(*),TAGNOD(*)
      my_real
     .        A(3,*), AR(3,*), MS(*), IN(*),
     .        STIFN(*), STIFR(*),FTHE(*),CONDN(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,LOC_PROC,NOD,I,J,L,IDEB,IAD,LEN,P,
     .        NBINDEX,INDEX,MSGOFF,SIZ,IERROR,
     .        STATUS(MPI_STATUS_SIZE),
     .        REQ_S(NSPMD),REQ_R(NSPMD),INDEXI(NSPMD)
      DATA MSGOFF/118/
      my_real
     .        SBUF(LCOMI2M*ISIZE), RBUF(LCOMI2M*ISIZE)
      my_real,
     .   DIMENSION(:,:),ALLOCATABLE :: SAV_ACC
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      ALLOCATE (SAV_ACC(ISIZE,NB_FRI2M))
      LOC_PROC = ISPMD + 1
C
      IDEB = 1
      L = 0
      DO I = 1, NSPMD
        LEN = IAD_I2M(I+1)-IAD_I2M(I)
        IF(LEN>0) THEN
          SIZ = LEN*ISIZE
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
        IF(INTTH2 == 1) THEN 
         IF (IDT_THERM== 1) THEN
          IF (IRODDL==0) THEN
#include      "vectorize.inc"
           DO J = 1, LEN
            NOD = FR_I2M(IAD+J)
            SBUF(IDEB)   = A(1,NOD)*TAGNOD(NOD)
            SBUF(IDEB+1) = A(2,NOD)*TAGNOD(NOD)
            SBUF(IDEB+2) = A(3,NOD)*TAGNOD(NOD)
            SBUF(IDEB+3) = MS(NOD)*TAGNOD(NOD)
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
            SBUF(IDEB+3) = MS(NOD)*TAGNOD(NOD)
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
            SBUF(IDEB+3) = MS(NOD)*TAGNOD(NOD)
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
      ENDDO
C
      IDEB = 1
      DO L=1,NBINDEX
        I = INDEXI(L)
        LEN = IAD_I2M(I+1)-IAD_I2M(I)
        SIZ = LEN*ISIZE
        MSGTYP = MSGOFF
        CALL MPI_ISEND(
     S    SBUF(IDEB),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G    MPI_COMM_WORLD,REQ_S(L),IERROR)
        IDEB = IDEB + SIZ
      ENDDO
C
      IF(INTTH2 == 1) THEN 
       IF (IDT_THERM== 1) THEN
        IF(IRODDL==0)THEN
         DO I=1,NB_FRI2M
          NOD = FR_LOCI2M(I)
          SAV_ACC(1,I)=A(1,NOD)*TAGNOD(NOD)
          SAV_ACC(2,I)=A(2,NOD)*TAGNOD(NOD)
          SAV_ACC(3,I)=A(3,NOD)*TAGNOD(NOD)
          SAV_ACC(4,I)=MS(NOD)*TAGNOD(NOD)
          SAV_ACC(5,I)=STIFN(NOD)*TAGNOD(NOD)
          SAV_ACC(6,I)=FTHE(NOD)*TAGNOD(NOD)
          SAV_ACC(7,I)=CONDN(NOD)*TAGNOD(NOD)
C
          IF(TAGNOD(NOD)==1)THEN
            A(1,NOD) = ZERO
            A(2,NOD) = ZERO
            A(3,NOD) = ZERO
            MS(NOD) = ZERO
            STIFN(NOD) = ZERO
            FTHE(NOD) = ZERO
            CONDN(NOD) = ZERO
          END IF
         ENDDO

        ELSE
         DO I=1,NB_FRI2M
          NOD = FR_LOCI2M(I)
          SAV_ACC( 1,I) =  A(1,NOD)*TAGNOD(NOD)
          SAV_ACC( 2,I) =  A(2,NOD)*TAGNOD(NOD)
          SAV_ACC( 3,I) =  A(3,NOD)*TAGNOD(NOD)
          SAV_ACC( 4,I) = AR(1,NOD)*TAGNOD(NOD)
          SAV_ACC( 5,I) = AR(2,NOD)*TAGNOD(NOD)
          SAV_ACC( 6,I) = AR(3,NOD)*TAGNOD(NOD)
          SAV_ACC( 7,I) = MS(NOD)*TAGNOD(NOD)
          SAV_ACC( 8,I) = IN(NOD)*TAGNOD(NOD)
          SAV_ACC( 9,I) = STIFN(NOD)*TAGNOD(NOD)
          SAV_ACC(10,I) = STIFR(NOD)*TAGNOD(NOD)
          SAV_ACC(11,I) = FTHE(NOD)*TAGNOD(NOD)
          SAV_ACC(12,I) = CONDN(NOD)*TAGNOD(NOD)
          IF(TAGNOD(NOD)==1)THEN
            A(1,NOD) = ZERO
            A(2,NOD) = ZERO
            A(3,NOD) = ZERO
            AR(1,NOD) = ZERO
            AR(2,NOD) = ZERO
            AR(3,NOD) = ZERO
            MS(NOD)   = ZERO
            IN(NOD)   = ZERO
            STIFN(NOD) = ZERO
            STIFR(NOD) = ZERO
            FTHE(NOD) = ZERO
            CONDN(NOD) = ZERO
          END IF
         ENDDO
C
        ENDIF
       ELSE
        IF(IRODDL==0)THEN
         DO I=1,NB_FRI2M
          NOD = FR_LOCI2M(I)
          SAV_ACC(1,I)=A(1,NOD)*TAGNOD(NOD)
          SAV_ACC(2,I)=A(2,NOD)*TAGNOD(NOD)
          SAV_ACC(3,I)=A(3,NOD)*TAGNOD(NOD)
          SAV_ACC(4,I)=MS(NOD)*TAGNOD(NOD)
          SAV_ACC(5,I)=STIFN(NOD)*TAGNOD(NOD)
          SAV_ACC(6,I)=FTHE(NOD)*TAGNOD(NOD)
C
          IF(TAGNOD(NOD)==1)THEN
            A(1,NOD) = ZERO
            A(2,NOD) = ZERO
            A(3,NOD) = ZERO
            MS(NOD) = ZERO
            STIFN(NOD) = ZERO
            FTHE(NOD) = ZERO
          END IF
         ENDDO

        ELSE
         DO I=1,NB_FRI2M
          NOD = FR_LOCI2M(I)
          SAV_ACC( 1,I) =  A(1,NOD)*TAGNOD(NOD)
          SAV_ACC( 2,I) =  A(2,NOD)*TAGNOD(NOD)
          SAV_ACC( 3,I) =  A(3,NOD)*TAGNOD(NOD)
          SAV_ACC( 4,I) = AR(1,NOD)*TAGNOD(NOD)
          SAV_ACC( 5,I) = AR(2,NOD)*TAGNOD(NOD)
          SAV_ACC( 6,I) = AR(3,NOD)*TAGNOD(NOD)
          SAV_ACC( 7,I) = MS(NOD)*TAGNOD(NOD)
          SAV_ACC( 8,I) = IN(NOD)*TAGNOD(NOD)
          SAV_ACC( 9,I) = STIFN(NOD)*TAGNOD(NOD)
          SAV_ACC(10,I) = STIFR(NOD)*TAGNOD(NOD)
          SAV_ACC(11,I) = FTHE(NOD)*TAGNOD(NOD)
          IF(TAGNOD(NOD)==1)THEN
            A(1,NOD) = ZERO
            A(2,NOD) = ZERO
            A(3,NOD) = ZERO
            AR(1,NOD) = ZERO
            AR(2,NOD) = ZERO
            AR(3,NOD) = ZERO
            MS(NOD)   = ZERO
            IN(NOD)   = ZERO
            STIFN(NOD) = ZERO
            STIFR(NOD) = ZERO
            FTHE(NOD) = ZERO
          END IF
         ENDDO
C
        ENDIF
       ENDIF
C
      ELSE
       IF(IRODDL==0)THEN
        DO I=1,NB_FRI2M
          NOD = FR_LOCI2M(I)
          SAV_ACC(1,I)=A(1,NOD)*TAGNOD(NOD)
          SAV_ACC(2,I)=A(2,NOD)*TAGNOD(NOD)
          SAV_ACC(3,I)=A(3,NOD)*TAGNOD(NOD)
          SAV_ACC(4,I)=MS(NOD)*TAGNOD(NOD)
          SAV_ACC(5,I)=STIFN(NOD)*TAGNOD(NOD)
C
          IF(TAGNOD(NOD)==1)THEN
            A(1,NOD) = ZERO
            A(2,NOD) = ZERO
            A(3,NOD) = ZERO
            MS(NOD) = ZERO
            STIFN(NOD) = ZERO
          END IF
        ENDDO

       ELSE
        DO I=1,NB_FRI2M
          NOD = FR_LOCI2M(I)
          SAV_ACC( 1,I) =  A(1,NOD)*TAGNOD(NOD)
          SAV_ACC( 2,I) =  A(2,NOD)*TAGNOD(NOD)
          SAV_ACC( 3,I) =  A(3,NOD)*TAGNOD(NOD)
          SAV_ACC( 4,I) = AR(1,NOD)*TAGNOD(NOD)
          SAV_ACC( 5,I) = AR(2,NOD)*TAGNOD(NOD)
          SAV_ACC( 6,I) = AR(3,NOD)*TAGNOD(NOD)
          SAV_ACC( 7,I) = MS(NOD)*TAGNOD(NOD)
          SAV_ACC( 8,I) = IN(NOD)*TAGNOD(NOD)
          SAV_ACC( 9,I) = STIFN(NOD)*TAGNOD(NOD)
          SAV_ACC(10,I) = STIFR(NOD)*TAGNOD(NOD)
          IF(TAGNOD(NOD)==1)THEN
            A(1,NOD) = ZERO
            A(2,NOD) = ZERO
            A(3,NOD) = ZERO
            AR(1,NOD) = ZERO
            AR(2,NOD) = ZERO
            AR(3,NOD) = ZERO
            MS(NOD)   = ZERO
            IN(NOD)   = ZERO
            STIFN(NOD) = ZERO
            STIFR(NOD) = ZERO
          END IF
        ENDDO
C
       ENDIF
C
      ENDIF
C
      L = 0
      DO P=1,NSPMD
       IF(P/=LOC_PROC)THEN
        LEN= IAD_I2M(P+1)-IAD_I2M(P)
        IF(LEN>0) THEN
          L=L+1
          IDEB = 1+(IAD_I2M(P)-1)*ISIZE
          IAD = IAD_I2M(P)-1
          CALL MPI_WAIT(REQ_R(L),STATUS,IERROR)
          IF(INTTH2 == 1) THEN 
           IF (IDT_THERM== 1) THEN
            IF(IRODDL==0)THEN
#include      "vectorize.inc"
             DO J = 1, LEN
              NOD = FR_I2M(IAD+J)
              A(1,NOD)   = A(1,NOD) + RBUF(IDEB)
              A(2,NOD)   = A(2,NOD) + RBUF(IDEB+1)
              A(3,NOD)   = A(3,NOD) + RBUF(IDEB+2)
              MS(NOD)    = MS(NOD)  + RBUF(IDEB+3)
              STIFN(NOD) = STIFN(NOD)+RBUF(IDEB+4)
              FTHE(NOD)  = FTHE(NOD)+RBUF(IDEB+5)
              CONDN(NOD) = CONDN(NOD)+RBUF(IDEB+6)
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
              CONDN(NOD) = CONDN(NOD)+RBUF(IDEB+11)
              IDEB = IDEB + ISIZE
             END DO
           ENDIF
          ELSE
            IF(IRODDL==0)THEN
#include      "vectorize.inc"
             DO J = 1, LEN
              NOD = FR_I2M(IAD+J)
              A(1,NOD)   = A(1,NOD) + RBUF(IDEB)
              A(2,NOD)   = A(2,NOD) + RBUF(IDEB+1)
              A(3,NOD)   = A(3,NOD) + RBUF(IDEB+2)
              MS(NOD)    = MS(NOD)  + RBUF(IDEB+3)
              STIFN(NOD) = STIFN(NOD)+RBUF(IDEB+4)
              FTHE(NOD)  = FTHE(NOD)+RBUF(IDEB+5)
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
           ENDIF
          ENDIF
         ELSE
           IF(IRODDL==0)THEN
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
          ENDIF
         ENDIF
        ENDIF
       ELSE
        IF(INTTH2 == 1) THEN 
         IF (IDT_THERM== 1) THEN
          IF(IRODDL==0)THEN
           DO J=1,NB_FRI2M
            NOD=FR_LOCI2M(J)
            A(1,NOD)   = A(1,NOD) + SAV_ACC(1,J)
            A(2,NOD)   = A(2,NOD) + SAV_ACC(2,J)
            A(3,NOD)   = A(3,NOD) + SAV_ACC(3,J)
            MS(NOD)    = MS(NOD)  + SAV_ACC(4,J)
            STIFN(NOD) = STIFN(NOD)+SAV_ACC(5,J)
            FTHE(NOD)  = FTHE(NOD) +SAV_ACC(6,J)
            CONDN(NOD) = CONDN(NOD) +SAV_ACC(7,J)
           ENDDO
          ELSE
            DO J=1,NB_FRI2M
              NOD=FR_LOCI2M(J)
              A(1,NOD)   = A(1,NOD) + SAV_ACC(1,J)
              A(2,NOD)   = A(2,NOD) + SAV_ACC(2,J)
              A(3,NOD)   = A(3,NOD) + SAV_ACC(3,J)
              AR(1,NOD)  = AR(1,NOD)+ SAV_ACC(4,J)
              AR(2,NOD)  = AR(2,NOD)+ SAV_ACC(5,J)
              AR(3,NOD)  = AR(3,NOD)+ SAV_ACC(6,J)
              MS(NOD)    = MS(NOD)  + SAV_ACC(7,J)
              IN(NOD)    = IN(NOD)  + SAV_ACC(8,J)
              STIFN(NOD) = STIFN(NOD)+SAV_ACC(9,J)
              STIFR(NOD) = STIFR(NOD)+SAV_ACC(10,J)
              FTHE(NOD)  = FTHE(NOD) +SAV_ACC(11,J)
              CONDN(NOD)  = CONDN(NOD)+SAV_ACC(12,J)
            ENDDO
          ENDIF
         ELSE
          IF(IRODDL==0)THEN
           DO J=1,NB_FRI2M
            NOD=FR_LOCI2M(J)
            A(1,NOD)   = A(1,NOD) + SAV_ACC(1,J)
            A(2,NOD)   = A(2,NOD) + SAV_ACC(2,J)
            A(3,NOD)   = A(3,NOD) + SAV_ACC(3,J)
            MS(NOD)    = MS(NOD)  + SAV_ACC(4,J)
            STIFN(NOD) = STIFN(NOD)+SAV_ACC(5,J)
            FTHE(NOD)  = FTHE(NOD) +SAV_ACC(6,J)
           ENDDO
          ELSE
            DO J=1,NB_FRI2M
              NOD=FR_LOCI2M(J)
              A(1,NOD)   = A(1,NOD) + SAV_ACC(1,J)
              A(2,NOD)   = A(2,NOD) + SAV_ACC(2,J)
              A(3,NOD)   = A(3,NOD) + SAV_ACC(3,J)
              AR(1,NOD)  = AR(1,NOD)+ SAV_ACC(4,J)
              AR(2,NOD)  = AR(2,NOD)+ SAV_ACC(5,J)
              AR(3,NOD)  = AR(3,NOD)+ SAV_ACC(6,J)
              MS(NOD)    = MS(NOD)  + SAV_ACC(7,J)
              IN(NOD)    = IN(NOD)  + SAV_ACC(8,J)
              STIFN(NOD) = STIFN(NOD)+SAV_ACC(9,J)
              STIFR(NOD) = STIFR(NOD)+SAV_ACC(10,J)
              FTHE(NOD)  = FTHE(NOD) +SAV_ACC(11,J)
            ENDDO
          ENDIF
         ENDIF
        ELSE
         IF(IRODDL==0)THEN
           DO J=1,NB_FRI2M
            NOD=FR_LOCI2M(J)
            A(1,NOD)   = A(1,NOD) + SAV_ACC(1,J)
            A(2,NOD)   = A(2,NOD) + SAV_ACC(2,J)
            A(3,NOD)   = A(3,NOD) + SAV_ACC(3,J)
            MS(NOD)    = MS(NOD)  + SAV_ACC(4,J)
            STIFN(NOD) = STIFN(NOD)+SAV_ACC(5,J)
           ENDDO
         ELSE
            DO J=1,NB_FRI2M
              NOD=FR_LOCI2M(J)
              A(1,NOD)   = A(1,NOD) + SAV_ACC(1,J)
              A(2,NOD)   = A(2,NOD) + SAV_ACC(2,J)
              A(3,NOD)   = A(3,NOD) + SAV_ACC(3,J)
              AR(1,NOD)  = AR(1,NOD)+ SAV_ACC(4,J)
              AR(2,NOD)  = AR(2,NOD)+ SAV_ACC(5,J)
              AR(3,NOD)  = AR(3,NOD)+ SAV_ACC(6,J)
              MS(NOD)    = MS(NOD)  + SAV_ACC(7,J)
              IN(NOD)    = IN(NOD)  + SAV_ACC(8,J)
              STIFN(NOD) = STIFN(NOD)+SAV_ACC(9,J)
              STIFR(NOD) = STIFR(NOD)+SAV_ACC(10,J)
            ENDDO
         ENDIF
        ENDIF
       ENDIF
      ENDDO
C
      DO L=1,NBINDEX
        CALL MPI_WAITANY(NBINDEX,REQ_S,INDEX,STATUS,IERROR)
      ENDDO
      DEALLOCATE(SAV_ACC)
C
#endif
      RETURN
      END
