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
#include "hardware.inc"
Chd|====================================================================
Chd|  SPMD_X_SECTION                source/mpi/sections/spmd_section.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_X_SECTION(NSTRF,X,MS,WEIGHT,WA)
C maj X (MS) N1, N2, N3 (+ NNODS) sur procs distants
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
#include      "com03_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSTRF(*), WEIGHT(*)
      my_real
     .        X(3,*), MS(*), WA(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGOFF,INFO,I,J,K,L,NELSEG,NNOD,NELC,NELTG,
     .        NN,P,ATID,ATAG,ALEN,IDEB,SIZ,K0,K1,K2,A_AR,IFRAM,
     .        REM_PROC,MSGTYP,REM_PROC2,MSGTYP2,N1,N2,N3,NOD,        
     .        SENDTO(PARASIZ),RECVFR(PARASIZ),LOC_PROC,NB_NOD,
     .        REQ_R(PARASIZ),BUFSIZ,IALL
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
      SAVE SENDTO,RECVFR,BUFSIZ
      DATA MSGOFF/4000/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      A_AR = 4
C
      IF(CODVERS<42) THEN
       IF (NCYCLE==1) THEN
C
        BUFSIZ = NSECT * 3 * A_AR
C
        DO I = 1,2*NSECT
          WA(I) = ZERO
        ENDDO
C 
        K1 = 1
        DO I=1,NSECT
          NELC = NSTRF(K1)
          N1 = NSTRF(K1+1)
          N2 = NSTRF(K1+2)
          N3 = NSTRF(K1+3)
          NELTG = NSTRF(K1+4+3*NELC)
          IF (WEIGHT(N1)==1.OR.WEIGHT(N2)==1.OR.WEIGHT(N3)==1)
     +     THEN
            WA(I) = ONE
          ENDIF
          IF (NELC+NELTG/=0.AND.(WEIGHT(N1)==0.OR.WEIGHT(N2)==0
     +                         .OR.WEIGHT(N3)==0.)) THEN
            WA(I+NSECT) = ONE
          ENDIF
          K1=K1+4+3*NSTRF(K1)
          K1=K1+1+2*NSTRF(K1)
        END DO
C
        DO I = 1, NSPMD
           SENDTO(I) = 0
           RECVFR(I) = 0
        ENDDO
        SIZ = 2*NSECT
        IDEB = SIZ + 1
        DO I = 1, NSPMD
          REM_PROC = MOD(LOC_PROC + I-1,NSPMD)+1
          MSGTYP = MSGOFF 
          REM_PROC2 = MOD(LOC_PROC+NSPMD-I-1,NSPMD)+1
          MSGTYP2 = MSGOFF 

          IF(REM_PROC/=LOC_PROC.OR.REM_PROC2/=LOC_PROC) THEN
            CALL MPI_SENDRECV(
     S       WA,SIZ,REAL,IT_SPMD(REM_PROC),MSGTYP,
     R       WA(IDEB),SIZ,REAL,IT_SPMD(REM_PROC2),MSGTYP2,
     G       MPI_COMM_WORLD,STATUS,IERROR)
          ENDIF
          IF (REM_PROC2/=LOC_PROC) THEN
            DO J = 1, NSECT
             SENDTO(REM_PROC2) = SENDTO(REM_PROC2) + 
     +                           NINT(WA(J)*WA(IDEB+NSECT+J-1))
             RECVFR(REM_PROC2) = RECVFR(REM_PROC2) + 
     +                           NINT(WA(NSECT+J)*WA(IDEB+J-1))
            ENDDO
          ENDIF
        ENDDO
       ENDIF
C
       DO I = 1, NSPMD
         IF(RECVFR(I)>0) THEN
           MSGTYP = MSGOFF 
           CALL MPI_IRECV(
     S        WA(1+I*BUFSIZ),BUFSIZ,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_R(I),IERROR)
         ENDIF
       ENDDO        
C
       L = 0
       K1 = 1
       DO I=1,NSECT
         N1 = NSTRF(K1+1)
         N2 = NSTRF(K1+2)
         N3 = NSTRF(K1+3)
         IF (WEIGHT(N1)==1)THEN
           WA(L+1) = N1
           WA(L+2) = X(1,N1)
           WA(L+3) = X(2,N1)
           WA(L+4) = X(3,N1)
           L = L + 4
         ENDIF
         IF (WEIGHT(N2)==1)THEN
           WA(L+1) = N2
           WA(L+2) = X(1,N2)
           WA(L+3) = X(2,N2)
           WA(L+4) = X(3,N2)
           L = L + 4
         ENDIF
         IF (WEIGHT(N3)==1)THEN
           WA(L+1) = N3
           WA(L+2) = X(1,N3)
           WA(L+3) = X(2,N3)
           WA(L+4) = X(3,N3)
           L = L + 4
         ENDIF
         K1=K1+4+3*NSTRF(K1)
         K1=K1+1+2*NSTRF(K1)
        END DO
C
        SIZ = L
        DO I=1,NSPMD
          IF(SENDTO(I)>0) THEN
           MSGTYP = MSGOFF
           CALL MPI_SEND(
     S     WA,SIZ,REAL,IT_SPMD(I),MSGTYP,
     G     MPI_COMM_WORLD,ierror)
          ENDIF
        ENDDO
C
        DO I = 1, NSPMD
         IF(RECVFR(I)>0) THEN
            IDEB = 1+BUFSIZ*I

            CALL MPI_WAIT(REQ_R(I),STATUS,ierror)
            CALL MPI_GET_COUNT(STATUS,REAL,SIZ,ierror)
            NB_NOD=SIZ/A_AR

            L = IDEB
            DO J = 1, NB_NOD
              NOD = NINT(WA(L))
              X(1,NOD)   = WA(L+1)
              X(2,NOD)   = WA(L+2)
              X(3,NOD)   = WA(L+3)
              L = L + A_AR
            ENDDO
         ENDIF
       ENDDO
      ELSE
       IF (NCYCLE==1) THEN
C
        BUFSIZ = 0
        K0=NSTRF(25)
        DO I = 1, NSECT
          N1 = NSTRF(K0+3)
          NNOD = NSTRF(K0+6)
          IFRAM = NSTRF(K0+26)
          IF (IFRAM<=10.OR.N1/=0) THEN
            BUFSIZ = BUFSIZ + 3*A_AR
          ENDIF
          IF(MOD(IFRAM,10)==1) THEN
            BUFSIZ = BUFSIZ + NNOD*A_AR
          ELSEIF( MOD(IFRAM,10)==2) THEN
            BUFSIZ = BUFSIZ + 2*NNOD*A_AR
          ENDIF
          K0=NSTRF(K0+24)
        ENDDO
C
        DO I = 1,2*NSECT
          WA(I) = ZERO
        ENDDO
C 
        K0=NSTRF(25)
        DO I=1,NSECT
          N1 = NSTRF(K0+3)
          N2 = NSTRF(K0+4)
          N3 = NSTRF(K0+5)
          NNOD = NSTRF(K0+6)
          K2 = K0+30+NSTRF(K0+14)
          NELSEG = NSTRF(K0+7)+NSTRF(K0+8)+NSTRF(K0+9)+NSTRF(K0+10)+
     +             NSTRF(K0+11)+NSTRF(K0+12)+NSTRF(K0+13)
          IFRAM = NSTRF(K0+26)
          IF (IFRAM<=10.OR.N1/=0) THEN
            IF (WEIGHT(N1)==1.OR.WEIGHT(N2)==1.OR.WEIGHT(N3)==1)
     +        THEN
              WA(I) = ONE
            ENDIF
            IF (NELSEG/=0.AND.(WEIGHT(N1)==0.OR.WEIGHT(N2)==0
     +                         .OR.WEIGHT(N3)==ZERO)) THEN
             WA(I+NSECT) = ONE
            ENDIF
          ENDIF
          IF(MOD(IFRAM,10)==1.OR.MOD(IFRAM,10)==2) THEN
           IALL = 1
           DO NN = 1, NNOD
             IF (WEIGHT(NSTRF(K2+NN-1))==1) THEN
               WA(I) = ONE
             ELSE
               IALL = ZERO
             ENDIF
           ENDDO
           IF (NELSEG/=0.AND.IALL==0) THEN
              WA(I+NSECT) = ONE
           ENDIF
          ENDIF
          K0=NSTRF(K0+24)
        END DO
C
        DO I = 1, NSPMD
           SENDTO(I) = 0
           RECVFR(I) = 0
        ENDDO
        SIZ = 2*NSECT
        IDEB = SIZ + 1
        DO I = 1, NSPMD
          REM_PROC = MOD(LOC_PROC + I-1,NSPMD)+1
          MSGTYP = MSGOFF 
          REM_PROC2 = MOD(LOC_PROC+NSPMD-I-1,NSPMD)+1
          MSGTYP2 = MSGOFF 

          IF(REM_PROC/=LOC_PROC.OR.REM_PROC2/=LOC_PROC) THEN
            CALL MPI_SENDRECV(
     S       WA,SIZ,REAL,IT_SPMD(REM_PROC),MSGTYP,
     R       WA(IDEB),SIZ,REAL,IT_SPMD(REM_PROC2),MSGTYP2,
     G       MPI_COMM_WORLD,STATUS,ierror)
          ENDIF

          IF (REM_PROC2/=LOC_PROC) THEN
            DO J = 1, NSECT
             SENDTO(REM_PROC2) = SENDTO(REM_PROC2) + 
     +                           NINT(WA(J)*WA(IDEB+NSECT+J-1))
             RECVFR(REM_PROC2) = RECVFR(REM_PROC2) + 
     +                           NINT(WA(NSECT+J)*WA(IDEB+J-1))
            ENDDO
          ENDIF
        ENDDO
       ENDIF
C
       DO I = 1, NSPMD
         IF(RECVFR(I)>0) THEN
           MSGTYP = MSGOFF
           CALL MPI_IRECV(
     S        WA(1+I*BUFSIZ),BUFSIZ,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_R(I),IERROR)
         ENDIF
       ENDDO        
C
       L = 0
       K0=NSTRF(25)
       DO I = 1, NSECT
         N1 = NSTRF(K0+3)
         N2 = NSTRF(K0+4)
         N3 = NSTRF(K0+5)
         NNOD = NSTRF(K0+6)
         K2 = K0+30+NSTRF(K0+14)
         NELSEG = NSTRF(K0+7)+NSTRF(K0+8)+NSTRF(K0+9)+NSTRF(K0+10)+
     +            NSTRF(K0+11)+NSTRF(K0+12)+NSTRF(K0+13)
         IFRAM = NSTRF(K0+26)
C
         IF (IFRAM<=10.OR.N1/=0) THEN
           IF (WEIGHT(N1)==1) THEN
             WA(L+1) = N1
             WA(L+2) = X(1,N1)
             WA(L+3) = X(2,N1)
             WA(L+4) = X(3,N1)
             L = L + 4
           ENDIF
           IF (WEIGHT(N2)==1) THEN
             WA(L+1) = N2
             WA(L+2) = X(1,N2)
             WA(L+3) = X(2,N2)
             WA(L+4) = X(3,N2)
             L = L + 4
           ENDIF
           IF (WEIGHT(N3)==1) THEN
             WA(L+1) = N3
             WA(L+2) = X(1,N3)
             WA(L+3) = X(2,N3)
             WA(L+4) = X(3,N3)
             L = L + 4
           ENDIF
         ENDIF
C
         IF(MOD(IFRAM,10)==1) THEN
           DO NN = 1, NNOD
             N3 = NSTRF(K2+NN-1)
             IF (WEIGHT(N3)==1) THEN
               WA(L+1) = N3
               WA(L+2) = X(1,N3)
               WA(L+3) = X(2,N3)
               WA(L+4) = X(3,N3)
               L = L + 4
             ENDIF
           ENDDO
C
         ELSEIF( MOD(IFRAM,10)==2) THEN
           DO NN = 1, NNOD
             N3 = NSTRF(K2+NN-1)
             IF (WEIGHT(N3)==1) THEN
               WA(L+1) = N3
               WA(L+2) = X(1,N3)
               WA(L+3) = X(2,N3)
               WA(L+4) = X(3,N3)
               L = L + 4
C
               WA(L+1) = -N3
               WA(L+2) = MS(N3)
               WA(L+3) = MS(N3)
               WA(L+4) = MS(N3)
               L = L + 4
             ENDIF
           ENDDO
         ENDIF
         K0=NSTRF(K0+24)
       ENDDO
C
       SIZ = L
       DO I=1,NSPMD
         IF(SENDTO(I)>0) THEN
           MSGTYP = MSGOFF 

           CALL MPI_SEND(
     S     WA,SIZ,REAL,IT_SPMD(I),MSGTYP,
     G     MPI_COMM_WORLD,ierror)

         ENDIF
       ENDDO
C
       DO I = 1, NSPMD
         IF(RECVFR(I)>0) THen
           IDEB = 1+BUFSIZ*I

           CALL MPI_WAIT(REQ_R(I),STATUS,ierror)
           CALL MPI_GET_COUNT(STATUS,REAL,SIZ,ierror)
           NB_NOD=SIZ/A_AR

           L = IDEB
           DO J = 1, NB_NOD
             NOD = NINT(WA(L))
             IF (NOD>0) THEN
               X(1,NOD)   = WA(L+1)
               X(2,NOD)   = WA(L+2)
               X(3,NOD)   = WA(L+3)
             ELSE
               MS(-NOD)   = WA(L+1)
             ENDIF
             L = L + A_AR
           ENDDO
         ENDIF
       ENDDO
C
      ENDIF
C
#endif
      RETURN
      END
C nouvelle routine maj section en SPMD
Chd|====================================================================
Chd|  SPMD_EXCH_SEC                 source/mpi/sections/spmd_section.F
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_SEC(NSTRF ,X      ,MS    ,WEIGHT,XSEC  ,
     2                         FR_SEC,IAD_SEC,LSEND1,LRECV1,LSEND2,
     3                         LRECV2, WEIGHT_MD)
C maj X (MS) N1, N2, N3 (+ NNODS) sur procs distants
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
#include      "com03_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSTRF(*), WEIGHT(*), FR_SEC(NSPMD+1,*), IAD_SEC(4,*),
     .        LSEND1, LSEND2, LRECV1, LRECV2,WEIGHT_MD(*)
      my_real
     .        X(3,*), MS(*), XSEC(4,3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER LOC_PROC,A_AR,N,L,I,J,II,K,M,JJ, LEN,A_AR2,
     .        MSGTYP,MSGOFF,MSGOFF2,SIZ,IDEBR,IDEBS,ICC,IFRAM,
     .        IERROR, NBIRECV, NBISEND, INDEX, NBRBY, NBNOD,
     .        PMAIN, IDEB, LENS, LENR, K0, K2, N1, N2, N3, NNOD,
     .        NELSEG,NN,NB,
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD), REQ_S(NSPMD),
     .        IRINDEX(NSPMD), ISINDEX(NSPMD),
     .        STATUS(MPI_STATUS_SIZE),IAD_STMP(NSPMD)
      DATA MSGOFF/4001/
      DATA MSGOFF2/4002/
      PARAMETER(A_AR = 5)
      PARAMETER(A_AR2 = 13)
      my_real
     .        MAS, XXC, YYC ,ZZC, DSEC(NSECT),
     .        SBUF(A_AR*LSEND1),SBUF2(A_AR2*LSEND2),
     .        RBUF(A_AR*LRECV1),RBUF2(A_AR2*LRECV2)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
      NBIRECV = 0
      NBISEND = 0
      IDEBR = 1
      IDEBS = 1
      DO I = 1, NSPMD
        IAD_RECV(I) = IDEBR
        IF(IAD_SEC(2,I)>0) THEN
          MSGTYP = MSGOFF 
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          SIZ = IAD_SEC(2,I)*A_AR
          CALL MPI_IRECV(
     S       RBUF(IDEBR),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G       MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
          IDEBR = IDEBR + SIZ
        ENDIF
        IAD_SEND(I) = IDEBS
        IF(IAD_SEC(1,I)>0) THEN
          NBISEND = NBISEND + 1
          ISINDEX(NBISEND) = I
          SIZ = IAD_SEC(1,I)*A_AR
          IDEBS = IDEBS + SIZ
          IAD_STMP(I)=IAD_SEND(I)
        ENDIF
      ENDDO        
      IAD_RECV(NSPMD+1) = IDEBR
C
      IDEB = 0
C 
      K0=NSTRF(25)
      DO I=1,NSECT
        PMAIN = FR_SEC(NSPMD+1,I)
        N1 = NSTRF(K0+3)
        N2 = NSTRF(K0+4)
        N3 = NSTRF(K0+5)
        NNOD = NSTRF(K0+6)
        K2 = K0+30+NSTRF(K0+14)
        NELSEG = NSTRF(K0+7)+NSTRF(K0+8)+NSTRF(K0+9)+NSTRF(K0+10)+
     +           NSTRF(K0+11)+NSTRF(K0+12)+NSTRF(K0+13)
        IFRAM = NSTRF(K0+26)
        IF(PMAIN>0.AND.LOC_PROC/=PMAIN) THEN
          L = IAD_STMP(PMAIN)
          IF (IFRAM<=10.OR.N1/=0) THEN
            IF(N1>0) THEN
              IF(WEIGHT(N1)==1) THEN
                SBUF(L) = 1
                SBUF(L+1) = X(1,N1)
                SBUF(L+2) = X(2,N1)
                SBUF(L+3) = X(3,N1)
                SBUF(L+4) = ZERO
                L = L + A_AR
              END IF
            END IF
            IF(N2>0) THEN
              IF(WEIGHT(N2)==1) THEN
                SBUF(L) = 2
                SBUF(L+1) = X(1,N2)
                SBUF(L+2) = X(2,N2)
                SBUF(L+3) = X(3,N2)
                SBUF(L+4) = ZERO
                L = L + A_AR
              END IF
            END IF
            IF(N3>0) THEN
              IF(WEIGHT(N3)==1) THEN
                SBUF(L) = 3
                SBUF(L+1) = X(1,N3)
                SBUF(L+2) = X(2,N3)
                SBUF(L+3) = X(3,N3)
                SBUF(L+4) = ZERO
                L = L + A_AR
              END IF
            END IF
          END IF
          IF(MOD(IFRAM,10)==1) THEN            
             XXC = ZERO
             YYC = ZERO
             ZZC = ZERO
             ICC = 0
             DO NN = 1, NNOD
               N = NSTRF(K2+NN-1)
               IF (WEIGHT_MD(N)==1) THEN
                 XXC=XXC+X(1,N)
                 YYC=YYC+X(2,N)
                 ZZC=ZZC+X(3,N)
                 ICC = ICC + 1
               END IF
             END DO
             IF(ICC>0) THEN
                SBUF(L) = 4
                SBUF(L+1) = XXC
                SBUF(L+2) = YYC
                SBUF(L+3) = ZZC
                SBUF(L+4) = ICC
                L = L + A_AR
             END IF
          ELSEIF(MOD(IFRAM,10)==2) THEN
             XXC = ZERO
             YYC = ZERO
             ZZC = ZERO
             MAS = ZERO
             ICC = 0
             DO NN = 1, NNOD
               N = NSTRF(K2+NN-1)
               IF (WEIGHT_MD(N)==1) THEN
                 XXC=XXC+X(1,N)*MS(N)
                 YYC=YYC+X(2,N)*MS(N)
                 ZZC=ZZC+X(3,N)*MS(N)
                 MAS=MAS+MS(N)
                 ICC = ICC + 1
               END IF
             END DO
             IF(ICC>0) THEN
                SBUF(L) = 5
                SBUF(L+1) = XXC
                SBUF(L+2) = YYC
                SBUF(L+3) = ZZC
                SBUF(L+4) = MAS
                L = L + A_AR
             END IF
          END IF
          IAD_STMP(PMAIN)=L
        ELSE
C pmain => stockage direct
          IF (IFRAM<=10.OR.N1/=0) THEN
            IF(N1>0) THEN
              IF(WEIGHT(N1)==1) THEN
                XSEC(1,1,I) = X(1,N1)
                XSEC(1,2,I) = X(2,N1)
                XSEC(1,3,I) = X(3,N1)
              END IF
            END IF
            IF(N2>0) THEN
              IF(WEIGHT(N2)==1) THEN
                XSEC(2,1,I) = X(1,N2)
                XSEC(2,2,I) = X(2,N2)
                XSEC(2,3,I) = X(3,N2)
              END IF
            END IF
            IF(N3>0) THEN
              IF(WEIGHT(N3)==1) THEN
                XSEC(3,1,I) = X(1,N3)
                XSEC(3,2,I) = X(2,N3)
                XSEC(3,3,I) = X(3,N3)
              END IF
            END IF
          END IF
          IF(MOD(IFRAM,10)==1) THEN            
             XXC = ZERO
             YYC = ZERO
             ZZC = ZERO
             ICC = 0
             DO NN = 1, NNOD
               N = NSTRF(K2+NN-1)
               IF (WEIGHT_MD(N)==1) THEN
                 XXC=XXC+X(1,N)
                 YYC=YYC+X(2,N)
                 ZZC=ZZC+X(3,N)
                 ICC = ICC + 1
               END IF
             END DO
C               SBUF(L) = 4
                XSEC(4,1,I) = XXC
                XSEC(4,2,I) = YYC
                XSEC(4,3,I) = ZZC
                DSEC(I) = ICC
          ELSEIF(MOD(IFRAM,10)==2) THEN
             XXC = ZERO
             YYC = ZERO
             ZZC = ZERO
             MAS = ZERO
             ICC = 0
             DO NN = 1, NNOD
               N = NSTRF(K2+NN-1)
               IF (WEIGHT_MD(N)==1) THEN
                 XXC=XXC+X(1,N)*MS(N)
                 YYC=YYC+X(2,N)*MS(N)
                 ZZC=ZZC+X(3,N)*MS(N)
                 MAS=MAS+MS(N)
                 ICC = ICC + 1
               END IF
             END DO
C               SBUF(L) = 5
                XSEC(4,1,I) = XXC
                XSEC(4,2,I) = YYC
                XSEC(4,3,I) = ZZC
                DSEC(I) = MAS
          END IF
        END IF
        K0=NSTRF(K0+24)
      END DO
C
      DO L = 1, NBISEND
        I = ISINDEX(L)
        SIZ = IAD_STMP(I)-IAD_SEND(I)
        IDEBS = IAD_SEND(I)
        MSGTYP = MSGOFF 
        CALL MPI_ISEND(
     S    SBUF(IDEBS),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G    MPI_COMM_WORLD,REQ_S(I),IERROR)
      ENDDO
C
      DO II = 1, NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,IERROR)
        I = IRINDEX(INDEX)
        IDEB = IAD_RECV(I)
        DO N = 1, NSECT
          PMAIN = FR_SEC(NSPMD+1,N)
          IF(LOC_PROC==PMAIN) THEN
            NB = FR_SEC(I,N)
            IF(NB>0) THEN
              DO K = 1, NB
                NN = NINT(RBUF(IDEB+(K-1)*A_AR ))
                IF(NN==1) THEN
                  XSEC(1,1,N) = RBUF(IDEB+(K-1)*A_AR+1)
                  XSEC(1,2,N) = RBUF(IDEB+(K-1)*A_AR+2)
                  XSEC(1,3,N) = RBUF(IDEB+(K-1)*A_AR+3)
                ELSEIF(NN==2) THEN
                  XSEC(2,1,N) = RBUF(IDEB+(K-1)*A_AR+1)
                  XSEC(2,2,N) = RBUF(IDEB+(K-1)*A_AR+2)
                  XSEC(2,3,N) = RBUF(IDEB+(K-1)*A_AR+3)
                ELSEIF(NN==3) THEN
                  XSEC(3,1,N) = RBUF(IDEB+(K-1)*A_AR+1)
                  XSEC(3,2,N) = RBUF(IDEB+(K-1)*A_AR+2)
                  XSEC(3,3,N) = RBUF(IDEB+(K-1)*A_AR+3)
                ELSEIF(NN==4.OR.NN==5) THEN
                  XSEC(4,1,N) = XSEC(4,1,N)+RBUF(IDEB+(K-1)*A_AR+1)
                  XSEC(4,2,N) = XSEC(4,2,N)+RBUF(IDEB+(K-1)*A_AR+2)
                  XSEC(4,3,N) = XSEC(4,3,N)+RBUF(IDEB+(K-1)*A_AR+3)
                  DSEC(N) = DSEC(N) + RBUF(IDEB+(K-1)*A_AR+4)
                END IF
              END DO
              IDEB = IDEB + A_AR*NB
            END IF
          END IF
        END DO
      END DO
      DO L = 1, NBISEND
        I = ISINDEX(L)
        CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
      END DO
C
C Calcul des valeurs nodales sur proc main si besoin (test global ?)
C     
      K0=NSTRF(25)
      DO N = 1, NSECT
        PMAIN = FR_SEC(NSPMD+1,N)
        IFRAM = NSTRF(K0+26)
        IF(LOC_PROC==PMAIN) THEN
          IF(MOD(IFRAM,10)==1.OR.MOD(IFRAM,10)==2)THEN
           IF(DSEC(N)/=0) THEN
            XSEC(4,1,N) =  XSEC(4,1,N)/DSEC(N)
            XSEC(4,2,N) =  XSEC(4,2,N)/DSEC(N)
            XSEC(4,3,N) =  XSEC(4,3,N)/DSEC(N)
           END IF
          END IF
        END IF
        K0=NSTRF(K0+24)
      END DO
C
      NBIRECV = 0
      IDEBR = 1
      DO I = 1, NSPMD
        IAD_RECV(I) = IDEBR
        IF(IAD_SEC(4,I)>0) THEN
          MSGTYP = MSGOFF2 
          NBIRECV = NBIRECV + 1
          IRINDEX(NBIRECV) = I
          SIZ = IAD_SEC(4,I)*A_AR2
          CALL MPI_IRECV(
     S       RBUF2(IDEBR),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G       MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
          IDEBR = IDEBR + SIZ
        ENDIF
      ENDDO
C reste a coder l'envoi des noeuds vers les procs concernes
      NBISEND = 0
      IF(IAD_SEC(3,NSPMD+1)>0) THEN
        L = 0
        K0=NSTRF(25)
        DO N = 1, NSECT
          PMAIN = FR_SEC(NSPMD+1,N)
          N1 = NSTRF(K0+3)
          N2 = NSTRF(K0+4)
          N3 = NSTRF(K0+5)
          IFRAM = NSTRF(K0+26)
          IF(LOC_PROC==PMAIN) THEN
            SBUF2(L+1) = N
            IF (IFRAM<=10.OR.N1/=0) THEN
              SBUF2(L+2) = XSEC(1,1,N)
              SBUF2(L+3) = XSEC(1,2,N)
              SBUF2(L+4) = XSEC(1,3,N)
              SBUF2(L+5) = XSEC(2,1,N)
              SBUF2(L+6) = XSEC(2,2,N)
              SBUF2(L+7) = XSEC(2,3,N)
              SBUF2(L+8) = XSEC(3,1,N)
              SBUF2(L+9) = XSEC(3,2,N)
              SBUF2(L+10)= XSEC(3,3,N)
            ELSE
              SBUF2(L+2) = ZERO
              SBUF2(L+3) = ZERO
              SBUF2(L+4) = ZERO
              SBUF2(L+5) = ZERO
              SBUF2(L+6) = ZERO
              SBUF2(L+7) = ZERO
              SBUF2(L+8) = ZERO
              SBUF2(L+9) = ZERO
              SBUF2(L+10)= ZERO
            END IF
            IF(MOD(IFRAM,10)==1.OR.MOD(IFRAM,10)==2) THEN            
              SBUF2(L+11) = XSEC(4,1,N)
              SBUF2(L+12) = XSEC(4,2,N)
              SBUF2(L+13) = XSEC(4,3,N)
            ELSE
              SBUF2(L+11) = ZERO
              SBUF2(L+12) = ZERO
              SBUF2(L+13) = ZERO
            END IF
            L = L + A_AR2
          END IF
          K0=NSTRF(K0+24)
        END DO
C      
        DO I = 1, NSPMD
          IF(IAD_SEC(3,I)>0) THEN
            MSGTYP = MSGOFF2 
            NBISEND = NBISEND + 1
            ISINDEX(NBISEND) = I
            CALL MPI_ISEND(
     S        SBUF2,L,REAL,IT_SPMD(I),MSGTYP,
     G        MPI_COMM_WORLD,REQ_S(I),IERROR)
          END IF
        END DO
      END IF
C
      DO II = 1, NBIRECV
        CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,IERROR)
        I = IRINDEX(INDEX)
        L = IAD_RECV(I)
        NBNOD = IAD_SEC(4,I)
        DO J = 1, NBNOD
          N = NINT(RBUF2(L))
          XSEC(1,1,N) = RBUF2(L+1)
          XSEC(1,2,N) = RBUF2(L+2)
          XSEC(1,3,N) = RBUF2(L+3)
          XSEC(2,1,N) = RBUF2(L+4)
          XSEC(2,2,N) = RBUF2(L+5)
          XSEC(2,3,N) = RBUF2(L+6)
          XSEC(3,1,N) = RBUF2(L+7)
          XSEC(3,2,N) = RBUF2(L+8)
          XSEC(3,3,N) = RBUF2(L+9)
          XSEC(4,1,N) = RBUF2(L+10)
          XSEC(4,2,N) = RBUF2(L+11)
          XSEC(4,3,N) = RBUF2(L+12)
          L = L + A_AR2
        END DO
      END DO
C
      DO L = 1, NBISEND
        I = ISINDEX(L)
        CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
      END DO
C
#endif
      RETURN
      END
C
C nouvelle routine cut section en SPMD (ecriture deplacement)
Chd|====================================================================
Chd|  SPMD_WRT_CUTD                 source/mpi/sections/spmd_section.F
Chd|-- called by -----------
Chd|        SECT_IO                       source/tools/sect/sectio.F    
Chd|-- calls ---------------
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE SPMD_WRT_CUTD(NNOD   ,NSTRF ,D    ,DR    ,RG_CUT,
     2                         IAD_CUT,NSIZE ,NNODG,WEIGHT,IFLG  )
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
#include      "com03_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSTRF(*), WEIGHT(*), RG_CUT(*), IAD_CUT(*),
     .        NNOD, NSIZE, NNODG, IFLG
      my_real
     .        D(3,*), DR(3,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER LOC_PROC,N,L,I,K,LEN,II,INDEX,NB,
     .        MSGTYP,MSGOFF,SIZ,IDEBR,
     .        IERROR, NBIRECV,IDEB, 
     .        IAD_RECV(NSPMD+1),REQ_R(NSPMD),IRINDEX(NSPMD),
     .        STATUS(MPI_STATUS_SIZE)
      DATA MSGOFF/4003/
      my_real
     .        SBUF((3*IFLG+1)*NNOD),RBUF((3*IFLG+1)*NSIZE),
     .        SECBUFG(3*IFLG,NNODG)
      REAL*4 R4
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
      IF(LOC_PROC/=1.AND.NNOD>0) THEN
        L = 0
        IF(IFLG==2) THEN
          DO K = 1, NNOD
            N = NSTRF(K)
            IF(WEIGHT(N)==1) THEN
              SBUF(L+1) = RG_CUT(K)
              SBUF(L+2) = D(1,N)
              SBUF(L+3) = D(2,N)
              SBUF(L+4) = D(3,N)
              SBUF(L+5) = DR(1,N)
              SBUF(L+6) = DR(2,N)
              SBUF(L+7) = DR(3,N)
              L = L + 7
            END IF
          END DO
        ELSEIF(IFLG==1) THEN 
          DO K = 1, NNOD
            N = NSTRF(K)
            IF(WEIGHT(N)==1) THEN
              SBUF(L+1) = RG_CUT(K)
              SBUF(L+2) = D(1,N)
              SBUF(L+3) = D(2,N)
              SBUF(L+4) = D(3,N)
              L = L + 4
            END IF
          END DO
        END IF
        MSGTYP = MSGOFF 
        CALL MPI_SEND(SBUF,L,REAL,IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,IERROR)
      ELSEIF(LOC_PROC==1) THEN
C P0
        NBIRECV = 0
        IDEBR = 1
        DO I = 2, NSPMD
          IAD_RECV(I) = IDEBR
          IF(IAD_CUT(I)>0) THEN
            MSGTYP = MSGOFF
            NBIRECV = NBIRECV + 1
            IRINDEX(NBIRECV) = I
            SIZ = IAD_CUT(I)*(1+IFLG*3)
            CALL MPI_IRECV(
     S       RBUF(IDEBR),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G       MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
            IDEBR = IDEBR + SIZ
          ENDIF
        END DO        
        IAD_RECV(NSPMD+1) = IDEBR
C
        IF(IFLG==2) THEN
          DO K = 1, NNOD
            N = NSTRF(K)
            IF(WEIGHT(N)==1) THEN
              I = RG_CUT(K)
              SECBUFG(1,I) = D(1,N)
              SECBUFG(2,I) = D(2,N)
              SECBUFG(3,I) = D(3,N)
              SECBUFG(4,I) = DR(1,N)
              SECBUFG(5,I) = DR(2,N)
              SECBUFG(6,I) = DR(3,N)
            END IF
          END DO
        ELSEIF(IFLG==1) THEN 
          DO K = 1, NNOD
            N = NSTRF(K)
            IF(WEIGHT(N)==1) THEN
              I = RG_CUT(K)
              SECBUFG(1,I) = D(1,N)
              SECBUFG(2,I) = D(2,N)
              SECBUFG(3,I) = D(3,N)
            END IF
          END DO
        END IF
C
        DO II = 1, NBIRECV
          CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,IERROR)
          I = IRINDEX(INDEX)
          IDEB = IAD_RECV(I)
          LEN = IFLG*3+1
          CALL MPI_GET_COUNT(STATUS,REAL,SIZ,IERROR)
          NB = SIZ/LEN
          IF(IFLG==2) THEN               
            DO K = 1, NB
              I = NINT(RBUF(IDEB+(K-1)*LEN))
              SECBUFG(1,I) = RBUF(IDEB+(K-1)*LEN+1)
              SECBUFG(2,I) = RBUF(IDEB+(K-1)*LEN+2)
              SECBUFG(3,I) = RBUF(IDEB+(K-1)*LEN+3)
              SECBUFG(4,I) = RBUF(IDEB+(K-1)*LEN+4)
              SECBUFG(5,I) = RBUF(IDEB+(K-1)*LEN+5)
              SECBUFG(6,I) = RBUF(IDEB+(K-1)*LEN+6)
            END DO
          ELSE
            DO K = 1, NB
              I = NINT(RBUF(IDEB+(K-1)*LEN))
              SECBUFG(1,I) = RBUF(IDEB+(K-1)*LEN+1)
              SECBUFG(2,I) = RBUF(IDEB+(K-1)*LEN+2)
              SECBUFG(3,I) = RBUF(IDEB+(K-1)*LEN+3)
            END DO
          END IF
        END DO
C ecriture p0
        IF(IFLG==2) THEN
          DO I = 1,NNODG
            R4 = SECBUFG(1,I)
            CALL WRITE_R_C(R4,1)
            R4 = SECBUFG(2,I)
            CALL WRITE_R_C(R4,1)
            R4 = SECBUFG(3,I)
            CALL WRITE_R_C(R4,1)
            R4 = SECBUFG(4,I)
            CALL WRITE_R_C(R4,1)
            R4 = SECBUFG(5,I)
            CALL WRITE_R_C(R4,1)
            R4 = SECBUFG(6,I)
            CALL WRITE_R_C(R4,1)
          END DO
        ELSEIF(IFLG==1) THEN
          DO I = 1,NNODG
            R4 = SECBUFG(1,I)
            CALL WRITE_R_C(R4,1)
            R4 = SECBUFG(2,I)
            CALL WRITE_R_C(R4,1)
            R4 = SECBUFG(3,I)
            CALL WRITE_R_C(R4,1)
            R4 = ZERO
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
          END DO
        END IF
      END IF
C
#endif
      RETURN
      END
C
C nouvelle routine cut section en SPMD (ecriture force)
Chd|====================================================================
Chd|  SPMD_WRT_CUTF                 source/mpi/sections/spmd_section.F
Chd|-- called by -----------
Chd|        SECT_IO                       source/tools/sect/sectio.F    
Chd|-- calls ---------------
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE SPMD_WRT_CUTF(NNOD  ,NSTRF,SECFCUM,RG_CUT,IAD_CUT,
     2                         NSIZE ,NNODG,WEIGHT ,IFLG )
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
#include      "com03_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSTRF(*), WEIGHT(*), RG_CUT(*), IAD_CUT(*),
     .        NNOD, NSIZE, NNODG, IFLG
      my_real
     .        SECFCUM(7,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER LOC_PROC,N,L,I,K,LEN,II,INDEX,NB,
     .        MSGTYP,MSGOFF,SIZ,IDEBR,
     .        IERROR, NBIRECV,IDEB, 
     .        IAD_RECV(NSPMD+1),REQ_R(NSPMD),IRINDEX(NSPMD),
     .        STATUS(MPI_STATUS_SIZE)
      DATA MSGOFF/4004/
      my_real
     .        SBUF((3*IFLG+1)*NNOD),RBUF((3*IFLG+1)*NSIZE),
     .        SECBUFG(3*IFLG,NNODG)
      REAL*4 R4
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
      IF(LOC_PROC/=1.AND.NNOD>0) THEN
        L = 0
        IF(IFLG==2) THEN
          DO K = 1, NNOD
            N = NSTRF(K)
            IF(WEIGHT(N)==1) THEN
              SBUF(L+1) = RG_CUT(K)
              SBUF(L+2) = SECFCUM(1,N)
              SBUF(L+3) = SECFCUM(2,N)
              SBUF(L+4) = SECFCUM(3,N)
              SBUF(L+5) = SECFCUM(5,N)
              SBUF(L+6) = SECFCUM(6,N)
              SBUF(L+7) = SECFCUM(7,N)
              L = L + 7
            END IF
          END DO
        ELSEIF(IFLG==1) THEN 
          DO K = 1, NNOD
            N = NSTRF(K)
            IF(WEIGHT(N)==1) THEN
              SBUF(L+1) = RG_CUT(K)
              SBUF(L+2) = SECFCUM(1,N)
              SBUF(L+3) = SECFCUM(2,N)
              SBUF(L+4) = SECFCUM(3,N)
              L = L + 4
            END IF
          END DO
        END IF
        MSGTYP = MSGOFF 
        CALL MPI_SEND(SBUF,L,REAL,IT_SPMD(1),MSGTYP,
     .                MPI_COMM_WORLD,IERROR)
      ELSEIF(LOC_PROC==1) THEN
C P0
        NBIRECV = 0
        IDEBR = 1
        DO I = 2, NSPMD
          IAD_RECV(I) = IDEBR
          IF(IAD_CUT(I)>0) THEN
            MSGTYP = MSGOFF 
            NBIRECV = NBIRECV + 1
            IRINDEX(NBIRECV) = I
            SIZ = IAD_CUT(I)*(1+IFLG*3)
            CALL MPI_IRECV(
     S       RBUF(IDEBR),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G       MPI_COMM_WORLD,REQ_R(NBIRECV),IERROR)
            IDEBR = IDEBR + SIZ
          ENDIF
        END DO        
        IAD_RECV(NSPMD+1) = IDEBR
C
        IF(IFLG==2) THEN
          DO I = 1, NNODG
            SECBUFG(1,I) = ZERO
            SECBUFG(2,I) = ZERO
            SECBUFG(3,I) = ZERO
            SECBUFG(4,I) = ZERO
            SECBUFG(5,I) = ZERO
            SECBUFG(6,I) = ZERO
          END DO
C
          DO K = 1, NNOD
            N = NSTRF(K)
            I = RG_CUT(K)
            SECBUFG(1,I) = SECFCUM(1,N)
            SECBUFG(2,I) = SECFCUM(2,N)
            SECBUFG(3,I) = SECFCUM(3,N)
            SECBUFG(4,I) = SECFCUM(5,N)
            SECBUFG(5,I) = SECFCUM(6,N)
            SECBUFG(6,I) = SECFCUM(7,N)
          END DO
        ELSEIF(IFLG==1) THEN 
          DO I = 1, NNODG
            SECBUFG(1,I) = ZERO
            SECBUFG(2,I) = ZERO
            SECBUFG(3,I) = ZERO
          END DO
C
          DO K = 1, NNOD
            N = NSTRF(K)
            I = RG_CUT(K)
            SECBUFG(1,I) = SECFCUM(1,N)
            SECBUFG(2,I) = SECFCUM(2,N)
            SECBUFG(3,I) = SECFCUM(3,N)
          END DO
        END IF
C
        DO II = 1, NBIRECV
          CALL MPI_WAITANY(NBIRECV,REQ_R,INDEX,STATUS,IERROR)
          CALL MPI_GET_COUNT(STATUS,REAL,NB,IERROR)
          I = IRINDEX(INDEX)
          IDEB = IAD_RECV(I)
          LEN = IFLG*3+1
          NB = NB / LEN
          IF(IFLG==2) THEN               
            DO K = 1, NB
              I = NINT(RBUF(IDEB+(K-1)*LEN))
              SECBUFG(1,I) = SECBUFG(1,I) + RBUF(IDEB+(K-1)*LEN+1)
              SECBUFG(2,I) = SECBUFG(2,I) + RBUF(IDEB+(K-1)*LEN+2)
              SECBUFG(3,I) = SECBUFG(3,I) + RBUF(IDEB+(K-1)*LEN+3)
              SECBUFG(4,I) = SECBUFG(4,I) + RBUF(IDEB+(K-1)*LEN+4)
              SECBUFG(5,I) = SECBUFG(5,I) + RBUF(IDEB+(K-1)*LEN+5)
              SECBUFG(6,I) = SECBUFG(6,I) + RBUF(IDEB+(K-1)*LEN+6)
            END DO
          ELSE
            DO K = 1, NB
              I = NINT(RBUF(IDEB+(K-1)*LEN))
              SECBUFG(1,I) = SECBUFG(1,I) + RBUF(IDEB+(K-1)*LEN+1)
              SECBUFG(2,I) = SECBUFG(2,I) + RBUF(IDEB+(K-1)*LEN+2)
              SECBUFG(3,I) = SECBUFG(3,I) + RBUF(IDEB+(K-1)*LEN+3)
            END DO
          END IF
        END DO
C ecriture p0
        IF(IFLG==2) THEN
          DO I = 1,NNODG
            R4 = SECBUFG(1,I)
            CALL WRITE_R_C(R4,1)
            R4 = SECBUFG(2,I)
            CALL WRITE_R_C(R4,1)
            R4 = SECBUFG(3,I)
            CALL WRITE_R_C(R4,1)
            R4 = SECBUFG(4,I)
            CALL WRITE_R_C(R4,1)
            R4 = SECBUFG(5,I)
            CALL WRITE_R_C(R4,1)
            R4 = SECBUFG(6,I)
            CALL WRITE_R_C(R4,1)
          END DO
        ELSEIF(IFLG==1) THEN
          DO I = 1,NNODG
            R4 = SECBUFG(1,I)
            CALL WRITE_R_C(R4,1)
            R4 = SECBUFG(2,I)
            CALL WRITE_R_C(R4,1)
            R4 = SECBUFG(3,I)
            CALL WRITE_R_C(R4,1)
            R4 = ZERO
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
            CALL WRITE_R_C(R4,1)
          END DO
        END IF
      END IF
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_EXCH_CUT                 source/mpi/sections/spmd_section.F
Chd|-- called by -----------
Chd|        SECT_IO                       source/tools/sect/sectio.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_CUT(NSTRF,SECFCUM,IAD_ELEM,FR_ELEM,SIZE,
     2                         LENR ,NNOD,WEIGHT)
C cumul de secfcum au noeuds frontieres puis mise a 0 sur procs additionnels
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
#include      "scr06_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NSTRF(*), IAD_ELEM(2,*), FR_ELEM(*), WEIGHT(*),
     .        SIZE, LENR, NNOD
       my_real
     .        SECFCUM(7,*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER MSGTYP,I,NOD,LOC_PROC,IERROR,
     .        SIZ,J,L,NB_NOD,
     .        STATUS(MPI_STATUS_SIZE),
     .        IAD_SEND(NSPMD+1),IAD_RECV(NSPMD+1),
     .        REQ_R(NSPMD),REQ_S(NSPMD),MSGOFF
       my_real
     .        RBUF(SIZE*LENR), SBUF(SIZE*LENR)
       DATA MSGOFF/4005/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
      L = 1
      IAD_RECV(1) = 1
      DO I=1,NSPMD
        SIZ = SIZE*(IAD_ELEM(1,I+1)-IAD_ELEM(1,I))
        IF(SIZ/=0)THEN
          MSGTYP = MSGOFF
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L               
      END DO
      L = 1
      IAD_SEND(1) = 1
      DO I=1,NSPMD
        IF(SIZE==6) THEN
#include      "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            NOD = FR_ELEM(J)
            SBUF(L  ) =  SECFCUM(1,NOD)
            SBUF(L+1) =  SECFCUM(2,NOD)
            SBUF(L+2) =  SECFCUM(3,NOD)
            SBUF(L+3) =  SECFCUM(5,NOD)
            SBUF(L+4) =  SECFCUM(6,NOD)
            SBUF(L+5) =  SECFCUM(7,NOD)
            L = L + SIZE
          END DO
        ELSE
#include      "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
            NOD = FR_ELEM(J)
            SBUF(L  ) =  SECFCUM(1,NOD)
            SBUF(L+1) =  SECFCUM(2,NOD)
            SBUF(L+2) =  SECFCUM(3,NOD)
            L = L + SIZE
          END DO
        END IF
        IAD_SEND(I+1) = L
      ENDDO
C
C   echange messages
C
      DO I=1,NSPMD
C--------------------------------------------------------------------
       IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
          MSGTYP = MSGOFF
          SIZ = IAD_SEND(I+1)-IAD_SEND(I)
          L = IAD_SEND(I)
          CALL MPI_ISEND(
     S      SBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
       ENDIF        
C--------------------------------------------------------------------
      ENDDO
C
      DO I = 1, NSPMD
        NB_NOD = IAD_ELEM(1,I+1)-IAD_ELEM(1,I)
        IF(NB_NOD>0)THEN
          CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
          L = IAD_RECV(I)
          IF(SIZE==6) THEN
#include      "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SECFCUM(1,NOD) = SECFCUM(1,NOD) + RBUF(L)
              SECFCUM(2,NOD) = SECFCUM(2,NOD) + RBUF(L+1)
              SECFCUM(3,NOD) = SECFCUM(3,NOD) + RBUF(L+2)
              SECFCUM(5,NOD) = SECFCUM(5,NOD) + RBUF(L+3)
              SECFCUM(6,NOD) = SECFCUM(6,NOD) + RBUF(L+4)
              SECFCUM(7,NOD) = SECFCUM(7,NOD) + RBUF(L+5)
              L = L + SIZE
            END DO
          ELSE
#include      "vectorize.inc"
            DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
              NOD = FR_ELEM(J)
              SECFCUM(1,NOD) = SECFCUM(1,NOD) + RBUF(L)
              SECFCUM(2,NOD) = SECFCUM(2,NOD) + RBUF(L+1)
              SECFCUM(3,NOD) = SECFCUM(3,NOD) + RBUF(L+2)
              L = L + SIZE
            END DO
          END IF
        END IF
      END DO
C
C Remise a 0 de SECFCUM si P non main
C
      IF(SIZE==6) THEN
#include "vectorize.inc"
        DO I = 1, NNOD
          NOD = NSTRF(I)
          SECFCUM(1,NOD) = SECFCUM(1,NOD)*WEIGHT(NOD)
          SECFCUM(2,NOD) = SECFCUM(2,NOD)*WEIGHT(NOD)
          SECFCUM(3,NOD) = SECFCUM(3,NOD)*WEIGHT(NOD)
          SECFCUM(5,NOD) = SECFCUM(5,NOD)*WEIGHT(NOD)
          SECFCUM(6,NOD) = SECFCUM(6,NOD)*WEIGHT(NOD)
          SECFCUM(7,NOD) = SECFCUM(7,NOD)*WEIGHT(NOD)
        END DO
      ELSE
#include "vectorize.inc"
        DO I = 1, NNOD
          NOD = NSTRF(I)
          SECFCUM(1,NOD) = SECFCUM(1,NOD)*WEIGHT(NOD)
          SECFCUM(2,NOD) = SECFCUM(2,NOD)*WEIGHT(NOD)
          SECFCUM(3,NOD) = SECFCUM(3,NOD)*WEIGHT(NOD)
        END DO
      END IF
C
      DO I = 1, NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I)>0)THEN
          CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
        ENDIF
      ENDDO
C
#endif
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SD_CUT                   source/mpi/sections/spmd_section.F
Chd|-- called by -----------
Chd|        SECT_READP                    source/tools/sect/sectio.F    
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_SD_CUT(SECBUFG,NNODG  ,SECBUF1,SECBUF2,NNOD,
     2                       FR_CUT ,IAD_CUT,IFLG   )
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
#include      "com03_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FR_CUT(*),IAD_CUT(*),
     .        NNOD, NNODG, IFLG
      my_real
     .        SECBUFG(*), SECBUF1(*), SECBUF2(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER LOC_PROC,N,L,I,K,LEN,P,NN,N0,OFFG,
     .        MSGTYP,MSGOFF,SIZ,IDEBR,
     .        IERROR, NBIRECV,IDEB, 
     .        IAD_RECV(NSPMD+1),
     .        STATUS(MPI_STATUS_SIZE)
      DATA MSGOFF/4006/
      my_real
     .        SBUF(6*IFLG*NNODG),RBUF(6*IFLG*NNOD)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      LOC_PROC = ISPMD + 1
C
      IF(LOC_PROC==1) THEN
        N0 = 0
        OFFG = 6*NNODG
C Traitement P0
        IF(IAD_CUT(1)/=0) THEN
          NN = IAD_CUT(1)
          IF(IFLG==2) THEN
            DO I = 1, NN
              N = FR_CUT(I+N0)
              SECBUF1(6*I-5) = SECBUFG((N-1)*6+1)
              SECBUF1(6*I-4) = SECBUFG((N-1)*6+2)
              SECBUF1(6*I-3) = SECBUFG((N-1)*6+3)
              SECBUF1(6*I-2) = SECBUFG((N-1)*6+4)
              SECBUF1(6*I-1) = SECBUFG((N-1)*6+5)
              SECBUF1(6*I)   = SECBUFG((N-1)*6+6)
              SECBUF2(6*I-5) = SECBUFG(OFFG+(N-1)*6+1)
              SECBUF2(6*I-4) = SECBUFG(OFFG+(N-1)*6+2)
              SECBUF2(6*I-3) = SECBUFG(OFFG+(N-1)*6+3)
              SECBUF2(6*I-2) = SECBUFG(OFFG+(N-1)*6+4)
              SECBUF2(6*I-1) = SECBUFG(OFFG+(N-1)*6+5)
              SECBUF2(6*I)   = SECBUFG(OFFG+(N-1)*6+6)
            END DO
          ELSEIF(IFLG==1) THEN 
            DO I = 1, NN
              N = FR_CUT(I+N0)
              SECBUF1(6*I-5) = SECBUFG((N-1)*6+1)
              SECBUF1(6*I-4) = SECBUFG((N-1)*6+2)
              SECBUF1(6*I-3) = SECBUFG((N-1)*6+3)
              SECBUF1(6*I-2) = SECBUFG((N-1)*6+4)
              SECBUF1(6*I-1) = SECBUFG((N-1)*6+5)
              SECBUF1(6*I)   = SECBUFG((N-1)*6+6)
            END DO
          END IF
          N0 = N0 + NN
        END IF
C Traitement autres procs
        DO P = 2, NSPMD
          IF(IAD_CUT(P)/=0) THEN
            L = 0
            NN = IAD_CUT(P)
            IF(IFLG==2) THEN
              DO K = 1, NN
                N = FR_CUT(K+N0)
                SBUF(L+1) = SECBUFG((N-1)*6+1)
                SBUF(L+2) = SECBUFG((N-1)*6+2)
                SBUF(L+3) = SECBUFG((N-1)*6+3)
                SBUF(L+4) = SECBUFG((N-1)*6+4)
                SBUF(L+5) = SECBUFG((N-1)*6+5)
                SBUF(L+6) = SECBUFG((N-1)*6+6)
                SBUF(L+7) = SECBUFG(OFFG+(N-1)*6+1)
                SBUF(L+8) = SECBUFG(OFFG+(N-1)*6+2)
                SBUF(L+9) = SECBUFG(OFFG+(N-1)*6+3)
                SBUF(L+10)= SECBUFG(OFFG+(N-1)*6+4)
                SBUF(L+11)= SECBUFG(OFFG+(N-1)*6+5)
                SBUF(L+12)= SECBUFG(OFFG+(N-1)*6+6)
                L = L + 12
              END DO
            ELSEIF(IFLG==1) THEN 
              DO K = 1, NN
                N = FR_CUT(K+N0)
                SBUF(L+1) = SECBUFG((N-1)*6+1)
                SBUF(L+2) = SECBUFG((N-1)*6+2)
                SBUF(L+3) = SECBUFG((N-1)*6+3)
                SBUF(L+4) = SECBUFG((N-1)*6+4)
                SBUF(L+5) = SECBUFG((N-1)*6+5)
                SBUF(L+6) = SECBUFG((N-1)*6+6)
                L = L + 6
              END DO
            END IF
            N0 = N0 + NN
            MSGTYP = MSGOFF
            CALL MPI_SEND(SBUF,L,REAL,IT_SPMD(P),MSGTYP,
     .                    MPI_COMM_WORLD,IERROR)
          END IF
        END DO
      ELSEIF(LOC_PROC/=1.AND.NNOD>0) THEN
        MSGTYP = MSGOFF 
        SIZ = NNOD*IFLG*6
        CALL MPI_RECV(
     S     RBUF,SIZ,REAL,IT_SPMD(1),MSGTYP,
     G     MPI_COMM_WORLD,STATUS,IERROR)
C
        IF(IFLG==2) THEN
          L = 0
          DO I = 1, NNOD
            SECBUF1(6*I-5) = RBUF(L+1)
            SECBUF1(6*I-4) = RBUF(L+2)
            SECBUF1(6*I-3) = RBUF(L+3)
            SECBUF1(6*I-2) = RBUF(L+4)
            SECBUF1(6*I-1) = RBUF(L+5)
            SECBUF1(6*I)   = RBUF(L+6)
            SECBUF2(6*I-5) = RBUF(L+7)
            SECBUF2(6*I-4) = RBUF(L+8)
            SECBUF2(6*I-3) = RBUF(L+9)
            SECBUF2(6*I-2) = RBUF(L+10)
            SECBUF2(6*I-1) = RBUF(L+11)
            SECBUF2(6*I)   = RBUF(L+12)
            L = L + 12
          END DO
        ELSEIF(IFLG==1) THEN 
          L = 0
          DO I = 1, NNOD
            SECBUF1(6*I-5) = RBUF(L+1)
            SECBUF1(6*I-4) = RBUF(L+2)
            SECBUF1(6*I-3) = RBUF(L+3)
            SECBUF1(6*I-2) = RBUF(L+4)
            SECBUF1(6*I-1) = RBUF(L+5)
            SECBUF1(6*I)   = RBUF(L+6)
            L = L + 6
          END DO
        END IF
      END IF
C
#endif
      RETURN
      END
