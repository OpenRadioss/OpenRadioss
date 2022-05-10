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
#if defined(MPI)
Chd|====================================================================
Chd|  SPMD_GET_MULT                 source/mpi/lag_multipliers/spmd_lag.F
Chd|-- called by -----------
Chd|        LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SPMD_GET_MULT(
     1     LAGCOMC,LAGCOMK,N_MULT ,BLL ,IADLL,
     2     LLL    ,JLL    ,SLL    ,XLL ,COMNTAG,
     3     ICFTAG ,JCFTAG ,FR_LAGF,N_IK)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#include      "mpif.h"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N_MULT, N_IK,
     .        LLL(*), JLL(*), SLL(*), IADLL(*),
     .        COMNTAG(*), ICFTAG(*), JCFTAG(*), FR_LAGF(3,*)
      my_real
     .        LAGCOMK(4,*),LAGCOMC(2,*), XLL(*), BLL(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF,MSGOFF2
      INTEGER MSGTYP,I,NCL,IKL,N, LOC_PROC

      DATA MSGOFF/12001/
      DATA MSGOFF2/12002/

       LOC_PROC = ISPMD+1
         
       IF (ISPMD.NE.0) THEN
        IF(FR_LAGF(1,LOC_PROC).GT.0) THEN

         MSGTYP = MSGOFF
         CALL MPI_SEND(
     1     LAGCOMC       ,2*N_MULT,REAL  ,IT_SPMD(1),MSGTYP,
     2     MPI_COMM_WORLD,IERROR)
         MSGTYP = MSGOFF2
         CALL MPI_SEND(
     1     LAGCOMK       ,4*N_IK,REAL  ,IT_SPMD(1),MSGTYP,
     2     MPI_COMM_WORLD,IERROR)
        END IF
       ELSE
         DO N = 1, N_MULT
           IADLL(N+1) = IADLL(N)+NINT(LAGCOMC(1,N))
           BLL(N)   = LAGCOMC(2,N)
Cel ICTAG et JCFTAG : id (pas de cond. autre que fixe pour le moment !)
           ICFTAG(N) = N
           JCFTAG(N) = N
         END DO
         DO N = 1, N_IK
           LLL(N) = NINT(LAGCOMK(1,N))
           JLL(N) = NINT(LAGCOMK(2,N))
           SLL(N) = NINT(LAGCOMK(3,N))
           XLL(N) = LAGCOMK(4,N)
Cel mise a jour du flag directement ici et non dans LTAG_FXV
           COMNTAG(LLL(N)) = COMNTAG(LLL(N))+1
         END DO     
C
         DO I=2,NSPMD
           NCL = FR_LAGF(1,I)
           IF(NCL.GT.0)THEN
             MSGTYP = MSGOFF 

             CALL MPI_RECV(
     1         LAGCOMC(1,N_MULT+1),2*NCL ,REAL  ,IT_SPMD(I),MSGTYP,
     2         MPI_COMM_WORLD     ,STATUS,IERROR)
             
             DO N = 1, NCL
               IADLL(N_MULT+N+1) = IADLL(N_MULT+N)
     +                            +NINT(LAGCOMC(1,N_MULT+N))
               BLL(N_MULT+N)     = LAGCOMC(2,N_MULT+N)
Cel ICTAG et JCFTAG : id (pas de cond. autre que fixe pour le moment !)
               ICFTAG(N_MULT+N) = N_MULT+N
               JCFTAG(N_MULT+N) = N_MULT+N
             END DO
             N_MULT=N_MULT+NCL
C
             MSGTYP = MSGOFF2 
             IKL = FR_LAGF(2,I)
             CALL MPI_RECV(
     1         LAGCOMK(1,N_IK+1),4*IKL ,REAL  ,IT_SPMD(I),MSGTYP,
     2         MPI_COMM_WORLD   ,STATUS,IERROR)
             DO N = 1, IKL
               LLL(N_IK+N) = NINT(LAGCOMK(1,N_IK+N))
               JLL(N_IK+N) = NINT(LAGCOMK(2,N_IK+N))
               SLL(N_IK+N) = NINT(LAGCOMK(3,N_IK+N))
               XLL(N_IK+N) = LAGCOMK(4,N_IK+N)
Cel mise a jour du flag directement ici et non dans LTAG_FXV
               COMNTAG(LLL(N_IK+N)) = COMNTAG(LLL(N_IK+N))+1
             END DO
             N_IK = N_IK + IKL
           END IF             
         END DO
c         IF(N_MULT.NE.FR_LAGF(1,NSPMD+1))
c     .     print*,'**error : wrong gather of LAG MULT EQUATIONS',
c     .     N_MULT,FR_LAGF(1,NSPMD+1)
c         IF(N_IK.NE.FR_LAGF(2,NSPMD+1))
c     .     print*,'**error : wrong gather of LAG MULT VARIABLES',
c     .     N_IK,FR_LAGF(2,NSPMD+1)
C

       END IF
C
       RETURN
       END
C
Chd|====================================================================
Chd|  SPMD_GG_MULT                  source/mpi/lag_multipliers/spmd_lag.F
Chd|-- called by -----------
Chd|        LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SPMD_GG_MULT(
     1     A       ,AR      ,V      ,VR   ,MS    ,
     2     IN      ,AG      ,ARG    ,VG   ,VRG   ,
     3     MSG     ,ING     ,FR_LAGF,ISIZ ,NBNODL,
     4     INDEXLAG,NODGLOB ,LLAGF  ,NLAGF_L)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#include      "mpif.h"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FR_LAGF(3,*), INDEXLAG(*), NODGLOB(*), LLAGF(*),
     .        NBNODL, NLAGF_L, ISIZ
      my_real
     .        A(3,*), AR(3,*), V(3,*), VR(3,*), MS(*), IN(*),
     .        AG(3,*), ARG(3,*), VG(3,*), VRG(3,*), MSG(*), ING(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF
      INTEGER MSGTYP,I,NNOD,N,NLAGF_G,P
      my_real
     .         BUFCOM(ISIZ,NBNODL)

      DATA MSGOFF/12003/
C
C
C         
       IF (ISPMD.NE.0) THEN
        IF(NLAGF_L.GT.0) THEN
         IF(IRODDL.NE.0)THEN
           DO I = 1, NLAGF_L
             N = LLAGF(I)
             BUFCOM(1,I) = NODGLOB(N)
             BUFCOM(2,I) = A(1,N)
             BUFCOM(3,I) = A(2,N)
             BUFCOM(4,I) = A(3,N)
             BUFCOM(5,I) = MS(N)
             BUFCOM(6,I) = V(1,N)
             BUFCOM(7,I) = V(2,N)
             BUFCOM(8,I) = V(3,N)
             BUFCOM(9,I) = AR(1,N)
             BUFCOM(10,I) = AR(2,N)
             BUFCOM(11,I) = AR(3,N)
             BUFCOM(12,I) = IN(N)
             BUFCOM(13,I) = VR(1,N)
             BUFCOM(14,I) = VR(2,N)
             BUFCOM(15,I) = VR(3,N)
           END DO
         ELSE
           DO I = 1, NLAGF_L
             N = LLAGF(I)
             BUFCOM(1,I) = NODGLOB(N)
             BUFCOM(2,I) = A(1,N)
             BUFCOM(3,I) = A(2,N)
             BUFCOM(4,I) = A(3,N)
             BUFCOM(5,I) = MS(N)
             BUFCOM(6,I) = V(1,N)
             BUFCOM(7,I) = V(2,N)
             BUFCOM(8,I) = V(3,N)
           END DO
         END IF
         MSGTYP = MSGOFF
         CALL MPI_SEND(
     1     BUFCOM        ,ISIZ*NLAGF_L,REAL  ,IT_SPMD(1),MSGTYP,
     2     MPI_COMM_WORLD,IERROR)
        END IF
Code processeur0 
       ELSE
         IF(IRODDL.NE.0)THEN
           DO I = 1, NLAGF_L
             N = LLAGF(I)
             INDEXLAG(NODGLOB(N)) = I
             AG(1,I) = A(1,N)
             AG(2,I) = A(2,N)
             AG(3,I) = A(3,N)
             MSG(I)  = MS(N)
             VG(1,I) = V(1,N)
             VG(2,I) = V(2,N)
             VG(3,I) = V(3,N)
             ARG(1,I)= AR(1,N)
             ARG(2,I)= AR(2,N)
             ARG(3,I)= AR(3,N)
             ING(I)  = IN(N)
             VRG(1,I)= VR(1,N)
             VRG(2,I)= VR(2,N)
             VRG(3,I)= VR(3,N)
           END DO
         ELSE
           DO I = 1, NLAGF_L
             N = LLAGF(I)
             INDEXLAG(NODGLOB(N)) = I
             AG(1,I) = A(1,N)
             AG(2,I) = A(2,N)
             AG(3,I) = A(3,N)
             MSG(I)  = MS(N)
             VG(1,I) = V(1,N)
             VG(2,I) = V(2,N)
             VG(3,I) = V(3,N)
           END DO
         END IF
         NLAGF_G = NLAGF_L
C
         DO P=2,NSPMD
           NNOD = FR_LAGF(3,P)
           IF(NNOD.GT.0)THEN
             MSGTYP = MSGOFF 
             CALL MPI_RECV(
     1         BUFCOM,ISIZ*NNOD ,REAL  ,IT_SPMD(P),MSGTYP,
     2         MPI_COMM_WORLD   ,STATUS,IERROR)
             IF(IRODDL.NE.0)THEN
               DO I = 1, NNOD
                 N = NINT(BUFCOM(1,I))
                 INDEXLAG(N) = NLAGF_G+I
                 AG(1,NLAGF_G+I) = BUFCOM(2,I) 
                 AG(2,NLAGF_G+I) = BUFCOM(3,I) 
                 AG(3,NLAGF_G+I) = BUFCOM(4,I) 
                 MSG(NLAGF_G+I)  = BUFCOM(5,I) 
                 VG(1,NLAGF_G+I) = BUFCOM(6,I) 
                 VG(2,NLAGF_G+I) = BUFCOM(7,I) 
                 VG(3,NLAGF_G+I) = BUFCOM(8,I) 
                 ARG(1,NLAGF_G+I)= BUFCOM(9,I) 
                 ARG(2,NLAGF_G+I)= BUFCOM(10,I)
                 ARG(3,NLAGF_G+I)= BUFCOM(11,I)
                 ING(NLAGF_G+I)  = BUFCOM(12,I)
                 VRG(1,NLAGF_G+I)= BUFCOM(13,I)
                 VRG(2,NLAGF_G+I)= BUFCOM(14,I)
                 VRG(3,NLAGF_G+I)= BUFCOM(15,I)
               END DO
             ELSE
               DO I = 1, NNOD
                 N = NINT(BUFCOM(1,I))
                 INDEXLAG(N) = NLAGF_G+I
                 AG(1,NLAGF_G+I) = BUFCOM(2,I) 
                 AG(2,NLAGF_G+I) = BUFCOM(3,I) 
                 AG(3,NLAGF_G+I) = BUFCOM(4,I) 
                 MSG(NLAGF_G+I)  = BUFCOM(5,I) 
                 VG(1,NLAGF_G+I) = BUFCOM(6,I) 
                 VG(2,NLAGF_G+I) = BUFCOM(7,I) 
                 VG(3,NLAGF_G+I) = BUFCOM(8,I) 
               END DO
             END IF
             NLAGF_G=NLAGF_G+NNOD
           END IF             
         END DO
c         IF(NLAGF_G.NE.FR_LAGF(3,NSPMD+1))
c     .     print*,'**error : wrong gather of LAG MULT NODAL VALUES',
c     .     NLAGF_G,FR_LAGF(3,NSPMD+1)
C

       END IF
C
       RETURN
       END
C
Chd|====================================================================
Chd|  SPMD_SG_MULT                  source/mpi/lag_multipliers/spmd_lag.F
Chd|-- called by -----------
Chd|        LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SPMD_SG_MULT(
     1     A      ,AR     ,AG     ,ARG     ,FR_LAGF,
     2     ISIZ   ,NBNODL ,LLAGF  ,NLAGF_L )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#include      "mpif.h"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FR_LAGF(3,*), LLAGF(*),
     .        NBNODL, NLAGF_L, ISIZ
      my_real
     .        A(3,*), AR(3,*),AG(3,*), ARG(3,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF
      INTEGER MSGTYP,I,NNOD,N,NLAGF_G,P
      my_real
     .         BUFCOM(ISIZ,NBNODL)

      DATA MSGOFF/12004/
C
C
C         
       IF(ISPMD.NE.0) THEN
        IF(NLAGF_L.GT.0) THEN

         MSGTYP = MSGOFF 
         CALL MPI_RECV(
     1     BUFCOM,ISIZ*NLAGF_L,REAL  ,IT_SPMD(1),MSGTYP,
     2     MPI_COMM_WORLD    ,STATUS,IERROR)
         IF(IRODDL.NE.0)THEN
           DO I = 1, NLAGF_L
             N = LLAGF(I)
             A(1,N)  = BUFCOM(1,I)
             A(2,N)  = BUFCOM(2,I)
             A(3,N)  = BUFCOM(3,I)
             AR(1,N) = BUFCOM(4,I)
             AR(2,N) = BUFCOM(5,I)
             AR(3,N) = BUFCOM(6,I)
           END DO
         ELSE
           DO I = 1, NLAGF_L
             N = LLAGF(I)
             A(1,N)  = BUFCOM(1,I)
             A(2,N)  = BUFCOM(2,I)
             A(3,N)  = BUFCOM(3,I)
           END DO
         END IF
        END IF
Code processeur0 
       ELSE
         IF(IRODDL.NE.0)THEN
           DO I = 1, NLAGF_L
             N = LLAGF(I)
             A(1,N)  = AG(1,I) 
             A(2,N)  = AG(2,I) 
             A(3,N)  = AG(3,I) 
             AR(1,N) = ARG(1,I)
             AR(2,N) = ARG(2,I)
             AR(3,N) = ARG(3,I)
           END DO
         ELSE
           DO I = 1, NLAGF_L
             N = LLAGF(I)
             A(1,N)  = AG(1,I)
             A(2,N)  = AG(2,I)
             A(3,N)  = AG(3,I)
           END DO
         END IF
         NLAGF_G = NLAGF_L
C
         DO P=2,NSPMD
           NNOD = FR_LAGF(3,P)
           IF(NNOD.GT.0)THEN
             IF(IRODDL.NE.0)THEN
               DO I = 1, NNOD
                 BUFCOM(1,I) = AG(1,NLAGF_G+I) 
                 BUFCOM(2,I) = AG(2,NLAGF_G+I) 
                 BUFCOM(3,I) = AG(3,NLAGF_G+I) 
                 BUFCOM(4,I) = ARG(1,NLAGF_G+I)
                 BUFCOM(5,I) = ARG(2,NLAGF_G+I)
                 BUFCOM(6,I) = ARG(3,NLAGF_G+I)
               END DO
             ELSE
               DO I = 1, NNOD
                 BUFCOM(1,I) = AG(1,NLAGF_G+I) 
                 BUFCOM(2,I) = AG(2,NLAGF_G+I) 
                 BUFCOM(3,I) = AG(3,NLAGF_G+I) 
               END DO
             END IF
             MSGTYP = MSGOFF 
             CALL MPI_SEND(
     1         BUFCOM        ,ISIZ*NNOD,REAL  ,IT_SPMD(P),MSGTYP,
     2         MPI_COMM_WORLD,IERROR)
             NLAGF_G=NLAGF_G+NNOD
           END IF             
         END DO

       END IF
C
       RETURN
       END
C
Chd|====================================================================
Chd|  SPMD_SG_FANI                  source/mpi/lag_multipliers/spmd_lag.F
Chd|-- called by -----------
Chd|        LAG_ANITHP                    source/tools/lagmul/lag_anith.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SPMD_SG_FANI(
     1     FANI,FANIG,FR_LAGF,NBNODL,LLAGF,NLAGF_L)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#include      "mpif.h"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FR_LAGF(3,*), LLAGF(*),
     .        NBNODL, NLAGF_L
      my_real
     .        FANI(3,*), FANIG(3,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF
      INTEGER MSGTYP,I,NNOD,N,NLAGF_G,P
      my_real
     .         BUFCOM(3,NBNODL)

      DATA MSGOFF/12005/
C
C
C         
       IF(ISPMD.NE.0) THEN
        IF(NLAGF_L.GT.0) THEN

         MSGTYP = MSGOFF 
         CALL MPI_RECV(
     1     BUFCOM,3*NLAGF_L,REAL  ,IT_SPMD(1),MSGTYP,
     2     MPI_COMM_WORLD    ,STATUS,IERROR)
           DO I = 1, NLAGF_L
             N = LLAGF(I)
             FANI(1,N)  = BUFCOM(1,I)
             FANI(2,N)  = BUFCOM(2,I)
             FANI(3,N)  = BUFCOM(3,I)
           END DO
        END IF
Code processeur0 
       ELSE
         DO I = 1, NLAGF_L
           N = LLAGF(I)
           FANI(1,N)  = FANIG(1,I) 
           FANI(2,N)  = FANIG(2,I) 
           FANI(3,N)  = FANIG(3,I) 
         END DO
         NLAGF_G = NLAGF_L
C
         DO P=2,NSPMD
           NNOD = FR_LAGF(3,P)
           IF(NNOD.GT.0)THEN
             DO I = 1, NNOD
               BUFCOM(1,I) = FANIG(1,NLAGF_G+I) 
               BUFCOM(2,I) = FANIG(2,NLAGF_G+I) 
               BUFCOM(3,I) = FANIG(3,NLAGF_G+I) 
             END DO
             MSGTYP = MSGOFF 
             CALL MPI_SEND(
     1         BUFCOM        ,3*NNOD,REAL  ,IT_SPMD(P),MSGTYP,
     2         MPI_COMM_WORLD,IERROR)
             NLAGF_G=NLAGF_G+NNOD
           END IF             
         END DO

       END IF
C
       RETURN
       END
C
Chd|====================================================================
Chd|  SPMD_EXCH_MULT                source/mpi/lag_multipliers/spmd_lag.F
Chd|-- called by -----------
Chd|        LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_MULT(
     1    A       ,AR     ,LLAGF , NLAGF_L, FR_LAGF,
     2    IAD_ELEM,FR_ELEM,LRBUF , ISIZ   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "mpif.h"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "scr01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLAGF_L,LRBUF,ISIZ,
     .        FR_LAGF(3,*),LLAGF(*),IAD_ELEM(2,*),FR_ELEM(*)
      my_real
     .        A(3,*), AR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, P, N, L, IERROR, MSGOFF, MSGOFF2, ISHIFT,
     .        LOC_PROC, MSGTYP, BUFSIZ, SIZ, INB, NB_NOD, NOD, LSEND,
     .        REQ_R(NSPMD), REQ_S(NSPMD),IAD_SEND(NSPMD+1),
     .        IAD_RECV(NSPMD+1), STATUS(MPI_STATUS_SIZE), ITAG(NUMNOD)
      my_real
     .        RBUF(LRBUF)
      DATA  MSGOFF/12006/
      DATA  MSGOFF2/12007/
C-----------------------------------------------
C
      LOC_PROC = ISPMD+1
C
      DO I = 1, NUMNOD
        ITAG(I) = 0
      END DO
      L = 0
      DO I = 1, NLAGF_L
        N=LLAGF(I)
        ITAG(N) = 1
      END DO
C
C Echange aux frontieres
C
      L = 1
      IAD_RECV(1) = 1
      DO I=1,NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I).NE.0)THEN
          SIZ = ISIZ*(IAD_ELEM(1,I+1)-IAD_ELEM(1,I))+1
          MSGTYP = MSGOFF2
          CALL MPI_IRECV(
     S      RBUF(L),SIZ,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_R(I),IERROR)
          L = L + SIZ
        ENDIF
        IAD_RECV(I+1) = L               
      END DO
C
      IAD_SEND(1) = L
      DO I=1,NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I).NE.0)THEN
         INB = L
         L = L + 1
         NB_NOD = 0
         ISHIFT = IAD_ELEM(1,I)-1
         IF(IRODDL.EQ.0)THEN
#include      "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
           NOD = FR_ELEM(J)
           IF(ITAG(NOD).EQ.1)THEN
            RBUF(L)   = J-ISHIFT
C J-ISHIFT donne l'adresse relative du noeud
            RBUF(L+1) = A(1,NOD)
            RBUF(L+2) = A(2,NOD)
            RBUF(L+3) = A(3,NOD)
            L = L + ISIZ
            NB_NOD = NB_NOD + 1
           ENDIF
          END DO
         ELSE
#include      "vectorize.inc"
          DO J=IAD_ELEM(1,I),IAD_ELEM(1,I+1)-1
           NOD = FR_ELEM(J)
           IF(ITAG(NOD).EQ.1)THEN
            RBUF(L)   = J-ISHIFT
C J-ISHIFT donne l'adresse relative du noeud
            RBUF(L+1) = A(1,NOD)
            RBUF(L+2) = A(2,NOD)
            RBUF(L+3) = A(3,NOD)
            RBUF(L+4) = AR(1,NOD)
            RBUF(L+5) = AR(2,NOD)
            RBUF(L+6) = AR(3,NOD)
            L = L + ISIZ
            NB_NOD = NB_NOD + 1
           ENDIF
          END DO
         END IF
         RBUF(INB) = NB_NOD
        ENDIF
        IAD_SEND(I+1) = L
      ENDDO
C
C   echange messages
C
      DO I=1,NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I).NE.0)THEN
          MSGTYP = MSGOFF2
          L = IAD_SEND(I+1)-IAD_SEND(I)
          LSEND = IAD_SEND(I)
          CALL MPI_ISEND(
     S      RBUF(LSEND),L,REAL,IT_SPMD(I),MSGTYP,
     G      MPI_COMM_WORLD,REQ_S(I),IERROR)
        ENDIF        
      ENDDO
C
C decompactage
C
      DO I = 1, NSPMD
       IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I).NE.0)THEN
         CALL MPI_WAIT(REQ_R(I),STATUS,IERROR)
         L = IAD_RECV(I)
         NB_NOD = NINT(RBUF(L))
         L = L + 1
         IF (NB_NOD.NE.0) THEN
          ISHIFT = IAD_ELEM(1,I)-1
          IF(IRODDL.EQ.0)THEN
#include      "vectorize.inc"
           DO J=1,NB_NOD
             NOD = FR_ELEM(NINT(RBUF(L))+ISHIFT)
C on recupere le bon noeud en fct de sa position relative dans fr_elem, liste triee
             A(1,NOD) = RBUF(L+1)
             A(2,NOD) = RBUF(L+2)
             A(3,NOD) = RBUF(L+3)
             L = L + ISIZ
           END DO
          ELSE
#include      "vectorize.inc"
           DO J=1,NB_NOD
             NOD = FR_ELEM(NINT(RBUF(L))+ISHIFT)
C on recupere le bon noeud en fct de sa position relative dans fr_elem, liste triee
             A(1,NOD) = RBUF(L+1)
             A(2,NOD) = RBUF(L+2)
             A(3,NOD) = RBUF(L+3)
             AR(1,NOD)= RBUF(L+4)
             AR(2,NOD)= RBUF(L+5)
             AR(3,NOD)= RBUF(L+6)
             L = L + ISIZ
           END DO
          END IF
         ENDIF
       ENDIF
      ENDDO
Cel wait terminaison isend
      DO I = 1, NSPMD
        IF(IAD_ELEM(1,I+1)-IAD_ELEM(1,I).NE.0)
     .    CALL MPI_WAIT(REQ_S(I),STATUS,IERROR)
      ENDDO
C
      RETURN
      END
C
#elif 1


Chd|====================================================================
Chd|  SPMD_SG_FANI                  source/mpi/lag_multipliers/spmd_lag.F
Chd|-- called by -----------
Chd|        LAG_ANITHP                    source/tools/lagmul/lag_anith.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_SG_FANI(
     1     RDUM1, RDUM2,IDUM1,IDUM2,IDUM3, IDUM4)
C-----------------------------------------------
#include "implicit_f.inc"
C-----------------------------------------------
      INTEGER
     .        IDUM1, IDUM2, IDUM3, IDUM4, IDUM5
      my_real
     .        RDUM1, RDUM2, RDUM3, RDUM4, RDUM5
      RETURN
      END
C routine simplifiee pour SMP
Chd|====================================================================
Chd|  SPMD_GET_MULT                 source/mpi/lag_multipliers/spmd_lag.F
Chd|-- called by -----------
Chd|        LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SPMD_GET_MULT(
     1     LAGCOMC,LAGCOMK,N_MULT ,BLL ,IADLL,
     2     LLL    ,JLL    ,SLL    ,XLL ,COMNTAG,
     3     ICFTAG ,JCFTAG ,FR_LAGF,N_IK)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N_MULT, N_IK,
     .        LLL(*), JLL(*), SLL(*), IADLL(*),
     .        COMNTAG(*), ICFTAG(*), JCFTAG(*), FR_LAGF(3,*)
      my_real
     .        LAGCOMK(4,*),LAGCOMC(2,*), XLL(*), BLL(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,NCL,IKL,N

      DO N = 1, N_MULT
        IADLL(N+1) = IADLL(N)+NINT(LAGCOMC(1,N))
        BLL(N)   = LAGCOMC(2,N)
Cel ICTAG et JCFTAG : id (pas de cond. autre que fixe pour le moment !)
        ICFTAG(N) = N
        JCFTAG(N) = N
      END DO
      DO N = 1, N_IK
        LLL(N) = NINT(LAGCOMK(1,N))
        JLL(N) = NINT(LAGCOMK(2,N))
        SLL(N) = NINT(LAGCOMK(3,N))
        XLL(N) = LAGCOMK(4,N)
Cel mise a jour du flag directement ici et non dans LTAG_FXV
        COMNTAG(LLL(N)) = COMNTAG(LLL(N))+1
      END DO     
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_GG_MULT                  source/mpi/lag_multipliers/spmd_lag.F
Chd|-- called by -----------
Chd|        LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_GG_MULT(
     1     A       ,AR      ,V      ,VR   ,MS    ,
     2     IN      ,AG      ,ARG    ,VG   ,VRG   ,
     3     MSG     ,ING     ,FR_LAGF,ISIZ ,NBNODL,
     4     INDEXLAG,NODGLOB ,LLAGF  ,NLAGF_L)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FR_LAGF(3,*), INDEXLAG(*), NODGLOB(*), LLAGF(*),
     .        NBNODL, NLAGF_L, ISIZ
      my_real
     .        A(3,*), AR(3,*), V(3,*), VR(3,*), MS(*), IN(*),
     .        AG(3,*), ARG(3,*), VG(3,*), VRG(3,*), MSG(*), ING(*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,N
C         
      IF(IRODDL.NE.0)THEN
        DO I = 1, NLAGF_L
          N = LLAGF(I)
          INDEXLAG(NODGLOB(N)) = I
          AG(1,I) = A(1,N)
          AG(2,I) = A(2,N)
          AG(3,I) = A(3,N)
          MSG(I)  = MS(N)
          VG(1,I) = V(1,N)
          VG(2,I) = V(2,N)
          VG(3,I) = V(3,N)
          ARG(1,I)= AR(1,N)
          ARG(2,I)= AR(2,N)
          ARG(3,I)= AR(3,N)
          ING(I)  = IN(N)
          VRG(1,I)= VR(1,N)
          VRG(2,I)= VR(2,N)
          VRG(3,I)= VR(3,N)
        END DO
      ELSE
        DO I = 1, NLAGF_L
          N = LLAGF(I)
          INDEXLAG(NODGLOB(N)) = I
          AG(1,I) = A(1,N)
          AG(2,I) = A(2,N)
          AG(3,I) = A(3,N)
          MSG(I)  = MS(N)
          VG(1,I) = V(1,N)
          VG(2,I) = V(2,N)
          VG(3,I) = V(3,N)
        END DO
      END IF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_SG_MULT                  source/mpi/lag_multipliers/spmd_lag.F
Chd|-- called by -----------
Chd|        LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_SG_MULT(
     1     A      ,AR     ,AG     ,ARG     ,FR_LAGF,
     2     ISIZ   ,NBNODL ,LLAGF  ,NLAGF_L )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FR_LAGF(3,*), LLAGF(*),
     .        NBNODL, NLAGF_L, ISIZ
      my_real
     .        A(3,*), AR(3,*),AG(3,*), ARG(3,*)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,N
C         
      IF(IRODDL.NE.0)THEN
        DO I = 1, NLAGF_L
          N = LLAGF(I)
          A(1,N)  = AG(1,I) 
          A(2,N)  = AG(2,I) 
          A(3,N)  = AG(3,I) 
          AR(1,N) = ARG(1,I)
          AR(2,N) = ARG(2,I)
          AR(3,N) = ARG(3,I)
        END DO
      ELSE
        DO I = 1, NLAGF_L
          N = LLAGF(I)
          A(1,N)  = AG(1,I)
          A(2,N)  = AG(2,I)
          A(3,N)  = AG(3,I)
        END DO
      END IF
C
      RETURN
      END
C
Chd|====================================================================
Chd|  SPMD_EXCH_MULT                source/mpi/lag_multipliers/spmd_lag.F
Chd|-- called by -----------
Chd|        LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPMD_EXCH_MULT(
     1    A       ,AR     ,LLAGF , NLAGF_L, FR_LAGF,
     2    IAD_ELEM,FR_ELEM,LRBUF , ISIZ   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
#include      "scr01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NLAGF_L,LRBUF,ISIZ,
     .        FR_LAGF(3,*),LLAGF(*),IAD_ELEM(2,*),FR_ELEM(*)
      my_real
     .        A(3,*), AR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      RETURN
      END
#endif
