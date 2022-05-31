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
Chd|====================================================================
Chd|  NEWSKW                        source/tools/skew/newskw.F    
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|        SPMD_SD_SKW                   source/mpi/output/spmd_sd_skw.F
Chd|====================================================================
      SUBROUTINE NEWSKW(SKEW    ,ISKWN        ,X           ,ISKWP_L ,NSKWP,
     1                  NUMSKW_L,NUMSKW_L_SEND,ISKWP_L_SEND,RECVCOUNT,ISKWP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISKWN(LISKN,*), NSKWP(*),ISKWP(*)
      INTEGER, DIMENSION(NUMSKW), INTENT(IN) :: ISKWP_L,ISKWP_L_SEND
      INTEGER, DIMENSION(NSPMD), INTENT(IN) :: RECVCOUNT
      INTEGER, INTENT(IN) :: NUMSKW_L,NUMSKW_L_SEND
C     REAL ou REAL*8
      my_real
     .   SKEW(LSKEW,*), X(3,*)
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       ISKWN : integer ; dimension=LISKN*number of skew
!               SKEW property array
!       NSKWP : integer ; dimension = NSPMD
!               number of skew per processor
!       ISKWP : integer ; dimension=NUMSKW+1
!               gives the ID processir of the current i SKEW
!               ISKWP < 0 --> the SKEW is local on a processor
!               and we don't need to communicate the data
!               ISKWP > 0 --> the SKEW is global and the data must be
!               communicated
!       NUMSKW_L : integer 
!                  number of local SKEW
!       ISKWP_L : integer ; dimension=NUMSKW_L_SEND
!                 index of local SKEW
!       NUMSKW_L_SEND : integer 
!                       number of sent SKEW 
!       ISKWP_L_SEND : integer ; dimension=NUMSKW_L_SEND
!                      index of sent SKEW
!       RECVCOUNT : integer ; dimension=NSPMD
!                   number of received SKEW
!       SKEW : real ; dimension=LISKN*number of skew
!              SKEW property array
!       X : real ; dimension=3*NUMNOD
!           position
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, N1, N2, N3, K, I, J, LOC_PROC,IMOV,IDIR,NN
C     REAL ou REAL*8
      my_real
     .   P(12), PP1, PP3, PP2
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
!$COMMENT
!       NEWSKW description
!       compute the SKEW and send it if required
!       
!       NEWSKW organization :
!       - compute SKEW
!       - send SKEW
!$ENDCOMMENT
      LOC_PROC = ISPMD+1
      DO NN=1,NUMSKW_L
        N = ISKWP_L(NN)
!        do n=1,NUMSKW
!        IF(abs(ISKWP(N+1)).EQ.LOC_PROC)THEN

C processeur concerne par le skew
          N1=ISKWN(1,N+1)
          N2=ISKWN(2,N+1)
          N3=ISKWN(3,N+1)
          IMOV=ISKWN(5,N+1)
          IDIR=ISKWN(6,N+1)
C----------------
C     SKEW MOBILE
C----------------
          IF (N1+N2+N3.NE.0) THEN
C
            IF(IMOV == 1)THEN
              IF(N2D.EQ.0)THEN
C-----------------
C     CALCUL DE X'(IDIR=1) Y'(IDIR=2) Z'(IDIR=3)
C-----------------
                IF (IDIR == 1)THEN
                  P(1)=X(1,N2)-X(1,N1)
                  P(2)=X(2,N2)-X(2,N1)
                  P(3)=X(3,N2)-X(3,N1)
                ELSEIF (IDIR == 2)THEN
                  P(4)=X(1,N2)-X(1,N1)
                  P(5)=X(2,N2)-X(2,N1)
                  P(6)=X(3,N2)-X(3,N1)
                ELSEIF (IDIR == 3)THEN
                  P(7)=X(1,N2)-X(1,N1)
                  P(8)=X(2,N2)-X(2,N1)
                  P(9)=X(3,N2)-X(3,N1)
                ENDIF
C-----------------
C     CALCUL DE Y0'(IDIR=1) Z0'(IDIR=2) X0'(IDIR=3)
C-----------------
                IF (IDIR == 1)THEN
                  P(4)=X(1,N3)-X(1,N1)
                  P(5)=X(2,N3)-X(2,N1)
                  P(6)=X(3,N3)-X(3,N1)
                ELSEIF (IDIR == 2)THEN
                  P(7)=X(1,N3)-X(1,N1)
                  P(8)=X(2,N3)-X(2,N1)
                  P(9)=X(3,N3)-X(3,N1)
                ELSEIF (IDIR == 3)THEN
                  P(1)=X(1,N3)-X(1,N1)
                  P(2)=X(2,N3)-X(2,N1)
                  P(3)=X(3,N3)-X(3,N1)
                ENDIF
              ELSE
                P(1)=ONE
                P(2)=ZERO
                P(3)=ZERO
                P(4)=X(1,N2)-X(1,N1)
                P(5)=X(2,N2)-X(2,N1)
                P(6)=X(3,N2)-X(3,N1)
              ENDIF
C------------------
C      SI X'=0
C      => X'=X(IDIR=1)
C      SI Y'=0
C      => Y'=Y(IDIR=2)
C      SI Z'=0
C      => Z'=Z(IDIR=3)
C------------------
              IF (IDIR == 1) THEN
                PP1=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
                IF(PP1.EQ.ZERO)THEN
                  P(1)=ONE
                  P(2)=ZERO
                  P(3)=ZERO
                  PP1 =ONE
                ENDIF
              ELSE IF (IDIR == 2)THEN 
                PP2=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
                IF(PP2.EQ.ZERO)THEN
                  P(4)=ZERO
                  P(5)=ONE
                  P(6)=ZERO
                  PP2 =ONE
                ENDIF
              ELSE IF (IDIR == 3)THEN
                PP3=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
                IF(PP3.EQ.ZERO)THEN
                  P(7)=ZERO
                  P(8)=ZERO
                  P(9)=ONE
                  PP3 =ONE
                ENDIF
              ENDIF
C-----------------
C     CALCUL DE Z'(IDIR=1) X'(IDIR=2) Y'(IDIR=3)
C-----------------
              IF (IDIR == 1)THEN
                P(7)=P(2)*P(6)-P(3)*P(5)
                P(8)=P(3)*P(4)-P(1)*P(6)
                P(9)=P(1)*P(5)-P(2)*P(4)
                PP3=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
              ELSEIF (IDIR == 2)THEN
                P(1)=P(5)*P(9)-P(6)*P(8)
                P(2)=P(6)*P(7)-P(4)*P(9)
                P(3)=P(4)*P(8)-P(5)*P(7)
                PP1=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
              ELSEIF (IDIR == 3)THEN
                P(4)=P(8)*P(3)-P(9)*P(2)
                P(5)=P(9)*P(1)-P(7)*P(3)
                P(6)=P(7)*P(2)-P(8)*P(1)
                PP2=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
              ENDIF
C------------------
C      SI Z'=0 (Y0'=X')
C      => Y0' != X'(IDIR=1)
C      SI X'=0 (Z0'=Y')
C      => Z0' != Y'(IDIR=2)
C      SI X'=0 (Z0'=Y')
C      => Z0' != Z'(IDIR=3)
C------------------
              IF (IDIR == 1) THEN
                IF(PP3.EQ.ZERO)THEN
                  IF(P(1).EQ.ZERO)THEN
                    P(4)=PP1
                    P(5)=P(2)
                  ELSE
                    P(4)=P(1)
                    P(5)=ABS(P(2))+PP1
                  ENDIF
                  P(6)=P(3)
                  P(7)=P(2)*P(6)-P(3)*P(5)
                  P(8)=P(3)*P(4)-P(1)*P(6)
                  P(9)=P(1)*P(5)-P(2)*P(4)
                  PP3=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
                ENDIF
              ELSEIF (IDIR == 2) THEN
                IF(PP1.EQ.ZERO)THEN
                  IF(P(4).EQ.ZERO)THEN
                    P(7)=PP2
                    P(8)=P(5)
                  ELSE
                    P(7)=P(4)
                    P(8)=ABS(P(5))+PP2
                  ENDIF
                  P(9)=P(6)
                  P(1)=P(5)*P(9)-P(6)*P(8)
                  P(2)=P(6)*P(7)-P(4)*P(9)
                  P(3)=P(4)*P(8)-P(5)*P(7)
                  PP1=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
                ENDIF
              ELSEIF (IDIR == 3) THEN
                IF(PP2.EQ.ZERO)THEN
                  IF(P(7).EQ.ZERO)THEN
                    P(1)=PP3
                    P(2)=P(8)
                  ELSE
                    P(1)=P(7)
                    P(2)=ABS(P(8))+PP3
                  ENDIF
                  P(3)=P(9)
                  P(4)=P(8)*P(3)-P(9)*P(2)
                  P(5)=P(9)*P(1)-P(7)*P(3)
                  P(6)=P(7)*P(2)-P(8)*P(1)
                  PP2=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
                ENDIF
              ENDIF
C-----------------
C     CALCUL DE Y'(IDIR=1) Z'(IDIR=2) X'(IDIR=3)
C-----------------
              IF (IDIR == 1) THEN
                P(4)=P(8)*P(3)-P(9)*P(2)
                P(5)=P(9)*P(1)-P(7)*P(3)
                P(6)=P(7)*P(2)-P(8)*P(1)
                PP2=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
              ELSEIF (IDIR == 2) THEN
                P(7)=P(2)*P(6)-P(3)*P(5)
                P(8)=P(3)*P(4)-P(1)*P(6)
                P(9)=P(1)*P(5)-P(2)*P(4)
                PP3=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
              ELSEIF (IDIR == 3) THEN
                P(1)=P(5)*P(9)-P(6)*P(8)
                P(2)=P(6)*P(7)-P(4)*P(9)
                P(3)=P(4)*P(8)-P(5)*P(7)
                PP1=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
              ENDIF
C
            ELSEIF(IMOV == 2)THEN
C-----------------
C     CALCUL DE X0'
C-----------------
              P(1)=X(1,N3)-X(1,N1)
              P(2)=X(2,N3)-X(2,N1)
              P(3)=X(3,N3)-X(3,N1)
C-----------------
C     CALCUL DE Z'
C-----------------
              P(7)=X(1,N2)-X(1,N1)
              P(8)=X(2,N2)-X(2,N1)
              P(9)=X(3,N2)-X(3,N1)
C------------------
C      SI Z'=0
C      => Z'=Z
C------------------
              PP3=SQRT(P(7)*P(7)+P(8)*P(8)+P(9)*P(9))
              IF(PP3.EQ.ZERO)THEN
                P(7)=ZERO
                P(8)=ZERO
                P(9)=ONE
                PP3 =ONE
              ENDIF
C-----------------
C       CALCUL DE Y' = Z' x X0'
C-----------------
              P(4)=P(8)*P(3)-P(9)*P(2)
              P(5)=P(9)*P(1)-P(7)*P(3)
              P(6)=P(7)*P(2)-P(8)*P(1)
              PP2=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
C------------------
C      SI Y'=0 (X0'=Z')
C      => X0' != Y'
C------------------
              IF(PP2.EQ.ZERO)THEN
                IF(P(7).EQ.ZERO)THEN
                  P(1)=PP3
                  P(2)=P(8)
                ELSE
                  P(1)=P(7)
                  P(2)=ABS(P(8))+PP3
                ENDIF
                P(3)=P(9)
                P(4)=P(8)*P(3)-P(9)*P(2)
                P(5)=P(9)*P(1)-P(7)*P(3)
                P(6)=P(7)*P(2)-P(8)*P(1)
                PP2=SQRT(P(4)*P(4)+P(5)*P(5)+P(6)*P(6))
              ENDIF
C-----------------
C       CALCUL DE X' = Y' x Z'
C-----------------
              P(1)=P(5)*P(9)-P(6)*P(8)
              P(2)=P(6)*P(7)-P(4)*P(9)
              P(3)=P(4)*P(8)-P(5)*P(7)
              PP1=SQRT(P(1)*P(1)+P(2)*P(2)+P(3)*P(3))
C
            END IF
C-----------
C     NORME
C-----------
            P(1)=P(1)/PP1
            P(2)=P(2)/PP1
            P(3)=P(3)/PP1
            P(4)=P(4)/PP2
            P(5)=P(5)/PP2
            P(6)=P(6)/PP2
            P(7)=P(7)/PP3
            P(8)=P(8)/PP3
            P(9)=P(9)/PP3
C-----------------
C     ORIGINE
C-----------------
            P(10) = X(1,N1)
            P(11) = X(2,N1)
            P(12) = X(3,N1)
C
            DO K=1,12
              SKEW(K,N+1)=P(K)
            END DO
C
!          ENDIF
        ENDIF
      END DO
C
      IF(NSPMD > 1) THEN
         CALL SPMD_SD_SKW(SKEW,ISKWP_L_SEND,NUMSKW_L_SEND,RECVCOUNT)
      END IF
C
      RETURN
      END SUBROUTINE NEWSKW
Chd|====================================================================
Chd|  NEWSKW_INIT                   source/tools/skew/newskw.F    
Chd|-- called by -----------
Chd|        RESOL                         source/engine/resol.F         
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NEWSKW_INIT(ISKWP ,NUMSKW_L,NSKWP,NUMSKW_L_SEND,ISKWP_L_SEND,RECVCOUNT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  ISKWP(*)
      INTEGER, DIMENSION(NSPMD), INTENT(IN) :: NSKWP
      INTEGER, DIMENSION(NSPMD), INTENT(INOUT) :: RECVCOUNT
      INTEGER, DIMENSION(NUMSKW), INTENT(INOUT) :: ISKWP_L_SEND
      INTEGER, INTENT(OUT) :: NUMSKW_L,NUMSKW_L_SEND
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       NSKWP : integer ; dimension = NSPMD
!               number of skew per processor
!       ISKWP : integer ; dimension=NUMSKW+1
!               gives the ID processir of the current i SKEW
!               ISKWP < 0 --> the SKEW is local on a processor
!               and we don't need to communicate the data
!               ISKWP > 0 --> the SKEW is global and the data must be
!               communicated
!       NUMSKW_L : integer 
!                  number of local SKEW
!       NUMSKW_L_SEND : integer 
!                       number of sent SKEW 
!       ISKWP_L_SEND : integer ; dimension=NUMSKW_L_SEND
!                      index of sent SKEW
!       RECVCOUNT : integer ; dimension=NSPMD
!                   number of received SKEW
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
C     REAL ou REAL*8
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "param_c.inc"
#include      "lagmult.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N, N1, N2, N3, K, I, J, LOC_PROC,IMOV,IDIR,NN,NN2
C     REAL ou REAL*8
      my_real
     .   P(12), PP1, PP3, PP2
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
!$COMMENT
!       NEWSKW_INIT description
!       initialization of the sent index array ISKWP_L_SEND
!       and the number of recevied SKEW value (for the comm)
!       
!       NEWSKW_INIT organization :
!       - initialize the ISKWP_L_SEND index array :
!         if ISKWP(N+A) = local proc, N+1 must be sent
!         if ISKWP(N+A) = -local proc, N+1 is local to 
!         the processor and the comm is not mandatory 
!         if LAG MULT method is used, need to always 
!         communicate the SKEW arraies
!$ENDCOMMENT

!       initialization
        LOC_PROC = ISPMD+1
        NUMSKW_L = 0
        NUMSKW_L_SEND = 0
        NN = 0
        NN2 = 0
        RECVCOUNT(1:NSPMD) = 0

!       loop over numskw : if ISKWP = local proc --> need to communicate
        DO N=1,NUMSKW
                !       LAG MULT method
                IF(LAG_NCF+LAG_NCL>0) THEN
                        IF(ABS(ISKWP(N+1)).EQ.LOC_PROC)THEN
                                NN2 = NN2 + 1
                                ISKWP_L_SEND(NN2) = N
                        ENDIF
                        IF(ISKWP(N+1)/=0) RECVCOUNT(ABS(ISKWP(N+1))) = RECVCOUNT(ABS(ISKWP(N+1))) + 10
                ELSE
                !       other
                        IF(ISKWP(N+1).EQ.LOC_PROC)THEN
                                NN2 = NN2 + 1
                                ISKWP_L_SEND(NN2) = N
                        ENDIF
                        IF(ISKWP(N+1)>0) RECVCOUNT(ISKWP(N+1)) = RECVCOUNT(ISKWP(N+1)) + 10
                ENDIF
        ENDDO
        IF(NUMSKW>0) THEN
                NUMSKW_L = NSKWP(ISPMD+1)
                NUMSKW_L_SEND = NN2    
        ELSE
                NUMSKW_L = 0
                NUMSKW_L_SEND = 0
        ENDIF
C
        RETURN
        END SUBROUTINE NEWSKW_INIT
