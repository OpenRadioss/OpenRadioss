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
Chd|  LAG_MPC                       source/tools/lagmul/lag_mpc.F 
Chd|-- called by -----------
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LAG_MPC(
     1   RBMPC     ,IMPCNC    ,IMPCNN    ,IMPCDL    ,IMPCSK    ,
     2   SKEW      ,IADLL     ,LLL       ,JLL       ,SLL       ,
     3   XLL       ,COMNTAG   ,ICFTAG    ,JCFTAG    ,MS        ,
     4   IN        ,V         ,VR        ,A         ,AR        ,
     5   ISKIP     ,NCF_S     ,NC        )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC, ISKIP, NCF_S,
     .        IMPCNC(*),IMPCNN(*),IMPCDL(*),IMPCSK(*),LLL(*),JLL(*),
     .        SLL(*),IADLL(*),COMNTAG(*),ICFTAG(*),JCFTAG(*)
C     REAL ou REAL*8
      my_real
     .  XLL(*),SKEW(LSKEW,*),RBMPC(*),MS(*),IN(*),V(3,*),VR(3,*),
     .  A(3,*),AR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, JJ, LL, IK, NK, KF, ISK, NN, NDL, IDL, NUMC, IC
      my_real COEF,HH,R,S1,S2,S3
C-----------------------------------------------
C        NC : nombre de condition cinematique
C        IC : numero de la condition cinematique (1,NC)
C        IK :
C        I  : numero global du noeud (1,NUMNOD)
C        J  : direction 1,2,3,4,5,6
C------
C        IADLL(NC)        : IAD = IADLL(IC)
C        IK = IAD,IAD+1,IAD+2,...
C        LLL(LAG_NKF)  : I = LLL(IK)
C        JLL(LAG_NKF)  : J = JLL(IK)
C======================================================================|
      KF = 0
      DO I=1,NUMMPC
        HH = ZERO
        R  = ZERO
        NC = NC + 1
        IK = IADLL(NC)-1
        NUMC = IMPCNC(I)
        NK = 0
        DO J=1,NUMC
          KF  = KF+1
          NN  = IMPCNN(KF)
          NDL = IMPCDL(KF)
          ISK = IMPCSK(KF)
          COEF= RBMPC(KF)
C---
          IF(ISK.EQ.1)THEN
            NK = NK + 1
            IK = IK + 1
            LLL(IK) = NN
            JLL(IK) = NDL
            SLL(IK) = 0
            XLL(IK) = COEF
            IF (NDL.GT.3) THEN
              NDL = NDL - 3
              HH = HH + COEF*COEF / IN(NN)
              R  = R  + COEF*(VR(NDL,NN) / DT12 + AR(NDL,NN))
            ELSE
              HH = HH + COEF*COEF / MS(NN)
              R  = R  + COEF*(V(NDL,NN)  / DT12 + A(NDL,NN))
            ENDIF
          ELSE
            IDL = NDL
            IF (NDL.GT.3) IDL = NDL - 3
            IF (IDL.EQ.1) THEN
              S1 = COEF*SKEW(1,ISK)
              S2 = COEF*SKEW(2,ISK)
              S3 = COEF*SKEW(3,ISK)
            ELSEIF (IDL.EQ.2) THEN
              S1 = COEF*SKEW(4,ISK)
              S2 = COEF*SKEW(5,ISK)
              S3 = COEF*SKEW(6,ISK)
            ELSEIF (IDL.EQ.3) THEN
              S1 = COEF*SKEW(7,ISK)
              S2 = COEF*SKEW(8,ISK)
              S3 = COEF*SKEW(9,ISK)
            ENDIF
            NK = NK + 3
            IF (NDL.GT.3) THEN
              IK = IK + 1
              LLL(IK) = NN
              JLL(IK) = 4
              SLL(IK) = 0
              XLL(IK) = S1
              IK = IK + 1
              LLL(IK) = NN
              JLL(IK) = 5
              SLL(IK) = 0
              XLL(IK) = S2
              IK = IK + 1
              LLL(IK) = NN
              JLL(IK) = 6
              SLL(IK) = 0
              XLL(IK) = S3
              HH = HH + (S1*S1 + S2*S2 + S3*S3) / IN(NN)
              R  = R  + S1*(VR(1,NN) / DT12 + AR(1,NN))
     .                + S2*(VR(2,NN) / DT12 + AR(2,NN))
     .                + S3*(VR(3,NN) / DT12 + AR(3,NN))
            ELSE
              IK = IK + 1
              LLL(IK) = NN
              JLL(IK) = 1
              SLL(IK) = 0
              XLL(IK) = S1
              IK = IK + 1
              LLL(IK) = NN
              JLL(IK) = 2
              SLL(IK) = 0
              XLL(IK) = S2
              IK = IK + 1
              LLL(IK) = NN
              JLL(IK) = 3
              SLL(IK) = 0
              XLL(IK) = S3
              HH = HH + (S1*S1 + S2*S2 + S3*S3) / MS(NN)
              R  = R  + S1*(V(1,NN) / DT12 + A(1,NN))
     .                + S2*(V(2,NN) / DT12 + A(2,NN))
     .                + S3*(V(3,NN) / DT12 + A(3,NN))
            ENDIF
          ENDIF
        ENDDO
        IADLL(NC+1) = IADLL(NC) + NK
C
C---    Solving local Lagrange multipliers
        IF (HH.NE.ZERO) R = R / HH
        DO IK=IADLL(NC),IADLL(NC+1)-1
          JJ = JLL(IK)
          LL = LLL(IK)
          IF (JJ.GT.3) THEN
            JJ = JJ - 3
            AR(JJ,LL) = AR(JJ,LL) - XLL(IK)*R / IN(LL)
          ELSE
            A(JJ,LL)  = A(JJ,LL)  - XLL(IK)*R / MS(LL)
          ENDIF
        ENDDO
        IF (COMNTAG(NN).EQ.1) THEN
          ISKIP = ISKIP + 1
          NC = NC - 1
        ELSE
          IC = NC - NCF_S
          ICFTAG(IC) = IC + ISKIP
          JCFTAG(IC+ISKIP) = NC
        ENDIF
      ENDDO
C---
      RETURN
      END
C
C nouvelle routine MPC SPMD
Chd|====================================================================
Chd|  LAG_MPCP                      source/tools/lagmul/lag_mpc.F 
Chd|-- called by -----------
Chd|        LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE LAG_MPCP(
     1   RBMPC     ,IMPCNC    ,IMPCNN    ,IMPCDL    ,IMPCSK    ,
     2   SKEW      ,LAGCOMC   ,LAGCOMK   ,NC        ,IK        )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC,  IK,
     .        IMPCNC(*),IMPCNN(*),IMPCDL(*),IMPCSK(*)
C     REAL ou REAL*8
      my_real
     .  LAGCOMK(4,*),LAGCOMC(2,*),
     .  SKEW(LSKEW,*),RBMPC(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, JJ, LL, NK, KF, ISK, NN, NDL, IDL, NUMC, IC
      my_real COEF,HH,R,S1,S2,S3
C-----------------------------------------------
C        NC : nombre de condition cinematique
C        IC : numero de la condition cinematique (1,NC)
C        IK :
C        I  : numero global du noeud (1,NUMNOD)
C        J  : direction 1,2,3,4,5,6
C------
C        BLL => LAGCOMC(2)
C        IADLL => LAGCOMC(1)
C        LLL => LAGCOMK(1)
C        JLL => LAGCOMK(2)
C        SLL => LAGCOMK(3)
C        XLL => LAGCOMK(4)
C======================================================================|
      KF = 0
      DO I=1,NUMMPC
        NC = NC + 1
        NUMC = IMPCNC(I)
        NK = 0
        DO J=1,NUMC
          KF  = KF+1
          NN  = IMPCNN(KF)
          NDL = IMPCDL(KF)
          ISK = IMPCSK(KF)
          COEF= RBMPC(KF)
C---
          IF(ISK.EQ.1)THEN
            NK = NK + 1
            IK = IK + 1
            LAGCOMK(1,IK) = NN
            LAGCOMK(2,IK) = NDL
            LAGCOMK(3,IK) = 0
            LAGCOMK(4,IK) = COEF
          ELSE
            NK = NK + 3
            IF (NDL.EQ.1) THEN
              IK = IK + 1
              LAGCOMK(1,IK) = NN
              LAGCOMK(2,IK) = 1
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = COEF*SKEW(1,ISK)
              IK = IK + 1
              LAGCOMK(1,IK) = NN
              LAGCOMK(2,IK) = 2
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = COEF*SKEW(2,ISK)
              IK = IK + 1
              LAGCOMK(1,IK) = NN
              LAGCOMK(2,IK) = 3
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = COEF*SKEW(3,ISK)
            ELSEIF (NDL.EQ.2) THEN
              IK = IK + 1
              LAGCOMK(1,IK) = NN
              LAGCOMK(2,IK) = 1
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = COEF*SKEW(4,ISK)
              IK = IK + 1
              LAGCOMK(1,IK) = NN
              LAGCOMK(2,IK) = 2
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = COEF*SKEW(5,ISK)
              IK = IK + 1
              LAGCOMK(1,IK) = NN
              LAGCOMK(2,IK) = 3
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = COEF*SKEW(6,ISK)
            ELSEIF (NDL.EQ.3) THEN
              IK = IK + 1
              LAGCOMK(1,IK) = NN
              LAGCOMK(2,IK) = 1
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = COEF*SKEW(7,ISK)
              IK = IK + 1
              LAGCOMK(1,IK) = NN
              LAGCOMK(2,IK) = 2
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = COEF*SKEW(8,ISK)
              IK = IK + 1
              LAGCOMK(1,IK) = NN
              LAGCOMK(2,IK) = 3
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = COEF*SKEW(9,ISK)
            ELSEIF (NDL.EQ.4) THEN
              IK = IK + 1
              LAGCOMK(1,IK) = NN
              LAGCOMK(2,IK) = 4
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = COEF*SKEW(1,ISK)
              IK = IK + 1
              LAGCOMK(1,IK) = NN
              LAGCOMK(2,IK) = 5
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = COEF*SKEW(2,ISK)
              IK = IK + 1
              LAGCOMK(1,IK) = NN
              LAGCOMK(2,IK) = 6
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = COEF*SKEW(3,ISK)
            ELSEIF (NDL.EQ.5) THEN
              IK = IK + 1
              LAGCOMK(1,IK) = NN
              LAGCOMK(2,IK) = 4
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = COEF*SKEW(4,ISK)
              IK = IK + 1
              LAGCOMK(1,IK) = NN
              LAGCOMK(2,IK) = 5
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = COEF*SKEW(5,ISK)
              IK = IK + 1
              LAGCOMK(1,IK) = NN
              LAGCOMK(2,IK) = 6
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = COEF*SKEW(6,ISK)
            ELSEIF (NDL.EQ.6) THEN
              IK = IK + 1
              LAGCOMK(1,IK) = NN
              LAGCOMK(2,IK) = 4
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = COEF*SKEW(7,ISK)
              IK = IK + 1
              LAGCOMK(1,IK) = NN
              LAGCOMK(2,IK) = 5
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = COEF*SKEW(8,ISK)
              IK = IK + 1
              LAGCOMK(1,IK) = NN
              LAGCOMK(2,IK) = 6
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = COEF*SKEW(9,ISK)
            ENDIF
          ENDIF
        ENDDO
        LAGCOMC(1,NC)=NK
        LAGCOMC(2,NC)=ZERO   ! a verifier
C voir si utile
c        IF (COMNTAG(NN).EQ.1) THEN
c          ISKIP = ISKIP + 1
c          NC = NC - 1
c        ELSE
c          IC = NC - NCF_S
c          ICFTAG(IC) = IC + ISKIP
c          JCFTAG(IC+ISKIP) = NC
c        ENDIF
      ENDDO
C---
      RETURN
      END
