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
Chd|  LAG_FXV                       source/tools/lagmul/lag_fxv.F 
Chd|-- called by -----------
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        FINTER_SMOOTH                 source/tools/curve/finter_smooth.F
Chd|====================================================================
      SUBROUTINE LAG_FXV(IBFV   ,VEL    ,SKEW   ,NPF    ,TF     ,
     2                   BLL    ,IADLL  ,LLL    ,JLL    ,SLL    ,
     3                   XLL    ,COMNTAG,ICFTAG ,JCFTAG ,MS     ,
     4                   IN     ,V      ,VR     ,A      ,AR     ,
     5                   ISKIP  ,NCF_S  ,NC     ,PYTHON)

      USE PYTHON_FUNCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC, ISKIP, NCF_S,
     .        LLL(*),JLL(*),SLL(*),IADLL(*),IBFV(NIFV,*),NPF(*),
     .        COMNTAG(*),ICFTAG(*),JCFTAG(*)
      my_real
     .  XLL(*),BLL(*),SKEW(LSKEW,*),VEL(LFXVELR,*),TF(*),MS(*),IN(*),
     .  V(3,*),VR(3,*),A(3,*),AR(3,*)
      TYPE(PYTHON_), INTENT(INOUT) :: PYTHON
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IC, N, K1, K2, K3, IK, ISK, IFUN, NNO, ISMOOTH
      my_real
     .  TS, DERI, VF, FAC, FACX, FINTER, FINTER_SMOOTH
      EXTERNAL FINTER, FINTER_SMOOTH
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
      DO N=1,NFXVEL
        IF (IBFV(8,N)/=0) THEN
          FACX   = VEL(5,N)
          TS = (TT + HALF*DT2)*FACX
          NNO = IABS(IBFV(1,N))
          ISK = IBFV(2,N)/10
          IFUN= IBFV(3,N)
          FAC = VEL(1,N)
          J=IBFV(2,N)-10*ISK
!!          VF = FINTER(IFUN, TS, NPF, TF, DERI)
          ISMOOTH = 0
          IF (IFUN > 0) ISMOOTH = NPF(2*NFUNCT+IFUN+1)
          IF (ISMOOTH == 0) THEN
            VF = FINTER(IFUN, TS, NPF, TF, DERI)
          ELSE IF (ISMOOTH > 0) THEN
            VF = FINTER_SMOOTH(IFUN, TS, NPF, TF, DERI)
          ELSE
            CALL PYTHON_CALL_FUNCT1D(PYTHON, -ISMOOTH, TS, VF) 
          ENDIF ! IF (ISMOOTH == 0)
          NC = NC + 1
          BLL(NC) = -VF*FAC / DT12
          IF (ISK<=1) THEN
            IADLL(NC+1)=IADLL(NC) + 1
            IK = IADLL(NC)
            LLL(IK) = NNO
            JLL(IK) = J
            SLL(IK) = 0
            XLL(IK) = ONE
          ELSE
            IF(J<=3)THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              IADLL(NC+1)=IADLL(NC) + 3
              IK = IADLL(NC)
              LLL(IK) = NNO
              JLL(IK) = 1
              SLL(IK) = 0
              XLL(IK) = SKEW(K1,ISK)
              IK = IK + 1
              LLL(IK) = NNO
              JLL(IK) = 2
              SLL(IK) = 0
              XLL(IK) = SKEW(K2,ISK)
              IK = IK + 1
              LLL(IK) = NNO
              JLL(IK) = 3
              SLL(IK) = 0
              XLL(IK) = SKEW(K3,ISK)
            ELSE
              J = J - 3
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              IK = IADLL(NC)
              LLL(IK) = NNO
              JLL(IK) = 4
              SLL(IK) = 0
              XLL(IK) = SKEW(K1,ISK)
              IK = IK + 1
              LLL(IK) = NNO
              JLL(IK) = 5
              SLL(IK) = 0
              XLL(IK) = SKEW(K2,ISK)
              IK = IK + 1
              LLL(IK) = NNO
              JLL(IK) = 6
              SLL(IK) = 0
              XLL(IK) = SKEW(K3,ISK)
            ENDIF
          ENDIF
          IC = NC - NCF_S
          ICFTAG(IC) = IC + ISKIP
          JCFTAG(IC+ISKIP) = NC
        ENDIF
      ENDDO
C---
      RETURN
      END
C
Chd|====================================================================
Chd|  LAG_FXVP                      source/tools/lagmul/lag_fxv.F 
Chd|-- called by -----------
Chd|        LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|        FINTER                        source/tools/curve/finter.F   
Chd|        FINTER_SMOOTH                 source/tools/curve/finter_smooth.F
Chd|====================================================================
      SUBROUTINE LAG_FXVP(IBFV   ,VEL    ,SKEW   ,NPF    ,TF     ,
     2                    LAGCOMC,LAGCOMK,NC     ,NODGLOB,WEIGHT ,
     3                    IK     ,PYTHON)
      USE PYTHON_FUNCT_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC, IK,
     .        IBFV(NIFV,*),NPF(*), NODGLOB(*), WEIGHT(*)
      my_real
     .        LAGCOMK(4,*),LAGCOMC(2,*),SKEW(LSKEW,*),VEL(LFXVELR,*),
     .        TF(*)
      TYPE(PYTHON_), INTENT(INOUT) :: PYTHON
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IC, N, K1, K2, K3, ISK, IFUN, NNO,ISMOOTH
      my_real
     .        TS, DERI, VF, FAC, FACX, FINTER, FINTER_SMOOTH
      EXTERNAL FINTER, FINTER_SMOOTH
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
      DO N=1,NFXVEL
        IF (IBFV(8,N)/=0) THEN
         NNO = IABS(IBFV(1,N))
         IF(WEIGHT(NNO)==1) THEN  ! une seule fois en SPMD
C   numerotation globale
          FACX   = VEL(5,N)
          TS = (TT + HALF*DT2)*FACX
          NNO = NODGLOB(NNO)
          ISK = IBFV(2,N)/10
          IFUN= IBFV(3,N)
          FAC = VEL(1,N)
          J=IBFV(2,N)-10*ISK
          ISMOOTH = 0
          IF (IFUN > 0) ISMOOTH = NPF(2*NFUNCT+IFUN+1)
!!          VF = FINTER(IFUN, TS, NPF, TF, DERI)
          IF (ISMOOTH == 0) THEN
            VF = FINTER(IFUN, TS, NPF, TF, DERI)
          ELSE IF(ISMOOTH > 0) THEN
            VF = FINTER_SMOOTH(IFUN, TS, NPF, TF, DERI)
          ELSE
            CALL PYTHON_CALL_FUNCT1D(PYTHON, -ISMOOTH, TS, VF) 
          ENDIF
          NC = NC + 1
          LAGCOMC(2,NC) = -VF*FAC / DT12
          IF (ISK<=1) THEN
            LAGCOMC(1,NC)=1
            IK = IK+1
            LAGCOMK(1,IK) = NNO
            LAGCOMK(2,IK) = J
            LAGCOMK(3,IK) = 0
            LAGCOMK(4,IK) = ONE
          ELSE
            IF(J<=3)THEN
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              LAGCOMC(1,NC)=3
              IK = IK+1
              LAGCOMK(1,IK) = NNO
              LAGCOMK(2,IK) = 1
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = SKEW(K1,ISK)
              IK = IK + 1
              LAGCOMK(1,IK) = NNO
              LAGCOMK(2,IK) = 2
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = SKEW(K2,ISK)
              IK = IK + 1
              LAGCOMK(1,IK) = NNO
              LAGCOMK(2,IK) = 3
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = SKEW(K3,ISK)
            ELSE
              J = J - 3
              K1=3*J-2
              K2=3*J-1
              K3=3*J
              IK = IK+1
              LAGCOMK(1,IK) = NNO
              LAGCOMK(2,IK) = 4
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = SKEW(K1,ISK)
              IK = IK + 1
              LAGCOMK(1,IK) = NNO
              LAGCOMK(2,IK) = 5
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = SKEW(K2,ISK)
              IK = IK + 1
              LAGCOMK(1,IK) = NNO
              LAGCOMK(2,IK) = 6
              LAGCOMK(3,IK) = 0
              LAGCOMK(4,IK) = SKEW(K3,ISK)
            ENDIF
          ENDIF
C          IC = NC - NCF_S
C          ICFTAG(IC) = IC + ISKIP
C          JCFTAG(IC+ISKIP) = NC
         ENDIF
        ENDIF
      ENDDO
C---
      RETURN
      END
