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
Chd|  LAG_RBY                       source/tools/lagmul/lag_rby.F 
Chd|-- called by -----------
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|-- calls ---------------
Chd|        LAG_RBY_COND                  source/tools/lagmul/lag_rby_cond.F
Chd|====================================================================
      SUBROUTINE LAG_RBY(RBYL   ,NPBYL  ,LPBYL  ,MASS   ,INER   ,
     2                   IADLL  ,LLL    ,JLL    ,SLL    ,XLL    ,
     3                   COMNTAG,V      ,VR     ,A      ,AR     ,
     4                   X      ,NC     ,NCR    )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "lagmult.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC, NCR, LLL(*),JLL(*),SLL(*),IADLL(*), 
     .        NPBYL(NNPBY,*), LPBYL(*), COMNTAG(*)
C     REAL
      my_real
     .  RBYL(NRBY,*),XLL(*),X(3,*),V(3,*),VR(3,*),A(3,*),AR(3,*),
     .  MASS(*),INER(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JF,N,NN,M,IK,IC,IAD,IFX,IFR,MSL,TNSL,NFIX,NFRE
      my_real RX,RY,RZ,R1,R2,R3,MMAS,XG(3),VG(3)
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
      TNSL = 0
      DO N = 1,NRBYLAG
        CALL LAG_RBY_COND(
     1       NPBYL     ,LPBYL(TNSL+1),RBYL   ,MASS      ,INER      ,
     2       X         ,V        ,VR         ,A         ,AR        ,
     3       IADLL     ,LLL      ,COMNTAG    ,N         ,NC        )
C
        MSL  = NPBYL(2,N)
        NFIX = NPBYL(4,N)
        NFRE = NPBYL(5,N)
        IF (NFIX.EQ.0) THEN
C         no condensation
          M  = NPBYL(1,N)
          DO I=1,MSL-1
            NN = LPBYL(TNSL+I)
            VG(1)=VR(1,M)
            VG(2)=VR(2,M)
            VG(3)=VR(3,M)
            R1 = X(1,NN) - X(1,M)
            R2 = X(2,NN) - X(2,M)
            R3 = X(3,NN) - X(3,M)
            RX = R1 + HALF*DT2*(VG(2)*R3 - VG(3)*R2)
            RY = R2 + HALF*DT2*(VG(3)*R1 - VG(1)*R3)
            RZ = R3 + HALF*DT2*(VG(1)*R2 - VG(2)*R1)
C---        ROTATIONS
            DO J=4,6
              NC = NC + 1
              IADLL(NC+1)=IADLL(NC) + 2
              IK = IADLL(NC)
              LLL(IK) = M
              JLL(IK) = J
              SLL(IK) = 0
              XLL(IK) = ONE
              IK = IK+1
              LLL(IK) = NN
              JLL(IK) = J
              SLL(IK) = 0
              XLL(IK) =-ONE
            ENDDO
C---        Trans x
            NC = NC + 1
            IADLL(NC+1)=IADLL(NC) + 4
            IK = IADLL(NC)
            LLL(IK) = NN
            JLL(IK) = 1
            SLL(IK) = 0
            XLL(IK) =-ONE
            IK = IK+1
            LLL(IK) = M
            JLL(IK) = 1
            SLL(IK) = 0
            XLL(IK) = ONE
            IK = IK+1
            LLL(IK) = M
            JLL(IK) = 5
            SLL(IK) = 0
            XLL(IK) = RZ
            IK = IK+1
            LLL(IK) = M
            JLL(IK) = 6
            SLL(IK) = 0
            XLL(IK) =-RY
C---        Trans y
            NC = NC + 1
            IADLL(NC+1)=IADLL(NC) + 4
            IK = IADLL(NC)
            LLL(IK) = NN
            JLL(IK) = 2
            SLL(IK) = 0
            XLL(IK) =-ONE
            IK = IK+1
            LLL(IK) = M
            JLL(IK) = 2
            SLL(IK) = 0
            XLL(IK) = ONE
            IK = IK+1
            LLL(IK) = M
            JLL(IK) = 4
            SLL(IK) = 0
            XLL(IK) =-RZ
            IK = IK+1
            LLL(IK) = M
            JLL(IK) = 6
            SLL(IK) = 0
            XLL(IK) = RX
C---        Trans z
            NC = NC + 1
            IADLL(NC+1)=IADLL(NC) + 4
            IK = IADLL(NC)
            LLL(IK) = NN
            JLL(IK) = 3
            SLL(IK) = 0
            XLL(IK) =-ONE
            IK = IK+1
            LLL(IK) = M
            JLL(IK) = 3
            SLL(IK) = 0
            XLL(IK) = ONE
            IK = IK+1
            LLL(IK) = M
            JLL(IK) = 4
            SLL(IK) = 0
            XLL(IK) = RY
            IK = IK+1
            LLL(IK) = M
            JLL(IK) = 5
            SLL(IK) = 0
            XLL(IK) =-RX
          ENDDO
        ELSEIF (NFRE.NE.0) THEN
C         partial condensation: secnds = free nodes -1
          JF = TNSL+2*MSL
          IFX  = NPBYL(7,N)
          IFR  = NPBYL(8,N)
          VG(1)=VR(1,IFR)
          VG(2)=VR(2,IFR)
          VG(3)=VR(3,IFR)
          DO I=2,NFRE
            NN = LPBYL(JF+I)
            R1 = X(1,NN) - X(1,IFR)
            R2 = X(2,NN) - X(2,IFR)
            R3 = X(3,NN) - X(3,IFR)
            RX = R1 + HALF*DT2*(VG(2)*R3 - VG(3)*R2)
            RY = R2 + HALF*DT2*(VG(3)*R1 - VG(1)*R3)
            RZ = R3 + HALF*DT2*(VG(1)*R2 - VG(2)*R1)
C---        Rot x,y,z
            DO J=4,6
              NC = NC + 1
              IADLL(NC+1)=IADLL(NC) + 2
              IK = IADLL(NC)
              LLL(IK) = IFR
              JLL(IK) = J
              SLL(IK) = 0
              XLL(IK) = ONE
              IK = IK+1
              LLL(IK) = NN
              JLL(IK) = J
              SLL(IK) = 0
              XLL(IK) =-ONE
            ENDDO
C---        Trans x
            NC = NC + 1
            IADLL(NC+1)=IADLL(NC) + 4
            IK = IADLL(NC)
            LLL(IK) = NN
            JLL(IK) = 1
            SLL(IK) = 0
            XLL(IK) =-ONE
            IK = IK+1
            LLL(IK) = IFR
            JLL(IK) = 1
            SLL(IK) = 0
            XLL(IK) = ONE
            IK = IK+1
            LLL(IK) = IFR
            JLL(IK) = 5
            SLL(IK) = 0
            XLL(IK) = RZ
            IK = IK+1
            LLL(IK) = IFR
            JLL(IK) = 6
            SLL(IK) = 0
            XLL(IK) =-RY
C---        Trans y
            NC = NC + 1
            IADLL(NC+1)=IADLL(NC) + 4
            IK = IADLL(NC)
            LLL(IK) = NN
            JLL(IK) = 2
            SLL(IK) = 0
            XLL(IK) =-ONE
            IK = IK+1
            LLL(IK) = IFR
            JLL(IK) = 2
            SLL(IK) = 0
            XLL(IK) = ONE
            IK = IK+1
            LLL(IK) = IFR
            JLL(IK) = 4
            SLL(IK) = 0
            XLL(IK) =-RZ
            IK = IK+1
            LLL(IK) = IFR
            JLL(IK) = 6
            SLL(IK) = 0
            XLL(IK) = RX
C---        Trans z
            NC = NC + 1
            IADLL(NC+1)=IADLL(NC) + 4
            IK = IADLL(NC)
            LLL(IK) = NN
            JLL(IK) = 3
            SLL(IK) = 0
            XLL(IK) =-ONE
            IK = IK+1
            LLL(IK) = IFR
            JLL(IK) = 3
            SLL(IK) = 0
            XLL(IK) = ONE
            IK = IK+1
            LLL(IK) = IFR
            JLL(IK) = 4
            SLL(IK) = 0
            XLL(IK) = RY
            IK = IK+1
            LLL(IK) = IFR
            JLL(IK) = 5
            SLL(IK) = 0
            XLL(IK) =-RX
          ENDDO
C         secnd = condensed node
          R1 = RBYL(11,N) - X(1,IFR)
          R2 = RBYL(12,N) - X(2,IFR)
          R3 = RBYL(13,N) - X(3,IFR)
          RX = R1 - HALF*DT2*(VG(2)*R3 - VG(3)*R2)
          RY = R2 - HALF*DT2*(VG(3)*R1 - VG(1)*R3)
          RZ = R3 - HALF*DT2*(VG(1)*R2 - VG(2)*R1)
C---      Trans x
          NC = NC + 1
          IADLL(NC+1)=IADLL(NC) + 4
          IK = IADLL(NC)
          LLL(IK) = IFX
          JLL(IK) = 1
          SLL(IK) = 0
          XLL(IK) =-ONE
          IK = IK+1
          LLL(IK) = IFR
          JLL(IK) = 1
          SLL(IK) = 0
          XLL(IK) = ONE
          IK = IK+1
          LLL(IK) = IFR
          JLL(IK) = 5
          SLL(IK) = 0
          XLL(IK) = RZ
          IK = IK+1
          LLL(IK) = IFR
          JLL(IK) = 6
          SLL(IK) = 0
          XLL(IK) =-RY
C---      Trans y
          NC = NC + 1
          IADLL(NC+1)=IADLL(NC) + 4
          IK = IADLL(NC)
          LLL(IK) = IFX
          JLL(IK) = 2
          SLL(IK) = 0
          XLL(IK) =-ONE
          IK = IK+1
          LLL(IK) = IFR
          JLL(IK) = 2
          SLL(IK) = 0
          XLL(IK) = ONE
          IK = IK+1
          LLL(IK) = IFR
          JLL(IK) = 4
          SLL(IK) = 0
          XLL(IK) =-RZ
          IK = IK+1
          LLL(IK) = IFR
          JLL(IK) = 6
          SLL(IK) = 0
          XLL(IK) = RX
C---      Trans z
          NC = NC + 1
          IADLL(NC+1)=IADLL(NC) + 4
          IK = IADLL(NC)
          LLL(IK) = IFX
          JLL(IK) = 3
          SLL(IK) = 0
          XLL(IK) =-ONE
          IK = IK+1
          LLL(IK) = IFR
          JLL(IK) = 3
          SLL(IK) = 0
          XLL(IK) = ONE
          IK = IK+1
          LLL(IK) = IFR
          JLL(IK) = 4
          SLL(IK) = 0
          XLL(IK) = RY
          IK = IK+1
          LLL(IK) = IFR
          JLL(IK) = 5
          SLL(IK) = 0
          XLL(IK) =-RX
        ENDIF
        TNSL = TNSL + 3*MSL
      ENDDO
      NCR = NC
C=======================================================================
      DO N=1,NRBYLAG
        NFIX = NPBYL(4,N) 
        NFRE = NPBYL(5,N)
        IF (NFIX.GT.0.AND.NFRE.GT.0) THEN
C         Partially condensed RB
          IFX  = NPBYL(7,N)
          IFR  = NPBYL(8,N)
C---      Rot x
          NC = NC + 1
          IADLL(NC+1)=IADLL(NC) + 4
          IK = IADLL(NC)
          LLL(IK) = IFR
          JLL(IK) = 4
          SLL(IK) = 0
          XLL(IK) =-ONE
          IK = IK+1
          LLL(IK) = IFX
          JLL(IK) = 4
          SLL(IK) = 0
          XLL(IK) = ONE
          IK = IK+1
          LLL(IK) = IFX
          JLL(IK) = 5
          SLL(IK) = 0
          XLL(IK) = ZERO
          IK = IK+1
          LLL(IK) = IFX
          JLL(IK) = 6
          SLL(IK) = 0
          XLL(IK) = ZERO
C---      Rot y
          NC = NC + 1
          IADLL(NC+1)=IADLL(NC) + 4
          IK = IADLL(NC)
          LLL(IK) = IFR
          JLL(IK) = 5
          SLL(IK) = 0
          XLL(IK) =-ONE
          IK = IK+1
          LLL(IK) = IFX
          JLL(IK) = 4
          SLL(IK) = 0
          XLL(IK) = ZERO
          IK = IK+1
          LLL(IK) = IFX
          JLL(IK) = 5
          SLL(IK) = 0
          XLL(IK) = ONE
          IK = IK+1
          LLL(IK) = IFX
          JLL(IK) = 6
          SLL(IK) = 0
          XLL(IK) = ZERO
C---      Rot z
          NC = NC + 1
          IADLL(NC+1)=IADLL(NC) + 4
          IK = IADLL(NC)
          LLL(IK) = IFR
          JLL(IK) = 6
          SLL(IK) = 0
          XLL(IK) =-ONE
          IK = IK+1
          LLL(IK) = IFX
          JLL(IK) = 4
          SLL(IK) = 0
          XLL(IK) = ZERO
          IK = IK+1
          LLL(IK) = IFX
          JLL(IK) = 5
          SLL(IK) = 0
          XLL(IK) = ZERO
          IK = IK+1
          LLL(IK) = IFX
          JLL(IK) = 6
          SLL(IK) = 0
          XLL(IK) = ONE
        ENDIF
      ENDDO
C---
      RETURN
      END
