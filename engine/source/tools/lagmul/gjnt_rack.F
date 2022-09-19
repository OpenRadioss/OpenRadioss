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
Chd|  GJNT_RACK                     source/tools/lagmul/gjnt_rack.F
Chd|-- called by -----------
Chd|        LAG_GJNT                      source/tools/lagmul/lag_gjnt.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GJNT_RACK(SK     ,L1     ,L2     ,ALPHA  ,X      ,
     2                     IADLL  ,LLL    ,JLL    ,SLL    ,XLL    ,
     3                     N0     ,N1     ,N2     ,NC     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N0,N1,N2,NC, LLL(*),JLL(*),SLL(*),IADLL(*)
      my_real
     .        X(3,*),XLL(*),SK(9),L1(3),L2(3),ALPHA
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IK,IC,IAD
      my_real
     .        XT1(3),YT1(3),ZT1(3),XT2(3),YT2(3),ZT2(3),
     .        X0,Y0,Z0,X1,Y1,Z1,X2,Y2,Z2,XR,YR,ZR,XM,YM,ZM,NORM
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
C---  Node 1 skew
      XT1(1) = SK(1)*L1(1)+SK(4)*L1(2)+SK(7)*L1(3)
      XT1(2) = SK(2)*L1(1)+SK(5)*L1(2)+SK(8)*L1(3)
      XT1(3) = SK(3)*L1(1)+SK(6)*L1(2)+SK(9)*L1(3)
      NORM = ONE/SQRT(XT1(1)*XT1(1)+XT1(2)*XT1(2)+XT1(3)*XT1(3))
      XT1(1) = XT1(1)*NORM
      XT1(2) = XT1(2)*NORM
      XT1(3) = XT1(3)*NORM
      IF (ABS(XT1(1))>ZEP99) THEN
        ZT1(1) =-XT1(3)
        ZT1(2) = ZERO
        ZT1(3) = XT1(1)
      ELSE
        ZT1(1) = ZERO
        ZT1(2) =-XT1(3)
        ZT1(3) = XT1(2)
      ENDIF
      NORM = ONE/SQRT(ZT1(1)*ZT1(1)+ZT1(2)*ZT1(2)+ZT1(3)*ZT1(3))
      ZT1(1) = ZT1(1)*NORM
      ZT1(2) = ZT1(2)*NORM
      ZT1(3) = ZT1(3)*NORM
      YT1(1) = ZT1(2)*XT1(3)-ZT1(3)*XT1(2)
      YT1(2) = ZT1(3)*XT1(1)-ZT1(1)*XT1(3)
      YT1(3) = ZT1(1)*XT1(2)-ZT1(2)*XT1(1)
C---  Node 2 skew
      XT2(1) = SK(1)*L2(1)+SK(4)*L2(2)+SK(7)*L2(3)
      XT2(2) = SK(2)*L2(1)+SK(5)*L2(2)+SK(8)*L2(3)
      XT2(3) = SK(3)*L2(1)+SK(6)*L2(2)+SK(9)*L2(3)
      NORM = ONE/SQRT(XT2(1)*XT2(1)+XT2(2)*XT2(2)+XT2(3)*XT2(3))
      XT2(1) = XT2(1)*NORM
      XT2(2) = XT2(2)*NORM
      XT2(3) = XT2(3)*NORM
      IF (ABS(XT2(1))>ZEP99) THEN
        ZT2(1) =-XT2(3)
        ZT2(2) = ZERO
        ZT2(3) = XT2(1)
      ELSE
        ZT2(1) = ZERO
        ZT2(2) =-XT2(3)
        ZT2(3) = XT2(2)
      ENDIF
      NORM = ONE/SQRT(ZT1(1)*ZT1(1)+ZT1(2)*ZT1(2)+ZT1(3)*ZT1(3))
      ZT1(1) = ZT1(1)*NORM
      ZT1(2) = ZT1(2)*NORM
      ZT1(3) = ZT1(3)*NORM
      YT2(1) = ZT2(2)*XT2(3)-ZT2(3)*XT2(2)
      YT2(2) = ZT2(3)*XT2(1)-ZT2(1)*XT2(3)
      YT2(3) = ZT2(1)*XT2(2)-ZT2(2)*XT2(1)
C---------------------------
      X0=X(1,N0)
      Y0=X(2,N0)
      Z0=X(3,N0)
      X1=X(1,N1) - X0
      Y1=X(2,N1) - Y0
      Z1=X(3,N1) - Z0
      X2=X(1,N2) - X0
      Y2=X(2,N2) - Y0
      Z2=X(3,N2) - Z0
C---------------------------    
C --- V_x
      NC = NC + 1
      IADLL(NC+1) = IADLL(NC) + 4
      IK = IADLL(NC)
      LLL(IK) = N1
      JLL(IK) = 1
      SLL(IK) = 0
      XLL(IK) =-ONE
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 1
      SLL(IK) = 0
      XLL(IK) = ONE
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 5
      SLL(IK) = 0
      XLL(IK) = Z1
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 6
      SLL(IK) = 0
      XLL(IK) =-Y1
C
C --- V_y
      NC = NC + 1
      IADLL(NC+1) = IADLL(NC) + 4
      IK = IADLL(NC)
      LLL(IK) = N1
      JLL(IK) = 2
      SLL(IK) = 0
      XLL(IK) =-ONE
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 2
      SLL(IK) = 0
      XLL(IK) = ONE
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 6
      SLL(IK) = 0
      XLL(IK) = X1
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 4
      SLL(IK) = 0
      XLL(IK) =-Z1
C
C --- V_z
      NC = NC + 1
      IADLL(NC+1) = IADLL(NC) + 4
      IK = IADLL(NC)
      LLL(IK) = N1
      JLL(IK) = 3
      SLL(IK) = 0
      XLL(IK) =-ONE
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 3
      SLL(IK) = 0
      XLL(IK) = ONE
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 4
      SLL(IK) = 0
      XLL(IK) = Y1
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 5
      SLL(IK) = 0
      XLL(IK) =-X1
C
C     Rotation N2
C
C --- VR_x
      NC = NC + 1
      IADLL(NC+1) = IADLL(NC) + 2
      IK = IADLL(NC)
      LLL(IK) = N2
      JLL(IK) = 4
      SLL(IK) = 0
      XLL(IK) = ONE
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 4
      SLL(IK) = 0
      XLL(IK) =-ONE
C
C --- VR_y
      NC = NC + 1
      IADLL(NC+1) = IADLL(NC) + 2
      IK = IADLL(NC)
      LLL(IK) = N2
      JLL(IK) = 5
      SLL(IK) = 0
      XLL(IK) = ONE
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 5
      SLL(IK) = 0
      XLL(IK) =-ONE
C
C --- VR_z
      NC = NC + 1
      IADLL(NC+1) = IADLL(NC) + 2
      IK = IADLL(NC)
      LLL(IK) = N2
      JLL(IK) = 6
      SLL(IK) = 0
      XLL(IK) = ONE
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 6
      SLL(IK) = 0
      XLL(IK) =-ONE
C
C
C --- Constraints
C     1
      NC = NC + 1
      IADLL(NC+1) = IADLL(NC) + 12
      IK = IADLL(NC)
      LLL(IK) = N1
      JLL(IK) = 4
      SLL(IK) = 0
      XLL(IK) = ALPHA*XT1(1)
      IK = IK+1
      LLL(IK) = N1
      JLL(IK) = 5
      SLL(IK) = 0
      XLL(IK) = ALPHA*XT1(2)
      IK = IK+1
      LLL(IK) = N1
      JLL(IK) = 6
      SLL(IK) = 0
      XLL(IK) = ALPHA*XT1(3)
c
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 4
      SLL(IK) = 0
      XLL(IK) =-ALPHA*XT1(1)
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 5
      SLL(IK) = 0
      XLL(IK) =-ALPHA*XT1(2)
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 6
      SLL(IK) = 0
      XLL(IK) =-ALPHA*XT1(3)
c
      IK = IK+1
      LLL(IK) = N2
      JLL(IK) = 1
      SLL(IK) = 0
      XLL(IK) = XT2(1)
      IK = IK+1
      LLL(IK) = N2
      JLL(IK) = 2
      SLL(IK) = 0
      XLL(IK) = XT2(2)
      IK = IK+1
      LLL(IK) = N2
      JLL(IK) = 3
      SLL(IK) = 0
      XLL(IK) = XT2(3)
C
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 1
      SLL(IK) = 0
      XLL(IK) =-XT2(1)
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 2
      SLL(IK) = 0
      XLL(IK) =-XT2(2)
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 3
      SLL(IK) = 0
      XLL(IK) =-XT2(3)
C
C     2
      NC = NC + 1
      IADLL(NC+1) = IADLL(NC) + 9
      IK = IADLL(NC)
      LLL(IK) = N2
      JLL(IK) = 1
      SLL(IK) = 0
      XLL(IK) =-YT2(1)
      IK = IK+1
      LLL(IK) = N2
      JLL(IK) = 2
      SLL(IK) = 0
      XLL(IK) =-YT2(2)
      IK = IK+1
      LLL(IK) = N2
      JLL(IK) = 3
      SLL(IK) = 0
      XLL(IK) =-YT2(3)
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 1
      SLL(IK) = 0
      XLL(IK) = YT2(1)
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 2
      SLL(IK) = 0
      XLL(IK) = YT2(2)
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 3
      SLL(IK) = 0
      XLL(IK) = YT2(3)
C
      IK = IK+1
      LLL(IK) = N2
      JLL(IK) = 4
      SLL(IK) = 0
      XLL(IK) = Y2*YT2(3) - Z2*YT2(2)
      IK = IK+1
      LLL(IK) = N2
      JLL(IK) = 5
      SLL(IK) = 0
      XLL(IK) = Z2*YT2(1) - X2*YT2(3)
      IK = IK+1
      LLL(IK) = N2
      JLL(IK) = 6
      SLL(IK) = 0
      XLL(IK) = X2*YT2(2) - Y2*YT2(1)
C
C     3
      NC = NC + 1
      IADLL(NC+1) = IADLL(NC) + 9
      IK = IADLL(NC)
      LLL(IK) = N2
      JLL(IK) = 1
      SLL(IK) = 0
      XLL(IK) =-ZT2(1)
      IK = IK+1
      LLL(IK) = N2
      JLL(IK) = 2
      SLL(IK) = 0
      XLL(IK) =-ZT2(2)
      IK = IK+1
      LLL(IK) = N2
      JLL(IK) = 3
      SLL(IK) = 0
      XLL(IK) =-ZT2(3)
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 1
      SLL(IK) = 0
      XLL(IK) = ZT2(1)
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 2
      SLL(IK) = 0
      XLL(IK) = ZT2(2)
      IK = IK+1
      LLL(IK) = N0
      JLL(IK) = 3
      SLL(IK) = 0
      XLL(IK) = ZT2(3)
C
      IK = IK+1
      LLL(IK) = N2
      JLL(IK) = 4
      SLL(IK) = 0
      XLL(IK) = Y2*ZT2(3) - Z2*ZT2(2)
      IK = IK+1
      LLL(IK) = N2
      JLL(IK) = 5
      SLL(IK) = 0
      XLL(IK) = Z2*ZT2(1) - X2*ZT2(3)
      IK = IK+1
      LLL(IK) = N2
      JLL(IK) = 6
      SLL(IK) = 0
      XLL(IK) = X1*ZT2(2) - Y2*ZT2(1)
C---
      RETURN
      END
