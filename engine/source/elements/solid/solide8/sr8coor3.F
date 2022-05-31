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
Chd|  SR8COOR3                      source/elements/solid/solide8/sr8coor3.F
Chd|-- called by -----------
Chd|        S8FORC3                       source/elements/solid/solide8/s8forc3.F
Chd|-- calls ---------------
Chd|        SORTHO3                       source/elements/solid/solide/sortho3.F
Chd|        SREPISOT3                     source/elements/solid/solide/srepiso3.F
Chd|====================================================================
      SUBROUTINE SR8COOR3(OFFG,OFF,X,V,IXS,
     .                   XLOC,YLOC,ZLOC,VXLOC,VYLOC,VZLOC,
     .                   MXT,NC,NGL,PID,
     .                   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .                   IOUTPRT, VGAX, VGAY, VGAZ, VGA2,NEL,
     .                   XGAX, XGAY, XGAZ, XGXA2, XGYA2, XGZA2,
     .                   XGXYA, XGYZA, XGZXA,IPARG) 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(11,*),IPARG(*),IOUTPRT,NEL
C     REAL ou REAL*8
      my_real
     .   OFFG(NEL),OFF(MVSIZ),X(3,*),V(3,*),
     .   R11(MVSIZ),R12(MVSIZ),R13(MVSIZ),
     .   R21(MVSIZ),R22(MVSIZ),R23(MVSIZ),
     .   R31(MVSIZ),R32(MVSIZ),R33(MVSIZ),
     .   VGAX(MVSIZ),VGAY(MVSIZ),VGAZ(MVSIZ),VGA2(MVSIZ),
     .   XGAX(MVSIZ),XGAY(MVSIZ),XGAZ(MVSIZ),
     .   XGXA2(MVSIZ),XGYA2(MVSIZ),XGZA2(MVSIZ),
     .   XGXYA(MVSIZ),XGYZA(MVSIZ),XGZXA(MVSIZ)
      my_real
     .   XLOC(MVSIZ,8), YLOC(MVSIZ,8), ZLOC(MVSIZ,8),
     .   VXLOC(MVSIZ,8),VYLOC(MVSIZ,8),VZLOC(MVSIZ,8)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NC(8,MVSIZ), MXT(MVSIZ), PID(MVSIZ), NGL(MVSIZ), I
C     REAL ou REAL*8
C
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   X5(MVSIZ), X6(MVSIZ), X7(MVSIZ), X8(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Y5(MVSIZ), Y6(MVSIZ), Y7(MVSIZ), Y8(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   Z5(MVSIZ), Z6(MVSIZ), Z7(MVSIZ), Z8(MVSIZ),VIS(MVSIZ),
     .   VX1(MVSIZ), VX2(MVSIZ), VX3(MVSIZ), VX4(MVSIZ),
     .   VX5(MVSIZ), VX6(MVSIZ), VX7(MVSIZ), VX8(MVSIZ),
     .   VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ),
     .   VY5(MVSIZ), VY6(MVSIZ), VY7(MVSIZ), VY8(MVSIZ),
     .   VZ1(MVSIZ), VZ2(MVSIZ), VZ3(MVSIZ), VZ4(MVSIZ),
     .   VZ5(MVSIZ), VZ6(MVSIZ), VZ7(MVSIZ), VZ8(MVSIZ),
     .   RX(MVSIZ) , RY(MVSIZ) , RZ(MVSIZ) ,
     .   SX(MVSIZ) , SY(MVSIZ) , SZ(MVSIZ) ,
     .   TX(MVSIZ) , TY(MVSIZ) , TZ(MVSIZ) 
      my_real 
     .   XL, YL, ZL, VXL, VYL, VZL, OFF_L
C=======================================================================
      OFF_L  = ZERO
      DO 10 I=LFT,LLT
      NGL(I)=IXS(11,I)
      MXT(I)=IXS(1,I)
      PID(I)=IXS(10,I)
      NC(1,I)=IXS(2,I)
      NC(2,I)=IXS(3,I)
      NC(3,I)=IXS(4,I)
      NC(4,I)=IXS(5,I)
      NC(5,I)=IXS(6,I)
      NC(6,I)=IXS(7,I)
      NC(7,I)=IXS(8,I)
   10 NC(8,I)=IXS(9,I)
C
C----------------------------
C     NODAL COORDINATES     |
C----------------------------
      DO I=LFT,LLT
        X1(I)=X(1,NC(1,I))
        Y1(I)=X(2,NC(1,I))
        Z1(I)=X(3,NC(1,I))
        X2(I)=X(1,NC(2,I))
        Y2(I)=X(2,NC(2,I))
        Z2(I)=X(3,NC(2,I))
        X3(I)=X(1,NC(3,I))
        Y3(I)=X(2,NC(3,I))
        Z3(I)=X(3,NC(3,I))
        X4(I)=X(1,NC(4,I))
        Y4(I)=X(2,NC(4,I))
        Z4(I)=X(3,NC(4,I))
        X5(I)=X(1,NC(5,I))
        Y5(I)=X(2,NC(5,I))
        Z5(I)=X(3,NC(5,I))
        X6(I)=X(1,NC(6,I))
        Y6(I)=X(2,NC(6,I))
        Z6(I)=X(3,NC(6,I))
        X7(I)=X(1,NC(7,I))
        Y7(I)=X(2,NC(7,I))
        Z7(I)=X(3,NC(7,I))
        X8(I)=X(1,NC(8,I))
        Y8(I)=X(2,NC(8,I))
        Z8(I)=X(3,NC(8,I))
      ENDDO
C-----------
      DO I=LFT,LLT
        VX1(I)=V(1,NC(1,I))
        VY1(I)=V(2,NC(1,I))
        VZ1(I)=V(3,NC(1,I))
        VX2(I)=V(1,NC(2,I))
        VY2(I)=V(2,NC(2,I))
        VZ2(I)=V(3,NC(2,I))
        VX3(I)=V(1,NC(3,I))
        VY3(I)=V(2,NC(3,I))
        VZ3(I)=V(3,NC(3,I))
        VX4(I)=V(1,NC(4,I))
        VY4(I)=V(2,NC(4,I))
        VZ4(I)=V(3,NC(4,I))
        VX5(I)=V(1,NC(5,I))
        VY5(I)=V(2,NC(5,I))
        VZ5(I)=V(3,NC(5,I))
        VX6(I)=V(1,NC(6,I))
        VY6(I)=V(2,NC(6,I))
        VZ6(I)=V(3,NC(6,I))
        VX7(I)=V(1,NC(7,I))
        VY7(I)=V(2,NC(7,I))
        VZ7(I)=V(3,NC(7,I))
        VX8(I)=V(1,NC(8,I))
        VY8(I)=V(2,NC(8,I))
        VZ8(I)=V(3,NC(8,I))
        OFF(I) = MIN(ONE,ABS(OFFG(I)))
        OFF_L  = MIN(OFF_L,OFFG(I))
      ENDDO
Clm
C     Prepare les sorties par PART.
C-----------
      IF(IOUTPRT.NE.0)THEN
       DO I=LFT,LLT
        VGAX(I)=VX1(I)+VX2(I)+VX3(I)+VX4(I)+VX5(I)+VX6(I)+VX7(I)+VX8(I)
        VGAY(I)=VY1(I)+VY2(I)+VY3(I)+VY4(I)+VY5(I)+VY6(I)+VY7(I)+VY8(I)
        VGAZ(I)=VZ1(I)+VZ2(I)+VZ3(I)+VZ4(I)+VZ5(I)+VZ6(I)+VZ7(I)+VZ8(I)
        VGA2(I)=VX1(I)*VX1(I)+VX2(I)*VX2(I)+VX3(I)*VX3(I)+VX4(I)*VX4(I)
     1         +VX5(I)*VX5(I)+VX6(I)*VX6(I)+VX7(I)*VX7(I)+VX8(I)*VX8(I)
     2         +VY1(I)*VY1(I)+VY2(I)*VY2(I)+VY3(I)*VY3(I)+VY4(I)*VY4(I)
     3         +VY5(I)*VY5(I)+VY6(I)*VY6(I)+VY7(I)*VY7(I)+VY8(I)*VY8(I)
     4         +VZ1(I)*VZ1(I)+VZ2(I)*VZ2(I)+VZ3(I)*VZ3(I)+VZ4(I)*VZ4(I)
     5         +VZ5(I)*VZ5(I)+VZ6(I)*VZ6(I)+VZ7(I)*VZ7(I)+VZ8(I)*VZ8(I)
       ENDDO
       IF(IPARG(80).EQ.1) THEN
         DO I=LFT,LLT
          XGAX(I)=X1(I)+X2(I)+X3(I)+X4(I)+X5(I)+X6(I)+X7(I)+X8(I)
          XGAY(I)=Y1(I)+Y2(I)+Y3(I)+Y4(I)+Y5(I)+Y6(I)+Y7(I)+Y8(I)
          XGAZ(I)=Z1(I)+Z2(I)+Z3(I)+Z4(I)+Z5(I)+Z6(I)+Z7(I)+Z8(I)
          XGXA2(I)=X1(I)**2+X2(I)**2+X3(I)**2+X4(I)**2
     1            +X5(I)**2+X6(I)**2+X7(I)**2+X8(I)**2
          XGYA2(I)=Y1(I)**2+Y2(I)**2+Y3(I)**2+Y4(I)**2
     1            +Y5(I)**2+Y6(I)**2+Y7(I)**2+Y8(I)**2
          XGZA2(I)=Z1(I)**2+Z2(I)**2+Z3(I)**2+Z4(I)**2
     1            +Z5(I)**2+Z6(I)**2+Z7(I)**2+Z8(I)**2
          XGXYA(I)=X1(I)*Y1(I)+X2(I)*Y2(I)+X3(I)*Y3(I)+X4(I)*Y4(I)
     1            +X5(I)*Y5(I)+X6(I)*Y6(I)+X7(I)*Y7(I)+X8(I)*Y8(I)
          XGYZA(I)=Y1(I)*Z1(I)+Y2(I)*Z2(I)+Y3(I)*Z3(I)+Y4(I)*Z4(I)
     1            +Y5(I)*Z5(I)+Y6(I)*Z6(I)+Y7(I)*Z7(I)+Y8(I)*Z8(I)
          XGZXA(I)=Z1(I)*X1(I)+Z2(I)*X2(I)+Z3(I)*X3(I)+Z4(I)*X4(I)
     1            +Z5(I)*X5(I)+Z6(I)*X6(I)+Z7(I)*X7(I)+Z8(I)*X8(I)
         ENDDO
       ENDIF
      ENDIF
Clm
C-----------
C     REPERE CONVECTE (ITERATIONS).
C-----------
      CALL SREPISOT3(
     .       X1, X2, X3, X4, X5, X6, X7, X8,
     .       Y1, Y2, Y3, Y4, Y5, Y6, Y7, Y8,
     .       Z1, Z2, Z3, Z4, Z5, Z6, Z7, Z8,
     .       RX, RY, RZ, SX, SY, SZ, TX, TY, TZ )
      CALL SORTHO3(
     .       RX , RY , RZ , SX , SY , SZ , TX , TY , TZ ,
     .       R11, R12, R13, R21, R22, R23, R31, R32, R33)
C-----------
C     PASSAGE AU REPERE CONVECTE.
C-----------
Clm   X=RX' <=> X'=t(R)X chgt de base.
       DO I=LFT,LLT
        XL=R11(I)*X1(I)+R21(I)*Y1(I)+R31(I)*Z1(I)
        YL=R12(I)*X1(I)+R22(I)*Y1(I)+R32(I)*Z1(I)
        ZL=R13(I)*X1(I)+R23(I)*Y1(I)+R33(I)*Z1(I)
        XLOC(I,1)=XL
        YLOC(I,1)=YL
        ZLOC(I,1)=ZL
        XL=R11(I)*X2(I)+R21(I)*Y2(I)+R31(I)*Z2(I)
        YL=R12(I)*X2(I)+R22(I)*Y2(I)+R32(I)*Z2(I)
        ZL=R13(I)*X2(I)+R23(I)*Y2(I)+R33(I)*Z2(I)
        XLOC(I,2)=XL
        YLOC(I,2)=YL
        ZLOC(I,2)=ZL
        XL=R11(I)*X3(I)+R21(I)*Y3(I)+R31(I)*Z3(I)
        YL=R12(I)*X3(I)+R22(I)*Y3(I)+R32(I)*Z3(I)
        ZL=R13(I)*X3(I)+R23(I)*Y3(I)+R33(I)*Z3(I)
        XLOC(I,3)=XL
        YLOC(I,3)=YL
        ZLOC(I,3)=ZL
        XL=R11(I)*X4(I)+R21(I)*Y4(I)+R31(I)*Z4(I)
        YL=R12(I)*X4(I)+R22(I)*Y4(I)+R32(I)*Z4(I)
        ZL=R13(I)*X4(I)+R23(I)*Y4(I)+R33(I)*Z4(I)
        XLOC(I,4)=XL
        YLOC(I,4)=YL
        ZLOC(I,4)=ZL
        XL=R11(I)*X5(I)+R21(I)*Y5(I)+R31(I)*Z5(I)
        YL=R12(I)*X5(I)+R22(I)*Y5(I)+R32(I)*Z5(I)
        ZL=R13(I)*X5(I)+R23(I)*Y5(I)+R33(I)*Z5(I)
        XLOC(I,5)=XL
        YLOC(I,5)=YL
        ZLOC(I,5)=ZL
        XL=R11(I)*X6(I)+R21(I)*Y6(I)+R31(I)*Z6(I)
        YL=R12(I)*X6(I)+R22(I)*Y6(I)+R32(I)*Z6(I)
        ZL=R13(I)*X6(I)+R23(I)*Y6(I)+R33(I)*Z6(I)
        XLOC(I,6)=XL
        YLOC(I,6)=YL
        ZLOC(I,6)=ZL
        XL=R11(I)*X7(I)+R21(I)*Y7(I)+R31(I)*Z7(I)
        YL=R12(I)*X7(I)+R22(I)*Y7(I)+R32(I)*Z7(I)
        ZL=R13(I)*X7(I)+R23(I)*Y7(I)+R33(I)*Z7(I)
        XLOC(I,7)=XL
        YLOC(I,7)=YL
        ZLOC(I,7)=ZL
        XL=R11(I)*X8(I)+R21(I)*Y8(I)+R31(I)*Z8(I)
        YL=R12(I)*X8(I)+R22(I)*Y8(I)+R32(I)*Z8(I)
        ZL=R13(I)*X8(I)+R23(I)*Y8(I)+R33(I)*Z8(I)
        XLOC(I,8)=XL
        YLOC(I,8)=YL
        ZLOC(I,8)=ZL
       ENDDO
C-----------
C     PASSAGE AU REPERE CONVECTE.
C-----------
       DO I=LFT,LLT
        VXL=R11(I)*VX1(I)+R21(I)*VY1(I)+R31(I)*VZ1(I)
        VYL=R12(I)*VX1(I)+R22(I)*VY1(I)+R32(I)*VZ1(I)
        VZL=R13(I)*VX1(I)+R23(I)*VY1(I)+R33(I)*VZ1(I)
        VXLOC(I,1)=VXL
        VYLOC(I,1)=VYL
        VZLOC(I,1)=VZL
        VXL=R11(I)*VX2(I)+R21(I)*VY2(I)+R31(I)*VZ2(I)
        VYL=R12(I)*VX2(I)+R22(I)*VY2(I)+R32(I)*VZ2(I)
        VZL=R13(I)*VX2(I)+R23(I)*VY2(I)+R33(I)*VZ2(I)
        VXLOC(I,2)=VXL
        VYLOC(I,2)=VYL
        VZLOC(I,2)=VZL
        VXL=R11(I)*VX3(I)+R21(I)*VY3(I)+R31(I)*VZ3(I)
        VYL=R12(I)*VX3(I)+R22(I)*VY3(I)+R32(I)*VZ3(I)
        VZL=R13(I)*VX3(I)+R23(I)*VY3(I)+R33(I)*VZ3(I)
        VXLOC(I,3)=VXL
        VYLOC(I,3)=VYL
        VZLOC(I,3)=VZL
        VXL=R11(I)*VX4(I)+R21(I)*VY4(I)+R31(I)*VZ4(I)
        VYL=R12(I)*VX4(I)+R22(I)*VY4(I)+R32(I)*VZ4(I)
        VZL=R13(I)*VX4(I)+R23(I)*VY4(I)+R33(I)*VZ4(I)
        VXLOC(I,4)=VXL
        VYLOC(I,4)=VYL
        VZLOC(I,4)=VZL
        VXL=R11(I)*VX5(I)+R21(I)*VY5(I)+R31(I)*VZ5(I)
        VYL=R12(I)*VX5(I)+R22(I)*VY5(I)+R32(I)*VZ5(I)
        VZL=R13(I)*VX5(I)+R23(I)*VY5(I)+R33(I)*VZ5(I)
        VXLOC(I,5)=VXL
        VYLOC(I,5)=VYL
        VZLOC(I,5)=VZL
        VXL=R11(I)*VX6(I)+R21(I)*VY6(I)+R31(I)*VZ6(I)
        VYL=R12(I)*VX6(I)+R22(I)*VY6(I)+R32(I)*VZ6(I)
        VZL=R13(I)*VX6(I)+R23(I)*VY6(I)+R33(I)*VZ6(I)
        VXLOC(I,6)=VXL
        VYLOC(I,6)=VYL
        VZLOC(I,6)=VZL
        VXL=R11(I)*VX7(I)+R21(I)*VY7(I)+R31(I)*VZ7(I)
        VYL=R12(I)*VX7(I)+R22(I)*VY7(I)+R32(I)*VZ7(I)
        VZL=R13(I)*VX7(I)+R23(I)*VY7(I)+R33(I)*VZ7(I)
        VXLOC(I,7)=VXL
        VYLOC(I,7)=VYL
        VZLOC(I,7)=VZL
        VXL=R11(I)*VX8(I)+R21(I)*VY8(I)+R31(I)*VZ8(I)
        VYL=R12(I)*VX8(I)+R22(I)*VY8(I)+R32(I)*VZ8(I)
        VZL=R13(I)*VX8(I)+R23(I)*VY8(I)+R33(I)*VZ8(I)
        VXLOC(I,8)=VXL
        VYLOC(I,8)=VYL
        VZLOC(I,8)=VZL
C
        VXL = (VXLOC(I,1)+VXLOC(I,2)+VXLOC(I,3)+VXLOC(I,4)
     .       + VXLOC(I,5)+VXLOC(I,6)+VXLOC(I,7)+VXLOC(I,8))*ONE_OVER_8
        VYL = (VYLOC(I,1)+VYLOC(I,2)+VYLOC(I,3)+VYLOC(I,4)
     .       + VYLOC(I,5)+VYLOC(I,6)+VYLOC(I,7)+VYLOC(I,8))*ONE_OVER_8
        VZL = (VZLOC(I,1)+VZLOC(I,2)+VZLOC(I,3)+VZLOC(I,4)
     .       + VZLOC(I,5)+VZLOC(I,6)+VZLOC(I,7)+VZLOC(I,8)) * ONE_OVER_8
        VXLOC(I,1)=VXLOC(I,1)-VXL
        VYLOC(I,1)=VYLOC(I,1)-VYL
        VZLOC(I,1)=VZLOC(I,1)-VZL
        VXLOC(I,2)=VXLOC(I,2)-VXL
        VYLOC(I,2)=VYLOC(I,2)-VYL
        VZLOC(I,2)=VZLOC(I,2)-VZL
        VXLOC(I,3)=VXLOC(I,3)-VXL
        VYLOC(I,3)=VYLOC(I,3)-VYL
        VZLOC(I,3)=VZLOC(I,3)-VZL
        VXLOC(I,4)=VXLOC(I,4)-VXL
        VYLOC(I,4)=VYLOC(I,4)-VYL
        VZLOC(I,4)=VZLOC(I,4)-VZL
        VXLOC(I,5)=VXLOC(I,5)-VXL
        VYLOC(I,5)=VYLOC(I,5)-VYL
        VZLOC(I,5)=VZLOC(I,5)-VZL
        VXLOC(I,6)=VXLOC(I,6)-VXL
        VYLOC(I,6)=VYLOC(I,6)-VYL
        VZLOC(I,6)=VZLOC(I,6)-VZL
        VXLOC(I,7)=VXLOC(I,7)-VXL
        VYLOC(I,7)=VYLOC(I,7)-VYL
        VZLOC(I,7)=VZLOC(I,7)-VZL
        VXLOC(I,8)=VXLOC(I,8)-VXL
        VYLOC(I,8)=VYLOC(I,8)-VYL
        VZLOC(I,8)=VZLOC(I,8)-VZL
       ENDDO
C-----------
      IF(OFF_L.LT.0.)THEN
        DO I=LFT,LLT
          IF(OFFG(I).LT.0.)THEN
            VXLOC(I,1)=0.
            VYLOC(I,1)=0.
            VZLOC(I,1)=0.
            VXLOC(I,2)=0.
            VYLOC(I,2)=0.
            VZLOC(I,2)=0.
            VXLOC(I,3)=0.
            VYLOC(I,3)=0.
            VZLOC(I,3)=0.
            VXLOC(I,4)=0.
            VYLOC(I,4)=0.
            VZLOC(I,4)=0.
            VXLOC(I,5)=0.
            VYLOC(I,5)=0.
            VZLOC(I,5)=0.
            VXLOC(I,6)=0.
            VYLOC(I,6)=0.
            VZLOC(I,6)=0.
            VXLOC(I,7)=0.
            VYLOC(I,7)=0.
            VZLOC(I,7)=0.
            VXLOC(I,8)=0.
            VYLOC(I,8)=0.
            VZLOC(I,8)=0.
          ENDIF
        ENDDO
      ENDIF
C-----------
      RETURN
      END

