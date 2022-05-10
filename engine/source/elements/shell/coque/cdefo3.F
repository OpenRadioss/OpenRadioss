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
Chd|  CDEFO3                        source/elements/shell/coque/cdefo3.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDEFO3(JFT,JLT,VL1,VL2,VL3,VL4,
     1            DT1C,PX1 ,PX2,PY1,PY2,AREA,
     2            EXX ,EYY,EXY,EXZ,EYZ ,X2  ,X3 ,X4 ,Y2 ,Y3  ,
     3            Y4  ,Z2 ,VX1,VX2,VX3 ,VX4 ,VY1,VY2,VY3,VY4 ,
     4            VZ1 ,VZ2,VZ3,VZ4,E1X ,E1Y ,E1Z,E2X,E2Y,E2Z ,
     5            E3X ,E3Y,E3Z,IHBE)
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
#include      "com08_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT,IHBE
C     REAL ou REAL*8
      my_real
     .   VL1(MVSIZ,3),VL2(MVSIZ,3),VL3(MVSIZ,3),VL4(MVSIZ,3),DT1C(*),
     .   PX1(MVSIZ), PX2(MVSIZ), PY1(MVSIZ), PY2(MVSIZ),
     .   EXX(MVSIZ), AREA(MVSIZ),
     .   EYY(MVSIZ), EXY(MVSIZ), EXZ(MVSIZ), EYZ(MVSIZ),
     .   X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ), Z2(MVSIZ),
     .   VX1(MVSIZ),  VX2(MVSIZ),  VX3(MVSIZ),  VX4(MVSIZ),
     .   VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), VY4(MVSIZ),
     .   VZ1(MVSIZ), VZ2(MVSIZ), VZ3(MVSIZ), VZ4(MVSIZ),
     .   VX13(MVSIZ), VX24(MVSIZ), VY13(MVSIZ), VY24(MVSIZ),
     .   VZ13(MVSIZ), VZ24(MVSIZ), 
     .   E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ), E2X(MVSIZ),
     .   E2Y(MVSIZ), E2Z(MVSIZ), E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
      my_real
     .     ZZ2(MVSIZ), EXZZ2(MVSIZ), EYZZ2(MVSIZ), EXZ2PY2(MVSIZ), 
     .     EXZ2PY1(MVSIZ), EYZ2PX2(MVSIZ), EYZ2PX1(MVSIZ),ZZZ(MVSIZ), 
     .     EXZ2(MVSIZ),EYZ2(MVSIZ),
     .     DT1V4, TMP1A(MVSIZ), TMP2A(MVSIZ), TMP3A(MVSIZ),
     .     TMP1B(MVSIZ), TMP2B(MVSIZ), TMP3B(MVSIZ),GZX(MVSIZ),
     .     GZY(MVSIZ)
c=======================================================================
C
      DO I=JFT,JLT
        VX1(I)=E1X(I)*VL1(I,1)+E1Y(I)*VL1(I,2)+E1Z(I)*VL1(I,3)
        VX2(I)=E1X(I)*VL2(I,1)+E1Y(I)*VL2(I,2)+E1Z(I)*VL2(I,3)
        VX3(I)=E1X(I)*VL3(I,1)+E1Y(I)*VL3(I,2)+E1Z(I)*VL3(I,3)
        VX4(I)=E1X(I)*VL4(I,1)+E1Y(I)*VL4(I,2)+E1Z(I)*VL4(I,3)
C
        VY4(I)=E2X(I)*VL4(I,1)+E2Y(I)*VL4(I,2)+E2Z(I)*VL4(I,3)
        VY3(I)=E2X(I)*VL3(I,1)+E2Y(I)*VL3(I,2)+E2Z(I)*VL3(I,3)
        VY2(I)=E2X(I)*VL2(I,1)+E2Y(I)*VL2(I,2)+E2Z(I)*VL2(I,3)
        VY1(I)=E2X(I)*VL1(I,1)+E2Y(I)*VL1(I,2)+E2Z(I)*VL1(I,3)
C
        VZ1(I)=E3X(I)*VL1(I,1)+E3Y(I)*VL1(I,2)+E3Z(I)*VL1(I,3)
        VZ2(I)=E3X(I)*VL2(I,1)+E3Y(I)*VL2(I,2)+E3Z(I)*VL2(I,3)
        VZ3(I)=E3X(I)*VL3(I,1)+E3Y(I)*VL3(I,2)+E3Z(I)*VL3(I,3)
        VZ4(I)=E3X(I)*VL4(I,1)+E3Y(I)*VL4(I,2)+E3Z(I)*VL4(I,3)
C
        VZ13(I)=VZ1(I)-VZ3(I)
        VZ24(I)=VZ2(I)-VZ4(I)
C
        EYZ(I)=PY1(I)*VZ13(I)+PY2(I)*VZ24(I)
        EXZ(I)=PX1(I)*VZ13(I)+PX2(I)*VZ24(I)
C
      ENDDO
C
      IF(IHBE.LE.1)THEN
        DO I=JFT,JLT
C z2 mis a zero pour non equilibre des foreces et moments d'hourglass
          Z2(I) = ZERO
          DT1V4 = QUART*DT1C(I)
          IF(IMPL_S.GT.0) DT1V4=ZERO
          TMP2A(I)=PY2(I)+PY1(I)
          TMP3A(I)=SIGN(MAX(ABS(TMP2A(I)),EM20),TMP2A(I))
          TMP1A(I)=DT1V4*(VZ13(I)-VZ24(I))*(VZ13(I)-VZ24(I))/TMP3A(I)
          VX13(I)=VX1(I)-VX3(I)
          VX24(I)=VX2(I)-VX4(I)
          VX13(I)=VX13(I)-TMP1A(I)
          VX24(I)=VX24(I)+TMP1A(I)
C
          EXX(I)=PX1(I)*VX13(I)+PX2(I)*VX24(I)
          EXY(I)=PY1(I)*VX13(I)+PY2(I)*VX24(I)
C
          TMP1B(I)=PX2(I)-PX1(I)
          TMP3B(I)=SIGN(MAX(ABS(TMP1B(I)),EM20),TMP1B(I))
          TMP2B(I)=DT1V4*(VZ13(I)+VZ24(I))*(VZ13(I)+VZ24(I))/TMP3B(I)
          VY13(I)=VY1(I)-VY3(I)
          VY24(I)=VY2(I)-VY4(I)
          VY13(I)=VY13(I)+TMP2B(I)
          VY24(I)=VY24(I)+TMP2B(I)
C
          EXY(I)=EXY(I)+PX1(I)*VY13(I)+PX2(I)*VY24(I)
          EYY(I)=PY1(I)*VY13(I)+PY2(I)*VY24(I)
        ENDDO
C
      ELSEIF(IHBE.EQ.2.OR.IHBE.EQ.3)THEN
C
        DO I=JFT,JLT
          DT1V4 = UNDEMI*DT1C(I)
          IF(IMPL_S.GT.0) DT1V4=ZERO
C
          GZX(I)  = EXZ(I)/AREA(I)
          EXZZ2(I)= GZX(I)*Z2(I)
          EXZ2(I) = GZX(I)*GZX(I)*DT1V4
          VX3(I)=VX3(I)         -EXZ2(I)*X3(I)-VX1(I)
          VX2(I)=VX2(I)+EXZZ2(I)-EXZ2(I)*X2(I)-VX1(I)
          VX4(I)=VX4(I)+EXZZ2(I)-EXZ2(I)*X4(I)-VX1(I)
          VX1(I)=ZERO
C
          GZY(I)  = EYZ(I)/AREA(I)
          EYZZ2(I)= GZY(I)*Z2(I)
          EYZ2(I) = GZY(I)*GZY(I)*DT1V4
          VY3(I)=VY3(I)         -EYZ2(I)*Y3(I)-VY1(I)
          VY2(I)=VY2(I)+EYZZ2(I)-EYZ2(I)*Y2(I)-VY1(I)
          VY4(I)=VY4(I)+EYZZ2(I)-EYZ2(I)*Y4(I)-VY1(I)
          VY1(I)=ZERO
C
          ZZZ(I)= (EXZ2(I)+EYZ2(I))*Z2(I)
          VZ3(I)=VZ3(I)-GZY(I)*Y3(I)-GZX(I)*X3(I)       -VZ1(I)
          VZ2(I)=VZ2(I)-GZY(I)*Y2(I)-GZX(I)*X2(I)-ZZZ(I)-VZ1(I)
          VZ4(I)=VZ4(I)-GZY(I)*Y4(I)-GZX(I)*X4(I)-ZZZ(I)-VZ1(I)
          VZ1(I)=ZERO
C
          VX13(I)=-VX3(I)
          VX24(I)=VX2(I)-VX4(I)
C
          EXX(I)=PX1(I)*VX13(I)+PX2(I)*VX24(I)
          EXY(I)=PY1(I)*VX13(I)+PY2(I)*VX24(I)
C
          VY13(I)=-VY3(I)
          VY24(I)=VY2(I)-VY4(I)
C
          EXY(I)=EXY(I)+PX1(I)*VY13(I)+PX2(I)*VY24(I)
          EYY(I)=PY1(I)*VY13(I)+PY2(I)*VY24(I)
        ENDDO
C
      ELSEIF(IHBE.EQ.4)THEN
C    old
        DO I=JFT,JLT
          DT1V4 = UNDEMI*DT1C(I)
          IF(IMPL_S.GT.0) DT1V4=ZERO
c wraping z1=z3=0 z4=z2 => modif des vitesses pour l'hourglass membrane
          ZZ2(I)=UNDEMI*Z2(I)
C
          GZX(I)     = EXZ(I)/AREA(I)
          EXZZ2(I)   = GZX(I)*ZZ2(I)
          EXZ2(I)    = GZX(I)*GZX(I)*DT1V4
          EXZ2PY2(I) = EXZ2(I)*PY2(I)
          EXZ2PY1(I) = EXZ2(I)*PY1(I)
          VX1(I)=VX1(I)-EXZZ2(I)-EXZ2PY2(I)
          VX3(I)=VX3(I)-EXZZ2(I)+EXZ2PY2(I)
          VX2(I)=VX2(I)+EXZZ2(I)+EXZ2PY1(I)
          VX4(I)=VX4(I)+EXZZ2(I)-EXZ2PY1(I)
C
          GZY(I)     = EYZ(I)/AREA(I)
          EYZZ2(I)   = GZY(I)*ZZ2(I)
          EYZ2(I)    = GZY(I)*GZY(I)*DT1V4
          EYZ2PX2(I) = EYZ2(I)*PX2(I)
          EYZ2PX1(I) = EYZ2(I)*PX1(I)
          VY1(I)=VY1(I)-EYZZ2(I)+EYZ2PX2(I)
          VY3(I)=VY3(I)-EYZZ2(I)-EYZ2PX2(I)
          VY2(I)=VY2(I)+EYZZ2(I)-EYZ2PX1(I)
          VY4(I)=VY4(I)+EYZZ2(I)+EYZ2PX1(I)
C
          VX13(I)=VX1(I)-VX3(I)
          VX24(I)=VX2(I)-VX4(I)
C
          EXX(I)=PX1(I)*VX13(I)+PX2(I)*VX24(I)
          EXY(I)=PY1(I)*VX13(I)+PY2(I)*VX24(I)
C
          VY13(I)=VY1(I)-VY3(I)
          VY24(I)=VY2(I)-VY4(I)
C
          EXY(I)=EXY(I)+PX1(I)*VY13(I)+PX2(I)*VY24(I)
          EYY(I)=PY1(I)*VY13(I)+PY2(I)*VY24(I)
        ENDDO
      ENDIF
c-----------
      RETURN
      END
c
Chd|====================================================================
Chd|  CDEFOT3                       source/elements/shell/coque/cdefo3.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CDEFOT3(JFT ,JLT  ,PX1  ,PX2  ,PY1   ,
     2                   PY2 ,VX13 ,VX24  ,VY13  ,VY24   ,
     3                   F_DEF )
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
#include      "com08_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT
C     REAL ou REAL*8
      my_real
     .   PX1(MVSIZ), PX2(MVSIZ), PY1(MVSIZ), PY2(MVSIZ),
     .   VX13(MVSIZ), VX24(MVSIZ), VY13(MVSIZ), VY24(MVSIZ),
     .   F_DEF(MVSIZ,8)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
      my_real
     .     S
c=======================================================================
        DO I=JFT,JLT
          F_DEF(I,1)=PX1(I)*VX13(I)+PX2(I)*VX24(I)
          F_DEF(I,2)=PY1(I)*VY13(I)+PY2(I)*VY24(I)
          F_DEF(I,3)=PY1(I)*VX13(I)+PY2(I)*VX24(I)
          F_DEF(I,4)=PX1(I)*VY13(I)+PX2(I)*VY24(I)
        ENDDO
c-----------
      RETURN
      END
