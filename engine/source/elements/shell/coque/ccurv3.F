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
Chd|  CCURV3                        source/elements/shell/coque/ccurv3.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CCURV3(JFT,JLT,VRL1,VRL2,VRL3,VRL4,
     1   PX1,PX2 ,PY1 ,PY2 ,AREA,
     2   RX1 ,RX2 ,RX3 ,RX4 ,RY1 ,RY2 ,RY3 ,RY4,
     3   E1X ,E1Y ,E1Z ,E2X ,E2Y ,E2Z ,E3X ,E3Y,
     4   E3Z ,KXX ,KYY ,KXY ,EXZ,EYZ,
     5   WXY ,ISMSTR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, ISMSTR
      my_real
     .     RX1(MVSIZ),  RX2(MVSIZ),  RX3(MVSIZ),  RX4(MVSIZ),
     .     RY1(MVSIZ),  RY2(MVSIZ),  RY3(MVSIZ),  RY4(MVSIZ),
     .     PX1(MVSIZ),  PX2(MVSIZ),  PY1(MVSIZ),  PY2(MVSIZ),
     .     E1X(MVSIZ),  E1Y(MVSIZ),  E1Z(MVSIZ),  E2X(MVSIZ),
     .     E2Y(MVSIZ),  E2Z(MVSIZ),  E3X(MVSIZ),  E3Y(MVSIZ),
     .     E3Z(MVSIZ),
     .     VRL1(MVSIZ,3), VRL2(MVSIZ,3), VRL3(MVSIZ,3), VRL4(MVSIZ,3),
     .     KXX(MVSIZ),  KYY(MVSIZ),
     .     KXY(MVSIZ),  AREA(MVSIZ), EXZ(MVSIZ),  EYZ(MVSIZ),WXY(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
      my_real
     .     RXAVT(MVSIZ), RX13T(MVSIZ), RX24T(MVSIZ), RY13T(MVSIZ),
     .     RYAVT(MVSIZ), RY24T(MVSIZ),
     .     RXAVTA,RX13TA,RX24TA,RY13TA,RYAVTA,RY24TA
C=======================================================================
      DO I=JFT,JLT
      RX1(I)=E1X(I)*VRL1(I,1)+E1Y(I)*VRL1(I,2)+E1Z(I)*VRL1(I,3)
      RX2(I)=E1X(I)*VRL2(I,1)+E1Y(I)*VRL2(I,2)+E1Z(I)*VRL2(I,3)
      RX3(I)=E1X(I)*VRL3(I,1)+E1Y(I)*VRL3(I,2)+E1Z(I)*VRL3(I,3)
      RX4(I)=E1X(I)*VRL4(I,1)+E1Y(I)*VRL4(I,2)+E1Z(I)*VRL4(I,3)
      RY1(I)=E2X(I)*VRL1(I,1)+E2Y(I)*VRL1(I,2)+E2Z(I)*VRL1(I,3)
      RY2(I)=E2X(I)*VRL2(I,1)+E2Y(I)*VRL2(I,2)+E2Z(I)*VRL2(I,3)
      RY3(I)=E2X(I)*VRL3(I,1)+E2Y(I)*VRL3(I,2)+E2Z(I)*VRL3(I,3)
      RY4(I)=E2X(I)*VRL4(I,1)+E2Y(I)*VRL4(I,2)+E2Z(I)*VRL4(I,3)
      ENDDO
C
      DO I=JFT,JLT
        RX13TA =RX1(I)-RX3(I)
        RXAVTA =RX1(I)+RX2(I)+RX3(I)+RX4(I)
        RX24TA =RX2(I)-RX4(I)
C
        KYY(I)=-PY1(I)*RX13TA-PY2(I)*RX24TA
        KXY(I)= PX1(I)*RX13TA+PX2(I)*RX24TA
C
        RY13TA = RY1(I)-RY3(I)
        RYAVTA = RY1(I)+RY2(I)+RY3(I)+RY4(I)
        RY24TA = RY2(I)-RY4(I)
C
        KXX(I)= PX1(I)*RY13TA+PX2(I)*RY24TA
        KXY(I)= PY1(I)*RY13TA+PY2(I)*RY24TA
     +       -KXY(I)
C
        EXZ(I)=EXZ(I)+RYAVTA*(.25*AREA(I))
        EYZ(I)=EYZ(I)-RXAVTA*(.25*AREA(I))
      ENDDO
C
C      
       IF (ISMSTR==10) THEN
        DO I=JFT,JLT
        RX13TA =RX1(I)-RX3(I)
        RX24TA =RX2(I)-RX4(I)
C
        RY13TA = RY1(I)-RY3(I)
        RY24TA = RY2(I)-RY4(I)
C
         WXY(I) = PY1(I)*RY13TA+PY2(I)*RY24TA +
     +            PX1(I)*RX13TA+PX2(I)*RX24TA
        ENDDO
       ENDIF
c-----------
      RETURN
      END
