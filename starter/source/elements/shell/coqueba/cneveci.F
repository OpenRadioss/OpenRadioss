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
Chd|  CNEVECI                       source/elements/shell/coqueba/cneveci.F
Chd|-- called by -----------
Chd|        CBAFINT_REG_INI               source/elements/shell/coqueba/cbafint_reg_ini.F
Chd|        CBAINIT3                      source/elements/shell/coqueba/cbainit3.F
Chd|        CNEPSINI                      source/elements/shell/coqueba/cnepsini.F
Chd|-- calls ---------------
Chd|        CLSKEW3                       source/elements/shell/coque/clskew.F
Chd|====================================================================
      SUBROUTINE CNEVECI(JFT ,JLT ,AREA,
     .           X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .           Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .           E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z )
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
#include      "scr21_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT,JLT
      my_real, DIMENSION(MVSIZ), INTENT(IN)  ::
     .                           X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4
      my_real, DIMENSION(MVSIZ), INTENT(OUT) :: AREA,
     .                   E1X, E2X, E3X, E1Y, E2Y, E3Y ,E1Z, E2Z, E3Z
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,I1
      my_real, DIMENSION(MVSIZ)  :: X21,Y21,Z21,X31,Y31,Z31,X42,Y42,Z42,
     .                              RX, RY, RZ, SX, SY, SZ,SUM
C=======================================================================
      DO I=JFT,JLT
        X21(I)=X2(I)-X1(I)
        Y21(I)=Y2(I)-Y1(I)
        Z21(I)=Z2(I)-Z1(I)
        X31(I)=X3(I)-X1(I)
        Y31(I)=Y3(I)-Y1(I)
        Z31(I)=Z3(I)-Z1(I)
        X42(I)=X4(I)-X2(I)
        Y42(I)=Y4(I)-Y2(I)
        Z42(I)=Z4(I)-Z2(I)
      ENDDO
C
      DO I=JFT,JLT
        RX(I)=X31(I)-X42(I)
        RY(I)=Y31(I)-Y42(I)
        RZ(I)=Z31(I)-Z42(I)
        SX(I)=X31(I)+X42(I)
        SY(I)=Y31(I)+Y42(I)
        SZ(I)=Z31(I)+Z42(I)
      ENDDO
C----------------------------
C     LOCAL SYSTEM
C----------------------------
      I1 = 0
      CALL CLSKEW3(JFT,JLT,I1,
     .   RX, RY, RZ,
     .   SX, SY, SZ,
     .   E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,SUM)
C
      DO I=JFT,JLT
        AREA(I) = FOURTH*SUM(I)
      ENDDO
c-----------
      RETURN
      END
