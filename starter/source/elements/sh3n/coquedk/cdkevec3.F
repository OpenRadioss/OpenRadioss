Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  CDKEVEC3                      source/elements/sh3n/coquedk/cdkevec3.F
Chd|-- called by -----------
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        C3NEWVE3                      source/elements/sh3n/coque3n/c3epsini.F
Chd|        CDKEPSINI                     source/elements/sh3n/coquedk/cdkepsini.F
Chd|        CDKFINT_REG_INI               source/elements/sh3n/coquedk/cdkfint_reg_ini.F
Chd|        CDKINIT3                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|-- calls ---------------
Chd|        CLSKEW3                       source/elements/shell/coque/clskew.F
Chd|====================================================================
      SUBROUTINE CDKEVEC3(JFT ,JLT ,AREA,
     .                    X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .                    Z1  ,Z2  ,Z3  ,E1X ,E2X ,E3X ,
     .                    E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
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
      INTEGER JFT, JLT
      my_real AREA(MVSIZ),
     .     X1(MVSIZ), X2(MVSIZ), X3(MVSIZ),
     .     Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ),
     .     Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ),
     .     E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .     E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .     E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,I1
      my_real, DIMENSION(MVSIZ) ::  SUM, RX, RY, RZ, SX, SY, SZ
C=======================================================================
      DO I=JFT,JLT
        RX(I) = X2(I) - X1(I)
        RY(I) = Y2(I) - Y1(I)
        RZ(I) = Z2(I) - Z1(I)
        SX(I) = X3(I) - X1(I)
        SY(I) = Y3(I) - Y1(I)
        SZ(I) = Z3(I) - Z1(I)
      ENDDO
C----------------------------
C     LOCAL SYSTEM
C----------------------------
      I1 = 0
      CALL CLSKEW3(JFT,JLT,I1,
     .     RX, RY, RZ, SX, SY, SZ, 
     .     E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,SUM)
C
      DO I=JFT,JLT
        AREA(I) = HALF*SUM(I)
      ENDDO
c-----------
      RETURN
      END
