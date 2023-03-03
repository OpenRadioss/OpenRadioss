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
Chd|  C3EVEC3                       source/elements/sh3n/coque3n/c3evec3.F
Chd|-- called by -----------
Chd|        C3EPSINI                      source/elements/sh3n/coque3n/c3epsini.F
Chd|        C3FINT_REG_INI                source/elements/sh3n/coque3n/c3fint_reg_ini.F
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        CDKINIT3                      source/elements/sh3n/coquedk/cdkinit3.F
Chd|        FSIGTINI                      source/constraints/fxbody/fsigtini.F
Chd|        INIRIG_MAT                    source/elements/initia/inirig_mat.F
Chd|        INIVOID                       source/elements/initia/inivoid.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3EVEC3(JFT ,JLT ,AREA,
     .                   X1  ,X2  ,X3  ,Y1  ,Y2  ,Y3  ,
     .                   Z1  ,Z2  ,Z3  ,E1X ,E2X ,E3X ,
     .                   E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z ,
     .                   X31 ,Y31 ,Z31 ,X2L ,X3L ,Y3L )
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
      my_real X1(MVSIZ), X2(MVSIZ), X3(MVSIZ),
     .        Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ),
     .        Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ),
     .        E1X(MVSIZ), E1Y(MVSIZ), E1Z(MVSIZ),
     .        E2X(MVSIZ), E2Y(MVSIZ), E2Z(MVSIZ),
     .        E3X(MVSIZ), E3Y(MVSIZ), E3Z(MVSIZ),
     .        X31(MVSIZ), Y31(MVSIZ), Z31(MVSIZ),
     .        X32(MVSIZ), Y32(MVSIZ), Z32(MVSIZ),
     .        X21(MVSIZ), Y21(MVSIZ), Z21(MVSIZ), AREA(MVSIZ),
     .        X2L(MVSIZ), X3L(MVSIZ), Y3L(MVSIZ)
C-----------------------------------------------
      INTEGER I
      my_real SUM
C-----------------------------------------------
      DO I=JFT,JLT
        X21(I)=X2(I)-X1(I)
        Y21(I)=Y2(I)-Y1(I)
        Z21(I)=Z2(I)-Z1(I)
        X31(I)=X3(I)-X1(I)
        Y31(I)=Y3(I)-Y1(I)
        Z31(I)=Z3(I)-Z1(I)
        X32(I)=X3(I)-X2(I)
        Y32(I)=Y3(I)-Y2(I)
        Z32(I)=Z3(I)-Z2(I)
      ENDDO
C
      DO I=JFT,JLT
        E1X(I)= X21(I)
        E1Y(I)= Y21(I)
        E1Z(I)= Z21(I)
        X2L(I) = SQRT(E1X(I)*E1X(I)+E1Y(I)*E1Y(I)+E1Z(I)*E1Z(I))
        E1X(I)=E1X(I)/X2L(I)
        E1Y(I)=E1Y(I)/X2L(I)
        E1Z(I)=E1Z(I)/X2L(I)
      ENDDO
C
      DO I=JFT,JLT
        E3X(I)=Y31(I)*Z32(I)-Z31(I)*Y32(I)
        E3Y(I)=Z31(I)*X32(I)-X31(I)*Z32(I)
        E3Z(I)=X31(I)*Y32(I)-Y31(I)*X32(I)
        SUM = SQRT(E3X(I)*E3X(I)+E3Y(I)*E3Y(I)+E3Z(I)*E3Z(I))
        E3X(I)=E3X(I)/SUM
        E3Y(I)=E3Y(I)/SUM
        E3Z(I)=E3Z(I)/SUM
        AREA(I) = HALF * SUM
      ENDDO
C
      DO I=JFT,JLT
        E2X(I)=E3Y(I)*E1Z(I)-E3Z(I)*E1Y(I)
        E2Y(I)=E3Z(I)*E1X(I)-E3X(I)*E1Z(I)
        E2Z(I)=E3X(I)*E1Y(I)-E3Y(I)*E1X(I)
        SUM = SQRT(E2X(I)*E2X(I)+E2Y(I)*E2Y(I)+E2Z(I)*E2Z(I))
        E2X(I)=E2X(I)/SUM
        E2Y(I)=E2Y(I)/SUM
        E2Z(I)=E2Z(I)/SUM
        Y3L(I)=E2X(I)*X31(I)+E2Y(I)*Y31(I)+E2Z(I)*Z31(I)
        X3L(I)=E1X(I)*X31(I)+E1Y(I)*Y31(I)+E1Z(I)*Z31(I)
      ENDDO
C-----------
      RETURN
      END
