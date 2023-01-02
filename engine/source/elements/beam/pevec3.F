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
Chd|  PEVEC3                        source/elements/beam/pevec3.F 
Chd|-- called by -----------
Chd|        PFORC3                        source/elements/beam/pforc3.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE PEVEC3(RLOC ,R    ,AL   ,NC1  ,NC2 ,
     .                  NC3  ,X1   ,X2   ,X3   ,Y1  ,
     .                  Y2   ,Y3   ,Z1   ,Z2   ,Z3  ,
     .                  RX1G ,RX2G ,RY1G ,RY2G ,RZ1G,
     .                  RZ2G ,E1X  ,E1Y  ,E1Z  ,E2X ,
     .                  E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z )
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
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ)
      my_real
     .   RLOC(3,*),R(3,*),AL(*),X1(MVSIZ),X2(MVSIZ),X3(MVSIZ),
     .   Y1(MVSIZ),Y2(MVSIZ),Y3(MVSIZ),Z1(MVSIZ),Z2(MVSIZ),
     .   Z3(MVSIZ),RX1G(MVSIZ),RX2G(MVSIZ),RY1G(MVSIZ),RY2G(MVSIZ),
     .   RZ1G(MVSIZ),RZ2G(MVSIZ),RX1(MVSIZ),RX2(MVSIZ),RY1(MVSIZ),RY2(MVSIZ),
     .   RZ1(MVSIZ),RZ2(MVSIZ),E1X(MVSIZ),E1Y(MVSIZ),E1Z(MVSIZ),E2X(MVSIZ),
     .   E2Y(MVSIZ),E2Z(MVSIZ),E3X(MVSIZ),E3Y(MVSIZ),E3Z(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
      my_real
     .   SUM(MVSIZ), 
     .   THETA, SUM2(MVSIZ), SUM3(MVSIZ), COST(MVSIZ),
     .   SINT(MVSIZ), R1PHI, R2PHI, THETAPHI, SUM2PHI, SUM3PHI, COPHI,
     .   SIPHI, SINPHI, COSPHI, SUMPHI
C--------------------------------------------
      DO I=LFT,LLT
        RX1G(I)=R(1,NC1(I))
        RY1G(I)=R(2,NC1(I))
        RZ1G(I)=R(3,NC1(I))
        RX2G(I)=R(1,NC2(I))
        RY2G(I)=R(2,NC2(I))
        RZ2G(I)=R(3,NC2(I))
      ENDDO
C
      DO I=LFT,LLT
        E2X(I)=RLOC(1,I)
        E2Y(I)=RLOC(2,I)
        E2Z(I)=RLOC(3,I)
      ENDDO
C
      DO I=LFT,LLT
        E1X(I)=X2(I)-X1(I)
        E1Y(I)=Y2(I)-Y1(I)
        E1Z(I)=Z2(I)-Z1(I)
      ENDDO
C
      DO I=LFT,LLT
        AL(I)=SQRT(E1X(I)**2+E1Y(I)**2+E1Z(I)**2)
      ENDDO
C
      DO I=LFT,LLT
        E1X(I)=E1X(I)/AL(I)
        E1Y(I)=E1Y(I)/AL(I)
        E1Z(I)=E1Z(I)/AL(I)
      ENDDO
C
      DO I=LFT,LLT
        E3X(I)=E1Y(I)*E2Z(I)-E1Z(I)*E2Y(I)
        E3Y(I)=E1Z(I)*E2X(I)-E1X(I)*E2Z(I)
        E3Z(I)=E1X(I)*E2Y(I)-E1Y(I)*E2X(I)
      ENDDO
C
      DO I=LFT,LLT
        E2X(I)=E3Y(I)*E1Z(I)-E3Z(I)*E1Y(I)
        E2Y(I)=E3Z(I)*E1X(I)-E3X(I)*E1Z(I)
        E2Z(I)=E3X(I)*E1Y(I)-E3Y(I)*E1X(I)
      ENDDO
C--------------------------------------------
C     TORSION MOYENNE EN COORDONNEES GLOBALES
C--------------------------------------------
      DO I=LFT,LLT
        RX1(I)=E1X(I)*RX1G(I)+E1Y(I)*RY1G(I)+E1Z(I)*RZ1G(I)
        RX2(I)=E1X(I)*RX2G(I)+E1Y(I)*RY2G(I)+E1Z(I)*RZ2G(I)
        THETA=(RX1(I)+RX2(I))/TWO*DT1
        SUM2(I)=SQRT(E2X(I)**2+E2Y(I)**2+E2Z(I)**2)
        SUM3(I)=SQRT(E3X(I)**2+E3Y(I)**2+E3Z(I)**2)
        COST(I)=COS(THETA)/SUM2(I)
        SINT(I) =SIN(THETA)/SUM3(I)
      ENDDO
C
C ... it is modified.
C
      DO I=LFT,LLT
        E2X(I)=E2X(I)*COST(I)+E3X(I)*SINT(I)
        E2Y(I)=E2Y(I)*COST(I)+E3Y(I)*SINT(I)
        E2Z(I)=E2Z(I)*COST(I)+E3Z(I)*SINT(I)
      ENDDO
C
      DO I=LFT,LLT
        SUM(I)=SQRT(E2X(I)**2+E2Y(I)**2+E2Z(I)**2)
      ENDDO
C
      DO I=LFT,LLT
        E2X(I)=E2X(I)/SUM(I)
        E2Y(I)=E2Y(I)/SUM(I)
        E2Z(I)=E2Z(I)/SUM(I)
      ENDDO
C
      DO I=LFT,LLT
        E3X(I)=E1Y(I)*E2Z(I)-E1Z(I)*E2Y(I)
        E3Y(I)=E1Z(I)*E2X(I)-E1X(I)*E2Z(I)
        E3Z(I)=E1X(I)*E2Y(I)-E1Y(I)*E2X(I)
      ENDDO
C
      DO I=LFT,LLT
        SUM(I)=SQRT(E3X(I)**2+E3Y(I)**2+E3Z(I)**2)
        E3X(I)=E3X(I)/SUM(I)
        E3Y(I)=E3Y(I)/SUM(I)
        E3Z(I)=E3Z(I)/SUM(I)
      ENDDO
C
      DO I=LFT,LLT
        RLOC(1,I)=E2X(I)
        RLOC(2,I)=E2Y(I)
        RLOC(3,I)=E2Z(I)
      ENDDO
C--------------------------------------------
      RETURN
      END
