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
Chd|  S8SDEFO3                      source/elements/solid/solide8s/s8sdefo3.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8SDEFO3(
     1   ULX1,    ULX2,    ULX3,    ULX4,
     2   ULX5,    ULX6,    ULX7,    ULX8,
     3   ULY1,    ULY2,    ULY3,    ULY4,
     4   ULY5,    ULY6,    ULY7,    ULY8,
     5   ULZ1,    ULZ2,    ULZ3,    ULZ4,
     6   ULZ5,    ULZ6,    ULZ7,    ULZ8,
     7   DN_X,    DN_Y,    DN_Z,    DXX,
     8   DXY,     DXZ,     DYX,     DYY,
     9   DYZ,     DZX,     DZY,     DZZ,
     A   D4,      D5,      D6,      WXX,
     B   WYY,     WZZ,     VOLO,    OFF,
     C   EINT,    OFFS,    DSV,     ICP,
     D   FAC,     SDV,     I_SH,    IDEGE,
     E   NEL,     ISMSTR)
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
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER ICP,I_SH,IDEGE(*)
C     REAL
      my_real
     .   DXX(*), DXY(*), DXZ(*),
     .   DYX(*), DYY(*), DYZ(*),
     .   DZX(*), DZY(*), DZZ(*), D4(*), D5(*), D6(*),
     .   WXX(*), WYY(*), WZZ(*), OFFS(*),DSV(*),
     .   VOLO(*),OFF(*),EINT(*),FAC(*),SDV(*),
     .   DN_X(MVSIZ,8),DN_Y(MVSIZ,8),DN_Z(MVSIZ,8),
     .   ULX1(MVSIZ), ULX2(MVSIZ), ULX3(MVSIZ), ULX4(MVSIZ),
     .   ULX5(MVSIZ), ULX6(MVSIZ), ULX7(MVSIZ), ULX8(MVSIZ),
     .   ULY1(MVSIZ), ULY2(MVSIZ), ULY3(MVSIZ), ULY4(MVSIZ),
     .   ULY5(MVSIZ), ULY6(MVSIZ), ULY7(MVSIZ), ULY8(MVSIZ),
     .   ULZ1(MVSIZ), ULZ2(MVSIZ), ULZ3(MVSIZ), ULZ4(MVSIZ),
     .   ULZ5(MVSIZ), ULZ6(MVSIZ), ULZ7(MVSIZ), ULZ8(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   DT1D2,DVC(MVSIZ),TOL,DT1D,UDT1
      DOUBLE PRECISION
     .   DVP,DV,DV1
C                                                                     12
      TOL = ONE-EM20
C-----------------------------------------------
      DO I=1,NEL
        DXX(I)=DN_X(I,1)*ULX1(I)+DN_X(I,2)*ULX2(I)
     .        +DN_X(I,3)*ULX3(I)+DN_X(I,4)*ULX4(I)
     .        +DN_X(I,5)*ULX5(I)+DN_X(I,6)*ULX6(I)
     .        +DN_X(I,7)*ULX7(I)+DN_X(I,8)*ULX8(I)
        DYY(I)=DN_Y(I,1)*ULY1(I)+DN_Y(I,2)*ULY2(I)
     .        +DN_Y(I,3)*ULY3(I)+DN_Y(I,4)*ULY4(I)
     .        +DN_Y(I,5)*ULY5(I)+DN_Y(I,6)*ULY6(I)
     .        +DN_Y(I,7)*ULY7(I)+DN_Y(I,8)*ULY8(I)
        DZZ(I)=DN_Z(I,1)*ULZ1(I)+DN_Z(I,2)*ULZ2(I)
     .        +DN_Z(I,3)*ULZ3(I)+DN_Z(I,4)*ULZ4(I)
     .        +DN_Z(I,5)*ULZ5(I)+DN_Z(I,6)*ULZ6(I)
     .        +DN_Z(I,7)*ULZ7(I)+DN_Z(I,8)*ULZ8(I)
        DXY(I)=DN_Y(I,1)*ULX1(I)+DN_Y(I,2)*ULX2(I)
     .        +DN_Y(I,3)*ULX3(I)+DN_Y(I,4)*ULX4(I)
     .        +DN_Y(I,5)*ULX5(I)+DN_Y(I,6)*ULX6(I)
     .        +DN_Y(I,7)*ULX7(I)+DN_Y(I,8)*ULX8(I)
        DYX(I)=DN_X(I,1)*ULY1(I)+DN_X(I,2)*ULY2(I)
     .        +DN_X(I,3)*ULY3(I)+DN_X(I,4)*ULY4(I)
     .        +DN_X(I,5)*ULY5(I)+DN_X(I,6)*ULY6(I)
     .        +DN_X(I,7)*ULY7(I)+DN_X(I,8)*ULY8(I)
        DYZ(I)=DN_Z(I,1)*ULY1(I)+DN_Z(I,2)*ULY2(I)
     .        +DN_Z(I,3)*ULY3(I)+DN_Z(I,4)*ULY4(I)
     .        +DN_Z(I,5)*ULY5(I)+DN_Z(I,6)*ULY6(I)
     .        +DN_Z(I,7)*ULY7(I)+DN_Z(I,8)*ULY8(I)
        DZY(I)=DN_Y(I,1)*ULZ1(I)+DN_Y(I,2)*ULZ2(I)
     .        +DN_Y(I,3)*ULZ3(I)+DN_Y(I,4)*ULZ4(I)
     .        +DN_Y(I,5)*ULZ5(I)+DN_Y(I,6)*ULZ6(I)
     .        +DN_Y(I,7)*ULZ7(I)+DN_Y(I,8)*ULZ8(I)
        DZX(I)=DN_X(I,1)*ULZ1(I)+DN_X(I,2)*ULZ2(I)
     .        +DN_X(I,3)*ULZ3(I)+DN_X(I,4)*ULZ4(I)
     .        +DN_X(I,5)*ULZ5(I)+DN_X(I,6)*ULZ6(I)
     .        +DN_X(I,7)*ULZ7(I)+DN_X(I,8)*ULZ8(I)
        DXZ(I)=DN_Z(I,1)*ULX1(I)+DN_Z(I,2)*ULX2(I)
     .        +DN_Z(I,3)*ULX3(I)+DN_Z(I,4)*ULX4(I)
     .        +DN_Z(I,5)*ULX5(I)+DN_Z(I,6)*ULX6(I)
     .        +DN_Z(I,7)*ULX7(I)+DN_Z(I,8)*ULX8(I)
        DVC(I) =ZERO
      ENDDO
C-------------Volume traitement------
      IF (ICP==2) THEN
        DO I=1,NEL
          DVC(I)=(DSV(I)-DVC(I))*FAC(I)*DT1
        ENDDO
      ELSEIF (ICP==1) THEN
        DO I=1,NEL
          DVC(I)=(DSV(I)-DVC(I))*DT1
        ENDDO
      ENDIF
      IF ((ICP>0.AND.ISMSTR/=10.AND.ISMSTR/=12).OR.I_SH>1) THEN
        DO I=1,NEL
          DV =DVC(I)*OFF(I)
          IF(IDEGE(I)>10) DV = ZERO
          SDV(I) =DV
          IF (DV>TOL) THEN
            DV =ZERO
          ENDIF
          IF(OFFS(I)==TWO.OR.ISMSTR==11) CYCLE
          DV1 = ONE- DV
          VOLO(I) = VOLO(I)*DV1
          EINT(I) = EINT(I)/DV1
c        ENDIF
        ENDDO
      ELSE
        DO I=1,NEL
          SDV(I) =ZERO
        ENDDO
      ENDIF
C
      DT1D2=HALF*DT1
      IF (ISCAU>0)DT1D2=DT1
      DT1D=TWO*DT1D2
      DO I=1,NEL
        D4(I)=DXY(I)+DYX(I)
        D5(I)=DYZ(I)+DZY(I)
        D6(I)=DXZ(I)+DZX(I)
        WZZ(I)=DT1D2*(DYX(I)-DXY(I))
        WYY(I)=DT1D2*(DXZ(I)-DZX(I))
        WXX(I)=DT1D2*(DZY(I)-DYZ(I))
      ENDDO
      UDT1 = ONE
      IF (DT1 > ZERO) UDT1 = ONE/DT1
      DO I=1,NEL
        DXX(I)= DXX(I)*UDT1
        DYY(I)= DYY(I)*UDT1
        DZZ(I)= DZZ(I)*UDT1
        D4(I) = D4(I)*UDT1
        D5(I) = D5(I)*UDT1
        D6(I) = D6(I)*UDT1
      ENDDO
C
      RETURN
      END
