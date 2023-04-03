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
Chd|  Q4DERI2                       source/elements/solid_2d/quad4/q4deri2.F
Chd|-- called by -----------
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|        Q4KE2                         source/elements/solid_2d/quad4/q4ke2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE Q4DERI2(
     1   OFFG,    OFF,     KSI,     ETA,
     2   WI,      YAVG,    Y12,     Y34,
     3   Y13,     Y24,     Y14,     Y23,
     4   Z12,     Z34,     Z13,     Z24,
     5   Z14,     Z23,     PY1,     PY2,
     6   PY3,     PY4,     PZ1,     PZ2,
     7   PZ3,     PZ4,     PYC1,    PYC2,
     8   PZC1,    PZC2,    BYZ1,    BYZ2,
     9   BYZ3,    BYZ4,    BZY1,    BZY2,
     A   BZY3,    BZY4,    AIRN,    VOLN,
     B   NU,      NEL,     JHBE)
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
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: JHBE
C     REAL
      my_real
     .    OFFG(*),OFF(*),KSI,ETA,WI,YAVG(*),
     .    Y12(*),Y34(*),Y13(*),Y24(*),Y14(*),Y23(*),
     .    Z12(*),Z34(*),Z13(*),Z24(*),Z14(*),Z23(*),
     .    PY1(*),PY2(*),PY3(*),PY4(*),
     .    PZ1(*),PZ2(*),PZ3(*),PZ4(*),
     .    PZC1(*),PZC2(*),PYC1(*),PYC2(*),
     .    BYZ1(*),BYZ2(*),BYZ3(*),BYZ4(*),
     .    BZY1(*),BZY2(*),BZY3(*),BZY4(*),
     .    AIRN(*),VOLN(*),NU(*)
C-----------------------------------------------
c FUNCTION: 
c ARGUMENTS:  (I: input, O: output, IO: input & output, W: workspace)
c TYPE NAME                FUNCTION
c  I   OFFG               - ELEMENT D/A FLAG, OVERALL
c  IO  OFF                - ELEMENT D/A FLAG, OF POINT
c  I   KSI,ETA,WI         - NATURAL COORDINATES, WEIGHT
c  O   YAVG(*)            - SUMMERY OF "Y" OF THE FOUR NODES
c  I   Y12(*)~Z23(*)      - DIFFERENCE: Yi-Yj, Zi-Zj
c  O   PY1(*)~PZ4(*)      - SHAPE DERIVATIVES (dNi/dY, dNi/dZ)
c  O   AIRN(*)            - W*|J|
c  O   VOLN(*)            - W*|J| FOR PLAIN CASE; r'*W*|J| FOR AXISYMMETRIC CASE
c  O   RX(*)~TZ(*)        - JACOBIAN MATRIX [J]
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .    DNDK1,DNDK2,DNDK3,DNDK4,
     .    DNDE1,DNDE2,DNDE3,DNDE4,
     .    DYDK(MVSIZ),DYDE(MVSIZ),DZDK(MVSIZ),DZDE(MVSIZ),
     .    DET(MVSIZ),YH(MVSIZ),ZH(MVSIZ)
      my_real
     .    QN1,QN2,QN3,QN4,YD,DETI,NU1
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
C     4.*(dNi/dK, dNi/dE)
      DNDK1 = ETA - ONE
      DNDK3 = ETA + ONE
      DNDK2 = - DNDK1
      DNDK4 = - DNDK3
      DNDE1 = KSI - ONE
      DNDE3 = KSI + ONE
      DNDE2 = - DNDE3
      DNDE4 = - DNDE1
C
      DO I=1,NEL
C       4.*(dY/dK, dY/dE, dZ/dK, dZ/dE)
        DYDK(I) = (Y14(I)-Y23(I))*ETA-Y13(I)+Y24(I)
        DYDE(I) = (Y12(I)+Y34(I))*KSI-Y13(I)-Y24(I)
        DZDK(I) = (Z14(I)-Z23(I))*ETA-Z13(I)+Z24(I)
        DZDE(I) = (Z12(I)+Z34(I))*ETA-Z13(I)-Z24(I)
C       8.*|J|
        DET(I) = (Y34(I)*Z12(I)-Y12(I)*Z34(I))*KSI +
     .           (Y23(I)*Z14(I)-Y14(I)*Z23(I))*ETA +
     .           Y13(I)*Z24(I)-Y24(I)*Z13(I)
C       W*|J|
        VOLN(I) = ONE_OVER_8*DET(I)*WI
        AIRN(I) = VOLN(I)
C       dNi/dY, dNi/dZ
C------add something later for DET=~ zero-----
        DETI=HALF/DET(I)
        PY1(I) = DETI*(DZDE(I)*DNDK1-DZDK(I)*DNDE1)
        PY2(I) = DETI*(DZDE(I)*DNDK2-DZDK(I)*DNDE2)
        PY3(I) = DETI*(DZDE(I)*DNDK3-DZDK(I)*DNDE3)
        PY4(I) = DETI*(DZDE(I)*DNDK4-DZDK(I)*DNDE4)
        PZ1(I) = DETI*(DYDK(I)*DNDE1-DYDE(I)*DNDK1)
        PZ2(I) = DETI*(DYDK(I)*DNDE2-DYDE(I)*DNDK2)
        PZ3(I) = DETI*(DYDK(I)*DNDE3-DYDE(I)*DNDK3)
        PZ4(I) = DETI*(DYDK(I)*DNDE4-DYDE(I)*DNDK4)
      ENDDO
C
      IF(N2D==1) THEN
        IF(JHBE==17) THEN
         DO I=1,NEL
            VOLN(I) = FOURTH*YAVG(I)*VOLN(I)
         ENDDO
        ENDIF
      ENDIF
C
      DO I=1,NEL
        OFF(I) = OFFG(I)
        IF(VOLN(I)<=ZERO .AND. OFF(I)/=ZERO) THEN
          VOLN(I) = EM20
          OFF(I) =ZERO
        ENDIF
      ENDDO
C----Assumed strain      
      DO I=1,NEL
        BYZ1(I) = -NU(I)*(PZ1(I) - PZC1(I))
        BYZ2(I) = -NU(I)*(PZ2(I) - PZC2(I))
        BYZ3(I) = -NU(I)*(PZ3(I) + PZC1(I))
        BYZ4(I) = -NU(I)*(PZ4(I) + PZC2(I))
        BZY1(I) = -NU(I)*(PY1(I) - PYC1(I))
        BZY2(I) = -NU(I)*(PY2(I) - PYC2(I))
        BZY3(I) = -NU(I)*(PY3(I) + PYC1(I))
        BZY4(I) = -NU(I)*(PY4(I) + PYC2(I))
      ENDDO
      DO I=1,NEL
        NU1 = ONE-NU(I)
        PY1(I) = PYC1(I) + NU1*(PY1(I) - PYC1(I))
        PY2(I) = PYC2(I) + NU1*(PY2(I) - PYC2(I))
        PY3(I) =-PYC1(I) + NU1*(PY3(I) + PYC1(I))
        PY4(I) =-PYC2(I) + NU1*(PY4(I) + PYC2(I))
        PZ1(I) = PZC1(I) + NU1*(PZ1(I) - PZC1(I))
        PZ2(I) = PZC2(I) + NU1*(PZ2(I) - PZC2(I))
        PZ3(I) =-PZC1(I) + NU1*(PZ3(I) + PZC1(I))
        PZ4(I) =-PZC2(I) + NU1*(PZ4(I) + PZC2(I))
      ENDDO
C
      RETURN
      END
	  