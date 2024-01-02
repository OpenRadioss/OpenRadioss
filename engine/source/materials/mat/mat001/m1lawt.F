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
Chd|  M1LAWT                        source/materials/mat/mat001/m1lawt.F
Chd|-- called by -----------
Chd|        TFORC3                        source/elements/truss/tforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M1LAWT(
     1   PM,      GEO,     OFF,     FOR,
     2   EINT,    AREA,    AL0,     STI,
     3   MAT,     MGM,     EPS,     AL,
     4   NEL)
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
#include      "param_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER MAT(MVSIZ),MGM(MVSIZ)
      my_real
     .   PM(NPROPM,*),GEO(NPROPG,*),OFF(*),FOR(*),EINT(*),
     .   AREA(*),AL0(*),STI(*),EPS(MVSIZ),AL(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real
     .   PR(MVSIZ),YM(MVSIZ),GAP(MVSIZ),YMA
C-----------------------------------------------      
      DO I=1,NEL
        YM(I)  = PM(20,MAT(I))
        PR(I)  = PM(21,MAT(I))
        GAP(I) = GEO(2,MGM(I))
      ENDDO
c
      DO I=1,NEL
        IF(GAP(I) > ZERO .AND. AL(I) <= (AL0(I)-GAP(I))) OFF(I)=ONE
      ENDDO
c
      DO I=1,NEL
        EINT(I)=EINT(I)+FOR(I)*EPS(I)*AL(I)*DT1*HALF
      ENDDO
c
      DO I=1,NEL
        AREA(I)=AREA(I)*(ONE - TWO*PR(I)*EPS(I)*DT1*OFF(I))
      ENDDO
c
      DO I=1,NEL
        YMA    = YM(I)*AREA(I)
        FOR(I) = FOR(I)+YMA*EPS(I)*DT1
        STI(I) = ZERO
        IF (AL(I) /= ZERO)
     .  STI(I) = YMA / AL(I)
        STI(I) = STI(I)*OFF(I)
        FOR(I) = FOR(I)*OFF(I)
      ENDDO
c
      DO I=1,NEL
        EINT(I)=EINT(I)+FOR(I)*EPS(I)*AL(I)*DT1*HALF
      ENDDO
C-----------------------------------------------
      RETURN
      END
