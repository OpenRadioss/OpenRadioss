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
Chd|  QLAGR2                        source/elements/solid_2d/quad/qlagr2.F
Chd|-- called by -----------
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QLAGR2(
     1   PM,      VOL0,    RHO,     EINT,
     2   VOLN,    DVOL,    MAT,     NEL)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
C     REAL
      my_real
     .   PM(NPROPM,*), VOL0(*), RHO(*), EINT(*),
     .   VOLN(*), DVOL(*)
      INTEGER MAT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   RHO0(MVSIZ)
C-----------------------------------------------
      DO 10 I=1,NEL
   10 RHO0(I)=PM(1,MAT(I))
C
      DO 20 I=1,NEL
   20 DVOL(I)=VOLN(I)-(RHO0(I)/RHO(I))*VOL0(I)
C
      DO 30 I=1,NEL
      RHO(I)=RHO0(I)*(VOL0(I)/VOLN(I))
   30 EINT(I)=EINT(I)*VOL0(I)
C
      RETURN
      END
