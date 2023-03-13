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
Chd|  QRROTA2                       source/elements/solid_2d/quad/qrrota2.F
Chd|-- called by -----------
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QRROTA2(
     .   R22, R23, R32, R33,
     .   F11, F21, F12, F22,
     .   T11, T21, T12, T22,
     .   T13, T23, T14, T24)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   R22(*), R23(*), R32(*), R33(*),
     .   F11(*), F21(*), F12(*), F22(*),
     .   T11(*), T21(*), T12(*), T22(*),
     .   T13(*), T23(*), T14(*), T24(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   Y,Z
C-----------------------------------------------
C     X=RX' <=> X'=t(R)X chgt de base.
      DO I=LFT,LLT
        Y=R22(I)*F11(I)+R32(I)*F21(I)
        Z=R23(I)*F11(I)+R33(I)*F21(I)
        F11(I)=Y
        F21(I)=Z
      END DO
      DO I=LFT,LLT
        Y=R22(I)*F12(I)+R32(I)*F22(I)
        Z=R23(I)*F12(I)+R33(I)*F22(I)
        F12(I)=Y
        F22(I)=Z
      END DO
      DO I=LFT,LLT
        Y=R22(I)*T11(I)+R32(I)*T21(I)
        Z=R23(I)*T11(I)+R33(I)*T21(I)
        T11(I)=Y
        T21(I)=Z
      END DO
      DO I=LFT,LLT
        Y=R22(I)*T12(I)+R32(I)*T22(I)
        Z=R23(I)*T12(I)+R33(I)*T22(I)
        T12(I)=Y
        T22(I)=Z
      END DO
      DO I=LFT,LLT
        Y=R22(I)*T13(I)+R32(I)*T23(I)
        Z=R23(I)*T13(I)+R33(I)*T23(I)
        T13(I)=Y
        T23(I)=Z
      END DO
      DO I=LFT,LLT
        Y=R22(I)*T14(I)+R32(I)*T24(I)
        Z=R23(I)*T14(I)+R33(I)*T24(I)
        T14(I)=Y
        T24(I)=Z
      END DO
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      RETURN
      END
