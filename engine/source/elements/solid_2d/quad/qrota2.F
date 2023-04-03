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
Chd|  QROTA2                        source/elements/solid_2d/quad/qrota2.F
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QROTA2(
     1   SIG,     S1,      S2,      S3,
     2   S4,      S5,      S6,      WYZ,
     3   NEL,     JCVT)
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
      INTEGER, INTENT(IN) :: JCVT
      INTEGER NEL
C     REAL
      my_real
     .   SIG(NEL,6),S1(*), S2(*), S3(*), S4(*), S5(*), S6(*), WYZ(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .    Q1(MVSIZ)
C-----------------------------------------------
      DO 10 I=1,NEL
      S1(I)=SIG(I,1)
      S2(I)=SIG(I,2)
      S3(I)=SIG(I,3)
      S4(I)=SIG(I,4)
      S5(I)=SIG(I,5)
   10 S6(I)=SIG(I,6)
C
      IF(JCVT/=0) RETURN
C---------------------------------
      DO 20 I=1,NEL
   20 Q1(I)=TWO*S4(I)*WYZ(I)
C
      DO 30 I=1,NEL
      SIG(I,1)=S1(I)+Q1(I)
      SIG(I,2)=S2(I)-Q1(I)
   30 SIG(I,4)=S4(I)+WYZ(I)*(S2(I)-S1(I))
C
      RETURN
      END
