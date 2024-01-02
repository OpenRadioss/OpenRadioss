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
Chd|  Q4KEL2                        source/elements/solid_2d/quad4/q4kel2.F
Chd|-- called by -----------
Chd|        Q4KE2                         source/elements/solid_2d/quad4/q4ke2.F
Chd|-- calls ---------------
Chd|        Q4KELIJ2                      source/elements/solid_2d/quad4/q4kelij2.F
Chd|        Q4KELIJS2                     source/elements/solid_2d/quad4/q4kelijs2.F
Chd|====================================================================
      SUBROUTINE Q4KEL2(
     1   PY1,     PY2,     PY3,     PY4,
     2   PZ1,     PZ2,     PZ3,     PZ4,
     3   PYC1,    PYC2,    PZC1,    PZC2,
     4   AY,      R22,     R23,     K11,
     5   K12,     K13,     K14,     K22,
     6   K23,     K24,     K33,     K34,
     7   K44,     K11U,    K12U,    K13U,
     8   K14U,    K22U,    K23U,    K24U,
     9   K33U,    K34U,    K44U,    K11L,
     A   K12L,    K13L,    K14L,    K22L,
     B   K23L,    K24L,    K33L,    K34L,
     C   K44L,    DD,      GG,      DG,
     D   G33,     IKSUP,   ICP,     OFF,
     E   NEL,     JCVT)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: JCVT
      INTEGER, INTENT(IN) :: NEL
      INTEGER ICP, IKSUP
      my_real
     .    PY1(*), PY2(*), PY3(*), PY4(*), 
     .    PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .    PYC1(*), PYC2(*), PZC1(*), PZC2(*), AY(*), R22(*), R23(*),
     .    K11(2,2,*), K12(2,2,*), K13(2,2,*), K14(2,2,*), K22(2,2,*), 
     .    K23(2,2,*), K24(2,2,*), K33(2,2,*), K34(2,2,*), K44(2,2,*), 
     .    K11U(2,2,*),K12U(2,2,*),K13U(2,2,*),K14U(2,2,*),K22U(2,2,*), 
     .    K23U(2,2,*),K24U(2,2,*),K33U(2,2,*),K34U(2,2,*),K44U(2,2,*), 
     .    K11L(2,2,*),K12L(2,2,*),K13L(2,2,*),K14L(2,2,*),K22L(2,2,*), 
     .    K23L(2,2,*),K24L(2,2,*),K33L(2,2,*),K34L(2,2,*),K44L(2,2,*), 
     .    DD(3,3,*), GG(*), DG(3,3,*), G33(3,3,*), OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, IS
      my_real
     .    PYC3(MVSIZ), PYC4(MVSIZ), PZC3(MVSIZ), PZC4(MVSIZ)
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO I=1,NEL
        PYC3(I) = -PYC1(I)
        PYC4(I) = -PYC2(I)
        PZC3(I) = -PZC1(I)
        PZC4(I) = -PZC2(I)
      ENDDO
C
      IF (IKSUP==0) THEN
        IS = 1
        CALL Q4KELIJS2(
     1   PY1,     PZ1,     PY1,     PZ1,
     2   PYC1,    PZC1,    PYC1,    PZC1,
     3   AY,      R22,     R23,     DD,
     4   GG,      K11,     K11U,    K11L,
     5   IS,      NEL,     JCVT)
        CALL Q4KELIJS2(
     1   PY2,     PZ2,     PY2,     PZ2,
     2   PYC2,    PZC2,    PYC2,    PZC2,
     3   AY,      R22,     R23,     DD,
     4   GG,      K22,     K22U,    K22L,
     5   IS,      NEL,     JCVT)
        CALL Q4KELIJS2(
     1   PY3,     PZ3,     PY3,     PZ3,
     2   PYC3,    PZC3,    PYC3,    PZC3,
     3   AY,      R22,     R23,     DD,
     4   GG,      K33,     K33U,    K33L,
     5   IS,      NEL,     JCVT)
        CALL Q4KELIJS2(
     1   PY4,     PZ4,     PY4,     PZ4,
     2   PYC4,    PZC4,    PYC4,    PZC4,
     3   AY,      R22,     R23,     DD,
     4   GG,      K44,     K44U,    K44L,
     5   IS,      NEL,     JCVT)
        IS = 0
        CALL Q4KELIJS2(
     1   PY1,     PZ1,     PY2,     PZ2,
     2   PYC1,    PZC1,    PYC2,    PZC2,
     3   AY,      R22,     R23,     DD,
     4   GG,      K12,     K12U,    K12L,
     5   IS,      NEL,     JCVT)
        CALL Q4KELIJS2(
     1   PY1,     PZ1,     PY3,     PZ3,
     2   PYC1,    PZC1,    PYC3,    PZC3,
     3   AY,      R22,     R23,     DD,
     4   GG,      K13,     K13U,    K13L,
     5   IS,      NEL,     JCVT)
        CALL Q4KELIJS2(
     1   PY1,     PZ1,     PY4,     PZ4,
     2   PYC1,    PZC1,    PYC4,    PZC4,
     3   AY,      R22,     R23,     DD,
     4   GG,      K14,     K14U,    K14L,
     5   IS,      NEL,     JCVT)
        CALL Q4KELIJS2(
     1   PY2,     PZ2,     PY3,     PZ3,
     2   PYC2,    PZC2,    PYC3,    PZC3,
     3   AY,      R22,     R23,     DD,
     4   GG,      K23,     K23U,    K23L,
     5   IS,      NEL,     JCVT)
        CALL Q4KELIJS2(
     1   PY2,     PZ2,     PY4,     PZ4,
     2   PYC2,    PZC2,    PYC4,    PZC4,
     3   AY,      R22,     R23,     DD,
     4   GG,      K24,     K24U,    K24L,
     5   IS,      NEL,     JCVT)
        CALL Q4KELIJS2(
     1   PY3,     PZ3,     PY4,     PZ4,
     2   PYC3,    PZC3,    PYC4,    PZC4,
     3   AY,      R22,     R23,     DD,
     4   GG,      K34,     K34U,    K34L,
     5   IS,      NEL,     JCVT)
      ELSE
        IS = 1
        CALL Q4KELIJ2(
     1   PY1,     PZ1,     PY1,     PZ1,
     2   PYC1,    PZC1,    PYC1,    PZC1,
     3   AY,      R22,     R23,     DD,
     4   DG,      G33,     K11,     IS,
     5   NEL,     JCVT)
        CALL Q4KELIJ2(
     1   PY2,     PZ2,     PY2,     PZ2,
     2   PYC2,    PZC2,    PYC2,    PZC2,
     3   AY,      R22,     R23,     DD,
     4   DG,      G33,     K22,     IS,
     5   NEL,     JCVT)
        CALL Q4KELIJ2(
     1   PY3,     PZ3,     PY3,     PZ3,
     2   PYC3,    PZC3,    PYC3,    PZC3,
     3   AY,      R22,     R23,     DD,
     4   DG,      G33,     K33,     IS,
     5   NEL,     JCVT)
        CALL Q4KELIJ2(
     1   PY4,     PZ4,     PY4,     PZ4,
     2   PYC4,    PZC4,    PYC4,    PZC4,
     3   AY,      R22,     R23,     DD,
     4   DG,      G33,     K44,     IS,
     5   NEL,     JCVT)
        IS = 0
        CALL Q4KELIJ2(
     1   PY1,     PZ1,     PY2,     PZ2,
     2   PYC1,    PZC1,    PYC2,    PZC2,
     3   AY,      R22,     R23,     DD,
     4   DG,      G33,     K12,     IS,
     5   NEL,     JCVT)
        CALL Q4KELIJ2(
     1   PY1,     PZ1,     PY3,     PZ3,
     2   PYC1,    PZC1,    PYC3,    PZC3,
     3   AY,      R22,     R23,     DD,
     4   DG,      G33,     K13,     IS,
     5   NEL,     JCVT)
        CALL Q4KELIJ2(
     1   PY1,     PZ1,     PY4,     PZ4,
     2   PYC1,    PZC1,    PYC4,    PZC4,
     3   AY,      R22,     R23,     DD,
     4   DG,      G33,     K14,     IS,
     5   NEL,     JCVT)
        CALL Q4KELIJ2(
     1   PY2,     PZ2,     PY3,     PZ3,
     2   PYC2,    PZC2,    PYC3,    PZC3,
     3   AY,      R22,     R23,     DD,
     4   DG,      G33,     K23,     IS,
     5   NEL,     JCVT)
        CALL Q4KELIJ2(
     1   PY2,     PZ2,     PY4,     PZ4,
     2   PYC2,    PZC2,    PYC4,    PZC4,
     3   AY,      R22,     R23,     DD,
     4   DG,      G33,     K24,     IS,
     5   NEL,     JCVT)
        CALL Q4KELIJ2(
     1   PY3,     PZ3,     PY4,     PZ4,
     2   PYC3,    PZC3,    PYC4,    PZC4,
     3   AY,      R22,     R23,     DD,
     4   DG,      G33,     K34,     IS,
     5   NEL,     JCVT)
      ENDIF
C
      RETURN
      END
