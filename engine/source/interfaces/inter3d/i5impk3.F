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
Chd|  I5IMPK3                       source/interfaces/inter3d/i5cork3.F
Chd|-- called by -----------
Chd|        INTVO3                        source/interfaces/inter3d/intvo3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I5IMPK3(
     1   IRECT,   MSR,     NSV,     IRTL,
     2   NUM_IMP, NS_IMP,  NE_IMP,  ANS,
     3   LFT,     LLT,     NFT)
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
      INTEGER, INTENT(INOUT) :: LFT
      INTEGER, INTENT(INOUT) :: LLT
      INTEGER, INTENT(INOUT) :: NFT
      INTEGER IRECT(4,*), MSR(*), NSV(*), IRTL(*)
      INTEGER NUM_IMP,NS_IMP(*),NE_IMP(*)
      my_real, DIMENSION(MVSIZ), INTENT(IN) :: ANS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL, IG, L, NN,J
C     REAL
C-----------------------------------------------
C
      J=0
      DO I=LFT,LLT
       IL=I+NFT
       IG=NSV(IL)
       L=IRTL(IL)
       IF (ANS(I)<ZERO) THEN
        J = J + 1
        NS_IMP(J+NUM_IMP)=IL
        NE_IMP(J+NUM_IMP)=L
       ENDIF
      ENDDO
      NUM_IMP=NUM_IMP+J
C
      RETURN
      END
