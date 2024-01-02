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
Chd|  I9IMP3                        source/interfaces/int09/i9imp3.F
Chd|-- called by -----------
Chd|        I9MAIN3                       source/interfaces/int09/i9main3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I9IMP3(
     1   ILOC,    CST,     NOR,     XFACE,
     2   N1,      N2,      N3,      SSC,
     3   TTC,     THK,     ANS,     LFT,
     4   LLT,     NFT)
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
      INTEGER ILOC(*)
      my_real NOR(3,*), CST(2,*)
      my_real, DIMENSION(MVSIZ), INTENT(IN) ::  XFACE,SSC,TTC,THK
      my_real, DIMENSION(MVSIZ), INTENT(IN) ::  N1,N2,N3
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) ::ANS
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IL
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      DO I=LFT,LLT
        IL=I+NFT
        ANS(I)= MIN(ZERO,(ANS(I)*XFACE(I)-THK(I)))
        IF(ANS(I)*XFACE(I)==ZERO)ILOC(IL)=-ILOC(IL)
      ENDDO

      DO I=LFT,LLT
        IL=I+NFT
        NOR(1,IL)=N1(I)
        NOR(2,IL)=N2(I)
        NOR(3,IL)=N3(I)
        CST(1,IL)=SSC(I)
        CST(2,IL)=TTC(I)
      ENDDO
C-----------------------------------------------
      RETURN
      END
