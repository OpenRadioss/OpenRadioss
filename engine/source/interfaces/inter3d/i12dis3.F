Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    i12dis3    ../engine/source/interfaces/inter3d/i12dis3.F
!||--- called by ------------------------------------------------------
!||    intti12a   ../engine/source/interfaces/interf/intti12.F
!||====================================================================
      SUBROUTINE I12DIS3(
     1   CRST,    NSV,     ITAB,    GAP,
     2   NOINT,   ITEST,   TAGKINE, SSC,
     3   TTC,     LFT,     LLT,     NFT)
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
      INTEGER NSV(*),ITAB(*),NOINT,ITEST,TAGKINE(*)
      my_real
     .  CRST(2,*),GAP
      my_real, DIMENSION(MVSIZ), INTENT(INOUT) ::  SSC,TTC
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,ISGN,II
C-----------------------------------------------
      
      DO I=LFT,LLT
        II=I+NFT
        IF(ABS(SSC(I))>ONE+GAP .OR. ABS(TTC(I))>ONE+GAP)THEN
          IF(TAGKINE(II)>0)ITEST=ITEST+1
        ENDIF
        IF(ABS(SSC(I))>ONE)SSC(I)=SSC(I)/ABS(SSC(I))
        IF(ABS(TTC(I))>ONE)TTC(I)=TTC(I)/ABS(TTC(I))
        CRST(1,II)=SSC(I)
        CRST(2,II)=TTC(I)
      ENDDO

      RETURN
      END
