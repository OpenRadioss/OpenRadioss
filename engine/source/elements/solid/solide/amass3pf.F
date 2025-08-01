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
!||    amass3pf   ../engine/source/elements/solid/solide/amass3pf.F
!||--- called by ------------------------------------------------------
!||    sforc3     ../engine/source/elements/solid/solide/sforc3.F
!||====================================================================
      SUBROUTINE AMASS3PF(
     1   FSKYM,   RHO,     VOLU,    IADS,
     2   OFF,     NEL,     NFT)
C------------------------------------------
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
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: NFT
C     REAL
      my_real
     .   FSKYM(*), RHO(*),VOLU(*), OFF(*)
      INTEGER IADS(8,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IFLAG,II, K
C     REAL
      my_real DMASS(MVSIZ)
C-----------------------------------------------
       DO  I=1,NEL
        DMASS(I)=ONE_OVER_8*RHO(I)*VOLU(I)*OFF(I)
       ENDDO
C
      DO I=1,NEL
        II = I +NFT
        K = IADS(1,II)
        FSKYM(K)= DMASS(I)
C
        K = IADS(2,II)
        FSKYM(K)= DMASS(I)
C
        K = IADS(3,II)
        FSKYM(K)= DMASS(I)
C
        K = IADS(4,II)
        FSKYM(K)= DMASS(I)
C
        K = IADS(5,II)
        FSKYM(K)= DMASS(I)
C
        II = I +NFT
        K = IADS(6,II)
        FSKYM(K)= DMASS(I)
C
        K = IADS(7,II)
        FSKYM(K)= DMASS(I)
C
        K = IADS(8,II)
        FSKYM(K)= DMASS(I)
      ENDDO
C
      RETURN
      END
