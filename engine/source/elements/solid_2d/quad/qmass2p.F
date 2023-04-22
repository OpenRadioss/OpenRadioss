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
Chd|  QMASS2P                       source/elements/solid_2d/quad/qmass2p.F
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE QMASS2P(
     1   OFF,     RHO,     AREA,    FSKY,
     2   FSKYV,   IADQ,    NEL,     NFT)
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
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: NFT
C     REAL
      my_real
     .   OFF(*), RHO(*), AREA(*), FSKYV(LSKY,8),FSKY(8,LSKY)
      INTEGER IADQ(4,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   MASS(MVSIZ)
      INTEGER I, II, K
C-----------------------------------------------
      DO I=1,NEL
       MASS(I)=RHO(I)*AREA(I)*OFF(I)*FOURTH
      ENDDO
C

      IF(IVECTOR==1) THEN
        DO I=1,NEL
C
          II=I+NFT
          K = IADQ(1,II)
          FSKYV(K,1)=MASS(I)
C
          K = IADQ(2,II)
          FSKYV(K,1)=MASS(I)
C
          K = IADQ(3,II)
          FSKYV(K,1)=MASS(I)
C
          K = IADQ(4,II)
          FSKYV(K,1)=MASS(I)
C
        ENDDO
C
      ELSE
        DO I=1,NEL
C
          II=I+NFT
          K = IADQ(1,II)
          FSKY(1,K) = MASS(I)
C
          K = IADQ(2,II)
          FSKY(1,K) = MASS(I)
C
          K = IADQ(3,II)
          FSKY(1,K) = MASS(I)
C
          K = IADQ(4,II)
          FSKY(1,K) = MASS(I)
C
        ENDDO
C
      ENDIF
C
      RETURN
      END
