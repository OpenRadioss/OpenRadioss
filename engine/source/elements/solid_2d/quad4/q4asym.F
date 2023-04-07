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
Chd|  Q4ASYM                        source/elements/solid_2d/quad4/q4ke2.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE Q4ASYM (LFT, LLT, KIJ, KIJU, KIJL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER LFT,LLT
      my_real
     .   KIJ(2,2,*), KIJU(2,2,*), KIJL(2,2,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER EP,I,J
      my_real
     .    AA,BB
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      DO EP=LFT,LLT
      DO J=1,2
      DO I=1,2
        AA = KIJ(I,J,EP) + KIJU(I,J,EP)
        BB = KIJ(I,J,EP) + KIJL(I,J,EP)
        IF (AA<ZERO .AND. BB<ZERO) THEN
c          KIJ(I,J,EP) = 0.5*(AA+BB)
c          KIJ(I,J,EP) = MIN(AA,BB)
c          KIJ(I,J,EP) = MAX(AA,BB)
        ELSEIF (AA>ZERO .AND. BB>ZERO) THEN
c          KIJ(I,J,EP) = 0.5*(AA+BB)
c          KIJ(I,J,EP) = MIN(AA,BB)
c          KIJ(I,J,EP) = MAX(AA,BB)
        ELSE
c          KIJ(I,J,EP) = 0.5*(AA+BB)
c          KIJ(I,J,EP) = MIN(AA,BB)
c          KIJ(I,J,EP) = MAX(AA,BB)
        ENDIF
      ENDDO
      ENDDO
      ENDDO
C
      RETURN
      END
