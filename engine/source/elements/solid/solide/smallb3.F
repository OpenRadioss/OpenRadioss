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
      !||====================================================================
      !||    smallb3      ../engine/source/elements/solid/solide/smallb3.F
      !||--- called by ------------------------------------------------------
      !||    ig3duforc3   ../engine/source/elements/ige3d/ig3duforc3.F
      !||    q4forc2      ../engine/source/elements/solid_2d/quad4/q4forc2.F
      !||    s10forc3     ../engine/source/elements/solid/solide10/s10forc3.F
      !||    s16forc3     ../engine/source/elements/thickshell/solide16/s16forc3.F
      !||    s20forc3     ../engine/source/elements/solid/solide20/s20forc3.F
      !||    s4forc3      ../engine/source/elements/solid/solide4/s4forc3.F
      !||    s6cforc3     ../engine/source/elements/thickshell/solide6c/s6cforc3.F
      !||    s8cforc3     ../engine/source/elements/thickshell/solide8c/s8cforc3.F
      !||    s8eforc3     ../engine/source/elements/solid/solide8e/s8eforc3.F
      !||    s8forc3      ../engine/source/elements/solid/solide8/s8forc3.F
      !||    s8sforc3     ../engine/source/elements/solid/solide8s/s8sforc3.F
      !||    s8zforc3     ../engine/source/elements/solid/solide8z/s8zforc3.F
      !||    scforc3      ../engine/source/elements/thickshell/solidec/scforc3.F
      !||    sforc3       ../engine/source/elements/solid/solide/sforc3.F
      !||    szforc3      ../engine/source/elements/solid/solidez/szforc3.F
      !||====================================================================
      SUBROUTINE SMALLB3(
     1   OFFG,    OFF,     NEL,     ISMSTR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: ISMSTR
      my_real OFFG(*),OFF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      IF(ISMSTR==1.OR.ISMSTR==3)THEN
        DO I=1,NEL
          IF(OFFG(I)>ZERO)OFFG(I)=TWO 
        ENDDO
      ENDIF
C
      DO I=1,NEL
        IF(OFF(I)<ONE)THEN
          IF(OFF(I)==ZERO)THEN
            OFFG(I)=ZERO
          ELSEIF(OFFG(I)>ONE)THEN
            OFFG(I) = ONE+OFF(I)
          ELSE
            OFFG(I) = OFF(I)
          END IF
        END IF
      ENDDO
C
      RETURN
      END
