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
Chd|  S10MALLA3                     source/elements/solid/solide10/s10malla3.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S10MALLA3(
     1   OFFG,    OFF,     WXX,     WYY,
     2   WZZ,     WXXG,    WYYG,    WZZG,
     3   WIP,     NEL,     ISMSTR,  JLAG)
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
      INTEGER, INTENT(IN) :: JLAG
      my_real
     .   OFFG(*),OFF(*),WXX(*),WYY(*),WZZ(*),
     .   WXXG(*),WYYG(*),WZZG(*),WIP
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, N
C     REAL
      my_real
     .   X , Y, Z ,DV
C-----------------------------------------------
C-----------------------------
C     ROTATION RBY DES COORDONNEES POUR SMALL STRAIN
C-----------------------------
      IF(ISMSTR<=4.AND.JLAG>0)THEN
       DO I=1,NEL
         WXXG(I)=WXXG(I)+WIP*WXX(I)
         WYYG(I)=WYYG(I)+WIP*WYY(I)
         WZZG(I)=WZZG(I)+WIP*WZZ(I)
       ENDDO
      ELSEIF(ISMSTR==11)THEN
       DO I=1,NEL
         WXXG(I)=WXXG(I)+WIP*WXX(I)
         WYYG(I)=WYYG(I)+WIP*WYY(I)
         WZZG(I)=WZZG(I)+WIP*WZZ(I)
       ENDDO
      ENDIF
C
      RETURN
      END
