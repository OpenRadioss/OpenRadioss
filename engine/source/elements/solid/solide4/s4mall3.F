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
!||    s4malla3   ../engine/source/elements/solid/solide4/s4mall3.F
!||--- called by ------------------------------------------------------
!||    s4forc3    ../engine/source/elements/solid/solide4/s4forc3.F
!||====================================================================
      SUBROUTINE S4MALLA3(
     1   SAV,     OFFG,    OFF,     WXX,
     2   WYY,     WZZ,     NEL,     ISMSTR,
     3   JLAG)
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
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JLAG
      INTEGER NEL
      my_real
     .   OFFG(*),OFF(*),WXX(*),WYY(*),WZZ(*)
      DOUBLE PRECISION
     .  SAV(NEL,9)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      DOUBLE PRECISION
     .   X , Y, Z ,DV
C-----------------------------------------------
C-----------------------------
C     ROTATION RBY DES COORDONNEES POUR SMALL STRAIN
C-----------------------------
      IF(ISMSTR==11)THEN
       DO I=1,NEL
C-----------------------------------------------
         IF(OFFG(I)==ZERO) CYCLE
         X=SAV(I,1)
         Y=SAV(I,4)
         Z=SAV(I,7)
         SAV(I,1) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,4) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,7) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,2)
         Y=SAV(I,5)
         Z=SAV(I,8)
         SAV(I,2) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,5) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,8) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,3)
         Y=SAV(I,6)
         Z=SAV(I,9)
         SAV(I,3) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,6) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,9) = Z - X*WYY(I) + Y*WXX(I) 
C
       ENDDO
      ELSEIF(ISMSTR<=4.AND.JLAG>0)THEN
       DO I=1,NEL
       IF(OFFG(I)>ONE)THEN
C-----------------------------------------------
         X=SAV(I,1)
         Y=SAV(I,2)
         Z=SAV(I,3)
         SAV(I,1) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,2) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,3) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,4)
         Y=SAV(I,5)
         Z=SAV(I,6)
         SAV(I,4) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,5) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,6) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,7)
         Y=SAV(I,8)
         Z=SAV(I,9)
         SAV(I,7) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,8) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,9) = Z - X*WYY(I) + Y*WXX(I) 
C
        ENDIF
       ENDDO
      ENDIF
C
      RETURN
      END
