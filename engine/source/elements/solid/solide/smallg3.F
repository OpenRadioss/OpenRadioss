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
Chd|  SMALLG3                       source/elements/solid/solide/small3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SMALLG3(
     1   SAV,     OFFG,    WXX,     WYY,
     2   WZZ,     R11,     R12,     R13,
     3   R21,     R22,     R23,     R31,
     4   R32,     R33,     NEL,     ISMSTR,
     5   JCVT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: JCVT
      INTEGER NEL
      my_real
     .   R11(*),R12(*),R13(*),
     .   R21(*),R22(*),R23(*),
     .   R31(*),R32(*),R33(*),
     .   OFFG(*),WXX(*),WYY(*),WZZ(*)
      DOUBLE PRECISION
     .  SAV(NEL,21)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,j
C     REAL
      DOUBLE PRECISION
     .   X , Y, Z   ,DV   ,WXXG,WYYG,WZZG
C-----------------------------------------------
C-----------------------------
C     ROTATION RBY DES COORDONNEES POUR SMALL STRAIN
C-----------------------------
C------------Wxx,y,z from local system to global 
      IF (JCVT > 0 ) THEN
       DO I=1,NEL
         WXXG=R11(I)*WXX(I)+R12(I)*WYY(I)+R13(I)*WZZ(I)
         WYYG=R21(I)*WXX(I)+R22(I)*WYY(I)+R23(I)*WZZ(I)
         WZZG=R31(I)*WXX(I)+R32(I)*WYY(I)+R33(I)*WZZ(I)
         WXX(I) = WXXG
         WYY(I) = WYYG
         WZZ(I) = WZZG
       END DO
      END IF 
      IF (ISMSTR==11) THEN
C      IF(ISMSTR==11.AND.NCYCLE>1)THEN
C----SAV : (ismstr=10,11) is differently saved to Ismstr=1      
       DO I=1,NEL
C-----------------------------------------------
         IF(OFFG(I)==ZERO) CYCLE
         X=SAV(I,1)
         Y=SAV(I,8)
         Z=SAV(I,15)
         SAV(I,1) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,8) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,15)= Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,2)
         Y=SAV(I,9)
         Z=SAV(I,16)
         SAV(I,2) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,9) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,16)= Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,3)
         Y=SAV(I,10)
         Z=SAV(I,17)
         SAV(I,3)  = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,10) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,17) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,4)
         Y=SAV(I,11)
         Z=SAV(I,18)
         SAV(I,4)  = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,11) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,18) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,5)
         Y=SAV(I,12)
         Z=SAV(I,19)
         SAV(I,5)  = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,12) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,19) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,6)
         Y=SAV(I,13)
         Z=SAV(I,20)
         SAV(I,6)  = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,13) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,20) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,7)
         Y=SAV(I,14)
         Z=SAV(I,21)
         SAV(I,7)  = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,14) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,21) = Z - X*WYY(I) + Y*WXX(I) 
       ENDDO
      ELSEIF(ISMSTR==1.OR.
     .  	((ISMSTR==2.OR.ISMSTR==12).AND.IDTMIN(1)==3))THEN
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
         X=SAV(I,10)
         Y=SAV(I,11)
         Z=SAV(I,12)
         SAV(I,10) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,11) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,12) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,13)
         Y=SAV(I,14)
         Z=SAV(I,15)
         SAV(I,13) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,14) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,15) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,16)
         Y=SAV(I,17)
         Z=SAV(I,18)
         SAV(I,16) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,17) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,18) = Z - X*WYY(I) + Y*WXX(I) 
C
         X=SAV(I,19)
         Y=SAV(I,20)
         Z=SAV(I,21)
         SAV(I,19) = X - Y*WZZ(I) + Z*WYY(I)
         SAV(I,20) = Y - Z*WXX(I) + X*WZZ(I)
         SAV(I,21) = Z - X*WYY(I) + Y*WXX(I) 
C
        ENDIF
       ENDDO
      ENDIF
C
      RETURN
      END
