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
Chd|  S4DERI3                       source/elements/solid/solide4/s4deri3.F
Chd|-- called by -----------
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|-- calls ---------------
Chd|        SCHKJABT3                     source/elements/solid/solide4/schkjabt3.F
Chd|====================================================================
      SUBROUTINE S4DERI3(
     1   OFF,     DET,     NGL,     DELTAX,
     2   MXT,     X1,      X2,      X3,
     3   X4,      Y1,      Y2,      Y3,
     4   Y4,      Z1,      Z2,      Z3,
     5   Z4,      PX1,     PX2,     PX3,
     6   PX4,     PY1,     PY2,     PY3,
     7   PY4,     PZ1,     PZ2,     PZ3,
     8   PZ4,     RX,      RY,      RZ,
     9   SX,      SY,      SZ,      TX,
     A   TY,      TZ,      SAV,     OFFG,
     B   NEL,     PM,      VOLDP,   ISMSTR,
     C   IFORMDT, JLAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: JLAG
      INTEGER, INTENT(IN) :: ISMSTR
      INTEGER, INTENT(IN) :: IFORMDT
      INTEGER NEL, MXT(MVSIZ)
      DOUBLE PRECISION
     .   X1(*), X2(*), X3(*), X4(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*), SAV(NEL,9),VOLDP(NEL)
     
      my_real
     .   OFF(*), DET(*), DELTAX(*), PM(NPROPM,*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),OFFG(*),
     .   RX(*), RY(*), RZ(*), SX(*), SY(*), SZ(*),TX(*), TY(*), TZ(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I,J
      INTEGER NNEGA,INDEX(MVSIZ)
      DOUBLE PRECISION
     .   X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43,B1DP,C1DP,D1DP
      my_real
     .   A1, A2, A3, A4, D, AREAMX2, 
     .   B1(MVSIZ), B2(MVSIZ), B3(MVSIZ), B4(MVSIZ), 
     .   C1(MVSIZ), C2(MVSIZ), C3(MVSIZ), C4(MVSIZ),
     .   D1(MVSIZ), D2(MVSIZ), D3(MVSIZ), D4(MVSIZ),
     .   PXX, PYY, PZZ, PXY, PYZ, PXZ, GFAC, AA, BB, P, LD
C-----------------------------------------------
C
      DO I=1,NEL
       X43 = X4(I) - X3(I)
       Y43 = Y4(I) - Y3(I)
       Z43 = Z4(I) - Z3(I)
       X41 = X4(I) - X1(I)
       Y41 = Y4(I) - Y1(I)
       Z41 = Z4(I) - Z1(I)
       X42 = X4(I) - X2(I)
       Y42 = Y4(I) - Y2(I)
       Z42 = Z4(I) - Z2(I)
C
       RX(I) =  -X41
       RY(I) =  -Y41
       RZ(I) =  -Z41
       SX(I) =  -X42
       SY(I) =  -Y42
       SZ(I) =  -Z42
C
       TX(I) =  -X43
       TY(I) =  -Y43
       TZ(I) =  -Z43
C
       B1DP  =  Y43*Z42 - Y42*Z43
       B1(I) =  B1DP
       B2(I) =  Y41*Z43 - Y43*Z41
       B3(I) =  Y42*Z41 - Y41*Z42
       B4(I) =  -(B1(I) + B2(I) + B3(I))
C
       C1DP  =  Z43*X42 - Z42*X43
       C1(I) =  C1DP
       C2(I) =  Z41*X43 - Z43*X41
       C3(I) =  Z42*X41 - Z41*X42
       C4(I) =  -(C1(I) + C2(I) + C3(I))
C
       D1DP  =  X43*Y42 - X42*Y43
       D1(I) =  D1DP
       D2(I) =  X41*Y43 - X43*Y41
       D3(I) =  X42*Y41 - X41*Y42
       D4(I) =  -(D1(I) + D2(I) + D3(I))
C
       VOLDP(I) = (X41*B1DP + Y41*C1DP + Z41*D1DP)*ONE_OVER_6
       DET(I) = VOLDP(I)
C
      ENDDO
C
C
      CALL SCHKJABT3(
     1   OFF,     DET,     NGL,     OFFG,
     2   NNEGA,   INDEX,   NEL,     ISMSTR,
     3   JLAG)
      IF (NNEGA>0) THEN
        IF (ISMSTR==10.OR.ISMSTR==12) THEN
#include "vectorize.inc"
         DO J=1,NNEGA
          I = INDEX(J)
            X1(I)=SAV(I,1)
            Y1(I)=SAV(I,4)
            Z1(I)=SAV(I,7)
            X2(I)=SAV(I,2)
            Y2(I)=SAV(I,5)
            Z2(I)=SAV(I,8)
            X3(I)=SAV(I,3)
            Y3(I)=SAV(I,6)
            Z3(I)=SAV(I,9)
            X4(I)=ZERO
            Y4(I)=ZERO
            Z4(I)=ZERO
         ENDDO
        ELSE
#include "vectorize.inc"
         DO J=1,NNEGA
          I = INDEX(J)
          X1(I)=SAV(I,1)
          Y1(I)=SAV(I,2)
          Z1(I)=SAV(I,3)
          X2(I)=SAV(I,4)
          Y2(I)=SAV(I,5)
          Z2(I)=SAV(I,6)
          X3(I)=SAV(I,7)
          Y3(I)=SAV(I,8)
          Z3(I)=SAV(I,9)
          X4(I)=ZERO
          Y4(I)=ZERO
          Z4(I)=ZERO
         ENDDO
	END IF
#include "vectorize.inc"
         DO J=1,NNEGA
          I = INDEX(J)
          X43 = X4(I) - X3(I)
          Y43 = Y4(I) - Y3(I)
          Z43 = Z4(I) - Z3(I)
          X41 = X4(I) - X1(I)
          Y41 = Y4(I) - Y1(I)
          Z41 = Z4(I) - Z1(I)
          X42 = X4(I) - X2(I)
          Y42 = Y4(I) - Y2(I)
          Z42 = Z4(I) - Z2(I)
C
          RX(I) =  -X41
          RY(I) =  -Y41
          RZ(I) =  -Z41
          SX(I) =  -X42
          SY(I) =  -Y42
          SZ(I) =  -Z42
          TX(I) =  -X43
          TY(I) =  -Y43
          TZ(I) =  -Z43
C
C
          B1DP  =  Y43*Z42 - Y42*Z43
          B1(I) =  B1DP
          B2(I) =  Y41*Z43 - Y43*Z41
          B3(I) =  Y42*Z41 - Y41*Z42
          B4(I) =  -(B1(I) + B2(I) + B3(I))
C
          C1DP  =  Z43*X42 - Z42*X43
          C1(I) =  C1DP
          C2(I) =  Z41*X43 - Z43*X41
          C3(I) =  Z42*X41 - Z41*X42
          C4(I) =  -(C1(I) + C2(I) + C3(I))
C
          D1DP  =  X43*Y42 - X42*Y43
          D1(I) =  D1DP
          D2(I) =  X41*Y43 - X43*Y41
          D3(I) =  X42*Y41 - X41*Y42
          D4(I) =  -(D1(I) + D2(I) + D3(I))
C
          VOLDP(I) = (X41*B1DP + Y41*C1DP + Z41*D1DP)*ONE_OVER_6
          DET(I) = VOLDP(I)
          OFFG(I) = TWO
         ENDDO
       END IF
C
      DO I=1,NEL
        D = ONE/DET(I)/SIX
        PX1(I)=-B1(I)*D
        PY1(I)=-C1(I)*D
        PZ1(I)=-D1(I)*D
        PX2(I)=-B2(I)*D
        PY2(I)=-C2(I)*D
        PZ2(I)=-D2(I)*D
        PX3(I)=-B3(I)*D
        PY3(I)=-C3(I)*D
        PZ3(I)=-D3(I)*D
        PX4(I)=-B4(I)*D
        PY4(I)=-C4(I)*D
        PZ4(I)=-D4(I)*D
       END DO

       IF(IDT1SOL==0)THEN

         DO I=1,NEL
	   D = MAX(PX1(I)*PX1(I)+PY1(I)*PY1(I)+PZ1(I)*PZ1(I),
     .    	   PX2(I)*PX2(I)+PY2(I)*PY2(I)+PZ2(I)*PZ2(I),
     .  	   PX3(I)*PX3(I)+PY3(I)*PY3(I)+PZ3(I)*PZ3(I),
     .  	   PX4(I)*PX4(I)+PY4(I)*PY4(I)+PZ4(I)*PZ4(I))
           DELTAX(I) = ONE / SQRT(D)
         END DO

       ELSEIF(IFORMDT==0)THEN
         DO I=1,NEL
	   D = PX1(I)*PX1(I)+PY1(I)*PY1(I)+PZ1(I)*PZ1(I)
     .       + PX2(I)*PX2(I)+PY2(I)*PY2(I)+PZ2(I)*PZ2(I)
     .       + PX3(I)*PX3(I)+PY3(I)*PY3(I)+PZ3(I)*PZ3(I)
     .       + PX4(I)*PX4(I)+PY4(I)*PY4(I)+PZ4(I)*PZ4(I)
           DELTAX(I) = ONE / SQRT(D)
         END DO

       ELSEIF(IFORMDT==1)THEN

         GFAC=PM(105,MXT(1))
         LD  =TWO*SQRT(MAX(ONE-GFAC,ZERO))+ONE
         DO I=1,NEL
           PXX=PX1(I)*PX1(I)+PX2(I)*PX2(I)+PX3(I)*PX3(I)+PX4(I)*PX4(I)
           PYY=PY1(I)*PY1(I)+PY2(I)*PY2(I)+PY3(I)*PY3(I)+PY4(I)*PY4(I)
           PZZ=PZ1(I)*PZ1(I)+PZ2(I)*PZ2(I)+PZ3(I)*PZ3(I)+PZ4(I)*PZ4(I)
           PXY=PX1(I)*PY1(I)+PX2(I)*PY2(I)+PX3(I)*PY3(I)+PX4(I)*PY4(I)
           PXZ=PX1(I)*PZ1(I)+PX2(I)*PZ2(I)+PX3(I)*PZ3(I)+PX4(I)*PZ4(I)
           PYZ=PY1(I)*PZ1(I)+PY2(I)*PZ2(I)+PY3(I)*PZ3(I)+PY4(I)*PZ4(I)
C
           AA = -(PXX+PYY+PZZ)
           BB =  (PXX*PYY+PXX*PZZ+PYY*PZZ-PXY**2-PXZ**2-PYZ**2) 
           P  = BB-THIRD*AA*AA
           D  = TWO*SQRT(THIRD*MAX(-P,ZERO))-THIRD*AA
C
 	   D=LD*D
C
           DELTAX(I) = ONE / SQRT(D)
         END DO

       ELSEIF(IFORMDT==2)THEN

         GFAC=PM(105,MXT(1))
         DO I=1,NEL
           PXX=PX1(I)*PX1(I)+PX2(I)*PX2(I)+PX3(I)*PX3(I)+PX4(I)*PX4(I)
           PYY=PY1(I)*PY1(I)+PY2(I)*PY2(I)+PY3(I)*PY3(I)+PY4(I)*PY4(I)
           PZZ=PZ1(I)*PZ1(I)+PZ2(I)*PZ2(I)+PZ3(I)*PZ3(I)+PZ4(I)*PZ4(I)
           PXY=PX1(I)*PY1(I)+PX2(I)*PY2(I)+PX3(I)*PY3(I)+PX4(I)*PY4(I)
           PXZ=PX1(I)*PZ1(I)+PX2(I)*PZ2(I)+PX3(I)*PZ3(I)+PX4(I)*PZ4(I)
           PYZ=PY1(I)*PZ1(I)+PY2(I)*PZ2(I)+PY3(I)*PZ3(I)+PY4(I)*PZ4(I)
C
           AA = -(PXX+PYY+PZZ)
           BB =  GFAC*(PXX*PYY+PXX*PZZ+PYY*PZZ-PXY**2-PXZ**2-PYZ**2) 
           P  = BB-THIRD*AA*AA
           D  = TWO*SQRT(THIRD*MAX(-P,ZERO))-THIRD*AA
C
           DELTAX(I) = ONE / SQRT(D)
         END DO

       END IF
C
      RETURN
C
 1000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB',I10/)
 2000 FORMAT(/' ZERO OR NEGATIVE VOLUME : DELETE 3D-ELEMENT NB',I10/)
      END
