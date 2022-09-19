Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|        SCHKJABT3                     source/elements/solid/solide4/s4deri3.F
Chd|====================================================================
      SUBROUTINE S4DERI3(OFF,DET,NGL,DELTAX,MXT,
     .   X1,  X2,  X3,  X4,  Y1,  Y2,  Y3,  Y4,
     .   Z1,  Z2,  Z3,  Z4,  PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4, PZ1, PZ2, PZ3, PZ4,
     .   RX,  RY,  RZ,  SX,  SY,  SZ , TX , TY , TZ,
     .   SAV ,OFFG,NEL, PM ,VOLDP)
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
#include      "com06_c.inc"
#include      "param_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, MXT(MVSIZ)
C     REAL
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
C     REAL
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
      DO I=LFT,LLT
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
      CALL SCHKJABT3(OFF,DET,NGL,OFFG,NNEGA,INDEX)
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
      DO I=LFT,LLT
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

         DO I=LFT,LLT
C          AREAMX2 = SQRT(MAX(B1(I)*B1(I)+C1(I)*C1(I)+D1(I)*D1(I),
C     .                       B2(I)*B2(I)+C2(I)*C2(I)+D2(I)*D2(I),
C     .                       B3(I)*B3(I)+C3(I)*C3(I)+D3(I)*D3(I),
C     .                       B4(I)*B4(I)+C4(I)*C4(I)+D4(I)*D4(I)))
C          DELTAX(I) = 6.*DET(I) / AREAMX2
	   D = MAX(PX1(I)*PX1(I)+PY1(I)*PY1(I)+PZ1(I)*PZ1(I),
     .    	   PX2(I)*PX2(I)+PY2(I)*PY2(I)+PZ2(I)*PZ2(I),
     .  	   PX3(I)*PX3(I)+PY3(I)*PY3(I)+PZ3(I)*PZ3(I),
     .  	   PX4(I)*PX4(I)+PY4(I)*PY4(I)+PZ4(I)*PZ4(I))
           DELTAX(I) = ONE / SQRT(D)
         END DO

       ELSEIF(IFORMDT==0)THEN
         DO I=LFT,LLT
	   D = PX1(I)*PX1(I)+PY1(I)*PY1(I)+PZ1(I)*PZ1(I)
     .       + PX2(I)*PX2(I)+PY2(I)*PY2(I)+PZ2(I)*PZ2(I)
     .       + PX3(I)*PX3(I)+PY3(I)*PY3(I)+PZ3(I)*PZ3(I)
     .       + PX4(I)*PX4(I)+PY4(I)*PY4(I)+PZ4(I)*PZ4(I)
           DELTAX(I) = ONE / SQRT(D)
         END DO

       ELSEIF(IFORMDT==1)THEN

         GFAC=PM(105,MXT(1))
         LD  =TWO*SQRT(MAX(ONE-GFAC,ZERO))+ONE
         DO I=LFT,LLT
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
         DO I=LFT,LLT
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
Chd|====================================================================
Chd|  S4DERIT3                      source/elements/solid/solide4/s4deri3.F
Chd|-- called by -----------
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S4KE3                         source/elements/solid/solide4/s4ke3.F
Chd|-- calls ---------------
Chd|        SCHKJAB3                      source/elements/solid/solide/sderi3.F
Chd|====================================================================
      SUBROUTINE S4DERIT3(OFF,DET,NGL,DELTAX,MXT,
     .   X1,  X2,  X3,  X4,  Y1,  Y2,  Y3,  Y4,
     .   Z1,  Z2,  Z3,  Z4,  PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4, PZ1, PZ2, PZ3, PZ4,
     .   RX,  RY,  RZ,  SX,  SY,  SZ , TX , TY , TZ ,
     .   PM ,VOLDP)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MXT(*)
      DOUBLE PRECISION
     .   X1(*), X2(*), X3(*), X4(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*),VOLDP(*)
C     REAL
      my_real
     .   OFF(*),DET(*),DELTAX(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),
     .   RX(*), RY(*), RZ(*), SX(*), SY(*), SZ(*),TX(*), TY(*), TZ(*),
     .   PM(NPROPM,*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NGL(*), I
C     REAL
C                                                                     12
      DOUBLE PRECISION
     .   X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43,B1DP,C1DP,D1DP
      my_real
     .   B1(MVSIZ), B2(MVSIZ), B3(MVSIZ), B4(MVSIZ),
     .   C1(MVSIZ), C2(MVSIZ), C3(MVSIZ), C4(MVSIZ),
     .   D1(MVSIZ), D2(MVSIZ), D3(MVSIZ), D4(MVSIZ),
     .   A1, A2, A3, A4, D, AREAMX2,
     .   PXX, PYY, PZZ, PXY, PYZ, PXZ, GFAC, AA, BB, P, LD
C-----------------------------------------------
C
      DO I=LFT,LLT
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
      CALL SCHKJAB3(OFF,DET,NGL)
C
      DO I=LFT,LLT
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

        DO I=LFT,LLT
C         AREAMX2 = SQRT(MAX(B1(I)*B1(I)+C1(I)*C1(I)+D1(I)*D1(I),
C    .  		    B2(I)*B2(I)+C2(I)*C2(I)+D2(I)*D2(I),
C    .  		    B3(I)*B3(I)+C3(I)*C3(I)+D3(I)*D3(I),
C    .  		    B4(I)*B4(I)+C4(I)*C4(I)+D4(I)*D4(I)))
C         DELTAX(I) = 6.*DET(I) / AREAMX2
          D = MAX(PX1(I)*PX1(I)+PY1(I)*PY1(I)+PZ1(I)*PZ1(I),
     .  	  PX2(I)*PX2(I)+PY2(I)*PY2(I)+PZ2(I)*PZ2(I),
     .  	  PX3(I)*PX3(I)+PY3(I)*PY3(I)+PZ3(I)*PZ3(I),
     .  	  PX4(I)*PX4(I)+PY4(I)*PY4(I)+PZ4(I)*PZ4(I))
          DELTAX(I) = ONE / SQRT(D)
        END DO


       ELSEIF(IFORMDT==1)THEN

         GFAC=PM(105,MXT(1))
         LD  =TWO*SQRT(MAX(ONE-GFAC,ZERO))+ONE
         DO I=LFT,LLT
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
           D=TWO*SQRT(THIRD*MAX(-P,ZERO))-THIRD*AA
C
 	   D=LD*D
C
           DELTAX(I) = ONE / SQRT(D)
         END DO

       ELSEIF(IFORMDT==2)THEN

         GFAC=PM(105,MXT(1))
         DO I=LFT,LLT
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
Chd|====================================================================
Chd|  SCHKJABT3                     source/elements/solid/solide4/s4deri3.F
Chd|-- called by -----------
Chd|        S4DERI3                       source/elements/solid/solide4/s4deri3.F
Chd|        S8EDERIC3                     source/elements/solid/solide8e/s8ederi3.F
Chd|        S8ZDERIC3                     source/elements/solid/solide8z/s8zderi3.F
Chd|        SDERI3                        source/elements/solid/solide/sderi3.F
Chd|        SZDERI3                       source/elements/solid/solidez/szderi3.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE SCHKJABT3(OFF,DET,NGL,OFFG,NNEGA,INDEX)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "com06_c.inc"
#include      "units_c.inc"
#include      "scr07_c.inc"
#include      "scr17_c.inc"
#include      "scr18_c.inc"
#include      "impl1_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NGL(*),NNEGA,INDEX(*)
C     REAL
      my_real
     .   OFF(*),DET(*),OFFG(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J ,ICOR
C     REAL
C-----------------------------------------------
      NNEGA = 0
      IF(JLAG/=0)THEN
C--- case total strain switched to small due to nega_v before      
        IF(ISMSTR==10.OR.(ISMSTR==12.AND.IDTMIN(1)/=3)) THEN
         DO I=LFT,LLT
          IF(OFFG(I) > ONE)THEN
           NNEGA=NNEGA+1
           INDEX(NNEGA)=I
          END IF
         ENDDO
        END IF
        ICOR = 0
        DO I=LFT,LLT
          IF(OFF(I) ==ZERO)THEN
            DET(I)=ONE
          ELSEIF(OFFG(I) > ONE)THEN
C
          ELSEIF((DET(I)<=VOLMIN).OR.(DET(I)<=ZERO))THEN
            ICOR = 1
          ENDIF
        ENDDO
        IF (ICOR>0.AND.INCONV==1) THEN
         DO I=LFT,LLT
          IF(DET(I)<=ZERO.AND.OFFG(I) <= ONE.AND.OFFG(I) /= ZERO)THEN
            NNEGA=NNEGA+1
            INDEX(NNEGA)=I
#include "lockon.inc"
           IF(ISMSTR<10) THEN
              CALL ANCMSG(MSGID=259,ANMODE=ANINFO,
     .                    I1=NGL(I))
	   ELSE
              CALL ANCMSG(MSGID=261,ANMODE=ANINFO,
     .                    I1=NGL(I))
	   END IF
#include "lockoff.inc"
          ENDIF
          IF (INEG_V==0) THEN
           CALL ANCMSG(MSGID=280,ANMODE=ANINFO)
           MSTOP = 1
          ENDIF
         ENDDO
        END IF !(ICOR>0.AND.IMCONV==1) THEN
      ELSE
      IF(IDTMIN(1)==1)THEN
        ICOR = 0
        DO I=LFT,LLT
          IF(OFF(I) ==ZERO)THEN
            DET(I)=1.
          ELSEIF((DET(I)<=VOLMIN).OR.(DET(I)<=ZERO))THEN
            ICOR = 1
          ENDIF
        ENDDO
        IF (ICOR>0.AND.IMCONV==1) THEN
          DO I=LFT,LLT
           IF(OFF(I)/=ZERO)THEN
            IF(DET(I)<=VOLMIN)THEN
              DET(I)=ONE
              OFF(I)=ZERO
#include "lockon.inc"
                WRITE(ISTDO,2000) NGL(I)
                WRITE(IOUT ,2000) NGL(I)
#include "lockoff.inc"
              IDEL7NOK = 1
            ELSEIF(DET(I)<=ZERO)THEN
              CALL ANCMSG(MSGID=166,ANMODE=ANINFO,
     .                    I1=NGL(I))
              MSTOP = 1
            ENDIF
           ENDIF
          ENDDO
        ENDIF
      ELSEIF(IDTMIN(1)==2)THEN
        ICOR = 0
        DO I=LFT,LLT
          IF(OFF(I) ==ZERO)THEN
            DET(I)=ONE
          ELSEIF((DET(I)<=VOLMIN).OR.(DET(I)<=ZERO))THEN
            ICOR=1
          ENDIF
        ENDDO
        IF (ICOR>0.AND.IMCONV==1) THEN
          DO I=LFT,LLT
            IF((OFF(I)/=ZERO).AND.
     .         (DET(I)<=VOLMIN.OR.DET(I)<=ZERO))THEN
              DET(I)=ONE
              OFF(I)=ZERO
#include "lockon.inc"
                WRITE(ISTDO,2000) NGL(I)
                WRITE(IOUT ,2000) NGL(I)
#include "lockoff.inc"
              IDEL7NOK = 1
            ENDIF
          ENDDO
        ENDIF
      ELSE
        ICOR = 0
        DO I=LFT,LLT
          IF(OFF(I) ==ZERO)THEN
            DET(I)=ONE
          ELSEIF(DET(I)<=ZERO)THEN
            ICOR=1
          ENDIF
        ENDDO
        IF (ICOR>0.AND.IMCONV==1) THEN
          DO I=LFT,LLT
           IF(OFF(I)/=ZERO)THEN
            IF(DET(I)<=ZERO)THEN
              CALL ANCMSG(MSGID=166,ANMODE=ANINFO,
     .                    I1=NGL(I))
              MSTOP = 1
            ENDIF
           ENDIF
          ENDDO
        ENDIF
      ENDIF
      END IF!(JLAG/=0)THEN
      RETURN
C
 1000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB',I10/)
 2000 FORMAT(/' ZERO OR NEGATIVE VOLUME : DELETE 3D-ELEMENT NB',I10/)
 3000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB:',I10/,
     +        ' ELEMENT IS SWITCHED TO SMALL STRAIN OPTION'/)
 4000 FORMAT(/' ZERO OR NEGATIVE VOLUME : 3D-ELEMENT NB:',I10/,
     +        ' TOTAL STRAIN ELEMENT IS SWITCHED TO SMALL STRAIN OPTION'/)
      END
Chd|====================================================================
Chd|  S4DERITO3                     source/elements/solid/solide4/s4deri3.F
Chd|-- called by -----------
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S4DERITO3(OFF,DET,
     .   X1,  X2,  X3,  X4,  Y1,  Y2,  Y3,  Y4,
     .   Z1,  Z2,  Z3,  Z4,  PX1, PX2, PX3, PX4,
     .   PY1, PY2, PY3, PY4, PZ1, PZ2, PZ3, PZ4,
     .   RX,  RY,  RZ,  SX,  SY,  SZ , TX , TY , TZ ,
     .   JAC_I )
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
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   OFF(*),DET(*),
     .   PX1(*), PX2(*), PX3(*), PX4(*),
     .   PY1(*), PY2(*), PY3(*), PY4(*),
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),
     .   RX(*), RY(*), RZ(*), SX(*), SY(*), SZ(*),TX(*), TY(*), TZ(*),
     .   JAC_I(10,*)
      DOUBLE PRECISION
     .   X1(*), X2(*), X3(*), X4(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  I
C     REAL
C                                                                     12
      my_real
     .   X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43
      my_real
     .   JI1, JI2, JI3,
     .   JI4, JI5, JI6,
     .   JI7, JI8, JI9,
     .   A1, A2, A3, A4, D, AREAMX2
C-----------------------------------------------
      DO I=LFT,LLT
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
      ENDDO
C
      DO I=LFT,LLT
       JI1 = JAC_I(1,I)
       JI2 = JAC_I(2,I)
       JI3 = JAC_I(3,I)
       JI4 = JAC_I(4,I)
       JI5 = JAC_I(5,I)
       JI6 = JAC_I(6,I)
       JI7 = JAC_I(7,I)
       JI8 = JAC_I(8,I)
       JI9 = JAC_I(9,I)
C
        PX1(I)= JI1
        PY1(I)= JI4
        PZ1(I)= JI7
        PX2(I)= JI2
        PY2(I)= JI5
        PZ2(I)= JI8
        PX3(I)= JI3
        PY3(I)= JI6
        PZ3(I)= JI9
        PX4(I)=-(JI1+JI2+JI3)
        PY4(I)=-(JI4+JI5+JI6)
        PZ4(I)=-(JI7+JI8+JI9)
      ENDDO
C
      RETURN
C
      END
