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
Chd|  S6CORTHO3                     source/elements/thickshell/solide6c/s6cortho3.F
Chd|-- called by -----------
Chd|        S6RCOOR3                      source/elements/thickshell/solide6c/s6rcoor3.F
Chd|        S6RCOORK                      source/elements/thickshell/solide6c/s6rcoork.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S6CORTHO3(
     .   X1, X2, X3, X4, X5, X6, 
     .   Y1, Y2, Y3, Y4, Y5, Y6, 
     .   Z1, Z2, Z3, Z4, Z5, Z6, 
     .   E1X, E2X, E3X, E1Y, E2Y, E3Y, E1Z, E2Z, E3Z ,
     .   RX , RY , RZ , SX , SY , SZ , TX , TY , TZ )
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
C     REAL
      my_real, INTENT(IN),  DIMENSION(MVSIZ) ::
     .   X1, X2, X3, X4, X5, X6, 
     .   Y1, Y2, Y3, Y4, Y5, Y6,   
     .   Z1, Z2, Z3, Z4, Z5, Z6
      my_real, INTENT(OUT),  DIMENSION(MVSIZ) ::
     .   RX, RY, RZ, SX, SY, SZ, TX, TY, TZ,
     .   E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   X14 ,Y14 , Z14
      my_real
     .   DET,C1,C2 
C-----------------------------------------------
      DO I=LFT,LLT
        X14=X1(I)+X4(I)
        Y14=Y1(I)+Y4(I)
        Z14=Z1(I)+Z4(I)
C--------ksi---------
        TX(I)=X2(I)+X5(I)-X14
        TY(I)=Y2(I)+Y5(I)-Y14
        TZ(I)=Z2(I)+Z5(I)-Z14
C--------eta---------
        RX(I)=X3(I)+X6(I)-X14
        RY(I)=Y3(I)+Y6(I)-Y14
        RZ(I)=Z3(I)+Z6(I)-Z14
       ENDDO
      DO I=LFT,LLT
        SX(I)= (X4(I)+X5(I)+X6(I)-X1(I)-X2(I)-X3(I))*THIRD
        SY(I)= (Y4(I)+Y5(I)+Y6(I)-Y1(I)-Y2(I)-Y3(I))*THIRD
        SZ(I)= (Z4(I)+Z5(I)+Z6(I)-Z1(I)-Z2(I)-Z3(I))*THIRD
      ENDDO
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      DO 100 I=LFT,LLT
C
       E3X(I) = TY(I) * RZ(I) - TZ(I) * RY(I) 
       E3Y(I) = TZ(I) * RX(I) - TX(I) * RZ(I) 
       E3Z(I) = TX(I) * RY(I) - TY(I) * RX(I) 
C
       DET = SQRT(E3X(I)*E3X(I) + E3Y(I)*E3Y(I) + E3Z(I)*E3Z(I))
       IF ( DET.NE.ZERO) DET = ONE / DET
       E3X(I) = E3X(I) * DET
       E3Y(I) = E3Y(I) * DET
       E3Z(I) = E3Z(I) * DET
C
       C1=SQRT(TX(I)*TX(I)+TY(I)*TY(I)+TZ(I)*TZ(I))
       C2=SQRT(RX(I)*RX(I)+RY(I)*RY(I)+RZ(I)*RZ(I))
       E1X(I)=TX(I)*C2 +(RY(I) * E3Z(I) - RZ(I) * E3Y(I))*C1  
       E1Y(I)=TY(I)*C2 +(RZ(I) * E3X(I) - RX(I) * E3Z(I))*C1  
       E1Z(I)=TZ(I)*C2 +(RX(I) * E3Y(I) - RY(I) * E3X(I))*C1
       DET = SQRT(E1X(I)*E1X(I) + E1Y(I)*E1Y(I) + E1Z(I)*E1Z(I))
       IF ( DET.NE.ZERO) DET = ONE / DET
       E1X(I) = E1X(I)*DET
       E1Y(I) = E1Y(I)*DET
       E1Z(I) = E1Z(I)*DET
C
       E2X(I) = E3Y(I) * E1Z(I) - E3Z(I) * E1Y(I)
       E2Y(I) = E3Z(I) * E1X(I) - E3X(I) * E1Z(I)
       E2Z(I) = E3X(I) * E1Y(I) - E3Y(I) * E1X(I)
 100  CONTINUE
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      RETURN
      END
