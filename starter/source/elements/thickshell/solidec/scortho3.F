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
Chd|  SCORTHO3                      source/elements/thickshell/solidec/scortho3.F
Chd|-- called by -----------
Chd|        SCCOOR3                       source/elements/thickshell/solidec/sccoor3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SCORTHO3(
     .      X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8,         
     .      Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8,         
     .      Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8,         
     .      RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,TZ   ,   
     .      E1X  ,E1Y  ,E1Z  ,E2X  ,E2Y  ,E2Z  ,E3X  ,E3Y  ,E3Z  )
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
C     REAL ou REAL*8
      my_real
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),  
     .   RX(*) ,RY(*) ,RZ(*) ,SX(*) ,SY(*) ,SZ(*) ,TX(*) ,TY(*) ,TZ(*),
     .   E1X(*),E1Y(*),E1Z(*),E2X(*),E2Y(*),E2Z(*),E3X(*),E3Y(*),E3Z(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL ou REAL*8
      my_real
     .   X17(MVSIZ) , X28(MVSIZ) , X35(MVSIZ) , X46(MVSIZ),
     .   Y17(MVSIZ) , Y28(MVSIZ) , Y35(MVSIZ) , Y46(MVSIZ),
     .   Z17(MVSIZ) , Z28(MVSIZ) , Z35(MVSIZ) , Z46(MVSIZ),
     .   A17(MVSIZ) , A28(MVSIZ) ,
     .   B17(MVSIZ) , B28(MVSIZ) ,
     .   C17(MVSIZ) , C28(MVSIZ) ,
     .   DET,C1,C2 
C-----------------------------------------------
      DO 10 I=LFT,LLT
      X17(I)=X7(I)-X1(I)
      X28(I)=X8(I)-X2(I)
      X35(I)=X5(I)-X3(I)
      X46(I)=X6(I)-X4(I)
      Y17(I)=Y7(I)-Y1(I)
      Y28(I)=Y8(I)-Y2(I)
      Y35(I)=Y5(I)-Y3(I)
      Y46(I)=Y6(I)-Y4(I)
      Z17(I)=Z7(I)-Z1(I)
      Z28(I)=Z8(I)-Z2(I)
      Z35(I)=Z5(I)-Z3(I)
      Z46(I)=Z6(I)-Z4(I)
 10   CONTINUE
      DO 20 I=LFT,LLT
      RX(I)=X17(I)+X28(I)-X35(I)-X46(I)
      RY(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
      RZ(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
      A17(I)=X17(I)+X46(I)
      A28(I)=X28(I)+X35(I)
      B17(I)=Y17(I)+Y46(I)
      B28(I)=Y28(I)+Y35(I)
      C17(I)=Z17(I)+Z46(I)
      C28(I)=Z28(I)+Z35(I)
 20   CONTINUE
      DO 30 I=LFT,LLT
      SX(I)=A17(I)+A28(I)
      SY(I)=B17(I)+B28(I)
      SZ(I)=C17(I)+C28(I)
      TX(I)=A17(I)-A28(I)
      TY(I)=B17(I)-B28(I)
      TZ(I)=C17(I)-C28(I)
 30   CONTINUE
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
