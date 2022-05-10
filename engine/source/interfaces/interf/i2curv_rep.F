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
Chd|  I2CURV_REP                    source/interfaces/interf/i2curv_rep.F
Chd|-- called by -----------
Chd|        I2CURVF                       source/interfaces/interf/i2curvf.F
Chd|        I2CURVFP                      source/interfaces/interf/i2curvfp.F
Chd|        I2CURVV                       source/interfaces/interf/i2curvv.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE I2CURV_REP(
     .           INOD    , X      ,V       ,LS1     ,LS2     ,
     .           LT1     ,LT2     ,E1X     ,E1Y     ,E1Z     ,
     .           E2X     ,E2Y     ,E2Z     ,E3X     ,E3Y     ,
     .           E3Z     )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER INOD(4)
C     REAL ou REAL*8
      my_real
     .   S,T,HSL
      my_real
     .   X(3,*),V(3,*)
      my_real
     .   E1X,E1Y,E1Z,E2X,E2Y,E2Z,E3X,E3Y,E3Z,
     .   LS1,LS2,LT1,LT2
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,J1,J2,J3,J4,II,JJ,L,NN,NIR
C     REAL ou REAL*8
      my_real
     .   X21,X32,X34,X41,Y21,Y32,Y34,Y41,Z21,Z32,Z34,Z41,
     .   SM,SP,TM,TP,SM2,SP2,TM2,TP2,SM3,SP3,TM3,TP3,SUMA,S1,S2
      my_real
     .   XM(4),YM(4),ZM(4)
C=======================================================================
      NIR=4                                                           
      IF (INOD(3) == INOD(4)) THEN                                      
        NIR=3                                                         
        XM(4)=ZERO                                                    
        YM(4)=ZERO                                                    
        ZM(4)=ZERO                                                    
      ENDIF                                                           
      DO JJ=1,NIR                                                     
        J=INOD(JJ)  
        XM(JJ)=X(1,J)+ V(1,J)*DT12*UNDEMI                                                
        YM(JJ)=X(2,J)+ V(2,J)*DT12*UNDEMI                                                
        ZM(JJ)=X(3,J)+ V(3,J)*DT12*UNDEMI                                                
      ENDDO                                                           
C-----repere local master                                             
      X21 = XM(2) - XM(1)                                             
      X32 = XM(3) - XM(2)                                             
      X34 = XM(3) - XM(4)                                             
      X41 = XM(4) - XM(1)                                             
      Y21 = YM(2) - YM(1)                                             
      Y32 = YM(3) - YM(2)                                             
      Y34 = YM(3) - YM(4)                                             
      Y41 = YM(4) - YM(1)                                             
      Z21 = ZM(2) - ZM(1)                                             
      Z32 = ZM(3) - ZM(2)                                             
      Z34 = ZM(3) - ZM(4)                                             
      Z41 = ZM(4) - ZM(1)                                             
                                                                      
      E1X = (X21+X34 )                                                
      E1Y = (Y21+Y34 )                                                
      E1Z = (Z21+Z34 )                                                

      E2X = (X32+X41 )                                                
      E2Y = (Y32+Y41 )                                                
      E2Z = (Z32+Z41 )                                                

      E3X = E1Y*E2Z-E1Z*E2Y                                           
      E3Y = E1Z*E2X-E1X*E2Z                                           
      E3Z = E1X*E2Y-E1Y*E2X                                           
C
      SUMA   = E3X*E3X+E3Y*E3Y+E3Z*E3Z                                
      SUMA   = UN / MAX(SQRT(SUMA),EM20)                              
      E3X = E3X * SUMA                                                
      E3Y = E3Y * SUMA                                                
      E3Z = E3Z * SUMA                                                
C
      S1     = E1X*E1X+E1Y*E1Y+E1Z*E1Z                                
      S2     = E2X*E2X+E2Y*E2Y+E2Z*E2Z                                
      SUMA   = SQRT(S1/S2)                                            
      E1X = E1X + (E2Y*E3Z-E2Z*E3Y)*SUMA                              
      E1Y = E1Y + (E2Z*E3X-E2X*E3Z)*SUMA                              
      E1Z = E1Z + (E2X*E3Y-E2Y*E3X)*SUMA                              
C
      SUMA   = E1X*E1X+E1Y*E1Y+E1Z*E1Z                                
      SUMA   = UN / MAX(SQRT(SUMA),EM20)                              
      E1X = E1X * SUMA                                                
      E1Y = E1Y * SUMA                                                
      E1Z = E1Z * SUMA                                                
C
      E2X = E3Y * E1Z - E3Z * E1Y                                     
      E2Y = E3Z * E1X - E3X * E1Z                                     
      E2Z = E3X * E1Y - E3Y * E1X                                     
C----- longueurs                                            
      LS1 = SQRT(X21**2 + Y21**2 + Z21**2)                  
      LS2 = SQRT(X34**2 + Y34**2 + Z34**2)                  
      LT1 = SQRT(X41**2 + Y41**2 + Z41**2)                  
      LT2 = SQRT(X32**2 + Y32**2 + Z32**2)   
C-----------
      RETURN
      END
