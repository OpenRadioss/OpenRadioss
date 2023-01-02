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
Chd|  CM58IN3                       source/materials/mat/mat058/cm58in3.F
Chd|-- called by -----------
Chd|        C3INIT3                       source/elements/sh3n/coque3n/c3init3.F
Chd|        CINIT3                        source/elements/shell/coque/cinit3.F
Chd|        CMAINI3                       source/elements/sh3n/coquedk/cmaini3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CM58IN3(IREP      ,IMAT  ,IPM   ,DIR1   ,DIR2  ,
     .                   UPARAM    ,UVAR  ,ALDT  ,NEL    ,NUVAR ,TAN_PHI  ,
     .                   X1  ,X2  ,X3  ,X4  ,Y1  ,Y2  ,
     .                   Y3  ,Y4  ,Z1  ,Z2  ,Z3  ,Z4  ,
     .                   E1X ,E2X ,E3X ,E1Y ,E2Y ,E3Y ,E1Z ,E2Z ,E3Z )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "scr03_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NUVAR,IPM(NPROPMI,*),IREP,IMAT
C     REAL
      my_real UPARAM(*),UVAR(NEL,*),ALDT(NEL),TAN_PHI(NEL),DIR1(NEL,2),DIR2(NEL,2)
      my_real, DIMENSION(NEL), INTENT(IN) :: E1X,E2X,E3X,E1Y,E2Y,E3Y,E1Z,E2Z,E3Z,
     .                                       X1,X2,X3,X4,Y1,Y2,Y3,Y4,Z1,Z2,Z3,Z4
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,IADB
      my_real
     .   R,S,D1,D2,D11,D12,D21,D22,U1X,U1Y,U2X,U2Y,DET,W1X,W2X,W1Y,W2Y,
     .   PHI,CSP,SNP,TANA,R1,R2,S1,S2,G0,AA,BB,SUMA,V1,V2,V3,WR,WS
      my_real , DIMENSION(NEL) :: E11,E12,E13,E21,E22,E23
C======================================================================|
C---  isoparametric (material) axes        
      IF (ITY == 3) THEN                   
C---    shell 4N                           
        DO I=1,NEL                       
          E11(I)= X2(I)+X3(I)-X1(I)-X4(I)  
          E12(I)= Y2(I)+Y3(I)-Y1(I)-Y4(I)  
          E13(I)= Z2(I)+Z3(I)-Z1(I)-Z4(I)  
          E21(I)= X3(I)+X4(I)-X1(I)-X2(I)  
          E22(I)= Y3(I)+Y4(I)-Y1(I)-Y2(I)  
          E23(I)= Z3(I)+Z4(I)-Z1(I)-Z2(I)  
        ENDDO                              
      ELSEIF (ITY == 7) THEN                              
C---    shell 3N                           
        DO I=1,NEL                       
          E11(I)= X2(I)-X1(I)              
          E12(I)= Y2(I)-Y1(I)              
          E13(I)= Z2(I)-Z1(I)              
          E21(I)= X3(I)-X1(I)              
          E22(I)= Y3(I)-Y1(I)              
          E23(I)= Z3(I)-Z1(I)              
        ENDDO                              
      ENDIF                                
C------  Initialise UVAR 
      IADB  = IPM(7,IMAT)-1                                        
      G0    = UPARAM(IADB+13)                                               
      DO I=1,NEL
        AA = DIR1(I,1)                                                     
        BB = DIR1(I,2)                                                     
        V1 = AA*E11(I) + BB*E21(I)                                         
        V2 = AA*E12(I) + BB*E22(I)                                         
        V3 = AA*E13(I) + BB*E23(I)                                         
        WR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)                              
        WS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)                              
        SUMA = MAX( SQRT(WR*WR + WS*WS), EM20)                             
        R1 = WR/SUMA                                                       
        S1 = WS/SUMA                                                       
        AA = DIR2(I,1)                                                     
        BB = DIR2(I,2)                                                     
        V1 = AA*E11(I) + BB*E21(I)                                         
        V2 = AA*E12(I) + BB*E22(I)                                         
        V3 = AA*E13(I) + BB*E23(I)                                         
        WR = V1*E1X(I)+ V2*E1Y(I) + V3*E1Z(I)                              
        WS = V1*E2X(I)+ V2*E2Y(I) + V3*E2Z(I)                              
        SUMA = MAX( SQRT(WR*WR + WS*WS), EM20)                             
        R2   = WR/SUMA
        S2   = WS/SUMA
        TANA = (R1*R2 + S1*S2) / (R1*S2 - R2*S1)   ! Tan(alpha_totale)     
c
        UVAR(I,1:NUVAR) = ZERO                                                 
        UVAR(I,6)  = TANA                                                  
        TAN_PHI(I) = TANA                                                  
        UVAR(I,10) = TANA*G0                                               
        UVAR(I,14) = ALDT(I)                                               
        UVAR(I,40) = ONE                                               
      ENDDO                                                                
C----
      RETURN
      END
