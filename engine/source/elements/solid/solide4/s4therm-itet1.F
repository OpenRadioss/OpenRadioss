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
Chd|  S4THERM_ITET1                 source/elements/solid/solide4/s4therm-itet1.F
Chd|-- called by -----------
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S4THERM_ITET1(PM   ,MAT  ,NC   ,NEL  ,
     .                         XX   ,YY   ,ZZ   ,DT1  ,DIE ,
     .                         TEMP ,FPHI ,OFFG ,OFF )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "scr_thermal_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL, MAT(*),NC(MVSIZ,10)
      my_real
     .    TEMP(*), DIE(*),
     .    FPHI(MVSIZ,10), PM(NPROPM,*), 
     .    DT1, OFF(*), OFFG(*)
      DOUBLE PRECISION
     .    XX(MVSIZ,10), YY(MVSIZ,10), ZZ(MVSIZ,10)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, MX
      my_real
     .   DET(NEL),
     .   PX1(NEL), PX2(NEL), PX3(NEL), PX4(NEL),  
     .   PY1(NEL), PY2(NEL), PY3(NEL), PY4(NEL),  
     .   PZ1(NEL), PZ2(NEL), PZ3(NEL), PZ4(NEL)
      my_real
     .   B1(NEL), B2(NEL), B3(NEL), B4(NEL), 
     .   C1(NEL), C2(NEL), C3(NEL), C4(NEL),
     .   D1(NEL), D2(NEL), D3(NEL), D4(NEL)
      my_real
     .   X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43
      my_real
     .  CA, CB, KC, PHIX, PHIY, PHIZ, A, D,
     .  TEMP1, TEMP2, TEMP3, TEMP4, TEMPEL
C
C---+----1
      DO I=1,NEL
       X43 = XX(I,4) - XX(I,3)
       Y43 = YY(I,4) - YY(I,3)
       Z43 = ZZ(I,4) - ZZ(I,3)
       X41 = XX(I,4) - XX(I,1)
       Y41 = YY(I,4) - YY(I,1)
       Z41 = ZZ(I,4) - ZZ(I,1)
       X42 = XX(I,4) - XX(I,2)
       Y42 = YY(I,4) - YY(I,2)
       Z42 = ZZ(I,4) - ZZ(I,2)
C
       B1(I) =  Y43*Z42 - Y42*Z43
       B2(I) =  Y41*Z43 - Y43*Z41
       B3(I) =  Y42*Z41 - Y41*Z42
       B4(I) =  -(B1(I) + B2(I) + B3(I))
C
       C1(I) =  Z43*X42 - Z42*X43
       C2(I) =  Z41*X43 - Z43*X41
       C3(I) =  Z42*X41 - Z41*X42
       C4(I) =  -(C1(I) + C2(I) + C3(I))
C
       D1(I) =  X43*Y42 - X42*Y43
       D2(I) =  X41*Y43 - X43*Y41
       D3(I) =  X42*Y41 - X41*Y42
       D4(I) =  -(D1(I) + D2(I) + D3(I))
C
       DET(I) = (X41*B1(I) + Y41*C1(I) + Z41*D1(I))*ONE_OVER_6
      ENDDO

C---+----1
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
      ENDDO

C---+----1
      MX = MAT(1)
      CA = PM(75,MX)
      CB = PM(76,MX)
C
      DO I=1,NEL
         IF(OFF(I)==ZERO.OR.OFFG(I)<=ZERO) CYCLE
         TEMP1 = TEMP(NC(I,1))
         TEMP2 = TEMP(NC(I,2))
         TEMP3 = TEMP(NC(I,3))
         TEMP4 = TEMP(NC(I,4))
C 
C - flux
C      
         PHIX =  TEMP1*PX1(I) + TEMP2*PX2(I) + TEMP3*PX3(I) + TEMP4*PX4(I)                
         PHIY =  TEMP1*PY1(I) + TEMP2*PY2(I) + TEMP3*PY3(I) + TEMP4*PY4(I)       
         PHIZ =  TEMP1*PZ1(I) + TEMP2*PZ2(I) + TEMP3*PZ3(I) + TEMP4*PZ4(I)        
C
         TEMPEL = FOURTH * (TEMP1 + TEMP2 + TEMP3 + TEMP4)
         KC = (CA + CB*TEMPEL)*DT1*DET(I)*THEACCFACT
         PHIX = KC*PHIX
         PHIY = KC*PHIY
         PHIZ = KC*PHIZ
C
C force thermique nodale
C
         A = FOURTH * DIE(I)
         FPHI(I,1) = A - (PHIX*PX1(I) + PHIY*PY1(I) + PZ1(I)*PHIZ)
         FPHI(I,2) = A - (PHIX*PX2(I) + PHIY*PY2(I) + PZ2(I)*PHIZ)
         FPHI(I,3) = A - (PHIX*PX3(I) + PHIY*PY3(I) + PZ3(I)*PHIZ)
         FPHI(I,4) = A - (PHIX*PX4(I) + PHIY*PY4(I) + PZ4(I)*PHIZ)
      ENDDO
C 
      RETURN
      END
