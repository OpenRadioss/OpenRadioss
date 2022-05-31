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
Chd|  STHERM                        source/elements/solid/solide/stherm.F
Chd|-- called by -----------
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE STHERM(PM   ,MAT  ,VOL,
     .                 NC1,  NC2, NC3,NC4,
     .                 NC5,  NC6, NC7,NC8,  
     .                 PX1, PX2, PX3, PX4,
     .                 PY1, PY2, PY3, PY4,
     .                 PZ1, PZ2, PZ3, PZ4,
     .                 DT1 ,TEMPNC,TEL,DIE,
     .                 FPHI,OFFG,OFF,
     .                 PARTSAV,IPARTS,VOL0)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
#include      "scr_thermal_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(*),NC1(MVSIZ),NC2(MVSIZ),
     .       NC3(MVSIZ),NC4(MVSIZ),NC5(MVSIZ),NC6(MVSIZ),
     .       NC7(MVSIZ),NC8(MVSIZ)
      INTEGER IPARTS(*)
C     REAL ou REAL*8
      my_real
     .    VOL(*), PX1(*), PX2(*), PX3(*), PX4(*), 
     .    PY1(*), PY2(*), PY3(*), PY4(*),
     .    PZ1(*), PZ2(*), PZ3(*), PZ4(*),
     .    TEMPNC(*), FPHI(MVSIZ,8), PM(NPROPM,*),DIE(*),
     .    DT1,TEL(*),OFF(*),OFFG(*)
      my_real PARTSAV(NPSAV,*),VOL0(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J, M, MX
C     REAL ou REAL*8
      my_real CA, CB, KC, PHIX, PHIY, PHIZ, A, B, SPH, T0
C 

      MX  =MAT(LFT)
      CA = PM(75,MX)
      CB = PM(76,MX)
      DO I=LFT,LLT
          IF(OFF(I).EQ.ZERO.OR.OFFG(I).LE.ZERO) CYCLE
C 
C - flux  
C      
          PHIX =  TEMPNC(NC1(I))*PX1(I) + TEMPNC(NC2(I))*PX2(I)  +
     .            TEMPNC(NC3(I))*PX3(I) + TEMPNC(NC4(I))*PX4(I)  -
     .            TEMPNC(NC5(I))*PX3(I) - TEMPNC(NC6(I))*PX4(I)  -
     .            TEMPNC(NC7(I))*PX1(I) - TEMPNC(NC8(I))*PX2(I)       
          
          PHIY =  TEMPNC(NC1(I))*PY1(I) + TEMPNC(NC2(I))*PY2(I)  +
     .            TEMPNC(NC3(I))*PY3(I) + TEMPNC(NC4(I))*PY4(I)  -
     .            TEMPNC(NC5(I))*PY3(I) - TEMPNC(NC6(I))*PY4(I)  -
     .            TEMPNC(NC7(I))*PY1(I) - TEMPNC(NC8(I))*PY2(I)
      
          PHIZ =  TEMPNC(NC1(I))*PZ1(I) + TEMPNC(NC2(I))*PZ2(I)  +
     .            TEMPNC(NC3(I))*PZ3(I) + TEMPNC(NC4(I))*PZ4(I)  -
     .            TEMPNC(NC5(I))*PZ3(I) - TEMPNC(NC6(I))*PZ4(I)  -
     .            TEMPNC(NC7(I))*PZ1(I) - TEMPNC(NC8(I))*PZ2(I)          
C
          KC = (CA + CB*TEL(I))*DT1*VOL(I)*THEACCFACT
          PHIX = KC*PHIX
          PHIY = KC*PHIY
          PHIZ = KC*PHIZ         
C
C force thermique nodale
C
          A = ONE_OVER_8* DIE(I)
          B = PHIX*PX1(I) + PHIY*PY1(I) + PZ1(I)*PHIZ        
          FPHI(I,1) = A - B
          FPHI(I,7) = A + B
          B = PHIX*PX2(I) + PHIY*PY2(I) + PZ2(I)*PHIZ
          FPHI(I,2) = A - B
          FPHI(I,8) = A + B
          B = PHIX*PX3(I) + PHIY*PY3(I) + PZ3(I)*PHIZ
          FPHI(I,3) = A - B
          FPHI(I,5) = A + B
          B = PHIX*PX4(I) + PHIY*PY4(I) + PZ4(I)*PHIZ
          FPHI(I,4) = A - B 
          FPHI(I,6) = A + B
      ENDDO
C  
      SPH = PM(69,MX)
      T0  = PM(79,MX)
      DO I=LFT,LLT
         IF(OFF(I).EQ.ZERO.OR.OFFG(I).LE.ZERO) CYCLE
         M   = IPARTS(I)
         PARTSAV(28,M)=PARTSAV(28,M) + SPH*VOL0(I)*(TEL(I)-T0) +DIE(I)
      ENDDO
      RETURN
      END
