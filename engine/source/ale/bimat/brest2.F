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
Chd|  BREST2                        source/ale/bimat/brest2.F     
Chd|-- called by -----------
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE BREST2(ALPH,VOLT,PM,OFF,VOL0,SOFF,IC,VOLN,
     .     AIRE, AIRES,
     .     D1, D2, D3, D4, D5, D6,
     .     D1S, D2S, D3S, D4S, D5S, D6S,
     .     VD2, DVOL,
     .     MAT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "com06_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   SOFF
      INTEGER IC(7,*), MAT(*)
      my_real
     .   ALPH(*), VOLT(*), PM(NPROPM,*), OFF(*), VOL0(*),VOLN(MVSIZ),
     .   AIRE(*), AIRES(*),
     .   D1(*), D2(*), D3(*), D4(*), D5(*), D6(*),
     .   D1S(*), D2S(*), D3S(*), D4S(*), D5S(*), D6S(*),
     .   VD2(*), DVOL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MT
C-----------------------------------------------
      DO I=LFT,LLT
        MT=IC(1,I)
        MAT(I)=NINT(PM(20+JMULT,MT))
        VOLN(I)=VOLT(I)
        AIRE(I)=AIRES(I)
        D1(I)=D1S(I)
        D2(I)=D2S(I)
        D3(I)=D3S(I)
        D4(I)=D4S(I)
        D5(I)=D5S(I)
        D6(I)=D6S(I)
      ENDDO

      RETURN
      END
