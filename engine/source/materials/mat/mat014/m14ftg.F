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
Chd|  M14FTG                        source/materials/mat/mat014/m14ftg.F
Chd|-- called by -----------
Chd|        M12LAW                        source/materials/mat/mat012/m12law.F
Chd|        M14LAW                        source/materials/mat/mat014/m14law.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M14FTG(SIG,AX ,AY ,AZ ,BX ,BY ,  
     2                  BZ ,CX ,CY ,CZ ,T1 ,T2 ,
     3                  T3 ,T4 ,T5 ,T6 ,NEL)
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
      INTEGER NEL
C     REAL ou REAL*8
      my_real
     .   SIG(NEL,6),T1(*), T2(*), T3(*), T4(*), T5(*), T6(*),
     .   AX(*),AY(*),AZ(*),BX(*),BY(*),BZ(*),CX(*),CY(*),CZ(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      DO 400 I=LFT,LLT
      SIG(I,1)=AX(I)*AX(I)*T1(I)
     .        +BX(I)*BX(I)*T2(I)
     .        +CX(I)*CX(I)*T3(I)
     .     +TWO*AX(I)*BX(I)*T4(I)
     .     +TWO*BX(I)*CX(I)*T5(I)
     .     +TWO*CX(I)*AX(I)*T6(I)
      SIG(I,2)=AY(I)*AY(I)*T1(I)
     .        +BY(I)*BY(I)*T2(I)
     .        +CY(I)*CY(I)*T3(I)
     .     +TWO*AY(I)*BY(I)*T4(I)
     .     +TWO*BY(I)*CY(I)*T5(I)
     .     +TWO*CY(I)*AY(I)*T6(I)
      SIG(I,3)=AZ(I)*AZ(I)*T1(I)
     .        +BZ(I)*BZ(I)*T2(I)
     .        +CZ(I)*CZ(I)*T3(I)
     .     +TWO*AZ(I)*BZ(I)*T4(I)
     .     +TWO*BZ(I)*CZ(I)*T5(I)
     .     +TWO*CZ(I)*AZ(I)*T6(I)
      SIG(I,4)=AX(I)*AY(I)*T1(I)
     .        +BX(I)*BY(I)*T2(I)
     .        +CX(I)*CY(I)*T3(I)
     .     +(AX(I)*BY(I)+BX(I)*AY(I))*T4(I)
     .     +(BX(I)*CY(I)+CX(I)*BY(I))*T5(I)
     .     +(CX(I)*AY(I)+AX(I)*CY(I))*T6(I)
      SIG(I,5)=AY(I)*AZ(I)*T1(I)
     .        +BY(I)*BZ(I)*T2(I)
     .       +CY(I)*CZ(I)*T3(I)
     .     +(AY(I)*BZ(I)+BY(I)*AZ(I))*T4(I)
     .     +(BY(I)*CZ(I)+CY(I)*BZ(I))*T5(I)
     .     +(CY(I)*AZ(I)+AY(I)*CZ(I))*T6(I)
      SIG(I,6)=AZ(I)*AX(I)*T1(I)
     .        +BZ(I)*BX(I)*T2(I)
     .        +CZ(I)*CX(I)*T3(I)
     .     +(AZ(I)*BX(I)+BZ(I)*AX(I))*T4(I)
     .     +(BZ(I)*CX(I)+CZ(I)*BX(I))*T5(I)
     .     +(CZ(I)*AX(I)+AZ(I)*CX(I))*T6(I)
 400  CONTINUE
C
      RETURN
      END
