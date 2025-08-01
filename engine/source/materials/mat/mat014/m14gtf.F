Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    m14gtf   ../engine/source/materials/mat/mat014/m14gtf.F
!||--- called by ------------------------------------------------------
!||    m12law   ../engine/source/materials/mat/mat012/m12law.F
!||    m14law   ../engine/source/materials/mat/mat014/m14law.F
!||====================================================================
      SUBROUTINE M14GTF(SIG,AX ,AY ,AZ ,BX ,BY,  
     2                  BZ ,CX ,CY ,CZ ,D1 ,D2,
     3                  D3 ,D4 ,D5 ,D6 ,T1 ,T2,
     4                  T3 ,T4 ,T5 ,T6 ,E1 ,E2,
     5                  E3 ,E4 ,E5 ,E6 ,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
      my_real
     .   SIG(NEL,6),
     .   AX(*),AY(*),AZ(*),BX(*),BY(*),BZ(*),CX(*),CY(*),CZ(*)
C     REAL
      my_real
     .   T1(*), T2(*), T3(*), T4(*), T5(*), T6(*), D1(*), D2(*),
     .   D3(*), D4(*), D5(*), D6(*), E1(*), E2(*), E3(*), E4(*), E5(*),
     .   E6(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C-----------------------------------------------
      DO 200 I=1,NEL
      E1(I)=AX(I)*AX(I)*D1(I)
     .     +AY(I)*AY(I)*D2(I)
     .     +AZ(I)*AZ(I)*D3(I)
     .     +AX(I)*AY(I)*D4(I)
     .     +AY(I)*AZ(I)*D5(I)
     .     +AZ(I)*AX(I)*D6(I)
      E2(I)=BX(I)*BX(I)*D1(I)
     .     +BY(I)*BY(I)*D2(I)
     .     +BZ(I)*BZ(I)*D3(I)
     .     +BX(I)*BY(I)*D4(I)
     .     +BY(I)*BZ(I)*D5(I)
     .     +BZ(I)*BX(I)*D6(I)
      E3(I)=CX(I)*CX(I)*D1(I)
     .     +CY(I)*CY(I)*D2(I)
     .     +CZ(I)*CZ(I)*D3(I)
     .     +CX(I)*CY(I)*D4(I)
     .     +CY(I)*CZ(I)*D5(I)
     .     +CZ(I)*CX(I)*D6(I)
      E4(I)=AX(I)*BX(I)*D1(I)
     .     +AY(I)*BY(I)*D2(I)
     .     +AZ(I)*BZ(I)*D3(I)
     .     +(AX(I)*BY(I)+AY(I)*BX(I))*D4(I)*HALF
     .     +(AY(I)*BZ(I)+AZ(I)*BY(I))*D5(I)*HALF
     .     +(AZ(I)*BX(I)+AX(I)*BZ(I))*D6(I)*HALF
      E5(I)=BX(I)*CX(I)*D1(I)
     .     +BY(I)*CY(I)*D2(I)
     .     +BZ(I)*CZ(I)*D3(I)
     .     +(BX(I)*CY(I)+BY(I)*CX(I))*D4(I)*HALF
     .     +(BY(I)*CZ(I)+BZ(I)*CY(I))*D5(I)*HALF
     .     +(BZ(I)*CX(I)+BX(I)*CZ(I))*D6(I)*HALF
      E6(I)=CX(I)*AX(I)*D1(I)
     .     +CY(I)*AY(I)*D2(I)
     .     +CZ(I)*AZ(I)*D3(I)
     .     +(CX(I)*AY(I)+CY(I)*AX(I))*D4(I)*HALF
     .     +(CY(I)*AZ(I)+CZ(I)*AY(I))*D5(I)*HALF
     .     +(CZ(I)*AX(I)+CX(I)*AZ(I))*D6(I)*HALF
      E4(I)=E4(I)*TWO
      E5(I)=E5(I)*TWO
      E6(I)=E6(I)*TWO
 200   CONTINUE
C
      DO 400 I=1,NEL
      T1(I)=AX(I)*AX(I)*SIG(I,1)
     .     +AY(I)*AY(I)*SIG(I,2)
     .     +AZ(I)*AZ(I)*SIG(I,3)
     .     +TWO*AX(I)*AY(I)*SIG(I,4)
     .     +TWO*AY(I)*AZ(I)*SIG(I,5)
     .     +TWO*AZ(I)*AX(I)*SIG(I,6)
      T2(I)=BX(I)*BX(I)*SIG(I,1)
     .     +BY(I)*BY(I)*SIG(I,2)
     .     +BZ(I)*BZ(I)*SIG(I,3)
     .     +TWO*BX(I)*BY(I)*SIG(I,4)
     .     +TWO*BY(I)*BZ(I)*SIG(I,5)
     .     +TWO*BZ(I)*BX(I)*SIG(I,6)
      T3(I)=CX(I)*CX(I)*SIG(I,1)
     .     +CY(I)*CY(I)*SIG(I,2)
     .     +CZ(I)*CZ(I)*SIG(I,3)
     .     +TWO*CX(I)*CY(I)*SIG(I,4)
     .     +TWO*CY(I)*CZ(I)*SIG(I,5)
     .     +TWO*CZ(I)*CX(I)*SIG(I,6)
      T4(I)=AX(I)*BX(I)*SIG(I,1)
     .     +AY(I)*BY(I)*SIG(I,2)
     .     +AZ(I)*BZ(I)*SIG(I,3)
     .     +(AX(I)*BY(I)+AY(I)*BX(I))*SIG(I,4)
     .     +(AY(I)*BZ(I)+AZ(I)*BY(I))*SIG(I,5)
     .     +(AZ(I)*BX(I)+AX(I)*BZ(I))*SIG(I,6)
      T5(I)=BX(I)*CX(I)*SIG(I,1)
     .     +BY(I)*CY(I)*SIG(I,2)
     .     +BZ(I)*CZ(I)*SIG(I,3)
     .     +(BX(I)*CY(I)+BY(I)*CX(I))*SIG(I,4)
     .     +(BY(I)*CZ(I)+BZ(I)*CY(I))*SIG(I,5)
     .     +(BZ(I)*CX(I)+BX(I)*CZ(I))*SIG(I,6)
      T6(I)=CX(I)*AX(I)*SIG(I,1)
     .     +CY(I)*AY(I)*SIG(I,2)
     .     +CZ(I)*AZ(I)*SIG(I,3)
     .     +(CX(I)*AY(I)+CY(I)*AX(I))*SIG(I,4)
     .     +(CY(I)*AZ(I)+CZ(I)*AY(I))*SIG(I,5)
     .     +(CZ(I)*AX(I)+CX(I)*AZ(I))*SIG(I,6)
 400  CONTINUE
C
      RETURN
      END
