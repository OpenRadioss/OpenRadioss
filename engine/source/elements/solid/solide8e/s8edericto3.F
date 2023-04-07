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
Chd|  S8EDERICTO3                   source/elements/solid/solide8e/s8ederi3.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8EDERICTO3(
     1   OFF,     DET,     PX1,     PX2,
     2   PX3,     PX4,     PY1,     PY2,
     3   PY3,     PY4,     PZ1,     PZ2,
     4   PZ3,     PZ4,     JAC_I,   NEL)
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NEL
C     REAL   
      my_real
     .   OFF(*),DET(*),  
     .   PX1(*), PX2(*), PX3(*), PX4(*),  
     .   PY1(*), PY2(*), PY3(*), PY4(*),  
     .   PZ1(*), PZ2(*), PZ3(*), PZ4(*),  
     .   JAC_I(10,MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J ,ICOR
C     REAL
C                                                                     12
      my_real
     .   AJI1(MVSIZ), AJI2(MVSIZ), AJI3(MVSIZ),
     .   AJI4(MVSIZ), AJI5(MVSIZ), AJI6(MVSIZ),
     .   AJI7(MVSIZ), AJI8(MVSIZ), AJI9(MVSIZ),
     .   AJ12(MVSIZ), AJ45(MVSIZ), AJ78(MVSIZ)
C-----------------------------------------------
C  
      DO I=1,NEL
      AJI1(I)=JAC_I(1,I)
      AJI4(I)=JAC_I(4,I)
      AJI7(I)=JAC_I(7,I)
      AJI2(I)=JAC_I(2,I)
      AJI5(I)=JAC_I(5,I)
      AJI8(I)=JAC_I(8,I)
      AJI3(I)=JAC_I(3,I)
      AJI6(I)=JAC_I(6,I)
      AJI9(I)=JAC_I(9,I)
      DET(I) =JAC_I(10,I)
      END DO
C
      DO 160 I=1,NEL
      AJ12(I)=AJI1(I)-AJI2(I)
      AJ45(I)=AJI4(I)-AJI5(I)
  160 AJ78(I)=AJI7(I)-AJI8(I)
      DO 170 I=1,NEL
      PX2(I)= AJ12(I)-AJI3(I)
      PY2(I)= AJ45(I)-AJI6(I)
      PZ2(I)= AJ78(I)-AJI9(I)
      PX4(I)=-AJ12(I)-AJI3(I)
      PY4(I)=-AJ45(I)-AJI6(I)
  170 PZ4(I)=-AJ78(I)-AJI9(I)
      DO 180 I=1,NEL
      AJ12(I)=AJI1(I)+AJI2(I)
      AJ45(I)=AJI4(I)+AJI5(I)
  180 AJ78(I)=AJI7(I)+AJI8(I)
      DO 190 I=1,NEL
      PX1(I)=-AJ12(I)-AJI3(I)
      PY1(I)=-AJ45(I)-AJI6(I)
      PZ1(I)=-AJ78(I)-AJI9(I)
      PX3(I)=AJ12(I)-AJI3(I)
      PY3(I)=AJ45(I)-AJI6(I)
  190 PZ3(I)=AJ78(I)-AJI9(I)
C       
      RETURN
      END 
