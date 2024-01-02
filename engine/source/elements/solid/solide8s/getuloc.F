Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  GETULOC                       source/elements/solid/solide8s/getuloc.F
Chd|-- called by -----------
Chd|        SRCOOR3_IMP                   source/elements/solid/solide8s/srcoor3_imp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE GETULOC(
     1   SAV,     XD1,     XD2,     XD3,
     2   XD4,     XD5,     XD6,     XD7,
     3   XD8,     YD1,     YD2,     YD3,
     4   YD4,     YD5,     YD6,     YD7,
     5   YD8,     ZD1,     ZD2,     ZD3,
     6   ZD4,     ZD5,     ZD6,     ZD7,
     7   ZD8,     ULX1,    ULX2,    ULX3,
     8   ULX4,    ULX5,    ULX6,    ULX7,
     9   ULX8,    ULY1,    ULY2,    ULY3,
     A   ULY4,    ULY5,    ULY6,    ULY7,
     B   ULY8,    ULZ1,    ULZ2,    ULZ3,
     C   ULZ4,    ULZ5,    ULZ6,    ULZ7,
     D   ULZ8,    R,       NEL)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL
C     REAL
!      my_real
!     .   OFFG(*)
      
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),
     .   SAV(NEL,21), R(3,3,MVSIZ),
     .   ULX1(MVSIZ), ULX2(MVSIZ), ULX3(MVSIZ), ULX4(MVSIZ),
     .   ULX5(MVSIZ), ULX6(MVSIZ), ULX7(MVSIZ), ULX8(MVSIZ),
     .   ULY1(MVSIZ), ULY2(MVSIZ), ULY3(MVSIZ), ULY4(MVSIZ),
     .   ULY5(MVSIZ), ULY6(MVSIZ), ULY7(MVSIZ), ULY8(MVSIZ),
     .   ULZ1(MVSIZ), ULZ2(MVSIZ), ULZ3(MVSIZ), ULZ4(MVSIZ),
     .   ULZ5(MVSIZ), ULZ6(MVSIZ), ULZ7(MVSIZ), ULZ8(MVSIZ)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I    
C=======================================================================
      DOUBLE PRECISION U,V,W,UL,VL,WL 

      DO I=1,NEL
        ULX1(I) = ZERO    ! sb : On pourrait eviter de stocker ces 3 tables (toujours zero)
        ULY1(I) = ZERO
        ULZ1(I) = ZERO

        U = (XD2(I)-XD1(I))
        V = (YD2(I)-YD1(I))
        W = (ZD2(I)-ZD1(I))
        UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
        VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
        WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
        ULX2(I) = UL-SAV(I,1)
        ULY2(I) = VL-SAV(I,2)
        ULZ2(I) = WL-SAV(I,3)

        U = (XD3(I)-XD1(I))
        V = (YD3(I)-YD1(I))
        W = (ZD3(I)-ZD1(I))
        UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
        VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
        WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
        ULX3(I) = UL-SAV(I,4)
        ULY3(I) = VL-SAV(I,5)
        ULZ3(I) = WL-SAV(I,6)
      
        U = (XD4(I)-XD1(I))
        V = (YD4(I)-YD1(I))
        W = (ZD4(I)-ZD1(I))
        UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
        VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
        WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
        ULX4(I) = UL-SAV(I,7)
        ULY4(I) = VL-SAV(I,8)
        ULZ4(I) = WL-SAV(I,9)
      
        U = (XD5(I)-XD1(I))
        V = (YD5(I)-YD1(I))
        W = (ZD5(I)-ZD1(I))
        UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
        VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
        WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
        ULX5(I) = UL-SAV(I,10)
        ULY5(I) = VL-SAV(I,11)
        ULZ5(I) = WL-SAV(I,12)
      
        U = (XD6(I)-XD1(I))
        V = (YD6(I)-YD1(I))
        W = (ZD6(I)-ZD1(I))
        UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
        VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
        WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
        ULX6(I) = UL-SAV(I,13)
        ULY6(I) = VL-SAV(I,14)
        ULZ6(I) = WL-SAV(I,15)
      
        U = (XD7(I)-XD1(I))
        V = (YD7(I)-YD1(I))
        W = (ZD7(I)-ZD1(I))
        UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
        VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
        WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
        ULX7(I) = UL-SAV(I,16)
        ULY7(I) = VL-SAV(I,17)
        ULZ7(I) = WL-SAV(I,18)
      
        U = (XD8(I)-XD1(I))
        V = (YD8(I)-YD1(I))
        W = (ZD8(I)-ZD1(I))
        UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
        VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
        WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
        ULX8(I) = UL-SAV(I,19)
        ULY8(I) = VL-SAV(I,20)
        ULZ8(I) = WL-SAV(I,21)
        
      ENDDO
      RETURN
      END
