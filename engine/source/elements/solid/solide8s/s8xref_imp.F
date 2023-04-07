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
Chd|  S8XREF_IMP                    source/elements/solid/solide8s/srcoor3_imp.F
Chd|-- called by -----------
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        SRCOOR3_IMP                   source/elements/solid/solide8s/srcoor3_imp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S8XREF_IMP(
     1   OFFG,    XREF,    XD1,     XD2,
     2   XD3,     XD4,     XD5,     XD6,
     3   XD7,     XD8,     YD1,     YD2,
     4   YD3,     YD4,     YD5,     YD6,
     5   YD7,     YD8,     ZD1,     ZD2,
     6   ZD3,     ZD4,     ZD5,     ZD6,
     7   ZD7,     ZD8,     R,       NEL)
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
      my_real
     .   OFFG(*)
      
      DOUBLE PRECISION 
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ), 
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ), 
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ), 
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ), 
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ), 
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ),
     .   R(3,3,MVSIZ), XREF(NEL,21)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      DOUBLE PRECISION
     .    U,V,W,UL,VL,WL
C=======================================================================
        DO I=1,NEL
          !IF(ABS(OFFG(I)) <= ONE )THEN           !sb voir comment faire avec OFFG
            U = XD2(I)-XD1(I)
            V = YD2(I)-YD1(I)
            W = ZD2(I)-ZD1(I)
            UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
            VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
            WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
            XREF(I,1) = UL
            XREF(I,2) = VL
            XREF(I,3) = WL

            U = XD3(I)-XD1(I)
            V = YD3(I)-YD1(I)
            W = ZD3(I)-ZD1(I)
            UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
            VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
            WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
            XREF(I,4) = UL
            XREF(I,5) = VL
            XREF(I,6) = WL
      
            U = XD4(I)-XD1(I)
            V = YD4(I)-YD1(I)
            W = ZD4(I)-ZD1(I)
            UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
            VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
            WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
            XREF(I,7) = UL
            XREF(I,8) = VL
            XREF(I,9) = WL
      
            U = XD5(I)-XD1(I)
            V = YD5(I)-YD1(I)
            W = ZD5(I)-ZD1(I)
            UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
            VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
            WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
            XREF(I,10) = UL
            XREF(I,11) = VL
            XREF(I,12) = WL
      
            U = XD6(I)-XD1(I)
            V = YD6(I)-YD1(I)
            W = ZD6(I)-ZD1(I)
            UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
            VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
            WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
            XREF(I,13) = UL
            XREF(I,14) = VL
            XREF(I,15) = WL
      
            U = XD7(I)-XD1(I)
            V = YD7(I)-YD1(I)
            W = ZD7(I)-ZD1(I)
            UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
            VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
            WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
            XREF(I,16) = UL
            XREF(I,17) = VL
            XREF(I,18) = WL
      
            U = XD8(I)-XD1(I)
            V = YD8(I)-YD1(I)
            W = ZD8(I)-ZD1(I)
            UL = R(1,1,I)*U+R(2,1,I)*V+R(3,1,I)*W
            VL = R(1,2,I)*U+R(2,2,I)*V+R(3,2,I)*W
            WL = R(1,3,I)*U+R(2,3,I)*V+R(3,3,I)*W
            XREF(I,19) = UL
            XREF(I,20) = VL
            XREF(I,21) = WL
        
        ENDDO
C-----------
      RETURN
      END
