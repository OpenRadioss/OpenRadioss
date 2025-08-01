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
!||    srepiso3   ../starter/source/elements/solid/solide/srepiso3.F
!||--- called by ------------------------------------------------------
!||    srcoor3    ../starter/source/elements/solid/solide/srcoor3.F
!||    srefsta3   ../starter/source/elements/solid/solide/srefsta3.F
!||====================================================================
      SUBROUTINE SREPISO3(
     .   X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,  
     .   Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,  
     .   Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8   ,  
     .   RX   ,RY   ,RZ   ,SX   ,SY   ,SZ   ,TX   ,TY   ,
     .   TZ   ,F1X  ,F1Y  ,F1Z  ,F2X  ,F2Y  ,F2Z  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*), X7(*), X8(*),
     .   Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*), Y8(*),  
     .   Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*),  
     .   RX(*) ,RY(*) ,RZ(*) ,SX(*) ,SY(*) ,SZ(*) ,TX(*) ,TY(*) ,TZ(*),
     .   F1X(*),F1Y(*),F1Z(*),F2X(*),F2Y(*),F2Z(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   X17 , X28 , X35 , X46, Y17 , Y28 , Y35 , Y46, 
     .   Z17 , Z28 , Z35 , Z46, A17 , A28 , B17 , B28 , C17 , C28 
C=======================================================================
C     Repere isoparametrique
C
      DO I=LFT,LLT
        F1X(I) = X2(I)+X3(I)-X1(I)-X4(I)                                     
        F1Y(I) = Y2(I)+Y3(I)-Y1(I)-Y4(I)                                     
        F1Z(I) = Z2(I)+Z3(I)-Z1(I)-Z4(I)                                     
        F2X(I) = X3(I)+X4(I)-X1(I)-X2(I)                                     
        F2Y(I) = Y3(I)+Y4(I)-Y1(I)-Y2(I)                                     
        F2Z(I) = Z3(I)+Z4(I)-Z1(I)-Z2(I)                                     
      ENDDO
      DO I=LFT,LLT
        X17 = X7(I) - X1(I)
        X28 = X8(I) - X2(I)
        X35 = X5(I) - X3(I)
        X46 = X6(I) - X4(I)
        Y17 = Y7(I) - Y1(I)
        Y28 = Y8(I) - Y2(I)
        Y35 = Y5(I) - Y3(I)
        Y46 = Y6(I) - Y4(I)
        Z17 = Z7(I) - Z1(I)
        Z28 = Z8(I) - Z2(I)
        Z35 = Z5(I) - Z3(I)
        Z46 = Z6(I) - Z4(I)
C
        A17 = X17 + X46
        A28 = X28 + X35
        B17 = Y17 + Y46
        B28 = Y28 + Y35
        C17 = Z17 + Z46
        C28 = Z28 + Z35
C
        RX(I) = X17 + X28 - X35 - X46
        RY(I) = Y17 + Y28 - Y35 - Y46
        RZ(I) = Z17 + Z28 - Z35 - Z46
        SX(I) = A17 + A28
        SY(I) = B17 + B28
        SZ(I) = C17 + C28
        TX(I) = A17 - A28
        TY(I) = B17 - B28
        TZ(I) = C17 - C28
      ENDDO
C-------------
      RETURN
      END SUBROUTINE SREPISO3
