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
Chd|  SUDERI3                       source/elements/elbuf_init/suderi3.F
Chd|-- called by -----------
Chd|        SUINIT3                       source/elements/elbuf_init/suinit3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SUDERI3(NEL   ,VOL  ,
     .              X1   ,X2   ,X3   ,X4   ,X5   ,X6   ,X7   ,X8   ,
     .              Y1   ,Y2   ,Y3   ,Y4   ,Y5   ,Y6   ,Y7   ,Y8   ,
     .              Z1   ,Z2   ,Z3   ,Z4   ,Z5   ,Z6   ,Z7   ,Z8)
C------------------------------------------------------
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
      my_real
     .   VOL(*), 
     .   X1(*), X2(*), X3(*), X4(*), X5(*), X6(*),
     .   X7(*), X8(*), Y1(*), Y2(*), Y3(*), Y4(*), Y5(*), Y6(*), Y7(*),
     .   Y8(*), Z1(*), Z2(*), Z3(*), Z4(*), Z5(*), Z6(*), Z7(*), Z8(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      
      DOUBLE PRECISION
     .   XD1(MVSIZ), XD2(MVSIZ), XD3(MVSIZ), XD4(MVSIZ),
     .   XD5(MVSIZ), XD6(MVSIZ), XD7(MVSIZ), XD8(MVSIZ),
     .   YD1(MVSIZ), YD2(MVSIZ), YD3(MVSIZ), YD4(MVSIZ),
     .   YD5(MVSIZ), YD6(MVSIZ), YD7(MVSIZ), YD8(MVSIZ),
     .   ZD1(MVSIZ), ZD2(MVSIZ), ZD3(MVSIZ), ZD4(MVSIZ),
     .   ZD5(MVSIZ), ZD6(MVSIZ), ZD7(MVSIZ), ZD8(MVSIZ)
C     REAL
      my_real
     .   JAC1(MVSIZ), JAC2(MVSIZ), JAC3(MVSIZ), JAC4(MVSIZ), JAC5(MVSIZ), JAC6(MVSIZ),
     .   JAC7(MVSIZ), JAC8(MVSIZ) , JAC9(MVSIZ),
     .   A17(MVSIZ) , A28(MVSIZ) ,
     .   B17(MVSIZ) , B28(MVSIZ) ,
     .   C17(MVSIZ) , C28(MVSIZ) 
      my_real
     .   X17(MVSIZ), X28(MVSIZ), X35(MVSIZ), X46(MVSIZ),
     .   Y17(MVSIZ), Y28(MVSIZ), Y35(MVSIZ), Y46(MVSIZ),
     .   Z17(MVSIZ), Z28(MVSIZ), Z35(MVSIZ), Z46(MVSIZ),
     .   JAC_59_68(MVSIZ), JAC_67_49(MVSIZ), JAC_48_57(MVSIZ),
     .   JAC12(MVSIZ),   JAC45(MVSIZ),   JAC78(MVSIZ)
C=======================================================================
      DO I=1,NEL
        XD1(I)=X1(I)
        XD2(I)=X2(I)
        XD3(I)=X3(I)
        XD4(I)=X4(I)
        XD5(I)=X5(I)
        XD6(I)=X6(I)
        XD7(I)=X7(I)
        XD8(I)=X8(I)
        YD1(I)=Y1(I)
        YD2(I)=Y2(I)
        YD3(I)=Y3(I)
        YD4(I)=Y4(I)
        YD5(I)=Y5(I)
        YD6(I)=Y6(I)
        YD7(I)=Y7(I)
        YD8(I)=Y8(I)
        ZD1(I)=Z1(I)
        ZD2(I)=Z2(I)
        ZD3(I)=Z3(I)
        ZD4(I)=Z4(I)
        ZD5(I)=Z5(I)
        ZD6(I)=Z6(I)
        ZD7(I)=Z7(I)
        ZD8(I)=Z8(I)
      ENDDO 

      DO I=1,NEL
        X17(I)=XD7(I)-XD1(I)
        X28(I)=XD8(I)-XD2(I)
        X35(I)=XD5(I)-XD3(I)
        X46(I)=XD6(I)-XD4(I)
        Y17(I)=YD7(I)-YD1(I)
        Y28(I)=YD8(I)-YD2(I)
        Y35(I)=YD5(I)-YD3(I)
        Y46(I)=YD6(I)-YD4(I)
        Z17(I)=ZD7(I)-ZD1(I)
        Z28(I)=ZD8(I)-ZD2(I)
        Z35(I)=ZD5(I)-ZD3(I)
        Z46(I)=ZD6(I)-ZD4(I)
      ENDDO 
C
      DO I=1,NEL               
        JAC1(I)=X17(I)+X28(I)-X35(I)-X46(I)
        JAC2(I)=Y17(I)+Y28(I)-Y35(I)-Y46(I)
        JAC3(I)=Z17(I)+Z28(I)-Z35(I)-Z46(I)
        A17(I)=X17(I)+X46(I)
        A28(I)=X28(I)+X35(I)
        B17(I)=Y17(I)+Y46(I)
        B28(I)=Y28(I)+Y35(I)
        C17(I)=Z17(I)+Z46(I)
        C28(I)=Z28(I)+Z35(I)
      ENDDO 
         
      DO I=1,NEL
        JAC4(I)=A17(I)+A28(I)
        JAC5(I)=B17(I)+B28(I)
        JAC6(I)=C17(I)+C28(I)
        JAC7(I)=A17(I)-A28(I)
        JAC8(I)=B17(I)-B28(I)
        JAC9(I)=C17(I)-C28(I)
      ENDDO 
C
      DO I=1,NEL
        JAC_59_68(I)=JAC5(I)*JAC9(I)-JAC6(I)*JAC8(I)
        JAC_67_49(I)=JAC6(I)*JAC7(I)-JAC4(I)*JAC9(I)
        JAC_48_57(I)=JAC4(I)*JAC8(I)-JAC5(I)*JAC7(I)
      ENDDO
C
      DO I=1,NEL
        VOL(I)=ONE_OVER_64*(JAC1(I)*JAC_59_68(I)+JAC2(I)*JAC_67_49(I)+JAC3(I)*JAC_48_57(I))
      ENDDO     
C-----------
      RETURN
      END
