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
Chd|  AGRAD3                        source/ale/ale3d/agrad3.F     
Chd|-- called by -----------
Chd|        AGRAD0                        source/ale/agrad0.F           
Chd|        NODAL_SCHLIEREN               source/output/anim/generate/nodal_schlieren.F
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE AGRAD3(IXS,X,ALE_CONNECTIVITY,GRAD)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      USE ALE_CONNECTIVITY_MOD
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
      INTEGER IXS(NIXS,NUMELS)
      my_real X(3,NUMNOD), GRAD(6,*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, IE, IV1, IV2, IV3, IV4, IV5, IV6, IAD2
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), X5(MVSIZ), X6(MVSIZ), X7(MVSIZ), X8(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ), Y5(MVSIZ), Y6(MVSIZ), Y7(MVSIZ), Y8(MVSIZ), Z1(MVSIZ),
     .   Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ), Z5(MVSIZ), Z6(MVSIZ), Z7(MVSIZ), Z8(MVSIZ), XC(MVSIZ), YC(MVSIZ),
     .   ZC(MVSIZ), N1X(MVSIZ), N2X(MVSIZ), N3X(MVSIZ), N4X(MVSIZ), N5X(MVSIZ), N6X(MVSIZ), N1Y(MVSIZ),
     .   N2Y(MVSIZ), N3Y(MVSIZ), N4Y(MVSIZ), N5Y(MVSIZ), N6Y(MVSIZ), N1Z(MVSIZ), N2Z(MVSIZ), N3Z(MVSIZ),
     .   N4Z(MVSIZ), N5Z(MVSIZ), N6Z(MVSIZ), DD1(MVSIZ), DD2(MVSIZ), DD3(MVSIZ), DD4(MVSIZ), DD5(MVSIZ),
     .   DD6(MVSIZ), D1X(MVSIZ), D2X(MVSIZ), D3X(MVSIZ), D4X(MVSIZ), D5X(MVSIZ), D6X(MVSIZ), D1Y(MVSIZ),
     .   D2Y(MVSIZ), D3Y(MVSIZ), D4Y(MVSIZ), D5Y(MVSIZ), D6Y(MVSIZ), D1Z(MVSIZ), D2Z(MVSIZ), D3Z(MVSIZ),
     .   D4Z(MVSIZ), D5Z(MVSIZ), D6Z(MVSIZ)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------    
      DO I=LFT,LLT
        II=I+NFT
        X1(I)=X(1,IXS(2,II))
        Y1(I)=X(2,IXS(2,II))
        Z1(I)=X(3,IXS(2,II))

        X2(I)=X(1,IXS(3,II))
        Y2(I)=X(2,IXS(3,II))
        Z2(I)=X(3,IXS(3,II))

        X3(I)=X(1,IXS(4,II))
        Y3(I)=X(2,IXS(4,II))
        Z3(I)=X(3,IXS(4,II))

        X4(I)=X(1,IXS(5,II))
        Y4(I)=X(2,IXS(5,II))
        Z4(I)=X(3,IXS(5,II))

        X5(I)=X(1,IXS(6,II))
        Y5(I)=X(2,IXS(6,II))
        Z5(I)=X(3,IXS(6,II))

        X6(I)=X(1,IXS(7,II))
        Y6(I)=X(2,IXS(7,II))
        Z6(I)=X(3,IXS(7,II))

        X7(I)=X(1,IXS(8,II))
        Y7(I)=X(2,IXS(8,II))
        Z7(I)=X(3,IXS(8,II))

        X8(I)=X(1,IXS(9,II))
        Y8(I)=X(2,IXS(9,II))
        Z8(I)=X(3,IXS(9,II))
      ENDDO
C------------------------------------------
C     NORMAL VECTOR (*2.)
C------------------------------------------
      DO I=LFT,LLT
        N1X(I) = (Y3(I)-Y1(I))*(Z2(I)-Z4(I)) - (Z3(I)-Z1(I))*(Y2(I)-Y4(I))
        N1Y(I) = (Z3(I)-Z1(I))*(X2(I)-X4(I)) - (X3(I)-X1(I))*(Z2(I)-Z4(I))
        N1Z(I) = (X3(I)-X1(I))*(Y2(I)-Y4(I)) - (Y3(I)-Y1(I))*(X2(I)-X4(I))

        N2X(I) = (Y7(I)-Y4(I))*(Z3(I)-Z8(I)) - (Z7(I)-Z4(I))*(Y3(I)-Y8(I))
        N2Y(I) = (Z7(I)-Z4(I))*(X3(I)-X8(I)) - (X7(I)-X4(I))*(Z3(I)-Z8(I))
        N2Z(I) = (X7(I)-X4(I))*(Y3(I)-Y8(I)) - (Y7(I)-Y4(I))*(X3(I)-X8(I))

        N3X(I) = (Y6(I)-Y8(I))*(Z7(I)-Z5(I)) - (Z6(I)-Z8(I))*(Y7(I)-Y5(I))
        N3Y(I) = (Z6(I)-Z8(I))*(X7(I)-X5(I)) - (X6(I)-X8(I))*(Z7(I)-Z5(I))
        N3Z(I) = (X6(I)-X8(I))*(Y7(I)-Y5(I)) - (Y6(I)-Y8(I))*(X7(I)-X5(I))

        N4X(I) = (Y2(I)-Y5(I))*(Z6(I)-Z1(I)) - (Z2(I)-Z5(I))*(Y6(I)-Y1(I))
        N4Y(I) = (Z2(I)-Z5(I))*(X6(I)-X1(I)) - (X2(I)-X5(I))*(Z6(I)-Z1(I))
        N4Z(I) = (X2(I)-X5(I))*(Y6(I)-Y1(I)) - (Y2(I)-Y5(I))*(X6(I)-X1(I))

        N5X(I) = (Y7(I)-Y2(I))*(Z6(I)-Z3(I)) - (Z7(I)-Z2(I))*(Y6(I)-Y3(I))
        N5Y(I) = (Z7(I)-Z2(I))*(X6(I)-X3(I)) - (X7(I)-X2(I))*(Z6(I)-Z3(I))
        N5Z(I) = (X7(I)-X2(I))*(Y6(I)-Y3(I)) - (Y7(I)-Y2(I))*(X6(I)-X3(I))

        N6X(I) = (Y8(I)-Y1(I))*(Z4(I)-Z5(I)) - (Z8(I)-Z1(I))*(Y4(I)-Y5(I))
        N6Y(I) = (Z8(I)-Z1(I))*(X4(I)-X5(I)) - (X8(I)-X1(I))*(Z4(I)-Z5(I))
        N6Z(I) = (X8(I)-X1(I))*(Y4(I)-Y5(I)) - (Y8(I)-Y1(I))*(X4(I)-X5(I))

        XC(I)  =  (X1(I)+X2(I)+X3(I)+X4(I)+X5(I)+X6(I)+X7(I)+X8(I))
        YC(I)  =  (Y1(I)+Y2(I)+Y3(I)+Y4(I)+Y5(I)+Y6(I)+Y7(I)+Y8(I))
        ZC(I)  =  (Z1(I)+Z2(I)+Z3(I)+Z4(I)+Z5(I)+Z6(I)+Z7(I)+Z8(I))
      ENDDO
C--------+---------+---------+---------+---------+---------+---------+--
C     DISTANCE BETWEEN ELEMS ( * 8. )
C-------------------------------------------------------------
      DO I=LFT,LLT
       IE =NFT+I
       IAD2 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
       IV1=ALE_CONNECTIVITY%ee_connect%connected(iad2 + 1 - 1)
       IF(IV1 <= 0) IV1=IE
       D1X(I) = - XC(I)
     .          +X(1,IXS(2,IV1))+X(1,IXS(3,IV1))+X(1,IXS(4,IV1))+X(1,IXS(5,IV1))
     .          +X(1,IXS(6,IV1))+X(1,IXS(7,IV1))+X(1,IXS(8,IV1))+X(1,IXS(9,IV1))
       D1Y(I) = - YC(I)
     .          +X(2,IXS(2,IV1))+X(2,IXS(3,IV1))+X(2,IXS(4,IV1))+X(2,IXS(5,IV1))
     .          +X(2,IXS(6,IV1))+X(2,IXS(7,IV1))+X(2,IXS(8,IV1))+X(2,IXS(9,IV1))
       D1Z(I) = - ZC(I)
     .          +X(3,IXS(2,IV1))+X(3,IXS(3,IV1))+X(3,IXS(4,IV1))+X(3,IXS(5,IV1))
     .          +X(3,IXS(6,IV1))+X(3,IXS(7,IV1))+X(3,IXS(8,IV1))+X(3,IXS(9,IV1))
      ENDDO
      DO I=LFT,LLT
       IE =NFT+I
       IAD2 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
       IV2=ALE_CONNECTIVITY%ee_connect%connected(iad2 + 2 - 1)
       IF(IV2 <= 0) IV2=IE
       D2X(I) = - XC(I)
     .          +X(1,IXS(2,IV2))+X(1,IXS(3,IV2))+X(1,IXS(4,IV2))+X(1,IXS(5,IV2))
     .          +X(1,IXS(6,IV2))+X(1,IXS(7,IV2))+X(1,IXS(8,IV2))+X(1,IXS(9,IV2))
       D2Y(I) = - YC(I)
     .          +X(2,IXS(2,IV2))+X(2,IXS(3,IV2))+X(2,IXS(4,IV2))+X(2,IXS(5,IV2))
     .          +X(2,IXS(6,IV2))+X(2,IXS(7,IV2))+X(2,IXS(8,IV2))+X(2,IXS(9,IV2))
       D2Z(I) = - ZC(I)
     .          +X(3,IXS(2,IV2))+X(3,IXS(3,IV2))+X(3,IXS(4,IV2))+X(3,IXS(5,IV2))
     .          +X(3,IXS(6,IV2))+X(3,IXS(7,IV2))+X(3,IXS(8,IV2))+X(3,IXS(9,IV2))
      ENDDO
      DO I=LFT,LLT
       IE =NFT+I
       IAD2 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
       IV3=ALE_CONNECTIVITY%ee_connect%connected(iad2 + 3 - 1)
       IF(IV3 <= 0) IV3=IE
       D3X(I) = - XC(I)
     .          +X(1,IXS(2,IV3))+X(1,IXS(3,IV3))+X(1,IXS(4,IV3))+X(1,IXS(5,IV3))
     .          +X(1,IXS(6,IV3))+X(1,IXS(7,IV3))+X(1,IXS(8,IV3))+X(1,IXS(9,IV3))
       D3Y(I) = - YC(I)
     .          +X(2,IXS(2,IV3))+X(2,IXS(3,IV3))+X(2,IXS(4,IV3))+X(2,IXS(5,IV3))
     .          +X(2,IXS(6,IV3))+X(2,IXS(7,IV3))+X(2,IXS(8,IV3))+X(2,IXS(9,IV3))
       D3Z(I) = - ZC(I)
     .          +X(3,IXS(2,IV3))+X(3,IXS(3,IV3))+X(3,IXS(4,IV3))+X(3,IXS(5,IV3))
     .          +X(3,IXS(6,IV3))+X(3,IXS(7,IV3))+X(3,IXS(8,IV3))+X(3,IXS(9,IV3))
      ENDDO
      DO I=LFT,LLT
       IE =NFT+I
       IAD2 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
       IV4=ALE_CONNECTIVITY%ee_connect%connected(iad2 + 4 - 1)
       IF(IV4 <= 0) IV4=IE
       D4X(I) = - XC(I)
     .          +X(1,IXS(2,IV4))+X(1,IXS(3,IV4))+X(1,IXS(4,IV4))+X(1,IXS(5,IV4))
     .          +X(1,IXS(6,IV4))+X(1,IXS(7,IV4))+X(1,IXS(8,IV4))+X(1,IXS(9,IV4))
       D4Y(I) = - YC(I)
     .          +X(2,IXS(2,IV4))+X(2,IXS(3,IV4))+X(2,IXS(4,IV4))+X(2,IXS(5,IV4))
     .          +X(2,IXS(6,IV4))+X(2,IXS(7,IV4))+X(2,IXS(8,IV4))+X(2,IXS(9,IV4))
       D4Z(I) = - ZC(I)
     .          +X(3,IXS(2,IV4))+X(3,IXS(3,IV4))+X(3,IXS(4,IV4))+X(3,IXS(5,IV4))
     .          +X(3,IXS(6,IV4))+X(3,IXS(7,IV4))+X(3,IXS(8,IV4))+X(3,IXS(9,IV4))
      ENDDO
      DO I=LFT,LLT
       IE =NFT+I
       IAD2 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
       IV5=ALE_CONNECTIVITY%ee_connect%connected(iad2 + 5 - 1)
       IF(IV5 <= 0) IV5=IE
       D5X(I) = - XC(I)
     .          +X(1,IXS(2,IV5))+X(1,IXS(3,IV5))+X(1,IXS(4,IV5))+X(1,IXS(5,IV5))
     .          +X(1,IXS(6,IV5))+X(1,IXS(7,IV5))+X(1,IXS(8,IV5))+X(1,IXS(9,IV5))
       D5Y(I) = - YC(I)
     .          +X(2,IXS(2,IV5))+X(2,IXS(3,IV5))+X(2,IXS(4,IV5))+X(2,IXS(5,IV5))
     .          +X(2,IXS(6,IV5))+X(2,IXS(7,IV5))+X(2,IXS(8,IV5))+X(2,IXS(9,IV5))
       D5Z(I) = - ZC(I)
     .          +X(3,IXS(2,IV5))+X(3,IXS(3,IV5))+X(3,IXS(4,IV5))+X(3,IXS(5,IV5))
     .          +X(3,IXS(6,IV5))+X(3,IXS(7,IV5))+X(3,IXS(8,IV5))+X(3,IXS(9,IV5))
      ENDDO
      DO I=LFT,LLT
       IE =NFT+I
       IAD2 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
       IV6=ALE_CONNECTIVITY%ee_connect%connected(iad2 + 6 - 1)
       IF(IV6 <= 0) IV6=IE
       D6X(I) = - XC(I)
     .          +X(1,IXS(2,IV6))+X(1,IXS(3,IV6))+X(1,IXS(4,IV6))+X(1,IXS(5,IV6))
     .          +X(1,IXS(6,IV6))+X(1,IXS(7,IV6))+X(1,IXS(8,IV6))+X(1,IXS(9,IV6))
       D6Y(I) = - YC(I)
     .          +X(2,IXS(2,IV6))+X(2,IXS(3,IV6))+X(2,IXS(4,IV6))+X(2,IXS(5,IV6))
     .          +X(2,IXS(6,IV6))+X(2,IXS(7,IV6))+X(2,IXS(8,IV6))+X(2,IXS(9,IV6))
       D6Z(I) = - ZC(I)
     .          +X(3,IXS(2,IV6))+X(3,IXS(3,IV6))+X(3,IXS(4,IV6))+X(3,IXS(5,IV6))
     .          +X(3,IXS(6,IV6))+X(3,IXS(7,IV6))+X(3,IXS(8,IV6))+X(3,IXS(9,IV6))
      ENDDO

      DO I=LFT,LLT
        DD1(I) = D1X(I)**2+D1Y(I)**2+D1Z(I)**2
        DD2(I) = D2X(I)**2+D2Y(I)**2+D2Z(I)**2
        DD3(I) = D3X(I)**2+D3Y(I)**2+D3Z(I)**2
        DD4(I) = D4X(I)**2+D4Y(I)**2+D4Z(I)**2
        DD5(I) = D5X(I)**2+D5Y(I)**2+D5Z(I)**2
        DD6(I) = D6X(I)**2+D6Y(I)**2+D6Z(I)**2
      ENDDO
C---------------------------------
C     GRADIENTS * SURFACES
C---------------------------------
      DO I=LFT,LLT
        GRAD(1,I)=FOUR*MAX(ZERO,D1X(I)*N1X(I) + D1Y(I)*N1Y(I)+D1Z(I)*N1Z(I)) / MAX(EM15,DD1(I))
        GRAD(2,I)=FOUR*MAX(ZERO,D2X(I)*N2X(I) + D2Y(I)*N2Y(I)+D2Z(I)*N2Z(I)) / MAX(EM15,DD2(I))
        GRAD(3,I)=FOUR*MAX(ZERO,D3X(I)*N3X(I) + D3Y(I)*N3Y(I)+D3Z(I)*N3Z(I)) / MAX(EM15,DD3(I))
        GRAD(4,I)=FOUR*MAX(ZERO,D4X(I)*N4X(I) + D4Y(I)*N4Y(I)+D4Z(I)*N4Z(I)) / MAX(EM15,DD4(I))
        GRAD(5,I)=FOUR*MAX(ZERO,D5X(I)*N5X(I) + D5Y(I)*N5Y(I)+D5Z(I)*N5Z(I)) / MAX(EM15,DD5(I))
        GRAD(6,I)=FOUR*MAX(ZERO,D6X(I)*N6X(I) + D6Y(I)*N6Y(I)+D6Z(I)*N6Z(I)) / MAX(EM15,DD6(I))
      ENDDO
C-----------------------------------------------      
      RETURN
      END
