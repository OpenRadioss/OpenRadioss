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
Chd|  AGRAD2                        source/ale/ale2d/agrad2.F     
Chd|-- called by -----------
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE AGRAD2(IXQ,X,ALE_CONNECTIVITY,GRAD,NEL)
C-----------------------------------------------
C   D e s c r i p t i on
C-----------------------------------------------
C This subroutine is calculating 2D gradient on faces
C-----------------------------------------------
C   M o d u l e s
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: IXQ(7,NUMELQ),NEL
      my_real, INTENT(IN) :: X(3,NUMNOD)
      my_real,INTENT(INOUT) :: GRAD(NEL,4)
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, II, IE, IV1, IV2, IV3, IV4, IAD1
      my_real  Y1(MVSIZ),  Y2(MVSIZ),  Y3(MVSIZ),  Y4(MVSIZ),
     .         Z1(MVSIZ),  Z2(MVSIZ),  Z3(MVSIZ),  Z4(MVSIZ),
     .         YC(MVSIZ),  ZC(MVSIZ), 
     .         N1Y(MVSIZ), N2Y(MVSIZ), N3Y(MVSIZ), N4Y(MVSIZ),
     .         N1Z(MVSIZ), N2Z(MVSIZ), N3Z(MVSIZ), N4Z(MVSIZ),
     .         DD1(MVSIZ), DD2(MVSIZ), DD3(MVSIZ), DD4(MVSIZ), 
     .         D1Y(MVSIZ), D2Y(MVSIZ), D3Y(MVSIZ), D4Y(MVSIZ),
     .         D1Z(MVSIZ), D2Z(MVSIZ), D3Z(MVSIZ), D4Z(MVSIZ)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

C ---- COORDINATES -----------------------------
      DO I=LFT,LLT
        II=I+NFT
        
          Y1(I)=X(2,IXQ(2,II))
        Z1(I)=X(3,IXQ(2,II))

        Y2(I)=X(2,IXQ(3,II))
        Z2(I)=X(3,IXQ(3,II))

        Y3(I)=X(2,IXQ(4,II))
        Z3(I)=X(3,IXQ(4,II))

        Y4(I)=X(2,IXQ(5,II))
        Z4(I)=X(3,IXQ(5,II))
      END DO

C ---- NORMAL VECTORS ON FACES------------------
      DO I=LFT,LLT
        N1Y(I)= (Z2(I)-Z1(I))
        N1Z(I)=-(Y2(I)-Y1(I))

        N2Y(I)= (Z3(I)-Z2(I))
        N2Z(I)=-(Y3(I)-Y2(I))

        N3Y(I)= (Z4(I)-Z3(I))
        N3Z(I)=-(Y4(I)-Y3(I))

        N4Y(I)= (Z1(I)-Z4(I))
        N4Z(I)=-(Y1(I)-Y4(I))

        YC(I) = (Y1(I)+Y2(I)+Y3(I)+Y4(I))
        ZC(I) = (Z1(I)+Z2(I)+Z3(I)+Z4(I))
      END DO

      IF(N2D == 1)THEN
        DO I=LFT,LLT
          N1Y(I)= N1Y(I)*(Y1(I)+Y2(I))*0.5
          N1Z(I)= N1Z(I)*(Y1(I)+Y2(I))*0.5
          N2Y(I)= N2Y(I)*(Y2(I)+Y3(I))*0.5
          N2Z(I)= N2Z(I)*(Y2(I)+Y3(I))*0.5
          N3Y(I)= N3Y(I)*(Y3(I)+Y4(I))*0.5
          N3Z(I)= N3Z(I)*(Y3(I)+Y4(I))*0.5
          N4Y(I)= N4Y(I)*(Y1(I)+Y4(I))*0.5
          N4Z(I)= N4Z(I)*(Y1(I)+Y4(I))*0.5
        END DO
      ENDIF
      
C ---- DISTANCES BETWEEN ELEMS (*4.)------------
      DO I=LFT,LLT
        IE =NFT+I
        IAD1 = ALE_CONNECTIVITY%ee_connect%iad_connect(IE)
        IV1 = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + 1 - 1)
        IV2 = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + 2 - 1)
        IV3 = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + 3 - 1)
        IV4 = ALE_CONNECTIVITY%ee_connect%connected(IAD1 + 4 - 1)
        
        IF(IV1 <= 0) IV1=IE
        IF(IV2 <= 0) IV2=IE
        IF(IV3 <= 0) IV3=IE
        IF(IV4 <= 0) IV4=IE

        D1Y(I) = - YC(I)+X(2,IXQ(2,IV1))+X(2,IXQ(3,IV1))+X(2,IXQ(4,IV1))+X(2,IXQ(5,IV1))
        D1Z(I) = - ZC(I)+X(3,IXQ(2,IV1))+X(3,IXQ(3,IV1))+X(3,IXQ(4,IV1))+X(3,IXQ(5,IV1))

        D2Y(I) = - YC(I)+X(2,IXQ(2,IV2))+X(2,IXQ(3,IV2))+X(2,IXQ(4,IV2))+X(2,IXQ(5,IV2))
        D2Z(I) = - ZC(I)+X(3,IXQ(2,IV2))+X(3,IXQ(3,IV2))+X(3,IXQ(4,IV2))+X(3,IXQ(5,IV2))

        D3Y(I) = - YC(I)+X(2,IXQ(2,IV3))+X(2,IXQ(3,IV3))+X(2,IXQ(4,IV3))+X(2,IXQ(5,IV3))
        D3Z(I) = - ZC(I)+X(3,IXQ(2,IV3))+X(3,IXQ(3,IV3))+X(3,IXQ(4,IV3))+X(3,IXQ(5,IV3))

        D4Y(I) = - YC(I)+X(2,IXQ(2,IV4))+X(2,IXQ(3,IV4))+X(2,IXQ(4,IV4))+X(2,IXQ(5,IV4))
        D4Z(I) = - ZC(I)+X(3,IXQ(2,IV4))+X(3,IXQ(3,IV4))+X(3,IXQ(4,IV4))+X(3,IXQ(5,IV4))
      END DO

      DO I=LFT,LLT
        DD1(I)=D1Y(I)**2+D1Z(I)**2
        DD2(I)=D2Y(I)**2+D2Z(I)**2
        DD3(I)=D3Y(I)**2+D3Z(I)**2
        DD4(I)=D4Y(I)**2+D4Z(I)**2
      END DO

C ---- GRADIENT * SURFACE-----------------------
      DO I=LFT,LLT
        GRAD(I,1)= FOUR*(D1Y(I)*N1Y(I)+D1Z(I)*N1Z(I)) / MAX(EM15,DD1(I))
        GRAD(I,2)= FOUR*(D2Y(I)*N2Y(I)+D2Z(I)*N2Z(I)) / MAX(EM15,DD2(I))
        GRAD(I,3)= FOUR*(D3Y(I)*N3Y(I)+D3Z(I)*N3Z(I)) / MAX(EM15,DD3(I))
        GRAD(I,4)= FOUR*(D4Y(I)*N4Y(I)+D4Z(I)*N4Z(I)) / MAX(EM15,DD4(I))
      END DO
C-----------------------------------------------      
      RETURN
      END
