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
Chd|  AFLUX3                        source/ale/ale3d/aflux3.F     
Chd|-- called by -----------
Chd|        AFLUX0                        source/ale/aflux0.F           
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        I22BUFBRIC_MOD                ../common_source/modules/interfaces/cut-cell-search_mod.F
Chd|====================================================================
      SUBROUTINE AFLUX3(PM,IXS,V,W,X,FLUX,FLU1,ALE_CONNECT,NSG, TAG22)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE I22BUFBRIC_MOD 
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
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "inter22.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,NUMELS), NSG
      my_real PM(NPROPM,NUMMAT), V(3,NUMNOD), W(3,NUMNOD), X(3,NUMNOD), FLUX(6,*), FLU1(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MAT(MVSIZ), 
     .   NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ), 
     .   NC5(MVSIZ), NC6(MVSIZ), NC7(MVSIZ), NC8(MVSIZ), 
     .   I,II
     
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ), 
     .   X5(MVSIZ), X6(MVSIZ), X7(MVSIZ), X8(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ), 
     .   Y5(MVSIZ), Y6(MVSIZ), Y7(MVSIZ), Y8(MVSIZ), 
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ), 
     .   Z5(MVSIZ), Z6(MVSIZ), Z7(MVSIZ), Z8(MVSIZ), 
     .   N1X(MVSIZ),N1Y(MVSIZ),N1Z(MVSIZ),
     .   N2X(MVSIZ),N2Y(MVSIZ),N2Z(MVSIZ),
     .   N3X(MVSIZ),N3Y(MVSIZ),N3Z(MVSIZ),
     .   N4X(MVSIZ),N4Y(MVSIZ),N4Z(MVSIZ),
     .   N5X(MVSIZ),N5Y(MVSIZ),N5Z(MVSIZ),
     .   N6X(MVSIZ),N6Y(MVSIZ),N6Z(MVSIZ),
     .   FLUX1(MVSIZ), FLUX2(MVSIZ), FLUX3(MVSIZ),
     .   FLUX4(MVSIZ), FLUX5(MVSIZ), FLUX6(MVSIZ),
     .   VX1(MVSIZ), VX2(MVSIZ), VX3(MVSIZ),
     .   VX4(MVSIZ), VX5(MVSIZ), VX6(MVSIZ),
     .   VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ),
     .   VY4(MVSIZ), VY5(MVSIZ), VY6(MVSIZ),
     .   VZ1(MVSIZ), VZ2(MVSIZ), VZ3(MVSIZ), 
     .   VZ4(MVSIZ), VZ5(MVSIZ), VZ6(MVSIZ), 
     .   VDX1(MVSIZ), VDX2(MVSIZ), VDX3(MVSIZ), VDX4(MVSIZ), 
     .   VDX5(MVSIZ), VDX6(MVSIZ), VDX7(MVSIZ), VDX8(MVSIZ), 
     .   VDY1(MVSIZ), VDY2(MVSIZ), VDY3(MVSIZ), VDY4(MVSIZ),
     .   VDY5(MVSIZ), VDY6(MVSIZ), VDY7(MVSIZ), VDY8(MVSIZ),
     .   VDZ1(MVSIZ), VDZ2(MVSIZ), VDZ3(MVSIZ), VDZ4(MVSIZ),
     .   VDZ5(MVSIZ), VDZ6(MVSIZ), VDZ7(MVSIZ), VDZ8(MVSIZ),
     .   REDUC,UPWL(6,MVSIZ),
     .   XC(MVSIZ),YC(MVSIZ),ZC(MVSIZ),
     .   XF1(MVSIZ),YF1(MVSIZ),ZF1(MVSIZ),
     .   XF2(MVSIZ),YF2(MVSIZ),ZF2(MVSIZ),
     .   XF3(MVSIZ),YF3(MVSIZ),ZF3(MVSIZ),
     .   XF4(MVSIZ),YF4(MVSIZ),ZF4(MVSIZ),
     .   XF5(MVSIZ),YF5(MVSIZ),ZF5(MVSIZ),
     .   XF6(MVSIZ),YF6(MVSIZ),ZF6(MVSIZ),
     .   TEST
   
      INTEGER,DIMENSION(:),   POINTER     :: pIsMain       
     
      my_real :: TAG22(MVSIZ), RATIOFACE(6)
      
      my_real,DIMENSION(:), POINTER       :: pFACE      !=> BRICK_LIST%POLY()%FACE()%Surf   !     WARNING :  all pointers begin with 1 not 0     
      my_real,DIMENSION(:)  , POINTER     :: pFullFACE  
     
      INTEGER MA,IC,JST(MVSIZ+1)
      INTEGER MCELL,IB, NIN, NBCUT, IAD2
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      !======================================================!
      ! INITIALIZATION : COORDINATES & RELATIVE VELOCITIES   !
      !======================================================!
      DO I=LFT,LLT
        II=I+NFT
        MAT(I)=IXS(1,II)
          !---8 local node numbers NC1 TO NC8 for solid element I ---!
        NC1(I)=IXS(2,II)
        NC2(I)=IXS(3,II)
        NC3(I)=IXS(4,II)
        NC4(I)=IXS(5,II)
        NC5(I)=IXS(6,II)
        NC6(I)=IXS(7,II)
        NC7(I)=IXS(8,II)
        NC8(I)=IXS(9,II)
          !
        !---Coordinates of the 8 nodes
        X1(I)=X(1,NC1(I))
        Y1(I)=X(2,NC1(I))
        Z1(I)=X(3,NC1(I))
        !
        X2(I)=X(1,NC2(I))
        Y2(I)=X(2,NC2(I))
        Z2(I)=X(3,NC2(I))
        !
        X3(I)=X(1,NC3(I))
        Y3(I)=X(2,NC3(I))
        Z3(I)=X(3,NC3(I))
        !
        X4(I)=X(1,NC4(I))
        Y4(I)=X(2,NC4(I))
        Z4(I)=X(3,NC4(I))
        !
        X5(I)=X(1,NC5(I))
        Y5(I)=X(2,NC5(I))
        Z5(I)=X(3,NC5(I))
        !
        X6(I)=X(1,NC6(I))
        Y6(I)=X(2,NC6(I))
        Z6(I)=X(3,NC6(I))
        !
        X7(I)=X(1,NC7(I))
        Y7(I)=X(2,NC7(I))
        Z7(I)=X(3,NC7(I))
        !
        X8(I)=X(1,NC8(I))
        Y8(I)=X(2,NC8(I))
        Z8(I)=X(3,NC8(I))
          !
        !Relative velocity on the 8 nodes.
        !  [VD_node] = [V_node] - [W_node]
          !  where
          !    [V_node] : material velocity on node
          !    [W_node] : grid velocity on node
          !
        VDX1(I)=V(1,NC1(I)) - W(1,NC1(I))
        VDY1(I)=V(2,NC1(I)) - W(2,NC1(I))
        VDZ1(I)=V(3,NC1(I)) - W(3,NC1(I))
        !
        VDX2(I)=V(1,NC2(I)) - W(1,NC2(I))
        VDY2(I)=V(2,NC2(I)) - W(2,NC2(I))
        VDZ2(I)=V(3,NC2(I)) - W(3,NC2(I))
        !
        VDX3(I)=V(1,NC3(I)) - W(1,NC3(I))
        VDY3(I)=V(2,NC3(I)) - W(2,NC3(I))
        VDZ3(I)=V(3,NC3(I)) - W(3,NC3(I))
        !
        VDX4(I)=V(1,NC4(I)) - W(1,NC4(I))
        VDY4(I)=V(2,NC4(I)) - W(2,NC4(I))
        VDZ4(I)=V(3,NC4(I)) - W(3,NC4(I))
        !
        VDX5(I)=V(1,NC5(I)) - W(1,NC5(I))
        VDY5(I)=V(2,NC5(I)) - W(2,NC5(I))
        VDZ5(I)=V(3,NC5(I)) - W(3,NC5(I))
        !
        VDX6(I)=V(1,NC6(I)) - W(1,NC6(I))
        VDY6(I)=V(2,NC6(I)) - W(2,NC6(I))
        VDZ6(I)=V(3,NC6(I)) - W(3,NC6(I))
        !
        VDX7(I)=V(1,NC7(I)) - W(1,NC7(I))
        VDY7(I)=V(2,NC7(I)) - W(2,NC7(I))
        VDZ7(I)=V(3,NC7(I)) - W(3,NC7(I))
        !
        VDX8(I)=V(1,NC8(I)) - W(1,NC8(I))
        VDY8(I)=V(2,NC8(I)) - W(2,NC8(I))
        VDZ8(I)=V(3,NC8(I)) - W(3,NC8(I))
      END DO      
      
      !======================================================!
      ! RELATIVE VELOCITIES ON EACH FACE                     !
      !    [V_face] = 1/4 Sum([V_node])                      !
      !    Results are divided by 2 at this step :           !
      !    [0.5*V_face] = 1/8 Sum([V_node])                  !       
      !======================================================!
      DO I=LFT,LLT
        ! X-component
        VX1(I)=ONE_OVER_8*(VDX1(I)+VDX2(I)+VDX3(I)+VDX4(I))
        VX2(I)=ONE_OVER_8*(VDX3(I)+VDX4(I)+VDX8(I)+VDX7(I))
        VX3(I)=ONE_OVER_8*(VDX5(I)+VDX6(I)+VDX7(I)+VDX8(I))
        VX4(I)=ONE_OVER_8*(VDX1(I)+VDX2(I)+VDX6(I)+VDX5(I))
        VX5(I)=ONE_OVER_8*(VDX2(I)+VDX3(I)+VDX7(I)+VDX6(I))
        VX6(I)=ONE_OVER_8*(VDX1(I)+VDX4(I)+VDX8(I)+VDX5(I))
        ! Y-component
        VY1(I)=ONE_OVER_8*(VDY1(I)+VDY2(I)+VDY3(I)+VDY4(I))
        VY2(I)=ONE_OVER_8*(VDY3(I)+VDY4(I)+VDY8(I)+VDY7(I))
        VY3(I)=ONE_OVER_8*(VDY5(I)+VDY6(I)+VDY7(I)+VDY8(I))
        VY4(I)=ONE_OVER_8*(VDY1(I)+VDY2(I)+VDY6(I)+VDY5(I))
        VY5(I)=ONE_OVER_8*(VDY2(I)+VDY3(I)+VDY7(I)+VDY6(I))
        VY6(I)=ONE_OVER_8*(VDY1(I)+VDY4(I)+VDY8(I)+VDY5(I))
        ! Z-component
        VZ1(I)=ONE_OVER_8*(VDZ1(I)+VDZ2(I)+VDZ3(I)+VDZ4(I))
        VZ2(I)=ONE_OVER_8*(VDZ3(I)+VDZ4(I)+VDZ8(I)+VDZ7(I))
        VZ3(I)=ONE_OVER_8*(VDZ5(I)+VDZ6(I)+VDZ7(I)+VDZ8(I))
        VZ4(I)=ONE_OVER_8*(VDZ1(I)+VDZ2(I)+VDZ6(I)+VDZ5(I))
        VZ5(I)=ONE_OVER_8*(VDZ2(I)+VDZ3(I)+VDZ7(I)+VDZ6(I))
        VZ6(I)=ONE_OVER_8*(VDZ1(I)+VDZ4(I)+VDZ8(I)+VDZ5(I))
      END DO
      
      !======================================================!
      ! NORMAL VECTORS ON EACH DACE                          !
      !    2S[n] = [diag1] x [diag2]                         ! 
      !    where                                             !
      !      [n] : unitary normal vector on face             !
      !======================================================!
      DO I=LFT,LLT
        ! Face-1
        N1X(I)=(Y3(I)-Y1(I))*(Z2(I)-Z4(I)) - (Z3(I)-Z1(I))*(Y2(I)-Y4(I))
        N1Y(I)=(Z3(I)-Z1(I))*(X2(I)-X4(I)) - (X3(I)-X1(I))*(Z2(I)-Z4(I))
        N1Z(I)=(X3(I)-X1(I))*(Y2(I)-Y4(I)) - (Y3(I)-Y1(I))*(X2(I)-X4(I))
        ! Face-2
        N2X(I)=(Y7(I)-Y4(I))*(Z3(I)-Z8(I)) - (Z7(I)-Z4(I))*(Y3(I)-Y8(I))
        N2Y(I)=(Z7(I)-Z4(I))*(X3(I)-X8(I)) - (X7(I)-X4(I))*(Z3(I)-Z8(I))
        N2Z(I)=(X7(I)-X4(I))*(Y3(I)-Y8(I)) - (Y7(I)-Y4(I))*(X3(I)-X8(I))
        ! Face-3
        N3X(I)=(Y6(I)-Y8(I))*(Z7(I)-Z5(I)) - (Z6(I)-Z8(I))*(Y7(I)-Y5(I))
        N3Y(I)=(Z6(I)-Z8(I))*(X7(I)-X5(I)) - (X6(I)-X8(I))*(Z7(I)-Z5(I))
        N3Z(I)=(X6(I)-X8(I))*(Y7(I)-Y5(I)) - (Y6(I)-Y8(I))*(X7(I)-X5(I))
        ! Face-4
        N4X(I)=(Y2(I)-Y5(I))*(Z6(I)-Z1(I)) - (Z2(I)-Z5(I))*(Y6(I)-Y1(I))
        N4Y(I)=(Z2(I)-Z5(I))*(X6(I)-X1(I)) - (X2(I)-X5(I))*(Z6(I)-Z1(I))
        N4Z(I)=(X2(I)-X5(I))*(Y6(I)-Y1(I)) - (Y2(I)-Y5(I))*(X6(I)-X1(I))
        ! Face-5
        N5X(I)=(Y7(I)-Y2(I))*(Z6(I)-Z3(I)) - (Z7(I)-Z2(I))*(Y6(I)-Y3(I))
        N5Y(I)=(Z7(I)-Z2(I))*(X6(I)-X3(I)) - (X7(I)-X2(I))*(Z6(I)-Z3(I))
        N5Z(I)=(X7(I)-X2(I))*(Y6(I)-Y3(I)) - (Y7(I)-Y2(I))*(X6(I)-X3(I))
        ! Face-6
        N6X(I)=(Y8(I)-Y1(I))*(Z4(I)-Z5(I)) - (Z8(I)-Z1(I))*(Y4(I)-Y5(I))
        N6Y(I)=(Z8(I)-Z1(I))*(X4(I)-X5(I)) - (X8(I)-X1(I))*(Z4(I)-Z5(I))
        N6Z(I)=(X8(I)-X1(I))*(Y4(I)-Y5(I)) - (Y8(I)-Y1(I))*(X4(I)-X5(I))
      END DO

      !======================================================!
      ! ELEMENT CLOSURE TEST                                 !
      !    (If at least there is one domain with closure)    ! 
      !    2S [CF].[n] = 0  => [2S*n] = 0                    !
      !    where                                             !
      !      C: element centroid                             !
      !      F: face centroid                                !
      !======================================================!
      IF(ICLOSE == 1) THEN
        DO I=LFT,LLT
            ! Solid Element Centorid
          XC(I)=ONE_OVER_8*(X1(I)+X2(I)+X3(I)+X4(I)+X5(I)+X6(I)+X7(I)+X8(I))
          YC(I)=ONE_OVER_8*(Y1(I)+Y2(I)+Y3(I)+Y4(I)+Y5(I)+Y6(I)+Y7(I)+Y8(I))
          ZC(I)=ONE_OVER_8*(Z1(I)+Z2(I)+Z3(I)+Z4(I)+Z5(I)+Z6(I)+Z7(I)+Z8(I))
            ! Solid faces centoids
            !   X-Components
          XF1(I)=FOURTH*(X1(I)+X2(I)+X3(I)+X4(I))
          XF2(I)=FOURTH*(X3(I)+X4(I)+X8(I)+X7(I))
          XF3(I)=FOURTH*(X5(I)+X6(I)+X7(I)+X8(I))
          XF4(I)=FOURTH*(X1(I)+X2(I)+X6(I)+X5(I))
          XF5(I)=FOURTH*(X2(I)+X3(I)+X7(I)+X6(I))
          XF6(I)=FOURTH*(X1(I)+X4(I)+X8(I)+X5(I))
            !   Y-Components
          YF1(I)=FOURTH*(Y1(I)+Y2(I)+Y3(I)+Y4(I))
          YF2(I)=FOURTH*(Y3(I)+Y4(I)+Y8(I)+Y7(I))
          YF3(I)=FOURTH*(Y5(I)+Y6(I)+Y7(I)+Y8(I))
          YF4(I)=FOURTH*(Y1(I)+Y2(I)+Y6(I)+Y5(I))
          YF5(I)=FOURTH*(Y2(I)+Y3(I)+Y7(I)+Y6(I))
          YF6(I)=FOURTH*(Y1(I)+Y4(I)+Y8(I)+Y5(I))
            !   Z-Components
          ZF1(I)=FOURTH*(Z1(I)+Z2(I)+Z3(I)+Z4(I))
          ZF2(I)=FOURTH*(Z3(I)+Z4(I)+Z8(I)+Z7(I))
          ZF3(I)=FOURTH*(Z5(I)+Z6(I)+Z7(I)+Z8(I))
          ZF4(I)=FOURTH*(Z1(I)+Z2(I)+Z6(I)+Z5(I))
          ZF5(I)=FOURTH*(Z2(I)+Z3(I)+Z7(I)+Z6(I))
          ZF6(I)=FOURTH*(Z1(I)+Z4(I)+Z8(I)+Z5(I))          
        ENDDO
          ! Face-1
        DO I=LFT,LLT
          TEST=(XF1(I)-XC(I))*N1X(I)+
     .         (YF1(I)-YC(I))*N1Y(I)+
     .         (ZF1(I)-ZC(I))*N1Z(I)
          IF(TEST <= 0)THEN
              N1X(I)=ZERO
              N1Y(I)=ZERO
              N1Z(I)=ZERO     
          ENDIF
        ENDDO
          ! Face-2
        DO I=LFT,LLT
          TEST=(XF2(I)-XC(I))*N2X(I)+
     .         (YF2(I)-YC(I))*N2Y(I)+
     .         (ZF2(I)-ZC(I))*N2Z(I)
          IF(TEST <= 0)THEN
              N2X(I)=ZERO
              N2Y(I)=ZERO
              N2Z(I)=ZERO     
          ENDIF
        ENDDO
          ! Face-3
        DO I=LFT,LLT
          TEST=(XF3(I)-XC(I))*N3X(I)+
     .         (YF3(I)-YC(I))*N3Y(I)+
     .         (ZF3(I)-ZC(I))*N3Z(I)
          IF(TEST <= 0)THEN
              N3X(I)=ZERO
              N3Y(I)=ZERO
              N3Z(I)=ZERO              
          ENDIF
        ENDDO
          ! Face-4
        DO I=LFT,LLT
          TEST=(XF4(I)-XC(I))*N4X(I)+
     .         (YF4(I)-YC(I))*N4Y(I)+
     .         (ZF4(I)-ZC(I))*N4Z(I)
            IF(TEST <= ZERO)THEN
              N4X(I)=ZERO
              N4Y(I)=ZERO
              N4Z(I)=ZERO              
          ENDIF
        ENDDO
          ! Face-5
        DO I=LFT,LLT
          TEST=(XF5(I)-XC(I))*N5X(I)+
     .         (YF5(I)-YC(I))*N5Y(I)+
     .         (ZF5(I)-ZC(I))*N5Z(I)
             IF(TEST <= ZERO)THEN
              N5X(I)=ZERO
              N5Y(I)=ZERO
              N5Z(I)=ZERO              
          ENDIF
        ENDDO
          ! Face-6
        DO I=LFT,LLT
          TEST=(XF6(I)-XC(I))*N6X(I)+
     .         (YF6(I)-YC(I))*N6Y(I)+
     .         (ZF6(I)-ZC(I))*N6Z(I)
          IF(TEST <= ZERO)THEN
              N6X(I)=ZERO
              N6Y(I)=ZERO
              N6Z(I)=ZERO      
          ENDIF
        ENDDO
      ENDIF
      
      !======================================================!
      ! FLUXES CALCULATION ON EACH FACE                      !
      !    FLUX_face = [V_face].[n]                          ! 
      !              = [0.5*V_face] . [2S*n]                 !       
      !======================================================!
      DO I=LFT,LLT
        FLUX1(I)=(VX1(I)*N1X(I)+VY1(I)*N1Y(I)+VZ1(I)*N1Z(I))
        FLUX2(I)=(VX2(I)*N2X(I)+VY2(I)*N2Y(I)+VZ2(I)*N2Z(I))
        FLUX3(I)=(VX3(I)*N3X(I)+VY3(I)*N3Y(I)+VZ3(I)*N3Z(I))
        FLUX4(I)=(VX4(I)*N4X(I)+VY4(I)*N4Y(I)+VZ4(I)*N4Z(I))
        FLUX5(I)=(VX5(I)*N5X(I)+VY5(I)*N5Y(I)+VZ5(I)*N5Z(I))
        FLUX6(I)=(VX6(I)*N6X(I)+VY6(I)*N6Y(I)+VZ6(I)*N6Z(I))
      END DO

      !======================================================!
      ! REDUCTION FACTOR FOR POLYHEDRA FACES                 !
      !   interface type22 (cut cell method)                 !
      !======================================================!
      IF(INT22 > 0)THEN
      print *, "**inter22, CURRENTLY EULERIAN ONLY"
      stop 2204
        NIN   = 1
        DO I=LFT,LLT
          IB = NINT(TAG22(I)) !GBUF%TAG22
          IF(IB > 0)THEN
            NBCUT        =  BRICK_LIST(NIN,IB)%NBCUT
            IF(NBCUT==0)CYCLE          
            MCELL        =  BRICK_LIST(NIN,IB)%mainID                
            pFACE        => BRICK_LIST(NIN,IB)%POLY(MCELL)%FACE(1:6)%Surf
            pIsMain    => BRICK_LIST(NIN,IB)%POLY(1:9)%IsMain
            IF(MCELL == 0) CYCLE            
            pFullFACE    => BRICK_LIST(NIN,IB)%Face_Brick(1:6)        
            RATIOFACE(1) =  pFACE(1) / pFullFACE(1) 
            RATIOFACE(2) =  pFACE(2) / pFullFACE(2) 
            RATIOFACE(3) =  pFACE(3) / pFullFACE(3) 
            RATIOFACE(4) =  pFACE(4) / pFullFACE(4) 
            RATIOFACE(5) =  pFACE(5) / pFullFACE(5) 
            RATIOFACE(6) =  pFACE(6) / pFullFACE(6)
            FLUX1(I)     =  RATIOFACE(1) * FLUX1(I)
            FLUX2(I)     =  RATIOFACE(2) * FLUX2(I)
            FLUX3(I)     =  RATIOFACE(3) * FLUX3(I)
            FLUX4(I)     =  RATIOFACE(4) * FLUX4(I)
            FLUX5(I)     =  RATIOFACE(5) * FLUX5(I)
            FLUX6(I)     =  RATIOFACE(6) * FLUX6(I)                                                  
          ENDIF                                       
        ENDDO!next IB 
      ENDIF
      
      !======================================================!
      ! TRIMATERIAL CASE INITIALIZATION (LAW51)              !
      ! -->RETURN                                            !
      !======================================================!
      IF(NINT(PM(19,MAT(1))) == 51)THEN
        DO I=LFT,LLT
          FLUX(1,I)=FLUX1(I)
          FLUX(2,I)=FLUX2(I)
          FLUX(3,I)=FLUX3(I)
          FLUX(4,I)=FLUX4(I)
          FLUX(5,I)=FLUX5(I)
          FLUX(6,I)=FLUX6(I)
        ENDDO
        RETURN
      ENDIF
      
      !======================================================!
      !  UPWIND TREATMENT                                    !
      !    reading coefficient for mass transportation UPWL  !
      !======================================================!
      IF (NSG == 1) THEN
         MA = MAT(1)
         DO I=LFT,LLT
           UPWL(1,I)=PM(16,MA)
           UPWL(2,I)=PM(16,MA)
           UPWL(3,I)=PM(16,MA)
           UPWL(4,I)=PM(16,MA)
           UPWL(5,I)=PM(16,MA)
           UPWL(6,I)=PM(16,MA)
         ENDDO
      ELSE
         IF (IVECTOR == 0) THEN
           DO I=LFT,LLT
             UPWL(1,I)=PM(16,MAT(I))
             UPWL(2,I)=PM(16,MAT(I))
             UPWL(3,I)=PM(16,MAT(I))
             UPWL(4,I)=PM(16,MAT(I))
             UPWL(5,I)=PM(16,MAT(I))
             UPWL(6,I)=PM(16,MAT(I))
           ENDDO
         ELSE
           IC=1
           JST(IC)=LFT
           DO I=LFT+1,LLT
             IF(MAT(I) /= MAT(I-1)) THEN
               IC = IC+1
               JST(IC)=I
             ENDIF
           ENDDO
           JST(IC+1)=LLT+1
           DO II=1,IC
             MA = MAT(II)
             DO I=JST(II),JST(II+1)-1
               UPWL(1,I)=PM(16,MA)
               UPWL(2,I)=PM(16,MA)
               UPWL(3,I)=PM(16,MA)
               UPWL(4,I)=PM(16,MA)
               UPWL(5,I)=PM(16,MA)
               UPWL(6,I)=PM(16,MA)
             ENDDO
           ENDDO
         ENDIF
      ENDIF
      
      !======================================================!
      !  BOUNDARY FACE : no volume flux by default           !
      !    slip wall bc                                      !
      !======================================================!
      DO I=LFT,LLT
       IAD2 = ALE_CONNECT%ee_connect%iad_connect(I + NFT)
       ! Face1-neighbor check
       REDUC=PM(92,MAT(I))
       II = ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
       IF(II == 0)THEN
        FLUX1(I)=FLUX1(I)*REDUC
       ENDIF
       ! Face2-neighbor check
       II = ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
       IF(II == 0)THEN
        FLUX2(I)=FLUX2(I)*REDUC
       ENDIF
       ! Face3-neighbor check
       II = ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
       IF(II == 0)THEN
        FLUX3(I)=FLUX3(I)*REDUC
       ENDIF
       ! Face4-neighbor check
       II = ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
       IF(II == 0)THEN
        FLUX4(I)=FLUX4(I)*REDUC
       ENDIF
       ! Face5-neighbor check
       II = ALE_CONNECT%ee_connect%connected(IAD2 + 5 - 1)
       IF(II == 0)THEN
        FLUX5(I)=FLUX5(I)*REDUC
       ENDIF
       ! Face6-neighbor check
       II = ALE_CONNECT%ee_connect%connected(IAD2 + 6 - 1)
       IF(II == 0)THEN
        FLUX6(I)=FLUX6(I)*REDUC
       ENDIF
      END DO !I=LFT,LLT

      !==========================================================!
      ! FLUXES OUTPUT                                            !
      !   FLUX_face   =      (1-sgn*UPWL)*FLUX_face              !
      !   FLUX_global = SUM  (1+sgn*UPWL)*FLUX_face              !
      !   where                                                  !
      !     UPWL : upwind factor for mass transportation         !
      !     sgn  : sgn(FLUX_face)                                !
      !==========================================================!
      DO I=LFT,LLT
        ! 6 fluxes on faces
        FLUX(1,I)=FLUX1(I)-UPWL(1,I)*ABS(FLUX1(I))
        FLUX(2,I)=FLUX2(I)-UPWL(2,I)*ABS(FLUX2(I))
        FLUX(3,I)=FLUX3(I)-UPWL(3,I)*ABS(FLUX3(I))
        FLUX(4,I)=FLUX4(I)-UPWL(4,I)*ABS(FLUX4(I))
        FLUX(5,I)=FLUX5(I)-UPWL(5,I)*ABS(FLUX5(I))
        FLUX(6,I)=FLUX6(I)-UPWL(6,I)*ABS(FLUX6(I))
        ! One global flux
        FLU1(I)  =FLUX1(I)+UPWL(1,I)*ABS(FLUX1(I))
     .           +FLUX2(I)+UPWL(2,I)*ABS(FLUX2(I))
     .           +FLUX3(I)+UPWL(3,I)*ABS(FLUX3(I))
     .           +FLUX4(I)+UPWL(4,I)*ABS(FLUX4(I))
     .           +FLUX5(I)+UPWL(5,I)*ABS(FLUX5(I))
     .           +FLUX6(I)+UPWL(6,I)*ABS(FLUX6(I))
      END DO
C-----------------------------------------------
      RETURN
      END
C
