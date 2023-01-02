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
Chd|  A4FLUX3                       source/ale/ale3d/a4flux3.F    
Chd|-- called by -----------
Chd|        AFLUX0                        source/ale/aflux0.F           
Chd|-- calls ---------------
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|====================================================================
      SUBROUTINE A4FLUX3(PM,IXS,V,W,X,FLUX,FLU1,ALE_CONNECT)
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
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IXS(NIXS,NUMELS)
      my_real PM(NPROPM,NUMMAT), V(3,NUMNOD), W(3,NUMNOD), X(3,NUMNOD), FLUX(6,*), FLU1(*)
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER MAT(MVSIZ), 
     .   NC1(MVSIZ), NC2(MVSIZ), NC3(MVSIZ), NC4(MVSIZ), I,J,II,IAD2
      my_real
     .   X1(MVSIZ), X2(MVSIZ), X3(MVSIZ), X4(MVSIZ),
     .   Y1(MVSIZ), Y2(MVSIZ), Y3(MVSIZ), Y4(MVSIZ),
     .   Z1(MVSIZ), Z2(MVSIZ), Z3(MVSIZ), Z4(MVSIZ),
     .   N1X(MVSIZ),N1Y(MVSIZ),N1Z(MVSIZ),
     .   N2X(MVSIZ),N2Y(MVSIZ),N2Z(MVSIZ),
     .   N3X(MVSIZ),N3Y(MVSIZ),N3Z(MVSIZ),
     .   N4X(MVSIZ),N4Y(MVSIZ),N4Z(MVSIZ),               
     .   FLUX2(MVSIZ), FLUX4(MVSIZ), FLUX5(MVSIZ), FLUX6(MVSIZ),
     .   VX1(MVSIZ), VX2(MVSIZ), VX3(MVSIZ), 
     .   VX4(MVSIZ),
     .   VY1(MVSIZ), VY2(MVSIZ), VY3(MVSIZ), 
     .   VY4(MVSIZ),  
     .   VZ1(MVSIZ), VZ2(MVSIZ), VZ3(MVSIZ),
     .   VZ4(MVSIZ),
     .   VDX1(MVSIZ),VDX2(MVSIZ),VDX3(MVSIZ),VDX4(MVSIZ),
     .   VDY1(MVSIZ),VDY2(MVSIZ),VDY3(MVSIZ),VDY4(MVSIZ),
     .   VDZ1(MVSIZ),VDZ2(MVSIZ),VDZ3(MVSIZ),VDZ4(MVSIZ),
     .   REDUC, UPWL(6,MVSIZ)
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      !======================================================!
      ! INITIALIZATION : COORDINATES & RELATIVE VELOCITIES   !
      !======================================================!
      DO I=LFT,LLT
        II=I+NFT
        MAT(I)=IXS(1,II)
        !---4 local node numbers NC1 TO NC4 for solid element I ---!
        NC1(I)=IXS(2,II)
        NC2(I)=IXS(4,II)
        NC3(I)=IXS(7,II)
        NC4(I)=IXS(6,II)
	!
        !---Coordinates of the 4 nodes
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
        !Relative velocity on the 4 nodes.
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
      ENDDO
 
      !======================================================!
      ! RELATIVE VELOCITIES ON EACH FACE                     !
      !    [V_face] = 1/3 Sum([V_node])                      !
      !      Results are divided by 3 later, then:           ! 
      !  [3*V_face] =     Sum([V_node])                      !        
      !======================================================!
      DO I=LFT,LLT
        ! X-component
        VX1(I)=(VDX1(I)+VDX2(I)+VDX3(I))
        VX2(I)=(VDX4(I)+VDX2(I)+VDX1(I))
        VX3(I)=(VDX4(I)+VDX3(I)+VDX2(I))
        VX4(I)=(VDX4(I)+VDX1(I)+VDX3(I))
        ! Y-component
        VY1(I)=(VDY1(I)+VDY2(I)+VDY3(I))
        VY2(I)=(VDY4(I)+VDY2(I)+VDY1(I))
        VY3(I)=(VDY4(I)+VDY3(I)+VDY2(I))
        VY4(I)=(VDY4(I)+VDY1(I)+VDY3(I))
        ! Z-component
        VZ1(I)=(VDZ1(I)+VDZ2(I)+VDZ3(I))
        VZ2(I)=(VDZ4(I)+VDZ2(I)+VDZ1(I))
        VZ3(I)=(VDZ4(I)+VDZ3(I)+VDZ2(I))
        VZ4(I)=(VDZ4(I)+VDZ1(I)+VDZ3(I))
      END DO
      
      !======================================================!
      ! NORMAL VECTORS ON EACH DACE                          !
      !    2S[n] = [diag1] x [diag2]                         ! 
      !    where                                             !
      !      [n] : unitary normal vector on face             !
      !======================================================!
      DO I=LFT,LLT
        ! Face-2
        N2X(I)=-(Y1(I)-Y4(I))*(Z2(I)-Z4(I)) +(Z1(I)-Z4(I))*(Y2(I)-Y4(I))
        N2Y(I)=-(Z1(I)-Z4(I))*(X2(I)-X4(I)) +(X1(I)-X4(I))*(Z2(I)-Z4(I))
        N2Z(I)=-(X1(I)-X4(I))*(Y2(I)-Y4(I)) +(Y1(I)-Y4(I))*(X2(I)-X4(I))
        ! Face-3
        N3X(I)=-(Y2(I)-Y4(I))*(Z3(I)-Z4(I)) +(Z2(I)-Z4(I))*(Y3(I)-Y4(I))
        N3Y(I)=-(Z2(I)-Z4(I))*(X3(I)-X4(I)) +(X2(I)-X4(I))*(Z3(I)-Z4(I))
        N3Z(I)=-(X2(I)-X4(I))*(Y3(I)-Y4(I)) +(Y2(I)-Y4(I))*(X3(I)-X4(I))
        ! Face-4
        N4X(I)=-(Y3(I)-Y4(I))*(Z1(I)-Z4(I)) +(Z3(I)-Z4(I))*(Y1(I)-Y4(I))
        N4Y(I)=-(Z3(I)-Z4(I))*(X1(I)-X4(I)) +(X3(I)-X4(I))*(Z1(I)-Z4(I))
        N4Z(I)=-(X3(I)-X4(I))*(Y1(I)-Y4(I)) +(Y3(I)-Y4(I))*(X1(I)-X4(I))
        ! Face-1
        N1X(I)= -N2X(I) -N3X(I) -N4X(I)
        N1Y(I)= -N2Y(I) -N3Y(I) -N4Y(I)
        N1Z(I)= -N2Z(I) -N3Z(I) -N4Z(I)
      END DO
      
      !======================================================!
      ! FLUXES CALCULATION ON EACH FACE                      !
      !    FLUX_face = [V_face].[n]                          ! 
      !    FLUX_face = 1/6 * ([3*V_face] . [2S*n])           !       
      !======================================================!
      DO I=LFT,LLT
        FLUX5(I)=(VX1(I)*N1X(I)+VY1(I)*N1Y(I)+VZ1(I)*N1Z(I))*ONE_OVER_6
        FLUX6(I)=(VX2(I)*N2X(I)+VY2(I)*N2Y(I)+VZ2(I)*N2Z(I))*ONE_OVER_6
        FLUX2(I)=(VX3(I)*N3X(I)+VY3(I)*N3Y(I)+VZ3(I)*N3Z(I))*ONE_OVER_6
        FLUX4(I)=(VX4(I)*N4X(I)+VY4(I)*N4Y(I)+VZ4(I)*N4Z(I))*ONE_OVER_6      
      END DO
      
      !======================================================!
      !  UPWIND TREATMENT                                    !
      !    reading coefficient for mass transportation UPWL  !
      !======================================================!
      DO J=1,6
        DO I=LFT,LLT
          UPWL(J,I)=PM(16,MAT(I))
        END DO !J
      END DO !I
      
      !======================================================!
      !  UPWIND TREATMENT                                    !
      !    reducing computed fluxes for :                    !
      !    * elements on boundaries                          !
      !    * elements connected to material law11 (=>UPWL=1) !
      !    FLUX_face = REDUC * FLUX_face                     !
      !    where                                             !
      !      REDUC = Flrd (from /ALE/MAT or /EULER/MAT card) !
      !======================================================!
      DO I=LFT,LLT
       REDUC=PM(92,MAT(I))
       ! Face2-neighbor check 
       IAD2 = ALE_CONNECT%ee_connect%iad_connect(I + NFT)
       II = ALE_CONNECT%ee_connect%connected(IAD2 + 3 - 1)
       IF(II == 0)THEN
        FLUX2(I)=FLUX2(I)*REDUC
       ELSEIF(II > 0)THEN
        IF(INT(PM(19,IXS(1,II))) == 11)THEN
          UPWL(2,I)=ONE
          FLUX2(I)=FLUX2(I)*PM(92,IXS(1,II))
        ENDIF
       ENDIF
       ! Face4-neighbor check
       II = ALE_CONNECT%ee_connect%connected(IAD2 + 4 - 1)
       IF(II == 0)THEN
        FLUX4(I)=FLUX4(I)*REDUC
       ELSEIF(II > 0)THEN
        IF(INT(PM(19,IXS(1,II))) == 11)THEN
          UPWL(4,I)=ONE
          FLUX4(I)=FLUX4(I)*PM(92,IXS(1,II))
        ENDIF
       ENDIF
       ! Face5-neighbor check
       II = ALE_CONNECT%ee_connect%connected(IAD2 + 1 - 1)
       IF(II == 0)THEN
        FLUX5(I)=FLUX5(I)*REDUC
       ELSEIF(II > 0)THEN
        IF(INT(PM(19,IXS(1,II))) == 11)THEN
          UPWL(5,I)=ONE
          FLUX5(I)=FLUX5(I)*PM(92,IXS(1,II))
        ENDIF
       ENDIF
       ! Face6-neighbor check
       II = ALE_CONNECT%ee_connect%connected(IAD2 + 2 - 1)
       IF(II == 0)THEN
        FLUX6(I)=FLUX6(I)*REDUC
       ELSEIF(II > 0)THEN
        IF(INT(PM(19,IXS(1,II))) == 11)THEN
          UPWL(6,I)=ONE
          FLUX6(I)=FLUX6(I)*PM(92,IXS(1,II))
        ENDIF
       ENDIF
      END DO

      !==========================================================!
      ! FLUXES OUTPUT                                            !
      !   FLUX_face   =      (1-sgn*UPWL)*FLUX_face              !
      !   FLUX_global = SUM{                                     !
      !                      (1+sgn*UPWL)*FLUX_face              !
      !                                            ,face=1..6 }  !
      !   where                                                  !
      !     UPWL : upwind factor for mass transportation         !
      !     sgn  : sign of computed flux on face                 !
      !==========================================================!
      DO I=LFT,LLT
        ! 6 fluxes on faces      
        FLUX(1,I)=ZERO
        FLUX(2,I)=FLUX2(I)-UPWL(2,I)*ABS(FLUX2(I))
        FLUX(3,I)=ZERO
        FLUX(4,I)=FLUX4(I)-UPWL(4,I)*ABS(FLUX4(I))
        FLUX(5,I)=FLUX5(I)-UPWL(5,I)*ABS(FLUX5(I))
        FLUX(6,I)=FLUX6(I)-UPWL(6,I)*ABS(FLUX6(I))
        ! One global flux
        FLU1(I)  =FLUX2(I)+UPWL(2,I)*ABS(FLUX2(I))
     .           +FLUX4(I)+UPWL(4,I)*ABS(FLUX4(I))
     .           +FLUX5(I)+UPWL(5,I)*ABS(FLUX5(I))
     .           +FLUX6(I)+UPWL(6,I)*ABS(FLUX6(I))
      END DO
C-----------------------------------------------
      RETURN
      END
C
