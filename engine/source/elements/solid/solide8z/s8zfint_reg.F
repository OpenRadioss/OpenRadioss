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
Chd|  S8ZFINT_REG                   source/elements/solid/solide8z/s8zfint_reg.F
Chd|-- called by -----------
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|-- calls ---------------
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|====================================================================
      SUBROUTINE S8ZFINT_REG(NLOC_DMG,
     .                     VAR_REG ,NEL      ,OFFG     ,VOL      ,
     .                     NC1     ,NC2      ,NC3      ,NC4      ,
     .                     NC5     ,NC6      ,NC7      ,NC8      ,
     .                     PX1     ,PX2      ,PX3      ,PX4      ,
     .                     PX5     ,PX6      ,PX7      ,PX8      ,
     .                     PY1     ,PY2      ,PY3      ,PY4      ,
     .                     PY5     ,PY6      ,PY7      ,PY8      ,
     .                     PZ1     ,PZ2      ,PZ3      ,PZ4      ,
     .                     PZ5     ,PZ6      ,PZ7      ,PZ8      ,
     .                     IMAT    ,H        ,WI       ,IP       ,
     .                     ITASK   ,DT2T     ,VOL0     )
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE NLOCAL_REG_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "parit_c.inc"
#include      "vect01_c.inc"
#include      "scr02_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ::  NEL,IMAT,IP,ITASK
      INTEGER, DIMENSION(NEL) :: NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8
      my_real, DIMENSION(NEL), INTENT(IN) :: 
     .   OFFG
      my_real, INTENT(INOUT)                     ::
     .   DT2T
      my_real, DIMENSION(NEL), INTENT(IN) :: 
     .   VAR_REG,PX1,PX2,PX3,PX4,PX5,PX6,PX7,PX8,
     .   PY1,PY2,PY3,PY4,PY5,PY6,PY7,PY8,PZ1,PZ2,
     .   PZ3,PZ4,PZ5,PZ6,PZ7,PZ8,VOL,H(8),VOL0
      TYPE(NLOCAL_STR_), TARGET :: NLOC_DMG 
      my_real
     .   WI
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,K,NNOD,N1,N2,N3,N4,N5,N6,N7,N8,L_NLOC
      my_real 
     . DX, DY, DZ, L2,XI,NTVAR,A,
     . B1,B2,B3,B4,B5,B6,B7,B8,
     . A1,A2,A3,A4,A5,A6,A7,A8,C1,C2,C3,C4,C5,C6,C7,C8,
     . ZETA,SSPNL,DTNL,LE_MAX,MAXSTIF
      my_real, DIMENSION(NEL) :: 
     . F1,F2,F3,F4,F5,F6,F7,F8,LC
      my_real, DIMENSION(:) ,ALLOCATABLE   :: 
     . BTB11,BTB12,BTB13,BTB14,BTB15,BTB16,BTB17,BTB18,
     . BTB22,BTB23,BTB24,BTB25,BTB26,BTB27,BTB28,BTB33,
     . BTB34,BTB35,BTB36,BTB37,BTB38,BTB44,BTB45,BTB46,
     . BTB47,BTB48,BTB55,BTB56,BTB57,BTB58,BTB66,BTB67,
     . BTB68,BTB77,BTB78,BTB88,STI1,STI2,STI3,STI4,STI5,
     . STI6,STI7,STI8
      INTEGER, DIMENSION(:), ALLOCATABLE   ::
     . POS1,POS2,POS3,POS4,POS5,POS6,POS7,POS8
      my_real, POINTER, DIMENSION(:) :: 
     . VNL,FNL,UNL,STIFNL,MASS,MASS0,VNL0
      ! Coefficient for non-local stability to take into account damping
      my_real, PARAMETER :: CDAMP = 0.7D0
c-----------------------------------------------------------------------
c     VAR_REG :  variable a regulariser (local cumulated plastic strain)
c     NTVAR  =  NT * VAR_REG
C=======================================================================
      L2     = NLOC_DMG%LEN(IMAT)**2
      XI     = NLOC_DMG%DAMP(IMAT)
      NNOD   = NLOC_DMG%NNOD
      L_NLOC = NLOC_DMG%L_NLOC
      ZETA   = NLOC_DMG%DENS(IMAT)
      SSPNL  = NLOC_DMG%SSPNL(IMAT)
      LE_MAX = NLOC_DMG%LE_MAX(IMAT) ! Maximal length of convergence
      LC(1:NEL) = ZERO
      VNL  => NLOC_DMG%VNL(1:L_NLOC)
      VNL0 => NLOC_DMG%VNL_OLD(1:L_NLOC)
      UNL  => NLOC_DMG%UNL(1:L_NLOC)
      ALLOCATE(BTB11(NEL),BTB12(NEL),BTB13(NEL),BTB14(NEL),BTB15(NEL),
     . BTB16(NEL),BTB17(NEL),BTB18(NEL),BTB22(NEL),BTB23(NEL),BTB24(NEL),
     . BTB25(NEL),BTB26(NEL),BTB27(NEL),BTB28(NEL),BTB33(NEL),BTB34(NEL),
     . BTB35(NEL),BTB36(NEL),BTB37(NEL),BTB38(NEL),BTB44(NEL),BTB45(NEL),
     . BTB46(NEL),BTB47(NEL),BTB48(NEL),BTB55(NEL),BTB56(NEL),BTB57(NEL),
     . BTB58(NEL),BTB66(NEL),BTB67(NEL),BTB68(NEL),BTB77(NEL),BTB78(NEL),
     . BTB88(NEL),POS1(NEL),POS2(NEL),POS3(NEL),POS4(NEL),POS5(NEL),
     . POS6(NEL),POS7(NEL),POS8(NEL))
      ! For nodal timestep
      IF (NODADT > 0) THEN
        ! Non-local nodal stifness
        ALLOCATE(STI1(NEL),STI2(NEL),STI3(NEL),STI4(NEL),STI5(NEL),STI6(NEL),
     .   STI7(NEL),STI8(NEL))
        ! Non-local mass
        MASS =>  NLOC_DMG%MASS(1:L_NLOC)
        ! Initial non-local mass
        MASS0 => NLOC_DMG%MASS0(1:L_NLOC)
      ENDIF
c
      !-----------------------------------------------------------------------
      ! Computation of the element volume and the BtB matrix product
      !-----------------------------------------------------------------------
      ! Loop over elements
# include "vectorize.inc"
      DO I=1,NEL
        !print*, ' pla ds sfint_reg' , VARNL(i)
        N1 = NLOC_DMG%IDXI(NC1(I))
        N2 = NLOC_DMG%IDXI(NC2(I))
        N3 = NLOC_DMG%IDXI(NC3(I))
        N4 = NLOC_DMG%IDXI(NC4(I))
        N5 = NLOC_DMG%IDXI(NC5(I))
        N6 = NLOC_DMG%IDXI(NC6(I))
        N7 = NLOC_DMG%IDXI(NC7(I))
        N8 = NLOC_DMG%IDXI(NC8(I))
c        
        ! Recovering the position of the non-local d.o.f.s
        POS1(I) = NLOC_DMG%POSI(N1)
        POS2(I) = NLOC_DMG%POSI(N2)
        POS3(I) = NLOC_DMG%POSI(N3)
        POS4(I) = NLOC_DMG%POSI(N4) 
        POS5(I) = NLOC_DMG%POSI(N5)
        POS6(I) = NLOC_DMG%POSI(N6)
        POS7(I) = NLOC_DMG%POSI(N7)
        POS8(I) = NLOC_DMG%POSI(N8)
c        
        ! Computation of the product BtxB 
        BTB11(I) = PX1(I)**2 + PY1(I)**2 + PZ1(I)**2
        BTB12(I) = PX1(I)*PX2(I) + PY1(I)*PY2(I) + PZ1(I)*PZ2(I)
        BTB13(I) = PX1(I)*PX3(I) + PY1(I)*PY3(I) + PZ1(I)*PZ3(I)
        BTB14(I) = PX1(I)*PX4(I) + PY1(I)*PY4(I) + PZ1(I)*PZ4(I)
        BTB15(I) = PX1(I)*PX5(I) + PY1(I)*PY5(I) + PZ1(I)*PZ5(I)
        BTB16(I) = PX1(I)*PX6(I) + PY1(I)*PY6(I) + PZ1(I)*PZ6(I)
        BTB17(I) = PX1(I)*PX7(I) + PY1(I)*PY7(I) + PZ1(I)*PZ7(I)
        BTB18(I) = PX1(I)*PX8(I) + PY1(I)*PY8(I) + PZ1(I)*PZ8(I)
        BTB22(I) = PX2(I)**2 + PY2(I)**2 + PZ2(I)**2
        BTB23(I) = PX2(I)*PX3(I) + PY2(I)*PY3(I) + PZ2(I)*PZ3(I)
        BTB24(I) = PX2(I)*PX4(I) + PY2(I)*PY4(I) + PZ2(I)*PZ4(I)
        BTB25(I) = PX2(I)*PX5(I) + PY2(I)*PY5(I) + PZ2(I)*PZ5(I)
        BTB26(I) = PX2(I)*PX6(I) + PY2(I)*PY6(I) + PZ2(I)*PZ6(I)
        BTB27(I) = PX2(I)*PX7(I) + PY2(I)*PY7(I) + PZ2(I)*PZ7(I)
        BTB28(I) = PX2(I)*PX8(I) + PY2(I)*PY8(I) + PZ2(I)*PZ8(I)
        BTB33(I) = PX3(I)**2 + PY3(I)**2 + PZ3(I)**2
        BTB34(I) = PX3(I)*PX4(I) + PY3(I)*PY4(I) + PZ3(I)*PZ4(I)
        BTB35(I) = PX3(I)*PX5(I) + PY3(I)*PY5(I) + PZ3(I)*PZ5(I)
        BTB36(I) = PX3(I)*PX6(I) + PY3(I)*PY6(I) + PZ3(I)*PZ6(I)
        BTB37(I) = PX3(I)*PX7(I) + PY3(I)*PY7(I) + PZ3(I)*PZ7(I)
        BTB38(I) = PX3(I)*PX8(I) + PY3(I)*PY8(I) + PZ3(I)*PZ8(I)
        BTB44(I) = PX4(I)**2 + PY4(I)**2 + PZ4(I)**2
        BTB45(I) = PX4(I)*PX5(I) + PY4(I)*PY5(I) + PZ4(I)*PZ5(I)
        BTB46(I) = PX4(I)*PX6(I) + PY4(I)*PY6(I) + PZ4(I)*PZ6(I)
        BTB47(I) = PX4(I)*PX7(I) + PY4(I)*PY7(I) + PZ4(I)*PZ7(I)
        BTB48(I) = PX4(I)*PX8(I) + PY4(I)*PY8(I) + PZ4(I)*PZ8(I)
        BTB55(I) = PX5(I)**2 + PY5(I)**2 + PZ5(I)**2
        BTB56(I) = PX5(I)*PX6(I) + PY5(I)*PY6(I) + PZ5(I)*PZ6(I)
        BTB57(I) = PX5(I)*PX7(I) + PY5(I)*PY7(I) + PZ5(I)*PZ7(I)
        BTB58(I) = PX5(I)*PX8(I) + PY5(I)*PY8(I) + PZ5(I)*PZ8(I)
        BTB66(I) = PX6(I)**2 + PY6(I)**2 + PZ6(I)**2
        BTB67(I) = PX6(I)*PX7(I) + PY6(I)*PY7(I) + PZ6(I)*PZ7(I)
        BTB68(I) = PX6(I)*PX8(I) + PY6(I)*PY8(I) + PZ6(I)*PZ8(I)
        BTB77(I) = PX7(I)**2 + PY7(I)**2 + PZ7(I)**2
        BTB78(I) = PX7(I)*PX8(I) + PY7(I)*PY8(I) + PZ7(I)*PZ8(I)
        BTB88(I) = PX8(I)**2 + PY8(I)**2 + PZ8(I)**2
c     
      ENDDO        
c
      !-----------------------------------------------------------------------
      ! Computation of non-local forces
      !-----------------------------------------------------------------------
      ! Loop over elements
# include "vectorize.inc"
      DO I=1,NEL
c
        ! If the element is not broken, normal computation
        IF (OFFG(I)/=ZERO) THEN  
          ! Computation of LEN**2*BTB*Unl product and NTN*UNL, NTN*VNL
          A1 = WI * VOL(I) * (H(1)*H(1)*UNL(POS1(I)) + H(1)*H(2)*UNL(POS2(I)) + H(1)*H(3)*UNL(POS3(I))
     .       + H(1)*H(4)*UNL(POS4(I)) + H(1)*H(5)*UNL(POS5(I)) + H(1)*H(6)*UNL(POS6(I))
     .       + H(1)*H(7)*UNL(POS7(I)) + H(1)*H(8)*UNL(POS8(I)))
c
          IF (NODADT == 0) THEN 
            A1 = A1 + WI * VOL(I) * XI * (H(1)*H(1)*VNL(POS1(I)) + H(1)*H(2)*VNL(POS2(I)) + H(1)*H(3)*VNL(POS3(I))
     .         + H(1)*H(4)*VNL(POS4(I)) + H(1)*H(5)*VNL(POS5(I)) + H(1)*H(6)*VNL(POS6(I))
     .         + H(1)*H(7)*VNL(POS7(I)) + H(1)*H(8)*VNL(POS8(I)))
          ELSE
            A1 = A1 + WI * VOL(I) * XI * (H(1)*H(1)*(SQRT(MASS(POS1(I))/MASS0(POS1(I))))*VNL(POS1(I)) + 
     .                                    H(1)*H(2)*(SQRT(MASS(POS2(I))/MASS0(POS2(I))))*VNL(POS2(I)) + 
     .                                    H(1)*H(3)*(SQRT(MASS(POS3(I))/MASS0(POS3(I))))*VNL(POS3(I)) + 
     .                                    H(1)*H(4)*(SQRT(MASS(POS4(I))/MASS0(POS4(I))))*VNL(POS4(I)) + 
     .                                    H(1)*H(5)*(SQRT(MASS(POS5(I))/MASS0(POS5(I))))*VNL(POS5(I)) + 
     .                                    H(1)*H(6)*(SQRT(MASS(POS6(I))/MASS0(POS6(I))))*VNL(POS6(I)) + 
     .                                    H(1)*H(7)*(SQRT(MASS(POS7(I))/MASS0(POS7(I))))*VNL(POS7(I)) + 
     .                                    H(1)*H(8)*(SQRT(MASS(POS8(I))/MASS0(POS8(I))))*VNL(POS8(I)))
          ENDIF
c        
          B1 = L2 * WI * VOL(I) * ( BTB11(I)*UNL(POS1(I)) + BTB12(I)*UNL(POS2(I)) 
     .       + BTB13(I)*UNL(POS3(I)) + BTB14(I)*UNL(POS4(I)) + BTB15(I)*UNL(POS5(I))
     .       + BTB16(I)*UNL(POS6(I)) + BTB17(I)*UNL(POS7(I)) + BTB18(I)*UNL(POS8(I)))
c     
          C1 = WI * VOL(I) * H(1) * VAR_REG(I)     
c     
          A2 = WI * VOL(I) * (H(2)*H(1)*UNL(POS1(I)) + H(2)*H(2)*UNL(POS2(I)) + H(2)*H(3)*UNL(POS3(I))
     .       + H(2)*H(4)*UNL(POS4(I)) + H(2)*H(5)*UNL(POS5(I)) + H(2)*H(6)*UNL(POS6(I))
     .       + H(2)*H(7)*UNL(POS7(I)) + H(2)*H(8)*UNL(POS8(I)))
c
          IF (NODADT == 0) THEN 
            A2 = A2 + WI * VOL(I) * XI * (H(2)*H(1)*VNL(POS1(I)) + H(2)*H(2)*VNL(POS2(I)) + H(2)*H(3)*VNL(POS3(I))
     .         + H(2)*H(4)*VNL(POS4(I)) + H(2)*H(5)*VNL(POS5(I)) + H(2)*H(6)*VNL(POS6(I))
     .         + H(2)*H(7)*VNL(POS7(I)) + H(2)*H(8)*VNL(POS8(I)))
          ELSE
            A2 = A2 + WI * VOL(I) * XI * (H(2)*H(1)*(SQRT(MASS(POS1(I))/MASS0(POS1(I))))*VNL(POS1(I)) + 
     .                                    H(2)*H(2)*(SQRT(MASS(POS2(I))/MASS0(POS2(I))))*VNL(POS2(I)) + 
     .                                    H(2)*H(3)*(SQRT(MASS(POS3(I))/MASS0(POS3(I))))*VNL(POS3(I)) + 
     .                                    H(2)*H(4)*(SQRT(MASS(POS4(I))/MASS0(POS4(I))))*VNL(POS4(I)) + 
     .                                    H(2)*H(5)*(SQRT(MASS(POS5(I))/MASS0(POS5(I))))*VNL(POS5(I)) + 
     .                                    H(2)*H(6)*(SQRT(MASS(POS6(I))/MASS0(POS6(I))))*VNL(POS6(I)) + 
     .                                    H(2)*H(7)*(SQRT(MASS(POS7(I))/MASS0(POS7(I))))*VNL(POS7(I)) + 
     .                                    H(2)*H(8)*(SQRT(MASS(POS8(I))/MASS0(POS8(I))))*VNL(POS8(I)))
          ENDIF
c        
          B2 = L2 * WI * VOL(I) * ( BTB12(I)*UNL(POS1(I)) + BTB22(I)*UNL(POS2(I)) 
     .       + BTB23(I)*UNL(POS3(I)) + BTB24(I)*UNL(POS4(I)) + BTB25(I)*UNL(POS5(I))
     .       + BTB26(I)*UNL(POS6(I)) + BTB27(I)*UNL(POS7(I)) + BTB28(I)*UNL(POS8(I)))
c     
          C2 = WI * VOL(I) * H(2) * VAR_REG(I) 
c     
          A3 = WI * VOL(I) * (H(3)*H(1)*UNL(POS1(I)) + H(3)*H(2)*UNL(POS2(I)) + H(3)*H(3)*UNL(POS3(I))
     .       + H(3)*H(4)*UNL(POS4(I)) + H(3)*H(5)*UNL(POS5(I)) + H(3)*H(6)*UNL(POS6(I))
     .       + H(3)*H(7)*UNL(POS7(I)) + H(3)*H(8)*UNL(POS8(I)))
c
          IF (NODADT == 0) THEN 
            A3 = A3 + WI * VOL(I) * XI * (H(3)*H(1)*VNL(POS1(I)) + H(3)*H(2)*VNL(POS2(I)) + H(3)*H(3)*VNL(POS3(I))
     .         + H(3)*H(4)*VNL(POS4(I)) + H(3)*H(5)*VNL(POS5(I)) + H(3)*H(6)*VNL(POS6(I))
     .         + H(3)*H(7)*VNL(POS7(I)) + H(3)*H(8)*VNL(POS8(I))) 
          ELSE
            A3 = A3 + WI * VOL(I) * XI * (H(3)*H(1)*(SQRT(MASS(POS1(I))/MASS0(POS1(I))))*VNL(POS1(I)) + 
     .                                    H(3)*H(2)*(SQRT(MASS(POS2(I))/MASS0(POS2(I))))*VNL(POS2(I)) + 
     .                                    H(3)*H(3)*(SQRT(MASS(POS3(I))/MASS0(POS3(I))))*VNL(POS3(I)) + 
     .                                    H(3)*H(4)*(SQRT(MASS(POS4(I))/MASS0(POS4(I))))*VNL(POS4(I)) + 
     .                                    H(3)*H(5)*(SQRT(MASS(POS5(I))/MASS0(POS5(I))))*VNL(POS5(I)) + 
     .                                    H(3)*H(6)*(SQRT(MASS(POS6(I))/MASS0(POS6(I))))*VNL(POS6(I)) + 
     .                                    H(3)*H(7)*(SQRT(MASS(POS7(I))/MASS0(POS7(I))))*VNL(POS7(I)) + 
     .                                    H(3)*H(8)*(SQRT(MASS(POS8(I))/MASS0(POS8(I))))*VNL(POS8(I)))
          ENDIF
c        
          B3 = L2 * WI * VOL(I) * ( BTB13(I)*UNL(POS1(I)) + BTB23(I)*UNL(POS2(I)) 
     .       + BTB33(I)*UNL(POS3(I)) + BTB34(I)*UNL(POS4(I)) + BTB35(I)*UNL(POS5(I))
     .       + BTB36(I)*UNL(POS6(I)) + BTB37(I)*UNL(POS7(I)) + BTB38(I)*UNL(POS8(I)))
c     
          C3 = WI * VOL(I) * H(3) * VAR_REG(I) 
c     
          A4 = WI * VOL(I) * (H(4)*H(1)*UNL(POS1(I)) + H(4)*H(2)*UNL(POS2(I)) + H(4)*H(3)*UNL(POS3(I))
     .       + H(4)*H(4)*UNL(POS4(I)) + H(4)*H(5)*UNL(POS5(I)) + H(4)*H(6)*UNL(POS6(I))
     .       + H(4)*H(7)*UNL(POS7(I)) + H(4)*H(8)*UNL(POS8(I)))
c
          IF (NODADT == 0) THEN 
            A4 = A4 + WI * VOL(I) * XI * (H(4)*H(1)*VNL(POS1(I)) + H(4)*H(2)*VNL(POS2(I)) + H(4)*H(3)*VNL(POS3(I))
     .         + H(4)*H(4)*VNL(POS4(I)) + H(4)*H(5)*VNL(POS5(I)) + H(4)*H(6)*VNL(POS6(I))
     .         + H(4)*H(7)*VNL(POS7(I)) + H(4)*H(8)*VNL(POS8(I)))  
          ELSE
            A4 = A4 + WI * VOL(I) * XI * (H(4)*H(1)*(SQRT(MASS(POS1(I))/MASS0(POS1(I))))*VNL(POS1(I)) + 
     .                                    H(4)*H(2)*(SQRT(MASS(POS2(I))/MASS0(POS2(I))))*VNL(POS2(I)) + 
     .                                    H(4)*H(3)*(SQRT(MASS(POS3(I))/MASS0(POS3(I))))*VNL(POS3(I)) + 
     .                                    H(4)*H(4)*(SQRT(MASS(POS4(I))/MASS0(POS4(I))))*VNL(POS4(I)) + 
     .                                    H(4)*H(5)*(SQRT(MASS(POS5(I))/MASS0(POS5(I))))*VNL(POS5(I)) + 
     .                                    H(4)*H(6)*(SQRT(MASS(POS6(I))/MASS0(POS6(I))))*VNL(POS6(I)) + 
     .                                    H(4)*H(7)*(SQRT(MASS(POS7(I))/MASS0(POS7(I))))*VNL(POS7(I)) + 
     .                                    H(4)*H(8)*(SQRT(MASS(POS8(I))/MASS0(POS8(I))))*VNL(POS8(I)))
          ENDIF
c        
          B4 = L2 * WI * VOL(I) * ( BTB14(I)*UNL(POS1(I)) + BTB24(I)*UNL(POS2(I)) 
     .       + BTB34(I)*UNL(POS3(I)) + BTB44(I)*UNL(POS4(I)) + BTB45(I)*UNL(POS5(I))
     .       + BTB46(I)*UNL(POS6(I)) + BTB47(I)*UNL(POS7(I)) + BTB48(I)*UNL(POS8(I)))
c     
          C4 = WI * VOL(I) * H(4) * VAR_REG(I)      
c     
          A5 = WI * VOL(I) * (H(5)*H(1)*UNL(POS1(I)) + H(5)*H(2)*UNL(POS2(I)) + H(5)*H(3)*UNL(POS3(I))
     .       + H(5)*H(4)*UNL(POS4(I)) + H(5)*H(5)*UNL(POS5(I)) + H(5)*H(6)*UNL(POS6(I))
     .       + H(5)*H(7)*UNL(POS7(I)) + H(5)*H(8)*UNL(POS8(I)))
c
          IF (NODADT == 0) THEN
            A5 = A5 + WI * VOL(I) * XI * (H(5)*H(1)*VNL(POS1(I)) + H(5)*H(2)*VNL(POS2(I)) + H(5)*H(3)*VNL(POS3(I))
     .         + H(5)*H(4)*VNL(POS4(I)) + H(5)*H(5)*VNL(POS5(I)) + H(5)*H(6)*VNL(POS6(I))
     .         + H(5)*H(7)*VNL(POS7(I)) + H(5)*H(8)*VNL(POS8(I)))  
          ELSE
            A5 = A5 + WI * VOL(I) * XI * (H(5)*H(1)*(SQRT(MASS(POS1(I))/MASS0(POS1(I))))*VNL(POS1(I)) + 
     .                                    H(5)*H(2)*(SQRT(MASS(POS2(I))/MASS0(POS2(I))))*VNL(POS2(I)) + 
     .                                    H(5)*H(3)*(SQRT(MASS(POS3(I))/MASS0(POS3(I))))*VNL(POS3(I)) + 
     .                                    H(5)*H(4)*(SQRT(MASS(POS4(I))/MASS0(POS4(I))))*VNL(POS4(I)) + 
     .                                    H(5)*H(5)*(SQRT(MASS(POS5(I))/MASS0(POS5(I))))*VNL(POS5(I)) + 
     .                                    H(5)*H(6)*(SQRT(MASS(POS6(I))/MASS0(POS6(I))))*VNL(POS6(I)) + 
     .                                    H(5)*H(7)*(SQRT(MASS(POS7(I))/MASS0(POS7(I))))*VNL(POS7(I)) + 
     .                                    H(5)*H(8)*(SQRT(MASS(POS8(I))/MASS0(POS8(I))))*VNL(POS8(I)))
          ENDIF
c        
          B5 = L2 * WI * VOL(I) * ( BTB15(I)*UNL(POS1(I)) + BTB25(I)*UNL(POS2(I)) 
     .       + BTB35(I)*UNL(POS3(I)) + BTB45(I)*UNL(POS4(I)) + BTB55(I)*UNL(POS5(I))
     .       + BTB56(I)*UNL(POS6(I)) + BTB57(I)*UNL(POS7(I)) + BTB58(I)*UNL(POS8(I)))
c     
          C5 = WI * VOL(I) * H(5) * VAR_REG(I)      
c     
          A6 = WI * VOL(I) * (H(6)*H(1)*UNL(POS1(I)) + H(6)*H(2)*UNL(POS2(I)) + H(6)*H(3)*UNL(POS3(I))
     .       + H(6)*H(4)*UNL(POS4(I)) + H(6)*H(5)*UNL(POS5(I)) + H(6)*H(6)*UNL(POS6(I))
     .       + H(6)*H(7)*UNL(POS7(I)) + H(6)*H(8)*UNL(POS8(I)))
c
          IF (NODADT == 0) THEN
            A6 = A6 + WI * VOL(I) * XI * (H(6)*H(1)*VNL(POS1(I)) + H(6)*H(2)*VNL(POS2(I)) + H(6)*H(3)*VNL(POS3(I))
     .         + H(6)*H(4)*VNL(POS4(I)) + H(6)*H(5)*VNL(POS5(I)) + H(6)*H(6)*VNL(POS6(I))
     .         + H(6)*H(7)*VNL(POS7(I)) + H(6)*H(8)*VNL(POS8(I))) 
          ELSE
            A6 = A6 + WI * VOL(I) * XI * (H(6)*H(1)*(SQRT(MASS(POS1(I))/MASS0(POS1(I))))*VNL(POS1(I)) + 
     .                                    H(6)*H(2)*(SQRT(MASS(POS2(I))/MASS0(POS2(I))))*VNL(POS2(I)) + 
     .                                    H(6)*H(3)*(SQRT(MASS(POS3(I))/MASS0(POS3(I))))*VNL(POS3(I)) + 
     .                                    H(6)*H(4)*(SQRT(MASS(POS4(I))/MASS0(POS4(I))))*VNL(POS4(I)) + 
     .                                    H(6)*H(5)*(SQRT(MASS(POS5(I))/MASS0(POS5(I))))*VNL(POS5(I)) + 
     .                                    H(6)*H(6)*(SQRT(MASS(POS6(I))/MASS0(POS6(I))))*VNL(POS6(I)) + 
     .                                    H(6)*H(7)*(SQRT(MASS(POS7(I))/MASS0(POS7(I))))*VNL(POS7(I)) + 
     .                                    H(6)*H(8)*(SQRT(MASS(POS8(I))/MASS0(POS8(I))))*VNL(POS8(I)))
          ENDIF
c        
          B6 = L2 * WI * VOL(I) * ( BTB16(I)*UNL(POS1(I)) + BTB26(I)*UNL(POS2(I)) 
     .       + BTB36(I)*UNL(POS3(I)) + BTB46(I)*UNL(POS4(I)) + BTB56(I)*UNL(POS5(I))
     .       + BTB66(I)*UNL(POS6(I)) + BTB67(I)*UNL(POS7(I)) + BTB68(I)*UNL(POS8(I)))
c     
          C6 = WI * VOL(I) * H(6) * VAR_REG(I)      
c     
          A7 = WI * VOL(I) * (H(7)*H(1)*UNL(POS1(I)) + H(7)*H(2)*UNL(POS2(I)) + H(7)*H(3)*UNL(POS3(I))
     .       + H(7)*H(4)*UNL(POS4(I)) + H(7)*H(5)*UNL(POS5(I)) + H(7)*H(6)*UNL(POS6(I))
     .       + H(7)*H(7)*UNL(POS7(I)) + H(7)*H(8)*UNL(POS8(I)))
c
          IF (NODADT == 0) THEN
            A7 = A7 + WI * VOL(I) * XI * (H(7)*H(1)*VNL(POS1(I)) + H(7)*H(2)*VNL(POS2(I)) + H(7)*H(3)*VNL(POS3(I))
     .         + H(7)*H(4)*VNL(POS4(I)) + H(7)*H(5)*VNL(POS5(I)) + H(7)*H(6)*VNL(POS6(I))
     .         + H(7)*H(7)*VNL(POS7(I)) + H(7)*H(8)*VNL(POS8(I)))
          ELSE
            A7 = A7 + WI * VOL(I) * XI * (H(7)*H(1)*(SQRT(MASS(POS1(I))/MASS0(POS1(I))))*VNL(POS1(I)) + 
     .                                    H(7)*H(2)*(SQRT(MASS(POS2(I))/MASS0(POS2(I))))*VNL(POS2(I)) + 
     .                                    H(7)*H(3)*(SQRT(MASS(POS3(I))/MASS0(POS3(I))))*VNL(POS3(I)) + 
     .                                    H(7)*H(4)*(SQRT(MASS(POS4(I))/MASS0(POS4(I))))*VNL(POS4(I)) + 
     .                                    H(7)*H(5)*(SQRT(MASS(POS5(I))/MASS0(POS5(I))))*VNL(POS5(I)) + 
     .                                    H(7)*H(6)*(SQRT(MASS(POS6(I))/MASS0(POS6(I))))*VNL(POS6(I)) + 
     .                                    H(7)*H(7)*(SQRT(MASS(POS7(I))/MASS0(POS7(I))))*VNL(POS7(I)) + 
     .                                    H(7)*H(8)*(SQRT(MASS(POS8(I))/MASS0(POS8(I))))*VNL(POS8(I)))
          ENDIF  
c        
          B7 = L2 * WI * VOL(I) * ( BTB17(I)*UNL(POS1(I)) + BTB27(I)*UNL(POS2(I)) 
     .       + BTB37(I)*UNL(POS3(I)) + BTB47(I)*UNL(POS4(I)) + BTB57(I)*UNL(POS5(I))
     .       + BTB67(I)*UNL(POS6(I)) + BTB77(I)*UNL(POS7(I)) + BTB78(I)*UNL(POS8(I)))
c     
          C7 = WI * VOL(I) * H(7) * VAR_REG(I)      
c     
          A8 = WI * VOL(I) * (H(8)*H(1)*UNL(POS1(I)) + H(8)*H(2)*UNL(POS2(I)) + H(8)*H(3)*UNL(POS3(I))
     .       + H(8)*H(4)*UNL(POS4(I)) + H(8)*H(5)*UNL(POS5(I)) + H(8)*H(6)*UNL(POS6(I))
     .       + H(8)*H(7)*UNL(POS7(I)) + H(8)*H(8)*UNL(POS8(I)))
c
          IF (NODADT == 0) THEN
            A8 = A8 + WI * VOL(I) * XI * (H(8)*H(1)*VNL(POS1(I)) + H(8)*H(2)*VNL(POS2(I)) + H(8)*H(3)*VNL(POS3(I))
     .         + H(8)*H(4)*VNL(POS4(I)) + H(8)*H(5)*VNL(POS5(I)) + H(8)*H(6)*VNL(POS6(I))
     .         + H(8)*H(7)*VNL(POS7(I)) + H(8)*H(8)*VNL(POS8(I)))  
          ELSE
            A8 = A8 + WI * VOL(I) * XI * (H(8)*H(1)*(SQRT(MASS(POS1(I))/MASS0(POS1(I))))*VNL(POS1(I)) + 
     .                                    H(8)*H(2)*(SQRT(MASS(POS2(I))/MASS0(POS2(I))))*VNL(POS2(I)) + 
     .                                    H(8)*H(3)*(SQRT(MASS(POS3(I))/MASS0(POS3(I))))*VNL(POS3(I)) + 
     .                                    H(8)*H(4)*(SQRT(MASS(POS4(I))/MASS0(POS4(I))))*VNL(POS4(I)) + 
     .                                    H(8)*H(5)*(SQRT(MASS(POS5(I))/MASS0(POS5(I))))*VNL(POS5(I)) + 
     .                                    H(8)*H(6)*(SQRT(MASS(POS6(I))/MASS0(POS6(I))))*VNL(POS6(I)) + 
     .                                    H(8)*H(7)*(SQRT(MASS(POS7(I))/MASS0(POS7(I))))*VNL(POS7(I)) + 
     .                                    H(8)*H(8)*(SQRT(MASS(POS8(I))/MASS0(POS8(I))))*VNL(POS8(I)))
          ENDIF
c        
          B8 = L2 * WI * VOL(I) * ( BTB18(I)*UNL(POS1(I)) + BTB28(I)*UNL(POS2(I)) 
     .       + BTB38(I)*UNL(POS3(I)) + BTB48(I)*UNL(POS4(I)) + BTB58(I)*UNL(POS5(I))
     .       + BTB68(I)*UNL(POS6(I)) + BTB78(I)*UNL(POS7(I)) + BTB88(I)*UNL(POS8(I)))
c     
          C8 = WI * VOL(I) * H(8) * VAR_REG(I)      
c
c       Fint = Vol * (L*L * BT*B*Unl + NT*N*Unl + Damp*NT*N*Vnl - NT*Vreg  )    
c       L    = longueur stocke ds la structure 
c       Unl  = cumul nodal def
c       vnl  = vitesse non locale
c          
          F1(I) = A1 + B1 - C1
          F2(I) = A2 + B2 - C2
          F3(I) = A3 + B3 - C3
          F4(I) = A4 + B4 - C4
          F5(I) = A5 + B5 - C5
          F6(I) = A6 + B6 - C6
          F7(I) = A7 + B7 - C7
          F8(I) = A8 + B8 - C8
c
          ! Computing nodal equivalent stiffness
          IF (NODADT > 0) THEN 
            STI1(I) = (ABS(L2*BTB11(I)  + H(1)*H(1)) + ABS(L2*BTB12(I)  + H(1)*H(2)) + ABS(L2*BTB13(I)  + H(1)*H(3)) +
     .                 ABS(L2*BTB14(I)  + H(1)*H(4)) + ABS(L2*BTB15(I)  + H(1)*H(5)) + ABS(L2*BTB16(I)  + H(1)*H(6)) +
     .                 ABS(L2*BTB17(I)  + H(1)*H(7)) + ABS(L2*BTB18(I)  + H(1)*H(8)))*WI*VOL(I)
            STI2(I) = (ABS(L2*BTB12(I)  + H(2)*H(1)) + ABS(L2*BTB22(I)  + H(2)*H(2)) + ABS(L2*BTB23(I)  + H(2)*H(3)) +
     .                 ABS(L2*BTB24(I)  + H(2)*H(4)) + ABS(L2*BTB25(I)  + H(2)*H(5)) + ABS(L2*BTB26(I)  + H(2)*H(6)) +
     .                 ABS(L2*BTB27(I)  + H(2)*H(7)) + ABS(L2*BTB28(I)  + H(2)*H(8)))*WI*VOL(I)
            STI3(I) = (ABS(L2*BTB13(I)  + H(3)*H(1)) + ABS(L2*BTB23(I)  + H(3)*H(2)) + ABS(L2*BTB33(I)  + H(3)*H(3)) +
     .                 ABS(L2*BTB34(I)  + H(3)*H(4)) + ABS(L2*BTB35(I)  + H(3)*H(5)) + ABS(L2*BTB36(I)  + H(3)*H(6)) +
     .                 ABS(L2*BTB37(I)  + H(3)*H(7)) + ABS(L2*BTB38(I)  + H(3)*H(8)))*WI*VOL(I)
            STI4(I) = (ABS(L2*BTB14(I)  + H(4)*H(1)) + ABS(L2*BTB24(I)  + H(4)*H(2)) + ABS(L2*BTB34(I)  + H(4)*H(3)) +
     .                 ABS(L2*BTB44(I)  + H(4)*H(4)) + ABS(L2*BTB45(I)  + H(4)*H(5)) + ABS(L2*BTB46(I)  + H(4)*H(6)) +
     .                 ABS(L2*BTB47(I)  + H(4)*H(7)) + ABS(L2*BTB48(I)  + H(4)*H(8)))*WI*VOL(I)
            STI5(I) = (ABS(L2*BTB15(I)  + H(5)*H(1)) + ABS(L2*BTB25(I)  + H(5)*H(2)) + ABS(L2*BTB35(I)  + H(5)*H(3)) +
     .                 ABS(L2*BTB45(I)  + H(5)*H(4)) + ABS(L2*BTB55(I)  + H(5)*H(5)) + ABS(L2*BTB56(I)  + H(5)*H(6)) +
     .                 ABS(L2*BTB57(I)  + H(5)*H(7)) + ABS(L2*BTB58(I)  + H(5)*H(8)))*WI*VOL(I)
            STI6(I) = (ABS(L2*BTB16(I)  + H(6)*H(1)) + ABS(L2*BTB26(I)  + H(6)*H(2)) + ABS(L2*BTB36(I)  + H(6)*H(3)) +
     .                 ABS(L2*BTB46(I)  + H(6)*H(4)) + ABS(L2*BTB56(I)  + H(6)*H(5)) + ABS(L2*BTB66(I)  + H(6)*H(6)) +
     .                 ABS(L2*BTB67(I)  + H(6)*H(7)) + ABS(L2*BTB68(I)  + H(6)*H(8)))*WI*VOL(I)
            STI7(I) = (ABS(L2*BTB17(I)  + H(7)*H(1)) + ABS(L2*BTB27(I)  + H(7)*H(2)) + ABS(L2*BTB37(I)  + H(7)*H(3)) +
     .                 ABS(L2*BTB47(I)  + H(7)*H(4)) + ABS(L2*BTB57(I)  + H(7)*H(5)) + ABS(L2*BTB67(I)  + H(7)*H(6)) +
     .                 ABS(L2*BTB77(I)  + H(7)*H(7)) + ABS(L2*BTB78(I)  + H(7)*H(8)))*WI*VOL(I)
            STI8(I) = (ABS(L2*BTB18(I)  + H(8)*H(1)) + ABS(L2*BTB28(I)  + H(8)*H(2)) + ABS(L2*BTB38(I)  + H(8)*H(3)) +
     .                 ABS(L2*BTB48(I)  + H(8)*H(4)) + ABS(L2*BTB58(I)  + H(8)*H(5)) + ABS(L2*BTB68(I)  + H(8)*H(6)) +
     .                 ABS(L2*BTB78(I)  + H(8)*H(7)) + ABS(L2*BTB88(I)  + H(8)*H(8)))*WI*VOL(I)
          ENDIF
c
        ! If the element is broken
        ELSE
c
          ! Initial element characteristic length
          LC(I) = VOL0(I)**THIRD 
c
          IF (NODADT > 0) THEN           
            ! Non-local absorbing forces
            F1(I) = SQRT(MASS(POS1(I))/MASS0(POS1(I)))*H(1)*ZETA*SSPNL*HALF*
     .                                (VNL(POS1(I))+VNL0(POS1(I)))*(THREE/FOUR)*(LC(I)**2)
            F2(I) = SQRT(MASS(POS2(I))/MASS0(POS2(I)))*H(2)*ZETA*SSPNL*HALF*
     .                                (VNL(POS2(I))+VNL0(POS2(I)))*(THREE/FOUR)*(LC(I)**2)
            F3(I) = SQRT(MASS(POS3(I))/MASS0(POS3(I)))*H(3)*ZETA*SSPNL*HALF*
     .                                (VNL(POS3(I))+VNL0(POS3(I)))*(THREE/FOUR)*(LC(I)**2)
            F4(I) = SQRT(MASS(POS4(I))/MASS0(POS4(I)))*H(4)*ZETA*SSPNL*HALF*
     .                                (VNL(POS4(I))+VNL0(POS4(I)))*(THREE/FOUR)*(LC(I)**2)
            F5(I) = SQRT(MASS(POS5(I))/MASS0(POS5(I)))*H(5)*ZETA*SSPNL*HALF*
     .                                (VNL(POS5(I))+VNL0(POS5(I)))*(THREE/FOUR)*(LC(I)**2)
            F6(I) = SQRT(MASS(POS6(I))/MASS0(POS6(I)))*H(6)*ZETA*SSPNL*HALF*
     .                                (VNL(POS6(I))+VNL0(POS6(I)))*(THREE/FOUR)*(LC(I)**2)
            F7(I) = SQRT(MASS(POS7(I))/MASS0(POS7(I)))*H(7)*ZETA*SSPNL*HALF*
     .                                (VNL(POS7(I))+VNL0(POS7(I)))*(THREE/FOUR)*(LC(I)**2)
            F8(I) = SQRT(MASS(POS8(I))/MASS0(POS8(I)))*H(8)*ZETA*SSPNL*HALF*
     .                                (VNL(POS8(I))+VNL0(POS8(I)))*(THREE/FOUR)*(LC(I)**2)
            ! Computing nodal equivalent stiffness
            STI1(I) = EM20
            STI2(I) = EM20
            STI3(I) = EM20
            STI4(I) = EM20
            STI5(I) = EM20
            STI6(I) = EM20
            STI7(I) = EM20
            STI8(I) = EM20
          ELSE
            ! Non-local absorbing forces
            F1(I) = H(1)*ZETA*SSPNL*HALF*(VNL(POS1(I))+VNL0(POS1(I)))*(THREE/FOUR)*(LC(I)**2)
            F2(I) = H(2)*ZETA*SSPNL*HALF*(VNL(POS2(I))+VNL0(POS2(I)))*(THREE/FOUR)*(LC(I)**2)
            F3(I) = H(3)*ZETA*SSPNL*HALF*(VNL(POS3(I))+VNL0(POS3(I)))*(THREE/FOUR)*(LC(I)**2)
            F4(I) = H(4)*ZETA*SSPNL*HALF*(VNL(POS4(I))+VNL0(POS4(I)))*(THREE/FOUR)*(LC(I)**2)
            F5(I) = H(5)*ZETA*SSPNL*HALF*(VNL(POS5(I))+VNL0(POS5(I)))*(THREE/FOUR)*(LC(I)**2)
            F6(I) = H(6)*ZETA*SSPNL*HALF*(VNL(POS6(I))+VNL0(POS6(I)))*(THREE/FOUR)*(LC(I)**2)
            F7(I) = H(7)*ZETA*SSPNL*HALF*(VNL(POS7(I))+VNL0(POS7(I)))*(THREE/FOUR)*(LC(I)**2)
            F8(I) = H(8)*ZETA*SSPNL*HALF*(VNL(POS8(I))+VNL0(POS8(I)))*(THREE/FOUR)*(LC(I)**2)
          ENDIF
        ENDIF
      ENDDO
c-----------------------------------------------------------------------
c     Assemblage       
c-----------------------------------------------------------------------
      IF (IPARIT == 0) THEN 
c
        FNL => NLOC_DMG%FNL(1:L_NLOC,ITASK+1)
        IF (NODADT > 0) STIFNL => NLOC_DMG%STIFNL(1:L_NLOC,ITASK+1) ! Non-local equivalent nodal stiffness
        DO I=1,NEL
          ! Assembling the forces in the classic way 
          FNL(POS1(I)) = FNL(POS1(I)) - F1(I)
          FNL(POS2(I)) = FNL(POS2(I)) - F2(I)
          FNL(POS3(I)) = FNL(POS3(I)) - F3(I)
          FNL(POS4(I)) = FNL(POS4(I)) - F4(I)
          FNL(POS5(I)) = FNL(POS5(I)) - F5(I)
          FNL(POS6(I)) = FNL(POS6(I)) - F6(I)
          FNL(POS7(I)) = FNL(POS7(I)) - F7(I)
          FNL(POS8(I)) = FNL(POS8(I)) - F8(I)
          IF (NODADT > 0) THEN
            ! Spectral radius of stiffness matrix
            MAXSTIF = MAX(STI1(I),STI2(I),STI3(I),STI4(I),STI5(I),STI6(I),STI7(I),STI8(I))
            ! Computing nodal stiffness
            STIFNL(POS1(I)) = STIFNL(POS1(I)) + MAXSTIF
            STIFNL(POS2(I)) = STIFNL(POS2(I)) + MAXSTIF
            STIFNL(POS3(I)) = STIFNL(POS3(I)) + MAXSTIF
            STIFNL(POS4(I)) = STIFNL(POS4(I)) + MAXSTIF
            STIFNL(POS5(I)) = STIFNL(POS5(I)) + MAXSTIF
            STIFNL(POS6(I)) = STIFNL(POS6(I)) + MAXSTIF
            STIFNL(POS7(I)) = STIFNL(POS7(I)) + MAXSTIF
            STIFNL(POS8(I)) = STIFNL(POS8(I)) + MAXSTIF
          ENDIF
        ENDDO
c
      ! If PARITH/ON
      ELSE
c
        DO I=1,NEL
          II  = I + NFT
c
          ! Spectral radius of stiffness matrix
          IF (NODADT > 0) THEN
            MAXSTIF = MAX(STI1(I),STI2(I),STI3(I),STI4(I),STI5(I),STI6(I),STI7(I),STI8(I))
          ENDIF
c
          K = NLOC_DMG%IADS(1,II)
          IF (IP == 1) THEN 
            NLOC_DMG%FSKY(K,1) = -F1(I)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,1) = MAXSTIF
          ELSE
            NLOC_DMG%FSKY(K,1) = NLOC_DMG%FSKY(K,1) - F1(I)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,1) = NLOC_DMG%STSKY(K,1) + MAXSTIF
          ENDIF
c
          K = NLOC_DMG%IADS(2,II)
          IF (IP == 1) THEN 
            NLOC_DMG%FSKY(K,1) = -F2(I)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,1) = MAXSTIF
          ELSE
            NLOC_DMG%FSKY(K,1) = NLOC_DMG%FSKY(K,1) - F2(I)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,1) = NLOC_DMG%STSKY(K,1) + MAXSTIF
          ENDIF
c
          K = NLOC_DMG%IADS(3,II)
          IF (IP == 1) THEN 
            NLOC_DMG%FSKY(K,1) = -F3(I)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,1) = MAXSTIF
          ELSE
            NLOC_DMG%FSKY(K,1) = NLOC_DMG%FSKY(K,1) - F3(I)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,1) = NLOC_DMG%STSKY(K,1) + MAXSTIF
          ENDIF
c
          K = NLOC_DMG%IADS(4,II)
          IF (IP == 1) THEN 
            NLOC_DMG%FSKY(K,1) = -F4(I)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,1) = MAXSTIF
          ELSE
            NLOC_DMG%FSKY(K,1) = NLOC_DMG%FSKY(K,1) - F4(I)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,1) = NLOC_DMG%STSKY(K,1) + MAXSTIF
          ENDIF
c
          K = NLOC_DMG%IADS(5,II)
          IF (IP == 1) THEN 
            NLOC_DMG%FSKY(K,1) = -F5(I)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,1) = MAXSTIF
          ELSE
            NLOC_DMG%FSKY(K,1) = NLOC_DMG%FSKY(K,1) - F5(I)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,1) = NLOC_DMG%STSKY(K,1) + MAXSTIF
          ENDIF
c
          K = NLOC_DMG%IADS(6,II)
          IF (IP == 1) THEN 
            NLOC_DMG%FSKY(K,1) = -F6(I)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,1) = MAXSTIF
          ELSE
            NLOC_DMG%FSKY(K,1) = NLOC_DMG%FSKY(K,1) - F6(I)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,1) = NLOC_DMG%STSKY(K,1) + MAXSTIF
          ENDIF
c
          K = NLOC_DMG%IADS(7,II)
          IF (IP == 1) THEN 
            NLOC_DMG%FSKY(K,1) = -F7(I)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,1) = MAXSTIF
          ELSE
            NLOC_DMG%FSKY(K,1) = NLOC_DMG%FSKY(K,1) - F7(I)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,1) = NLOC_DMG%STSKY(K,1) + MAXSTIF
          ENDIF
c
          K = NLOC_DMG%IADS(8,II)
          IF (IP == 1) THEN 
            NLOC_DMG%FSKY(K,1) = -F8(I)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,1) = MAXSTIF
          ELSE
            NLOC_DMG%FSKY(K,1) = NLOC_DMG%FSKY(K,1) - F8(I)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,1) = NLOC_DMG%STSKY(K,1) + MAXSTIF
          ENDIF
c
        ENDDO
      ENDIF      
c      
      !-----------------------------------------------------------------------
      ! Computing non-local timestep
      !-----------------------------------------------------------------------   
      IF (NODADT == 0) THEN
        DO I = 1,NEL
          ! If the element is not broken, normal computation
          IF (OFFG(I)/=ZERO) THEN
            ! Non-local critical time-step in the plane
            DTNL = (TWO*(MIN(VOL(I)**THIRD,LE_MAX))*SQRT(THREE*ZETA))/
     .                  SQRT(TWELVE*L2 + (MIN(VOL(I)**THIRD,LE_MAX))**2)
            ! Retaining the minimal value
            DT2T = MIN(DT2T,DTFAC1(1)*CDAMP*DTNL)
          ENDIF
        ENDDO
      ENDIF
c-----------
      ! Deallocation of tables
      IF (ALLOCATED(BTB11)) DEALLOCATE(BTB11)
      IF (ALLOCATED(BTB12)) DEALLOCATE(BTB12)
      IF (ALLOCATED(BTB13)) DEALLOCATE(BTB13)
      IF (ALLOCATED(BTB14)) DEALLOCATE(BTB14)
      IF (ALLOCATED(BTB15)) DEALLOCATE(BTB15)
      IF (ALLOCATED(BTB16)) DEALLOCATE(BTB16)
      IF (ALLOCATED(BTB17)) DEALLOCATE(BTB17)
      IF (ALLOCATED(BTB18)) DEALLOCATE(BTB18)
      IF (ALLOCATED(BTB22)) DEALLOCATE(BTB22)
      IF (ALLOCATED(BTB23)) DEALLOCATE(BTB23)      
      IF (ALLOCATED(BTB24)) DEALLOCATE(BTB24)
      IF (ALLOCATED(BTB25)) DEALLOCATE(BTB25)
      IF (ALLOCATED(BTB26)) DEALLOCATE(BTB26)
      IF (ALLOCATED(BTB27)) DEALLOCATE(BTB27)
      IF (ALLOCATED(BTB28)) DEALLOCATE(BTB28)
      IF (ALLOCATED(BTB33)) DEALLOCATE(BTB33)      
      IF (ALLOCATED(BTB34)) DEALLOCATE(BTB34)
      IF (ALLOCATED(BTB35)) DEALLOCATE(BTB35)
      IF (ALLOCATED(BTB36)) DEALLOCATE(BTB36)
      IF (ALLOCATED(BTB37)) DEALLOCATE(BTB37)
      IF (ALLOCATED(BTB38)) DEALLOCATE(BTB38)
      IF (ALLOCATED(BTB44)) DEALLOCATE(BTB44)
      IF (ALLOCATED(BTB45)) DEALLOCATE(BTB45)
      IF (ALLOCATED(BTB46)) DEALLOCATE(BTB46)
      IF (ALLOCATED(BTB47)) DEALLOCATE(BTB47)
      IF (ALLOCATED(BTB48)) DEALLOCATE(BTB48)
      IF (ALLOCATED(BTB55)) DEALLOCATE(BTB55)
      IF (ALLOCATED(BTB56)) DEALLOCATE(BTB56)
      IF (ALLOCATED(BTB57)) DEALLOCATE(BTB57)
      IF (ALLOCATED(BTB58)) DEALLOCATE(BTB58)
      IF (ALLOCATED(BTB66)) DEALLOCATE(BTB66)
      IF (ALLOCATED(BTB67)) DEALLOCATE(BTB67)
      IF (ALLOCATED(BTB68)) DEALLOCATE(BTB68)
      IF (ALLOCATED(BTB77)) DEALLOCATE(BTB77)
      IF (ALLOCATED(BTB78)) DEALLOCATE(BTB78)
      IF (ALLOCATED(BTB88)) DEALLOCATE(BTB88)
      IF (ALLOCATED(POS1))  DEALLOCATE(POS1)
      IF (ALLOCATED(POS2))  DEALLOCATE(POS2) 
      IF (ALLOCATED(POS3))  DEALLOCATE(POS3)
      IF (ALLOCATED(POS4))  DEALLOCATE(POS4) 
      IF (ALLOCATED(POS5))  DEALLOCATE(POS5)
      IF (ALLOCATED(POS6))  DEALLOCATE(POS6) 
      IF (ALLOCATED(POS7))  DEALLOCATE(POS7)
      IF (ALLOCATED(POS8))  DEALLOCATE(POS8)
      IF (ALLOCATED(STI1))  DEALLOCATE(STI1)
      IF (ALLOCATED(STI2))  DEALLOCATE(STI2) 
      IF (ALLOCATED(STI3))  DEALLOCATE(STI3)
      IF (ALLOCATED(STI4))  DEALLOCATE(STI4) 
      IF (ALLOCATED(STI5))  DEALLOCATE(STI5)
      IF (ALLOCATED(STI6))  DEALLOCATE(STI6) 
      IF (ALLOCATED(STI7))  DEALLOCATE(STI7)
      IF (ALLOCATED(STI8))  DEALLOCATE(STI8)
c
      END
