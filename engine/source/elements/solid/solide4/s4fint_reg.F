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
Chd|  S4FINT_REG                    source/elements/solid/solide4/s4fint_reg.F
Chd|-- called by -----------
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|-- calls ---------------
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|====================================================================
      SUBROUTINE S4FINT_REG(NLOC_DMG,VAR_REG  ,
     .                      NEL     ,OFF      ,VOL      ,NC1     ,
     .                      NC2     ,NC3      ,NC4      ,PX1     ,
     .                      PX2     ,PX3      ,PX4      ,PY1     ,
     .                      PY2     ,PY3      ,PY4      ,PZ1     ,
     .                      PZ2     ,PZ3      ,PZ4      ,IMAT    ,
     .                      ITASK   ,DT2T     )
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
#include      "param_c.inc"
#include      "parit_c.inc"
#include      "vect01_c.inc"
#include      "scr02_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ::  NEL, IMAT, ITASK
      INTEGER, DIMENSION(NEL) :: NC1,NC2,NC3,NC4
      my_real, INTENT(INOUT)                     ::
     .  DT2T
      my_real, DIMENSION(NEL), INTENT(IN) :: 
     .   VOL,OFF,VAR_REG,
     .   PX1,PX2,PX3,PX4,PY1,PY2,PY3,PY4,PZ1,PZ2,PZ3,PZ4
      TYPE(NLOCAL_STR_), TARGET :: NLOC_DMG 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,K,NNOD,N1,N2,N3,N4,L_NLOC
      my_real 
     . DX, DY, DZ, L2,NTN,NTN_UNL,NTN_VNL,XI,NTVAR,A,
     . B1,B2,B3,B4,ZETA,SSPNL,DTNL,LE_MAX,MAXSTIF
      my_real, DIMENSION(NEL) :: 
     . F1,F2,F3,F4
      my_real, DIMENSION(:) ,ALLOCATABLE   :: 
     . BTB11,BTB12,BTB13,BTB14,BTB22,BTB23,BTB24,
     . BTB33,BTB34,BTB44,STI1,STI2,STI3,STI4
      INTEGER, DIMENSION(:), ALLOCATABLE   ::
     . POS1,POS2,POS3,POS4
      my_real, POINTER, DIMENSION(:) :: 
     . VNL,FNL,UNL,STIFNL,MASS,MASS0,VNL0
      ! Coefficient for non-local stability to take into account damping
      my_real, PARAMETER :: CDAMP = 0.7D0
c-----------------------------------------------------------------------
c     VAR_REG :  variable a regulariser (local cumulated plastic strain)
c     NTVAR  =  NT * VAR_REG
c=======================================================================
      L2     = NLOC_DMG%LEN(IMAT)**2
      XI     = NLOC_DMG%DAMP(IMAT)
      NNOD   = NLOC_DMG%NNOD
      L_NLOC = NLOC_DMG%L_NLOC
      ZETA   = NLOC_DMG%DENS(IMAT)
      SSPNL  = NLOC_DMG%SSPNL(IMAT)
      LE_MAX = NLOC_DMG%LE_MAX(IMAT) ! Maximal length of convergence
      NTN    = QUATRE*QUATRE
      ALLOCATE(BTB11(NEL),BTB12(NEL),BTB13(NEL),BTB14(NEL),BTB22(NEL),
     . BTB23(NEL),BTB24(NEL),BTB33(NEL),BTB34(NEL),BTB44(NEL),POS1(NEL),
     . POS2(NEL),POS3(NEL),POS4(NEL))
      ! For nodal timestep
      IF (NODADT > 0) THEN
        ! Non-local nodal stifness
        ALLOCATE(STI1(NEL),STI2(NEL),STI3(NEL),STI4(NEL))
        ! Non-local mass
        MASS =>  NLOC_DMG%MASS(1:L_NLOC)
        ! Initial non-local mass
        MASS0 => NLOC_DMG%MASS0(1:L_NLOC)
      ENDIF
      VNL  => NLOC_DMG%VNL(1:L_NLOC)
      VNL0 => NLOC_DMG%VNL_OLD(1:L_NLOC)
      UNL  => NLOC_DMG%UNL(1:L_NLOC)
c
      !-----------------------------------------------------------------------
      ! Computation of the element volume and the BtB matrix product
      !-----------------------------------------------------------------------
      ! Loop over elements
# include "vectorize.inc"
      DO I=1,NEL
c
        ! Recovering the nodes of the tetra element
        N1 = NLOC_DMG%IDXI(NC1(I))
        N2 = NLOC_DMG%IDXI(NC2(I))
        N3 = NLOC_DMG%IDXI(NC3(I))
        N4 = NLOC_DMG%IDXI(NC4(I))
c        
        ! Recovering the positions of the first d.o.fs of each nodes
        POS1(I) = NLOC_DMG%POSI(N1)
        POS2(I) = NLOC_DMG%POSI(N2)
        POS3(I) = NLOC_DMG%POSI(N3)
        POS4(I) = NLOC_DMG%POSI(N4) 
c        
        ! Computation of the product BtxB 
        BTB11(I) = PX1(I)**2 + PY1(I)**2 + PZ1(I)**2
        BTB12(I) = PX1(I)*PX2(I) + PY1(I)*PY2(I) + PZ1(I)*PZ2(I)
        BTB13(I) = PX1(I)*PX3(I) + PY1(I)*PY3(I) + PZ1(I)*PZ3(I)
        BTB14(I) = PX1(I)*PX4(I) + PY1(I)*PY4(I) + PZ1(I)*PZ4(I)
        BTB22(I) = PX2(I)**2 + PY2(I)**2 + PZ2(I)**2
        BTB23(I) = PX2(I)*PX3(I) + PY2(I)*PY3(I) + PZ2(I)*PZ3(I)
        BTB24(I) = PX2(I)*PX4(I) + PY2(I)*PY4(I) + PZ2(I)*PZ4(I)
        BTB33(I) = PX3(I)**2 + PY3(I)**2 + PZ3(I)**2
        BTB34(I) = PX3(I)*PX4(I) + PY3(I)*PY4(I) + PZ3(I)*PZ4(I)
        BTB44(I) = PX4(I)**2 + PY4(I)**2 + PZ4(I)**2
c
      ENDDO
c      
      !-----------------------------------------------------------------------
      ! Computation of non-local forces
      !-----------------------------------------------------------------------
      ! Loop over elements
# include "vectorize.inc"
      DO I = 1, NEL      
c     
        ! If the element is not broken, normal computation
        IF (OFF(I)/=ZERO) THEN 
c
          ! Computing the product NtN*UNL
          NTN_UNL = (UNL(POS1(I)) + UNL(POS2(I)) + UNL(POS3(I)) + UNL(POS4(I))) / NTN
c        
          ! Computing the product XDAMP*NtN*VNL
          NTN_VNL = (VNL(POS1(I)) + VNL(POS2(I)) + VNL(POS3(I)) + VNL(POS4(I))) / NTN
          IF (NODADT > 0) THEN 
            NTN_VNL = (SQRT(MASS(POS1(I))/MASS0(POS1(I)))*VNL(POS1(I)) + 
     .                 SQRT(MASS(POS2(I))/MASS0(POS2(I)))*VNL(POS2(I)) + 
     .                 SQRT(MASS(POS3(I))/MASS0(POS3(I)))*VNL(POS3(I)) + 
     .                 SQRT(MASS(POS4(I))/MASS0(POS4(I)))*VNL(POS4(I))) / NTN
          ENDIF
c
          ! Computation of the product LEN**2 * BtxB
          B1 = L2 * VOL(I) * ( BTB11(I)*UNL(POS1(I)) + BTB12(I)*UNL(POS2(I)) 
     .       + BTB13(I)*UNL(POS3(I)) + BTB14(I)*UNL(POS4(I)))
c        
          B2 = L2 * VOL(I) * ( BTB12(I)*UNL(POS1(I)) + BTB22(I)*UNL(POS2(I)) 
     .       + BTB23(I)*UNL(POS3(I)) + BTB24(I)*UNL(POS4(I)))
c        
          B3 = L2 * VOL(I) * ( BTB13(I)*UNL(POS1(I)) + BTB23(I)*UNL(POS2(I)) 
     .       + BTB33(I)*UNL(POS3(I)) + BTB34(I)*UNL(POS4(I)))
c        
          B4 = L2 * VOL(I) * ( BTB14(I)*UNL(POS1(I)) + BTB24(I)*UNL(POS2(I)) 
     .       + BTB34(I)*UNL(POS3(I)) + BTB44(I)*UNL(POS4(I)))  
c
          ! Multiplication by the volume of the element    
          NTN_UNL = NTN_UNL * VOL(I)
          NTN_VNL = NTN_VNL * XI * VOL(I)
c
          ! Introducing the internal variable to be regularized
          NTVAR   = VAR_REG(I)*QUART* VOL(I)
c
          ! Computing the elementary non-local forces
          A = NTN_UNL + NTN_VNL - NTVAR
          F1(I) = A + B1
          F2(I) = A + B2
          F3(I) = A + B3
          F4(I) = A + B4
c
          ! Computing nodal equivalent stiffness
          IF (NODADT > 0) THEN 
            STI1(I) = (ABS(L2*BTB11(I)  + UN/NTN) + ABS(L2*BTB12(I)  + UN/NTN) + ABS(L2*BTB13(I)  + UN/NTN) +
     .                 ABS(L2*BTB14(I)  + UN/NTN))* VOL(I)
            STI2(I) = (ABS(L2*BTB12(I)  + UN/NTN) + ABS(L2*BTB22(I)  + UN/NTN) + ABS(L2*BTB23(I)  + UN/NTN) +
     .                 ABS(L2*BTB24(I)  + UN/NTN))* VOL(I) 
            STI3(I) = (ABS(L2*BTB13(I)  + UN/NTN) + ABS(L2*BTB23(I)  + UN/NTN) + ABS(L2*BTB33(I)  + UN/NTN) +
     .                 ABS(L2*BTB34(I)  + UN/NTN))* VOL(I) 
            STI4(I) = (ABS(L2*BTB14(I)  + UN/NTN) + ABS(L2*BTB24(I)  + UN/NTN) + ABS(L2*BTB34(I)  + UN/NTN) +
     .                 ABS(L2*BTB44(I)  + UN/NTN))* VOL(I) 
          ENDIF
c
        ! If the element is broken, computation of absorbing forces
        ELSE
          IF (NODADT > 0) THEN     
            ! Non-local absorbing forces
            F1(I) = SQRT(MASS(POS1(I))/MASS0(POS1(I)))*ZETA*SSPNL*UNDEMI*
     .                          (VNL(POS1(I))+VNL0(POS1(I)))*(SQRT(TROIS)/QUATRE)*(LE_MAX**2)
            F2(I) = SQRT(MASS(POS2(I))/MASS0(POS2(I)))*ZETA*SSPNL*UNDEMI*
     .                          (VNL(POS2(I))+VNL0(POS2(I)))*(SQRT(TROIS)/QUATRE)*(LE_MAX**2)
            F3(I) = SQRT(MASS(POS3(I))/MASS0(POS3(I)))*ZETA*SSPNL*UNDEMI*
     .                          (VNL(POS3(I))+VNL0(POS3(I)))*(SQRT(TROIS)/QUATRE)*(LE_MAX**2)
            F4(I) = SQRT(MASS(POS4(I))/MASS0(POS4(I)))*ZETA*SSPNL*UNDEMI*
     .                          (VNL(POS4(I))+VNL0(POS4(I)))*(SQRT(TROIS)/QUATRE)*(LE_MAX**2) 
            ! Computing nodal equivalent stiffness
            STI1(I) = EM20
            STI2(I) = EM20
            STI3(I) = EM20
            STI4(I) = EM20        
          ELSE
            ! Non-local absorbing forces
            F1(I) = ZETA*SSPNL*UNDEMI*(VNL(POS1(I))+VNL0(POS1(I)))*(SQRT(TROIS)/QUATRE)*(LE_MAX**2)
            F2(I) = ZETA*SSPNL*UNDEMI*(VNL(POS2(I))+VNL0(POS2(I)))*(SQRT(TROIS)/QUATRE)*(LE_MAX**2)
            F3(I) = ZETA*SSPNL*UNDEMI*(VNL(POS3(I))+VNL0(POS3(I)))*(SQRT(TROIS)/QUATRE)*(LE_MAX**2)
            F4(I) = ZETA*SSPNL*UNDEMI*(VNL(POS4(I))+VNL0(POS4(I)))*(SQRT(TROIS)/QUATRE)*(LE_MAX**2)          
          ENDIF
        ENDIF
      ENDDO
c-----------------------------------------------------------------------
c     Assemblage       
c-----------------------------------------------------------------------
      ! If PARITH/OFF
      IF (IPARIT == 0) THEN 
        FNL => NLOC_DMG%FNL(1:L_NLOC,ITASK+1)
        IF (NODADT > 0) STIFNL => NLOC_DMG%STIFNL(1:L_NLOC,ITASK+1) ! Non-local equivalent nodal stiffness
        DO I=1,NEL
          ! Assembling the forces in the classic way 
          FNL(POS1(I)) = FNL(POS1(I)) - F1(I)
          FNL(POS2(I)) = FNL(POS2(I)) - F2(I)
          FNL(POS3(I)) = FNL(POS3(I)) - F3(I)
          FNL(POS4(I)) = FNL(POS4(I)) - F4(I)
          IF (NODADT > 0) THEN
            ! Spectral radius of stiffness matrix
            MAXSTIF = MAX(STI1(I),STI2(I),STI3(I),STI4(I))
            ! Computing nodal stiffness
            STIFNL(POS1(I)) = STIFNL(POS1(I)) + MAXSTIF
            STIFNL(POS2(I)) = STIFNL(POS2(I)) + MAXSTIF
            STIFNL(POS3(I)) = STIFNL(POS3(I)) + MAXSTIF
            STIFNL(POS4(I)) = STIFNL(POS4(I)) + MAXSTIF
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
            MAXSTIF = MAX(STI1(I),STI2(I),STI3(I),STI4(I))
          ENDIF
c
          K = NLOC_DMG%IADS(1,II)
          NLOC_DMG%FSKY(K,1) = -F1(I)
          IF (NODADT > 0) NLOC_DMG%STSKY(K,1) = MAXSTIF
c
          K = NLOC_DMG%IADS(3,II)
          NLOC_DMG%FSKY(K,1) = -F2(I)
          IF (NODADT > 0) NLOC_DMG%STSKY(K,1) = MAXSTIF
c
          K = NLOC_DMG%IADS(6,II)
          NLOC_DMG%FSKY(K,1) = -F3(I)
          IF (NODADT > 0) NLOC_DMG%STSKY(K,1) = MAXSTIF
c
          K = NLOC_DMG%IADS(5,II)
          NLOC_DMG%FSKY(K,1) = -F4(I)
          IF (NODADT > 0) NLOC_DMG%STSKY(K,1) = MAXSTIF
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
          IF (OFF(I)/=ZERO) THEN
            ! Non-local critical time-step in the plane
            DTNL = (DEUX*(MIN(VOL(I)**TIERS,LE_MAX))*SQRT(TROIS*ZETA))/
     .                  SQRT(DOUZE*L2 + (MIN(VOL(I)**TIERS,LE_MAX))**2)
            ! Retaining the minimal value
            DT2T = MIN(DT2T,DTFAC1(1)*CDAMP*DTNL)
          ENDIF
        ENDDO
      ENDIF
c
c-----------
      ! Deallocation of tables
      IF (ALLOCATED(BTB11)) DEALLOCATE(BTB11)
      IF (ALLOCATED(BTB12)) DEALLOCATE(BTB12)
      IF (ALLOCATED(BTB13)) DEALLOCATE(BTB13)
      IF (ALLOCATED(BTB14)) DEALLOCATE(BTB14)
      IF (ALLOCATED(BTB22)) DEALLOCATE(BTB22)
      IF (ALLOCATED(BTB23)) DEALLOCATE(BTB23)      
      IF (ALLOCATED(BTB24)) DEALLOCATE(BTB24)
      IF (ALLOCATED(BTB33)) DEALLOCATE(BTB33)
      IF (ALLOCATED(BTB34)) DEALLOCATE(BTB34)
      IF (ALLOCATED(BTB44)) DEALLOCATE(BTB44)
      IF (ALLOCATED(POS1))  DEALLOCATE(POS1)
      IF (ALLOCATED(POS2))  DEALLOCATE(POS2) 
      IF (ALLOCATED(POS3))  DEALLOCATE(POS3)
      IF (ALLOCATED(POS4))  DEALLOCATE(POS4)
      IF (ALLOCATED(STI1))  DEALLOCATE(STI1)
      IF (ALLOCATED(STI2))  DEALLOCATE(STI2) 
      IF (ALLOCATED(STI3))  DEALLOCATE(STI3)
      IF (ALLOCATED(STI4))  DEALLOCATE(STI4) 
c
      END
