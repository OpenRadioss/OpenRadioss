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
Chd|  S6CFINT_REG                   source/elements/thickshell/solide6c/s6cfint_reg.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|====================================================================
      SUBROUTINE S6CFINT_REG(
     1   NLOC_DMG,VAR_REG, NEL,     OFF,
     2   VOL,     NC1,     NC2,     NC3,
     3   NC4,     NC5,     NC6,     PX1,
     4   PX2,     PX3,     PX4,     PY1,
     5   PY2,     PY3,     PY4,     PZ1,
     6   PZ2,     PZ3,     PZ4,     IMAT,
     7   ITASK,   DT2T,    VOL0,    NFT,
     8   NLAY,    WS,      AS,      AREA,
     9   BUFNLTS)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE NLOCAL_REG_MOD
      USE ELBUFDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "parit_c.inc"
#include      "scr02_c.inc"
#include      "scr18_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: 
     .   NFT,NLAY,NEL,IMAT,ITASK 
      INTEGER, INTENT(IN), DIMENSION(NEL) :: 
     .   NC1,NC2,NC3,NC4,NC5,NC6
      my_real, INTENT(INOUT) ::
     .   DT2T
      my_real, DIMENSION(9,9), INTENT(IN) :: 
     .   WS,AS
      my_real, DIMENSION(NEL,NLAY), INTENT(INOUT) ::
     .   VAR_REG
      my_real, DIMENSION(NEL), INTENT(IN) :: 
     .   VOL,OFF,VOL0,PX1,PX2,PX3,PX4,AREA,
     .   PY1,PY2,PY3,PY4,PZ1,PZ2,PZ3,PZ4
      TYPE(NLOCAL_STR_), INTENT(INOUT), TARGET :: NLOC_DMG 
      TYPE(BUF_NLOCTS_), INTENT(INOUT), TARGET :: BUFNLTS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,K,NNOD,N1,N2,N3,N4,N5,N6,
     .        L_NLOC,NDDL,NDNOD
      my_real 
     . L2,NTN,NTN_UNL,NTN_VNL,XI,NTVAR,A,DTNL,LE_MAX,
     . B1,B2,B3,B4,B5,B6,ZETA,SSPNL,MAXSTIF,
     . BTH1,BTH2,NTH1,NTH2,DT2P,DTNOD,K1,K2,K12,
     . DTNL_TH
      my_real, DIMENSION(NEL,NLAY) :: 
     . F1,F2,F3,F4,F5,F6
      my_real, DIMENSION(NEL) ::
     . LC,THK,LTHK,PXX1,PXX2,PXX3,
     . PXX4,PXX5,PXX6,PYY1,PYY2,PYY3,
     . PYY4,PYY5,PYY6,PZZ1,PZZ2,PZZ3,
     . PZZ4,PZZ5,PZZ6
      my_real, DIMENSION(:) ,ALLOCATABLE   :: 
     . BTB11,BTB12,BTB13,BTB14,BTB15,BTB16,
     . BTB22,BTB23,BTB24,BTB25,BTB26,BTB33,
     . BTB34,BTB35,BTB36,BTB44,BTB45,BTB46,
     . BTB55,BTB56,BTB66
      my_real, DIMENSION(:,:) ,ALLOCATABLE ::
     . STI1,STI2,STI3,STI4,STI5,STI6
      INTEGER, DIMENSION(:), ALLOCATABLE   ::
     . POS1,POS2,POS3,POS4,POS5,POS6
      my_real, POINTER, DIMENSION(:) :: 
     . VNL,FNL,UNL,STIFNL,MASS,MASS0,VNL0
      my_real, POINTER, DIMENSION(:,:)     :: 
     . MASSTH,UNLTH,VNLTH,FNLTH
      my_real, DIMENSION(:,:), ALLOCATABLE :: 
     . STIFNLTH,DTN
      ! Safety coefficient for non-local stability vs mechanical stability
      ! (it has been slightly increased vs nloc_dmg_init.F)
      my_real, PARAMETER :: CSTA  = 40.0D0
      ! Coefficient for non-local stability to take into account damping
      my_real, PARAMETER :: CDAMP = 0.7D0
      my_real
     . ZS(10,9)      
      ! Position of nodes in the thickshell thickness
      DATA ZS / 
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,0.               ,0.               ,
     1 0.               ,
     2 -1.              ,0.               ,1.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,0.               ,0.               ,
     2 0.               ,
     3 -1.              ,-.549193338482966,0.549193338482966,
     3 1.               ,0.               ,0.               ,
     3 0.               ,0.               ,0.               ,
     3 0.               ,
     4 -1.              ,-.600558677589454,0.               ,
     4 0.600558677589454,1.               ,0.               ,
     4 0.               ,0.               ,0.               ,
     4 0.               ,
     5 -1.              ,-.812359691877328,-.264578928334038,
     5 0.264578928334038,0.812359691877328,1.               ,
     5 0.               ,0.               ,0.               ,
     5 0.               ,
     6 -1.              ,-.796839450334708,-.449914286274731,
     6 0.               ,0.449914286274731,0.796839450334708,
     6 1.               ,0.               ,0.               ,
     6 0.               ,
     7 -1.              ,-.898215824685518,-.584846546513270,
     7 -.226843756241524,0.226843756241524,0.584846546513270,
     7 0.898215824685518,1.               ,0.               ,
     7 0.               ,
     8 -1.              ,-.878478166955581,-.661099443664978,
     8 -.354483526205989,0.               ,0.354483526205989,
     8 0.661099443664978,0.878478166955581,1.               ,
     8 0.               ,
     9 -1.              ,-.936320479015252,-.735741735638020,
     9 -.491001129763160,-.157505717044458,0.157505717044458,
     9 0.491001129763160,0.735741735638020,0.936320479015252,
     9 1.               /
C=======================================================================
      L2     = NLOC_DMG%LEN(IMAT)**2
      XI     = NLOC_DMG%DAMP(IMAT)
      NNOD   = NLOC_DMG%NNOD
      L_NLOC = NLOC_DMG%L_NLOC
      ZETA   = NLOC_DMG%DENS(IMAT)
      SSPNL  = NLOC_DMG%SSPNL(IMAT)
      LE_MAX = NLOC_DMG%LE_MAX(IMAT) ! Maximal length of convergence
      NTN    = SIX*SIX
      LC(1:NEL) = ZERO
      ALLOCATE(BTB11(NEL),BTB12(NEL),BTB13(NEL),BTB14(NEL),BTB15(NEL),
     .         BTB16(NEL),BTB22(NEL),BTB23(NEL),BTB24(NEL),BTB25(NEL),
     .         BTB26(NEL),BTB33(NEL),BTB34(NEL),BTB35(NEL),BTB36(NEL),
     .         BTB44(NEL),BTB45(NEL),BTB46(NEL),BTB55(NEL),BTB56(NEL),
     .         BTB66(NEL),POS1(NEL) ,POS2(NEL) ,POS3(NEL) ,POS4(NEL) ,
     .         POS5(NEL) ,POS6(NEL) )
      ! For nodal timestep
      IF (NODADT > 0) THEN
        ! Non-local nodal stifness
        ALLOCATE(STI1(NEL,NLAY),STI2(NEL,NLAY),STI3(NEL,NLAY),
     .           STI4(NEL,NLAY),STI5(NEL,NLAY),STI6(NEL,NLAY))
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
        ! Recovering the nodes of the brick element
        N1 = NLOC_DMG%IDXI(NC1(I))
        N2 = NLOC_DMG%IDXI(NC2(I))
        N3 = NLOC_DMG%IDXI(NC3(I))
        N4 = NLOC_DMG%IDXI(NC4(I))
        N5 = NLOC_DMG%IDXI(NC5(I))
        N6 = NLOC_DMG%IDXI(NC6(I))
c
        ! Recovering the positions of the first d.o.fs of each nodes
        POS1(I) = NLOC_DMG%POSI(N1)
        POS2(I) = NLOC_DMG%POSI(N2)
        POS3(I) = NLOC_DMG%POSI(N3)
        POS4(I) = NLOC_DMG%POSI(N4)
        POS5(I) = NLOC_DMG%POSI(N5)
        POS6(I) = NLOC_DMG%POSI(N6)
c
        ! Computation of derivatives of shape functions
        PXX1(I) = PX1(I)-PX4(I)
        PYY1(I) = PY1(I)-PY4(I)
        PZZ1(I) = PZ1(I)-PZ4(I)
c
        PXX2(I) = PX2(I)-PX4(I)
        PYY2(I) = PY2(I)-PY4(I)
        PZZ2(I) = PZ2(I)-PZ4(I)
c
        PXX3(I) = PX3(I)-PX4(I)
        PYY3(I) = PY3(I)-PY4(I)
        PZZ3(I) = PZ3(I)-PZ4(I)
c
        PXX4(I) = PX1(I)+PX4(I)
        PYY4(I) = PY1(I)+PY4(I)
        PZZ4(I) = PZ1(I)+PZ4(I)
c
        PXX5(I) = PX2(I)+PX4(I)
        PYY5(I) = PY2(I)+PY4(I)
        PZZ5(I) = PZ2(I)+PZ4(I)
c
        PXX6(I) = PX3(I)+PX4(I)
        PYY6(I) = PY3(I)+PY4(I)
        PZZ6(I) = PZ3(I)+PZ4(I)
c 
        ! Computation of the product BtxB 
        BTB11(I) = PXX1(I)**2 + PYY1(I)**2 + PZZ1(I)**2
        BTB12(I) = PXX1(I)*PXX2(I) + PYY1(I)*PYY2(I) + PZZ1(I)*PZZ2(I)
        BTB13(I) = PXX1(I)*PXX3(I) + PYY1(I)*PYY3(I) + PZZ1(I)*PZZ3(I)
        BTB14(I) = PXX1(I)*PXX4(I) + PYY1(I)*PYY4(I) + PZZ1(I)*PZZ4(I)
        BTB15(I) = PXX1(I)*PXX5(I) + PYY1(I)*PYY5(I) + PZZ1(I)*PZZ5(I)
        BTB16(I) = PXX1(I)*PXX6(I) + PYY1(I)*PYY6(I) + PZZ1(I)*PZZ6(I)
c 
        BTB22(I) = PXX2(I)**2 + PYY2(I)**2 + PZZ2(I)**2
        BTB23(I) = PXX2(I)*PXX3(I) + PYY2(I)*PYY3(I) + PZZ2(I)*PZZ3(I)
        BTB24(I) = PXX2(I)*PXX4(I) + PYY2(I)*PYY4(I) + PZZ2(I)*PZZ4(I)
        BTB25(I) = PXX2(I)*PXX5(I) + PYY2(I)*PYY5(I) + PZZ2(I)*PZZ5(I)
        BTB26(I) = PXX2(I)*PXX6(I) + PYY2(I)*PYY6(I) + PZZ2(I)*PZZ6(I)
c
        BTB33(I) = PXX3(I)**2 + PYY3(I)**2 + PZZ3(I)**2
        BTB34(I) = PXX3(I)*PXX4(I) + PYY3(I)*PYY4(I) + PZZ3(I)*PZZ4(I)
        BTB35(I) = PXX3(I)*PXX5(I) + PYY3(I)*PYY5(I) + PZZ3(I)*PZZ5(I)
        BTB36(I) = PXX3(I)*PXX6(I) + PYY3(I)*PYY6(I) + PZZ3(I)*PZZ6(I)
c
        BTB44(I) = PXX4(I)**2 + PYY4(I)**2 + PZZ4(I)**2
        BTB45(I) = PXX4(I)*PXX5(I) + PYY4(I)*PYY5(I) + PZZ4(I)*PZZ5(I)
        BTB46(I) = PXX4(I)*PXX6(I) + PYY4(I)*PYY6(I) + PZZ4(I)*PZZ6(I)
c
        BTB55(I) = PXX5(I)**2 + PYY5(I)**2 + PZZ5(I)**2
        BTB56(I) = PXX5(I)*PXX6(I) + PYY5(I)*PYY6(I) + PZZ5(I)*PZZ6(I)
c
        BTB66(I) = PXX6(I)**2 + PYY6(I)**2 + PZZ6(I)**2
c        
      ENDDO
c
      !-----------------------------------------------------------------------
      ! Pre-treatment non-local regularization in the thickshell thickness
      !-----------------------------------------------------------------------
      IF ((L2>ZERO).AND.(NLAY > 1)) THEN 
c
        ! Compute thickshell thickness
        DO I = 1,NEL
          THK(I)  = VOL(I)/AREA(I)
          LTHK(I) = (ZS(NLAY+1,NLAY)-ZS(NLAY,NLAY))*THK(I)*HALF
        ENDDO
c
        ! Allocation of the velocities predictor
        NDDL = NLAY
        IF (NODADT > 0) THEN 
          ALLOCATE(STIFNLTH(NEL,NDDL+1))
          ALLOCATE(DTN(NEL,NDDL+1))
        ENDIF
        NDNOD = NDDL+1
c 
        ! Pointing the non-local values in the thickness of the corresponding element
        MASSTH => BUFNLTS%MASSTH(1:NEL,1:NDNOD)
        UNLTH  => BUFNLTS%UNLTH(1:NEL ,1:NDNOD)
        VNLTH  => BUFNLTS%VNLTH(1:NEL ,1:NDNOD)
        FNLTH  => BUFNLTS%FNLTH(1:NEL ,1:NDNOD)    
c
        DO K = 1,NDNOD
          DO I = 1,NEL
            ! Resetting non-local forces
            FNLTH(I,K) = ZERO
            ! Resetting non-local nodal stiffness
            IF (NODADT > 0) THEN
              STIFNLTH(I,K) = EM20
            ENDIF
          ENDDO
        ENDDO
c
        ! Computation of non-local forces in the shell thickness
        DO K = 1, NDDL
c        
          ! Computation of shape functions value
          NTH1 = (AS(K,NDDL)   - ZS(K+1,NDDL)) / 
     .           (ZS(K,NDDL)   - ZS(K+1,NDDL))
          NTH2 = (AS(K,NDDL)   - ZS(K,NDDL))   / 
     .           (ZS(K+1,NDDL) - ZS(K,NDDL))
c          
          ! Loop over elements
          DO I = 1,NEL
c
            ! Computation of B-matrix values
            BTH1 = (ONE/(ZS(K,NDDL)   - ZS(K+1,NDDL)))*(TWO/THK(I))
            BTH2 = (ONE/(ZS(K+1,NDDL) - ZS(K,NDDL)))*(TWO/THK(I))   
c         
            ! Computation of the non-local K matrix
            K1   = L2*(BTH1**2)  + NTH1**2
            K12  = L2*(BTH1*BTH2)+ (NTH1*NTH2)
            K2   = L2*(BTH2**2)  + NTH2**2
c
            ! Computation of the non-local forces
            FNLTH(I,K)   = FNLTH(I,K) + (K1*UNLTH(I,K) + K12*UNLTH(I,K+1) 
     .                                + XI*((NTH1**2)*VNLTH(I,K) 
     .                                + (NTH1*NTH2)*VNLTH(I,K+1))
     .                                - (NTH1*VAR_REG(I,K)))*HALF*WS(K,NDDL)*VOL(I)  
            FNLTH(I,K+1) = FNLTH(I,K+1) + (K12*UNLTH(I,K) + K2*UNLTH(I,K+1)
     .                                + XI*(NTH1*NTH2*VNLTH(I,K) 
     .                                + (NTH2**2)*VNLTH(I,K+1))
     .                                - NTH2*VAR_REG(I,K))*HALF*WS(K,NDDL)*VOL(I)  
c
            ! Computation of non-local nodal stiffness
            IF (NODADT > 0) THEN 
              STIFNLTH(I,K)   = STIFNLTH(I,K)   + MAX(ABS(K1)+ABS(K12),ABS(K12)+ABS(K2))*HALF*WS(K,NDDL)*VOL(I)
              STIFNLTH(I,K+1) = STIFNLTH(I,K+1) + MAX(ABS(K1)+ABS(K12),ABS(K12)+ABS(K2))*HALF*WS(K,NDDL)*VOL(I)            
            ENDIF
c
          ENDDO
        ENDDO
c       
        ! Updating non-local mass with /DT/NODA
        IF (NODADT > 0) THEN 
C
          ! Initial computation of the nodal timestep
          DTNOD = EP20
          DO K = 1,NDNOD
            DO I = 1,NEL
              DTN(I,K) = DTFAC1(11)*CDAMP*SQRT(TWO*MASSTH(I,K)/MAX(STIFNLTH(I,K),EM20)) 
              DTNOD    = MIN(DTN(I,K),DTNOD)
            ENDDO
          ENDDO
C
          ! /DT/NODA/CSTX - Constant timestep with added mass
          IF ((IDTMIN(11)==3).OR.(IDTMIN(11)==4).OR.(IDTMIN(11)==8)) THEN  
            ! Added mass computation if necessary
            IF (DTNOD < DTMIN1(11)*(SQRT(CSTA))) THEN
              DO K = 1,NDNOD
                DO I = 1,NEL
                  IF (DTN(I,K) < DTMIN1(11)) THEN
                    DT2P        = DTMIN1(11)/(DTFAC1(11)*CDAMP)
                    MASSTH(I,K) = MAX(MASSTH(I,K),CSTA*HALF*STIFNLTH(I,K)*DT2P*DT2P*ONEP00001)
                  ENDIF
                ENDDO
              ENDDO
            ENDIF
            DTNOD = DTMIN1(11)*(SQRT(CSTA))
          ENDIF
C
          ! Classical nodal timestep check
          IF (DTNOD < DT2T) THEN
            DT2T = MIN(DT2T,DTNOD)
          ENDIF
        ENDIF
c
        DO K = 1,NDNOD
          DO I = 1,NEL
            ! Updating the non-local in-thickness velocities   
            VNLTH(I,K) = VNLTH(I,K) - (FNLTH(I,K)/MASSTH(I,K))*DT12
          ENDDO
        ENDDO
c          
        DO K = 1,NDNOD
          DO I = 1,NEL
            ! Computing the non-local in-thickness cumulated values
            UNLTH(I,K) = UNLTH(I,K) + VNLTH(I,K)*DT1
          ENDDO
        ENDDO
c
        ! Transfert at the integration point
        DO K = 1, NDDL
          !Computation of shape functions value
          NTH1 = (AS(K,NDDL)   - ZS(K+1,NDDL))/
     .           (ZS(K,NDDL)   - ZS(K+1,NDDL))
          NTH2 = (AS(K,NDDL)   - ZS(K,NDDL))/
     .           (ZS(K+1,NDDL) - ZS(K,NDDL))
          ! Loop over elements
          DO I = 1,NEL
            !Integration points non-local variables
            VAR_REG(I,K) = NTH1*UNLTH(I,K) + NTH2*UNLTH(I,K+1)
          ENDDO  
        ENDDO
      ENDIF
c      
      !-----------------------------------------------------------------------
      ! Computation of non-local forces
      !-----------------------------------------------------------------------
      ! Loop over elements
      DO K = 1,NLAY
c
        ! Loop over elements
# include "vectorize.inc"
        DO I = 1, NEL
c     
          ! If the element is not broken, normal computation
          IF (OFF(I) /= ZERO) THEN 
c
            ! Computing the product NtN*UNL
            NTN_UNL = (UNL(POS1(I)+K-1) + UNL(POS2(I)+K-1) + UNL(POS3(I)+K-1) + UNL(POS4(I)+K-1)
     .              +  UNL(POS5(I)+K-1) + UNL(POS6(I)+K-1)) / NTN
c        
            ! Computing the product XDAMP*NtN*VNL
            NTN_VNL = (VNL(POS1(I)+K-1) + VNL(POS2(I)+K-1) + VNL(POS3(I)+K-1) + VNL(POS4(I)+K-1)
     .              +  VNL(POS5(I)+K-1) + VNL(POS6(I)+K-1)) / NTN
            IF (NODADT > 0) THEN 
              NTN_VNL = (SQRT(MASS(POS1(I)+K-1)/MASS0(POS1(I)+K-1))*VNL(POS1(I)+K-1) + 
     .                   SQRT(MASS(POS2(I)+K-1)/MASS0(POS2(I)+K-1))*VNL(POS2(I)+K-1) + 
     .                   SQRT(MASS(POS3(I)+K-1)/MASS0(POS3(I)+K-1))*VNL(POS3(I)+K-1) + 
     .                   SQRT(MASS(POS4(I)+K-1)/MASS0(POS4(I)+K-1))*VNL(POS4(I)+K-1) +
     .                   SQRT(MASS(POS5(I)+K-1)/MASS0(POS5(I)+K-1))*VNL(POS5(I)+K-1) + 
     .                   SQRT(MASS(POS6(I)+K-1)/MASS0(POS6(I)+K-1))*VNL(POS6(I)+K-1)) / NTN
            ENDIF
c        
            ! Computation of the product LEN**2 * BtxB
            B1 = L2 * VOL(I) * WS(K,NLAY) *HALF * ( BTB11(I)*UNL(POS1(I)+K-1) + BTB12(I)*UNL(POS2(I)+K-1) 
     .                + BTB13(I)*UNL(POS3(I)+K-1) + BTB14(I)*UNL(POS4(I)+K-1) + BTB15(I)*UNL(POS5(I)+K-1)
     .                + BTB16(I)*UNL(POS6(I)+K-1) )
c        
            B2 = L2 * VOL(I) * WS(K,NLAY) *HALF * ( BTB12(I)*UNL(POS1(I)+K-1) + BTB22(I)*UNL(POS2(I)+K-1) 
     .                + BTB23(I)*UNL(POS3(I)+K-1) + BTB24(I)*UNL(POS4(I)+K-1) + BTB25(I)*UNL(POS5(I)+K-1)
     .                + BTB26(I)*UNL(POS6(I)+K-1) )
c        
            B3 = L2 * VOL(I) * WS(K,NLAY) *HALF * ( BTB13(I)*UNL(POS1(I)+K-1) + BTB23(I)*UNL(POS2(I)+K-1) 
     .                + BTB33(I)*UNL(POS3(I)+K-1) + BTB34(I)*UNL(POS4(I)+K-1) + BTB35(I)*UNL(POS5(I)+K-1)
     .                + BTB36(I)*UNL(POS6(I)+K-1) )
c        
            B4 = L2 * VOL(I) * WS(K,NLAY) *HALF * ( BTB14(I)*UNL(POS1(I)+K-1) + BTB24(I)*UNL(POS2(I)+K-1) 
     .                + BTB34(I)*UNL(POS3(I)+K-1) + BTB44(I)*UNL(POS4(I)+K-1) + BTB45(I)*UNL(POS5(I)+K-1)
     .                + BTB46(I)*UNL(POS6(I)+K-1) )
c       
            B5 = L2 * VOL(I) * WS(K,NLAY) *HALF * ( BTB15(I)*UNL(POS1(I)+K-1) + BTB25(I)*UNL(POS2(I)+K-1) 
     .                + BTB35(I)*UNL(POS3(I)+K-1) + BTB45(I)*UNL(POS4(I)+K-1) + BTB55(I)*UNL(POS5(I)+K-1)
     .                + BTB56(I)*UNL(POS6(I)+K-1) )
c        
            B6 = L2 * VOL(I) * WS(K,NLAY) *HALF * ( BTB16(I)*UNL(POS1(I)+K-1) + BTB26(I)*UNL(POS2(I)+K-1) 
     .                + BTB36(I)*UNL(POS3(I)+K-1) + BTB46(I)*UNL(POS4(I)+K-1) + BTB56(I)*UNL(POS5(I)+K-1)
     .                + BTB66(I)*UNL(POS6(I)+K-1) )
c
            ! Multiplication by the volume of the element    
            NTN_UNL = NTN_UNL * VOL(I) * WS(K,NLAY) * HALF
            NTN_VNL = NTN_VNL * XI * VOL(I) * WS(K,NLAY) * HALF
c
            ! Introducing the internal variable to be regularized
            NTVAR   = VAR_REG(I,K)*ONE_OVER_6* VOL(I) * WS(K,NLAY) * HALF
c
            ! Computing the elementary non-local forces
            A = NTN_UNL + NTN_VNL - NTVAR
            F1(I,K) = A + B1
            F2(I,K) = A + B2
            F3(I,K) = A + B3
            F4(I,K) = A + B4
            F5(I,K) = A + B5
            F6(I,K) = A + B6
c
            ! Computing nodal equivalent stiffness
            IF (NODADT > 0) THEN 
              STI1(I,K) = (ABS(L2*BTB11(I) + ONE/NTN) + ABS(L2*BTB12(I) + ONE/NTN) + ABS(L2*BTB13(I) + ONE/NTN) +
     .                     ABS(L2*BTB14(I) + ONE/NTN) + ABS(L2*BTB15(I) + ONE/NTN) + ABS(L2*BTB16(I) + ONE/NTN))
     .                     *VOL(I)*WS(K,NLAY)*HALF
              STI2(I,K) = (ABS(L2*BTB12(I) + ONE/NTN) + ABS(L2*BTB22(I) + ONE/NTN) + ABS(L2*BTB23(I) + ONE/NTN) +
     .                     ABS(L2*BTB24(I) + ONE/NTN) + ABS(L2*BTB25(I) + ONE/NTN) + ABS(L2*BTB26(I) + ONE/NTN))
     .                     *VOL(I)*WS(K,NLAY)*HALF
              STI3(I,K) = (ABS(L2*BTB13(I) + ONE/NTN) + ABS(L2*BTB23(I) + ONE/NTN) + ABS(L2*BTB33(I) + ONE/NTN) +
     .                     ABS(L2*BTB34(I) + ONE/NTN) + ABS(L2*BTB35(I) + ONE/NTN) + ABS(L2*BTB36(I) + ONE/NTN))
     .                     *VOL(I)*WS(K,NLAY)*HALF
              STI4(I,K) = (ABS(L2*BTB14(I) + ONE/NTN) + ABS(L2*BTB24(I) + ONE/NTN) + ABS(L2*BTB34(I) + ONE/NTN) +
     .                     ABS(L2*BTB44(I) + ONE/NTN) + ABS(L2*BTB45(I) + ONE/NTN) + ABS(L2*BTB46(I) + ONE/NTN))
     .                     *VOL(I)*WS(K,NLAY)*HALF
              STI5(I,K) = (ABS(L2*BTB15(I) + ONE/NTN) + ABS(L2*BTB25(I) + ONE/NTN) + ABS(L2*BTB35(I) + ONE/NTN) +
     .                     ABS(L2*BTB45(I) + ONE/NTN) + ABS(L2*BTB55(I) + ONE/NTN) + ABS(L2*BTB56(I) + ONE/NTN))
     .                     *VOL(I)*WS(K,NLAY)*HALF
              STI6(I,K) = (ABS(L2*BTB16(I) + ONE/NTN) + ABS(L2*BTB26(I) + ONE/NTN) + ABS(L2*BTB36(I) + ONE/NTN) +
     .                     ABS(L2*BTB46(I) + ONE/NTN) + ABS(L2*BTB56(I) + ONE/NTN) + ABS(L2*BTB66(I) + ONE/NTN))
     .                     *VOL(I)*WS(K,NLAY)*HALF
            ENDIF
c            
          ! If the element is broken, the non-local wave is absorbed  
          ELSE
c
            ! Initial element characteristic length
            LC(I) = (VOL0(I)*WS(K,NLAY)*HALF)**THIRD  
c
            IF (NODADT > 0) THEN
     
              ! Non-local absorbing forces
              F1(I,K) = SQRT(MASS(POS1(I)+K-1)/MASS0(POS1(I)+K-1))*ZETA*SSPNL*HALF*
     .                       (VNL(POS1(I)+K-1)+VNL0(POS1(I)+K-1))*(TWO*THIRD)*(LC(I)**2)
              F2(I,K) = SQRT(MASS(POS2(I)+K-1)/MASS0(POS2(I)+K-1))*ZETA*SSPNL*HALF*
     .                       (VNL(POS2(I)+K-1)+VNL0(POS2(I)+K-1))*(TWO*THIRD)*(LC(I)**2)
              F3(I,K) = SQRT(MASS(POS3(I)+K-1)/MASS0(POS3(I)+K-1))*ZETA*SSPNL*HALF*
     .                       (VNL(POS3(I)+K-1)+VNL0(POS3(I)+K-1))*(TWO*THIRD)*(LC(I)**2)
              F4(I,K) = SQRT(MASS(POS4(I)+K-1)/MASS0(POS4(I)+K-1))*ZETA*SSPNL*HALF*
     .                       (VNL(POS4(I)+K-1)+VNL0(POS4(I)+K-1))*(TWO*THIRD)*(LC(I)**2)
              F5(I,K) = SQRT(MASS(POS5(I)+K-1)/MASS0(POS5(I)+K-1))*ZETA*SSPNL*HALF*
     .                       (VNL(POS5(I)+K-1)+VNL0(POS5(I)+K-1))*(TWO*THIRD)*(LC(I)**2)
              F6(I,K) = SQRT(MASS(POS6(I)+K-1)/MASS0(POS6(I)+K-1))*ZETA*SSPNL*HALF*
     .                       (VNL(POS6(I)+K-1)+VNL0(POS6(I)+K-1))*(TWO*THIRD)*(LC(I)**2)
              ! Computing nodal equivalent stiffness
              STI1(I,K) = EM20
              STI2(I,K) = EM20
              STI3(I,K) = EM20
              STI4(I,K) = EM20
              STI5(I,K) = EM20
              STI6(I,K) = EM20
            ELSE
              ! Non-local absorbing forces
              F1(I,K) = ZETA*SSPNL*HALF*(VNL(POS1(I)+K-1)+VNL0(POS1(I)+K-1))*(TWO*THIRD)*(LC(I)**2)
              F2(I,K) = ZETA*SSPNL*HALF*(VNL(POS2(I)+K-1)+VNL0(POS2(I)+K-1))*(TWO*THIRD)*(LC(I)**2)
              F3(I,K) = ZETA*SSPNL*HALF*(VNL(POS3(I)+K-1)+VNL0(POS3(I)+K-1))*(TWO*THIRD)*(LC(I)**2)
              F4(I,K) = ZETA*SSPNL*HALF*(VNL(POS4(I)+K-1)+VNL0(POS4(I)+K-1))*(TWO*THIRD)*(LC(I)**2)
              F5(I,K) = ZETA*SSPNL*HALF*(VNL(POS5(I)+K-1)+VNL0(POS5(I)+K-1))*(TWO*THIRD)*(LC(I)**2)
              F6(I,K) = ZETA*SSPNL*HALF*(VNL(POS6(I)+K-1)+VNL0(POS6(I)+K-1))*(TWO*THIRD)*(LC(I)**2)         
            ENDIF
          ENDIF
        ENDDO
      ENDDO
c      
      !-----------------------------------------------------------------------
      ! Assembling of the non-local forces
      !-----------------------------------------------------------------------
c
      ! If PARITH/OFF
      IF (IPARIT == 0) THEN 
        ! Recovering non-local internal forces
        FNL => NLOC_DMG%FNL(1:L_NLOC,ITASK+1)                       ! Non-local forces
        IF (NODADT > 0) STIFNL => NLOC_DMG%STIFNL(1:L_NLOC,ITASK+1) ! Non-local equivalent nodal stiffness
        ! Loop over elements
        DO I=1,NEL
          ! Loop over non-local degrees of freedom
# include "vectorize.inc"
          DO K=1,NLAY
            ! Assembling the forces in the classic way 
            FNL(POS1(I)+K-1) = FNL(POS1(I)+K-1) - F1(I,K)
            FNL(POS2(I)+K-1) = FNL(POS2(I)+K-1) - F2(I,K)
            FNL(POS3(I)+K-1) = FNL(POS3(I)+K-1) - F3(I,K)
            FNL(POS4(I)+K-1) = FNL(POS4(I)+K-1) - F4(I,K)
            FNL(POS5(I)+K-1) = FNL(POS5(I)+K-1) - F5(I,K)
            FNL(POS6(I)+K-1) = FNL(POS6(I)+K-1) - F6(I,K)  
            IF (NODADT > 0) THEN
              ! Spectral radius of stiffness matrix
              MAXSTIF = MAX(STI1(I,K),STI2(I,K),STI3(I,K),
     .                      STI4(I,K),STI5(I,K),STI6(I,K))
              ! Computing nodal stiffness
              STIFNL(POS1(I)+K-1) = STIFNL(POS1(I)+K-1) + MAXSTIF
              STIFNL(POS2(I)+K-1) = STIFNL(POS2(I)+K-1) + MAXSTIF
              STIFNL(POS3(I)+K-1) = STIFNL(POS3(I)+K-1) + MAXSTIF
              STIFNL(POS4(I)+K-1) = STIFNL(POS4(I)+K-1) + MAXSTIF
              STIFNL(POS5(I)+K-1) = STIFNL(POS5(I)+K-1) + MAXSTIF
              STIFNL(POS6(I)+K-1) = STIFNL(POS6(I)+K-1) + MAXSTIF
            ENDIF
          ENDDO
        ENDDO
c
      ! If PARITH/ON
      ELSE
        ! Loop over additional d.o.fs
        DO J = 1,NLAY
c
          ! Loop over elements
          DO I=1,NEL
            II  = I + NFT
c
            ! Spectral radius of stiffness matrix
            IF (NODADT > 0) THEN
              MAXSTIF = MAX(STI1(I,J),STI2(I,J),STI3(I,J),STI4(I,J),
     .                      STI5(I,J),STI6(I,J))
            ENDIF
c            
            K = NLOC_DMG%IADS(1,II)
            NLOC_DMG%FSKY(K,J) = -F1(I,J)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
c
            K = NLOC_DMG%IADS(2,II)
            NLOC_DMG%FSKY(K,J) = -F2(I,J)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
c
            K = NLOC_DMG%IADS(3,II)
            NLOC_DMG%FSKY(K,J) = -F3(I,J)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
c
            K = NLOC_DMG%IADS(5,II)
            NLOC_DMG%FSKY(K,J) = -F4(I,J)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
c
            K = NLOC_DMG%IADS(6,II)
            NLOC_DMG%FSKY(K,J) = -F5(I,J)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
c
            K = NLOC_DMG%IADS(7,II)
            NLOC_DMG%FSKY(K,J) = -F6(I,J)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
c
          ENDDO
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
            DTNL = (TWO*(MIN((VOL(I))**THIRD,LE_MAX))*SQRT(THREE*ZETA))/
     .              SQRT(TWELVE*L2 + (MIN((VOL(I))**THIRD,LE_MAX))**2)
            ! Non-local critical time-step in the thickness
            IF (NLAY > 1) THEN 
              DTNL_TH = (TWO*(MIN(LTHK(I),LE_MAX))*SQRT(THREE*ZETA))/
     .                SQRT(TWELVE*L2 + (MIN(LTHK(I),LE_MAX))**2)
            ELSE 
              DTNL_TH = EP20
            ENDIF
            ! Retaining the minimal value
            DT2T = MIN(DT2T,DTFAC1(1)*CDAMP*DTNL_TH,DTFAC1(1)*CDAMP*DTNL)
          ENDIF
        ENDDO
      ENDIF
c
      ! Deallocation of tables
      IF (ALLOCATED(BTB11))    DEALLOCATE(BTB11)
      IF (ALLOCATED(BTB12))    DEALLOCATE(BTB12)
      IF (ALLOCATED(BTB13))    DEALLOCATE(BTB13)
      IF (ALLOCATED(BTB14))    DEALLOCATE(BTB14)
      IF (ALLOCATED(BTB15))    DEALLOCATE(BTB15)
      IF (ALLOCATED(BTB16))    DEALLOCATE(BTB16)
      IF (ALLOCATED(BTB22))    DEALLOCATE(BTB22)
      IF (ALLOCATED(BTB23))    DEALLOCATE(BTB23)      
      IF (ALLOCATED(BTB24))    DEALLOCATE(BTB24)
      IF (ALLOCATED(BTB25))    DEALLOCATE(BTB25)
      IF (ALLOCATED(BTB26))    DEALLOCATE(BTB26)
      IF (ALLOCATED(BTB33))    DEALLOCATE(BTB33)
      IF (ALLOCATED(BTB34))    DEALLOCATE(BTB34)
      IF (ALLOCATED(BTB35))    DEALLOCATE(BTB35)
      IF (ALLOCATED(BTB36))    DEALLOCATE(BTB36)
      IF (ALLOCATED(BTB44))    DEALLOCATE(BTB44)
      IF (ALLOCATED(BTB45))    DEALLOCATE(BTB45)
      IF (ALLOCATED(BTB46))    DEALLOCATE(BTB46)
      IF (ALLOCATED(BTB55))    DEALLOCATE(BTB55)
      IF (ALLOCATED(BTB56))    DEALLOCATE(BTB56)
      IF (ALLOCATED(BTB66))    DEALLOCATE(BTB66)
      IF (ALLOCATED(POS1))     DEALLOCATE(POS1)
      IF (ALLOCATED(POS2))     DEALLOCATE(POS2) 
      IF (ALLOCATED(POS3))     DEALLOCATE(POS3)
      IF (ALLOCATED(POS4))     DEALLOCATE(POS4) 
      IF (ALLOCATED(POS5))     DEALLOCATE(POS5)
      IF (ALLOCATED(POS6))     DEALLOCATE(POS6) 
      IF (ALLOCATED(STI1))     DEALLOCATE(STI1)
      IF (ALLOCATED(STI2))     DEALLOCATE(STI2) 
      IF (ALLOCATED(STI3))     DEALLOCATE(STI3)
      IF (ALLOCATED(STI4))     DEALLOCATE(STI4) 
      IF (ALLOCATED(STI5))     DEALLOCATE(STI5)
      IF (ALLOCATED(STI6))     DEALLOCATE(STI6) 
      IF (ALLOCATED(STIFNLTH)) DEALLOCATE(STIFNLTH)
c-----------
      END
