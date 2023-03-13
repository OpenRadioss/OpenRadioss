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
Chd|  CDKFINT_REG                   source/elements/sh3n/coquedk/cdkfint_reg.F
Chd|-- called by -----------
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|====================================================================
      SUBROUTINE CDKFINT_REG(NLOC_DMG,VAR_REG ,THK     ,NEL     ,
     .                       OFF     ,AREA    ,NC1     ,NC2     ,
     .                       NC3     ,PX2     ,PY2     ,PX3     ,
     .                       PY3     ,KSI     ,ETA     ,BUFNL   ,
     .                       IMAT    ,NDDL    ,ITASK   ,NG      ,
     .                       DT2T    ,THK0    ,AREA0   )
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
#include      "vect01_c.inc"
#include      "com20_c.inc"
#include      "com08_c.inc"
#include      "scr02_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER                                    :: NEL,IMAT,NDDL,ITASK,NG
      INTEGER, DIMENSION(NEL)                    :: NC1,NC2,NC3
      my_real, DIMENSION(NEL,NDDL), INTENT(INOUT):: 
     .  VAR_REG
      my_real, DIMENSION(NEL), INTENT(IN)        :: 
     .  AREA,OFF,PX2,PY2,PX3,PY3,THK,THK0,AREA0
      TYPE(NLOCAL_STR_), TARGET                  :: NLOC_DMG 
      TYPE(BUF_NLOC_)  ,TARGET                   :: BUFNL
      my_real
     .  KSI,ETA
      my_real, INTENT(INOUT)                     ::
     .  DT2T
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,K,N1,N2,N3,L_NLOC,II,J,NDNOD
      my_real 
     .  L2,NTN_UNL1,NTN_UNL2,NTN_UNL3,XI,NTVAR,A,
     .  B1,B2,B3,ZETA,SSPNL,
     .  NTH1,NTH2,BTH1,BTH2,K1,K2,K12,
     .  NTN_VNL1,NTN_VNL2,NTN_VNL3,H(3),NTVAR1,NTVAR2,
     .  NTVAR3,LE_MAX,MAXSTIF,DTNOD,DT2P
      my_real, DIMENSION(:,:), ALLOCATABLE :: 
     .  F1,F2,F3,STI1,STI2,STI3
      my_real, DIMENSION(:) ,ALLOCATABLE   :: 
     . BTB11,BTB12,BTB13,BTB22,BTB23,BTB33,VOL
      INTEGER, DIMENSION(:), ALLOCATABLE   ::
     . POS1,POS2,POS3
      my_real, POINTER, DIMENSION(:)       :: 
     .  VNL,FNL,UNL,STIFNL,MASS,MASS0,VNL0
      my_real, POINTER, DIMENSION(:,:)     :: 
     .  MASSTH,UNLTH,VNLTH,FNLTH
      my_real, DIMENSION(:,:), ALLOCATABLE :: 
     .  STIFNLTH,DTN
      ! Safety coefficient for non-local stability vs mechanical stability
      ! (it has been slightly increased vs nloc_dmg_init.F)
      my_real, PARAMETER :: CSTA  = 40.0D0
      ! Coefficient for non-local stability to take into account damping
      my_real, PARAMETER :: CDAMP = 0.7D0
c-----------------------------------------------------------------------
c     VAR_REG :  variable a regulariser (local cumulated plastic strain)
c     NTVAR  =  NT * VAR_REG
C=======================================================================
      ! Recovering non-local parameters
      L2     = NLOC_DMG%LEN(IMAT)**2 ! Non-local internal length ** 2
      XI     = NLOC_DMG%DAMP(IMAT)   ! Non-local damping parameter
      ZETA   = NLOC_DMG%DENS(IMAT)   ! Non-local density
      SSPNL  = NLOC_DMG%SSPNL(IMAT)  ! Non-local sound speed
      L_NLOC = NLOC_DMG%L_NLOC       ! Length of non-local tables
      LE_MAX = NLOC_DMG%LE_MAX(IMAT) ! Maximal length of convergence
      H(1)   = ONE-ETA-KSI
      H(2)   = ETA
      H(3)   = KSI
      ! Allocation of elementary forces vectors
      ALLOCATE(F1(NEL,NDDL),F2(NEL,NDDL),F3(NEL,NDDL))
      ! Only for nodal timestep
      IF (NODADT > 0) THEN
        ! Nodal stiffness
        ALLOCATE(STI1(NEL,NDDL),STI2(NEL,NDDL),STI3(NEL,NDDL))
        ! Non-local mass
        MASS  => NLOC_DMG%MASS(1:L_NLOC)
        ! Initial non-local mass
        MASS0 => NLOC_DMG%MASS0(1:L_NLOC)
      ENDIF
      ALLOCATE(BTB11(NEL),BTB12(NEL),BTB13(NEL),BTB22(NEL),
     .         BTB23(NEL),BTB33(NEL),VOL(NEL),POS1(NEL),
     .         POS2(NEL),POS3(NEL))
      ! Recovering non-local data
      VNL  => NLOC_DMG%VNL(1:L_NLOC)     ! Non-local variable velocities
      VNL0 => NLOC_DMG%VNL_OLD(1:L_NLOC) ! Non-local variable velocities
      UNL  => NLOC_DMG%UNL(1:L_NLOC)     ! Non-local cumulated variable
c         
      !-----------------------------------------------------------------------
      ! Computation of the element volume and the BtB matrix product
      !-----------------------------------------------------------------------
      ! Loop over elements
# include "vectorize.inc"
      DO I=1,NEL
c
        ! Recovering the nodes of the triangle element
        N1  = NLOC_DMG%IDXI(NC1(I))
        N2  = NLOC_DMG%IDXI(NC2(I))
        N3  = NLOC_DMG%IDXI(NC3(I))
c
        ! Recovering the positions of the first d.o.fs of each nodes
        POS1(I) = NLOC_DMG%POSI(N1)
        POS2(I) = NLOC_DMG%POSI(N2)
        POS3(I) = NLOC_DMG%POSI(N3)  
c
        ! Computation of the element volume
        VOL(I)  = THIRD*AREA(I)*THK(I)
c 
        ! Computation of the product BtxB 
        BTB11(I) = (-PX3(I)-PX2(I))**2 + (-PY2(I)-PY3(I))**2
        BTB12(I) = (-PX3(I)-PX2(I))*PX2(I) + (-PY2(I)-PY3(I))*PY2(I)
        BTB13(I) = (-PX3(I)-PX2(I))*PX3(I) + (-PY2(I)-PY3(I))*PY3(I)
        BTB22(I) = PX2(I)**2 + PY2(I)**2
        BTB23(I) = PX2(I)*PX3(I) + PY2(I)*PY3(I)
        BTB33(I) = PX3(I)**2 + PY3(I)**2
c
      ENDDO   
c
      !-----------------------------------------------------------------------
      ! Pre-treatment non-local regularization in the thickness
      !-----------------------------------------------------------------------
      ! Only if NDDL > 1
      IF ((NDDL > 1).AND.(L2>ZERO)) THEN 
c
        ! Allocation of the velocities predictor
        IF (NDDL > 2) THEN 
          IF (NODADT > 0) THEN 
            ALLOCATE(STIFNLTH(NEL,NDDL+1))
            ALLOCATE(DTN(NEL,NDDL+1))
          ENDIF
          NDNOD = NDDL+1
        ELSE
          IF (NODADT > 0) THEN 
            ALLOCATE(STIFNLTH(NEL,NDDL))
            ALLOCATE(DTN(NEL,NDDL))
          ENDIF
          NDNOD = NDDL
        ENDIF
c 
        ! Pointing the non-local values in the thickness of the corresponding element
        MASSTH => BUFNL%MASSTH(1:NEL,1:NDNOD)
        UNLTH  => BUFNL%UNLTH(1:NEL,1:NDNOD)
        VNLTH  => BUFNL%VNLTH(1:NEL,1:NDNOD)
        FNLTH  => BUFNL%FNLTH(1:NEL,1:NDNOD)
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
          IF ((NDDL==2).AND.(K==2)) THEN 
            NTH1 = (Z0(K,NDDL) - ZTH(K,NDDL))   / (ZTH(K-1,NDDL) - ZTH(K,NDDL))
            NTH2 = (Z0(K,NDDL) - ZTH(K-1,NDDL)) / (ZTH(K,NDDL)   - ZTH(K-1,NDDL))
          ELSE 
            NTH1 = (Z0(K,NDDL) - ZTH(K+1,NDDL)) / (ZTH(K,NDDL)   - ZTH(K+1,NDDL))
            NTH2 = (Z0(K,NDDL) - ZTH(K,NDDL))   / (ZTH(K+1,NDDL) - ZTH(K,NDDL))
          ENDIF
c          
          ! Loop over elements
          DO I = 1,NEL
            ! Computation of B-matrix values
            IF ((NDDL==2).AND.(K==2)) THEN
              BTH1 = (ONE/(ZTH(K-1,NDDL)  - ZTH(K,NDDL)))*(ONE/THK(I))
              BTH2 = (ONE/(ZTH(K,NDDL)    - ZTH(K-1,NDDL)))*(ONE/THK(I))
            ELSE
              BTH1 = (ONE/(ZTH(K,NDDL)    - ZTH(K+1,NDDL)))*(ONE/THK(I))
              BTH2 = (ONE/(ZTH(K+1,NDDL)  - ZTH(K,NDDL)))*(ONE/THK(I))
            ENDIF               
c         
            ! Computation of the non-local K matrix
            K1   = L2*(BTH1**2)  + NTH1**2
            K12  = L2*(BTH1*BTH2)+ (NTH1*NTH2)
            K2   = L2*(BTH2**2)  + NTH2**2
c
            ! Computation of the non-local forces
            IF ((NDDL==2).AND.(K==2)) THEN
              FNLTH(I,K-1) = FNLTH(I,K-1) + (K1*UNLTH(I,K-1) + K12*UNLTH(I,K) 
     .                                    + XI*((NTH1**2)*VNLTH(I,K-1) 
     .                                    + (NTH1*NTH2)*VNLTH(I,K))
     .                                    - (NTH1*VAR_REG(I,K)))*VOL(I)*WF(K,NDDL)
              FNLTH(I,K)   = FNLTH(I,K)   + (K12*UNLTH(I,K-1) + K2*UNLTH(I,K)
     .                                    + XI*(NTH1*NTH2*VNLTH(I,K-1) 
     .                                    + (NTH2**2)*VNLTH(I,K))
     .                                    - NTH2*VAR_REG(I,K))*VOL(I)*WF(K,NDDL)
            ELSE
              FNLTH(I,K)   = FNLTH(I,K)   + (K1*UNLTH(I,K) + K12*UNLTH(I,K+1) 
     .                                    + XI*((NTH1**2)*VNLTH(I,K) 
     .                                    + (NTH1*NTH2)*VNLTH(I,K+1))
     .                                    - (NTH1*VAR_REG(I,K)))*VOL(I)*WF(K,NDDL)
              FNLTH(I,K+1) = FNLTH(I,K+1) + (K12*UNLTH(I,K) + K2*UNLTH(I,K+1)
     .                                    + XI*(NTH1*NTH2*VNLTH(I,K) 
     .                                    + (NTH2**2)*VNLTH(I,K+1))
     .                                    - NTH2*VAR_REG(I,K))*VOL(I)*WF(K,NDDL)
            ENDIF
c
            ! Computation of non-local nodal stiffness
            IF (NODADT > 0) THEN 
              IF ((NDDL==2).AND.(K==2)) THEN
                STIFNLTH(I,K-1) = STIFNLTH(I,K-1) + MAX(ABS(K1)+ABS(K12),ABS(K12)+ABS(K2))*VOL(I)*WF(K,NDDL)
                STIFNLTH(I,K)   = STIFNLTH(I,K)   + MAX(ABS(K1)+ABS(K12),ABS(K12)+ABS(K2))*VOL(I)*WF(K,NDDL)
              ELSE
                STIFNLTH(I,K)   = STIFNLTH(I,K)   + MAX(ABS(K1)+ABS(K12),ABS(K12)+ABS(K2))*VOL(I)*WF(K,NDDL) 
                STIFNLTH(I,K+1) = STIFNLTH(I,K+1) + MAX(ABS(K1)+ABS(K12),ABS(K12)+ABS(K2))*VOL(I)*WF(K,NDDL) 
              ENDIF              
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
          IF ((NDDL==2).AND.(K==2)) THEN
            NTH1 = (Z0(K,NDDL) - ZTH(K,NDDL))/(ZTH(K-1,NDDL)   - ZTH(K,NDDL))
            NTH2 = (Z0(K,NDDL) - ZTH(K-1,NDDL)) /(ZTH(K,NDDL) - ZTH(K-1,NDDL))
          ELSE
            NTH1 = (Z0(K,NDDL) - ZTH(K+1,NDDL))/(ZTH(K,NDDL)   - ZTH(K+1,NDDL))
            NTH2 = (Z0(K,NDDL) - ZTH(K,NDDL))  /(ZTH(K+1,NDDL) - ZTH(K,NDDL))
          ENDIF
          ! Loop over elements
          DO I = 1,NEL
            !Integration points non-local variables
            IF ((NDDL==2).AND.(K==2)) THEN
              VAR_REG(I,K) = NTH1*UNLTH(I,K-1) + NTH2*UNLTH(I,K)
            ELSE
              VAR_REG(I,K) = NTH1*UNLTH(I,K)   + NTH2*UNLTH(I,K+1)
            ENDIF
          ENDDO  
        ENDDO          
      ENDIF
c
      !-----------------------------------------------------------------------
      ! Computation of the elementary non-local forces
      !-----------------------------------------------------------------------
      ! Loop over the non-local d.o.fs
      DO K = 1, NDDL
c
        ! Loop over elements
# include "vectorize.inc"
        DO I = 1, NEL        
c        
          ! If the element is not broken, normal computation
          IF (OFF(I)/=ZERO) THEN 
            ! Computation of the product LEN**2 * BtxB
            B1 = (L2 * VOL(I)) * WF(K,NDDL)*(BTB11(I)*UNL(POS1(I)+K-1) + BTB12(I)*UNL(POS2(I)+K-1) 
     .                                 + BTB13(I)*UNL(POS3(I)+K-1))
c        
            B2 = (L2 * VOL(I)) * WF(K,NDDL)*(BTB12(I)*UNL(POS1(I)+K-1) + BTB22(I)*UNL(POS2(I)+K-1)
     .                                 + BTB23(I)*UNL(POS3(I)+K-1))
c        
            B3 = (L2 * VOL(I)) * WF(K,NDDL)*(BTB13(I)*UNL(POS1(I)+K-1) + BTB23(I)*UNL(POS2(I)+K-1) 
     .                                 + BTB33(I)*UNL(POS3(I)+K-1))
c
            ! Computing the product NtN*UNL
            NTN_UNL1 = H(1)*H(1)*UNL(POS1(I)+K-1) + H(1)*H(2)*UNL(POS2(I)+K-1) + H(1)*H(3)*UNL(POS3(I)+K-1)
            NTN_UNL2 = H(2)*H(1)*UNL(POS1(I)+K-1) + H(2)*H(2)*UNL(POS2(I)+K-1) + H(2)*H(3)*UNL(POS3(I)+K-1)
            NTN_UNL3 = H(3)*H(1)*UNL(POS1(I)+K-1) + H(3)*H(2)*UNL(POS2(I)+K-1) + H(3)*H(3)*UNL(POS3(I)+K-1)
c
            ! Computing the product XDAMP*NtN*VNL
            NTN_VNL1 = H(1)*H(1)*VNL(POS1(I)+K-1) + H(1)*H(2)*VNL(POS2(I)+K-1) + H(1)*H(3)*VNL(POS3(I)+K-1)
            IF (NODADT > 0) THEN 
              NTN_VNL1 = H(1)*H(1)*(SQRT(MASS(POS1(I)+K-1)/MASS0(POS1(I)+K-1)))*VNL(POS1(I)+K-1) + 
     .                   H(1)*H(2)*(SQRT(MASS(POS2(I)+K-1)/MASS0(POS2(I)+K-1)))*VNL(POS2(I)+K-1) + 
     .                   H(1)*H(3)*(SQRT(MASS(POS3(I)+K-1)/MASS0(POS3(I)+K-1)))*VNL(POS3(I)+K-1)
            ENDIF
            NTN_VNL2 = H(2)*H(1)*VNL(POS1(I)+K-1) + H(2)*H(2)*VNL(POS2(I)+K-1) + H(2)*H(3)*VNL(POS3(I)+K-1)
            IF (NODADT > 0) THEN 
              NTN_VNL2 = H(2)*H(1)*(SQRT(MASS(POS1(I)+K-1)/MASS0(POS1(I)+K-1)))*VNL(POS1(I)+K-1) + 
     .                   H(2)*H(2)*(SQRT(MASS(POS2(I)+K-1)/MASS0(POS2(I)+K-1)))*VNL(POS2(I)+K-1) + 
     .                   H(2)*H(3)*(SQRT(MASS(POS3(I)+K-1)/MASS0(POS3(I)+K-1)))*VNL(POS3(I)+K-1)
            ENDIF
            NTN_VNL3 = H(3)*H(1)*VNL(POS1(I)+K-1) + H(3)*H(2)*VNL(POS2(I)+K-1) + H(3)*H(3)*VNL(POS3(I)+K-1)
            IF (NODADT > 0) THEN 
              NTN_VNL3 = H(3)*H(1)*(SQRT(MASS(POS1(I)+K-1)/MASS0(POS1(I)+K-1)))*VNL(POS1(I)+K-1) + 
     .                   H(3)*H(2)*(SQRT(MASS(POS2(I)+K-1)/MASS0(POS2(I)+K-1)))*VNL(POS2(I)+K-1) + 
     .                   H(3)*H(3)*(SQRT(MASS(POS3(I)+K-1)/MASS0(POS3(I)+K-1)))*VNL(POS3(I)+K-1)
            ENDIF
c          
            ! Multiplication by the volume of the element
            NTN_UNL1 = NTN_UNL1 * VOL(I) * WF(K,NDDL)
            NTN_UNL2 = NTN_UNL2 * VOL(I) * WF(K,NDDL)
            NTN_UNL3 = NTN_UNL3 * VOL(I) * WF(K,NDDL)
            NTN_VNL1 = NTN_VNL1 * XI  * VOL(I) * WF(K,NDDL)
            NTN_VNL2 = NTN_VNL2 * XI  * VOL(I) * WF(K,NDDL)
            NTN_VNL3 = NTN_VNL3 * XI  * VOL(I) * WF(K,NDDL)
c
            ! Introducing the internal variable to be regularized
            NTVAR1  = VAR_REG(I,K)*H(1)*VOL(I)* WF(K,NDDL)
            NTVAR2  = VAR_REG(I,K)*H(2)*VOL(I)* WF(K,NDDL)
            NTVAR3  = VAR_REG(I,K)*H(3)*VOL(I)* WF(K,NDDL)
c
            ! Computing the elementary non-local forces
            F1(I,K) = NTN_UNL1 + NTN_VNL1 - NTVAR1 + B1
            F2(I,K) = NTN_UNL2 + NTN_VNL2 - NTVAR2 + B2
            F3(I,K) = NTN_UNL3 + NTN_VNL3 - NTVAR3 + B3
c
            ! Computing nodal equivalent stiffness
            IF (NODADT > 0) THEN 
              STI1(I,K) = (ABS(L2*BTB11(I) + H(1)*H(1)) + ABS(L2*BTB12(I) + H(1)*H(2)) + 
     .                     ABS(L2*BTB13(I) + H(1)*H(3)))* VOL(I) * WF(K,NDDL)
              STI2(I,K) = (ABS(L2*BTB12(I) + H(2)*H(1)) + ABS(L2*BTB22(I) + H(2)*H(2)) + 
     .                     ABS(L2*BTB23(I) + H(2)*H(3)))* VOL(I) * WF(K,NDDL)
              STI3(I,K) = (ABS(L2*BTB13(I) + H(3)*H(1)) + ABS(L2*BTB23(I) + H(3)*H(2)) + 
     .                     ABS(L2*BTB33(I) + H(3)*H(3)))* VOL(I) * WF(K,NDDL)
            ENDIF
c
          ! If the element is broken, the non-local wave is absorbed
          ELSE
            IF (NODADT > 0) THEN 
              ! Non-local absorbing forces
              F1(I,K) = SQRT(MASS(POS1(I)+K-1)/MASS0(POS1(I)+K-1))*H(1)*WF(K,NDDL)*ZETA*SSPNL*
     .                     HALF*(VNL(POS1(I)+K-1)+VNL0(POS1(I)+K-1))*SQRT((FOUR/SQRT(THREE))*(AREA0(I)))*THK0(I)
              F2(I,K) = SQRT(MASS(POS2(I)+K-1)/MASS0(POS2(I)+K-1))*H(2)*WF(K,NDDL)*ZETA*SSPNL*
     .                     HALF*(VNL(POS2(I)+K-1)+VNL0(POS2(I)+K-1))*SQRT((FOUR/SQRT(THREE))*(AREA0(I)))*THK0(I) 
              F3(I,K) = SQRT(MASS(POS3(I)+K-1)/MASS0(POS3(I)+K-1))*H(3)*WF(K,NDDL)*ZETA*SSPNL*
     .                     HALF*(VNL(POS3(I)+K-1)+VNL0(POS3(I)+K-1))*SQRT((FOUR/SQRT(THREE))*(AREA0(I)))*THK0(I)
              ! Computing nodal equivalent stiffness
              STI1(I,K) = EM20
              STI2(I,K) = EM20
              STI3(I,K) = EM20          
            ELSE
              ! Non-local absorbing forces
              F1(I,K) = H(1)*WF(K,NDDL)*ZETA*SSPNL*HALF*(VNL(POS1(I)+K-1)+VNL0(POS1(I)+K-1))*
     .                                    SQRT((FOUR/SQRT(THREE))*(AREA0(I)))*THK0(I)
              F2(I,K) = H(2)*WF(K,NDDL)*ZETA*SSPNL*HALF*(VNL(POS2(I)+K-1)+VNL0(POS2(I)+K-1))*
     .                                    SQRT((FOUR/SQRT(THREE))*(AREA0(I)))*THK0(I) 
              F3(I,K) = H(3)*WF(K,NDDL)*ZETA*SSPNL*HALF*(VNL(POS3(I)+K-1)+VNL0(POS3(I)+K-1))*
     .                                    SQRT((FOUR/SQRT(THREE))*(AREA0(I)))*THK0(I)
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
        FNL => NLOC_DMG%FNL(1:L_NLOC,ITASK+1)                        ! Non-local forces
        IF (NODADT > 0) STIFNL => NLOC_DMG%STIFNL(1:L_NLOC,ITASK+1)  ! Non-local equivalent nodal stiffness
        ! Loop over elements
        DO I=1,NEL
          ! Loop over non-local degrees of freedom
# include "vectorize.inc"
          DO K = 1,NDDL
            ! Assembling non-local forces
            FNL(POS1(I)+K-1) = FNL(POS1(I)+K-1) - F1(I,K)
            FNL(POS2(I)+K-1) = FNL(POS2(I)+K-1) - F2(I,K)
            FNL(POS3(I)+K-1) = FNL(POS3(I)+K-1) - F3(I,K)
            IF (NODADT > 0) THEN
              ! Spectral radius of stiffness matrix
              MAXSTIF = MAX(STI1(I,K),STI2(I,K),STI3(I,K))
              ! Computing nodal stiffness
              STIFNL(POS1(I)+K-1) = STIFNL(POS1(I)+K-1) + MAXSTIF
              STIFNL(POS2(I)+K-1) = STIFNL(POS2(I)+K-1) + MAXSTIF
              STIFNL(POS3(I)+K-1) = STIFNL(POS3(I)+K-1) + MAXSTIF
            ENDIF
          ENDDO
        ENDDO
c
      ! If PARITH/ON
      ELSE
        ! Loop over additional d.o.fs
        DO J = 1,NDDL
c
          ! Loop over elements
          DO I=1,NEL
            II  = I + NFT
c
            ! Spectral radius of stiffness matrix
            IF (NODADT > 0) THEN
              MAXSTIF = MAX(STI1(I,J),STI2(I,J),STI3(I,J))
            ENDIF
c
            K = NLOC_DMG%IADTG(1,II)
            IF (NG == 1) THEN 
              NLOC_DMG%FSKY(K,J) = -F1(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
            ELSE
              NLOC_DMG%FSKY(K,J) = NLOC_DMG%FSKY(K,J) - F1(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = NLOC_DMG%STSKY(K,J) + MAXSTIF
            ENDIF
c
            K = NLOC_DMG%IADTG(2,II)
            IF (NG == 1) THEN 
              NLOC_DMG%FSKY(K,J) = -F2(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
            ELSE
              NLOC_DMG%FSKY(K,J) = NLOC_DMG%FSKY(K,J) - F2(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = NLOC_DMG%STSKY(K,J) + MAXSTIF
            ENDIF
c
            K = NLOC_DMG%IADTG(3,II)
            IF (NG == 1) THEN 
              NLOC_DMG%FSKY(K,J) = -F3(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
            ELSE
              NLOC_DMG%FSKY(K,J) = NLOC_DMG%FSKY(K,J) - F3(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = NLOC_DMG%STSKY(K,J) + MAXSTIF
            ENDIF
c
          ENDDO
c
        ENDDO
c
      ENDIF
      ! Deallocation of tables
      IF (ALLOCATED(F1))       DEALLOCATE(F1)
      IF (ALLOCATED(F2))       DEALLOCATE(F2)
      IF (ALLOCATED(F3))       DEALLOCATE(F3)
      IF (ALLOCATED(STIFNLTH)) DEALLOCATE(STIFNLTH)
      IF (ALLOCATED(BTB11))    DEALLOCATE(BTB11)
      IF (ALLOCATED(BTB12))    DEALLOCATE(BTB12)
      IF (ALLOCATED(BTB13))    DEALLOCATE(BTB13)
      IF (ALLOCATED(BTB22))    DEALLOCATE(BTB22)
      IF (ALLOCATED(BTB23))    DEALLOCATE(BTB23)
      IF (ALLOCATED(BTB33))    DEALLOCATE(BTB33)
      IF (ALLOCATED(POS1))     DEALLOCATE(POS1)
      IF (ALLOCATED(POS2))     DEALLOCATE(POS2) 
      IF (ALLOCATED(POS3))     DEALLOCATE(POS3)
      IF (ALLOCATED(STI1))     DEALLOCATE(STI1)
      IF (ALLOCATED(STI2))     DEALLOCATE(STI2) 
      IF (ALLOCATED(STI3))     DEALLOCATE(STI3)
      IF (ALLOCATED(VOL))      DEALLOCATE(VOL)
c
      END
