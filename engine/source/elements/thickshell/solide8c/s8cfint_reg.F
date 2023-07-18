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
      SUBROUTINE S8CFINT_REG(
     1   NLOC_DMG,VAR_REG, NEL,     OFFG,
     2   VOL,     NC1,     NC2,     NC3,
     3   NC4,     NC5,     NC6,     NC7,
     4   NC8,     PX1,     PX2,     PX3,
     5   PX4,     PX5,     PX6,     PX7,
     6   PX8,     PY1,     PY2,     PY3,
     7   PY4,     PY5,     PY6,     PY7,
     8   PY8,     PZ1,     PZ2,     PZ3,
     9   PZ4,     PZ5,     PZ6,     PZ7,
     A   PZ8,     IMAT,    ITASK,   DT2T,
     B   VOL0,    NFT,     NLAY,    NPTR,
     C   NPTS,    IR,      IS,      WS,
     D   AS  ,    ICR,     ICS,     ICT,
     E   BUFNLTS, AREA,    NODADT,  DT1, 
     F   DT12,    DTFAC1,  DTMIN1,  IDTMIN)
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER, INTENT(IN) :: NFT,NLAY,NPTR,NPTS,IR,IS,
     .                       NEL,IMAT,ITASK,ICR,ICS,ICT
      INTEGER, DIMENSION(NEL), INTENT(IN) :: NC1,NC2,NC3,NC4,NC5,NC6,NC7,NC8
      my_real, DIMENSION(NEL), INTENT(IN) :: 
     .   OFFG
      my_real, INTENT(INOUT) ::
     .   DT2T
      my_real, DIMENSION(NEL), INTENT(IN) :: 
     .   VOL,VOL0,AREA
      TYPE(NLOCAL_STR_), INTENT(INOUT) :: NLOC_DMG 
      my_real, DIMENSION(9,9), INTENT(IN) :: 
     .   WS,AS
      my_real, DIMENSION(NEL,NPTR*NPTS*NLAY), INTENT(INOUT) :: 
     .   VAR_REG,PX1,PX2,PX3,PX4,PX5,PX6,PX7,PX8,
     .   PY1,PY2,PY3,PY4,PY5,PY6,PY7,PY8,PZ1,PZ2,
     .   PZ3,PZ4,PZ5,PZ6,PZ7,PZ8
      TYPE(BUF_NLOCTS_), INTENT(INOUT), TARGET :: BUFNLTS
      my_real, INTENT(IN) ::
     .   DT1,DT12
      INTEGER, INTENT(IN) :: NODADT,IDTMIN(102)
      my_real, INTENT(IN) :: DTFAC1(102),DTMIN1(102)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,II,K,NNOD,N1,N2,N3,N4,N5,N6,N7,N8,
     .        L_NLOC,IP,NDDL,NDNOD
      my_real 
     . L2,XI,B1,B2,B3,B4,B5,B6,B7,B8,H(8),
     . A1,A2,A3,A4,A5,A6,A7,A8,
     . C1,C2,C3,C4,C5,C6,C7,C8,
     . ZETA,SSPNL,DTNL,LE_MAX,MAXSTIF,
     . ZR,ZS,ZT,PR(8),PS(8),PT(8),WI,
     . BTH1,BTH2,NTH1,NTH2,DT2P,DTNOD,
     . K1,K2,K12,DTNL_TH
      my_real, DIMENSION(NEL) :: 
     . LC,THK,LTHK
      my_real, DIMENSION(NEL,NLAY) :: 
     . F1,F2,F3,F4,F5,F6,F7,F8
      my_real, DIMENSION(:,:) ,ALLOCATABLE ::
     . STI1,STI2,STI3,STI4,STI5,STI6,STI7,STI8
      my_real, DIMENSION(:) ,ALLOCATABLE   :: 
     . BTB11,BTB12,BTB13,BTB14,BTB15,BTB16,BTB17,BTB18,
     . BTB22,BTB23,BTB24,BTB25,BTB26,BTB27,BTB28,BTB33,
     . BTB34,BTB35,BTB36,BTB37,BTB38,BTB44,BTB45,BTB46,
     . BTB47,BTB48,BTB55,BTB56,BTB57,BTB58,BTB66,BTB67,
     . BTB68,BTB77,BTB78,BTB88
      INTEGER, DIMENSION(:), ALLOCATABLE   ::
     . POS1,POS2,POS3,POS4,POS5,POS6,POS7,POS8
      my_real, DIMENSION(:,:), ALLOCATABLE :: 
     . STIFNLTH,DTN
      ! Safety coefficient for non-local stability vs mechanical stability
      ! (it has been slightly increased vs nloc_dmg_init.F)
      my_real, PARAMETER :: CSTA  = 40.0D0
      ! Coefficient for non-local stability to take into account damping
      my_real, PARAMETER :: CDAMP = 0.7D0
      my_real
     . ZTH(10,9)      
      ! Position of nodes in the thickshell thickness
      DATA ZTH / 
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
      LC(1:NEL) = ZERO
      ALLOCATE(
     .  BTB11(NEL),BTB12(NEL),BTB13(NEL),BTB14(NEL),BTB15(NEL),
     .  BTB16(NEL),BTB17(NEL),BTB18(NEL),BTB22(NEL),BTB23(NEL),BTB24(NEL),
     .  BTB25(NEL),BTB26(NEL),BTB27(NEL),BTB28(NEL),BTB33(NEL),BTB34(NEL),
     .  BTB35(NEL),BTB36(NEL),BTB37(NEL),BTB38(NEL),BTB44(NEL),BTB45(NEL),
     .  BTB46(NEL),BTB47(NEL),BTB48(NEL),BTB55(NEL),BTB56(NEL),BTB57(NEL),
     .  BTB58(NEL),BTB66(NEL),BTB67(NEL),BTB68(NEL),BTB77(NEL),BTB78(NEL),
     .  BTB88(NEL),POS1(NEL),POS2(NEL),POS3(NEL),POS4(NEL),POS5(NEL),
     .  POS6(NEL),POS7(NEL),POS8(NEL))
      ! For nodal timestep
      IF (NODADT > 0) THEN
        ! Non-local nodal stifness
        ALLOCATE(STI1(NEL,NLAY),STI2(NEL,NLAY),STI3(NEL,NLAY),STI4(NEL,NLAY),
     .           STI5(NEL,NLAY),STI6(NEL,NLAY),STI7(NEL,NLAY),STI8(NEL,NLAY))
      ENDIF
c
      !-----------------------------------------------------------------------
      ! Computation of the position of the non-local d.o.fs
      !-----------------------------------------------------------------------
      ! Loop over elements
#include "vectorize.inc"
      DO I=1,NEL
c
        ! Number of the element nodes
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
          LTHK(I) = (ZTH(NLAY+1,NLAY)-ZTH(NLAY,NLAY))*THK(I)*HALF
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
        DO K = 1,NDNOD
          DO I = 1,NEL
            ! Resetting non-local forces
            BUFNLTS%FNLTH(I,K) = ZERO
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
          ! Integration point number
          IP = IR + ((IS-1) + (K-1)*NPTS)*NPTR 
c        
          ! Computation of shape functions value
          NTH1 = (AS(K,NDDL)   - ZTH(K+1,NDDL)) / 
     .           (ZTH(K,NDDL)  - ZTH(K+1,NDDL))
          NTH2 = (AS(K,NDDL)   - ZTH(K,NDDL))   / 
     .           (ZTH(K+1,NDDL) - ZTH(K,NDDL))
c
          ! Weight of integration
          WI = HALF*WS(IR,NPTR)*HALF*WS(IS,NPTS)*HALF*WS(K,NLAY)   
c          
          ! Loop over elements
          DO I = 1,NEL
c
            ! Computation of B-matrix values
            BTH1 = (ONE/(ZTH(K,NDDL)   - ZTH(K+1,NDDL)))*(TWO/THK(I))
            BTH2 = (ONE/(ZTH(K+1,NDDL) - ZTH(K,NDDL)))*(TWO/THK(I))   
c         
            ! Computation of the non-local K matrix
            K1   = L2*(BTH1**2)  + NTH1**2
            K12  = L2*(BTH1*BTH2)+ (NTH1*NTH2)
            K2   = L2*(BTH2**2)  + NTH2**2
c
            ! Computation of the non-local forces
            BUFNLTS%FNLTH(I,K) = BUFNLTS%FNLTH(I,K) 
     .                         + (K1*BUFNLTS%UNLTH(I,K) + K12*BUFNLTS%UNLTH(I,K+1) 
     .                         + XI*((NTH1**2)*BUFNLTS%VNLTH(I,K) 
     .                         + (NTH1*NTH2)*BUFNLTS%VNLTH(I,K+1))
     .                         - (NTH1*VAR_REG(I,IP)))*WI*VOL(I)  
            BUFNLTS%FNLTH(I,K+1) = BUFNLTS%FNLTH(I,K+1) 
     .                         + (K12*BUFNLTS%UNLTH(I,K) + K2*BUFNLTS%UNLTH(I,K+1)
     .                         + XI*(NTH1*NTH2*BUFNLTS%VNLTH(I,K) 
     .                         + (NTH2**2)*BUFNLTS%VNLTH(I,K+1))
     .                         - NTH2*VAR_REG(I,IP))*WI*VOL(I)  
c
            ! Computation of non-local nodal stiffness
            IF (NODADT > 0) THEN 
              STIFNLTH(I,K)   = STIFNLTH(I,K)   + MAX(ABS(K1)+ABS(K12),ABS(K12)+ABS(K2))*WI*VOL(I)
              STIFNLTH(I,K+1) = STIFNLTH(I,K+1) + MAX(ABS(K1)+ABS(K12),ABS(K12)+ABS(K2))*WI*VOL(I)            
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
              DTN(I,K) = DTFAC1(11)*CDAMP*SQRT(TWO*BUFNLTS%MASSTH(I,K)/MAX(STIFNLTH(I,K),EM20)) 
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
                    DT2P = DTMIN1(11)/(DTFAC1(11)*CDAMP)
                    BUFNLTS%MASSTH(I,K) = MAX(BUFNLTS%MASSTH(I,K),CSTA*HALF*STIFNLTH(I,K)*DT2P*DT2P*ONEP00001)
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
            BUFNLTS%VNLTH(I,K) = BUFNLTS%VNLTH(I,K) - (BUFNLTS%FNLTH(I,K)/BUFNLTS%MASSTH(I,K))*DT12
          ENDDO
        ENDDO
c          
        DO K = 1,NDNOD
          DO I = 1,NEL
            ! Computing the non-local in-thickness cumulated values
            BUFNLTS%UNLTH(I,K) = BUFNLTS%UNLTH(I,K) + BUFNLTS%VNLTH(I,K)*DT1
          ENDDO
        ENDDO
c
        ! Transfert at the integration point
        DO K = 1, NDDL
          ! Integration point number
          IP = IR + ((IS-1) + (K-1)*NPTS)*NPTR 
          !Computation of shape functions value
          NTH1 = (AS(K,NDDL)    - ZTH(K+1,NDDL))/
     .           (ZTH(K,NDDL)   - ZTH(K+1,NDDL))
          NTH2 = (AS(K,NDDL)    - ZTH(K,NDDL))/
     .           (ZTH(K+1,NDDL) - ZTH(K,NDDL))
          ! Loop over elements
          DO I = 1,NEL
            !Integration points non-local variables
            VAR_REG(I,IP) = NTH1*BUFNLTS%UNLTH(I,K) + NTH2*BUFNLTS%UNLTH(I,K+1)
          ENDDO  
        ENDDO
      ENDIF
c            
      !-----------------------------------------------------------------------
      ! Computation of non-local forces
      !-----------------------------------------------------------------------
      ! Loop over layers
      DO K = 1,NLAY
c
        ! Integration points positions and weight
        IF (ICT == 1) THEN
          ZR = AS(IR,NPTR)
          ZS = AS(IS,NPTS)
          ZT = AS(K,NLAY)
        ELSEIF (ICS == 1) THEN
          ZR = AS(IR,NPTR)
          ZS = AS(K,NLAY)
          ZT = AS(IS,NPTS)
        ELSEIF (ICR == 1) THEN
          ZR = AS(K,NLAY)
          ZS = AS(IR,NPTR)
          ZT = AS(IS,NPTS)
        ENDIF 
        WI = HALF*WS(IR,NPTR)*HALF*WS(IS,NPTS)*HALF*WS(K,NLAY)
c
        ! Shape functions value  
        CALL BASIS8 (ZR,ZS,ZT,H,PR,PS,PT)
c
        ! Integration point number
        IP = IR + ((IS-1) + (K-1)*NPTS)*NPTR
c
        ! Loop over elements
#include "vectorize.inc"
        DO I=1,NEL
c
          ! If the element is not broken, normal computation
          IF (OFFG(I)/=ZERO) THEN
c
            ! Computation of the product BtxB 
            BTB11(I) = PX1(I,IP)**2 + PY1(I,IP)**2 + PZ1(I,IP)**2
            BTB12(I) = PX1(I,IP)*PX2(I,IP) + PY1(I,IP)*PY2(I,IP) + PZ1(I,IP)*PZ2(I,IP)
            BTB13(I) = PX1(I,IP)*PX3(I,IP) + PY1(I,IP)*PY3(I,IP) + PZ1(I,IP)*PZ3(I,IP)
            BTB14(I) = PX1(I,IP)*PX4(I,IP) + PY1(I,IP)*PY4(I,IP) + PZ1(I,IP)*PZ4(I,IP)
            BTB15(I) = PX1(I,IP)*PX5(I,IP) + PY1(I,IP)*PY5(I,IP) + PZ1(I,IP)*PZ5(I,IP)
            BTB16(I) = PX1(I,IP)*PX6(I,IP) + PY1(I,IP)*PY6(I,IP) + PZ1(I,IP)*PZ6(I,IP)
            BTB17(I) = PX1(I,IP)*PX7(I,IP) + PY1(I,IP)*PY7(I,IP) + PZ1(I,IP)*PZ7(I,IP)
            BTB18(I) = PX1(I,IP)*PX8(I,IP) + PY1(I,IP)*PY8(I,IP) + PZ1(I,IP)*PZ8(I,IP)
            BTB22(I) = PX2(I,IP)**2 + PY2(I,IP)**2 + PZ2(I,IP)**2
            BTB23(I) = PX2(I,IP)*PX3(I,IP) + PY2(I,IP)*PY3(I,IP) + PZ2(I,IP)*PZ3(I,IP)
            BTB24(I) = PX2(I,IP)*PX4(I,IP) + PY2(I,IP)*PY4(I,IP) + PZ2(I,IP)*PZ4(I,IP)
            BTB25(I) = PX2(I,IP)*PX5(I,IP) + PY2(I,IP)*PY5(I,IP) + PZ2(I,IP)*PZ5(I,IP)
            BTB26(I) = PX2(I,IP)*PX6(I,IP) + PY2(I,IP)*PY6(I,IP) + PZ2(I,IP)*PZ6(I,IP)
            BTB27(I) = PX2(I,IP)*PX7(I,IP) + PY2(I,IP)*PY7(I,IP) + PZ2(I,IP)*PZ7(I,IP)
            BTB28(I) = PX2(I,IP)*PX8(I,IP) + PY2(I,IP)*PY8(I,IP) + PZ2(I,IP)*PZ8(I,IP)
            BTB33(I) = PX3(I,IP)**2 + PY3(I,IP)**2 + PZ3(I,IP)**2
            BTB34(I) = PX3(I,IP)*PX4(I,IP) + PY3(I,IP)*PY4(I,IP) + PZ3(I,IP)*PZ4(I,IP)
            BTB35(I) = PX3(I,IP)*PX5(I,IP) + PY3(I,IP)*PY5(I,IP) + PZ3(I,IP)*PZ5(I,IP)
            BTB36(I) = PX3(I,IP)*PX6(I,IP) + PY3(I,IP)*PY6(I,IP) + PZ3(I,IP)*PZ6(I,IP)
            BTB37(I) = PX3(I,IP)*PX7(I,IP) + PY3(I,IP)*PY7(I,IP) + PZ3(I,IP)*PZ7(I,IP)
            BTB38(I) = PX3(I,IP)*PX8(I,IP) + PY3(I,IP)*PY8(I,IP) + PZ3(I,IP)*PZ8(I,IP)
            BTB44(I) = PX4(I,IP)**2 + PY4(I,IP)**2 + PZ4(I,IP)**2
            BTB45(I) = PX4(I,IP)*PX5(I,IP) + PY4(I,IP)*PY5(I,IP) + PZ4(I,IP)*PZ5(I,IP)
            BTB46(I) = PX4(I,IP)*PX6(I,IP) + PY4(I,IP)*PY6(I,IP) + PZ4(I,IP)*PZ6(I,IP)
            BTB47(I) = PX4(I,IP)*PX7(I,IP) + PY4(I,IP)*PY7(I,IP) + PZ4(I,IP)*PZ7(I,IP)
            BTB48(I) = PX4(I,IP)*PX8(I,IP) + PY4(I,IP)*PY8(I,IP) + PZ4(I,IP)*PZ8(I,IP)
            BTB55(I) = PX5(I,IP)**2 + PY5(I,IP)**2 + PZ5(I,IP)**2
            BTB56(I) = PX5(I,IP)*PX6(I,IP) + PY5(I,IP)*PY6(I,IP) + PZ5(I,IP)*PZ6(I,IP)
            BTB57(I) = PX5(I,IP)*PX7(I,IP) + PY5(I,IP)*PY7(I,IP) + PZ5(I,IP)*PZ7(I,IP)
            BTB58(I) = PX5(I,IP)*PX8(I,IP) + PY5(I,IP)*PY8(I,IP) + PZ5(I,IP)*PZ8(I,IP)
            BTB66(I) = PX6(I,IP)**2 + PY6(I,IP)**2 + PZ6(I,IP)**2
            BTB67(I) = PX6(I,IP)*PX7(I,IP) + PY6(I,IP)*PY7(I,IP) + PZ6(I,IP)*PZ7(I,IP)
            BTB68(I) = PX6(I,IP)*PX8(I,IP) + PY6(I,IP)*PY8(I,IP) + PZ6(I,IP)*PZ8(I,IP)
            BTB77(I) = PX7(I,IP)**2 + PY7(I,IP)**2 + PZ7(I,IP)**2
            BTB78(I) = PX7(I,IP)*PX8(I,IP) + PY7(I,IP)*PY8(I,IP) + PZ7(I,IP)*PZ8(I,IP)
            BTB88(I) = PX8(I,IP)**2 + PY8(I,IP)**2 + PZ8(I,IP)**2
c  
            ! Computation of LEN**2*BTB*Unl product and NTN*UNL, NTN*VNL
            A1 = VOL(I) * WI * (H(1)*H(1)*NLOC_DMG%UNL(POS1(I)+K-1) + H(1)*H(2)*NLOC_DMG%UNL(POS2(I)+K-1) + H(1)*H(3)*NLOC_DMG%UNL(POS3(I)+K-1)
     .                        + H(1)*H(4)*NLOC_DMG%UNL(POS4(I)+K-1) + H(1)*H(5)*NLOC_DMG%UNL(POS5(I)+K-1) + H(1)*H(6)*NLOC_DMG%UNL(POS6(I)+K-1)
     .                        + H(1)*H(7)*NLOC_DMG%UNL(POS7(I)+K-1) + H(1)*H(8)*NLOC_DMG%UNL(POS8(I)+K-1))
c
            IF (NODADT == 0) THEN 
              A1 = A1 + VOL(I) * WI * XI * (H(1)*H(1)*NLOC_DMG%VNL(POS1(I)+K-1) + H(1)*H(2)*NLOC_DMG%VNL(POS2(I)+K-1) + H(1)*H(3)*NLOC_DMG%VNL(POS3(I)+K-1)
     .                                    + H(1)*H(4)*NLOC_DMG%VNL(POS4(I)+K-1) + H(1)*H(5)*NLOC_DMG%VNL(POS5(I)+K-1) + H(1)*H(6)*NLOC_DMG%VNL(POS6(I)+K-1)
     .                                    + H(1)*H(7)*NLOC_DMG%VNL(POS7(I)+K-1) + H(1)*H(8)*NLOC_DMG%VNL(POS8(I)+K-1))
            ELSE
              A1 = A1 + VOL(I) * WI * XI * (H(1)*H(1)*(SQRT(NLOC_DMG%MASS(POS1(I)+K-1)/NLOC_DMG%MASS0(POS1(I)+K-1)))*NLOC_DMG%VNL(POS1(I)+K-1) + 
     .                                      H(1)*H(2)*(SQRT(NLOC_DMG%MASS(POS2(I)+K-1)/NLOC_DMG%MASS0(POS2(I)+K-1)))*NLOC_DMG%VNL(POS2(I)+K-1) + 
     .                                      H(1)*H(3)*(SQRT(NLOC_DMG%MASS(POS3(I)+K-1)/NLOC_DMG%MASS0(POS3(I)+K-1)))*NLOC_DMG%VNL(POS3(I)+K-1) + 
     .                                      H(1)*H(4)*(SQRT(NLOC_DMG%MASS(POS4(I)+K-1)/NLOC_DMG%MASS0(POS4(I)+K-1)))*NLOC_DMG%VNL(POS4(I)+K-1) + 
     .                                      H(1)*H(5)*(SQRT(NLOC_DMG%MASS(POS5(I)+K-1)/NLOC_DMG%MASS0(POS5(I)+K-1)))*NLOC_DMG%VNL(POS5(I)+K-1) + 
     .                                      H(1)*H(6)*(SQRT(NLOC_DMG%MASS(POS6(I)+K-1)/NLOC_DMG%MASS0(POS6(I)+K-1)))*NLOC_DMG%VNL(POS6(I)+K-1) + 
     .                                      H(1)*H(7)*(SQRT(NLOC_DMG%MASS(POS7(I)+K-1)/NLOC_DMG%MASS0(POS7(I)+K-1)))*NLOC_DMG%VNL(POS7(I)+K-1) + 
     .                                      H(1)*H(8)*(SQRT(NLOC_DMG%MASS(POS8(I)+K-1)/NLOC_DMG%MASS0(POS8(I)+K-1)))*NLOC_DMG%VNL(POS8(I)+K-1))
            ENDIF
c        
            B1 = L2 * VOL(I) * WI * ( BTB11(I)*NLOC_DMG%UNL(POS1(I)+K-1) + BTB12(I)*NLOC_DMG%UNL(POS2(I)+K-1) + BTB13(I)*NLOC_DMG%UNL(POS3(I)+K-1)
     .                              + BTB14(I)*NLOC_DMG%UNL(POS4(I)+K-1) + BTB15(I)*NLOC_DMG%UNL(POS5(I)+K-1) + BTB16(I)*NLOC_DMG%UNL(POS6(I)+K-1)
     .                              + BTB17(I)*NLOC_DMG%UNL(POS7(I)+K-1) + BTB18(I)*NLOC_DMG%UNL(POS8(I)+K-1))
c     
            C1 = VOL(I) * WI * H(1) * VAR_REG(I,IP)     
c     
            A2 = VOL(I) * WI * (H(2)*H(1)*NLOC_DMG%UNL(POS1(I)+K-1) + H(2)*H(2)*NLOC_DMG%UNL(POS2(I)+K-1) + H(2)*H(3)*NLOC_DMG%UNL(POS3(I)+K-1)
     .                        + H(2)*H(4)*NLOC_DMG%UNL(POS4(I)+K-1) + H(2)*H(5)*NLOC_DMG%UNL(POS5(I)+K-1) + H(2)*H(6)*NLOC_DMG%UNL(POS6(I)+K-1)
     .                        + H(2)*H(7)*NLOC_DMG%UNL(POS7(I)+K-1) + H(2)*H(8)*NLOC_DMG%UNL(POS8(I)+K-1))
c
            IF (NODADT == 0) THEN 
              A2 = A2 + VOL(I) * WI * XI * (H(2)*H(1)*NLOC_DMG%VNL(POS1(I)+K-1) + H(2)*H(2)*NLOC_DMG%VNL(POS2(I)+K-1) + H(2)*H(3)*NLOC_DMG%VNL(POS3(I)+K-1)
     .                                    + H(2)*H(4)*NLOC_DMG%VNL(POS4(I)+K-1) + H(2)*H(5)*NLOC_DMG%VNL(POS5(I)+K-1) + H(2)*H(6)*NLOC_DMG%VNL(POS6(I)+K-1)
     .                                    + H(2)*H(7)*NLOC_DMG%VNL(POS7(I)+K-1) + H(2)*H(8)*NLOC_DMG%VNL(POS8(I)+K-1))
            ELSE
              A2 = A2 + VOL(I) * WI * XI * (H(2)*H(1)*(SQRT(NLOC_DMG%MASS(POS1(I)+K-1)/NLOC_DMG%MASS0(POS1(I)+K-1)))*NLOC_DMG%VNL(POS1(I)+K-1) + 
     .                                      H(2)*H(2)*(SQRT(NLOC_DMG%MASS(POS2(I)+K-1)/NLOC_DMG%MASS0(POS2(I)+K-1)))*NLOC_DMG%VNL(POS2(I)+K-1) + 
     .                                      H(2)*H(3)*(SQRT(NLOC_DMG%MASS(POS3(I)+K-1)/NLOC_DMG%MASS0(POS3(I)+K-1)))*NLOC_DMG%VNL(POS3(I)+K-1) + 
     .                                      H(2)*H(4)*(SQRT(NLOC_DMG%MASS(POS4(I)+K-1)/NLOC_DMG%MASS0(POS4(I)+K-1)))*NLOC_DMG%VNL(POS4(I)+K-1) + 
     .                                      H(2)*H(5)*(SQRT(NLOC_DMG%MASS(POS5(I)+K-1)/NLOC_DMG%MASS0(POS5(I)+K-1)))*NLOC_DMG%VNL(POS5(I)+K-1) + 
     .                                      H(2)*H(6)*(SQRT(NLOC_DMG%MASS(POS6(I)+K-1)/NLOC_DMG%MASS0(POS6(I)+K-1)))*NLOC_DMG%VNL(POS6(I)+K-1) + 
     .                                      H(2)*H(7)*(SQRT(NLOC_DMG%MASS(POS7(I)+K-1)/NLOC_DMG%MASS0(POS7(I)+K-1)))*NLOC_DMG%VNL(POS7(I)+K-1) + 
     .                                      H(2)*H(8)*(SQRT(NLOC_DMG%MASS(POS8(I)+K-1)/NLOC_DMG%MASS0(POS8(I)+K-1)))*NLOC_DMG%VNL(POS8(I)+K-1))
            ENDIF
c        
            B2 = L2 * VOL(I) * WI * ( BTB12(I)*NLOC_DMG%UNL(POS1(I)+K-1) + BTB22(I)*NLOC_DMG%UNL(POS2(I)+K-1) + BTB23(I)*NLOC_DMG%UNL(POS3(I)+K-1)
     .                              + BTB24(I)*NLOC_DMG%UNL(POS4(I)+K-1) + BTB25(I)*NLOC_DMG%UNL(POS5(I)+K-1) + BTB26(I)*NLOC_DMG%UNL(POS6(I)+K-1)
     .                              + BTB27(I)*NLOC_DMG%UNL(POS7(I)+K-1) + BTB28(I)*NLOC_DMG%UNL(POS8(I)+K-1))
c     
            C2 = VOL(I) * WI * H(2) * VAR_REG(I,IP) 
c     
            A3 = VOL(I) * WI * (H(3)*H(1)*NLOC_DMG%UNL(POS1(I)+K-1) + H(3)*H(2)*NLOC_DMG%UNL(POS2(I)+K-1) + H(3)*H(3)*NLOC_DMG%UNL(POS3(I)+K-1)
     .                        + H(3)*H(4)*NLOC_DMG%UNL(POS4(I)+K-1) + H(3)*H(5)*NLOC_DMG%UNL(POS5(I)+K-1) + H(3)*H(6)*NLOC_DMG%UNL(POS6(I)+K-1)
     .                        + H(3)*H(7)*NLOC_DMG%UNL(POS7(I)+K-1) + H(3)*H(8)*NLOC_DMG%UNL(POS8(I)+K-1))
c
            IF (NODADT == 0) THEN 
              A3 = A3 + VOL(I) * WI * XI * (H(3)*H(1)*NLOC_DMG%VNL(POS1(I)+K-1) + H(3)*H(2)*NLOC_DMG%VNL(POS2(I)+K-1) + H(3)*H(3)*NLOC_DMG%VNL(POS3(I)+K-1)
     .                                    + H(3)*H(4)*NLOC_DMG%VNL(POS4(I)+K-1) + H(3)*H(5)*NLOC_DMG%VNL(POS5(I)+K-1) + H(3)*H(6)*NLOC_DMG%VNL(POS6(I)+K-1)
     .                                    + H(3)*H(7)*NLOC_DMG%VNL(POS7(I)+K-1) + H(3)*H(8)*NLOC_DMG%VNL(POS8(I)+K-1)) 
            ELSE
              A3 = A3 + VOL(I) * WI * XI * (H(3)*H(1)*(SQRT(NLOC_DMG%MASS(POS1(I)+K-1)/NLOC_DMG%MASS0(POS1(I)+K-1)))*NLOC_DMG%VNL(POS1(I)+K-1) + 
     .                                      H(3)*H(2)*(SQRT(NLOC_DMG%MASS(POS2(I)+K-1)/NLOC_DMG%MASS0(POS2(I)+K-1)))*NLOC_DMG%VNL(POS2(I)+K-1) + 
     .                                      H(3)*H(3)*(SQRT(NLOC_DMG%MASS(POS3(I)+K-1)/NLOC_DMG%MASS0(POS3(I)+K-1)))*NLOC_DMG%VNL(POS3(I)+K-1) + 
     .                                      H(3)*H(4)*(SQRT(NLOC_DMG%MASS(POS4(I)+K-1)/NLOC_DMG%MASS0(POS4(I)+K-1)))*NLOC_DMG%VNL(POS4(I)+K-1) + 
     .                                      H(3)*H(5)*(SQRT(NLOC_DMG%MASS(POS5(I)+K-1)/NLOC_DMG%MASS0(POS5(I)+K-1)))*NLOC_DMG%VNL(POS5(I)+K-1) + 
     .                                      H(3)*H(6)*(SQRT(NLOC_DMG%MASS(POS6(I)+K-1)/NLOC_DMG%MASS0(POS6(I)+K-1)))*NLOC_DMG%VNL(POS6(I)+K-1) + 
     .                                      H(3)*H(7)*(SQRT(NLOC_DMG%MASS(POS7(I)+K-1)/NLOC_DMG%MASS0(POS7(I)+K-1)))*NLOC_DMG%VNL(POS7(I)+K-1) + 
     .                                      H(3)*H(8)*(SQRT(NLOC_DMG%MASS(POS8(I)+K-1)/NLOC_DMG%MASS0(POS8(I)+K-1)))*NLOC_DMG%VNL(POS8(I)+K-1))
            ENDIF
c        
            B3 = L2 * VOL(I) * WI * ( BTB13(I)*NLOC_DMG%UNL(POS1(I)+K-1) + BTB23(I)*NLOC_DMG%UNL(POS2(I)+K-1) + BTB33(I)*NLOC_DMG%UNL(POS3(I)+K-1)
     .                              + BTB34(I)*NLOC_DMG%UNL(POS4(I)+K-1) + BTB35(I)*NLOC_DMG%UNL(POS5(I)+K-1) + BTB36(I)*NLOC_DMG%UNL(POS6(I)+K-1)
     .                              + BTB37(I)*NLOC_DMG%UNL(POS7(I)+K-1) + BTB38(I)*NLOC_DMG%UNL(POS8(I)+K-1))
c     
            C3 = VOL(I) * WI * H(3) * VAR_REG(I,IP) 
c     
            A4 = VOL(I) * WI * (H(4)*H(1)*NLOC_DMG%UNL(POS1(I)+K-1) + H(4)*H(2)*NLOC_DMG%UNL(POS2(I)+K-1) + H(4)*H(3)*NLOC_DMG%UNL(POS3(I)+K-1)
     .                        + H(4)*H(4)*NLOC_DMG%UNL(POS4(I)+K-1) + H(4)*H(5)*NLOC_DMG%UNL(POS5(I)+K-1) + H(4)*H(6)*NLOC_DMG%UNL(POS6(I)+K-1)
     .                        + H(4)*H(7)*NLOC_DMG%UNL(POS7(I)+K-1) + H(4)*H(8)*NLOC_DMG%UNL(POS8(I)+K-1))
c
            IF (NODADT == 0) THEN 
              A4 = A4 + VOL(I) * WI * XI * (H(4)*H(1)*NLOC_DMG%VNL(POS1(I)+K-1) + H(4)*H(2)*NLOC_DMG%VNL(POS2(I)+K-1) + H(4)*H(3)*NLOC_DMG%VNL(POS3(I)+K-1)
     .                                    + H(4)*H(4)*NLOC_DMG%VNL(POS4(I)+K-1) + H(4)*H(5)*NLOC_DMG%VNL(POS5(I)+K-1) + H(4)*H(6)*NLOC_DMG%VNL(POS6(I)+K-1)
     .                                    + H(4)*H(7)*NLOC_DMG%VNL(POS7(I)+K-1) + H(4)*H(8)*NLOC_DMG%VNL(POS8(I)+K-1))  
            ELSE
              A4 = A4 + VOL(I) * WI * XI * (H(4)*H(1)*(SQRT(NLOC_DMG%MASS(POS1(I)+K-1)/NLOC_DMG%MASS0(POS1(I)+K-1)))*NLOC_DMG%VNL(POS1(I)+K-1) + 
     .                                      H(4)*H(2)*(SQRT(NLOC_DMG%MASS(POS2(I)+K-1)/NLOC_DMG%MASS0(POS2(I)+K-1)))*NLOC_DMG%VNL(POS2(I)+K-1) + 
     .                                      H(4)*H(3)*(SQRT(NLOC_DMG%MASS(POS3(I)+K-1)/NLOC_DMG%MASS0(POS3(I)+K-1)))*NLOC_DMG%VNL(POS3(I)+K-1) + 
     .                                      H(4)*H(4)*(SQRT(NLOC_DMG%MASS(POS4(I)+K-1)/NLOC_DMG%MASS0(POS4(I)+K-1)))*NLOC_DMG%VNL(POS4(I)+K-1) + 
     .                                      H(4)*H(5)*(SQRT(NLOC_DMG%MASS(POS5(I)+K-1)/NLOC_DMG%MASS0(POS5(I)+K-1)))*NLOC_DMG%VNL(POS5(I)+K-1) + 
     .                                      H(4)*H(6)*(SQRT(NLOC_DMG%MASS(POS6(I)+K-1)/NLOC_DMG%MASS0(POS6(I)+K-1)))*NLOC_DMG%VNL(POS6(I)+K-1) + 
     .                                      H(4)*H(7)*(SQRT(NLOC_DMG%MASS(POS7(I)+K-1)/NLOC_DMG%MASS0(POS7(I)+K-1)))*NLOC_DMG%VNL(POS7(I)+K-1) + 
     .                                      H(4)*H(8)*(SQRT(NLOC_DMG%MASS(POS8(I)+K-1)/NLOC_DMG%MASS0(POS8(I)+K-1)))*NLOC_DMG%VNL(POS8(I)+K-1))
            ENDIF
c        
            B4 = L2 * VOL(I) * WI * ( BTB14(I)*NLOC_DMG%UNL(POS1(I)+K-1) + BTB24(I)*NLOC_DMG%UNL(POS2(I)+K-1) + BTB34(I)*NLOC_DMG%UNL(POS3(I)+K-1)
     .                              + BTB44(I)*NLOC_DMG%UNL(POS4(I)+K-1) + BTB45(I)*NLOC_DMG%UNL(POS5(I)+K-1) + BTB46(I)*NLOC_DMG%UNL(POS6(I)+K-1)
     .                              + BTB47(I)*NLOC_DMG%UNL(POS7(I)+K-1) + BTB48(I)*NLOC_DMG%UNL(POS8(I)+K-1))
c     
            C4 = VOL(I) * WI * H(4) * VAR_REG(I,IP)
c     
            A5 = VOL(I) * WI * (H(5)*H(1)*NLOC_DMG%UNL(POS1(I)+K-1) + H(5)*H(2)*NLOC_DMG%UNL(POS2(I)+K-1) + H(5)*H(3)*NLOC_DMG%UNL(POS3(I)+K-1)
     .                        + H(5)*H(4)*NLOC_DMG%UNL(POS4(I)+K-1) + H(5)*H(5)*NLOC_DMG%UNL(POS5(I)+K-1) + H(5)*H(6)*NLOC_DMG%UNL(POS6(I)+K-1)
     .                        + H(5)*H(7)*NLOC_DMG%UNL(POS7(I)+K-1) + H(5)*H(8)*NLOC_DMG%UNL(POS8(I)+K-1))
c
            IF (NODADT == 0) THEN
              A5 = A5 + VOL(I) * WI * XI * (H(5)*H(1)*NLOC_DMG%VNL(POS1(I)+K-1) + H(5)*H(2)*NLOC_DMG%VNL(POS2(I)+K-1) + H(5)*H(3)*NLOC_DMG%VNL(POS3(I)+K-1)
     .                                    + H(5)*H(4)*NLOC_DMG%VNL(POS4(I)+K-1) + H(5)*H(5)*NLOC_DMG%VNL(POS5(I)+K-1) + H(5)*H(6)*NLOC_DMG%VNL(POS6(I)+K-1)
     .                                    + H(5)*H(7)*NLOC_DMG%VNL(POS7(I)+K-1) + H(5)*H(8)*NLOC_DMG%VNL(POS8(I)+K-1))  
            ELSE
              A5 = A5 + VOL(I) * WI * XI * (H(5)*H(1)*(SQRT(NLOC_DMG%MASS(POS1(I)+K-1)/NLOC_DMG%MASS0(POS1(I)+K-1)))*NLOC_DMG%VNL(POS1(I)+K-1) + 
     .                                      H(5)*H(2)*(SQRT(NLOC_DMG%MASS(POS2(I)+K-1)/NLOC_DMG%MASS0(POS2(I)+K-1)))*NLOC_DMG%VNL(POS2(I)+K-1) + 
     .                                      H(5)*H(3)*(SQRT(NLOC_DMG%MASS(POS3(I)+K-1)/NLOC_DMG%MASS0(POS3(I)+K-1)))*NLOC_DMG%VNL(POS3(I)+K-1) + 
     .                                      H(5)*H(4)*(SQRT(NLOC_DMG%MASS(POS4(I)+K-1)/NLOC_DMG%MASS0(POS4(I)+K-1)))*NLOC_DMG%VNL(POS4(I)+K-1) + 
     .                                      H(5)*H(5)*(SQRT(NLOC_DMG%MASS(POS5(I)+K-1)/NLOC_DMG%MASS0(POS5(I)+K-1)))*NLOC_DMG%VNL(POS5(I)+K-1) + 
     .                                      H(5)*H(6)*(SQRT(NLOC_DMG%MASS(POS6(I)+K-1)/NLOC_DMG%MASS0(POS6(I)+K-1)))*NLOC_DMG%VNL(POS6(I)+K-1) + 
     .                                      H(5)*H(7)*(SQRT(NLOC_DMG%MASS(POS7(I)+K-1)/NLOC_DMG%MASS0(POS7(I)+K-1)))*NLOC_DMG%VNL(POS7(I)+K-1) + 
     .                                      H(5)*H(8)*(SQRT(NLOC_DMG%MASS(POS8(I)+K-1)/NLOC_DMG%MASS0(POS8(I)+K-1)))*NLOC_DMG%VNL(POS8(I)+K-1))
            ENDIF
c        
            B5 = L2 * VOL(I) * WI * ( BTB15(I)*NLOC_DMG%UNL(POS1(I)+K-1) + BTB25(I)*NLOC_DMG%UNL(POS2(I)+K-1) + BTB35(I)*NLOC_DMG%UNL(POS3(I)+K-1)
     .                              + BTB45(I)*NLOC_DMG%UNL(POS4(I)+K-1) + BTB55(I)*NLOC_DMG%UNL(POS5(I)+K-1) + BTB56(I)*NLOC_DMG%UNL(POS6(I)+K-1)
     .                              + BTB57(I)*NLOC_DMG%UNL(POS7(I)+K-1) + BTB58(I)*NLOC_DMG%UNL(POS8(I)+K-1))
c     
            C5 = VOL(I) * WI * H(5) * VAR_REG(I,IP)      
c     
            A6 = VOL(I) * WI * (H(6)*H(1)*NLOC_DMG%UNL(POS1(I)+K-1) + H(6)*H(2)*NLOC_DMG%UNL(POS2(I)+K-1) + H(6)*H(3)*NLOC_DMG%UNL(POS3(I)+K-1)
     .                        + H(6)*H(4)*NLOC_DMG%UNL(POS4(I)+K-1) + H(6)*H(5)*NLOC_DMG%UNL(POS5(I)+K-1) + H(6)*H(6)*NLOC_DMG%UNL(POS6(I)+K-1)
     .                        + H(6)*H(7)*NLOC_DMG%UNL(POS7(I)+K-1) + H(6)*H(8)*NLOC_DMG%UNL(POS8(I)+K-1))
c
            IF (NODADT == 0) THEN
              A6 = A6 + VOL(I) * WI * XI * (H(6)*H(1)*NLOC_DMG%VNL(POS1(I)+K-1) + H(6)*H(2)*NLOC_DMG%VNL(POS2(I)+K-1) + H(6)*H(3)*NLOC_DMG%VNL(POS3(I)+K-1)
     .                                    + H(6)*H(4)*NLOC_DMG%VNL(POS4(I)+K-1) + H(6)*H(5)*NLOC_DMG%VNL(POS5(I)+K-1) + H(6)*H(6)*NLOC_DMG%VNL(POS6(I)+K-1)
     .                                    + H(6)*H(7)*NLOC_DMG%VNL(POS7(I)+K-1) + H(6)*H(8)*NLOC_DMG%VNL(POS8(I)+K-1)) 
            ELSE
              A6 = A6 + VOL(I) * WI * XI * (H(6)*H(1)*(SQRT(NLOC_DMG%MASS(POS1(I)+K-1)/NLOC_DMG%MASS0(POS1(I)+K-1)))*NLOC_DMG%VNL(POS1(I)+K-1) + 
     .                                      H(6)*H(2)*(SQRT(NLOC_DMG%MASS(POS2(I)+K-1)/NLOC_DMG%MASS0(POS2(I)+K-1)))*NLOC_DMG%VNL(POS2(I)+K-1) + 
     .                                      H(6)*H(3)*(SQRT(NLOC_DMG%MASS(POS3(I)+K-1)/NLOC_DMG%MASS0(POS3(I)+K-1)))*NLOC_DMG%VNL(POS3(I)+K-1) + 
     .                                      H(6)*H(4)*(SQRT(NLOC_DMG%MASS(POS4(I)+K-1)/NLOC_DMG%MASS0(POS4(I)+K-1)))*NLOC_DMG%VNL(POS4(I)+K-1) + 
     .                                      H(6)*H(5)*(SQRT(NLOC_DMG%MASS(POS5(I)+K-1)/NLOC_DMG%MASS0(POS5(I)+K-1)))*NLOC_DMG%VNL(POS5(I)+K-1) + 
     .                                      H(6)*H(6)*(SQRT(NLOC_DMG%MASS(POS6(I)+K-1)/NLOC_DMG%MASS0(POS6(I)+K-1)))*NLOC_DMG%VNL(POS6(I)+K-1) + 
     .                                      H(6)*H(7)*(SQRT(NLOC_DMG%MASS(POS7(I)+K-1)/NLOC_DMG%MASS0(POS7(I)+K-1)))*NLOC_DMG%VNL(POS7(I)+K-1) + 
     .                                      H(6)*H(8)*(SQRT(NLOC_DMG%MASS(POS8(I)+K-1)/NLOC_DMG%MASS0(POS8(I)+K-1)))*NLOC_DMG%VNL(POS8(I)+K-1))
            ENDIF
c        
            B6 = L2 * VOL(I) * WI * ( BTB16(I)*NLOC_DMG%UNL(POS1(I)+K-1) + BTB26(I)*NLOC_DMG%UNL(POS2(I)+K-1) + BTB36(I)*NLOC_DMG%UNL(POS3(I)+K-1)
     .                              + BTB46(I)*NLOC_DMG%UNL(POS4(I)+K-1) + BTB56(I)*NLOC_DMG%UNL(POS5(I)+K-1) + BTB66(I)*NLOC_DMG%UNL(POS6(I)+K-1)
     .                              + BTB67(I)*NLOC_DMG%UNL(POS7(I)+K-1) + BTB68(I)*NLOC_DMG%UNL(POS8(I)+K-1))
c     
            C6 = VOL(I) * WI * H(6) * VAR_REG(I,IP)      
c     
            A7 = VOL(I) * WI * (H(7)*H(1)*NLOC_DMG%UNL(POS1(I)+K-1) + H(7)*H(2)*NLOC_DMG%UNL(POS2(I)+K-1) + H(7)*H(3)*NLOC_DMG%UNL(POS3(I)+K-1)
     .                        + H(7)*H(4)*NLOC_DMG%UNL(POS4(I)+K-1) + H(7)*H(5)*NLOC_DMG%UNL(POS5(I)+K-1) + H(7)*H(6)*NLOC_DMG%UNL(POS6(I)+K-1)
     .                        + H(7)*H(7)*NLOC_DMG%UNL(POS7(I)+K-1) + H(7)*H(8)*NLOC_DMG%UNL(POS8(I)+K-1))
c
            IF (NODADT == 0) THEN
              A7 = A7 + VOL(I) * WI * XI * (H(7)*H(1)*NLOC_DMG%VNL(POS1(I)+K-1) + H(7)*H(2)*NLOC_DMG%VNL(POS2(I)+K-1) + H(7)*H(3)*NLOC_DMG%VNL(POS3(I)+K-1)
     .                                    + H(7)*H(4)*NLOC_DMG%VNL(POS4(I)+K-1) + H(7)*H(5)*NLOC_DMG%VNL(POS5(I)+K-1) + H(7)*H(6)*NLOC_DMG%VNL(POS6(I)+K-1)
     .                                    + H(7)*H(7)*NLOC_DMG%VNL(POS7(I)+K-1) + H(7)*H(8)*NLOC_DMG%VNL(POS8(I)+K-1))
            ELSE
              A7 = A7 + VOL(I) * WI * XI * (H(7)*H(1)*(SQRT(NLOC_DMG%MASS(POS1(I)+K-1)/NLOC_DMG%MASS0(POS1(I)+K-1)))*NLOC_DMG%VNL(POS1(I)+K-1) + 
     .                                      H(7)*H(2)*(SQRT(NLOC_DMG%MASS(POS2(I)+K-1)/NLOC_DMG%MASS0(POS2(I)+K-1)))*NLOC_DMG%VNL(POS2(I)+K-1) + 
     .                                      H(7)*H(3)*(SQRT(NLOC_DMG%MASS(POS3(I)+K-1)/NLOC_DMG%MASS0(POS3(I)+K-1)))*NLOC_DMG%VNL(POS3(I)+K-1) + 
     .                                      H(7)*H(4)*(SQRT(NLOC_DMG%MASS(POS4(I)+K-1)/NLOC_DMG%MASS0(POS4(I)+K-1)))*NLOC_DMG%VNL(POS4(I)+K-1) + 
     .                                      H(7)*H(5)*(SQRT(NLOC_DMG%MASS(POS5(I)+K-1)/NLOC_DMG%MASS0(POS5(I)+K-1)))*NLOC_DMG%VNL(POS5(I)+K-1) + 
     .                                      H(7)*H(6)*(SQRT(NLOC_DMG%MASS(POS6(I)+K-1)/NLOC_DMG%MASS0(POS6(I)+K-1)))*NLOC_DMG%VNL(POS6(I)+K-1) + 
     .                                      H(7)*H(7)*(SQRT(NLOC_DMG%MASS(POS7(I)+K-1)/NLOC_DMG%MASS0(POS7(I)+K-1)))*NLOC_DMG%VNL(POS7(I)+K-1) + 
     .                                      H(7)*H(8)*(SQRT(NLOC_DMG%MASS(POS8(I)+K-1)/NLOC_DMG%MASS0(POS8(I)+K-1)))*NLOC_DMG%VNL(POS8(I)+K-1))
            ENDIF  
c        
            B7 = L2 * VOL(I) * WI * ( BTB17(I)*NLOC_DMG%UNL(POS1(I)+K-1) + BTB27(I)*NLOC_DMG%UNL(POS2(I)+K-1) + BTB37(I)*NLOC_DMG%UNL(POS3(I)+K-1)
     .                              + BTB47(I)*NLOC_DMG%UNL(POS4(I)+K-1) + BTB57(I)*NLOC_DMG%UNL(POS5(I)+K-1) + BTB67(I)*NLOC_DMG%UNL(POS6(I)+K-1)
     .                              + BTB77(I)*NLOC_DMG%UNL(POS7(I)+K-1) + BTB78(I)*NLOC_DMG%UNL(POS8(I)+K-1))
c     
            C7 = VOL(I) * WI * H(7) * VAR_REG(I,IP)      
c     
            A8 = VOL(I) * WI * (H(8)*H(1)*NLOC_DMG%UNL(POS1(I)+K-1) + H(8)*H(2)*NLOC_DMG%UNL(POS2(I)+K-1) + H(8)*H(3)*NLOC_DMG%UNL(POS3(I)+K-1)
     .                        + H(8)*H(4)*NLOC_DMG%UNL(POS4(I)+K-1) + H(8)*H(5)*NLOC_DMG%UNL(POS5(I)+K-1) + H(8)*H(6)*NLOC_DMG%UNL(POS6(I)+K-1)
     .                        + H(8)*H(7)*NLOC_DMG%UNL(POS7(I)+K-1) + H(8)*H(8)*NLOC_DMG%UNL(POS8(I)+K-1))
c
            IF (NODADT == 0) THEN
              A8 = A8 + VOL(I) * WI * XI * (H(8)*H(1)*NLOC_DMG%VNL(POS1(I)+K-1) + H(8)*H(2)*NLOC_DMG%VNL(POS2(I)+K-1) + H(8)*H(3)*NLOC_DMG%VNL(POS3(I)+K-1)
     .                                    + H(8)*H(4)*NLOC_DMG%VNL(POS4(I)+K-1) + H(8)*H(5)*NLOC_DMG%VNL(POS5(I)+K-1) + H(8)*H(6)*NLOC_DMG%VNL(POS6(I)+K-1)
     .                                    + H(8)*H(7)*NLOC_DMG%VNL(POS7(I)+K-1) + H(8)*H(8)*NLOC_DMG%VNL(POS8(I)+K-1))  
            ELSE
              A8 = A8 + VOL(I) * WI * XI * (H(8)*H(1)*(SQRT(NLOC_DMG%MASS(POS1(I)+K-1)/NLOC_DMG%MASS0(POS1(I)+K-1)))*NLOC_DMG%VNL(POS1(I)+K-1) + 
     .                                      H(8)*H(2)*(SQRT(NLOC_DMG%MASS(POS2(I)+K-1)/NLOC_DMG%MASS0(POS2(I)+K-1)))*NLOC_DMG%VNL(POS2(I)+K-1) + 
     .                                      H(8)*H(3)*(SQRT(NLOC_DMG%MASS(POS3(I)+K-1)/NLOC_DMG%MASS0(POS3(I)+K-1)))*NLOC_DMG%VNL(POS3(I)+K-1) + 
     .                                      H(8)*H(4)*(SQRT(NLOC_DMG%MASS(POS4(I)+K-1)/NLOC_DMG%MASS0(POS4(I)+K-1)))*NLOC_DMG%VNL(POS4(I)+K-1) + 
     .                                      H(8)*H(5)*(SQRT(NLOC_DMG%MASS(POS5(I)+K-1)/NLOC_DMG%MASS0(POS5(I)+K-1)))*NLOC_DMG%VNL(POS5(I)+K-1) + 
     .                                      H(8)*H(6)*(SQRT(NLOC_DMG%MASS(POS6(I)+K-1)/NLOC_DMG%MASS0(POS6(I)+K-1)))*NLOC_DMG%VNL(POS6(I)+K-1) + 
     .                                      H(8)*H(7)*(SQRT(NLOC_DMG%MASS(POS7(I)+K-1)/NLOC_DMG%MASS0(POS7(I)+K-1)))*NLOC_DMG%VNL(POS7(I)+K-1) + 
     .                                      H(8)*H(8)*(SQRT(NLOC_DMG%MASS(POS8(I)+K-1)/NLOC_DMG%MASS0(POS8(I)+K-1)))*NLOC_DMG%VNL(POS8(I)+K-1))
            ENDIF
c        
            B8 = L2 * VOL(I) * WI * ( BTB18(I)*NLOC_DMG%UNL(POS1(I)+K-1) + BTB28(I)*NLOC_DMG%UNL(POS2(I)+K-1) + BTB38(I)*NLOC_DMG%UNL(POS3(I)+K-1)
     .                              + BTB48(I)*NLOC_DMG%UNL(POS4(I)+K-1) + BTB58(I)*NLOC_DMG%UNL(POS5(I)+K-1) + BTB68(I)*NLOC_DMG%UNL(POS6(I)+K-1)
     .                              + BTB78(I)*NLOC_DMG%UNL(POS7(I)+K-1) + BTB88(I)*NLOC_DMG%UNL(POS8(I)+K-1))
c     
            C8 = VOL(I) * WI * H(8) * VAR_REG(I,IP)    
c
c           Fint = Vol * (L*L * BT*B*Unl + NT*N*Unl + Damp*NT*N*Vnl - NT*Vreg  )    
c          
            F1(I,K) = A1 + B1 - C1
            F2(I,K) = A2 + B2 - C2
            F3(I,K) = A3 + B3 - C3
            F4(I,K) = A4 + B4 - C4
            F5(I,K) = A5 + B5 - C5
            F6(I,K) = A6 + B6 - C6
            F7(I,K) = A7 + B7 - C7
            F8(I,K) = A8 + B8 - C8  
c
            ! Computing nodal equivalent stiffness
            IF (NODADT > 0) THEN 
              STI1(I,K) = (ABS(L2*BTB11(I)  + H(1)*H(1)) + ABS(L2*BTB12(I)  + H(1)*H(2)) + ABS(L2*BTB13(I)  + H(1)*H(3)) +
     .                     ABS(L2*BTB14(I)  + H(1)*H(4)) + ABS(L2*BTB15(I)  + H(1)*H(5)) + ABS(L2*BTB16(I)  + H(1)*H(6)) +
     .                     ABS(L2*BTB17(I)  + H(1)*H(7)) + ABS(L2*BTB18(I)  + H(1)*H(8)))*VOL(I) * WI
              STI2(I,K) = (ABS(L2*BTB12(I)  + H(2)*H(1)) + ABS(L2*BTB22(I)  + H(2)*H(2)) + ABS(L2*BTB23(I)  + H(2)*H(3)) +
     .                     ABS(L2*BTB24(I)  + H(2)*H(4)) + ABS(L2*BTB25(I)  + H(2)*H(5)) + ABS(L2*BTB26(I)  + H(2)*H(6)) +
     .                     ABS(L2*BTB27(I)  + H(2)*H(7)) + ABS(L2*BTB28(I)  + H(2)*H(8)))*VOL(I) * WI
              STI3(I,K) = (ABS(L2*BTB13(I)  + H(3)*H(1)) + ABS(L2*BTB23(I)  + H(3)*H(2)) + ABS(L2*BTB33(I)  + H(3)*H(3)) +
     .                     ABS(L2*BTB34(I)  + H(3)*H(4)) + ABS(L2*BTB35(I)  + H(3)*H(5)) + ABS(L2*BTB36(I)  + H(3)*H(6)) +
     .                     ABS(L2*BTB37(I)  + H(3)*H(7)) + ABS(L2*BTB38(I)  + H(3)*H(8)))*VOL(I) * WI
              STI4(I,K) = (ABS(L2*BTB14(I)  + H(4)*H(1)) + ABS(L2*BTB24(I)  + H(4)*H(2)) + ABS(L2*BTB34(I)  + H(4)*H(3)) +
     .                     ABS(L2*BTB44(I)  + H(4)*H(4)) + ABS(L2*BTB45(I)  + H(4)*H(5)) + ABS(L2*BTB46(I)  + H(4)*H(6)) +
     .                     ABS(L2*BTB47(I)  + H(4)*H(7)) + ABS(L2*BTB48(I)  + H(4)*H(8)))*VOL(I) * WI
              STI5(I,K) = (ABS(L2*BTB15(I)  + H(5)*H(1)) + ABS(L2*BTB25(I)  + H(5)*H(2)) + ABS(L2*BTB35(I)  + H(5)*H(3)) +
     .                     ABS(L2*BTB45(I)  + H(5)*H(4)) + ABS(L2*BTB55(I)  + H(5)*H(5)) + ABS(L2*BTB56(I)  + H(5)*H(6)) +
     .                     ABS(L2*BTB57(I)  + H(5)*H(7)) + ABS(L2*BTB58(I)  + H(5)*H(8)))*VOL(I) * WI
              STI6(I,K) = (ABS(L2*BTB16(I)  + H(6)*H(1)) + ABS(L2*BTB26(I)  + H(6)*H(2)) + ABS(L2*BTB36(I)  + H(6)*H(3)) +
     .                     ABS(L2*BTB46(I)  + H(6)*H(4)) + ABS(L2*BTB56(I)  + H(6)*H(5)) + ABS(L2*BTB66(I)  + H(6)*H(6)) +
     .                     ABS(L2*BTB67(I)  + H(6)*H(7)) + ABS(L2*BTB68(I)  + H(6)*H(8)))*VOL(I) * WI
              STI7(I,K) = (ABS(L2*BTB17(I)  + H(7)*H(1)) + ABS(L2*BTB27(I)  + H(7)*H(2)) + ABS(L2*BTB37(I)  + H(7)*H(3)) +
     .                     ABS(L2*BTB47(I)  + H(7)*H(4)) + ABS(L2*BTB57(I)  + H(7)*H(5)) + ABS(L2*BTB67(I)  + H(7)*H(6)) +
     .                     ABS(L2*BTB77(I)  + H(7)*H(7)) + ABS(L2*BTB78(I)  + H(7)*H(8)))*VOL(I) * WI
              STI8(I,K) = (ABS(L2*BTB18(I)  + H(8)*H(1)) + ABS(L2*BTB28(I)  + H(8)*H(2)) + ABS(L2*BTB38(I)  + H(8)*H(3)) +
     .                     ABS(L2*BTB48(I)  + H(8)*H(4)) + ABS(L2*BTB58(I)  + H(8)*H(5)) + ABS(L2*BTB68(I)  + H(8)*H(6)) +
     .                     ABS(L2*BTB78(I)  + H(8)*H(7)) + ABS(L2*BTB88(I)  + H(8)*H(8)))*VOL(I) * WI
            ENDIF
c
          ! If the element is broken
          ELSE
c
            ! Initial element characteristic length
            LC(I) = (WI*VOL0(I))**THIRD 
c
            IF (NODADT > 0) THEN           
              ! Non-local absorbing forces
              F1(I,K) = SQRT(NLOC_DMG%MASS(POS1(I)+K-1)/NLOC_DMG%MASS0(POS1(I)+K-1))*H(1)*ZETA*SSPNL*HALF*
     .                                  (NLOC_DMG%VNL(POS1(I)+K-1)+NLOC_DMG%VNL_OLD(POS1(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F2(I,K) = SQRT(NLOC_DMG%MASS(POS2(I)+K-1)/NLOC_DMG%MASS0(POS2(I)+K-1))*H(2)*ZETA*SSPNL*HALF*
     .                                  (NLOC_DMG%VNL(POS2(I)+K-1)+NLOC_DMG%VNL_OLD(POS2(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F3(I,K) = SQRT(NLOC_DMG%MASS(POS3(I)+K-1)/NLOC_DMG%MASS0(POS3(I)+K-1))*H(3)*ZETA*SSPNL*HALF*
     .                                  (NLOC_DMG%VNL(POS3(I)+K-1)+NLOC_DMG%VNL_OLD(POS3(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F4(I,K) = SQRT(NLOC_DMG%MASS(POS4(I)+K-1)/NLOC_DMG%MASS0(POS4(I)+K-1))*H(4)*ZETA*SSPNL*HALF*
     .                                  (NLOC_DMG%VNL(POS4(I)+K-1)+NLOC_DMG%VNL_OLD(POS4(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F5(I,K) = SQRT(NLOC_DMG%MASS(POS5(I)+K-1)/NLOC_DMG%MASS0(POS5(I)+K-1))*H(5)*ZETA*SSPNL*HALF*
     .                                  (NLOC_DMG%VNL(POS5(I)+K-1)+NLOC_DMG%VNL_OLD(POS5(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F6(I,K) = SQRT(NLOC_DMG%MASS(POS6(I)+K-1)/NLOC_DMG%MASS0(POS6(I)+K-1))*H(6)*ZETA*SSPNL*HALF*
     .                                  (NLOC_DMG%VNL(POS6(I)+K-1)+NLOC_DMG%VNL_OLD(POS6(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F7(I,K) = SQRT(NLOC_DMG%MASS(POS7(I)+K-1)/NLOC_DMG%MASS0(POS7(I)+K-1))*H(7)*ZETA*SSPNL*HALF*
     .                                  (NLOC_DMG%VNL(POS7(I)+K-1)+NLOC_DMG%VNL_OLD(POS7(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F8(I,K) = SQRT(NLOC_DMG%MASS(POS8(I)+K-1)/NLOC_DMG%MASS0(POS8(I)+K-1))*H(8)*ZETA*SSPNL*HALF*
     .                                  (NLOC_DMG%VNL(POS8(I)+K-1)+NLOC_DMG%VNL_OLD(POS8(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              ! Computing nodal equivalent stiffness
              STI1(I,K) = EM20
              STI2(I,K) = EM20
              STI3(I,K) = EM20
              STI4(I,K) = EM20
              STI5(I,K) = EM20
              STI6(I,K) = EM20
              STI7(I,K) = EM20
              STI8(I,K) = EM20
            ELSE
              ! Non-local absorbing forces
              F1(I,K) = H(1)*ZETA*SSPNL*HALF*(NLOC_DMG%VNL(POS1(I)+K-1)+NLOC_DMG%VNL_OLD(POS1(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F2(I,K) = H(2)*ZETA*SSPNL*HALF*(NLOC_DMG%VNL(POS2(I)+K-1)+NLOC_DMG%VNL_OLD(POS2(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F3(I,K) = H(3)*ZETA*SSPNL*HALF*(NLOC_DMG%VNL(POS3(I)+K-1)+NLOC_DMG%VNL_OLD(POS3(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F4(I,K) = H(4)*ZETA*SSPNL*HALF*(NLOC_DMG%VNL(POS4(I)+K-1)+NLOC_DMG%VNL_OLD(POS4(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F5(I,K) = H(5)*ZETA*SSPNL*HALF*(NLOC_DMG%VNL(POS5(I)+K-1)+NLOC_DMG%VNL_OLD(POS5(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F6(I,K) = H(6)*ZETA*SSPNL*HALF*(NLOC_DMG%VNL(POS6(I)+K-1)+NLOC_DMG%VNL_OLD(POS6(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F7(I,K) = H(7)*ZETA*SSPNL*HALF*(NLOC_DMG%VNL(POS7(I)+K-1)+NLOC_DMG%VNL_OLD(POS7(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F8(I,K) = H(8)*ZETA*SSPNL*HALF*(NLOC_DMG%VNL(POS8(I)+K-1)+NLOC_DMG%VNL_OLD(POS8(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
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
        ! Loop over elements
        DO I=1,NEL
          ! Loop over non-local degrees of freedom
#include "vectorize.inc"
          DO K=1,NLAY
            ! Assembling the forces in the classic way 
            NLOC_DMG%FNL(POS1(I)+K-1,ITASK+1) = NLOC_DMG%FNL(POS1(I)+K-1,ITASK+1) - F1(I,K)
            NLOC_DMG%FNL(POS2(I)+K-1,ITASK+1) = NLOC_DMG%FNL(POS2(I)+K-1,ITASK+1) - F2(I,K)
            NLOC_DMG%FNL(POS3(I)+K-1,ITASK+1) = NLOC_DMG%FNL(POS3(I)+K-1,ITASK+1) - F3(I,K)
            NLOC_DMG%FNL(POS4(I)+K-1,ITASK+1) = NLOC_DMG%FNL(POS4(I)+K-1,ITASK+1) - F4(I,K)
            NLOC_DMG%FNL(POS5(I)+K-1,ITASK+1) = NLOC_DMG%FNL(POS5(I)+K-1,ITASK+1) - F5(I,K)
            NLOC_DMG%FNL(POS6(I)+K-1,ITASK+1) = NLOC_DMG%FNL(POS6(I)+K-1,ITASK+1) - F6(I,K)
            NLOC_DMG%FNL(POS7(I)+K-1,ITASK+1) = NLOC_DMG%FNL(POS7(I)+K-1,ITASK+1) - F7(I,K)
            NLOC_DMG%FNL(POS8(I)+K-1,ITASK+1) = NLOC_DMG%FNL(POS8(I)+K-1,ITASK+1) - F8(I,K)   
            IF (NODADT > 0) THEN
              ! Spectral radius of stiffness matrix
              MAXSTIF = MAX(STI1(I,K),STI2(I,K),STI3(I,K),STI4(I,K),
     .                      STI5(I,K),STI6(I,K),STI7(I,K),STI8(I,K))
              ! Computing nodal stiffness
              NLOC_DMG%STIFNL(POS1(I)+K-1,ITASK+1) = NLOC_DMG%STIFNL(POS1(I)+K-1,ITASK+1) + MAXSTIF
              NLOC_DMG%STIFNL(POS2(I)+K-1,ITASK+1) = NLOC_DMG%STIFNL(POS2(I)+K-1,ITASK+1) + MAXSTIF
              NLOC_DMG%STIFNL(POS3(I)+K-1,ITASK+1) = NLOC_DMG%STIFNL(POS3(I)+K-1,ITASK+1) + MAXSTIF
              NLOC_DMG%STIFNL(POS4(I)+K-1,ITASK+1) = NLOC_DMG%STIFNL(POS4(I)+K-1,ITASK+1) + MAXSTIF
              NLOC_DMG%STIFNL(POS5(I)+K-1,ITASK+1) = NLOC_DMG%STIFNL(POS5(I)+K-1,ITASK+1) + MAXSTIF
              NLOC_DMG%STIFNL(POS6(I)+K-1,ITASK+1) = NLOC_DMG%STIFNL(POS6(I)+K-1,ITASK+1) + MAXSTIF
              NLOC_DMG%STIFNL(POS7(I)+K-1,ITASK+1) = NLOC_DMG%STIFNL(POS7(I)+K-1,ITASK+1) + MAXSTIF
              NLOC_DMG%STIFNL(POS8(I)+K-1,ITASK+1) = NLOC_DMG%STIFNL(POS8(I)+K-1,ITASK+1) + MAXSTIF
            ENDIF
          ENDDO
        ENDDO
c
      ! If PARITH/ON
      ELSE
        ! Loop over additional d.o.fs
        DO J = 1,NLAY
c
          ! Integration point number
          IP = IR + ((IS-1) + (J-1)*NPTS)*NPTR
c
          ! Loop over elements
          DO I=1,NEL
            II  = I + NFT
c
            ! Spectral radius of stiffness matrix
            IF (NODADT > 0) THEN
              MAXSTIF = MAX(STI1(I,J),STI2(I,J),STI3(I,J),STI4(I,J),
     .                      STI5(I,J),STI6(I,J),STI7(I,J),STI8(I,J))
            ENDIF
c
            K = NLOC_DMG%IADS(1,II)
            IF (IP == 1) THEN 
              NLOC_DMG%FSKY(K,J) = -F1(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
            ELSE
              NLOC_DMG%FSKY(K,J) = NLOC_DMG%FSKY(K,J) - F1(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = NLOC_DMG%STSKY(K,J) + MAXSTIF
            ENDIF
c
            K = NLOC_DMG%IADS(2,II)
            IF (IP == 1) THEN 
              NLOC_DMG%FSKY(K,J) = -F2(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
            ELSE
              NLOC_DMG%FSKY(K,J) = NLOC_DMG%FSKY(K,J) - F2(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = NLOC_DMG%STSKY(K,J) + MAXSTIF
            ENDIF
c
            K = NLOC_DMG%IADS(3,II)
            IF (IP == 1) THEN 
              NLOC_DMG%FSKY(K,J) = -F3(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
            ELSE
              NLOC_DMG%FSKY(K,J) = NLOC_DMG%FSKY(K,J) - F3(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = NLOC_DMG%STSKY(K,J) + MAXSTIF
            ENDIF
c
            K = NLOC_DMG%IADS(4,II)
            IF (IP == 1) THEN 
              NLOC_DMG%FSKY(K,J) = -F4(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
            ELSE
              NLOC_DMG%FSKY(K,J) = NLOC_DMG%FSKY(K,J) - F4(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = NLOC_DMG%STSKY(K,J) + MAXSTIF
            ENDIF
c
            K = NLOC_DMG%IADS(5,II)
            IF (IP == 1) THEN 
              NLOC_DMG%FSKY(K,J) = -F5(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
            ELSE
              NLOC_DMG%FSKY(K,J) = NLOC_DMG%FSKY(K,J) - F5(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = NLOC_DMG%STSKY(K,J) + MAXSTIF
            ENDIF
c
            K = NLOC_DMG%IADS(6,II)
            IF (IP == 1) THEN 
              NLOC_DMG%FSKY(K,J) = -F6(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
            ELSE
              NLOC_DMG%FSKY(K,J) = NLOC_DMG%FSKY(K,J) - F6(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = NLOC_DMG%STSKY(K,J) + MAXSTIF
            ENDIF
c
            K = NLOC_DMG%IADS(7,II)
            IF (IP == 1) THEN 
              NLOC_DMG%FSKY(K,J) = -F7(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
            ELSE
              NLOC_DMG%FSKY(K,J) = NLOC_DMG%FSKY(K,J) - F7(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = NLOC_DMG%STSKY(K,J) + MAXSTIF
            ENDIF
c
            K = NLOC_DMG%IADS(8,II)
            IF (IP == 1) THEN 
              NLOC_DMG%FSKY(K,J) = -F8(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
            ELSE
              NLOC_DMG%FSKY(K,J) = NLOC_DMG%FSKY(K,J) - F8(I,J)
              IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = NLOC_DMG%STSKY(K,J) + MAXSTIF
            ENDIF
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
          IF (OFFG(I)/=ZERO) THEN
            ! Non-local critical time-step in the plane
            DTNL = (TWO*(MIN((VOL(I))**THIRD,LE_MAX))*SQRT(THREE*ZETA))/
     .              SQRT(TWELVE*L2 + (MIN((VOL(I))**THIRD,LE_MAX))**2)
            ! Non-local critical time-step in the thickness
            IF ((L2>ZERO).AND.(NLAY > 1)) THEN 
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
