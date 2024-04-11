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
Chd|  SCFINT_REG                    source/elements/thickshell/solidec/scfint_reg.F
Chd|-- called by -----------
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|-- calls ---------------
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|====================================================================
      SUBROUTINE SCFINT_REG(
     1   NLOC_DMG,VAR_REG ,NEL     ,OFF     ,
     2   VOL     ,NLOCS   ,AREA    ,
     3   PX1     ,PX2     ,PX3     ,PX4     ,
     4   PY1     ,PY2     ,PY3     ,PY4     ,
     5   PZ1     ,PZ2     ,PZ3     ,PZ4     ,
     6   IMAT    ,ITASK   ,DT2T    ,VOL0    ,
     7   NFT     ,NLAY    ,WS      ,AS      ,
     8   BUFNLTS )
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
      INTEGER, INTENT(IN)    :: NFT               !< Address of the first element of the group
      INTEGER, INTENT(IN)    :: NLAY              !< Number of layers in the elements
      INTEGER, INTENT(IN)    :: NEL               !< Number of elements in the group
      INTEGER, INTENT(IN)    :: IMAT              !< Material internal Id
      INTEGER, INTENT(IN)    :: ITASK             !< Number of the thread
      my_real, INTENT(INOUT) :: DT2T              !< Element critical timestep
      my_real, DIMENSION(9,9), INTENT(IN) :: WS   !< Weight of integration points through thickness
      my_real, DIMENSION(9,9), INTENT(IN) :: AS   !< Position of integration points through thickness
      my_real, DIMENSION(NEL,NLAY), INTENT(INOUT) :: VAR_REG !< Variable to regularize
      my_real, DIMENSION(NEL), INTENT(IN) :: VOL  !< Current volume of the element
      my_real, DIMENSION(NEL), INTENT(IN) :: OFF  !< Flag for element deletion
      my_real, DIMENSION(NEL), INTENT(IN) :: VOL0 !< Initial volume of the element
      my_real, DIMENSION(NEL), INTENT(IN) :: AREA !< Area of the thickshell element
      my_real, DIMENSION(NEL), INTENT(IN) :: PX1  !< Derivative of the first node shape function along x
      my_real, DIMENSION(NEL), INTENT(IN) :: PX2  !< Derivative of the second node shape function along x
      my_real, DIMENSION(NEL), INTENT(IN) :: PX3  !< Derivative of the third node shape function along x
      my_real, DIMENSION(NEL), INTENT(IN) :: PX4  !< Derivative of the fourth node shape function along x
      my_real, DIMENSION(NEL), INTENT(IN) :: PY1  !< Derivative of the first node shape function along y
      my_real, DIMENSION(NEL), INTENT(IN) :: PY2  !< Derivative of the second node shape function along y
      my_real, DIMENSION(NEL), INTENT(IN) :: PY3  !< Derivative of the third node shape function along y
      my_real, DIMENSION(NEL), INTENT(IN) :: PY4  !< Derivative of the fourth node shape function along y
      my_real, DIMENSION(NEL), INTENT(IN) :: PZ1  !< Derivative of the first node shape function along z
      my_real, DIMENSION(NEL), INTENT(IN) :: PZ2  !< Derivative of the second node shape function along z
      my_real, DIMENSION(NEL), INTENT(IN) :: PZ3  !< Derivative of the third node shape function along z
      my_real, DIMENSION(NEL), INTENT(IN) :: PZ4  !< Derivative of the fourth node shape function along z
      TYPE(NLOCAL_STR_), INTENT(INOUT), TARGET :: NLOC_DMG !< Non-local data structure
      TYPE(BUF_NLOCTS_), INTENT(INOUT), TARGET :: BUFNLTS !< Non-local specific buffer for thickshell thickness
      TYPE(BUF_NLOCS_) , INTENT(IN) :: NLOCS      !< Non-local effective nodes data structure
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II,J,K,L,N1,N2,N3,N4,N5,N6,N7,N8,
     .        L_NLOC,NDDL,NDNOD,NINDX6,INDX6(NEL),
     .        NINDX8,INDX8(NEL)
      INTEGER, DIMENSION(:), ALLOCATABLE   ::
     .   POS1,POS2,POS3,POS4,POS5,POS6,POS7,POS8
      my_real 
     .   L2,NTN_UNL,NTN_VNL,XI,NTVAR,A,DTNL,LE_MAX,
     .   B1,B2,B3,B4,B5,B6,B7,B8,ZETA,SSPNL,MAXSTIF,
     .   BTH1,BTH2,NTH1,NTH2,DT2P,DTNOD,K1,K2,K12,
     .   DTNL_TH,NTN6,NTN8
      my_real, DIMENSION(:) ,ALLOCATABLE   :: 
     .   BTB11,BTB12,BTB13,BTB14,BTB22,BTB23,BTB24,
     .   BTB33,BTB34,BTB44,LC,THK,LTHK
      my_real, DIMENSION(:,:) ,ALLOCATABLE ::
     .   F1,F2,F3,F4,F5,F6,F7,F8,STIFNLTH,DTN,
     .   STI1,STI2,STI3,STI4,STI5,STI6,STI7,STI8
      my_real, POINTER, DIMENSION(:) :: 
     .   VNL,FNL,UNL,STIFNL,MASS,MASS0,VNL0
      my_real, POINTER, DIMENSION(:,:) :: 
     .   MASSTH,UNLTH,VNLTH,FNLTH
      ! Safety coefficient for non-local stability vs mechanical stability
      my_real, PARAMETER :: CSTA  = 40.0D0
      ! Coefficient for non-local stability to take into account damping
      my_real, PARAMETER :: CDAMP = 0.7D0
      my_real
     .   ZS(10,9)      
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
c 
      ! Recover non-local parameters
      L2     = NLOC_DMG%LEN(IMAT)**2 !< Squared internal length 
      XI     = NLOC_DMG%DAMP(IMAT)   !< Damping parameter
      L_NLOC = NLOC_DMG%L_NLOC       !< Size of non-local tables
      ZETA   = NLOC_DMG%DENS(IMAT)   !< Density parameter
      SSPNL  = NLOC_DMG%SSPNL(IMAT)  !< Non-local "Sound speed"
      LE_MAX = NLOC_DMG%LE_MAX(IMAT) !< Maximal length of convergence
c
      ! Allocation of variable table
      ALLOCATE(BTB11(NEL),BTB12(NEL),BTB13(NEL),BTB14(NEL),BTB22(NEL),
     .         BTB23(NEL),BTB24(NEL),BTB33(NEL),BTB34(NEL),BTB44(NEL),
     .         F1(NEL,NLAY),F2(NEL,NLAY),F3(NEL,NLAY),F4(NEL,NLAY),
     .         F5(NEL,NLAY),F6(NEL,NLAY),F7(NEL,NLAY),F8(NEL,NLAY),
     .         POS1(NEL),POS2(NEL),POS3(NEL),POS4(NEL),POS5(NEL),
     .         POS6(NEL),POS7(NEL),POS8(NEL),LC(NEL),THK(NEL),LTHK(NEL))
c 
      ! Degenerated geometry index tables
      NINDX6 = 0
      INDX6(1:NEL) = 0
      NTN6 = SIX*SIX
      NINDX8 = 0
      INDX8(1:NEL) = 0
      NTN8 = EIGHT*EIGHT
c
      ! Local variables initialization
      LC(1:NEL) = ZERO              
      ! For nodal timestep
      IF (NODADT > 0) THEN
        ! Non-local nodal stifness
        ALLOCATE(STI1(NEL,NLAY),STI2(NEL,NLAY),STI3(NEL,NLAY),STI4(NEL,NLAY),
     .           STI5(NEL,NLAY),STI6(NEL,NLAY),STI7(NEL,NLAY),STI8(NEL,NLAY))
        ! Non-local mass
        MASS =>  NLOC_DMG%MASS(1:L_NLOC)
        ! Initial non-local mass
        MASS0 => NLOC_DMG%MASS0(1:L_NLOC)
      ENDIF
      ! Current timestep non-local velocities
      VNL  => NLOC_DMG%VNL(1:L_NLOC)
      ! Previous timestep non-local velocities
      VNL0 => NLOC_DMG%VNL_OLD(1:L_NLOC)
      ! Non-local displacements
      UNL  => NLOC_DMG%UNL(1:L_NLOC)
c
      !--------------------------------------------------------------------------------
      ! Computation of the position of the non-local d.o.fs and the BtB matrix product
      !--------------------------------------------------------------------------------
      ! Loop over elements
      DO I=1,NEL
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
      ENDDO 
c
      ! Loop over elements
      DO I=1,NEL
c
        ! Degenerated brick elements
        IF (NLOCS%NL_ISOLNOD(I) == 6) THEN 
c
          ! Indexing degenerated penta bricks
          NINDX6 = NINDX6 + 1
          INDX6(NINDX6) = I 
c
          ! Recovering the nodes of the brick element
          N1 = NLOC_DMG%IDXI(NLOCS%NL_SOLNOD(1,I))
          N2 = NLOC_DMG%IDXI(NLOCS%NL_SOLNOD(2,I))
          N3 = NLOC_DMG%IDXI(NLOCS%NL_SOLNOD(3,I))
          N4 = NLOC_DMG%IDXI(NLOCS%NL_SOLNOD(4,I))
          N5 = NLOC_DMG%IDXI(NLOCS%NL_SOLNOD(5,I))
          N6 = NLOC_DMG%IDXI(NLOCS%NL_SOLNOD(6,I))
c
          ! Recovering the positions of the first d.o.fs of each nodes
          POS1(I) = NLOC_DMG%POSI(N1)
          POS2(I) = NLOC_DMG%POSI(N2)
          POS3(I) = NLOC_DMG%POSI(N3)
          POS4(I) = NLOC_DMG%POSI(N4)
          POS5(I) = NLOC_DMG%POSI(N5)
          POS6(I) = NLOC_DMG%POSI(N6)
c
        ! Classic hexa brick elements
        ELSEIF (NLOCS%NL_ISOLNOD(I) == 8) THEN
c 
          ! Indexing classic hexa bricks
          NINDX8 = NINDX8 + 1
          INDX8(NINDX8) = I 
c
          ! Recovering the nodes of the brick element
          N1 = NLOC_DMG%IDXI(NLOCS%NL_SOLNOD(1,I))
          N2 = NLOC_DMG%IDXI(NLOCS%NL_SOLNOD(2,I))
          N3 = NLOC_DMG%IDXI(NLOCS%NL_SOLNOD(3,I))
          N4 = NLOC_DMG%IDXI(NLOCS%NL_SOLNOD(4,I))
          N5 = NLOC_DMG%IDXI(NLOCS%NL_SOLNOD(5,I))
          N6 = NLOC_DMG%IDXI(NLOCS%NL_SOLNOD(6,I))
          N7 = NLOC_DMG%IDXI(NLOCS%NL_SOLNOD(7,I))
          N8 = NLOC_DMG%IDXI(NLOCS%NL_SOLNOD(8,I))
c
          ! Recovering the positions of the first d.o.fs of each nodes
          POS1(I) = NLOC_DMG%POSI(N1)
          POS2(I) = NLOC_DMG%POSI(N2)
          POS3(I) = NLOC_DMG%POSI(N3)
          POS4(I) = NLOC_DMG%POSI(N4)
          POS5(I) = NLOC_DMG%POSI(N5)
          POS6(I) = NLOC_DMG%POSI(N6)
          POS7(I) = NLOC_DMG%POSI(N7)
          POS8(I) = NLOC_DMG%POSI(N8)
c
        ENDIF          
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
      ! Loop over layers
      DO K = 1,NLAY
c
        ! Loop over classic hexa bricks
#include "vectorize.inc"
        DO II = 1, NINDX8
c 
          ! Number of the element
          I = INDX8(II)        
c     
          ! If the element is not broken, normal computation
          IF (OFF(I) /= ZERO) THEN 
c
            ! Computing the product NtN*UNL
            NTN_UNL = (UNL(POS1(I)+K-1) + UNL(POS2(I)+K-1) + UNL(POS3(I)+K-1) + UNL(POS4(I)+K-1)
     .              +  UNL(POS5(I)+K-1) + UNL(POS6(I)+K-1) + UNL(POS7(I)+K-1) + UNL(POS8(I)+K-1)) / NTN8
c        
            ! Computing the product NtN*VNL
            NTN_VNL = (VNL(POS1(I)+K-1) + VNL(POS2(I)+K-1) + VNL(POS3(I)+K-1) + VNL(POS4(I)+K-1)
     .              +  VNL(POS5(I)+K-1) + VNL(POS6(I)+K-1) + VNL(POS7(I)+K-1) + VNL(POS8(I)+K-1)) / NTN8
            IF (NODADT > 0) THEN 
              NTN_VNL = (SQRT(MASS(POS1(I)+K-1)/MASS0(POS1(I)+K-1))*VNL(POS1(I)+K-1) + 
     .                   SQRT(MASS(POS2(I)+K-1)/MASS0(POS2(I)+K-1))*VNL(POS2(I)+K-1) + 
     .                   SQRT(MASS(POS3(I)+K-1)/MASS0(POS3(I)+K-1))*VNL(POS3(I)+K-1) + 
     .                   SQRT(MASS(POS4(I)+K-1)/MASS0(POS4(I)+K-1))*VNL(POS4(I)+K-1) +
     .                   SQRT(MASS(POS5(I)+K-1)/MASS0(POS5(I)+K-1))*VNL(POS5(I)+K-1) + 
     .                   SQRT(MASS(POS6(I)+K-1)/MASS0(POS6(I)+K-1))*VNL(POS6(I)+K-1) + 
     .                   SQRT(MASS(POS7(I)+K-1)/MASS0(POS7(I)+K-1))*VNL(POS7(I)+K-1) + 
     .                   SQRT(MASS(POS8(I)+K-1)/MASS0(POS8(I)+K-1))*VNL(POS8(I)+K-1)) / NTN8
            ENDIF
c        
            ! Computation of the product LEN**2 * BtxB
            B1 = L2 * VOL(I) * WS(K,NLAY) *HALF * ( BTB11(I)*UNL(POS1(I)+K-1) + BTB12(I)*UNL(POS2(I)+K-1) 
     .                + BTB13(I)*UNL(POS3(I)+K-1) + BTB14(I)*UNL(POS4(I)+K-1) - BTB13(I)*UNL(POS5(I)+K-1)
     .                - BTB14(I)*UNL(POS6(I)+K-1) - BTB11(I)*UNL(POS7(I)+K-1) - BTB12(I)*UNL(POS8(I)+K-1))
c        
            B2 = L2 * VOL(I) * WS(K,NLAY) *HALF * ( BTB12(I)*UNL(POS1(I)+K-1) + BTB22(I)*UNL(POS2(I)+K-1) 
     .                + BTB23(I)*UNL(POS3(I)+K-1) + BTB24(I)*UNL(POS4(I)+K-1) - BTB23(I)*UNL(POS5(I)+K-1)
     .                - BTB24(I)*UNL(POS6(I)+K-1) - BTB12(I)*UNL(POS7(I)+K-1) - BTB22(I)*UNL(POS8(I)+K-1))
c        
            B3 = L2 * VOL(I) * WS(K,NLAY) *HALF * ( BTB13(I)*UNL(POS1(I)+K-1) + BTB23(I)*UNL(POS2(I)+K-1) 
     .                + BTB33(I)*UNL(POS3(I)+K-1) + BTB34(I)*UNL(POS4(I)+K-1) - BTB33(I)*UNL(POS5(I)+K-1)
     .                - BTB34(I)*UNL(POS6(I)+K-1) - BTB13(I)*UNL(POS7(I)+K-1) - BTB23(I)*UNL(POS8(I)+K-1))
c        
            B4 = L2 * VOL(I) * WS(K,NLAY) *HALF * ( BTB14(I)*UNL(POS1(I)+K-1) + BTB24(I)*UNL(POS2(I)+K-1) 
     .                + BTB34(I)*UNL(POS3(I)+K-1) + BTB44(I)*UNL(POS4(I)+K-1) - BTB34(I)*UNL(POS5(I)+K-1)
     .                - BTB44(I)*UNL(POS6(I)+K-1) - BTB14(I)*UNL(POS7(I)+K-1) - BTB24(I)*UNL(POS8(I)+K-1))
c       
            B5 = L2 * VOL(I) * WS(K,NLAY) *HALF * ( -BTB13(I)*UNL(POS1(I)+K-1)- BTB23(I)*UNL(POS2(I)+K-1) 
     .                - BTB33(I)*UNL(POS3(I)+K-1) - BTB34(I)*UNL(POS4(I)+K-1) + BTB33(I)*UNL(POS5(I)+K-1)
     .                + BTB34(I)*UNL(POS6(I)+K-1) + BTB13(I)*UNL(POS7(I)+K-1) + BTB23(I)*UNL(POS8(I)+K-1))
c        
            B6 = L2 * VOL(I) * WS(K,NLAY) *HALF * ( -BTB14(I)*UNL(POS1(I)+K-1)- BTB24(I)*UNL(POS2(I)+K-1) 
     .                - BTB34(I)*UNL(POS3(I)+K-1) - BTB44(I)*UNL(POS4(I)+K-1) + BTB34(I)*UNL(POS5(I)+K-1)
     .                + BTB44(I)*UNL(POS6(I)+K-1) + BTB14(I)*UNL(POS7(I)+K-1) + BTB24(I)*UNL(POS8(I)+K-1))
c        
            B7 = L2 * VOL(I) * WS(K,NLAY) *HALF * ( -BTB11(I)*UNL(POS1(I)+K-1)- BTB12(I)*UNL(POS2(I)+K-1) 
     .                - BTB13(I)*UNL(POS3(I)+K-1) - BTB14(I)*UNL(POS4(I)+K-1) + BTB13(I)*UNL(POS5(I)+K-1)
     .                + BTB14(I)*UNL(POS6(I)+K-1) + BTB11(I)*UNL(POS7(I)+K-1) + BTB12(I)*UNL(POS8(I)+K-1))
c        
            B8 = L2 * VOL(I) * WS(K,NLAY) *HALF * ( -BTB12(I)*UNL(POS1(I)+K-1)- BTB22(I)*UNL(POS2(I)+K-1) 
     .                - BTB23(I)*UNL(POS3(I)+K-1) - BTB24(I)*UNL(POS4(I)+K-1) + BTB23(I)*UNL(POS5(I)+K-1)
     .                + BTB24(I)*UNL(POS6(I)+K-1) + BTB12(I)*UNL(POS7(I)+K-1) + BTB22(I)*UNL(POS8(I)+K-1))       
c
            ! Multiplication by the volume of the element (and damping parameter XI)  
            NTN_UNL = NTN_UNL * VOL(I) * WS(K,NLAY) * HALF
            NTN_VNL = NTN_VNL * XI * VOL(I) * WS(K,NLAY) * HALF
c
            ! Introducing the internal variable to be regularized
            NTVAR   = VAR_REG(I,K)*ONE_OVER_8* VOL(I) * WS(K,NLAY) * HALF
c
            ! Computing the elementary non-local forces
            A = NTN_UNL + NTN_VNL - NTVAR
            F1(I,K) = A + B1
            F2(I,K) = A + B2
            F3(I,K) = A + B3
            F4(I,K) = A + B4
            F5(I,K) = A + B5
            F6(I,K) = A + B6
            F7(I,K) = A + B7
            F8(I,K) = A + B8
c
            ! Computing nodal equivalent stiffness
            IF (NODADT > 0) THEN 
              STI1(I,K) = (ABS(L2*BTB11(I)  + ONE/NTN8) + ABS(L2*BTB12(I)  + ONE/NTN8) + ABS(L2*BTB13(I)  + ONE/NTN8) +
     .                     ABS(L2*BTB14(I)  + ONE/NTN8) + ABS(-L2*BTB13(I) + ONE/NTN8) + ABS(-L2*BTB14(I) + ONE/NTN8) +
     .                     ABS(-L2*BTB11(I) + ONE/NTN8) + ABS(-L2*BTB12(I) + ONE/NTN8))*VOL(I)*WS(K,NLAY)*HALF
              STI2(I,K) = (ABS(L2*BTB12(I)  + ONE/NTN8) + ABS(L2*BTB22(I)  + ONE/NTN8) + ABS(L2*BTB23(I)  + ONE/NTN8) +
     .                     ABS(L2*BTB24(I)  + ONE/NTN8) + ABS(-L2*BTB23(I) + ONE/NTN8) + ABS(-L2*BTB24(I) + ONE/NTN8) +
     .                     ABS(-L2*BTB12(I) + ONE/NTN8) + ABS(-L2*BTB22(I) + ONE/NTN8))*VOL(I)*WS(K,NLAY)*HALF
              STI3(I,K) = (ABS(L2*BTB13(I)  + ONE/NTN8) + ABS(L2*BTB23(I)  + ONE/NTN8) + ABS(L2*BTB33(I)  + ONE/NTN8) +
     .                     ABS(L2*BTB34(I)  + ONE/NTN8) + ABS(-L2*BTB33(I) + ONE/NTN8) + ABS(-L2*BTB34(I) + ONE/NTN8) +
     .                     ABS(-L2*BTB13(I) + ONE/NTN8) + ABS(-L2*BTB23(I) + ONE/NTN8))*VOL(I)*WS(K,NLAY)*HALF
              STI4(I,K) = (ABS(L2*BTB14(I)  + ONE/NTN8) + ABS(L2*BTB24(I)  + ONE/NTN8) + ABS(L2*BTB34(I)  + ONE/NTN8) +
     .                     ABS(L2*BTB44(I)  + ONE/NTN8) + ABS(-L2*BTB34(I) + ONE/NTN8) + ABS(-L2*BTB44(I) + ONE/NTN8) +
     .                     ABS(-L2*BTB14(I) + ONE/NTN8) + ABS(-L2*BTB24(I) + ONE/NTN8))*VOL(I)*WS(K,NLAY)*HALF
              STI5(I,K) = (ABS(-L2*BTB13(I) + ONE/NTN8) + ABS(-L2*BTB23(I) + ONE/NTN8) + ABS(-L2*BTB33(I) + ONE/NTN8) +
     .                     ABS(-L2*BTB34(I) + ONE/NTN8) + ABS(L2*BTB33(I)  + ONE/NTN8) + ABS(L2*BTB34(I)  + ONE/NTN8) +
     .                     ABS(L2*BTB13(I)  + ONE/NTN8) + ABS(L2*BTB23(I)  + ONE/NTN8))*VOL(I)*WS(K,NLAY)*HALF
              STI6(I,K) = (ABS(-L2*BTB14(I) + ONE/NTN8) + ABS(-L2*BTB24(I) + ONE/NTN8) + ABS(-L2*BTB34(I) + ONE/NTN8) +
     .                     ABS(-L2*BTB44(I) + ONE/NTN8) + ABS(L2*BTB34(I)  + ONE/NTN8) + ABS(L2*BTB44(I) + ONE/NTN8)  +
     .                     ABS(L2*BTB14(I)  + ONE/NTN8) + ABS(L2*BTB24(I)  + ONE/NTN8))*VOL(I)*WS(K,NLAY)*HALF
              STI7(I,K) = (ABS(-L2*BTB11(I) + ONE/NTN8) + ABS(-L2*BTB12(I) + ONE/NTN8) + ABS(-L2*BTB13(I) + ONE/NTN8) +
     .                     ABS(-L2*BTB14(I) + ONE/NTN8) + ABS(L2*BTB13(I)  + ONE/NTN8) + ABS(L2*BTB14(I) + ONE/NTN8)  +
     .                     ABS(L2*BTB11(I)  + ONE/NTN8) + ABS(L2*BTB12(I)  + ONE/NTN8))*VOL(I)*WS(K,NLAY)*HALF    
              STI8(I,K) = (ABS(-L2*BTB12(I) + ONE/NTN8) + ABS(-L2*BTB22(I) + ONE/NTN8) + ABS(-L2*BTB23(I) + ONE/NTN8) +
     .                     ABS(-L2*BTB24(I) + ONE/NTN8) + ABS(L2*BTB23(I)  + ONE/NTN8) + ABS(L2*BTB24(I) + ONE/NTN8)  +
     .                     ABS(L2*BTB12(I)  + ONE/NTN8) + ABS(L2*BTB22(I)  + ONE/NTN8))*VOL(I)*WS(K,NLAY)*HALF
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
     .                       (VNL(POS1(I)+K-1)+VNL0(POS1(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F2(I,K) = SQRT(MASS(POS2(I)+K-1)/MASS0(POS2(I)+K-1))*ZETA*SSPNL*HALF*
     .                       (VNL(POS2(I)+K-1)+VNL0(POS2(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F3(I,K) = SQRT(MASS(POS3(I)+K-1)/MASS0(POS3(I)+K-1))*ZETA*SSPNL*HALF*
     .                       (VNL(POS3(I)+K-1)+VNL0(POS3(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F4(I,K) = SQRT(MASS(POS4(I)+K-1)/MASS0(POS4(I)+K-1))*ZETA*SSPNL*HALF*
     .                       (VNL(POS4(I)+K-1)+VNL0(POS4(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F5(I,K) = SQRT(MASS(POS5(I)+K-1)/MASS0(POS5(I)+K-1))*ZETA*SSPNL*HALF*
     .                       (VNL(POS5(I)+K-1)+VNL0(POS5(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F6(I,K) = SQRT(MASS(POS6(I)+K-1)/MASS0(POS6(I)+K-1))*ZETA*SSPNL*HALF*
     .                       (VNL(POS6(I)+K-1)+VNL0(POS6(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F7(I,K) = SQRT(MASS(POS7(I)+K-1)/MASS0(POS7(I)+K-1))*ZETA*SSPNL*HALF*
     .                       (VNL(POS7(I)+K-1)+VNL0(POS7(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F8(I,K) = SQRT(MASS(POS8(I)+K-1)/MASS0(POS8(I)+K-1))*ZETA*SSPNL*HALF*
     .                       (VNL(POS8(I)+K-1)+VNL0(POS8(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
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
              F1(I,K) = ZETA*SSPNL*HALF*(VNL(POS1(I)+K-1)+VNL0(POS1(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F2(I,K) = ZETA*SSPNL*HALF*(VNL(POS2(I)+K-1)+VNL0(POS2(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F3(I,K) = ZETA*SSPNL*HALF*(VNL(POS3(I)+K-1)+VNL0(POS3(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F4(I,K) = ZETA*SSPNL*HALF*(VNL(POS4(I)+K-1)+VNL0(POS4(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F5(I,K) = ZETA*SSPNL*HALF*(VNL(POS5(I)+K-1)+VNL0(POS5(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F6(I,K) = ZETA*SSPNL*HALF*(VNL(POS6(I)+K-1)+VNL0(POS6(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F7(I,K) = ZETA*SSPNL*HALF*(VNL(POS7(I)+K-1)+VNL0(POS7(I)+K-1))*(THREE/FOUR)*(LC(I)**2)
              F8(I,K) = ZETA*SSPNL*HALF*(VNL(POS8(I)+K-1)+VNL0(POS8(I)+K-1))*(THREE/FOUR)*(LC(I)**2)          
            ENDIF
          ENDIF
        ENDDO
c
        ! Loop over penta degenerated bricks
#include "vectorize.inc"
        DO II = 1, NINDX6
c 
          ! Number of the element
          I = INDX6(II)        
c     
          ! If the element is not broken, normal computation
          IF (OFF(I) /= ZERO) THEN 
c
            ! Computing the product NtN*UNL
            NTN_UNL = (UNL(POS1(I)+K-1) + UNL(POS2(I)+K-1) + UNL(POS3(I)+K-1) +
     .                 UNL(POS4(I)+K-1) + UNL(POS5(I)+K-1) + UNL(POS6(I)+K-1)) / NTN6
c        
            ! Computing the product NtN*VNL
            NTN_VNL = (VNL(POS1(I)+K-1) + VNL(POS2(I)+K-1) + VNL(POS3(I)+K-1) + 
     .                 VNL(POS4(I)+K-1) + VNL(POS5(I)+K-1) + VNL(POS6(I)+K-1)) / NTN6
            IF (NODADT > 0) THEN 
              NTN_VNL = (SQRT(MASS(POS1(I)+K-1)/MASS0(POS1(I)+K-1))*VNL(POS1(I)+K-1) + 
     .                   SQRT(MASS(POS2(I)+K-1)/MASS0(POS2(I)+K-1))*VNL(POS2(I)+K-1) + 
     .                   SQRT(MASS(POS3(I)+K-1)/MASS0(POS3(I)+K-1))*VNL(POS3(I)+K-1) + 
     .                   SQRT(MASS(POS4(I)+K-1)/MASS0(POS4(I)+K-1))*VNL(POS4(I)+K-1) +
     .                   SQRT(MASS(POS5(I)+K-1)/MASS0(POS5(I)+K-1))*VNL(POS5(I)+K-1) + 
     .                   SQRT(MASS(POS6(I)+K-1)/MASS0(POS6(I)+K-1))*VNL(POS6(I)+K-1)) / NTN6
            ENDIF
c        
            ! Computation of the product LEN**2 * BtxB
            B1 = L2 * VOL(I) * WS(K,NLAY) *HALF * ( BTB11(I)*UNL(POS1(I)+K-1) + BTB12(I)*UNL(POS2(I)+K-1) 
     .                + BTB13(I)*UNL(POS3(I)+K-1) - BTB13(I)*UNL(POS4(I)+K-1) - BTB12(I)*UNL(POS5(I)+K-1)
     .                - BTB11(I)*UNL(POS6(I)+K-1) )
c        
            B2 = L2 * VOL(I) * WS(K,NLAY) *HALF * ( BTB12(I)*UNL(POS1(I)+K-1) + BTB22(I)*UNL(POS2(I)+K-1) 
     .                + BTB23(I)*UNL(POS3(I)+K-1) - BTB23(I)*UNL(POS4(I)+K-1) - BTB22(I)*UNL(POS5(I)+K-1)
     .                - BTB12(I)*UNL(POS6(I)+K-1) )
c        
            B3 = L2 * VOL(I) * WS(K,NLAY) *HALF * ( BTB13(I)*UNL(POS1(I)+K-1) + BTB23(I)*UNL(POS2(I)+K-1) 
     .                + BTB33(I)*UNL(POS3(I)+K-1) - BTB33(I)*UNL(POS4(I)+K-1) - BTB23(I)*UNL(POS5(I)+K-1)
     .                - BTB13(I)*UNL(POS6(I)+K-1) )
c        
            B4 = L2 * VOL(I) * WS(K,NLAY) *HALF * (-BTB13(I)*UNL(POS1(I)+K-1) - BTB23(I)*UNL(POS2(I)+K-1) 
     .                - BTB33(I)*UNL(POS3(I)+K-1) + BTB33(I)*UNL(POS4(I)+K-1) + BTB23(I)*UNL(POS5(I)+K-1)
     .                + BTB13(I)*UNL(POS6(I)+K-1) )
c       
            B5 = L2 * VOL(I) * WS(K,NLAY) *HALF * ( -BTB12(I)*UNL(POS1(I)+K-1)- BTB22(I)*UNL(POS2(I)+K-1) 
     .                - BTB23(I)*UNL(POS3(I)+K-1) + BTB23(I)*UNL(POS4(I)+K-1) + BTB22(I)*UNL(POS5(I)+K-1)
     .                + BTB12(I)*UNL(POS6(I)+K-1) )
c        
            B6 = L2 * VOL(I) * WS(K,NLAY) *HALF * ( -BTB11(I)*UNL(POS1(I)+K-1)- BTB12(I)*UNL(POS2(I)+K-1) 
     .                - BTB13(I)*UNL(POS3(I)+K-1) + BTB13(I)*UNL(POS4(I)+K-1) + BTB12(I)*UNL(POS5(I)+K-1)
     .                + BTB11(I)*UNL(POS6(I)+K-1) )  
c
            ! Multiplication by the volume of the element (and the damping parameter XI)  
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
              STI1(I,K) = (ABS(L2*BTB11(I)  + ONE/NTN6) + ABS(L2*BTB12(I)  + ONE/NTN6) + 
     .                     ABS(L2*BTB13(I)  + ONE/NTN6) + ABS(-L2*BTB13(I) + ONE/NTN6) +
     .                     ABS(-L2*BTB12(I) + ONE/NTN6) + ABS(-L2*BTB11(I) + ONE/NTN6))*VOL(I)*WS(K,NLAY)*HALF
              STI2(I,K) = (ABS(L2*BTB12(I)  + ONE/NTN6) + ABS(L2*BTB22(I)  + ONE/NTN6) +
     .                     ABS(L2*BTB23(I)  + ONE/NTN6) + ABS(-L2*BTB23(I) + ONE/NTN6) +
     .                     ABS(-L2*BTB22(I) + ONE/NTN6) + ABS(-L2*BTB12(I) + ONE/NTN6))*VOL(I)*WS(K,NLAY)*HALF
              STI3(I,K) = (ABS(L2*BTB13(I)  + ONE/NTN6) + ABS(L2*BTB23(I)  + ONE/NTN6) + 
     .                     ABS(L2*BTB33(I)  + ONE/NTN6) + ABS(-L2*BTB33(I) + ONE/NTN6) +
     .                     ABS(-L2*BTB23(I) + ONE/NTN6) + ABS(-L2*BTB13(I) + ONE/NTN6))*VOL(I)*WS(K,NLAY)*HALF
              STI4(I,K) = (ABS(-L2*BTB13(I) + ONE/NTN6) + ABS(-L2*BTB23(I) + ONE/NTN6) + 
     .                     ABS(-L2*BTB33(I) + ONE/NTN6) + ABS(L2*BTB33(I)  + ONE/NTN6) +
     .                     ABS(L2*BTB23(I)  + ONE/NTN6) + ABS(L2*BTB13(I)  + ONE/NTN6))*VOL(I)*WS(K,NLAY)*HALF
              STI5(I,K) = (ABS(-L2*BTB12(I) + ONE/NTN6) + ABS(-L2*BTB22(I) + ONE/NTN6) +
     .                     ABS(-L2*BTB23(I) + ONE/NTN6) + ABS(L2*BTB23(I)  + ONE/NTN6) +
     .                     ABS(L2*BTB22(I)  + ONE/NTN6) + ABS(L2*BTB12(I)  + ONE/NTN6))*VOL(I)*WS(K,NLAY)*HALF
              STI6(I,K) = (ABS(-L2*BTB11(I) + ONE/NTN6) + ABS(-L2*BTB12(I) + ONE/NTN6) + 
     .                     ABS(-L2*BTB13(I) + ONE/NTN6) + ABS(L2*BTB13(I)  + ONE/NTN6) +
     .                     ABS(L2*BTB12(I)  + ONE/NTN6) + ABS(L2*BTB11(I)  + ONE/NTN6))*VOL(I)*WS(K,NLAY)*HALF
            ENDIF
c                   
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
c        
        ! Loop over non-local degrees of freedom
        DO K=1,NLAY
          ! Loop over classic hexa brick elements
#include "vectorize.inc"
          DO II=1,NINDX8
            I = INDX8(II)          
            ! Assembling the forces in the classic way 
            FNL(POS1(I)+K-1) = FNL(POS1(I)+K-1) - F1(I,K)
            FNL(POS2(I)+K-1) = FNL(POS2(I)+K-1) - F2(I,K)
            FNL(POS3(I)+K-1) = FNL(POS3(I)+K-1) - F3(I,K)
            FNL(POS4(I)+K-1) = FNL(POS4(I)+K-1) - F4(I,K)
            FNL(POS5(I)+K-1) = FNL(POS5(I)+K-1) - F5(I,K)
            FNL(POS6(I)+K-1) = FNL(POS6(I)+K-1) - F6(I,K)
            FNL(POS7(I)+K-1) = FNL(POS7(I)+K-1) - F7(I,K)
            FNL(POS8(I)+K-1) = FNL(POS8(I)+K-1) - F8(I,K)     
            IF (NODADT > 0) THEN
              ! Spectral radius of stiffness matrix
              MAXSTIF = MAX(STI1(I,K),STI2(I,K),STI3(I,K),STI4(I,K),
     .                      STI5(I,K),STI6(I,K),STI7(I,K),STI8(I,K))
              ! Computing nodal stiffness
              STIFNL(POS1(I)+K-1) = STIFNL(POS1(I)+K-1) + MAXSTIF
              STIFNL(POS2(I)+K-1) = STIFNL(POS2(I)+K-1) + MAXSTIF
              STIFNL(POS3(I)+K-1) = STIFNL(POS3(I)+K-1) + MAXSTIF
              STIFNL(POS4(I)+K-1) = STIFNL(POS4(I)+K-1) + MAXSTIF
              STIFNL(POS5(I)+K-1) = STIFNL(POS5(I)+K-1) + MAXSTIF
              STIFNL(POS6(I)+K-1) = STIFNL(POS6(I)+K-1) + MAXSTIF
              STIFNL(POS7(I)+K-1) = STIFNL(POS7(I)+K-1) + MAXSTIF
              STIFNL(POS8(I)+K-1) = STIFNL(POS8(I)+K-1) + MAXSTIF
            ENDIF
          ENDDO
c
          ! Loop over classic hexa brick elements
#include "vectorize.inc"
          DO II=1,NINDX6
            I = INDX6(II)     
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
          ! Loop over classic hexa bricks
          DO L = 1,NINDX8
            I  = INDX8(L)
            II = I + NFT
c
            ! Spectral radius of stiffness matrix
            IF (NODADT > 0) THEN
              MAXSTIF = MAX(STI1(I,J),STI2(I,J),STI3(I,J),STI4(I,J),
     .                      STI5(I,J),STI6(I,J),STI7(I,J),STI8(I,J))
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
            K = NLOC_DMG%IADS(4,II)
            NLOC_DMG%FSKY(K,J) = -F4(I,J)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
c
            K = NLOC_DMG%IADS(5,II)
            NLOC_DMG%FSKY(K,J) = -F5(I,J)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
c
            K = NLOC_DMG%IADS(6,II)
            NLOC_DMG%FSKY(K,J) = -F6(I,J)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
c
            K = NLOC_DMG%IADS(7,II)
            NLOC_DMG%FSKY(K,J) = -F7(I,J)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
c
            K = NLOC_DMG%IADS(8,II)
            NLOC_DMG%FSKY(K,J) = -F8(I,J)
            IF (NODADT > 0) NLOC_DMG%STSKY(K,J) = MAXSTIF
c
          ENDDO
c
          ! Loop over penta degenerated bricks
          DO L = 1,NINDX6
            I  = INDX6(L)
            II = I + NFT
c
            ! Spectral radius of stiffness matrix
            IF (NODADT > 0) THEN
              MAXSTIF = MAX(STI1(I,J),STI2(I,J),STI3(I,J),
     .                      STI4(I,J),STI5(I,J),STI6(I,J))
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
            IF ((L2>ZERO).AND.(NLAY > 1)) THEN 
              DTNL_TH = (TWO*(MIN(LTHK(I),LE_MAX))*SQRT(THREE*ZETA))/
     .                SQRT(TWELVE*L2 + (MIN(LTHK(I),LE_MAX))**2)
            ELSE 
              DTNL_TH = EP20
            ENDIF
            ! Keeping the minimal value
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
      IF (ALLOCATED(BTB22))    DEALLOCATE(BTB22)
      IF (ALLOCATED(BTB23))    DEALLOCATE(BTB23)      
      IF (ALLOCATED(BTB24))    DEALLOCATE(BTB24)
      IF (ALLOCATED(BTB33))    DEALLOCATE(BTB33)
      IF (ALLOCATED(BTB34))    DEALLOCATE(BTB34)
      IF (ALLOCATED(BTB44))    DEALLOCATE(BTB44)
      IF (ALLOCATED(POS1))     DEALLOCATE(POS1)
      IF (ALLOCATED(POS2))     DEALLOCATE(POS2) 
      IF (ALLOCATED(POS3))     DEALLOCATE(POS3)
      IF (ALLOCATED(POS4))     DEALLOCATE(POS4) 
      IF (ALLOCATED(POS5))     DEALLOCATE(POS5)
      IF (ALLOCATED(POS6))     DEALLOCATE(POS6) 
      IF (ALLOCATED(POS7))     DEALLOCATE(POS7)
      IF (ALLOCATED(POS8))     DEALLOCATE(POS8)
      IF (ALLOCATED(F1))       DEALLOCATE(F1)
      IF (ALLOCATED(F2))       DEALLOCATE(F2) 
      IF (ALLOCATED(F3))       DEALLOCATE(F3)
      IF (ALLOCATED(F4))       DEALLOCATE(F4) 
      IF (ALLOCATED(F5))       DEALLOCATE(F5)
      IF (ALLOCATED(F6))       DEALLOCATE(F6) 
      IF (ALLOCATED(F7))       DEALLOCATE(F7)
      IF (ALLOCATED(F8))       DEALLOCATE(F8)
      IF (ALLOCATED(STI1))     DEALLOCATE(STI1)
      IF (ALLOCATED(STI2))     DEALLOCATE(STI2) 
      IF (ALLOCATED(STI3))     DEALLOCATE(STI3)
      IF (ALLOCATED(STI4))     DEALLOCATE(STI4) 
      IF (ALLOCATED(STI5))     DEALLOCATE(STI5)
      IF (ALLOCATED(STI6))     DEALLOCATE(STI6) 
      IF (ALLOCATED(STI7))     DEALLOCATE(STI7)
      IF (ALLOCATED(STI8))     DEALLOCATE(STI8)
      IF (ALLOCATED(STIFNLTH)) DEALLOCATE(STIFNLTH)
      IF (ALLOCATED(DTN))      DEALLOCATE(DTN)
      IF (ALLOCATED(LC))       DEALLOCATE(LC)
      IF (ALLOCATED(THK))      DEALLOCATE(THK)
      IF (ALLOCATED(LTHK))     DEALLOCATE(LTHK)
c-----------
      END
