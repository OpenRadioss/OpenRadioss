!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
!||====================================================================
!||    inigrav_m51_any_eos    ../starter/source/initial_conditions/inigrav/inigrav_m51_any_eos.F90
!||--- called by ------------------------------------------------------
!||    inigrav_load           ../starter/source/initial_conditions/inigrav/inigrav_load.F
!||--- calls      -----------------------------------------------------
!||    eossolve               ../starter/source/initial_conditions/inigrav/inigrav_m51_any_eos.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE inigrav_m51_any_eos(NELG, NEL , NG   , MATID, IPM, GRAV0, DEPTH, PM,     BUFMAT, ELBUF_TAB,&
                             PSURF,LIST, ALE_CONNECTIVITY,  IX , NIX  , NFT  , BUFMATG, IPARG,&
                             MAT_PARAM, NUMMAT, NPF, TF,NPROPMI, NPROPM,NGROUP, NPARG, SNPC,STF,R0)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      USE ELBUFDEF_MOD
      USE ALE_CONNECTIVITY_MOD
      USE MULTIMAT_PARAM_MOD , ONLY : M51_N0PHAS, M51_NVPHAS
      USE MATPARAM_DEF_MOD
      USE EOSMAIN_MOD , ONLY : EOSMAIN
      USE CONSTANT_MOD , ONLY : ZERO, ONE, EM20
      USE PRECISION_MOD , ONLY : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Includes
! ----------------------------------------------------------------------------------------------------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      INTEGER,INTENT(IN) :: NPROPMI, NPROPM,NGROUP,  NPARG, SNPC,STF !< array sizes
      INTEGER, INTENT(IN) :: NEL, NG, MATID, IPM(NPROPMI, *),LIST(NEL),NELG,IX(NIX,*),NFT,NIX,IPARG(NPARG,NGROUP), NUMMAT
      real(kind=WP), INTENT(IN) :: GRAV0, DEPTH(*), PM(NPROPM, *), BUFMAT(*),PSURF,BUFMATG(*)
      TYPE(ELBUF_STRUCT_), DIMENSION(NGROUP), TARGET, INTENT(IN) :: ELBUF_TAB
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT), INTENT(IN) :: MAT_PARAM
      INTEGER, INTENT(IN) :: NPF(SNPC)
      real(kind=WP), INTENT(IN) :: TF(STF)
      real(kind=WP), INTENT(IN) :: R0
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
      INTEGER :: I, K1, K2, K3, K4, K,J,IFORM,IFORMv,IV,IADBUF,ML,NGv,KTY,KLT,N,MFT,IS,NELGv
      real(kind=WP) :: PGRAV, RHO0,ALPHA1, ALPHA2, ALPHA3, ALPHA4,PEXT
      real(kind=WP) :: RHO10, RHO20, RHO30, RHO40, RHO1, RHO2, RHO3, RHO4, MU
      real(kind=WP) :: EINT1, EINT2, EINT3, EINT4, VOL, VOL1, VOL2, VOL3,VOL4
      real(kind=WP) :: EINT10, EINT20, EINT30, EINT40
      TYPE(G_BUFEL_), POINTER :: GBUF
      TYPE(BUF_MAT_) ,POINTER :: MBUF,MBUFv
      INTEGER :: IAD
      INTEGER :: PARENT_MID, SUBMAT_MID1, SUBMAT_MID2, SUBMAT_MID3, SUBMAT_MID4
      INTEGER :: EOSTYP1, EOSTYP2, EOSTYP3, EOSTYP4
      real(kind=WP) :: ABS_GRAV0
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

! LIST IS SUBGROUP TO TREAT : ONLY ELEM WITH RELEVANT PARTS ARE KEPT
! NEL IS ISEZ OF LIST
! NELG IS SIZE OF ORIGINAL GROUP : needed to shift indexes in GBUF%SIG & MBUF%VAR

      ABS_GRAV0 = ABS(GRAV0)
      !Global buffer
      GBUF => ELBUF_TAB(NG)%GBUF
      !Material buffer
      MBUF  => ELBUF_TAB(NG)%BUFLY(1)%MAT(1,1,1)

      IFORM = NINT(BUFMAT(31))
      K1 = M51_N0PHAS + (1 - 1) * M51_NVPHAS
      K2 = M51_N0PHAS + (2 - 1) * M51_NVPHAS
      K3 = M51_N0PHAS + (3 - 1) * M51_NVPHAS
      K4 = M51_N0PHAS + (4 - 1) * M51_NVPHAS
      !NUVAR = IPM(8, MATID)
      PEXT  = BUFMAT(8)
      IF(IFORM /= 6)THEN
      !all value are in UPARAM=>BUFMAT()
        !Initial densities
        RHO10 = BUFMAT(9)
        RHO20 = BUFMAT(10)
        RHO30 = BUFMAT(11)
        RHO40 = BUFMAT(12)
        !Initial energies
        EINT10 = BUFMAT(32)
        EINT20 = BUFMAT(33)
        EINT30 = BUFMAT(34)
        EINT40 = BUFMAT(48)
        !Get submat material IDs and EoS types from MAT_PARAM
        PARENT_MID = MATID
        SUBMAT_MID1 = 0
        SUBMAT_MID2 = 0
        SUBMAT_MID3 = 0
        SUBMAT_MID4 = 0
        IF(MAT_PARAM(PARENT_MID)%MULTIMAT%NB >= 1) SUBMAT_MID1 = MAT_PARAM(PARENT_MID)%MULTIMAT%MID(1)
        IF(MAT_PARAM(PARENT_MID)%MULTIMAT%NB >= 2) SUBMAT_MID2 = MAT_PARAM(PARENT_MID)%MULTIMAT%MID(2)
        IF(MAT_PARAM(PARENT_MID)%MULTIMAT%NB >= 3) SUBMAT_MID3 = MAT_PARAM(PARENT_MID)%MULTIMAT%MID(3)
        IF(MAT_PARAM(PARENT_MID)%MULTIMAT%NB >= 4) SUBMAT_MID4 = MAT_PARAM(PARENT_MID)%MULTIMAT%MID(4)
      ENDIF
      DO K = 1, NEL
         I = LIST(K)
         IF(IFORM==6)THEN
         !value are not in buffer UPARAM=>BUFMAT but are cell dependent
           ML     = 0
           IFORMv = 0
           IAD = ALE_CONNECTIVITY%ee_connect%iad_connect(I + NFT)
           DO J=1,ALE_CONNECTIVITY%ee_connect%iad_connect(I + NFT + 1) - ALE_CONNECTIVITY%ee_connect%iad_connect(I + NFT)
             IV = ALE_CONNECTIVITY%ee_connect%connected(IAD + J - 1)
             IFORMv = 1000
             IF(IV/=0)            ML     = NINT(PM(19,IX(1,IV)))
             IF(IV/=0)            IADBUF = IPM(7,IX(1,IV))
             IF(IV/=0.AND.ML==51) IFORMv  = BUFMATG(IADBUF+31-1)
             IF(ML==51.AND.IFORMv<=1)EXIT
           ENDDO
           DO N=1,NGROUP
              KTY = IPARG(5,N)
              KLT = IPARG(2,N)
              MFT = IPARG(3,N)
              IF (KTY==1 .AND. IV<=KLT+MFT) EXIT
           ENDDO
           IF (KTY/=1 .OR. IV>KLT+MFT) CYCLE
           NGv   = N
           MBUFv => ELBUF_TAB(NGv)%BUFLY(1)%MAT(1,1,1)
           NELGv = KLT
           IS    = IV-MFT
           RHO10 = BUFMATG(IADBUF-1+09)
           RHO20 = BUFMATG(IADBUF-1+10)
           RHO30 = BUFMATG(IADBUF-1+11)
           RHO40 = BUFMATG(IADBUF-1+12)
           !energies
           EINT1 = BUFMATG(IADBUF-1+32)
           EINT2 = BUFMATG(IADBUF-1+33)
           EINT3 = BUFMATG(IADBUF-1+34)
           EINT4 = BUFMATG(IADBUF-1+48)
           !Get submat material IDs from the neighbor's parent material
           PARENT_MID = IX(1,IV)
           SUBMAT_MID1 = 0
           SUBMAT_MID2 = 0
           SUBMAT_MID3 = 0
           SUBMAT_MID4 = 0
           IF(MAT_PARAM(PARENT_MID)%MULTIMAT%NB >= 1) SUBMAT_MID1 = MAT_PARAM(PARENT_MID)%MULTIMAT%MID(1)
           IF(MAT_PARAM(PARENT_MID)%MULTIMAT%NB >= 2) SUBMAT_MID2 = MAT_PARAM(PARENT_MID)%MULTIMAT%MID(2)
           IF(MAT_PARAM(PARENT_MID)%MULTIMAT%NB >= 3) SUBMAT_MID3 = MAT_PARAM(PARENT_MID)%MULTIMAT%MID(3)
           IF(MAT_PARAM(PARENT_MID)%MULTIMAT%NB >= 4) SUBMAT_MID4 = MAT_PARAM(PARENT_MID)%MULTIMAT%MID(4)
           !vol frac
           ALPHA1 = MBUFv%VAR(IS + (K1 + 23 - 1) * NELGv)
           ALPHA2 = MBUFv%VAR(IS + (K2 + 23 - 1) * NELGv)
           ALPHA3 = MBUFv%VAR(IS + (K3 + 23 - 1) * NELGv)
           ALPHA4 = MBUFv%VAR(IS + (K4 + 23 - 1) * NELGv)
         ELSE
           EINT1 = EINT10
           EINT2 = EINT20
           EINT3 = EINT30
           EINT4 = EINT40
           !Volumic fractions
           ALPHA1 = MBUF%VAR(I + (K1 + 23 - 1) * NELG)
           ALPHA2 = MBUF%VAR(I + (K2 + 23 - 1) * NELG)
           ALPHA3 = MBUF%VAR(I + (K3 + 23 - 1) * NELG)
           ALPHA4 = MBUF%VAR(I + (K4 + 23 - 1) * NELG)
         ENDIF

      !Mean initial density
         RHO0 = ALPHA1 * RHO10 + ALPHA2 * RHO20 + ALPHA3 * RHO30 + ALPHA4 * RHO40
         IF(R0 > ZERO) RHO0=R0
      !Hydrostatic pressure
         PGRAV = PSURF + RHO0  * ABS_GRAV0 * DEPTH(K)
      !Solve for partial densities
      !  Skip phases with negligible volume fraction to avoid numerical issues
      !  (division by near-zero volume in EOSSOLVE). Inspired by law 151 treatment.
         VOL  = GBUF%VOL(I)

         IF (ALPHA1 > EM20) THEN
           VOL1 = ALPHA1*VOL
           CALL EOSSOLVE(PGRAV, EINT1, MU, RHO10, VOL1, PEXT,&
                SUBMAT_MID1,  MAT_PARAM, NUMMAT,&
                PM, IPM, NPF, TF, BUFMATG, NPROPM,NPROPMI,SNPC,STF)
           RHO1 = RHO10 * (MU + ONE)
         ELSE
           RHO1 = RHO10
         ENDIF

         IF (ALPHA2 > EM20) THEN
           VOL2 = ALPHA2*VOL
           CALL EOSSOLVE(PGRAV, EINT2, MU, RHO20, VOL2, PEXT,&
                SUBMAT_MID2,  MAT_PARAM, NUMMAT,&
                PM, IPM, NPF, TF, BUFMATG,NPROPM,NPROPMI,SNPC,STF)
           RHO2 = RHO20 * (MU + ONE)
         ELSE
           RHO2 = RHO20
         ENDIF

         IF (ALPHA3 > EM20) THEN
           VOL3 = ALPHA3*VOL
           CALL EOSSOLVE(PGRAV, EINT3, MU, RHO30, VOL3, PEXT,&
                SUBMAT_MID3,  MAT_PARAM, NUMMAT,&
                PM, IPM, NPF, TF, BUFMATG, NPROPM,NPROPMI,SNPC,STF)
           RHO3 = RHO30 * (MU + ONE)
         ELSE
           RHO3 = RHO30
         ENDIF

         IF (ALPHA4 > EM20) THEN
           VOL4 = ALPHA4*VOL
           CALL EOSSOLVE(PGRAV, EINT4, MU, RHO40, VOL4, PEXT,&
                SUBMAT_MID4,  MAT_PARAM, NUMMAT,&
                PM, IPM, NPF, TF, BUFMATG, NPROPM,NPROPMI,SNPC,STF)
           RHO4 = RHO40 * (MU + ONE)
         ELSE
           RHO4 = RHO40
         ENDIF
      !Store partial densities
         MBUF%VAR(I + (K1 + 09 - 1) * NELG) = RHO1  !initialize rho(t=0)
         MBUF%VAR(I + (K2 + 09 - 1) * NELG) = RHO2
         MBUF%VAR(I + (K3 + 09 - 1) * NELG) = RHO3
         MBUF%VAR(I + (K4 + 09 - 1) * NELG) = RHO4
         MBUF%VAR(I + (K1 + 12 - 1) * NELG) = RHO1  !initialize rhoOLD(t=0)
         MBUF%VAR(I + (K2 + 12 - 1) * NELG) = RHO2
         MBUF%VAR(I + (K3 + 12 - 1) * NELG) = RHO3
         MBUF%VAR(I + (K4 + 12 - 1) * NELG) = RHO4
         MBUF%VAR(I + (K1 + 20 - 1) * NELG) = RHO1  !keep rho0 (initial state is element dependent)
         MBUF%VAR(I + (K2 + 20 - 1) * NELG) = RHO2
         MBUF%VAR(I + (K3 + 20 - 1) * NELG) = RHO3
         MBUF%VAR(I + (K4 + 20 - 1) * NELG) = RHO4
      !Store pressures
         MBUF%VAR(I + (K1 + 18 - 1) * NELG) = PGRAV !P(t=0)
         MBUF%VAR(I + (K2 + 18 - 1) * NELG) = PGRAV
         MBUF%VAR(I + (K3 + 18 - 1) * NELG) = PGRAV
         MBUF%VAR(I + (K4 + 18 - 1) * NELG) = PGRAV
      !Internal energies
         IF (RHO10 /= ZERO) THEN
            MBUF%VAR(I + (K1 + 08 - 1) * NELG) = EINT1 * RHO1 / RHO10 !rho.e(t)
            MBUF%VAR(I + (K1 + 21 - 1) * NELG) = EINT1 * RHO1 / RHO10 !rho0.e0
         ELSE
            MBUF%VAR(I + (K1 + 08 - 1) * NELG) = ZERO
            MBUF%VAR(I + (K1 + 21 - 1) * NELG) = ZERO
         ENDIF
         IF (RHO20 /= ZERO) THEN
            MBUF%VAR(I + (K2 + 08 - 1) * NELG) = EINT2 * RHO2 / RHO20 !rho.e(t)
            MBUF%VAR(I + (K2 + 21 - 1) * NELG) = EINT2 * RHO2 / RHO20 !rho0.e0
         ELSE
            MBUF%VAR(I + (K2 + 08 - 1) * NELG) = ZERO
            MBUF%VAR(I + (K2 + 21 - 1) * NELG) = ZERO
         ENDIF
         IF (RHO30 /= ZERO) THEN
            MBUF%VAR(I + (K3 + 08 - 1) * NELG) = EINT3 * RHO3 / RHO30 !rho.e(t)
            MBUF%VAR(I + (K3 + 21 - 1) * NELG) = EINT3 * RHO3 / RHO30 !rho0.e0
         ELSE
            MBUF%VAR(I + (K3 + 08 - 1) * NELG) = ZERO
            MBUF%VAR(I + (K3 + 21 - 1) * NELG) = ZERO
         ENDIF

         GBUF%RHO(I)            = ALPHA1 * RHO1 + ALPHA2 * RHO2 + ALPHA3 * RHO3 + ALPHA4 * RHO4
         GBUF%EINT(I)           = ALPHA1 * EINT1+ ALPHA2 * EINT2+ ALPHA3 * EINT3+ ALPHA4 * EINT4
        
         GBUF%SIG(I)            = - PGRAV
         GBUF%SIG(I + NELG)     = - PGRAV
         GBUF%SIG(I + 2 * NELG) = - PGRAV
         
         MBUF%VAR(I + 3 * NELG) = PGRAV

      ENDDO
      
      END SUBROUTINE inigrav_m51_any_eos

!||====================================================================
!||    eossolve              ../starter/source/initial_conditions/inigrav/inigrav_m51_any_eos.F90
!||--- called by ------------------------------------------------------
!||    inigrav_m51_any_eos   ../starter/source/initial_conditions/inigrav/inigrav_m51_any_eos.F90
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
      SUBROUTINE EOSSOLVE(PRES, EINT, MU, RHO0, VOL0, PEXT,&
                          SUBMAT_MID, MAT_PARAM, NUMMAT,&
                          PM, IPM, NPF, TF, BUFMAT, NPROPM,NPROPMI,SNPC,STF)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      USE EOSMAIN_MOD , ONLY : EOSMAIN
      USE MATPARAM_DEF_MOD
      USE PRECISION_MOD , ONLY : WP
      USE CONSTANT_MOD , ONLY : ZERO, ONE, EM4, EM15
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      IMPLICIT NONE
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Includes
! ----------------------------------------------------------------------------------------------------------------------
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      INTEGER,INTENT(IN) :: NPROPM,NPROPMI,SNPC,STF
      real(kind=WP), INTENT(IN)    :: PRES, PEXT
      real(kind=WP), INTENT(OUT)   :: MU
      real(kind=WP), INTENT(INOUT) :: RHO0, EINT, VOL0
      INTEGER, INTENT(IN)    :: SUBMAT_MID, NUMMAT
      TYPE(MATPARAM_STRUCT_) ,DIMENSION(NUMMAT), INTENT(IN) :: MAT_PARAM
      real(kind=WP), INTENT(IN)    :: PM(NPROPM, *), BUFMAT(*)
      INTEGER, INTENT(IN)    :: IPM(NPROPMI, *), NPF(SNPC)
      real(kind=WP), INTENT(IN)    :: TF(STF)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
      real(kind=WP) :: TOL, ERROR, INCR, VOL, VOL_prev, DFUNC
      INTEGER :: ITER, MAX_ITER, EOSTYP, NVAREOS, MLW, NVARTMP
      LOGICAL :: CONT
      !arrays for EOSMAIN (nel=1)
      INTEGER, PARAMETER :: NEL = 1
      real(kind=WP) :: OFF_L(1), EINT_L(1), RHO_L(1), RHO0_L(1)
      real(kind=WP) :: MU_L(1), MU2_L(1), ESPE_L(1), DVOL_L(1), DF_L(1)
      real(kind=WP) :: VNEW_L(1), PSH_L(1), PNEW_L(1), DPDM_L(1), DPDE_L(1)
      real(kind=WP) :: THETA_L(1), SIG_L(1,6), MU_BAK_L(1), BFRAC_L(1)
      real(kind=WP) :: VAREOS_L(6)
      real(kind=WP) :: FUNC
      INTEGER :: MAT_L(1)
      INTEGER :: VARTMP_L(1,3)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      !Skip if no material or zero volume
      MU = ZERO
      IF(SUBMAT_MID <= 0) RETURN
      IF(RHO0 == ZERO) RETURN
      IF(VOL0 == ZERO) VOL0 = ONE

      !Get EoS type from the submat material parameters
      EOSTYP = MAT_PARAM(SUBMAT_MID)%IEOS
      IF(EOSTYP <= 0) EOSTYP = IPM(4, SUBMAT_MID)
      IF(EOSTYP <= 0) RETURN
      MLW = MAT_PARAM(SUBMAT_MID)%ILAW

      !Initialize solver
      ITER        = 0
      MAX_ITER    = 30
      TOL         = EM4
      CONT        = .TRUE.
      VOL         = VOL0
      VOL_prev    = VOL0
      MU          = ZERO
      NVAREOS     = 6
      NVARTMP     = 3

      !Initialize EOSMAIN arrays
      OFF_L(1)    = ONE
      RHO0_L(1)   = RHO0
      PSH_L(1)    = PEXT
      PNEW_L(1)   = ZERO
      DPDM_L(1)   = ZERO
      DPDE_L(1)   = ZERO
      THETA_L(1)  = ZERO
      DVOL_L(1)   = ZERO
      MAT_L(1)    = SUBMAT_MID
      MU_BAK_L(1) = ZERO
      BFRAC_L(1)  = ZERO
      SIG_L(1,:)  = ZERO
      VAREOS_L(:) = ZERO
      VARTMP_L(:,:) = 1

      DO WHILE (CONT .AND. ITER < MAX_ITER)
         !Prepare EOSMAIN inputs
         MU_L(1)    = MU
         MU2_L(1)   = MAX(ZERO,MU)*MU
         RHO_L(1)   = RHO0*(ONE+MU)
         DF_L(1)    = RHO0 / RHO_L(1)
         VNEW_L(1)  = VOL0 / (ONE+MU)
          EINT_L(1)  = EINT/DF_L(1)  !/ MAX(EM15, VNEW_L(1))   ! iflag=2 : convert total energy to specific energy (per unit volume)
         ! ESPE_L(1)  = DF_L(1) * EINT_L(1) will be updated in eosmain

         !Call EOSMAIN to get pressure and derivatives
         CALL EOSMAIN(2     , NEL    , EOSTYP , PM    , OFF_L , EINT_L,&
                      RHO_L , RHO0_L , MU_L   , MU2_L , ESPE_L,&
                      DVOL_L, DF_L   , VNEW_L , MAT_L , PSH_L ,&
                      PNEW_L, DPDM_L , DPDE_L , THETA_L,&
                      BUFMAT, SIG_L  , MU_BAK_L, MLW  ,&
                      NPF   , TF     , VAREOS_L, NVAREOS,&
                      MAT_PARAM(SUBMAT_MID),&
                      BFRAC_L, NVARTMP, VARTMP_L)

         FUNC = PNEW_L(1) - PRES

         !Newton-Raphson : find MU such that P(MU) = PRES
         DFUNC = DPDM_L(1) !/ RHO0
         IF(DFUNC == ZERO) EXIT

         INCR = - FUNC / DFUNC
         IF(INCR == ZERO) EXIT  !P(mu=0)=PRES nothing to do

         ERROR = ABS(FUNC)
         MU = MU + INCR !/ RHO0

         !Energy increment along isentropic path
         VOL = VOL0 / (ONE + MU)
         EINT = EINT - (PEXT + PNEW_L(1)) * (VOL - VOL_prev) / VOL0
         VOL_prev = VOL

         ITER = ITER + 1
         IF (ERROR < TOL) THEN
           CONT = .FALSE.
         ENDIF
      ENDDO

      END SUBROUTINE EOSSOLVE
