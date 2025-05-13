      !||====================================================================
      !||    multi_solve_eint_mod   ../starter/source/initial_conditions/inimap/multi_solve_eint.F90
      !||--- called by ------------------------------------------------------
      !||    ini_inimap1d           ../starter/source/initial_conditions/inimap/ini_inimap1d.F
      !||====================================================================
      MODULE MULTI_SOLVE_EINT_MOD
      CONTAINS
      !||====================================================================
      !||    multi_solve_eint   ../starter/source/initial_conditions/inimap/multi_solve_eint.F90
      !||--- called by ------------------------------------------------------
      !||    ini_inimap1d       ../starter/source/initial_conditions/inimap/ini_inimap1d.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      SUBROUTINE MULTI_SOLVE_EINT(MATID, NFT,    NEL, PRESIN, EINT, RHO,   IFIRST, ILAST, ELEM_LIST, IPM, &
                                  PM   , BUFMAT, MLW, THETA, SNPC, STF ,NPC,    TF, VAREOS, NVAREOS, &
                                  MAT_PARAM, NVARTMP, VARTMP, NUMMAT, NPROPMI, NPROPM)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      USE MATPARAM_DEF_MOD
      USE CONSTANT_MOD , ONLY : EP20, EM06, ZERO, ONE
      USE EOSMAIN_MOD , ONLY : EOSMAIN
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit None
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included Files
! ----------------------------------------------------------------------------------------------------------------------
#include      "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      INTEGER, INTENT(IN) :: MATID, NFT, NEL, IFIRST, ILAST, ELEM_LIST(*), IPM(NPROPMI, NUMMAT), MLW, NVAREOS
      INTEGER, INTENT(IN) :: SNPC,STF,NUMMAT,NPROPMI,NPROPM !< array sizes
      my_real, INTENT(IN) :: PM(NPROPM, NUMMAT), BUFMAT(*), RHO(NEL), PRESIN(NEL)
      my_real, INTENT(OUT) :: EINT(NEL)
      my_real, INTENT(INOUT) :: THETA(NEL), VAREOS(NVAREOS*NEL)
      INTEGER,INTENT(IN)::NPC(SNPC)
      my_real,INTENT(IN)::TF(STF)
      TYPE(MATPARAM_STRUCT_) ,INTENT(IN) :: MAT_PARAM
      INTEGER,INTENT(IN) :: NVARTMP
      INTEGER,INTENT(INOUT) :: VARTMP(NEL,NVARTMP)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
      INTEGER :: II, ELEMID, EOSTYPE,LOCALID, REMAINING_ELTS, ITER, MAX_ITER
      INTEGER :: MAT(NEL)
      my_real :: BID(NEL) ! bfrac not used in this context
      my_real :: VOL(NEL), ERROR(NEL), TOL, OFF(NEL), RHOZERO(NEL), RHO0
      my_real :: MU(NEL), DF(NEL), MU2(NEL), ESPE(NEL), DVOL(NEL), PSH(NEL)
      my_real :: SSP(NEL), DPDE(NEL), ECOLD(NEL), PRES(NEL), POLD(NEL)
      my_real :: FUNC, DFUNC, INCR,SIG(NEL,6),MUOLD(NEL)
      LOGICAL :: CONVERGED(NEL)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   PreCondition
! ----------------------------------------------------------------------------------------------------------------------
      IF(IFIRST>ILAST .OR. IFIRST<1)RETURN !DO NO INIT TO 0.
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      EOSTYPE = MAT_PARAM%IEOS  ! should use that except law5 with internal EOS
      EOSTYPE = IPM(4, MATID)
      RHO0 = PM(1, MATID)

      VOL(1:NEL) = ZERO
      OFF(1:NEL) = ONE
      DVOL(1:NEL) = ZERO
      ERROR(1:NEL) = ZERO
      CONVERGED(1:NEL) = .TRUE.
      REMAINING_ELTS = 0
      RHOZERO(1:NEL) = RHO0
      MAT(1:NEL) = MATID
      PSH(1:NEL) = PM(88, MATID)
      SIG(1:NEL,1)=-PM(31,MATID)
      SIG(1:NEL,2)=-PM(31,MATID)
      SIG(1:NEL,3)=-PM(31,MATID)
      SIG(1:NEL,4)= ZERO
      SIG(1:NEL,5)= ZERO
      SIG(1:NEL,6)= ZERO
      MUOLD(1:NEL)   = ZERO
      THETA(1:NEL) = ZERO

      DO II = IFIRST, ILAST
         ELEMID = ELEM_LIST(II)
         LOCALID = ELEMID - NFT
         VOL(LOCALID) = ONE
         ERROR(LOCALID) = EP20
         CONVERGED(LOCALID) = .FALSE.
         REMAINING_ELTS = REMAINING_ELTS + 1
      ENDDO

      MAX_ITER = 30
      ITER = 0
      TOL = EM06
      POLD(1:NEL)=PRESIN(1:NEL)
      DO II=1,NEL
        SIG(II,1:3)=-POLD(II)
      ENDDO
      DO WHILE(REMAINING_ELTS  /=  0 .AND. ITER  <  MAX_ITER) 
         ITER = ITER + 1
         DO II = IFIRST, ILAST
            ELEMID = ELEM_LIST(II)
            LOCALID = ELEMID - NFT
            MU(LOCALID) = RHO(LOCALID) / RHOZERO(LOCALID) - ONE
            DF(LOCALID) = RHOZERO(LOCALID) / RHO(LOCALID)
         ENDDO
         
         CALL EOSMAIN(2, NEL, EOSTYPE, PM, OFF, EINT(1:NEL), &
             RHO(1:NEL), RHOZERO, MU, MU2, ESPE, &
             DVOL, DF, VOL, MAT, PSH, &
             PRES, SSP, DPDE, THETA, ECOLD, BUFMAT, SIG, MUOLD, MLW, POLD, NPC, TF, VAREOS, NVAREOS, MAT_PARAM, &
             BID, NVARTMP, VARTMP)
         
         DO II = IFIRST, ILAST
            ELEMID = ELEM_LIST(II)
            LOCALID = ELEMID - NFT
            IF (.NOT. CONVERGED(LOCALID)) THEN
               FUNC  = PRES(LOCALID) - PRESIN(LOCALID)
               DFUNC = DPDE(LOCALID) / (ONE + MU(LOCALID))      !variable is EINT=rho.e, but EoS solves P(mu,E:=rho0.e) and returns dPdE.  variable change : d(rho.e)=(1+mu)d(rho0.e)=(1+mu)dE  => dP/d(rho.e) = dPdE/(1+mu)
               IF (DFUNC  ==  ZERO) THEN
                  REMAINING_ELTS = REMAINING_ELTS - 1
                  CONVERGED(LOCALID) = .TRUE.
                  VOL(LOCALID) = ZERO
               ELSE
                  INCR = - FUNC / DFUNC
                  ERROR(LOCALID) = ABS(INCR) / ABS(INCR + EINT(LOCALID))
                  EINT(LOCALID) = EINT(LOCALID) + INCR
                  IF (ERROR(LOCALID)  <  TOL) THEN
                     REMAINING_ELTS = REMAINING_ELTS - 1
                     CONVERGED(LOCALID) = .TRUE.
                     VOL(LOCALID) = ZERO
                  ENDIF
               ENDIF
            ENDIF
         ENDDO
      ENDDO
      
      IF (REMAINING_ELTS  /=  0) THEN
         PRINT*, "*** Convergence issue in /INIMAP1D or /INIMAP2D option"
      ENDIF

      END SUBROUTINE MULTI_SOLVE_EINT
      END MODULE MULTI_SOLVE_EINT_MOD
