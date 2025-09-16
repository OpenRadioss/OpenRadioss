Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!||====================================================================
!||    multi_submatlaw_mod              ../engine/source/multifluid/multi_submatlaw.F
!||--- called by ------------------------------------------------------
!||    multi_inlet_ebcs                 ../engine/source/multifluid/multi_inlet_ebcs.F
!||    multi_muscl_compute_pressure     ../engine/source/multifluid/multi_muscl_compute_pressure.F90
!||    multi_muscl_fluxes_computation   ../engine/source/multifluid/multi_muscl_fluxes_computation.F
!||    multi_nrf_ebcs                   ../engine/source/multifluid/multi_nrf_ebcs.F
!||    multi_pressure_equilibrium       ../engine/source/multifluid/multi_pressure_equilibrium.F
!||    multi_solve_eint                 ../engine/source/multifluid/multi_solve_eint.F90
!||====================================================================
      MODULE MULTI_SUBMATLAW_MOD
      CONTAINS
!||====================================================================
!||    multi_submatlaw                ../engine/source/multifluid/multi_submatlaw.F
!||--- called by ------------------------------------------------------
!||    multi_inlet_ebcs               ../engine/source/multifluid/multi_inlet_ebcs.F
!||    multi_muscl_compute_pressure   ../engine/source/multifluid/multi_muscl_compute_pressure.F90
!||    multi_nrf_ebcs                 ../engine/source/multifluid/multi_nrf_ebcs.F
!||    multi_pressure_equilibrium     ../engine/source/multifluid/multi_pressure_equilibrium.F
!||    multi_solve_eint               ../engine/source/multifluid/multi_solve_eint.F90
!||--- calls      -----------------------------------------------------
!||    arret                          ../engine/source/system/arret.F
!||    eosmain                        ../common_source/eos/eosmain.F
!||--- uses       -----------------------------------------------------
!||    alemuscl_mod                   ../common_source/modules/ale/alemuscl_mod.F
!||    eosmain_mod                    ../common_source/eos/eosmain.F
!||    matparam_def_mod               ../common_source/modules/mat_elem/matparam_def_mod.F90
!||====================================================================
      SUBROUTINE MULTI_SUBMATLAW(
     1   IFLAG,       MATLAW,      LOCAL_MATID, NEL,
     2   EINT,        PRES,        RHO,         SSP,
     3   VOL,         GRUN,        PM,          IPM,
     4   NPROPM,      NPROPMI,     BUFMAT,      OFF,
     5   THETA,       BURNFRAC,    BURNTIME,    DELTAX,
     6   CURRENT_TIME,SIGOLD,      SNPC,        STF,
     7   NPF,         TF,          VAREOS,      NVAREOS,
     8   MAT_PARAM,   NVARTMP_EOS, VARTMP_EOS,  NUMMAT,
     9   ABURN)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MATPARAM_DEF_MOD , ONLY : MATPARAM_STRUCT_
      USE EOSMAIN_MOD , ONLY : EOSMAIN
      USE ALEMUSCL_MOD , only : ALEMUSCL_Param
C-----------------------------------------------
C     I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C     C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com08_c.inc"
C-----------------------------------------------
C     D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER,INTENT(IN) :: SNPC, STF,NUMMAT
      INTEGER, INTENT(IN) :: IFLAG
      INTEGER, INTENT(IN) :: MATLAW, LOCAL_MATID, NEL, NPROPM, NPROPMI
      my_real, INTENT(IN) :: PM(NPROPM, NUMMAT),SIGOLD(NEL,6)
      INTEGER, INTENT(IN) :: IPM(NPROPMI, NUMMAT)
      my_real, INTENT(IN) :: VOL(NEL), OFF(NEL)
      my_real, INTENT(OUT) :: GRUN(NEL)
      my_real, INTENT(INOUT) :: EINT(NEL), RHO(NEL), THETA(NEL)
      my_real, INTENT(OUT) :: PRES(NEL), SSP(NEL)
      my_real, INTENT(IN) :: BURNTIME(NEL), DELTAX(NEL), CURRENT_TIME
      my_real, INTENT(INOUT) :: BURNFRAC(NEL), BUFMAT(*)
      INTEGER, INTENT(IN) :: NPF(SNPC),NVAREOS
      my_real, INTENT(IN) :: TF(STF),VAREOS(NVAREOS*NEL)
      TYPE(MATPARAM_STRUCT_), INTENT(IN) :: MAT_PARAM !material data structure
      INTEGER,INTENT(IN) :: NVARTMP_EOS
      INTEGER,INTENT(INOUT) :: VARTMP_EOS(NEL,NVARTMP_EOS)
      my_real,INTENT(INOUT) :: ABURN(NEL) !< after burning
C-----------------------------------------------
C     L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: II, EOSTYPE
      my_real :: C0, C1, RHO0(NEL)
      my_real :: MU(NEL), MU2(NEL), ESPE(NEL), DPDE(NEL), ECOLD(NEL), DVOL(NEL),
     .     DF(NEL), PSH(NEL),MUOLD(NEL)
      INTEGER :: MAT(NEL)
      my_real :: GAMMA, PSTAR
      my_real :: B1, B2, R1, R2, W1, VDET, BHE
      my_real :: R1V, R2V, WDR1V, WDR2V, DR1V, ER1V, ER2V
      my_real :: TB, BFRAC1, BFRAC2
      INTEGER :: IBFRAC
      INTEGER :: LFT, LLT
      my_real :: QOPT,EADD,TBEGIN,TEND,RR,A,M,N,RR2,ALPHA_UNIT,LAMBDA !< jwl afterburning
      my_real :: pold(nel)
      my_real :: FAC_pred
C-----------------------------------------------
C     B e g i n n i n g   o f   s u b r o u t i n e
C-----------------------------------------------
      FAC_pred = one
      IF(ALEMUSCL_Param%IALEMUSCL /= 0) FAC_pred = HALF ! with prediction /correction steps, Miller's extension for JWL must use a half time step
      LFT = 1
      LLT = NEL
      RHO0(1:NEL) = PM(1, LOCAL_MATID)
      MAT(1:NEL) = LOCAL_MATID
      DVOL(1:NEL) = ZERO
      MU2(1:NEL) = ZERO
      MU(1:NEL) = ZERO
      ESPE(1:NEL) = ZERO
      DPDE(1:NEL) = ZERO
      ECOLD(1:NEL) = ZERO
      DVOL(1:NEL) = ZERO
      DF(1:NEL) = ONE
      PSH(1:NEL) = ZERO
      MUOLD(1:NEL) = ZERO
      GAMMA = ZERO
      PSTAR = ZERO
      DO II = 1, NEL
         IF (VOL(II) > ZERO) THEN
            MU(II)    = RHO(II) / RHO0(II) - ONE
            DF(II)    = RHO0(II) / RHO(II)
            PSH(II)   = PM(88, LOCAL_MATID)
         ELSE
            MU(II) = ZERO
            DF(II) = ONE
            PSH(II) = -1E20
         ENDIF
      ENDDO
      SELECT CASE (MATLAW)
         CASE (5)
C     JWL material
            B1     = PM(33, LOCAL_MATID)
            B2     = PM(34, LOCAL_MATID)
            R1     = PM(35, LOCAL_MATID)
            R2     = PM(36, LOCAL_MATID)
            W1     = PM(45, LOCAL_MATID)
            VDET   = PM(38, LOCAL_MATID)
            BHE    = PM(40, LOCAL_MATID)
            C0     = PM(43, LOCAL_MATID)
            C1     = PM(44, LOCAL_MATID)
            QOPT   = PM(42, LOCAL_MATID)
            EADD   = PM(160,LOCAL_MATID)
            TBEGIN = PM(161,LOCAL_MATID)
            TEND   = PM(162,LOCAL_MATID)
            RR     = PM(163,LOCAL_MATID)
            A      = PM(164,LOCAL_MATID)
            M      = PM(165,LOCAL_MATID)
            N      = PM(166,LOCAL_MATID)
            RR2    = PM(167,LOCAL_MATID)
            ALPHA_UNIT = PM(168,MAT(1))
            IBFRAC = NINT(PM(41, LOCAL_MATID))
            GRUN(1:NEL) = ZERO
            DO II = 1, NEL
               IF (VOL(II) > ZERO) THEN
                  !================    BURNT FRACTION     ============================!
                  IF (IFLAG == 1) THEN
                     IF (BURNFRAC(II) < ONE) THEN
                        TB = - BURNTIME(II)
                        BFRAC1 = ZERO
                        BFRAC2 = ZERO
                        IF (IBFRAC /= 1 .AND. CURRENT_TIME > TB) THEN
                           BFRAC1 = ONE
                           IF (DELTAX(II) > ZERO) THEN
                              BFRAC1 = VDET * (CURRENT_TIME - TB) / THREE_HALF / DELTAX(II)
                           ENDIF
                        ENDIF
                        IF (IBFRAC /= 2) THEN
                           BFRAC2 = BHE * (ONE - DF(II))
                        ENDIF
                        !BURNFRAC(II) = MAX(BFRAC1, BFRAC2, BURNFRAC(II))
                        BURNFRAC(II) = MAX(BFRAC1, BFRAC2)
                        IF (BURNFRAC(II) < EM04) THEN
                           BURNFRAC(II) = ZERO
                        ENDIF
                        IF (BURNFRAC(II) > ONE) THEN
                           BURNFRAC(II) = ONE
                        ENDIF
                     ENDIF
                  ENDIF
                  !================  END BURNT FRACTION   ============================!
               ENDIF
            ENDDO

            ! POLD
            POLD(:) = ZERO
            DO II = 1, NEL
               IF (VOL(II) > ZERO) THEN
                  R1V = B1 * W1 / (R1 * DF(II))
                  R2V = B2 * W1 / (R2 * DF(II))
                  WDR1V = B1 - R1V
                  WDR2V = B2 - R2V
                  DR1V = W1 * EINT(II)
                  ER1V = EXP(-R1 * DF(II))
                  ER2V = EXP(-R2 * DF(II))
                  POLD(II) = - PSH(II) + WDR1V * ER1V + WDR2V * ER2V + DR1V
                  POLD(II) = BURNFRAC(II) * POLD(II) + (ONE - BURNFRAC(II)) * (-PSH(II)+(C0 + C1 * MU(II)))
               ENDIF
            ENDDO

            !================     AFTERBURNING      ============================!
            IF(EADD /= ZERO)THEN
              SELECT CASE (NINT(QOPT))
                CASE(0) !=== instantaneous release
                  DO II=1,NEL
                    IF (VOL(II) > ZERO) THEN
                      LAMBDA = ZERO
                      IF(CURRENT_TIME > TEND .AND. ABURN(II)==ONE)THEN
                        LAMBDA   = ONE
                        ABURN(II) = ONE
                      ELSEIF (CURRENT_TIME <= TBEGIN)THEN
                        LAMBDA   = ZERO
                        ABURN(II) = ZERO
                      ELSE
                        LAMBDA   = ONE
                        EINT(II)  = EINT(II)+(LAMBDA-ABURN(II))*EADD*MAX(EM20,VOL(II)/DF(II))
                        ABURN(II) = ONE
                      ENDIF
                    ENDIF !VOL(II) > ZERO
                  ENDDO
                CASE(1) !=== afterburning with constant rate from Tbegin to Tend
                  DO II=1,NEL
                    IF (VOL(II) > ZERO) THEN
                      LAMBDA = ZERO
                      IF(CURRENT_TIME > TEND .AND. ABURN(II)==ONE)THEN
                        LAMBDA   = ONE
                        ABURN(II) = ONE
                      ELSEIF (CURRENT_TIME <= TBEGIN)THEN
                        LAMBDA   = ZERO
                        ABURN(II) = ZERO
                      ELSE
                        LAMBDA   = (CURRENT_TIME-TBEGIN)*RR
                        LAMBDA   = MIN(ONE,LAMBDA)
                        EINT(II)  = EINT(II)+(LAMBDA-ABURN(II))*EADD*MAX(EM20,VOL(II)/DF(II))
                        ABURN(II) = LAMBDA
                      ENDIF
                    ENDIF !VOL(II) > ZERO
                  ENDDO
                CASE(2) !=== afterburning with linear rate from Tbegin to Tend
                  DO II=1,NEL
                    IF (VOL(II) > ZERO) THEN
                      LAMBDA = ZERO
                      IF(CURRENT_TIME > TEND .AND. ABURN(II)==ONE)THEN    ! .AND. ABURN(II)==ONE  needed to add last increment
                        LAMBDA   = ONE
                        ABURN(II) = ONE
                      ELSEIF (CURRENT_TIME <= TBEGIN)THEN
                        LAMBDA   = ZERO
                        ABURN(II) = ZERO
                      ELSE
                        LAMBDA   = HALF*RR*CURRENT_TIME**2 - RR*TBEGIN*CURRENT_TIME + RR2
                        LAMBDA   = MAX(ZERO,MIN(ONE,LAMBDA))
                        EINT(II)  = EINT(II)+(LAMBDA-ABURN(II))*EADD*MAX(EM20,VOL(II)/DF(II))
                        ABURN(II) = LAMBDA
                      ENDIF
                    ENDIF !VOL(II) > ZERO
                  ENDDO
                CASE(3) !=== Miller s extension, rate is depedent on Pressure
                  DO II=1,NEL
                    IF (VOL(II) > ZERO) THEN
                      LAMBDA = ZERO
                      IF(POLD(II)+PSH(II) > ZERO )THEN
                        LAMBDA=ABURN(II)+ FAC_pred*DT1*A*EXP( M*LOG(ONE+ABURN(II)) )*EXP(N*LOG(ALPHA_UNIT*(POLD(II)+PSH(II))))
                        LAMBDA  = MAX(LAMBDA,ZERO)
                        LAMBDA  = MIN(LAMBDA,ONE)
                        EINT(II) = EINT(II)+(LAMBDA-ABURN(II))*EADD*MAX(EM20,VOL(II)/DF(II))/FAC_pred
                        ABURN(II)= LAMBDA
                      ENDIF
                    ENDIF !VOL(II) > ZERO
                  ENDDO
               END SELECT
            ENDIF !EADD /= ZERO
            !================  END AFTERBURNING     ============================!

            DO II = 1, NEL
               IF (VOL(II) > ZERO) THEN
                  R1V = B1 * W1 / (R1 * DF(II))
                  R2V = B2 * W1 / (R2 * DF(II))
                  WDR1V = B1 - R1V
                  WDR2V = B2 - R2V
                  DR1V = W1 * EINT(II)
                  ER1V = EXP(-R1 * DF(II))
                  ER2V = EXP(-R2 * DF(II))
                  PRES(II) = - PSH(II) + WDR1V * ER1V + WDR2V * ER2V + DR1V
                  SSP(II) = B1 * ER1V * (-W1 / DF(II) / R1 + R1 * DF(II) - W1)
     .                 + B2 * ER2V * (-W1 / DF(II) / R2 + R2 * DF(II) - W1)
     .                 + DR1V + (PRES(II) + PSH(II)) * W1
                  PRES(II) = BURNFRAC(II) * PRES(II) + 
     .                 (ONE - BURNFRAC(II)) * (-PSH(II)+(C0 + C1 * MU(II)))
                  GRUN(II) = BURNFRAC(II) * W1
                  SSP(II) = BURNFRAC(II) * (SSP(II) * DF(II)  / RHO0(II)) + 
     .                 (ONE - BURNFRAC(II)) * (C1 / RHO0(II))
                  !SSP(II) = SQRT(SSP(II))
                  !SSP(II) = MAX(SSP(II), VDET * (ONE - BURNFRAC(II)))
               ENDIF
            ENDDO
         CASE (3, 4, 6, 49)
            EOSTYPE      = IPM(4, LOCAL_MATID)
            MUOLD(1:NEL) = MU(1:NEL)
C     -> FLAG = 2 : compute pressure as a function of rho and e, as well as dpdm and dpde
            CALL EOSMAIN(2       , NEL        , EOSTYPE   , PM     , OFF  , EINT,
     .                   RHO     , RHO0       , MU        , MU2    , ESPE ,
     .                   DVOL    , DF         , VOL       , MAT    , PSH  ,
     .                   PRES    , SSP        , DPDE      , THETA  , 
     .                   BUFMAT  , SIGOLD     , MUOLD     , MATLAW ,
     .                   NPF     , TF         , VAREOS    , NVAREOS, MAT_PARAM,
     .                   BURNFRAC, NVARTMP_EOS, VARTMP_EOS)
C     SQUARE sound speed : c^2 = 1/rho0 * dp/dmu
            DO II = 1, NEL
               IF (VOL(II) > ZERO) THEN
                  SSP(II) = SSP(II) / RHO0(II)
                  GRUN(II) = DPDE(II) / (ONE + MU(II))
               ELSE
                  SSP(II) = ZERO
                  GRUN(II) = ZERO
               ENDIF
            ENDDO
         CASE DEFAULT
            PRINT*, "LAW", MATLAW, "NOT COMPATIBLE WITH LAW 151 (MULTIFLUID)"
            CALL ARRET(2)
      END SELECT
C-----------------------------------------------  
C     E n d   o f   s u b r o u t i n e
C-----------------------------------------------
      END SUBROUTINE MULTI_SUBMATLAW


      END MODULE MULTI_SUBMATLAW_MOD
