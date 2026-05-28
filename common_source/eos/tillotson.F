Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!||    tillotson_mod   ../common_source/eos/tillotson.F
!||--- called by ------------------------------------------------------
!||    eosmain         ../common_source/eos/eosmain.F
!||    eosmain51       ../engine/source/materials/mat/mat051/eosmain51.F90
!||====================================================================
        MODULE TILLOTSON_MOD
          IMPLICIT NONE
        CONTAINS

!||====================================================================
!||    tillotson       ../common_source/eos/tillotson.F
!||--- called by ------------------------------------------------------
!||    eosmain         ../common_source/eos/eosmain.F
!||    eosmain51       ../engine/source/materials/mat/mat051/eosmain51.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    eos_param_mod   ../common_source/modules/mat_elem/eos_param_mod.F90
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
          SUBROUTINE TILLOTSON(
     .        IFLAG, NEL, PMIN, OFF, EINT, MU, MU2,
     .        ESPE, DVOL, DF, VNEW, PSH,
     .        PNEW, DPDM, DPDE, VAREOS, NVAREOS, EOS_STRUCT)

            USE CONSTANT_MOD , ONLY : ZERO, EM15, HALF, ONE, TWO
            USE EOS_PARAM_MOD, ONLY : eos_param_
            USE precision_mod, ONLY : WP

            IMPLICIT NONE

            INTEGER, INTENT(IN) :: IFLAG, NEL, NVAREOS

            REAL(KIND=WP), INTENT(IN) :: PMIN
            REAL(KIND=WP), INTENT(IN) :: OFF(NEL), MU(NEL), MU2(NEL)
            REAL(KIND=WP), INTENT(IN) :: ESPE(NEL), DVOL(NEL), DF(NEL), VNEW(NEL)

            REAL(KIND=WP), INTENT(INOUT) :: EINT(NEL)
            REAL(KIND=WP), INTENT(INOUT) :: PSH(NEL)
            REAL(KIND=WP), INTENT(INOUT) :: PNEW(NEL), DPDM(NEL), DPDE(NEL)
            REAL(KIND=WP), INTENT(INOUT) :: VAREOS(NEL, NVAREOS)

            TYPE(EOS_PARAM_), INTENT(IN) :: EOS_STRUCT

            INTEGER :: I, REG

            REAL(KIND=WP) :: A, B, C1, C2_VAL
            REAL(KIND=WP) :: EREF, ALPHA, BETA, EIV, ECV, VIV
            REAL(KIND=WP) :: INV_EREF, INV_ERANGE

            REAL(KIND=WP) :: MU_I, MU2_I, E_I, OFF_I, PSH_I
            REAL(KIND=WP) :: ETA, ETA2, INV_ETA, INV_ETA2, ETAE
            REAL(KIND=WP) :: W, INV_W, INV_W2
            REAL(KIND=WP) :: ABW, ABWX
            REAL(KIND=WP) :: DE_W, DN_W
            REAL(KIND=WP) :: C2I

            REAL(KIND=WP) :: XX, C1EB, EAX, C1EBEAX
            REAL(KIND=WP) :: P12, P3, P4
            REAL(KIND=WP) :: DE_P12, DE_P3, DE_P4
            REAL(KIND=WP) :: DM_P12, DM_P3, DM_P4
            REAL(KIND=WP) :: DPDM12, DPDM3, DPDM4

            REAL(KIND=WP) :: I12, I4, KK, TMP3
            REAL(KIND=WP) :: DVV, DENOM
            REAL(KIND=WP) :: P_LOC, DPDM_LOC, DPDE_LOC

            C1      = EOS_STRUCT%UPARAM(1)
            C2_VAL  = EOS_STRUCT%UPARAM(2)
            A       = EOS_STRUCT%UPARAM(3)
            B       = EOS_STRUCT%UPARAM(4)
            EREF    = EOS_STRUCT%UPARAM(5)
            ECV     = EOS_STRUCT%UPARAM(6)
            VIV     = EOS_STRUCT%UPARAM(7)
            ALPHA   = EOS_STRUCT%UPARAM(8)
            BETA    = EOS_STRUCT%UPARAM(9)
            EIV     = EOS_STRUCT%UPARAM(10)

            INV_EREF  = ONE / EREF  ! pre-condition (checked during Starter Reader) : |EREF| > 1E-20

            IF(ECV /= EIV)THEN
              INV_ERANGE = ONE / (ECV - EIV) ! relevant only in Region=3 if EIV/=ECV
            ELSE
              INV_ERANGE = ZERO ! not used otherwise
            ENDIF

            PSH(1:NEL) = EOS_STRUCT%PSH

            ! ================================================================
            ! IFLAG = 0 : first staggered pass
            ! IFLAG = 2 : collocated explicit pass
            ! These two branches share the same pressure/derivative kernel.
            ! ================================================================
            IF (IFLAG == 0 .OR. IFLAG == 2) THEN

              !$OMP SIMD
              DO I = 1, NEL

                MU_I  = MU(I)
                MU2_I = MU2(I)
                E_I   = ESPE(I)
                OFF_I = OFF(I)
                PSH_I = PSH(I)

                ! Region identification, scalar only.
                IF (MU_I < ZERO) THEN
                  IF (DF(I) > VIV) THEN
                    REG = 4
                  ELSEIF (E_I >= ECV) THEN
                    REG = 4
                  ELSEIF (E_I < EIV) THEN
                    REG = 2
                  ELSE
                    REG = 3
                  ENDIF
                ELSE
                  REG = 1
                ENDIF

                VAREOS(I, 1) = REAL(REG, KIND=WP)

                IF (IFLAG == 2 .AND. VNEW(I) <= ZERO) CYCLE

                ETA      = ONE + MU_I
                ETA2     = ETA * ETA
                INV_ETA  = ONE / ETA
                INV_ETA2 = INV_ETA * INV_ETA
                ETAE     = ETA * E_I

                DE_W = INV_EREF * INV_ETA2
                DN_W = -TWO * E_I * DE_W * INV_ETA

                W      = ONE + E_I * DE_W
                INV_W  = ONE / W
                INV_W2 = INV_W * INV_W
                ABW    = A + B * INV_W

                I12 = ZERO
                I4  = ZERO
                TMP3 = ZERO

                P12 = ZERO
                P4  = ZERO

                DE_P12 = ZERO
                DE_P4  = ZERO
                DPDM12 = ZERO
                DPDM4  = ZERO

                ! ------------------------------------------------------------
                ! Regions 1, 2 and 3: Tillotson compressed / cold branch.
                ! Original semantics preserved:
                ! P12 is floored and multiplied by OFF before DPDM12.
                ! ------------------------------------------------------------
                IF (REG <= 3) THEN

                  IF (REG == 1) THEN
                    C2I = C2_VAL
                  ELSE
                    C2I = ZERO
                  ENDIF

                  P12 = ABW * ETAE + C1 * MU_I + C2I * MU2_I
                  P12 = MAX(P12, PMIN) * OFF_I

                  DE_P12 = -B * DE_W * ETAE * INV_W2 + ABW * ETA
                  DM_P12 = -B * DN_W * ETAE * INV_W2 + ABW * E_I + C1 + TWO * C2I * MU_I

                  DPDM12 = DM_P12 + P12 * INV_ETA2 * DE_P12
                  I12 = ONE

                ENDIF

                ! ------------------------------------------------------------
                ! Regions 3 and 4: hot expansion branch.
                ! ------------------------------------------------------------
                IF (REG >= 3) THEN

                  XX       = MU_I * INV_ETA
                  C1EB     = C1 * EXP(BETA * XX)
                  EAX      = EXP(-ALPHA * XX * XX)
                  C1EBEAX  = C1EB * EAX
                  ABWX     = A + B * EAX * INV_W

                  P4 = C1EBEAX * MU_I + ABWX * ETAE
                  P4 = MAX(P4, PMIN) * OFF_I

                  DE_P4 = -B * EAX * DE_W * ETAE * INV_W2 + ABWX * ETA

                  DM_P4 = BETA * INV_ETA2 * C1EBEAX * MU_I
     .                  - TWO * C1EBEAX * ALPHA * XX * MU_I * INV_ETA2
     .                  + C1EBEAX
     .                  + ETAE * ( -TWO * B * ALPHA * XX * EAX * INV_W * INV_ETA2
     .                  - B * EAX * DN_W * INV_W2 )
     .                  + ABWX * E_I

                  DPDM4 = DM_P4 + P4 * INV_ETA2 * DE_P4
                  I4 = ONE

                ENDIF

                ! ------------------------------------------------------------
                ! Region 3 transition interpolation.
                ! Original semantics preserved:
                ! interpolation uses already floored/OFF-scaled P12 and P4.
                ! ------------------------------------------------------------
                IF (REG == 3) THEN
                  KK   = (E_I - EIV) * INV_ERANGE
                  I12  = ONE - KK
                  I4   = KK
                  TMP3 = (P4 - P12) * INV_ERANGE
                ENDIF

                PNEW(I) = I12 * P12 + I4 * P4
                DPDM(I) = I12 * DPDM12 + I4 * DPDM4
                DPDE(I) = I12 * DE_P12 + I4 * DE_P4 + TMP3

                ! Final scaling, kept identical to the current routine.
                PNEW(I) = (MAX(PNEW(I), PMIN) - PSH_I) * OFF_I
                DPDM(I) = DPDM(I) * OFF_I
                DPDE(I) = DPDE(I) * OFF_I

              ENDDO

            ! ================================================================
            ! IFLAG = 1 : second staggered pass, CN closure.
            ! ================================================================
            ELSEIF (IFLAG == 1) THEN

              !$OMP SIMD
              DO I = 1, NEL

                MU_I  = MU(I)
                MU2_I = MU2(I)
                E_I   = ESPE(I)
                OFF_I = OFF(I)
                PSH_I = PSH(I)

                ! Region identification, scalar only.
                IF (MU_I < ZERO) THEN
                  IF (DF(I) > VIV) THEN
                    REG = 4
                  ELSEIF (E_I >= ECV) THEN
                    REG = 4
                  ELSEIF (E_I < EIV) THEN
                    REG = 2
                  ELSE
                    REG = 3
                  ENDIF
                ELSE
                  REG = 1
                ENDIF

                VAREOS(I, 1) = REAL(REG, KIND=WP)

                DVV = HALF * DVOL(I) * DF(I) / MAX(EM15, VNEW(I))

                ETA      = ONE + MU_I
                ETA2     = ETA * ETA
                INV_ETA  = ONE / ETA
                INV_ETA2 = INV_ETA * INV_ETA
                ETAE     = ETA * E_I

                DE_W = INV_EREF * INV_ETA2
                DN_W = -TWO * E_I * DE_W * INV_ETA

                W      = ONE + E_I * DE_W
                INV_W  = ONE / W
                INV_W2 = INV_W * INV_W
                ABW    = A + B * INV_W

                SELECT CASE (REG)

                CASE (1, 2)

                  IF (REG == 1) THEN
                    C2I = C2_VAL
                  ELSE
                    C2I = ZERO
                  ENDIF

                  P12 = ABW * ETAE + C1 * MU_I + C2I * MU2_I

                  DE_P12 = -B * DE_W * ETAE * INV_W2 + ABW * ETA

                  DM_P12 = -B * DN_W * ETAE * INV_W2 + ABW * E_I  + C1 + TWO * C2I * MU_I

                  DENOM = ONE + DVV * DE_P12
                  DENOM = SIGN(MAX(ABS(DENOM), EM15), DENOM)

                  P12 = P12 / DENOM

                  P_LOC    = P12
                  DPDE_LOC = DE_P12 / DENOM
                  DPDM_LOC = DM_P12 + P12 * INV_ETA2 * DE_P12

                CASE (4)

                  XX       = MU_I * INV_ETA
                  C1EB     = C1 * EXP(BETA * XX)
                  EAX      = EXP(-ALPHA * XX * XX)
                  C1EBEAX  = C1EB * EAX
                  ABWX     = A + B * EAX * INV_W

                  P4 = C1EBEAX * MU_I + ABWX * ETAE

                  DE_P4 = -B * EAX * DE_W * ETAE * INV_W2 + ABWX * ETA

                  DM_P4 = BETA * INV_ETA2 * C1EBEAX * MU_I
     .                  - TWO * C1EBEAX * ALPHA * XX * MU_I * INV_ETA2
     .                  + C1EBEAX
     .                  + ETAE * ( -TWO * B * ALPHA * XX * EAX * INV_W * INV_ETA2
     .                  - B * EAX * DN_W * INV_W2 )
     .                  + ABWX * E_I

                  DENOM = ONE + DVV * DE_P4
                  DENOM = SIGN(MAX(ABS(DENOM), EM15), DENOM)

                  P4 = P4 / DENOM

                  P_LOC    = P4
                  DPDE_LOC = DE_P4 / DENOM
                  DPDM_LOC = DM_P4 + P4 * INV_ETA2 * DE_P4

                CASE DEFAULT

                  ! Region 3 transition.
                  XX       = MU_I * INV_ETA
                  C1EB     = C1 * EXP(BETA * XX)
                  EAX      = EXP(-ALPHA * XX * XX)
                  C1EBEAX  = C1EB * EAX
                  ABWX     = A + B * EAX * INV_W

                  P12 = ABW * ETAE + C1 * MU_I

                  DE_P12 = -B * DE_W * ETAE * INV_W2 + ABW * ETA

                  DM_P12 = -B * DN_W * ETAE * INV_W2 + ABW * E_I + C1

                  P4 = C1EBEAX * MU_I + ABWX * ETAE

                  DE_P4 = -B * EAX * DE_W * ETAE * INV_W2 + ABWX * ETA

                  DM_P4 = BETA * INV_ETA2 * C1EBEAX * MU_I
     .                  - TWO * C1EBEAX * ALPHA * XX * MU_I * INV_ETA2
     .                  + C1EBEAX
     .                  + ETAE * ( -TWO * B * ALPHA * XX * EAX * INV_W * INV_ETA2
     .                  - B * EAX * DN_W * INV_W2 )
     .                  + ABWX * E_I

                  KK   = (E_I - EIV) * INV_ERANGE
                  I12  = ONE - KK
                  TMP3 = (P4 - P12) * INV_ERANGE

                  P3     = I12 * P12    + KK * P4
                  DE_P3  = I12 * DE_P12 + KK * DE_P4 + TMP3
                  DM_P3  = I12 * DM_P12 + KK * DM_P4

                  DENOM = ONE + DVV * DE_P3
                  DENOM = SIGN(MAX(ABS(DENOM), EM15), DENOM)

                  P3 = P3 / DENOM

                  P_LOC    = P3
                  DPDE_LOC = DE_P3 / DENOM
                  DPDM_LOC = DM_P3 + P3 * INV_ETA2 * DE_P3

                END SELECT

                PNEW(I) = (MAX(P_LOC, PMIN) - PSH_I) * OFF_I
                DPDM(I) = DPDM_LOC * OFF_I
                DPDE(I) = DPDE_LOC * OFF_I

                EINT(I) = EINT(I) - HALF * DVOL(I) * (PNEW(I) + PSH_I)

              ENDDO

            ENDIF

          END SUBROUTINE TILLOTSON

        END MODULE TILLOTSON_MOD
