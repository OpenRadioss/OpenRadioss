!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      !||====================================================================
      !||    multi_solve_eint_mod   ../engine/source/multifluid/multi_solve_eint.F90
      !||--- called by ------------------------------------------------------
      !||    multi_inlet_ebcs       ../engine/source/multifluid/multi_inlet_ebcs.F
      !||====================================================================
      MODULE MULTI_SOLVE_EINT_MOD
      CONTAINS
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief solve internal energy
!! \details 
      !||====================================================================
      !||    multi_solve_eint      ../engine/source/multifluid/multi_solve_eint.F90
      !||--- called by ------------------------------------------------------
      !||    multi_inlet_ebcs      ../engine/source/multifluid/multi_inlet_ebcs.F
      !||--- calls      -----------------------------------------------------
      !||    multi_submatlaw       ../engine/source/multifluid/multi_submatlaw.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod          ../common_source/modules/constant_mod.F
      !||    eosmain_mod           ../common_source/eos/eosmain.F
      !||    matparam_def_mod      ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    multi_submatlaw_mod   ../engine/source/multifluid/multi_submatlaw.F
      !||====================================================================
      SUBROUTINE MULTI_SOLVE_EINT(MATLAW   , LOCAL_MATID, PM        , IPM         , NPROPM , NPROPMI,&
                                  EINT     , RHO        , PRES      , SSP         , &
                                  BURNFRAC , BURNTIME   , DELTAX    , CURRENT_TIME, &
                                  BUFMAT   , OFF        , SNPC,STF  , NPF         , TF     , VAREOS , NVAREOS,&
                                  MAT_PARAM, NVARTMP_EOS, VARTMP_EOS, NUMMAT      )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Module
! ----------------------------------------------------------------------------------------------------------------------
      USE MATPARAM_DEF_MOD , ONLY : MATPARAM_STRUCT_
      USE MULTI_SUBMATLAW_MOD , ONLY : MULTI_SUBMATLAW
      USE CONSTANT_MOD , ONLY : ZERO, ONE, EM06
      USE EOSMAIN_MOD , ONLY : EOSMAIN
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      INTEGER,INTENT(IN) :: SNPC,STF,NUMMAT, NPROPM, NPROPMI !< array size
      INTEGER, INTENT(IN) :: MATLAW, LOCAL_MATID
      my_real, INTENT(IN) :: PM(NPROPM, NUMMAT)
      INTEGER, INTENT(IN) :: IPM(NPROPMI, NUMMAT)
      my_real, INTENT(INOUT) :: RHO(1)
      my_real, INTENT(INOUT) :: SSP(1), PRES(1), EINT(1)
      my_real, INTENT(INOUT) :: BURNFRAC(1), BURNTIME(1), DELTAX(1), CURRENT_TIME, BUFMAT(*)
      my_real, INTENT(INOUT) :: OFF(1)
      INTEGER, INTENT(IN) :: NPF(SNPC),NVAREOS
      my_real, INTENT(IN) :: TF(STF),VAREOS(NVAREOS*1)
      TYPE(MATPARAM_STRUCT_), INTENT(IN) :: MAT_PARAM !material data structure
      INTEGER,INTENT(IN) :: NVARTMP_EOS
      INTEGER,INTENT(INOUT) :: VARTMP_EOS(1,NVARTMP_EOS)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
      INTEGER :: ITER, MAX_ITER
      my_real :: TOL, ERROR
      my_real :: FUNC, DFUNC, PSTAR, GRUN(1), VOL(1), INCR, TEMP(1), PRESK(1), DUMMY(6)
      LOGICAL :: CONT
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      MAX_ITER = 50
      TOL = EM06
      ERROR = ONE
      DUMMY(1:6)=ZERO
      
      !dummy
      VOL = ONE

      !Initialization
      TEMP = ZERO
      ITER = 0
      CONT = .TRUE.
      DO WHILE (CONT .AND. ITER  <  MAX_ITER) 
         ITER = ITER + 1
         CALL MULTI_SUBMATLAW( &
         0,           MATLAW,      LOCAL_MATID, 1, &
         EINT,        PRESK,       RHO,         SSP, &
         VOL,         GRUN,        PM,          IPM, &
         NPROPM,      NPROPMI,     BUFMAT,      OFF, &
         TEMP,        BURNFRAC,    BURNTIME,    DELTAX, &
         CURRENT_TIME,DUMMY,       SNPC    ,    STF , &
         NPF,         TF,          VAREOS,      NVAREOS, &
         MAT_PARAM,   NVARTMP_EOS, VARTMP_EOS,  NUMMAT)
         FUNC = PRESK(1) - PRES(1)
         ERROR  = ABS(FUNC)
         IF (ERROR  <  TOL * (ABS(PRES(1)) + ONE)) THEN
            CONT = .FALSE.
         ENDIF
         DFUNC = GRUN(1)
         IF (GRUN(1)  >  ZERO) THEN
            INCR = -FUNC / DFUNC
            EINT(1) = EINT(1) + INCR
         ELSE
            CONT = .FALSE.
            EINT(1) = ZERO
         ENDIF

      ENDDO
      END SUBROUTINE MULTI_SOLVE_EINT
! ----------------------------------------------------------------------------------------------------------------------
      END MODULE MULTI_SOLVE_EINT_MOD
