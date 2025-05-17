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
      !||    multi_muscl_compute_pressure_mod   ../engine/source/multifluid/multi_muscl_compute_pressure.F90
      !||--- called by ------------------------------------------------------
      !||    multi_muscl_fluxes_computation     ../engine/source/multifluid/multi_muscl_fluxes_computation.F
      !||====================================================================
      MODULE MULTI_MUSCL_COMPUTE_PRESSURE_MOD
      CONTAINS
      !||====================================================================
      !||    multi_muscl_compute_pressure     ../engine/source/multifluid/multi_muscl_compute_pressure.F90
      !||--- called by ------------------------------------------------------
      !||    multi_muscl_fluxes_computation   ../engine/source/multifluid/multi_muscl_fluxes_computation.F
      !||--- calls      -----------------------------------------------------
      !||    multi_submatlaw                  ../engine/source/multifluid/multi_submatlaw.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod                     ../common_source/modules/constant_mod.F
      !||    matparam_def_mod                 ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||    multi_submatlaw_mod              ../engine/source/multifluid/multi_submatlaw.F
      !||====================================================================
      SUBROUTINE MULTI_MUSCL_COMPUTE_PRESSURE(MATLAW, LOCAL_MATID, PM, IPM, NPROPM, NPROPMI, &
                                  EINT, RHO, PRES, SSP, &
                                  BURNFRAC, BURNTIME, DELTAX, CURRENT_TIME, &
                                  BUFMAT, OFF, SIG, SNPC, STF, NPF, TF, VAREOS,NVAREOS, MAT_PARAM, &
                                  NVARTMP_EOS, VARTMP_EOS, NUMMAT)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      USE MATPARAM_DEF_MOD , ONLY : MATPARAM_STRUCT_
      USE MULTI_SUBMATLAW_MOD , ONLY : MULTI_SUBMATLAW
      USE CONSTANT_MOD , ONLY : ZERO, EM06, ONE
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit None
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      INTEGER, INTENT(IN) :: MATLAW, LOCAL_MATID, NPROPM, NPROPMI, NUMMAT, SNPC, STF
      my_real, INTENT(IN) :: PM(NPROPM, *), CURRENT_TIME
      INTEGER, INTENT(IN) :: IPM(NPROPMI, *)
      my_real, INTENT(IN) :: SIG(6)
      my_real, INTENT(INOUT) :: SSP(1), PRES(1), EINT(1), RHO(1), BURNFRAC(1), BURNTIME(1), DELTAX(1)
      my_real, INTENT(OUT) :: OFF(1)
      my_real, INTENT(INOUT) :: BUFMAT(*)
      INTEGER,INTENT(IN)::NPF(SNPC),NVAREOS
      my_real,INTENT(IN)::TF(STF),VAREOS(NVAREOS*1)
      TYPE(MATPARAM_STRUCT_), INTENT(IN) :: MAT_PARAM !material data structure
      INTEGER,INTENT(IN) :: NVARTMP_EOS
      INTEGER,INTENT(INOUT) :: VARTMP_EOS(1,NVARTMP_EOS)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local Variables
! ----------------------------------------------------------------------------------------------------------------------
      INTEGER :: ITER, MAX_ITER
      my_real :: TOL, ERROR
      my_real :: GRUN(1), VOL(1), TEMP(1)
      LOGICAL :: CONT
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      MAX_ITER = 50
      TOL = EM06
      ERROR = ONE
      
      ! Dummy
      VOL = ONE

      ! Initialization
      TEMP = ZERO
      ITER = 0
      CONT = .TRUE.
      CALL MULTI_SUBMATLAW( &
         0,           MATLAW,      LOCAL_MATID, 1, &
         EINT,        PRES,        RHO,         SSP, &
         VOL,         GRUN,        PM,          IPM, &
         NPROPM,      NPROPMI,     BUFMAT,      OFF, &
         TEMP,        BURNFRAC,    BURNTIME,    DELTAX, &
         CURRENT_TIME,SIG(1:6),    SNPC    ,    STF, &
         NPF,         TF,          VAREOS,      NVAREOS, &
         MAT_PARAM   ,NVARTMP_EOS, VARTMP_EOS,  NUMMAT)

      END SUBROUTINE MULTI_MUSCL_COMPUTE_PRESSURE
      END MODULE MULTI_MUSCL_COMPUTE_PRESSURE_MOD
