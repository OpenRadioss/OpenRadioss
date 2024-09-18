!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Here is a small description of the routine, [after the header]
!! \details if needed, more details can be added here
      MODULE MULTIMAT_PARAM_MOD
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
!  [ the module names in use must be in uppercase for now, it will change latter]
!  [ ONLY is mandatory, note the space before the ,]      
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! [ no comment on the same line as #include #define #ifdef, #endif ]
! [ my_real.inc must be includeed, it was included in "implicit_f.inc"]
#include "my_real.inc"      
        INTEGER, PARAMETER :: M51_N0PHAS = 04
        INTEGER, PARAMETER :: M51_NVPHAS = 23
        INTEGER, PARAMETER :: M51_IFLG6_SIZE = 37
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ---------------------------------------------------------------------------------------------------------------------- 
        TYPE MULTIMAT_PARAM_                                 !< data structure for MAT_PARAM buffer
          integer :: nb = 0                                  !< number of submaterial
          integer,allocatable,dimension(:) :: mid            !< material internal identifier for each submaterial
          my_real,allocatable,dimension(:) :: vfrac          !< volume fraction for each submaterial
        END TYPE MULTIMAT_PARAM_
     
        logical :: M20_DISCRETE_FILL = .false.               !< LAW20 global parameters
        my_real :: M51_SSP0MAX, M51_LC0MAX, M51_TCP_REF      !< LAW51 global parameters
        INTEGER :: M51_IFLG6 = 0                             !< LAW51 global parameters
        INTEGER :: M51_lSET_IFLG6 = 0                        !< LAW51 global parameters
        INTEGER :: M51_ILOOP_NRF = 0                         !< LAW51 global parameters

      END MODULE MULTIMAT_PARAM_MOD

      
