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

      !||====================================================================
      !||    glob_therm_init_mod   ../starter/source/constraints/thermic/glob_therm_init.F90
      !||--- called by ------------------------------------------------------
      !||    starter0              ../starter/source/starter/starter0.F
      !||====================================================================
      module glob_therm_init_mod
      contains

! ======================================================================================================================
! \brief initializes global parameters and flags for thermal options
!! \details

! ======================================================================================================================


      !||====================================================================
      !||    glob_therm_init   ../starter/source/constraints/thermic/glob_therm_init.F90
      !||--- called by ------------------------------------------------------
      !||    starter0          ../starter/source/starter/starter0.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      subroutine glob_therm_init(glob_therm)

!-----------------------------------------------
!     M o d u l e s
!-----------------------------------------------
      use glob_therm_mod
      use constant_mod, only : zero
! ----------------------------------------------------------------------------------------------------------------------

          implicit none

! ----------------------------------------------------------------------------------------------------------------------
!     included files
! ----------------------------------------------------------------------------------------------------------------------

#include "my_real.inc"

!-----------------------------------------------
!     D u m m y   A r g u m e n t s
!-----------------------------------------------
      type (glob_therm_) ,intent(inout) :: glob_therm
!=======================================================================
   
      glob_therm%itherm_fe    = 0
      glob_therm%itherm       = 0
      glob_therm%intheat      = 0    !< thermal flux flag in contact interfaces
!
      glob_therm%nimtemp      = 0    !<  number of /imptemp   input cards
      glob_therm%nintemp      = 0    !<  number of /initemp   input cards
      glob_therm%nimpflux     = 0    !<  number of /impflux   input cards
      glob_therm%nconvec      = 0    !<  number of /convec    input cards
      glob_therm%nradia       = 0    !<  number of /radiation input cards


      glob_therm%nfxtemp      = 0    !<  number of nodes with imposed temperature
      glob_therm%nfxflux      = 0    !<  number of entities with imposed thermal flux
      glob_therm%numconv      = 0    !<  number of segments subject to convection
      glob_therm%numradia     = 0    !<  number of segments subject to radiation

      glob_therm%nift         = 4    !<  size of imposed nodal temperature table : IBFT
      glob_therm%nitflux      = 12   !<  size of imposed thermal flux data table : IB(NITFLUX,*)
      glob_therm%niconv       = 11   !<  size of convection data table : IB 
      glob_therm%niradia      = 11   !<  size of radiation data table  : IBCR
      glob_therm%lfacther     = 6    !<  size 
!
      glob_therm%nodadt_therm = 0  
      glob_therm%idt_therm    = 0  
! 
      glob_therm%dt_therm     = zero  
      glob_therm%theaccfact   = zero  
      glob_therm%dtfactherm   = zero  
!
      glob_therm%heat_meca    = zero  
      glob_therm%heat_conv    = zero  
      glob_therm%heat_radia   = zero  
      glob_therm%heat_fflux   = zero  
      glob_therm%heat_stored  = zero  

!-----------
      return
      end subroutine glob_therm_init
!
      end module glob_therm_init_mod
