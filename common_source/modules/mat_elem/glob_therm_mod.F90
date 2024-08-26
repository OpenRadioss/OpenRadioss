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

      !||====================================================================
      !||    glob_therm_mod         ../common_source/modules/mat_elem/glob_therm_mod.F90
      !||--- called by ------------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      module glob_therm_mod

! ======================================================================================================================
!! \brief module to define data structure for global thermal model parameters and flags
!! \details 

!----------------------------------------------------------------------- 
!     included files
!----------------------------------------------------------------------- 

      implicit none
!
#include "my_real.inc"
!
!=======================================================================      
      
      type glob_therm_

        integer     :: itherm_fe     !< thermal option flag for lagrangial analysis   
        integer     :: itherm        !< thermal option flag for ale/eulerian analysis 
        integer     :: intheat       !< thermal option in interfaces
!
        integer     :: nimtemp       !< number of /imptemp   input cards
        integer     :: nintemp       !< number of /initemp   input cards
        integer     :: nimpflux      !< number of /impflux   input cards
        integer     :: nconvec       !< number of /convec    input cards
        integer     :: nradia        !< number of /radiation input cards
!
        integer     :: nfxtemp       !< number of nodes with imposed temperature
        integer     :: nfxflux       !< number of entities with imposed thermal flux
        integer     :: numconv       !< number of segments subject to convection
        integer     :: numradia      !< number of segments subject to radiation

        integer     :: nift          !< size of IBFT table for imposed nodal temperature
        integer     :: nitflux       !< size of IB table for imposed thermal flux IB(NITFLUX,*)
        integer     :: niconv        !< size of convection data table : IB 
        integer     :: niradia       !< size of IBCR table for imposed radiation
        integer     :: lfacther      !< 
!
        integer     :: nodadt_therm  !< nodal thermal time step flag
        integer     :: idt_therm     !< thermal time step flag
!
        my_real     :: dt_therm      !< thermal time step value
        my_real     :: theaccfact    !< thermal model acceleration factor
        my_real     :: dtfactherm    !< thermal time step reduction factor
!
        my_real     :: heat_meca     !< cumulated mecanical heat flux
        my_real     :: heat_conv     !< cumulated convection heat flux
        my_real     :: heat_radia    !< cumulated radiation heat flux
        my_real     :: heat_fflux    !< cumulated fixed heat flux      
        my_real     :: heat_stored   !< cumulated total heat flux
!
!         my_real, dimension(:), allocatable :: FTHE
!         my_real, dimension(:), allocatable :: FTHESKYI
!         my_real, dimension(:), allocatable :: FTHESKY
!         my_real, dimension(:), allocatable :: TEMP_FTHESKYI
!         my_real, dimension(:), allocatable :: CONDN
!         my_real, dimension(:), allocatable :: CONDNSKY 
!         my_real, dimension(:), allocatable :: CONDNSKYI
!         my_real, dimension(:), allocatable :: TEMP_CONDNSKYI

      end type glob_therm_   
!
!------------------------------
      end module glob_therm_mod
