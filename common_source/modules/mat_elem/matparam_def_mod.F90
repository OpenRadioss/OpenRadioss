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
!Chd|====================================================================
!Chd|  matparam_def_mod              modules/mat_elem/matparam_def_mod.F90
!Chd|-- called by -----------
!Chd|-- calls ---------------
!Chd|        fail_param_mod                modules/mat_elem/fail_param_mod.F90
!Chd|        names_and_titles_mod          modules/names_and_titles_mod.F90
!Chd|        table4d_mod                   modules/table4d_mod.F90         
!Chd|        visc_param_mod                modules/mat_elem/visc_param_mod.F90
!Chd|====================================================================
      module matparam_def_mod
!
      use table4d_mod
      use visc_param_mod
      use fail_param_mod
      use names_and_titles_mod
! ----------------------------------------------------------------------------------------------------------------------
!     included files
!----------------------------------------------------------------------- 
      implicit none

#include "my_real.inc"

!=======================================================================      
  !! \brief module to define data structure for all material model parameters 
  !! \details  allocatable dimension : nummat
!=======================================================================      
!
      type matparam_struct_
      
        character(len=nchartitle) :: title  !< material law title
        integer     :: ilaw                 !< material law number (type)    
        integer     :: mat_id               !< material law id   
        integer     :: nuparam              !< number of real value material paraameters
        integer     :: niparam              !< number of int value material parameters
        integer     :: nfunc                !< number of local functions in material
        integer     :: ntable               !< number of local function tables
        integer     :: nsubmat              !< number of submaterials (multi-mat law51)
        integer     :: nfail                !< number of failure models
        integer     :: ivisc                !< viscosity model number
        integer     :: ieos                 !< eos model number
        integer     :: itherm               !< therm model number                       
        ! -------  material characteristics flags
        integer     :: compressibility      !< "compressible","incompressible","elasto_plastic"
        integer     :: smstr                !< "small_strain", "large_strain"
        integer     :: strain_formulation   !< "total", "incremental"
        integer     :: ipres                !< "hydrostatic",hydro_eos","hook"
        integer     :: orthotropy           !< "isotropic", "orthotropic", "anisotropic"
        ! ------- compatibility flags
        integer     :: prop_solid           !< "solid_isotropic","solid_orthotropic","solid_composite","solid_cohesive"   ,"solid_porous","solid_all"
        integer     :: prop_shell           !< "shell_isotropic","shell_orthotropic","shell_composite","shell_anisotropic","shell_all"
        integer     :: prop_beam            !< "beam_classic"   ,"beam_integrated"  ,"beam_all"
        integer     :: prop_spring          !< "spring_predit"  ,"spring_material"  ,"spring_all"
        integer     :: prop_truss           !< "truss"
        integer     :: prop_sph             !< "sph"
        integer     :: compatibility_eos    !< "eos"
        integer     :: compatibility_visc   !< "visc"
!        integer     :: compatibility_nloc   !< "nloc"
        ! --------------------------------- !<  
        integer     :: nloc                 !< non-local variable regularization flag
        integer     :: ifailwave            !< failwave propagation flag
        integer     :: ixfem                !< xfem flag
        ! --------------------------------- !<
        integer     :: nmod                 !< number of rupture/damage modes
        ! --------------------------------- !
        my_real     ::  rho
        my_real     ::  rho0        
        my_real     ::  young        
!                        
        my_real                   ,dimension(:) ,allocatable :: uparam !< real value material parameter table (nuparam)
        integer                   ,dimension(:) ,allocatable :: iparam !< integer value material parameter table (niparam)
        type (table_4d_)          ,dimension(:) ,allocatable :: table  !< local function tables
        character(len=nchartitle) ,dimension(:) ,allocatable :: mode   !< damage mode keywords
!                
        type (fail_param_),dimension(:) ,allocatable :: fail     !< failure models data structure (nfail)
        type (visc_param_)                           :: visc     !< viscosity model data structure        

!        type (eos_param_)    :: eos                             !< eos model data structure (to be defined)     
!        type (therm_param_)  :: therm                           !< thermal model data structure (to be defined)           
!        type (submat_)  ,dimension(:) ,allocatable :: submat    !< multi material data structure (to be defined) 

      end type matparam_struct_
!
!---------------
      end module matparam_def_mod
