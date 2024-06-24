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
!Chd|  mat_elem_mod                  modules/mat_elem/mat_elem_mod.F90
!Chd|-- called by -----------
!Chd|-- calls ---------------
!Chd|        elbufdef_mod                  modules/mat_elem/elbufdef_mod.F90
!Chd|        group_param_mod               modules/mat_elem/group_param_mod.F90
!Chd|        matparam_def_mod              modules/mat_elem/matparam_def_mod.F90
!Chd|        prop_param_mod                modules/mat_elem/prop_param_mod.F90
!Chd|====================================================================

      module mat_elem_mod
      
! ======================================================================================================================
!! \brief module to host the top level material, property and element datatype
!! \details 

      use elbufdef_mod
      use matparam_def_mod
      use prop_param_mod
      use group_param_mod

! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------

        type mat_elem_
           integer :: ngroup       !< number of element groups
           integer :: nummat       !< number of /mat  cards
           integer :: numgeo       !< number of /prop cards
           integer :: numsubstack  !< number of substack created from /prop/type17, type51 or type52 => ns_stack (see stackgroup.F)
           integer :: numstack     !< number of /stack  used with /pcompp
           integer :: numply       !< number of /ply    used with /pcompp

           type (elbuf_struct_)    ,dimension(:)   ,allocatable :: elbuf          !< global element group buffer structure
           type (elbuf_struct_)    ,dimension(:,:) ,allocatable :: xfem_tab       !< element buffer for xfem elements      
           type (group_param_)     ,dimension(:)   ,allocatable :: group_param    !< common element group data
        
           type (matparam_struct_) ,dimension(:)   ,pointer     :: mat_param      !< material model data structure
        
!           type (prop_param_)      ,dimension(:)   ,allocatable :: prop_param     !< element property data structure
!           type (prop_param_)      ,dimension(:)   ,allocatable :: prop_stack     !< element stack data
!           type (prop_param_)      ,dimension(:)   ,allocatable :: prop_ply       !< element ply data
!           type (prop_param_)      ,dimension(:)   ,allocatable :: prop_substack  !< element substack data      
     
        end type mat_elem_

      end module mat_elem_mod
