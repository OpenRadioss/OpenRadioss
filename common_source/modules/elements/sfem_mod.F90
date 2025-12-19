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
!||    sfem_mod        ../common_source/modules/elements/sfem_mod.F90
!||--- called by ------------------------------------------------------
!||    alemain         ../engine/source/ale/alemain.F
!||    forint          ../engine/source/elements/forint.F
!||    radioss2        ../engine/source/engine/radioss2.F
!||    rdcomi          ../engine/source/output/restart/rdcomm.F
!||    rdresa          ../engine/source/output/restart/rdresa.F
!||    rdresb          ../engine/source/output/restart/rdresb.F
!||    resol           ../engine/source/engine/resol.F
!||    resol_head      ../engine/source/engine/resol_head.F
!||    restalloc       ../engine/source/output/restart/arralloc.F
!||    wrcomi          ../engine/source/output/restart/wrcomm.F
!||    wrrestp         ../engine/source/output/restart/wrrestp.F
!||--- uses       -----------------------------------------------------
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
      module sfem_mod
        use precision_mod, only: WP
        implicit none
        private :: WP
        ! ----------------------------------------------------------------------------------------------------------------------
        ! T y p e s
        ! ----------------------------------------------------------------------------------------------------------------------
!
        type sfem_
          integer :: isfem                                     !< flag of smoothing FEM (nodal pressure)
          integer :: ne_sfem                                   !< Number of nodes excluded from SFEM
          integer :: s_sfem_nodvar                             !< size of work array sfem_nodvar,sfem_nodvar_ale:  2*numnod
!                    
          integer,dimension(:),  allocatable         ::  in_sfem         !< in_sfem(ne_sfem) list of nodes excluded from SFEM
          integer,dimension(:),  allocatable         ::  itag_nsfem      !< itag_nsfem(numnod) tag of nodes =0 will be excluded
          real(kind=WP) ,dimension(:), allocatable   ::  sfem_nodvar     !< work arrary(2*numnod)
          real(kind=WP) ,dimension(:), allocatable   ::  sfem_nodvar_ale !< work arrary(2*numnod)
        end type sfem_
!
!===================================================================================================
      contains

!! \brief allocate sfem type
!||====================================================================
!||    allocate_sfem   ../common_source/modules/elements/sfem_mod.F90
!||--- called by ------------------------------------------------------
!||    restalloc       ../engine/source/output/restart/arralloc.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    my_alloc_mod    ../common_source/tools/memory/my_alloc.F90
!||====================================================================
        subroutine allocate_sfem(sfem,numnod)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use my_alloc_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer , INTENT (IN   )       :: numnod                                    
          type(sfem_),INTENT(INOUT)      :: sfem
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          if (sfem%isfem>0) THEN
            sfem%s_sfem_nodvar = 2*numnod 
            call my_alloc(sfem%in_sfem,sfem%ne_sfem)
            call my_alloc(sfem%itag_nsfem,numnod)
            sfem%itag_nsfem = 1
          else 
            sfem%s_sfem_nodvar = 1 
          end if
          call my_alloc(sfem%sfem_nodvar,sfem%s_sfem_nodvar)
          call my_alloc(sfem%sfem_nodvar_ale,sfem%s_sfem_nodvar)
        end subroutine allocate_sfem
!! \brief initialization sfem type
!||====================================================================
!||    sfem_init   ../common_source/modules/elements/sfem_mod.F90
!||--- called by ------------------------------------------------------
!||    resol       ../engine/source/engine/resol.F
!||====================================================================
        subroutine sfem_init(sfem)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(sfem_),INTENT(INOUT)                           :: sfem
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer  :: i,n
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
          do i =1,sfem%ne_sfem
            n  = sfem%in_sfem(i)
            sfem%itag_nsfem(n) = 0
          end do
!          
        end subroutine sfem_init
      end module sfem_mod

