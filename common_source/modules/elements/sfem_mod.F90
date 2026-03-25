!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
        type spmd_
          integer :: s_size !< number of entity to send
          integer :: r_size  !< number of entity to receive
          integer, dimension(:), allocatable :: send_iad !< address array for sending data
          integer, dimension(:), allocatable :: rcv_iad  !< address array for receiving data

          real(kind=WP), dimension(:,:), allocatable :: r_buffer !< buffer for receiving data
          real(kind=WP), dimension(:,:), allocatable :: s_buffer !< buffer for sending data
        end type spmd_               
        
        type sfem_                    
          integer, dimension(:), allocatable :: need_it !< array to know if we need to compute SFEM contribution for each element

          integer :: tetra_fsky_dim1 !< first dimension of the array tetra_fsky
          integer :: tetra_fsky_dim2 !< second dimension of the array tetra_fsky
          integer :: sub_tetra_fsky_dim2(2) !< second dimension of the array tetra_fsky for each sub case (4 or 10 nodes / ismstr)
          real(kind=WP), dimension(:,:), allocatable :: tetra_fsky !< Fsky array for tetra elements
          real(kind=WP) ,dimension(:), allocatable :: nodvar     !< work arrary(2*numnod)  

          integer, dimension(:,:), allocatable :: tetra4_iad !< address array for tetra4 SFEM
          integer :: node_nb !< number of nodes connected to tetra element with SFEM option
          integer, dimension(:), allocatable :: node_list !< list of nodes connected to tetra element with SFEM option
          integer, dimension(:), allocatable :: node_iad !< ! adress of the first contribution of a node

          integer :: s_request_nb !< number of request for sending data
          integer :: r_request_nb !< number of request for receiving data
          integer, dimension(:), allocatable :: s_request !< array of request for sending data
          integer, dimension(:), allocatable :: r_request !< array of request for receiving data
          integer, dimension(:), allocatable :: s_index !< array of processor index for sending data
          integer, dimension(:), allocatable :: r_index !< array of processor index for receiving data

          type(spmd_), dimension(:), allocatable :: spmd !< spmd data structure   
        end type sfem_

        type global_sfem_
          integer :: isfem !< flag of smoothing FEM (nodal pressure)
          integer :: ne_sfem !< Number of nodes excluded from SFEM        
          integer :: s_sfem_nodvar !< size of work array sfem_nodvar:  2*numnod               
          integer,dimension(:),  allocatable ::  in_sfem !< in_sfem(ne_sfem) list of nodes excluded from SFEM
          integer,dimension(:),  allocatable ::  itag_nsfem !< itag_nsfem(numnod) tag of nodes =0 will be excluded        
          type(sfem_) :: ale
          type(sfem_) :: lag
        end type global_sfem_
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
          integer , INTENT (IN   ) :: numnod                                    
          type(global_sfem_),INTENT(INOUT) :: sfem
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
            call my_alloc(sfem%in_sfem,0)
            call my_alloc(sfem%itag_nsfem,0)            
          end if
          call my_alloc(sfem%lag%nodvar,sfem%s_sfem_nodvar)
          call my_alloc(sfem%ale%nodvar,sfem%s_sfem_nodvar)
        end subroutine allocate_sfem
      end module sfem_mod

