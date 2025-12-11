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
      module init_ale_boundary_condition_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief
!! \details
        subroutine init_ale_boundary_condition(nv46,nparg,ngroup,iparg,ale_connect)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use mvsiz_mod, only : MVSIZ          
          use ale_connectivity_mod , only : t_ale_connectivity
          use initbuf_mod , only : initbuf
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nv46 !< number of element's facet (2d-->4, 3d-->6)
          integer, intent(in) :: nparg !< first dimension of iparg array
          integer, intent(in) :: ngroup !< number of element group
          integer, dimension(nparg,ngroup), intent(in) :: iparg !< group element data
          type(t_ale_connectivity), intent(inout) :: ale_connect !< ALE data structure for connectivity          
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          logical :: already_
          integer :: ng,mtn,llt,nft,iad,ity
          integer :: npt,jale,ismstr,jeul,jtur
          integer :: jthe,jlag,jmult,jhbe,jivf
          integer :: nvaux,jpor,jcvt,jclose,jplasol
          integer :: irep,iint,igtyp,israt,isrot
          integer :: icsen,isorth,isorthg,ifailure,jsms
          integer :: nb_bc
          integer :: i,j,iad2,elem_id
          integer, dimension(mvsiz) :: elem_id_with_bc
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to minimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate( ale_connect%group(ngroup)) ! allocate %group array
          ! --------------
          ! loop over the element group to initialize the list of elements with boudary conditions
          do ng=1,ngroup
            ale_connect%group(ng)%nb_element_with_bc = 0 ! initialize the number of element with boundary conditions
            ! check if the current group is an ALE group
            if(iparg(76,ng)==1) cycle ! --> this group is not active
            call initbuf(iparg,ng, &
                         mtn,llt,nft,iad,ity, &
                         npt,jale,ismstr,jeul,jtur, &
                         jthe,jlag,jmult,jhbe,jivf, &
                         nvaux,jpor,jcvt,jclose,jplasol, &
                         irep,iint,igtyp,israt,isrot, &
                         icsen,isorth,isorthg,ifailure,jsms )
            if(jale+jeul==0) cycle
            if(iparg(8,ng)==1) cycle
            if(iparg(1,ng)/=51) cycle

            nb_bc = 0
            do i=1,llt
              iad2 = ale_connect%ee_connect%iad_connect(i+nft) ! get the address of %connected array
              already_ = .false.              
              do j=1,nv46
                elem_id = ale_connect%ee_connect%connected(iad2+j-1) ! get the neighbor element id
                if(elem_id==0.and..not.already_) then ! if elem_id = 0 --> boundary conditions
                  nb_bc = nb_bc + 1
                  elem_id_with_bc(nb_bc) = i ! save the element id
                  already_ = .true.
                endif
              enddo
            enddo
            ale_connect%group(ng)%nb_element_with_bc = nb_bc
            allocate(ale_connect%group(ng)%list_element_with_bc(nb_bc))
            ale_connect%group(ng)%list_element_with_bc(1:nb_bc) = elem_id_with_bc(1:nb_bc)
          enddo
          ! --------------

          ! --------------
          ! loop over the element group to ensure that %list_element_with_bc is allocated
          do ng=1,ngroup
            if(.not.allocated(ale_connect%group(ng)%list_element_with_bc)) then
              allocate(ale_connect%group(ng)%list_element_with_bc(0))
            endif
          enddo
          ! --------------
        
          return
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine init_ale_boundary_condition
      end module init_ale_boundary_condition_mod
