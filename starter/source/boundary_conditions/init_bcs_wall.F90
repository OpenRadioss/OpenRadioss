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
!! \brief This subroutine is initializing the sliding wall boundary conditions
!! \details identification of related faces is done. Identified faces are recorded in specific buffer bcs%wall(id)%list(*)
!! \details binary comparison (IAND) is done to identify relevant faces
      subroutine init_bcs_wall( igrnod, ngrnod, numnod, ale_connectivity, multi_fvm,&
                                ixs,nixs,numels, ixq,nixq,numelq, ixtg,nixtg,numeltg, n2d ,  &
                                ngroup, nparg, iparg, iworking_array, ipri)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use groupdef_mod , only : group_
      use ale_connectivity_mod , only : t_ale_connectivity
      use multi_fvm_mod , only : multi_fvm_struct
      use bcs_mod , only : bcs
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
#include "my_real.inc"
#include "units_c.inc"
#include "nchar_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer, intent(in) :: ipri                                                  !< flag from /IOFLAG input option
      integer, intent(in) :: ngrnod, numnod                                        !< sizes for array definition
      type (group_), dimension(ngrnod), target :: igrnod                           !< data buffer for gorup of nodes
      TYPE(t_ale_connectivity) :: ale_connectivity                                 !< data buffer for ale connectivities
      type(multi_fvm_struct), intent(inout) :: multi_fvm                           !< data buffer for collocated scheme (multifluid law 151)
      integer, intent(in) :: nixs,nixq,nixtg                                       !<  size for array definition (elem connectivities)
      integer, intent(in) :: numels,numelq,numeltg                                 !< size for array definition (elem connectivities)
      integer, intent(in) :: ixs(nixs,numels),ixq(nixq,numelq),ixtg(nixtg,numeltg) !< data for elems connectivities
      integer, intent(in) :: n2d                                                   !< flag for 2d/3d analysis
      integer, intent(in) :: ngroup,nparg                                          !< size for array definition                                     
      integer, intent(in) :: iparg(nparg,ngroup)                                   !< data buffer for elem groups
      integer, intent(inout) :: iworking_array(2,numnod)                           !< working array defined in lectur:IWCIN2
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
      my_real :: tstart,tstop
      integer :: user_grnod_id, user_sensor_id
      integer :: internal_grnod_id, internal_sensor_id
      integer :: ii,jj,kk,kv
      integer :: num_nodes_in_group
      integer :: icode,ng,nseg
      integer :: iad1,lgth,iadv,lgthv,iv,ivv,ie
      integer :: ity,jale,jeul,nel,nft,isolnod
      integer :: ipos
      integer,allocatable,dimension(:) :: adjacent_elem
      logical :: l_tagnod(numnod)
      logical :: is_tria
 ! ----------------------------------------------------------------------------------------------------------------------
!                                                   Preconditions
! ----------------------------------------------------------------------------------------------------------------------
      if(bcs%num_wall == 0)return        ! if no option /BCS/WALL in input file then return
      if(.NOT. MULTI_FVM%IS_USED)return  ! compatible only with collocated scheme (material law 151)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
      if(ipri >= 3)write(iout, 2010)
      is_tria = .false.


      do ii = 1, bcs%num_wall

        l_tagnod(1:numnod) = .false.

        !RETRIEVE RELATED GRNOD
        internal_grnod_id = bcs%wall(ii)%grnod_id
        num_nodes_in_group = igrnod(internal_grnod_id)%nentity
        
        !TAG RELATED NODES
        do jj=1, num_nodes_in_group
          l_tagnod(igrnod(internal_grnod_id)%entity(jj)) = .true.
        end do

        !INIT
        nseg = 0
        ipos = 0
        iworking_array(2,1:NUMNOD) = 0
        !LOOP OVER ALL ELEMS
        do ng=1,ngroup
          nel = iparg(2,ng)
          nft = iparg(3,ng)
          ity = iparg(5,ng)
          jale = iparg(7,ng)
          jeul = iparg(11,ng)
          isolnod = iparg(28,ng)
          if(jale==0 .and. jeul==0)cycle

          !---hexa---
          if(ity == 1 .and. isolnod == 8)then
            do jj=1,nel
              ! tag switched on for node in user list
              icode = 0
              kk = 0 ! number of identified nodes
              if(l_tagnod(ixs(2,jj+nft)))then; icode = IBSET(icode,0); kk=kk+1 ; end if
              if(l_tagnod(ixs(3,jj+nft)))then; icode = IBSET(icode,1); kk=kk+1 ; end if
              if(l_tagnod(ixs(4,jj+nft)))then; icode = IBSET(icode,2); kk=kk+1 ; end if
              if(l_tagnod(ixs(5,jj+nft)))then; icode = IBSET(icode,3); kk=kk+1 ; end if
              if(l_tagnod(ixs(6,jj+nft)))then; icode = IBSET(icode,4); kk=kk+1 ; end if
              if(l_tagnod(ixs(7,jj+nft)))then; icode = IBSET(icode,5); kk=kk+1 ; end if
              if(l_tagnod(ixs(8,jj+nft)))then; icode = IBSET(icode,6); kk=kk+1 ; end if
              if(l_tagnod(ixs(9,jj+nft)))then; icode = IBSET(icode,7); kk=kk+1 ; end if
              if(kk < 4)cycle ! at least 4 nodes are requires to define a face
              kk = 0 !number of identified faces
              if(015 == IAND(icode,015))then; kk=kk+1 ; iworking_array(2,nseg+kk) = 1 ; end if
              if(204 == IAND(icode,204))then; kk=kk+1 ; iworking_array(2,nseg+kk) = 2 ; end if
              if(240 == IAND(icode,240))then; kk=kk+1 ; iworking_array(2,nseg+kk) = 3 ; end if
              if(051 == IAND(icode,051))then; kk=kk+1 ; iworking_array(2,nseg+kk) = 4 ; end if
              if(102 == IAND(icode,102))then; kk=kk+1 ; iworking_array(2,nseg+kk) = 5 ; end if
              if(153 == IAND(icode,153))then; kk=kk+1 ; iworking_array(2,nseg+kk) = 6 ; end if
              iworking_array(1,nseg+1:nseg+kk) = jj+nft
              nseg = nseg + kk
            end do!next jj

          !---tetra---
          elseif(ity == 1 .and. isolnod == 4)then
            do jj=1,nel
              ! tag switched on for node in user list
              icode = 0
              kk = 0 ! number of identified nodes
              if(l_tagnod(ixs(2,jj+nft)))then; icode = IBSET(icode,0); kk=kk+1 ; end if
              if(l_tagnod(ixs(3,jj+nft)))then; icode = IBSET(icode,1); kk=kk+1 ; end if
              if(l_tagnod(ixs(4,jj+nft)))then; icode = IBSET(icode,2); kk=kk+1 ; end if
              if(l_tagnod(ixs(5,jj+nft)))then; icode = IBSET(icode,3); kk=kk+1 ; end if
              if(l_tagnod(ixs(6,jj+nft)))then; icode = IBSET(icode,4); kk=kk+1 ; end if
              if(l_tagnod(ixs(7,jj+nft)))then; icode = IBSET(icode,5); kk=kk+1 ; end if
              if(l_tagnod(ixs(8,jj+nft)))then; icode = IBSET(icode,6); kk=kk+1 ; end if
              if(l_tagnod(ixs(9,jj+nft)))then; icode = IBSET(icode,7); kk=kk+1 ; end if
              if(kk < 4)cycle ! at least 4 nodes are requires to define a face
              kk = 0 !number of identified faces
              !if(015 == IAND(icode,015))then; kk=kk+1 ; iworking_array(2,nseg+kk) = 1 ; end if
              if(204 == IAND(icode,204))then; kk=kk+1 ; iworking_array(2,nseg+kk) = 2 ; end if
              !if(240 == IAND(icode,240))then; kk=kk+1 ; iworking_array(2,nseg+kk) = 3 ; end if
              if(051 == IAND(icode,051))then; kk=kk+1 ; iworking_array(2,nseg+kk) = 4 ; end if
              if(102 == IAND(icode,102))then; kk=kk+1 ; iworking_array(2,nseg+kk) = 5 ; end if
              if(153 == IAND(icode,153))then; kk=kk+1 ; iworking_array(2,nseg+kk) = 6 ; end if
              iworking_array(1,nseg+1:nseg+kk) = jj+nft
              nseg = nseg + kk
            end do!next j

          !---quad---
          elseif(ity == 2)then
            do jj=1,nel
              ! tag switched on for node in user list
              icode = 0
              kk = 0 ! number of identified nodes
              if(l_tagnod(ixq(2,jj+nft)))then; icode = IBSET(icode,0); kk=kk+1 ; end if
              if(l_tagnod(ixq(3,jj+nft)))then; icode = IBSET(icode,1); kk=kk+1 ; end if
              if(l_tagnod(ixq(4,jj+nft)))then; icode = IBSET(icode,2); kk=kk+1 ; end if
              if(l_tagnod(ixq(5,jj+nft)))then; icode = IBSET(icode,3); kk=kk+1 ; end if
              if(kk < 2)cycle ! at least 4 nodes are requires to define a face
              kk = 0 ! number of identified faces
              if(03 == IAND(icode,03))then; kk=kk+1 ; iworking_array(2,nseg+kk) = 1 ; end if
              if(06 == IAND(icode,06))then; kk=kk+1 ; iworking_array(2,nseg+kk) = 2 ; end if
              if(12 == IAND(icode,12))then; kk=kk+1 ; iworking_array(2,nseg+kk) = 3 ; end if
              if(09 == IAND(icode,09))then; kk=kk+1 ; iworking_array(2,nseg+kk) = 4 ; end if
              iworking_array(1,nseg+1:nseg+kk) = jj+nft
              nseg = nseg + kk
            end do!next jj

          !---tria---
          elseif(ity == 7 .and. n2d /= 0)then
            is_tria = .true.
            do jj=1,nel
               ! tag switched on for node in user list
               icode = 0
               kk = 0 ! number of identified nodes
               if(l_tagnod(ixtg(2,jj+nft)))then; icode = IBSET(icode,0); kk=kk+1 ; end if
               if(l_tagnod(ixtg(3,jj+nft)))then; icode = IBSET(icode,1); kk=kk+1 ; end if
               if(l_tagnod(ixtg(4,jj+nft)))then; icode = IBSET(icode,2); kk=kk+1 ; end if
               if(kk < 2)cycle ! at least 4 nodes are requires to define a face
               kk = 0 !number of identified faces
               if(3 == IAND(icode,3))then; kk=kk+1 ; iworking_array(2,nseg+kk) = 1 ; end if
               if(6 == IAND(icode,6))then; kk=kk+1 ; iworking_array(2,nseg+kk) = 2 ; end if
               if(5 == IAND(icode,5))then; kk=kk+1 ; iworking_array(2,nseg+kk) = 3 ; end if
               iworking_array(1,nseg+1:nseg+kk) = jj+nft
               nseg = nseg + kk
            end do!next jj

          end if! ity

        enddo!next ng

        allocate(bcs%wall(ii)%list%elem(nseg))
        allocate(bcs%wall(ii)%list%face(nseg))
        ! allocate(bcs%wall(ii)%list%adjacent_elem(nseg)) !do not need to store in global datastrucure : printout only
        allocate(adjacent_elem(nseg)) !Starter printout only (local array)

        !searching for adjacent elems on related face (and face from this adjacent elem)
        do jj=1,nseg
          ie = iworking_array(1,jj)
          iad1 = ale_connectivity%ee_connect%iad_connect(ie)
          lgth = ale_connectivity%ee_connect%iad_connect(ie+1)-ale_connectivity%ee_connect%iad_connect(ie)
              kk = iworking_array(2,jj)
              iv = ale_connectivity%ee_connect%connected(iad1 + kk - 1)
              if (iv > 0) then
                iadv = ale_connectivity%ee_connect%iad_connect(iv)
                lgthv = ale_connectivity%ee_connect%iad_connect(iv+1)-ale_connectivity%ee_connect%iad_connect(iv)
                do kv = 1, lgthv
                  ivv = ale_connectivity%ee_connect%connected(iadv + kv - 1)
                  if(ivv == iworking_array(1,jj) ) then
                    ipos = ipos + 1 ! skip data when there is no adjacent elem
                    bcs%wall(ii)%list%elem(ipos) = ie
                    bcs%wall(ii)%list%face(ipos) = kk
                    adjacent_elem(ipos)=iv !for user post verification from Starter listing file
                    exit
                  endif
                end do
              end if
        end do !next jj

        if(ipri >= 3)then
          !effective size & printout
           bcs%wall(ii)%list%size = ipos
           if(ipos > 0)then
             write(iout, 2011)bcs%wall(ii)%user_id
             write(iout, 2019)ipos/2  ! elem_i/elem_j and elem_j/elem_i related to the same internal face
             write(iout, 2020)
             write(iout, 2021)
             do jj=1,ipos
               ie = bcs%wall(ii)%list%elem(jj)
               iv = adjacent_elem(jj)
               ! convert intenal ids (ie,iv) into user ids
               if(n2d==0)then
                 ie = ixs(nixs,ie)
                 iv = ixs(nixs,iv)
               else
                 if(is_tria)then
                   ie = ixtg(nixtg,ie)
                   iv = ixtg(nixtg,iv)
                 else
                   ie = ixq(nixq,ie)
                   iv = ixq(nixq,iv)
                 end if
               end if
               ! print user ids
               !  do no print elem_i/elem_j and elem_j/elem_i it is the same internal face
               if(ie < iv)write(iout, fmt='(5X,I10,2X,I10)')ie,iv
             end do
             write(iout, 2022)
           endif
        endif

        deallocate(adjacent_elem)

      end do !next ii




      return
! ----------------------------------------------------------------------------------------------------------------------
 2010 FORMAT(5X, &
             5X,/,'    INIT. WALL BOUNDARY CONDITIONS  ', /,&
                  '    ------------------------------  ', /)

 2011 format(5X, 'bcs identifier . . . . . . . . . . . . =', I10)
 2019 format(5X, 'number of identified faces . . . . . . =', I10)
 2020 format(5X, 'list of identified faces :' )
 2021 format(5X, '      ELEM /      ELEM   ' )
 2022 format(5X)


! ----------------------------------------------------------------------------------------------------------------------
      end subroutine init_bcs_wall
