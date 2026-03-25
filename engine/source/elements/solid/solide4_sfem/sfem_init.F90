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
      module sfem_init_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief Initiliation of the SFEM data structure. 2 sub-structures are initialized in this routine: 
!!        * the first one is related to the lagrangian part of the SFEM contribution
!!        * the second one is related to the ALE part of the SFEM contribution
!!        The data structure contains 2 parts : the spmd part & the computation part 
!! \details
        subroutine sfem_init(numels,numels10,numels8,numnod,ngroup, &
                             nparg,ispmd,nspmd, &
                             iparg,ixs,ixs10,sfem,nodes)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use element_mod , only : nixs,nixs10
          use sfem_mod , only : global_sfem_
          use array_mod , only : array_type_int_1d,alloc_int_1d_array,dealloc_int_1d_array
          use nodal_arrays_mod , only : nodal_arrays_
          use initbuf_mod          
          use sfem_init_spmd_mod , only : sfem_init_spmd
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
          integer, intent(in) :: numels !< local number of solid elements
          integer, intent(in) :: numels10 !< number of tetra10 elements
          integer, intent(in) :: numels8 !< number of hexa8 elements          
          integer, intent(in) :: numnod !< number of nodes
          integer, intent(in) :: ngroup !< number of element groups
          integer, intent(in) :: nparg !< number of parameters per element group
          integer, intent(in) :: ispmd !< mpi rank of the current task
          integer, intent(in) :: nspmd !< number of mpi tasks

          integer, dimension(nparg,ngroup), intent(in) :: iparg !< element group parameters
          integer, dimension(nixs,numels), intent(in) :: ixs !< solid element data
          integer, dimension(nixs10,numels10), intent(in) :: ixs10 !< element connectivity for the additional nodes of the tetra10 elements
          type(global_sfem_), intent(inout) :: sfem !< sfem data structure
          type(nodal_arrays_), intent(in) :: nodes !< nodes data structure

! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          logical :: condition
          integer :: i,j,k,n
          integer :: nnod,icpre
          integer :: mtn,llt,nft,iad,ity
          integer :: npt,jale,ismstr,jeul,jtur
          integer :: jthe,jlag,jmult,jhbe,jivf
          integer :: nvaux,jpor,jcvt,jclose,jplasol
          integer :: irep,iint,igtyp,israt,isrot
          integer :: icsen,isorth,isorthg,ifailure,jsms

          integer :: my_address
          integer :: tetra_fsky_dim,fsky_dim2
          integer :: lag_ale
          integer, dimension(:), allocatable :: local_elm_nb
          integer, dimension(:,:), allocatable :: local_elm_list

! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

! ----------------------------------------------------------------------------------------------------------------------
          ! --------------------------- 
          ! Initialization of itag_nsfem array to mark nodes with sfem option
          do i =1,sfem%ne_sfem
            n  = sfem%in_sfem(i)
            sfem%itag_nsfem(n) = 0
          end do
          ! --------------------------- 

          ! --------------------------- 
          ! ALE initialization          
          lag_ale = 0
          allocate(local_elm_nb(2*numnod+1))
          local_elm_nb(1:2*numnod+1) = 0

          fsky_dim2 = 2
          sfem%lag%sub_tetra_fsky_dim2(1) = 1
          sfem%lag%sub_tetra_fsky_dim2(2) = 1
          ! -----------
          ! loop over the element groups to count the number of contribution for each node
          do i=1,ngroup
            nnod = iparg(28,i) ! get the number of node per element for the current group
            icpre = iparg(10,i) ! get the value of ICPRE for the current group
            if (nnod /= 4 .and. nnod /= 10) cycle ! only tetrahedral elements are supported
            call initbuf(iparg,i, &
                         mtn,llt,nft,iad,ity, &
                         npt,jale,ismstr,jeul,jtur, &
                         jthe,jlag,jmult,jhbe,jivf, &
                         nvaux,jpor,jcvt,jclose,jplasol, &
                         irep,iint,igtyp,israt,isrot, &
                         icsen,isorth,isorthg,ifailure,jsms)
            condition = (nnod==4.and.isrot==3).or.(icpre/=0.and.(nnod==10.or.(nnod==4.and.isrot==1)))
            if(jeul==1) condition = .false.
            if(jale==1) condition = .false.            
            if(.not.condition) cycle ! for 4-node elements, only ISROT=3 is supported, and for both 4-node and 10-node elements, only ICPRE=1 is supported
            ! 4 nodes of the tetra
            do j=1,llt
              local_elm_nb(ixs(2,j+nft)) = local_elm_nb(ixs(2,j+nft)) + 1
              local_elm_nb(ixs(4,j+nft)) = local_elm_nb(ixs(4,j+nft)) + 1
              local_elm_nb(ixs(7,j+nft)) = local_elm_nb(ixs(7,j+nft)) + 1
              local_elm_nb(ixs(6,j+nft)) = local_elm_nb(ixs(6,j+nft)) + 1
            enddo            
            if(nnod==10) then        
              ! additional 6 nodes of the tetra10
              do k=1,6
                do j=1,llt
                  if(ixs10(k,j+nft-numels8) /= 0) then
                    local_elm_nb(ixs10(k,j+nft-numels8)) = local_elm_nb(ixs10(k,j+nft-numels8)) + 1
                  endif
                enddo
              enddo
              if(ismstr==10) then
                fsky_dim2 = 8
                sfem%lag%sub_tetra_fsky_dim2(1) = 4
                sfem%lag%sub_tetra_fsky_dim2(2) = 4
              endif
            endif
            if(nnod==4.and.isrot==3) then
              if(ismstr==1.or.ismstr==2.or.ismstr==11.or.ismstr==12) then ! small strain formulation contribution
                do j=1,llt
                  local_elm_nb(ixs(2,j+nft)) = local_elm_nb(ixs(2,j+nft)) + 1
                  local_elm_nb(ixs(4,j+nft)) = local_elm_nb(ixs(4,j+nft)) + 1
                  local_elm_nb(ixs(7,j+nft)) = local_elm_nb(ixs(7,j+nft)) + 1
                  local_elm_nb(ixs(6,j+nft)) = local_elm_nb(ixs(6,j+nft)) + 1
                enddo 
              endif
            endif
          end do
          ! -----------

          tetra_fsky_dim = 0
          local_elm_nb(numnod+1) = 0
          do i=1,numnod
            tetra_fsky_dim = tetra_fsky_dim + local_elm_nb(i)
          enddo
          do i=2,numnod+1
            local_elm_nb(numnod+i) = local_elm_nb(numnod+i-1) + local_elm_nb(i-1)
          enddo

          allocate(local_elm_list(tetra_fsky_dim,3))          
          local_elm_list(1:tetra_fsky_dim,1:3) = 0
          local_elm_nb(1:numnod) = 0
          ! -----------
          ! loop over the element groups to save the element connectivity for each node in local_elm_list   
          do i=1,ngroup
            nnod = iparg(28,i) ! get the number of node per element for the current group
            icpre = iparg(10,i) ! get the value of ICPRE for the current group
            if (nnod /= 4 .and. nnod /= 10) cycle ! only tetrahedral elements are supported
            call initbuf(iparg,i, &
                         mtn,llt,nft,iad,ity, &
                         npt,jale,ismstr,jeul,jtur, &
                         jthe,jlag,jmult,jhbe,jivf, &
                         nvaux,jpor,jcvt,jclose,jplasol, &
                         irep,iint,igtyp,israt,isrot, &
                         icsen,isorth,isorthg,ifailure,jsms)
            condition = (nnod==4.and.isrot==3).or.(icpre/=0.and.(nnod==10.or.(nnod==4.and.isrot==1)))
            if(jeul==1) condition = .false.
            if(jale==1) condition = .false.            
            if(.not.condition) cycle ! for 4-node elements, only ISROT=3 is supported, and for both 4-node and 10-node elements, only ICPRE=1 is supported
            ! 4 nodes of the tetra

            do j=1,llt
              local_elm_nb(ixs(2,j+nft)) = local_elm_nb(ixs(2,j+nft)) + 1
              my_address = local_elm_nb(ixs(2,j+nft)) + local_elm_nb(numnod+ixs(2,j+nft))
              local_elm_list(my_address,1) = ixs(11,j+nft) ! store the element id corresponding to the current node
              local_elm_list(my_address,2) = j+nft
              local_elm_list(my_address,3) = 1

              local_elm_nb(ixs(4,j+nft)) = local_elm_nb(ixs(4,j+nft)) + 1
              my_address = local_elm_nb(ixs(4,j+nft)) + local_elm_nb(numnod+ixs(4,j+nft))
              local_elm_list(my_address,1) = ixs(11,j+nft) ! store the element id corresponding to the current node
              local_elm_list(my_address,2) = j+nft
              local_elm_list(my_address,3) = 2

              local_elm_nb(ixs(7,j+nft)) = local_elm_nb(ixs(7,j+nft)) + 1
              my_address = local_elm_nb(ixs(7,j+nft)) + local_elm_nb(numnod+ixs(7,j+nft))
              local_elm_list(my_address,1) = ixs(11,j+nft) ! store the element id corresponding to the current node
              local_elm_list(my_address,2) = j+nft
              local_elm_list(my_address,3) = 3

              local_elm_nb(ixs(6,j+nft)) = local_elm_nb(ixs(6,j+nft)) + 1
              my_address = local_elm_nb(ixs(6,j+nft)) + local_elm_nb(numnod+ixs(6,j+nft))
              local_elm_list(my_address,1) = ixs(11,j+nft) ! store the element id corresponding to the current node
              local_elm_list(my_address,2) = j+nft              
              local_elm_list(my_address,3) = 4
            enddo
            if(nnod==4.and.isrot==3) then
              if(ismstr==1.or.ismstr==2.or.ismstr==11.or.ismstr==12) then ! small strain formulation contribution

                do j=1,llt
                  local_elm_nb(ixs(2,j+nft)) = local_elm_nb(ixs(2,j+nft)) + 1
                  my_address = local_elm_nb(ixs(2,j+nft)) + local_elm_nb(numnod+ixs(2,j+nft))
                  local_elm_list(my_address,1) = ixs(11,j+nft) ! store the element id corresponding to the current node
                  local_elm_list(my_address,2) = j+nft
                  local_elm_list(my_address,3) = -1

                  local_elm_nb(ixs(4,j+nft)) = local_elm_nb(ixs(4,j+nft)) + 1
                  my_address = local_elm_nb(ixs(4,j+nft)) + local_elm_nb(numnod+ixs(4,j+nft))
                  local_elm_list(my_address,1) = ixs(11,j+nft) ! store the element id corresponding to the current node
                  local_elm_list(my_address,2) = j+nft
                  local_elm_list(my_address,3) = -2

                  local_elm_nb(ixs(7,j+nft)) = local_elm_nb(ixs(7,j+nft)) + 1
                  my_address = local_elm_nb(ixs(7,j+nft)) + local_elm_nb(numnod+ixs(7,j+nft))
                  local_elm_list(my_address,1) = ixs(11,j+nft) ! store the element id corresponding to the current node
                  local_elm_list(my_address,2) = j+nft
                  local_elm_list(my_address,3) = -3

                  local_elm_nb(ixs(6,j+nft)) = local_elm_nb(ixs(6,j+nft)) + 1
                  my_address = local_elm_nb(ixs(6,j+nft)) + local_elm_nb(numnod+ixs(6,j+nft))
                  local_elm_list(my_address,1) = ixs(11,j+nft) ! store the element id corresponding to the current node
                  local_elm_list(my_address,2) = j+nft              
                  local_elm_list(my_address,3) = -4
                enddo                
              endif
            endif
            if(nnod==10) then        
              ! additional nodes of the tetra10
              do k=1,6
                do j=1,llt
                  if(ixs10(k,j+nft-numels8)/=0) then
                    local_elm_nb(ixs10(k,j+nft-numels8)) = local_elm_nb(ixs10(k,j+nft-numels8)) + 1
                    my_address = local_elm_nb(ixs10(k,j+nft-numels8)) + local_elm_nb(numnod+ixs10(k,j+nft-numels8))
                    local_elm_list(my_address,1) = ixs(11,j+nft) ! store the element id corresponding to the current node
                    local_elm_list(my_address,2) = j+nft
                    local_elm_list(my_address,3) = k + 4                                             
                  endif
                enddo
              enddo
            endif
          end do
          ! initialization of LAG data structure
          call sfem_init_spmd(lag_ale,numels,numnod,ngroup, &
                             nparg,ispmd,nspmd,tetra_fsky_dim,fsky_dim2, &
                             iparg,ixs,sfem%lag,nodes,sfem%itag_nsfem,local_elm_nb,local_elm_list)
          deallocate(local_elm_nb)
          deallocate(local_elm_list)
          ! ---------------------------           

          ! --------------------------- 
          ! ALE initialization

          lag_ale = 1
          allocate(local_elm_nb(2*numnod+1))
          local_elm_nb(1:2*numnod+1) = 0

          fsky_dim2 = 2
          sfem%ale%sub_tetra_fsky_dim2(1) = 4
          sfem%ale%sub_tetra_fsky_dim2(2) = 4
          ! -----------
          ! loop over the element groups to count the number of contribution for each node                    
          do i=1,ngroup
            nnod = iparg(28,i) ! get the number of node per element for the current group
            icpre = iparg(10,i) ! get the value of ICPRE for the current group
            if (nnod /= 4 ) cycle ! only tetrahedral elements are supported
            call initbuf(iparg,i, &
                         mtn,llt,nft,iad,ity, &
                         npt,jale,ismstr,jeul,jtur, &
                         jthe,jlag,jmult,jhbe,jivf, &
                         nvaux,jpor,jcvt,jclose,jplasol, &
                         irep,iint,igtyp,israt,isrot, &
                         icsen,isorth,isorthg,ifailure,jsms)
            condition = (nnod==4.and.isrot==3)
            if(jeul==1) condition = .false.
            if(jlag==1) condition = .false.            
            if(.not.condition) cycle ! for 4-node elements, only ISROT=3 is supported
            ! 4 nodes of the tetra
            do j=1,llt
              local_elm_nb(ixs(2,j+nft)) = local_elm_nb(ixs(2,j+nft)) + 1
              local_elm_nb(ixs(4,j+nft)) = local_elm_nb(ixs(4,j+nft)) + 1
              local_elm_nb(ixs(7,j+nft)) = local_elm_nb(ixs(7,j+nft)) + 1
              local_elm_nb(ixs(6,j+nft)) = local_elm_nb(ixs(6,j+nft)) + 1
            enddo            
          end do
          ! -----------

          tetra_fsky_dim = 0
          local_elm_nb(numnod+1) = 0
          do i=1,numnod
            tetra_fsky_dim = tetra_fsky_dim + local_elm_nb(i)
          enddo
          do i=2,numnod
            local_elm_nb(numnod+i) = local_elm_nb(numnod+i-1) + local_elm_nb(i-1)
          enddo

          allocate(local_elm_list(tetra_fsky_dim,3))          
          local_elm_list(1:tetra_fsky_dim,1:3) = 0
          local_elm_nb(1:numnod) = 0
          ! -----------
          ! loop over the element groups to save the element connectivity for each node in local_elm_list             
          do i=1,ngroup
            nnod = iparg(28,i) ! get the number of node per element for the current group
            icpre = iparg(10,i) ! get the value of ICPRE for the current group
            if (nnod /= 4 ) cycle ! only tetrahedral elements are supported
            call initbuf(iparg,i, &
                         mtn,llt,nft,iad,ity, &
                         npt,jale,ismstr,jeul,jtur, &
                         jthe,jlag,jmult,jhbe,jivf, &
                         nvaux,jpor,jcvt,jclose,jplasol, &
                         irep,iint,igtyp,israt,isrot, &
                         icsen,isorth,isorthg,ifailure,jsms)
            condition = (nnod==4.and.isrot==3)
            if(jeul==1) condition = .false.
            if(jlag==1) condition = .false.            
            if(.not.condition) cycle ! for 4-node elements, only ISROT=3 is supported
            ! 4 nodes of the tetra

            do j=1,llt
              local_elm_nb(ixs(2,j+nft)) = local_elm_nb(ixs(2,j+nft)) + 1
              my_address = local_elm_nb(ixs(2,j+nft)) + local_elm_nb(numnod+ixs(2,j+nft))
              local_elm_list(my_address,1) = ixs(11,j+nft) ! store the element id corresponding to the current node
              local_elm_list(my_address,2) = j+nft
              local_elm_list(my_address,3) = 1

              local_elm_nb(ixs(4,j+nft)) = local_elm_nb(ixs(4,j+nft)) + 1
              my_address = local_elm_nb(ixs(4,j+nft)) + local_elm_nb(numnod+ixs(4,j+nft))
              local_elm_list(my_address,1) = ixs(11,j+nft) ! store the element id corresponding to the current node
              local_elm_list(my_address,2) = j+nft
              local_elm_list(my_address,3) = 2

              local_elm_nb(ixs(7,j+nft)) = local_elm_nb(ixs(7,j+nft)) + 1
              my_address = local_elm_nb(ixs(7,j+nft)) + local_elm_nb(numnod+ixs(7,j+nft))
              local_elm_list(my_address,1) = ixs(11,j+nft) ! store the element id corresponding to the current node
              local_elm_list(my_address,2) = j+nft
              local_elm_list(my_address,3) = 3

              local_elm_nb(ixs(6,j+nft)) = local_elm_nb(ixs(6,j+nft)) + 1
              my_address = local_elm_nb(ixs(6,j+nft)) + local_elm_nb(numnod+ixs(6,j+nft))
              local_elm_list(my_address,1) = ixs(11,j+nft) ! store the element id corresponding to the current node
              local_elm_list(my_address,2) = j+nft              
              local_elm_list(my_address,3) = 4
            enddo
          end do
          ! -----------

          ! initialization of ALE data structure
          call sfem_init_spmd(lag_ale,numels,numnod,ngroup, &
                             nparg,ispmd,nspmd,tetra_fsky_dim,fsky_dim2, &
                             iparg,ixs,sfem%ale,nodes,sfem%itag_nsfem,local_elm_nb,local_elm_list)
          deallocate(local_elm_nb)      
          deallocate(local_elm_list)    
          ! --------------------------- 

        end subroutine sfem_init
      end module sfem_init_mod
