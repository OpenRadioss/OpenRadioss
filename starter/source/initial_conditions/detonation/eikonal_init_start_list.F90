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
!||    eikonal_init_start_list_mod    ../starter/source/initial_conditions/detonation/eikonal_init_start_list.F90
!||--- called by ------------------------------------------------------
!||    eikonal_fast_marching_method   ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
!||====================================================================
      module eikonal_init_start_list_mod
      implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief initialize narrow band for fast marching method depending on user input
!! \details
!||====================================================================
!||    eikonal_init_start_list             ../starter/source/initial_conditions/detonation/eikonal_init_start_list.F90
!||--- called by ------------------------------------------------------
!||    eikonal_fast_marching_method        ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
!||--- calls      -----------------------------------------------------
!||    eikonal_bcs_sym_tag                 ../starter/source/initial_conditions/detonation/eikonal_bcs_sym_tag.F90
!||--- uses       -----------------------------------------------------
!||    detonators_mod                      ../starter/share/modules1/detonators_mod.F
!||    eikonal_bcs_sym_tag_mod             ../starter/source/initial_conditions/detonation/eikonal_bcs_sym_tag.F90
!||====================================================================
        subroutine eikonal_init_start_list(nstart, start_elem_list, start_elem_tdet, detonators, numel, numnod, &
          nvois, nod2el, knod2el, ale_connectivity, elem_list_bij, neldet, xel, x, &
          nix, ix, mat_det, vel, uelem_list, elem_list, isym, itag_boundFaces, Lmax)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero, ep21
          use detonators_mod , only : detonators_struct_
          use ale_connectivity_mod , only : t_ale_connectivity
          use precision_mod, only : WP
          use insertion_sort_mod , only : integer_insertion_sort_with_index
          use eikonal_bcs_sym_tag_mod , only : eikonal_bcs_sym_tag
          !use RESTMOD , only : itab  !for debug prurpose (user identifiers of nodes)
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
          integer,intent(inout) :: nstart
          integer,intent(in) :: numel, numnod !< array size
          integer,allocatable,dimension(:),intent(out) :: start_elem_list
          real(kind=WP),allocatable,dimension(:),intent(out) :: start_elem_tdet
          type (detonators_struct_),intent(in) :: detonators
          integer,intent(in) :: nvois !< max number of adjacent elems
          integer,intent(in) :: nod2el(nvois*numel) !< connectivity node->quad
          integer,intent(in) :: knod2el(1:numnod+1) !< quad_id -> index in nod2el (size 4)
          type (t_ale_connectivity), intent(inout) :: ale_connectivity
          integer,intent(in) :: neldet
          integer,intent(in) :: elem_list_bij(numel) !< global to local
          integer,intent(in) :: uelem_list(neldet) !< user ids
          integer,intent(in) :: elem_list(neldet) !< local to global
          real(kind=WP),intent(in) :: xel(3,neldet)
          real(kind=WP),intent(in) :: x(3,numnod)
          integer,intent(in) :: nix
          integer,intent(in) :: ix(nix,numel)
          integer,intent(in) :: mat_det
          real(kind=WP),intent(in) :: vel(neldet)
          integer, intent(inout) :: isym(2)
          integer,intent(inout) :: itag_boundFaces(neldet,nvois) ! for a given explosive element, several faces may be related by planes of symmetry
          real(kind=WP),intent(in) :: Lmax
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: idet, ndet_pts
          integer :: ii,jj,ielem
          integer :: inod, nnod, nod_id
          integer :: num_adj, num_adj2
          integer, allocatable,dimension(:) :: adjacent_elem
          integer :: adjacent_elem2(64)
          integer,allocatable,dimension(:) :: itag_elem
          integer :: iad1,lgth !< variable for ale_connectivity elem-elem buffer
          integer :: iev, iel, ie
          integer:: I_frame1, I_frame2
          integer :: I_shadow_flag
          real(kind=WP) :: dx,dy,dz,dl,tdet,dcj
          real(kind=WP) :: xdet, ydet, zdet !< detonator location
          real(kind=WP) :: r0  !optionnal initialization radius
          integer, allocatable, dimension(:):: indx
          integer, allocatable, dimension(:) :: int_tmp_array
          integer,allocatable,dimension(:) :: start_elem_uid     !< user identifier for sorting
          real(kind=WP), allocatable, dimension(:) :: real_tmp_array
          real(kind=WP),allocatable,dimension(:) :: tmp_tdet
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          nstart = 0
          if(neldet == 0)return
          allocate(itag_elem(numel)) !tag to check if elem was already found
          allocate(tmp_tdet(neldet))
          allocate(adjacent_elem(numel))
          tmp_tdet(:) = ep21
          ! loop over detonation point with shadowing option (I_shadow_flag=1)
          ! and build ADJACENT NEIGHBORHOOD (3-STAGE-PROCESS)
          ndet_pts = detonators%n_det_point
          do idet = 1, ndet_pts
            I_shadow_flag = detonators%point(idet)%shadow
            if(I_shadow_flag == 0)cycle
            I_frame1 = detonators%point(idet)%iframe1
            I_frame2 = detonators%point(idet)%iframe2
            nnod = detonators%point(idet)%nnod
            itag_elem(1:numel) = 0
            ! loop over detonation points
            do inod = 1,nnod
              nod_id = detonators%point(idet)%nodlist(inod)
              r0 = detonators%point(idet)%r0
              if(r0 == zero)then
                ! INIT METHOD : Manathan neighborhood initialization
                adjacent_elem(1:256) = 0
                num_adj = 0
                ! 1st STAGE : mark with 1000
                ! 2nd STAGE : mark with  100
                ! 3rd STAGE : mark with   10
                ! 4th STAGE : mark with   +1 (only if <10)
                ! 5th STAGE : mark with   +1 (only if <2)
                ! result keep elem with TAG > 2 => double layer Manathan neighborhood
                !   use to initialize with Spherical detonation wave
                !   intialization with a single element lead to a large numerical error near the detonation point (which will be propagated especially in diagonal direction)
                !   +---------+---------+---------+---------+
                !   |         |         |         |     +1  |
                !   +---------+---------+---------+---------+
                !   |         |      2  |      2  |     10  |
                !   +---------+---------+---------+---------+
                !   |         |      2  |     10  |    100  |
                !   +---------+---------+---------+---------+
                !   |      +1 |     10  |    100  |   1000  |
                !   +---------+---------+---------+---------O  <---- Det point



                ! --- FIRST STAGE
                ! loop over attached elems
                 do ii=Knod2el(nod_id)+1,Knod2el(nod_id+1) ! loop over elems connected to nod_id
                  ielem = nod2el(abs(ii))
                  if(itag_elem(ielem) == 0)then
                    if(mat_det /=0 .and. ix(1,ielem) /= mat_det)cycle
                    itag_elem(ielem) = 1000
                    num_adj = num_adj + 1
                    adjacent_elem(num_adj) = elem_list_bij(ielem)
                  end if
                end do
                ! --- SECOND STAGE
                ! Adjacent elems (face to face)
                do ii=1,num_adj
                  ie = elem_list(adjacent_elem(ii))  ! local to global
                  if(itag_elem(ie) /= 1000)cycle
                  iad1 = ale_connectivity%ee_connect%iad_connect(ie)
                  lgth = ale_connectivity%ee_connect%iad_connect(ie+1) - iad1
                  do jj=1,lgth
                    iev = ale_connectivity%ee_connect%connected(iad1 + jj - 1)
                    if(iev == 0)cycle
                    iel = elem_list_bij(iev)
                    if(itag_elem(iev) == 0)then
                      if(mat_det /=0 .and. ix(1,iev) /= mat_det)cycle
                      itag_elem(iev) = 100
                      num_adj = num_adj + 1
                      adjacent_elem(num_adj) = elem_list_bij(iev)
                    end if
                  end do
                end do
                ! --- THIRD STAGE
                ! Adjacent elems (face to face)
                do ii=1,num_adj
                  ie = elem_list(adjacent_elem(ii))  ! local to global
                  if(itag_elem(ie) /= 100)cycle
                  iad1 = ale_connectivity%ee_connect%iad_connect(ie)
                  lgth = ale_connectivity%ee_connect%iad_connect(ie+1) - iad1
                  do jj=1,lgth
                    iev = ale_connectivity%ee_connect%connected(iad1 + jj - 1)
                    if(iev == 0)cycle
                    iel = elem_list_bij(iev)
                    if(itag_elem(iev) == 0)then
                      if(mat_det /=0 .and. ix(1,iev) /= mat_det)cycle
                      itag_elem(iev) = 10
                      num_adj = num_adj + 1
                      adjacent_elem(num_adj) = elem_list_bij(iev)
                    end if
                  end do
                end do
                ! --- FOURTH STAGE
                ! Adjacent elems (face to face)
                num_adj2 = 0
                do ii=1,num_adj
                  ie = elem_list(adjacent_elem(ii))  ! local to global
                  if(itag_elem(ie) /= 10)cycle
                  iad1 = ale_connectivity%ee_connect%iad_connect(ie)
                  lgth = ale_connectivity%ee_connect%iad_connect(ie+1) - iad1
                  do jj=1,lgth
                    iev = ale_connectivity%ee_connect%connected(iad1 + jj - 1)
                    if(iev == 0)cycle
                    iel = elem_list_bij(iev)
                    if(itag_elem(iev) < 2 )then
                      if(mat_det /=0 .and. ix(1,iev) /= mat_det)cycle
                      itag_elem(iev) = itag_elem(iev) + 1
                      if(itag_elem(iev) == 2)then
                        num_adj2 = num_adj2 + 1
                        adjacent_elem2(num_adj2) = elem_list_bij(iev)
                      end if
                    end if
                  end do
                end do
                ! --- FIFTH STAGE
                ! Adjacent elems (face to face)
                do ii=1,num_adj2
                  ie = elem_list(adjacent_elem2(ii))  ! local to global
                  if(itag_elem(ie) >= 10 .or. itag_elem(ie) == 1)cycle
                  iad1 = ale_connectivity%ee_connect%iad_connect(ie)
                  lgth = ale_connectivity%ee_connect%iad_connect(ie+1) - iad1
                  do jj=1,lgth
                    iev = ale_connectivity%ee_connect%connected(iad1 + jj - 1)
                    if(iev == 0)cycle
                    iel = elem_list_bij(iev)
                    if(itag_elem(iev) < 2 )then
                      if(mat_det /=0 .and. ix(1,iev) /= mat_det)cycle
                      itag_elem(iev) = itag_elem(iev) + 1
                     end if
                  end do
                end do
                ! --- LAST STAGE
                ! Manathan neighborhood
                do ii=1,numel
                  if(itag_elem(ii) > 1 .and. itag_elem(ii) < 10)then
                    num_adj = num_adj + 1
                    adjacent_elem(num_adj) = elem_list_bij(ii)
                  end if
                end do

              elseif(r0 /= zero)then
              ! INIT METHOD : SPHERICAL WAVE WITHIN RADIUS
              ! SELECT ALL THE ELEMENTS WITHIN A RADIUS R0 FROM THE DETONATION POINT
                 adjacent_elem(1:numel) = 0
                 num_adj = 0
                 itag_elem(:) = 0
                 !select all in the given radius, set itag_elem to 2
                 !matid = detonators%point(idet)%mat
                 xdet = x(1,nod_id)
                 ydet = x(2,nod_id)
                 zdet = x(3,nod_id)
                 do ii=1,neldet
                     dx = xdet - xel(1,ii)
                     dy = ydet - xel(2,ii)
                     dz = zdet - xel(3,ii)
                     dl = sqrt(dx*dx + dy*dy + dz*dz)
                     if(dl <= r0)then
                        itag_elem(ii) = 1
                        num_adj = num_adj + 1
                        adjacent_elem(num_adj) = ii
                     end if
                 end do

              end if

              ! ---INIT WITH RADIAL DISTANCE
              dcj = zero
              do ii=1,num_adj
                iel = adjacent_elem(ii)
                dcj = max(dcj,vel(iel)) ! chapman jouget velocity
              end do
              xdet = x(1,nod_id)
              ydet = x(2,nod_id)
              zdet = x(3,nod_id)
              if(nvois <6)then
                do ii=1,num_adj
                  iel = adjacent_elem(ii)
                  dy = ydet - xel(2,iel)  !xel-storage is 1:y 2:z
                  dz = zdet - xel(3,iel)
                  dl = sqrt(dy*dy + dz*dz)
                  tdet = detonators%point(idet)%tdet + dl / dcj
                  tmp_tdet(iel) = min (tmp_tdet(iel), tdet)
                end do
              else
                do ii=1,num_adj
                  iel = adjacent_elem(ii)
                  dx = xdet - xel(1,iel)  !xel-storage is 0:x 1:y 2:z
                  dy = ydet - xel(2,iel)  !xel-storage is 1:y 2:z
                  dz = zdet - xel(3,iel)
                  dl = sqrt(dx*dx + dy*dy + dz*dz)
                  tdet = detonators%point(idet)%tdet + dl / dcj
                  tmp_tdet(iel) = min (tmp_tdet(iel), tdet)
                end do
              end if

            end do ! next inod

             ! symmetry condition (one per detonator, not per det point comosing the detonator)
            If(I_frame1 > 0 .or. I_frame2 > 0)then
              ! we do have a plane of symmetry => defining tags for mirror elements
              call eikonal_bcs_sym_tag(neldet, elem_list, uelem_list, numnod, x, nix, numel, ix, &
                                        detonators, idet, itag_boundFaces, isym, nvois, Lmax)
            endif

          end do !next idet

          !numbering elem to init
          nstart = 0
          do ii=1,neldet
            if(tmp_tdet(ii) /= ep21)then
              nstart = nstart + 1
            end if
          end do

          !sorting them in expected data structure for FMM
          allocate(start_elem_list(nstart))
          allocate(start_elem_tdet(nstart))
          allocate(start_elem_uid(nstart))

          nstart = 0
          do ii=1,neldet
            if(tmp_tdet(ii) /= ep21)then
              nstart = nstart + 1
              start_elem_list(nstart) = ii
              start_elem_tdet(nstart) = tmp_tdet(ii)
              start_elem_uid(nstart) = uelem_list(ii)
            end if
          end do

          ! SORTING (SPMD & PARITH/ON)
          ! ensuring same order of treatment
          allocate(indx(nstart))
          allocate(int_tmp_array(nstart))
          allocate(real_tmp_array(nstart))
          int_tmp_array(1:nstart) = start_elem_list(1:nstart)
          real_tmp_array(1:nstart) = start_elem_tdet(1:nstart)
          indx(1:nstart)= [(ii, ii=1,nstart)]
          call integer_insertion_sort_with_index(start_elem_uid, indx, nstart)
          ! sort with same order (algo already ran -> use index(:) result)
          do ii=1,nstart
            start_elem_list(ii) = int_tmp_array(indx(ii))
            start_elem_tdet(ii) = real_tmp_array(indx(ii))
          end do
          deallocate(int_tmp_array)
          deallocate(real_tmp_array)
          deallocate(indx)
          deallocate(start_elem_uid)

          !remaing deallocate
          deallocate(itag_elem)
          deallocate(tmp_tdet)
          deallocate(adjacent_elem)

        end subroutine eikonal_init_start_list
! ----------------------------------------------------------------------------------------------------------------------
      end module eikonal_init_start_list_mod
