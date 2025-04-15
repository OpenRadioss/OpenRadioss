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
      !||    eikonal_init_start_list_2d_mod   ../starter/source/initial_conditions/detonation/eikonal_init_start_list_2d.F90
      !||--- called by ------------------------------------------------------
      !||    eikonal_fast_marching_method     ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
      !||====================================================================
      module eikonal_init_start_list_2d_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!! \brief initialize narrow band for fast marching method depending on user input
!! \details
      !||====================================================================
      !||    eikonal_init_start_list_2d     ../starter/source/initial_conditions/detonation/eikonal_init_start_list_2d.F90
      !||--- called by ------------------------------------------------------
      !||    eikonal_fast_marching_method   ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
      !||--- uses       -----------------------------------------------------
      !||    detonators_mod                 ../starter/share/modules1/detonators_mod.F
      !||====================================================================
        subroutine eikonal_init_start_list_2d(nstart, start_elem_list, start_elem_tdet, detonators, numel, numnod, &
                                              nvois, nod2el, knod2el, ale_connectivity, elem_list_bij, neldet, xel, x, &
                                              nix, ix, mat_det, vel)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero, ep21
          use detonators_mod , only : detonators_struct_
          use ale_connectivity_mod , only : t_ale_connectivity
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,intent(inout) :: nstart
          integer,intent(in) :: numel, numnod !< array size
          integer,allocatable,dimension(:),intent(out) :: start_elem_list
          my_real,allocatable,dimension(:),intent(out) :: start_elem_tdet
          type (detonators_struct_),intent(in) :: detonators
          integer,intent(in) :: nvois !< max number of adjacent elems
          integer,intent(in) :: nod2el(nvois*numel) !< connectivity node->quad
          integer,intent(in) :: knod2el(numnod+1) !< quad_id -> index in nod2el (size 4)
          type (t_ale_connectivity), intent(inout) :: ale_connectivity
          integer,intent(in) :: elem_list_bij(numel)
          integer,intent(in) :: neldet
          my_real,intent(in) :: xel(3,neldet)
          my_real,intent(in) :: x(3,numnod)
          integer,intent(in) :: nix
          integer,intent(in) :: ix(nix,numel)
          integer,intent(in) :: mat_det
          my_real,intent(in) :: vel(neldet)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: idet, ndet_pts
          integer :: ii,jj,ielem
          integer :: I_shadow_flag
          integer :: inod, nnod, nod_id
          integer :: num_adj, adjacent_elem(64) ! Maximum number of elements in diagonal neighborhood
          integer,allocatable,dimension(:) :: itag_elem
          integer iad1,lgth !< variable for ale_connectivity elem-elem buffer
          integer :: iev, iel, ie
          my_real :: dx,dy,dz,dl,tdet,dcj
          my_real :: xdet, ydet,zdet

          my_real,allocatable,dimension(:) :: tmp_tdet
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
        allocate(itag_elem(numel)); !tag to check if elem was already found

        allocate(tmp_tdet(neldet))
        tmp_tdet(:) = ep21

        ! loop over detonation point with shadowing option (I_shadow_flag=1)
        ! and build ADJACENT NEIGHBORHOOD (3-STAGE-PROCESS)
        ndet_pts = detonators%n_det_point
        do idet = 1, ndet_pts
          I_shadow_flag = detonators%point(idet)%shadow
          nnod = detonators%point(idet)%nnod
          itag_elem(1:numel) = 0
          ! loop over detonation points
          do inod = 1,nnod
            nod_id = detonators%point(idet)%nodlist(inod)
            adjacent_elem(1:64) = 0
            num_adj = 0 !max 64
            ! --- FIRST STAGE
            ! loop over attached elems
            do ii=Knod2el(nod_id)+1,Knod2el(nod_id+1)
              ielem = nod2el(ii)
              if(itag_elem(ielem) == 0)then
                if(mat_det /=0 .and. ix(1,ielem) /= mat_det)cycle
                itag_elem(ielem) = 100
                num_adj = num_adj + 1
                adjacent_elem(num_adj) = ielem
              end if
            end do
            ! --- SECOND STAGE
            ! Manathan neighborhood
            do ii=1,num_adj
              ie = adjacent_elem(ii)
              iad1 = ale_connectivity%ee_connect%iad_connect(ie)
              lgth = ale_connectivity%ee_connect%iad_connect(ie+1) - iad1
              do jj=1,lgth
                iev = ale_connectivity%ee_connect%connected(iad1 + jj - 1)
                if(iev == 0)cycle
                iel = elem_list_bij(iev)
                if(itag_elem(iel) == 0)then
                   if(mat_det /=0 .and. ix(1,iev) /= mat_det)cycle
                   itag_elem(iel) = 10
                   num_adj = num_adj + 1
                   adjacent_elem(num_adj) = iel
                 end if
              end do
            end do
            ! --- THIRD STAGE
            ! Adjacent neighborhood
            do ii=1,num_adj
              ie = adjacent_elem(ii)
              if(itag_elem(ie) /= 10)cycle
              iad1 = ale_connectivity%ee_connect%iad_connect(ie)
              lgth = ale_connectivity%ee_connect%iad_connect(ie+1) - iad1
              do jj=1,lgth
                iev = ale_connectivity%ee_connect%connected(iad1 + jj - 1)
                if(iev == 0)cycle
                iel = elem_list_bij(iev)
                if(itag_elem(iel) < 10)then
                   if(mat_det /=0 .and. ix(1,iev) /= mat_det)cycle
                   itag_elem(iel) = itag_elem(iel) + 1
                 end if
              end do
            end do
            do ii=1,numel
              if(itag_elem(ii) == 2)then
                 num_adj = num_adj + 1
                 adjacent_elem(num_adj) = ii
              endif
            end do

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
                iel = elem_list_bij(iel)
                dy = ydet - xel(2,iel)  !xel-storage is 1:y 2:z
                dz = zdet - xel(3,iel)
                dl = sqrt(dy*dy + dz*dz)
                tdet = dl / dcj
                tmp_tdet(iel) = min (tmp_tdet(iel), tdet)
              end do
            else
              do ii=1,num_adj
                iel = adjacent_elem(ii)
                iel = elem_list_bij(iel)
                dx = xdet - xel(1,iel)  !xel-storage is 0:x 1:y 2:z
                dy = ydet - xel(2,iel)  !xel-storage is 1:y 2:z
                dz = zdet - xel(3,iel)
                dl = sqrt(dx*dx + dy*dy + dz*dz)
                tdet = dl / dcj
                tmp_tdet(iel) = min (tmp_tdet(iel), tdet)
              end do
            end if

          enddo ! next inod

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
        nstart = 0
        do ii=1,neldet
          if(tmp_tdet(ii) /= ep21)then
            nstart = nstart + 1
            start_elem_list(nstart) = ii
            start_elem_tdet(nstart) = tmp_tdet(ii)
          end if
        end do

        deallocate(itag_elem)
        deallocate(tmp_tdet)

      end subroutine eikonal_init_start_list_2d
! ----------------------------------------------------------------------------------------------------------------------
      end module eikonal_init_start_list_2d_mod
