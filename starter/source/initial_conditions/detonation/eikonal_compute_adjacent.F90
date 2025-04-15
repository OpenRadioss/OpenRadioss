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
      !||    eikonal_compute_adjacent_mod   ../starter/source/initial_conditions/detonation/eikonal_compute_adjacent.F90
      !||--- called by ------------------------------------------------------
      !||    eikonal_fast_marching_method   ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
      !||====================================================================
      module eikonal_compute_adjacent_mod
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
      !||====================================================================
      !||    eikonal_compute_adjacent          ../starter/source/initial_conditions/detonation/eikonal_compute_adjacent.F90
      !||--- called by ------------------------------------------------------
      !||    eikonal_fast_marching_method      ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
      !||--- calls      -----------------------------------------------------
      !||    eikonal_godunov_operator_2d       ../starter/source/initial_conditions/detonation/eikonal_godunov_operator_2d.F90
      !||    eikonal_godunov_operator_3d       ../starter/source/initial_conditions/detonation/eikonal_godunov_operator_3d.F90
      !||--- uses       -----------------------------------------------------
      !||    eikonal_godunov_operator_2d_mod   ../starter/source/initial_conditions/detonation/eikonal_godunov_operator_2d.F90
      !||    eikonal_godunov_operator_3d_mod   ../starter/source/initial_conditions/detonation/eikonal_godunov_operator_3d.F90
      !||====================================================================
        subroutine eikonal_compute_adjacent(ie, ALE_CONNECTIVITY,neldet, &
                                              tdet, tdet_adj, vel,vel_adj, xel,xel_adj, numel,elem_list_bij, &
                                              updown,num_new_activated, list_new_activated,  mat_det, &
                                              nix,ix,nvois)
!! \brief In order to apply Godunov operator the adjacent data must be used. This subroutine is collecting these data (positions, velocities, arrival times)
!! \details ALE_EE_CONNECT is used to retrive data from adjacent element (ALE,EULER only)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use ale_connectivity_mod , only : t_ale_connectivity
          use constant_mod , only : ep21,one,zero
          use eikonal_godunov_operator_2d_mod, only : eikonal_godunov_operator_2d
          use eikonal_godunov_operator_3d_mod, only : eikonal_godunov_operator_3d
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
          integer,intent(in) :: ie
          type (t_ale_connectivity), intent(inout) :: ale_connectivity
          integer,intent(in) :: neldet
          my_real,intent(inout) :: tdet_adj(6)
          my_real,intent(inout) :: xel_adj(3,6)
          my_real,intent(inout) :: vel_adj(6)
          integer,intent(in) :: numel
          integer, intent(in) :: elem_list_bij(numel)  ! size
          integer,intent(inout) :: updown(neldet)
          integer,intent(inout) :: num_new_activated, list_new_activated(6)
          my_real,intent(in) :: vel(neldet),xel(3,neldet)
          my_real,intent(inout) :: tdet(neldet)
          integer,intent(in) :: mat_det
          integer,intent(in) :: nix !size for ix array
          integer,intent(in) :: ix(nix,numel)
          integer,intent(in) :: nvois !< max number of adjacent elems
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: jj, kk
          integer :: iad1,lgth   !< address of the first element in the list of connected elements, and number of connected elements
          integer :: iad2,lgth2  !< same for the  adjacent elements  of the adjacent element
          integer :: iel,iel_v
          integer :: iev,iev_v
          integer :: mid
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
           num_new_activated = 0
           list_new_activated(1:nvois)=0
           iad1 = ale_connectivity%ee_connect%iad_connect(ie)
           lgth = ale_connectivity%ee_connect%iad_connect(ie+1) - iad1
           do jj=1,lgth
             iev = ale_connectivity%ee_connect%connected(iad1 + jj - 1)
             if(iev == 0)cycle
             iel = elem_list_bij(iev)
             if(updown(iel) == 1)cycle ! already computed
             if(updown(iel) == -1) then ! add in narrow band
               if(mat_det /= 0)then
                 mid = ix(1,iev)
                 if(mid /= mat_det) cycle
               end if
               updown(iel) = 0
               num_new_activated = num_new_activated + 1
               list_new_activated(num_new_activated) = iel
             endif

             !update arrival time this adjacent elem (whatever is updown for the adjacent elem)
             iad2 = ale_connectivity%ee_connect%iad_connect(iev)
             lgth2 = ale_connectivity%ee_connect%iad_connect(iev+1) - iad2
             tdet_adj(1:nvois) = ep21
             xel_adj(1:3,1:nvois) = zero
             do kk=1,lgth2
                vel_adj(kk) = vel(iev)
                iev_v = ale_connectivity%ee_connect%connected(iad2 + kk - 1)
                if(iev_v == 0)cycle
                iel_v = elem_list_bij(iev_v)
                tdet_adj(kk) = tdet(iel_v)
                vel_adj(kk) = vel(iel_v)
                xel_adj(1:3,kk) = xel(1:3,iel_v)
             end do

             if(nvois == 4)then
               call eikonal_Godunov_Operator_2d(xel(1,iel), tdet(iel), xel_adj, tdet_adj, 4, Vel(iel), Vel_adj)
             elseif (nvois == 3)then
               tdet_adj(4) = ep21
               call eikonal_Godunov_Operator_2d(xel(1,iel), tdet(iel), xel_adj, tdet_adj, 3, Vel(iel), Vel_adj)
             else
               call eikonal_Godunov_Operator_3d(xel(1,iel), tdet(iel), xel_adj, tdet_adj, 6, Vel(iel), Vel_adj)
             end if



           ENDDO
        end subroutine eikonal_compute_adjacent
! ----------------------------------------------------------------------------------------------------------------------
        end module eikonal_compute_adjacent_mod
