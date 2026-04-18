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
!||    eikonal_compute_adjacent_mod   ../starter/source/initial_conditions/detonation/eikonal_compute_adjacent.F90
!||--- called by ------------------------------------------------------
!||    eikonal_fast_marching_method   ../starter/source/initial_conditions/detonation/eikonal_fast_marching_method.F90
!||====================================================================
      module eikonal_compute_adjacent_mod
      implicit none
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
          nix,ix,nvois,itag_boundFaces,numnod,x)
!! \brief In order to apply Godunov operator the adjacent data must be used. This subroutine is collecting these data (positions, velocities, arrival times)
!! \details ALE_EE_CONNECT is used to retrive data from adjacent element (ALE,EULER only)
!
! updown(iel) = -1 : element is not in the narrow band, its arrival time has not been computed yet
! updown(iel) = 0 : element is in the narrow band, its arrival time has been computed and will be updated
! updown(iel) = 1 : element is not in the narrow band, its arrival time has been computed and will not be updated
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use ale_connectivity_mod , only : t_ale_connectivity
          use constant_mod , only : em20,two,ep21,one,zero
          use eikonal_godunov_operator_2d_mod, only : eikonal_godunov_operator_2d
          use eikonal_godunov_operator_3d_mod, only : eikonal_godunov_operator_3d
          use precision_mod, only : WP
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
          integer,intent(in) :: ie
          type (t_ale_connectivity), intent(inout) :: ale_connectivity
          integer,intent(in) :: neldet
          real(kind=WP),intent(inout) :: tdet_adj(6)
          real(kind=WP),intent(inout) :: xel_adj(3,6)
          real(kind=WP),intent(inout) :: vel_adj(6)
          integer,intent(in) :: numel
          integer, intent(in) :: elem_list_bij(numel)  ! size
          integer,intent(inout) :: updown(neldet)
          integer,intent(inout) :: num_new_activated, list_new_activated(6)
          real(kind=WP),intent(in) :: vel(neldet),xel(3,neldet)
          real(kind=WP),intent(inout) :: tdet(neldet)
          integer,intent(in) :: mat_det
          integer,intent(in) :: nix !size for ix array
          integer,intent(in) :: ix(nix,numel)
          integer,intent(in) :: nvois !< max number of adjacent elems
          integer,intent(inout) :: itag_boundFaces(neldet,nvois) ! for a given explosive element, several faces may be related by planes of symmetry
          integer,intent(in) :: numnod
          real(kind=WP),intent(in) :: x(3,numnod)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: jj, kk
          integer :: iad1,lgth   !< address of the first element in the list of connected elements, and number of connected elements
          integer :: iad2,lgth2  !< same for the  adjacent elements  of the adjacent element
          integer :: iel,iel_v
          integer :: iev,iev_v
          integer :: mid
          logical :: is_boundary
          real(kind=WP) :: PP(3,4),NN(3),DL,NORM,DF(3),UU(3),VV(3)
          integer :: ict2d(4,2)
          integer :: ict3d(6,4)
          integer inod(4)
          !data ict2d/1,2,2,3,3,4,4,1/
          !data ict3d/1,2,3,4,3,4,7,8,5,6,7,8,1,2,6,5,1,3,6,7,1,4,5,8/
          data ict2d/1,2,3,4,2,3,4,1/
          data ict3d/1,4,8,5,2,4, 2,3,7,6,6,8, 3,7,6,2,7,5, 4,8,5,1,3,1/
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
            end if

            !update arrival time this adjacent elem (whatever is updown for the adjacent elem)
            iad2 = ale_connectivity%ee_connect%iad_connect(iev)
            lgth2 = ale_connectivity%ee_connect%iad_connect(iev+1) - iad2
            tdet_adj(1:nvois) = ep21
            xel_adj(1:3,1:nvois) = zero
            do kk=1,lgth2
              vel_adj(kk) = vel(iel)
              iev_v = ale_connectivity%ee_connect%connected(iad2 + kk - 1)
              is_boundary = (itag_boundFaces(iel,kk)==1)
               if (iev_v == 0 .and. is_boundary)then
                ! treatment of ghost cell (Neumann Bcs)
                tdet_adj(kk) = tdet(iel)
                vel_adj(kk) = vel(iel)
                ! normal vector
                if(nvois < 6)then
                  !2d
                  inod(1:2) = 1 + ict2d(kk,1:2)
                  PP(2:3,1) = x(2:3, ix(inod(1), iev) )
                  PP(2:3,2) = x(2:3, ix(inod(2), iev) )
                  NN(2) =  PP(3,2)-PP(3,1)
                  NN(3) = -PP(2,2)+PP(2,1)
                  NORM = max(em20,sqrt(NN(2)*NN(2) + NN(3)*NN(3)))
                  NN(2:3) = NN(2:3)/NORM
                  DF(2:3) = PP(2:3,1)-xel(2:3,iel)
                  DL = two*dot_product(NN(2:3),DF(2:3))
                  xel_adj(1,kk) = xel(1,iel)
                  xel_adj(2,kk) = xel(2,iel) + DL*NN(2)
                  xel_adj(3,kk) = xel(3,iel) + DL*NN(3)
                else
                  !3d : (points are coplanar , is_boundary => all nodes on boundary plane
                  inod(1:4) = 1 + ict3d(kk,1:4)
                  PP(1:3,1) = x(1:3, ix(inod(1), iev) )
                  PP(1:3,2) = x(1:3, ix(inod(2), iev) )
                  PP(1:3,3) = x(1:3, ix(inod(3), iev) )

                  ! u = P1P2, v = P2P3
                  uu(:) = PP(:,2) - PP(:,1)
                  vv(:) = PP(:,3) - PP(:,2)
                  ! N = u x v
                  NN(1) = uu(2)*vv(3) - uu(3)*vv(2)
                  NN(2) = uu(3)*vv(1) - uu(1)*vv(3)
                  NN(3) = uu(1)*vv(2) - uu(2)*vv(1)
                  NORM = max(em20,sqrt(NN(1)*NN(1) + NN(2)*NN(2) + NN(3)*NN(3)))
                  NN(1:3) = NN(1:3)/NORM
                  DF(1:3) = PP(1:3,1)-xel(1:3,iel)
                  DL = two*dot_product(NN(1:3),DF(1:3))
                  xel_adj(1,kk) = xel(1,iel) + DL*NN(1)
                  xel_adj(2,kk) = xel(2,iel) + DL*NN(2)
                  xel_adj(3,kk) = xel(3,iel) + DL*NN(3)
                end if

               elseif(iev_v /= 0)then
                ! existing adjacent element
                iel_v = elem_list_bij(iev_v)
                tdet_adj(kk) = tdet(iel_v)
                vel_adj(kk) = vel(iel_v)
                xel_adj(1:3,kk) = xel(1:3,iel_v)
              end if
            end do

            if(nvois == 4)then
              call eikonal_Godunov_Operator_2d(xel(1,iel), tdet(iel), xel_adj, tdet_adj, 4, Vel(iel), Vel_adj)
            else if (nvois == 3)then
              tdet_adj(4) = ep21
              call eikonal_Godunov_Operator_2d(xel(1,iel), tdet(iel), xel_adj, tdet_adj, 3, Vel(iel), Vel_adj)
            else
              call eikonal_Godunov_Operator_3d(xel(1,iel), tdet(iel), xel_adj, tdet_adj, 6, Vel(iel), Vel_adj)
            end if

          END DO
        end subroutine eikonal_compute_adjacent
! ----------------------------------------------------------------------------------------------------------------------
      end module eikonal_compute_adjacent_mod
