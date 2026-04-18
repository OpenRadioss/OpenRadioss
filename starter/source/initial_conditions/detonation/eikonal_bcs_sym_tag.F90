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
!||    eikonal_bcs_sym_tag_mod   ../starter/source/initial_conditions/detonation/eikonal_bcs_sym_tag.F90
!||--- called by ------------------------------------------------------
!||    eikonal_init_start_list   ../starter/source/initial_conditions/detonation/eikonal_init_start_list.F90
!||====================================================================
      module eikonal_bcs_sym_tag_mod
      implicit none
      contains
!||====================================================================
!||    eikonal_bcs_sym_tag       ../starter/source/initial_conditions/detonation/eikonal_bcs_sym_tag.F90
!||--- called by ------------------------------------------------------
!||    eikonal_init_start_list   ../starter/source/initial_conditions/detonation/eikonal_init_start_list.F90
!||--- uses       -----------------------------------------------------
!||    detonators_mod            ../starter/share/modules1/detonators_mod.F
!||====================================================================
        subroutine eikonal_bcs_sym_tag(neldet, elem_list, uelem_list, numnod, x, nix, numel, ix, &
                                        detonators, idet, itag_boundFaces, ISYM, nvois, Lmax)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Description
! ----------------------------------------------------------------------------------------------------------------------
! Whenever a face is identifyied as laying on the symmetry plane it is tagged in the data structure itag_boundFaces.
! This information is used in the eikonal solver to apply the appropriate boundary conditions on the symmetry plane.
!
!                  +------------+
!                  |            |
!                  |      o-----|-----o <--ghost centroid
!                  |            | <--boundary face
!                  +------------+
!
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : zero, em04, em12
          use precision_mod, only : WP
          use detonators_mod , only : detonators_struct_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,intent(in) :: neldet  !< number of explosive elements
          integer,intent(in) :: numel   !< element data structure (full)
          integer,intent(in) :: numnod  !< number of nodes in the input file
          integer,intent(in) :: nix     !< size for data structure ix

          integer,intent(in) :: elem_list(neldet)  !< global elem ids  (local : 1:neldet)
          integer,intent(in) :: uelem_list(neldet) !< user elem ids
          integer,intent(in) :: ix(nix,numel)             !< elem connectivity data structure
          real(kind=WP),intent(in) :: x(3,numnod)      !< node coordinates
          integer,intent(inout) :: ISYM(2) !< global tag if symmetry condition is defined in the input file
          integer,intent(in) :: nvois
          integer,intent(in) :: idet
          type (detonators_struct_),intent(in) :: detonators
          integer,intent(inout) :: itag_boundFaces(neldet,nvois) ! for a given explosive element, several faces may be related by planes of symmetry
          real(kind=WP),intent(in) :: Lmax
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer,allocatable,dimension(:) :: indx
          integer,allocatable,dimension(:) :: int_tmp_array
          real(kind=WP),allocatable,dimension(:) :: real_tmp_array
          integer :: kk
          integer :: iad,lgth
          integer :: ICT(6,4)
          integer :: listnod(4) !< list of face nodes (2d:2max, 3d:4max)
          integer :: inod !< loop variable to identify face nodes
          integer :: Iframe1, Iframe2
          integer :: ie,iel,ifac,ii,itag
          real(kind=WP) :: OO(3,1:2) !< origins (iframe1 & iframe 2)
          real(kind=WP) :: NN(3,1:2) !< normal vectors (iframe1 & iframe 2)
          real(kind=WP) :: P(3)  !< temp point to identify if a node is laying on the symmetry plane
          real(kind=WP) :: DOTPROD, vx, vy, vz !< geometric variables used to identify if a node is laying on the symmetry plane
          real(kind=WP) :: tol !< tolerance to identify if a node is laying on the symmetry plane
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

          if(uelem_list(1) == uelem_list(1)) then
          end if

          iframe1 = detonators%point(idet)%Iframe1
          iframe2 = detonators%point(idet)%Iframe2

          If(Iframe1 > 0)then
             ISYM(1) = 1
          endif

          If(Iframe2 > 0)then
            ISYM(2) = 1
          endif

          if(nvois ==6)then
            ! 3D (HEXA) : 6  4-node faces
            ICT(1,1:4)=(/1,2,3,4/)
            ICT(2,1:4)=(/4,3,7,8/)
            ICT(3,1:4)=(/8,7,6,5/)
            ICT(4,1:4)=(/5,6,2,1/)
            ICT(5,1:4)=(/2,6,7,3/)
            ICT(6,1:4)=(/4,8,5,1/)
          else
            ! 2D (QUAD/TRIA) : 4 2-node faces
            ICT(:,:) = 0
            ICT(1,1:2)=(/1,2/)
            ICT(2,1:2)=(/2,3/)
            ICT(3,1:2)=(/3,4/)
            ICT(4,1:2)=(/4,1/)
          end if

          OO(1:3,1) = detonators%point(idet)%O1(1:3)
          OO(1:3,2) = detonators%point(idet)%O2(1:3)
          NN(1:3,1) = detonators%point(idet)%N1(1:3)
          NN(1:3,2) = detonators%point(idet)%N2(1:3)

          tol = em12 + Lmax*em04

          IF(NVOIS < 6)THEN
            do ii=1,2
              if(isym(ii) == 0)cycle
              do iel=1,neldet
                ie = elem_list(iel)
                do ifac=1,nvois
                  listnod(1:2) = ix(1+ict(ifac,1:2),ie)
                  if(listnod(1) == listnod(2) .or. listnod(2) == 0)cycle  !tria case
                  itag = 0
                  do inod=1,2
                    p(1:3) = x(1:3,listnod(inod))
                    ! <op,n> = 0  => on plane of symmetry
                    vy = p(2)-oo(2,ii)
                    vz = p(3)-oo(3,ii)
                    dotprod = (vy*nn(2,ii) + vz*nn(3,ii))
                    if(abs(dotprod) < tol) itag=itag+1
                  end do
                  if(itag == 2)then
                    itag_boundfaces(iel,ifac) = 1
                  end if
                end do
              end do
            end do

          ELSE
           do ii=1,2
              if(isym(ii) == 0)cycle
              do iel=1,neldet
                ie = elem_list(iel)
                do ifac=1,nvois
                  listnod(1:4) = ix(1+ict(ifac,1:4),ie)
                  itag = 0
                  do inod=1,4
                    if(listnod(inod) == 0)cycle
                    p(1:3) = x(1:3,listnod(inod))
                    ! <op,n> = 0  => on plane of symmetry
                    vx = p(1)-oo(1,ii)
                    vy = p(2)-oo(2,ii)
                    vz = p(3)-oo(3,ii)
                    dotprod = (vx*nn(1,ii) + vy*nn(2,ii) + vz*nn(3,ii))
                    if(abs(dotprod) < tol) itag=itag + 1
                  end do
                  if(itag >= 3)then
                      itag_boundfaces(iel,ifac) = 1
                  end if
                end do
              end do
            end do

          ENDIF

          ! itag = 0
          ! do iel=1,neldet
          !   do ifac=1,4
          !     if(itag_boundfaces(iel,ifac) > 0)then
          !       itag = itag + 1
          !       exit
          !     end if
          !   enddo
          ! end do
          ! print *, "debug, number of boundary faces =", itag

        end subroutine eikonal_bcs_sym_tag
! ----------------------------------------------------------------------------------------------------------------------
      end module eikonal_bcs_sym_tag_mod


