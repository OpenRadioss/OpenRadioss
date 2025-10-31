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
!||    min_dist_grnod_to_surface_mod   ../starter/source/model/transformation/min_distance_grnod_to_surface.F90
!||--- called by ------------------------------------------------------
!||    lectrans                        ../starter/source/model/transformation/lectrans.F
!||--- uses       -----------------------------------------------------
!||====================================================================
      module min_dist_grnod_to_surface_mod
        use precision_mod, only : WP
        implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Compute minimum distance from a group of nodes to a surface
!=======================================================================================================================
!||====================================================================
!||    min_dist_grnod_to_surface   ../starter/source/model/transformation/min_distance_grnod_to_surface.F90
!||--- called by ------------------------------------------------------
!||    lectrans                    ../starter/source/model/transformation/lectrans.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                      ../starter/source/output/message/message.F
!||--- uses       -----------------------------------------------------
!||    message_mod                 ../starter/share/message_module/message_mod.F
!||====================================================================
        subroutine min_dist_grnod_to_surface(nodes , n_nodes, surf_nodes, n_surf_nodes,x    , &
                                         &   numnod, pflag  , idir      , gap         ,isk  , &
                                         &   skew  , lskew  , sskew     ,id           ,titr , &
                                         &   nchartitle)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use message_mod, only : ancmsg, msgerror, aninfo
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer,                                   intent(in) :: nodes(n_nodes)               !< Group of nodes
          integer,                                   intent(in) :: n_nodes                      !< Number of nodes in group
          integer,                                   intent(in) :: surf_nodes(n_surf_nodes)     !< Surface nodes
          integer,                                   intent(in) :: n_surf_nodes                 !< Number of surface nodes
          integer,                                   intent(in) :: numnod                       !< Total number of nodes in model
          integer,                                   intent(in) :: pflag                        !< 1: position grnod above surface in idir direction, 2: position grnod below surface in idir direction
          integer,                                   intent(in) :: idir                         !< Direction (1,2,3) in which to position grnod with respect to surface
          integer,                                   intent(in) :: isk                          !< Index of skew (0 if no skew)
          integer,                                   intent(in) :: lskew                        !< Length of skew
          integer,                                   intent(in) :: sskew                        !< Sum of skews
          real(kind=WP),                             intent(inout) :: x(3, numnod)              !< Coordinates of all nodes in model
          real(kind=WP),                             intent(in) :: gap                          !< Gap between grnod and surface in idir direction
          real(kind=WP),                             intent(in) :: skew(lskew,sskew/lskew)      !< Skew matrices
          integer,                                   intent(in) ::  nchartitle                  !< Length of title string
          character(len=nchartitle),                 intent(in) ::  titr                        !< Title of transformation
          integer,                                   intent(in) ::  id                          !< ID of transformation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i, igrnod, isurfnod                                            !< local entities id's
          real(kind=WP) :: min_pos, max_pos                                         !< min and max positions of group nodes and surface nodes in idir direction
          real(kind=WP) :: minx_isurfnod, miny_isurfnod, minz_isurfnod              !< min coordinates of surface nodes
          real(kind=WP) :: maxx_isurfnod, maxy_isurfnod, maxz_isurfnod              !< max coordinates of surface nodes
          real(kind=WP) :: minx_igrnod, miny_igrnod, minz_igrnod                    !< min coordinates of group nodes
          real(kind=WP) :: maxx_igrnod, maxy_igrnod, maxz_igrnod                    !< max coordinates of group nodes
          real(kind=WP) :: invertskew_x(3),invertskew_Y(3),invertskew_Z(3)          !< Inverse of skew matrix
          real(kind=WP) :: orig(3)                                                  !< origin of skew system
          real(kind=WP) :: detskew                                                  !< Determinant of skew matrix
          real(kind=WP) :: norm(3)                                                  !< Norms of inverse skew vectors
          real(kind=WP) :: dist                                                     !< Distance to move grnod to surface
          real(kind=WP), dimension(:,:), allocatable :: xn                          !< Transformed coordinates of group nodes
          real(kind=WP), dimension(:,:), allocatable :: xsn                         !< Transformed coordinates of surface nodes
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(xn(3, n_nodes), xsn(3, n_surf_nodes))
          xn(:,:) = 0.0_WP
          xsn(:,:) = 0.0_WP
          max_pos = -huge(1.0_WP)
          min_pos = huge(1.0_WP)
          orig(:) = huge(1.0_WP)


          ! Initialize min and max coordinates

          minx_isurfnod = huge(1.0_WP)
          miny_isurfnod = huge(1.0_WP)
          minz_isurfnod = huge(1.0_WP)
          maxx_isurfnod = -huge(1.0_WP)
          maxy_isurfnod = -huge(1.0_WP)
          maxz_isurfnod = -huge(1.0_WP)

          minx_igrnod = huge(1.0_WP)
          miny_igrnod = huge(1.0_WP)
          minz_igrnod = huge(1.0_WP)
          maxx_igrnod = -huge(1.0_WP)
          maxy_igrnod = -huge(1.0_WP)
          maxz_igrnod = -huge(1.0_WP)

          if (isk > 0) then
            orig(1)=skew(10,isk)
            orig(2)=skew(11,isk)
            orig(3)=skew(12,isk)

            ! Apply skew transformation to group nodes and surface nodes
            do i=1, n_nodes
              igrnod = nodes(i)
              xn(1,i) = skew(1,isk)*(x(1,igrnod) - orig(1)) + skew(2,isk)*(x(2,igrnod) - orig(2)) + &
                     &  skew(3,isk)*(x(3,igrnod) - orig(3))
              xn(2,i) = skew(4,isk)*(x(1,igrnod) - orig(1)) + skew(5,isk)*(x(2,igrnod) - orig(2)) + &
                     &  skew(6,isk)*(x(3,igrnod) - orig(3))
              xn(3,i) = skew(7,isk)*(x(1,igrnod) - orig(1)) + skew(8,isk)*(x(2,igrnod) - orig(2)) + &
                     &  skew(9,isk)*(x(3,igrnod) - orig(3))
            end do
            do i=1, n_surf_nodes
              isurfnod = surf_nodes(i)
              xsn(1,i) = skew(1,isk)*(x(1,isurfnod) - orig(1)) + skew(2,isk)*(x(2,isurfnod) - orig(2)) + &
                     &   skew(3,isk)*(x(3,isurfnod) - orig(3))
              xsn(2,i) = skew(4,isk)*(x(1,isurfnod) - orig(1)) + skew(5,isk)*(x(2,isurfnod) - orig(2)) + &
                     &   skew(6,isk)*(x(3,isurfnod) - orig(3))
              xsn(3,i) = skew(7,isk)*(x(1,isurfnod) - orig(1)) + skew(8,isk)*(x(2,isurfnod) - orig(2)) + &
                     &   skew(9,isk)*(x(3,isurfnod) - orig(3))
            end do
          else
            do i=1, n_nodes
              igrnod = nodes(i)
              xn(1,i) = x(1,igrnod)
              xn(2,i) = x(2,igrnod)
              xn(3,i) = x(3,igrnod)
            end do
            do i=1, n_surf_nodes
              isurfnod = surf_nodes(i)
              xsn(1,i) = x(1,isurfnod)
              xsn(2,i) = x(2,isurfnod)
              xsn(3,i) = x(3,isurfnod)
            end do
          endif
          
          do i=1, n_surf_nodes
            isurfnod = surf_nodes(i)
            minx_isurfnod = min(minx_isurfnod, xsn(1,i))
            maxx_isurfnod = max(maxx_isurfnod, xsn(1,i))
            miny_isurfnod = min(miny_isurfnod, xsn(2,i))
            maxy_isurfnod = max(maxy_isurfnod, xsn(2,i))
            minz_isurfnod = min(minz_isurfnod, xsn(3,i))
            maxz_isurfnod = max(maxz_isurfnod, xsn(3,i))
          end do

          do i=1, n_nodes
            igrnod = nodes(i)
            minx_igrnod = min(minx_igrnod, xn(1,i))
            maxx_igrnod = max(maxx_igrnod, xn(1,i))
            miny_igrnod = min(miny_igrnod, xn(2,i))
            maxy_igrnod = max(maxy_igrnod, xn(2,i))
            minz_igrnod = min(minz_igrnod, xn(3,i))
            maxz_igrnod = max(maxz_igrnod, xn(3,i))
          end do
          
          if (pflag == 1) then
            if (idir == 1) then
              max_pos = minx_isurfnod
              min_pos = maxx_igrnod
            else if (idir == 2) then
              max_pos = miny_isurfnod
              min_pos = maxy_igrnod
            else ! if (idir == 3) then
              max_pos = minz_isurfnod
              min_pos = maxz_igrnod
            end if

            if (max_pos - min_pos - gap < 0.0_WP) then
              call ancmsg(msgid=3111,       &
                    &     msgtype=msgerror, &
                    &     anmode=aninfo,    &
                    &     i1=id,            &
                    &     c1=titr)
            end if
          else if (pflag == 2) then
            if (idir == 1) then
              max_pos = minx_igrnod
              min_pos = maxx_isurfnod
            else if (idir == 2) then
              max_pos = miny_igrnod
              min_pos = maxy_isurfnod
            else ! if (idir == 3) then
              max_pos = minz_igrnod
              min_pos = maxz_isurfnod
            end if

            if (max_pos - min_pos - gap < 0.0_WP) then
              call ancmsg(msgid=3111,       &
                    &     msgtype=msgerror, &
                    &     anmode=aninfo,    &
                    &     i1=id,            &
                    &     c1=titr)
            end if
          end if

          dist = max_pos - min_pos - gap

          ! Compute inverse of skew matrix for transforming back to global coordinates
          ! skew is a 3x3 matrix stored in column-major order

          ! error : correct above
          ! skew_X = (skew(1,isk), skew(2,isk), skew(3,isk)) ! - X axis direction
          ! skew_Y = (skew(4,isk), skew(5,isk), skew(6,isk)) ! - Y axis direction
          ! skew_Z = (skew(7,isk), skew(8,isk), skew(9,isk)) ! - Z axis direction

          if (isk > 0) then
            detskew = (skew(1,isk)*skew(5,isk)*skew(9,isk)) + (skew(4,isk)*skew(8,isk)*skew(3,isk))&
                  & + (skew(7,isk)*skew(2,isk)*skew(6,isk)) &
                  & - (skew(7,isk)*skew(5,isk)*skew(3,isk)) - (skew(8,isk)*skew(6,isk)*skew(1,isk))&
                  & - (skew(9,isk)*skew(2,isk)*skew(4,isk))

            detskew = max(detskew, 1e-20)

            ! Compute inverse using formula for 3x3 matrix
            ! inv(A) = 1/det(A) * adj(A)

            invertskew_x(1) = (skew(5,isk)*skew(9,isk)-skew(6,isk)*skew(8,isk))/detskew
            invertskew_x(2) = (skew(8,isk)*skew(3,isk)-skew(2,isk)*skew(9,isk))/detskew
            invertskew_x(3) = (skew(2,isk)*skew(6,isk)-skew(5,isk)*skew(3,isk))/detskew

            invertskew_Y(1) = (skew(7,isk)*skew(6,isk)-skew(4,isk)*skew(9,isk))/detskew
            invertskew_Y(2) = (skew(1,isk)*skew(9,isk)-skew(7,isk)*skew(3,isk))/detskew
            invertskew_Y(3) = (skew(4,isk)*skew(3,isk)-skew(1,isk)*skew(6,isk))/detskew

            invertskew_Z(1) = (skew(4,isk)*skew(8,isk)-skew(7,isk)*skew(5,isk))/detskew
            invertskew_Z(2) = (skew(7,isk)*skew(2,isk)-skew(1,isk)*skew(8,isk))/detskew
            invertskew_Z(3) = (skew(1,isk)*skew(5,isk)-skew(4,isk)*skew(2,isk))/detskew

            ! normalize the inverse vectors to ensure they are unit vectors
            norm(1) = sqrt(invertskew_x(1)**2 + invertskew_x(2)**2 + invertskew_x(3)**2)
            norm(2) = sqrt(invertskew_Y(1)**2 + invertskew_Y(2)**2 + invertskew_Y(3)**2)
            norm(3) = sqrt(invertskew_Z(1)**2 + invertskew_Z(2)**2 + invertskew_Z(3)**2)
          
            invertskew_x(1) = invertskew_x(1)/norm(1)
            invertskew_x(2) = invertskew_x(2)/norm(1)
            invertskew_x(3) = invertskew_x(3)/norm(1)

            invertskew_Y(1) = invertskew_Y(1)/norm(2)
            invertskew_Y(2) = invertskew_Y(2)/norm(2)
            invertskew_Y(3) = invertskew_Y(3)/norm(2)

            invertskew_Z(1) = invertskew_Z(1)/norm(3)
            invertskew_Z(2) = invertskew_Z(2)/norm(3)
            invertskew_Z(3) = invertskew_Z(3)/norm(3)
          else
            ! For global coordinates, set inverse to identity
            detskew = 1.0_WP
            invertskew_x(1) = 1.0_WP
            invertskew_x(2) = 0.0_WP
            invertskew_x(3) = 0.0_WP

            invertskew_Y(1) = 0.0_WP
            invertskew_Y(2) = 1.0_WP
            invertskew_Y(3) = 0.0_WP

            invertskew_Z(1) = 0.0_WP
            invertskew_Z(2) = 0.0_WP
            invertskew_Z(3) = 1.0_WP
          end if

          ! Move group nodes in idir direction by dist

          if (isk == 0) then
            ! global translation of group nodes in idir direction
            do i=1, n_nodes
              igrnod = nodes(i)
              if (pflag == 1)  then
                x(idir,igrnod) = xn(idir,i) + dist
              else if (pflag == 2) then
                x(idir,igrnod) = xn(idir,i) - dist
              end if
            end do
          else if (isk > 0) then
            ! skewed translation of group nodes in idir direction
            do i=1, n_nodes
              igrnod = nodes(i)
              !xn(idir,i) = xn(idir,i) - dist
              if (pflag == 1)  then
                xn(idir,i) = xn(idir,i) + dist
              else if (pflag == 2) then
                xn(idir,i) = xn(idir,i) - dist
              end if
              ! local to global transformation after translation in local system

              x(1,igrnod) = invertskew_x(1) * (xn(1,i)) + invertskew_x(2) * (xn(2,i)) &
                        & + invertskew_x(3) * (xn(3,i)) + orig(1)
              x(2,igrnod) = invertskew_Y(1) * (xn(1,i)) + invertskew_Y(2) * (xn(2,i)) &
                        & + invertskew_Y(3) * (xn(3,i)) + orig(2)
              x(3,igrnod) = invertskew_Z(1) * (xn(1,i)) + invertskew_Z(2) * (xn(2,i)) &
                        & + invertskew_Z(3) * (xn(3,i)) + orig(3)
            end do
          end if

          if(allocated(xn)) deallocate(xn)
          if(allocated(xsn)) deallocate(xsn)
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine min_dist_grnod_to_surface
      end module min_dist_grnod_to_surface_mod
