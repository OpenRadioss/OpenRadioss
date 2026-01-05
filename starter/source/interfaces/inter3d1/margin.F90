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
!||    margin_reduction_mod   ../starter/source/interfaces/inter3d1/margin.F90
!||--- called by ------------------------------------------------------
!||    i25buc_vox1            ../starter/source/interfaces/inter3d1/i25buc_vox1.F
!||    i7buc_vox1             ../starter/source/interfaces/inter3d1/i7buc_vox1.F
!||====================================================================
      module margin_reduction_mod
        implicit none
      contains
! ======================================================================================================================
!                                                   procedures
! ======================================================================================================================
!||====================================================================
!||    margin_reduction   ../starter/source/interfaces/inter3d1/margin.F90
!||--- called by ------------------------------------------------------
!||    i25buc_vox1        ../starter/source/interfaces/inter3d1/i25buc_vox1.F
!||    i7buc_vox1         ../starter/source/interfaces/inter3d1/i7buc_vox1.F
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine margin_reduction(X,NUMNOD,IRECT,NRTM,NSV,NSN,DRAD,GAP,DGAPLOAD,BUMULT,STIFN,DD0)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod, only : FOUR
          use precision_mod, only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: NRTM !< number of main segment
          integer, intent(in) :: nsn !< number of S nodes
          integer, intent(in) :: numnod !< number of nodes
          integer, intent(in) :: NSV(nsn) !< S nodes
          real(kind=WP), intent(in) :: X(3,NUMNOD) !< coordinates of the nodes
          real(kind=WP), intent(in) :: STIFN(nsn) !< stiffness of the interface
          integer, intent(in) :: IRECT(4,NRTM) !< nodes of the main segments
          real(kind=WP), intent(in) :: DRAD !< radiation length
          real(kind=WP), intent(in) :: DGAPLOAD !< gap load
          real(kind=WP), intent(in) :: GAP !< gap
          real(kind=WP), intent(in) :: BUMULT !< multiplicator
          real(kind=WP), intent(inout) :: DD0 !< margin reduction to be updated
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: candidate_count
          integer :: min_candidate_count
          integer :: i, j, k
          real(kind=wp) :: dsav !< temporary variable for margin reduction
          real(kind=wp) :: xmax, xmin, ymax, ymin, zmax, zmin
          real(kind=wp) :: xmax_basis, xmin_basis, ymax_basis, ymin_basis, zmax_basis, zmin_basis
          real(kind=wp), dimension(:), allocatable :: volume
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
          dsav = DD0
          allocate(volume(nrtm))
          ! Initialize bounding box with first segment (avoid vector-subscript temporaries)
          ! extend bounding box for remaining segments
          do i = 1, nrtm
            volume(i) = 0.0_wp
            ! Unrolled loop over 4 nodes of each segment
            j = irect(1,i)
            xmin = x(1,j); xmax = x(1,j)
            ymin = x(2,j); ymax = x(2,j)
            zmin = x(3,j); zmax = x(3,j)
            j = irect(2,i)
            xmin = min(xmin, x(1,j)); xmax = max(xmax, x(1,j))
            ymin = min(ymin, x(2,j)); ymax = max(ymax, x(2,j))
            zmin = min(zmin, x(3,j)); zmax = max(zmax, x(3,j))
            j = irect(3,i)
            xmin = min(xmin, x(1,j)); xmax = max(xmax, x(1,j))
            ymin = min(ymin, x(2,j)); ymax = max(ymax, x(2,j))
            zmin = min(zmin, x(3,j)); zmax = max(zmax, x(3,j))
            j = irect(4,i)
            xmin = min(xmin, x(1,j)); xmax = max(xmax, x(1,j))
            ymin = min(ymin, x(2,j)); ymax = max(ymax, x(2,j))
            zmin = min(zmin, x(3,j)); zmax = max(zmax, x(3,j))
            !bounding volume
            volume(i) = (xmax - xmin) * (ymax - ymin) * (zmax - zmin)
          enddo

          !find the index of the biggest volume
          k = maxloc(volume(1:nrtm), dim=1)
          !set [xyz][min/max]_basis with the biggest volume segment
          j = irect(1,k)
          xmin_basis = x(1,j); xmax_basis = x(1,j)
          ymin_basis = x(2,j); ymax_basis = x(2,j)
          zmin_basis = x(3,j); zmax_basis = x(3,j)
          j = irect(2,k)
          xmin_basis = min(xmin_basis, x(1,j)); xmax_basis = max(xmax_basis, x(1,j))
          ymin_basis = min(ymin_basis, x(2,j)); ymax_basis = max(ymax_basis, x(2,j))
          zmin_basis = min(zmin_basis, x(3,j)); zmax_basis = max(zmax_basis, x(3,j))
          j = irect(3,k)
          xmin_basis = min(xmin_basis, x(1,j)); xmax_basis = max(xmax_basis, x(1,j))
          ymin_basis = min(ymin_basis, x(2,j)); ymax_basis = max(ymax_basis, x(2,j))
          zmin_basis = min(zmin_basis, x(3,j)); zmax_basis = max(zmax_basis, x(3,j))
          j = irect(4,k)
          xmin_basis = min(xmin_basis, x(1,j)); xmax_basis = max(xmax_basis, x(1,j))
          ymin_basis = min(ymin_basis, x(2,j)); ymax_basis = max(ymax_basis, x(2,j))
          zmin_basis = min(zmin_basis, x(3,j)); zmax_basis = max(zmax_basis, x(3,j))
          xmax = xmax_basis + max(gap+dgapload,drad)
          ymax = ymax_basis + max(gap+dgapload,drad)
          zmax = zmax_basis + max(gap+dgapload,drad)
          xmin = xmin_basis - max(gap+dgapload,drad)
          ymin = ymin_basis - max(gap+dgapload,drad)
          zmin = zmin_basis - max(gap+dgapload,drad)
          candidate_count = 0
          do  i=1,nsn
            j=nsv(i)
            if(stifn(i) == 0.0_wp) cycle
            ! count the number of nsn within the box defined by xmin, xmax, ymin, ymax, zmin, zmax
            if( x(1,j) >= xmin .and. x(1,j) <= xmax&
              .and. x(2,j) >= ymin .and. x(2,j) <= ymax&
              .and. x(3,j) >= zmin .and. x(3,j) <= zmax ) then
              candidate_count = candidate_count + 1
            end if
          enddo
          min_candidate_count = candidate_count


          candidate_count = nsn
          do while(candidate_count > max(1000 + min_candidate_count, nsn/100))
            xmax = xmax_basis + bumult*dd0 + max(gap+dgapload,drad)
            ymax = ymax_basis + bumult*dd0 + max(gap+dgapload,drad)
            zmax = zmax_basis + bumult*dd0 + max(gap+dgapload,drad)
            xmin = xmin_basis - bumult*dd0 - max(gap+dgapload,drad)
            ymin = ymin_basis - bumult*dd0 - max(gap+dgapload,drad)
            zmin = zmin_basis - bumult*dd0 - max(gap+dgapload,drad)
            candidate_count = 0
            do  i=1,nsn
              j=nsv(i)
              ! count the number of nsn within the box defined by xmin, xmax, ymin, ymax, zmin, zmax
              if( x(1,j) >= xmin .and. x(1,j) <= xmax&
                .and. x(2,j) >= ymin .and. x(2,j) <= ymax&
                .and. x(3,j) >= zmin .and. x(3,j) <= zmax ) then
                candidate_count = candidate_count + 1
              end if
            enddo
            if(candidate_count <= max(1000 + min_candidate_count, nsn/100)) exit
            dd0 = dd0 * 0.75
            write(6,*) "reduction of margin for single element interface, new margin size: ", bumult*dd0, candidate_count,nsn
            if(dd0 < (dsav*0.001)) exit ! avoid too small margin
          enddo
          deallocate(volume)


        end subroutine margin_reduction
      end module margin_reduction_mod
