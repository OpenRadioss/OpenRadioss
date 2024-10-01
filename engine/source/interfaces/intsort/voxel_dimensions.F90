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

      !||====================================================================
      !||    voxel_dimensions_mod         ../engine/source/interfaces/intsort/voxel_dimensions.F90
      !||--- called by ------------------------------------------------------
      !||    inter7_collision_detection   ../engine/source/interfaces/intsort/inter7_collision_detection.F90
      !||    inter_prepare_sort           ../engine/source/interfaces/generic/inter_prepare_sort.F
      !||====================================================================
      module voxel_dimensions_mod
      contains
      !||====================================================================
      !||    compute_voxel_dimensions   ../engine/source/interfaces/intsort/voxel_dimensions.F90
      !||--- called by ------------------------------------------------------
      !||    inter_prepare_sort         ../engine/source/interfaces/generic/inter_prepare_sort.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    constant_mod               ../common_source/modules/constant_mod.F
      !||    inter_struct_mod           ../engine/share/modules/inter_struct_mod.F
      !||    my_alloc_mod               ../common_source/tools/memory/my_alloc.F90
      !||====================================================================
        subroutine compute_voxel_dimensions(nrtm,nmn, inter_struct)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use inter_struct_mod
          use MY_ALLOC_MOD, only: my_alloc
          use constant_mod, only: third
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
          integer, intent(in) :: nmn !< number of main nodes
          integer, intent(in) :: nrtm !< number of main segments
          type(inter_struct_type), intent(inout) :: inter_struct !< interface structure
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer  :: nbx, nby, nbz
          my_real :: aaa
          integer(kind=8) :: nbx8, nby8, nbz8, res8
          integer :: res
          integer(8), parameter :: lvoxel = HUGE(nbx)
          integer :: i
! ----------------------------------------------------------------------------------------------------------------------

          inter_struct%istart = 1 !< starting group
          if (nmn /= 0) then
            aaa = sqrt(nmn / &
            & ((inter_struct%box_limit_main(7) - inter_struct%box_limit_main(10)) *&
            & (inter_struct%box_limit_main(8) - inter_struct%box_limit_main(11)) &
            & + (inter_struct%box_limit_main(8) - inter_struct%box_limit_main(11)) *&
            & (inter_struct%box_limit_main(9) - inter_struct%box_limit_main(12)) &
            & + (inter_struct%box_limit_main(9) - inter_struct%box_limit_main(12)) *&
            & (inter_struct%box_limit_main(7) - inter_struct%box_limit_main(10))))
          else
            aaa = 0
          endif

          aaa = 0.75 * aaa

          nbx = nint(aaa * (inter_struct%box_limit_main(7) - inter_struct%box_limit_main(10)))
          nby = nint(aaa * (inter_struct%box_limit_main(8) - inter_struct%box_limit_main(11)))
          nbz = nint(aaa * (inter_struct%box_limit_main(9) - inter_struct%box_limit_main(12)))

          nbx = max(nbx, 1)
          nby = max(nby, 1)
          nbz = max(nbz, 1)

          nbx8 = nbx
          nby8 = nby
          nbz8 = nbz
          res8 = (nbx8 + 2) * (nby8 + 2) * (nbz8 + 2)

          if (res8 > lvoxel) then
            aaa = lvoxel
            aaa = aaa / ((nbx8 + 2) * (nby8 + 2) * (nbz8 + 2))
            aaa = aaa ** third
            nbx = int((nbx + 2) * aaa) - 2
            nby = int((nby + 2) * aaa) - 2
            nbz = int((nbz + 2) * aaa) - 2
            nbx = max(nbx, 1)
            nby = max(nby, 1)
            nbz = max(nbz, 1)
          endif
          nbx8=nbx
          nby8=nby
          nbz8=nbz
          res8=(nbx8+2)*(nby8+2)*(nbz8+2)

          if(res8 > lvoxel) then
            nbx = min(100,max(nbx8,1))
            nby = min(100,max(nby8,1))
            nbz = min(100,max(nbz8,1))
          endif
          res = (nbx+2)*(nby+2)*(nbz+2)

!$OMP SINGLE
          if(nrtm > 0) then
            if(allocated(inter_struct%voxel) .and. inter_struct%voxel_size < res8) deallocate(inter_struct%voxel)
            if(.not.allocated(inter_struct%voxel)) then
              call my_alloc(inter_struct%voxel,res8)
              inter_struct%voxel_size = res8
            endif
            do i=1,inter_struct%voxel_size
              inter_struct%voxel(i)=0
            enddo
            inter_struct%nbx = nbx
            inter_struct%nby = nby
            inter_struct%nbz = nbz
          else
            inter_struct%nbx = 0
            inter_struct%nby = 0
            inter_struct%nbz = 0
            inter_struct%voxel_size = 0
          end if
!$OMP END SINGLE NOWAIT
        end subroutine compute_voxel_dimensions
      end module voxel_dimensions_mod
