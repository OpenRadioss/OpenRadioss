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
!! \brief Data pre-treatment before saving in RESTART FILE
!! \details  necessary buffer specific to option /BCS/WALL/...
!
      subroutine w_bcs_proc(bcs_per_proc,cel,scel,len_ia,len_am)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
        use bcs_mod , only : bcs_struct_
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
        implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
        integer,intent(in) :: scel                       !< size for array definition
        integer,intent(in),dimension(scel) :: cel        !< application : global_elem_id -> local_elem_id
        type(bcs_struct_),intent(inout) :: bcs_per_proc  !< local data structure for bcs
        integer,intent(inout) :: len_ia,len_am           !< buffer size for records (integer and real)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
        integer, dimension(1) :: itmp
        integer :: ilen,ii,jj,ielem
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------

        itmp(1) = bcs_per_proc%num_wall
        call write_i_c(itmp,1)
        len_ia = len_ia + 1

        if(bcs_per_proc%num_wall > 0)then
          do ii=1,bcs_per_proc%num_wall
            ilen = bcs_per_proc%wall(ii)%list%size
            if(ilen > 0)then
              do jj=1, ilen
                ielem = bcs_per_proc%wall(ii)%list%elem(jj)
                bcs_per_proc%wall(ii)%list%elem(jj) = cel(ielem) !local numbering
              enddo
            end if

            call write_bcs_wall(bcs_per_proc%wall(ii))
            len_ia = len_ia + 7 + 3*ilen
            len_am = len_am + 2

          enddo!next ii
        endif

! ----------------------------------------------------------------------------------------------------------------------
        return
      end subroutine w_bcs_proc
