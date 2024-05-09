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
      module inter_sh_offset_mod
#include "my_real.inc"
        type sh_offset_
          integer ::  nsh_oset                                 ! number of offset shell to be projected
          integer, dimension(:,:) , allocatable :: ix_offset      ! (4,nsh_oset)
          integer, dimension(:)  ,  allocatable :: intag       ! (numnod) node connected to offset shell
          integer, dimension(:,:),  allocatable :: iad_offset    ! (2,nspmd+1) comm work array
          integer, dimension(:)  ,  allocatable :: fr_offset     ! comm work array
          my_real, dimension(:)  ,  allocatable :: offset_n    ! (numnod) nodal offset
          my_real, dimension(:,:) , allocatable :: norm_n      ! (3,numnod) nodal normal
        end type  sh_offset_
      end module inter_sh_offset_mod
