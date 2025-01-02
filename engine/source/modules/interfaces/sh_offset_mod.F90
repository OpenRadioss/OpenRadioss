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
      !||    inter_sh_offset_mod   ../engine/source/modules/interfaces/sh_offset_mod.F90
      !||--- called by ------------------------------------------------------
      !||    inter_sh_offset_dim   ../engine/source/interfaces/shell_offset/inter_offset_dim.F90
      !||    inter_sh_offset_ini   ../engine/source/interfaces/shell_offset/inter_offset_ini.F90
      !||    offset_nproj          ../engine/source/interfaces/shell_offset/offset_nproj.F90
      !||    resol                 ../engine/source/engine/resol.F
      !||    resol_init            ../engine/source/engine/resol_init.F
      !||====================================================================
      module inter_sh_offset_mod
#include "my_real.inc"
        type sh_offset_
          integer ::  nsh_oset                                 ! number of offset shell to be projected
          integer ::  nnsh_oset                                ! number of nodal offset 
          integer, dimension(:,:) , allocatable :: ix_offset   ! (4,nsh_oset)
          integer, dimension(:)  ,  allocatable :: intag       ! (numnod) global node to local offset
          integer, dimension(:)  ,  allocatable :: indexg      ! (nnsh_oset) to global node number
          integer, dimension(:,:),  allocatable :: iad_offset  ! (2,nspmd+1) comm work array
          integer, dimension(:)  ,  allocatable :: fr_offset   ! comm work array
          my_real, dimension(:)  ,  allocatable :: offset_n    ! (nnsh_oset) nodal offset
          my_real, dimension(:,:) , allocatable :: norm_n      ! (3,nnsh_oset) nodal normal
          double precision,dimension(:,:,:), allocatable :: norm_n6 ! (6,3,nnsh_oset) nodal normal P/ON
        end type  sh_offset_
      end module inter_sh_offset_mod
