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
#include "my_real.inc"
!-----------------------------------------------------------------------
module loads_mod
!-----------------------------------------------------------------------
   use pload_cyl_mod
   use domdec_load_mod
   use inivel_mod
!-----------------------------------------------------------------------
   type loads_
      integer :: nload_cyl
      integer :: nload_cload                                          !< nb of concentrated loads
      integer :: nload_pload                                          !< nb of pressure loads
      integer :: ninivelt                                             !< nb of inivel (/inivel) w/ t_start
      integer :: ninivelt_g                                           !< max nb of inivel (each domain) w/ t_start
      type (press_cyl_) ,dimension(:) ,allocatable   :: load_cyl
      type (domdec_load_), dimension(:), allocatable :: cyl_restart
      type (inivel_), dimension(:), allocatable      :: inivelt
      integer :: s_global_segment_id
      integer, dimension(:,:), allocatable :: global_segment_id
      integer, dimension(:,:), allocatable :: index_load              !< index : global load id --> local load id
   end type loads_
!-----------------------------------------------------------------------
end module loads_mod
