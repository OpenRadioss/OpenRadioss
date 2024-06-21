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
module imp_intbufdef
!-----------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!! \brief Common Module Implicit interfaces
! ----------------------------------------------------------------------------------------------------------------------
type imp_intbuf_struct_
!=================================================
! define typeintbuf_struct_ for interface buffer imp_intbuf_tab
!=======================================================================
! define sizes (integers arrays)
!=======================================================================
      integer ::   s_i_stok     !  replace num_imp (after)
      integer ::   s_cand_n     !  replace ns_imp (after)
      integer ::   s_cand_e     !  replace ne_imp (after)
      integer ::   s_indsubt    !  replace ind_imp (after)
!=======================================================================
! define sizes (float arrays)
!=======================================================================
      integer ::   s_hj           !  (4,s_i_stok)
      integer ::   s_nj           !  (3,s_i_stok)       normal
      integer ::   s_stif         !  (3,s_i_stok)       n
!=======================================================================
! define arrays (integers arrays)
!=======================================================================
      integer, dimension(:) , pointer ::  i_stok
      integer, dimension(:) , pointer ::  cand_n
      integer, dimension(:) , pointer ::  cand_e
      integer, dimension(:) , pointer ::  indsubt
!type24
!=======================================================================
! define arrays (float arrays)
!=======================================================================
      my_real, dimension(:) , pointer ::   hj
      my_real, dimension(:) , pointer ::   nj
      my_real, dimension(:) , pointer ::   stif
!=======================================================================
   end type imp_intbuf_struct_
!=======================================================================
end module imp_intbufdef


module imp_intbuf
! ----------------------------------------------------------------------------------------------------------------------
!! \brief Common Module Implicit interfaces
! ----------------------------------------------------------------------------------------------------------------------
   use intbufdef_mod
   use imp_intbufdef
   type(intbuf_struct_),dimension(:),allocatable :: intbuf_tab_cp
   type(imp_intbuf_struct_),dimension(:),allocatable :: intbuf_tab_imp
end module imp_intbuf


module imp_inttd
! ----------------------------------------------------------------------------------------------------------------------
!! \brief Common Module for penetration detection
! ----------------------------------------------------------------------------------------------------------------------
   integer, dimension(:),allocatable :: ns_imp1,iad1_nin
   integer, dimension(:),allocatable :: ne_imp1,ind_imp1
!
end module imp_inttd

