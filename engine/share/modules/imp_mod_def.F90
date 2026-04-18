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
!||    imp_intbufdef    ../engine/share/modules/imp_mod_def.F90
!||--- called by ------------------------------------------------------
!||    i24ke3           ../engine/source/interfaces/int24/i24ke3.F
!||    imp_intbuf       ../engine/share/modules/imp_mod_def.F90
!||    imp_intbuf_ini   ../engine/source/implicit/imp_solv.F
!||--- uses       -----------------------------------------------------
!||    precision_mod    ../common_source/modules/precision_mod.F90
!||====================================================================
      module imp_intbufdef
        use precision_mod, only : WP
        implicit none
        private :: WP
!-----------------------------------------------------------------------
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
          integer, dimension(:) , pointer ::  i_stok => null()
          integer, dimension(:) , pointer ::  cand_n => null()
          integer, dimension(:) , pointer ::  cand_e => null()
          integer, dimension(:) , pointer ::  indsubt => null()
!type24
!=======================================================================
! define arrays (float arrays)
!=======================================================================
          real(kind=WP), dimension(:) , pointer ::   hj => null()
          real(kind=WP), dimension(:) , pointer ::   nj => null()
          real(kind=WP), dimension(:) , pointer ::   stif => null()
!=======================================================================
        end type imp_intbuf_struct_
!=======================================================================
      end module imp_intbufdef


!||====================================================================
!||    imp_intbuf      ../engine/share/modules/imp_mod_def.F90
!||--- called by ------------------------------------------------------
!||    cp_impbuf       ../engine/source/implicit/produt_v.F
!||    deallocm_imp    ../engine/source/implicit/imp_solv.F
!||    dim_int7        ../engine/source/implicit/ind_glob_k.F
!||    dim_int_k       ../engine/source/implicit/ind_glob_k.F
!||    dim_kine_i      ../engine/source/implicit/ind_glob_k.F
!||    i24main_tri     ../engine/source/interfaces/intsort/i24main_tri.F
!||    i24mainf        ../engine/source/interfaces/int24/i24main.F
!||    i25main_tri     ../engine/source/interfaces/intsort/i25main_tri.F
!||    imp_int_k       ../engine/source/implicit/imp_int_k.F
!||    ind_int_k       ../engine/source/implicit/ind_glob_k.F
!||    resol           ../engine/source/engine/resol.F
!||--- uses       -----------------------------------------------------
!||    imp_intbufdef   ../engine/share/modules/imp_mod_def.F90
!||    intbufdef_mod   ../common_source/modules/interfaces/intbufdef_mod.F90
!||====================================================================
      module imp_intbuf
! ----------------------------------------------------------------------------------------------------------------------
!! \brief Common Module Implicit interfaces
! ----------------------------------------------------------------------------------------------------------------------
        use intbufdef_mod
        use imp_intbufdef
        type(intbuf_struct_),dimension(:),allocatable :: intbuf_tab_cp
        type(imp_intbuf_struct_),dimension(:),allocatable :: intbuf_tab_imp
      end module imp_intbuf


!||====================================================================
!||    imp_inttd    ../engine/share/modules/imp_mod_def.F90
!||--- called by ------------------------------------------------------
!||    cp_inttd     ../engine/source/implicit/imp_int_k.F
!||    imp_int_k    ../engine/source/implicit/imp_int_k.F
!||    imp_rnumcd   ../engine/source/implicit/imp_int_k.F
!||    sav_inttd    ../engine/source/implicit/imp_int_k.F
!||====================================================================
      module imp_inttd
        implicit none
! ----------------------------------------------------------------------------------------------------------------------
!! \brief Common Module for penetration detection
! ----------------------------------------------------------------------------------------------------------------------
        integer, dimension(:),allocatable :: ns_imp1,iad1_nin
        integer, dimension(:),allocatable :: ne_imp1,ind_imp1
!
      end module imp_inttd

