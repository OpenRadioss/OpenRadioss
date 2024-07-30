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
      !||    skew_mod          ../common_source/modules/skew_mod.F90
      !||--- called by ------------------------------------------------------
      !||    check_skew        ../starter/source/spmd/domain_decomposition/check_skew.F
      !||    ddsplit           ../starter/source/restart/ddsplit/ddsplit.F
      !||    domdec2           ../starter/source/spmd/domdec2.F
      !||    dyna_ina          ../engine/source/implicit/imp_dyna.F
      !||    dyna_wex          ../engine/source/implicit/imp_dyna.F
      !||    force             ../engine/source/loads/general/force.F90
      !||    force_imp         ../engine/source/loads/general/force_imp.F
      !||    imp_chkm          ../engine/source/implicit/imp_solv.F
      !||    imp_solv          ../engine/source/implicit/imp_solv.F
      !||    lectur            ../engine/source/input/lectur.F
      !||    radioss2          ../engine/source/engine/radioss2.F
      !||    rdcomi            ../engine/source/output/restart/rdcomm.F
      !||    rdresa            ../engine/source/output/restart/rdresa.F
      !||    rdresb            ../engine/source/output/restart/rdresb.F
      !||    resol             ../engine/source/engine/resol.F
      !||    resol_head        ../engine/source/engine/resol_head.F
      !||    restalloc         ../engine/source/output/restart/arralloc.F
      !||    split_skew        ../starter/source/restart/ddsplit/split_skew.F
      !||    split_skew_save   ../starter/source/restart/ddsplit/split_skew.F
      !||    wrrestp           ../engine/source/output/restart/wrrestp.F
      !||====================================================================
      module skew_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
  ! ======================================================================================================================
  !                                                   TYPES
  ! ======================================================================================================================
  !! \brief module to host the skew datatype
  !! \details 

          !< List of processors for each skew
          type plist_skew_
               integer, dimension(:), allocatable :: plist  !< list of MPI domains for each skew
          end type plist_skew_

          type skew_
             integer :: total_skew_number                    !< Total number of skews : deck skews + 1 + SPH + NSUBMOD 
             integer :: skew_number                          !< input deck skew
             integer :: n_skew_var                           !< Number of variables per Skew for Skew array
             my_real, dimension(:,:),  allocatable :: skew   !< Float variables for skew
             ! integer :: s_iskew                              !< size of iskew
             ! integer, dimension (:,:), allocatable :: iskwn  !< iskwn array
             ! integer, dimension (:), allocatable :: iskew    !< node skew 
             type(plist_skew_), dimension(:), allocatable :: multiple_skew
          end type skew_

      end module skew_mod
