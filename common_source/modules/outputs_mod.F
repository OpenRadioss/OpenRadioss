Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      !||====================================================================
      !||    outputs_mod               ../common_source/modules/outputs_mod.F
      !||--- called by ------------------------------------------------------
      !||    ani_pcont                 ../engine/source/output/anim/generate/ani_pcont.F
      !||    ani_pcont21               ../engine/source/output/anim/generate/ani_pcont.F
      !||    genh3d                    ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    h3d_nodal_scalar          ../engine/source/output/h3d/h3d_results/h3d_nodal_scalar.F
      !||    hm_read_node              ../starter/source/elements/reader/hm_read_node.F
      !||    i21ass3                   ../engine/source/interfaces/int21/i21ass3.F
      !||    i21mainf                  ../engine/source/interfaces/int21/i21mainf.F
      !||    i24for3                   ../engine/source/interfaces/int24/i24for3.F
      !||    i24mainf                  ../engine/source/interfaces/int24/i24main.F
      !||    i25for3                   ../engine/source/interfaces/int25/i25for3.F
      !||    i25mainf                  ../engine/source/interfaces/int25/i25mainf.F
      !||    i7for3                    ../engine/source/interfaces/int07/i7for3.F
      !||    i7mainf                   ../engine/source/interfaces/int07/i7mainf.F
      !||    intfop2                   ../engine/source/interfaces/interf/intfop2.F
      !||    inttri                    ../engine/source/interfaces/intsort/inttri.F
      !||    lectur                    ../engine/source/input/lectur.F
      !||    rdcomi                    ../engine/source/output/restart/rdcomm.F
      !||    rdcomr                    ../engine/source/output/restart/rdcomm.F
      !||    rdresb                    ../engine/source/output/restart/rdresb.F
      !||    resol                     ../engine/source/engine/resol.F
      !||    resol_init                ../engine/source/engine/resol_init.F
      !||    restalloc                 ../engine/source/output/restart/arralloc.F
      !||    sortie_main               ../engine/source/output/sortie_main.F
      !||    spmd_exch_efric           ../engine/source/mpi/interfaces/spmd_exch_efric.F
      !||    spmd_exch_press           ../engine/source/mpi/interfaces/spmd_exch_press.F
      !||    spmd_exch_sorting_efric   ../engine/source/mpi/interfaces/spmd_exch_sorting_efric.F
      !||    spmd_tri10box             ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri11gat             ../engine/source/mpi/interfaces/spmd_int.F
      !||    sponof1                   ../engine/source/elements/sph/sponof1.F
      !||    wrcomi                    ../engine/source/output/restart/wrcomm.F
      !||    wrcomr                    ../engine/source/output/restart/wrcomm.F
      !||    wrrestp                   ../engine/source/output/restart/wrrestp.F
      !||====================================================================
        MODULE OUTPUTS_MOD
#include "my_real.inc"
!--------------------------------------------------------------------
!       OUTPUTS_MOD : for outputs tabs
!--------------------------------------------------------------------
        integer  S_EFRICINT, S_EFRICINTG, S_EFRIC, S_EFRICG     
        integer  NINEFRIC_STAMP,NINEFRIC ! number of interfaces defined for output fricional energy
        my_real, DIMENSION(:,:) , ALLOCATABLE ::   EFRIC, EFRIC_STAMP
        my_real, DIMENSION(:) , ALLOCATABLE ::   EFRICG, EFRICG_STAMP
        my_real :: XI_RES = 0.
        my_real :: YI_RES = 0.
        my_real :: ZI_RES = 0.

        END MODULE OUTPUTS_MOD
