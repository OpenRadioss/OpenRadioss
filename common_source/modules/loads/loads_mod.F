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
#include "my_real.inc"
c-----------------------------------------------------------------------
      !||====================================================================
      !||    loads_mod                   ../common_source/modules/loads/loads_mod.F
      !||--- called by ------------------------------------------------------
      !||    ddsplit                     ../starter/source/restart/ddsplit/ddsplit.F
      !||    domain_decomposition_pcyl   ../starter/source/loads/general/load_pcyl/domain_decomposition_pcyl.F
      !||    domdec2                     ../starter/source/spmd/domdec2.F
      !||    fillcne                     ../starter/source/spmd/domdec2.F
      !||    genh3d                      ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    h3d_pre_skin_ixskin         ../engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
      !||    h3d_pre_skin_scalar         ../engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
      !||    h3d_skin_dim                ../engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
      !||    h3d_skin_ixskin             ../engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
      !||    h3d_skin_pre_dim            ../engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
      !||    h3d_skin_pre_map            ../engine/source/output/h3d/h3d_results/h3d_skin_pre_map.F
      !||    h3d_skin_scalar             ../engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
      !||    h3d_skin_vector             ../engine/source/output/h3d/h3d_results/h3d_skin_vector.F
      !||    hm_read_cload               ../starter/source/loads/general/cload/hm_read_cload.F
      !||    hm_read_pcyl                ../starter/source/loads/general/load_pcyl/hm_read_pcyl.F
      !||    hm_read_pload               ../starter/source/loads/general/pload/hm_read_pload.F
      !||    lech3d                      ../engine/source/output/h3d/h3d_build_fortran/lech3d.F
      !||    lectur                      ../engine/source/input/lectur.F
      !||    pressure_cyl                ../engine/source/loads/general/load_pcyl/pressure_cyl.F
      !||    radioss2                    ../engine/source/engine/radioss2.F
      !||    rdcomi                      ../engine/source/output/restart/rdcomm.F
      !||    rdresa                      ../engine/source/output/restart/rdresa.F
      !||    rdresb                      ../engine/source/output/restart/rdresb.F
      !||    read_pcyl                   ../engine/source/output/restart/read_pcyl.F
      !||    resol                       ../engine/source/engine/resol.F
      !||    resol_head                  ../engine/source/engine/resol_head.F
      !||    resol_init                  ../engine/source/engine/resol_init.F
      !||    sortie_main                 ../engine/source/output/sortie_main.F
      !||    split_pcyl                  ../starter/source/loads/general/load_pcyl/split_pcyl.F
      !||    w_pon                       ../starter/source/restart/ddsplit/w_pon.F
      !||    wrcomi                      ../engine/source/output/restart/wrcomm.F
      !||    write_pcyl                  ../engine/source/output/restart/write_pcyl.F
      !||    wrrestp                     ../engine/source/output/restart/wrrestp.F
      !||--- uses       -----------------------------------------------------
      !||    domdec_load_mod             ../common_source/modules/loads/domdec_load_mod.F
      !||    inivel_mod                  ../common_source/modules/inivel_mod.F90
      !||    pload_cyl_mod               ../common_source/modules/loads/pload_cyl_mod.F
      !||====================================================================
      MODULE LOADS_MOD
c-----------------------------------------------------------------------
      USE PLOAD_CYL_MOD
      USE DOMDEC_LOAD_MOD
      USE INIVEL_MOD
c-----------------------------------------------------------------------
      TYPE LOADS_
        INTEGER :: NLOAD_CYL
        INTEGER :: NLOAD_CLOAD ! nb of concentrated loads
        INTEGER :: NLOAD_PLOAD ! nb of pressure loads
        INTEGER :: NINIVELT    ! nb of inivel (/INIVEL) w/ T_start
        INTEGER :: NINIVELT_G  ! max nb of inivel (each domain) w/ T_start
        TYPE (PRESS_CYL_) ,DIMENSION(:) ,ALLOCATABLE   :: LOAD_CYL
        TYPE (DOMDEC_LOAD_), DIMENSION(:), ALLOCATABLE :: CYL_RESTART
        TYPE (INIVEL_), DIMENSION(:), ALLOCATABLE      :: INIVELT
        INTEGER :: S_GLOBAL_SEGMENT_ID
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: GLOBAL_SEGMENT_ID
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: INDEX_LOAD ! index : global load id --> local load id
      END TYPE LOADS_
c-----------------------------------------------------------------------
      END MODULE LOADS_MOD
