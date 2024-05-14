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
#include "my_real.inc"
!-----------------------------------------------------------------------
!hd|====================================================================
!hd|  LOADS_MOD                     modules/loads/loads_mod.F
!hd|-- called by -----------
!hd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
!hd|        DOMAIN_DECOMPOSITION_PCYL     starter/source/loads/general/load_pcyl/domain_decomposition_pcyl.F
!hd|        DOMDEC2                       starter/source/spmd/domdec2.F
!hd|        FILLCNE                       starter/source/spmd/domdec2.F
!hd|        HM_READ_CLOAD                 starter/source/loads/general/cload/hm_read_cload.F
!hd|        HM_READ_PCYL                  starter/source/loads/general/load_pcyl/hm_read_pcyl.F
!hd|        HM_READ_PLOAD                 starter/source/loads/general/pload/hm_read_pload.F
!hd|        LECTUR                        starter/source/starter/lectur.F
!hd|        SPLIT_PCYL                    starter/source/loads/general/load_pcyl/split_pcyl.F
!hd|        W_PON                         starter/source/restart/ddsplit/w_pon.F
!hd|        GENH3D                        engine/source/output/h3d/h3d_results/genh3d.F
!hd|        H3D_PRE_SKIN_IXSKIN           engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
!hd|        H3D_PRE_SKIN_SCALAR           engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
!hd|        H3D_SKIN_DIM                  engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
!hd|        H3D_SKIN_IXSKIN               engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
!hd|        H3D_SKIN_PRE_DIM              engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
!hd|        H3D_SKIN_PRE_MAP              engine/source/output/h3d/h3d_results/h3d_skin_pre_map.F
!hd|        H3D_SKIN_SCALAR               engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
!hd|        H3D_SKIN_VECTOR               engine/source/output/h3d/h3d_results/h3d_skin_vector.F
!hd|        LECH3D                        engine/source/output/h3d/h3d_build_fortran/lech3d.F
!hd|        LECTUR                        engine/source/input/lectur.F
!hd|        PRESSURE_CYL                  engine/source/loads/general/load_pcyl/pressure_cyl.F
!hd|        RADIOSS2                      engine/source/engine/radioss2.F
!hd|        RDCOMI                        engine/source/output/restart/rdcomm.F
!hd|        RDRESA                        engine/source/output/restart/rdresa.F
!hd|        RDRESB                        engine/source/output/restart/rdresb.F
!hd|        READ_PCYL                     engine/source/output/restart/read_pcyl.F
!hd|        RESOL                         engine/source/engine/resol.F
!hd|        RESOL_HEAD                    engine/source/engine/resol_head.F
!hd|        RESTALLOC                     engine/source/output/restart/arralloc.F
!hd|        SORTIE_MAIN                   engine/source/output/sortie_main.F
!hd|        WRCOMI                        engine/source/output/restart/wrcomm.F
!hd|        WRITE_PCYL                    engine/source/output/restart/write_pcyl.F
!hd|        WRRESTP                       engine/source/output/restart/wrrestp.F
!hd|-- calls ---------------
!hd|        DOMDEC_LOAD_MOD               modules/loads/domdec_load_mod.F
!hd|        PLOAD_CYL_MOD                 modules/loads/pload_cyl_mod.F
!hd|====================================================================
      MODULE LOADS_MOD
!-----------------------------------------------------------------------
        USE PLOAD_CYL_MOD
        USE DOMDEC_LOAD_MOD
!-----------------------------------------------------------------------
        TYPE LOADS_
          INTEGER :: NLOAD_CYL
          INTEGER :: NLOAD_CLOAD ! nb of concentrated loads
          INTEGER :: NLOAD_PLOAD ! nb of pressure loads
          TYPE (PRESS_CYL_) ,DIMENSION(:) ,ALLOCATABLE :: LOAD_CYL
          TYPE (DOMDEC_LOAD_), DIMENSION(:), ALLOCATABLE :: CYL_RESTART
          INTEGER :: S_GLOBAL_SEGMENT_ID
          INTEGER, DIMENSION(:,:), ALLOCATABLE :: GLOBAL_SEGMENT_ID
          INTEGER, DIMENSION(:,:), ALLOCATABLE :: INDEX_LOAD ! index : global load id --> local load id
        END TYPE LOADS_
!-----------------------------------------------------------------------
      END MODULE LOADS_MOD
