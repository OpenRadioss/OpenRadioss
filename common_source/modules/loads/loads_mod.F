Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|====================================================================
Chd|  LOADS_MOD                     modules/loads/loads_mod.F     
Chd|-- called by -----------
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        DOMAIN_DECOMPOSITION_PCYL     starter/source/loads/general/load_pcyl/domain_decomposition_pcyl.F
Chd|        DOMDEC2                       starter/source/spmd/domdec2.F 
Chd|        FILLCNE                       starter/source/spmd/domdec2.F 
Chd|        HM_READ_CLOAD                 starter/source/loads/general/cload/hm_read_cload.F
Chd|        HM_READ_PCYL                  starter/source/loads/general/load_pcyl/hm_read_pcyl.F
Chd|        HM_READ_PLOAD                 starter/source/loads/general/pload/hm_read_pload.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        SPLIT_PCYL                    starter/source/loads/general/load_pcyl/split_pcyl.F
Chd|        W_PON                         starter/source/restart/ddsplit/w_pon.F
Chd|        GENH3D                        engine/source/output/h3d/h3d_results/genh3d.F
Chd|        H3D_PRE_SKIN_IXSKIN           engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_PRE_SKIN_SCALAR           engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|        H3D_SKIN_DIM                  engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_SKIN_IXSKIN               engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_SKIN_PRE_DIM              engine/source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_SKIN_PRE_MAP              engine/source/output/h3d/h3d_results/h3d_skin_pre_map.F
Chd|        H3D_SKIN_SCALAR               engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|        H3D_SKIN_VECTOR               engine/source/output/h3d/h3d_results/h3d_skin_vector.F
Chd|        LECH3D                        engine/source/output/h3d/h3d_build_fortran/lech3d.F
Chd|        LECTUR                        engine/source/input/lectur.F  
Chd|        PRESSURE_CYL                  engine/source/loads/general/load_pcyl/pressure_cyl.F
Chd|        RADIOSS2                      engine/source/engine/radioss2.F
Chd|        RDCOMI                        engine/source/output/restart/rdcomm.F
Chd|        RDRESA                        engine/source/output/restart/rdresa.F
Chd|        RDRESB                        engine/source/output/restart/rdresb.F
Chd|        READ_PCYL                     engine/source/output/restart/read_pcyl.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        RESOL_HEAD                    engine/source/engine/resol_head.F
Chd|        RESTALLOC                     engine/source/output/restart/arralloc.F
Chd|        SORTIE_MAIN                   engine/source/output/sortie_main.F
Chd|        WRCOMI                        engine/source/output/restart/wrcomm.F
Chd|        WRITE_PCYL                    engine/source/output/restart/write_pcyl.F
Chd|        WRRESTP                       engine/source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|        DOMDEC_LOAD_MOD               modules/loads/domdec_load_mod.F
Chd|        PLOAD_CYL_MOD                 modules/loads/pload_cyl_mod.F 
Chd|====================================================================
      MODULE LOADS_MOD
c-----------------------------------------------------------------------
      USE PLOAD_CYL_MOD
      USE DOMDEC_LOAD_MOD
c-----------------------------------------------------------------------
      TYPE LOADS_
        INTEGER :: NLOAD_CYL
        TYPE (PRESS_CYL_) ,DIMENSION(:) ,ALLOCATABLE :: LOAD_CYL
        TYPE (DOMDEC_LOAD_), DIMENSION(:), ALLOCATABLE :: CYL_RESTART
        INTEGER :: S_GLOBAL_SEGMENT_ID
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: GLOBAL_SEGMENT_ID
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: INDEX_LOAD ! index : global load id --> local load id
      END TYPE LOADS_
c-----------------------------------------------------------------------
      END MODULE LOADS_MOD
