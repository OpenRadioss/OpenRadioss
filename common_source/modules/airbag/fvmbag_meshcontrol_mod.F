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
Chd|====================================================================
Chd|  FVMBAG_MESHCONTROL_MOD        modules/fvmbag_meshcontrol_mod.F
Chd|-- called by -----------
Chd|        APPLYSORT2FVM                 starter/source/airbag/fvmesh0.F
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        DOMETIS                       starter/source/spmd/domain_decomposition/grid2mat.F
Chd|        FVBAG_VERTEX                  starter/source/spmd/domain_decomposition/grid2mat.F
Chd|        FVMESH0                       starter/source/airbag/fvmesh0.F
Chd|        FVWRESTP                      starter/source/restart/ddsplit/fvwrestp.F
Chd|        HYPERMESH_TETRA               starter/stub/fvmbags_stub.F   
Chd|        INIT_MONVOL                   starter/source/airbag/init_monvol.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        MESHGEMS_TETRA                starter/stub/fvmbags_stub.F   
Chd|        WRITEMESH                     starter/source/airbag/writeMesh.F
Chd|        ALEVFLU                       engine/source/output/anim/generate/genani.F
Chd|        FVBAG0                        engine/source/airbag/fvbag0.F 
Chd|        FVBAG1                        engine/source/airbag/fvbag1.F 
Chd|        FVBAG2                        engine/source/airbag/fvbag2.F 
Chd|        FVRREST                       engine/source/output/restart/rdresb.F
Chd|        FVUPD1                        engine/source/airbag/fvupd.F  
Chd|        FVWREST                       engine/source/output/restart/wrrest.F
Chd|        FV_UP_SWITCH                  engine/source/airbag/fv_up_switch.F
Chd|        SORTIE_MAIN                   engine/source/output/sortie_main.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE FVMBAG_MESHCONTROL_MOD
#include      "my_real.inc"
      INTEGER, DIMENSION(:), ALLOCATABLE :: KMESH
      LOGICAL :: TETRAMESHER_USED
      INTEGER :: NB_TOTAL_NODE, NB_TOTAL_NODE_OLD
      INTEGER :: NB_TOTAL_SOLIDS, NB_TOTAL_SOLIDS_OLD
      INTEGER, DIMENSION(:), ALLOCATABLE :: IXS_TEMP
      INTEGER, DIMENSION(:), ALLOCATABLE :: IBUFSSG_TEMP
      my_real, DIMENSION(:, :), ALLOCATABLE :: NODE_COORD
      INTEGER, DIMENSION(:), ALLOCATABLE :: BUFALEI_MGM
      INTEGER, DIMENSION(:), ALLOCATABLE :: ITAB_TMP, ITABM1_TMP
      END MODULE FVMBAG_MESHCONTROL_MOD
