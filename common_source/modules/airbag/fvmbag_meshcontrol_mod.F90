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
!hd|====================================================================
!hd|  FVMBAG_MESHCONTROL_MOD        modules/airbag/fvmbag_meshcontrol_mod.F
!hd|-- called by -----------
!hd|        APPLYSORT2FVM                 starter/source/airbag/fvmesh0.F
!hd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
!hd|        DOMETIS                       starter/source/spmd/domain_decomposition/grid2mat.F
!hd|        FVBAG_VERTEX                  starter/source/spmd/domain_decomposition/grid2mat.F
!hd|        FVMESH0                       starter/source/airbag/fvmesh0.F
!hd|        FVWRESTP                      starter/source/restart/ddsplit/fvwrestp.F
!hd|        HYPERMESH_TETRA               starter/stub/fvmbags_stub.F
!hd|        INIT_MONVOL                   starter/source/airbag/init_monvol.F
!hd|        LECTUR                        starter/source/starter/lectur.F
!hd|        WRITEMESH                     starter/source/airbag/writeMesh.F
!hd|        ALEVFLU                       engine/source/output/anim/generate/genani.F
!hd|        FVBAG0                        engine/source/airbag/fvbag0.F
!hd|        FVBAG1                        engine/source/airbag/fvbag1.F
!hd|        FVBAG2                        engine/source/airbag/fvbag2.F
!hd|        FVRREST                       engine/source/output/restart/rdresb.F
!hd|        FVUPD1                        engine/source/airbag/fvupd.F
!hd|        FVWREST                       engine/source/output/restart/wrrest.F
!hd|        FV_UP_SWITCH                  engine/source/airbag/fv_up_switch.F
!hd|        SORTIE_MAIN                   engine/source/output/sortie_main.F
!hd|-- calls ---------------
!hd|====================================================================
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
