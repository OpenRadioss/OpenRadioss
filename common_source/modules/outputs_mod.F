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
Chd|  OUTMAX_MOD                    modules/outputs_mod.F          
Chd|-- called by -----------
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        INI_H3DTMAX_ENGINE            starter/source/starter/contrl.F
Chd|        INI_OUTMAX_AUTO               starter/source/elements/elbuf_init/ini_outmax_auto.F
Chd|        WRCOMIP                       starter/source/restart/ddsplit/wrcommp.F
Chd|        W_OUTMAXN                     starter/source/restart/ddsplit/w_outmaxn.F
Chd|        GENH3D                        engine/source/output/h3d/h3d_results/genh3d.F
Chd|        H3D_NODAL_TENSOR              engine/source/output/h3d/h3d_results/h3d_nodal_tensor.F
Chd|        H3D_NODAL_VECTOR              engine/source/output/h3d/h3d_results/h3d_nodal_vector.F
Chd|        INI_TMAX                      engine/source/output/ini_outmax.F
Chd|        RDCOMI                        engine/source/output/restart/rdcomm.F
Chd|        RDRESB                        engine/source/output/restart/rdresb.F
Chd|        RESOL_INIT                    engine/source/engine/resol_init.F
Chd|        RESTALLOC                     engine/source/output/restart/arralloc.F
Chd|        SORTIE_MAIN                   engine/source/output/sortie_main.F
Chd|        UPD_TMAX                      engine/source/output/upd_outmax.F
Chd|        WRCOMI                        engine/source/output/restart/wrcomm.F
Chd|        WRRESTP                       engine/source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|====================================================================
        MODULE OUTPUTS_MOD
#include "my_real.inc"
!--------------------------------------------------------------------
!       OUTPUTS_MOD : for outputs tabs
!--------------------------------------------------------------------
        integer  S_EFRICINT, S_EFRICINTG, S_EFRIC, S_EFRICG     
        integer  NINEFRIC_STAMP,NINEFRIC ! number of interfaces defined for output fricional energy
        my_real, DIMENSION(:,:) , ALLOCATABLE ::   EFRIC, EFRIC_STAMP
        my_real, DIMENSION(:) , ALLOCATABLE ::   EFRICG, EFRICG_STAMP

        END MODULE OUTPUTS_MOD
