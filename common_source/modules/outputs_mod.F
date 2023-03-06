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
Chd|  OUTPUTS_MOD                   modules/outputs_mod.F         
Chd|-- called by -----------
Chd|        ANI_PCONT                     engine/source/output/anim/generate/ani_pcont.F
Chd|        ANI_PCONT21                   engine/source/output/anim/generate/ani_pcont.F
Chd|        GENH3D                        engine/source/output/h3d/h3d_results/genh3d.F
Chd|        H3D_NODAL_SCALAR              engine/source/output/h3d/h3d_results/h3d_nodal_scalar.F
Chd|        I21ASS3                       engine/source/interfaces/int21/i21ass3.F
Chd|        I21MAINF                      engine/source/interfaces/int21/i21mainf.F
Chd|        I24FOR3                       engine/source/interfaces/int24/i24for3.F
Chd|        I24MAINF                      engine/source/interfaces/int24/i24main.F
Chd|        I25FOR3                       engine/source/interfaces/int25/i25for3.F
Chd|        I25MAINF                      engine/source/interfaces/int25/i25mainf.F
Chd|        I7FOR3                        engine/source/interfaces/int07/i7for3.F
Chd|        I7MAINF                       engine/source/interfaces/int07/i7mainf.F
Chd|        INTFOP2                       engine/source/interfaces/interf/intfop2.F
Chd|        INTTRI                        engine/source/interfaces/intsort/inttri.F
Chd|        LECTUR                        engine/source/input/lectur.F  
Chd|        RDCOMI                        engine/source/output/restart/rdcomm.F
Chd|        RDRESB                        engine/source/output/restart/rdresb.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        RESOL_INIT                    engine/source/engine/resol_init.F
Chd|        RESTALLOC                     engine/source/output/restart/arralloc.F
Chd|        SORTIE_MAIN                   engine/source/output/sortie_main.F
Chd|        SPMD_EXCH_EFRIC               engine/source/mpi/interfaces/spmd_exch_efric.F
Chd|        SPMD_EXCH_PRESS               engine/source/mpi/interfaces/spmd_exch_press.F
Chd|        SPMD_EXCH_SORTING_EFRIC       engine/source/mpi/interfaces/spmd_exch_sorting_efric.F
Chd|        SPMD_TRI10BOX                 engine/source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI11GAT                 engine/source/mpi/interfaces/spmd_int.F
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
        my_real :: XI_RES = 0.
        my_real :: YI_RES = 0.
        my_real :: ZI_RES = 0.

        END MODULE OUTPUTS_MOD
