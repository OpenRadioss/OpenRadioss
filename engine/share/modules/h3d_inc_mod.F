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
Chd|====================================================================
Chd|  H3D_INC_MOD                   share/modules/h3d_inc_mod.F   
Chd|-- called by -----------
Chd|        ALLOCBUF_AUTO                 source/elements/elbuf/allocbuf_auto.F
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|        H3D_PRE_SKIN_IXSKIN           source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_PRE_SKIN_SCALAR           source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|        H3D_SKIN_IXSKIN               source/output/h3d/h3d_results/h3d_skin_ixskin.F
Chd|        H3D_SKIN_OFF                  source/output/h3d/h3d_results/h3d_skin_off.F
Chd|        H3D_SKIN_PRE_MAP              source/output/h3d/h3d_results/h3d_skin_pre_map.F
Chd|        H3D_SKIN_SCALAR               source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|        H3D_SKIN_VECTOR               source/output/h3d/h3d_results/h3d_skin_vector.F
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|        PBLAST_1                      source/loads/pblast/pblast_1.F
Chd|        PBLAST_2                      source/loads/pblast/pblast_2.F
Chd|        PBLAST_3                      source/loads/pblast/pblast_3.F
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|====================================================================
      MODULE H3D_INC_MOD
C-----------------------------------------------
      INTEGER NUMSKINP,NUMSKINP0
C-----------------------------------------------
      
      END MODULE H3D_INC_MOD
