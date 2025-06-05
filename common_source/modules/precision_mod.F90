!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||    precision_mod                  ../common_source/modules/precision_mod.F90
      !||--- called by ------------------------------------------------------
      !||    check_sorting_criteria         ../engine/source/interfaces/intsort/check_sorting_criteria.F90
      !||    chk_shell_offset               ../starter/source/elements/shell/shell_offset/chk_shell_offset.F90
      !||    compute_voxel_dimensions       ../engine/source/interfaces/intsort/voxel_dimensions.F90
      !||    create_plane_clause            ../starter/source/model/sets/create_plane_clause.F90
      !||    dim_shell_offsetp              ../starter/source/elements/shell/shell_offset/dim_shell_offsetp.F90
      !||    fill_surf_plane                ../starter/source/model/sets/fill_gr_surf_plane.F90
      !||    fill_voxel_local               ../engine/source/interfaces/intsort/fill_voxel.F90
      !||    fill_voxel_local_partial       ../engine/source/interfaces/intsort/fill_voxel.F90
      !||    fill_voxel_remote              ../engine/source/interfaces/intsort/fill_voxel.F90
      !||    funct_python_update_elements   ../engine/source/tools/curve/funct_python_update_elements.F90
      !||    get_list_remnode               ../starter/source/interfaces/inter3d1/get_list_remnode.F90
      !||    hm_read_funct_python           ../starter/source/tools/curve/hm_read_funct_python.F90
      !||    hm_read_mat57                  ../starter/source/materials/mat/mat057/hm_read_mat57.F90
      !||    hm_read_mat81                  ../starter/source/materials/mat/mat081/hm_read_mat81.F90
      !||    inter7_candidate_pairs         ../engine/source/interfaces/intsort/inter7_candidate_pairs.F90
      !||    inter7_collision_detection     ../engine/source/interfaces/intsort/inter7_collision_detection.F90
      !||    inter7_deserialize             ../engine/source/interfaces/intsort/inter7_candidate_pairs.F90
      !||    inter7_filter_cand             ../engine/source/interfaces/intsort/inter7_filter_cand.F90
      !||    inter7_gather_cand             ../engine/source/interfaces/int07/inter7_gather_cand.F90
      !||    inter7_penetration             ../engine/source/interfaces/intsort/inter7_penetration.F90
      !||    inter7_serialize               ../engine/source/interfaces/intsort/inter7_candidate_pairs.F90
      !||    law81_upd                      ../starter/source/materials/mat/mat081/law81_upd.F90
      !||    pblast_mod                     ../common_source/modules/loads/pblast_mod.F90
      !||    s20temp                        ../starter/source/elements/solid/solide20/s20temp.F90
      !||    sh_offset_jonct_chk            ../starter/source/elements/shell/shell_offset/sh_offset_jonkt_chk.F90
      !||    sh_offset_nproj                ../starter/source/elements/shell/shell_offset/shell_offset_nproj.F90
      !||    sh_offset_setn                 ../starter/source/elements/shell/shell_offset/sh_offset_setn.F90
      !||    shell_offset_ini               ../starter/source/elements/shell/shell_offset/shell_offset_ini.F90
      !||    shell_offsetp                  ../starter/source/elements/shell/shell_offset/shell_offsetp.F90
      !||    sigeps01                       ../starter/source/materials/mat/mat001/sigeps01.F90
      !||    w_inivel_str                   ../starter/source/restart/ddsplit/w_inivel_str.F90
      !||====================================================================
      MODULE precision_mod
      IMPLICIT NONE
#ifdef MYREAL8
        INTEGER, PARAMETER :: WP = 8
#else
        INTEGER, PARAMETER :: WP = 4
#endif
      END MODULE precision_mod

