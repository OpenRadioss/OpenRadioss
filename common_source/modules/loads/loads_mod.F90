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
!-----------------------------------------------------------------------
!||====================================================================
!||    loads_mod                   ../common_source/modules/loads/loads_mod.F90
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
!||    prelech3d                   ../engine/source/output/h3d/h3d_build_fortran/prelech3d.F90
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
      module loads_mod
!-----------------------------------------------------------------------
        use pload_cyl_mod
        use domdec_load_mod
        use inivel_mod
!-----------------------------------------------------------------------
        type loads_
          integer :: nload_cyl
          integer :: nload_cload                                          !< nb of concentrated loads
          integer :: nload_pload                                          !< nb of pressure loads
          integer :: ninivelt                                             !< nb of inivel (/inivel) w/ t_start
          integer :: ninivelt_g                                           !< max nb of inivel (each domain) w/ t_start
          type (press_cyl_) ,dimension(:) ,allocatable   :: load_cyl
          type (domdec_load_), dimension(:), allocatable :: cyl_restart
          type (inivel_), dimension(:), allocatable      :: inivelt
          integer :: s_global_segment_id
          integer, dimension(:,:), allocatable :: global_segment_id
          integer, dimension(:,:), allocatable :: index_load              !< index : global load id --> local load id
        end type loads_
!-----------------------------------------------------------------------
      end module loads_mod
