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
      !||====================================================================
      !||    format_mod                   ../starter/share/modules1/format_mod.F90
      !||--- called by ------------------------------------------------------
      !||    admlcnt                      ../starter/source/model/remesh/admlist.F
      !||    admlist                      ../starter/source/model/remesh/admlist.F
      !||    bigbox                       ../starter/source/model/box/bigbox.F
      !||    bigbox2                      ../starter/source/model/box/bigbox.F
      !||    bigsbox                      ../starter/source/model/box/bigbox.F
      !||    contrl                       ../starter/source/starter/contrl.F
      !||    desout                       ../starter/source/output/outp/desout.F
      !||    dometis                      ../starter/source/spmd/domain_decomposition/grid2mat.F
      !||    findele                      ../starter/source/boundary_conditions/ebcs/findele.F
      !||    fredec0                      ../starter/source/starter/freform.F
      !||    fredec5                      ../starter/source/starter/freform.F
      !||    fredec_2key_4id              ../starter/source/starter/freform.F
      !||    fredec_2key_4id_t            ../starter/source/starter/freform.F
      !||    fredec_2key_id_or_key_id     ../starter/source/starter/freform.F
      !||    fredec_key_3id_t             ../starter/source/starter/freform.F
      !||    hm_read_ale_link             ../starter/source/constraints/ale/hm_read_ale_link_vel.F
      !||    hm_read_frm                  ../starter/source/tools/skew/hm_read_frm.F
      !||    hm_read_inter_type24         ../starter/source/interfaces/int24/hm_read_inter_type24.F
      !||    hm_read_link                 ../starter/source/constraints/rigidlink/hm_read_rlink.F
      !||    hm_read_mat29_31             ../starter/source/materials/mat/matuser/hm_read_mat_user29_31.F
      !||    hm_read_mat38                ../starter/source/materials/mat/mat038/hm_read_mat38.F
      !||    hm_read_mat_99               ../starter/source/materials/mat/matuser/hm_read_mat_user_99.F
      !||    hm_read_node                 ../starter/source/elements/reader/hm_read_node.F
      !||    hm_read_perturb_part_shell   ../starter/source/general_controls/computation/hm_read_perturb_part_shell.F
      !||    hm_read_skw                  ../starter/source/tools/skew/hm_read_skw.F
      !||    hm_read_thgrki               ../starter/source/output/th/hm_read_thgrki.F
      !||    hm_read_thgrki_rbody         ../starter/source/output/th/hm_read_thgrki_rbody.F
      !||    hm_read_thgrsens             ../starter/source/output/th/hm_read_thgrsens.F
      !||    hm_read_thgrsurf             ../starter/source/output/th/hm_read_thgrsurf.F
      !||    hm_read_unit                 ../starter/source/general_controls/computation/hm_read_unit.F
      !||    hm_thgrki_vent               ../starter/source/output/th/hm_thgrki_vent.F
      !||    i11pwr3                      ../starter/source/interfaces/inter3d1/i11pwr3.F
      !||    i12chk3                      ../starter/source/interfaces/inter3d1/i12chk3.F
      !||    i12tid3                      ../starter/source/interfaces/inter3d1/i12tid3.F
      !||    i18pwr3                      ../starter/source/interfaces/inter3d1/i18pwr3.F
      !||    i1tid2                       ../starter/source/interfaces/inter2d1/i1tid2.F
      !||    i1tid3                       ../starter/source/interfaces/inter3d1/i1tid3.F
      !||    i20edge1                     ../starter/source/interfaces/inter3d1/i20surfi.F
      !||    i20pwr3                      ../starter/source/interfaces/inter3d1/i20pwr3.F
      !||    i20pwr3a                     ../starter/source/interfaces/inter3d1/i20pwr3.F
      !||    i20pwr3ae                    ../starter/source/interfaces/inter3d1/i20pwr3.F
      !||    i20pwr3e                     ../starter/source/interfaces/inter3d1/i20pwr3.F
      !||    i20surfi                     ../starter/source/interfaces/inter3d1/i20surfi.F
      !||    i21pwr3                      ../starter/source/interfaces/inter3d1/i21pwr3.F
      !||    i23pwr3                      ../starter/source/interfaces/inter3d1/i23pwr3.F
      !||    i24edge1                     ../starter/source/interfaces/inter3d1/i24surfi.F
      !||    i24edge2                     ../starter/source/interfaces/inter3d1/i24surfi.F
      !||    i24pen3                      ../starter/source/interfaces/inter3d1/i24pen3.F
      !||    i24surfi                     ../starter/source/interfaces/inter3d1/i24surfi.F
      !||    i25surfi                     ../starter/source/interfaces/inter3d1/i25surfi.F
      !||    i3pen2                       ../starter/source/interfaces/inter2d1/i3pen2.F
      !||    i3pen3                       ../starter/source/interfaces/inter3d1/i3pen3.F
      !||    i5pwr3                       ../starter/source/interfaces/inter3d1/i3pen3.F
      !||    i6pen3                       ../starter/source/interfaces/inter3d1/i6pen3.F
      !||    i7pwr3                       ../starter/source/interfaces/inter3d1/i7pwr3.F
      !||    ingrbric                     ../starter/source/interfaces/interf1/ingrbric.F
      !||    ingrbric_centroids           ../starter/source/interfaces/interf1/ingrbric_centroids.F
      !||    ingrbric_nodes               ../starter/source/interfaces/interf1/ingrbric_nodes.F
      !||    ini_fxbody                   ../starter/source/constraints/fxbody/ini_fxbody.F
      !||    inivoid                      ../starter/source/elements/initia/inivoid.F
      !||    inpoint                      ../starter/source/interfaces/interf1/inpoint.F
      !||    inslin                       ../starter/source/interfaces/interf1/inslin.F
      !||    insurf                       ../starter/source/interfaces/interf1/insurf.F
      !||    insurf23                     ../starter/source/interfaces/interf1/insurf23.F
      !||    insurf_dx                    ../starter/source/interfaces/interf1/insurf_dx.F
      !||    insurfigeo                   ../starter/source/interfaces/interf1/insurfigeo.F
      !||    kinchk                       ../starter/source/constraints/general/kinchk.F
      !||    lec_inistate_yfile           ../starter/source/initial_conditions/inista/lec_inistate_yfile.F
      !||    lecig3d                      ../starter/source/elements/ige3d/lecig3d.F
      !||    leclas                       ../starter/source/loads/laser/leclas.F
      !||    leclas1                      ../starter/source/loads/laser/leclas1.F
      !||    nbadigemesh                  ../starter/source/elements/ige3d/nbadigemesh.F
      !||    prelecig3d                   ../starter/source/elements/ige3d/prelecig3d.F
      !||    printgroup                   ../starter/source/output/outfile/printgroup.F
      !||    r2r_listcnt                  ../starter/source/coupling/rad2rad/routines_r2r.F
      !||    read_dfs_detcord             ../starter/source/initial_conditions/detonation/read_dfs_detcord.F
      !||    read_dfs_wave_shaper         ../starter/source/initial_conditions/detonation/read_dfs_wave_shaper.F
      !||    read_sensor_hic              ../starter/source/tools/sensor/read_sensor_hic.F
      !||    read_sensor_user             ../starter/source/tools/sensor/read_sensor_user.F
      !||    remn_i2op                    ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    rinit3                       ../starter/source/elements/spring/rinit3.F
      !||    rm_cand24                    ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    spdometis                    ../starter/source/spmd/domain_decomposition/grid2mat.F
      !||    unit_code                    ../starter/source/general_controls/computation/unit_code.F
      !||    yctrl                        ../starter/source/initial_conditions/inista/yctrl.F
      !||====================================================================
      module format_mod

        ! number of character per field/column
        INTEGER, PARAMETER :: LFIELD = 10

        ! input format
        CHARACTER(LEN=50), PARAMETER :: FMT_I      = "(I10)"
        CHARACTER(LEN=50), PARAMETER :: FMT_2I     = "(2I10)"
        CHARACTER(LEN=50), PARAMETER :: FMT_3I     = "(3I10)"
        CHARACTER(LEN=50), PARAMETER :: FMT_5I     = "(5I10)"
        CHARACTER(LEN=50), PARAMETER :: FMT_6I     = "(6I10)"
        CHARACTER(LEN=50), PARAMETER :: FMT_8I     = "(8I10)"
        CHARACTER(LEN=50), PARAMETER :: FMT_10I    = "(10I10)"
        CHARACTER(LEN=50), PARAMETER :: FMT_I_2F   = "(I10,2F20.0)"
        CHARACTER(LEN=50), PARAMETER :: FMT_I_3F   = "(I10,3F20.0)"
        CHARACTER(LEN=50), PARAMETER :: FMT_F      = "(F20.0)"
        CHARACTER(LEN=50), PARAMETER :: FMT_2F     = "(2F20.0)"
        CHARACTER(LEN=50), PARAMETER :: FMT_3F     = "(3F20.0)"
        CHARACTER(LEN=50), PARAMETER :: FMT_5F     = "(5F20.0)"
        CHARACTER(LEN=50), PARAMETER :: FMT_A      = "(A10)"
        CHARACTER(LEN=50), PARAMETER :: FMT_A_I    = "(A10,I10)"
        CHARACTER(LEN=50), PARAMETER :: FMT_A_I_F  = "(A10,I10,F20.0)"
        CHARACTER(LEN=50), PARAMETER :: FMT_THGR   = "(2I10,A80)"

        ! output format
        CHARACTER(LEN=50), PARAMETER :: FMW_I      = "(I10)"
        CHARACTER(LEN=50), PARAMETER :: FMW_2I     = "(2I10)"
        CHARACTER(LEN=50), PARAMETER :: FMW_4I     = "(4I10)"
        CHARACTER(LEN=50), PARAMETER :: FMW_5I     = "(5I10)"
        CHARACTER(LEN=50), PARAMETER :: FMW_6I     = "(6I10)"
        CHARACTER(LEN=50), PARAMETER :: FMW_7I     = "(7I10)"
        CHARACTER(LEN=50), PARAMETER :: FMW_10I    = "(10I10)"
        CHARACTER(LEN=50), PARAMETER :: FMW_I_A    = "(I10,A)"
        CHARACTER(LEN=50), PARAMETER :: FMW_A_I    = "(A,I10)"
        CHARACTER(LEN=50), PARAMETER :: FMW_A_I_A  = "(A,I10,A)"
        CHARACTER(LEN=50), PARAMETER :: FMW_I_3F   = "(I10,1P3G20.13)"
        CHARACTER(LEN=50), PARAMETER :: FMW_5I_F   = "(5I10,1PG20.13)"
        CHARACTER(LEN=50), PARAMETER :: FMW_7I_2F  = "(7I10,1P2G20.13)"
        CHARACTER(LEN=50), PARAMETER :: FMW_2I_X_F = "(2I10,10X,1PG20.13)"

      contains

      end module format_mod
