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
      !||====================================================================
      !||    r2r_mod                         ../starter/share/modules1/r2r_mod.F
      !||--- called by ------------------------------------------------------
      !||    c3grtails                       ../starter/source/elements/sh3n/coque3n/c3grtails.F
      !||    cgrhead                         ../starter/source/elements/shell/coque/cgrhead.F
      !||    cgrtails                        ../starter/source/elements/shell/coque/cgrtails.F
      !||    chk_flg_fsi                     ../starter/source/coupling/rad2rad/routines_r2r.F
      !||    dometis                         ../starter/source/spmd/domain_decomposition/grid2mat.F
      !||    find_dt_engine                  ../starter/source/coupling/rad2rad/r2r_speedup.F
      !||    hm_pre_read_link                ../starter/source/constraints/rigidlink/hm_pre_read_rlink.F
      !||    hm_pre_read_preload             ../starter/source/loads/general/preload/hm_pre_read_preload.F
      !||    hm_prelecjoi                    ../starter/source/constraints/general/cyl_joint/hm_prelecjoi.F
      !||    hm_preread_cload                ../starter/source/loads/general/cload/hm_preread_cload.F
      !||    hm_preread_pload                ../starter/source/loads/general/pload/hm_preread_pload.F
      !||    hm_preread_rbe2                 ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    hm_preread_rbe3                 ../starter/source/constraints/general/rbe3/hm_preread_rbe3.F
      !||    hm_preread_rbody                ../starter/source/constraints/general/rbody/hm_preread_rbody.F
      !||    hm_preread_sphio                ../starter/source/loads/sph/hm_preread_sphio.F
      !||    hm_read_admas                   ../starter/source/tools/admas/hm_read_admas.F
      !||    hm_read_cload                   ../starter/source/loads/general/cload/hm_read_cload.F
      !||    hm_read_cyljoint                ../starter/source/constraints/general/cyl_joint/hm_read_cyljoint.F
      !||    hm_read_friction_models         ../starter/source/interfaces/friction/reader/hm_read_friction_models.F
      !||    hm_read_friction_orientations   ../starter/source/interfaces/friction/reader/hm_read_friction_orientations.F
      !||    hm_read_gauge                   ../starter/source/output/gauge/hm_read_gauge.F
      !||    hm_read_gjoint                  ../starter/source/constraints/general/gjoint/hm_read_gjoint.F
      !||    hm_read_inivol                  ../starter/source/initial_conditions/inivol/hm_read_inivol.F
      !||    hm_read_interfaces              ../starter/source/interfaces/reader/hm_read_interfaces.F
      !||    hm_read_intsub                  ../starter/source/output/subinterface/hm_read_intsub.F
      !||    hm_read_link                    ../starter/source/constraints/rigidlink/hm_read_rlink.F
      !||    hm_read_load_centri             ../starter/source/loads/general/load_centri/hm_read_load_centri.F
      !||    hm_read_load_pressure           ../starter/source/loads/general/load_pressure/hm_read_load_pressure.F
      !||    hm_read_mpc                     ../starter/source/constraints/general/mpc/hm_read_mpc.F
      !||    hm_read_pblast                  ../starter/source/loads/pblast/hm_read_pblast.F
      !||    hm_read_pfluid                  ../starter/source/loads/general/pfluid/hm_read_pfluid.F
      !||    hm_read_pload                   ../starter/source/loads/general/pload/hm_read_pload.F
      !||    hm_read_rbe2                    ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    hm_read_rbe3                    ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
      !||    hm_read_rbody                   ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    hm_read_rbody_lagmul            ../starter/source/constraints/general/rbody/hm_read_rbody_lagmul.F
      !||    hm_read_rwall_cyl               ../starter/source/constraints/general/rwall/hm_read_rwall_cyl.F
      !||    hm_read_rwall_lagmul            ../starter/source/constraints/general/rwall/hm_read_rwall_lagmul.F
      !||    hm_read_rwall_paral             ../starter/source/constraints/general/rwall/hm_read_rwall_paral.F
      !||    hm_read_rwall_plane             ../starter/source/constraints/general/rwall/hm_read_rwall_plane.F
      !||    hm_read_rwall_spher             ../starter/source/constraints/general/rwall/hm_read_rwall_spher.F
      !||    hm_read_rwall_therm             ../starter/source/constraints/general/rwall/hm_read_rwall_therm.F
      !||    hm_read_spcnd                   ../starter/source/constraints/sph/hm_read_spcnd.F
      !||    hm_read_sphio                   ../starter/source/loads/sph/hm_read_sphio.F
      !||    hm_tagpart                      ../starter/source/groups/hm_tagpart.F
      !||    hm_tagpart2                     ../starter/source/groups/hm_tagpart2.F
      !||    i11sti3                         ../starter/source/interfaces/inter3d1/i11sti3.F
      !||    inirby                          ../starter/source/constraints/general/rbody/inirby.F
      !||    init_monvol                     ../starter/source/airbag/init_monvol.F
      !||    lecextlnk                       ../starter/source/coupling/rad2rad/lecextlnk.F
      !||    lecint                          ../starter/source/interfaces/interf1/lecint.F
      !||    lecsec42                        ../starter/source/tools/sect/hm_read_sect.F
      !||    lecsec4bolt                     ../starter/source/tools/sect/lecsec4bolt.F
      !||    lectur                          ../starter/source/starter/lectur.F
      !||    new_link                        ../starter/source/coupling/rad2rad/new_link.F
      !||    nodgr_r2r                       ../starter/source/coupling/rad2rad/routines_r2r.F
      !||    outrin                          ../starter/source/materials/time_step/outri.F
      !||    pgrtails                        ../starter/source/elements/beam/pgrtails.F
      !||    prelecsec                       ../starter/source/tools/sect/prelecsec.F
      !||    prelecsec4bolt                  ../starter/source/tools/sect/prelecsec4bolt.F
      !||    preread_rbody_lagmul            ../starter/source/constraints/general/rbody/preread_rbody_lagmul.F
      !||    printgroup                      ../starter/source/output/outfile/printgroup.F
      !||    qgrtails                        ../starter/source/elements/solid_2d/quad/qgrtails.F
      !||    r2r_check                       ../starter/source/coupling/rad2rad/r2r_check.F
      !||    r2r_check_seg                   ../starter/source/coupling/rad2rad/routines_r2r.F
      !||    r2r_clean_inter                 ../starter/source/coupling/rad2rad/r2r_clean_inter.F
      !||    r2r_count                       ../starter/source/coupling/rad2rad/r2r_count.F
      !||    r2r_domdec                      ../starter/source/coupling/rad2rad/r2r_domdec.F
      !||    r2r_exist                       ../starter/source/coupling/rad2rad/routines_r2r.F
      !||    r2r_fork                        ../starter/source/coupling/rad2rad/r2r_fork.F
      !||    r2r_group                       ../starter/source/coupling/rad2rad/r2r_group.F
      !||    r2r_input                       ../starter/source/coupling/rad2rad/r2r_input.F
      !||    r2r_listcnt                     ../starter/source/coupling/rad2rad/routines_r2r.F
      !||    r2r_monvol                      ../starter/source/coupling/rad2rad/r2r_prelec.F
      !||    r2r_nom_opt                     ../starter/source/coupling/rad2rad/routines_r2r.F
      !||    r2r_prelec                      ../starter/source/coupling/rad2rad/r2r_prelec.F
      !||    r2r_prelec_name                 ../starter/source/coupling/rad2rad/r2r_prelec_name.F
      !||    r2r_speedup                     ../starter/source/coupling/rad2rad/r2r_speedup.F
      !||    r2r_split                       ../starter/source/coupling/rad2rad/r2r_split.F
      !||    r2r_void                        ../starter/source/coupling/rad2rad/r2r_void.F
      !||    r2r_void_1d                     ../starter/source/coupling/rad2rad/r2r_void.F
      !||    read_flag_ale                   ../starter/source/coupling/rad2rad/r2r_fork.F
      !||    read_monvol                     ../starter/source/airbag/read_monvol.F
      !||    read_rwall                      ../starter/source/constraints/general/rwall/read_rwall.F
      !||    rgrtails                        ../starter/source/elements/spring/rgrtails.F
      !||    setrbyon                        ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    sgrhead                         ../starter/source/elements/solid/solide/sgrhead.F
      !||    sgrtails                        ../starter/source/elements/solid/solide/sgrtails.F
      !||    spgrtails                       ../starter/source/elements/sph/spgrtails.F
      !||    st_qaprint_constraints          ../starter/source/output/qaprint/st_qaprint_constraints.F
      !||    st_qaprint_multidomains         ../starter/source/output/qaprint/st_qaprint_multidomains.F
      !||    t3grtails                       ../starter/source/elements/solid_2d/tria/t3grtails.F
      !||    tag_elem_void_r2r               ../starter/source/coupling/rad2rad/tagelem_r2r.F
      !||    tag_elem_void_r2r_lin           ../starter/source/coupling/rad2rad/tagelem_r2r.F
      !||    tagint_r2r                      ../starter/source/coupling/rad2rad/tagint_r2r.F
      !||    tagnod_r2r_s                    ../starter/source/coupling/rad2rad/tagnod_r2r.F
      !||    tgrtails                        ../starter/source/elements/truss/tgrtails.F
      !||    xgrtails                        ../starter/source/elements/xelem/xgrtails.F
      !||====================================================================
      MODULE R2R_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
      INTEGER, DIMENSION(:), ALLOCATABLE :: ISUBDOM_PART,NNCL
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGNO,TAGRBY,TAGINT,TAGMON
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_ELC,TAG_ELS,TAG_ELP
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_ELR,TAG_ELT,TAG_PART
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_ELQ,TAG_ELG,TAG_SUBS
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_MAT,TAG_PROP,TAG_SURF
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGSEC,TAGCYL,TAGINT_WARN
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGLNK,TAGRB3,TAGRB2,TAGJOIN
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGSPHIO,TAGSPHBCS,TAGSPH2S
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAGMPC,FRONT_R2R,FLAGKIN_R2R
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_ELCF,TAG_ELCF2,TAG_ELSF,TAG_ELSF2
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_ELSP,NOM_OPT_TEMP,TAGGAU,TAG_INIVOL      
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISURF_R2R,ISLIN_R2R,IGRBRIC_R2R                          
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: ISUBDOM,IPART_R2R
      my_real ,DIMENSION(:), ALLOCATABLE :: PM_R2R,LINK_DOM               
C-----------------------------------------------
      END MODULE R2R_MOD
