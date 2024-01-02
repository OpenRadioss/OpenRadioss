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
Chd|  R2R_MOD                       share/modules1/r2r_mod.F      
Chd|-- called by -----------
Chd|        C3GRTAILS                     source/elements/sh3n/coque3n/c3grtails.F
Chd|        CGRHEAD                       source/elements/shell/coque/cgrhead.F
Chd|        CGRTAILS                      source/elements/shell/coque/cgrtails.F
Chd|        CHK_FLG_FSI                   source/coupling/rad2rad/routines_r2r.F
Chd|        DOMETIS                       source/spmd/domain_decomposition/grid2mat.F
Chd|        FIND_DT_ENGINE                source/coupling/rad2rad/r2r_speedup.F
Chd|        HM_PRELECJOI                  source/constraints/general/cyl_joint/hm_prelecjoi.F
Chd|        HM_PREREAD_CLOAD              source/loads/general/cload/hm_preread_cload.F
Chd|        HM_PREREAD_PLOAD              source/loads/general/pload/hm_preread_pload.F
Chd|        HM_PREREAD_RBE2               source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        HM_PREREAD_RBE3               source/constraints/general/rbe3/hm_preread_rbe3.F
Chd|        HM_PREREAD_RBODY              source/constraints/general/rbody/hm_preread_rbody.F
Chd|        HM_PREREAD_SPHIO              source/loads/sph/hm_preread_sphio.F
Chd|        HM_PRE_READ_LINK              source/constraints/rigidlink/hm_pre_read_rlink.F
Chd|        HM_PRE_READ_PRELOAD           source/loads/general/preload/hm_pre_read_preload.F
Chd|        HM_READ_ADMAS                 source/tools/admas/hm_read_admas.F
Chd|        HM_READ_CLOAD                 source/loads/general/cload/hm_read_cload.F
Chd|        HM_READ_CYLJOINT              source/constraints/general/cyl_joint/hm_read_cyljoint.F
Chd|        HM_READ_FRICTION_MODELS       source/interfaces/friction/reader/hm_read_friction_models.F
Chd|        HM_READ_FRICTION_ORIENTATIONS source/interfaces/friction/reader/hm_read_friction_orientations.F
Chd|        HM_READ_GAUGE                 source/output/gauge/hm_read_gauge.F
Chd|        HM_READ_GJOINT                source/constraints/general/gjoint/hm_read_gjoint.F
Chd|        HM_READ_INIVOL                source/initial_conditions/inivol/hm_read_inivol.F
Chd|        HM_READ_INTERFACES            source/interfaces/reader/hm_read_interfaces.F
Chd|        HM_READ_INTSUB                source/output/subinterface/hm_read_intsub.F
Chd|        HM_READ_LINK                  source/constraints/rigidlink/hm_read_rlink.F
Chd|        HM_READ_LOAD_CENTRI           source/loads/general/load_centri/hm_read_load_centri.F
Chd|        HM_READ_LOAD_PRESSURE         source/loads/general/load_pressure/hm_read_load_pressure.F
Chd|        HM_READ_MPC                   source/constraints/general/mpc/hm_read_mpc.F
Chd|        HM_READ_PBLAST                source/loads/pblast/hm_read_pblast.F
Chd|        HM_READ_PFLUID                source/loads/general/pfluid/hm_read_pfluid.F
Chd|        HM_READ_PLOAD                 source/loads/general/pload/hm_read_pload.F
Chd|        HM_READ_RBE2                  source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        HM_READ_RBE3                  source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        HM_READ_RBODY                 source/constraints/general/rbody/hm_read_rbody.F
Chd|        HM_READ_RBODY_LAGMUL          source/constraints/general/rbody/hm_read_rbody_lagmul.F
Chd|        HM_READ_RWALL_CYL             source/constraints/general/rwall/hm_read_rwall_cyl.F
Chd|        HM_READ_RWALL_LAGMUL          source/constraints/general/rwall/hm_read_rwall_lagmul.F
Chd|        HM_READ_RWALL_PARAL           source/constraints/general/rwall/hm_read_rwall_paral.F
Chd|        HM_READ_RWALL_PLANE           source/constraints/general/rwall/hm_read_rwall_plane.F
Chd|        HM_READ_RWALL_SPHER           source/constraints/general/rwall/hm_read_rwall_spher.F
Chd|        HM_READ_RWALL_THERM           source/constraints/general/rwall/hm_read_rwall_therm.F
Chd|        HM_READ_SPCND                 source/constraints/sph/hm_read_spcnd.F
Chd|        HM_READ_SPHIO                 source/loads/sph/hm_read_sphio.F
Chd|        HM_TAGPART                    source/groups/hm_tagpart.F    
Chd|        HM_TAGPART2                   source/groups/hm_tagpart2.F   
Chd|        I11STI3                       source/interfaces/inter3d1/i11sti3.F
Chd|        INIRBY                        source/constraints/general/rbody/inirby.F
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|        LECEXTLNK                     source/coupling/rad2rad/lecextlnk.F
Chd|        LECINT                        source/interfaces/interf1/lecint.F
Chd|        LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|        LECSEC4BOLT                   source/tools/sect/lecsec4bolt.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        NEW_LINK                      source/coupling/rad2rad/new_link.F
Chd|        OUTRIN                        source/materials/time_step/outri.F
Chd|        PGRTAILS                      source/elements/beam/pgrtails.F
Chd|        PRELECSEC                     source/tools/sect/prelecsec.F 
Chd|        PRELECSEC4BOLT                source/tools/sect/prelecsec4bolt.F
Chd|        PREREAD_RBODY_LAGMUL          source/constraints/general/rbody/preread_rbody_lagmul.F
Chd|        PRINTGROUP                    source/output/outfile/printgroup.F
Chd|        QGRTAILS                      source/elements/solid_2d/quad/qgrtails.F
Chd|        R2R_CHECK                     source/coupling/rad2rad/r2r_check.F
Chd|        R2R_CHECK_SEG                 source/coupling/rad2rad/routines_r2r.F
Chd|        R2R_CLEAN_INTER               source/coupling/rad2rad/r2r_clean_inter.F
Chd|        R2R_COUNT                     source/coupling/rad2rad/r2r_count.F
Chd|        R2R_DOMDEC                    source/coupling/rad2rad/r2r_domdec.F
Chd|        R2R_FORK                      source/coupling/rad2rad/r2r_fork.F
Chd|        R2R_GROUP                     source/coupling/rad2rad/r2r_group.F
Chd|        R2R_INPUT                     source/coupling/rad2rad/r2r_input.F
Chd|        R2R_MONVOL                    source/coupling/rad2rad/r2r_prelec.F
Chd|        R2R_NOM_OPT                   source/coupling/rad2rad/routines_r2r.F
Chd|        R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|        R2R_PRELEC_NAME               source/coupling/rad2rad/r2r_prelec_name.F
Chd|        R2R_SPEEDUP                   source/coupling/rad2rad/r2r_speedup.F
Chd|        R2R_SPLIT                     source/coupling/rad2rad/r2r_split.F
Chd|        R2R_VOID                      source/coupling/rad2rad/r2r_void.F
Chd|        R2R_VOID_1D                   source/coupling/rad2rad/r2r_void.F
Chd|        READ_FLAG_ALE                 source/coupling/rad2rad/r2r_fork.F
Chd|        READ_MONVOL                   source/airbag/read_monvol.F   
Chd|        READ_RWALL                    source/constraints/general/rwall/read_rwall.F
Chd|        RGRTAILS                      source/elements/spring/rgrtails.F
Chd|        SETRBYON                      source/constraints/general/rbody/hm_read_rbody.F
Chd|        SGRHEAD                       source/elements/solid/solide/sgrhead.F
Chd|        SGRTAILS                      source/elements/solid/solide/sgrtails.F
Chd|        SPGRTAILS                     source/elements/sph/spgrtails.F
Chd|        ST_QAPRINT_CONSTRAINTS        source/output/qaprint/st_qaprint_constraints.F
Chd|        ST_QAPRINT_MULTIDOMAINS       source/output/qaprint/st_qaprint_multidomains.F
Chd|        T3GRTAILS                     source/elements/solid_2d/tria/t3grtails.F
Chd|        TAGINT_R2R                    source/coupling/rad2rad/tagint_r2r.F
Chd|        TAGNOD_R2R_S                  source/coupling/rad2rad/tagnod_r2r.F
Chd|        TAG_ELEM_VOID_R2R             source/coupling/rad2rad/tagelem_r2r.F
Chd|        TAG_ELEM_VOID_R2R_LIN         source/coupling/rad2rad/tagelem_r2r.F
Chd|        TGRTAILS                      source/elements/truss/tgrtails.F
Chd|        XGRTAILS                      source/elements/xelem/xgrtails.F
Chd|        NODGR_R2R                     source/coupling/rad2rad/routines_r2r.F
Chd|        R2R_EXIST                     source/coupling/rad2rad/routines_r2r.F
Chd|        R2R_LISTCNT                   source/coupling/rad2rad/routines_r2r.F
Chd|-- calls ---------------
Chd|====================================================================
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
