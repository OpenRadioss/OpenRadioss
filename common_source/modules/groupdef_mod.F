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
Chd|  GROUPDEF_MOD                  modules/groupdef_mod.F        
Chd|-- called by -----------
Chd|        ALETG                         starter/source/airbag/fvmbag1.F
Chd|        ALE_BOX_COLORATION            starter/source/initial_conditions/inivol/ale_box_coloration.F
Chd|        BIGBOX                        starter/source/model/box/bigbox.F
Chd|        BIGBOX2                       starter/source/model/box/bigbox.F
Chd|        BIGSBOX                       starter/source/model/box/bigbox.F
Chd|        BULKFAKEIGEO3                 starter/source/elements/ige3d/bulkfakeigeo3.F
Chd|        C3GRHEAD                      starter/source/elements/sh3n/coque3n/c3grhead.F
Chd|        C3GRTAILS                     starter/source/elements/sh3n/coque3n/c3grtails.F
Chd|        C3INIT3                       starter/source/elements/sh3n/coque3n/c3init3.F
Chd|        CBAINIT3                      starter/source/elements/shell/coqueba/cbainit3.F
Chd|        CGRHEAD                       starter/source/elements/shell/coque/cgrhead.F
Chd|        CGRTAILS                      starter/source/elements/shell/coque/cgrtails.F
Chd|        CHECK_SURF                    starter/source/groups/check_surf.F
Chd|        CINIT3                        starter/source/elements/shell/coque/cinit3.F
Chd|        CREATE_MAP_TABLES             starter/source/model/sets/map_tables.F
Chd|        CREATE_SUBS_CLAUSE            starter/source/model/sets/create_subs_clause.F
Chd|        CREATE_SUBS_LIST              starter/source/model/sets/create_subs_clause.F
Chd|        CREATE_SUBS_LIST_G            starter/source/model/sets/create_subs_clause.F
Chd|        C_GROUP_STR                   starter/source/restart/ddsplit/c_group_str.F
Chd|        C_GR_ENTITY_E                 starter/source/model/group/c_gr_entity.F
Chd|        C_GR_ENTITY_N                 starter/source/model/group/c_gr_entity.F
Chd|        C_GR_ENTITY_P                 starter/source/model/group/c_gr_entity.F
Chd|        C_ISLIN_STR                   starter/source/restart/ddsplit/c_islin_str.F
Chd|        C_ISURF_STR                   starter/source/restart/ddsplit/c_isurf_str.F
Chd|        C_PON                         starter/source/restart/ddsplit/c_pon.F
Chd|        DAMPDTNODA                    starter/source/general_controls/damping/dampdtnoda.F
Chd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
Chd|        DEALLOCATE_IGRSURF_SPLIT      starter/source/spmd/deallocate_igrsurf_split.F
Chd|        DOMDEC2                       starter/source/spmd/domdec2.F 
Chd|        DOMETIS                       starter/source/spmd/domain_decomposition/grid2mat.F
Chd|        DSRGCNT                       starter/source/output/anim/dsrgcnt.F
Chd|        DSRGNOR                       starter/source/output/anim/dsrgnor.F
Chd|        DXYZSRG                       starter/source/output/anim/dxyzsrg.F
Chd|        ECRSUB2                       starter/source/model/assembling/hm_read_subset.F
Chd|        FAIL_WINDSHIELD_INIT          starter/source/materials/fail/windshield_alter/fail_windshield_init.F
Chd|        FILLCNE                       starter/source/spmd/domdec2.F 
Chd|        FILL_GR                       starter/source/model/sets/fill_gr.F
Chd|        FILL_IGR                      starter/source/model/sets/fill_igr.F
Chd|        FILL_LINE                     starter/source/model/sets/fill_gr.F
Chd|        FILL_SURF                     starter/source/model/sets/fill_gr.F
Chd|        FVBAG_TAGELE                  starter/source/airbag/fvmbag1.F
Chd|        FVBAG_VERTEX                  starter/source/spmd/domain_decomposition/grid2mat.F
Chd|        FVBRIC0                       starter/source/airbag/fvbric0.F
Chd|        FVBRIC01                      starter/source/airbag/fvbric01.F
Chd|        FVINJECT                      starter/source/airbag/fvinject.F
Chd|        FVINJECTINT                   starter/source/airbag/fvinjectint.F
Chd|        FVMESH0                       starter/source/airbag/fvmesh0.F
Chd|        FVTGI                         starter/source/airbag/fvmbag1.F
Chd|        FVVENTHOLE                    starter/source/airbag/fvventhole.F
Chd|        FVVENTHOLEINT                 starter/source/airbag/fvventholeint.F
Chd|        GENANI1                       starter/source/output/anim/genani1.F
Chd|        GROUP_INI                     starter/source/model/group/group_ini.F
Chd|        GR_ENTITY_INI                 starter/source/model/group/gr_entity_ini.F
Chd|        HM_BIGBOX                     starter/source/model/box/hm_bigbox.F
Chd|        HM_BIGBOX2                    starter/source/model/box/hm_bigbox2.F
Chd|        HM_BIGSBOX                    starter/source/groups/hm_bigsbox.F
Chd|        HM_DEBUG_PRINT_OPTION         starter/source/devtools/hm_reader/hm_debug_print_option.F
Chd|        HM_ELNGR                      starter/source/groups/hm_elngr.F
Chd|        HM_ELNGRR                     starter/source/groups/hm_elngrr.F
Chd|        HM_ELNGRS                     starter/source/groups/hm_elngr.F
Chd|        HM_GROGRO                     starter/source/groups/hm_grogro.F
Chd|        HM_GROGRONOD                  starter/source/groups/hm_grogronod.F
Chd|        HM_LECGRE                     starter/source/groups/hm_lecgre.F
Chd|        HM_LECGRN                     starter/source/groups/hm_lecgrn.F
Chd|        HM_LINENGR                    starter/source/groups/hm_linengr.F
Chd|        HM_LINES_OF_LINES             starter/source/groups/hm_lines_of_lines.F
Chd|        HM_PRELECGRNS                 starter/source/groups/hm_prelecgrns.F
Chd|        HM_PRELECJOI                  starter/source/constraints/general/cyl_joint/hm_prelecjoi.F
Chd|        HM_PREREAD_BCSCYC             starter/source/constraints/general/bcs/lecbcscyc.F
Chd|        HM_PREREAD_BEM                starter/source/loads/bem/hm_read_bem.F
Chd|        HM_PREREAD_CLOAD              starter/source/loads/general/cload/hm_preread_cload.F
Chd|        HM_PREREAD_CONVEC             starter/source/loads/thermic/hm_preread_convec.F
Chd|        HM_PREREAD_EIG                starter/source/general_controls/computation/hm_read_eig.F
Chd|        HM_PREREAD_GRAV               starter/source/loads/general/grav/hm_preread_grav.F
Chd|        HM_PREREAD_IMPACC             starter/source/constraints/general/impvel/hm_preread_impacc.F
Chd|        HM_PREREAD_IMPDISP            starter/source/constraints/general/impvel/hm_preread_impdisp.F
Chd|        HM_PREREAD_IMPFLUX            starter/source/constraints/thermic/hm_preread_impflux.F
Chd|        HM_PREREAD_IMPTEMP            starter/source/constraints/thermic/hm_preread_imptemp.F
Chd|        HM_PREREAD_IMPVEL             starter/source/constraints/general/impvel/hm_preread_impvel.F
Chd|        HM_PREREAD_LOAD_CENTRI        starter/source/loads/general/load_centri/hm_preread_load_centri.F
Chd|        HM_PREREAD_LOAD_PRESSURE      starter/source/loads/general/load_pressure/hm_preread_load_pressure.F
Chd|        HM_PREREAD_MERGE              starter/source/constraints/general/merge/hm_preread_merge.F
Chd|        HM_PREREAD_PBLAST             starter/source/loads/pblast/hm_preread_pblast.F
Chd|        HM_PREREAD_PFLUID             starter/source/loads/general/pfluid/hm_preread_pfluid.F
Chd|        HM_PREREAD_PLOAD              starter/source/loads/general/pload/hm_preread_pload.F
Chd|        HM_PREREAD_RADIATION          starter/source/loads/thermic/hm_preread_radiation.F
Chd|        HM_PREREAD_RBE2               starter/source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        HM_PREREAD_RBE3               starter/source/constraints/general/rbe3/hm_preread_rbe3.F
Chd|        HM_PREREAD_RBODY              starter/source/constraints/general/rbody/hm_preread_rbody.F
Chd|        HM_PREREAD_SPHIO              starter/source/loads/sph/hm_preread_sphio.F
Chd|        HM_PREREAD_XELEM              starter/source/elements/reader/hm_preread_xelem.F
Chd|        HM_PRE_READ_LINK              starter/source/constraints/rigidlink/hm_pre_read_rlink.F
Chd|        HM_READ_ACTIV                 starter/source/tools/activ/hm_read_activ.F
Chd|        HM_READ_ADMAS                 starter/source/tools/admas/hm_read_admas.F
Chd|        HM_READ_ALEBCS                starter/source/constraints/ale/hm_read_alebcs.F
Chd|        HM_READ_ALE_LINK              starter/source/constraints/ale/hm_read_ale_link_vel.F
Chd|        HM_READ_BCS                   starter/source/constraints/general/bcs/hm_read_bcs.F
Chd|        HM_READ_BEM                   starter/source/loads/bem/hm_read_bem.F
Chd|        HM_READ_CLOAD                 starter/source/loads/general/cload/hm_read_cload.F
Chd|        HM_READ_CLUSTER               starter/source/output/cluster/hm_read_cluster.F
Chd|        HM_READ_CONVEC                starter/source/loads/thermic/hm_read_convec.F
Chd|        HM_READ_CYLJOINT              starter/source/constraints/general/cyl_joint/hm_read_cyljoint.F
Chd|        HM_READ_DAMP                  starter/source/general_controls/damping/hm_read_damp.F
Chd|        HM_READ_DRAPE                 starter/source/properties/composite_options/drape/hm_read_drape.F
Chd|        HM_READ_DT                    starter/source/general_controls/engine/hm_read_dt.F
Chd|        HM_READ_EBCS_FLUXOUT          starter/source/boundary_conditions/ebcs/hm_read_ebcs_fluxout.F
Chd|        HM_READ_EBCS_GRADP0           starter/source/boundary_conditions/ebcs/hm_read_ebcs_gradp0.F
Chd|        HM_READ_EBCS_INIP             starter/source/boundary_conditions/ebcs/hm_read_ebcs_inip.F
Chd|        HM_READ_EBCS_INIV             starter/source/boundary_conditions/ebcs/hm_read_ebcs_iniv.F
Chd|        HM_READ_EBCS_INLET            starter/source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
Chd|        HM_READ_EBCS_MONVOL           starter/source/boundary_conditions/ebcs/hm_read_ebcs_monvol.F
Chd|        HM_READ_EBCS_NORMV            starter/source/boundary_conditions/ebcs/hm_read_ebcs_normv.F
Chd|        HM_READ_EBCS_NRF              starter/source/boundary_conditions/ebcs/hm_read_ebcs_nrf.F
Chd|        HM_READ_EBCS_PRES             starter/source/boundary_conditions/ebcs/hm_read_ebcs_pres.F
Chd|        HM_READ_EBCS_VALVIN           starter/source/boundary_conditions/ebcs/hm_read_ebcs_valvin.F
Chd|        HM_READ_EBCS_VALVOUT          starter/source/boundary_conditions/ebcs/hm_read_ebcs_valvout.F
Chd|        HM_READ_EBCS_VEL              starter/source/boundary_conditions/ebcs/hm_read_ebcs_vel.F
Chd|        HM_READ_EIG                   starter/source/general_controls/computation/hm_read_eig.F
Chd|        HM_READ_FRICTION              starter/source/interfaces/friction/reader/hm_read_friction.F
Chd|        HM_READ_FRICTION_MODELS       starter/source/interfaces/friction/reader/hm_read_friction_models.F
Chd|        HM_READ_FRICTION_ORIENTATIONS starter/source/interfaces/friction/reader/hm_read_friction_orientations.F
Chd|        HM_READ_GRAV                  starter/source/loads/general/grav/hm_read_grav.F
Chd|        HM_READ_GRPART                starter/source/groups/hm_read_grpart.F
Chd|        HM_READ_IMPACC                starter/source/constraints/general/impvel/hm_read_impacc.F
Chd|        HM_READ_IMPFLUX               starter/source/constraints/thermic/hm_read_impflux.F
Chd|        HM_READ_IMPTEMP               starter/source/constraints/thermic/hm_read_imptemp.F
Chd|        HM_READ_IMPVEL                starter/source/constraints/general/impvel/hm_read_impvel.F
Chd|        HM_READ_INIGRAV               starter/source/initial_conditions/inigrav/hm_read_inigrav.F
Chd|        HM_READ_INIMAP1D              starter/source/initial_conditions/inimap/hm_read_inimap1d.F
Chd|        HM_READ_INIMAP2D              starter/source/initial_conditions/inimap/hm_read_inimap2d.F
Chd|        HM_READ_INISTATE_D00          starter/source/elements/initia/hm_read_inistate_d00.F
Chd|        HM_READ_INITEMP               starter/source/initial_conditions/thermic/hm_read_initemp.F
Chd|        HM_READ_INIVEL                starter/source/initial_conditions/general/inivel/hm_read_inivel.F
Chd|        HM_READ_INIVOL                starter/source/initial_conditions/inivol/hm_read_inivol.F
Chd|        HM_READ_INTERFACES            starter/source/interfaces/reader/hm_read_interfaces.F
Chd|        HM_READ_INTER_FSI             starter/source/interfaces/reader/hm_read_inter_fsi.F
Chd|        HM_READ_INTER_HERTZ_TYPE17    starter/source/interfaces/int17/hm_read_inter_hertz_type17.F
Chd|        HM_READ_INTER_LAGDT_TYPE07    starter/source/interfaces/int07/hm_read_inter_lagdt_type07.F
Chd|        HM_READ_INTER_LAGMUL          starter/source/interfaces/reader/hm_read_inter_lagmul.F
Chd|        HM_READ_INTER_LAGMUL_TYPE02   starter/source/interfaces/int02/hm_read_inter_lagmul_type02.F
Chd|        HM_READ_INTER_LAGMUL_TYPE07   starter/source/interfaces/int07/hm_read_inter_lagmul_type07.F
Chd|        HM_READ_INTER_LAGMUL_TYPE16   starter/source/interfaces/int16/hm_read_inter_lagmul_type16.F
Chd|        HM_READ_INTER_LAGMUL_TYPE17   starter/source/interfaces/int17/hm_read_inter_lagmul_type17.F
Chd|        HM_READ_INTER_STRUCT          starter/source/interfaces/reader/hm_read_inter_struct.F
Chd|        HM_READ_INTER_TYPE01          starter/source/interfaces/int01/hm_read_inter_type01.F
Chd|        HM_READ_INTER_TYPE02          starter/source/interfaces/int02/hm_read_inter_type02.F
Chd|        HM_READ_INTER_TYPE03          starter/source/interfaces/int03/hm_read_inter_type03.F
Chd|        HM_READ_INTER_TYPE05          starter/source/interfaces/int05/hm_read_inter_type05.F
Chd|        HM_READ_INTER_TYPE06          starter/source/interfaces/int06/hm_read_inter_type06.F
Chd|        HM_READ_INTER_TYPE07          starter/source/interfaces/int07/hm_read_inter_type07.F
Chd|        HM_READ_INTER_TYPE08          starter/source/interfaces/int08/hm_read_inter_type08.F
Chd|        HM_READ_INTER_TYPE09          starter/source/interfaces/int09/hm_read_inter_type09.F
Chd|        HM_READ_INTER_TYPE10          starter/source/interfaces/int10/hm_read_inter_type10.F
Chd|        HM_READ_INTER_TYPE11          starter/source/interfaces/int11/hm_read_inter_type11.F
Chd|        HM_READ_INTER_TYPE12          starter/source/interfaces/int12/hm_read_inter_type12.F
Chd|        HM_READ_INTER_TYPE14          starter/source/interfaces/int14/hm_read_inter_type14.F
Chd|        HM_READ_INTER_TYPE15          starter/source/interfaces/int15/hm_read_inter_type15.F
Chd|        HM_READ_INTER_TYPE18          starter/source/interfaces/int18/hm_read_inter_type18.F
Chd|        HM_READ_INTER_TYPE20          starter/source/interfaces/int20/hm_read_inter_type20.F
Chd|        HM_READ_INTER_TYPE21          starter/source/interfaces/int21/hm_read_inter_type21.F
Chd|        HM_READ_INTER_TYPE22          starter/source/interfaces/int22/hm_read_inter_type22.F
Chd|        HM_READ_INTER_TYPE23          starter/source/interfaces/int23/hm_read_inter_type23.F
Chd|        HM_READ_INTER_TYPE24          starter/source/interfaces/int24/hm_read_inter_type24.F
Chd|        HM_READ_INTER_TYPE25          starter/source/interfaces/int25/hm_read_inter_type25.F
Chd|        HM_READ_INTSUB                starter/source/output/subinterface/hm_read_intsub.F
Chd|        HM_READ_LINES                 starter/source/groups/hm_read_lines.F
Chd|        HM_READ_LINK                  starter/source/constraints/rigidlink/hm_read_rlink.F
Chd|        HM_READ_LOAD_CENTRI           starter/source/loads/general/load_centri/hm_read_load_centri.F
Chd|        HM_READ_LOAD_PRESSURE         starter/source/loads/general/load_pressure/hm_read_load_pressure.F
Chd|        HM_READ_MERGE                 starter/source/constraints/general/merge/hm_read_merge.F
Chd|        HM_READ_MERGE_NODE            starter/source/elements/reader/hm_read_merge_node.F
Chd|        HM_READ_MONVOL_TYPE1          starter/source/airbag/hm_read_monvol_type1.F
Chd|        HM_READ_MONVOL_TYPE10         starter/source/airbag/hm_read_monvol_type10.F
Chd|        HM_READ_MONVOL_TYPE11         starter/source/airbag/hm_read_monvol_type11.F
Chd|        HM_READ_MONVOL_TYPE2          starter/source/airbag/hm_read_monvol_type2.F
Chd|        HM_READ_MONVOL_TYPE3          starter/source/airbag/hm_read_monvol_type3.F
Chd|        HM_READ_MONVOL_TYPE4          starter/source/airbag/hm_read_monvol_type4.F
Chd|        HM_READ_MONVOL_TYPE5          starter/source/airbag/hm_read_monvol_type5.F
Chd|        HM_READ_MONVOL_TYPE6          starter/source/airbag/hm_read_monvol_type6.F
Chd|        HM_READ_MONVOL_TYPE7          starter/source/airbag/hm_read_monvol_type7.F
Chd|        HM_READ_MONVOL_TYPE8          starter/source/airbag/hm_read_monvol_type8.F
Chd|        HM_READ_MONVOL_TYPE9          starter/source/airbag/hm_read_monvol_type9.F
Chd|        HM_READ_NBCS                  starter/source/constraints/general/bcs/hm_read_nbcs.F
Chd|        HM_READ_PBLAST                starter/source/loads/pblast/hm_read_pblast.F
Chd|        HM_READ_PERTURB               starter/source/general_controls/computation/hm_read_perturb.F
Chd|        HM_READ_PERTURB_FAIL          starter/source/general_controls/computation/hm_read_perturb_fail.F
Chd|        HM_READ_PERTURB_PART_SHELL    starter/source/general_controls/computation/hm_read_perturb_part_shell.F
Chd|        HM_READ_PERTURB_PART_SOLID    starter/source/general_controls/computation/hm_read_perturb_part_solid.F
Chd|        HM_READ_PFLUID                starter/source/loads/general/pfluid/hm_read_pfluid.F
Chd|        HM_READ_PLOAD                 starter/source/loads/general/pload/hm_read_pload.F
Chd|        HM_READ_PRETHGROU             starter/source/output/th/hm_read_prethgrou.F
Chd|        HM_READ_RADIATION             starter/source/loads/thermic/hm_read_radiation.F
Chd|        HM_READ_RAND                  starter/source/general_controls/computation/hm_read_rand.F
Chd|        HM_READ_RBE2                  starter/source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        HM_READ_RBE3                  starter/source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        HM_READ_RBODY                 starter/source/constraints/general/rbody/hm_read_rbody.F
Chd|        HM_READ_RBODY_LAGMUL          starter/source/constraints/general/rbody/hm_read_rbody_lagmul.F
Chd|        HM_READ_RETRACTOR             starter/source/tools/seatbelts/hm_read_retractor.F
Chd|        HM_READ_RWALL_CYL             starter/source/constraints/general/rwall/hm_read_rwall_cyl.F
Chd|        HM_READ_RWALL_LAGMUL          starter/source/constraints/general/rwall/hm_read_rwall_lagmul.F
Chd|        HM_READ_RWALL_PARAL           starter/source/constraints/general/rwall/hm_read_rwall_paral.F
Chd|        HM_READ_RWALL_PLANE           starter/source/constraints/general/rwall/hm_read_rwall_plane.F
Chd|        HM_READ_RWALL_SPHER           starter/source/constraints/general/rwall/hm_read_rwall_spher.F
Chd|        HM_READ_RWALL_THERM           starter/source/constraints/general/rwall/hm_read_rwall_therm.F
Chd|        HM_READ_SENSORS               starter/source/tools/sensor/hm_read_sensors.F
Chd|        HM_READ_SLIPRING              starter/source/tools/seatbelts/hm_read_slipring.F
Chd|        HM_READ_SPCND                 starter/source/constraints/sph/hm_read_spcnd.F
Chd|        HM_READ_SPHIO                 starter/source/loads/sph/hm_read_sphio.F
Chd|        HM_READ_SUBSET                starter/source/model/assembling/hm_read_subset.F
Chd|        HM_READ_SURF                  starter/source/groups/hm_read_surf.F
Chd|        HM_READ_SURFSURF              starter/source/groups/hm_read_surfsurf.F
Chd|        HM_READ_THGROU                starter/source/output/th/hm_read_thgrou.F
Chd|        HM_READ_THGRPA_SUB            starter/source/output/th/hm_read_thgrpa.F
Chd|        HM_READ_THGRSENS              starter/source/output/th/hm_read_thgrsens.F
Chd|        HM_READ_THGRSURF              starter/source/output/th/hm_read_thgrsurf.F
Chd|        HM_READ_THPART                starter/source/output/thpart/hm_read_thpart.F
Chd|        HM_READ_XELEM                 starter/source/elements/reader/hm_read_xelem.F
Chd|        HM_SET                        starter/source/model/sets/hm_set.F
Chd|        HM_SUBMODGRE                  starter/source/groups/hm_submodgr.F
Chd|        HM_SUBMODGRN                  starter/source/groups/hm_submodgr.F
Chd|        HM_SURFGR2                    starter/source/groups/hm_surfgr2.F
Chd|        HM_SURFNOD                    starter/source/groups/hm_surfnod.F
Chd|        HM_TAGPART                    starter/source/groups/hm_tagpart.F
Chd|        HM_TAGPART2                   starter/source/groups/hm_tagpart2.F
Chd|        HM_YCTRL                      starter/source/elements/initia/hm_yctrl.F
Chd|        I1CHK3                        starter/source/interfaces/inter3d1/i1chk3.F
Chd|        I20INI3                       starter/source/interfaces/inter3d1/i20ini3.F
Chd|        I20STI3                       starter/source/interfaces/inter3d1/i20sti3.F
Chd|        I20SURFI                      starter/source/interfaces/inter3d1/i20surfi.F
Chd|        I21ELS3                       starter/source/interfaces/inter3d1/i21els3.F
Chd|        I24GAPM                       starter/source/interfaces/inter3d1/i24sti3.F
Chd|        I24STI3                       starter/source/interfaces/inter3d1/i24sti3.F
Chd|        I24SURFI                      starter/source/interfaces/inter3d1/i24surfi.F
Chd|        I25SURFI                      starter/source/interfaces/inter3d1/i25surfi.F
Chd|        I2CHK3                        starter/source/interfaces/inter3d1/i2chk3.F
Chd|        I3STI3                        starter/source/interfaces/inter3d1/i3sti3.F
Chd|        I7STI3                        starter/source/interfaces/inter3d1/i7sti3.F
Chd|        I9STI3                        starter/source/interfaces/int09/i9sti3.F
Chd|        IG3DGRHEAD                    starter/source/elements/ige3d/ig3dgrhead.F
Chd|        IG3DGRTAILS                   starter/source/elements/ige3d/ig3dgrtails.F
Chd|        IGRSURF_SPLIT                 starter/source/spmd/igrsurf_split.F
Chd|        INIEBCS                       starter/source/boundary_conditions/ebcs/iniebcs.F
Chd|        INIGRAV_LOAD                  starter/source/initial_conditions/inigrav/inigrav_load.F
Chd|        INIGRAV_PART_LIST             starter/source/initial_conditions/inigrav/inigrav_part_list.F
Chd|        ININT3                        starter/source/interfaces/inter3d1/inint3.F
Chd|        ININTR                        starter/source/interfaces/interf1/inintr.F
Chd|        ININTSUB                      starter/source/interfaces/interf1/inintsub.F
Chd|        ININTSUB_11                   starter/source/output/subinterface/inintsub_11.F
Chd|        ININTSUB_25                   starter/source/output/subinterface/inintsub_25.F
Chd|        ININTSUB_7                    starter/source/output/subinterface/inintsub_7.F
Chd|        INISMS                        starter/source/general_controls/computation/hm_read_sms.F
Chd|        INISOLDIST                    starter/source/initial_conditions/inivol/inisoldist.F
Chd|        INISRF                        starter/source/constraints/general/rbody/inisrf.F
Chd|        INITIA                        starter/source/elements/initia/initia.F
Chd|        INIT_MONVOL                   starter/source/airbag/init_monvol.F
Chd|        INIVEL                        starter/source/initial_conditions/general/inivel/inivel.F
Chd|        INI_FVMINIVEL                 starter/source/elements/initia/ini_fvminivel.F
Chd|        INI_INIMAP1D                  starter/source/initial_conditions/inimap/ini_inimap1d.F
Chd|        INI_INIMAP2D                  starter/stub/ini_inimap2d.F   
Chd|        INSPCND                       starter/source/elements/sph/inspcnd.F
Chd|        IPARI_L_INI                   starter/source/restart/ddsplit/ipari_l_ini.F
Chd|        ISLIN_INI                     starter/source/model/group/islin_ini.F
Chd|        ISURF_INI                     starter/source/model/group/isurf_ini.F
Chd|        LAGM_INI                      starter/source/tools/lagmul/lagm_ini.F
Chd|        LCE16Q3                       starter/source/elements/solid_2d/quad/lce16q.F
Chd|        LCE16S3                       starter/source/elements/reader/hm_read_solid.F
Chd|        LECGGROUP                     starter/source/groups/lecggroup.F
Chd|        LECGROUP                      starter/source/groups/lecgroup.F
Chd|        LECINS                        starter/source/interfaces/interf1/lecins.F
Chd|        LECINT                        starter/source/interfaces/interf1/lecint.F
Chd|        LECSEC42                      starter/source/tools/sect/hm_read_sect.F
Chd|        LECSEC4BOLT                   starter/source/tools/sect/lecsec4bolt.F
Chd|        LECTRANS                      starter/source/model/transformation/lectrans.F
Chd|        LECTRANSSUB                   starter/source/model/submodel/lectranssub.F
Chd|        LECTUR                        starter/source/starter/lectur.F
Chd|        LEC_INIMAP1D_FILE             starter/source/initial_conditions/inimap/lec_inimap1d_file.F
Chd|        LEC_INIMAP2D_FILE             starter/source/initial_conditions/inimap/lec_inimap2d_file.F
Chd|        LEC_INISTATE                  starter/source/elements/initia/lec_inistate.F
Chd|        LGMINI_BC                     starter/source/tools/lagmul/lgmini_bc.F
Chd|        LGMINI_I7                     starter/source/tools/lagmul/lgmini_i7.F
Chd|        LINE_DECOMP                   starter/source/model/group/line_decomp.F
Chd|        MERGE_NODE                    starter/source/elements/nodes/merge_node.F
Chd|        MONVOL_BUILD_EDGES            starter/share/modules1/monvol_struct_mod.F
Chd|        MONVOL_CHECK_SURFCLOSE        starter/share/modules1/monvol_struct_mod.F
Chd|        MONVOL_CHECK_VENTHOLE_SURF    starter/share/modules1/monvol_struct_mod.F
Chd|        MONVOL_COMPUTE_VOLUME         starter/share/modules1/monvol_struct_mod.F
Chd|        MONVOL_ORIENT_SURF            starter/share/modules1/monvol_struct_mod.F
Chd|        MONVOL_REVERSE_NORMALS        starter/share/modules1/monvol_struct_mod.F
Chd|        MONVOL_TRIANGULATE_SURFACE    starter/source/airbag/monvol_triangulate_surface.F
Chd|        PGRHEAD                       starter/source/elements/beam/pgrhead.F
Chd|        PGRTAILS                      starter/source/elements/beam/pgrtails.F
Chd|        PRELECDET                     starter/source/initial_conditions/detonation/prelecdet.F
Chd|        PRELECSEC                     starter/source/tools/sect/prelecsec.F
Chd|        PRELECSEC4BOLT                starter/source/tools/sect/prelecsec4bolt.F
Chd|        PREPARE_SPLIT_I17             starter/source/restart/ddsplit/inter_tools.F
Chd|        PREREAD_RBODY_LAGMUL          starter/source/constraints/general/rbody/preread_rbody_lagmul.F
Chd|        PRESEARCHIGEO3D               starter/source/elements/ige3d/searchigeo3d.F
Chd|        PRE_STACKGROUP                starter/source/stack/pres_stackgroup.F
Chd|        PRINTBCS                      starter/source/constraints/general/bcs/printbcs.F
Chd|        PRINTGROUP                    starter/source/output/outfile/printgroup.F
Chd|        QA_PRINT_GROUPS               starter/source/output/qaprint/st_qaprint_groups.F
Chd|        QA_PRINT_SURF                 starter/source/output/qaprint/st_qaprint_surf.F
Chd|        QGRHEAD                       starter/source/elements/solid_2d/quad/qgrhead.F
Chd|        QGRTAILS                      starter/source/elements/solid_2d/quad/qgrtails.F
Chd|        QSURFTAG                      starter/source/groups/qsurftag.F
Chd|        R2R_CHECK                     starter/source/coupling/rad2rad/r2r_check.F
Chd|        R2R_COUNT                     starter/source/coupling/rad2rad/r2r_count.F
Chd|        R2R_DOMDEC                    starter/source/coupling/rad2rad/r2r_domdec.F
Chd|        R2R_GROUP                     starter/source/coupling/rad2rad/r2r_group.F
Chd|        R2R_MONVOL                    starter/source/coupling/rad2rad/r2r_prelec.F
Chd|        R2R_PRELEC                    starter/source/coupling/rad2rad/r2r_prelec.F
Chd|        R2R_SPLIT                     starter/source/coupling/rad2rad/r2r_split.F
Chd|        RATIO_FILL                    starter/source/initial_conditions/inivol/ratio_fill.F
Chd|        READ_DETONATORS               starter/source/initial_conditions/detonation/read_detonators.F
Chd|        READ_DFS_DETCORD              starter/source/initial_conditions/detonation/read_dfs_detcord.F
Chd|        READ_DFS_DETLINE              starter/source/initial_conditions/detonation/read_dfs_detline.F
Chd|        READ_DFS_DETPLAN              starter/source/initial_conditions/detonation/read_dfs_detplan.F
Chd|        READ_DFS_DETPOINT             starter/source/initial_conditions/detonation/read_dfs_detpoint.F
Chd|        READ_DFS_WAVE_SHAPER          starter/source/initial_conditions/detonation/read_dfs_wave_shaper.F
Chd|        READ_EBCS                     starter/source/boundary_conditions/ebcs/read_ebcs.F
Chd|        READ_ENGINE_DRIVER            starter/source/general_controls/engine/read_engine_driver.F
Chd|        READ_IMPDISP                  starter/source/constraints/general/impvel/read_impdisp.F
Chd|        READ_IMPDISP_FGEO             starter/source/constraints/general/impvel/read_impdisp_fgeo.F
Chd|        READ_IMPVEL                   starter/source/constraints/general/impvel/read_impvel.F
Chd|        READ_IMPVEL_FGEO              starter/source/constraints/general/impvel/read_impvel_fgeo.F
Chd|        READ_IMPVEL_LAGMUL            starter/source/constraints/general/impvel/read_impvel_lagmul.F
Chd|        READ_MONVOL                   starter/source/airbag/read_monvol.F
Chd|        READ_RWALL                    starter/source/constraints/general/rwall/read_rwall.F
Chd|        READ_SENSOR_ENERGY            starter/source/tools/sensor/read_sensor_energy.F
Chd|        READ_SENSOR_NIC               starter/source/tools/sensor/read_sensor_nic.F
Chd|        READ_SENSOR_TEMP              starter/source/tools/sensor/read_sensor_temp.F
Chd|        RECONNECT                     starter/source/elements/nodes/reconnect.F
Chd|        REORDR                        starter/source/elements/solid_2d/quad/reordr.F
Chd|        RGRHEAD                       starter/source/elements/spring/rgrhead.F
Chd|        RGRTAILS                      starter/source/elements/spring/rgrtails.F
Chd|        SBOXBOXSURF                   starter/source/model/box/bigbox.F
Chd|        SEARCHIGEO3D                  starter/source/elements/ige3d/searchigeo3d.F
Chd|        SEARCHIGEO3D2                 starter/source/elements/ige3d/searchigeo3d.F
Chd|        SEARCHIGEO3D3                 starter/source/elements/ige3d/searchigeo3d.F
Chd|        SEARCHIGEO3DOLD               starter/source/elements/ige3d/searchigeo3d.F
Chd|        SEC_NODES                     starter/source/tools/sect/hm_read_sect.F
Chd|        SEC_NODES_SOL                 starter/source/tools/sect/hm_read_sect.F
Chd|        SETELOFF                      starter/source/constraints/general/rbody/hm_read_rbody.F
Chd|        SETELOFF2                     starter/source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        SETRB2ON                      starter/source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        SETRBYON                      starter/source/constraints/general/rbody/hm_read_rbody.F
Chd|        SET_IBUFSSG_IO                starter/source/starter/lectur.F
Chd|        SGRHEAD                       starter/source/elements/solid/solide/sgrhead.F
Chd|        SGRTAILS                      starter/source/elements/solid/solide/sgrtails.F
Chd|        SORT_SURF                     starter/source/groups/sort_surf.F
Chd|        SPLIT_EBCS                    starter/source/boundary_conditions/ebcs/split_ebcs.F
Chd|        SPLIT_INTERFACES              starter/source/restart/ddsplit/split_interfaces.F
Chd|        SSURFIGEO                     starter/source/groups/ssurftagigeo.F
Chd|        SSURFTAG                      starter/source/groups/ssurftag.F
Chd|        SSURFTAGIGEO                  starter/source/groups/ssurftagigeo.F
Chd|        STACKGROUP                    starter/source/stack/stackgroup.F
Chd|        STACKGROUP_DRAPE              starter/source/stack/stackgroup_drape.F
Chd|        ST_QAPRINT_DFS_DETONATORS     starter/source/output/qaprint/st_qaprint_dfs_detonators.F
Chd|        ST_QAPRINT_DFS_LASERS         starter/source/output/qaprint/st_qaprint_dfs_lasers.F
Chd|        ST_QAPRINT_DRIVER             starter/source/output/qaprint/st_qaprint_driver.F
Chd|        ST_QAPRINT_GROUPS             starter/source/output/qaprint/st_qaprint_groups.F
Chd|        ST_QAPRINT_INIVOL             starter/source/output/qaprint/st_qaprint_inivol.F
Chd|        ST_QAPRINT_INTERNAL_GROUPS    starter/source/output/qaprint/st_qaprint_internal_groups.F
Chd|        ST_QAPRINT_MADYMO             starter/source/output/qaprint/st_qaprint_madymo.F
Chd|        ST_QAPRINT_MULTIDOMAINS       starter/source/output/qaprint/st_qaprint_multidomains.F
Chd|        ST_QAPRINT_SET                starter/source/output/qaprint/st_qaprint_set.F
Chd|        ST_QAPRINT_SURF               starter/source/output/qaprint/st_qaprint_surf.F
Chd|        ST_QAPRINT_THGROU             starter/source/output/qaprint/st_qaprint_time_histories.F
Chd|        ST_QAPRINT_TIME_HISTORIES     starter/source/output/qaprint/st_qaprint_time_histories.F
Chd|        ST_QAPRINT_TRANSFORMATIONS    starter/source/output/qaprint/st_qaprint_transformations.F
Chd|        SUBSET_INI                    starter/source/model/group/subset_ini.F
Chd|        SURFACE_MIN_MAX_COMPUTATION   starter/source/initial_conditions/inivol/surface_min_max_computation.F
Chd|        SURFTAG                       starter/source/groups/surftag.F
Chd|        SURFTAGADM                    starter/source/groups/surftag.F
Chd|        SURFTAGE                      starter/source/groups/surftage.F
Chd|        SURFTAGX                      starter/source/groups/surftag.F
Chd|        T3GRHEAD                      starter/source/elements/solid_2d/tria/t3grhead.F
Chd|        T3GRTAILS                     starter/source/elements/solid_2d/tria/t3grtails.F
Chd|        TAGINT_R2R                    starter/source/coupling/rad2rad/tagint_r2r.F
Chd|        TAG_ELEM_VOID_R2R             starter/source/coupling/rad2rad/tagelem_r2r.F
Chd|        TAG_ELEM_VOID_R2R_LIN         starter/source/coupling/rad2rad/tagelem_r2r.F
Chd|        TGRHEAD                       starter/source/elements/truss/tgrhead.F
Chd|        TGRTAILS                      starter/source/elements/truss/tgrtails.F
Chd|        THPRIN_SUB                    starter/source/output/th/thprin.F
Chd|        TSURFTAG                      starter/source/groups/tsurftag.F
Chd|        W_FRONT                       starter/source/restart/ddsplit/w_front.F
Chd|        W_GROUP_STR                   starter/source/restart/ddsplit/w_group_str.F
Chd|        W_GR_ENTITY_E                 starter/source/restart/ddsplit/w_gr_entity.F
Chd|        W_GR_ENTITY_N                 starter/source/restart/ddsplit/w_gr_entity.F
Chd|        W_GR_ENTITY_P                 starter/source/restart/ddsplit/w_gr_entity.F
Chd|        W_ISLIN_STR                   starter/source/restart/ddsplit/w_islin_str.F
Chd|        W_ISURF_STR                   starter/source/restart/ddsplit/w_isurf_str.F
Chd|        W_PON                         starter/source/restart/ddsplit/w_pon.F
Chd|        W_SUBSET_STR                  starter/source/restart/ddsplit/w_subset_str.F
Chd|        XGRHEAD                       starter/source/elements/xelem/xgrhead.F
Chd|        XGRTAILS                      starter/source/elements/xelem/xgrtails.F
Chd|        YCTRL                         starter/source/initial_conditions/inista/yctrl.F
Chd|        ELEGROR                       starter/source/groups/elegror.F
Chd|        ELEGROR_SEATBELT              starter/source/groups/elegror_seatbelt.F
Chd|        GRFIND                        starter/source/starter/freform.F
Chd|        GRSIZE                        starter/source/system/nintrr.F
Chd|        GRSIZEN                       starter/source/system/nintrr.F
Chd|        GRSIZE_ELE                    starter/source/system/nintrr.F
Chd|        GRSIZE_ELE_TRANS              starter/source/system/nintrr.F
Chd|        GRSIZE_R2R                    starter/source/coupling/rad2rad/routines_r2r.F
Chd|        NGR2USRN                      starter/source/system/nintrr.F
Chd|        NINTRIGR                      starter/source/system/nintrr.F
Chd|        NODGRNR5                      starter/source/starter/freform.F
Chd|        NODGRNR6                      starter/source/starter/freform.F
Chd|        NODGR_R2R                     starter/source/coupling/rad2rad/routines_r2r.F
Chd|        R2R_EXIST                     starter/source/coupling/rad2rad/routines_r2r.F
Chd|        GROUP_MOD                     starter/share/modules1/group_mod.F
Chd|        AIRBAGB                       engine/source/airbag/airbag2.F
Chd|        AIRBAGB1                      engine/source/airbag/airbagb1.F
Chd|        ALELIN                        engine/source/ale/grid/alelin.F
Chd|        ALEMAIN                       engine/source/ale/alemain.F   
Chd|        ALEWDX                        engine/source/ale/grid/alewdx.F
Chd|        ALLOC_GROUP_STR               engine/source/groups/alloc_group_str.F
Chd|        ALLOC_LINE_STR                engine/source/groups/alloc_line_str.F
Chd|        ALLOC_SUBSET_STR              engine/source/groups/alloc_subset_str.F
Chd|        ALLOC_SURF_STR                engine/source/groups/alloc_surf_str.F
Chd|        DAMPING                       engine/source/assembly/damping.F
Chd|        DAMPING44                     engine/source/assembly/damping.F
Chd|        DAMPING51                     engine/source/assembly/damping.F
Chd|        DESACTI                       engine/source/elements/desacti.F
Chd|        DIM_GLOB_K                    engine/source/implicit/ind_glob_k.F
Chd|        DSRGCNT                       engine/source/output/anim/generate/dsrgcnt.F
Chd|        DSRGNOR                       engine/source/output/anim/generate/dsrgnor.F
Chd|        DTNODA                        engine/source/time_step/dtnoda.F
Chd|        DTNODAMP                      engine/source/time_step/dtnodamp.F
Chd|        DTNODAMS                      engine/source/time_step/dtnodams.F
Chd|        DTNODARAYL                    engine/source/time_step/dtnodarayl.F
Chd|        DXYZSRG                       engine/source/output/anim/generate/dxyzsrg.F
Chd|        EBCS_MAIN                     engine/source/boundary_conditions/ebcs/ebcs_main.F
Chd|        EIG                           engine/stub/eig.F             
Chd|        EIG1                          engine/stub/eig1.F            
Chd|        EIGCOND                       engine/stub/eigcond.F         
Chd|        EIGP                          engine/stub/eigp.F            
Chd|        ELOFF                         engine/source/elements/eloff.F
Chd|        FILTER_SAE211                 engine/source/tools/sensor/filter_sae211.F
Chd|        FORINT                        engine/source/elements/forint.F
Chd|        FORINTC                       engine/source/elements/forintc.F
Chd|        GENANI                        engine/source/output/anim/generate/genani.F
Chd|        GENH3D                        engine/source/output/h3d/h3d_results/genh3d.F
Chd|        GROUP_INI                     engine/source/groups/group_ini.F
Chd|        H3D_NODAL_VECTOR              engine/source/output/h3d/h3d_results/h3d_nodal_vector.F
Chd|        H3D_VELVECZ22                 engine/source/output/h3d/h3d_results/h3d_velvecz22.F
Chd|        HIST1                         engine/source/output/th/hist1.F
Chd|        HIST2                         engine/source/output/th/hist2.F
Chd|        I14CAN                        engine/source/interfaces/int14/i14can.F
Chd|        I14CMP                        engine/source/interfaces/int14/i14cmp.F
Chd|        I14DMP                        engine/source/interfaces/int14/i14dmp.F
Chd|        I14ELA                        engine/source/interfaces/int14/i14ela.F
Chd|        I14FRT                        engine/source/interfaces/int14/i14frt.F
Chd|        I14IST                        engine/source/interfaces/int14/i14ist.F
Chd|        I14WFS                        engine/source/interfaces/int14/i14wfs.F
Chd|        I15ASS                        engine/source/interfaces/int15/i15ass.F
Chd|        I15CAN                        engine/source/interfaces/int15/i15can.F
Chd|        I15CMP                        engine/source/interfaces/int15/i15cmp.F
Chd|        I15FOR1                       engine/source/interfaces/int15/i15for1.F
Chd|        I15FORT1                      engine/source/interfaces/int15/i15fort1.F
Chd|        I15TOT1                       engine/source/interfaces/int15/i15tot1.F
Chd|        I15TOTT1                      engine/source/interfaces/int15/i15tott1.F
Chd|        I16MAIN                       engine/source/interfaces/int16/i16main.F
Chd|        I17MAIN                       engine/source/interfaces/int17/i17main.F
Chd|        I17MAIN_CRIT_TRI              engine/source/interfaces/int17/i17main_pena.F
Chd|        I17MAIN_PENA                  engine/source/interfaces/int17/i17main_pena.F
Chd|        I17MAIN_TRI                   engine/source/interfaces/int17/i17main_pena.F
Chd|        I22MAINF                      engine/source/interfaces/int22/i22mainf.F
Chd|        I22MAIN_TRI                   engine/source/interfaces/intsort/i22main_tri.F
Chd|        I22SUBVOL                     engine/source/interfaces/int22/i22subvol.F
Chd|        I7MAINF                       engine/source/interfaces/int07/i7mainf.F
Chd|        IMP_BUCK                      engine/source/implicit/imp_buck.F
Chd|        IMP_INISI                     engine/source/implicit/imp_pcg.F
Chd|        IMP_INIST                     engine/source/implicit/imp_pcg.F
Chd|        IMP_INTTD0                    engine/source/implicit/imp_int_k.F
Chd|        IMP_LANZP                     engine/source/implicit/imp_lanz.F
Chd|        IMP_PCGH                      engine/source/implicit/imp_pcg.F
Chd|        IMP_PPCGH                     engine/source/implicit/imp_pcg.F
Chd|        IMP_SOLV                      engine/source/implicit/imp_solv.F
Chd|        IMP_SOL_INIT                  engine/source/implicit/imp_sol_init.F
Chd|        IMP_TRIPI                     engine/source/implicit/imp_int_k.F
Chd|        IMP_UPDST                     engine/source/implicit/imp_pcg.F
Chd|        IMP_UPDV2                     engine/source/implicit/imp_pcg.F
Chd|        INIT_TH                       engine/source/output/th/init_th.F
Chd|        INIT_TH0                      engine/source/output/th/init_th0.F
Chd|        INIT_TH_GROUP                 engine/source/output/th/init_th_group.F
Chd|        INT18_LAW151_ALLOC            engine/source/interfaces/int18/int18_law151_alloc.F
Chd|        INT18_LAW151_INIT             engine/source/interfaces/int18/int18_law151_init.F
Chd|        INT18_LAW151_NSV_SHIFT        engine/source/interfaces/int18/int18_law151_nsv_shift.F
Chd|        INT18_LAW151_UPDATE           engine/source/interfaces/int18/int18_law151_update.F
Chd|        INTFOP1                       engine/source/interfaces/interf/intfop1.F
Chd|        INTFOP2                       engine/source/interfaces/interf/intfop2.F
Chd|        INTTRI                        engine/source/interfaces/intsort/inttri.F
Chd|        LAG_BCS                       engine/source/tools/lagmul/lag_bcs.F
Chd|        LAG_MULT                      engine/source/tools/lagmul/lag_mult.F
Chd|        LECDAMP                       engine/source/input/lecdamp.F 
Chd|        LECINV                        engine/source/input/lecinv.F  
Chd|        LECTUR                        engine/source/input/lectur.F  
Chd|        LINE_INI                      engine/source/groups/line_ini.F
Chd|        LIN_SOLV                      engine/source/implicit/lin_solv.F
Chd|        LIN_SOLVH0                    engine/source/implicit/lin_solv.F
Chd|        LIN_SOLVH1                    engine/source/implicit/lin_solv.F
Chd|        LIN_SOLVHM                    engine/source/implicit/lin_solv.F
Chd|        LIN_SOLVIH2                   engine/source/implicit/lin_solv.F
Chd|        LTAG_BCS                      engine/source/tools/lagmul/lag_ntag.F
Chd|        MAV_LT2                       engine/source/implicit/produt_v.F
Chd|        MAV_LTH                       engine/source/implicit/produt_v.F
Chd|        MAV_LTH0                      engine/source/implicit/produt_v.F
Chd|        MAV_LTP                       engine/source/implicit/produt_v.F
Chd|        MMAV_LTH                      engine/source/implicit/produt_v.F
Chd|        MONVOL0                       engine/source/airbag/monvol0.F
Chd|        MONV_IMP                      engine/source/airbag/monv_imp0.F
Chd|        MONV_KD                       engine/source/airbag/monv_imp0.F
Chd|        MONV_M3                       engine/source/airbag/monv_imp0.F
Chd|        MONV_PREM                     engine/source/airbag/monv_imp0.F
Chd|        MPP_INIT                      engine/source/mpi/interfaces/spmd_i7tool.F
Chd|        MV_MATV                       engine/source/airbag/monv_imp0.F
Chd|        NL_SOLV                       engine/source/implicit/nl_solv.F
Chd|        PBLAST                        engine/source/loads/pblast/pblast.F
Chd|        PBLAST_1                      engine/source/loads/pblast/pblast_1.F
Chd|        PBLAST_2                      engine/source/loads/pblast/pblast_2.F
Chd|        PBLAST_3                      engine/source/loads/pblast/pblast_3.F
Chd|        R2R_EXCHANGE                  engine/source/coupling/rad2rad/r2r_exchange.F
Chd|        R2R_EXCH_ITAG                 engine/source/coupling/rad2rad/r2r_exchange.F
Chd|        R2R_GETDATA                   engine/source/coupling/rad2rad/r2r_getdata.F
Chd|        R2R_INIT                      engine/source/coupling/rad2rad/r2r_init.F
Chd|        R2R_SENDKINE                  engine/source/coupling/rad2rad/r2r_getdata.F
Chd|        RADIOSS2                      engine/source/engine/radioss2.F
Chd|        RBYFOR                        engine/source/constraints/general/rbody/rbyfor.F
Chd|        RESOL                         engine/source/engine/resol.F  
Chd|        RESOL_HEAD                    engine/source/engine/resol_head.F
Chd|        RESOL_INIT                    engine/source/engine/resol_init.F
Chd|        S10CNIDAMP                    engine/source/elements/solid/solide10/s10cndf.F
Chd|        S10CNISTAT                    engine/source/elements/solid/solide10/s10cndf.F
Chd|        SENSOR_BASE                   engine/source/tools/sensor/sensor_base.F
Chd|        SENSOR_DIST_SURF              engine/source/tools/sensor/sensor_dist_surf.F
Chd|        SENSOR_DIST_SURF0             engine/source/tools/sensor/sensor_dist_surf0.F
Chd|        SENSOR_ENERGY                 engine/source/tools/sensor/sensor_energy.F
Chd|        SENSOR_ENERGY_PART            engine/source/tools/sensor/sensor_energy_part.F
Chd|        SENSOR_HIC                    engine/source/tools/sensor/sensor_hic.F
Chd|        SENSOR_INIT                   engine/source/tools/sensor/sensor_init.F
Chd|        SENSOR_NIC                    engine/source/tools/sensor/sensor_nic.F
Chd|        SENSOR_SPMD                   engine/source/tools/sensor/sensor_spmd.F
Chd|        SENSOR_TEMP                   engine/source/tools/sensor/sensor_temp.F
Chd|        SENSOR_TEMP0                  engine/source/tools/sensor/sensor_temp0.F
Chd|        SINIT22_FVM                   engine/source/interfaces/int22/sinit22_fvm.F
Chd|        SMS_INI_PART                  engine/source/ams/sms_init.F  
Chd|        SMS_MASS_SCALE_2              engine/source/ams/sms_mass_scale_2.F
Chd|        SORTIE_MAIN                   engine/source/output/sortie_main.F
Chd|        SPHPREP                       engine/source/elements/sph/sphprep.F
Chd|        SPMD_I7FCOM_POFF              engine/source/mpi/forces/spmd_i7fcom_poff.F
Chd|        SPMD_I7FCOM_PON               engine/source/mpi/forces/spmd_i7fcom_pon.F
Chd|        SPMD_I7XVCOM2                 engine/source/mpi/interfaces/spmd_i7xvcom2.F
Chd|        SPONOF1                       engine/source/elements/sph/sponof1.F
Chd|        SPONOF2                       engine/source/elements/sph/sponof2.F
Chd|        SRFVIT                        engine/source/constraints/general/rbody/srfvit.F
Chd|        STATIC                        engine/source/general_controls/damping/static.F
Chd|        SUBSET_INI                    engine/source/groups/subset_ini.F
Chd|        SURF_INI                      engine/source/groups/surf_ini.F
Chd|        SURF_MASS_MONV                engine/source/output/th/surf_mass.F
Chd|        SWITCH_TO_DTNODA              engine/source/time_step/switch_to_dtnoda.F
Chd|        TAGOFF3N                      engine/source/interfaces/interf/chkstfn3.F
Chd|        VELVECZ22                     engine/source/output/anim/generate/velvecz22.F
Chd|        VOLPVGB                       engine/source/airbag/volpvg.F 
Chd|        WRRESTP                       engine/source/output/restart/wrrestp.F
Chd|        W_GROUP_STR                   engine/source/output/restart/w_group_str.F
Chd|        W_GR_ENTITY                   engine/source/output/restart/w_gr_entity.F
Chd|        W_LINE_STR                    engine/source/output/restart/w_line_str.F
Chd|        W_SUBSET_STR                  engine/source/output/restart/w_subset_str.F
Chd|        W_SURF_STR                    engine/source/output/restart/w_surf_str.F
Chd|        NGR2USR                       engine/source/input/freform.F 
Chd|        GROUP_MOD                     engine/share/modules/group_mod.F
Chd|        FIND_DT_FOR_TARGETED_ADDED_MASSengine/source/time_step/find_dt_for_targeted_added_mass.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE GROUPDEF_MOD
C----------------------------------------------------------------------- 
#include "nchar_c.inc"     
!#include "nchara_c.inc"   !!!  engine
c=======================================================================      
!---------
!   NEW DATA STRUCTURE
!---------
c=======================================================================   
c=======================================================================   
!                               SUBSETS
c=======================================================================   
C=======================================================================
      TYPE SUBSET_
        INTEGER   :: ID       ! - SUBSET identifier
        INTEGER   :: LEVEL    ! - Hierarchy level
        INTEGER   :: TH_FLAG  ! - TIME HISTORY flag
        INTEGER   :: PARENT   ! - SUBSET PARENT
        INTEGER   :: NCHILD   ! - SUBSETS number of childs
        INTEGER   :: NPART    ! - SUBSET number of part (within one subset)
        INTEGER   :: NTPART   ! - Total number of parts (within parent subset)
        INTEGER   :: THIAD    ! - IAD for global ITHBUF storage variables
        INTEGER, DIMENSION(:)   ,ALLOCATABLE :: CHILD ! - SUBSET's child
        INTEGER, DIMENSION(:)   ,ALLOCATABLE :: PART  ! - SUBSET's part
        INTEGER, DIMENSION(:)   ,ALLOCATABLE :: TPART ! - PARTS  (parts of the parent SUBSET)
!---TIME HISTORY ( /TH et /ATH )
        INTEGER, DIMENSION(:)   ,ALLOCATABLE :: NVARTH ! - ! nb of subset TH/ATH variable
!---
        CHARACTER :: TITLE*nchartitle ! SUBSET title
      END TYPE SUBSET_
C-----------------------------------------------------------------------

c=======================================================================   
c=======================================================================   
!                               GROUPS
c=======================================================================   
C=======================================================================

C-----------------------------------------------------------------------
      TYPE GROUP_
        INTEGER   :: ID ! - GROUP identifier
        CHARACTER :: TITLE*nchartitle ! GROUP title
        INTEGER   :: SET_GROUP ! need to not printout empty set groups
        INTEGER   :: NENTITY ! - Entities' number of one the GROUP (NODES, ELEMENTS, PARTS)
        INTEGER, DIMENSION(:),ALLOCATABLE :: ENTITY   ! -Entity identifier (node, element or part)
        INTEGER   :: GRTYPE ! TYPE ( 0-NOEUDS, 1-BRIC, 2-QUAD,   3-SHELL_4N,
!                                    4-TRUSS,  5-BEAM, 6-SPRINGS,7-SHELL_3N )
!!                   GRTYPE --- > OBSOLETE
        INTEGER   :: SORTED ! FLAG for sorted/unsorted nodes
!                    = 0 ! sorted
!                    = 1 ! unsorted
        INTEGER   :: GRPGRP ! TYPE OF nodal (element, part) GROUP
!  For node group:
!                                 = 1 FOR /GRNOD/NOD
!                                 = 2 FOR /GRNOD/GNROD
!                                 = 0 ALL THE REST
!  For element (part) group:
!                                 = 1 ELEM
!                                 = 2 GRELEM
!                                 = 3 SUBSET,PART,MAT,PROP
!                                 = 4 BOX,GENE
!                                 = 5 SUBMOD
        INTEGER   :: LEVEL ! Hierarchy level
!                             (FLAG 'SUBLEVEL DONE' FOR GROUPS OF GROUPS)
!                            = 0 ---> not yet initialized
!                            = 1 ---> done
!--------------
!---
!  - R2R -
!                              ! R2R_ALL <--- IGROUP2(8,I) = IGROUP2(2,I) -- before splitting
!                              ! R2R_SHARE <--- IGROUP2(9,I)   (shared on boundary subdomain)
        INTEGER   :: R2R_ALL   ! Multidomaines -> number of node(elems, parts) before split
        INTEGER   :: R2R_SHARE ! shared on boundary subdomain
!--------------
      END TYPE GROUP_
C-----------------------------------------------------------------------

c=======================================================================   
c=======================================================================   
!                               SURFS
c=======================================================================   
C=======================================================================

C-----------------------------------------------------------------------
      TYPE SURF_
        INTEGER   :: ID ! - SURFACE (or LINE) identifier
        CHARACTER :: TITLE*nchartitle ! SURF title
        INTEGER   :: NSEG ! - Number of surfaces (lines) within  /SURF
        INTEGER   :: NSEG_IGE    ! Number of iso-surfaces
        INTEGER   :: IAD_IGE    ! Pointer to fictive node in IGA tabulars
        INTEGER   :: SET_GROUP ! need to not printout empty set groups
        INTEGER   :: TYPE ! OPEN / CLOSED surface flag
!                        SURF_TYPE = 0         : SEGMENTS
!                        SURF_TYPE = 100       : HYPER-ELLIPSOIDE MADYMO.
!                        SURF_TYPE = 101       : HYPER-ELLIPSOIDE RADIOSS.
!                        SURF_TYPE = 200       : INFINITE PLANE
        INTEGER   :: ID_MADYMO  ! Coupled madimo surface identifier
!                                 (computed in Radioss Engine, when receiving Datas from MaDyMo).
!                                 ID MaDyMo - for entity type which impose surface movement:
!                                 No systeme MaDyMo for entity type which impose surface movement
        INTEGER   :: IAD_BUFR   ! Analytical Surfaces address (reals BUFSF - temp)
        INTEGER   :: NB_MADYMO  ! No de l'entite qui impose le mvt de la surface.
!                                --> No systeme Radioss ou MaDyMO.
        INTEGER   :: TYPE_MADYMO  ! Entity type which impose surface movement.
!                                  = 1 : Rigid Body.
!                                  = 2 : MADYMO Hyper-ellipsoide.
        INTEGER   :: LEVEL        ! FLAG "SUBLEVEL DONE" FOR SURFACES OF SURFACES
!                                 = 0 ! initialized surface
!                                 = 1 ! uninitialized surface
        INTEGER   :: TH_SURF      ! FLAG for /TH/SURF
!                                 = 0 ! unsaved surface for /TH/SURF
!                                 = 1 ! saved surface for /TH/SURF
        INTEGER   :: ISH4N3N      ! FLAG = 1 (only SH4N and SH3N considered - for airbags)
!
        INTEGER   :: NSEG_R2R_ALL   ! Multidomaines -> number of segments before split
        INTEGER   :: NSEG_R2R_SHARE ! shared on boundary subdomain segments
!
        INTEGER       , DIMENSION(:) , ALLOCATABLE :: REVERSED
        INTEGER   , DIMENSION(:),   ALLOCATABLE :: ELTYP  ! dim = (NSEG)
 ! - type of element attached to the segment of the surface
!                           ITYP = 0  - surf of segments
!                           ITYP = 1  - surf of solids
!                           ITYP = 2  - surf of quads
!                           ITYP = 3  - surf of SH4N
!                           ITYP = 4  - line of trusses
!                           ITYP = 5  - line of beams
!                           ITYP = 6  - line of springs
!                           ITYP = 7  - surf of SH3N
!                           ITYP = 8  - line of XELEM (nstrand element)
!                           ITYP = 101 - ISOGEOMETRIQUE
        INTEGER   , DIMENSION(:),   ALLOCATABLE :: ELEM   ! dim = (NSEG)
        INTEGER   , DIMENSION(:,:), ALLOCATABLE :: NODES  ! dim =(NSEG,4) -SURF
!                                                         ! dim =(NSEG,2) -LINE
        INTEGER   , DIMENSION(:),   ALLOCATABLE :: PROC   ! dim = (NSEG)
!                               field to store the processor ID  (/LINE only)
!
!   1. Set processor only when no element is set in lines
!   2. Split lines accordingly
!
!  ISOGEOMETRIQUE :
        INTEGER   , DIMENSION(:),   ALLOCATABLE :: ELTYP_IGE  ! dim = (NSEG)
        INTEGER   , DIMENSION(:),   ALLOCATABLE :: ELEM_IGE   ! dim = (NSEG)
        INTEGER   , DIMENSION(:,:), ALLOCATABLE :: NODES_IGE  ! dim =(NSEG,4) -SURF
!                                                             ! dim =(NSEG,2) -LINE
!!        TYPE (SEG_)   , DIMENSION(:) , ALLOCATABLE :: SEG     ! - segments of the surface
!!        TYPE (SEG_)   , DIMENSION(:) , ALLOCATABLE :: SEG_IGE ! - ISOGEOMETRIQUES segments of the surface
!--------------
      END TYPE SURF_
!--------------
!!      TYPE SEG_
!!        INTEGER   :: ELTYP ! - type of element attached to the segment of the surface
!                           ITYP = 0  - surf of segments
!                           ITYP = 1  - surf of solids
!                           ITYP = 2  - surf of quads
!                           ITYP = 3  - surf of SH4N
!                           ITYP = 4  - line of trusses
!                           ITYP = 5  - line of beams
!                           ITYP = 6  - line of springs
!                           ITYP = 7  - surf of SH3N
!                           ITYP = 8  - line of XELEM (nstrand element)
!                           ITYP = 101 - ISOGEOMETRIQUE
!!        INTEGER   :: ELEM ! - element attached to the segment of the surface
!!        INTEGER, DIMENSION(:),ALLOCATABLE :: NODES   ! - 4 nodes of the segment for /SURF
!                                                    ! - 2 nodes of the segment for /LINE
!
!!        INTEGER   :: PROC  ! field to store the processor ID  (/LINE only)
!
!   1. Set processor only when no element is set in lines
!   2. Split lines accordingly
!
!!      END TYPE SEG_
c---------------
      END MODULE GROUPDEF_MOD
