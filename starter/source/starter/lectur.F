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
Chd|  LECTUR                        source/starter/lectur.F       
Chd|-- called by -----------
Chd|        STARTER0                      source/starter/starter0.F     
Chd|-- calls ---------------
Chd|        ADDMAST10                     source/tools/admas/addmast10.F
Chd|        ADD_MASS_STAT                 source/tools/admas/add_mass_stat.F
Chd|        ADMBCS                        source/model/remesh/admbcs.F  
Chd|        ALELEC                        source/ale/alelec.F           
Chd|        ALLOCXFEM                     source/elements/xfem/allocxfem.F
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANI_FASOLFR1                  source/output/anim/ani_fasolfr.F
Chd|        ANI_FASOLFR2                  source/output/anim/ani_fasolfr.F
Chd|        ANI_SEGQUADFR1                source/output/anim/ani_segquadfr.F
Chd|        ANI_SEGQUADFR2                source/output/anim/ani_segquadfr.F
Chd|        ANODIN                        source/output/analyse/analyse_node.c
Chd|        APARTIN                       source/output/analyse/analyse_part.c
Chd|        APPLYSORT2FLUX                source/restart/ddsplit/w_ithflux.F
Chd|        APPLYSORT2FVM                 source/airbag/fvmesh0.F       
Chd|        ARRET                         source/system/arret.F         
Chd|        BCSCYCMODIF_ND                source/elements/solid/solide10/dim_s10edg.F
Chd|        BCSMODIF_ND                   source/elements/solid/solide10/dim_s10edg.F
Chd|        BUILD_ADDCNEL_SUB             source/model/mesh/build_addcnel_sub.F
Chd|        BUILD_ADMESH                  source/model/remesh/build_admesh.F
Chd|        BUILD_CNEL                    source/model/mesh/build_cnel.F
Chd|        BUILD_CNEL_SUB                source/model/mesh/build_cnel_sub.F
Chd|        BUILD_CSRECT                  source/model/mesh/build_cnel.F
Chd|        BULKFAKEIGEO3                 source/elements/ige3d/bulkfakeigeo3.F
Chd|        C3GRHEAD                      source/elements/sh3n/coque3n/c3grhead.F
Chd|        C3GRTAILS                     source/elements/sh3n/coque3n/c3grtails.F
Chd|        CDK6INX                       source/elements/sh3n/coquedk6/cdk6inx.F
Chd|        CGRHEAD                       source/elements/shell/coque/cgrhead.F
Chd|        CGRTAILS                      source/elements/shell/coque/cgrtails.F
Chd|        CHECKRBY                      source/constraints/general/rbody/checkrby.F
Chd|        CHECK_DYNAIN                  source/starter/check_dynain.F 
Chd|        CHECK_PTHICKFAIL              source/materials/fail/check_pthickfail.F
Chd|        CHECK_QEPH_STRA               source/starter/check_qeph_stra.F
Chd|        CHECK_SURF                    source/groups/check_surf.F    
Chd|        CHKFUNCT                      source/tools/curve/lecfun.F   
Chd|        CHKTYP2                       source/interfaces/interf1/chktyp2.F
Chd|        CHK_DTTSH                     source/elements/thickshell/solidec/scdtchk3.F
Chd|        COMPUTE_CONNECT_PARTELM       source/model/sets/compute_connect_partelm.F
Chd|        COPY_TO_MONVOL                share/modules1/monvol_struct_mod.F
Chd|        COPY_TO_VOLMON                share/modules1/monvol_struct_mod.F
Chd|        CREATE_MAP_TABLES             source/model/sets/map_tables.F
Chd|        CREATE_SEATBELT               source/tools/seatbelts/create_seatbelt.F
Chd|        DAMPDTNODA                    source/general_controls/damping/dampdtnoda.F
Chd|        DDPRINT                       source/spmd/domdec2.F         
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|        DEALLOCATE_DETONATORS         share/modules1/detonators_mod.F
Chd|        DEALLOCATE_ELBUF              source/elements/elbuf_init/deallocate_buffer.F
Chd|        DEALLOCATE_IGRSURF_SPLIT      source/spmd/deallocate_igrsurf_split.F
Chd|        DEALLOCATE_JOINT              source/constraints/general/cyl_joint/deallocate_joint.F
Chd|        DEALLOCATE_SPLIT_CFD_SOLIDE   source/spmd/split_cfd_solide.F
Chd|        DESOUT                        source/output/outp/desout.F   
Chd|        DIM_S10EDG                    source/elements/solid/solide10/dim_s10edg.F
Chd|        DOMDEC1                       source/spmd/domain_decomposition/domdec1.F
Chd|        DOMDEC2                       source/spmd/domdec2.F         
Chd|        DOMETIS                       source/spmd/domain_decomposition/grid2mat.F
Chd|        DSDIM0                        source/implicit/dsolve/dsdim.F
Chd|        ELBUF_INI                     source/elements/elbuf_init/elbuf_ini.F
Chd|        FAILWAVE_INIT                 source/materials/fail/failwave_init.F
Chd|        FICTIVMASSIGEO                source/groups/ssurftagigeo.F  
Chd|        FILLCNCND                     source/elements/solid/solide10/dim_s10edg.F
Chd|        FILLCNE                       source/spmd/domdec2.F         
Chd|        FILLCNE_PXFEM                 source/properties/composite_options/stack/preplyxfem.F
Chd|        FILLCNE_XFEM                  source/elements/xfem/fillcne_xfem.F
Chd|        FILLCNI2                      source/spmd/domdec2.F         
Chd|        FILL_INTERCEP                 source/spmd/node/ddtools.F    
Chd|        FIXMODIF_ND                   source/elements/solid/solide10/dim_s10edg.F
Chd|        FLOWDEC                       source/fluid/flowdec.F        
Chd|        FSDCOD                        source/system/fsdcod.F        
Chd|        FVDIM                         source/airbag/fvmesh.F        
Chd|        FVMESH0                       source/airbag/fvmesh0.F       
Chd|        FXBELNUM                      source/constraints/fxbody/fxbelnum.F
Chd|        FXBGRAV                       source/constraints/fxbody/fxbgrav.F
Chd|        FXBTAGN                       source/constraints/fxbody/fxbtagn.F
Chd|        GENANI1                       source/output/anim/genani1.F  
Chd|        GET_SIZE_INTER24              source/spmd/get_size_tag.F    
Chd|        GET_SIZE_NUMNOD_LOCAL         source/spmd/get_size_tag.F    
Chd|        GLOBMAT                       source/materials/globmat.F    
Chd|        GLOBVARS                      source/spmd/globvars.F        
Chd|        GROUP_INI                     source/model/group/group_ini.F
Chd|        HM_GROGRONOD                  source/groups/hm_grogronod.F  
Chd|        HM_LECGRN                     source/groups/hm_lecgrn.F     
Chd|        HM_LINES_OF_LINES             source/groups/hm_lines_of_lines.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_PRELECGRNS                 source/groups/hm_prelecgrns.F 
Chd|        HM_PRELECJOI                  source/constraints/general/cyl_joint/hm_prelecjoi.F
Chd|        HM_PREREAD_BCSCYC             source/constraints/general/bcs/lecbcscyc.F
Chd|        HM_PREREAD_BEM                source/loads/bem/hm_read_bem.F
Chd|        HM_PREREAD_CLOAD              source/loads/general/cload/hm_preread_cload.F
Chd|        HM_PREREAD_CONVEC             source/loads/thermic/hm_preread_convec.F
Chd|        HM_PREREAD_EIG                source/general_controls/computation/hm_read_eig.F
Chd|        HM_PREREAD_GRAV               source/loads/general/grav/hm_preread_grav.F
Chd|        HM_PREREAD_IMPACC             source/constraints/general/impvel/hm_preread_impacc.F
Chd|        HM_PREREAD_IMPDISP            source/constraints/general/impvel/hm_preread_impdisp.F
Chd|        HM_PREREAD_IMPFLUX            source/constraints/thermic/hm_preread_impflux.F
Chd|        HM_PREREAD_IMPTEMP            source/constraints/thermic/hm_preread_imptemp.F
Chd|        HM_PREREAD_IMPVEL             source/constraints/general/impvel/hm_preread_impvel.F
Chd|        HM_PREREAD_IMPVEL0            source/constraints/general/impvel/hm_preread_impvel0.F
Chd|        HM_PREREAD_LOAD_CENTRI        source/loads/general/load_centri/hm_preread_load_centri.F
Chd|        HM_PREREAD_LOAD_PRESSURE      source/loads/general/load_pressure/hm_preread_load_pressure.F
Chd|        HM_PREREAD_MERGE              source/constraints/general/merge/hm_preread_merge.F
Chd|        HM_PREREAD_PART               source/model/assembling/hm_read_part.F
Chd|        HM_PREREAD_PBLAST             source/loads/pblast/hm_preread_pblast.F
Chd|        HM_PREREAD_PFLUID             source/loads/general/pfluid/hm_preread_pfluid.F
Chd|        HM_PREREAD_PLOAD              source/loads/general/pload/hm_preread_pload.F
Chd|        HM_PREREAD_RADIATION          source/loads/thermic/hm_preread_radiation.F
Chd|        HM_PREREAD_RBE2               source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        HM_PREREAD_RBE3               source/constraints/general/rbe3/hm_preread_rbe3.F
Chd|        HM_PREREAD_RBODY              source/constraints/general/rbody/hm_preread_rbody.F
Chd|        HM_PREREAD_SPHIO              source/loads/sph/hm_preread_sphio.F
Chd|        HM_PREREAD_XELEM              source/elements/reader/hm_preread_xelem.F
Chd|        HM_PRE_READ_LINK              source/constraints/rigidlink/hm_pre_read_rlink.F
Chd|        HM_PRE_READ_PRELOAD           source/loads/general/preload/hm_pre_read_preload.F
Chd|        HM_READ_ACTIV                 source/tools/activ/hm_read_activ.F
Chd|        HM_READ_ADMAS                 source/tools/admas/hm_read_admas.F
Chd|        HM_READ_ALEBCS                source/constraints/ale/hm_read_alebcs.F
Chd|        HM_READ_ALE_LINK              source/constraints/ale/hm_read_ale_link_vel.F
Chd|        HM_READ_BCS                   source/constraints/general/bcs/hm_read_bcs.F
Chd|        HM_READ_BEAM                  source/elements/reader/hm_read_beam.F
Chd|        HM_READ_BEM                   source/loads/bem/hm_read_bem.F
Chd|        HM_READ_BOX                   source/model/box/hm_read_box.F
Chd|        HM_READ_CLOAD                 source/loads/general/cload/hm_read_cload.F
Chd|        HM_READ_CLUSTER               source/output/cluster/hm_read_cluster.F
Chd|        HM_READ_CONVEC                source/loads/thermic/hm_read_convec.F
Chd|        HM_READ_CYLJOINT              source/constraints/general/cyl_joint/hm_read_cyljoint.F
Chd|        HM_READ_DAMP                  source/general_controls/damping/hm_read_damp.F
Chd|        HM_READ_DRAPE                 source/properties/composite_options/drape/hm_read_drape.F
Chd|        HM_READ_EIG                   source/general_controls/computation/hm_read_eig.F
Chd|        HM_READ_EREF                  source/loads/reference_state/eref/hm_read_eref.F
Chd|        HM_READ_FRICTION_MODELS       source/interfaces/friction/reader/hm_read_friction_models.F
Chd|        HM_READ_FRICTION_ORIENTATIONS source/interfaces/friction/reader/hm_read_friction_orientations.F
Chd|        HM_READ_FRM                   source/tools/skew/hm_read_frm.F
Chd|        HM_READ_FUNC2D                source/tools/curve/hm_read_func2d.F
Chd|        HM_READ_FUNCT                 source/tools/curve/hm_read_funct.F
Chd|        HM_READ_FXB1                  source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_READ_FXB2                  source/constraints/fxbody/hm_read_fxb.F
Chd|        HM_READ_GAUGE                 source/output/gauge/hm_read_gauge.F
Chd|        HM_READ_GJOINT                source/constraints/general/gjoint/hm_read_gjoint.F
Chd|        HM_READ_GRAV                  source/loads/general/grav/hm_read_grav.F
Chd|        HM_READ_GRPART                source/groups/hm_read_grpart.F
Chd|        HM_READ_IMPACC                source/constraints/general/impvel/hm_read_impacc.F
Chd|        HM_READ_IMPFLUX               source/constraints/thermic/hm_read_impflux.F
Chd|        HM_READ_IMPTEMP               source/constraints/thermic/hm_read_imptemp.F
Chd|        HM_READ_IMPVEL                source/constraints/general/impvel/hm_read_impvel.F
Chd|        HM_READ_INICRACK              source/initial_conditions/inicrack/hm_read_inicrack.F
Chd|        HM_READ_INIGRAV               source/initial_conditions/inigrav/hm_read_inigrav.F
Chd|        HM_READ_INIMAP1D              source/initial_conditions/inimap/hm_read_inimap1d.F
Chd|        HM_READ_INIMAP2D              source/initial_conditions/inimap/hm_read_inimap2d.F
Chd|        HM_READ_INITEMP               source/initial_conditions/thermic/hm_read_initemp.F
Chd|        HM_READ_INIVEL                source/initial_conditions/general/inivel/hm_read_inivel.F
Chd|        HM_READ_INIVOL                source/initial_conditions/inivol/hm_read_inivol.F
Chd|        HM_READ_INTERFACES            source/interfaces/reader/hm_read_interfaces.F
Chd|        HM_READ_INTSUB                source/output/subinterface/hm_read_intsub.F
Chd|        HM_READ_LINES                 source/groups/hm_read_lines.F 
Chd|        HM_READ_LINK                  source/constraints/rigidlink/hm_read_rlink.F
Chd|        HM_READ_LOAD_CENTRI           source/loads/general/load_centri/hm_read_load_centri.F
Chd|        HM_READ_LOAD_PRESSURE         source/loads/general/load_pressure/hm_read_load_pressure.F
Chd|        HM_READ_MERGE                 source/constraints/general/merge/hm_read_merge.F
Chd|        HM_READ_MERGE_NODE            source/elements/reader/hm_read_merge_node.F
Chd|        HM_READ_MOVE_FUNCT            source/tools/curve/hm_read_move_funct.F
Chd|        HM_READ_MPC                   source/constraints/general/mpc/hm_read_mpc.F
Chd|        HM_READ_MPC0                  source/constraints/general/mpc/hm_read_mpc.F
Chd|        HM_READ_NBCS                  source/constraints/general/bcs/hm_read_nbcs.F
Chd|        HM_READ_NODE                  source/elements/reader/hm_read_node.F
Chd|        HM_READ_PART                  source/model/assembling/hm_read_part.F
Chd|        HM_READ_PBLAST                source/loads/pblast/hm_read_pblast.F
Chd|        HM_READ_PCYL                  source/loads/general/load_pcyl/hm_read_pcyl.F
Chd|        HM_READ_PERTURB               source/general_controls/computation/hm_read_perturb.F
Chd|        HM_READ_PFLUID                source/loads/general/pfluid/hm_read_pfluid.F
Chd|        HM_READ_PLOAD                 source/loads/general/pload/hm_read_pload.F
Chd|        HM_READ_PRELECDRAPE           source/properties/composite_options/drape/hm_read_drape.F
Chd|        HM_READ_PRELOAD               source/loads/general/preload/hm_read_preload.F
Chd|        HM_READ_PRETHGROU             source/output/th/hm_read_prethgrou.F
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|        HM_READ_QUAD                  source/elements/reader/hm_read_quad.F
Chd|        HM_READ_RADIATION             source/loads/thermic/hm_read_radiation.F
Chd|        HM_READ_RAND                  source/general_controls/computation/hm_read_rand.F
Chd|        HM_READ_RBE2                  source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        HM_READ_RBE3                  source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        HM_READ_RBODY                 source/constraints/general/rbody/hm_read_rbody.F
Chd|        HM_READ_RBODY_LAGMUL          source/constraints/general/rbody/hm_read_rbody_lagmul.F
Chd|        HM_READ_RETRACTOR             source/tools/seatbelts/hm_read_retractor.F
Chd|        HM_READ_RIVET                 source/elements/reader/hm_read_rivet.F
Chd|        HM_READ_SENSORS               source/tools/sensor/hm_read_sensors.F
Chd|        HM_READ_SH3N                  source/elements/reader/hm_read_sh3n.F
Chd|        HM_READ_SHELL                 source/elements/reader/hm_read_shell.F
Chd|        HM_READ_SKW                   source/tools/skew/hm_read_skw.F
Chd|        HM_READ_SLIPRING              source/tools/seatbelts/hm_read_slipring.F
Chd|        HM_READ_SOLID                 source/elements/reader/hm_read_solid.F
Chd|        HM_READ_SPCND                 source/constraints/sph/hm_read_spcnd.F
Chd|        HM_READ_SPHCEL                source/elements/reader/hm_read_sphcel.F
Chd|        HM_READ_SPHIO                 source/loads/sph/hm_read_sphio.F
Chd|        HM_READ_SPRING                source/elements/reader/hm_read_spring.F
Chd|        HM_READ_SUBSET                source/model/assembling/hm_read_subset.F
Chd|        HM_READ_SURF                  source/groups/hm_read_surf.F  
Chd|        HM_READ_SURFSURF              source/groups/hm_read_surfsurf.F
Chd|        HM_READ_TABLE1                source/tools/curve/hm_read_table.F
Chd|        HM_READ_TABLE2                source/tools/curve/hm_read_table.F
Chd|        HM_READ_THGROU                source/output/th/hm_read_thgrou.F
Chd|        HM_READ_THPART                source/output/thpart/hm_read_thpart.F
Chd|        HM_READ_TRIA                  source/elements/reader/hm_read_tria.F
Chd|        HM_READ_TRUSS                 source/elements/reader/hm_read_truss.F
Chd|        HM_READ_WINDOW_USER           source/tools/userwi/hm_read_window_user.F
Chd|        HM_READ_XELEM                 source/elements/reader/hm_read_xelem.F
Chd|        HM_READ_XREF                  source/loads/reference_state/xref/hm_read_xref.F
Chd|        HM_SET                        source/model/sets/hm_set.F    
Chd|        HM_SETFXRBYON                 source/constraints/fxbody/hm_setfxrbyon.F
Chd|        HM_YCTRL                      source/elements/initia/hm_yctrl.F
Chd|        I24SETNODES                   source/interfaces/inter3d1/i24setnodes.F
Chd|        IEDGE_XFEM                    source/elements/xfem/iedge_xfem.F
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        IG3DGRHEAD                    source/elements/ige3d/ig3dgrhead.F
Chd|        IG3DGRTAILS                   source/elements/ige3d/ig3dgrtails.F
Chd|        IGRSURF_SPLIT                 source/spmd/igrsurf_split.F   
Chd|        IND_S10EDG                    source/elements/solid/solide10/dim_s10edg.F
Chd|        INICRKFILL                    source/elements/xfem/inicrkfill.F
Chd|        INIEBCS                       source/boundary_conditions/ebcs/iniebcs.F
Chd|        INIEBCSP0                     source/boundary_conditions/ebcs/iniebcsp0.F
Chd|        INIMU2                        source/ale/bimat/inimu2.F     
Chd|        INIMU3                        source/ale/bimat/inimu3.F     
Chd|        INIMUL                        source/ale/bimat/inimul.F     
Chd|        ININTR                        source/interfaces/interf1/inintr.F
Chd|        ININTR1                       source/interfaces/interf1/inintr1.F
Chd|        ININTR2                       source/interfaces/inter3d1/inintr2.F
Chd|        ININTR_ORTHDIRFRIC            source/interfaces/interf1/inintr_orthdirfric.F
Chd|        ININTR_THKVAR                 source/interfaces/interf1/inintr_thkvar.F
Chd|        ININTSUB                      source/interfaces/interf1/inintsub.F
Chd|        INIRBE3                       source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        INISEN                        source/tools/sensor/inisen.F  
Chd|        INISMS                        source/general_controls/computation/hm_read_sms.F
Chd|        INITIA                        source/elements/initia/initia.F
Chd|        INIT_JOINT                    source/constraints/general/cyl_joint/init_joint.F
Chd|        INIT_MAT_WEIGHT               source/user_interface/set_dd_mat_weight.F
Chd|        INIT_MLAW_TAG                 source/elements/elbuf_init/init_mlaw_tag.F
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|        INIT_PERMUTATION              source/starter/lectur.F       
Chd|        INIT_RANDOM                   source/general_controls/computation/init_random.F
Chd|        INIVCHK                       source/constraints/general/kinchk.F
Chd|        INIVEL                        source/initial_conditions/general/inivel/inivel.F
Chd|        INI_BCSCYC                    source/constraints/general/bcs/lecbcscyc.F
Chd|        INI_H3DTMAX_ENGINE            source/starter/contrl.F       
Chd|        INI_IFRONT                    source/spmd/node/ddtools.F    
Chd|        INT8_INI                      source/interfaces/intbuf/intbuf_ini_starter.F
Chd|        INTBUF_FRIC_COPY              source/interfaces/interf1/intbuf_fric_copy.F
Chd|        INTBUF_FRIC_INI_STARTER       source/interfaces/intbuf/intbufFric_ini_starter.F
Chd|        INTBUF_INI_STARTER            source/interfaces/intbuf/intbuf_ini_starter.F
Chd|        INTSTAMP_ZERO                 share/modules1/intstamp_mod.F 
Chd|        INTXRBE3                      source/constraints/general/rbe3/hm_read_rbe3.F
Chd|        INVERTED_GROUP_DEALLOC        source/model/sets/inverted_group_dealloc.F
Chd|        INVERTED_GROUP_INIT           source/model/sets/inverted_group_init.F
Chd|        ISLIN_INI                     source/model/group/islin_ini.F
Chd|        ISURF_INI                     source/model/group/isurf_ini.F
Chd|        KINCHK                        source/constraints/general/kinchk.F
Chd|        KININI                        source/constraints/general/kinini.F
Chd|        KINREM                        source/constraints/general/kinchk.F
Chd|        LAGM_INI                      source/tools/lagmul/lagm_ini.F
Chd|        LAGM_NHF                      source/tools/lagmul/lagm_nhf.F
Chd|        LASERP1                       source/loads/laser/laserp.F   
Chd|        LASERP3                       source/loads/laser/laserp.F   
Chd|        LCE16Q3                       source/elements/solid_2d/quad/lce16q.F
Chd|        LCE16S3                       source/elements/reader/hm_read_solid.F
Chd|        LCE16S4                       source/elements/reader/hm_read_solid.F
Chd|        LECACC                        source/tools/accele/lecacc.F  
Chd|        LECEXTLNK                     source/coupling/rad2rad/lecextlnk.F
Chd|        LECFILL                       source/elements/initia/lecfill.F
Chd|        LECGGROUP                     source/groups/lecggroup.F     
Chd|        LECGROUP                      source/groups/lecgroup.F      
Chd|        LECIG3D                       source/elements/ige3d/lecig3d.F
Chd|        LECINS                        source/interfaces/interf1/lecins.F
Chd|        LECINT                        source/interfaces/interf1/lecint.F
Chd|        LECLAS                        source/loads/laser/leclas.F   
Chd|        LECREFSTA                     source/loads/reference_state/refsta/lecrefsta.F
Chd|        LECSEC0                       source/tools/sect/hm_read_sect.F
Chd|        LECSEC42                      source/tools/sect/hm_read_sect.F
Chd|        LECSEC4BOLT                   source/tools/sect/lecsec4bolt.F
Chd|        LECSTACK_PLY                  source/properties/composite_options/stack/lecstack_ply.F
Chd|        LECSTAMP                      source/interfaces/interf1/lecstamp.F
Chd|        LECSUBMOD                     source/model/submodel/lecsubmod.F
Chd|        LECTRANS                      source/model/transformation/lectrans.F
Chd|        LECTRANSSUB                   source/model/submodel/lectranssub.F
Chd|        LEC_DDW                       source/spmd/domain_decomposition/grid2mat.F
Chd|        LEC_DDW_POIN                  source/spmd/domain_decomposition/grid2mat.F
Chd|        LEC_INISTATE                  source/elements/initia/lec_inistate.F
Chd|        LGMINI_I7                     source/tools/lagmul/lgmini_i7.F
Chd|        LGMINI_RWL                    source/tools/lagmul/lgmini_rwl.F
Chd|        LINE_DECOMP                   source/model/group/line_decomp.F
Chd|        M20DCOD                       source/system/fsdcod.F        
Chd|        MATPARAM_R2R_VOID             source/elements/elbuf_init/matparam_r2r_void.F
Chd|        MERGE                         source/model/submodel/merge.F 
Chd|        MERGE_NODE                    source/elements/nodes/merge_node.F
Chd|        MONVOL_ALLOCATE               share/modules1/monvol_struct_mod.F
Chd|        MONVOL_DEALLOCATE             share/modules1/monvol_struct_mod.F
Chd|        MULTI_CONNECTIVITY            source/multifluid/multi_connectivity.F
Chd|        MULTI_UNPLUG_NEIGHBORS        source/multifluid/multi_unplug_neighbors.F
Chd|        NBFUNCT                       source/tools/curve/nbfunc.F   
Chd|        NODM11                        source/materials/mat/mat011/nodm11.F
Chd|        OUTRI                         source/materials/time_step/outri.F
Chd|        OUTRIN                        source/materials/time_step/outri.F
Chd|        PAROI                         source/materials/mat/mat006/paroi.F
Chd|        PGRHEAD                       source/elements/beam/pgrhead.F
Chd|        PGRTAILS                      source/elements/beam/pgrtails.F
Chd|        PORNOD                        source/ale/pornod.F           
Chd|        PRECRKXFEM                    source/elements/xfem/precrkxfem.F
Chd|        PRELECIG3D                    source/elements/ige3d/prelecig3d.F
Chd|        PRELECSEC                     source/tools/sect/prelecsec.F 
Chd|        PRELECSEC4BOLT                source/tools/sect/prelecsec4bolt.F
Chd|        PRELEC_DDW                    source/spmd/domain_decomposition/grid2mat.F
Chd|        PRELEC_DDW_POIN               source/spmd/domain_decomposition/grid2mat.F
Chd|        PREPARE_INT25                 source/model/mesh/build_cnel.F
Chd|        PREPARE_SPLIT_I25E2E          source/spmd/prepare_split_i25e2e.F
Chd|        PREPLYXFEM                    source/properties/composite_options/stack/preplyxfem.F
Chd|        PRERAFIG3D                    source/elements/ige3d/prerafig3d.F
Chd|        PREREAD_RBODY_LAGMUL          source/constraints/general/rbody/preread_rbody_lagmul.F
Chd|        PREREAD_RBODY_SET             source/model/sets/preread_rbody_set.F
Chd|        PRESCRINT                     source/interfaces/interf1/prescrint.F
Chd|        PRESEARCHIGEO3D               source/elements/ige3d/searchigeo3d.F
Chd|        PRETAG_XFEM                   source/elements/xfem/pretag_xfem.F
Chd|        PRE_CNDPON                    source/elements/solid/solide10/dim_s10edg.F
Chd|        PRE_STACKGROUP                source/stack/pres_stackgroup.F
Chd|        PRINTBCS                      source/constraints/general/bcs/printbcs.F
Chd|        PRINTGROUP                    source/output/outfile/printgroup.F
Chd|        PRINTSTSZ                     source/spmd/node/st_array_size.F
Chd|        QGRHEAD                       source/elements/solid_2d/quad/qgrhead.F
Chd|        QGRTAILS                      source/elements/solid_2d/quad/qgrtails.F
Chd|        R2R_CHECK                     source/coupling/rad2rad/r2r_check.F
Chd|        R2R_CLEAN_INTER               source/coupling/rad2rad/r2r_clean_inter.F
Chd|        R2R_DOMDEC                    source/coupling/rad2rad/r2r_domdec.F
Chd|        R2R_GROUP                     source/coupling/rad2rad/r2r_group.F
Chd|        R2R_NOM_OPT                   source/coupling/rad2rad/routines_r2r.F
Chd|        R2R_SPEEDUP                   source/coupling/rad2rad/r2r_speedup.F
Chd|        R2R_SPLIT                     source/coupling/rad2rad/r2r_split.F
Chd|        R2R_VOID                      source/coupling/rad2rad/r2r_void.F
Chd|        RBE2MODIF1_ND                 source/elements/solid/solide10/dim_s10edg.F
Chd|        READ_DETONATORS               source/initial_conditions/detonation/read_detonators.F
Chd|        READ_EBCS                     source/boundary_conditions/ebcs/read_ebcs.F
Chd|        READ_ENGINE_DRIVER            source/general_controls/engine/read_engine_driver.F
Chd|        READ_MATERIAL_MODELS          source/materials/read_material_models.F
Chd|        READ_MONVOL                   source/airbag/read_monvol.F   
Chd|        READ_RWALL                    source/constraints/general/rwall/read_rwall.F
Chd|        RECONNECT                     source/elements/nodes/reconnect.F
Chd|        REINI_MATPROP                 source/spmd/domain_decomposition/grid2mat.F
Chd|        REINI_MATPROP2                source/spmd/domain_decomposition/grid2mat.F
Chd|        REORD_ICND                    source/elements/solid/solide10/dim_s10edg.F
Chd|        RGRHEAD                       source/elements/spring/rgrhead.F
Chd|        RGRTAILS                      source/elements/spring/rgrtails.F
Chd|        RIGID_MAT                     source/materials/mat/mat019/rigid_mat.F
Chd|        RIGMODIF1_ND                  source/elements/solid/solide10/dim_s10edg.F
Chd|        SCRINT                        source/interfaces/interf1/scrint.F
Chd|        SEARCHIGEO3D                  source/elements/ige3d/searchigeo3d.F
Chd|        SENSOR_USER_CONVERT_LOCAL_ID  source/tools/sensor/sensor_user_convert_local_id.F
Chd|        SENSOR_USER_INIT              source/tools/sensor/sensor_user_init.F
Chd|        SETELOFF                      source/constraints/general/rbody/hm_read_rbody.F
Chd|        SETELOFF2                     source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        SETLENWA                      source/restart/ddsplit/setlenwa.F
Chd|        SETMULTI                      source/restart/ddsplit/setlenwa.F
Chd|        SETRB2ON                      source/constraints/general/rbe2/hm_read_rbe2.F
Chd|        SETRBYON                      source/constraints/general/rbody/hm_read_rbody.F
Chd|        SET_ADMESH                    source/model/remesh/set_admesh.F
Chd|        SET_ELGROUP_PARAM             source/elements/shell/coque/set_elgroup_param.F
Chd|        SET_FRONT8                    source/spmd/node/ddtools.F    
Chd|        SET_IBUFSSG_IO                source/starter/lectur.F       
Chd|        SET_INTERCEP                  source/spmd/node/ddtools.F    
Chd|        SET_POIN_UMP                  source/system/set_poin_ump.F  
Chd|        SGRHEAD                       source/elements/solid/solide/sgrhead.F
Chd|        SGRTAILS                      source/elements/solid/solide/sgrtails.F
Chd|        SHELLTHK_UPD                  source/properties/composite_options/drape/shellthk_upd.F
Chd|        SHSUB                         source/elements/shell/subcycling/shsub.F
Chd|        SMS_INIT                      source/ams/sms_init.F         
Chd|        SMS_INI_JAD_1                 source/ams/sms_init.F         
Chd|        SMS_INI_JAD_2                 source/ams/sms_init.F         
Chd|        SMS_INI_JAD_3                 source/ams/sms_init.F         
Chd|        SMS_INI_KAD                   source/ams/sms_init.F         
Chd|        SMS_INI_KDI                   source/ams/sms_init.F         
Chd|        SORTGROUP                     source/system/nintrr.F        
Chd|        SORT_SURF                     source/groups/sort_surf.F     
Chd|        SPDOMETIS                     source/spmd/domain_decomposition/grid2mat.F
Chd|        SPGRHEAD                      source/elements/sph/spgrhead.F
Chd|        SPGRTAILS                     source/elements/sph/spgrtails.F
Chd|        SPHDCOD                       source/elements/sph/sphdcod.F 
Chd|        SPHONF0                       source/elements/sph/sphonf0.F 
Chd|        SPINIH                        source/elements/sph/spinih.F  
Chd|        SPLIT_CFD_SOLIDE              source/spmd/split_cfd_solide.F
Chd|        SPLIT_PCYL                    source/loads/general/load_pcyl/split_pcyl.F
Chd|        SPMD_ANIM_PLY_INIT            source/spmd/spmd_anim_ply_init.F
Chd|        SPTRI                         source/elements/sph/sptri.F   
Chd|        STACKGROUP                    source/stack/stackgroup.F     
Chd|        STACKGROUP_DRAPE              source/stack/stackgroup_drape.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STATE_ADMESH                  source/model/remesh/state_admesh.F
Chd|        STIFN0_ND                     source/elements/solid/solide10/dim_s10edg.F
Chd|        STIFN1_ND                     source/elements/solid/solide10/dim_s10edg.F
Chd|        STOPTIME                      source/system/timer.F         
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|        SUBSET_INI                    source/model/group/subset_ini.F
Chd|        SURFEXT_TAGN                  source/groups/ssurftag.F      
Chd|        T3GRHEAD                      source/elements/solid_2d/tria/t3grhead.F
Chd|        T3GRTAILS                     source/elements/solid_2d/tria/t3grtails.F
Chd|        TABLE_ZERO                    source/tools/curve/table_tools.F
Chd|        TET4_10                       source/starter/lectur.F       
Chd|        TGRHEAD                       source/elements/truss/tgrhead.F
Chd|        TGRTAILS                      source/elements/truss/tgrtails.F
Chd|        THPINIT                       source/output/th/thpinit.F    
Chd|        THSKEWC                       source/output/th/thskewc.F    
Chd|        TH_SURF_LOAD_PRESSURE         source/output/th/th_surf_load_pressure.F
Chd|        TITRE2                        source/output/outfile/titre2.F
Chd|        TITRE3                        source/output/outfile/titre3.F
Chd|        TRACE_IN1                     source/system/trace_back.F    
Chd|        TRACE_OUT1                    source/system/trace_back.F    
Chd|        TRIINTFRIC                    source/interfaces/interf1/trintfric.F
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|        UPGRADE_IXINT                 source/interfaces/interf1/upgrade_ixint.F
Chd|        XFEM_CRACK_INIT               source/elements/xfem/xfem_crack_init.F
Chd|        XGRHEAD                       source/elements/xelem/xgrhead.F
Chd|        XGRTAILS                      source/elements/xelem/xgrtails.F
Chd|        YCTRL                         source/initial_conditions/inista/yctrl.F
Chd|        SET_USRTOS                    source/model/sets/ipartm1.F   
Chd|        STRR                          source/tools/univ/strr.F      
Chd|        ALEFVM_MOD                    ../common_source/modules/ale/alefvm_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale/ale_connectivity_mod.F
Chd|        ALE_EBCS_MOD                  ../common_source/modules/ale/ale_ebcs_mod.F
Chd|        ALE_MOD                       ../common_source/modules/ale/ale_mod.F
Chd|        BPRELOAD_MOD                  share/modules1/bpreload_mod.F 
Chd|        CHECK_MOD                     ../common_source/modules/check_mod.F
Chd|        CLUSTER_MOD                   share/modules1/cluster_mod.F  
Chd|        DDWEIGHTS_MOD                 share/modules1/ddweights_mod.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|        DSGRAPH_MOD                   share/modules1/dsgraph_mod.F  
Chd|        EBCS_MOD                      ../common_source/modules/boundary_conditions/ebcs_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/mat_elem/elbufdef_mod.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        FUNC2D_MOD                    share/modules1/func2d_mod.F   
Chd|        FVBAG_MOD                     share/modules1/fvbag_mod.F    
Chd|        FVMBAG_MESHCONTROL_MOD        ../common_source/modules/airbag/fvmbag_meshcontrol_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        GROUP_MOD                     share/modules1/group_mod.F    
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        INIGRAV                       share/modules1/inigrav_mod.F  
Chd|        INIMAP1D_MOD                  share/modules1/inimap1d_mod.F 
Chd|        INIMAP2D_MOD                  share/modules1/inimap2d_mod.F 
Chd|        INIVOL_ARRAY_MOD              share/modules1/inivol_mod.F   
Chd|        INIVOL_DEF_MOD                share/modules1/inivol_mod.F   
Chd|        INIVOL_INTERF_MOD             share/modules1/inivol_mod.F   
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        INT8_MOD                      ../common_source/modules/interfaces/int8_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUFMOD                     share/modules1/restart_mod.F  
Chd|        INTBUFSCRATCH_MOD             source/interfaces/interf1/intbufscratch_mod.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        INTERFACES_MOD                ../common_source/modules/interfaces/interfaces_mod.F
Chd|        INTSTAMP_GLOB_MOD             share/modules1/intstamp_glob_mod.F
Chd|        INVERTED_GROUP_MOD            share/modules1/inverted_group_mod.F
Chd|        IXINTMOD                      share/modules1/restart_mod.F  
Chd|        JOINT_MOD                     share/modules1/joint_mod.F    
Chd|        LOADS_MOD                     ../common_source/modules/loads/loads_mod.F
Chd|        MAPPING_OPTION_MOD            share/modules1/dichotomy_mod.F
Chd|        MAT_ELEM_MOD                  ../common_source/modules/mat_elem/mat_elem_mod.F
Chd|        MESHSURFIG3D_MOD              source/elements/ige3d/meshsurfig3d_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MID_PID_MOD                   share/modules1/mid_pid_mod.F  
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/ale/multi_fvm_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        NOD2EL_MOD                    share/modules1/nod2el_mod.F   
Chd|        OPTIONDEF_MOD                 ../common_source/modules/optiondef_mod.F
Chd|        OPTION_MOD                    share/modules1/options_mod.F  
Chd|        OUTPUT_MOD                    ../common_source/modules/output/output_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        RANDOM_MOD                    share/modules1/random_mod.F   
Chd|        REORDER_MOD                   share/modules1/reorder_mod.F  
Chd|        RESTMOD                       share/modules1/restart_mod.F  
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SETDEF_MOD                    ../common_source/modules/setdef_mod.F
Chd|        SET_MOD                       share/modules1/set_mod.F      
Chd|        SKEW_MOD                      share/modules1/skew_mod.F     
Chd|        SMS_MOD                       share/modules1/sms_mod.F      
Chd|        SPH_MOD                       share/modules1/sph_mod.F      
Chd|        SPLIT_CFD_MOD                 share/modules1/split_cfd_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|        STACK_VAR_MOD                 share/modules1/stack_var_mod.F
Chd|        STATE_MOD                     ../common_source/modules/state_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_GLOB_MOD                share/modules1/table_glob_mod.F
Chd|        USER_SENSOR_MOD               share/modules1/user_interface_mod.F
Chd|        USER_WINDOWS_MOD              ../common_source/modules/user_windows_mod.F
Chd|        XFEM2DEF_MOD                  ../common_source/modules/xfem2def_mod.F
Chd|====================================================================
      SUBROUTINE LECTUR(MULTI_FVM                   ,LSUBMODEL          ,IS_DYNA         ,DETONATORS   ,EBCS_TAB,
     .                  SEATBELT_CONVERTED_ELEMENTS ,NB_SEATBELT_SHELLS ,NB_DYNA_INCLUDE ,USER_WINDOWS ,OUTPUT  ,
     .                  MAT_ELEM)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE DSGRAPH_MOD
      USE FVBAG_MOD
      USE RESTMOD
      USE INTBUFMOD
      USE IXINTMOD
      USE NOD2EL_MOD
      USE INTSTAMP_GLOB_MOD
      USE SUBMODEL_MOD
      USE SMS_MOD
      USE TABLE_GLOB_MOD
      USE R2R_MOD
      USE ELBUFDEF_MOD
      USE ELBUFTAG_MOD
      USE MESSAGE_MOD
      USE FRONT_MOD
      USE SPH_MOD
      USE CLUSTER_MOD
      USE INTBUFDEF_MOD
      USE INTBUFSCRATCH_MOD
      USE DDWEIGHTS_MOD
      USE XFEM2DEF_MOD
      USE STACK_MOD
      USE ALEFVM_MOD
      USE INT8_MOD
      USE FVMBAG_MESHCONTROL_MOD
      USE MULTI_FVM_MOD
      USE STACK_VAR_MOD
      USE BPRELOAD_MOD
      USE REORDER_MOD
      USE INIGRAV
      USE INTBUF_FRIC_MOD
      USE INIMAP1D_MOD
      USE INIMAP2D_MOD
      USE FUNC2D_MOD
      USE GROUPDEF_MOD
      USE GROUP_MOD
      USE OPTIONDEF_MOD
      USE OPTION_MOD
      USE OPTIONDEF_MOD
      USE MID_PID_MOD
      USE FAILWAVE_MOD
      USE NLOCAL_REG_MOD
      USE SKEW_MOD
      USE MESHSURFIG3D_MOD
      USE MAT_ELEM_MOD
      USE SPLIT_CFD_MOD
      USE PINCHTYPE_MOD
      USE CHECK_MOD
      USE INOUTFILE_MOD
      USE MONVOL_STRUCT_MOD
      USE SETDEF_MOD
      USE SET_MOD
      USE DETONATORS_MOD
      USE ALE_CONNECTIVITY_MOD
      USE DRAPE_MOD
      USE INIVOL_DEF_MOD
      USE INIVOL_ARRAY_MOD
      USE INIVOL_INTERF_MOD
      USE SENSOR_MOD
      USE RANDOM_MOD
      USE ALE_EBCS_MOD
      USE INVERTED_GROUP_MOD
      USE MAPPING_OPTION_MOD
      USE EBCS_MOD
      USE JOINT_MOD
      USE HM_OPTION_READ_MOD
      USE SEATBELT_MOD
      USE LOADS_MOD
      USE STATE_MOD
      USE USER_WINDOWS_MOD
      USE USER_SENSOR_MOD
      USE ALE_MOD
      USE OUTPUT_MOD
      USE INTERFACES_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(MULTI_FVM_STRUCT)             :: MULTI_FVM
      TYPE(SUBMODEL_DATA)                :: LSUBMODEL(NSUBMOD)
      INTEGER,INTENT(IN)                 ::IS_DYNA
      TYPE(DETONATOR_STRUCT_),TARGET     :: DETONATORS
      TYPE(T_EBCS_TAB), INTENT(INOUT)    :: EBCS_TAB
      INTEGER,INTENT(IN)                 ::NB_SEATBELT_SHELLS
      INTEGER,INTENT(INOUT)              ::SEATBELT_CONVERTED_ELEMENTS(3,NB_SEATBELT_SHELLS)
      INTEGER,INTENT(IN)                 ::NB_DYNA_INCLUDE
      TYPE(USER_WINDOWS_), INTENT(INOUT) :: USER_WINDOWS
      TYPE(OUTPUT_),  INTENT(INOUT)      :: OUTPUT
c      TYPE(SENSORS_), INTENT(INOUT)      :: SENSORS! declared as global in USER_SENSOR_MOD
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "r4r8_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "hash_id.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com06_c.inc"
#include      "com09_c.inc"
#include      "com10_c.inc"
#include      "com_engcards_c.inc"
#include      "com_xfem1.inc"
#include      "eigcom.inc"
#include      "flowcom.inc"
#include      "fxbcom.inc"
#include      "intstamp_c.inc"
#include      "lagmult.inc"
#include      "sphcom.inc"
#include      "param_c.inc"
#include      "remesh_c.inc"
#include      "tabsiz_c.inc"
#include      "tablen_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr06_c.inc"
#include      "scr10_c.inc"
#include      "scr12_c.inc" 
#include      "scr14_c.inc"
#include      "scr15_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "scr19_c.inc"
#include      "scr21_c.inc"
#include      "scr23_c.inc"
#include      "scry_c.inc"
#include      "sms_c.inc"
#include      "spmd_c.inc"
#include      "ddspmd_c.inc"
#include      "sysunit.inc"
#include      "titr_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "submod_c.inc"
#include      "r2r_c.inc"
#include      "intread_c.inc"
#include      "elbuf_c.inc"
#include      "userlib.inc"
#include      "drape_c.inc"
#include      "mmale51_c.inc"
#include      "thermal_c.inc"
#include      "boltpr_c.inc"
#include      "inigrav_c.inc"
#include      "inter18.inc"
#include      "inter22.inc"
#include      "eos.inc"
#include      "ige3d_c.inc"
#include      "random_c.inc"
#include       "my_allocate.inc"
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
      INTEGER  NLOCAL
      EXTERNAL NLOCAL
      INTEGER  SET_USRTOS
      EXTERNAL SET_USRTOS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      TYPE INTERMASURFEP
        INTEGER, DIMENSION(:), POINTER :: P
      END TYPE INTERMASURFEP

      INTEGER II,I,J,KK,N, NPTS, NMNT, NRTMT_25,MLW,
     .   NUMEL, IFIP, IS_EULER,NB_EULER_GROUPS,
     .   K5B,NAIRWA,NTHWA, LWASPIO, LEN_G,LEN_M,LEN,
     .   IADBUF, IADGEO, NUVAR, NUVARI,
     .   NS_I7,NEL,ND,AUX,NS_I21,
     .   NPT,NS_I11,
     .   SVOLMON0,FLAG_GOTO,
     .   INNOD,INSEG,NSIGI, NSIGS, NSIGSH, NSIGSPH,
     .   LB_MAX, P, NG,
     .   IINU ,NEL3D,NEL2D,NEL1D,IMAX,JMAX,
     .   OFF, NELEM, IDDLEVEL, NELEMINT,
     .   IFIXIN,IFIEND,ICO,
     .   IDS,IUN,L_MUL_LAG,NCMAX,NKMAX,
     .   MAXRTMS,MAXNSNE,
     .   MAXRTM,LWAT, L_MUL_LAG1,ISHIF,LIBAGALE,
     .   LENTHG, LBUFMAT, LBUFGEO, LBUFSF,
     .   LNOM_OPT, LENVOLU, ILEN, LCNE, LCNI2G,LENPOR,
     .   PM1SHF, NFX, AIPM, ANOD, AMOD, NBNO, NBMO,
     .   ALM, NELS, NELC, NELTG, NLGRAV, AGRVI, AGRVR,
     .   NNT, RCLEN,PM1SPH, STAT, NELDMAX, VERSDD,
     .   DSNISM, NSLEVEL,NSDEC,NSVMAX,NSPRI,DSARCH,NELT,NELP,NSEGS,
     .   NNFT, NDOFMIN, NMANIM, DSANIM,NRCVVOIS0,NSIGRS,
     .   LRBAGALE,FLAGG,ICOUNT,SWAFT, SWA4, SMATER, SEL2FA,
     .   SNFACPTX,SIXEDGE,SOFFX1,SNUMX1,SXNORM,SINVERT,SFUNC1,SIAD,
     .   SMAS,LEN_RM,LAG_NCF0,LAG_NKF0,
     .   LAG_NHF0,LAG_NCL0,LAG_NKL0,MAXNNOD, IBID,
     .   SRTRANS,LCNE_CRKXFEM,NSEGSMAX,XFEMON,
     .   LXINTD2,IN10,IN20,SNOM_OPT_OLD,LENI,FLAG_ALLOCATE,
     .   PROC_BID,FLG_R2R_ERR,NSPCOND0,LENTHGR,FLAG_XFEM,
     .   IADTABIGE,NDOUBLONIGE,DECALIGEO,HM_NSENSOR,
     .   IPARSENS,NBT8,
     .   TAB_SOL(6),ISTR_24,IDEL_SOLID,
     .   LCNCND,I24MAXNSNE,NSIGBEAM,NSIGTRUSS,S_LOADPINTER,
     .   FLAGF,ITHFLAG,MAXRTM_T2,NS_I2,SITAGE,NCTRLMAX,INLIN,SVR_1
      INTEGER (KIND=8) EMAX
      INTEGER (KIND=8) K0,K1,K2,K3,K4,K5,K6,K7,K8
! integer 8 version of the variables NUMELC etc.
      INTEGER(KIND=8) NUMELCK8
      INTEGER(KIND=8) NUMELTGK8
      INTEGER(KIND=8) NUMELSK8
      INTEGER(KIND=8) NUMELRK8
      INTEGER(KIND=8) NUMELPK8
      INTEGER(KIND=8) NUMELTK8
      INTEGER(KIND=8) NUMELQK8
      INTEGER(KIND=8) NUMELXK8
      INTEGER(KIND=8) NUMELIG3DK8
      INTEGER(KIND=8) NUMSPHK8
      INTEGER(KIND=8) SVEUL8

      INTEGER
     .   ISUBMOD(NSUBMOD),
     .   IUPARAM(100),DDSTAT(50,PARASIZ),IGRNRB2(NRBE2),
     .   LCNE_PXFEM
      INTEGER NSNT,NMNT_2

      INTEGER, DIMENSION(:), ALLOCATABLE :: POIN_UMP_OLD
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TAB_UMP_OLD
      my_real, DIMENSION(:), ALLOCATABLE :: CPUTIME_MP_OLD

      INTEGER, DIMENSION(:,:), ALLOCATABLE :: POIN_PART_SHELL,POIN_PART_TRI
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: POIN_PART_SOL
      TYPE(MID_PID_TYPE), DIMENSION(:), ALLOCATABLE :: MID_PID_SHELL,MID_PID_TRI
      TYPE(MID_PID_TYPE), DIMENSION(:,:), ALLOCATABLE :: MID_PID_SOL
      LOGICAL MARQUEUR3

      INTEGER, DIMENSION(:),ALLOCATABLE ::
     *        IWCONT, IWCIN2      , IKINE1LAG ,DSDOF, TAGXREF,
     *        ADDCNE, ADDCNE_PXFEM, FXBTAG    ,ADDCNE_CRKXFEM,
     *        TAGREFSTA,CSRECT

      INTEGER, DIMENSION(:),ALLOCATABLE ::
     *        ISOLNOD,ISOLOFF,ISHEOFF,ITRUOFF,IPOUOFF,
     *        IRESOFF,ITRIOFF,IGRNRBY,IQUAOFF
C
      INTEGER(KIND=8) :: KVOISPH8,NUMSPH8,SIXSP8,LIMIT8
      INTEGER :: INTEGER_LIMIT32

      INTEGER, DIMENSION(:),ALLOCATABLE :: CEP,CEL,CNE,
     .                                     CNI2, CELI2, CEPI2,
     .                                     CEPSP, CELSPH, ITAGSH,
     .                                     CNE_PXFEM,CEL_PXFEM
      INTEGER, DIMENSION(:), ALLOCATABLE :: FXBIPM, FXBNOD, FXBELM,
     .                                      FXBGRVI, EIGIPM, EIGIBUF,
     .                                      IMERGE,INTIDS,
     .                                      IMERGE2,IADMERGE2,
     .                                      NSLNRBM,  SLNRBM,
     .                                      IGEO_STACK
      INTEGER, DIMENSION(:,:,:),  ALLOCATABLE :: ELDOM
      INTEGER, DIMENSION(:),      ALLOCATABLE :: CEPTMP, NELDOM
      INTEGER, DIMENSION(:),      ALLOCATABLE ::  LLL
      INTEGER, DIMENSION(:,:),    ALLOCATABLE :: ELSUB
      INTEGER(KIND=8) ,TARGET :: DSMEMORY(7,NSPMD)
      INTEGER, DIMENSION(:,:),   ALLOCATABLE :: FXANI,IWORKSH, FXB_MATRIX_ADD
      INTEGER, DIMENSION(:),     ALLOCATABLE :: FASTAG,SEGTAG
      INTEGER(KIND=8),TARGET :: MEMFLOW(2,NSPMD)
      INTEGER, DIMENSION(:),     ALLOCATABLE :: IFLOW
C
      INTEGER, DIMENSION(:),     ALLOCATABLE :: KINWORK

      INTEGER, DIMENSION(:),     ALLOCATABLE :: CNE_CRKXFEM,CEL_CRKXFEM,ITAGN,ITAGE,CEP_CRKXFEM,IEDGE_TMP0,CRKNODIAD
      INTEGER, DIMENSION(:,:),   ALLOCATABLE :: IEDGE_TMP
C
      INTEGER, DIMENSION(:),     ALLOCATABLE :: NALE_R2R
      INTEGER, DIMENSION(:,:),   ALLOCATABLE :: FRONTB_R2R
c elem sorting
      INTEGER, DIMENSION(:), ALLOCATABLE :: IXS_S ,IXS_S_IND,
     2     IXQ_S,IXQ_S_IND,IXC_S,IXC_S_IND,
     3     IXT_S ,IXT_S_IND,IXP_S,IXP_S_IND,IXR_S,IXR_S_IND,
     4     IXTG_S,IXTG_S_IND

c tab IBUFSSG_IO specific inlet/outlet
      INTEGER, DIMENSION(:), ALLOCATABLE :: IBUFSSG_IO, RESERVEP

      INTEGER, DIMENSION(:), ALLOCATABLE :: IXR_KJ,R_SKEW
C
C Sol2sph
      INTEGER, DIMENSION(:), ALLOCATABLE :: SOL2SPH_TYP
c tab /BCS/CYCLIC
      INTEGER, DIMENSION(:), ALLOCATABLE :: IBCSCYC,LBCSCYC,ITAGCYC
C
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: QP_IPERTURB,RBY_MSN
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: QP_RPERTURB,RBY_INIAXIS
C
      my_real
     .  EANIT2(10),COST_R2R,TOTMAS
      TARGET :: EANIT2
C OpenMP specific
       INTEGER ITASK, NP
#if defined(_OPENMP)
       INTEGER OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS
       EXTERNAL OMP_GET_THREAD_NUM, OMP_GET_NUM_THREADS
#endif

      my_real
     .       , DIMENSION(:), ALLOCATABLE ::
     .   FXBRPM, FXBMOD, FXBGLM, FXBCPM, FXBCPS, FXBLM,
     .   FXBFLS, FXBDLS, FXBDEP, FXBVIT, FXBACC, FXBSIG,
     .   FXBGRVR, EIGRPM ,RMSTIFN, RMSTIFR,STIFFN,
     .   MS_PLY0, ZI_PLY0,MSZ20,MSZ2,LELX,FXB_MATRIX
      my_real
     .       , DIMENSION(:,:), ALLOCATABLE :: MBUFEL, MDEPL,RNOISE
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: RFLOW,CMERGE,DNULL
      my_real
     .   PROBINT, FLREC6(6), DSCUTFRQ
      my_real, DIMENSION(:), ALLOCATABLE ::
     .   XFILTR,STFAC,FRIC_P,FRIGAP,I2RUPT,AREASL,THK_PART,
     .   GEO_STACK

      my_real, DIMENSION(:,:,:), ALLOCATABLE :: XREFC,XREFTG,XREFS
      my_real, DIMENSION(:), ALLOCATABLE :: XYZREF

      my_real, DIMENSION(:), ALLOCATABLE :: DT_R2R
      TYPE(INTERSURFP)   , DIMENSION(:,:), ALLOCATABLE :: INTERCEP
      my_real DTNODA
      my_real, DIMENSION(:), ALLOCATABLE :: SH4ANG,SH3ANG
      my_real, DIMENSION(:), ALLOCATABLE :: MS_B,IN_B,DTELEM
C
      TYPE (STACK_PLY)  :: STACK
C
      INTEGER, DIMENSION(:), ALLOCATABLE :: IDRAPEID,PERTURB
      TYPE (FVM_INIVEL_STRUCT), DIMENSION(:), ALLOCATABLE :: FVM_INIVEL
      TYPE (FAILWAVE_STR_) :: FAILWAVE
      TYPE (NLOCAL_STR_)   :: NLOC_DMG
      TYPE (PINCH)         :: PINCH_DATA
      TYPE (DRAPE_)   ,DIMENSION(:), ALLOCATABLE   :: DRAPE,DRAPE_WRK
      TYPE (DRAPEG_)                               :: DRAPEG
      TYPE(DRAPE_WORK_) ,DIMENSION(:), ALLOCATABLE   :: IWORK_T
      INTEGER :: EBCS_TAG_CELL_SPMD(NUMELQ+NUMELTG+NUMELS)
C
      TYPE (DYNAIN_DATABASE)   :: DYNAIN_DATA
      TYPE (INTERFACES_)       :: INTERFACES
      TYPE (MAT_ELEM_) ,INTENT(INOUT) :: MAT_ELEM


C-----------------------------------------------
      CHARACTER(LEN=4096) :: SCR_FILE_NAME
      CHARACTER(LEN=ncharline) :: RLINE
      CHARACTER (LEN=4) :: CWIN
      LOGICAL :: IS_AVAILABLE
      INTEGER NLINES,NUSERWI,USERWI_ID
      INTEGER SCR_FILE_NAME_LEN
      CHARACTER(LEN=ncharkey) :: KEY
      CHARACTER TITLE*nchartitle
C-----------------------------------------------
C Model Checker Memory
      DATA IUN/1/
C======================================================================|
C   Allocations MA (Entiers)
C======================================================================|
      TYPE :: int_ptr_array
        INTEGER, DIMENSION(:), POINTER :: ptr
      END TYPE int_ptr_array
      TYPE(int_ptr_array) :: IBUFTMP(0:1),NIGE_TMP(0:1)
      TYPE :: real_ptr_array
      my_real, DIMENSION(:), POINTER :: ptr2
      END TYPE real_ptr_array
      TYPE(real_ptr_array) :: RIGE_TMP(0:1),XIGE_TMP(0:1),
     .                        VIGE_TMP(0:1)
      INTEGER, DIMENSION(:), ALLOCATABLE ::
     .    FR_IAD,FUNCRYPT,
     .    IWORK,ITRI,KSYSUSR,PTSHEL,PTSH3N,PTSOL,PTQUAD,
     .    PTSPH,ISPTAG,DD_TMP,ITAG,ITAGND_SHXFEM,
     .    ITHPART,ITHSUB,ITHBUFTMP,DD_TMP2,
     .    IADHF,JCIHF,JLL,IWA,WEIGHT_RM,
     .    PTSPRI,PTBEAM,PTTRUSS
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: IPARGTMP
      TARGET :: IWORK

      INTEGER, DIMENSION(:,:), ALLOCATABLE :: TAB_UMP_LOC
      INTEGER, DIMENSION(:,:,:), ALLOCATABLE :: TAB_UMP_LOC2
      INTEGER, DIMENSION(:), POINTER ::
     .    IPARTTH,IPARTS,IPARTQ,IPARTC,IPARTT,IPARTP,IPARTR,
     .    IPARTG,IPARTX,IPARTSP,NPC1,IXS10,IXS20,IXS16,IPRES,
     .    IBMPC2,IBMPC3,IBMPC4,IWORK2,
     .    ITRI1,ITRI2,ITRI3,INDEX,
     .    INDEX1,INDEX2,IWD,IWEIG,INUM,EADD,ITR1,ITR2,XEP,
     .    IPARTTHI,IPARTIG3D,IEDGESH4,IEDGESH3,IELCRK4,IELCRK3
      INTEGER BID13(1)
      INTEGER L0,L1,L2,L3,L4,L5,L6,L7,L8,L9,L10,L11,L12,SIXTG0,
     .    SIXS0,SIXS10,SIXS20,SIXS16,SIWORK,SIWORK2,SIEXTAG,
     .    NUMCLD,NUMPRES,NUMLINK,NUMGRAV,NUMRBYMOU,
     .    SNRBODY,SLRBODY,LITHPART,LITHSUB,IDX,IDX1,IDX2,IDXCNT,
     .    LITHBUFI,LITHBUFMX,NTHGRPMX,SINDEX,SITRI,LDD_IAD,
     .    LSIGI,LSIGSH,LSIGSP,LSIGSPH,SINSCR,
     .    SIPART0,SIPARTTH,SIPARTS,SIPARTQ,SIPARTC,SIPARTT,SIPARTP,
     .    SIPARTR,SIPARTG,SIPARTX,SIPARTSP,ITER,
     .    LIXINT,SBUFALE,NVARTOT,
     .    NVARTOT0,NVARTOTMAX,NVARABF,
     .    PIXS10,PIXS16,PIXS20,NUMCFIELD,SIXINT,NUMLOADP,IXEL,
     .    SIPARTIG3D,IDXIGE1,IDXIGE2,
     .    IDXIGECNT,TAGSURFIGE,LSIGRS,LSIGBEAM,LSIGTRUSS,NSETFRICTOT,
     .    IORTHFRICMAX,COEFSLEN,NPFRICORTH,NGRPF,LENG,
     .    NIMPDISP,NIMPVEL,NIMPACC,NIMPV_LAGM,NFV0,NSETMAX,NFXVEL0
      TYPE(CLUSTER_),DIMENSION(:),ALLOCATABLE :: CLUSTERS
      TYPE(INTBUF_STRUCT_),DIMENSION(:),ALLOCATABLE :: INTBUF_TAB
      TYPE(SCRATCH_STRUCT_),DIMENSION(:),ALLOCATABLE :: INSCR
C---  Bolt preloading
      INTEGER SIPRELOAD, SPRELOAD   !NUMPRELOAD,

c---  Element Buffer  --------------------------------------------------
      TYPE(ELBUF_STRUCT_),DIMENSION(:)  ,ALLOCATABLE :: ELBUF_TAB
      TYPE(ELBUF_STRUCT_),DIMENSION(:,:),ALLOCATABLE :: XFEM_TAB
      TYPE(MLAW_TAG_)    ,DIMENSION(:)  ,ALLOCATABLE, TARGET :: MTAG_INI,MTAG_R2R
      TYPE(MLAW_TAG_)    ,DIMENSION(:)  ,POINTER     :: MLAW_TAG
      TYPE(EOS_TAG_)     ,DIMENSION(:)  ,ALLOCATABLE :: EOS_TAG
      TYPE(PROP_TAG_)    ,DIMENSION(0:MAXPROP)       :: PROP_TAG
      TYPE(FAIL_TAG_)    ,DIMENSION(0:MAXFAIL)       :: FAIL_TAG
c---- Xfem ------------------------------------------------------------
      TYPE (XFEM_SHELL_) ,DIMENSION(:), ALLOCATABLE :: CRKSHELL     ! NLEVMAX
      TYPE (XFEM_LVSET_) ,DIMENSION(:), ALLOCATABLE :: CRKLVSET     ! NLEVMAX
      TYPE (XFEM_SKY_)   ,DIMENSION(:), ALLOCATABLE :: CRKSKY       ! NLEVMAX
      TYPE (XFEM_AVX_)   ,DIMENSION(:), ALLOCATABLE :: CRKAVX       ! NLEVMAX
      TYPE (XFEM_EDGE_)  ,DIMENSION(:), ALLOCATABLE :: CRKEDGE      ! NXLAYMAX
      TYPE(XFEM_PHANTOM_),DIMENSION(:), ALLOCATABLE :: XFEM_PHANTOM ! NXLAYMAX
C     NCRKPART & IND_CRK are global values shared by all processors (only for ANIM)
C ---- Interface t8
      TYPE(INT8_STRUCT_)      , DIMENSION(:,:), ALLOCATABLE :: INTERT8
      INTEGER  NCRKPART
      INTEGER, DIMENSION(:), ALLOCATABLE :: INDX_CRK
      INTEGER, DIMENSION(:), ALLOCATABLE :: PERMIGE
      TYPE(TABCONPATCH_IG3D_), DIMENSION(:), ALLOCATABLE :: TABCONPATCH
c---  Material data  --------------------------------------------------
      TYPE(MATPARAM_STRUCT_) , DIMENSION(:), ALLOCATABLE , TARGET :: MPARAM_INI,MPARAM_R2R
c---  Element group parameter table  --------------------------------------------------
      TYPE(GROUP_PARAM_) , DIMENSION(:), ALLOCATABLE :: GROUP_PARAM_TAB   ! NGROUP
C AMS
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: T2MAIN_SMS
C
      INTEGER :: SRNOISE1,SRNOISE2
C T2 SPT 27/28
      INTEGER  NSN_MULTI_CONNEC
      INTEGER, ALLOCATABLE, DIMENSION(:) :: T2_NB_CONNEC
C MERGE RBODY
      INTEGER, DIMENSION(:), ALLOCATABLE :: MGRBY
C PINCHING
      INTEGER SPINCH
C For /H3D/STRESS/TENS/OUTER
      INTEGER, ALLOCATABLE, DIMENSION(:) :: TAG_SKINS6
      INTEGER, ALLOCATABLE, DIMENSION(:,:) :: SEATBELT_SHELL_TO_SPRING
C======================================================================|
C   Allocations AM (Reels)
C======================================================================|
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: DBRWORK
      my_real, DIMENSION(:), ALLOCATABLE ::
     .    RWORK,STIFINT,MWA,MSS,MSSX,MSSF,MSQ,MSR,
     .    INP,INR,INS,VNS,VNSX,STC,STT,STP,STR,STTG,STUR,
     .    BNS,BNSX,VOLNOD,BVOLNOD,ETNOD,NSHNOD,XELEMWA,
     .    XNUM,XTMP,RTRANS,MSIG3D,STIFINTR,STRC,STRR,STRP,STRTG,
     .    VNIGE,BNIGE
      TARGET ::
     .    RWORK
      my_real, DIMENSION(:), POINTER ::
     .    THKEC,EANIT,PRES,WMA
      INTEGER SRWORK
      INTEGER LXINTD
      my_real TOTADDMAS
      CHARACTER*ncharline ERR_MSG
      INTEGER, DIMENSION(:), ALLOCATABLE :: IBORDEDGE,INOM_OPT
      DOUBLE PRECISION RSIBUFSSG,RNIGE,RRIGE,RXIGE,RVIGE
      INTEGER INTMAX, LLINAL,ITET4_10
C---------      Itet=2 of S10
      INTEGER, DIMENSION(:), ALLOCATABLE :: ICNDS10,ITAGND,ADDCNCND,
     .                                      CNCND, CELCND, CEPCND

      !Pointer to send a valid explicit address as argument in cas of not allocated
      INTEGER(KIND=8)   ,POINTER :: pMEMFLOW

      DATA INTMAX /2147483647/
      my_real,
     .         DIMENSION(:), ALLOCATABLE :: FILLSOL
C
C Dynamical User Library
      CHARACTER FILNAM*512,CLAW*4
      INTEGER LEN_FILNAM
      INTEGER IADBOXMAX
      INTEGER, DIMENSION(:), ALLOCATABLE :: IADBOXMAX_NODE,IADBOXMAX_SURF,
     .   IADBOXMAX_LINE,IADBOXMAX_ELEM
C
      my_real, DIMENSION(:,:), ALLOCATABLE ::
     .    SIGI,SIGSH,SIGSP,SIGSPH,SIGRS,SIGBEAM,SIGTRUSS
      INTEGER, DIMENSION(:), ALLOCATABLE ::
     .    STRSGLOB,STRAGLOB,ORTHOGLOB
      INTEGER  ISIGSH,IYLDINI,KSIGSH3,FAIL_INI(5),IUSOLYLD,IUSERL
C
      INTEGER FVMAIN(NVOLU + NMONVOL),NBSUBMAT
C GROUPS OF GROUPS
      INTEGER :: MEM_MARGIN
      PARAMETER (MEM_MARGIN = 250000)
C--- INterface Friction model
      TYPE(INTBUF_FRIC_STRUCT_), DIMENSION(:), ALLOCATABLE ::  INTBUF_FRIC_TAB
      INTEGER, DIMENSION(:), ALLOCATABLE ::TABCOUPLEPARTS_FRIC_TMP,TABPARTS_FRIC_TMP,
     .                                     TAGPRT_FRIC,NSETINIT,IFRICORTH_TMP,
     .                                      PFRICORTH ,IREPFORTH ,LENGRPF
      my_real, DIMENSION(:), ALLOCATABLE ::TABCOEF_FRIC_TMP , VFORTH ,PHIFORTH
      TYPE(INIMAP1D_STRUCT), DIMENSION(:), ALLOCATABLE :: INIMAP1D
      TYPE(INIMAP2D_STRUCT), DIMENSION(:), ALLOCATABLE :: INIMAP2D
      TYPE(FUNC2D_STRUCT), DIMENSION(:), ALLOCATABLE :: FUNC2D
!       DDSPLIT local arrays :
      LOGICAL :: FLAG_24_25
      INTEGER :: NINDX_NM,NINDX_SCRT,I24MAXNSNE2
      INTEGER, DIMENSION(NSPMD) :: NUMNOD_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_NM,INDX_NM
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_SCRATCH,INDX_SCRT
C FLEXIBLE BODY
      INTEGER  FXB_LAST_ADRESS(10)
      CHARACTER, DIMENSION(:), ALLOCATABLE :: FXBFILE_TAB*2148
C
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       INDX_XXX : size = NUMNOD
!                 index of non-zero TAG_XXX value
!                 used for optimize the initialization
!                 of TAG_XXX array (XXX = NM or SCRT for SCRATCH)
!                 allocated array in lectur and threadprivate array
!       NINDX_XXX : number of non-zero TAG_XXX value
!       TAG_XXX : size = NUMNOD
!                array used to tag an element for
!                a given interface ; allocated in lectur
!                allocated array in lectur and threadprivate array
!       FLAG_24_25 : logical, flag for interface 24 or 25
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
      INTEGER, DIMENSION(:), ALLOCATABLE :: TAG_SKN
      TYPE(SKEW_TYPE), DIMENSION(:), ALLOCATABLE :: MULTIPLE_SKEW
      TYPE (SENSOR_STR_) ,DIMENSION(:) ,ALLOCATABLE :: SENSOR_TMP
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       TAG_SKN : integer ; dimension=NUMSKW+1
!                 tag array --> tag the i SKEW if a SPRING uses it
!                 tag array=0 --> the SKEW is not used by a SPRING
!                 tag array=1 --> the SKEW is used by one SPRING
!                 tag array>1 --> the SKEW is used by several SPRING
!                 tag array <0 --> the SKEW is used by several options (has to be duplicated to all domains that have the nodes)
!       MULTIPLE_SKEW : SKEW_TYPE ; dimension=NUMSKW+1
!                       MULTIPLE_SKEW(I)%PLIST(:) is a list of processor
!                       where the SKEW is stuck
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
        TYPE(SURF_), DIMENSION(:,:), ALLOCATABLE :: IGRSURF_PROC
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       IGRSURF_PROC : SURF_ ; dimension=NSURF*NSPMD
!                 local surface property array (=IGRSURF for each proc)
!                 %ELTYP --> type of element (shell, triangle...)
!                 %ELEM  --> element id
!                 %NSEG --> total element number
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
        INTEGER :: GRNOD_UID
        INTEGER, DIMENSION(NSPMD) :: SIZE_ALE_ELM
        TYPE(split_cfd_type), DIMENSION(:),ALLOCATABLE :: ALE_ELM
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       SIZE_ALE_ELM : integer ; dimension=NSPMD ; size of ALE_ELM%SOL_ID array
!       ALE_ELM : split_cfd_type ; dimension=NSPMD ; solid element ID used
!                 during the domain splitting (ALE part)
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
        INTEGER :: LEN_TMP_NAME
        CHARACTER(len=4096) :: TMP_NAME
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       LEN_TMP_NAME : integer ; size of TMP_NAME
!       TMP_NAME : character ; local name of file, when -outfile or
!                  -infile cdl are used, need to define the folder paths
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
        TYPE(MONVOL_STRUCT_), DIMENSION(:), ALLOCATABLE :: T_MONVOL
        TYPE(MONVOL_METADATA_) :: T_MONVOL_METADATA
        TYPE(t_ale_connectivity) :: ALE_CONNECTIVITY

        INTEGER :: NBR_TH_MONVOL,NBR_TH_MONVOL01(9)! number of /TH/MONV
c For /RANDOM --------------------------------------------------
      INTEGER,DIMENSION(:),ALLOCATABLE :: IRAND
      my_real,DIMENSION(:),ALLOCATABLE :: ALEA,XSEED

!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       BOOL_ALE_TG : logical, true only if 2d model + MULTI_FVM used
!       INDX_xxx : integer ; dimension=NUMELxxx ; index for the surface
!                  of the remote connected element
!       FACE_ELM_xxx : integer ; dimension=(6/4/3*NUMELxxx,2) ; surface
!                  of the remote connected element
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
        LOGICAL :: BOOL_ALE_TG
        INTEGER, DIMENSION(:), ALLOCATABLE :: INDX_S,INDX_Q,INDX_TG
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: FACE_ELM_S,FACE_ELM_Q,FACE_ELM_TG
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       INV_GROUP : structure ; connectivity ELEMENT -> PART
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
      TYPE(INVERTGROUP_STRUCT_) :: INV_GROUP
      TYPE(MAPPING_STRUCT_) :: MAP_TABLES
      ! -------------
      ! Load structure
      TYPE (LOADS_) :: LOADS ! global structure for /LOAD
      TYPE (LOADS_), DIMENSION(NSPMD) :: LOADS_PER_PROC ! local structure for each processor --> used for the restart operation
      ! -------------
C MERGE NODES
      INTEGER NMERGE_NODE_CAND,NMERGE_NODE_DEST,NMERGE_TOT,ALE_EULER
      INTEGER, DIMENSION(:), ALLOCATABLE :: MERGE_NODE_TAB
      my_real,DIMENSION(:),ALLOCATABLE :: MERGE_NODE_TOL
C
      my_real,DIMENSION(:),ALLOCATABLE :: DGAPINT, INTGAPLOADP ,DGAPLOADINT
C SEATBELTS
      INTEGER NB_MAT_SEATBELT,NB_MAT
C   
      INTEGER  :: NUMSH3,NUMSH4, NSLICE,NPT_DRAPE,JJ,ISL, IP,IE, IDSHEL
      INTEGER , DIMENSION(:), ALLOCATABLE :: INDXSH  
      INTEGER :: NUMBER_LOAD_CYL
      INTEGER :: S_NOD2ELS,S_NOD2ELTG,S_NOD2EL1D
c=======================================================================
!     domain decomposition statistic
      DDSTAT(1:50,1:PARASIZ)=0
      I22LEN_L = 0 
      FVMAIN(1:NVOLU + NMONVOL) = 0
      M51_IFLG6 = 0
      NVARTOTMAX = 0
      ERR_MSG='BEGINNING'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      FLAG_GOTO = 0
      CALL TITRE2
      NVARABF = 1
      INTBAG=0
      L_MUL_LAG=0
      LAG_NCF = 0
      LAG_NKF = 0
      LAG_NHF = 0
      LAG_NCL = 0
      LAG_NKL = 0
      LAG_NHL = 0
      NUMELS8A = 0
      NAIRWA = 0
      NMANIM=0
      DSANIM=0
      IMPL_S0 = 0
      FLG_SPLIT = 0
      NVARTOTMAX = 0
      NXLAYMAX = 0   ! max layer nb in parts xfem
C lic. Init
      LIDFS = 0
C bc heat
      NUMCONV = 0
      NFXTEMP = 0
      NFXFLUX = 0
      NUMRADIA= 0
C ply xfem
      IPLYXFEM  = 0
      NPLYXFE = 0
      EPLYXFE = 0
      INTPLYXFEM = 0
C
      INTER_ITHKNOD=0  !defined in interface module (common_source directory)
      IRIGID_MAT = 0
C
!      IKINE1LAG = 0
      IALELAG = 0
C  added nodal mass
      TOTADDMAS = ZERO
      IPART_STACK = 0
      IPART_PCOMPP = 0
C
      SFRONTB_R2R = 1
C Flag to set for Domain Decomposition and Additional nodes
      USER_GRP_DOMAIN=0
      NSNT=0
      NMNT_2=0
c OpenMP specific
      ITASK=0
C     flag need generalize BUFINTI in DDPSLIT with Interface type 11.
C     BEFORE ININTR / I11STO KD(11)=KD(10)+4*NRTS
C     After ININTR :         KD(11)=KD(10)+2*NRTS
C     Idem with  (KD(12) = KD(11) + 4*NRTM )
      I11FLAG=0
      INTER18_AUTOPARAM = 0
      INTER18_IS_VARIABLE_GAP_DEFINED = .FALSE.
      NBPRELD = 10
C
      ALLOCATE(EOS_TAG(0:MAXEOS))
C Interface give CPU to Main surface - INITIALIZE ARRAY
C new initiation for XFEM CRACK
C
      ALLOCATE(INTERCEP(3,NINTER))

      DO I=1,NINTER
        NULLIFY(INTERCEP(1,I)%P)
        NULLIFY(INTERCEP(2,I)%P)
        NULLIFY(INTERCEP(3,I)%P)
      ENDDO
C------------------------------------------------------------------
C Initialisations variables NNOISE (pour fichiers Rad2noise Engine
C------------------------------------------------------------------
      NNOISE_SAV = 0
      SINOISE = 0
      SFNOISE = 0
      ALLOCATE(INOISE(0))
      ALLOCATE(FNOISE(0))
C----------------------------------------------
C     ALLOCATION TO REDUCE STACKSIZE
C----------------------------------------------
C INTEGER
      ALLOCATE(IWCONT(5*NUMNOD),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IWCONT')

      ALLOCATE(IWCIN2(2*NUMNOD),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IWCIN2')

C
      ALLOCATE(IKINE1LAG(3*NUMNOD),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IKINE1LAG')
        IKINE1LAG(1:3*NUMNOD)=0

      ALLOCATE(DSDOF(NUMNOD),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='DSDOF')
      DSDOF(1:NUMNOD)=0

      ALLOCATE( ADDCNE(0:NUMNOD+1),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='ADDCNE')
C ADDCNE_PXFEM needed when IPLYXFEM used
      ALLOCATE(ADDCNE_PXFEM(0:NUMNOD +1),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='ADDCNE_PXFEM')
C
      IF(NFXBODY>0) THEN
        NBIPM = 45
        ALLOCATE(FXBTAG(NUMNOD),FXBIPM(NBIPM*NFXBODY),STAT=stat)
        FXBIPM(1:NBIPM*NFXBODY) = ZERO
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='FXBTAG')
        ! Table of FXBODY file name for QAPRINT
        ALLOCATE(FXBFILE_TAB(NFXBODY))
      ELSE
        NBIPM = 1
        ALLOCATE(FXBTAG(1),FXBIPM(1),STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='FXBTAG')
        ALLOCATE(FXBFILE_TAB(0))
      ENDIF
C
      ALLOCATE(ISOLNOD(NUMELS),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='ISOLNOD')
      ALLOCATE(ISOLOFF(NUMELS),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='ISOLOFF')
      ALLOCATE(ISHEOFF(NUMELC),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='ISHEOFF')
      ALLOCATE(ITRUOFF(NUMELT),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='ITRUOFF')
      ALLOCATE(IPOUOFF(NUMELP),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IPOUOFF')
      ALLOCATE(IRESOFF(NUMELR),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IRESOFF')
      ALLOCATE(ITRIOFF(NUMELTG),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='ITRIOFF')
      ALLOCATE(IGRNRBY(NRBODY),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IGRNRBY')
      IGRNRBY(1:NRBODY) = 0
      ALLOCATE(IQUAOFF(NUMELQ),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IQUAOFF')
C Float
C----------------------------------------------
C     ALLOC TABLES INTEGER IGEO et IPM
C----------------------------------------------
      LEN_G = NPROPGI*NUMGEO
      LEN_M = NPROPMI*NUMMAT
      ALLOCATE(IGEO(LEN_G),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IGEO')
      ALLOCATE(IPM (LEN_M),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IPM')
      IGEO = 0
      IPM  = 0
C----------------------------------------------
C      ALLOC DDWEIGHTS ARRAY FROM MODULE
C----------------------------------------------
      CALL INIT_MAT_WEIGHT(NUMMAT)
C----------------------------------------------
      ALLOCATE(IPART_STATE(NPART),STAT=stat)
      IF(STAT /= 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                         C1='IPART_STATE')
      ELSE
        IPART_STATE=0
      END IF
      CALL TRACE_OUT1()
C----------------------------------------------
C     ALLOC ET INIT DU TAB. TAG NOEUDS UTILISES SUR P0
C----------------------------------------------
      ERR_CATEGORY='INTERNAL'
c obsolete code, replaced by new chained-list IFRONT
C     codage des differents types de noeuds frontiere:
C     0 noeud pas sur le proc
C     1 frontiere acceleration
C     10 frontiere cinematique
C     100 frontitere interface
C     combinaisons possibles


c SIFRONT minimum size NUMNOD. Value set to 2*NUMNOD
      SIFRONT = 2*NUMNOD

c Linked-list IFRONT
C IFRONT%IENTRY : entry in IFRONT for node N
C IFRONT%P(1,N) : SPMD domain for node N
C IFRONT%P(2,N) : next index in IFRONT for node N
      ALLOCATE(IFRONT%P(2,SIFRONT),STAT=stat)
      ALLOCATE(IFRONT%IENTRY(NUMNOD),STAT=stat)

c IENTRY2 use to save IENTRY
      ALLOCATE(IENTRY2(NUMNOD),STAT=stat)
      IF(STAT/=0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                         C1='IFRONT')
      ENDIF
c FLAGKIN array to identify boundary nodes with kinematic constraints
c (FLAGKIN(N)=1 <=> old FRONT TAG=10)
      ALLOCATE(FLAGKIN(NUMNOD),STAT=stat)
      IF(STAT/=0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                         C1='FLAGKIN')
      ENDIF
C----------------------------------------------
      CALL INI_IFRONT()
      IENTRY2(1:NUMNOD) = -1
      FLAGKIN(1:NUMNOD) = 0

c      IF(FLAG_GOTO == 1) GOTO 207 !!go to traitement rad2rad
C--------------------------------------------
C     CALCUL ISECUT
C--------------------------------------------
      ISECUT=0
      CALL LECSEC0(LSUBMODEL)
C--------------------------------------------
C     IMPOSED VELOCITIES : Check rotational DOFs : IMPOSE_DR
C--------------------------------------------
      IMPOSE_DR=0
      CALL HM_PREREAD_IMPVEL0(IMPOSE_DR,UNITAB,LSUBMODEL) !read /IMPDISP
C--------------------------------------------
C     STOCKAGE DYNAMIQUE (GENERAL) REEL
C--------------------------------------------
      IFRWV=0
208   SX  = 3*NUMNOD
      SD  = 5*NUMNOD
      SV  = 3*NUMNOD
      SVR = 3*NUMNOD*MAX(IRODDL,IRODDL0)
      SVR_1 = NUMNOD*MAX(IRODDL,IRODDL0)
      STHKE = NUMELC+NUMELTG
      SMS = NUMNOD
      SPINCH= NPINCH
      SIN = NUMNOD*MAX(IRODDL,IRODDL0)
      IF(ISECUT>0 .OR. ISUB>0 .OR. MSHSUB>0 .OR. IISROT>0 .OR.
     .   IMPOSE_DR>0 .OR. IDROT == 1) THEN
        SDR = 3*NUMNOD*MAX(IRODDL,IRODDL0)
      ELSE
        SDR = 0
      ENDIF
      IF(FLAG_GOTO == 1) GOTO 258
C--------------------------------------------
      IF(NDAMP > 0) THEN
        SDAMPR = NRDAMP*NDAMP
        SDAMP  = 3*(1+MAX(IRODDL,IRODDL0))*NUMNOD
        ALLOCATE(DAMPR(SDAMPR+SDAMP)     ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='DAMPR')
        DAMP  => DAMPR(SDAMPR+1:SDAMPR+SDAMP)
        DAMPR = 0
      ELSE
        SDAMPR = 0
        SDAMP  = 0
        ALLOCATE(DAMPR(SDAMPR)     ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='DAMPR')
        ALLOCATE(DAMP(SDAMP)       ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='DAMP')
      ENDIF
      ALLOCATE(X(SX)       ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='X')
      ALLOCATE(D(SD)       ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='D')
      ALLOCATE(V(SV)       ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='V')
      ALLOCATE(VR(SVR)     ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='VR')
      ALLOCATE(DR(SDR)     ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR)
      ALLOCATE(THKE(STHKE) ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='THKE')
      ALLOCATE(MS(SMS)     ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='MS')
      ALLOCATE(IN(SIN)     ,STAT=stat)
      ALLOCATE(XYZREF(SX)       ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='XYZREF')
      ALLOCATE(SH4ANG(NUMELC) ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='SH4ANG')
      ALLOCATE(SH3ANG(NUMELTG) ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='SH3ANG')
      IF(NUMNOD > 0) THEN
        X  = 0
        D  = 0
        V  = 0
        MS = 0
      ENDIF
      IF(SVR > 0) VR = 0
      IF(SDR > 0) DR = 0
      IF(SIN > 0) IN = 0
C--------------------------------------------
258   IF(NUMELC<STHKE) THEN
        THKEC => THKE(NUMELC+1:STHKE)
      ELSE
        THKEC => THKE
      END IF
      IF(STHKE > 0) THKE = 0
      IF(NUMELC > 0) SH4ANG = 0
      IF(NUMELTG > 0) SH3ANG = 0
c
      CALL NBFUNCT(NFUNCT,NTABLE,NPTS,LSUBMODEL)
C--------------------------------------------
C     STOCKAGE DYNAMIQUE (GENERALE) ENTIER
C--------------------------------------------
C---  Longueurs
      SICODE  = NUMNOD
      SLSHSUB = MSHSUB*NUMNOD
      SISKEW  = NUMNOD
      SISKWN   = LISKN*((NUMSKW+1)+MIN(IUN,NSPCOND)*NUMSPH+(NUMFRAM+1)+
     .           NSUBMOD)
      SIFRAME = LISKN*(NUMFRAM+1)
c     SNETH = 2*NSNOD+NSELS+NSELQ+NSELC+NSELT+NSELP+NSELR+NSELTG = 0
      SIBCSLAG= 5*NBCSLAG
      SIPART0 = LIPART1*NPART+LIPART1*NTHPART
      SIPARTTH= 2*9*NPART+2*9*NTHPART
      SIPARTS = NUMELS
      SIPARTQ = NUMELQ
      SIPARTC = NUMELC
      SIPARTT = NUMELT
      SIPARTP = NUMELP
      SIPARTR = NUMELR
      SIPARTG = NUMELTG
      SIPARTX = NUMELX
      SIPARTSP= NUMSPH
      SIPARTIG3D = NUMELIG3D
      SIPART  = SIPART0+SIPARTTH+SIPARTS+SIPARTQ+SIPARTC+SIPARTT+SIPARTP
     .        + SIPARTR+SIPARTG+SIPARTX+SIPARTIG3D+SIPARTSP
      NUMEL   = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR
     .        + NUMELTG+NUMELX+NUMSPH+NUMELIG3D
c
      SNPC    = 3*NFUNCT+1
      SIXTG0  = NIXTG*NUMELTG
      SIXTG   = SIXTG0
      SIXS0   = NIXS*NUMELS
      SIXS10  = NUMELS10*6
      SIXS20  = NUMELS20*12
      SIXS16  = NUMELS16*8
      SIXS    = SIXS0+SIXS10+SIXS20+SIXS16
      SIXQ    = NIXQ*NUMELQ
      SIXC    = NIXC*NUMELC
      SIXT    = NIXT*NUMELT
      SIXP    = NIXP*NUMELP
      SIXR    = NIXR*NUMELR
      SITAB   = NUMNOD
      SITABM1 = 2*NUMNOD
      SGJBUFI = LKJNI*NGJOINT
!---------
      SLACCELM= 3*NACCELM
      SNOM_OPT1= NRBODY+NACCELM+NVOLU+NMONVOL+NINTER+NINTSUB+
     +          NRWALL+NJOINT+NSECT+NLINK+
     +          NUMSKW+1+NUMFRAM+1+NFXBODY+NFLOW+NRBE2+
     +          NRBE3+NSUBMOD+NFXVEL+NUMBCS+NUMMPC+
     +          NGJOINT+NUNIT0+NFUNCT+NADMESH+
     +          NSPHIO+NSPCOND+NRBYKIN+NEBCS+
     +          NINICRACK+NODMAS+NBGAUGE+NCLUSTER+NINTERFRIC+
     +          NRBMERGE+NUMBCSN+NSLIPRING+NRETRACTOR
      SNOM_OPT = SNOM_OPT1*LNOPT1+1
      SINOM_OPT= 33
      SNOM_SECT= ncharline*NSECT
      IF(FLAG_GOTO==1) GOTO 209
C
C---  Allocations
      ALLOCATE(ICODE(SICODE)     ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='ICODE')
      ALLOCATE(LSHSUB(SLSHSUB)   ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='LSHSUB')
      ALLOCATE(ISKEW(SISKEW)     ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='ISKEW')
      ALLOCATE(ISKWN(SISKWN)     ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='ISKWN')
      ALLOCATE(IBCSLAG(SIBCSLAG) ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IBCSLAG')
      ALLOCATE(IPART(SIPART)     ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IPART')
      ALLOCATE(NPC (SNPC)        ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='NPC')
      ALLOCATE(IXTG(SIXTG)       ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IXTG')
      ALLOCATE(IXS(SIXS)         ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IXS')
      ALLOCATE(IXQ(SIXQ)         ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IXQ')
      ALLOCATE(IXC(SIXC)         ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IXC')
      ALLOCATE(IXT(SIXT)         ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IXT')
      ALLOCATE(IXP(SIXP)         ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IXP')
      ALLOCATE(IXR(SIXR)         ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IXR')
      ALLOCATE(ITAB(SITAB)       ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='ITAB')
      ALLOCATE(ITABM1(SITABM1)   ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='ITABM1')
      ALLOCATE(GJBUFI(SGJBUFI)   ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='GJBUFI')
      ALLOCATE(LACCELM(SLACCELM) ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='LACCELM')

      ALLOCATE(NOM_OPT(SNOM_OPT) ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='NOM_OPT')
      ALLOCATE(INOM_OPT(0:SINOM_OPT) ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='INOM_OPT')
      ALLOCATE(NOM_SECT(SNOM_SECT) ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='NOM_SECT')
      ALLOCATE(IXR_KJ(5*(NUMELR+1)) ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IXR_KJ')
      ALLOCATE(IWORKSH(3,NUMELC+NUMELTG)         ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IXC')
      IF(NUMELIG3D > 0) THEN
        ALLOCATE(WIGE(NUMNOD) ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                           C1='WIGE')
        DEG_MAX=0
      ELSE
        ALLOCATE(WIGE(0) ,STAT=stat)
      ENDIF
      ALLOCATE(R_SKEW(NUMELR) ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='R_SKEW')
C
C --- Initialisations
      IF(SICODE > 0) ICODE   = 0
      IF(SLSHSUB > 0) LSHSUB  = 0
      IF(SISKEW > 0) ISKEW   = 0
      IF(SISKWN > 0) ISKWN   = 0
      IF(SIBCSLAG > 0) IBCSLAG = 0
      IF(SIPART > 0) IPART   = 0
      IF(SNPC > 0) NPC     = 0
      IF(SITAB > 0) ITAB    = 0
      IF(SITABM1 > 0) ITABM1  = 0
      IF(SGJBUFI > 0) GJBUFI  = 0
      IF(SLACCELM > 0) LACCELM = 0
      IF(SNOM_OPT > 0) NOM_OPT = 0
      IF(SINOM_OPT > 0) INOM_OPT = 0
      IF(NUMELR > 0) IXR_KJ = 0
      IF(NUMELC+NUMELTG > 0) IWORKSH = 0
      IF(NUMELR > 0) R_SKEW = 0
C
C---  Pointeurs : sous-tableaux
      IF(SISKWN-SIFRAME<SISKWN) THEN
      IFRAME => ISKWN(SISKWN-SIFRAME+1:SISKWN)
      ELSE
        IFRAME => ISKWN
      END IF
!--- NEW DATA STRUCTE ALLOCATION FOR GROUPS OF ENTITIES
      ALLOCATE(SUBSETS(NSUBS))
      SUBSETS(1:NSUBS)%ID=0
      ALLOCATE(IGRNOD(NGRNOD+NSETS))
      ALLOCATE(IGRBRIC(NGRBRIC+NSETS))
      ALLOCATE(IGRQUAD(NGRQUAD+NSETS))
      ALLOCATE(IGRSH4N(NGRSHEL+NSETS))
      ALLOCATE(IGRSH3N(NGRSH3N+2*NSETS))
      ALLOCATE(IGRTRUSS(NGRTRUS+NSETS))
      ALLOCATE(IGRBEAM(NGRBEAM+NSETS))
      ALLOCATE(IGRSPRING(NGRSPRI+NSETS))
      ALLOCATE(IGRPART(NGRPART+NSETS))
      ALLOCATE(IGRSURF(NSURF+NSETS))
      ALLOCATE(IGRSURF_PROC(NSURF+NSETS,NSPMD))
      ALLOCATE(IGRSLIN(NSLIN+NSETS))
      IGRNOD(1:NGRNOD+NSETS)%NENTITY = 0
      IGRBRIC(1:NGRBRIC+NSETS)%NENTITY = 0
      IGRQUAD(1:NGRQUAD+NSETS)%NENTITY = 0
      IGRSH4N(1:NGRSHEL+NSETS)%NENTITY = 0
      IGRSH3N(1:NGRSH3N+2*NSETS)%NENTITY = 0
      IGRTRUSS(1:NGRTRUS+NSETS)%NENTITY = 0
      IGRBEAM(1:NGRBEAM+NSETS)%NENTITY = 0
      IGRSPRING(1:NGRSPRI+NSETS)%NENTITY = 0
      IGRPART(1:NGRPART+NSETS)%NENTITY = 0
!--  variable initialization to not printout the empty SET groups
      IGRNOD(1:NGRNOD+NSETS)%SET_GROUP = 0
      IGRBRIC(1:NGRBRIC+NSETS)%SET_GROUP = 0
      IGRQUAD(1:NGRQUAD+NSETS)%SET_GROUP = 0
      IGRSH4N(1:NGRSHEL+NSETS)%SET_GROUP = 0
      IGRSH3N(1:NGRSH3N+2*NSETS)%SET_GROUP = 0
      IGRTRUSS(1:NGRTRUS+NSETS)%SET_GROUP = 0
      IGRBEAM(1:NGRBEAM+NSETS)%SET_GROUP = 0
      IGRSPRING(1:NGRSPRI+NSETS)%SET_GROUP = 0
      IGRPART(1:NGRPART+NSETS)%SET_GROUP = 0
      IGRSURF(1:NSURF+NSETS)%SET_GROUP = 0
      IGRSLIN(1:NSLIN+NSETS)%SET_GROUP = 0

      IGRSURF(1:NSURF+NSETS)%NSEG = 0
      IGRSURF(1:NSURF+NSETS)%NSEG_IGE = 0
      IGRSURF(1:NSURF+NSETS)%SET_GROUP = 0
      IGRSURF(1:NSURF+NSETS)%NB_MADYMO = 0
      IGRSURF(1:NSURF+NSETS)%NSEG_R2R_ALL = 0
      IGRSURF(1:NSURF+NSETS)%NSEG_R2R_SHARE = 0
!--
!      IF(NBOX > 0)    CALL MY_ALLOC(IGRBOX,NBOX)
C     ipart
216   L0 = SIPART0
      L1 = L0 + SIPARTTH
      L2 = L1 + SIPARTS
      L3 = L2 + SIPARTQ
      L4 = L3 + SIPARTC
      L5 = L4 + SIPARTT
      L6 = L5 + SIPARTP
      L7 = L6 + SIPARTR
      L8 = L7 + 0
      L9 = L8 + SIPARTG
      L10= L9 + SIPARTX
      L11= L10+ SIPARTSP
      L12= L11+ SIPARTIG3D
      IF(L1>L0) THEN
      IPARTTH => IPART(L0+1:L1)
      ELSE
        IPARTTH => IPART
      END IF
      IF(L2>L1) THEN
      IPARTS => IPART(L1+1:L2)
      ELSE
        IPARTS => IPART
      END IF
      IF(L3>L2) THEN
      IPARTQ => IPART(L2+1:L3)
      ELSE
        IPARTQ => IPART
      END IF
      IF(L4>L3) THEN
      IPARTC => IPART(L3+1:L4)
      ELSE
        IPARTC => IPART
      END IF
      IF(L5>L4) THEN
      IPARTT => IPART(L4+1:L5)
      ELSE
        IPARTT => IPART
      END IF
      IF(L6>L5) THEN
      IPARTP => IPART(L5+1:L6)
      ELSE
        IPARTP => IPART
      END IF
      IF(L7>L6) THEN
      IPARTR => IPART(L6+1:L7)
      ELSE
        IPARTR => IPART
      END IF
      IF(L9>L8) THEN
      IPARTG => IPART(L8+1:L9)
      ELSE
        IPARTG => IPART
      END IF
      IF(L10>L9) THEN
      IPARTX => IPART(L9+1:L10)
      ELSE
        IPARTX => IPART
      END IF
      IF(L11>L10) THEN
      IPARTSP=> IPART(L10+1:L11)
      ELSE
        IPARTSP => IPART
      END IF
      IF(L12>L11) THEN
      IPARTIG3D=> IPART(L11+1:L12)
      ELSE
        IPARTIG3D => IPART
      END IF
      IF(FLAG_GOTO==1) GOTO 217
c
      IF(NFUNCT+2<=SNPC-NFUNCT) THEN
        NPC1   => NPC(NFUNCT+2:SNPC-NFUNCT)
      ELSE
        NPC1   => NPC
      END IF

212      IF(SIXS0+SIXS10>SIXS0) THEN
      IXS10  => IXS(SIXS0+1:SIXS0+SIXS10)
      ELSE
c        IXS10  => IXS
c          NULLIFY(IXS10)
           ALLOCATE(IXS10(1))
      END IF
      IF(SIXS0+SIXS10+SIXS20>SIXS0+SIXS10) THEN
      IXS20  => IXS(SIXS0+SIXS10+1:SIXS0+SIXS10+SIXS20)
      ELSE
c        IXS20  => IXS
          ALLOCATE(IXS20(1))
      END IF
      IF(SIXS>SIXS0+SIXS10+SIXS20) THEN
      IXS16  => IXS(SIXS0+SIXS10+SIXS20+1:SIXS)
      ELSE
c        IXS16  => IXS
c         NULLIFY(IXS16)
          ALLOCATE(IXS16(1))
      END IF
      PIXS10 = MIN(SIXS,SIXS0+1              )
      PIXS20 = MIN(SIXS,SIXS0+SIXS10+1       )
      PIXS16 = MIN(SIXS,SIXS0+SIXS10+SIXS20+1)

      IF(FLAG_GOTO==1) GOTO 213
C
C     .. TO BE MAINTAINED (cf doc/inom_opt.txt) ..
210   INOM_OPT(1) = NRBODY
      INOM_OPT(2) = INOM_OPT(1) + NACCELM
      INOM_OPT(3) = INOM_OPT(2) + NVOLU + NMONVOL
      INOM_OPT(4) = INOM_OPT(3) + NINTER
      INOM_OPT(5) = INOM_OPT(4) + NINTSUB
      INOM_OPT(6) = INOM_OPT(5) + NRWALL
      INOM_OPT(7) = INOM_OPT(6) + 0 !obsolete option removed
      INOM_OPT(8) = INOM_OPT(7) + NJOINT
      INOM_OPT(9) = INOM_OPT(8) + NSECT
      INOM_OPT(10)= INOM_OPT(9) + NLINK
      INOM_OPT(11)= INOM_OPT(10)+ NUMSKW+1+NUMFRAM+1+NSUBMOD
      INOM_OPT(12)= INOM_OPT(11)+ NFXBODY
      INOM_OPT(13)= INOM_OPT(12)+ NFLOW
      INOM_OPT(14)= INOM_OPT(13)+ NRBE2
      INOM_OPT(15)= INOM_OPT(14)+ NRBE3
C
      INOM_OPT(16)= INOM_OPT(15)+ NFXVEL
      INOM_OPT(17)= INOM_OPT(16)+ NUMBCS + NUMBCSN
      INOM_OPT(18)= INOM_OPT(17)+ NUMMPC
      INOM_OPT(19)= INOM_OPT(18)+ NGJOINT
      INOM_OPT(20)= INOM_OPT(19)+ NUNIT0
      INOM_OPT(21)= INOM_OPT(20)+ NFUNCT
      INOM_OPT(22)= INOM_OPT(21)+ NADMESH
      INOM_OPT(23)= INOM_OPT(22)+ NSPHIO
      INOM_OPT(24)= INOM_OPT(23)+ NSPCOND
      INOM_OPT(25)= INOM_OPT(24)+ NEBCS
      INOM_OPT(26)= INOM_OPT(25)+ NINICRACK
      INOM_OPT(27)= INOM_OPT(26)+ NODMAS
      INOM_OPT(28)= INOM_OPT(27)+ NBGAUGE
      INOM_OPT(29)= INOM_OPT(28)+ NCLUSTER
      INOM_OPT(30)= INOM_OPT(29)+ NINTERFRIC
      INOM_OPT(31)= INOM_OPT(30)+ NRBMERGE
      INOM_OPT(32)= INOM_OPT(31)+ NSLIPRING
      INOM_OPT(33)= INOM_OPT(32)+ NRETRACTOR
C
      IF(FLAG_GOTO==1) GOTO 211

      CALL ANODIN( NUMNOD)
      IF(NPART==0) THEN
        CALL APARTIN(NPART+1)
      ELSE
      CALL APARTIN(NPART)
      END IF
C--------------------------------------------
C     LECTURE DES FONCTIONS & TABLES
C--------------------------------------------
      ERR_MSG='FUNCTIONS & TABLES'
      ERR_CATEGORY='FUNCTIONS & TABLES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c
      IF(NFUNCT > 0 .OR. NTABLE > 0) THEN
C
C NTABLE = NFUNCT + ...
        WRITE(ISTDO,'(A)')' .. FUNCTIONS & TABLES'
        ALLOCATE(TABLE(NTABLE) ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='TABLE')
        CALL TABLE_ZERO(TABLE)
        IF(NFUNCT > 0) THEN
          ERR_MSG='FUNCTIONS'
          CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
          ALLOCATE(TF(NPTS)        ,STAT=stat)
          ALLOCATE(FUNCRYPT(NFUNCT) ,STAT=stat)
          FUNCRYPT = 0
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                         C1='TF')
          IF(NPTS > 0) TF = 0
          CALL HM_READ_FUNCT(NPC    ,TF     ,NFUNCT ,TABLE, NPTS,
     .                NOM_OPT(LNOPT1*INOM_OPT(20)+1) ,FUNCRYPT, UNITAB, LSUBMODEL)
          CALL TRACE_OUT1()
        END IF
C       TABLES
        ERR_MSG='TABLES 1'
        CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
        CALL HM_READ_TABLE1 (NTABLE, TABLE ,NFUNCT ,NPC ,TF ,
     .                NOM_OPT(LNOPT1*INOM_OPT(20)+1), UNITAB, LSUBMODEL)
        CALL CHKFUNCT (NFUNCT, NPC,NOM_OPT(LNOPT1*INOM_OPT(20)+1))
        IF(NFUNCT > 0) THEN
          CALL HM_READ_MOVE_FUNCT(NPC    ,TF     ,NFUNCT ,TABLE, NTABLE,FUNCRYPT, UNITAB, LSUBMODEL)
          DEALLOCATE(FUNCRYPT)
        END IF
        CALL HM_READ_TABLE2 (NTABLE, TABLE ,NFUNCT , UNITAB, LSUBMODEL )
        CALL TRACE_OUT1()
      ELSE
C       TABLES
        ERR_MSG='TABLES 0'
        CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
        NPTS = 0
        ALLOCATE(TF(NPTS) ,STAT=stat)
        ALLOCATE(TABLE(0) ,STAT=stat)
        CALL TRACE_OUT1()
      ENDIF
      STF =  NPTS
      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES FONCTIONS 2D
C--------------------------------------------
      ALLOCATE(FUNC2D(NFUNC2D))
      IF(NFUNC2D > 0) THEN
         CALL HM_READ_FUNC2D(FUNC2D, LSUBMODEL, UNITAB)
      ENDIF
C--------------------------------------------
C     STOCKAGE DYNAMIQUE (GENERAL) REEL - suite
C-------------------------------------------
      ERR_MSG='DYNAMIC STORAGE'
      ERR_MSG='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SPM = NUMMAT*NPROPM
      SSKEW = LSKEW*(NUMSKW+1)
      IF(NSPCOND > 0) SSKEW = SSKEW + LSKEW*NUMSPH
      IF(NSUBMOD > 0) SSKEW = SSKEW + LSKEW*NSUBMOD
      SXFRAME = NXFRAME*(NUMFRAM+1)
      SSKEW = SSKEW + SXFRAME
      SGEO  = NUMGEO*NPROPG
      SEANI = NUMELS+NUMELQ+NUMELC+NUMELTG
      ISHIF = NUMELS+NUMELQ+NUMELC

      ALLOCATE(PM(SPM)     ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='PM')
      ALLOCATE(GEO(SGEO)   ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='GEO')
c dans le skew, on met tous les skew du modele
      ALLOCATE(SKEW(SSKEW) ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='SKEW')
      IF(SSKEW-SXFRAME<SSKEW) THEN
      XFRAME => SKEW(SSKEW-SXFRAME+1:SSKEW)
      ELSE
        XFRAME => SKEW
      END IF
c      ALLOCATE(XFRAME(SXFRAME) ,STAT=stat)
      ALLOCATE(EANI(SEANI) ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='EANI')
      IF(SEANI > 0) EANI = 0
      IF(NUMELTG > 0) THEN
        EANIT => EANI(ISHIF+1:SEANI)
      ELSE
        EANIT => EANIT2
      ENDIF
      PM   = 0
      SKEW = 0
      GEO  = 0
      ISHIF =NUMELS+NUMELQ+NUMELC
      CALL TRACE_OUT1()
C--------------------------------------------
      ERR_MSG='KINEMATIC INITIALIZATION'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL KININI(D      )
      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES MATERIAUX
C--------------------------------------------
      ERR_MSG='MATERIALS'
      ERR_CATEGORY='MATERIALS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL SAV_BUF_POINT(NPC    ,5)
      CALL SAV_BUF_POINT(TF     ,6)
      WRITE(ISTDO,'(A)')TITRE(11)
C
      SRWORK  = MAX(NUMMAT*10000,1000000)
      SBUFMAT = 0
      ALLOCATE(RWORK(SRWORK)    ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='RWORK')
      IF(SRWORK > 0) RWORK = ZERO
c------------------------------------
      ALLOCATE(MTAG_INI   (NUMMAT))
      ALLOCATE(MPARAM_INI (NUMMAT))
      CALL INIT_MLAW_TAG(MTAG_INI,NUMMAT)
      MAT_ELEM%MAT_PARAM(1:NUMMAT) => MPARAM_INI(1:NUMMAT)
      MLAW_TAG(1:NUMMAT) => MTAG_INI(1:NUMMAT)
      NLOC_DMG%IMOD = 0
c---------------------------------------------------------------
      CALL READ_MATERIAL_MODELS(
     .     MAT_ELEM    ,MLAW_TAG ,FAIL_TAG ,EOS_TAG   ,
     .     RWORK       ,SBUFMAT  ,IPM      ,PM       ,UNITAB    ,
     .     MULTI_FVM   ,MAXEOS   ,FAILWAVE ,NLOC_DMG ,LSUBMODEL ,
     .     TABLE       ,NPC      )
c---------------------------------------------------------------
      ALLOCATE(BUFMAT(SBUFMAT)    ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='BUFMAT')
      BUFMAT = RWORK(1:SBUFMAT)
      IF(ALLOCATED(RWORK)) DEALLOCATE(RWORK)
      CALL TRACE_OUT1()
c
      ERR_MSG='STORAGE'
      ERR_MSG='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c
      CALL TITRE3
C--------------------------------------------
C     INITIALISATION DES POINTEURS UTILISATEUR
C--------------------------------------------
        CALL SAV_BUF_POINT(PM     ,1)
        CALL SAV_BUF_POINT(BUFMAT ,2)
        CALL SAV_BUF_POINT(GEO    ,3)
C        CALL SAV_BUF_POINT(AM(M26),4)
cma53a1  !!!!!!! a faire
        CALL SAV_BUF_POINT(ISKWN  ,7)
        CALL SAV_BUF_POINT(SKEW   ,8)
        CALL SAV_BUF_POINT(IPM ,11)
        CALL SAV_BUF_POINT(IGEO,12)
      CALL TRACE_OUT1()
C--------------------------------------------
C       READ NODES / CNODES / BUILD GHOST NODES
C--------------------------------------------
C     NODES
      ERR_MSG='NODES'
      ERR_CATEGORY='NODES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      ALLOCATE(CMERGE(NUMCNOD),STAT=stat)
      IF(STAT /= 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                         C1='CMERGE')
      ENDIF
      CMERGE = ZERO
C
      WRITE(ISTDO,'(A)')TITRE(12)
      CALL HM_READ_NODE(X       ,ITAB    ,ITABM1 ,CMERGE ,UNITAB  ,
     .                  WIGE    ,LSUBMODEL,IS_DYNA)

      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES SUBMODELS & TRANSFORMATION DE SUBMODEL
C--------------------------------------------
      ERR_MSG='SUBMODELS'
      ERR_CATEGORY='SUBMODELS'
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR)
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SRTRANS = NRTRANS * NTRANSF
      ALLOCATE(RTRANS(SRTRANS)     ,STAT=stat)
      IF(SRTRANS > 0) RTRANS = ZERO

      IF(NSUBMOD > 0)THEN
        WRITE(ISTDO,'(A)')' .. SUBMODELS'
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='RTRANS')
        CALL LECTRANSSUB(X      ,IGRNOD  ,ITAB   ,ITABM1 ,UNITAB,
     .                 RTRANS   ,LSUBMODEL,IS_DYNA)
        CALL LECSUBMOD(ISUBMOD,X,UNITAB,ITABM1,RTRANS,ITAB,LSUBMODEL,IS_DYNA)
      ENDIF

      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES REPERES OBLIQUES
C   + LECTURE DES REFERENTIELS
C--------------------------------------------
C     SKEWS
      ERR_MSG='SKEWS'
      ERR_CATEGORY='SKEWS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NUMSKW/=0)WRITE(ISTDO,'(A)')TITRE(14)
        CALL HM_READ_SKW(SKEW   ,ISKWN  ,X      ,
     .                   ITAB   ,ITABM1 ,BID13  ,
     .                   LSUBMODEL,RTRANS,
     .                   NOM_OPT(LNOPT1*INOM_OPT(10)+1),UNITAB)
C
        CALL HM_READ_FRM(ISKWN  ,X      ,ITAB   ,ITABM1 ,XFRAME ,
     .                   LSUBMODEL,RTRANS,
     .                   NOM_OPT(LNOPT1*INOM_OPT(10)+1),UNITAB)
      CALL TRACE_OUT1()
C--------------------------------------------
C     PRELECTURE DES DRAPE
C--------------------------------------------
      IF(NDRAPE > 0) THEN
        ALLOCATE(IDRAPEID(NDRAPE)     ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='DRAPE')
        IDRAPEID = 0
        CALL HM_READ_PRELECDRAPE(IDRAPEID,LSUBMODEL)
      ELSE
        ALLOCATE(IDRAPEID(0))
      ENDIF
C--------------------------------------------
C     LECTURE DES PROPRIETES
C--------------------------------------------
      IF(NSPHSOL/=0)THEN
        CALL HM_PREREAD_PART(IPART,IGEO,LSUBMODEL)
      END IF
C--------------------------------------------
      NRBAG=0
      SBUFGEO = 0
C     PROPERTIES
      ERR_MSG='PROPERTIES'
      ERR_CATEGORY='PROPERTIES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c
      IF(NUMGEO > 0)THEN
        WRITE(ISTDO,'(A)')TITRE(31)
        SRWORK  = NUMGEO*(BGEOSIZE+MAXFUNC+MAXMAT+MAXPID+MAXTAB)
        SBUFGEO = 0
        ALLOCATE(DBRWORK(SRWORK)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='DBRWORK')
        DBRWORK = ZERO
        ALLOCATE(KNOT(SKNOT)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='KNOT')
        CALL SAV_BUF_POINT(DBRWORK,4)
C
        IADGEO=1
        CALL HM_READ_PROPERTIES(GEO        , X           , IXC      , PM       ,ITABM1    ,
     .                          DBRWORK    , SBUFGEO     , ISKWN    , IGEO     ,IPM       ,
     .                          NPC        , TF          , UNITAB   , RTRANS   ,LSUBMODEL ,
     .                          PROP_TAG   , IPART       , KNOT     , IDRAPEID ,STACK_INFO,
     .                          NUMGEOSTACK, NPROP_STACK , MULTI_FVM, IADGEO)
C
        ALLOCATE(BUFGEO(SBUFGEO)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='BUFGEO')
        BUFGEO(1:SBUFGEO) = DBRWORK(1:SBUFGEO)
        DEALLOCATE(DBRWORK)
        CALL SAV_BUF_POINT(BUFGEO,4)
      ELSE
        ALLOCATE(BUFGEO(SBUFGEO)    ,STAT=stat)
      ENDIF
C
      IF(NUMPLY /= 0) THEN
        ALLOCATE(PLY_INFO(2,NUMPLY),STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                            MSGTYPE=MSGERROR,
     .                            C1='PLY')
        PLY_INFO = 0
      ELSE
        ALLOCATE(PLY_INFO(0,0))
      ENDIF
c
      IF(NUMSTACK > 0) THEN
C prelecture
         LEN_G = NPROPGI*(NUMSTACK + NUMPLY)
         ALLOCATE(IGEO_STACK(LEN_G),STAT=stat)
         IGEO_STACK = 0
         LEN_G = NPROPG*(NUMSTACK + NUMPLY )
         ALLOCATE(GEO_STACK(LEN_G),STAT=stat)
         GEO_STACK = ZERO
         CALL LECSTACK_PLY(GEO_STACK ,X          ,IXC       ,PM   ,ITABM1  ,
     .                     ISKWN     ,IGEO_STACK ,IPM       ,NPC  ,TF      ,
     .                     UNITAB    , RTRANS    ,LSUBMODEL,IPART ,IDRAPEID,
     .                     PLY_INFO  ,STACK_INFO,NUMGEOSTACK, NPROP_STACK)
        ELSE
             ALLOCATE(IGEO_STACK(0),STAT=stat)
             ALLOCATE(GEO_STACK(0),STAT=stat)
      ENDIF
C-----allocate DR if necessary------
      IF(SDR==0 .AND. IDROT == 1) THEN
       SDR = 3*NUMNOD*MAX(IRODDL,IRODDL0)
       IF(ALLOCATED(DR)) DEALLOCATE(DR)
       ALLOCATE(DR(SDR)     ,STAT=stat)
       IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                            MSGTYPE=MSGERROR)
       DR = ZERO
      ENDIF
C

C      print*, 'NPINCH', NPINCH, 'NUMNOD', NUMNOD
      SPINCH= NPINCH
C      print*, SPINCH
        ALLOCATE(PINCH_DATA%XPINCH(3,SPINCH)       ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='XPINCH')
        ALLOCATE(PINCH_DATA%DPINCH(3,SPINCH)       ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='DPINCH')
        ALLOCATE(PINCH_DATA%VPINCH(3,SPINCH)       ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='VPINCH')
        ALLOCATE(PINCH_DATA%MSPINCH(SPINCH)       ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='MSPINCH')
C
      IF(NPINCH > 0) THEN
        PINCH_DATA%XPINCH(1:3,1:SPINCH) = ZERO
        PINCH_DATA%DPINCH(1:3,1:SPINCH) = ZERO
        PINCH_DATA%VPINCH(1:3,1:SPINCH) = ZERO
        PINCH_DATA%MSPINCH(1:SPINCH) = ZERO
      ENDIF
C
      CALL TRACE_OUT1()
C-----------------------------------------------------
C     LECTURE DES PARTS
C     REMPLACEMENT DES NUMEROS EXTERNES DES MATERIAUX
C     REMPLACEMENT DES NUMEROS EXTERNES DES PROPRIETES
C     PAR LES NUMEROS INTERNES
C-----------------------------------------------------
      ERR_MSG='PARTS'
      ERR_CATEGORY='PARTS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SIWORK  = MAX(2*NUMELS,2*NUMELQ,3*(NPART+NTHPART),3*NUMSKW,NUMELS,
     *              NUMELC,NUMELTG,NUMMAT+NUMGEO,NUMELT+NUMELP+NUMELR+
     *              NUMELX+NUMELIG3D)
      ALLOCATE(IWORK(SIWORK)    ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IWORK')
      IWORK = 0
      ALLOCATE(THK_PART(NPART)    ,STAT=stat)
C
      CALL HM_READ_PART(IPART  ,PM      ,GEO       ,IPM     ,IGEO ,IWORK ,THK_PART,
     .                  UNITAB,LSUBMODEL,MULTI_FVM ,MLAW_TAG)

      CALL TRACE_OUT1()
      
C--------------------------------------------
C     STOCKAGE DYNAMIQUE (A.L.E.) ENTIER
C--------------------------------------------
      ALE%GLOBAL%SNALE  = MAX(IALE,IEULER,IALELAG)*NUMNOD
      ALE%GLOBAL%SIELVS = 6*NUMELS+MAX(IALE,ITHERM,IEULER,IALELAG)* (4 * NUMELQ + 3 * NUMELTG)
      SIFILL = NMULT*NUMNOD
      SIMS   = NMULT*NUMNOD
C---
      ALLOCATE(IFILL(SIFILL),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID   = 268,
     .                           ANMODE  = ANINFO,
     .                           MSGTYPE = MSGERROR,
     .                           C1      = 'IFILL')

      ALLOCATE(IMS(SIMS),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID   = 268,
     .                           ANMODE  = ANINFO,
     .                           MSGTYPE = MSGERROR,
     .                           C1      = 'IMS')


      IF(SIFILL > 0) IFILL = 0
      IF(SIMS   > 0) IMS   = 0

      ALLOCATE(DFLOW(3*NUMNOD*IALELAG)      ,STAT=stat)
      ALLOCATE(VFLOW(3*NUMNOD*IALELAG)      ,STAT=stat)
      ALLOCATE(WFLOW(3*NUMNOD*IALELAG)      ,STAT=stat)

      IF(IALELAG > 0) THEN
       DFLOW = ZERO
       VFLOW = ZERO
       WFLOW = ZERO
      ENDIF

      IF(ALEFVM_Param%IEnabled > 0)THEN
        ALLOCATE(ALEFVM_Buffer%FCELL(6,NUMELS)   ,STAT=stat)
        ALEFVM_Buffer%FCELL(:,:) = ZERO
      ENDIF

C----------------------------------      
C--------------------------------------------
C     MULTIDOMAINS
C--------------------------------------------
      ERR_MSG='MULTIDOMAINS'
      ERR_CATEGORY='MULTIDOMAINS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      NR2R = 5
      R2R_SIU = 0
      SIEXLNK = NR2R*NR2RLNK
      IF((NR2RLNK+NSUBDOM)>0) THEN
        ALLOCATE(IEXLNK(SIEXLNK)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IEXLNK')
        IEXLNK = 0
        WRITE(ISTDO,'(A)')  ' .. EXTERNAL COUPLING'
        CALL LECEXTLNK(IEXLNK,IPART,LSUBMODEL)
        NL_DDR2R = NR2RLNK
      ELSE
        ALLOCATE(IEXLNK(0))
      ENDIF
      IF(NSUBDOM > 0) THEN
        ALLOCATE(TAG_PART(NPART),IPART_R2R(4,NPART))
        TAG_PART(:)=0
        CALL R2R_VOID(IPART)
        NL_DDR2R = 4
        R2R_SIU = 1
c       complete mlaw_tag for new void materials
        IF(NUMMAT > NUMMAT0) THEN
         ALLOCATE(MTAG_R2R(NUMMAT))
         ALLOCATE(MPARAM_R2R(NUMMAT))
         CALL INIT_MLAW_TAG(MTAG_R2R   ,NUMMAT)
         MTAG_R2R(1:NUMMAT0) = MTAG_INI(1:NUMMAT0)
         CALL MATPARAM_R2R_VOID(MPARAM_R2R, MPARAM_INI   ,NUMMAT0 ,NUMMAT)
         MLAW_TAG(1:NUMMAT)     => MTAG_R2R(1:NUMMAT)
         MAT_ELEM%MAT_PARAM(1:NUMMAT) => MPARAM_R2R(1:NUMMAT)
         MAT_ELEM%NUMMAT = NUMMAT
         DEALLOCATE(MTAG_INI)
         DEALLOCATE(MPARAM_INI)
        ENDIF
      ELSE
        ALLOCATE(TAG_PART(0),IPART_R2R(4,0))
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C--------------------------------------------
C     POIN UMP
C--------------------------------------------
!     TABMP_L defined in tabsiz_c
      TABMP_L = 10
C
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='PARTS')

      ALLOCATE( POIN_PART_SHELL(2,NPART) )
      ALLOCATE( POIN_PART_TRI(2,NPART) )
      ALLOCATE( POIN_PART_SOL(2,NPART,7) )
      ALLOCATE( MID_PID_SHELL(NUMMAT),MID_PID_TRI(NUMMAT) )
      ALLOCATE( MID_PID_SOL(NUMMAT,7) )
      POIN_PART_SHELL(1:2,1:NPART) = 0
      POIN_PART_TRI(1:2,1:NPART) = 0
      POIN_PART_SOL(1:2,1:NPART,1:7) = 0

      ALLOCATE(POIN_UMP(NUMMAT), STAT=stat)
      ALLOCATE(TAB_UMP_LOC(5,NPART), STAT=stat)
      TAB_UMP_LOC(1:5,1:NPART) = 0
C
      CALL SET_POIN_UMP(IPART,IPM,TAB_UMP_LOC,POIN_UMP,TAILLE2)
      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES SOLIDES
C--------------------------------------------
      ERR_MSG='ELEMENTS'
      ERR_CATEGORY='ELEMENTS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NUMELS/=0)THEN
       WRITE(ISTDO,'(A)')TITRE(15)

       CALL HM_READ_SOLID(IXS    ,PM      ,ITAB    ,ITABM1  ,
     .             IPART  ,IPARTS  ,EANI    ,IXS10   ,IXS20   ,IXS16   ,
     .             IGEO   ,LSUBMODEL,IS_DYNA,X     )

      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES ELEMENTS 2D
C--------------------------------------------
      IF(NUMELQ/=0)THEN
        WRITE(ISTDO,'(A)')TITRE(16)
        CALL HM_READ_QUAD(IXQ    ,ITAB   ,ITABM1 ,IPART  ,IPARTQ ,
     .             IPM     ,IGEO   ,UNITAB ,LSUBMODEL)
      ENDIF
C--------------------------------------------
C     LECTURE DES COQUES
C--------------------------------------------
       ALLOCATE(ITAG(NUMNOD),STAT=stat)
       IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                              MSGTYPE=MSGERROR,
     .                         C1='ITAG')
      ITAG=0
C
      IF(NUMELC/=0)THEN
       WRITE(ISTDO,'(A)')TITRE(17)
       CALL HM_READ_SHELL(IXC    ,ITAB   ,ITABM1 ,IPART  ,IPARTC ,
     .             THKE   ,IPM     ,IGEO   ,UNITAB ,ITAG   ,SH4ANG, LSUBMODEL)
      ENDIF
C
C          ATTENTION, LES ELEMENTS COQUE SONT PERMUTES
C          POUR GENERER DES GROUPES OPTIMISES
C          APRES LA LECTURE DES PID
C
C--------------------------------------------
C     LECTURE DES BARRES
C--------------------------------------------
      IF(NUMELT/=0)THEN
       WRITE(ISTDO,'(A)')TITRE(18)
       CALL HM_READ_TRUSS(IXT    ,ITAB   ,ITABM1 ,IPART  ,IPARTT  ,
     .             IPM    ,IGEO    ,LSUBMODEL)
      ENDIF
C--------------------------------------------
C     LECTURE DES POUTRES
C--------------------------------------------
      IF(NUMELP/=0)THEN
       WRITE(ISTDO,'(A)')TITRE(19)
       CALL HM_READ_BEAM(IXP    ,ITAB   ,ITABM1 ,IPART  ,IPARTP  ,
     .             IPM    ,IGEO    ,LSUBMODEL)
      ENDIF
C--------------------------------------------
C     LECTURE DES RESSORTS
C--------------------------------------------
      REINT = ZERO
      IF(NUMELR/=0)THEN
       WRITE(ISTDO,'(A)')TITRE(20)
       CALL HM_READ_SPRING(IXR    ,ITAB   ,ITABM1 ,IPART  ,IPARTR  ,
     .             IGEO  ,IXR_KJ ,LSUBMODEL,ISKWN,R_SKEW,IPM)
      ENDIF
C--------------------------------------------
C     LECTURE DES COQUES TRIANGLE
C--------------------------------------------
      IF(NUMELTG/=0)THEN
        IF(N2D==0 .AND. NUMELTRIA==0)THEN
           WRITE(ISTDO,'(A)')TITRE(21)
            CALL HM_READ_SH3N(  IXTG ,ITAB   ,ITABM1  ,IPART  ,IPARTG  ,
     .                     THKEC  ,PM      ,GEO    ,EANIT   ,IGEO   ,
     .                     IPM    ,UNITAB  ,SH3ANG , LSUBMODEL)
        ELSEIF(NUMELTRIA==NUMELTG)THEN
           WRITE(ISTDO,'(A)')TITRE(23)
            CALL HM_READ_TRIA(IXTG   ,ITAB   ,ITABM1  ,IPART  ,IPARTG  ,
     .                     PM     ,GEO    ,EANIT   ,IGEO   ,IPM     ,
     .                     UNITAB    , LSUBMODEL)
        ELSE
          !check is IXTG array is used with proper modeling : 2D-TRIA or 3D-SH3N
          IF(NUMELTG>0 .AND. N2D>0 .AND. NUMELTRIA==0)THEN
               CALL ANCMSG(MSGID=66,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     C1='SH3N',
     .                     C2='2D-ANALYSIS'
     .                     )
          ELSEIF(NUMELTG>0 .AND. N2D==0 .AND. NUMELTRIA==NUMELTG)THEN
               CALL ANCMSG(MSGID=66,
     .                     MSGTYPE=MSGERROR,
     .                     ANMODE=ANINFO,
     .                     C1='TRIA',
     .                     C2='3D-ANALYSIS'
     .                     )
          ENDIF
          NUMELTG = 0
        ENDIF
      ENDIF
C--------------------------------------------
C     Check XFEM FLAG
      IF(NUMELTG + NUMELC == 0) ICRACK3D = 0
C--------------------------------------------
C     LECTURE DES ELEMENTS ISO-GEOMETRIQUES
C--------------------------------------------
      NCTRLMAX = 0
      IF(NUMELIG3D/=0)THEN

       SKXIG3D = NIXIG3D*NUMELIG3D
       WRITE(ISTDO,'(A)')TITRE(22)

       CALL PRELECIG3D(SIXIG3D)

       ALLOCATE(KXIG3D(SKXIG3D)    ,STAT=stat)
       IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                            MSGTYPE=MSGERROR,
     .                        C1='KXIG3D')
       ALLOCATE(IXIG3D(SIXIG3D+ADDSIXIG3D)    ,STAT=stat)
       IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                            MSGTYPE=MSGERROR,
     .                        C1='IXIG3D')
       ALLOCATE(TABCONPATCH(NBPART_IG3D),STAT=stat)
       IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                          MSGTYPE=MSGERROR,
     .                         C1='TABCON_PATCH_IG3D')
       KXIG3D = 0
       IXIG3D = 0
       CALL LECIG3D(
     .           ITAB    ,IPART   ,IPARTIG3D  ,IPM     ,IGEO    ,
     .           KXIG3D  ,IXIG3D  ,ITABM1     ,NCTRLMAX,TABCONPATCH)

       SKNOTLOCPC = DEG_MAX*3*(NUMNOD+L_TAB_NEWFCT)*NUMGEO  ! IL FAUDRAIT AVOIR ONE NOMBRE DE POINT IGE ET DES INDICES
cc Sknotlocpc est un peu surdimmensionne car il prend en compte les points de travail temporaire
       IF(SKNOTLOCPC > INTMAX .OR. SKNOTLOCPC < ZERO) THEN ! L_TAB_NEWFCT pour se laisser la taille de travail
         SKNOTLOCPC = INTMAX
       ELSE
         SKNOTLOCPC = INT(SKNOTLOCPC)
       ENDIF
       ALLOCATE(KNOTLOCPC(SKNOTLOCPC)    ,STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                              MSGTYPE=MSGERROR,
     .                          C1='KNOTLOCPC')
       KNOTLOCPC(:)=0

       SKNOTLOCEL = 2*3*NUMELIG3D
       IF(SKNOTLOCEL > INTMAX .OR. SKNOTLOCEL < ZERO) THEN
         SKNOTLOCEL = INTMAX
       ELSE
         SKNOTLOCEL = INT(SKNOTLOCEL)
       ENDIF
       ALLOCATE(KNOTLOCEL(SKNOTLOCEL)    ,STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                              MSGTYPE=MSGERROR,
     .                          C1='KNOTLOCEL')
       KNOTLOCEL(:)=0

       CALL PRERAFIG3D(KNOT,KNOTLOCPC,KNOTLOCEL,
     .                 KXIG3D,IXIG3D,IGEO,
     .                 IPARTIG3D,
     .                 X,V,D,MS,WIGE,TABCONPATCH,1)
c
       SIXIG3D=SIXIG3D+ADDSIXIG3D
       ALLOCATE(MSIG3D(NUMELIG3D*NCTRLMAX)     ,STAT=stat)
       MSIG3D(1:NUMELIG3D*NCTRLMAX) = ZERO
      ELSE
        ALLOCATE(KXIG3D(0)    ,STAT=stat)
        ALLOCATE(MSIG3D(0)    ,STAT=stat)
        ALLOCATE(IXIG3D(0)    ,STAT=stat)
        ALLOCATE(KNOTLOCEL(0) ,STAT=stat)
        ALLOCATE(KNOTLOCPC(0) ,STAT=stat)
      ENDIF
C--------------------------------------------
C     PRELECTURE GRNOD/NODENS
C--------------------------------------------
      IF(NUMELX > 0 .AND. NGRNOD > 0 )THEN
        WRITE(ISTDO,'(A)')' .. NODENS GROUP '
        CALL HM_PRELECGRNS(ITABM1 ,IGRNOD, LSUBMODEL)
      ENDIF
C---------------------------------------------
C      PRELECTURE DES ELEMENTS MULTI-PURPOSE.
C---------------------------------------------
      ERR_MSG='MULTI-PURPOSE ELEMENTS'
      ERR_CATEGORY='MULTI-PURPOSE ELEMENTS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NUMELX > 0) THEN
        SKXX = NIXX*NUMELX
        CALL HM_PREREAD_XELEM(SIXX, IGRNOD,LSUBMODEL)
        ALLOCATE(KXX(SKXX)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='KXX')
        ALLOCATE(IXX(SIXX+150)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IXX')
        KXX = 0
        IXX = 0
        ALLOCATE(LELX(NUMELX)    ,STAT=stat)
        LELX(1:NUMELX) = ZERO
        CALL HM_READ_XELEM(IGRNOD  ,ITAB   ,ITABM1 ,IPART ,IPARTX,
     .              IPM     ,IGEO   ,KXX    ,IXX   ,LSUBMODEL)
      ELSE
        SKXX = 0
        SIXX = 0
        ALLOCATE(KXX(SKXX)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='KXX')
        ALLOCATE(IXX(SIXX)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IXX')
        ALLOCATE(LELX(NUMELX)    ,STAT=stat)
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     ADAPTIVE MESHING
C--------------------------------------------
      ERR_MSG='ADAPTIVE MESHING'
      ERR_CATEGORY='ADAPTIVE MESHING'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      LEVELMAX=0
      LSH4TRIM=0
      LSH3TRIM=0
      IF(NADMESH/=0)THEN

        ALLOCATE(SH4TREE(KSH4TREE,NUMELC),STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='SH4TREE')
        SH4TREE=0
        ALLOCATE(SH3TREE(KSH3TREE,NUMELTG),STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='SH3TREE')
        SH3TREE=0
        ALLOCATE(IPADMESH(KIPADMESH,NPART),STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IPADMESH')
        IPADMESH=0

        ALLOCATE(PADMESH(KPADMESH,NPART),STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='PADMESH')
        PADMESH=ZERO

        CALL SET_ADMESH(IPART ,IPADMESH,PADMESH,UNITAB,LSUBMODEL )

        IF(IADMSTAT/=0)THEN
          LSH4TRIM=NUMELC
          ALLOCATE(SH4TRIM(LSH4TRIM),STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='SH4TRIM')
          SH4TRIM=0
          LSH3TRIM=NUMELTG
          ALLOCATE(SH3TRIM(LSH3TRIM),STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='SH3TRIM')
          SH3TRIM=0
          CALL STATE_ADMESH(
     .                IPART ,IPARTC ,IPARTG ,IXC ,IXTG ,
     .                SH4TREE, SH3TREE, SH4TRIM, SH3TRIM,
     .                LSUBMODEL)
        END IF
        CALL BUILD_ADMESH(
     .               IPART ,IPARTC ,IPARTG ,IXC ,IXTG ,
     .               X     ,ITAB   ,ITABM1  ,SH4TREE, SH3TREE,
     .               IPADMESH,PADMESH)
      ELSE
        ALLOCATE(SH4TREE(0,0))
        ALLOCATE(SH3TREE(0,0))
        ALLOCATE(IPADMESH(0,0))
        ALLOCATE(PADMESH(0,0))
        ALLOCATE(SH4TRIM(0))
        ALLOCATE(SH3TRIM(0))
      END IF

      IF(ISTATCND/=0)THEN
        ALLOCATE(MSCND(NUMNOD),INCND(NUMNOD),STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='MSCND')
        MSCND=ZERO
        INCND=ZERO
      ELSE
        ALLOCATE(MSCND(0),INCND(0))
      END IF
      CALL TRACE_OUT1()

C--------------------------------------------
C     REINITIALISATION MAT/PROP
C--------------------------------------------

      IF(TAILLE2>0) THEN
       ALLOCATE(TAB_UMP_LOC2(7+6,TAILLE2,2),STAT=stat)
       TAB_UMP_LOC2 = 0
       CALL REINI_MATPROP(TAILLE,TAILLE2,TAB_UMP_LOC,TAB_UMP_LOC2,
     .           IXS,IXQ,IXC,IXT,IXP,IXR,
     .           IXTG,EANI,POIN_UMP)

       ALLOCATE( TAB_UMP(7,TAILLE), STAT=stat)
       TAB_UMP = 0
       IF(TAILLE>0) THEN
        CALL REINI_MATPROP2(TAILLE,TAILLE2,
     .          TAB_UMP_LOC,TAB_UMP_LOC2,TAB_UMP,TAB_SOL,
     .           POIN_UMP)
       ENDIF
       DEALLOCATE(TAB_UMP_LOC2)
      ENDIF
      DEALLOCATE(TAB_UMP_LOC)
C--------------------------------------------
C     TABLEAUX X-FEM (SHELL 4-N + SHELL 3-N)
C     xfem for crack propagation (mono + multi layer shells)
C--------------------------------------------
      IF(ICRACK3D > 0) THEN
        NXEL   = 3  ! nb of phantom elements within one layer (change to NXEL=3)
        XFEMON = 1
        IF(IPARI0 /= 1) IPARI0=1 ! force flag parith/on pour XFEM (pareil engine)
      ELSE
        NXEL   = 0
        XFEMON = 0
      ENDIF
C-----
      ERR_MSG='XFEM FOR SHELLS - ALLOCATIONS'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C-----
      LEN = XFEMON*NUMNOD
      ALLOCATE(ADDCNE_CRKXFEM(0:LEN+1),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='ADDCNE_CRKXFEM')
      ADDCNE_CRKXFEM(0:LEN+1) = 0
c
      ALLOCATE(ITAGN(LEN),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='ITAGN')
      ALLOCATE(INOD_CRKXFEM(LEN),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='INOD_CRKXFEM')
      ALLOCATE(IBORDNODE(LEN),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANSTOP,
     .                           MSGTYPE=MSGERROR,C1='IBORDNODE')
      LEN = XFEMON*(NUMELC+NUMELTG)
      SITAGE=LEN
      ALLOCATE(ITAGE(LEN),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='ITAGE')
      ALLOCATE(IEL_CRKXFEM(LEN),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='IEL_CRKXFEM')
c
      ITAGN          = 0
      ITAGE          = 0
      INOD_CRKXFEM   = 0
      IEL_CRKXFEM    = 0
      IBORDNODE      = 0
c
      CALL TRACE_OUT1()
C--------------------------------------------
C     INVERSE CONNECTIVITY (Starter only)
C--------------------------------------------
C Create IXTG1 array and set to 0
      ERR_MSG='CONNECTIVITY'
      ERR_CATEGORY='CONNECTIVITY'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      ! -------------------
      ! initialisation of invert_group structure, used in
      ! HM_READ_SET and in HM_READ_SENSORS for user sensor
      CALL INVERTED_GROUP_INIT(0,INV_GROUP)
      ! example :
      CALL COMPUTE_CONNECT_PARTELM(IPARTS ,IPARTQ ,IPARTC   ,IPARTT ,IPARTP,
     .                             IPARTG ,IPARTR ,INV_GROUP)
      ! -------------------
      IF(NUMELTG6 ==0 )THEN
        SIXTG1  = 0
      ELSE
        SIXTG1 = 4*NUMELTG
      ENDIF
      ALLOCATE(IXTG1(SIXTG1), STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IXTG1')
      IXTG1 = 0

214   ALLOCATE(KNOD2ELS(NUMNOD+1),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='KNOD2ELS')
      KNOD2ELS=0
      ALLOCATE(KNOD2ELC(NUMNOD+1),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR)
      KNOD2ELC=0
      ALLOCATE(KNOD2ELTG(NUMNOD+1),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='KNOD2ELTG')
      KNOD2ELTG=0
      ALLOCATE(KNOD2EL1D(NUMNOD+1),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='KNOD2EL1D')
      KNOD2EL1D=0

      ALLOCATE(KNOD2ELQ(NUMNOD+1),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR)
      KNOD2ELQ=0


      S_NOD2ELS = 8*NUMELS+6*NUMELS10+12*NUMELS20+8*NUMELS16
      ALLOCATE(NOD2ELS(S_NOD2ELS),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='NOD2ELS')
      NOD2ELS=0
      ALLOCATE(NOD2ELC(4*NUMELC),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='NOD2ELC')
      NOD2ELC=0
      S_NOD2ELTG = 3*NUMELTG+3*NUMELTG6
      ALLOCATE(NOD2ELTG(S_NOD2ELTG),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='NOD2ELTG')
      NOD2ELTG=0
      S_NOD2EL1D=2*NUMELT+2*NUMELP+3*NUMELR+2*SIXX
      ALLOCATE(NOD2EL1D(S_NOD2EL1D),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='NOD2EL1D')
      NOD2EL1D=0
      ALLOCATE(KNOD2ELIG3D(NUMNOD+1),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='KNOD2ELIG3D')
      KNOD2ELIG3D=0
      ALLOCATE(NOD2ELIG3D(NCTRLMAX*NUMELIG3D),
     .         STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='NOD2ELIG3D')
      NOD2ELIG3D=0
      ALLOCATE(NOD2ELQ(4*NUMELQ),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='NOD2ELQ')
      NOD2ELQ=0

      IF(FLAG_GOTO==1) GOTO 215

C--------------------------------------------
      CALL BUILD_CNEL(
     2   IXS        ,IXQ        ,IXC        ,IXT      ,IXP      ,
     3   IXR        ,IXTG       ,IXS10      ,IXS20    ,
     4   IXS16      ,IXTG1      ,IGEO       ,KNOD2ELS ,KNOD2ELC ,
     5   KNOD2ELTG  ,NOD2ELS    ,NOD2ELC    ,NOD2ELTG ,NOD2EL1D ,
     6   KNOD2EL1D  ,KXX        ,IXX        ,X        ,LELX     ,
     7   IXIG3D     ,KXIG3D     ,KNOD2ELIG3D,NOD2ELIG3D,KNOD2ELQ,
     8   NOD2ELQ    )
      IF(ALLOCATED(IWORK)) DEALLOCATE(IWORK)
      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES PARAMETRES GLOBAUX + PARTICULES SPH.
C---------------------------------------------
      ERR_MSG='SPH'
      ERR_CATEGORY='SPH'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NSPHSOL/=0)THEN
        ALLOCATE(SPH2SOL(NUMSPH)      ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='SPH2SOL')
        SPH2SOL=0
        ALLOCATE(SOL2SPH(2*NUMELS8)      ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='SOL2SPH')
        SOL2SPH=0
        ALLOCATE(IRST(3*NSPHSOL)      ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IRST')
        IRST=0
        ALLOCATE(SOL2SPH_TYP(NUMELS8)      ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='SOL2SPH_TYP')
        SOL2SPH_TYP=0
      ELSE
        ALLOCATE(SPH2SOL(0)      ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='SPH2SOL')
        ALLOCATE(SOL2SPH(0)      ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='SOL2SPH')
        ALLOCATE(IRST(0)      ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IRST')
        ALLOCATE(SOL2SPH_TYP(0)      ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='SOL2SPH_TYP')
      END IF
      IF(NUMSPH > 0) THEN
        SSPBUF  = NSPBUF*NUMSPH
        SKXSP   = NISP*NUMSPH
        SIXSP   = KVOISPH*NUMSPH
c test with INTEGER 64 bits to avoid integer 32 bits overflow with huge cases (10 Millions SPH cells)
        KVOISPH8 = KVOISPH
        NUMSPH8 = NUMSPH

c limit INTEGER 32 bits ((2^31)-1), we add a security marge of 5%
        SIXSP8 = (NUMSPH8/(NSPMD))*KVOISPH8
        LIMIT8 = (HUGE(INTEGER_LIMIT32)-1)*0.95!((2**31)-1)*0.95
        IF(SIXSP8>LIMIT8)THEN
          CALL ANCMSG(MSGID=981,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP)
        ENDIF

        SNOD2SP = NUMNOD
        ALLOCATE(KXSP(SKXSP)      ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='KXSP')
        ALLOCATE(IXSP(KVOISPH,NUMSPH)      ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IXSP')
        ALLOCATE(NOD2SP(SNOD2SP)  ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='NOD2SP')
        ALLOCATE(SPBUF(SSPBUF)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='SPBUF')

        IF(NSPHIO>0.AND.NBPARTINLET>0)THEN
          ALLOCATE(RESERVEP(NBPARTINLET)    ,STAT=stat)
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,
     .                               ANMODE=ANSTOP,
     .                               MSGTYPE=MSGERROR,
     .                         C1='RESERVEP')
          RESERVEP(1:NBPARTINLET) = ZERO
        ELSE
          ALLOCATE(RESERVEP(1))
          RESERVEP(1)=ZERO
        ENDIF
        KXSP   = 0
        IXSP   = 0
        NOD2SP = 0
        SPBUF  = ZERO
        WRITE(ISTDO,'(A)')' .. SPH PARTICLES DEFINITION'
        CALL HM_READ_SPHCEL(ITAB    ,ITABM1   ,IPART   ,
     2              IPARTSP ,IPM    ,IGEO    ,KXSP    ,IXSP    ,
     3              NOD2SP, RESERVEP,IXS     ,IPARTS  ,EANI    ,
     4              SPH2SOL,SOL2SPH ,IRST    ,X       ,SOL2SPH_TYP,
     5              LSUBMODEL)

      ELSE
        SSPBUF  = 0
        SKXSP   = 0
        SIXSP   = 0
        SNOD2SP = 0
        ALLOCATE(KXSP(SKXSP)      ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='KXSP')
        ALLOCATE(IXSP(0,0)      ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IXSP')
        ALLOCATE(NOD2SP(SNOD2SP)  ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='NOD2SP')
        ALLOCATE(SPBUF(SSPBUF)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='SPBUF')
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES SUBSETS
C--------------------------------------------
      TAGSURFIGE = 0
      SIBUFSSG = 0  ! to be removed
!
      ERR_MSG='SUBSETS'
      ERR_CATEGORY='SUBSETS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NSUBS > 0) THEN
        WRITE(ISTDO,'(A)')' .. SUBSETS'
!
        CALL HM_READ_SUBSET(SUBSETS,IPART,NSUBS,NPART,LSUBMODEL)
        CALL SUBSET_INI(SUBSETS)
!
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES BOXES (BOX et BOX des BOX)
C--------------------------------------------
      CALL STARTIME(19,1)
C--------------------------------------------
      ERR_MSG='BOXES'
      ERR_CATEGORY='BOXES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
      ALLOCATE(IBOX (NBBOX))
      IF(NBBOX > 0) THEN
        WRITE(ISTDO,'(A)')' .. BOX '
c
        CALL HM_READ_BOX(IBOX   ,UNITAB ,ITABM1   ,ISKWN    ,SKEW     ,
     .                   X      ,RTRANS ,LSUBMODEL)
C
      ENDIF
C--------------------
      CALL TRACE_OUT1()
C--------------------------
C    ELEMENT GROUT READING
C--------------------------
      ERR_MSG='GROUPS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
!
      IDXIGECNT= 1
!
      WRITE(ISTDO,'(A)')' .. ELEMENT GROUPS'
      ERR_CATEGORY='ELEMENT GROUPS'
      FLAGG = 0
      IADBOXMAX = 1
C     count group elements
      ALLOCATE(IXS_S(NUMELS),IXS_S_IND(NUMELS),IXQ_S(NUMELQ),
     2         IXQ_S_IND(NUMELQ),IXC_S(NUMELC),IXC_S_IND(NUMELC),
     3         IXT_S(NUMELT),IXT_S_IND(NUMELT),IXP_S(NUMELP),
     4         IXP_S_IND(NUMELP),IXR_S(NUMELR),IXR_S_IND(NUMELR),
     5         IXTG_S(NUMELTG),IXTG_S_IND(NUMELTG))

      CALL LECGROUP(
     1     ITAB    ,ITABM1            ,ISUBMOD ,
     2     X       ,IXS      ,IXQ     ,IXC     ,IXT     ,IXP     ,
     3     IXR     ,IXTG     ,                           IPART   ,
     4     IPARTS  ,IPARTQ   ,IPARTC  ,IPARTT  ,IPARTP  ,IPARTR  ,
     5     IPARTG  ,FLAGG    ,SH4TREE ,SH3TREE ,
     6     SKEW    ,ISKWN    ,UNITAB  ,IBOX    ,
     7     IXS10  ,IXS16   ,IXS20   ,RTRANS,LSUBMODEL,
     8     IXS_S ,IXS_S_IND,IXQ_S,IXQ_S_IND,IXC_S,IXC_S_IND,
     9     IXT_S ,IXT_S_IND,IXP_S,IXP_S_IND,IXR_S,IXR_S_IND,
     A     IXTG_S,IXTG_S_IND,IADBOXMAX,SUBSETS,IGRBRIC,IGRQUAD,
     B     IGRSH4N,IGRSH3N,IGRTRUSS,IGRBEAM,IGRSPRING)
C---
      LENI=MAX(NUMELS,NUMELQ,NUMELC,NUMELT,NUMELP,NUMELR,NUMELTG)
!
      CALL SORTGROUP(
     1     IXS_S ,IXS_S_IND,IXQ_S,IXQ_S_IND,IXC_S,IXC_S_IND,
     2     IXT_S ,IXT_S_IND,IXP_S,IXP_S_IND,IXR_S,IXR_S_IND,
     3     IXTG_S,IXTG_S_IND,IXS,IXQ,IXC,IXT,IXP,IXR,IXTG,LENI)
!
      FLAGG = 1
      CALL LECGROUP(
     1     ITAB    ,ITABM1           ,ISUBMOD ,
     2     X       ,IXS     ,IXQ     ,IXC     ,IXT     ,IXP     ,
     3     IXR     ,IXTG    ,                           IPART   ,
     4     IPARTS  ,IPARTQ  ,IPARTC  ,IPARTT  ,IPARTP  ,IPARTR  ,
     5     IPARTG  ,FLAGG   ,SH4TREE ,SH3TREE ,
     6     SKEW    ,ISKWN   ,UNITAB  ,IBOX    ,
     7     IXS10 ,IXS16,IXS20,RTRANS,LSUBMODEL,
     8     IXS_S ,IXS_S_IND,IXQ_S,IXQ_S_IND,IXC_S,IXC_S_IND,
     9     IXT_S ,IXT_S_IND,IXP_S,IXP_S_IND,IXR_S,IXR_S_IND,
     A     IXTG_S,IXTG_S_IND,IADBOXMAX,SUBSETS,IGRBRIC,IGRQUAD,
     B     IGRSH4N,IGRSH3N,IGRTRUSS,IGRBEAM,IGRSPRING)
!
      DEALLOCATE(IXS_S ,IXS_S_IND,IXQ_S,IXQ_S_IND,IXC_S,IXC_S_IND,
     2     IXT_S ,IXT_S_IND,IXP_S,IXP_S_IND,IXR_S,IXR_S_IND,
     3     IXTG_S,IXTG_S_IND)
C--------------------------------------------
C    LECTURE DES GROUPES DE PARTS (1st LEVEL)
C--------------------------------------------
      WRITE(ISTDO,'(A)')' .. PART GROUPS'
      ERR_CATEGORY='PART GROUPS'

      FLAGG = 0 !TAG ENTITY & ALLOCATE
      CALL HM_READ_GRPART(IGRPART , IPART, ISUBMOD, FLAGG ,NGRPART,LSUBMODEL, SUBSETS )

      FLAGG = 1 !BUILD GROUPS
      CALL HM_READ_GRPART(IGRPART , IPART, ISUBMOD, FLAGG ,NGRPART,LSUBMODEL, SUBSETS )
C--------------------------------------------
C    LECTURE DES GROUPES DES GROUPES
C--------------------------------------------
      ERR_CATEGORY='GROUP OF GROUPS'
      ICOUNT = 1
      ITER   = 0
      DO WHILE (ICOUNT > 0)
        ITER  = ITER  + 1
        FLAGG = 0
C---
        CALL LECGGROUP(
     .       FLAGG   ,
     .       ICOUNT  ,ITER    ,IGRBRIC,IGRQUAD  ,IGRSH4N,
     .       IGRSH3N ,IGRTRUSS,IGRBEAM,IGRSPRING,IGRPART,
     .       LSUBMODEL)
C---
        FLAGG = 1
C---
        CALL LECGGROUP(
     .       FLAGG   ,
     .       ICOUNT  ,ITER    ,IGRBRIC,IGRQUAD  ,IGRSH4N,
     .       IGRSH3N ,IGRTRUSS,IGRBEAM,IGRSPRING,IGRPART,
     .       LSUBMODEL)
      ENDDO
C--------------------------------------------
C    LECTURE DES SURFACES
C--------------------------------------------

      ! allocation for pre-read of Rbody needed for /SET
      IF(NRBODY > 0) THEN
        ALLOCATE(RBY_MSN(2,NRBODY))
        CALL PREREAD_RBODY_SET(LSUBMODEL,ITABM1,RBY_MSN)
      ELSE
        ALLOCATE(RBY_MSN(0,0))
      ENDIF

      ! PART UID to Internal ID conversion

      CALL CREATE_MAP_TABLES ( MAP_TABLES ,1     ,
     *                         LSUBMODEL ,SUBSETS,
     *                         IPART,
     *                         IXS  ,IXQ  ,IXC   ,IXTG ,
     *                         IXT  ,IXP  ,IXR   ,KXSP,IBID,
     *                         RBY_MSN)


      SBUFSF = 0
      IF(NSURF > 0)THEN
        WRITE(ISTDO,'(A)')' .. SURFACES '
        ERR_CATEGORY='SURFACES DEFINITION'
        ALLOCATE(RWORK(LISURF1*NSURF)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='RWORK')
c
C- Isogeometric Elements
        IADTABIGE = 0
        DECALIGEO  = 0
        IDXIGE1 = 0
        IDXIGE2 = 1
        RNIGE=(1+NSURF*NUMELIG3D*16*6)
        IF(RNIGE > INTMAX .OR. RNIGE < ZERO) THEN
          SNIGE = INTMAX
        ELSE
        SNIGE = INT(RNIGE)
        ENDIF
        ALLOCATE(NIGE_TMP(IDXIGE1)%ptr(SNIGE)   ,STAT=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='NIGE')
        ENDIF
        RRIGE=(1+NSURF*NUMELIG3D*3*16*6)
        IF(RRIGE > INTMAX .OR. RRIGE < ZERO) THEN
          SRIGE = INTMAX
        ELSE
          SRIGE = INT(RRIGE)
        ENDIF
        ALLOCATE(RIGE_TMP(IDXIGE1)%ptr2(SRIGE)   ,STAT=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='RIGE')
        ENDIF
        RXIGE=(1+NSURF*NUMELIG3D*3*16*6)
        IF(RXIGE > INTMAX .OR. RXIGE < ZERO) THEN
          SXIGE = INTMAX
        ELSE
          SXIGE = INT(RXIGE)
        ENDIF
        ALLOCATE(XIGE_TMP(IDXIGE1)%ptr2(SXIGE)   ,STAT=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='XIGEO')
        ENDIF
        RVIGE=(1+NSURF*NUMELIG3D*3*16*6)
        IF(RVIGE > INTMAX .OR. RVIGE < ZERO) THEN
          SVIGE = INTMAX
        ELSE
          SVIGE = INT(RVIGE)
        ENDIF
        ALLOCATE(VIGE_TMP(IDXIGE1)%ptr2(SVIGE)   ,STAT=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='VIGEO')
        ENDIF
C
        SNIGE = 0
        SRIGE = 0
        SXIGE = 0
        SVIGE = 0
        RWORK  = ZERO
        IDXIGECNT = IDXIGECNT + 1
        IDXIGE1   = MOD(IDXIGECNT,2)
        IDXIGE2   = MOD(IDXIGECNT+1,2)
        FLAGG  = 0
        INSEG    = 0
        NUMFAKENODIGEO = 0
        IADBOXMAX = 1
C
        CALL HM_READ_SURF(
     1       ITAB      ,ITABM1    ,
     2       IGRSURF   ,IXS       ,IXQ       ,IXC       ,IXT       ,
     3       IXP       ,IXR       ,IXTG
     4                 ,IPART     ,IPARTS    ,IPARTQ    ,IPARTC    ,
     5       IPARTT    ,IPARTP    ,IPARTR    ,IPARTG    ,X         ,
     6       SBUFSF    ,ISKWN     ,SKEW      ,
     7       RWORK     ,KNOD2ELS  ,NOD2ELS   ,SH4TREE   ,SH3TREE   ,
     8       ISUBMOD   ,FLAGG     ,UNITAB    ,IBOX      ,
     9       IXS10     ,IXS16     ,IXS20     ,RTRANS,
     A       LSUBMODEL ,KNOD2ELC  ,NOD2ELC   ,KNOD2ELTG  ,NOD2ELTG,
     B       KXIG3D    ,IXIG3D    ,IPARTIG3D ,
     C       KNOT      ,IGEO      ,WIGE      ,KNOD2ELIG3D,NOD2ELIG3D,
     D       V         ,NIGE_TMP(IDXIGE1)%ptr,
     E       RIGE_TMP(IDXIGE1)%ptr2,XIGE_TMP(IDXIGE1)%ptr2,
     F       VIGE_TMP(IDXIGE1)%ptr2,IADTABIGE,DECALIGEO,IADBOXMAX,KNOD2ELQ,
     G       NOD2ELQ   ,SUBSETS   ,IGRBRIC   ,IGRSH4N   ,IGRSH3N   ,
     F       KNOTLOCPC ,KNOTLOCEL ,NSETS     ,MAP_TABLES)
C
C- Isogeometric Elements
        ALLOCATE(NIGE_TMP(IDXIGE2)%ptr(SNIGE+NUMFAKENODIGEO)   ,STAT=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANSTOP,
     .                  C1='NIGE')
        ENDIF
        SNIGE = SNIGE + NUMFAKENODIGEO
        DEALLOCATE(NIGE_TMP(IDXIGE1)%ptr)

        ALLOCATE(RIGE_TMP(IDXIGE2)%ptr2(SRIGE+3*NUMFAKENODIGEO)   ,STAT=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='RIGE')
        ENDIF
        SRIGE = SRIGE + 3*NUMFAKENODIGEO
        DEALLOCATE(RIGE_TMP(IDXIGE1)%ptr2)

        ALLOCATE(XIGE_TMP(IDXIGE2)%ptr2(SXIGE+3*NUMFAKENODIGEO)   ,STAT=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='XIGE')
        ENDIF
        SXIGE = SXIGE + 3*NUMFAKENODIGEO
        DEALLOCATE(XIGE_TMP(IDXIGE1)%ptr2)

        ALLOCATE(VIGE_TMP(IDXIGE2)%ptr2(SVIGE+3*NUMFAKENODIGEO)   ,STAT=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='VIGE')
        ENDIF
        SVIGE = SVIGE + 3*NUMFAKENODIGEO
        DEALLOCATE(VIGE_TMP(IDXIGE1)%ptr2)
C
C       fill, 1st level surfaces
        FLAGG = 1
        INSEG = 0
        CALL HM_READ_SURF(
     1       ITAB      ,ITABM1    ,
     2       IGRSURF   ,IXS       ,IXQ       ,IXC       ,IXT       ,
     3       IXP       ,IXR       ,IXTG
     4                 ,IPART     ,IPARTS    ,IPARTQ    ,IPARTC    ,
     5       IPARTT    ,IPARTP    ,IPARTR    ,IPARTG    ,X         ,
     6       SBUFSF    ,ISKWN     ,SKEW      ,
     7       RWORK     ,KNOD2ELS  ,NOD2ELS   ,SH4TREE   ,SH3TREE   ,
     8       ISUBMOD   ,FLAGG     ,UNITAB    ,IBOX      ,
     9       IXS10     ,IXS16     ,IXS20     ,RTRANS,
     A       LSUBMODEL ,KNOD2ELC  ,NOD2ELC   ,KNOD2ELTG  ,NOD2ELTG ,
     B       KXIG3D    ,IXIG3D    ,IPARTIG3D ,
     C       KNOT      ,IGEO      ,WIGE      ,KNOD2ELIG3D,NOD2ELIG3D,
     D       V         ,NIGE_TMP(IDXIGE2)%ptr,
     E       RIGE_TMP(IDXIGE2)%ptr2,XIGE_TMP(IDXIGE2)%ptr2,
     F       VIGE_TMP(IDXIGE2)%ptr2,IADTABIGE,DECALIGEO,IADBOXMAX,KNOD2ELQ,
     G       NOD2ELQ   ,SUBSETS   ,IGRBRIC   ,IGRSH4N   ,IGRSH3N   ,
     H       KNOTLOCPC ,KNOTLOCEL ,NSETS     ,MAP_TABLES)
C
c        IF(NUMELIG3D>0) THEN
        IF(NUMFAKENODIGEO>0) THEN
        ALLOCATE(PERMIGE(NUMFAKENODIGEO)   ,STAT=stat)
c        ALLOCATE(PERMIGE(IADTABIGE)   ,STAT=stat)
c
        CALL PRESEARCHIGEO3D(IGRSURF,XIGE_TMP(IDXIGE2)%ptr2,PERMIGE)
c        CALL MYQSORT3D(IADTABIGE,XIGE_TMP(IDXIGE2)%ptr2,PERMIGE)
c
        ALLOCATE(NIGE_TMP(IDXIGE1)%ptr(SNIGE)   ,STAT=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='NIGE')
        ENDIF

        ALLOCATE(RIGE_TMP(IDXIGE1)%ptr2(SRIGE)   ,STAT=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='RIGE')
        ENDIF

        ALLOCATE(XIGE_TMP(IDXIGE1)%ptr2(SXIGE)   ,STAT=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='XIGE')
        ENDIF

        ALLOCATE(VIGE_TMP(IDXIGE1)%ptr2(SVIGE)   ,STAT=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='VIGE')
        ENDIF
c
c        CALL SEARCHIGEO3D2(IGRSURF   ,IADTABIGE   ,PERMIGE  ,
c     .                    NIGE_TMP(IDXIGE2)%ptr   ,NIGE_TMP(IDXIGE1)%ptr,
c     .                    RIGE_TMP(IDXIGE2)%ptr2  ,RIGE_TMP(IDXIGE1)%ptr2,
c     .                    XIGE_TMP(IDXIGE2)%ptr2  ,XIGE_TMP(IDXIGE1)%ptr2,
c     .                    VIGE_TMP(IDXIGE2)%ptr2  ,VIGE_TMP(IDXIGE1)%ptr2,
c     .                    NDOUBLONIGE)

        CALL SEARCHIGEO3D(IGRSURF   ,IADTABIGE   ,PERMIGE  ,
     .                    NIGE_TMP(IDXIGE2)%ptr   ,NIGE_TMP(IDXIGE1)%ptr,
     .                    RIGE_TMP(IDXIGE2)%ptr2  ,RIGE_TMP(IDXIGE1)%ptr2,
     .                    XIGE_TMP(IDXIGE2)%ptr2  ,XIGE_TMP(IDXIGE1)%ptr2,
     .                    VIGE_TMP(IDXIGE2)%ptr2  ,VIGE_TMP(IDXIGE1)%ptr2,
     .                    NDOUBLONIGE)

c
        DEALLOCATE(RIGE_TMP(IDXIGE2)%ptr2,XIGE_TMP(IDXIGE2)%ptr2,VIGE_TMP(IDXIGE2)%ptr2)
c
        SNIGE = NUMFAKENODIGEO
        SRIGE = 3*NUMFAKENODIGEO
        SXIGE = 3*NUMFAKENODIGEO
        SVIGE = 3*NUMFAKENODIGEO

c        SNIGE = SNIGE - NDOUBLONIGE
c        SRIGE = SRIGE - 3*NDOUBLONIGE
c        SXIGE = SXIGE - 3*NDOUBLONIGE
c        SVIGE = SVIGE - 3*NDOUBLONIGE
c
        ALLOCATE(NIGE_TMP(IDXIGE2)%ptr(SNIGE)   ,STAT=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='NIGE')
        ENDIF
        DO I=1,SNIGE
           NIGE_TMP(IDXIGE2)%ptr(I) = NIGE_TMP(IDXIGE1)%ptr(I)
        ENDDO
c
        ALLOCATE(RIGE_TMP(IDXIGE2)%ptr2(SRIGE)   ,STAT=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='RIGE')
        ENDIF
        DO I=1,SRIGE
           RIGE_TMP(IDXIGE2)%ptr2(I) = RIGE_TMP(IDXIGE1)%ptr2(I)
        ENDDO
c
        ALLOCATE(XIGE_TMP(IDXIGE2)%ptr2(SXIGE)   ,STAT=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='XIGE')
        ENDIF
        DO I=1,SXIGE
           XIGE_TMP(IDXIGE2)%ptr2(I) = XIGE_TMP(IDXIGE1)%ptr2(I)
        ENDDO
c
        ALLOCATE(VIGE_TMP(1)%ptr2(SVIGE)   ,STAT=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='VIGE')
        ENDIF
        DO I=1,SVIGE
           VIGE_TMP(IDXIGE2)%ptr2(I) = VIGE_TMP(IDXIGE1)%ptr2(I)
        ENDDO
c
        TAGSURFIGE=1
        DEALLOCATE(RIGE_TMP(IDXIGE1)%ptr2,XIGE_TMP(IDXIGE1)%ptr2,
     .             VIGE_TMP(IDXIGE1)%ptr2)
        DEALLOCATE(PERMIGE)
c
      ENDIF
c
C-------
C       LECTURE DES SURFACES DES SURFACES
C-------
        ICOUNT = 1
        ITER   = 0
        DO WHILE (ICOUNT == 1)
          FLAGG = 0
          ITER  = ITER  + 1
          INSEG = 0
C---      count next level
          CALL HM_READ_SURFSURF(IGRSURF, INSEG, FLAGG, ICOUNT, ITER, NSETS, LSUBMODEL)
C---
C-------------------------------------------------
          FLAGG = 1
C---      fill next level
          CALL HM_READ_SURFSURF(IGRSURF, INSEG, FLAGG, ICOUNT, ITER, NSETS, LSUBMODEL)
C---
        ENDDO
        ALLOCATE(BUFSF(SBUFSF)       ,STAT=stat)
        IF(STAT /= 0) THEN
          CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='BUFSF')
        ENDIF
        IF(SBUFSF > 0) BUFSF = RWORK(1:SBUFSF)
        IF(ALLOCATED(RWORK)) DEALLOCATE(RWORK)
      ELSE
        ALLOCATE(BUFSF(SBUFSF)       ,STAT=stat)
      ENDIF

      NSEGS=NPART
!
      NSEGSMAX=0
      DO I = 1,NSURF
        NSEGS=NSEGS+IGRSURF(I)%NSEG
      ENDDO
      DO I = 1,NGRNOD
        NSEGSMAX= MAX(NSEGSMAX,IGRNOD(I)%NENTITY)
      ENDDO
      DO I = 1,NGRBRIC
        NSEGSMAX= MAX(NSEGSMAX,IGRBRIC(I)%NENTITY)
      ENDDO
      DO I = 1,NGRQUAD
        NSEGSMAX= MAX(NSEGSMAX,IGRQUAD(I)%NENTITY)
      ENDDO
      DO I = 1,NGRSHEL
        NSEGSMAX= MAX(NSEGSMAX,IGRSH4N(I)%NENTITY)
      ENDDO
      DO I = 1,NGRSH3N
        NSEGSMAX= MAX(NSEGSMAX,IGRSH3N(I)%NENTITY)
      ENDDO
      DO I = 1,NGRTRUS
        NSEGSMAX= MAX(NSEGSMAX,IGRTRUSS(I)%NENTITY)
      ENDDO
      DO I = 1,NGRBEAM
        NSEGSMAX= MAX(NSEGSMAX,IGRBEAM(I)%NENTITY)
      ENDDO
      DO I = 1,NGRSPRI
        NSEGSMAX= MAX(NSEGSMAX,IGRSPRING(I)%NENTITY)
      ENDDO
      DO I = 1,NGRPART
        NSEGSMAX= MAX(NSEGSMAX,IGRPART(I)%NENTITY)
      ENDDO
      NSEGS=NSEGS+NSEGSMAX
C--------------------------------------------
C     LECTURE DES LIGNES
C--------------------------------------------
      IF(NSLIN > 0) THEN
        WRITE(ISTDO,'(A)')' .. LINES '
        ERR_CATEGORY='LINES'
        FLAGG = 0
        IADBOXMAX = 1
!
        CALL HM_READ_LINES(
     1           ITAB    ,ITABM1  ,
     2           ISUBMOD ,IGRSLIN ,IGRSURF ,X       ,IXS    ,
     3           IXQ     ,IXC     ,IXT     ,IXP     ,IXR    ,
     4           IXTG    ,IPART   ,IPARTS  ,IPARTQ  ,IPARTC ,
     5           IPARTT  ,IPARTP  ,IPARTR  ,IPARTG  ,
     6           NSEGS   ,         FLAGG   ,SKEW    ,ISKWN  ,
     7           UNITAB  ,IBOX    ,RTRANS  ,LSUBMODEL,
     8           IPARTX  ,KXX     ,IXX     ,IADBOXMAX,SUBSETS,
     9           IGRTRUSS,IGRBEAM,IGRSPRING,NSETS   ,MAP_TABLES)
C---
        FLAGG = 1
C---
        CALL HM_READ_LINES(
     1           ITAB    ,ITABM1  ,
     2           ISUBMOD ,IGRSLIN ,IGRSURF ,X       ,IXS    ,
     3           IXQ     ,IXC     ,IXT     ,IXP     ,IXR    ,
     4           IXTG    ,IPART   ,IPARTS  ,IPARTQ  ,IPARTC ,
     5           IPARTT  ,IPARTP  ,IPARTR  ,IPARTG  ,
     6           NSEGS   ,         FLAGG   ,SKEW    ,ISKWN  ,
     7           UNITAB  ,IBOX    ,RTRANS  ,LSUBMODEL,
     8           IPARTX  ,KXX     ,IXX     ,IADBOXMAX,SUBSETS,
     9           IGRTRUSS,IGRBEAM,IGRSPRING,NSETS   ,MAP_TABLES)
C-------
C       LECTURE DES LIGNES DES LIGNES
C-------
        ICOUNT = 1
        ITER   = 0
        DO WHILE (ICOUNT == 1)
          ITER  = ITER  + 1
          INSEG = 0
          FLAGG = 0
C---      count next level
          CALL HM_LINES_OF_LINES(IGRSLIN ,INSEG   ,FLAGG ,ICOUNT ,ITER ,NSETS, LSUBMODEL)
C---      fill next level
          FLAGG = 1
          CALL HM_LINES_OF_LINES(IGRSLIN ,INSEG   ,FLAGG ,ICOUNT ,ITER ,NSETS, LSUBMODEL)
C---
        ENDDO
      ENDIF
C--------------------------------------------
C     LECTURE DES INIITAL CRACKS
C--------------------------------------------
      IF(NINICRACK > 0) THEN
        WRITE(ISTDO,'(A)')' .. INITIAL CRACK '
        ERR_CATEGORY='INITIAL CRACKS'
        SILEVSET = NINICRACK
        ALLOCATE(INICRACK(SILEVSET)     ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='INICRACK')
        ! Reading /INICRACK cards
        CALL HM_READ_INICRACK(ITABM1   ,INICRACK ,UNITAB   ,LSUBMODEL)
C---
      ELSE
        SILEVSET = 0
        ALLOCATE(INICRACK(SILEVSET))
      ENDIF
C
C--------------------------------------------
C     LECTURE DES GROUPES DE NOEUDS
C--------------------------------------------
      IF(NGRNOD > 0)THEN
        WRITE(ISTDO,'(A)')' .. NODE GROUP'
        ERR_CATEGORY='NODE GROUPS'
        INNOD = 0
        MAXNNOD = 1
        IADBOXMAX = 1

        FLAGG = 0 !TAGNODES & ALLOCATE
        CALL HM_LECGRN(
     1           ITAB    ,ITABM1  ,IGRNOD  ,
     2           ISUBMOD ,X       ,GEO     ,IXS     ,
     3           IXQ     ,IXC     ,IXT     ,IXP     ,IXR     ,
     4           IXTG                               ,IPART   ,
     5           IPARTS  ,IPARTQ  ,IPARTC  ,IPARTT  ,IPARTP  ,
     6           IPARTR  ,IPARTG  ,IPARTSP ,KXSP    ,
     7           FLAGG   ,MAXNNOD ,SKEW    ,ISKWN   ,
     8           UNITAB  ,IBOX    ,IXS10   ,IXS20   ,
     9           IXS16   ,RTRANS ,LSUBMODEL,IXX,
     A           KXX     ,IPARTX  ,IADBOXMAX,IGRSLIN,SUBSETS ,
     B           IGRBRIC ,IGRQUAD ,IGRSH4N ,IGRSH3N ,IGRTRUSS,
     C           IGRBEAM ,IGRSPRING,IGRSURF,NSETS   )
!
        FLAGG = 1 !BUILD GROUPS
        CALL HM_LECGRN(
     1           ITAB    ,ITABM1  ,IGRNOD  ,
     2           ISUBMOD ,X       ,GEO     ,IXS     ,
     3           IXQ     ,IXC     ,IXT     ,IXP     ,IXR     ,
     4           IXTG                               ,IPART   ,
     5           IPARTS  ,IPARTQ  ,IPARTC  ,IPARTT  ,IPARTP  ,
     6           IPARTR  ,IPARTG  ,IPARTSP ,KXSP    ,
     7           FLAGG   ,MAXNNOD ,SKEW    ,ISKWN   ,
     8           UNITAB  ,IBOX    ,IXS10   ,IXS20   ,
     9           IXS16   ,RTRANS ,LSUBMODEL,IXX,
     A           KXX     ,IPARTX  ,IADBOXMAX,IGRSLIN,SUBSETS ,
     B           IGRBRIC ,IGRQUAD ,IGRSH4N ,IGRSH3N ,IGRTRUSS,
     C           IGRBEAM ,IGRSPRING,IGRSURF,NSETS   )

C-------
C       LECTURE DES GROUPES DES GROUPES
        ICOUNT = 1
        ITER   = 0
        DO WHILE (ICOUNT == 1)
          ITER  = ITER  + 1
          FLAGG = 0
          CALL HM_GROGRONOD(IGRNOD ,ICOUNT ,FLAGG  ,ITER,'NODE',LSUBMODEL)
C---      fill next level
          FLAGG =1
          CALL HM_GROGRONOD(IGRNOD ,ICOUNT ,FLAGG  ,ITER,'NODE',LSUBMODEL)
C---
        ENDDO
      ENDIF

C--------------------------------------------
      CALL STOPTIME(19,1)
C--------------------------------------------
C     /SET
C--------------------------------------------
      CALL STARTIME(17,1)

      ALLOCATE(SET (NSETS))
      IF(NSETS > 0)THEN
        WRITE(ISTDO,'(A)')' .. SET'
        ERR_CATEGORY='SET'


        CALL HM_SET(SET      ,LSUBMODEL ,INV_GROUP ,MAP_TABLES,IPART     ,
     *              IGRSURF  ,IGRNOD    ,IGRSLIN   ,IGRPART  ,IGRBRIC   ,
     *              IGRQUAD  ,IGRSH4N   ,IGRSH3N   ,IGRTRUSS ,IGRBEAM   ,
     *              IGRSPRING,IXS       ,IXS10     ,IXC      ,IXTG      ,
     *              KNOD2ELS ,NOD2ELS   ,KNOD2ELC  ,NOD2ELC  ,KNOD2ELTG ,
     *              NOD2ELTG ,IPARTC    ,IPARTG    ,IPARTS   ,SH4TREE   ,
     *              SH3TREE  ,IXQ	,KNOD2ELQ  ,NOD2ELQ  ,X 	,
     *              IXT      ,IXP       ,IXR       ,IXX      ,KXX       ,
     *              KXSP     ,IXS20     ,IXS16     ,GEO      ,ITABM1    ,
     *              IBOX     ,SKEW      ,IPARTQ    ,IPARTT   ,IPARTP    ,
     *              IPARTR   ,SUBSETS   ,RBY_MSN   )

C        IF(ALLOCATED(RBY_MSN)) DEALLOCATE(RBY_MSN)
      ENDIF
C--------------------------------------------
      CALL STOPTIME(17,1)
C--------------------------------------------
C     LECTURE DES DRAPES
C--------------------------------------------
      ERR_MSG='DRAPE'
      ERR_CATEGORY='DRAPE'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
       NUMELC_DRAPE = 0
       NUMELTG_DRAPE = 0
      IF(NDRAPE > 0) THEN
        WRITE(ISTDO,'(A)')' .. DRAPE'
        ALLOCATE(DRAPE_WRK(NUMELC + NUMELTG),DRAPEG%INDX(NUMELC + NUMELTG))
        DRAPEG%INDX = 0  
        ALLOCATE(INDXSH(NUMELC + NUMELTG))
        INDXSH = 0
C-----------------
C     Stack part Pre orginisation
C-------------------------
        NUMELC_DRAPE = 0
        NUMELTG_DRAPE = 0
        STDRAPE = 0
        SCDRAPE = 0
        IF(IPART_STACK > 0 .OR. IPART_PCOMPP > 0) THEN
          ALLOCATE(IWORK_T(NUMELC+NUMELTG))
          CALL PRE_STACKGROUP(
     .                   IGRSH3N    ,IGRSH4N    ,IXC        ,IXTG      ,
     .                   IGEO       ,GEO        ,IGEO_STACK ,IWORKSH    ,
     .                   IWORK_T  )
        ENDIF        
      !!   
         CALL HM_READ_DRAPE(DRAPE_WRK ,IWORK_T       ,IWORKSH   ,IGRSH3N   ,IGRSH4N  ,
     .                     IXC       ,IXTG           ,IGEO       ,IGEO_STACK,LSUBMODEL,
     .                     UNITAB    ,INDXSH         )
          IF( NUMELC_DRAPE  > 0) SCDRAPE = NUMELC
          IF( NUMELTG_DRAPE > 0) STDRAPE = NUMELTG
          IF( (NUMELC_DRAPE + NUMELTG_DRAPE )> 0) THEN
            ALLOCATE(DRAPE(NUMELC_DRAPE +NUMELTG_DRAPE) ) 
            DRAPEG%NUMSH4 = NUMELC_DRAPE
            DRAPEG%NUMSH3 = NUMELTG_DRAPE 
            DO I=1,NUMELC_DRAPE + NUMELTG_DRAPE
               IDSHEL = INDXSH(I)  
               NPT_DRAPE = DRAPE_WRK(IDSHEL)%NPLY_DRAPE  
               DRAPE(I)%NPLY_DRAPE = NPT_DRAPE 
               NPT = IWORKSH(1,IDSHEL) 
               DRAPE(I)%NPLY = NPT 
               DRAPEG%INDX(IDSHEL) = I
               ALLOCATE(DRAPE(I)%DRAPE_PLY(NPT_DRAPE))                                                        
               ALLOCATE(DRAPE(I)%INDX_PLY(NPT))                                                          
               DRAPE(I)%INDX_PLY = 0
               DO JJ =1,NPT_DRAPE                                                                                    
                IP = DRAPE_WRK(IDSHEL)%INDX_PLY(JJ)                                                                 
                DRAPE(I)%INDX_PLY(IP) = JJ                                                                      
                NSLICE = DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%NSLICE   
                DRAPE(I)%DRAPE_PLY(JJ)%NSLICE = NSLICE   
                DRAPE(I)%DRAPE_PLY(JJ)%IPID = DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IPID 
                ALLOCATE(DRAPE(I)%DRAPE_PLY(JJ)%RDRAPE(NSLICE,2))                                        
                ALLOCATE(DRAPE(I)%DRAPE_PLY(JJ)%IDRAPE(NSLICE,2))   
                DO ISL = 1,NSLICE                                                                                     
                   DRAPE(I)%DRAPE_PLY(JJ)%RDRAPE(ISL,1)  = DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE(ISL,1)           
                   DRAPE(I)%DRAPE_PLY(JJ)%RDRAPE(ISL,2)  = DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE(ISL,2)           
                   DRAPE(I)%DRAPE_PLY(JJ)%IDRAPE(ISL,1)  = DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE(ISL,1)           
                   DRAPE(I)%DRAPE_PLY(JJ)%IDRAPE(ISL,2)  = DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE(ISL,2) 
                ENDDO ! nbre of slice
             ENDDO                                                                                  
           ENDDO 
           !! Dallocation of work drape momory 
           DO I=1,NUMELC_DRAPE + NUMELTG_DRAPE
               IDSHEL = INDXSH(I)                                                 
               NPT = IWORKSH(1,IDSHEL)  
               IF(ALLOCATED(DRAPE_WRK(IDSHEL)%DRAPE_PLY)) THEN  
                 NPT_DRAPE = DRAPE_WRK(IDSHEL)%NPLY_DRAPE                                                                         
                 DO JJ=1,NPT_DRAPE 
                     IP = DRAPE_WRK(IDSHEL)%INDX_PLY(JJ)                          
                     DEALLOCATE(DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%RDRAPE,DRAPE_WRK(IDSHEL)%DRAPE_PLY(IP)%IDRAPE)
                  ENDDO
                  DEALLOCATE(DRAPE_WRK(IDSHEL)%DRAPE_PLY) 
               ENDIF     
           ENDDO 
           DEALLOCATE(DRAPE_WRK)
           DEALLOCATE(INDXSH )
        ENDIF ! NUMSH3+NUMSH4
        IF(ALLOCATED(IDRAPEID)) DEALLOCATE(IDRAPEID)
     
         IF(IPART_STACK > 0 .OR. IPART_PCOMPP > 0) THEN
            CALL STACKGROUP_DRAPE(DRAPE, DRAPEG , IWORK_T   , IWORKSH  ,
     .                   IGRSH3N    ,IGRSH4N    ,IXC        ,IXTG      ,
     .                   IGEO       ,GEO        ,THKE       , STACK    ,
     .                   IGEO_STACK ,GEO_STACK , STACK_INFO ,
     .                   NUMGEOSTACK,NPROP_STACK,PLY_INFO)
         ELSE
            ALLOCATE(STACK%GEO(0,0))
            ALLOCATE(STACK%IGEO(0,0))
            ALLOCATE(STACK%PM(0,0))
         ENDIF        
      ELSE ! with out drape
        ALLOCATE(DRAPE(0))
        ALLOCATE(DRAPEG%INDX(0))
        IF(IPART_STACK > 0 .OR. IPART_PCOMPP > 0) THEN
            CALL STACKGROUP(
     .                   IGRSH3N    ,IGRSH4N    ,IXC        ,IXTG      ,
     .                   IGEO       ,GEO        ,IWORKSH    ,THKE      ,
     .                   STACK      ,IPM        ,IGEO_STACK ,GEO_STACK ,
     .                   STACK_INFO ,NUMGEOSTACK,NPROP_STACK)
         ELSE
            ALLOCATE(STACK%GEO(0,0))
            ALLOCATE(STACK%IGEO(0,0))
            ALLOCATE(STACK%PM(0,0))
         ENDIF  
      ENDIF
C--------------------------------------------
      IF(NSUBDOM==0) GOTO 218
C--------------------------------------------
C     MULTIDOMAINS - INTERFACES
C--------------------------------------------
         WRITE(ISTDO,'(A)')' .. MULTIDOMAINS INTERFACES DETECTION '
         IDXCNT= 1  ! used by temporary local array "IBUFTMP"
         IADBUF= 1
         IDXCNT= IDXCNT + 1
         IDX1  = MOD(IDXCNT,2)
         IDX2  = MOD(IDXCNT+1,2)
         FLAGG = 0
         INNOD = 0
         FLG_R2R_ERR = 0
         ALE_EULER = 0
         NSPCOND0 = NSPCOND
! tmp +++
         RSIBUFSSG=NUMNOD+NSUBDOM
         IF(RSIBUFSSG > INTMAX .OR. RSIBUFSSG < ZERO) THEN
           SIBUFSSG = INTMAX
         ELSE
           SIBUFSSG = INT(RSIBUFSSG)
         ENDIF
         ALLOCATE(IBUFTMP(IDX1)%ptr(SIBUFSSG),STAT=stat)
          IF(STAT /= 0) THEN
            CALL ANCMSG(MSGID=727,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANSTOP,
     .                C1='BUFFSG')
         ENDIF
! tmp ---
C---
         ALLOCATE(TAGNO(2*NUMNOD+NPART),STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='TAGNO')
         TAGNO(:) = 0
         ALLOCATE(NALE_R2R(ALE%GLOBAL%SNALE),STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='NALE_R2R')
         NALE_R2R(:) = 1
         ALLOCATE(DT_R2R(4*NSUBDOM),STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='DT_R2R')
         DT_R2R(:) = 0
C---  Premiere Passe -> comptage
         CALL R2R_GROUP(NGRNOD,
     1        INNOD,FLAGG,IPARTS,IPARTQ,IPARTC,IPARTT,IPARTP,
     2        IPARTR,IPARTG,IPARTSP,IXS10,IXS20,IXS16,1,
     3        IBUFTMP(IDX1)%ptr,IXR_KJ,INOM_OPT,IPART,
     4        IADBUF,NALE_R2R,FLG_R2R_ERR ,
     5        STACK%PM ,IWORKSH  ,IGRBRIC ,IGRQUAD   ,IGRSH4N ,
     6        IGRSH3N  ,IGRTRUSS ,IGRBEAM ,IGRSPRING ,IGRNOD  ,
     7        IGRSURF  ,IGRSLIN, LSUBMODEL,ALE_EULER ,IGEO    ,
     8        NLOC_DMG ,DETONATORS,SENSORS%NSENSOR,SEATBELT_SHELL_TO_SPRING,
     9        NB_SEATBELT_SHELLS)
C---
         ALLOCATE(IBUFTMP(IDX2)%ptr(SIBUFSSG+INNOD)   ,STAT=stat)
         IF(STAT /= 0) THEN
            CALL ANCMSG(MSGID=727,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANSTOP,
     .                  C1='BUFFSG')
         ENDIF
         IBUFTMP(IDX2)%ptr = 0
         DO I=1,SIBUFSSG
            IBUFTMP(IDX2)%ptr(I) = IBUFTMP(IDX1)%ptr(I)
         ENDDO
         SIBUFSSG = SIBUFSSG+INNOD
         DEALLOCATE(IBUFTMP(IDX1)%ptr)

         FLAGG = 1

C---  Deuxieme Passe -> creation des interfaces
         CALL R2R_GROUP(NGRNOD,
     1        INNOD,FLAGG,IPARTS,IPARTQ,IPARTC,IPARTT,IPARTP,
     2        IPARTR,IPARTG,IPARTSP,IXS10,IXS20,IXS16,2,
     3        IBUFTMP(IDX2)%ptr,IXR_KJ,INOM_OPT,IPART,
     4        IADBUF,NALE_R2R,FLG_R2R_ERR ,
     5        STACK%PM ,IWORKSH  ,IGRBRIC ,IGRQUAD   ,IGRSH4N ,
     6        IGRSH3N  ,IGRTRUSS ,IGRBEAM ,IGRSPRING ,IGRNOD  ,
     7        IGRSURF  ,IGRSLIN, LSUBMODEL,ALE_EULER ,IGEO    ,
     8        NLOC_DMG ,DETONATORS,SENSORS%NSENSOR,SEATBELT_SHELL_TO_SPRING,
     9        NB_SEATBELT_SHELLS)
C--------------------------------------------
C     MULTIDOMAINS - SPLIT DES TABLEAUX
C--------------------------------------------
         WRITE(ISTDO,'(A)')' .. MULTIDOMAINS DATA SPLIT '
C---  Premiere Passe -> comptage
         CALL R2R_SPLIT(
     1             NSLIN,
     2             NSURF,0,EANI,IBUFTMP(IDX2)%ptr,IXR_KJ,
     3             INOM_OPT,RESERVEP,NALE_R2R,NSPCOND0,
     4             SUBSETS,IGRSURF,IGRNOD,IGRBRIC,IGRQUAD,
     5             IGRSH4N,IGRSH3N,IGRTRUSS,IGRBEAM,IGRSPRING,
     6             IGRPART,IGRSLIN,LSUBMODEL,RBY_MSN,IWORKSH,
     7             SEATBELT_SHELL_TO_SPRING,NB_SEATBELT_SHELLS)
C---  Deuxieme Passe -> split
         CALL R2R_SPLIT(
     1             NSLIN,
     2             NSURF,1,EANI,IBUFTMP(IDX2)%ptr,IXR_KJ,
     3             INOM_OPT,RESERVEP,NALE_R2R,NSPCOND0,
     4             SUBSETS,IGRSURF,IGRNOD,IGRBRIC,IGRQUAD,
     5             IGRSH4N,IGRSH3N,IGRTRUSS,IGRBEAM,IGRSPRING,
     6             IGRPART,IGRSLIN,LSUBMODEL,RBY_MSN,IWORKSH,
     7             SEATBELT_SHELL_TO_SPRING,NB_SEATBELT_SHELLS)
         DEALLOCATE(IBUFTMP(IDX2)%ptr)
C--------------------------------------------
C     MULTIDOMAINS - MISE A JOUR DES STRUCTURES DE DONNEES
C--------------------------------------------

         WRITE(ISTDO,'(A)')' .. MULTIDOMAINS DATA UPDATE '
         DEALLOCATE(KNOD2ELS,KNOD2ELC,KNOD2ELTG,KNOD2EL1D,KNOD2ELIG3D,KNOD2ELQ)
         DEALLOCATE(NOD2ELS,NOD2ELC,NOD2ELTG,NOD2EL1D,NOD2ELIG3D,NOD2ELQ)
         DEALLOCATE(XYZREF)
         ALLOCATE(XYZREF(3*NUMNOD)       ,STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                              MSGTYPE=MSGERROR,
     .                         C1='XYZREF')
         FLAG_GOTO = 1

C-----on realloue le tableau FRONT-------------
c         GOTO 206 ! plus besoin de reallouer, on flushe simplement sur new numnod
         CALL INI_IFRONT()
         IENTRY2(1:NUMNOD) = -1
         FLAGKIN(1:NUMNOD) = 0

207      CONTINUE !plus utilise
c         DO I=1,NUMNOD
c          FRONT(I,1)=FRONT_R2R(I)
c        ENDDO

cc r2r with new IFRONT
         DO I=1,NUMNOD
           IF(FRONT_R2R(I)==1)THEN
             CALL IFRONTPLUS(I,1)
             !FLAGKIN array to identify boundary nodes with
             !kinematic constraints (old FRONT TAG=10)
             IF(FLAGKIN_R2R(I)==1)FLAGKIN(I)=1
           ENDIF
         ENDDO

         DEALLOCATE(FRONT_R2R,FLAGKIN_R2R)
         SFRONTB_R2R = NUMNOD
         SNOM_OPT_OLD = SNOM_OPT
         IN10 = INOM_OPT(10)
         IN20 = INOM_OPT(20)

C-----ARRAY size update & rebuild THKEC--
         GOTO 208
209      CONTINUE

C----- OPTION name update ----------
         GOTO 210
211      CONTINUE
         CALL R2R_NOM_OPT(NOM_OPT,INOM_OPT,IN10,IN20,SNOM_OPT_OLD)

C-----on repointe les tableaux IXS10,20,16--
         GOTO 212
213      CONTINUE

C-----on realloue les NOD2EL----------------
         GOTO 214
215      CONTINUE

C-----on repointe les tableaux IPART--------
         GOTO 216
217      CONTINUE

C-----on recalcule les connectivit   s inverses--------

         CALL BUILD_CNEL(
     2   IXS        ,IXQ        ,IXC        ,IXT      ,IXP      ,
     3   IXR        ,IXTG       ,IXS10      ,IXS20    ,
     4   IXS16      ,IXTG1      ,IGEO       ,KNOD2ELS ,KNOD2ELC ,
     5   KNOD2ELTG  ,NOD2ELS    ,NOD2ELC    ,NOD2ELTG ,NOD2EL1D ,
     6   KNOD2EL1D  ,KXX        ,IXX        ,X        ,LELX     ,
     7   IXIG3D     ,KXIG3D     ,KNOD2ELIG3D,NOD2ELIG3D,KNOD2ELQ,
     8   NOD2ELQ    )
         IF(ALLOCATED(IWORK)) DEALLOCATE(IWORK)

C-----on reinitialise les pointeurs utilisateurs-------
         CALL SAV_BUF_POINT(PM     ,1)
         CALL SAV_BUF_POINT(BUFMAT ,2)
         CALL SAV_BUF_POINT(GEO    ,3)
         CALL SAV_BUF_POINT(ISKWN  ,7)
         CALL SAV_BUF_POINT(SKEW   ,8)
         CALL SAV_BUF_POINT(IPM ,11)
         CALL SAV_BUF_POINT(IGEO,12)

C--------------------------------------------

218   CONTINUE
         IF(.NOT. ALLOCATED(TAGNO)) ALLOCATE(TAGNO(0))
         IF(.NOT. ALLOCATED(NALE_R2R)) ALLOCATE(NALE_R2R(0))
         IF(.NOT. ALLOCATED(DT_R2R)) ALLOCATE(DT_R2R(0))
C--------------------------------------------
C     MULTIDOMAINS - CHECK DES INTERFACES
C--------------------------------------------

C---  Check multidomains datas
      IF(NR2RLNK/=0) THEN
         CALL R2R_CHECK(IEXLNK,IGRNOD,IPART)
      ENDIF
C---  Allocation de FRONTB_R2R
      ALLOCATE(FRONTB_R2R(SFRONTB_R2R,NSPMD),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                           C1='FRONT_R2R')
      FRONTB_R2R = 0

C ------------------------------------------------
C Tri des surfaces pour les Airbags et Modif files
C ------------------------------------------------
      CALL SORT_SURF(IGRSURF,IXS,IXC,IXTG,IXQ)
C--------------------------------------------
      CALL PRINTGROUP(
     1                ITAB    ,ITABM1   ,IGRNOD   ,NINICRACK,BUFSF   ,
     2                IGRSURF ,IGRSLIN  ,INICRACK ,IXS      ,IXQ     ,
     3                IXC     ,IXT     ,IXP     ,IXR     ,IXTG    ,
     4                IXS10   ,IXS20    ,IXS16    ,IPART    ,
     5                KXX     ,IXIG3D   ,KXIG3D   ,
     6                IGRQUAD ,IGRBRIC  ,IGRSH4N  ,IGRSH3N  ,IGRTRUSS,
     7                IGRBEAM ,IGRSPRING,IGRPART  )
      CALL TRACE_OUT1()
      DEALLOCATE(KXX,IXX,LELX)
C--------------------------------------------
C     TRANSFORMATIONS
C--------------------------------------------
      ERR_MSG='TRANSFORMATIONS'
      ERR_CATEGORY='TRANSFORMATIONS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL LECTRANS(X        ,IGRNOD  ,ITAB   ,ITABM1 ,UNITAB,
     .              LSUBMODEL,RTRANS)
      CALL TRACE_OUT1()
      CLOSE(UNIT=IUSBM)
C--------------------------------------------
C     READING OF /MERGE/NODE
C--------------------------------------------
      ERR_MSG='/MERGE/NODE'
      ERR_CATEGORY='/MERGE/NODE'
C
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
      ALLOCATE(MERGE_NODE_TAB(4*NB_MERGE_NODE),STAT=stat)
      ALLOCATE(MERGE_NODE_TOL(NB_MERGE_NODE),STAT=stat)
      MERGE_NODE_TAB = 0
      MERGE_NODE_TOL = ZERO
      NMERGE_NODE_CAND = 0
      NMERGE_NODE_DEST = 0
      IF(STAT /= 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                         C1='IMERGE')
      ENDIF
      IF(NB_MERGE_NODE > 0) THEN
        CALL HM_READ_MERGE_NODE(X,LSUBMODEL,UNITAB,IGRNOD,MERGE_NODE_TAB,
     .                          MERGE_NODE_TOL,NMERGE_NODE_CAND,NMERGE_NODE_DEST)
      ENDIF
C
      CALL TRACE_OUT1()
C--------------------------------------------
C     MERGE OF NODES (cnodes + /MERGE/NODE)
C--------------------------------------------
      ERR_MSG='MERGING NODES'
      ERR_CATEGORY='MERGING NODES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
      NMERGE_TOT = NUMCNOD + NMERGE_NODE_DEST
      ALLOCATE(IMERGE(3*NMERGE_TOT),STAT=stat)
      ALLOCATE(IMERGE2(NUMNOD+1),STAT=stat)
      ALLOCATE(IADMERGE2(NUMNOD+1),STAT=stat)
      IF(STAT /= 0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                         C1='IMERGE')
      ENDIF
C
      IMERGE = 0
      IMERGE2 = 0
      IADMERGE2 =0
      NMERGED = 0
C
C--   CNODE merging
      IF(NUMCNOD > 0)
     .    CALL MERGE(X    ,ITAB   ,ITABM1  ,CMERGE ,IMERGE,
     .               IMERGE2,IADMERGE2,NMERGE_TOT)
      DEALLOCATE(CMERGE)
C
C--   /MERGE/NODE merging
      IF(NB_MERGE_NODE > 0)
     .    CALL MERGE_NODE(X    ,ITAB   ,ITABM1  ,IMERGE,IMERGE2,
     .                    IADMERGE2,NMERGE_TOT,MERGE_NODE_TAB,MERGE_NODE_TOL,
     .                    NMERGE_NODE_CAND,NMERGE_NODE_DEST,IXS,IXS10,IXS20,
     .                    IXS16,IXQ,IXC,IXT,IXP,
     .                    IXR,IXTG,EANI,IGRNOD)
C
      CALL TRACE_OUT1()
C--------------------------------------------
C     Reinitialize merged connectivities / groups
C--------------------------------------------
      ERR_MSG='REINIT CONNECTIVITY'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NMERGED > 0) THEN
        CALL RECONNECT(
     .           IXS      ,IXS10    ,IXS20    ,IXS16    ,IXQ      ,
     .           IXC      ,IXT      ,IXP      ,IXR      ,IXTG     ,
     .           IGRNOD   ,IGRSURF  ,IGRSLIN  ,
     .           ISKWN    ,IMERGE   ,NMERGE_TOT)
C
        KNOD2ELS  = 0
        KNOD2ELC  = 0
        KNOD2ELTG = 0
        KNOD2EL1D  = 0
        KNOD2ELIG3D  = 0
        NOD2ELS   = 0
        NOD2ELC   = 0
        NOD2ELTG  = 0
        NOD2EL1D   = 0
        NOD2ELIG3D   = 0
        KNOD2ELQ  = 0
        NOD2ELQ  = 0
        CALL BUILD_CNEL(
     2   IXS        ,IXQ        ,IXC        ,IXT      ,IXP      ,
     3   IXR        ,IXTG       ,IXS10      ,IXS20    ,
     4   IXS16      ,IXTG1      ,IGEO       ,KNOD2ELS ,KNOD2ELC ,
     5   KNOD2ELTG  ,NOD2ELS    ,NOD2ELC    ,NOD2ELTG ,NOD2EL1D ,
     6   KNOD2EL1D  ,KXX        ,IXX        ,X        ,LELX     ,
     7   IXIG3D     ,KXIG3D     ,KNOD2ELIG3D,NOD2ELIG3D,KNOD2ELQ,
     8   NOD2ELQ    )
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
      IF(MSHSUB /= 0)
     .  CALL SHSUB(LSHSUB  ,IXS     ,IXS10   ,IXS20   ,IXS16   ,
     .             IXC     ,IXT     ,IXP     ,IXR     ,IXTG    )
C--------------------------------------------
C     LECTURE DE RANDOM NOISE
C--------------------------------------------
      ERR_MSG='RANDOM NOISE'
      ERR_CATEGORY='RANDOM NOISE'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))

      CALL INIT_RANDOM( )

      IF(RAND_STRUCT%CMD) NRAND = NRAND + 1
      ALLOCATE(IRAND(NRAND),STAT=stat)
      ALLOCATE(ALEA(NRAND) ,STAT=stat)
      ALLOCATE(XSEED(NRAND),STAT=stat)

      CALL HM_READ_RAND(X     ,IGRNOD   ,ITAB,IRAND,ALEA,XSEED,
     .                  UNITAB,LSUBMODEL)

      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES SLIPRINGS AND RETRACTORS
C--------------------------------------------
      ERR_MSG='SEATBELTS'
      ERR_CATEGORY='SEATBELTS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
      NB_MAT_SEATBELT = 0
      CALL HM_OPTION_COUNT('/MAT/LAW114',NB_MAT)
      NB_MAT_SEATBELT = NB_MAT_SEATBELT + NB_MAT
      CALL HM_OPTION_COUNT('/MAT/SPR_SEATBELT',NB_MAT)
      NB_MAT_SEATBELT = NB_MAT_SEATBELT + NB_MAT
      CALL HM_OPTION_COUNT('/MAT/LAW119',NB_MAT)
      NB_MAT_SEATBELT = NB_MAT_SEATBELT + NB_MAT
      CALL HM_OPTION_COUNT('/MAT/SH_SEATBELT',NB_MAT)
      NB_MAT_SEATBELT = NB_MAT_SEATBELT + NB_MAT
C
      IF(NSLIPRING + NRETRACTOR > 0) WRITE(ISTDO,'(A)')' .. SLIPRING/RETRACTOR'
      IF(NSLIPRING > 0) CALL HM_READ_SLIPRING(
     1                                          LSUBMODEL,ITABM1,IXR,ITAB,UNITAB,
     2                                          X,NPC1,NOM_OPT(LNOPT1*INOM_OPT(31)+1),ALEA,IGRNOD,
     2                                          IGRSH4N,IXC,IPM)
      IF(NRETRACTOR > 0) CALL HM_READ_RETRACTOR(
     1                                           LSUBMODEL,ITABM1,IXR,ITAB,UNITAB,
     2                                           X,NPC1,NOM_OPT(LNOPT1*INOM_OPT(32)+1),ALEA,IPM)
      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES CONDITIONS LIMITES
C--------------------------------------------
      ERR_MSG='BCS'
      ERR_CATEGORY='BCS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NBCSCYC>0) THEN
        CALL HM_PREREAD_BCSCYC(IGRNOD ,NOM_OPT(LNOPT1*INOM_OPT(16)+1),LSUBMODEL,SLBCSCYC)
      END IF
      SIBCSCYC = 4*NBCSCYC
      ALLOCATE(IBCSCYC(SIBCSCYC),LBCSCYC(SLBCSCYC),STAT=stat)
      LBCSCYC = 0
      IF(NUMBCS /= 0 .OR. NALEBCS /= 0 .OR. NUMBCSN /= 0) THEN
        WRITE(ISTDO,'(A)')TITRE(13)
C READ /BCS & /BCS/LAGMUL
        CALL HM_READ_BCS(ICODE  ,ISKEW  ,ITAB   ,ITABM1  ,D       ,
     .              IGRNOD   ,IBCSLAG ,LAG_NCF ,LAG_NKF ,LAG_NHF,
     .              IKINE1LAG,ISKWN,NOM_OPT(LNOPT1*INOM_OPT(16)+1),
     .              UNITAB   ,LSUBMODEL,IBCSCYC,LBCSCYC)
C READ /ALE/BCS
        CALL HM_READ_ALEBCS(ICODE  ,ISKEW  ,ITAB   ,ITABM1  ,D       ,
     .              IGRNOD   ,IBCSLAG ,LAG_NCF ,LAG_NKF ,LAG_NHF,
     .              IKINE1LAG,ISKWN,NOM_OPT(LNOPT1*INOM_OPT(16)+1),
     .              LSUBMODEL)
C READ /NBCS
        CALL HM_READ_NBCS(ICODE  ,ISKEW  ,ITAB   ,ITABM1  ,D       ,
     .                    IGRNOD   ,IBCSLAG ,LAG_NCF ,LAG_NKF ,LAG_NHF,
     .                    IKINE1LAG,ISKWN,NOM_OPT(LNOPT1*INOM_OPT(16)+1),LSUBMODEL)
C PRINT /BCS
        CALL PRINTBCS(ICODE  ,ISKEW  ,ITAB   ,ITABM1  ,D       ,
     .              IGRNOD   ,IBCSLAG ,LAG_NCF ,LAG_NKF ,LAG_NHF,
     .              IKINE1LAG,ISKWN,NOM_OPT(LNOPT1*INOM_OPT(16)+1), NBCSLAG)
      ENDIF
C
      ALLOCATE(ICODEP(0),ISKEWP(0))
      CALL TRACE_OUT1()
C     Adaptive meshing : Sending down the bcs
      ERR_MSG='ADAPTIVE MESHING BCS'
      ERR_CATEGORY='ADAPTIVE MESHING BCS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NADMESH/=0)THEN
        CALL ADMBCS(IXC ,IPARTC,IXTG,IPARTG,IPART  ,
     .              ICODE,ISKEW,ITAB,SH4TREE,SH3TREE)
      END IF
      CALL TRACE_OUT1()
C--------------------------------------------------------------------------
C     TRI DES BRICK ET QUAD, CLASSEMENT PAR LOI
C--------------------------------------------------------------------------
      ERR_MSG='SOLIDS SORT'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SIWORK = 2*MAX(NUMELS,NUMELQ)
      ALLOCATE(IWORK(SIWORK)  ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='IWORK')
      IF(NUMELS /= 0)
     .  CALL LCE16S3(
     .            IXS    ,BID13   ,PM      ,IWORK   ,ITAB    ,ITABM1  ,
     .            ICODE  ,IPARTS  ,IGRBRIC ,GEO     ,EANI    ,
     .            IXS10  ,IPART   ,IXS20   ,IXS16   ,KNOD2ELS,NOD2ELS ,
     .            IGRSURF,SPH2SOL ,SOL2SPH )
      IF(NUMELQ /= 0)
     .  CALL LCE16Q3(
     .            IXQ    ,BID13   ,PM      ,IWORK   ,ITAB    ,ITABM1  ,
     .            ICODE  ,IPARTQ  ,IGRQUAD ,IPM     ,IGEO    )
      IF(ALLOCATED(IWORK)) DEALLOCATE(IWORK)
      CALL TRACE_OUT1()
C------------------------------------------------------------------
C     RAYLEIGH DAMPING
C--------------------------------------------
      ERR_MSG='DAMPING'
      ERR_CATEGORY='DAMPING'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NDAMP > 0) CALL HM_READ_DAMP(DAMPR   ,IGRNOD  ,ISKWN,LSUBMODEL,UNITAB)
      CALL TRACE_OUT1()
C--------------------------------------------
C     MULTI-POINT CONSTRAINTS (1)
C--------------------------------------------
      ERR_MSG='MPCS 1'
      ERR_CATEGORY='MPCS 1'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NUMMPC > 0) THEN
        CALL HM_READ_MPC0 (LMPC,LSUBMODEL)
      ELSE
        LMPC=0
      ENDIF
      SRBMPC = LMPC
      ALLOCATE(RBMPC(SRBMPC)  ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='RBMPC')
      IF(SRBMPC > 0) RBMPC = ZERO
      CALL TRACE_OUT1()
C--------------------------------------------
C     STOCKAGE DYNAMIQUE REEL
C--------------------------------------------
      ERR_MSG='DYNAMIC STORAGE REAL'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SGJBUFR = LKJNR*NGJOINT
      SW      = 3*NUMNOD*IALE
      NUMELSK8     =    NUMELS
      NUMELQK8     =    NUMELQ
      SVEUL   = (LVEUL*NUMELS+10*NUMELQ)*IEULER
      SVEUL8  = (INT(LVEUL,KIND(SVEUL8))*NUMELSK8+10*NUMELQK8)*INT(IEULER,KIND(SVEUL8))
      SFILL   = NMULT*NUMNOD
      SDFILL  = NMULT*NUMNOD
      SALPH   = 2*NMULT*(NUMELQ+NUMELS)
      SWB     = 0
      IF(ALE%GRID%NWALE == 2) THEN
        SWB  = 3*NUMNOD
      ELSEIF(ALE%GRID%NWALE == 4) THEN
        SWB  = 4*NUMNOD
      ENDIF
      ALLOCATE(WB(SWB)          ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='WB')
      IF(SWB > 0) WB = ZERO
      IF(ALE%GRID%NWALE == 4) THEN
        WMA => WB(3*NUMNOD+1:SWB)
      ELSE
        WMA => WB
      ENDIF
C
      SDSAVE   = 0
      SDRSAVE  = 0
      SASAVE   = 0
      SARSAVE  = 0
      SSTSAVE  = 0
      SSTRSAVE = 0
      SFSAVSAV = 0
      IF(MSHSUB == 1)THEN
        SDSAVE   = 3*NUMNOD
        SDRSAVE  = 6*NUMNOD
        SASAVE   = 3*NUMNOD
        SARSAVE  = 3*NUMNOD
        SSTSAVE  = NUMNOD
        SSTRSAVE = NUMNOD
        SFSAVSAV = NTHVKI*NSECT
      ELSEIF(ILAG == 1 .AND. (IALE+IEULER) > 0) THEN
        SDSAVE  = 3*NUMNOD
        SASAVE  = 3*NUMNOD
      ENDIF
      ALLOCATE(GJBUFR(SGJBUFR)  ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='GJBUFR')
      ALLOCATE(W(SW)            ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='W')
      ALLOCATE(VEUL(SVEUL8)      ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='VEUL')
      ALLOCATE(FILL(SFILL)      ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='FILL')
      ALLOCATE(DFILL(SDFILL)    ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='DFILL')
      ALLOCATE(ALPH(SALPH)      ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='ALPH')
      ALLOCATE(DSAVE(SDSAVE)    ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='DSAVE')
      ALLOCATE(DRSAVE(SDRSAVE)  ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='DRSAVE')
      ALLOCATE(ASAVE(SASAVE)    ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='ASAVE')
      ALLOCATE(ARSAVE(SARSAVE)  ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='ARSAVE')
      ALLOCATE(STSAVE(SSTSAVE)  ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='STSAVE')
      ALLOCATE(STRSAVE(SSTRSAVE),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='STRSAVE')
      ALLOCATE(FSAVSAV(SFSAVSAV),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='FSAVSAV')
      IF(SGJBUFR > 0)  GJBUFR  = ZERO
      IF(SW > 0)       W       = ZERO
      IF(SVEUL8 > 0)    VEUL    = ZERO
      IF(SFILL > 0)    FILL    = ZERO
      IF(SDFILL > 0)   DFILL   = ZERO
      IF(SALPH > 0)    ALPH    = ZERO
      IF(SDSAVE > 0)   DSAVE   = ZERO
      IF(SDRSAVE > 0)  DRSAVE  = ZERO
      IF(SASAVE > 0)   ASAVE   = ZERO
      IF(SARSAVE > 0)  ARSAVE  = ZERO
      IF(SSTSAVE > 0)  STSAVE  = ZERO
      IF(SSTRSAVE > 0) STRSAVE = ZERO
      IF(SFSAVSAV > 0) FSAVSAV = ZERO
      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES JOINTS COMPLEXES (GJOINT)
C--------------------------------------------
      ERR_MSG='GJOINTS'
      ERR_CATEGORY='GJOINTS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      JOINT_SMS = .FALSE.
      IF(ISMS/=0) JOINT_SMS = .TRUE.
      IF(NGJOINT/=0) CALL HM_READ_GJOINT(
     1           GJBUFI  ,GJBUFR  ,ITAB    ,ITABM1  ,X       ,
     2           MS      ,IN      ,LAG_NCF ,LAG_NKF ,LAG_NHF ,
     3           D       ,UNITAB  ,IKINE1LAG,NOM_OPT(LNOPT1*INOM_OPT(18)+1),LSUBMODEL)
      CALL TRACE_OUT1()
C--------------------------------------------
C     A.L.E.
C     TABLEAUX DE VOISINS (OU FACETTES) DES ELEMENTS
C     TABLEAUX DES NOEUDS VOISINS
C--------------------------------------------
      CALL ALE_CONNECTIVITY%ALE_CONNECTIVITY_INIT()
      ERR_MSG='ALE LINKS'
      ERR_CATEGORY='ALE'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NALELK/=0) THEN
        WRITE(ISTDO,'(A)')TITRE(29)
        LLINAL = 7 * NALELK
        SLINALE=LLINAL
        ALLOCATE(LINALE(SLINALE),STAT=stat)
        IF(IERR/=0) THEN
          WRITE(IOUT,*) ' ** ERROR IN MEMORY ALLOCATION'
          WRITE(ISTDO,*)' ** ERROR IN MEMORY ALLOCATION'
        ENDIF
        CALL HM_READ_ALE_LINK(ICODE, ISKEW, ITAB, ITABM1, D,
     .       IGRNOD, IBCSLAG, LAG_NCF, LAG_NKF, LAG_NHF,
     .       IKINE1LAG, LINALE, LSUBMODEL, UNITAB)
      ELSE
        ALLOCATE(LINALE(0))
      ENDIF
      ERR_MSG='ALE NEIGHBOURS'
      IF(IALE+IEULER+IALELAG/=0)THEN
       WRITE(ISTDO,'(A)')TITRE(30)
       CALL ALELEC(
     1     ICODE    ,IXS      ,IXQ       ,IXC               ,IXT     ,
     2     IXTG     ,PM       ,IGEO      ,ITAB              ,GEO     ,
     3     NALE_R2R ,NSUBDOM  ,MULTI_FVM ,ALE_CONNECTIVITY)
C     ---------------------------------------------------------------
C     Unplug neighbor elements in case of direct lagrangian coupling
C     ---------------------------------------------------------------
       IF(MULTI_FVM%IS_USED) THEN
          CALL MULTI_UNPLUG_NEIGHBORS(ALE_CONNECTIVITY, IXS, IXQ, IXTG)
       ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     MULTIMATERIAUX
C     INITIALISATION DES POURCENTAGES NODAUX
C--------------------------------------------
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
       IF(NMULT>0)THEN
        WRITE(ISTDO,'('' .. MULTIMATERIALS'')')
        IF(NUMELS>0)
     .      CALL INIMU3(PM     ,IXS    ,FILL   ,DFILL  )
        IF(NUMELQ>0)
     .      CALL INIMU2(PM     ,IXQ    ,FILL   ,DFILL  )
          CALL INIMUL  (PM     ,FILL   ,DFILL  )
       ENDIF
      ENDIF
      CALL TRACE_OUT1()
C---------------------------------------------------
C     DETECTION DES ELEMENTS LOIS 6 PAROI---->LOI 17
C---------------------------------------------------
      ERR_MSG='CFD BOUNDARY ELEMENTS'
      ERR_CATEGORY='CFD BOUNDARY ELEMENTS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(IALE+IEULER/=0)
     +   CALL PAROI(PM     ,IXS    ,IXQ    ,ICODE  ,ALE_CONNECTIVITY%NALE   )
      CALL TRACE_OUT1()
C--------------------------------------------
C     STOCKAGE DYNAMIQUE (CHARGEMENT) ENTIER
C--------------------------------------------
C      LECTURE DES ELEMENTS MULTI-PURPOSE.
C---------------------------------------------
      ERR_MSG='MULTI-PURPOSE ELEMENTS'
      ERR_CATEGORY='MULTI-PURPOSE ELEMENTS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NUMELX > 0) THEN
        SKXX = NIXX*NUMELX
        CALL HM_PREREAD_XELEM(SIXX, IGRNOD,LSUBMODEL)
        ALLOCATE(KXX(SKXX)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='KXX')
        ALLOCATE(IXX(SIXX+150)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IXX')
        KXX = 0
        IXX = 0
        ALLOCATE(LELX(NUMELX)    ,STAT=stat)
        LELX(1:NUMELX) = 0
        CALL HM_READ_XELEM(IGRNOD  ,ITAB   ,ITABM1 ,IPART ,IPARTX,
     .              IPM     ,IGEO   ,KXX    ,IXX   ,LSUBMODEL)
      ELSE
        SKXX = 0
        SIXX = 0
        ALLOCATE(KXX(SKXX)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='KXX')
        ALLOCATE(IXX(SIXX)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IXX')
        ALLOCATE(LELX(NUMELX)    ,STAT=stat)
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES CONDITIONS DE SYMETRIE SPH.
C---------------------------------------------
      ERR_MSG='SPH SYM'
      ERR_CATEGORY='SPH SYM'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SISPSYM  = NSPCOND*NUMSPH
      SISPCOND = NSPCOND*NISPCOND
      ALLOCATE(ISPSYM(SISPSYM)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='ISPSYM')
      ALLOCATE(ISPCOND(SISPCOND)  ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='ISPCOND')
      IF(NSPCOND > 0) THEN
        ISPSYM  = 0
        ISPCOND = 0
         WRITE(ISTDO,'(A)')' .. SPH SYMMETRY CONDITIONS'
        CALL HM_READ_SPCND(ISPCOND ,ISKEW   ,ITAB    ,ITABM1  ,D       ,
     .               IGRNOD  ,NOD2SP ,IFRAME ,NOM_OPT(LNOPT1*INOM_OPT(23)+1),
     .               LSUBMODEL)
      ENDIF
      CALL TRACE_OUT1()
C---------------------------------------------
C      LECTURE DES INLET/OUTLET SPH.
C---------------------------------------------
      ERR_MSG='SPH I/O'
      ERR_CATEGORY='SPH I/O'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      LWASPIO=0
      SISPHIO = NISPHIO*NSPHIO
      SSPHVELN= NSPHIO*NUMSPH*2
      ALLOCATE(ISPHIO(SISPHIO)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='ISPHIO')
      ALLOCATE(SPHVELN(SSPHVELN)  ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='SPHVELN')
      LVSPHIO = 0
      IF(NSPHIO > 0)THEN
         WRITE(ISTDO,'(A)')' .. SPH INLET/OUTLET DEFINITION'
         ISPHIO = 0
         NSEG_IO = 0
         CALL HM_PREREAD_SPHIO(IGRSURF   ,SVSPHIO      ,
     .                   NOM_OPT(LNOPT1*INOM_OPT(22)+1),
     .                   LSUBMODEL)
         ALLOCATE(VSPHIO(SVSPHIO)  ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='VSPHIO')
         VSPHIO(1:SVSPHIO)=ZERO

c         ALLOCATE(VSPHIO2(SVSPHIO2)  ,STAT=stat)
c         VSPHIO2(1:SVSPHIO2)=ZERO

         CALL HM_READ_SPHIO(ISPHIO   ,VSPHIO   ,IPART    ,IGRSURF  ,
     .                      NOD2SP   ,IPARTSP  ,ITAB     ,X        ,
     .                      LVSPHIO  ,LWASPIO  ,ITABM1   ,UNITAB   ,
     .                      LSUBMODEL)
C---- -----------------------------------------
C        PREPARATION de la LISTE TRIEE des PARTICULES ON/OFF par PART.
C---------------------------------------------
         SLPRTSPH = 2*(NPART+1)
         SLONFSPH = NUMSPH
         ALLOCATE(LPRTSPH(SLPRTSPH)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='LPRTSPH')
         ALLOCATE(LONFSPH(SLONFSPH)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                             C1='LONFSPH')
         LPRTSPH = 0
         LONFSPH = 0
         CALL SPHONF0(KXSP    ,IXSP    ,NOD2SP  ,IPART  ,IPARTSP ,
     .                LPRTSPH ,LONFSPH )
         SPHVELN = ZERO
         IF(NSPMD > 1)THEN
c           CALL ANSTCKC(27,'SPH INLET/OUTLET DEFINITION')
c           CALL ANCERR(755,ANINFO)
         END IF
      ELSE
         SLPRTSPH = 0
         SLONFSPH = 0
         SVSPHIO  = 0
c         SVSPHIO2  = 0
         ALLOCATE(LPRTSPH(SLPRTSPH)    ,STAT=stat)
         ALLOCATE(LONFSPH(SLONFSPH)    ,STAT=stat)
         ALLOCATE(VSPHIO(SVSPHIO)      ,STAT=stat)
c         ALLOCATE(VSPHIO2(SVSPHIO2)      ,STAT=stat)
      ENDIF
      IF(NUMSPH > 0)THEN
C---------------------------------------------
C        REMPLISSAGE de SPBUF(2) = H
C---------------------------------------------
        CALL SPINIH(KXSP    ,IPART  ,IPARTSP ,SPBUF  ,PM     )
C---------------------------------------------
C        TRI STARTER : Remplissage de IXSP
C---------------------------------------------
        CALL SPTRI(KXSP    ,IXSP    ,NOD2SP  ,X      ,SPBUF  ,
     .             LPRTSPH ,LONFSPH )
      END IF
      CALL TRACE_OUT1()
C---------------------------------------------
C       Masses nodales fluides
C---------------------------------------------
      ERR_MSG='FLUID NODAL MASSES'
      ERR_CATEGORY='FLUID NODAL MASSES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SMSNF = NUMNOD*MAX(IALE,IEULER,IALELAG)
      ALLOCATE(MSNF(SMSNF)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='MSNF')
      MSNF = ZERO
      CALL TRACE_OUT1()
C--------------------------------------------
C     PRE-LECTURE DES FORCES CONCENTREES & PRESSIONS
C--------------------------------------------
      ERR_MSG='CONCENTRED LOADS'
      ERR_CATEGORY='CONCENTRED LOADS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      NUMCLD  = NCONLD
      NUMPRES = NPRELD
      IF(NSUBDOM>0) ALLOCATE(NNCL(NCONLD+NPRELD))
      CALL HM_PREREAD_CLOAD(NUMCLD, IGRNOD ,IGRSURF,LSUBMODEL)
      CALL HM_PREREAD_PLOAD(NUMPRES,IGRNOD ,IGRSURF,LSUBMODEL)
      SIBCL = (NUMCLD + NUMPRES)*NIBCLD
      SFORC = (NUMCLD + NUMPRES)*LFACCLD
      ALLOCATE(IBCL(SIBCL)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IBCL')
      ALLOCATE(FORC(SFORC)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='FORC')
      IF(NUMPRES>0) THEN
        IPRES  => IBCL(NUMCLD*NIBCLD+1:SIBCL)
      ELSE
        IPRES  => IBCL
      END IF
      IBCL = 0
      FORC = ZERO
C--------------------------------------------
C     LECTURE DES FORCES CONCENTREES
C--------------------------------------------
      IF(NCONLD/=0) THEN
        WRITE(ISTDO,'(A)')TITRE(33)
C       NCONLD ET NPRELD sont modifies dans LECCLD et LECPRE
        CALL HM_READ_CLOAD(IBCL   ,FORC   ,NCONLD,ITAB   ,ITABM1 ,
     .              IGRNOD ,IBCL   ,UNITAB,ISKWN ,LSUBMODEL  )
        IF(NCONLD*LFACCLD<SFORC) THEN
        PRES => FORC(NCONLD*LFACCLD+1:SFORC)
        ELSE
          PRES => FORC
      ENDIF
      ENDIF
      IF(NCONLD*LFACCLD<SFORC) THEN
      PRES => FORC(NCONLD*LFACCLD+1:SFORC)
      ELSE
        PRES => FORC
      END IF
      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES PRESSIONS
C--------------------------------------------
      ERR_MSG='PRESSURE LOADS'
      ERR_CATEGORY='PRESSURE LOADS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NPRELD/=0)THEN
        WRITE(ISTDO,'(A)')TITRE(34)
        CALL HM_READ_PLOAD(IPRES   ,PRES   , NPRELD , ITAB    , ITABM1 ,
     .              IGRSURF ,UNITAB ,LSUBMODEL)
        NCONLD=NCONLD+NPRELD
      ENDIF
C NCONLD ET NPRELD sont modifies dans LECCLD et LECPRE
      CALL TRACE_OUT1()
      IF(NSUBDOM>0) DEALLOCATE(NNCL)
c
c     pressure load in cylindrical coordintes
c
      CALL HM_READ_PCYL(LOADS   ,IGRSURF ,SENSORS%NSENSOR,SENSORS%SENSOR_TAB ,TABLE   ,
     .                  IFRAME  ,UNITAB  ,LSUBMODEL,NUMBER_LOAD_CYL  )
C--------------------------------------------
C     "LOAD FIELDS"
C--------------------------------------------
      ERR_MSG      = 'LOAD FIELDS'
      ERR_CATEGORY = 'LOAD FIELDS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
C     CENTRIFUGAL LOADS
      CALL HM_PREREAD_LOAD_CENTRI(NUMCFIELD,IGRNOD,IGRSURF,LSUBMODEL)
      SCFIELD      = LFACLOAD*NLOADC
      SICFIELD     = SIZFIELD*NLOADC
      SLCFIELD     = NUMCFIELD
C
C     PFLUID & PBLAST
      NUMLOADP=0
      NINTLOADP=0
      CALL HM_PREREAD_PFLUID(NUMLOADP,IGRNOD,IGRSURF,LSUBMODEL)
      CALL HM_PREREAD_PBLAST(NUMLOADP,IGRSURF,LSUBMODEL)
      CALL HM_PREREAD_LOAD_PRESSURE(NUMLOADP,IGRSURF,LSUBMODEL)
      NLOADP       = NLOADP_F+NLOADP_B+NLOADP_HYD
      SLOADP       = LFACLOAD*NLOADP
      SILOADP      = SIZLOADP*NLOADP
      SLLOADP      = NUMLOADP
C
      CALL TRACE_OUT1()
c------------------------------------------------------
c     IMPOSED DISPLACEMENTS, VELOCITIES AND ACCELERATIONS
c------------------------------------------------------
      ERR_MSG='IMPOSED VELOCITIES'
      ERR_CATEGORY='IMPOSED VELOCITIES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c
      NFVLAG = 0    ! Lagrangian multiplier flag
C---
c     Input  : NFXVEL = number of input cards : /IMDISP + /IMPVEL + /IMPACC
c     Output : NFXVEL = number of imposed nodes (disp + vel + acc)
c
c---  Calculate number of nodes with imposed disp, vel, acc for allocation
c
      IF(NFXVEL > 0) THEN
        NFV0 = NFXVEL
c
        CALL HM_PREREAD_IMPDISP(NIMPDISP  ,IGRNOD  ,IPART   ,IPARTR   ,
     .                          UNITAB    ,LSUBMODEL)
c
        CALL HM_PREREAD_IMPVEL(NIMPVEL  ,IGRNOD  ,IPART   ,IPARTR   , NFVLAG,
     .                         UNITAB   ,LSUBMODEL)
c
        CALL HM_PREREAD_IMPACC(NIMPACC ,IGRNOD ,LSUBMODEL)
c
        NFXVEL = NIMPDISP + NIMPVEL + NIMPACC
      ELSE
        NFV0 = 0
        NIMPDISP = 0
        NIMPVEL = 0
        NIMPACC = 0
        NIMPV_LAGM  = 0
      ENDIF
c---
      SIBFV   = NFXVEL * NIFV
      SVEL    = NFXVEL * LFXVELR
      NFXVEL0 = NFXVEL
      ALLOCATE(IBFV(SIBFV)    ,STAT=stat)
      ALLOCATE(VEL(SVEL    )  ,STAT=stat)
      IBFV(1:SIBFV) = 0
      VEL (1:SVEL)  = ZERO
c------------------------------------------------------
c
      IF(NFXVEL > 0) THEN
c
        WRITE(ISTDO,'(A)')TITRE(44)
c
        CALL HM_READ_IMPVEL(
     .       VEL      ,IBFV     ,D        ,IKINE1LAG,
     .       ITAB     ,ITABM1   ,IGRNOD   ,X        ,IXR      ,
     .       IPART    ,IPARTR   ,ISKWN    ,NOM_OPT(LNOPT1*INOM_OPT(15)+1),
     .       NIMPDISP ,NIMPVEL  ,UNITAB   ,LSUBMODEL)

      ENDIF
c------------------------------------------------------
c     IMPOSED ACCELERATIONS
c------------------------------------------------------
      IF(NIMPACC > 0) THEN
        CALL HM_READ_IMPACC(
     .             VEL      ,IBFV     ,NFXVEL0  ,ITAB     ,ITABM1   ,
     .             D        ,IGRNOD   ,ISKWN    ,UNITAB   ,LSUBMODEL,
     .             NFXVEL   ,NIMPACC  )
      ENDIF
c
C /BCS/CYCLIC ini&check
      IF(NBCSCYC > 0) THEN
        ALLOCATE(ITAGCYC(NUMNOD)  ,STAT=stat)
        CALL INI_BCSCYC(IBCSCYC,LBCSCYC,SKEW,X,ITAB,ICODE,IBFV,ITAGCYC)
      ELSE
        ALLOCATE(ITAGCYC(0))
      END IF
C--------------------------------------------
      SFSAV = NTHVKI * (NINTER+NRWALL+NRBODY+NSECT+NJOINT+NRBAG+NVOLU+NMONVOL+NFXBODY+NINTSUB)
      ALLOCATE(FSAV(SFSAV)  ,STAT=stat)
      FSAV = ZERO
      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES VITESSES INITIALES
C--------------------------------------------
      ERR_MSG='INITIAL VELOCITIES'
      ERR_CATEGORY='INITIAL VELOCITIES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
      IF(NRBODY > 0) THEN
        ALLOCATE(RBY_INIAXIS(7,NRBODY))
        RBY_INIAXIS = ZERO
      ELSE
        ALLOCATE(RBY_INIAXIS(0,0))
      ENDIF
C
      IF(NINVEL/=0.OR.ISIGI>=3)THEN
        SIWORK = NINVEL
        ALLOCATE(IWORK(SIWORK)  ,STAT=stat)
        IWORK = 0
       WRITE(ISTDO,'(A)')TITRE(35)
       ALLOCATE(FVM_INIVEL(NINVEL))
       DO I = 1, NINVEL
          FVM_INIVEL(I)%FLAG = .FALSE.
       ENDDO
C
        CALL HM_READ_INIVEL(V       ,W       ,ITAB   ,ITABM1  ,VR      ,
     .              IGRNOD    ,IGRBRIC ,ISKWN    ,SKEW    ,IWORK    ,
     .              X       ,UNITAB  ,LSUBMODEL,RTRANS,XFRAME  ,
     .              IFRAME    ,VFLOW   ,WFLOW    ,KXSP    ,MULTI_FVM,
     .              FVM_INIVEL,IGRQUAD ,IGRSH3N  ,RBY_MSN ,RBY_INIAXIS)
        CALL INIVEL(V       ,VR      ,SVR_1 ,ITABM1     )
C
        IF(ALLOCATED(IWORK)) DEALLOCATE(IWORK)
        NINVEL  = SIWORK
C
      ELSE
        ALLOCATE(FVM_INIVEL(0:0))
      ENDIF
C
      IF(ALLOCATED(RBY_MSN)) DEALLOCATE(RBY_MSN)
C
      CALL TRACE_OUT1()

C     *****************************************************************    C
C     Check if ALE or EULER materials are used with lagrangian thermics
C     *****************************************************************    C

      IF(IALE + IEULER /= 0) THEN
         IF(ITHERM_FE == 0 .AND. (NIMTEMP /= 0 .OR. NINTEMP /=0)) THEN
            CALL ANCMSG(MSGID=1724, ANMODE=ANINFO, MSGTYPE=MSGWARNING)
         ENDIF
      ENDIF

C--------------------------------------------
C     LECTURE DES TEMPURATURES INITIALES
C--------------------------------------------
C
      ERR_MSG='INITIAL TEMPERATURES'
      ERR_CATEGORY='INITIAL TEMPERATURES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(ITHERM_FE > 0 ) THEN
         ALLOCATE(MCP(NUMNOD),TEMP(NUMNOD))
         DO J = 1,NUMNOD
           MCP(J) = ZERO
           TEMP(J) = ZERO
         ENDDO
C ----
         IF(NINTEMP > 0)THEN
           WRITE(ISTDO,'(A)')TITRE(35)
           ALLOCATE(INTIDS(NINTEMP))
           CALL HM_READ_INITEMP(TEMP  ,ITAB  ,ITABM1  ,IGRNOD  ,INTIDS ,UNITAB,
     .                          LSUBMODEL )
           DEALLOCATE(INTIDS)
         ENDIF
C
c         CALL PRELECTHERBC(IGRSURF, IGRNOD, IGRBRIC)

         CALL HM_PREREAD_IMPTEMP(IGRSURF, IGRNOD, IGRBRIC, UNITAB, LSUBMODEL)
c
         CALL HM_PREREAD_CONVEC(IGRSURF, IGRNOD, IGRBRIC, UNITAB, LSUBMODEL)
c
         CALL HM_PREREAD_RADIATION(IGRSURF, IGRNOD, IGRBRIC,UNITAB, LSUBMODEL)
c
         CALL HM_PREREAD_IMPFLUX(IGRSURF, IGRNOD, IGRBRIC,UNITAB, LSUBMODEL)
c

         ALLOCATE(IBCV(NICONV*NUMCONV),    FCONV(LFACTHER*NUMCONV),
     .            IBFTEMP(NIFT*NFXTEMP),   FBFTEMP(LFACTHER*NFXTEMP),
     .            IBFFLUX(NITFLUX*NFXFLUX),FBFFLUX(LFACTHER*NFXFLUX),
     .            IBCR(NIRADIA*NUMRADIA),  FRADIA(LFACTHER*NUMRADIA),
     .            STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                              MSGTYPE=MSGERROR,
     .                              C1='THERMAL DATA')
          IBCR(1:NIRADIA*NUMRADIA) = 0
C
         IF(NUMCONV > 0 ) THEN
           IBCV = 0
           FCONV = ZERO
           CALL HM_READ_CONVEC(IBCV,FCONV,ITAB,IXS,IGRSURF,UNITAB,LSUBMODEL)
         ENDIF
C
         IF(NUMRADIA > 0 ) THEN
           CALL HM_READ_RADIATION(IBCR,FRADIA,ITAB,IXS,IGRSURF,UNITAB,LSUBMODEL)
         ENDIF
C
         IF(NFXTEMP > 0) THEN
           IBFTEMP = 0
           FBFTEMP = ZERO
           CALL HM_READ_IMPTEMP(IBFTEMP,FBFTEMP,NFXTEMP,ITABM1,
     .                    IGRNOD ,IBFTEMP,ITAB   ,UNITAB,LSUBMODEL)
         ENDIF
C
         IF(NFXFLUX > 0 ) THEN
           IBFFLUX = 0
           FBFFLUX = ZERO
           CALL HM_READ_IMPFLUX(IBFFLUX ,FBFFLUX ,ITAB   ,IXS    ,IGRSURF   ,
     .                    UNITAB  ,IGRNOD  ,IGRBRIC, LSUBMODEL)
         ENDIF
C
       ELSE
         ALLOCATE(MCP(0),TEMP(0))
         ALLOCATE(IBCV(0),FCONV(0),IBFTEMP(0),FBFTEMP(0),
     .            IBFFLUX(0),FBFFLUX(0),IBCR(0),FRADIA(0))
       ENDIF
C
      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES POINTS DE DETONATION
C--------------------------------------------
      ERR_MSG='DETONATORS'
      ERR_CATEGORY='DETONATORS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      !new Reader
      CALL READ_DETONATORS(ITABM1   ,ITAB   ,IGRNOD  ,
     .                     IPM      ,X      ,IXQ     ,IXS    ,UNITAB,
     .                     LSUBMODEL,DETONATORS)
      CALL TRACE_OUT1()
C--------------------------------------------
      IF(ISIGI==2 .OR. ISIGI==4) THEN
        SFZERO = 3*NUMNOD
      ELSEIF(IABS(ISIGI)==5) THEN
        SFZERO = 3*4*(NUMELC+NUMELTG)
      ELSE
        SFZERO = 0
      ENDIF
      ALLOCATE(FZERO(SFZERO)  ,STAT=stat)
      IF(SFZERO > 0) FZERO = ZERO
C--------------------------------------------
C     LECTURE DES IMPACT LASER
C--------------------------------------------
      ERR_MSG='LASER IMPACTS'
      ERR_CATEGORY='LASER IMPACTS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL LECLAS(LSUBMODEL)
      CALL TRACE_OUT1()
C-------------------------------------------------
C     GENERATION FACETTES EXTERNES (FICHIERS ANIM)
C     ALE-EULER SEULEMENT
C-------------------------------------------------
      NFACX=0
      ERR_MSG='ELEMENTARY BOUNDARY CONDITIONS'
      ERR_CATEGORY='ELEMENTARY BOUNDARY CONDITIONS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      EBCS_TAG_CELL_SPMD(1:NUMELQ+NUMELTG+NUMELS)=0
      CALL READ_EBCS(IGRSURF,MULTI_FVM,NPC1,(LNOPT1*INOM_OPT(24)+1),LSUBMODEL,EBCS_TAB)
      IF(NEBCS > 0)THEN
        !allocate & count
        IF(.NOT. ALLOCATED(SENSOR_TMP)) ALLOCATE( SENSOR_TMP(0) )
        CALL INIEBCS(ALE_CONNECTIVITY, 0, IGRSURF, IXS, IXQ, IXTG,
     .        PM, IGEO, X, SENSOR_TMP, MONVOl, MULTI_FVM%IS_USED, 
     .        EBCS_TAB, EBCS_TAG_CELL_SPMD)
        DEALLOCATE(SENSOR_TMP)
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES ACCELEROMETRES
C--------------------------------------------
      ERR_MSG='ACCELEROMETERS'
      ERR_CATEGORY='ACCELEROMETERS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SACCELM = NACCELM * LLACCELM
      ALLOCATE(ACCELM(SACCELM)    ,STAT=stat)
      IF(SACCELM > 0) ACCELM = ZERO
      IF(NACCELM > 0) CALL LECACC(
     1          LACCELM,ACCELM ,ITABM1 ,UNITAB,IXC,
     2          ISKWN,NOM_OPT(LNOPT1*INOM_OPT(1)+1), LSUBMODEL)
      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES GAUGES
C--------------------------------------------
      ERR_MSG='GAUGES'
      ERR_CATEGORY='GAUGES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      ALLOCATE(LGAUGE(3*NBGAUGE) ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='LGAUGE')
      IF(NBGAUGE > 0) LGAUGE=0
      ALLOCATE(GAUGE(LLGAUGE*NBGAUGE) ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='GAUGE')
      IF(NBGAUGE > 0) GAUGE=ZERO
      IF(NBGAUGE > 0) CALL HM_READ_GAUGE(
     1          LGAUGE,GAUGE ,ITABM1 ,UNITAB,IXC,
     2          ISKWN,NOM_OPT(LNOPT1*INOM_OPT(27)+1),LSUBMODEL)
      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES SENSORS
C--------------------------------------------
      ERR_MSG='SENSORS'
      ERR_CATEGORY='SENSORS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c
      ! -------------
      ! size initialization for user sensor
      CALL SENSOR_USER_INIT()
      ! -------------
c
      CALL HM_READ_SENSORS(
     .     SENSORS   ,LACCELM  ,ITABM1    ,IPART     ,LGAUGE    ,
     .     SUBSETS   ,NSETS    ,IGRSURF   ,IGRNOD    ,BUFSF     ,
     .     SKEW      ,ISKWN    ,UNITAB    ,LSUBMODEL ,HM_NSENSOR)
      ! -----------------
      ! check if a user sensor is used with a list of node
      ! convert the list of User node ID into Local node ID

      IF(SENSOR_USER_STRUCT%IS_USED) THEN
          IF(SENSOR_USER_STRUCT%POINTER_NODE > 0) THEN
              CALL SENSOR_USER_CONVERT_LOCAL_ID(
     .         ITABM1,SENSOR_USER_STRUCT%POINTER_NODE,SENSOR_USER_STRUCT%NUMBER_NODE,
     .         SENSOR_USER_STRUCT%NODE_LIST,1,IPART)
          ENDIF
      ENDIF
        ! -----------------
      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES PRELOADS
C--------------------------------------------
      ERR_MSG='BOLT PRELOADING'
      ERR_CATEGORY='BOLT PRELOADING'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c

      NUMPRELOAD  = NPRELOAD
      IF(NPRELOAD /= 0) THEN
       IF(NSECT /= 0)THEN
        CALL PRELECSEC4BOLT(SNSTRF,SSECBUF,IGRNOD,ITABM1,0,
     .                      NOM_OPT(LNOPT1*INOM_OPT(8)+1),IGRBRIC,LSUBMODEL)
        ALLOCATE(NSTRF(SNSTRF)    ,STAT=stat)
        ALLOCATE(SECBUF(SSECBUF)  ,STAT=stat)
        NSTRF  = 0
        SECBUF = ZERO
        CALL LECSEC4BOLT(IXS    ,IXQ    ,IXC    ,IXT    ,IXP    ,IXR    ,
     2                   IXTG   ,X      ,NSTRF  ,ITAB   ,ITABM1 ,
     3                   IGRNOD ,SECBUF ,
     4                   IPARI  ,IXS10  ,IXS20  ,IXS16  ,UNITAB ,
     5                   ISKWN  ,XFRAME ,EANI,NOM_SECT,RTRANS,
     6                   LSUBMODEL,NOM_OPT(LNOPT1*INOM_OPT(8)+1),IGRBRIC)
       ENDIF
       CALL HM_PRE_READ_PRELOAD(NSTRF,LSUBMODEL) !to calculate NUMPRELOAD = NUMPRELOAD + NN (NN = NSTRF(K0+7) = NSEGS)
       SIPRELOAD = 3*NUMPRELOAD               !! sb - A ajuster
       SPRELOAD  = 6*NUMPRELOAD               !! sb - A ajuster
       ALLOCATE(IPRELOAD(SIPRELOAD)    ,STAT=stat)
       IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IPRELOAD')
       ALLOCATE(PRELOAD(SPRELOAD)    ,STAT=stat)
       IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='PRELOAD')
       ALLOCATE(IFLAG_BPRELOAD(NUMELS)    ,STAT=stat)
       IF(STAT /= 0) CALL ANCMSG(MSGID= 268,ANMODE=ANINFO,
     .                          MSGTYPE= MSGERROR,
     .                               C1= 'IFLAG_BPRELOAD')
       IPRELOAD       = 0
       IFLAG_BPRELOAD = 0
       PRELOAD        = ZERO

       WRITE(ISTDO,'(A)')TITRE(34)
       CALL HM_READ_PRELOAD(IXS   ,IXS10     ,IPRELOAD ,PRELOAD,IFLAG_BPRELOAD,
     .                      NSTRF ,SENSORS   ,UNITAB ,X        ,
     .                      EANI  ,ITAB      ,LSUBMODEL)
c
       IF(ALLOCATED(NSTRF))  DEALLOCATE(NSTRF)
       IF(ALLOCATED(SECBUF)) DEALLOCATE(SECBUF)
       SNSTRF  = 0
       SSECBUF = 0
      ELSE
        ALLOCATE(IPRELOAD(0)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IPRELOAD')
        ALLOCATE(PRELOAD(0)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='PRELOAD')
        ALLOCATE(IFLAG_BPRELOAD(0)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID= 268,ANMODE=ANINFO,
     .                          MSGTYPE= MSGERROR,
     .                               C1= 'IFLAG_BPRELOAD')
      ENDIF
      CALL TRACE_OUT1()
C-------------------------------------------------
C     LECTURE DES LIENS RIGIDES
C--------------------------------------------
      ERR_MSG='RIGID LINKS'
      ERR_CATEGORY='RIGID LINKS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL HM_PRE_READ_LINK(NUMLINK, IGRNOD,LSUBMODEL)
      SNNLINK = 10*NLINK
      SLNLINK = NUMLINK
      ALLOCATE(NNLINK(SNNLINK)    ,STAT=stat)
      ALLOCATE(LNLINK(SLNLINK)    ,STAT=stat)
      IF(SNNLINK > 0) NNLINK = 0
      IF(SLNLINK > 0) LNLINK = 0
      IF(NLINK > 0) THEN
      CALL HM_READ_LINK(
     1        NNLINK  ,LNLINK ,ITAB    ,ITABM1  ,D       ,
     2        IGRNOD  ,ISKWN  ,IFRAME  ,NOM_OPT(LNOPT1*INOM_OPT(9)+1),LSUBMODEL)
      ENDIF
C
      SFBVEL   = 3*NIBVEL
      SGRAV    = LFACGRV*NGRAV
      SFR_WAVE = NUMNOD*IFRWV
      SPARTS0  = NPART
      ALLOCATE(FBVEL(SFBVEL)        ,STAT=stat)
      ALLOCATE(GRAV   (SGRAV   )    ,STAT=stat)
      ALLOCATE(FR_WAVE(SFR_WAVE)    ,STAT=stat)
      ALLOCATE(PARTS0(SPARTS0)      ,STAT=stat)
      IF(SFBVEL > 0) FBVEL   = ZERO
      IF(SGRAV  > 0) GRAV    = ZERO
      IF(SFR_WAVE > 0) FR_WAVE = ZERO
      IF(SPARTS0  > 0) PARTS0  = ZERO
c--------------------------------------------
      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES ANCIENS POIDS
C--------------------------------------------
      CALL PRELEC_DDW(FILNAM,LEN_FILNAM,MARQUEUR3)
      IF(MARQUEUR3) THEN
        WRITE(IOUT,'(A)')' '
        WRITE(IOUT,'(A)')
     .  ' --------------------------------------'
        WRITE(IOUT,'(A)')
     .  ' DDW OPTION FOR THE DOMAIN DECOMPOSITION'
        WRITE(IOUT,'(A)')
     .  ' --------------------------------------'
       WRITE(ISTDO,*)
     . '.. DDW OPTION FOR THE DOMAIN DECOMPOSITION'

       ALLOCATE(TAB_UMP_OLD(7,TAILLE_OLD),STAT=stat)
       ALLOCATE(CPUTIME_MP_OLD(TAILLE_OLD),STAT=stat)
       TAB_UMP_OLD = ZERO
       CPUTIME_MP_OLD = ZERO

       CALL LEC_DDW(FILNAM,LEN_FILNAM,TAB_UMP_OLD,CPUTIME_MP_OLD)

       CALL PRELEC_DDW_POIN(FILNAM,LEN_FILNAM)
       ALLOCATE(POIN_UMP_OLD(NUMMAT_OLD), STAT=stat)
       POIN_UMP_OLD = ZERO

        CALL LEC_DDW_POIN(FILNAM,LEN_FILNAM,POIN_UMP_OLD)
      ELSE
       ALLOCATE(TAB_UMP_OLD(0,0),STAT=stat)
       ALLOCATE(CPUTIME_MP_OLD(0),STAT=stat)
       ALLOCATE(POIN_UMP_OLD(0), STAT=stat)
      ENDIF
C--------------------------------------------
C     USER S WINDOW
C--------------------------------------------
      ERR_MSG='USER WINDOWS'
      ERR_CATEGORY='USER WINDOWS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
      IF(USER_WINDOWS%HAS_USER_WINDOW /= 0 ) THEN
!
!==============
        CALL HM_READ_WINDOW_USER(USER_WINDOWS,LSUBMODEL,ITAB,
     *                           X, V, VR, MS, IN)
!==============
!
      ENDIF
      CALL TRACE_OUT1()
C---------------------------
C Calcul ELEM RBY ON/OFF pour domdec
C---------------------------
      ERR_MSG='RIGID BODIES ON'
      ERR_CATEGORY='RIGID BODIES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NUMELS /=0) ISOLOFF = 0
      IF(NUMELC /=0) ISHEOFF = 0
      IF(NUMELT /=0) ITRUOFF = 0
      IF(NUMELP /=0) IPOUOFF = 0
      IF(NUMELR /=0) IRESOFF = 0
      IF(NUMELTG /=0) ITRIOFF = 0
      IF(NUMELQ /=0) IQUAOFF = 0
      CALL SETRBYON(
     1   IXS      ,IXC     ,IXTG   ,IGRNOD  ,IGRNRBY ,
     2   ISOLOFF  ,ISHEOFF ,ITRIOFF,KNOD2ELS,KNOD2ELC,
     5   KNOD2ELTG,NOD2ELS ,NOD2ELC,NOD2ELTG,IXQ     ,
     6   IQUAOFF  ,KNOD2ELQ,NOD2ELQ,LSUBMODEL)
      CALL TRACE_OUT1()
C---------------------------
C Calcul ELEM OFF (RBE2) pour domdec
C---------------------------
      ERR_MSG='RBE2 ON'
      ERR_CATEGORY='RBE2'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL SETRB2ON(
     1   IXS    ,IXC    ,IXTG   ,IGRNOD ,
     2   IGRNRB2,ISOLOFF,ISHEOFF,ITRIOFF,ITABM1,
     3   LSUBMODEL)
      CALL TRACE_OUT1()
C---------------------------------------------
C Calcul FLEXIBLE BODY - ELEM OFF pour domdec
C---------------------------------------------
      ERR_MSG='FLEXIBLE BODIES ON'
      ERR_CATEGORY='FLEXIBLE BODIES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NFXBODY > 0)THEN
        LENMOD=0
        CALL HM_SETFXRBYON(ITABM1,IXS,ISOLOFF,IXC,ISHEOFF,
     1                     IXT,ITRUOFF,IXP,IPOUOFF,IXR,IRESOFF,
     2                     IXTG,ITRIOFF,FXBIPM,LSUBMODEL)
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES CLUSTERS DES ELEMENTS
C--------------------------------------------
      ERR_MSG='CLUSTERS'
      ERR_CATEGORY='CLUSTERS'
      ALLOCATE(CLUSTERS(NCLUSTER), STAT=Stat)
      CALL HM_READ_CLUSTER(
     .       CLUSTERS ,UNITAB   ,ISKWN    ,IGRBRIC  ,IGRSPRING,
     .       IXS      ,IXR      ,NOM_OPT(LNOPT1*INOM_OPT(28)+1),
     .       LSUBMODEL)
C--------------------------------------------
C     LECTURE DES INITIAL VOLUME FRACTIONS
C--------------------------------------------
        CALL HM_READ_INIVOL(USER_INIVOL , KVOL, IGRSURF ,IPART ,MULTI_FVM, BUFMAT, IPM, ITAB, NBSUBMAT, LSUBMODEL, UNITAB)

C--------------------------------------------
C     Surfaces fictives IGE
C--------------------------------------------
      IF(TAGSURFIGE>0) THEN
        SNIGE = IADTABIGE
        NIGE => NIGE_TMP(IDXIGE2)%ptr
C--------------------------------------------
        SRIGE = IADTABIGE
        RIGE => RIGE_TMP(IDXIGE2)%ptr2
C--------------------------------------------
        SXIGE = IADTABIGE
        XIGE => XIGE_TMP(IDXIGE2)%ptr2
C--------------------------------------------
        SVIGE = IADTABIGE
        VIGE => VIGE_TMP(IDXIGE2)%ptr2
      ENDIF
      CALL TRACE_OUT1()


C -------------------------------------------------
C Check des surfaces pour les airbags
C -------------------------------------------------
      CALL CHECK_SURF(IGRSURF,1)
C----------------------------------
C     LECTURE DES VOLUMES MONITORES
C----------------------------------
      ERR_MSG='MONITORED VOLUMES'
      ERR_CATEGORY='MONITORED VOLUMES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IBAGSURF = 0
      LIBAGALE = 0
      LRBAGALE = 0
      NVENTTOT = 0
      ALLOCATE(T_MONVOL(NVOLU + NMONVOL))
      CALL MONVOL_ALLOCATE(NVOLU + NMONVOL, T_MONVOL, T_MONVOL_METADATA)
      IF(NVOLU + NMONVOL> 0) THEN
        WRITE(ISTDO,'(A)') ' .. MONITORED VOLUMES '

        CALL READ_MONVOL(T_MONVOL, T_MONVOL_METADATA, ITAB, ITABM1, IPM, IGEO,
     .       X, PM, GEO, IXC, IXTG, SENSORS,
     .       UNITAB, NPC1, NPC, TF, IGRSURF, IGRBRIC, NOM_OPT(LNOPT1*INOM_OPT(2)+1),IFRAME, XFRAME,
     .       LSUBMODEL)

        CALL INIT_MONVOL(T_MONVOL   , T_MONVOL_METADATA,
     2       GEO     ,PM      ,
     3       IXC     ,IXTG    ,X       ,NPC1    ,
     4       ITABM1  ,ITAB      ,IGRSURF ,
     5       SENSORS ,IGRBRIC  ,SBUFALE ,
     6       NAIRWA  ,IXS     ,V       ,LIBAGALE,
     7       NPC     ,TF      ,LRBAGALE,IPM     ,IGEO    ,
     8       UNITAB  )

        LIBAGJET=0
        LRBAGJET=0
        LIBAGHOL=0
        LRBAGHOL=0
        LRCBAG  = 0
        LICBAG = NICBAG * NVOLU * NVOLU
        DO II = 1, NVOLU
           LIBAGJET = LIBAGJET + NIBJET * T_MONVOL(II)%NJET
           LRBAGJET = LRBAGJET + NRBJET * T_MONVOL(II)%NJET
           LIBAGHOL = LIBAGHOL + NIBHOL * T_MONVOL(II)%NVENT
           LRBAGHOL = LRBAGHOL + NRBHOL * T_MONVOL(II)%NVENT
           IF(T_MONVOL(II)%TYPE == 5 .OR. T_MONVOL(II)%TYPE == 9) THEN
              LRCBAG = LRCBAG + T_MONVOL(II)%NCA * NRCBAG
           ENDIF
        ENDDO

        SVOLMON0 = NRVOLU * NVOLU + LRCBAG + LRBAGJET + LRBAGHOL
        SVOLMON = SVOLMON0 + SBUFALE + 1
        ALLOCATE(VOLMON(SVOLMON), STAT = stat)
        VOLMON(1:SVOLMON) = ZERO

        SMONVOL = NIMV * NVOLU + LICBAG + LIBAGJET + LIBAGHOL + LIBAGALE
        ALLOCATE(MONVOL(SMONVOL), STAT = stat)
        MONVOL(1:SMONVOL) = 0
      ELSE
        ALLOCATE(MONVOL(0))
        ALLOCATE(VOLMON(0))
      ENDIF
      CALL TRACE_OUT1()
C---------------------------
C Calcul de DOF pour domdec implicite
C---------------------------
      ERR_MSG='IMPLICIT DOMAIN DECOMPOSITION'
      ERR_CATEGORY='IMPLICIT DOMAIN DECOMPOSITION'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL DSDIM0(
     1    DSDOF,IXS , IXQ,  IXC , IXT,
     2    IXP  ,IXR , IXTG, KXX,
     3    IXX  ,GEO )
C---------------------------
C DOMAIN DECOMPOSITION SUR MODELE COMPLET
C CEP tableau donnant pour chaque element le proc associe.
C allocation de taille NELEM
C---------------------------
C IDDLEVEL indique le niveau de la domdec :
C 0 == niveau 1) non prise en compte des interfaces (input v31)
C 1 == niveau 2) prise en compte des interfaces dans la dd (input v41)
C---------------------------
      IDDLEVEL = 0
      NELEMINT = 0
      IFIXIN = 1
      IFIEND = 1
      DO I = 1, NUMNOD
        IWCONT(I) = 0
        IWCONT(NUMNOD+I) = 0
        IWCIN2(I)= 0
        IWCIN2(NUMNOD+I)= 0
c save IENTRY
c replace save of old FRONT
c FRONT(I,NSPMD+1) = FRONT(I,1)
        IENTRY2(I) = IFRONT%IENTRY(I)
      ENDDO
        DO I=0,MAXLAW
           SOL1TNL(I,1)=ZERO
           SOL1TNL(I,2)=ZERO
           SOL1TNL(I,3)=ZERO

           SOL8TNL(I,1)=ZERO
           SOL8TNL(I,2)=ZERO
           SOL8TNL(I,3)=ZERO

           DO J=0,3
            SHTNL(I,J,1)=ZERO
            SHTNL(I,J,2)=ZERO
            SHTNL(I,J,3)=ZERO

            TRITNL(I,J,1)=ZERO
            TRITNL(I,J,2)=ZERO
            TRITNL(I,J,3)=ZERO
           ENDDO
        ENDDO
        DO I=1,10
          SOLTELT(I)=ZERO
          SHTELT(I)=ZERO
          TRITELT(I)=ZERO
        ENDDO
        TPSREF = ZERO
C       Sauvegarde longueurs tableaux lagmult
        LAG_NCF0 = LAG_NCF
        LAG_NKF0 = LAG_NKF
        LAG_NHF0 = LAG_NHF
        LAG_NCL0 = LAG_NCL
        LAG_NKL0 = LAG_NKL
      CALL TRACE_OUT1()
C-----------------
C  Update stack Due to DRAPE
C-------------------------
      IF(NDRAPE > 0 .AND. (IPART_STACK > 0 .OR. IPART_PCOMPP > 0)) THEN
         CALL SHELLTHK_UPD(DRAPE     ,STACK    ,THKE     ,IXC       ,IXTG      ,
     .                     IGEO      ,IWORKSH     ,DRAPEG%INDX)
      ENDIF
C-----------------------------------------------------
C     LECTURE DES /PERTURB( random noise sur les epaisseurs des shells/part )
C-----------------------------------------------------
      ERR_MSG='PERTURB'
      ERR_CATEGORY='PERTURB'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))

      IF(IPERTURB /= 0) THEN
        SRNOISE1=NPERTURB
        SRNOISE2=NUMELC+NUMELTG+NUMELS+NUMSPH
        ALLOCATE(RNOISE(NPERTURB,NUMELC+NUMELTG+NUMELS+NUMSPH))
        RNOISE(1:NPERTURB,1:NUMELC+NUMELTG+NUMELS+NUMSPH) = ZERO
        ALLOCATE(PERTURB(NPERTURB))
        PERTURB(1:NPERTURB) = 0
        ALLOCATE(QP_IPERTURB(NPERTURB,6))
        QP_IPERTURB(1:NPERTURB,1:6) = 0
        ALLOCATE(QP_RPERTURB(NPERTURB,4))
        QP_RPERTURB(1:NPERTURB,1:4) = ZERO
        CALL HM_READ_PERTURB(IPART   ,RNOISE   ,IPARTC   ,IPARTG  ,IPARTSP  ,
     .                  IGRPART ,IPM      ,IPARTS   ,PERTURB ,QP_IPERTURB,
     .                  QP_RPERTURB       ,LSUBMODEL,UNITAB  )
      ELSE
        SRNOISE1=1
        SRNOISE2=1
        ALLOCATE(RNOISE(1,1))
        RNOISE(1,1) = ZERO
        ALLOCATE(PERTURB(1))
        PERTURB(1) = 0
        ALLOCATE(QP_IPERTURB(0,0))
        ALLOCATE(QP_RPERTURB(0,0))
      ENDIF

      CALL TRACE_OUT1()
C-----------------
C  Global Mat for PID 11 and PID51 (for shell)
C-------------------------
!!      IF(IGLOBMAT > 0) THEN ! global flag can be added
      CALL GLOBMAT(IGEO  , GEO   ,PM ,STACK%PM, STACK%GEO,STACK%IGEO)
!!      ENDIF
C-------------------------
C Fill index to renumber Solid elements after Domain Decomposition
C Array has 2 parts :
C PERMUTATION%SOLID(1:NUMELS) : INDEX(NEW ID)=OLD_ID
C PERMUTATION%SOLID(NUMELS+1:2*NUMELS) : INDEX(OLD)=NEW_ID
      ALLOCATE(PERMUTATION%SOLID(MAX(2*NUMELS,1)))
      ALLOCATE(PERMUTATION%SHELL(MAX(2*NUMELC,1)))
      ALLOCATE(PERMUTATION%TRIANGLE(MAX(2*NUMELTG,1)))
      PERMUTATION%TRIANGLE = 0
      PERMUTATION%SHELL = 0
      PERMUTATION%SOLID = 0

C------------------------------------------------------------------------
C     REMPLACEMENT DES NOS EXTERNES DES FCTS ET SKEW PAR LES NOS SYSTEMES
C------------------------------------------------------------------------
      ERR_MSG='USER TO SYSTEM RENUMBERING'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL FSDCOD(BUFMAT   ,PM       ,GEO      ,IBCL      ,IPRES    ,
     .            IBFV     ,ISKEW    ,ISKWN    ,SENSORS   ,
     .            ITABM1   ,SKEW     ,LACCELM  ,BID13     ,BUFGEO   ,
     .            IBCSLAG  ,IGEO     ,IPM      ,
     .            DAMPR    ,IBFTEMP  ,IBCV     ,IBFV      ,
     .            IBCR     ,TABLE    ,NPC1     ,NPC       ,TF       ,
     .            NOM_OPT(LNOPT1*INOM_OPT(3)+1),IBFFLUX   )
C
c------------------------------------------------------------------------
c     Update & check parameters of material laws
c------------------------------------------------------------------------
      CALL UPDMAT(BUFMAT   ,PM      ,IPM       ,TABLE    ,NPC1     ,
     .            NPC      ,TF      ,SENSORS   ,NLOC_DMG ,MLAW_TAG ,
     .            MAT_ELEM%MAT_PARAM)
C
      CALL TRACE_OUT1()
C------------------------------------------------------------------------
C     OPTIONS SPH:
C     REMPLACEMENT DES NOS EXTERNES DES FCTS
C------------------------------------------------------------------------
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NSPHIO/=0)
     .  CALL SPHDCOD(NPC1,ISPHIO,NOM_OPT(LNOPT1*INOM_OPT(22)+1))
      CALL TRACE_OUT1()
C
C------------------------------------------------------------------------
C
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C
C 100 return adress for domain decomposition after reading of contact interfaces or AMS element selection
C
CXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX

 100  CONTINUE

      CALL INIT_PERMUTATION()
C
      IF(IDDLEVEL==1) THEN
        TOTADDMAS = ZERO
        MS = ZERO
        IN = ZERO
        MCP = ZERO
        MSNF = ZERO
C
        IF((NSUBDOM>0)) THEN
C       For multidomains - Mass and inertia must be nonzero for coupled nodes
          DO I=1,NUMNOD
            IF(TAGNO(I+N_PART) > 1) THEN
              MS(I)=1e-20
              IF(IRODDL==1) IN(I)=1e-20
            ENDIF
          END DO
        ENDIF
C
      ENDIF
C
c-----------------------------------------------------------------------
c     Create seatbelt entities + domdec
c-----------------------------------------------------------------------
C
      N_SEATBELT = 0
      IF(NB_MAT_SEATBELT > 0) THEN
        ERR_MSG='SEATBELTS'
        ERR_CATEGORY='SEATBELTS'
        CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
        WRITE(ISTDO,'(A)')' .. SEATBELT INITIALIZATION'
        CALL CREATE_SEATBELT(IXR,ITAB,KNOD2EL1D,NOD2EL1D,IPM,
     .                       X,SENSORS,BUFMAT,PM,GEO,
     .                       IDDLEVEL,KNOD2ELC,NOD2ELC,IXC,IGEO,
     .                       ISKWN   )
        CALL TRACE_OUT1()
      ENDIF
C
c-----------------------------------------------------------------------
C
      IF(ISMS == 0) THEN
        IF(.NOT. ALLOCATED(TAGPRT_SMS)) THEN
          ALLOCATE(TAGPRT_SMS(0))
          ALLOCATE(NATIV_SMS(0))
          ALLOCATE(T2MAIN_SMS(4,0))
        ENDIF
      ELSEIF(ISMS/=0)THEN
        ERR_MSG='AMS'
        ERR_CATEGORY='AMS'
        WRITE(ISTDO,'(A)')' .. AMS INITIALIZATION'
        IF(IDDLEVEL==0) THEN
C
          ERR_MSG='AMS INITIALIZATION PHASE I'
          CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
          ALLOCATE(TAGPRT_SMS(NPART),NATIV_SMS(NUMNOD),T2MAIN_SMS(4,NUMNOD),STAT=stat)
C
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                         C1='TAGPRT_SMS / NATIV_SMS / T2MAIN_SMS')
          TAGPRT_SMS=0
          NATIV_SMS(1:NUMNOD)=0
          T2MAIN_SMS=0
C
          CALL INISMS(IGRPART  ,IPARTS     ,IPARTQ   ,IPARTC  ,
     .            IPARTT   ,IPARTP    ,IPARTR      ,IPARTG  ,
     .                IPARTX   ,TAGPRT_SMS )
C
          CALL TRACE_OUT1()
C
        ELSE
C
C         AMS Prepare DOMETIS
C
          ERR_MSG='AMS INITIALIZATION PHASE II'
          CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
          ALLOCATE(KINWORK(NUMNOD),STAT=STAT)
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                         C1='KINWORK')
          CALL KINREM(D       ,KINWORK ,RWBUF   ,ITAB   ,NPRW   ,
     .                LPRW    ,NPBY    ,LPBY    )
C
          ALLOCATE(TAGREL_SMS(NGROUP),TAGSLV_RBY_SMS(NUMNOD),TAGMSR_RBY_SMS(NUMNOD),
     .             KAD_SMS(NUMNOD+1), JAD_SMS(NUMNOD+1), IAD_SMS(NUMNOD+1), LAD_SMS(NUMNOD+1),
     .                JADC_SMS(4*NUMELC),
     .                JADS_SMS(8*NUMELS), JADS10_SMS(6*NUMELS10),
     .                JADT_SMS(2*NUMELT),
     .                JADP_SMS(2*NUMELP),
     .                JADR_SMS(3*NUMELR),
     .                JADTG_SMS(3*NUMELTG), JADRB_SMS(NRBODY),
     .             STAT=STAT)
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                         C1='TAGREL_SMS')
C
C
          CALL SMS_INIT(
     1     IXS     ,IXQ      ,IXC     ,IXT     ,IXP       ,
     2     IXR     ,IXTG     ,IXTG1   ,IXS10   ,IXS16     ,
     3     IXS20   ,IPARG    ,DSDOF   ,
     4     ICODT   ,ICODR    ,KINWORK ,
     5                        IPARTS     ,IPARTQ   ,IPARTC  ,
     6     IPARTT   ,IPARTP   ,IPARTR      ,IPARTG  ,
     7     IPARTX   ,TAGPRT_SMS ,ITAB   ,IRBE2   ,
     8     IRBE3    ,LRBE2      ,LRBE3      ,NPRW   ,LPRW    ,
     9     IPART    ,IGEO       ,IPM        ,NATIV_SMS,NPBY  ,
     A     LPBY       ,TAGMSR_RBY_SMS,TAGSLV_RBY_SMS,NOM_OPT)
C
          CALL SMS_INI_KAD(
     1     IXS     ,IXQ      ,IXC     ,IXT     ,IXP       ,
     2     IXR     ,IXTG     ,IXTG1   ,IXS10   ,IXS16     ,
     3     IXS20   ,IPARG    ,MS      ,MS0     ,DSDOF     ,
     4     ICODT   ,ICODR    ,KINET   ,
     5     KAD_SMS                    ,IPARTS   ,IPARTQ   ,
     6     IPARTC  ,IPARTT  ,IPARTP   ,IPARTR    ,
     7     IPARTG  ,IPARTX  ,TAGPRT_SMS,TAGREL_SMS,ITAB    ,
     8     IRBE2   ,IRBE3     ,LRBE2    ,LRBE3    ,
     9     NPRW    ,LPRW      ,IPART    ,IGEO     ,NATIV_SMS)

C
          ALLOCATE(KDI_SMS(KNZ_SMS),PK_SMS(KNZ_SMS),
     .             STAT=STAT)
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                         C1='KDI_SMS')
C
          CALL SMS_INI_KDI(
     2             IXC      ,IPARG   ,IXS      ,IXT      ,IXP     ,
     3             IXR      ,IXTG    ,IXS10    ,DSDOF    ,KAD_SMS ,
     4             KDI_SMS  ,JADC_SMS,JADS_SMS ,JADS10_SMS ,
     5             JADT_SMS ,JADP_SMS,
     6             JADR_SMS,JADTG_SMS,TAGPRT_SMS,IAD_SMS ,
     7             TAGREL_SMS,IPARTS ,IPARTQ   ,IPARTC   ,IPARTT   ,
     8             IPARTP    ,IPARTR ,IPARTG   ,IPARTX   ,
     9             NPBY     ,LPBY     ,KINET   ,TAGSLV_RBY_SMS,IPARI  ,
     A             INTBUF_TAB,LAD_SMS ,IPART   ,IGEO     ,NATIV_SMS)
C
          ALLOCATE(IDI_SMS(NNZ_SMS),JDI_SMS(NNZ_SMS),STAT=STAT)
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                         C1='JDI_SMS')
C
          CALL SMS_INI_JAD_1(
     2           IXC      ,IPARG   ,IXS      ,IXT       ,IXP      ,
     3           IXR      ,IXTG    ,IXS10    ,DSDOF     ,JADC_SMS ,
     4           JADS_SMS ,JADS10_SMS,JADT_SMS ,JADP_SMS,JADR_SMS ,
     5           JADTG_SMS,TAGPRT_SMS,KAD_SMS,KDI_SMS  ,PK_SMS    ,
     6           TAGREL_SMS,IPARTS ,IPARTQ   ,IPARTC   ,IPARTT    ,
     7           IPARTP    ,IPARTR ,IPARTG   ,IPARTX    ,
     8           NPBY     ,LPBY     ,KINET   ,TAGSLV_RBY_SMS,IPARI ,
     9           INTBUF_TAB,LAD_SMS ,IPART   ,IGEO      ,NATIV_SMS ,
     A           IAD_SMS   ,IDI_SMS,JAD_SMS  ,JDI_SMS  ,T2MAIN_SMS)

          DEALLOCATE(JDI_SMS)

          ALLOCATE(JDI_SMS(NNZ_SMS),STAT=STAT)
          IF(STAT/=0) THEN
            CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .          C1='JDI_SMS')
            CALL ARRET(2)
          ENDIF

          CALL SMS_INI_JAD_2(
     2           IXC      ,IPARG   ,IXS      ,IXT      ,IXP     ,
     3           IXR      ,IXTG    ,IXS10    ,DSDOF    ,JADC_SMS ,
     4           JADS_SMS ,JADS10_SMS,JADT_SMS,JADP_SMS,JADR_SMS ,
     5           JADTG_SMS,TAGPRT_SMS,KAD_SMS,KDI_SMS  ,
     7           TAGREL_SMS,IPARTS ,IPARTQ   ,IPARTC   ,IPARTT   ,
     8           IPARTP    ,IPARTR ,IPARTG   ,IPARTX   ,
     9           NPBY     ,LPBY  ,KINET      ,TAGSLV_RBY_SMS,IPARI   ,
     A           INTBUF_TAB,LAD_SMS   ,NPRW  ,LPRW    ,TAGMSR_RBY_SMS,
     C           INTSTAMP  ,IPART     ,IGEO  ,NATIV_SMS,IRBE2        ,
     D           LRBE2     ,IAD_SMS   ,IDI_SMS,JAD_SMS ,JDI_SMS  ,
     E           T2MAIN_SMS)

          DEALLOCATE(JDI_SMS)

          ALLOCATE(JDI_SMS(NNZ_SMS),STAT=STAT)
          IF(STAT/=0) THEN
            CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .          C1='JDI_SMS')
            CALL ARRET(2)
          ENDIF
          ALLOCATE(JSM_SMS(NNZ_SMS),STAT=STAT)
          IF(STAT/=0) THEN
            CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .          C1='JSM_SMS')
            CALL ARRET(2)
          ENDIF
C
          CALL SMS_INI_JAD_3(
     2           IXC      ,IPARG   ,IXS      ,IXT      ,IXP     ,
     3           IXR      ,IXTG    ,IXS10    ,DSDOF    ,JADC_SMS,
     4           JADS_SMS ,JADS10_SMS,JADT_SMS ,JADP_SMS,JADR_SMS ,
     5           JADTG_SMS,TAGPRT_SMS,KAD_SMS  ,KDI_SMS ,
     6           TAGREL_SMS,IPARTS ,IPARTQ   ,IPARTC   ,IPARTT   ,
     7           IPARTP    ,IPARTR ,IPARTG   ,IPARTX   ,
     8           NPBY     ,LPBY     ,KINET    ,
     9           TAGSLV_RBY_SMS,IPARI  ,INTBUF_TAB,
     A           LAD_SMS ,JSM_SMS ,INTSTAMP  ,IPART    ,
     B           IGEO    ,TAGMSR_RBY_SMS,NATIV_SMS,
     C           IAD_SMS   ,IDI_SMS,JAD_SMS  ,JDI_SMS  ,T2MAIN_SMS)
C
          DEALLOCATE(KINWORK)
          DEALLOCATE(T2MAIN_SMS)
          CALL TRACE_OUT1()
C
C         If no element selected AMS is deactivated
          IF((ISMS_SELEC >= 2).AND.(NNZ_SMS == 0)) ISMS_SELEC = 0
C
        END IF
C
      END IF
C-----
      NELEM = NUMELC+NUMELTG+NUMELS+NUMELR
     +      + NUMELP+NUMELT+NUMELQ+NUMELX+NUMELIG3D
C

      ALLOCATE(IELEM21(NELEM),STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IELEM21')
      IELEM21=0
C
      ERR_MSG='DOMAIN DECOMPOSITION'
      ERR_CATEGORY='DOMAIN DECOMPOSITION'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(IDDLEVEL==1) THEN
         NGROUP = 0
         LBUFEL = 0
         SEGINDX=0
c treatment for new IFRONT
c reinit
         CALL INI_IFRONT()
c reset with savec IENTRY2
         DO I=1,NUMNOD
           IF(IENTRY2(I)/=-1)THEN
             CALL IFRONTPLUS(I,1)
           ENDIF
         ENDDO
C        Remise a jour de FRONT pour les procs differents de 0
C        IL faut prendre en compte les options ou front(i,p) = 1, p<>1
C        les sensors de type 2 modifient front sur p<>1
      ELSEIF(IDDLEVEL==0) THEN
         IF(NELEM+NCONLD+NUMCONV+NUMRADIA+NFXFLUX+SLCFIELD>0) THEN
           SCEP = NELEM+NCONLD+NUMCONV+NUMRADIA+NFXFLUX+SLLOADP+NUMBER_LOAD_CYL
           SCEL = NELEM+NCONLD+NUMCONV+NUMRADIA+NFXFLUX+SLLOADP+NUMBER_LOAD_CYL
           ALLOCATE(CEP(SCEP))
           ALLOCATE(CEL(SCEL))
         ELSE
           SCEP = 1
           SCEL = 1
           ALLOCATE(CEP(SCEP))
           ALLOCATE(CEL(SCEL))
         ENDIF
      ENDIF
      EMAX = MAX(NUMELC,NUMELTG,NUMELS,NUMELR,
     .             NUMELP,NUMELT,NUMELQ,NUMELX,NUMELIG3D)
      K1 = 1
      K2 = K1+EMAX
      K3 = K2+EMAX
      K4 = K3+2*EMAX
      K5 = K4+2*EMAX
      K6 = K5 + NELEM
      K7 = K6 + NELEM
      K8 = K7 + NELEM
C allocation moyenne normalement suffisante si taille de groupe moyenne > NVSIZ/2
      LDD_IAD =  ((NELEM+NUMSPH)/NVSIZ/2)*(NSPMD+1)
      ALLOCATE(DD_TMP(LDD_IAD) ,STAT=stat)
      DD_TMP = 0
      IDX    = 1

      ALLOCATE(IWORK(K8) ,STAT=stat)
      IF(EMAX>0) THEN
        ITRI1  => IWORK(1:K2)
        ITRI2  => IWORK(K2+1:K3)
        INDEX1 => IWORK(K3+1:K4)
        INDEX2 => IWORK(K4+1:K5)
      ELSE
        ITRI1  => IWORK
        ITRI2  => IWORK
        INDEX1 => IWORK
        INDEX2 => IWORK
      END IF
      IF(NELEM>0) THEN
        INUM   => IWORK(K5+1:K6)
        IWD    => IWORK(K6+1:K7)
        IWEIG  => IWORK(K7+1:K8)
      ELSE
          INUM   => IWORK
          IWD    => IWORK
          IWEIG  => IWORK
      END IF

      IF(.NOT.ALLOCATED(IXINT)) ALLOCATE(IXINT(0))
      IF(.NOT. ALLOCATED(NPBY)) ALLOCATE(NPBY(0))
      IF(.NOT. ALLOCATED(LPBY)) ALLOCATE(LPBY(0))
      IF(.NOT. ALLOCATED( RBY)) ALLOCATE( RBY(0))

          CALL DOMETIS(
     1     IXS       ,IXQ      ,IXC      ,IXT     ,IXP      ,
     2     IXR       ,IXTG     ,CEP     ,GEO      ,
     3     ITRI1     ,ITRI2    ,INDEX1   ,INDEX2  ,INUM     ,
     4     IWD       ,IWCONT   ,NELEM    ,IDDLEVEL,NELEMINT ,
     5     IXINT     ,PM       ,X        ,KXX     ,IXX      ,
     6     ADDCNE    ,IGEO     ,EANI     ,IWCIN2  ,DSDOF    ,
     7     ISOLOFF   ,ISHEOFF  ,ITRIOFF  ,ITRUOFF ,IPOUOFF  ,
     8     IRESOFF   ,IELEM21  ,IPM      ,IXS10   ,D        ,
     9     CLUSTERS  ,KXIG3D   ,IXIG3D   ,COST_R2R,BUFMAT,
     1     TAILLE   ,POIN_UMP,TAB_UMP  ,
     2     POIN_UMP_OLD,TAB_UMP_OLD,CPUTIME_MP_OLD,
     3     NSNT, NMNT_2,TABMP_L,IQUAOFF,
     4     IGRSURF  , FVMAIN,
     5     ITAB      ,IPART    ,IPARTC   ,IPARTG   ,IPARTS,
     6     POIN_PART_SHELL,POIN_PART_TRI,POIN_PART_SOL,
     7     MID_PID_SHELL,MID_PID_TRI,MID_PID_SOL,T_MONVOL,
     8     EBCS_TAG_CELL_SPMD,NPBY,LPBY)

      DEALLOCATE(IWORK)
C---------------------------
C     Domdec SPH
C---------------------------
      IF(NUMSPH > 0)THEN
          IF(IDDLEVEL==0) THEN
            ALLOCATE(CEPSP(NUMSPH),STAT=stat)
            IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                                 MSGTYPE=MSGERROR,
     .                             C1='CEPSP')

            ALLOCATE(CELSPH(NUMSPH),STAT=stat)
            IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                                 MSGTYPE=MSGERROR,
     .                             C1='CELSPH')
          END IF

            CALL SPDOMETIS(KXSP, IXSP, NOD2SP, CEPSP, RESERVEP,
     .                     SPH2SOL, CEP)
      ELSE
          IF(IDDLEVEL==0) ALLOCATE(CELSPH(1))
          IF(.NOT.(ALLOCATED(CEPSP))) ALLOCATE(CEPSP(0),STAT=stat)
      END IF
C---------------------------
C     IMPACTS LASER TRAITEMENT SPMD 1ere Phase
C---------------------------
      IF(NLASER>0) THEN
        CALL LASERP1(ILAS   ,CEP,IXQ    )
      ENDIF
      OFF = 1
      CALL TRACE_OUT1()
C---------------------------
C     DEFINE ELEMENT GROUPS
C---------------------------
      ERR_MSG='ELEMENTS GROUPS'
      ERR_CATEGORY='ELEM/PROP/MAT COMPATIBILITY'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))

      NUMELCK8     =    NUMELC
      NUMELTGK8    =    NUMELTG
      NUMELSK8     =    NUMELS
      NUMELRK8     =    NUMELR
      NUMELPK8     =    NUMELP
      NUMELTK8     =    NUMELT
      NUMELQK8     =    NUMELQ
      NUMELXK8     =    NUMELX
      NUMELIG3DK8  =    NUMELIG3D
      NUMSPHK8     =    NUMSPH
! working int8 to avoid integer overflow for large models
      EMAX = MAX(23*NUMELCK8,24*NUMELTGK8+1,29*NUMELSK8+1,18*NUMELRK8,
     .           18*NUMELPK8+1,16*NUMELTK8,19*NUMELQK8,
     .           15*NUMELXK8+1,24*NUMELIG3DK8+1,NUMSPHK8) + 1

      ALLOCATE(IPARGTMP(NPARG,NUMEL)    ,STAT=stat)
      IPARGTMP = 0
      ALLOCATE(IWORK(EMAX) ,STAT=stat)
      IF(STAT /= 0) THEN
        CALL ANCMSG(MSGID=727,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANSTOP,
     .              C1='IPARG')
      ENDIF
C     REMPLACEMENT DES NOS EXTERNES DES SS-MATERIAUX PAR LES NOS SYSTEMES
      IF(IDDLEVEL == 0) CALL M20DCOD(MLAW_TAG,IPM, PM)
C
C adresse temporaire de DD_IAD
C nombre de super groupes
      NSPGROUP = 0
C buffer max (super groupe)
      LB_MAX = 0
C
      WRITE(ISTDO,'(A)')TITRE(37)
C---------------------------------
C-  PRE TRI + SUPER GROUPES
C---------------------------------
      NGR_SOL = 0
      IF(NUMELS/=0) THEN
        K0 = 1
        K1 = K0 + NUMELS*16
        K2 = K1 + NUMELS
        K3 = K2 + NUMELS+1
        K4 = K3 + NUMELS*2
        K5 = K4 + NUMELS*7
        K5B= K4 + NUMELS
        K6 = K5 + NUMELS
        !warning: please also update any index change
        !         for MODIF option (MODIF_SPMD.F)
        IWORK  = 0
        INUM   => IWORK(1:K1)
        ITRI1  => IWORK(K1+1:K2)
        EADD   => IWORK(K2+1:K3)
        INDEX1 => IWORK(K3+1:K4)
        ITRI2  => IWORK(K4+1:K5)
        ITRI3  => IWORK(K5+1:K6)
C
C
        CALL SGRHEAD(
     1       IXS     ,PM     ,GEO    ,INUM   ,BID13   ,
     2       ITRI1   ,EADD   ,INDEX1 ,ITRI2  ,IPARTS  ,
     3       ND      ,IGRSURF,IGRBRIC,EANI   ,
     4       CEP(OFF),ITRI3  ,IXS10  ,IXS20  ,IXS16   ,
     5       IGEO   ,IPM    ,NOD2ELS,ISOLOFF ,
     6       TAGPRT_SMS,SPH2SOL,SOL2SPH,
     7       SOL2SPH_TYP    ,IFLAG_BPRELOAD, CLUSTERS ,
     8       RNOISE(1,MIN(SRNOISE2,NUMELC+NUMELTG+1)))
C---------------------------------
C-  GROUPAGE SPMD
C---------------------------------
C test non depassement de LDD_IAD
        IF(IDX+ND*(NSPMD+1)>LDD_IAD)THEN
          ALLOCATE(DD_TMP2(IDX-1))
          DO I = 1, IDX-1
            DD_TMP2(I)=DD_TMP(I)
          END DO
          DEALLOCATE(DD_TMP)
          ALLOCATE(DD_TMP(IDX+ND*(NSPMD+1)))
          DO I = 1, IDX-1
            DD_TMP(I)=DD_TMP2(I)
          END DO
          DEALLOCATE(DD_TMP2)
        END IF
C
        GRSOL_ID1 = NGROUP
        CALL SGRTAILS(
     1       IXS      ,PM       ,IPARGTMP ,GEO      ,
     2       EADD     ,ND       ,IPARTS   ,DD_TMP(IDX),
     3       IDX      ,EANI     ,INUM     ,INDEX1   ,
     4       CEP(OFF) ,ITRI1    ,IXS10    ,IGRSURF  ,IGRBRIC  ,
     5       IXS20    ,IXS16    ,IGEO     ,IDDLEVEL,
     6       IPM      ,NOD2ELS  ,ISOLOFF  ,ISOLNOD  ,
     7       TAGPRT_SMS,USER_INIVOL  ,SPH2SOL  ,SOL2SPH  ,SOL2SPH_TYP,
     8       IFLAG_BPRELOAD, CLUSTERS ,MAT_ELEM%MAT_PARAM ,RNOISE(1,MIN(SRNOISE2,NUMELC+NUMELTG+1)),
     9       IPRI)
        GRSOL_ID2 = NGROUP
        NGR_SOL   = GRSOL_ID2 - GRSOL_ID1
C
        OFF = OFF + NUMELS
C After IDDLEVEL Finish Indexes - Fill PERMUTATION%SOLID(NUMELS+1,PERMUTATION%SOLID(2*NUMELS)
        DO I=1,NUMELS
           N=PERMUTATION%SOLID(I)
           PERMUTATION%SOLID(NUMELS+N)=I
        ENDDO
!       already done in SGRHEAD / SGRTAILS
!        CALL APPLYSORT2CLUSTER(CLUSTERS,PERMUTATION%SOLID(NUMELS+1:2*NUMELS))
        CALL APPLYSORT2FLUX(IBFFLUX,NITFLUX,NFXFLUX,PERMUTATION%SOLID(NUMELS+1:2*NUMELS))
        CALL APPLYSORT2FLUX(IBCR,NIRADIA   ,NUMRADIA,PERMUTATION%SOLID(NUMELS+1:2*NUMELS))
        CALL APPLYSORT2FLUX(IBCV,NICONV    ,NUMCONV,PERMUTATION%SOLID(NUMELS+1:2*NUMELS))
      ENDIF

C
C---- QUADS
C
      IF(NUMELQ/=0) THEN
          K1 = 9*NUMELQ
          K2 = 10*NUMELQ
          K3 = 11*NUMELQ+1
          K4 = 13*NUMELQ+1
          K5 = 18*NUMELQ+1
          K6 = 19*NUMELQ+1
          !warning: please also update any index change
          !         for MODIF option (MODIF_SPMD.F)
          IWORK  = 0
          INUM   => IWORK(1:K1)
          ITR1   => IWORK(K1+1:K2)
          EADD   => IWORK(K2+1:K3)
          INDEX1 => IWORK(K3+1:K4)
          ITRI1  => IWORK(K4+1:K5)
          XEP    => IWORK(K5+1:K6)
C
          CALL QGRHEAD(
     1      IXQ       ,PM       ,GEO       ,INUM     ,BID13    ,
     2      ITR1      ,EADD     ,INDEX1    ,ITRI1    ,IPARTQ   ,
     4      ND        ,IGRSURF  ,IGRQUAD   ,CEP(OFF) ,
     5      XEP       ,IGEO      ,IPM      ,IQUAOFF  )
C---------------------------------
C-  GROUPAGE SPMD
C---------------------------------
C test non depassement de LDD_IAD
          IF(IDX+ND*(NSPMD+1)>LDD_IAD)THEN
            ALLOCATE(DD_TMP2(IDX-1))
            DO I = 1, IDX-1
              DD_TMP2(I)=DD_TMP(I)
            END DO
            DEALLOCATE(DD_TMP)
            ALLOCATE(DD_TMP(IDX+ND*(NSPMD+1)))
            DO I = 1, IDX-1
              DD_TMP(I)=DD_TMP2(I)
            END DO
            DEALLOCATE(DD_TMP2)
          END IF
C
          CALL QGRTAILS(
     1      IXQ       ,PM       ,IPARGTMP   ,GEO     ,
     2      EADD      ,ND       ,DD_TMP(IDX),IDX     ,
     3      INUM      ,INDEX1   ,CEP(OFF)   ,IPARTQ  ,
     4      ITR1      ,IGRSURF  ,IGRQUAD    ,
     5      IGEO      ,IPM      ,IQUAOFF    ,USER_INIVOL, IPRI)
          OFF = OFF + NUMELQ
       ENDIF
C
C---- COQUES
C
      IF(NUMELC/=0) THEN
C---------------------------------
C-  PRE TRI + SUPER GROUPES
C---------------------------------
       K0 = 1
       K1 = 9*NUMELC
       K2 = 11*NUMELC
       K3 = 12*NUMELC+1
       K4 = 14*NUMELC+1
       K5 = 21*NUMELC+1
       K6 = 22*NUMELC+1
       K7 = 23*NUMELC+1
       !warning: please also update any index change
       !         for MODIF option (MODIF_SPMD.F)
       IWORK  = 0
       INUM   => IWORK(1:K1)
       ITR1   => IWORK(K1+1:K1+NUMELC)
       ITR2   => IWORK(K1+NUMELC+1:K2)
       EADD   => IWORK(K2+1:K3)
       INDEX1 => IWORK(K3+1:K4)
       ITRI1  => IWORK(K4+1:K5)
       XEP    => IWORK(K5+1:K6)
       ALLOCATE(XNUM(NUMELC) ,STAT=stat)
       XNUM = ZERO
C
       CALL CGRHEAD(
     1   IXC     ,PM      ,GEO     ,INUM    ,BID13   ,
     2   ITR1    ,EADD    ,INDEX1  ,ITRI1   ,XNUM    ,
     3   IPARTC  ,ND      ,THKE    ,IGRSURF ,IGRSH4N ,
     4   CEP(OFF),XEP     ,IGEO    ,IPM     ,
     5   IPART   ,SH4TREE ,NOD2ELC ,ISHEOFF ,SH4TRIM ,
     6   TAGPRT_SMS,LGAUGE,IWORKSH ,
     7   STACK   ,DRAPE   ,RNOISE  ,SH4ANG,DRAPEG)
C---------------------------------
C-  GROUPAGE SPMD
C---------------------------------
c
C test non depassement de LDD_IAD
         IF(IDX+ND*(NSPMD+1)>LDD_IAD)THEN
           ALLOCATE(DD_TMP2(IDX-1))
           DO I = 1, IDX-1
             DD_TMP2(I)=DD_TMP(I)
           END DO
           DEALLOCATE(DD_TMP)
           ALLOCATE(DD_TMP(IDX+ND*(NSPMD+1)))
           DO I = 1, IDX-1
             DD_TMP(I)=DD_TMP2(I)
           END DO
           DEALLOCATE(DD_TMP2)
         END IF

C
         CALL CGRTAILS(
     1        IXC      ,PM       ,IPARGTMP ,GEO     ,
     2        EADD     ,ND       ,IPARTC  ,DD_TMP     ,
     3        IDX      ,INUM     ,ITR1    ,
     4        INDEX1   ,CEP(OFF) ,THKE    ,XNUM       ,
     5        IGRSURF  ,IGRSH4N  ,IGEO     ,IPM     ,
     6        IPART    ,SH4TREE  ,NOD2ELC  ,ISHEOFF ,
     7        SH4TRIM  ,TAGPRT_SMS, LGAUGE,IWORKSH    ,
     8        STACK    ,DRAPE   ,RNOISE  ,MAT_ELEM%MAT_PARAM,
     9        SH4ANG, IDDLEVEL , DRAPEG,IPRI)

         OFF = OFF + NUMELC

         DO I=1,NUMELC
            N=PERMUTATION%SHELL(I)
            PERMUTATION%SHELL(NUMELC+N)=I
         ENDDO

         DEALLOCATE(XNUM)
      ENDIF
C------
      IF(NUMELT/=0) THEN
       K1 = 7*NUMELT
       K2 = 8*NUMELT
       K3 = 9*NUMELT+1
       K4 = 11*NUMELT+1
       K5 = 15*NUMELT+1
       K6 = 16*NUMELT+1
       !warning: please also update any index change
       !         for MODIF option (MODIF_SPMD.F)
       IWORK  = 0
       INUM   => IWORK(1:K1)
       ITR1   => IWORK(K1+1:K2)
       EADD   => IWORK(K2+1:K3)
       INDEX1 => IWORK(K3+1:K4)
       ITRI1  => IWORK(K4+1:K5)
       XEP    => IWORK(K5+1:K6)
C
       CALL TGRHEAD(
     1      IXT     ,PM     ,GEO    ,INUM   ,BID13   ,
     2      ITR1    ,EADD   ,INDEX1 ,ITRI1  ,
     3      IPARTT  ,ND     ,IGRSURF,IGRTRUSS,
     4      CEP(OFF),XEP    ,ITRUOFF,
     5      TAGPRT_SMS)
C---------------------------------
C-  GROUPAGE SPMD
C---------------------------------
C test non depassement de LDD_IAD
          IF(IDX+ND*(NSPMD+1)>LDD_IAD)THEN
            ALLOCATE(DD_TMP2(IDX-1))
            DO I = 1, IDX-1
              DD_TMP2(I)=DD_TMP(I)
            END DO
            DEALLOCATE(DD_TMP)
            ALLOCATE(DD_TMP(IDX+ND*(NSPMD+1)))
            DO I = 1, IDX-1
              DD_TMP(I)=DD_TMP2(I)
            END DO
            DEALLOCATE(DD_TMP2)
          END IF
C
          CALL TGRTAILS(
     1      IXT    ,IPARGTMP,PM       ,GEO     ,
     2      EADD   ,ND      ,DD_TMP   ,IDX     ,
     3      INUM   ,INDEX1  ,CEP(OFF) ,IPARTT  ,
     4      ITR1   ,IGRSURF ,IGRTRUSS ,ITRUOFF  ,
     5      TAGPRT_SMS,NOD2EL1D,IPRI)
          OFF = OFF + NUMELT
      ENDIF
C
C-----
      IF(NUMELP > 0) THEN
       K1 = 8*NUMELP
       K2 = 9*NUMELP
       K3 = 10*NUMELP+1
       K4 = 12*NUMELP+1
       K5 = 17*NUMELP+1
       K6 = 18*NUMELP+1
      !warning: please also update any index change
      !         for MODIF option (MODIF_SPMD.F)
       IWORK  = 0
       INUM   => IWORK(1:K1)
       ITR1   => IWORK(K1+1:K2)
       EADD   => IWORK(K2+1:K3)
       INDEX1 => IWORK(K3+1:K4)
       ITRI1  => IWORK(K4+1:K5)
       XEP    => IWORK(K5+1:K6)
       CALL PGRHEAD(
     1   IXP      ,PM       ,GEO      ,INUM     ,
     2   ITR1     ,EADD     ,INDEX1   ,ITRI1    ,IPARTP   ,
     3   ND       ,IGRSURF  ,IGRBEAM  ,CEP(OFF) ,
     4   XEP      ,IGEO     ,IPOUOFF  ,TAGPRT_SMS , IPM )
C---------------------------------
C-  GROUPAGE SPMD
C---------------------------------
C test non depassement de LDD_IAD
          IF(IDX+ND*(NSPMD+1)>LDD_IAD)THEN
            ALLOCATE(DD_TMP2(IDX-1))
            DO I = 1, IDX-1
              DD_TMP2(I)=DD_TMP(I)
            END DO
            DEALLOCATE(DD_TMP)
            ALLOCATE(DD_TMP(IDX+ND*(NSPMD+1)))
            DO I = 1, IDX-1
              DD_TMP(I)=DD_TMP2(I)
            END DO
            DEALLOCATE(DD_TMP2)
          END IF
C
          CALL PGRTAILS(
     1      IXP    ,IPARGTMP,PM         ,GEO     ,
     2      EADD   ,ND      ,DD_TMP     ,IDX     ,
     3      INUM   ,INDEX1  ,CEP(OFF)   ,IPARTP  ,
     4      ITR1   ,IGRSURF ,IGRBEAM    ,IGEO    ,
     5      IPM    ,IPOUOFF ,TAGPRT_SMS,
     6      NOD2EL1D, IPRI)
          OFF = OFF + NUMELP
        ENDIF
C
C-----
C
      IF(NUMELR/=0) THEN
        K1 = 9*NUMELR
        K2 = 10*NUMELR
        K3 = 11*NUMELR+1
        K4 = 13*NUMELR+1
        K5 = 17*NUMELR+1
        K6 = 18*NUMELR+1
        !warning: please also update any index change
        !         for MODIF option (MODIF_SPMD.F)
       IWORK  = 0
       INUM   => IWORK(1:K1)
       ITR1   => IWORK(K1+1:K2)
       EADD   => IWORK(K2+1:K3)
       INDEX1 => IWORK(K3+1:K4)
       ITRI1  => IWORK(K4+1:K5)
       XEP    => IWORK(K5+1:K6)
C
          CALL RGRHEAD(
     1      IXR     ,GEO    ,INUM   ,BID13   ,IGEO   ,
     2      ITR1    ,EADD   ,INDEX1 ,ITRI1   ,
     4      IPARTR  ,ND     ,IGRSURF,IGRSPRING,
     5      CEP(OFF),XEP    ,IRESOFF,
     6      TAGPRT_SMS, CLUSTERS,IPM,R_SKEW)
C---------------------------------
C-  GROUPAGE SPMD
C---------------------------------
C test non depassement de LDD_IAD
          IF(IDX+ND*(NSPMD+1)>LDD_IAD)THEN
            ALLOCATE(DD_TMP2(IDX-1))
            DO I = 1, IDX-1
              DD_TMP2(I)=DD_TMP(I)
            END DO
            DEALLOCATE(DD_TMP)
            ALLOCATE(DD_TMP(IDX+ND*(NSPMD+1)))
            DO I = 1, IDX-1
              DD_TMP(I)=DD_TMP2(I)
            END DO
            DEALLOCATE(DD_TMP2)
          END IF
C
          CALL RGRTAILS(
     1      IXR      ,IPARGTMP   ,GEO      ,EADD      ,IGEO     ,
     2      ND       ,DD_TMP     ,IDX      ,INUM     ,
     3      INDEX1   ,CEP(OFF)   ,IPARTR   ,ITR1     ,
     4      IGRSURF  ,IGRSPRING  ,IRESOFF  ,TAGPRT_SMS ,NOD2EL1D,
     5      IPM , CLUSTERS,R_SKEW,IPRI)
          OFF = OFF + NUMELR
        ENDIF
C
      IF(NUMELTG/=0) THEN
C---------------------------------
C-  PRE TRI + SUPER GROUPES
C---------------------------------
       K1 = 10*NUMELTG
       K2 = 12*NUMELTG
       K3 = 13*NUMELTG+1
       K4 = 15*NUMELTG+1
       K5 = 22*NUMELTG+1
       K6 = 23*NUMELTG+1
       K7 = 24*NUMELTG+1
       K8 = 25*NUMELTG+1
       !warning: please also update any index change
       !         for MODIF option (MODIF_SPMD.F)
       IWORK  = 0
       INUM   => IWORK(1:K1)
       ITR1   => IWORK(K1+1:K2)
       EADD   => IWORK(K2+1:K3)
       INDEX1 => IWORK(K3+1:K4)
       ITRI1  => IWORK(K4+1:K5)
       XEP    => IWORK(K5+1:K6)

       ALLOCATE(XNUM(NUMELTG) ,STAT=stat)
       XNUM = ZERO

       IF(NUMELTG6>0) THEN
         CALL CDK6INX(IXTG   ,IXTG1   ,EANIT   )
       ENDIF
       IF(N2D==0)THEN
         CALL C3GRHEAD(
     1     IXTG    ,PM      ,GEO      ,INUM    ,BID13   ,
     2     ITR1    ,EADD    ,INDEX1   ,ITRI1   ,XNUM   ,
     3     IPARTG  ,ND      ,THKEC    ,IGRSURF ,IGRSH3N ,
     4     CEP(OFF),XEP      ,IXTG1   ,EANIT   ,
     5     IGEO    ,IPM     ,IPART    ,SH3TREE ,NOD2ELTG,
     6     ITRIOFF ,SH3TRIM ,TAGPRT_SMS,
     7     IWORKSH , STACK  ,DRAPE  ,RNOISE(1,MIN(SRNOISE2,NUMELC+1)),
     8     MULTI_FVM , SH3ANG,DRAPEG)
       ELSE
         CALL T3GRHEAD(
     1     IXTG    ,PM      ,GEO      ,INUM    ,BID13   ,
     2     ITR1    ,EADD    ,INDEX1   ,ITRI1   ,XNUM   ,
     3     IPARTG  ,ND      ,THKEC    ,IGRSURF ,IGRSH3N ,
     4     CEP(OFF),XEP     ,IXTG1    ,EANIT   ,
     5     IGEO    ,IPM     ,IPART    ,SH3TREE ,NOD2ELTG,
     6     ITRIOFF ,SH3TRIM ,TAGPRT_SMS,
     7     IWORKSH , STACK  ,DRAPE  ,RNOISE(1,MIN(SRNOISE2,NUMELC+1)),
     8     MULTI_FVM ,SH3ANG,DRAPEG)
       ENDIF
C---------------------------------
C-  GROUPAGE SPMD
C---------------------------------
C test non depassement de LDD_IAD
         IF(IDX+ND*(NSPMD+1)>LDD_IAD)THEN
           ALLOCATE(DD_TMP2(IDX-1))
           DO I = 1, IDX-1
             DD_TMP2(I)=DD_TMP(I)
           END DO
           DEALLOCATE(DD_TMP)
           ALLOCATE(DD_TMP(IDX+ND*(NSPMD+1)))
           DO I = 1, IDX-1
             DD_TMP(I)=DD_TMP2(I)
           END DO
           DEALLOCATE(DD_TMP2)
         END IF
C
         IF(N2D==0)THEN
           CALL C3GRTAILS(
     1          IXTG     ,PM       ,IPARGTMP ,GEO      ,
     2          EADD     ,ND       ,IPARTG   ,DD_TMP     ,
     3          IDX      ,INUM     ,INDEX1   ,CEP(OFF)   ,
     4          THKEC    ,XNUM     ,ITR1     ,IGRSURF  ,IGRSH3N    ,
     5          EANIT    ,IGEO     ,IPM      ,IXTG1    ,
     6          IPART    ,SH3TREE  ,NOD2ELTG ,ITRIOFF  ,
     7          SH3TRIM  ,TAGPRT_SMS,IWORKSH ,STACK      ,
     8          DRAPE   ,RNOISE(1,MIN(SRNOISE2,NUMELC+1)) ,
     9          MAT_ELEM%MAT_PARAM,SH3ANG,DRAPEG,IPRI )
         ELSE
           CALL T3GRTAILS(
     1          IXTG     ,PM       ,IPARGTMP ,GEO      ,
     2          EADD     ,ND       ,IPARTG   ,DD_TMP     ,
     3          IDX      ,INUM     ,INDEX1   ,CEP(OFF)   ,
     4          THKEC    ,XNUM     ,ITR1     ,IGRSURF  ,IGRSH3N    ,
     5          EANIT    ,IGEO     ,IPM      ,IXTG1    ,
     6          IPART    ,SH3TREE  ,NOD2ELTG ,ITRIOFF  ,
     7          SH3TRIM  ,TAGPRT_SMS,IWORKSH ,STACK      ,
     8          DRAPE   ,RNOISE(1,MIN(SRNOISE2,NUMELC+1)) ,USER_INIVOL,
     9          SH3ANG ,DRAPEG,IPRI)
         ENDIF
         OFF = OFF + NUMELTG
         DO I=1,NUMELTG
            N=PERMUTATION%TRIANGLE(I)
            PERMUTATION%TRIANGLE(NUMELTG+N)=I
         ENDDO

       DEALLOCATE(XNUM)
      ENDIF

      IF(TETRAMESHER_USED) THEN
         CALL APPLYSORT2FVM(T_MONVOL, IXS_TEMP, IXC, IXTG,
     .        ITAB, NOM_OPT(LNOPT1*INOM_OPT(2)+1), NB_TOTAL_NODE)
      ELSE
         CALL APPLYSORT2FVM(T_MONVOL, IXS, IXC, IXTG,
     .        ITAB, NOM_OPT(LNOPT1*INOM_OPT(2)+1), NUMNOD)
      ENDIF
C---------------------------------
      IF(NUMSPH/=0) THEN
          IWORK  = 0
          EADD   => IWORK(1:NUMSPH+1)
          CALL SPGRHEAD(KXSP    ,IXSP    ,IPARGTMP,PM     ,IPART  ,
     2                  IPARTSP ,EADD    ,CEPSP   ,ND     ,IPM    ,
     3                  IGEO    ,SPBUF  ,SPH2SOL,
     4                  SOL2SPH ,IRST    )
C---------------------------------
C-  GROUPAGE SPMD
C---------------------------------
C test non depassement de LDD_IAD
          IF(IDX+ND*(NSPMD+1)>LDD_IAD)THEN
            ALLOCATE(DD_TMP2(IDX-1))
            DO I = 1, IDX-1
              DD_TMP2(I)=DD_TMP(I)
            END DO
            DEALLOCATE(DD_TMP)
            ALLOCATE(DD_TMP(IDX+ND*(NSPMD+1)))
            DO I = 1, IDX-1
              DD_TMP(I)=DD_TMP2(I)
            END DO
            DEALLOCATE(DD_TMP2)
          END IF
C
          CALL SPGRTAILS(KXSP    ,IPARGTMP,PM      ,IPART      ,
     2                   IPARTSP ,EADD    ,ND      ,CEPSP,DD_TMP     ,
     3                   IDX     ,IXSP    ,IPM  , IGEO      ,
     4                   SPBUF   ,SPH2SOL,SOL2SPH  ,
     5                   IRST    ,NOD2SP  ,IPRI)
        ENDIF
C---------------------------------
      IF(NUMELX>0) THEN
!
          K1=6*NUMELX
          K2=K1+NUMELX
          K3=K2+NUMELX+1
          K4=K3+NUMELX*2
          K5=K4+NUMELX*4
          K6=K5+NUMELX
          !warning: please also update any index change
          !         for MODIF option (MODIF_SPMD.F)
          IWORK  = 0
          INUM   => IWORK(1:K1)
          ITR1   => IWORK(K1+1:K2)
          EADD   => IWORK(K2+1:K3)
          INDEX1 => IWORK(K3+1:K4)
          ITRI1  => IWORK(K4+1:K5)
          XEP    => IWORK(K5+1:K6)
!
          CALL XGRHEAD(
     1     KXX,      GEO,    INUM,    ITR1,
     2     EADD,     INDEX1, ITRI1,   IPARTX,
     3     ND,       IGRSURF,
     4     CEP(OFF), XEP,IPM)
C---------------------------------
C-  GROUPAGE SPMD
C---------------------------------
C test non depassement de LDD_IAD
          IF(IDX+ND*(NSPMD+1)>LDD_IAD)THEN
            ALLOCATE(DD_TMP2(IDX-1))
            DO I = 1, IDX-1
              DD_TMP2(I)=DD_TMP(I)
            END DO
            DEALLOCATE(DD_TMP)
            ALLOCATE(DD_TMP(IDX+ND*(NSPMD+1)))
            DO I = 1, IDX-1
              DD_TMP(I)=DD_TMP2(I)
            END DO
            DEALLOCATE(DD_TMP2)
          END IF
C
          CALL XGRTAILS(
     1      KXX    ,IPARGTMP   ,GEO     ,EADD     ,
     2      ND     ,DD_TMP     ,IDX     ,LB_MAX  ,INUM     ,
     3      INDEX1 ,CEP(OFF)   ,IPARTX  ,ITR1    ,IGRSURF  ,
     4      IXX    ,IGEO)
           OFF = OFF + NUMELX
       ENDIF
      CALL TRACE_OUT1()
C
C build Inverse connectivity - update after all element/sph grouping
C
        KNOD2ELS  = 0
        KNOD2ELC  = 0
        KNOD2ELTG = 0
        KNOD2EL1D  = 0
        KNOD2ELIG3D  = 0
        NOD2ELS   = 0
        NOD2ELC   = 0
        NOD2ELTG  = 0
        NOD2EL1D   = 0
        NOD2ELIG3D   = 0
        KNOD2ELQ  = 0
        NOD2ELQ  = 0
        CALL BUILD_CNEL(
     2   IXS        ,IXQ        ,IXC        ,IXT      ,IXP      ,
     3   IXR        ,IXTG       ,IXS10      ,IXS20    ,
     4   IXS16      ,IXTG1      ,IGEO       ,KNOD2ELS ,KNOD2ELC ,
     5   KNOD2ELTG  ,NOD2ELS    ,NOD2ELC    ,NOD2ELTG ,NOD2EL1D ,
     6   KNOD2EL1D  ,KXX        ,IXX        ,X        ,LELX     ,
     7   IXIG3D     ,KXIG3D     ,KNOD2ELIG3D,NOD2ELIG3D,KNOD2ELQ,
     8   NOD2ELQ    )

C---------------------------------
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NUMELIG3D>0) THEN

        K1=(NIXIG3D+1)*NUMELIG3D
        K2=K1+NUMELIG3D
        K3=K2+NUMELIG3D+1
        K4=K3+NUMELIG3D*2
        K5=K4+NUMELIG3D*4
        K6=K5+NUMELIG3D

        IWORK  = 0
        INUM   => IWORK(1:K1)
        ITR1   => IWORK(K1+1:K2)
        EADD   => IWORK(K2+1:K3)
        INDEX1 => IWORK(K3+1:K4)
        ITRI1  => IWORK(K4+1:K5)
        XEP    => IWORK(K5+1:K6)
C
        CALL IG3DGRHEAD(
     1    KXIG3D    ,GEO       ,INUM      ,ITR1      ,EADD      ,
     2    INDEX1    ,ITRI1     ,IPARTIG3D ,ND        ,IGRSURF   ,
     3    CEP(OFF)  ,XEP       ,IGEO      ,
     4    IPM       ,PM        ,NIGE      ,KNOTLOCEL)
C--------------------------------
C-  GROUPAGE SPMD
C---------------------------------
C test non depassement de LDD_IAD
        IF(IDX+ND*(NSPMD+1)>LDD_IAD)THEN
          ALLOCATE(DD_TMP2(IDX-1))
          DO I = 1, IDX-1
            DD_TMP2(I)=DD_TMP(I)
          END DO
          DEALLOCATE(DD_TMP)
          ALLOCATE(DD_TMP(IDX+ND*(NSPMD+1)))
          DO I = 1, IDX-1
            DD_TMP(I)=DD_TMP2(I)
          END DO
          DEALLOCATE(DD_TMP2)
        END IF
C
        CALL IG3DGRTAILS(
     1    KXIG3D    ,IPARGTMP  ,GEO       ,EADD      ,ND        ,
     2    DD_TMP    ,IDX       ,LB_MAX    ,INUM      ,INDEX1    ,
     3    CEP(OFF)  ,IPARTIG3D ,ITR1      ,IGRSURF   ,
     4    IXIG3D    ,IGEO      ,
     5    PM        ,NIGE      ,KNOTLOCEL)
        OFF = OFF + NUMELIG3D
      ENDIF
      DEALLOCATE(IWORK)
      CALL TRACE_OUT1()

C--------------------------------------------
C     REFERENCE METRIQUE
C--------------------------------------------
      ERR_MSG='REFERENCE METRICS'
      ERR_CATEGORY='REFERENCE METRICS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
      XYZREF = X
C
      IF(IDDLEVEL==0)THEN

        IF(NXREF > 0 .OR. NEREF > 0 .OR. IREFSTA > 0) THEN
          ALLOCATE(XREFC(4,3,NUMELC))
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='XREFC')
          ALLOCATE(XREFTG(3,3,NUMELTG))
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='XREFTG')
          ALLOCATE(XREFS(8,3,NUMELS8))
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='XREFS')
          ALLOCATE(TAGXREF(NUMNOD))
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='TAGXREF')
          ALLOCATE(TAGREFSTA(NUMNOD))
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='TAGREFSTA')
        ELSE
          ALLOCATE(XREFC(1,1,1))
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='XREFC')
          ALLOCATE(XREFTG(1,1,1))
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='XREFTG')
          ALLOCATE(XREFS(1,1,1))
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='XREFS')
          ALLOCATE(TAGXREF(1))
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='TAGXREF')
          ALLOCATE(TAGREFSTA(1))
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,C1='TAGREFSTA')
        ENDIF
      ENDIF
C
      TAGXREF = 0
      TAGREFSTA = 0
C
      IF(NXREF > 0) THEN
        WRITE(ISTDO,'(A)')' .. REFERENCE STATE (XREF)'
        CALL HM_READ_XREF(ITABM1   ,IPART    ,IPARTC   ,IPARTG   ,IPARTS    ,
     .                    UNITAB   ,IXC      ,IXTG     ,IXS      ,X         ,
     .                    XREFC    ,XREFTG   ,XREFS    ,RTRANS   ,LSUBMODEL ,
     .                    TAGXREF  ,IDDLEVEL ,EANI     ,IPM      ,IGEO )
      ENDIF
      IF(IREFSTA > 0) THEN
        WRITE(ISTDO,'(A)')' .. REFERENCE STATE (REFSTA)'
        CALL LECREFSTA(ITABM1  ,UNITAB  ,IXC     ,IXTG  ,IXS     ,
     .                 XYZREF  ,XREFC   ,XREFTG  ,XREFS ,TAGXREF ,
     .                 IDDLEVEL,TAGREFSTA )
C
        IF(IDDLEVEL==0 .AND. ((NINTER > 0).OR.(ISMS == 1))) THEN
          REWIND(IIN6)
        ELSE
          IF(IPID /= 0) CLOSE(IIN6)
          IREFSTA = 0
C          NXREF   = NXREF + 1
          NXREF   = 1
        ENDIF
      ENDIF
      IF(NEREF > 0 ) THEN
        WRITE(ISTDO,'(A)')' .. REFERENCE STATE (EREF)'
        CALL HM_READ_EREF(ITABM1  ,IPART   ,IPARTC  ,IPARTG  ,IPARTS  ,
     .                    IXC     ,IXTG    ,IXS     ,X       ,XREFC   ,
     .                    XREFTG  ,XREFS   ,LSUBMODEL,IDDLEVEL,ITAB    ,
     .                    TAGXREF ,TAGREFSTA )
C
        IF(IDDLEVEL ==1 .OR. ((NINTER == 0).AND.(ISMS == 0))) NXREF = 1
      ENDIF
C-------------------------------------------------
      !check if a law 151 is associated to any PART
      MULTI_FVM%IS_ASSOCIATED_TO_A_PART = .FALSE.
      DO NG=1,NGROUP
        MLW = IPARGTMP(1,NG)
        IF(MLW == 151)THEN
          MULTI_FVM%IS_ASSOCIATED_TO_A_PART = .TRUE.
          EXIT
        ENDIF
      ENDDO
C-------------------------------------------------
      !check if all part are using law 151
      MULTI_FVM%ARE_ALL_PARTS_151 = .TRUE.
      NB_EULER_GROUPS = 0
      DO NG=1,NGROUP
        MLW = IPARGTMP(1,NG)
        IS_EULER = IPARGTMP(11,NG)
        IF(IS_EULER == 1 ) NB_EULER_GROUPS=NB_EULER_GROUPS+1
        IF(MLW /= 151 .AND. IS_EULER == 1)THEN
          MULTI_FVM%ARE_ALL_PARTS_151 = .FALSE.
          EXIT
        ENDIF
      ENDDO
      IF(NB_EULER_GROUPS == 0)MULTI_FVM%ARE_ALL_PARTS_151 = .FALSE.
C-------------------------------------------------
      !copy IPARG <- IPARGTMP, and Deallocate IPARGTMP
      SIPARG = NPARG*NGROUP
      IF(ALLOCATED(IPARG))DEALLOCATE(IPARG)
      ALLOCATE(IPARG(SIPARG)    ,STAT=stat)
      DO J=1,NPARG
       DO I=1,NGROUP
        IPARG((I-1)*NPARG + J) = IPARGTMP(J,I)
       ENDDO
      ENDDO
      DEALLOCATE(IPARGTMP)
C-------------------------------------------------
C provisoire
      IF(ISMS/=0)THEN
        DO N=1,NGROUP
          IPARG(NPARG*(N-1)+52)=1
        END DO
      END IF
      NBR_GPMP = NGROUP


c-----------------------------------------------------------------------
c     set default material/property parameters by element group
c-----------------------------------------------------------------------
      IF(ALLOCATED(GROUP_PARAM_TAB)) DEALLOCATE(GROUP_PARAM_TAB)
      ALLOCATE(GROUP_PARAM_TAB(NGROUP)   ,STAT=stat)
c
      CALL SET_ELGROUP_PARAM(GROUP_PARAM_TAB  ,IPARG    ,NGROUP   ,N2D       ,
     .                       IPM     ,IGEO    ,PM       ,GEO      ,BUFMAT    )
C-------------------------------------------------
      CALL TRACE_OUT1()
C--------------------------------------------
C     Itet=2 of S10 : dynamic condensation
C--------------------------------------------
      IF(NUMELS10>0) THEN
       IF(ALLOCATED(ITAGND)) DEALLOCATE(ITAGND)
       ALLOCATE(ITAGND(NUMNOD),STAT=stat)
       ITAGND(1:NUMNOD)=0
       CALL DIM_S10EDG(NS10E, IXS10 ,IPARG,ITAGND)
       IF(NS10E>0) THEN
        IF(ALLOCATED(ICNDS10)) DEALLOCATE(ICNDS10)
        ALLOCATE(ICNDS10(3*NS10E),STAT=stat)
        ICNDS10(1:3*NS10E)=0
        ITAGND(1:NUMNOD)=0
        CALL IND_S10EDG(ICNDS10, IXS, IXS10 ,IPARG,ITAGND)
        IF(IPARI0/=0) CALL REORD_ICND(ICNDS10, ITAGND)
       END IF
      ELSE
       IF(ALLOCATED(ITAGND)) DEALLOCATE(ITAGND)
       ALLOCATE(ITAGND(0),STAT=stat)
      END IF

C--------------------------------------------
C     DOMAIN DECOMPOSITION 1 (reconstruction des tableaux)
C--------------------------------------------
C si NSPMD = 1 il faut qd meme construire dd_iad et fr_iad
      ERR_MSG='DOMAIN DECOMPOSITION ARRAYS'
      ERR_CATEGORY='DOMAIN DECOMPOSITION'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SDD_IAD = (NSPMD+1)*NSPGROUP
      ALLOCATE(DD_IAD(SDD_IAD)    ,STAT=stat)
      DD_IAD = 0
      CALL DOMDEC1(
     1       IPARG  ,IXS    ,IXQ       ,IXC     ,IXT     ,
     2       IXP    ,IXR    ,IXTG    ,DD_IAD  ,
     3       X      ,DD_TMP    ,IXS10   ,IXS20   ,
     4       IXS16  ,KXX    ,IXX       ,KXSP    ,IXSP    ,
     5       CEPSP  ,IXTG1)
C
      DEALLOCATE(DD_TMP)
      CALL TRACE_OUT1()
C--------------------------------------------
C     Multidomains -> modif domdec
C--------------------------------------------
      ERR_MSG='MULTIDOMAINS'
      ERR_CATEGORY='MULTIDOMAINS'
      IF((NSUBDOM>0).AND.(IDDOM==0).AND.(FLG_R2R_ERR==0)) THEN
        WRITE(ISTDO,'(A)')' .. MULTIDOMAINS DOMDEC SYNCHRONIZATION '
        CALL R2R_DOMDEC(IEXLNK,IGRNOD,FRONTB_R2R,DT_R2R,0)
      ENDIF
C--------------------------------------------
C     STOCKAGE DYNAMIQUE (RESOLUTION) REEL
C--------------------------------------------
      ERR_MSG='ELEMENT BUFFER ALLOCATION'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SELBUF = LBUFEL
      ALLOCATE(ELBUF(SELBUF)    ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='ELBUF')
      ELBUF = ZERO
      CALL TRACE_OUT1()
C--------------------------------------------
C     GRAVITY
C--------------------------------------------
      ERR_MSG='GRAVITY'
      ERR_CATEGORY='GRAVITY'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c      CALL PRELECGRAV(NUMGRAV ,IGRNOD)
      CALL HM_PREREAD_GRAV(NUMGRAV ,IGRNOD , LSUBMODEL)
      SIGRV = NIGRV*NGRAV
      SLGRAV = NUMGRAV
      IF(IDDLEVEL==0)THEN
        ALLOCATE(IGRV(SIGRV)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IGRV')
        ALLOCATE(LGRAV(SLGRAV)  ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='LGRAV')
      END IF
      IGRV = 0
      CALL HM_READ_GRAV(IGRV   ,LGRAV   ,GRAV      ,ITAB    ,ITABM1  ,
     .                 IGRNOD  ,NPC     ,SENSORS   ,UNITAB  ,ISKWN   ,
     .                 ITAGND  ,LSUBMODEL)
      CALL TRACE_OUT1()
C----------------------------------
C     LECTURE DES CARTES INIGRAV
C----------------------------------
      ERR_MSG='INIGRAV'
      ERR_CATEGORY='GRAVITY'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NINIGRAV > 0) SINIGRAV = NINIGRAV
      IF(IDDLEVEL == 0) THEN
        ALLOCATE(INIGRV(04,SINIGRAV)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='INIGRV')
        ALLOCATE(LINIGRAV(11,SINIGRAV)  ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='LINIGRAV')
      END IF
      IF(NINIGRAV > 0) THEN
        IF(IDDLEVEL == 0) THEN
          INIGRV = 0
          WRITE(ISTDO,'(A)') ' .. INITIAL GRAVITY LOADING'
          CALL HM_READ_INIGRAV(IGRV     ,LGRAV    ,GRAV     ,ITAB     ,ITABM1   ,
     .                         IGRPART  ,NPC      ,UNITAB   ,ISKWN    ,
     .                         ITAGND   ,IGRSURF  ,TF       ,BUFSF    ,LSUBMODEL)
        ENDIF
      ENDIF
      CALL TRACE_OUT1()
C----------------------------------
C     LECTURE DES CARTES INIMAP1D
C----------------------------------
      ERR_MSG = 'INIMAP1D'
      ERR_CATEGORY= 'INITIALIZATION'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))

      IF(IDDLEVEL==0 .AND. NINIMAP1D+NINIMAP2D>0)WRITE(ISTDO,'(A)')TITRE(54)

      IF(IDDLEVEL==0)THEN
        ALLOCATE(INIMAP1D(NINIMAP1D))
        IF(NINIMAP1D > 0) THEN
          CALL HM_READ_INIMAP1D(INIMAP1D ,NPC     , ITABM1, X, IGRBRIC,
     .                      IGRQUAD  ,IGRSH3N, MULTI_FVM, UNITAB, LSUBMODEL)
           IF(.NOT. MULTI_FVM%IS_USED) THEN
              DO KK = 1, NINIMAP1D
                 ALLOCATE(INIMAP1D(KK)%TAGNODE(NUMNOD))
                 INIMAP1D(KK)%TAGNODE(1:NUMNOD) = 0
              ENDDO
           ENDIF
        ENDIF
      ENDIF
      CALL TRACE_OUT1()
C----------------------------------
C     LECTURE DES CARTES INIMAP2D
C----------------------------------
      ERR_MSG = 'INIMAP2D'
      ERR_CATEGORY= 'INITIALIZATION'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(IDDLEVEL==0)THEN
        ALLOCATE(INIMAP2D(NINIMAP2D))
        IF(NINIMAP2D > 0) THEN
          CALL HM_READ_INIMAP2D(INIMAP2D, FUNC2D, ITABM1, X, IGRBRIC,
     .                      IGRQUAD , IGRSH3N, UNITAB, LSUBMODEL)
           IF(.NOT. MULTI_FVM%IS_USED) THEN
              DO KK = 1, NINIMAP2D
                 ALLOCATE(INIMAP2D(KK)%TAGNODE(NUMNOD))
                 INIMAP2D(KK)%TAGNODE(1:NUMNOD) = 0
              ENDDO
           ENDIF
        ENDIF
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     "LOAD FIELDS" : CENTRIFUGAL,FLUID,BLAST
C--------------------------------------------
      !ALLOCATIONS
      ERR_MSG='LOAD FIELDS'
      ERR_CATEGORY='LOAD FIELDS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(IDDLEVEL == 0)THEN
C
C        Centrifugal Loads
         ALLOCATE(ICFIELD(SICFIELD)    ,STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='ICFIELD')
         ALLOCATE(LCFIELD(SLCFIELD)    ,STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='LCFIELD')
         ALLOCATE(CFIELD(SCFIELD)      ,STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='CFIELD')
!INITIALIZATIONS
         IF(ALLOCATED(ICFIELD)) ICFIELD(:) = 0
         IF(ALLOCATED(LCFIELD)) LCFIELD(:) = 0
         IF(ALLOCATED(CFIELD )) CFIELD(:)  = ZERO
!READING CARDS & STORING DATA
         IF(NLOADC/=0)THEN
           CALL HM_READ_LOAD_CENTRI(ICFIELD ,LCFIELD ,CFIELD    ,ITAB    ,ITABM1  ,
     .                              IGRNOD  ,NPC     ,SENSORS   ,UNITAB  ,IFRAME  ,
     .                              LSUBMODEL)
         END IF
C
C        PFLUID & PBLAST & LOAD PRESSURE
         ALLOCATE(ILOADP(SILOADP)    ,STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='ILOADP')
         ALLOCATE(LLOADP(SLLOADP)    ,STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='LLOADP')
         ALLOCATE(LOADP(SLOADP)      ,STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='LOADP')
         ALLOCATE(INTERLOADP(NINTLOADP)    ,STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='INTERLOADP')
         ALLOCATE(INTGAPLOADP(NINTLOADP)    ,STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='INTGAPLOADP')

         S_LOADPINTER = 0
         IF(NINTLOADP > 0) THEN
            S_LOADPINTER = NINTER*NLOADP_HYD            
            ALLOCATE(KLOADPINTER(NINTER + 1)    ,STAT=stat)
            IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='KLOADPINTER')
            ALLOCATE(LOADPINTER(S_LOADPINTER)    ,STAT=stat)
            IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='LOADPINTER')
            ALLOCATE(DGAPINT(NINTER)    ,STAT=stat)
            IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='DGAPINT')
            ALLOCATE(DGAPLOADINT(S_LOADPINTER)    ,STAT=stat)
            IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='DGAPLOADINT')
         ELSE
            ALLOCATE(KLOADPINTER(0))
            ALLOCATE(LOADPINTER(0))
            ALLOCATE(DGAPINT(0))
            ALLOCATE(DGAPLOADINT(0))
         ENDIF

!INITIALIZATIONS
         IF(ALLOCATED(ILOADP )) ILOADP(:)  = 0
         IF(ALLOCATED(LLOADP )) LLOADP(:)  = 0
         IF(ALLOCATED(LOADP  )) LOADP(:)   = ZERO
         IF(ALLOCATED(INTERLOADP )) INTERLOADP(:)  = 0
         IF(ALLOCATED(KLOADPINTER )) KLOADPINTER(:)  = 0
         IF(ALLOCATED(LOADPINTER ))  LOADPINTER(:)  = 0
         IF(ALLOCATED( INTGAPLOADP ))  INTGAPLOADP(:)=ZERO
         IF(ALLOCATED( DGAPINT ))  DGAPINT(:)=ZERO
         IF(ALLOCATED( DGAPLOADINT )) DGAPLOADINT(:)=ZERO
!READING CARDS & STORING DATA
         NUMLOADP=0
         NINTLOADP = 0
         NINTLOADP21 = 0
         IF(NLOADP_F/=0)THEN
           CALL HM_READ_PFLUID(NUMLOADP ,ILOADP  ,LLOADP  ,LOADP   ,NPC      ,
     .                        SENSORS   ,IGRSURF ,UNITAB  ,IFRAME  ,LSUBMODEL)
         END IF
         IF(NLOADP_B/=0)THEN
           CALL HM_READ_PBLAST(ITAB    ,ITABM1  ,UNITAB  ,IGRSURF,  NUMLOADP,
     .                        ILOADP   ,LLOADP  ,LOADP   ,X      , BUFSF    ,
     .                        LSUBMODEL,RTRANS)
         ENDIF
         IF(NLOADP_HYD/=0)THEN
           CALL HM_READ_LOAD_PRESSURE(
     .                       NUMLOADP   ,ILOADP     ,LLOADP       ,INTERLOADP ,LOADP      ,
     .                       KLOADPINTER,LOADPINTER ,NPC          ,SENSORS    ,IGRSURF    ,
     .                       UNITAB     ,ISKWN      ,LSUBMODEL    ,DGAPINT    ,INTGAPLOADP,
     .                       DGAPLOADINT,S_LOADPINTER)

         END IF

         DEALLOCATE( INTERLOADP,INTGAPLOADP )
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES RBE2 Constraints
C--------------------------------------------
      ERR_MSG='RBE2'
      ERR_CATEGORY='RBE2'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL HM_PREREAD_RBE2(SIRBE2,SLRBE2,IGRNOD,LSUBMODEL)
       IF(IDDLEVEL==0)THEN
        ALLOCATE(IRBE2(SIRBE2)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IRBE2')
       END IF
       IF(IDDLEVEL==0)THEN
        ALLOCATE(LRBE2(SLRBE2)  ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='LRBE2')
       END IF
       IF(SIRBE2 > 0) THEN
        IRBE2 = 0
        LRBE2 = 0
        CALL HM_READ_RBE2(
     .        IRBE2   ,LRBE2 ,ITAB      ,ITABM1    ,IGRNOD,
     .        ISKWN   ,D     ,IDDLEVEL  ,NOM_OPT(LNOPT1*INOM_OPT(13)+1),ITAGND,
     .        ICNDS10 ,LSUBMODEL)
       ENDIF


      CALL C_NEW_HASH(GRNOD_UID,NGRNOD)
      DO I=1,NGRNOD
         CALL C_HASH_INSERT(GRNOD_UID,IGRNOD(I)%ID,I)
      ENDDO

C--------------------------------------------
C     LECTURE DES RBE3 Interpolation Constraints
C--------------------------------------------
      CALL TRACE_OUT1()
      ERR_MSG='RBE3'
      ERR_CATEGORY='RBE3'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL HM_PREREAD_RBE3(SIRBE3,SLRBE3,IGRNOD,GRNOD_UID,LSUBMODEL)
       LXINTD = 0
       SLRBE3 = 2*SLRBE3
       SFRBE3 = (3+1)*SLRBE3
       IF(IDDLEVEL==0)THEN
        ALLOCATE(IRBE3(SIRBE3)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IRBE3')
        ALLOCATE(LRBE3(SLRBE3), FRBE3(SFRBE3)   ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='LRBE3')
       END IF
       IF(SIRBE3 > 0) THEN
        IRBE3 = 0
        LRBE3 = 0
        FRBE3 = ZERO
          CALL HM_READ_RBE3(IRBE3  ,LRBE3  ,FRBE3  ,ITAB   ,ITABM1  ,
     .                      IGRNOD ,ISKWN  ,LXINTD ,D    ,IDDLEVEL,
     .                      NOM_OPT(LNOPT1*INOM_OPT(14)+1),ITAGND ,
     .                      GRNOD_UID,UNITAB,LSUBMODEL)
       ENDIF

      CALL C_DELETE_HASH(GRNOD_UID)

      CALL TRACE_OUT1()

C---------------------------------------------
C    CHECK ENGINE FILE : DYNAIN FILE
C-------------------------------------------
      DYNAIN_DATA%DYNAIN_CHECK = 0
      CALL CHECK_DYNAIN(IPART,IPARTC,IPARTG,IXC,IXTG,DYNAIN_DATA%DYNAIN_CHECK)

C---------------------------------------------
C    CHECK ENGINE FILE /H3D/?/TMAX initialization
C-------------------------------------------
      CALL INI_H3DTMAX_ENGINE(IPARG,IPART,IPARTS,IPARTC,IPARTG,IDDLEVEL)
C---------------------------------------------
C    CHECK ENGINE FILE DYNAIN or STATE if to use F.I. total strain for QEPH
C-------------------------------------------
      ISTR_24 = 0
      IF(NUMELC/=0) CALL CHECK_QEPH_STRA(ISTR_24)
C--------------------------------------------
C     LECTURE DES ELEMENTS DESACTIVABLES
C--------------------------------------------
      ERR_MSG='ELEMENTS DEACTIVATION'
      ERR_CATEGORY='ELEMENTS DEACTIVATION'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SIACTIV = LACTIV*NACTIV
      IF(IDDLEVEL==0)THEN
        ALLOCATE(IACTIV(SIACTIV), FACTIV(LRACTIV*NACTIV) ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR, C1='IACTIV')
      END IF
      IACTIV = 0
      IF(NACTIV > 0) CALL HM_READ_ACTIV(IACTIV   ,FACTIV  ,SENSORS,IGRBRIC,
     .                             IGRQUAD  ,IGRSH4N ,IGRSH3N ,IGRTRUSS,IGRBEAM,
     .                             IGRSPRING,LSUBMODEL,UNITAB)
C--------------------------------------------
      SIBMPC = NUMMPC + LMPC*3
      IF(IDDLEVEL==0)THEN
        ALLOCATE(IBMPC(SIBMPC)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IBMPC')
      END IF
      IBMPC  = 0
      IF(LMPC>0) THEN
      IBMPC2 => IBMPC(NUMMPC+1:NUMMPC+LMPC)
      IBMPC3 => IBMPC(NUMMPC+LMPC+1:NUMMPC+LMPC*2)
      IBMPC4 => IBMPC(NUMMPC+LMPC*2+1:SIBMPC)
      ELSE
        IBMPC2 => IBMPC
        IBMPC3 => IBMPC
        IBMPC4 => IBMPC
      END IF

      SKINET = NUMNOD
      IF(IDDLEVEL==0)THEN
        ALLOCATE(KINET(SKINET)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='KINET')
      END IF
      KINET  = 0

      SIPARI = NPARI*NINTER
      IF(IDDLEVEL==0)THEN
        ALLOCATE(IPARI(SIPARI)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='IPARI')
      END IF
      IPARI = 0
      CALL TRACE_OUT1()
C--------------------------------------------
C     External faces of solid elements
C--------------------------------------------
      ERR_MSG='SOLID ELEMENTS FACES'
      ERR_CATEGORY='SOLID ELEMENTS FACES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      ALLOCATE(FASTAG(NUMELS)  ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='FASTAG')
      CALL ANI_FASOLFR1(IXS,IXC,IXTG,FASTAG,ISOLNOD)
      SFASOLFR = 2*NFASOLFR
      IF(IDDLEVEL==0)THEN
        ALLOCATE(FASOLFR(SFASOLFR),STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='FASOLFR')
      END IF
      CALL ANI_FASOLFR2(FASTAG,FASOLFR,ISOLNOD)
      DEALLOCATE(FASTAG)
      CALL TRACE_OUT1()
C--------------------------------------------
C     External Segs of quad elements
C--------------------------------------------
      ERR_MSG='QUAD ELEMENTS SEGS'
      ERR_CATEGORY='QUAD ELEMENTS SEGS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      ALLOCATE(SEGTAG(4*NUMELQ)  ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='SEGTAG')
       SEGTAG (1:4*NUMELQ) = 0
      CALL ANI_SEGQUADFR1(IXQ ,SEGTAG ,KNOD2ELQ ,NOD2ELQ ,X ,NSEGQUADFR)
      SSEGQUADFR = 2*NSEGQUADFR
      IF(IDDLEVEL==0)THEN
        ALLOCATE(SEGQUADFR(SSEGQUADFR),STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='SEGTAG')
      END IF
      CALL ANI_SEGQUADFR2(SEGTAG,SEGQUADFR)
      DEALLOCATE(SEGTAG)
      CALL TRACE_OUT1()
C--------------------------------------------
C     MULTI-POINT CONSTRAINTS (2)
C--------------------------------------------
      ERR_MSG='MPCS 2'
      ERR_CATEGORY='MPCS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NUMMPC > 0) CALL HM_READ_MPC (
     1           RBMPC   ,IBMPC  ,IBMPC2  ,IBMPC3  ,IBMPC4  ,
     2           ISKWN   ,ITAB   ,ITABM1  ,LAG_NCF ,LAG_NKF ,
     3           LAG_NHF ,D      ,IKINE1LAG,
     4           NOM_OPT(LNOPT1*INOM_OPT(17)+1),ITAGND,
     5           LSUBMODEL,UNITAB)
      CALL TRACE_OUT1()
C--------------------------------------------
C     OPTIMIZATION (Part 3 & 4)
C--------------------------------------------
       IF(ALLOCATED(TAGPRT_FRIC)) DEALLOCATE(TAGPRT_FRIC)
       ALLOCATE(TAGPRT_FRIC(NPART),STAT=stat)
       TAGPRT_FRIC(1:NPART) = 0

C--------------------------------------------!
C    FRICTION MODEL :  BUFFER STRUCTURE ALLOCATION, Lectur of option
C--------------------------------------------!
        IORTHFRICMAX = 0
        NPFRICORTH = 0
        IF(NINTERFRIC > 0) THEN

           IF(IDDLEVEL == 0) ALLOCATE(INTBUF_FRIC_TAB(NINTERFRIC), STAT=Stat)

C  1st step : counting number of set of parts in friction models
            ALLOCATE(TABCOUPLEPARTS_FRIC_TMP(1),STAT=stat)
            ALLOCATE(TABCOEF_FRIC_TMP(1),STAT=stat)
            ALLOCATE(IFRICORTH_TMP(1),STAT=stat)
            ALLOCATE(LENGRPF(NPART),STAT=stat)
            LENGRPF(1:NPART) = 0

            LENG = 0
            DO N=1,NGRPART
               LENG = MAX (LENG,IGRPART(N)%NENTITY)
            ENDDO

            FLAGF = 0
            NSETFRICTOT = 0
            COEFSLEN = 0
            NGRPF = 0
            NSETMAX = 0

            CALL HM_READ_FRICTION_MODELS(
     1        NOM_OPT(LNOPT1*INOM_OPT(29)+1),UNITAB,IGRPART    ,IPART  ,TAGPRT_FRIC,
     2        TABCOUPLEPARTS_FRIC_TMP ,TABCOEF_FRIC_TMP ,INTBUF_FRIC_TAB,NSETFRICTOT ,
     3        FLAGF    ,COEFSLEN      , IORTHFRICMAX    ,IFRICORTH_TMP   ,NGRPF      ,
     4        LENGRPF  ,LENG          , NSETMAX         ,LSUBMODEL )

            DEALLOCATE(TABCOUPLEPARTS_FRIC_TMP)
            DEALLOCATE(TABCOEF_FRIC_TMP)
            DEALLOCATE(IFRICORTH_TMP)

C  2nd step : storing parts ids and coefficients in temperarly tabs  :
            ALLOCATE(TABCOUPLEPARTS_FRIC_TMP(2*NINTERFRIC*NSETMAX),STAT=stat)
            COEFSLEN = NINTERFRIC*(2*NSETMAX+1)
            ALLOCATE(TABCOEF_FRIC_TMP(8*COEFSLEN),STAT=stat)
            TABCOUPLEPARTS_FRIC_TMP(1:2*NINTERFRIC*NSETMAX) = 0
            TABCOEF_FRIC_TMP(1:8*COEFSLEN) = ZERO

            ALLOCATE(IFRICORTH_TMP(NINTERFRIC*NSETMAX),STAT=stat)
            IFRICORTH_TMP(1:NINTERFRIC*NSETMAX) = 0

            FLAGF = 1
            NSETFRICTOT = 0
            COEFSLEN = 0
            NSETMAX = 0

            CALL HM_READ_FRICTION_MODELS(
     1        NOM_OPT(LNOPT1*INOM_OPT(29)+1),UNITAB,IGRPART    ,IPART  ,TAGPRT_FRIC,
     2        TABCOUPLEPARTS_FRIC_TMP ,TABCOEF_FRIC_TMP ,INTBUF_FRIC_TAB,NSETFRICTOT ,
     3        FLAGF    ,COEFSLEN    , IORTHFRICMAX    ,IFRICORTH_TMP   ,NGRPF        ,
     4        LENGRPF  ,LENG        , NSETMAX         , LSUBMODEL )

C  3rd step : Tri of tabs

            COEFSLEN = NINTERFRIC*(2*NSETMAX+1)
            ALLOCATE(NSETINIT(NINTERFRIC),STAT=stat)
            ALLOCATE(TABPARTS_FRIC_TMP(2*NINTERFRIC*NSETMAX),STAT=stat)
            NSETINIT(1:NINTERFRIC) = 0
            TABPARTS_FRIC_TMP (1:2*NINTERFRIC*NSETMAX) = 0

            CALL TRIINTFRIC(
     .          TABCOUPLEPARTS_FRIC_TMP  ,TABCOEF_FRIC_TMP  ,INTBUF_FRIC_TAB   ,
     .          TABPARTS_FRIC_TMP,NSETFRICTOT,NSETINIT,IORTHFRICMAX,IFRICORTH_TMP,
     .          NSETMAX        )

C   4th step :  ALLOCATION OF NEW BUFFER FOR INTERFACE FRICTION

             IF(IDDLEVEL == 0) CALL INTBUF_FRIC_INI_STARTER(INTBUF_FRIC_TAB )

C    4th step :    Final storing of structures in buffer
             CALL INTBUF_FRIC_COPY(
     .          TABCOUPLEPARTS_FRIC_TMP  ,TABCOEF_FRIC_TMP,TABPARTS_FRIC_TMP   ,
     .           NSETINIT                ,IFRICORTH_TMP   , INTBUF_FRIC_TAB    )

             DEALLOCATE(TABCOUPLEPARTS_FRIC_TMP)
             DEALLOCATE(TABCOEF_FRIC_TMP)
             DEALLOCATE(TABPARTS_FRIC_TMP )

             DEALLOCATE( NSETINIT )
             DEALLOCATE(IFRICORTH_TMP)
             DEALLOCATE(LENGRPF )

C------/FRICTION/ORIENTATION READING FOR ORTHOTROPIC FRICTION

             IF(IORTHFRICMAX > 0) THEN

                FLAGF = 0
                NPFRICORTH = 0
c               KFRICORIENT = 0
                IF(.NOT.ALLOCATED(PFRICORTH))ALLOCATE(PFRICORTH(NPART),STAT=stat)
                IF(.NOT.ALLOCATED(IREPFORTH))ALLOCATE(IREPFORTH(1),STAT=stat)
                IF(.NOT.ALLOCATED(VFORTH))ALLOCATE(VFORTH(1),STAT=stat)
                IF(.NOT.ALLOCATED(PHIFORTH))ALLOCATE(PHIFORTH(1),STAT=stat)

                PFRICORTH(1:NPART) = 0

                CALL HM_READ_FRICTION_ORIENTATIONS (INTBUF_FRIC_TAB ,
     1                 NPFRICORTH ,IGRPART     ,IPART       ,PFRICORTH  ,
     2                 IREPFORTH  ,ISKWN       ,PHIFORTH    ,VFORTH    ,SKEW    ,
     3                 FLAGF      ,TAGPRT_FRIC ,RTRANS      ,LSUBMODEL ,UNITAB  )

                DEALLOCATE(IREPFORTH,VFORTH,PHIFORTH)

                ALLOCATE(IREPFORTH(NPFRICORTH),STAT=stat)
                ALLOCATE(VFORTH(3*NPFRICORTH),STAT=stat)
                ALLOCATE(PHIFORTH(NPFRICORTH),STAT=stat)

                IREPFORTH(1:NPFRICORTH) = 0
                VFORTH(1:3*NPFRICORTH) = ZERO
                PHIFORTH(1:NPFRICORTH) = ZERO

                FLAGF = 1
                CALL HM_READ_FRICTION_ORIENTATIONS (INTBUF_FRIC_TAB ,
     1                 NPFRICORTH ,IGRPART     ,IPART      ,PFRICORTH  ,
     2                 IREPFORTH  ,ISKWN       ,PHIFORTH   ,VFORTH     ,SKEW    ,
     3                 FLAGF      ,TAGPRT_FRIC ,RTRANS     ,LSUBMODEL  ,UNITAB  )
              ENDIF

         ELSEIF(IDDLEVEL == 0) THEN !NINTERFRIC = 0
              ALLOCATE(INTBUF_FRIC_TAB(0))
         ENDIF
         IF(.NOT.ALLOCATED(PFRICORTH))ALLOCATE(PFRICORTH(0))
         IF(.NOT.ALLOCATED(IREPFORTH))ALLOCATE(IREPFORTH(1))
         IF(.NOT.ALLOCATED(VFORTH))   ALLOCATE(VFORTH(1))
         IF(.NOT.ALLOCATED(PHIFORTH)) ALLOCATE(PHIFORTH(1))


C--------------------------------------------
C     ALE CONNECTIVITY
C--------------------------------------------
      CALL ALE_CONNECTIVITY%ALE_CONNECTIVITY_INIT()
      IF(ALE_CONNECTIVITY%has_ne_connect) THEN
         CALL ALE_CONNECTIVITY%ALE_COMPUTE_CONNECTIVITY(NUMNOD, NUMELQ, NUMELTG, NUMELS,
     .        NIXQ, NIXTG, NIXS, ITAB,
     .        IXQ, IXTG, IXS)
      ENDIF
      CALL ALE_CONNECTIVITY%ALE_COMPUTE_EE_CONNECTIVITY(PM,IGEO,
     .     NPROPGI,NUMGEO, NPROPM, NUMMAT , NUMNOD, NUMELQ, NUMELTG, NUMELS, N2D,
     .     IALE  , IEULER, ITHERM, IALELAG,
     .     NIXQ  , NIXTG , NIXS  , ITAB   ,
     .     IXQ   , IXTG  , IXS   )
C
      IF(NSUBDOM > 0) THEN
C---------------Deactivation of ALE flags if no more ALE elements in domain -------------C
        IF(ALE_EULER == 0) THEN
          IALE = 0
          IEULER = 0
        ENDIF
      ENDIF
C
C--------------------------------------------
C     LECTURE DES INTERFACES
C--------------------------------------------
      ERR_MSG='INTERFACES'
      ERR_CATEGORY='INTERFACES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      NINTSTAMP=0
      NMNT   = 0
      INTERFACES%PARAMETERS%ISTIF_DT = 0
C PROBINT egalement initialisee dans ENGINE (rdresa)
      PROBINT=HALF
      INTERFACES%PARAMETERS%INT25_EROSION_SOLID = 0

      IF(NINTER == 0.AND.NINTERFRIC > 0 )THEN
            CALL ANCMSG(MSGID=1593,
     .                  MSGTYPE=MSGWARNING,
     .                  ANMODE=ANINFO_BLIND_1)
      ENDIF

      IF(NINTER > 0)THEN
       IF(IDDLEVEL == 0) THEN
         ALLOCATE(XFILTR(NINTER)       ,STAT=stat)
         ALLOCATE(STFAC(NINTER)        ,STAT=stat)
         ALLOCATE(FRIC_P(10*NINTER)    ,STAT=stat)
         ALLOCATE(I2RUPT(6*NINTER)     ,STAT=stat)
         ALLOCATE(AREASL(NINTER)       ,STAT=stat)
         ALLOCATE(FRIGAP(NPARIR*NINTER),STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                              MSGTYPE=MSGERROR,
     .                              C1='XFILTR')
       END IF
       XFILTR = ZERO
       STFAC  = ZERO
       FRIC_P = ZERO
       FRIGAP = ZERO
       I2RUPT = ZERO
       AREASL = ZERO
C
       IF(IDDLEVEL == 0) CALL STARTIME(10,1)
       IF(IDDLEVEL == 1) CALL STARTIME(11,1)

       WRITE(ISTDO,'(A)')TITRE(38)
       IF(NINTSUB/=0)THEN
          CALL HM_READ_INTSUB(IGRNOD ,IGRSURF,NOM_OPT(LNOPT1*INOM_OPT(3)+1),IGRSLIN,LSUBMODEL)
       END IF
       IDS = 117
       I = 0
c      CALL ANCNTS(IDS, I)
C
       NSN_MULTI_CONNEC = 0
       ALLOCATE(T2_NB_CONNEC(NUMNOD))
       T2_NB_CONNEC(1:NUMNOD) = 0
C------------------------------------------------------------
C     INTERFACE READING
C--------------------------------------------------------------
       NINTER25 = 0
       CALL HM_READ_INTERFACES(
     1           IPARI    ,FRIGAP   ,NINTER   ,ITAB       ,ITABM1                      ,
     2           IGRNOD   ,IGRSURF  ,IGRSLIN  ,IGRBRIC    ,IGRSH3N                     ,
     3           IGRTRUSS ,NPC      ,ISKWN    ,XFILTR     ,STFAC                       ,
     4           FRIC_P   ,I2RUPT   ,AREASL   ,UNITAB     ,NOM_OPT(LNOPT1*INOM_OPT(3)+1),
     5           DEF_INTER,NPC1     ,SENSORS  ,MULTI_FVM ,NOM_OPT(LNOPT1*INOM_OPT(29)+1),
     6           INTBUF_FRIC_TAB    ,IDDLEVEL ,LSUBMODEL  ,TF                          ,
     7           KLOADPINTER        ,DGAPINT  ,INTERFACES)
C

       IF(.NOT. ALLOCATED(ALE_CONNECTIVITY%NALE)) ALLOCATE(ALE_CONNECTIVITY%NALE(0))

c       IF(NINTER25 /= 0)THEN
c        DO N=1,NGROUP
c          IF(IPARG(NPARG*(N-1)+43) >0.AND.IPARG(NPARG*(N-1)+5)==1)THEN
c             INTERFACES%PARAMETERS%INT25_EROSION_SOLID = 1
c          ENDIF
c        END DO
c       END IF
c       INTERFACES%PARAMETERS%INT25_EROSION_SOLID = 1


       CALL LECINT (IPARI       ,NINTER    ,IPM       ,BUFMAT   ,                                                 
     .              NMNT        ,ITAB      ,ITABM1    ,GEO      ,                                                 
     .              PM          ,X         ,IGRNOD    ,IGRSURF  ,IGRSLIN  ,                                       
     .              NPC         ,PROBINT   ,LAG_NCF   ,                                                           
     .              LAG_NKF     ,LAG_NCL   ,LAG_NKL   ,LAG_NHF  ,MAXRTM   ,                                       
     .              ISKWN       ,MAXRTMS   ,IGEO      ,                                                           
     .              XFILTR      ,STFAC     ,FRIC_P    ,FRIGAP   ,                                                 
     .              I2RUPT      ,AREASL    ,UNITAB    ,IXS      ,NOM_OPT(LNOPT1*INOM_OPT(3)+1),                   
     .              ITAG        ,IXC       ,IXTG      ,KNOD2ELC ,KNOD2ELTG,                                       
     .              NOD2ELC     ,NOD2ELTG  ,KNOD2ELS  ,NOD2ELS  ,IXS10   ,                                        
     .              IXS16       ,IXS20     ,DEF_INTER ,MAXNSNE  ,                                                 
     .              NPC1        ,MULTI_FVM ,NOM_OPT(LNOPT1*INOM_OPT(29)+1),INTBUF_FRIC_TAB,            
     .              IGRBRIC     ,IGRSH3N   ,IGRTRUSS  ,MAXRTM_T2,NSN_MULTI_CONNEC,           
     .              T2_NB_CONNEC,IDDLEVEL ,ALE_CONNECTIVITY%NALE,INTERFACES)

c       IF(INTERFACES%PARAMETERS%INT25_EROSION_SOLID == 2) THEN
c         INTERFACES%PARAMETERS%INT25_EROSION_SOLID = 1
c       ELSE
c         INTERFACES%PARAMETERS%INT25_EROSION_SOLID = 0
c       ENDIF
C
       !need to allocate only once at first passage in lectur
       FLAG_ALLOCATE = 1
       !PROC argument is used only for call in ddsplit
       PROC_BID = 0

       IF(IDDLEVEL == 0) THEN
         !--------------------------------------------!
         !   NEW INTERFACE BUFFER STRUCTURE ALLOCATION
         !--------------------------------------------!
         ALLOCATE(INTBUF_TAB(NINTER), STAT=Stat)
         !--------------------------------------------!

         !--------------------------------------------!
         !NEW INTERFACE BUFFER STRUCTURE INITIALIZATION
         !--------------------------------------------!
         CALL INTBUF_INI_STARTER(INTBUF_TAB, IPARI, NUMNOD,
     .                           I11FLAG, FLAG_ALLOCATE, PROC_BID ,INTBUF_FRIC_TAB)
         !--------------------------------------------!

         CALL INT8_INI(INTBUF_TAB,IPARI,NBT8)
         ALLOCATE(INTERT8(NSPMD,NBT8))
         DO P = 1,NSPMD
           DO I = 1,NBT8
              ALLOCATE(INTERT8(P,I)%BUFFER(NSPMD))
              DO J=1,NSPMD
              INTERT8(P,I)%BUFFER(J)%NBMAIN = -1
              INTERT8(P,I)%BUFFER(J)%NBSECND_TOT = 0
              ENDDO
           ENDDO
         ENDDO

       END IF
C
C-----Allocation structures INTSTAMP
C
       IF(NINTSTAMP/=0)THEN
         IF(IDDLEVEL == 0) THEN
           ALLOCATE(INTSTAMP(NINTSTAMP)    ,STAT=stat)
           IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                                MSGTYPE=MSGERROR,
     .                         C1='INTSTAMP')
           CALL INTSTAMP_ZERO(INTSTAMP)
         END IF
       ELSE
         IF(IDDLEVEL == 0) THEN
           ALLOCATE(INTSTAMP(0))
         ENDIF
       END IF
C
       CALL LECINS(IPARI    ,ITAB      ,PM       ,IPM                          ,BUFMAT ,
     .             IGRNOD   ,IGRSURF   ,IGRSLIN  ,XFILTR                       ,STFAC  ,
     .             FRIC_P   ,FRIGAP    ,I2RUPT   ,AREASL                       ,LIXINT ,
     .             X        ,NINTER    ,IXS      ,NOM_OPT(LNOPT1*INOM_OPT(3)+1),
     .             IXC      ,IXTG      ,KNOD2ELC ,KNOD2ELTG                    ,NOD2ELC,
     .             NOD2ELTG ,INTBUF_TAB,KNOD2ELS ,NOD2ELS                      ,IXS10  ,
     .             IXS16    ,IXS20     ,NIGE     ,RIGE                         ,XIGE   ,
     .             VIGE     ,IGRBRIC   ,MULTI_FVM,ALE_CONNECTIVITY%NALE        ,IGEO   ,
     .             INTERFACES)
C
cc     DEALLOCATE(XFILTR)
cc     DEALLOCATE(STFAC)
cc     DEALLOCATE(FRIC_P)
C      DEALLOCATE(FRIGAP)
cc     DEALLOCATE(I2RUPT)
cc     DEALLOCATE(AREASL)
C----
c      CALL ANCNTG(IDS, I, J)
       IDS =  60
c      CALL ANCHECK(IDS)
       IF(NINTSUB/=0)THEN
         CALL ININTSUB(
     .          ITAB      ,IGRNOD  ,IGRSURF   ,
     .          IPARI  ,MAXRTM,NOM_OPT(LNOPT1*INOM_OPT(3)+1),
     .          INTBUF_TAB,MAXRTMS ,IGRSLIN   )
       ENDIF
C----
       IF(IDDLEVEL == 0) THEN
         ALLOCATE(INSCR(NINTER)    ,STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                              MSGTYPE=MSGERROR,
     .                         C1='INSCR')

       ENDIF

       CALL PRESCRINT(IPARI,INTBUF_TAB,INSCR)

       IF(IDDLEVEL == 0) THEN

         DO I=1,NINTER
           ALLOCATE(INSCR(I)%WA(INSCR(I)%SINSCR) ,STAT=stat)
           IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                                MSGTYPE=MSGERROR,
     .                           C1='INSCR')
           INSCR(I)%WA = 0
         ENDDO
       ELSE
         DO I=1,NINTER
           INSCR(I)%WA = 0
         ENDDO
       END IF
       CALL SCRINT(IPARI   ,INSCR,  INTBUF_TAB)

      IF(IDDLEVEL == 0) CALL STOPTIME(10,1)
      IF(IDDLEVEL == 1) CALL STOPTIME(11,1)

      WRITE(ISTDO,'(A)')TITRE(68)
C-----
c       if (LSCR /= SINSCR) then
c          print*,'allocation error in inscr: ',SINSCR,'>',SIWORK
c       endif
       AUX = MAX( NUMNOD , NUMELT+NUMELP+NUMELR+NUMELTG+NUMELC+100 ,
     .            MAXRTM+100 )
       NS_I7 = 2*NUMNOD + 2002 + 4*AUX
C
       NS_I11 = 2002 + NMNT
       AUX = 2002 + 8*MAXRTMS
       NS_I11 = MAX(NS_I11,AUX)
C
C      Max size for interface type2 - i2buc1
       AUX = MAX( NUMNOD , MAXRTM_T2+100 )
       NS_I2 = 2*NUMNOD + 2002 + 4*AUX
C
       IFIP=MAX(NS_I7,NS_I11,
     .   NUMNOD+2+4*NUMELC+4*NUMELTG+8*NUMELS
     .  +2*NUMELT+2*NUMELP+2*NUMELR)
C-----
       SIWORK = MAX(NS_I7,NS_I11,NUMNOD+2+4*NUMELC+4*NUMELTG+8*NUMELS
     .        + 2*NUMELT+2*NUMELP+2*NUMELR+16*NUMELS10+ 2*(SIXX-1)
     .        + MAXNSNE,NS_I2)
C
       SRWORK = MAX(6000,NUMNOD)
       ALLOCATE(IWORK(SIWORK)    ,STAT=stat)
       ALLOCATE(RWORK(SRWORK)    ,STAT=stat)
       IWORK = 0
       RWORK = ZERO
C-----
       IF(LXINTD>0.AND.NSPMD>1)
     .   LIXINT = LIXINT + LXINTD
       IF(IDDLEVEL==0)THEN
         IF( ALLOCATED(IXINT) ) DEALLOCATE( IXINT )
         ALLOCATE(IXINT(LIXINT))
         ALLOCATE(XTMP(3*NUMNOD)    ,STAT=stat)
         XTMP = D(1:3*NUMNOD)
       ENDIF
       IFIXIN = IFIP
       IFIEND = IFIXIN
       NELEMINT = 0
       SIXINT = LIXINT
       LIXINT = 0
C
C read /INIBRI/FILL before interfaces stiffness
       IF(.NOT.ALLOCATED(FILLSOL)) ALLOCATE(FILLSOL(NUMELS),STAT=stat)
       IF(STAT/=0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                      C1='FILLSOL')
       IF(NUMELS/=0) CALL LECFILL(IXS,FILLSOL,UNITAB,LSUBMODEL)

      IF(IDDLEVEL == 0) CALL STARTIME(12,1)
      IF(IDDLEVEL == 1) CALL STARTIME(13,1)



C
C fill interface structure to be used by sorting
c set INTERCEP for INT7 only to avoid bug with INT20 dur to renumerotation
c in I20NLG (other interface types still done in SET_INTERCEP)
        CALL FILL_INTERCEP(IPARI,INTBUF_TAB,INTERCEP)
C
        CALL ININTR(IPARI     ,INSCR    ,X,V     ,IXS      ,IXQ      ,
     2               IXC      ,PM       ,GEO     ,ITAB     ,MS       ,
     3               IWORK    ,RWORK    ,IXTG    ,D        ,IXT      ,
     4               IXP      ,IXR      ,ALE_CONNECTIVITY  ,NELEMINT ,IDDLEVEL ,
     5               LIXINT   ,IGRBRIC  ,IWCONT  ,IWCIN2   ,KNOD2ELS ,
     7               KNOD2ELC ,KNOD2ELTG,NOD2ELS ,NOD2ELC  ,NOD2ELTG ,
     8               IGRSURF  ,IELEM21  ,SH4TREE ,SH3TREE  ,IPART    ,
     9               IPARTC   ,IPARTG   ,THKE    ,THK_PART ,NOD2EL1D ,
     A               KNOD2EL1D,IXS10    ,SIXINT  ,FRIGAP   ,IXS16    ,
     B               IXS20    ,IPM      ,NOM_OPT(LNOPT1*INOM_OPT(3)+1),IPARTS,
     C               KXX      ,IXX      ,IGEO    ,INTERCEP ,LELX     ,
     D               INTBUF_TAB,FILLSOL ,STACK%PM,IWORKSH  ,NSNT     ,
     E               NMNT_2   ,KXIG3D   ,IXIG3D  ,KNOD2ELQ ,NOD2ELQ  ,
     F               SEGQUADFR,TAGPRT_FRIC,INTBUF_FRIC_TAB ,IPARTT   ,
     G               IPARTP   ,IPARTX   ,IPARTR  ,NSN_MULTI_CONNEC ,T2_NB_CONNEC,
     H               ICODE    ,ISKEW    )
      IF(IDDLEVEL == 0) CALL STOPTIME(12,1)
      IF(IDDLEVEL == 1) CALL STOPTIME(13,1)

      IDEL_SOLID = 0
      DO I=1,NINTER 
         IF(IPARI(NPARI*(I-1)+7)==25.AND.IPARI(NPARI*(I-1)+100)>0) THEN
            IDEL_SOLID = IDEL_SOLID + 1
         ENDIF
      ENDDO
      IF(IDEL_SOLID == 0) INTERFACES%PARAMETERS%INT25_EROSION_SOLID = 0
         
        DEALLOCATE (T2_NB_CONNEC)
        DEALLOCATE (RWORK)
        DEALLOCATE (IWORK)

        IF(IDDLEVEL==1)  THEN
          DEALLOCATE(XFILTR)
          DEALLOCATE(FRIC_P)
          DEALLOCATE(FRIGAP)
        END IF
C-------------RBE3 use IXINT--pour opt.
        IF(LXINTD>0.AND.NSPMD>1)THEN
         IF(LIXINT+LXINTD > SIXINT)THEN
           LXINTD2=LXINTD/6
           CALL UPGRADE_IXINT(SIXINT,NELEMINT,LXINTD2)
         ENDIF
         CALL INTXRBE3(IRBE3  ,LRBE3  ,NELEMINT,IXINT,LIXINT)
        ENDIF
        CALL TRACE_OUT1()
C-----
        IF(IDDLEVEL==0)THEN
          D(1:3*NUMNOD) = XTMP(1:3*NUMNOD)
          DEALLOCATE(XTMP)
        END IF
C
      ELSEIF(IDDLEVEL == 0) THEN !NINTER = 0

        SINSCR = 0
        ALLOCATE(INTSTAMP(0))

        !--------------------------------------------!
        !   NEW INTERFACE BUFFER STRUCTURE ALLOCATION
        !--------------------------------------------!
        ALLOCATE(INTBUF_TAB(0), STAT=Stat)
        !--------------------------------------------!
C
      ENDIF


C--------------------------------------------
C     LECTURE DES MURS RIGIDES
C--------------------------------------------
        ERR_MSG='RIGID WALLS'
        ERR_CATEGORY='RIGID WALLS'
        CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
        SNPRW  = NRWALL*NNPRW
        SIWORK = NRWALL*NUMNOD
        SLPRW  = 0
        IF(IDDLEVEL==0) THEN
          ALLOCATE(NPRW(SNPRW)    ,STAT=stat)
        END IF
        ALLOCATE(IWORK(SIWORK)  ,STAT=stat)
        NPRW  = 0
        IWORK = 0
C
        SRWBUF = NRWLP*NRWALL
        SRWSAV = 0
        SRWORK = 3*NUMNOD*NRWALL+SRWBUF
        ALLOCATE(RWORK(SRWORK)    ,STAT=stat)
        RWORK = ZERO
        NRWLAG = 0
c
        IF(NRWALL > 0) THEN
          WRITE(ISTDO,'(A)')TITRE(39)
          CALL READ_RWALL(
     1         RWORK    ,NPRW     ,IWORK    ,SLPRW    ,MS       ,
     2         V        ,ITAB     ,ITABM1   ,X        ,IXS      ,
     3         IXQ      ,NPC1     ,D        ,IGRNOD   ,
     4         SRWBUF   ,IMERGE   ,UNITAB   ,
     5         IKINE1LAG,IDDLEVEL ,LSUBMODEL ,RTRANS  ,
     6         NOM_OPT(LNOPT1*INOM_OPT(5)+1),ITAGND)
        ENDIF
C

        IF(IDDLEVEL==0) THEN
          ALLOCATE(LPRW(SLPRW)  ,STAT=stat)
        END IF
        LPRW = IWORK(1:SLPRW)
        DEALLOCATE(IWORK)
        IF(IDDLEVEL==0) THEN
          ALLOCATE(RWBUF(SRWBUF)  ,STAT=stat)
          IF(STAT /= 0) THEN
            CALL ANCMSG(MSGID=727,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANSTOP,
     .                  C1='RWBUF')
          ENDIF
        END IF
        RWBUF = RWORK(1:SRWBUF)
        DEALLOCATE(RWORK)
        IF(ALLOCATED(RWSAV)) DEALLOCATE(RWSAV)
        ALLOCATE(RWSAV(SRWSAV)  ,STAT=stat)
        CALL TRACE_OUT1()
C
C--------------------------------------------
C     LECTURE DES MASSES AJOUTEES
C--------------------------------------------
        ERR_MSG='ADDED MASSES'
        ERR_CATEGORY='ADDED MASSES'
        CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
        IF(NODMAS > 0)THEN
           WRITE(ISTDO,'(A)')TITRE(43)
          IF(IDDLEVEL==0) THEN
            ALLOCATE(IPMAS(NODMAS),STAT=stat)
            IPMAS(1:NODMAS)%NPART = 0
            IPMAS(1:NODMAS)%WEIGHT_FLAG = 0
            IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                                 MSGTYPE=MSGERROR,
     .                                 C1='IPMAS')
          ENDIF
!---
           FLAGG = 0
!---
           CALL HM_READ_ADMAS(
     .                  MS      ,ITABM1   ,IGRNOD   ,UNITAB ,IGRSURF,
     .                  IPART   ,IPMAS    ,TOTADDMAS,FLAGG  ,IGRPART,
     .                  X       ,LSUBMODEL)
!---
           FLAGG = 1
!---
           CALL HM_READ_ADMAS(
     .                  MS      ,ITABM1   ,IGRNOD   ,UNITAB ,IGRSURF,
     .                  IPART   ,IPMAS    ,TOTADDMAS,FLAGG  ,IGRPART,
     .                  X       ,LSUBMODEL)
!---
           IF(NS10E>0) CALL ADDMAST10(ICNDS10,  MS    )
C---
        ELSE
          IF(IDDLEVEL==0) THEN
            ALLOCATE(IPMAS(0))
          ENDIF
        ENDIF
        CALL TRACE_OUT1()
C--------------------------------------------
C     LECTURE DES STRUCTURES RIGIDES
C--------------------------------------------
        ERR_MSG='RIGID ENTITIES'
        ERR_CATEGORY='RIGID BODY'
        CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
        CALL HM_PREREAD_RBODY    (SLPBY  ,IGRNOD  ,LSUBMODEL)
        CALL PREREAD_RBODY_LAGMUL(SLPBYL ,IGRNOD  ,LSUBMODEL)
        CALL HM_PREREAD_MERGE(SMGRBY, SLPBY, IGRNOD, LSUBMODEL)
        SNPBY   = NNPBY*NRBYKIN
        SNPBYL  = NNPBY*NRBYLAG
        SNRBODY = SNPBY + SNPBYL
        SLRBODY = SLPBY + SLPBYL
        SRBY    = NRBY*NRBODY
        IF(IDDLEVEL==0) THEN
          IF(ALLOCATED(NPBY)) DEALLOCATE(NPBY)
          IF(ALLOCATED(LPBY)) DEALLOCATE(LPBY)
          IF(ALLOCATED(RBY)) DEALLOCATE(RBY)
          ALLOCATE(NPBY(SNRBODY),STAT=stat)
          ALLOCATE(LPBY(SLRBODY),STAT=stat)
          ALLOCATE(RBY(SRBY)    ,STAT=stat)
        END IF
        IF(NRBMERGE > 0) THEN
          ALLOCATE(MGRBY(NMGRBY*SMGRBY),STAT=stat)
        ELSE
          ALLOCATE(MGRBY(0),STAT=stat)
        ENDIF
        NPBY  = 0
        LPBY  = 0
        MGRBY = 0
        RBY   = ZERO
        IF(SNPBY<SNRBODY) THEN
          NPBYL => NPBY(SNPBY+1:SNRBODY)
        ELSE
          NPBYL => NPBY
        END IF
        IF(SLPBY<SLRBODY) THEN
        LPBYL => LPBY(SLPBY+1:SLRBODY)
        ELSE
          LPBYL => LPBY
        END IF
        IF(NRBY *NRBYKIN<SRBY) THEN
          RBYL  => RBY(NRBY *NRBYKIN+1:SRBY)
        ELSE
          RBYL  => RBY
        END IF
C
        IF(NRBODY  > 0) WRITE(ISTDO,'(A)')TITRE(41)
        IF(NRBYKIN > 0) THEN
            CALL HM_READ_RBODY(
     1       RBY       ,NPBY    ,LPBY    ,ITAB    ,ITABM1    ,
     2       IGRNOD    ,IGRSURF ,IBFV    ,IGRV    ,LGRAV     ,
     3       SENSORS   ,IMERGE  ,UNITAB  ,ISKWN   ,NOM_OPT   ,
     4       SLRBODY   ,KNOD2ELS,KNOD2ELC,KNOD2ELTG,KNOD2EL1D,
     5       KNOD2ELQ  ,ITAGND  ,ICNDS10 ,LSUBMODEL,ICFIELD  ,
     6       LCFIELD   )
        ENDIF
C--------------------------------------------
C     LECTURE DES FUSIONS DE RIGID BODY
C--------------------------------------------
        IF(NRBMERGE > 0) THEN
          CALL HM_READ_MERGE(
     .      MGRBY,SMGRBY ,NPBY,LPBY   ,SLRBODY,
     .      RBY  ,NOM_OPT,INOM_OPT(30),IGRNOD ,
     .      ITAB ,ITABM1 ,LGRAV       ,IGRV   ,
     .      LSUBMODEL)
        ENDIF
C--------------------------------------------
C     CHECK DES STRUCTURES RIGIDES
C--------------------------------------------
        IF(NRBYKIN > 0) THEN
          CALL CHECKRBY(
     1       RBY    ,NPBY    ,LPBY   ,ITAB   ,
     2       D      ,IDDLEVEL,NOM_OPT,SLRBODY)
        ENDIF
C--------------------------------------------
C     LECTURE RB LAGRANGE
C--------------------------------------------
        IF(NRBYLAG > 0) THEN
         CALL HM_READ_RBODY_LAGMUL(RBYL    ,NPBYL   ,LPBYL   ,IGRNOD   ,LSUBMODEL ,
     .                             ITAB    ,ITABM1  ,D       ,IKINE1LAG,NOM_OPT)
        ENDIF
C
        CALL TRACE_OUT1()
C--------------------------------------------
      IF(NINTER > 0) THEN
C--------------------------------------------
C
        IF(NINTSTAMP/=0)THEN
          ERR_MSG='INTERFACES TYPE21'
          ERR_CATEGORY='INTERFACES'
          CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c          IF(IDDLEVEL==0)THEN
            CALL LECSTAMP(IPARI  ,INTSTAMP ,UNITAB, NPBY,
     .                    ICODE  ,NOM_OPT(LNOPT1*INOM_OPT(3)+1),LSUBMODEL)
c          ENDIF
          CALL TRACE_OUT1()
        END IF
C--------------------------------------------
C OPTIMISATION INTERFACE SPMD
C--------------------------------------------
        ERR_CATEGORY='INTERNAL'
        IF(IDDLEVEL==0)THEN
          IF(IALE+IEULER/=0)
     +       CALL PAROI(PM     ,IXS    ,IXQ    ,ICODE  ,ALE_CONNECTIVITY%NALE   )
          IF(NUMELS/=0)
     +       CALL LCE16S4(IXS    ,PM     ,ICODE  )
C
          LAG_NCF = LAG_NCF0
          LAG_NKF = LAG_NKF0
          LAG_NHF = LAG_NHF0
          LAG_NCL = LAG_NCL0
          LAG_NKL = LAG_NKL0
        ENDIF
      ENDIF

C--------------------------------------------
C
C     After IDDLEVEL - we do not enter inintr anymore
C     set I11FLAG to 1
      I11FLAG = 1

      DEALLOCATE(IELEM21)
C
C---------------------------------
      IF((SEANI > 0).AND.(IDDLEVEL==1))  EANI = 0
C---------------------------------
#ifdef DNC
C--------------------------------------------
C     LECTURE et PREPARATION DES ELEMENTS FINIS A ENVOYER A MADYMO :
C     "EXTENDED COUPLING".
C--------------------------------------------
      ERR_MSG='MADYMO INTERFACED FEM'
      ERR_CATEGORY='MADYMO INTERFACED FEM'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NEXMAD/=0) THEN
        SIWORK2 = MAX(NPART,NUMNOD,2*NUMNOD+NUMELC+NUMELTG+NUMELS)
        SIWORK  = NPART+NUMNOD+2*(NUMELC+NUMELTG+NUMELS)

        ALLOCATE(IWORK(SIWORK+SIWORK2),STAT=stat)
        IWORK  = 0
        IF(SIWORK<SIWORK+SIWORK2) THEN
        IWORK2 => IWORK(SIWORK+1:SIWORK+SIWORK2)
        ELSE
          IWORK2 => IWORK
        ENDIF
      ENDIF
      IF(ALLOCATED(ICONX)) DEALLOCATE(ICONX)
      IF(NEXMAD/=0) THEN
       WRITE(ISTDO,'(A)')' .. FEM INTERFACED TO MADYMO'
        CALL HM_READ_MADYMO_EXFEM(IWORK(7*NCONX+1),ITAB ,ITABM1   ,IPART   ,IPARTC,
     .               IPARTG   ,IPARTS   ,IXC      ,IXTG    ,IXS     ,
     .               IWORK2   ,GEO      ,PM       ,IWORK   ,IGEO    ,
     .               IPM      ,LSUBMODEL)

        SMADPRT = NMADPRT
        SMADSH4 = NMADSH4
        SMADSH3 = NMADSH3
        SMADSOL = NMADSOL
        SMADNOD = NMADNOD

        SMADFAIL= NUMELC+NUMELTG+NUMELS

        SIEXTAG  = 2*NMADNOD+NMADSH4+NMADSH3+NMADSOL

        SIEXMAD  =  NMADPRT+NMADSH4+NMADSH3+NMADSOL+NMADNOD
     .              + NUMELC+NUMELTG+NUMELS

        SICONX =   7*NCONX+SIEXMAD+SIEXTAG
        ALLOCATE(ICONX(SICONX),STAT=stat)
        ICONX(1:SICONX) = 0
        DO I=1,7*NCONX+SIEXMAD
            ICONX(I) = IWORK(I)
        ENDDO
      ELSE
        SICONX  = 7*NCONX
        ALLOCATE(ICONX(SICONX),STAT=stat)
        IF(SICONX > 0)THEN
          ICONX(1:7*NCONX)=IWORK(1:7*NCONX)
        ENDIF
      ENDIF
      IF(ALLOCATED(IWORK)) DEALLOCATE(IWORK)
      CALL TRACE_OUT1()
#else
      IF(.NOT. ALLOCATED(ICONX)) ALLOCATE(ICONX(0))
#endif
C---------------------------------------------
C     LECTURE DES CORPS FLEXIBLES
C---------------------------------------------
      ERR_MSG='FLEXIBLE BODIES'
      ERR_CATEGORY='FLEXIBLE BODIES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C

      IF(NFXBODY == 0) THEN
         IF(IDDLEVEL==0) THEN
           ALLOCATE(FXBNOD(0),FXB_MATRIX(0),FXB_MATRIX_ADD(4,0))
           ALLOCATE(FXBGLM(0), FXBCPM(0) , FXBRPM(0),
     .              FXBCPS(0) , FXBLM(0)  , FXBFLS(0),
     .              FXBDLS(0), FXBDEP(0), FXBVIT(0),
     .              FXBACC(0), FXBMOD(0), FXBELM(0),
     .              FXBSIG(0), FXBGRVI(0), FXBGRVR(0))
         ENDIF
      ELSE IF(NFXBODY>0) THEN
C
         IF(IDDLEVEL==1) THEN
C--      length must be reset for second pass
           LENGLM=0
           LENCP=0
           LENLM=0
           LENFLS=0
           LENDLS=0
           LENVAR=0
           LENRPM=0
           LENMCD=0
           LENELM=0
           LENSIG=0
           LENGRVI=0
           LENGRVR=0
         ENDIF
C
         IF(IDDLEVEL==0) THEN
           INQUIRE(IOLENGTH=RCLEN) FLREC6
           OPEN(UNIT=IFXM,STATUS='SCRATCH',
     .        ACCESS='DIRECT',RECL=RCLEN)
           OPEN(UNIT=IFXS,STATUS='SCRATCH',
     .        ACCESS='DIRECT',RECL=RCLEN)
           WRITE(ISTDO,'(A)')TITRE(51)
           ALLOCATE(FXBNOD(LENNOD),FXB_MATRIX(LENMAT),FXB_MATRIX_ADD(4,LENMAT))
         ENDIF
C
         CALL HM_READ_FXB1(NOM_OPT(LNOPT1*INOM_OPT(11)+1),FXBNOD,FXBIPM,FXB_MATRIX,FXB_MATRIX_ADD,
     .                     NMANIM,ITAB,ITABM1,FXBFILE_TAB,LSUBMODEL)
C
         DO NFX=1,NFXBODY
            AIPM=(NFX-1)*NBIPM
            ANOD=FXBIPM(AIPM+6)
            NBNO=FXBIPM(AIPM+3)
            NBMO=FXBIPM(AIPM+4)+FXBIPM(AIPM+17)
            FXBIPM(AIPM+19)=LENELM+1
            FXBIPM(AIPM+20)=LENSIG+1
            FXBIPM(AIPM+26)=LENGRVI+1
            FXBIPM(AIPM+27)=LENGRVR+1
C
            CALL FXBTAGN(
     .      FXBNOD(ANOD), NBNO,    FXBIPM(AIPM+18), IBCL   ,      IPRES   ,
     .      IXS    ,      IXC    , IXT    ,         IXP    ,      IXR    ,
     .      IXTG   ,      IPARG  , FXBTAG,          NBMO,     FXBIPM(AIPM+4),
     .      NELS   ,      NELC,    NELTG,           IGRV   ,LGRAV  ,
     .      NLGRAV ,      IPARI  , INTBUF_TAB     , FXBIPM(AIPM+29), NELT,
     .      NELP)
            FXBIPM(AIPM+21)=NELS
            FXBIPM(AIPM+22)=NELC
            FXBIPM(AIPM+23)=NELTG
            FXBIPM(AIPM+34)=NELT
            FXBIPM(AIPM+35)=NELP
            FXBIPM(AIPM+24)=0
            FXBIPM(AIPM+25)=NLGRAV
         ENDDO
C
         IF(IDDLEVEL==0) THEN
           ALLOCATE(FXBGLM(LENGLM), FXBCPM(LENCP) , FXBRPM(LENRPM),
     .              FXBCPS(LENCP) , FXBLM(LENLM)  , FXBFLS(LENFLS),
     .              FXBDLS(LENDLS), FXBDEP(LENVAR), FXBVIT(LENVAR),
     .              FXBACC(LENVAR), FXBMOD(LENMOD*6), FXBELM(LENELM),
     .              FXBSIG(LENSIG), FXBGRVI(LENGRVI), FXBGRVR(LENGRVR))
         ENDIF
C
         FXBELM(1:LENELM)= 0
         DO NFX=1,NFXBODY
            AIPM=(NFX-1)*NBIPM
            ANOD=FXBIPM(AIPM+6)
            NBNO=FXBIPM(AIPM+3)
            ALM=FXBIPM(AIPM+19)
            IF(FXBIPM(AIPM+4)>0) CALL FXBELNUM(
     .              FXBNOD(ANOD), NBNO,    IPARG  , FXBTAG, FXBELM(ALM),
     .              IXS    ,      IXC    , IXTG   , IPARTS ,IPARTC   ,
     .              IPARTG  ,     IXT    , IXP    , IPARTT  ,IPARTP    )
         ENDDO
C
         CALL HM_READ_FXB2(FXBIPM,  FXBRPM, FXBNOD, FXBGLM,
     .                FXBCPM,   FXBCPS,  FXBLM,  FXBFLS, FXBDLS,
     .                FXBMOD, ITAB    , ITABM1 , NOM_OPT(LNOPT1*INOM_OPT(11)+1),FXB_LAST_ADRESS,
     .                LSUBMODEL)
C
C
      ELSEIF(IDDLEVEL==0) THEN
         ALLOCATE(FXBNOD(0) , FXBMOD(0), FXBGLM(0), FXBGRVI(0),
     .            FXBCPM(0) , FXBCPS(0), FXBLM(0) , FXBFLS(0) ,
     .            FXBDLS(0) , FXBDEP(0), FXBVIT(0), FXBACC(0) ,
     .            FXBRPM(0) , FXBELM(0), FXBSIG(0),
     .            FXBGRVR(0))
      ENDIF
C
      CALL TRACE_OUT1()
      ERR_MSG='EIGEN MODES'
      ERR_CATEGORY='EIGEN MODES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NEIG>0) THEN
C
         INQUIRE(IOLENGTH=RCLEN) FLREC6
         OPEN(UNIT=IEIGM,STATUS='SCRATCH',
     .        ACCESS='DIRECT',RECL=RCLEN)
C
         WRITE(ISTDO,'(A)')TITRE(52)
         CALL HM_PREREAD_EIG(IGRNOD   ,NNT   ,LSUBMODEL)
         NEIPM=17
         NERPM=4
         LEIBUF = NNT
         IF(IDDLEVEL==0) THEN
           ALLOCATE(EIGIPM(NEIPM*NEIG), EIGIBUF(NNT))
           ALLOCATE(EIGRPM(NERPM*NEIG))
           EIGIPM = 0
           EIGIBUF = 0
           EIGRPM = ZERO
         ENDIF
C
         CALL HM_READ_EIG(EIGIPM, EIGIBUF, EIGRPM, IGRNOD   ,ITABM1   ,
     .                       UNITAB, LSUBMODEL)
      ELSEIF(IDDLEVEL==0) THEN
         ALLOCATE(EIGIPM(0), EIGIBUF(0))
         ALLOCATE(EIGRPM(0))
      ENDIF
      CALL TRACE_OUT1()
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NDSOLV==1) THEN
        WRITE(6,*) "ERROR Deprecated Linear solver"
        CALL ARRET(5)
      ELSEIF(IDDLEVEL==0) THEN
         NSLEVEL=0
         ALLOCATE(CEPTMP(0), NELDOM(0), ELDOM(0,0,0),
     .            ELSUB(0,0))
      ENDIF
      CALL TRACE_OUT1()
C
C  shell composite xfem
C
      ERR_MSG='COMPOSITE SHELLS'
      ERR_CATEGORY='COMPOSITE SHELLS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
      IF(IDDLEVEL==0) THEN
       IF(IPLYXFEM > 0) THEN
         ALLOCATE(MS_PLY0(NUMNOD*NPLYMAX),STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                              MSGTYPE=MSGERROR,
     .                         C1='MS_PLY0')
         MS_PLY0=ZERO
         ALLOCATE(ZI_PLY0(NUMNOD*NPLYMAX),STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                              MSGTYPE=MSGERROR,
     .                         C1='ZI_PLY0')
         ZI_PLY0=ZERO
         ALLOCATE(MSZ20(NUMNOD),STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                              MSGTYPE=MSGERROR,
     .                         C1='MSZ20')
         MSZ20=ZERO
         ALLOCATE(ITAGND_SHXFEM(NUMNOD),STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                              MSGTYPE=MSGERROR,
     .                         C1='ITAGND_SHXFEM')
         ITAGND_SHXFEM=0
         ALLOCATE(ITAGSH(NUMELC),STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                              MSGTYPE=MSGERROR,
     .                         C1='ITAGSH')
         ITAGSH=0
         ALLOCATE(INOD_PXFEM(NUMNOD),STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                              MSGTYPE=MSGERROR,
     .                         C1='INOD_PXFEM')
         INOD_PXFEM=0
         ALLOCATE(IEL_PXFEM(NUMELC),STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                              MSGTYPE=MSGERROR,
     .                         C1='IEL_PXFEM')
         IEL_PXFEM=0
       ELSE
          ALLOCATE(MS_PLY0(0),ZI_PLY0(0),ITAGND_SHXFEM(0),
     .             ITAGSH(0),INOD_PXFEM(0),IEL_PXFEM(0))
          ALLOCATE(MS_PLY(0),ZI_PLY(0),MSZ20(0))
       ENDIF
      ENDIF
C
      CALL TRACE_OUT1()
C
C-----------------------------------------------------------
C
      ERR_MSG='ARRAYS ALLOCATION FOR INTIA'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
C     tab masse
      IF(IDDLEVEL == 0) THEN
        ALLOCATE(MSC(NUMELC)   ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='MSC')
        ALLOCATE(MSTG(NUMELTG)   ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='MSTG')
        ALLOCATE(INC(NUMELC)   ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='INC')
        ALLOCATE(INTG(NUMELTG)   ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='INTG')
        ALLOCATE(PTG(3,NUMELTG)    ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='PTG')
        IF(ITHERM_FE > 0)THEN
          ALLOCATE(MCPC(NUMELC)    ,STAT=stat)
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='MCPC')
          ALLOCATE(MCPTG(NUMELTG)  ,STAT=stat)
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='MCPTG')
        ELSE
          ALLOCATE(MCPC(0),MCPTG(0))
        END IF
      ENDIF
C
      MSC   = ZERO
      MSTG  = ZERO
      INC   = ZERO
      INTG  = ZERO
      PTG   = ZERO
      MCPC  = ZERO
      MCPTG = ZERO
C
C-------------------------------------------------------
C
      IF(IDDLEVEL == 0) THEN
        IF(IREST_MSELT/=0)THEN
          ALLOCATE(MSSA(NUMELS)     ,STAT=stat)
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='MSSA')
          ALLOCATE(MSRT(NUMELR)     ,STAT=stat)
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                             MSGTYPE=MSGERROR,
     .                         C1='MSRT')
        ELSE
          ALLOCATE(MSSA(0)          ,STAT=stat)
          ALLOCATE(MSRT(0)          ,STAT=stat)
        ENDIF
C-------------------------------------------------------
        IF(I7STIFS/=0)THEN
          ALLOCATE(STIFINT(NUMNOD+NUMFAKENODIGEO)    ,STAT=stat)
          ALLOCATE(STIFINTR(NUMNOD)   ,STAT=stat)
        ELSE
          ALLOCATE(STIFINT(0)    ,STAT=stat)
          ALLOCATE(STIFINTR(0)   ,STAT=stat)
        ENDIF
C-------------------------------------------------------
        IF(IRIGID_MAT > 0) THEN
           ALLOCATE(SLNRBM(NUMNOD) ,NSLNRBM(NUMNOD))
           ALLOCATE( RMSTIFN(NUMNOD), RMSTIFR(NUMNOD))
         ELSE
           ALLOCATE( SLNRBM(0),NSLNRBM(0),RMSTIFN(0), RMSTIFR(0))
           ALLOCATE( FRONT_RM(0))
        ENDIF
C-------------------------------------------------------
        ALLOCATE(FXANI(2,NMANIM), MBUFEL(LBUFEL,NMANIM),
     .         MDEPL(3*NUMNOD,NMANIM))
        ALLOCATE(STIFFN(NUMNOD*2)            ,STAT=stat)
      ENDIF
C
      STIFINT = ZERO
      STIFINTR = ZERO
      SLNRBM= 0
      NSLNRBM=0
      IF(NUMNOD  > 0) STIFFN  = EM20
C
      CALL TRACE_OUT1()
C--------------------------------------------
      IF(ABS(ISIGI)==3.OR.ABS(ISIGI)==4.OR.ABS(ISIGI)==5)THEN
        IF(.NOT.ALLOCATED(PTSHEL)) THEN
         ALLOCATE (PTSHEL(NUMELC)   ,STAT=stat) 
         PTSHEL = 0
        ENDIF 
        IF(.NOT.ALLOCATED(PTSH3N)) THEN 
          ALLOCATE (PTSH3N(NUMELTG)  ,STAT=stat)
           PTSH3N = 0
         ENDIF   
      ELSE
        ALLOCATE (PTSHEL(0)  ,STAT=stat)
        ALLOCATE (PTSH3N(0)  ,STAT=stat)
      END IF
      ! still need for *Y00, *sty files - not yet covered by CFG files (hm_reader)
      IF(IDDLEVEL == 0) CALL YCTRL(IGRBRIC)
!
      IF (IDDLEVEL == 0) CALL HM_YCTRL(UNITAB,LSUBMODEL,IGRBRIC,IXC,IXTG, PTSHEL,PTSH3N)
C
C----------------------------------------------------------
C
C XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C  The following is executed with iddlevel=0 only for AMS with automatic element selection
C  or if no contact interfaces and no ams
      IF((IDDLEVEL == 1).OR.(ISMS_SELEC >= 3).OR.((NINTER == 0).AND.(ISMS == 0))) THEN
C XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C
C--------------------------------------------
C     ELEMENT BUFFER INTIALIZATION
C--------------------------------------------
      ERR_MSG='ELEMENT BUFFER INITIALIZATION'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      WRITE(ISTDO,'(A)')TITRE(45)
      NUMEL=2*(NUMELC+NUMELQ+NUMELT+NUMELS+NUMELP+NUMELR+
     &         NUMELTG+NUMELX+NUMSPH+NUMELIG3D)
C
C--------------------------------------------
C     NEW ELEMENT BUFFER STRUCTURE ALLOCATION
C--------------------------------------------
c
      FLAG_XFEM = 0
      ALLOCATE(ELBUF_TAB(NGROUP), STAT=Stat)
C
      CALL ELBUF_INI(ELBUF_TAB,MAT_ELEM%MAT_PARAM,
     .               MLAW_TAG ,PROP_TAG ,FAIL_TAG ,
     .               IGEO     ,IPM      ,PM       ,IPARG    ,IPART     ,
     .               IPARTSP  ,IXS      ,IXQ      ,IXC      ,IXTG      ,
     .               FLAG_XFEM,IPARTIG3D,STACK    ,IGEO_STACK,
     .               IXT      ,IXP      ,IXR      ,KXX      ,GEO       ,
     .               EOS_TAG  ,MAXEOS   ,ISTR_24  ,IPRI)
C---
C  if xfem
c---
      IF(ICRACK3D > 0) THEN
        FLAG_XFEM = 1

        ALLOCATE(XFEM_TAB(NGROUP,NXEL), STAT=Stat)
c
        DO IXEL=1,NXEL
          CALL ELBUF_INI(XFEM_TAB(1:NGROUP,IXEL),MAT_ELEM%MAT_PARAM,
     .         MLAW_TAG ,PROP_TAG ,FAIL_TAG ,
     .         IGEO                   ,IPM      ,PM       ,IPARG    ,IPART     ,
     .         IPARTSP                ,IXS      ,IXQ      ,IXC      ,IXTG      ,
     .         FLAG_XFEM              ,IPARTIG3D,STACK    ,IGEO_STACK,
     .         IXT                    ,IXP      ,IXR      ,KXX      ,GEO       ,
     .         EOS_TAG                ,MAXEOS   ,ISTR_24  ,IPRI)
        ENDDO
      ELSE
        ALLOCATE(XFEM_TAB(0,0), STAT=Stat)
      ENDIF
C--------------------------------------------
C     WARNING FOR PTHICKFAIL
C--------------------------------------------
      CALL CHECK_PTHICKFAIL(ELBUF_TAB,IPARG    ,GEO      ,BUFMAT   ,SBUFMAT  ,
     .                      IPM      ,STACK    ,IGEO     ,NUMMAT   ,NUMGEO   ,
     .                      NGROUP   ,NPARG    ,NPROPMI  ,NPROPGI  ,NPROPG   )
C
C--------------------------------------------
C     CHECK COMPATIBILITY /DTTSH
C--------------------------------------------
      IF(NUMELS>0) THEN
       CALL CHK_DTTSH(ELBUF_TAB,IXS ,IPARG ,D   )
      END IF
C
C-----------------------------------------------------------
C     ALLOCATION OF ARRAYS FOR INITIA - DEALLOCATED AFTER INTIA
C-----------------------------------------------------------
C
      NSIGS   =11
      LSIGSH  = 0
      LSIGSP  = 0
      LSIGSPH = 0
      LSIGI   = MAX (NUMELS+NUMELQ,NUMSOL+NUMQUAD)
      LSIGRS  = 0
      LSIGBEAM = 0
      LSIGTRUSS = 0
      NSIGI = 0
      NSIGSPH= 11
      NSIGSH = 0
      NSIGRS = 0
      NSIGBEAM = 0
      NSIGTRUSS = 0
      IF(ISIGI/=0)THEN
        NSIGSH = NVSHELL
        NSIGRS = 54  ! not cover NUVAR --> to be replaced by NSIGRS = NVSPRI
        NSIGBEAM = NVBEAM
        NSIGTRUSS = NVTRUSS
        IF(NUBEAM > 0) NSIGBEAM = NSIGBEAM + NUBEAM
        IF(IUSHELL/=0) NSIGSH = NSIGSH + NUSHELL
        IF(IORTSHEL/=0) NSIGSH = NSIGSH + NORTSHEL
        IF(NVSHELL1/=0)NSIGSH = NSIGSH + NVSHELL1
        IF(NVSHELL2 /= 0)NSIGSH = NSIGSH + NVSHELL2 + 3
        NSIGI= NVSOLID1 + NVSOLID2 + NVSOLID3 + NUSOLID + 4 + NVSOLID4 +
     .         NVSOLID5 + NVSOLID6 + 7


        IF(IABS(ISIGI) == 3 .OR. IABS(ISIGI) == 4 .OR.
     .      IABS(ISIGI) == 5) THEN
          LSIGSH  = NUMSHEL+NUMSH3N
          LSIGSP  = MAX(NUMSOL+NUMQUAD,NUMELS+NUMELQ)
          LSIGSPH = NUMSPHY
          LSIGRS = NUMSPRI
          LSIGBEAM = NUMBEAM
          LSIGTRUSS = NUMTRUS
        ELSE
          LSIGSH  = NUMELC+NUMELTG
          LSIGSP  = NUMELS+NUMELQ
          LSIGSPH = NUMELS+NUMELQ
          LSIGRS  = NUMELR
          LSIGBEAM = NUMELP
          LSIGTRUSS = NUMELT
        END IF
      END IF
C
      IF(ABS(ISIGI)==3.OR.ABS(ISIGI)==4.OR.ABS(ISIGI)==5)THEN
         IMAX  = MAX(NUMELS,NUMELQ,NUMELC,NUMELT,NUMELP,NUMELR,
     .               NUMELTG,NUMSOL,NUMQUAD,NUMSHEL,NUMSH3N,
     .               NUMSPHY,NUMSPRI,NUMBEAM,NUMTRUS)
      ELSE
         IMAX  = MAX(NUMELS,NUMELQ,NUMELC,NUMELT,NUMELP,NUMELR,
     .               NUMELTG,NUMELIG3D)
      END IF
      SINDEX = 2*IMAX
      SITRI  = IMAX
      IF(ABS(ISIGI)==3.OR.ABS(ISIGI)==4.OR.ABS(ISIGI)==5)THEN
        JMAX=MAX(NUMELC,NUMSOL,NUMQUAD,NUMSHEL,NUMSH3N,NUMSPHY,
     .           NUMELR,NUMSPRI,NUMELP,NUMBEAM,NUMTRUS)
      ELSE
        JMAX=0
      END IF
C
C------- refsta
      IF(ABS(ISIGI)==3.OR.ABS(ISIGI)==4.OR.ABS(ISIGI)==5)THEN
        ALLOCATE (PTSOL(NUMELS)    ,STAT=stat)
        ALLOCATE (PTQUAD(NUMELQ)   ,STAT=stat)
        ALLOCATE (PTSPH(NUMSPH)    ,STAT=stat)
        ALLOCATE (PTSPRI(NUMELR)   ,STAT=stat)
        ALLOCATE (PTBEAM(NUMELP)   ,STAT=stat)
        ALLOCATE (PTTRUSS(NUMELT)  ,STAT=stat)
        PTSOL  = 0
        PTQUAD = 0
        PTSPH  = 0
        PTSPRI = 0
        PTBEAM = 0
        PTTRUSS= 0
      ELSE
        ALLOCATE (PTSOL(0)   ,STAT=stat)
        ALLOCATE (PTQUAD(0)  ,STAT=stat)
        ALLOCATE (PTSPH(0)   ,STAT=stat)
        ALLOCATE (PTSPRI(0)  ,STAT=stat)
        ALLOCATE (PTBEAM(0)  ,STAT=stat)
        ALLOCATE (PTTRUSS(0) ,STAT=stat)
      END IF
C
      ICO = 0
      ITET4_10=0
      CALL TET4_10(IGEO,ITET4_10)
      IF(NUMELS10/=0.OR.NUMELS16/=0.OR.NUMELS20/=0.OR.ITET4_10/=0) THEN
        ICO=12
      ENDIF
C
C non optimise (12 max(8,10,12)
      ALLOCATE(MSS(8*NUMELS)     ,STAT=stat)
      ALLOCATE(MSSX(ICO*NUMELS)  ,STAT=stat)
      ALLOCATE(MSSF(8*NUMELS*MAX(IALE,IEULER,IALELAG))  ,STAT=stat)
      ALLOCATE(MSQ(NUMELQ)       ,STAT=stat)
      IF(.NOT.ALLOCATED(MSTR)) ALLOCATE(MSTR(NUMELT)      ,STAT=stat)
      IF(.NOT.ALLOCATED(MSP)) ALLOCATE(MSP(NUMELP)       ,STAT=stat)
      ALLOCATE(MSR(NUMELR*3)     ,STAT=stat)
      ALLOCATE(INP(NUMELP)       ,STAT=stat)
      ALLOCATE(INR(NUMELR*3)     ,STAT=stat)
      ALLOCATE(INS(NUMELS*8)     ,STAT=stat)
      MSS   = ZERO
      MSSX  = ZERO
      MSSF  = ZERO
      MSQ   = ZERO
      MSTR  = ZERO
      MSP   = ZERO
      MSR   = ZERO
      INP   = ZERO
      INR   = ZERO
      INS   = ZERO
C------
C      ELSE
C        ALLOCATE(MSS(0)     ,STAT=stat)
C        ALLOCATE(MSSX(0)    ,STAT=stat)
C        ALLOCATE(MSSF(0)    ,STAT=stat)
C        ALLOCATE(MSQ(0)     ,STAT=stat)
C        ALLOCATE(MSTR(0)    ,STAT=stat)
C        ALLOCATE(MSP(0)     ,STAT=stat)
C        ALLOCATE(MSR(0)     ,STAT=stat)
C        ALLOCATE(INP(0)     ,STAT=stat)
C        ALLOCATE(INR(0)     ,STAT=stat)
C        ALLOCATE(INS(0)     ,STAT=stat)
C      ENDIF
C----
      ALLOCATE(XELEMWA(MAXNX*16)     ,STAT=stat)
      XELEMWA = ZERO
      IF(I7STIFS/=0) THEN
        ICO = 0
        IF(NUMELS10/=0.OR.NUMELS16/=0.OR.NUMELS20/=0.OR.ITET4_10/=0) THEN
          ICO=12
        ENDIF
        ALLOCATE(VNS(NUMELS*8+NUMELIG3D*NCTRLMAX)     ,STAT=stat)
        ALLOCATE(VNSX(NUMELS*ICO)  ,STAT=stat)
        ALLOCATE(STC(NUMELC)       ,STAT=stat)
        ALLOCATE(STT(NUMELT)       ,STAT=stat)
        ALLOCATE(STP(NUMELP)       ,STAT=stat)
        ALLOCATE(STR(NUMELR)       ,STAT=stat)
        ALLOCATE(STTG(NUMELTG)     ,STAT=stat)
        ALLOCATE(STUR(0)           ,STAT=stat)
        ALLOCATE(BNS(NUMELS*8+NUMELIG3D*NCTRLMAX)     ,STAT=stat)
        ALLOCATE(BNSX(NUMELS*ICO)  ,STAT=stat)
        ALLOCATE(VNIGE(NUMELIG3D*NCTRLMAX)     ,STAT=stat)
        ALLOCATE(BNIGE(NUMELIG3D*NCTRLMAX)     ,STAT=stat)
        VNS = ZERO
        VNSX = ZERO
        STC = ZERO
        STT = ZERO
        STP = ZERO
        STR = ZERO
        STTG = ZERO
        STUR = ZERO
        BNS = ZERO
        BNSX = ZERO
        VNIGE = ZERO
        BNIGE = ZERO
      ELSE
        ALLOCATE(VNS(0))
        ALLOCATE(VNSX(0))
        ALLOCATE(STC(0))
        ALLOCATE(STT(0))
        ALLOCATE(STP(0))
        ALLOCATE(STR(0))
        ALLOCATE(STTG(0))
        ALLOCATE(STUR(0))
        ALLOCATE(BNS(0))
        ALLOCATE(BNSX(0))
        ALLOCATE(VNIGE(0))
        ALLOCATE(BNIGE(0))
      ENDIF
      IF(I7STIFS/=0)THEN
        ALLOCATE(VOLNOD(NUMNOD+NUMFAKENODIGEO)     ,STAT=stat)
        ALLOCATE(BVOLNOD(NUMNOD+NUMFAKENODIGEO)    ,STAT=stat)
        ALLOCATE(ETNOD(NUMNOD)      ,STAT=stat)
        ALLOCATE(NSHNOD(NUMNOD)     ,STAT=stat)
        VOLNOD  = ZERO
        BVOLNOD = ZERO
        ETNOD   = ZERO
        NSHNOD  = ZERO
        STIFINT = ZERO
        STIFINTR = ZERO
      ELSE
        ALLOCATE(VOLNOD(0)     ,STAT=stat)
        ALLOCATE(BVOLNOD(0)    ,STAT=stat)
        ALLOCATE(ETNOD(0)      ,STAT=stat)
        ALLOCATE(NSHNOD(0)     ,STAT=stat)
      ENDIF

C-- Rot. Stiffness parithon computation -> allocated even if no interfaces, to avoid "if" in element routines
      ALLOCATE(STRC(NUMELC)       ,STAT=stat)
      ALLOCATE(STRP(NUMELP)       ,STAT=stat)
      ALLOCATE(STRR(NUMELR)       ,STAT=stat)
      ALLOCATE(STRTG(NUMELTG)       ,STAT=stat)
      STRC = ZERO
      STRP = ZERO
      STRR = ZERO
      STRTG = ZERO
C---
      ALLOCATE(INDEX(SINDEX)    ,STAT=stat)
      ALLOCATE(ITRI(SITRI)      ,STAT=stat)
      ALLOCATE(KSYSUSR(2*JMAX)  ,STAT=stat)
      ALLOCATE(ISPTAG(NUMSPH)   ,STAT=stat)
      IF(SINDEX > 0) INDEX = 0
      IF(SITRI  > 0) ITRI  = 0
      IF(JMAX   > 0) KSYSUSR = 0
      IF(NUMSPH > 0) ISPTAG  = 0
C
      IF(NRBYKIN>0) THEN
         ALLOCATE(IWA(NUMNOD),STAT=stat)
      ELSE
         ALLOCATE(IWA(0),STAT=stat)
      ENDIF
C
      CALL TRACE_OUT1()

      ERR_MSG='INITIALIZATION'
      ERR_CATEGORY='ELEMENT INITIALIZATION'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C
C----------------------------------
C -- LECTURE OF INITIAL STATE DATA
C----------------------------------
C
      ALLOCATE(SIGI(NSIGS,LSIGI)                  ,STAT=stat)
      ALLOCATE(SIGSH(MAX(1,NSIGSH),MAX(1,LSIGSH)) ,STAT=stat)
      ALLOCATE(SIGSP(NSIGI,LSIGSP)                ,STAT=stat)
      ALLOCATE(SIGSPH(NSIGSPH,LSIGSPH)            ,STAT=stat)
      ALLOCATE(SIGRS(NSIGRS,LSIGRS)               ,STAT=stat)
      ALLOCATE(SIGBEAM(NSIGBEAM,LSIGBEAM)         ,STAT=stat)
      ALLOCATE(SIGTRUSS(NSIGTRUSS,LSIGTRUSS)      ,STAT=stat)
      ALLOCATE(STRSGLOB(NUMELS)                   ,STAT=stat)
      ALLOCATE(STRAGLOB(NUMELS)                   ,STAT=stat)
      ALLOCATE(ORTHOGLOB(NUMELS)                  ,STAT=stat)
C
      IF(LSIGI   > 0) SIGI   = ZERO
      IF(LSIGSH  > 0) SIGSH  = ZERO
      IF(LSIGSP  > 0) SIGSP  = ZERO
      IF(LSIGSPH > 0) SIGSPH = ZERO
      IF(LSIGRS  > 0) SIGRS  = ZERO
      IF(LSIGBEAM > 0)SIGBEAM= ZERO
      IF(LSIGTRUSS > 0)SIGTRUSS= ZERO
      IF(NUMELS  > 0) STRSGLOB  = -1
      IF(NUMELS  > 0) STRAGLOB  = -1
      IF(NUMELS  > 0) ORTHOGLOB  = 0
C

      IF(.NOT. ALLOCATED(IDRAPE)) ALLOCATE(IDRAPE(0))

      CALL LEC_INISTATE(   IXS     ,IXQ     ,IXC     ,IXT     ,
     1            IXP     ,IXR     ,GEO     ,PM      ,KXSP    ,
     2            IXTG    ,INDEX   ,ITRI    ,
     3            NSIGSH  ,IGEO    ,IPM     ,NSIGS   ,NSIGSPH ,
     4            KSYSUSR ,PTSHEL  ,PTSH3N  ,PTSOL   ,PTQUAD  ,
     5            PTSPH   ,NUMEL   ,NSIGRS  ,UNITAB  ,ISOLNOD ,
     6            LSUBMODEL,RTRANS ,IDRAPE  ,NSIGI   ,
     7            PTSPRI  ,NSIGBEAM,PTBEAM  ,NSIGTRUSS,PTTRUSS ,
     8            SIGI    ,SIGSH   ,SIGSP   ,SIGSPH  ,SIGRS   ,
     9            SIGBEAM ,SIGTRUSS,STRSGLOB,STRAGLOB,ORTHOGLOB,
     A            ISIGSH  ,IYLDINI ,KSIGSH3 ,FAIL_INI,IUSOLYLD,
     B            IUSERL  ,IGRBRIC ,MAP_TABLES,IPARG ,STACK ,IWORKSH)
C
C----------------------------------
C -- ELEMENT INITIALIZATION
C----------------------------------
C
      ALLOCATE(DTELEM(2*NUMEL)  ,STAT=stat)
      IF(STAT/=0) THEN
        CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                         C1='DTELEM')
      ENDIF
      IF(NUMEL   > 0) DTELEM = ZERO
C
      CALL STARTIME(14,1)
        
      CALL INITIA(IPARG   ,ELBUF      ,MS        ,IN      ,V       ,
     1            X       ,IXS        ,IXQ       ,IXC     ,IXT     ,
     2            IXP     ,IXR        ,DETONATORS,GEO     ,PM      ,
     3            RBY     ,NPBY       ,LPBY      ,NPC     ,NPTS    ,
     4            TF      ,VEUL       ,ALE_CONNECTIVITY   ,SKEW    ,FILL    ,
     5            IPART   ,ITAB       ,
     6            IXTG    ,THKE       ,NLOC_DMG  ,GROUP_PARAM_TAB  ,
     7            IGRNOD  ,IGRSURF    ,BUFSF     ,VR      ,
     8            BUFMAT  ,XLAS       ,ILAS      ,DTELEM  ,MSS     ,
     9            MSQ     ,MSC        ,MSTR      ,MSP     ,MSR     ,
     A            MSTG    ,PTG        ,INC       ,
     B            INP     ,INR        ,INTG      ,INDEX   ,
     C            ITRI    ,KXX        ,IXX       ,XELEMWA ,
     E            IWA     ,
     F            KXSP    ,IXSP       ,NOD2SP    ,ISPCOND ,ICODE   ,
     G            ISKEW   ,ISKWN      ,ISPSYM    ,XFRAME  ,ISPTAG  ,
     H            SPBUF   ,MSSX       ,NSIGI     ,
     I            NPBYL   ,LPBYL      ,RBYL      ,MSNF    ,MSSF    ,
     J            NSIGSH  ,IGEO       ,IPM       ,NSIGS   ,
     K            NSIGSPH ,VNS        ,VNSX      ,STC     ,STT     ,
     L            STP     ,STR        ,STTG      ,STUR    ,BNS     ,
     M            BNSX    ,VOLNOD     ,BVOLNOD   ,ETNOD   ,NSHNOD  ,
     N            STIFINT ,FXBDEP     ,FXBVIT    ,FXBACC  ,FXBIPM  ,
     O            FXBRPM  ,FXBELM     ,FXBSIG    ,FXBMOD  ,INS     ,
     P            PTSHEL  ,PTSH3N     ,PTSOL     ,PTQUAD  ,
     Q            WMA     ,PTSPH      ,FXBNOD    ,MBUFEL  ,MDEPL   ,
     R            FXANI   ,NUMEL      ,NSIGRS    ,
     T            SH4TREE ,SH3TREE    ,MCP       ,TEMP    ,
     U            IMERGE2 ,IADMERGE2        ,
     V            SLNRBM  ,NSLNRBM    ,RMSTIFN   ,RMSTIFR ,
     U            MS_PLY0 ,ZI_PLY0    ,ITAGND_SHXFEM,ITAGSH  ,MCPC    ,
     W            MCPTG   ,XREFC      ,XREFTG    ,XREFS   ,MSSA    ,
     X            MSRT    ,IRBE2      ,LRBE2     ,USER_INIVOL  ,KVOL    , NBSUBMAT,
     Y            IXS10,IXS16,IXS20,TOTADDMAS,
     Z            IPMAS   ,STIFFN     ,MSZ20     ,ITAGN   ,SITAGE,
     1            ITAGE   ,IXR_KJ     ,ELBUF_TAB,
     2            NOM_OPT ,INOM_OPT(13),INOM_OPT(21),INOM_OPT(20),
     3            SOL2SPH ,IRST       ,SH3TRIM   ,XFEM_TAB,
     4            KXIG3D  ,IXIG3D     ,MSIG3D    ,KNOT    ,NCTRLMAX,
     5            WIGE    ,STACK      ,
     7            RNOISE  ,DRAPE    ,SH4ANG  ,SH3ANG  ,
     8            GEO_STACK,IGEO_STACK,STIFINTR  ,STRC ,STRP    ,
     8            STRR    ,STRTG      ,PERTURB   ,ITAGND  ,NATIV_SMS,
     9            ILOADP  ,LOADP      ,PTSPRI    ,NSIGBEAM,
     A            PTBEAM  ,NSIGTRUSS  ,PTTRUSS,
     B            MULTI_FVM  ,SIGI    ,SIGSH     ,SIGSP    ,
     C            SIGSPH  ,SIGRS      ,SIGBEAM   ,SIGTRUSS ,STRSGLOB,
     D            STRAGLOB,ORTHOGLOB  ,ISIGSH    ,IYLDINI  ,KSIGSH3 ,
     E            FAIL_INI,IUSOLYLD   ,IUSERL    ,IDDLEVEL ,INIMAP1D,
     F            INIMAP2D,FUNC2D     ,FVM_INIVEL,TAGPRT_SMS,IGRBRIC,
     G            IGRQUAD ,IGRSH4N    ,IGRSH3N   ,IGRPART  ,TOTMAS  ,
     H            KNOTLOCPC,KNOTLOCEL ,VNIGE     ,BNIGE    ,FXBGLM  ,
     I            FXBCPM  ,FXBCPS     ,FXBLM     ,FXBFLS   ,FXBDLS  ,
     J            FXB_MATRIX,FXB_MATRIX_ADD,FXB_LAST_ADRESS,INOM_OPT(11),R_SKEW,
     K            KNOD2EL1D,NOD2EL1D  ,EBCS_TAB  ,RBY_INIAXIS,ALEA  ,
     L            KNOD2ELC,NOD2ELC    ,DR        ,SLRBODY , DRAPEG  ,
     M            IPARI   ,INTBUF_TAB ,INTERFACES) 


      IF(NINTER>0.AND.NUMELIG3D>0) THEN
        CALL FICTIVMASSIGEO(INTBUF_TAB,NCTRLMAX,MSIG3D  ,KXIG3D)
        IF(I7STIFS/=0)THEN
          CALL BULKFAKEIGEO3(ELBUF_TAB,IPARG,PM,KXIG3D,IGRSURF,STIFINT)
        ENDIF
      ENDIF
C
C----------------------------------
C
      CALL STOPTIME(14,1)


      CALL TRACE_OUT1()
!---
C--------------------------------------------
C     INITIALISATION DES BUFFERS  --- IGRNOD, IGRBRIC, ..., IGRSURF, ... ---
C--------------------------------------------
      ERR_MSG='GROUP ENTITIES BUFFER INITIALIZATION'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
!!      WRITE(ISTDO,'(A)')TITRE(45)
!
      CALL GROUP_INI(IGRNOD   ,IGRBRIC ,IGRQUAD   ,IGRSH4N ,IGRSH3N,
     .               IGRTRUSS ,IGRBEAM ,IGRSPRING ,IGRPART )
      CALL ISURF_INI(IGRSURF)
      CALL ISLIN_INI(IGRSLIN)
!
      CALL TRACE_OUT1()
C---
C
!!      DEALLOCATE(SIGI)
!!      DEALLOCATE(SIGSH)
!!      DEALLOCATE(SIGSP)
      DEALLOCATE(SIGSPH)
!!      DEALLOCATE(SIGRS)
!!      DEALLOCATE(SIGBEAM)
!!      DEALLOCATE(SIGTRUSS)
      DEALLOCATE(XELEMWA)
      DEALLOCATE(STRSGLOB)
      DEALLOCATE(STRAGLOB)
      DEALLOCATE(ORTHOGLOB)
C
      DEALLOCATE(PTSHEL)
      DEALLOCATE(PTSH3N)
      DEALLOCATE(PTSOL)
      DEALLOCATE(PTQUAD)
      DEALLOCATE(PTSPH)
      DEALLOCATE(PTSPRI)
      DEALLOCATE(PTBEAM)
      DEALLOCATE(PTTRUSS)
      DEALLOCATE(MSS)
      DEALLOCATE(MSSX)
      DEALLOCATE(MSSF)
      DEALLOCATE(MSQ)
      DEALLOCATE(MSR)
      IF(ALLOCATED(MSIG3D)) DEALLOCATE(MSIG3D)
      IF(ALLOCATED(TABCONPATCH)) DEALLOCATE(TABCONPATCH)
      DEALLOCATE(INP)
      DEALLOCATE(INR)
      DEALLOCATE(INS)
      DEALLOCATE(VNS)
      DEALLOCATE(VNSX)
      DEALLOCATE(STC)
      DEALLOCATE(STT)
      DEALLOCATE(STP)
      DEALLOCATE(STR)
      DEALLOCATE(STTG)
      DEALLOCATE(STUR)
      DEALLOCATE(BNS)
      DEALLOCATE(BNSX)
      DEALLOCATE(VOLNOD)
      DEALLOCATE(BVOLNOD)
      DEALLOCATE(ETNOD)
      DEALLOCATE(NSHNOD)
      DEALLOCATE(VNIGE)
      DEALLOCATE(BNIGE)
      DEALLOCATE(STRC)
      DEALLOCATE(STRP)
      DEALLOCATE(STRR)
      DEALLOCATE(STRTG)
      DEALLOCATE(ISPTAG)
      DEALLOCATE(INDEX)
      DEALLOCATE(ITRI)
      DEALLOCATE(KSYSUSR)
      DEALLOCATE(IWA)

C
C XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C
      ENDIF !IF((IDDLEVEL == 1).OR.(ISMS_SELEC >= 3))
C
C XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C
      IF((IDDLEVEL == 0).AND.((NINTER > 0).OR.(ISMS == 1))) THEN
        IDDLEVEL = 1
        WRITE(ISTDO,*)
     .  '.. RETURNS TO DOMAIN DECOMPOSITION FOR OPTIMIZATION'
C XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C return to domain decomposition
C   ---> for interface and AMS  - small loop, without INITIA.F
C-  ---> for AMS with automatic element selection - big loop with INITIA.F
C XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C
        IF(IALE+IEULER/=0) CALL PAROI(PM     ,IXS    ,IXQ    ,ICODE  ,ALE_CONNECTIVITY%NALE   )
        IF(NUMELS/=0) CALL LCE16S4(IXS    ,PM     ,ICODE  )
C
        LAG_NCF = LAG_NCF0
        LAG_NKF = LAG_NKF0
        LAG_NHF = LAG_NHF0
        LAG_NCL = LAG_NCL0
        LAG_NKL = LAG_NKL0
C
        DEALLOCATE(ELBUF)
        DEALLOCATE(DD_IAD)
C
        IF(ISMS_SELEC >= 3) THEN
C XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
C       Additional treatments for big loop - element buffer deallocation
C XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
          DEALLOCATE(DTELEM)
          FLAG_XFEM = 0
          CALL DEALLOCATE_ELBUF(
     .               ELBUF_TAB,MLAW_TAG ,PROP_TAG ,FAIL_TAG ,
     .               IGEO     ,IPM      ,PM       ,IPARG    ,IPART     ,
     .               IPARTSP  ,IXS      ,IXQ      ,IXC      ,IXTG      ,
     .               KXSP     ,FLAG_XFEM,IPARTIG3D,STACK    ,IGEO_STACK,
     .               IXT      ,IXP      ,IXR      ,KXX      ,GEO       )
          DEALLOCATE(ELBUF_TAB)
C       XFEM buffer deallocation
          IF(ICRACK3D > 0) THEN
            FLAG_XFEM = 1
            DO IXEL=1,NXEL
              CALL DEALLOCATE_ELBUF(
     .               XFEM_TAB(1:NGROUP,IXEL),MLAW_TAG ,PROP_TAG ,FAIL_TAG ,
     .               IGEO     ,IPM      ,PM       ,IPARG    ,IPART     ,
     .               IPARTSP  ,IXS      ,IXQ      ,IXC      ,IXTG      ,
     .               KXSP     ,FLAG_XFEM,IPARTIG3D,STACK    ,IGEO_STACK,
     .               IXT      ,IXP      ,IXR      ,KXX      ,GEO      )
            ENDDO
          ENDIF
C
          REWIND(IIN4)
          REWIND(IIN5)
C XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
        ENDIF
C
        GOTO 100
C
      ENDIF
C
C------------------------------------------------------------------------
C
C  Check tied contacts -> hierarchy + warnings AMS
      ERR_CATEGORY='INTERFACES'
      CALL CHKTYP2 (IPARI,   ITAB    ,
     .              NOM_OPT(LNOPT1*INOM_OPT(3)+1),INTBUF_TAB,NATIV_SMS)
c-----------------------------------------------------------------------------------
c     Initialization of frontwave structure for failure propagation
c
      CALL FAILWAVE_INIT(FAILWAVE,IPARG,IXC,IXTG,NUMNOD)
c
c-----------------------------------------------------------------------------------
c
C rigid material
C

      ERR_MSG='RIGID MATERIALS'
      ERR_CATEGORY='RIGID MATERIALS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
       IF(IRIGID_MAT > 0) THEN
C
C  NFRBYM = 28 (
C  NIRBYM = 2
         ALLOCATE(RBYM(NFRBYM*NRBYM),IRBYM(NRBYM*NIRBYM),
     .                                         LNRBYM(NGSLNRBYM))
C
         CALL RIGID_MAT(NRBYM   ,NGSLNRBYM ,SLNRBM, NSLNRBM ,RMSTIFN,
     .                  RMSTIFR ,X     ,V       ,MS      , IN       ,
     .                  RBYM  ,IRBYM    ,LNRBYM ,NOM_OPT)
C
           LEN_RM = NRBYM*NSPMD
           ALLOCATE(FRONT_RM(LEN_RM))
           FRONT_RM = 0
           ALLOCATE(WEIGHT_RM(NRBYM))
           WEIGHT_RM = 1
       ELSE
          ALLOCATE( RBYM(0),IRBYM(0),LNRBYM(0), WEIGHT_RM(0))
       ENDIF
      CALL TRACE_OUT1()
      ERR_MSG='DEALLOCATION'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      DEALLOCATE(SLNRBM,NSLNRBM,RMSTIFN,RMSTIFR )
C----
      IF(ALLOCATED(MSIG3D)) DEALLOCATE(MSIG3D)
      IF(ALLOCATED(ITAG)) DEALLOCATE(ITAG)
C
C xfem for compostie
C
      CALL TRACE_OUT1()
      ERR_MSG='XFEM FOR COMPOSIT'
      ERR_CATEGORY='XFEM FOR COMPOSIT'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(IPLYXFEM > 0) THEN
        NPLYXFE = 0
        EPLYXFE = 0
        DO I=1,NUMNOD
         IF(ITAGND_SHXFEM(I) > 0 )THEN
           NPLYXFE = NPLYXFE + 1
           INOD_PXFEM(I) = NPLYXFE
         ENDIF
        ENDDO
C
        DO I=1,NUMELC
          IF(ITAGSH(I) > 0) THEN
           EPLYXFE = EPLYXFE + 1
           IEL_PXFEM(I) = EPLYXFE
          ENDIF
        ENDDO
          ALLOCATE(MS_PLY(NPLYXFE*NPLYMAX),STAT=stat)
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                         C1='MS_PLY')
          MS_PLY=ZERO
          ALLOCATE(ZI_PLY(NPLYXFE*NPLYMAX),STAT=stat)
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                         C1='ZI_PLY')
          ZI_PLY=ZERO
C
          ALLOCATE(MSZ2(NPLYXFE),STAT=stat)
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                         C1='MSZ2')
          MSZ2=ZERO
C
          CALL PREPLYXFEM(MS_PLY0,ZI_PLY0,IEL_PXFEM,INOD_PXFEM,IXC,
     .                  MS_PLY,ZI_PLY,ADDCNE_PXFEM,MSZ20,MSZ2)
          LCNE_PXFEM = ADDCNE_PXFEM(NPLYXFE+1) - 1
          ALLOCATE(CNE_PXFEM(LCNE_PXFEM),CEL_PXFEM(EPLYXFE))
          CNE_PXFEM = 0
          CEL_PXFEM = 0
C
          CALL FILLCNE_PXFEM(IEL_PXFEM,INOD_PXFEM,IXC,CEP,ADDCNE_PXFEM,
     .                       CNE_PXFEM, CEL_PXFEM)
C
      ENDIF
C
      DEALLOCATE(MS_PLY0,ZI_PLY0,MSZ20,ITAGSH)
      DEALLOCATE(ITAGND_SHXFEM)
      CALL TRACE_OUT1()
c-------------------------------------------------------------------
c-------------------------------------------------------------------
C     XFEM for crack propagation within shell (mono + multi layers)
c-------------------------------------------------------------------
      ERR_MSG='XFEM FOR SHELLS'
      ERR_CATEGORY='XFEM FOR SHELLS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c
c------------------------------
      ALLOCATE(CRKLVSET(NLEVMAX)  ,STAT=stat)
      ALLOCATE(CRKSHELL(NLEVMAX)  ,STAT=stat)
      ALLOCATE(CRKSKY(NLEVMAX)    ,STAT=stat)
      ALLOCATE(CRKAVX(NLEVMAX)    ,STAT=stat)
      ALLOCATE(INDX_CRK(NLEVMAX)  ,STAT=stat)
c
      INDX_CRK = 0  ! For Anim
      NCRKPART = 0  ! Nombre des parts xfem  (local proc)
      NCRKXFE  = 0  ! Nombre des noeuds xfem
      ECRKXFE  = 0  ! Nombre des elements xfem
      ECRKXFEC = 0  ! Nombre des shells 4N xfem
      ECRKXFETG= 0  ! Nombre des shells 3N xfem
c------------------------------
      IF(ICRACK3D > 0) THEN
c-----
        IF(ICRACK3D == 1) THEN
          WRITE(ISTDO,'(A)')' .. XFEM MULTI-LAYER SHELL'
        ELSEIF(ICRACK3D == 2)THEN
          WRITE(ISTDO,'(A)')' .. XFEM MONO-LAYER SHELL'
        ELSEIF(ICRACK3D == 3)THEN
          WRITE(ISTDO,'(A)')' .. XFEM MIXED MONO/MULTI-LAYER SHELL'
        ENDIF
c-----
c       numerotation locale des noeuds et elems fantomes des parts xfem
c       IEL_CRKXFEM  : local system numerotation of Xfem shells
c       INOD_CRKXFEM : local system numerotation of Xfem nodes
c--------------------------------------------------
c       build local Xfem node and element tables
        CALL PRETAG_XFEM(IPARG  ,ITAGE   ,IEL_CRKXFEM,ITAGN   ,INOD_CRKXFEM)
c
c       build xfem sky adress table
        CALL PRECRKXFEM(IPARG ,IXC  ,IXTG   ,NCRKXFE ,
     .                  IEL_CRKXFEM ,INOD_CRKXFEM    ,ADDCNE_CRKXFEM)
c--------------------------------------------------
c       ADDCNE_CRKXFEM = tableau adresses sky Xfem
        LCNE_CRKXFEM = ADDCNE_CRKXFEM(NCRKXFE+1) - 1    ! longueur tableau sky CNE_CRKXFEM
        ALLOCATE(CRKNODIAD(LCNE_CRKXFEM)    ,STAT=stat)
        ALLOCATE(CNE_CRKXFEM(LCNE_CRKXFEM)  ,STAT=stat)
        ALLOCATE(CEL_CRKXFEM(ECRKXFE)       ,STAT=stat)
        ALLOCATE(CEP_CRKXFEM(ECRKXFE)       ,STAT=stat)
        ALLOCATE(NODLEVXF(NCRKXFE)          ,STAT=stat)
        ALLOCATE(CRKEDGE(NXLAYMAX)          ,STAT=stat)
        ALLOCATE(XFEM_PHANTOM(NXLAYMAX)     ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANSTOP,MSGTYPE=MSGERROR,C1='NODLEVXF')
        CRKNODIAD   = 0
        CNE_CRKXFEM = 0
        CEL_CRKXFEM = 0
        CEP_CRKXFEM = 0   ! Proc num of each xfem element
        NUMELCRK    = 0
        NODLEVXF    = 0
c--------------------------------------------------
        CALL FILLCNE_XFEM(LCNE_CRKXFEM,IPARG,
     .       IEL_CRKXFEM   ,INOD_CRKXFEM ,IXC        ,IXTG       ,CEP      ,
     .       ADDCNE_CRKXFEM,CNE_CRKXFEM  ,CEL_CRKXFEM,CEP_CRKXFEM,CRKNODIAD)
c
        CALL XFEM_CRACK_INIT(IPARG   ,IXC     ,IXTG    ,INOD_CRKXFEM,NODLEVXF,
     .                       INDX_CRK,NCRKPART,CRKSHELL)
c--------------------------------------------------
        DEALLOCATE(ITAGN,ITAGE)
C       NODGLOBXFE
        SNODGLOBXFE = 4*ECRKXFE*NLEVMAX  ! nb max de noeuds phant, tous les plis. faux si mixte
C
        NUMEDGES = 0   ! nb des edges glob (pareil tous les plis)
        SIEDGESH = 4*ECRKXFEC + 3*ECRKXFETG
c
        ALLOCATE(IEDGESH(SIEDGESH),STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANSTOP,MSGTYPE=MSGERROR,C1='IEDGESH')
        ALLOCATE(IBORDEDGE(SIEDGESH)     ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANSTOP,MSGTYPE=MSGERROR,C1='IBORDEDGE')
        ALLOCATE(NODEDGE(2*SIEDGESH),STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANSTOP,MSGTYPE=MSGERROR,C1='NODEDGE')
        ALLOCATE(IEDGE(SIEDGESH),STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANSTOP,MSGTYPE=MSGERROR,C1='IEDGE')
        ALLOCATE(IEDGE_TMP0(SIEDGESH),STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANSTOP,MSGTYPE=MSGERROR,C1='IEDGE_TMP0')
        IEDGESH   = 0
        IBORDEDGE = 0
        NODEDGE   = 0
        IEDGE     = 0
        IEDGE_TMP0= 0
!
        IF(ECRKXFEC > 0) THEN
          IEDGESH4 => IEDGESH(1:4*ECRKXFEC)
          IELCRK4  => IEL_CRKXFEM(1:NUMELC)
        ELSE
          IEDGESH4 => IEDGESH
          IELCRK4  => IEL_CRKXFEM
        ENDIF
!
        IF(ECRKXFETG > 0) THEN
          IEDGESH3 => IEDGESH(1+4*ECRKXFEC:SIEDGESH)
          IELCRK3  => IEL_CRKXFEM(1+NUMELC:NUMELC+NUMELTG)
        ELSE
          IEDGESH3 => IEDGESH
          IELCRK3  => IEL_CRKXFEM
        ENDIF
c--------------------------------------------------
        CALL IEDGE_XFEM(
     .       IBORDNODE ,IXC        ,IXTG       ,IEDGESH4  ,IEDGESH3  ,
     .       IBORDEDGE ,NODEDGE    ,IELCRK4    ,IELCRK3   ,IEDGE     ,
     .       CEP_CRKXFEM,IEDGE_TMP0)
c
        CALL ALLOCXFEM(IXC    ,IXTG  ,IPARG  ,LCNE_CRKXFEM,CRKLVSET,
     .                 CRKSKY ,CRKAVX,CRKEDGE,XFEM_PHANTOM)
c--------------------------------------------------
        IF(NINICRACK > 0)   ! initial cracks
     .    CALL INICRKFILL (ELBUF_TAB,XFEM_TAB,
     .         IXC     ,IXTG       ,IPARG       ,INICRACK,
     .         X       ,IEL_CRKXFEM,INOD_CRKXFEM,XREFC   ,XREFTG  ,
     .         IEDGESH4  ,IEDGESH3,NODEDGE ,CRKLVSET,
     .         CRKSHELL,CRKEDGE    ,XFEM_PHANTOM         ,ITAB  )
c--------------------------------------------------
        ALLOCATE(IEDGE_TMP(3,NUMEDGES),STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANSTOP,
     .                             MSGTYPE=MSGERROR,C1='IEDGE_TMP')
        IF(NUMEDGES > 0) THEN
          DO I=1,NUMEDGES
            IEDGE_TMP(1,I) = 0
            IEDGE_TMP(2,I) = 0
            IEDGE_TMP(3,I) = IEDGE_TMP0(I)
          ENDDO
        ENDIF
        DEALLOCATE(IEDGE_TMP0)
C---
        ALLOCATE(ELCUTC(2*(NUMELC+NUMELTG))     ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANSTOP,
     .                             MSGTYPE=MSGERROR,C1='ELCUTC')
        ELCUTC = 0
C---
        ALLOCATE(NODENR(NCRKXFE)     ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANSTOP,
     .                             MSGTYPE=MSGERROR,C1='NODENR')
        NODENR = 0
C---
        ALLOCATE(KXFENOD2ELC(NCRKXFE)     ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANSTOP,
     .                             MSGTYPE=MSGERROR,C1='KXFENOD2ELC')
        KXFENOD2ELC = 0
C---
        ALLOCATE(ENRTAG(NUMNOD*IENRNOD)     ,STAT=stat)
        IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANSTOP,
     .                             MSGTYPE=MSGERROR,C1='ENRTAG')
        ENRTAG = 0
C---
      ELSE
        NUMEDGES = 0
        SIEDGESH = 0
        NUMELCRK= 0
        ALLOCATE(CNE_CRKXFEM(0),CEL_CRKXFEM(0),CEP_CRKXFEM(0))
        ALLOCATE(IEDGESH(0))
        ALLOCATE(IBORDEDGE(0))
        ALLOCATE(NODEDGE(0))
        ALLOCATE(IEDGE(0))
        ALLOCATE(IEDGE_TMP(0,0))
        ALLOCATE(CRKNODIAD(0))
        ALLOCATE(NODLEVXF(0))
        ALLOCATE(CRKEDGE(0))
C
        ALLOCATE(ELCUTC(0))
        ALLOCATE(NODENR(0))
        ALLOCATE(KXFENOD2ELC(0))
        ALLOCATE(ENRTAG(0))
      ENDIF   !   ICRACK3D > 0  (Xfem)
C----------------------------------
      CALL TRACE_OUT1()
C----------------------------------
C     RBE2 Desactivation des elements initialization for ITRUOFF ...
C----------------------------------
      ERR_MSG='RIGID BODY ELEMENT DEACTIVATION'
      ERR_CATEGORY='RIGID BODY'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL SETELOFF2(IXS    ,IXC    ,IXT    ,IXP    ,IXR    ,
     2               IXTG   ,IPARG  ,ISOLOFF,ISHEOFF,
     3              ITRUOFF,IPOUOFF,IRESOFF,ITRIOFF,IGRNRB2,
     4               IGRNOD ,IRBE2  )
C----------------------------------
C     RBODY Desactivation des elements des rigid body (on par defaut)
C----------------------------------
      CALL SETELOFF(IXS    ,IXC    ,IXT    ,IXP    ,IXR    ,
     2              IXTG   ,IPARG  ,        ISOLOFF,ISHEOFF,
     3              ITRUOFF,IPOUOFF,IRESOFF,ITRIOFF,IGRNRBY,
     4              IGRNOD ,ELBUF_TAB,IQUAOFF,IXQ  )
      CALL TRACE_OUT1()
C----------------------------------
C     Interf Stamp. Tri et initialisations
C----------------------------------
      ERR_MSG='STAMPING INITIALIZATION'
      ERR_CATEGORY='INTERFACES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NINTER/=0)THEN
C-----
        AUX = MAX( NUMNOD , NUMELT+NUMELP+NUMELR+NUMELTG+NUMELC+100 ,
     .             MAXRTM+100 )
        NS_I21 = 2*NUMNOD + 2002 + 4*AUX
        SIWORK = NS_I21
        SRWORK = MAX(6000,NUMNOD)
        ALLOCATE(IWORK(SIWORK)    ,STAT=stat)
        ALLOCATE(RWORK(SRWORK)    ,STAT=stat)
        IWORK = 0
        RWORK = ZERO
C-----
        CALL ININTR_THKVAR(ELBUF_TAB,
     1               IPARI   ,INTBUF_TAB       ,INSCR    ,X       ,
     2               IXS     ,IXC     ,PM      ,GEO      ,ITAB    ,
     3               IWORK   ,RWORK   ,IXTG    ,D        ,
     4               IPARG   ,KNOD2ELS ,
     5               KNOD2ELC,KNOD2ELTG,NOD2ELS,NOD2ELC  ,NOD2ELTG,
     6               INTSTAMP,SKEW     ,MS     ,IN       ,V       ,
     7               VR      ,RBY      ,NPBY   ,LPBY     ,IPARTS  ,
     8               IPARTC  ,IPARTG,THK_PART,NOM_OPT,INOM_OPT(3))
        DEALLOCATE(RWORK)
        DEALLOCATE(IWORK)
C-----
      END IF
C-----

      DEALLOCATE(THK_PART)
      CALL TRACE_OUT1()
C-------------------------------------------------------------
C     Set INTERCEP only for INTERFACE24 (flag=0)
C-------------------------------------------------------------
      CALL SET_INTERCEP(IPARI,INTERCEP,0,INTBUF_TAB,ITAB,CEP)      ! this call is maintained here to avoid a bug
C-------------------------------------------------------------
C     Interface type 24 - set FRONTPLUS to neighboug surfaces
C-------------------------------------------------------------
      I24MAXNSNE = 0
      CALL I24SETNODES(IPARI,INTBUF_TAB,INTERCEP,ITAB,I24MAXNSNE)

C-------------------------------------------------------------
C----------------------------------
C     Interf. type 7 et 21 : affectation des rigidites cote second
C----------------------------------
      ERR_MSG='INTERFACES STIFFNESS'
      ERR_CATEGORY='INTERFACES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NINTER>0)THEN
        IF(I7STIFS/=0)
     .    CALL ININTR1 (IPARI   ,STIFINT, INTBUF_TAB ,STFAC)
        DEALLOCATE(STFAC)
      ENDIF
      CALL TRACE_OUT1()

C--------FRICTION OROTHTROPIC DIRECTIONS COMPUTATION -----
      IF(NINTER > 0 .AND.NINTERFRIC >0.AND. IORTHFRICMAX > 0) THEN

        CALL ININTR_ORTHDIRFRIC(
     A       IPARI   ,INTBUF_TAB,INTBUF_FRIC_TAB,IGEO  ,GEO      ,
     B       X       , IXTG     ,IXC          ,IPARTG  , IPARTC  ,
     C       PFRICORTH,IREPFORTH,PHIFORTH     , VFORTH ,KNOD2ELC ,
     D       KNOD2ELTG,NOD2ELTG ,NOD2ELC      ,IWORKSH  ,PM      ,
     E       STACK%PM ,THKE    ,SKEW          ,ITAB     ,IPART   )

c        DEALLOCATE(PFRICORTH ,IREPFORTH , VFORTH ,PHIFORTH  )

      ENDIF

        DEALLOCATE(TAGPRT_FRIC)
C---------------------------
C     IMPACTS LASER TRAITEMENT SPMD 2eme Phase
C---------------------------
      ERR_MSG='LASER IMPACT PHASE 2'
      ERR_MSG='LASER'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NLASER>0) THEN
          CALL LASERP3(ILAS   ,IPARG  )
        ENDIF
      CALL TRACE_OUT1()
C----------------------------------
C     LECTURE DES RIVETS
C----------------------------------
      ERR_MSG='RIVETS'
      ERR_CATEGORY='RIVETS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SLRIVET = NRIVET*4
      SRIVET  = NRIVET*NRIVF
      ALLOCATE(LRIVET(SLRIVET)    ,STAT=stat)
      ALLOCATE(RIVET(SRIVET)      ,STAT=stat)
      LRIVET = 0
      RIVET  = ZERO
C
      IF(NRIVET/=0)THEN
        WRITE(ISTDO,'(A)') ' .. RIVETS '
        CALL HM_READ_RIVET(LRIVET ,V      ,VR     ,MS     ,IN     ,
     2                     RIVET  ,GEO    ,ITAB   ,ITABM1 ,D      ,
     3                     IPART  ,IGEO   ,LSUBMODEL)
      ENDIF
c      CALL ANCHECK(81)
      CALL TRACE_OUT1()
C----------------------------------
C     SEATBELT 2D->1D for SECTIONS
C----------------------------------
        IF(NB_SEATBELT_SHELLS /= 0)THEN
          CALL MY_ALLOC(SEATBELT_SHELL_TO_SPRING,NUMELC,2)
          IF(NUMELC > 0)THEN
            SEATBELT_SHELL_TO_SPRING(1:NUMELC,1) = 0
            SEATBELT_SHELL_TO_SPRING(1:NUMELC,2) = 0
          ENDIF
c
          DO I=1,NB_SEATBELT_SHELLS

            L0 = 0
            IF(SEATBELT_CONVERTED_ELEMENTS(2,I) /= 0) THEN
              L0 = SET_USRTOS(SEATBELT_CONVERTED_ELEMENTS(1,I),MAP_TABLES%ISH4NM,NUMELC)
            ENDIF

            L1 = 0
            IF(SEATBELT_CONVERTED_ELEMENTS(2,I) /= 0) THEN
              L1 = SET_USRTOS(SEATBELT_CONVERTED_ELEMENTS(2,I),MAP_TABLES%ISPRINGM,NUMELR)
            ENDIF

            L2 = 0
            IF(SEATBELT_CONVERTED_ELEMENTS(3,I) /= 0) THEN
              L2 = SET_USRTOS(SEATBELT_CONVERTED_ELEMENTS(3,I),MAP_TABLES%ISPRINGM,NUMELR)
            ENDIF

            IF(L0 /= 0) THEN
              SEATBELT_SHELL_TO_SPRING(L0,1) = L1
              SEATBELT_SHELL_TO_SPRING(L0,2) = L2
            ENDIF
            
          ENDDO
        ELSE
          CALL MY_ALLOC(SEATBELT_SHELL_TO_SPRING,1,2)
          SEATBELT_SHELL_TO_SPRING(1,1) = 0
          SEATBELT_SHELL_TO_SPRING(1,2) = 0
        ENDIF
C----------------------------------
C     LECTURE DES SECTIONS
C----------------------------------
      ERR_MSG='SECTIONS'
      ERR_CATEGORY='SECTIONS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NSECT/=0)THEN
       WRITE(ISTDO,'(A)')  ' .. SECTIONS'
        CALL PRELECSEC(
     1      SNSTRF  ,SSECBUF   ,ITABM1  ,0       ,NOM_OPT(LNOPT1*INOM_OPT(8)+1),
     2      IGRBRIC ,IGRQUAD   ,IGRSH4N ,IGRSH3N ,IGRTRUSS ,
     3      IGRBEAM ,IGRSPRING ,IGRNOD  ,LSUBMODEL, SEATBELT_SHELL_TO_SPRING,
     4      NB_SEATBELT_SHELLS)
        ALLOCATE(NSTRF(SNSTRF)    ,STAT=stat)
        ALLOCATE(SECBUF(SSECBUF)  ,STAT=stat)
        NSTRF  = 0
        SECBUF = ZERO
        CALL LECSEC42(IXS    ,IXQ    ,IXC    ,IXT    ,IXP    ,IXR    ,
     2                IXTG   ,X      ,NSTRF  ,ITAB   ,ITABM1 ,
     3                IGRNOD   ,SECBUF ,
     4                IPARI  ,IXS10  ,IXS20  ,IXS16  ,UNITAB ,
     5                ISKWN  ,XFRAME ,ISOLNOD,NOM_SECT,RTRANS,
     6                LSUBMODEL,NOM_OPT(LNOPT1*INOM_OPT(8)+1),IGRBRIC,IGRQUAD,IGRSH4N,
     7                IGRTRUSS ,IGRBEAM,IGRSPRING,IGRSH3N,SEATBELT_SHELL_TO_SPRING,
     8                NB_SEATBELT_SHELLS)
      ELSE
        SNSTRF  = 0
        SSECBUF = 0
        ALLOCATE(NSTRF(SNSTRF)    ,STAT=stat)
        ALLOCATE(SECBUF(SSECBUF)  ,STAT=stat)
      ENDIF
      IF(ALLOCATED(SEATBELT_SHELL_TO_SPRING)) DEALLOCATE(SEATBELT_SHELL_TO_SPRING)
      CALL TRACE_OUT1()
C--------------------------------------------
C     SENSORS INITIALIZATION
C--------------------------------------------
c
      CALL INISEN(SENSORS ,IPARI,NOM_OPT,INOM_OPT(5),
     .            INOM_OPT(8),INOM_OPT(4),IXR ,R_SKEW ,NUMELR,
     .            NSECT   ,NINTER ,NINTSUB ,NRWALL   ,NRBODY  )
c
C----------------------------------
C     LECTURE DES JOINTS
C----------------------------------
      ERR_MSG='JOINTS'
      ERR_CATEGORY='JOINTS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      CALL HM_PRELECJOI(SLJOINT ,IGRNOD,LSUBMODEL)
      ALLOCATE(LJOINT(SLJOINT)    ,STAT=stat)
      LJOINT  = 0

      ALLOCATE( CYL_JOIN(NJOINT) )

      IF(NJOINT/=0)THEN
        WRITE(ISTDO,'(A)') ' .. CYLINDRICAL JOINTS'
        CALL INIT_JOINT(NJOINT)
        CALL HM_READ_CYLJOINT(LJOINT ,ITABM1 ,D      ,ITAB   ,IGRNOD ,
     .                        NOM_OPT(LNOPT1*INOM_OPT(7)+1),LSUBMODEL)
      ENDIF
c      CALL ANCHECK(83)
      CALL TRACE_OUT1()
C-------------------------------------------------
C     BLOCAGE DES NOEUDS MAT 11 HORS DOMAINE CALCULE
C-------------------------------------------------
      ERR_MSG='BLOCK BOUNDARY MATERIAL NODES'
      ERR_CATEGORY='BLOCK BOUNDARY MATERIAL NODES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(IALE+IEULER /= 0 .AND. NUMELQ+NUMELS  > 0) THEN
        CALL NODM11(PM,IXS,IXQ,ICODE)
      ENDIF
      CALL TRACE_OUT1()
C-------------------------------------------------
C     LISTE DES NOEUDS CORRESPONDANT A ONE MILIEU POREUX
C-------------------------------------------------
      ERR_MSG='POROUS NODES'
      ERR_CATEGORY='POROUS NODES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(IALE+IEULER /=0 .AND. NUMELQ+NUMELS >0)THEN
        SIWORK = NUMNOD+4*NFACX
        ALLOCATE(IWORK(SIWORK)    ,STAT=stat)
        SIWORK = 0
        CALL PORNOD(GEO    ,IXS    ,IXQ    ,IWORK  ,ICODE  ,
     +              ITAB   ,NPBY   ,LPBY   ,IGEO)
        SNODPOR = NUMPOR
        ALLOCATE(NODPOR(SNODPOR)    ,STAT=stat)
        NODPOR = IWORK(1:SNODPOR)
        DEALLOCATE(IWORK)
      ELSE
        SNODPOR = 0
        ALLOCATE(NODPOR(SNODPOR)    ,STAT=stat)
      ENDIF
      CALL TRACE_OUT1()
C---
      IF(KCONTACT/=0)THEN
        KCONTACT=1
        SICONTACT=NUMNOD
        ALLOCATE(ICONTACT(SICONTACT))
        ICONTACT = 0
      ELSE
        ALLOCATE(ICONTACT(0))
      END IF
      IF(NADMESH/=0)THEN
        SRCONTACT=NUMNOD
        ALLOCATE(RCONTACT(SRCONTACT))
        RCONTACT = EP30
        ALLOCATE(ACONTACT(SRCONTACT))
        ACONTACT = EP30
        ALLOCATE(PCONTACT(SRCONTACT))
        PCONTACT = ZERO
      ELSE
        ALLOCATE(RCONTACT(0))
        ALLOCATE(ACONTACT(0))
        ALLOCATE(PCONTACT(0))
      END IF
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
      CALL FVDIM(T_MONVOL)
      ALLOCATE(FVDATA(NFVBAG))

C
      ERR_MSG='FVMBAG MESHING'
      ERR_CATEGORY='FVMBAG MESHING'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(TETRAMESHER_USED) THEN
         CALL FVMESH0(T_MONVOL, NODE_COORD,  IXS_TEMP, IXC, IXTG, PM,
     .        IPM, ITAB,  IGRSURF, XYZREF  ,
     .        NOM_OPT(LNOPT1*INOM_OPT(2)+1), NB_TOTAL_NODE)
      ELSE
         CALL FVMESH0(T_MONVOL, X,  IXS, IXC, IXTG, PM,
     .        IPM, ITAB,  IGRSURF, XYZREF  ,
     .        NOM_OPT(LNOPT1*INOM_OPT(2)+1), NUMNOD)
      ENDIF

      CALL COPY_TO_VOLMON(T_MONVOL, LRCBAG, T_MONVOL_METADATA%RCBAG, SVOLMON, VOLMON)

      CALL COPY_TO_MONVOL(T_MONVOL, LICBAG, T_MONVOL_METADATA%ICBAG, SMONVOL, MONVOL)

      CALL TRACE_OUT1()
      ERR_MSG='BEM FLOW'
      ERR_CATEGORY='BEM FLOW'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C----------------------------------
C     LECTURE DES ECOULEMENTS (FLUIDE INCOMPRESSIBLE OU THERMIQUE)
C----------------------------------
      IF(NFLOW>0) THEN
         CALL HM_PREREAD_BEM(IGRSURF, IGRNOD , NNFT  ,
     .                 UNITAB , NOM_OPT(LNOPT1*INOM_OPT(12)+1), LSUBMODEL)
C
         ALLOCATE(IFLOW(LIFLOW), RFLOW(LRFLOW))
         IFLOW(1:LIFLOW) = 0
         RFLOW(1:LRFLOW) = ZERO
C
         DO I=1,NSPMD
            MEMFLOW(1,I)=0
            MEMFLOW(2,I)=0
         ENDDO
C
         CALL HM_READ_BEM(IGRSURF, IFLOW,   RFLOW,
     .                 NPC1   , IGRNOD , MEMFLOW(1,1),UNITAB,
     .                 X, NOM_OPT(LNOPT1*INOM_OPT(12)+1),LGAUGE, IGRV, LSUBMODEL)
C
      ELSE
         ALLOCATE(IFLOW(0), RFLOW(0))
      ENDIF
      CALL TRACE_OUT1()
      ERR_MSG='EULERIAN BOUNDARY CONDITIONS'
      ERR_CATEGORY='EULERIAN BOUNDARY CONDITIONS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c
      IF(NEBCS > 0)THEN
         SEGINDX = 0
         !update due to domain decomposition
         CALL INIEBCS(ALE_CONNECTIVITY, 1,IGRSURF, IXS, IXQ, IXTG,
     .        PM, IGEO, X, SENSORS, MONVOl, MULTI_FVM%IS_USED, EBCS_TAB, EBCS_TAG_CELL_SPMD)
         !initialization
        CALL INIEBCSP0(X, IPARG, ELBUF_TAB, EBCS_TAB, IXS, IXQ, IXTG, IPARTS, IPARTQ, IPARTG, PM, IPM)
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
C     MULTIPLICATEURS DE LAGRANGE
C--------------------------------------------
      ERR_MSG='LAGRANGE MULTIPLIERS'
      ERR_CATEGORY='LAGRANGE MULTIPLIERS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      NCMAX   = LAG_NCF + LAG_NCL
      NKMAX   = LAG_NKF + LAG_NKL
      LAG_NHL = LAG_NCL * 10

      IF( ALLOCATED(IADHF) )  DEALLOCATE(IADHF) 
      IF( ALLOCATED(IADLL) )  DEALLOCATE(IADLL) 
      IF( ALLOCATED(LLL)   )  DEALLOCATE(LLL)  
      IF( ALLOCATED(JLL)   )  DEALLOCATE(JLL)   
      ALLOCATE(IADHF(LAG_NCF + 1))
      ALLOCATE(IADLL(LAG_NCF + 1))
      ALLOCATE(LLL(LAG_NKF))
      ALLOCATE(JLL(LAG_NKF))
      IF(LAG_NCF > 0) THEN
C---
        CALL LAGM_INI(LAG_NHF ,IADHF   ,IADLL   ,JLL     ,LLL     ,
     2                IPARI   ,INTBUF_TAB,IGRNOD,         IBCSLAG ,
     3                MS      ,IN      ,GJBUFI  ,IBMPC   ,IBMPC2  ,
     4                IBMPC3  ,IBMPC4  ,IBFV    ,VEL     ,ITAB    ,
     5                NOM_OPT,INOM_OPT(3),INOM_OPT(15),INOM_OPT(16),
     6                INOM_OPT(17),INOM_OPT(18))
        ALLOCATE(JCIHF(LAG_NHF),  STAT=stat)
        CALL LAGM_NHF(LAG_NCF, IADLL  ,JLL    ,LLL    ,JCIHF  )
C---
        SLAGBUF = LAG_NHF + 3*LAG_NCF+2
        ALLOCATE(LAGBUF(SLAGBUF),  STAT=stat)
        L1 = LAG_NCF + 1
        L2 = L1 + LAG_NHF
        L3 = L2 + LAG_NCF + 1
        L4 = L3 + LAG_NKF
        L5 = L4 + LAG_NKF
        LAGBUF = 0
        LAGBUF(1:L1)    = IADHF(1:LAG_NCF + 1)
        LAGBUF(L1+1:L2) = JCIHF(1:LAG_NHF)
        DEALLOCATE(JLL)
        DEALLOCATE(IADHF)
        DEALLOCATE(JCIHF)
        ELSE
        SLAGBUF = 0
        ALLOCATE(LAGBUF(SLAGBUF))
      ENDIF
C---
      IF(NRWLAG>0)
     .  CALL LGMINI_RWL(NPRW   , LPRW     , MS     , ITAB,
     .                  NOM_OPT(LNOPT1*INOM_OPT(5)+1))
      IF(NINTER>0)
     .  CALL LGMINI_I7(IPARI   ,INTBUF_TAB   , MS     , ITAB   , IGRNOD,
     .                 NOM_OPT(LNOPT1*INOM_OPT(3)+1))
      NHMAX = LAG_NHF + LAG_NHL
      LWAT  = 0
      IF(LAG_NCL/=0) LWAT  = MAX(6*(NUMELS16+NUMELS20),6*NUMNOD)
      L_MUL_LAG1 = 2*NCMAX + 4*NKMAX + LWAT + NUMNOD + 2
      IF(NCMAX>0) THEN
          L_MUL_LAG = MAX(L_MUL_LAG1,
     .                11*NCMAX + 4*NKMAX + 3*NHMAX + 6*NUMNOD + 2)
      ENDIF
      SLAMBDA = NCMAX
      ALLOCATE(LAMBDA(SLAMBDA),  STAT=stat)
      IF(SLAMBDA > 0)  LAMBDA = ZERO
      CALL TRACE_OUT1()
C----------------------------------
C     CALCUL FORCES GRAVITE MODALES
C----------------------------------
      ERR_MSG='GRAVITY NODAL FORCES'
      ERR_CATEGORY='GRAVITY NODAL FORCES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NFXBODY>0) THEN
         DO NFX=1,NFXBODY
            AIPM=(NFX-1)*NBIPM
            ANOD=FXBIPM(AIPM+6)
            NLGRAV=FXBIPM(AIPM+25)
            AGRVI=FXBIPM(AIPM+26)
            AGRVR=FXBIPM(AIPM+27)
            AMOD=FXBIPM(AIPM+7)
            IF(NLGRAV>0)
     .         CALL FXBGRAV(
     . IGRV   , LGRAV  , FXBIPM(AIPM+18), FXBNOD(ANOD),
     . FXBGRVI(AGRVI), FXBGRVR(AGRVR), FXBIPM(AIPM+3), FXBMOD(AMOD),
     . FXBIPM(AIPM+4), FXBIPM(AIPM+17), MS     , GRAV   ,
     . SKEW          , FXBIPM(AIPM+29), NFX    , FXBIPM(AIPM+30))
         ENDDO
      ENDIF
      CALL TRACE_OUT1()

C--------------------------------------------
C     THPARTS TREADING
C--------------------------------------------
      ERR_MSG='TIME HISTORY PARTS'
      ERR_CATEGORY='TIME HISTORY'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NTHPART >0) THEN
        CALL HM_READ_THPART(IPART    ,IGRBRIC ,IGRQUAD  ,IGRSH4N ,IGRSH3N,
     .                      IGRTRUSS ,IGRBEAM ,IGRSPRING, LSUBMODEL)
      ENDIF
      CALL TRACE_OUT1()

      CALL CREATE_MAP_TABLES ( MAP_TABLES ,2     ,
     *                         LSUBMODEL ,SUBSETS,
     *                         IPART,
     *                         IXS  ,IXQ  ,IXC   ,IXTG ,
     *                         IXT  ,IXP  ,IXR   ,KXSP,LRIVET,
     *                         IBID )
C----------------------------------
C     TH GROUP READING
C----------------------------------
      ERR_MSG='TIME HISTORY GROUPS'
      ERR_CATEGORY='TIME HISTORY'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
c
      ! Number of /TH read by hm reader
      CALL HM_OPTION_COUNT('/TH' ,NTHGRP0)
      CALL HM_OPTION_COUNT('/ATH',NTHGRP01(1))
      CALL HM_OPTION_COUNT('/BTH',NTHGRP01(2))
      CALL HM_OPTION_COUNT('/CTH',NTHGRP01(3))
      CALL HM_OPTION_COUNT('/DTH',NTHGRP01(4))
      CALL HM_OPTION_COUNT('/ETH',NTHGRP01(5))
      CALL HM_OPTION_COUNT('/FTH',NTHGRP01(6))
      CALL HM_OPTION_COUNT('/GTH',NTHGRP01(7))
      CALL HM_OPTION_COUNT('/HTH',NTHGRP01(8))
      CALL HM_OPTION_COUNT('/ITH',NTHGRP01(9))
      DO I=1,9
        NTHGRPMX = MAX(NTHGRP0,NTHGRP01(I))
      ENDDO
c
      ! Number of /TH/MONV
      NBR_TH_MONVOL        = 0
      NBR_TH_MONVOL01(1:9) = 0
      CALL HM_OPTION_COUNT('/TH/MONV' ,NBR_TH_MONVOL)
      CALL HM_OPTION_COUNT('/ATH/MONV',NBR_TH_MONVOL01(1))
      CALL HM_OPTION_COUNT('/BTH/MONV',NBR_TH_MONVOL01(2))
      CALL HM_OPTION_COUNT('/CTH/MONV',NBR_TH_MONVOL01(3))
      CALL HM_OPTION_COUNT('/DTH/MONV',NBR_TH_MONVOL01(4))
      CALL HM_OPTION_COUNT('/ETH/MONV',NBR_TH_MONVOL01(5))
      CALL HM_OPTION_COUNT('/FTH/MONV',NBR_TH_MONVOL01(6))
      CALL HM_OPTION_COUNT('/GTH/MONV',NBR_TH_MONVOL01(7))
      CALL HM_OPTION_COUNT('/HTH/MONV',NBR_TH_MONVOL01(8))
      CALL HM_OPTION_COUNT('/ITH/MONV',NBR_TH_MONVOL01(9))
      DO I=1,9
        NBR_TH_MONVOL = MAX(NBR_TH_MONVOL,NBR_TH_MONVOL01(I))
      ENDDO
c
      OUTPUT%TH%SITHGRP = (NTHGRP0+NBR_TH_MONVOL)*NITHGR
      LITHPART  = NTHGRPMX*(NPART+NTHPART)
      LITHSUB   = NTHGRPMX*NSUBS
      LITHBUFMX = 0
      LITHBUFI  = 0
      NVARTOT   = 0
      NVARTOT0  = 0
c
      ! New routine to pre-read /TH with hm_reader and old reader
      ! (needed to estimate sizes of buffers)
      CALL HM_READ_PRETHGROU(LITHBUFMX,NVARTOT0,LSUBMODEL,0)
      DO I=1,9
        CALL HM_READ_PRETHGROU(LITHBUFI,NVARTOT,LSUBMODEL,I)
        LITHBUFMX  = MAX(LITHBUFMX,LITHBUFI,NVARTOT,NVARTOT0)
        NVARTOTMAX = MAX(NVARTOTMAX,NVARTOT,NVARTOT0)
      ENDDO
c
      SITHVAR = NVARTOT0*10+NVARTOT*10+NVOLU*10
      ALLOCATE(ITHPART(LITHPART)   ,  STAT=stat)
      ALLOCATE(ITHSUB(LITHSUB)     ,  STAT=stat)
      ALLOCATE(ITHBUFTMP(LITHBUFMX),  STAT=stat)
      ALLOCATE(ITHVAR(SITHVAR)     ,  STAT=stat)
      IF(SITHVAR > 0) ITHVAR(1:SITHVAR) = 0
      CALL MY_ALLOC(OUTPUT%TH%ITHGRP,OUTPUT%TH%SITHGRP)
c
      OUTPUT%TH%ITHGRP(1:OUTPUT%TH%SITHGRP)    = 0
      ITHPART   = 0
      ITHSUB    = 0
      ITHBUFTMP = 0
      OUTPUT%TH%SITHBUF = 0
      ITHFLAG   = 10
c-----
      INTERFACES%PARAMETERS%INTCAREA =0
      ! New routine to read /TH with hm_reader and old reader
      CALL HM_READ_THGROU(
     1       OUTPUT%TH%ITHGRP   ,ITHBUFTMP,ITAB     ,ITABM1   ,IXTG     ,
     2       IXS      ,IXQ      ,IXC      ,IXT      ,IXP     ,IXR      ,
     3       KXX      ,IXX      ,IPART    ,OUTPUT%TH%SITHBUF,
     4       NTHWA    ,KXSP     ,LRIVET   ,ISKWN    ,IFRAME  ,
     5       NTHGRP   ,ITHPART  ,ITHSUB   ,FXBIPM   ,IPART   ,LIPART1  ,
     6       8        ,12       ,IMERGE   ,ITHVAR   ,
     7       1        ,NVARABF  ,NOM_OPT  ,INOM_OPT(11),INOM_OPT(3),
     8       INOM_OPT(5),INOM_OPT(8),INOM_OPT(7),
     9       INOM_OPT(2),INOM_OPT(1),INOM_OPT(10),INOM_OPT(27),
     A       INOM_OPT(28),INOM_OPT(22),ISPHIO,SRTHBUF,T_MONVOL           ,
     B       IGRSURF    ,SUBSETS  ,ITHFLAG,NPBY     ,LSUBMODEL, IPARG  ,
     C       IPARTS   ,IPARTQ   ,IPARTC   ,IPARTT   ,IPARTP  ,IPARTR   ,
     D       IPARTG   ,IPARTX   ,IPARTSP  ,IPARTIG3D,LITHBUFMX,
     E       MAP_TABLES, 0,INOM_OPT(31),INOM_OPT(32),SENSORS   ,
     F       INTERFACES,IPARI   )
C
      CALL MY_ALLOC(OUTPUT%TH%ITHBUF,OUTPUT%TH%SITHBUF)
      OUTPUT%TH%ITHBUF(1:OUTPUT%TH%SITHBUF) = ITHBUFTMP(1:OUTPUT%TH%SITHBUF)

      If (LITHBUFMX < OUTPUT%TH%SITHBUF) then
        print*,'Allocation error :LITHBUFMX, SITHBUF=',LITHBUFMX,OUTPUT%TH%SITHBUF
      endif
      ALLOCATE(RTHBUF(SRTHBUF),  STAT=stat)
      IF(SRTHBUF > 0) CALL THSKEWC(
     1       RTHBUF   ,OUTPUT%TH%ITHGRP ,OUTPUT%TH%ITHBUF,X     ,IXC     ,IXTG   ,SKEW,NTHGRP)

C--------
C ithgrpa
C--------
      IF(NTHGRP01(1) > 0) THEN
       OUTPUT%TH%SITHGRPA = (NTHGRP01(1)+NBR_TH_MONVOL)*NITHGR
       CALL MY_ALLOC(OUTPUT%TH%ITHGRPA,OUTPUT%TH%SITHGRPA)
       OUTPUT%TH%ITHGRPA   = 0
       ITHPART   = 0
       ITHSUB    = 0
       ITHBUFTMP = 0
       OUTPUT%TH%SITHBUFA  = 0
       ITHFLAG   = 1
c-----
        IF(NPART+NTHPART>0) THEN
        IPARTTHI=>IPARTTH(1:2*(NPART+NTHPART))
        ELSE
          IPARTTHI=>IPARTTH
        END IF
        CALL HM_READ_THGROU(
     1       OUTPUT%TH%ITHGRPA  ,ITHBUFTMP,ITAB     ,ITABM1   ,IXTG     ,
     2       IXS      ,IXQ      ,IXC      ,IXT      ,IXP     ,IXR      ,
     3       KXX      ,IXX      ,IPART    ,OUTPUT%TH%SITHBUFA ,
     4       NTHWA    ,KXSP     ,LRIVET   ,ISKWN    ,IFRAME  ,
     5       NTHGRP1(1),ITHPART ,ITHSUB   ,FXBIPM   ,IPARTTHI,2        ,
     6       1        ,1        ,IMERGE   ,ITHVAR   ,
     7       1        ,NVARABF  ,NOM_OPT  ,INOM_OPT(11),INOM_OPT(3),
     8       INOM_OPT(5),INOM_OPT(8),INOM_OPT(7),
     9       INOM_OPT(2),INOM_OPT(1),INOM_OPT(10),INOM_OPT(27),
     A       INOM_OPT(28),INOM_OPT(22),ISPHIO,SRTHBUF,T_MONVOL           ,
     B       IGRSURF    ,SUBSETS  ,ITHFLAG,NPBY     ,LSUBMODEL, IPARG  ,
     C       IPARTS   ,IPARTQ   ,IPARTC   ,IPARTT   ,IPARTP  ,IPARTR   ,
     D       IPARTG   ,IPARTX   ,IPARTSP  ,IPARTIG3D,LITHBUFMX,
     E       MAP_TABLES, 1,INOM_OPT(31),INOM_OPT(32),SENSORS,
     F       INTERFACES,IPARI   )
C
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFA,OUTPUT%TH%SITHBUFA)
        OUTPUT%TH%ITHBUFA = ITHBUFTMP(1:OUTPUT%TH%SITHBUFA)
      ELSE
            OUTPUT%TH%SITHGRPA = 0
            OUTPUT%TH%SITHBUFA = 0
            CALL MY_ALLOC(OUTPUT%TH%ITHGRPA,OUTPUT%TH%SITHGRPA)
            CALL MY_ALLOC(OUTPUT%TH%ITHBUFA,OUTPUT%TH%SITHBUFA)
      ENDIF
C--------
C ithgrpb
C--------
      IF(NTHGRP01(2) > 0) THEN
        OUTPUT%TH%SITHGRPB = (NTHGRP01(2)+NBR_TH_MONVOL)*NITHGR
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPB,OUTPUT%TH%SITHGRPB)
        OUTPUT%TH%ITHGRPB   = 0
        ITHPART   = 0
        ITHSUB    = 0
        ITHBUFTMP = 0
        OUTPUT%TH%SITHBUFB  = 0
        ITHFLAG   = 2
c-----
        IF(NPART+NTHPART>0) THEN
        IPARTTHI=>IPARTTH(1+2*(NPART+NTHPART):4*(NPART+NTHPART))
        ELSE
          IPARTTHI=>IPARTTH
        END IF
        CALL HM_READ_THGROU(
     1       OUTPUT%TH%ITHGRPB  ,ITHBUFTMP,ITAB     ,ITABM1   ,IXTG     ,
     2       IXS      ,IXQ      ,IXC      ,IXT      ,IXP     ,IXR      ,
     3       KXX      ,IXX      ,IPART    ,OUTPUT%TH%SITHBUFB ,
     4       NTHWA    ,KXSP     ,LRIVET   ,ISKWN    ,IFRAME  ,
     5       NTHGRP1(2),ITHPART ,ITHSUB   ,FXBIPM   ,IPARTTHI,2        ,
     6       1        ,1        ,IMERGE   ,ITHVAR   ,
     7       1        ,NVARABF  ,NOM_OPT  ,INOM_OPT(11),INOM_OPT(3),
     8       INOM_OPT(5),INOM_OPT(8),INOM_OPT(7),
     9       INOM_OPT(2),INOM_OPT(1),INOM_OPT(10),INOM_OPT(27),
     A       INOM_OPT(28),INOM_OPT(22),ISPHIO,SRTHBUF,T_MONVOL           ,
     B       IGRSURF    ,SUBSETS  ,ITHFLAG,NPBY     ,LSUBMODEL, IPARG  ,
     C       IPARTS   ,IPARTQ   ,IPARTC   ,IPARTT   ,IPARTP  ,IPARTR   ,
     D       IPARTG   ,IPARTX   ,IPARTSP  ,IPARTIG3D,LITHBUFMX,
     E       MAP_TABLES, 2,INOM_OPT(31),INOM_OPT(32),SENSORS,
     F       INTERFACES,IPARI   )
C
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFB,OUTPUT%TH%SITHBUFB)
        OUTPUT%TH%ITHBUFB = ITHBUFTMP(1:OUTPUT%TH%SITHBUFB)
      ELSE
            OUTPUT%TH%SITHGRPB = 0
            OUTPUT%TH%SITHBUFB = 0
            CALL MY_ALLOC(OUTPUT%TH%ITHGRPB,OUTPUT%TH%SITHGRPB)
            CALL MY_ALLOC(OUTPUT%TH%ITHBUFB,OUTPUT%TH%SITHBUFB)
      ENDIF
C--------
C ithgrpc
C--------
      IF(NTHGRP01(3) > 0) THEN
        OUTPUT%TH%SITHGRPC = (NTHGRP01(3)+NBR_TH_MONVOL)*NITHGR
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPC,OUTPUT%TH%SITHGRPC)
        OUTPUT%TH%ITHGRPC   = 0
        ITHPART   = 0
        ITHSUB    = 0
        ITHBUFTMP = 0
        OUTPUT%TH%SITHBUFC  = 0
        ITHFLAG   = 3
c-----
        IF(NPART+NTHPART>0) THEN
        IPARTTHI=>IPARTTH(1+4*(NPART+NTHPART):6*(NPART+NTHPART))
        ELSE
          IPARTTHI=>IPARTTH
        END IF
        CALL HM_READ_THGROU(
     1       OUTPUT%TH%ITHGRPC  ,ITHBUFTMP,ITAB     ,ITABM1   ,IXTG     ,
     2       IXS      ,IXQ      ,IXC      ,IXT      ,IXP     ,IXR      ,
     3       KXX      ,IXX      ,IPART    ,OUTPUT%TH%SITHBUFC ,
     4       NTHWA    ,KXSP     ,LRIVET   ,ISKWN    ,IFRAME  ,
     5       NTHGRP1(3),ITHPART ,ITHSUB   ,FXBIPM   ,IPARTTHI,2        ,
     6       1        ,1        ,IMERGE   ,ITHVAR   ,
     7       1        ,NVARABF  ,NOM_OPT  ,INOM_OPT(11),INOM_OPT(3),
     8       INOM_OPT(5),INOM_OPT(8),INOM_OPT(7),
     9       INOM_OPT(2),INOM_OPT(1),INOM_OPT(10),INOM_OPT(27),
     A       INOM_OPT(28),INOM_OPT(22),ISPHIO,SRTHBUF,T_MONVOL           ,
     B       IGRSURF    ,SUBSETS  ,ITHFLAG,NPBY     ,LSUBMODEL, IPARG  ,
     C       IPARTS   ,IPARTQ   ,IPARTC   ,IPARTT   ,IPARTP  ,IPARTR   ,
     D       IPARTG   ,IPARTX   ,IPARTSP  ,IPARTIG3D,LITHBUFMX,
     E       MAP_TABLES, 3,INOM_OPT(31),INOM_OPT(32),SENSORS,
     F       INTERFACES,IPARI   )
C
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFC, OUTPUT%TH%SITHBUFC)
        OUTPUT%TH%ITHBUFC(1:OUTPUT%TH%SITHBUFC) = ITHBUFTMP(1:OUTPUT%TH%SITHBUFC)
      ELSE
        OUTPUT%TH%SITHGRPC = 0
        OUTPUT%TH%SITHBUFC = 0
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFC, OUTPUT%TH%SITHBUFC)
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPC, OUTPUT%TH%SITHGRPC)
      ENDIF
C--------
C ithgrpd
C--------
      IF(NTHGRP01(4) > 0) THEN
        OUTPUT%TH%SITHGRPD = (NTHGRP01(4)+NBR_TH_MONVOL)*NITHGR
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPD,OUTPUT%TH%SITHGRPD)
        OUTPUT%TH%ITHGRPD   = 0
        ITHPART   = 0
        ITHSUB    = 0
        ITHBUFTMP = 0
        OUTPUT%TH%SITHBUFD  = 0
        ITHFLAG   = 4
c-----
        IF(NPART+NTHPART>0) THEN
        IPARTTHI=>IPARTTH(1+6*(NPART+NTHPART):8*(NPART+NTHPART))
        ELSE
          IPARTTHI=>IPARTTH
        END IF
        CALL HM_READ_THGROU(
     1       OUTPUT%TH%ITHGRPD  ,ITHBUFTMP,ITAB     ,ITABM1   ,IXTG     ,
     2       IXS      ,IXQ      ,IXC      ,IXT      ,IXP     ,IXR      ,
     3       KXX      ,IXX      ,IPART    ,OUTPUT%TH%SITHBUFD ,
     4       NTHWA    ,KXSP     ,LRIVET   ,ISKWN    ,IFRAME  ,
     5       NTHGRP1(4),ITHPART ,ITHSUB   ,FXBIPM   ,IPARTTHI,2        ,
     6       1        ,1        ,IMERGE   ,ITHVAR   ,
     7       1        ,NVARABF  ,NOM_OPT  ,INOM_OPT(11),INOM_OPT(3),
     8       INOM_OPT(5),INOM_OPT(8),INOM_OPT(7),
     9       INOM_OPT(2),INOM_OPT(1),INOM_OPT(10),INOM_OPT(27),
     A       INOM_OPT(28),INOM_OPT(22),ISPHIO,SRTHBUF,T_MONVOL           ,
     B       IGRSURF    ,SUBSETS  ,ITHFLAG,NPBY     ,LSUBMODEL, IPARG  ,
     C       IPARTS   ,IPARTQ   ,IPARTC   ,IPARTT   ,IPARTP  ,IPARTR   ,
     D       IPARTG   ,IPARTX   ,IPARTSP  ,IPARTIG3D,LITHBUFMX,
     E       MAP_TABLES, 4,INOM_OPT(31),INOM_OPT(32),SENSORS,
     F       INTERFACES,IPARI   )
C
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFD,OUTPUT%TH%SITHBUFD)
        OUTPUT%TH%ITHBUFD(1:OUTPUT%TH%SITHBUFD) = ITHBUFTMP(1:OUTPUT%TH%SITHBUFD)
      ELSE
        OUTPUT%TH%SITHGRPD = 0
        OUTPUT%TH%SITHBUFD = 0
        ALLOCATE(OUTPUT%TH%ITHBUFD(OUTPUT%TH%SITHBUFD),  STAT=stat)
        ALLOCATE(OUTPUT%TH%ITHGRPD(OUTPUT%TH%SITHGRPD),  STAT=stat)
      ENDIF
C--------
C ithgrpe
C--------
      IF(NTHGRP01(5) > 0) THEN
        OUTPUT%TH%SITHGRPE = (NTHGRP01(5)+NBR_TH_MONVOL)*NITHGR
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPE,OUTPUT%TH%SITHGRPE)
        OUTPUT%TH%ITHGRPE   = 0
        ITHPART   = 0
        ITHSUB    = 0
        ITHBUFTMP = 0
        OUTPUT%TH%SITHBUFE  = 0
        ITHFLAG   = 5
c-----
        IF(NPART+NTHPART>0) THEN
        IPARTTHI=>IPARTTH(1+8*(NPART+NTHPART):10*(NPART+NTHPART))
        ELSE
          IPARTTHI=>IPARTTH
        END IF
c-----
        CALL HM_READ_THGROU(
     1       OUTPUT%TH%ITHGRPE  ,ITHBUFTMP,ITAB     ,ITABM1   ,IXTG     ,
     2       IXS      ,IXQ      ,IXC      ,IXT      ,IXP     ,IXR      ,
     3       KXX      ,IXX      ,IPART    ,OUTPUT%TH%SITHBUFE ,
     4       NTHWA    ,KXSP     ,LRIVET   ,ISKWN    ,IFRAME  ,
     5       NTHGRP1(5),ITHPART ,ITHSUB   ,FXBIPM   ,IPARTTHI,2        ,
     6       1        ,1        ,IMERGE   ,ITHVAR   ,
     7       1        ,NVARABF  ,NOM_OPT  ,INOM_OPT(11),INOM_OPT(3),
     8       INOM_OPT(5),INOM_OPT(8),INOM_OPT(7),
     9       INOM_OPT(2),INOM_OPT(1),INOM_OPT(10),INOM_OPT(27),
     A       INOM_OPT(28),INOM_OPT(22),ISPHIO,SRTHBUF,T_MONVOL           ,
     B       IGRSURF    ,SUBSETS  ,ITHFLAG,NPBY     ,LSUBMODEL, IPARG  ,
     C       IPARTS   ,IPARTQ   ,IPARTC   ,IPARTT   ,IPARTP  ,IPARTR   ,
     D       IPARTG   ,IPARTX   ,IPARTSP  ,IPARTIG3D,LITHBUFMX,
     E       MAP_TABLES, 5,INOM_OPT(31),INOM_OPT(32),SENSORS,
     F       INTERFACES,IPARI   )
C
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFE,OUTPUT%TH%SITHBUFE)
        OUTPUT%TH%ITHBUFE = ITHBUFTMP(1:OUTPUT%TH%SITHBUFE)
      ELSE
        OUTPUT%TH%SITHGRPE = 0
        OUTPUT%TH%SITHBUFE = 0
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFE,OUTPUT%TH%SITHBUFE)
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPE,OUTPUT%TH%SITHGRPE)
      ENDIF
C--------
C ithgrpf
C--------
      IF(NTHGRP01(6) > 0) THEN
        OUTPUT%TH%SITHGRPF = (NTHGRP01(6)+NBR_TH_MONVOL)*NITHGR
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPF,OUTPUT%TH%SITHGRPF)
        OUTPUT%TH%ITHGRPF   = 0
        ITHPART   = 0
        ITHSUB    = 0
        ITHBUFTMP = 0
        OUTPUT%TH%SITHBUFF  = 0
        ITHFLAG   = 6
c-----
        IF(NPART+NTHPART>0) THEN
        IPARTTHI=>IPARTTH(1+10*(NPART+NTHPART):12*(NPART+NTHPART))
        ELSE
          IPARTTHI=>IPARTTH
        END IF
        CALL HM_READ_THGROU(
     1       OUTPUT%TH%ITHGRPF  ,ITHBUFTMP,ITAB     ,ITABM1   ,IXTG     ,
     2       IXS      ,IXQ      ,IXC      ,IXT      ,IXP     ,IXR      ,
     3       KXX      ,IXX      ,IPART    ,OUTPUT%TH%SITHBUFF ,
     4       NTHWA    ,KXSP     ,LRIVET   ,ISKWN    ,IFRAME  ,
     5       NTHGRP1(6),ITHPART ,ITHSUB   ,FXBIPM   ,IPARTTHI,2        ,
     6       1        ,1        ,IMERGE   ,ITHVAR   ,
     7       1        ,NVARABF  ,NOM_OPT  ,INOM_OPT(11),INOM_OPT(3),
     8       INOM_OPT(5),INOM_OPT(8),INOM_OPT(7),
     9       INOM_OPT(2),INOM_OPT(1),INOM_OPT(10),INOM_OPT(27),
     A       INOM_OPT(28),INOM_OPT(22),ISPHIO,SRTHBUF,T_MONVOL           ,
     B       IGRSURF    ,SUBSETS  ,ITHFLAG,NPBY     ,LSUBMODEL, IPARG  ,
     C       IPARTS   ,IPARTQ   ,IPARTC   ,IPARTT   ,IPARTP  ,IPARTR   ,
     D       IPARTG   ,IPARTX   ,IPARTSP  ,IPARTIG3D,LITHBUFMX,
     E       MAP_TABLES, 6,INOM_OPT(31),INOM_OPT(32),SENSORS,
     F       INTERFACES,IPARI   )
C
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFF,OUTPUT%TH%SITHBUFF)
        OUTPUT%TH%ITHBUFF(1:OUTPUT%TH%SITHBUFF) = ITHBUFTMP(1:OUTPUT%TH%SITHBUFF)
      ELSE
        OUTPUT%TH%SITHGRPF = 0
        OUTPUT%TH%SITHBUFF = 0
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFF,OUTPUT%TH%SITHBUFF)
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPF,OUTPUT%TH%SITHGRPF)
      ENDIF
C--------
C ithgrpg
C--------
      IF(NTHGRP01(7) > 0) THEN
        OUTPUT%TH%SITHGRPG = (NTHGRP01(7)+NBR_TH_MONVOL)*NITHGR
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPG,OUTPUT%TH%SITHGRPG)
        OUTPUT%TH%ITHGRPG = 0
        OUTPUT%TH%SITHBUFG = 0
        ITHPART   = 0
        ITHSUB    = 0
        ITHBUFTMP = 0
        ITHFLAG   = 7
c-----
        IF(NPART+NTHPART>0) THEN
        IPARTTHI=>IPARTTH(1+12*(NPART+NTHPART):14*(NPART+NTHPART))
        ELSE
          IPARTTHI=>IPARTTH
        END IF
        CALL HM_READ_THGROU(
     1       OUTPUT%TH%ITHGRPG  ,ITHBUFTMP,ITAB     ,ITABM1   ,IXTG     ,
     2       IXS      ,IXQ      ,IXC      ,IXT      ,IXP     ,IXR      ,
     3       KXX      ,IXX      ,IPART    ,OUTPUT%TH%SITHBUFG ,
     4       NTHWA    ,KXSP     ,LRIVET   ,ISKWN    ,IFRAME  ,
     5       NTHGRP1(7),ITHPART ,ITHSUB   ,FXBIPM   ,IPARTTHI,2        ,
     6       1        ,1        ,IMERGE   ,ITHVAR   ,
     7       1        ,NVARABF  ,NOM_OPT  ,INOM_OPT(11),INOM_OPT(3),
     8       INOM_OPT(5),INOM_OPT(8),INOM_OPT(7),
     9       INOM_OPT(2),INOM_OPT(1),INOM_OPT(10),INOM_OPT(27),
     A       INOM_OPT(28),INOM_OPT(22),ISPHIO,SRTHBUF,T_MONVOL           ,
     B       IGRSURF    ,SUBSETS  ,ITHFLAG,NPBY     ,LSUBMODEL, IPARG  ,
     C       IPARTS   ,IPARTQ   ,IPARTC   ,IPARTT   ,IPARTP  ,IPARTR   ,
     D       IPARTG   ,IPARTX   ,IPARTSP  ,IPARTIG3D,LITHBUFMX,
     E       MAP_TABLES, 7,INOM_OPT(31),INOM_OPT(32),SENSORS,
     F       INTERFACES,IPARI   )
C
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFG,OUTPUT%TH%SITHBUFG)
        OUTPUT%TH%ITHBUFG(1:OUTPUT%TH%SITHBUFG) = ITHBUFTMP(1:OUTPUT%TH%SITHBUFG)
      ELSE
        OUTPUT%TH%SITHGRPG = 0
        OUTPUT%TH%SITHBUFG = 0
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFG,OUTPUT%TH%SITHBUFG)
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPG,OUTPUT%TH%SITHGRPG)
      ENDIF
C--------
C ithgrph
C--------
      IF(NTHGRP01(8) > 0) THEN
        OUTPUT%TH%SITHGRPH = (NTHGRP01(8)+NBR_TH_MONVOL)*NITHGR
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPH,OUTPUT%TH%SITHGRPH)
        OUTPUT%TH%ITHGRPH   = 0
        ITHPART   = 0
        ITHSUB    = 0
        ITHBUFTMP = 0
        OUTPUT%TH%SITHBUFH  = 0
        ITHFLAG   = 8
c-----
        IF(NPART+NTHPART>0) THEN
        IPARTTHI=>IPARTTH(1+14*(NPART+NTHPART):16*(NPART+NTHPART))
        ELSE
          IPARTTHI=>IPARTTH
        END IF
        CALL HM_READ_THGROU(
     1       OUTPUT%TH%ITHGRPH  ,ITHBUFTMP,ITAB     ,ITABM1   ,IXTG     ,
     2       IXS      ,IXQ      ,IXC      ,IXT      ,IXP     ,IXR      ,
     3       KXX      ,IXX      ,IPART    ,OUTPUT%TH%SITHBUFH ,
     4       NTHWA    ,KXSP     ,LRIVET   ,ISKWN    ,IFRAME  ,
     5       NTHGRP1(8),ITHPART ,ITHSUB   ,FXBIPM   ,IPARTTHI,2        ,
     6       1        ,1        ,IMERGE   ,ITHVAR   ,
     7       1        ,NVARABF  ,NOM_OPT  ,INOM_OPT(11),INOM_OPT(3),
     8       INOM_OPT(5),INOM_OPT(8),INOM_OPT(7),
     9       INOM_OPT(2),INOM_OPT(1),INOM_OPT(10),INOM_OPT(27),
     A       INOM_OPT(28),INOM_OPT(22),ISPHIO,SRTHBUF,T_MONVOL           ,
     B       IGRSURF    ,SUBSETS  ,ITHFLAG,NPBY     ,LSUBMODEL, IPARG  ,
     C       IPARTS   ,IPARTQ   ,IPARTC   ,IPARTT   ,IPARTP  ,IPARTR   ,
     D       IPARTG   ,IPARTX   ,IPARTSP  ,IPARTIG3D,LITHBUFMX,
     E       MAP_TABLES, 8,INOM_OPT(31),INOM_OPT(32),SENSORS,
     F       INTERFACES,IPARI   )
C
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFH,OUTPUT%TH%SITHBUFH)
        OUTPUT%TH%ITHBUFH(1:OUTPUT%TH%SITHBUFH) = ITHBUFTMP(1:OUTPUT%TH%SITHBUFH)
      ELSE
        OUTPUT%TH%SITHGRPH = 0
        OUTPUT%TH%SITHBUFH = 0
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFH,OUTPUT%TH%SITHBUFH)
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPH,OUTPUT%TH%SITHGRPH)
      ENDIF
C--------
C ithgrpi
C--------
      IF(NTHGRP01(9) > 0) THEN
        OUTPUT%TH%SITHGRPI = (NTHGRP01(9)+NBR_TH_MONVOL)*NITHGR
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPI,OUTPUT%TH%SITHGRPI)
        OUTPUT%TH%ITHGRPI   = 0
        ITHPART   = 0
        ITHSUB    = 0
        ITHBUFTMP = 0
        OUTPUT%TH%SITHBUFI  = 0
        ITHFLAG   = 9
c-----
        IF(NPART+NTHPART>0) THEN
        IPARTTHI=>IPARTTH(1+16*(NPART+NTHPART):18*(NPART+NTHPART))
        ELSE
          IPARTTHI=>IPARTTH
        END IF
        CALL HM_READ_THGROU(
     1       OUTPUT%TH%ITHGRPI  ,ITHBUFTMP,ITAB     ,ITABM1   ,IXTG     ,
     2       IXS      ,IXQ      ,IXC      ,IXT      ,IXP     ,IXR      ,
     3       KXX      ,IXX      ,IPART    ,OUTPUT%TH%SITHBUFI ,
     4       NTHWA    ,KXSP     ,LRIVET   ,ISKWN    ,IFRAME  ,
     5       NTHGRP1(9),ITHPART ,ITHSUB   ,FXBIPM   ,IPARTTHI,2        ,
     6       1        ,1        ,IMERGE   ,ITHVAR   ,
     7       1        ,NVARABF  ,NOM_OPT  ,INOM_OPT(11),INOM_OPT(3),
     8       INOM_OPT(5),INOM_OPT(8),INOM_OPT(7),
     9       INOM_OPT(2),INOM_OPT(1),INOM_OPT(10),INOM_OPT(27),
     A       INOM_OPT(28),INOM_OPT(22),ISPHIO,SRTHBUF,T_MONVOL           ,
     B       IGRSURF    ,SUBSETS  ,ITHFLAG,NPBY     ,LSUBMODEL, IPARG  ,
     C       IPARTS   ,IPARTQ   ,IPARTC   ,IPARTT   ,IPARTP  ,IPARTR   ,
     D       IPARTG   ,IPARTX   ,IPARTSP  ,IPARTIG3D,LITHBUFMX,
     E       MAP_TABLES, 9,INOM_OPT(31),INOM_OPT(32),SENSORS,
     F       INTERFACES,IPARI   )
C
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFI,OUTPUT%TH%SITHBUFI)
        OUTPUT%TH%ITHBUFI = ITHBUFTMP(1:OUTPUT%TH%SITHBUFI)
      ELSE
        OUTPUT%TH%SITHGRPI = 0
        OUTPUT%TH%SITHBUFI = 0
        CALL MY_ALLOC(OUTPUT%TH%ITHBUFI,OUTPUT%TH%SITHBUFI)
        CALL MY_ALLOC(OUTPUT%TH%ITHGRPI,OUTPUT%TH%SITHGRPI)
      ENDIF
C

      IF(ALLOCATED(ITHPART))   DEALLOCATE(ITHPART)
      IF(ALLOCATED(ITHSUB))    DEALLOCATE(ITHSUB)
      IF(ALLOCATED(ITHBUFTMP)) DEALLOCATE(ITHBUFTMP)
C
C -------------------------------------------------
C Check des surfaces pour les sorties TH
C -------------------------------------------------
      CALL CHECK_SURF(IGRSURF,2)
C -------------------------------------------------
C /TH/SURF : outputting Pressure and Area : 
C -------------------------------------------------
      CALL TH_SURF_LOAD_PRESSURE(IGRSURF  , OUTPUT%TH%TH_SURF , IPRES  ,ILOADP  ,LLOADP  ,
     .                           SIZLOADP ,NLOADP             ,SLLOADP ,NIBCLD  ,NPRELD  ,
     .                           NSURF    ,NUMNOD   )
C
C------------------------- CHECK -------------------
C-- La variable NSMAT (nb de Material ds TH),
C     est disponible uniquement apres THGROU en version block
c      CALL ANCHECK(4)
C
c      CALL ANCHECK(8)
C
c      CALL ANCHECK(11)
C
c      CALL ANCHECK(12)
C
c      CALL ANCHECK(16)
C
c      CALL ANCHECK(18)
C
c      CALL ANCHECK(20)
C
c      CALL ANCHECK(24)
C
c      CALL ANCHECK(27)
C
c      CALL ANCHECK(31)
C
c      CALL ANCHECK(34)
C
c      CALL ANCHECK(38)
C
c      CALL ANCHECK(48)
C
c      CALL ANCHECK(45)
C
c      CALL ANCHECK(50)
C
c      CALL ANCHECK(55)
C
c      CALL ANCHECK(57)
C
c      CALL ANCHECK(58)
C
c      CALL ANCHECK(59)
C
c      CALL ANCHECK(61)
C
c      CALL ANCHECK(80)
C
C
C affectation du numero de processeur (spmd)
C
      CALL THPINIT(OUTPUT%TH%ITHGRP,OUTPUT%TH%ITHBUF,IPARG  ,DD_IAD ,LRIVET ,
     .             0       ,NTHGRP )
      IF(NTHGRP01(1) > 0)
     .  CALL THPINIT(OUTPUT%TH%ITHGRPA,OUTPUT%TH%ITHBUFA,IPARG     ,DD_IAD ,LRIVET ,
     .               I      ,NTHGRP1(1)   )
      IF(NTHGRP01(2) > 0)
     .  CALL THPINIT(OUTPUT%TH%ITHGRPB,OUTPUT%TH%ITHBUFB,IPARG     ,DD_IAD ,LRIVET ,
     .               I      ,NTHGRP1(2)   )
      IF(NTHGRP01(3) > 0)
     .  CALL THPINIT(OUTPUT%TH%ITHGRPC,OUTPUT%TH%ITHBUFC,IPARG     ,DD_IAD ,LRIVET ,
     .               I      ,NTHGRP1(3)   )
      IF(NTHGRP01(4) > 0)
     .  CALL THPINIT(OUTPUT%TH%ITHGRPD,OUTPUT%TH%ITHBUFD,IPARG     ,DD_IAD ,LRIVET ,
     .               I      ,NTHGRP1(4)   )
      IF(NTHGRP01(5) > 0)
     .  CALL THPINIT(OUTPUT%TH%ITHGRPE,OUTPUT%TH%ITHBUFE,IPARG     ,DD_IAD ,LRIVET ,
     .               I      ,NTHGRP1(5)   )
      IF(NTHGRP01(6) > 0)
     .  CALL THPINIT(OUTPUT%TH%ITHGRPF,OUTPUT%TH%ITHBUFF,IPARG     ,DD_IAD ,LRIVET ,
     .               I      ,NTHGRP1(6)   )
      IF(NTHGRP01(7) > 0)
     .  CALL THPINIT(OUTPUT%TH%ITHGRPG,OUTPUT%TH%ITHBUFG,IPARG     ,DD_IAD ,LRIVET ,
     .               I      ,NTHGRP1(7)   )
      IF(NTHGRP01(8) > 0)
     .  CALL THPINIT(OUTPUT%TH%ITHGRPH,OUTPUT%TH%ITHBUFH,IPARG     ,DD_IAD ,LRIVET ,
     .               I      ,NTHGRP1(8)   )
      IF(NTHGRP01(9) > 0)
     .  CALL THPINIT(OUTPUT%TH%ITHGRPI,OUTPUT%TH%ITHBUFI,IPARG     ,DD_IAD ,LRIVET ,
     .               I      ,NTHGRP1(9)   )
      CALL TRACE_OUT1()
C--------------------------------------------
C     Multidomains -> deallocation des tableaux.
C--------------------------------------------
      IF(NSUBDOM>0) THEN
        DEALLOCATE(TAG_PART,TAGMON,TAGRBY,TAGINT,TAGCYL,TAGLNK)
        DEALLOCATE(TAGRB3,TAGRB2,TAGJOIN,TAGMPC,TAG_MAT)
      ENDIF
C--------------------------------------------
C     STOCKAGE DYNAMIQUE (CONNEXIONS RIGIDES a MADYMO).
C--------------------------------------------
      ERR_MSG='DYNAMIC STORAGE MADYMO LINK'
      ERR_CATEGORY='DYNAMIC STORAGE MADYMO LINK'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      SRCONX = NCONX*NRCNX
      ALLOCATE(RCONX(SRCONX)    ,STAT=stat)
      RCONX = ZERO
C--------------------------------------------
C     RECALCUL DE NRBODY (NRBYKIN MIS A JOUR EN AMONT)
C---------------------------------------------------------------------
      IF(NRBMERGE > 0) THEN
        NRBODY = NRBYKIN + NRBYLAG
      ENDIF
C--------------------------------------------
C     TABLEAU DE TRAVAIL WA(LENWA)
C     PARTIE NON SAUVEGARDEE SUR LE FICHIER DE RESTART
C--------------------------------------------
      NRCVVOIS0 = 0
C appel a routine generique ici et dans ddsplit
      CALL SETLENWA(
     1      LENWA ,NTHWA  ,NAIRWA    ,NUMELS   ,NUMELQ,
     2      NUMELC,NUMELTG,NUMELT    ,NUMELP   ,NUMELR,
     3      NUMNOD,NMNT   ,L_MUL_LAG1,L_MUL_LAG,MAXNX ,
     4      LWASPH,NUMSPH    ,LWASPIO, NRCVVOIS0 )
C init MULTIMAX
      ALLOCATE(MWA(LENWA)     ,  STAT=stat)
      MWA = ZERO
      CALL SETMULTI(IPARI  )
      CALL TRACE_OUT1()
C---------------------------------------------------------------------
C      INITIALISATION DES INTERFACES DEUXIEME PARTIE
C     INIT INTERFACE TYPE 6 + STATISTIQUES BUCKET SORT TYPE 4
C     MISE DANS FRONTIERE(1) DES NOEUDS DS INTERFACE
C---------------------------------------------------------------------
      ERR_MSG='INTERFACE INITIALIZATION PHASE 2'
      ERR_CATEGORY='INTERFACES'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      I2NSNT = 0
C     Mass and inertia are not modified - a specific array is used
      ALLOCATE(MS_B(NUMNOD),STAT=stat)
      MS_B(1:NUMNOD)=MS(1:NUMNOD)
      IF(IRODDL==1) THEN
        ALLOCATE(IN_B(NUMNOD),STAT=stat)
        IN_B(1:NUMNOD)=IN(1:NUMNOD)
      ELSE
        ALLOCATE(IN_B(1))
      ENDIF
C
      IF(NS10E>0.AND.N2D==0) CALL STIFN0_ND(ICNDS10,STIFFN)
      IF(NINTER > 0) THEN
       CALL ININTR2(IPARI   ,INSCR   ,X       ,
     .               IXS     ,IXQ     ,IXC     ,PM      ,GEO     ,
     .               INSCR   ,ITAB    ,MS      ,NPBY    ,LPBY    ,
     .               MWA     ,D       ,I2NSNT  ,IN      ,
     .               STIFFN,STIFINT ,NOM_OPT(LNOPT1*INOM_OPT(3)+1),INOD_PXFEM ,MS_PLY,
     .               INTBUF_TAB,STIFINTR,ITAGND,ICNDS10,MS_B,IN_B,NSTRF,ITAGCYC,
     .               IRBE2   ,IRBE3  ,LRBE3    ,
     .               KNOD2ELS ,NOD2ELS , IXS10   ,IXS16  ,IXS20,
     .               S_NOD2ELS )    
      ENDIF
      CALL TRACE_OUT1()
C---------------------------------------------
C     Update of STIFFN for TETRA10 for time step estimation
C--------------------------------------------
      IF(NS10E>0.AND.N2D==0) CALL STIFN1_ND(ICNDS10,STIFFN)
      IF(NDAMP>0) CALL DAMPDTNODA(MS_B,IN_B,STIFFN,STIFFN(NUMNOD+1),
     1                            IGRNOD,DAMPR )
C--------------------------------------------
C     TRI ET IMPRESSION DES DT ELEM
C--------------------------------------------
      CALL OUTRI(DTELEM,IXS,IXQ,IXC,IXT,IXP,IXR,IXTG,
     .           KXX,KXSP,KXIG3D,IGEO,NUMEL)
C--------------------------------------------
C     TRI ET IMPRESSION DES DT NODAUX
C--------------------------------------------
      CALL OUTRIN(MS_B,IN_B,STIFFN,STIFFN(NUMNOD+1),ITAB,DTNODA)
C---------------------------------------------
C     Target time step estimation - (type2 effect on nodal time step is taken into account in ININTR2)
C--------------------------------------------
      ERR_MSG='ADDED MASS ESTIMATION'
      ERR_CATEGORY='ADDED MASS ESTIMATION'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(N2D==0) CALL ADD_MASS_STAT(MS_B,IN_B,STIFFN,STIFFN(NUMNOD+1),ITAB,TOTMAS)
C---------------------------------
C     MULTIDOMAINS SPEEDUP ESTIMATION
C---------------------------------
      IF(NSUBDOM>0) THEN
        CALL R2R_SPEEDUP(DTELEM,DTNODA,DT_R2R,COST_R2R,ISOLOFF,
     .                   ISHEOFF,ITRUOFF ,IPOUOFF ,IRESOFF ,ITRIOFF,
     .                   IQUAOFF)
      ENDIF
C--------------------------------------------
      DEALLOCATE(STIFFN)
      DEALLOCATE(STIFINT)
      DEALLOCATE(STIFINTR)
      DEALLOCATE(MS_B)
      DEALLOCATE(IN_B)
      DEALLOCATE(DTELEM)
      CALL TRACE_OUT1()
C--------------------------------------------
C     INI & CHECK RBE3
C--------------------------------------------
      ERR_MSG='RBE3 INITIALIZATION'
      ERR_CATEGORY='RBE3'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(SIRBE3 > 0) THEN
        CALL INIRBE3(IRBE3  ,LRBE3  ,FRBE3  ,SKEW   ,X     ,
     .               MS     ,IN     ,
     .               NOM_OPT(LNOPT1*INOM_OPT(14)+1))
      ENDIF
      CALL TRACE_OUT1()
C--------------------------------------------
      ERR_MSG='KINEMATIC CONDITIONS CHECK'
      ERR_CATEGORY='KINEMATIC CONDITIONS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C--------------------------------------------
C     traitement for 2nd pass /RBODY/RBE2 /BCS /IMPVEL w/ Itet2 of S10
C--------------------------------------------
      IF(NS10E>0) THEN
       CALL RIGMODIF1_ND(NPBY,LPBY,ITAGND)
       CALL RBE2MODIF1_ND(IRBE2,LRBE2,ITAGND)
       CALL BCSMODIF_ND(ICODE, ITAGND,ICNDS10,ITAB)
       CALL FIXMODIF_ND(IBFV , ITAGND,ICNDS10,ITAB)
       CALL BCSCYCMODIF_ND(IBCSCYC,LBCSCYC,ITAGND,ITAB)
      END IF
C--------------------------------------------
C     CHECK DE CONDITIONS CINEMATIQUES
C--------------------------------------------
C D(3,NUMNOD) UTILISE DANS LE STARTER COMME FLAG
C DE CONDITION CINEMATIQUE IKINE(NUMNOD)
C--------------------------------------------
      CALL KINCHK(D       ,RWBUF   ,ITAB   ,NPRW   ,LPRW    ,KINET  ,
     .            NPBY    , LPBY   ,IRBE2  ,LRBE2  ,IRBE3   ,LRBE3  ,
     .            NOM_OPT ,INOM_OPT(5),INOM_OPT(13),INOM_OPT(14)    ,
     .            ITAGCYC )
      IF(NINVEL/=0)
     . CALL INIVCHK(D  ,RWBUF,ITAB,NPRW,LPRW,KINET,
     1              NPBY, LPBY,IRBE2,LRBE2,IRBE3,LRBE3,
     2              FRBE3,X   ,SKEW ,V   ,VR   )
#ifdef DNC
      IF(NEXMAD/=0)
     .  CALL MADCHK(D      ,ITAB   ,ICONX(7*NCONX+1))
#endif
      CALL TRACE_OUT1()
C--------------------------------------------
C     Initial mass
C--------------------------------------------
      ERR_MSG='MASS ARRAY ALLOCATION'
      ERR_CATEGORY='INIIAL MASS'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      ALLOCATE(MS0(NUMNOD)     ,STAT=stat)
      IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                           MSGTYPE=MSGERROR,
     .                         C1='MS0')
      MS0(1:NUMNOD)=MS(1:NUMNOD)
      CALL TRACE_OUT1()
C--------------------------------------------
C     Inlet / Outlet
C--------------------------------------------
c build structure surfaces specific Inlet Outlet
      IF(NSPHIO > 0)THEN
         SIBUFSSG_IO = 4*NSEG_IO
         ALLOCATE(IBUFSSG_IO(SIBUFSSG_IO)    ,STAT=stat)
         IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                              MSGTYPE=MSGERROR,
     .                         C1='IBUFSSG_IO')
         IBUFSSG_IO(1:SIBUFSSG_IO) = ZERO
         CALL SET_IBUFSSG_IO(ISPHIO, IGRSURF, IBUFSSG_IO)
      ELSE
         ALLOCATE(IBUFSSG_IO(1))
         SIBUFSSG_IO = ZERO
      ENDIF

!--------------------------------------------
!     Split the surface & add the MONVOL nodes
!     on a given processor
!--------------------------------------------
      CALL IGRSURF_SPLIT(SCEP,CEP,T_MONVOL,IGRSURF,IGRSURF_PROC)
C--------------------------------------------
C     DOMAIN DECOMPOSITION 2 (DEFINITION DES FRONTIERES)
C--------------------------------------------
      ERR_MSG='DOMAIN DECOMPOSITION PHASE 2'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
        SFR_IAD = (NSPMD+1)*2
        ALLOCATE(FR_IAD(SFR_IAD))
        IF(I2NSNT>0) THEN
          ALLOCATE(CELI2(I2NSNT))
          ALLOCATE(CEPI2(I2NSNT))
          ALLOCATE(ADDCNI2(0:NUMNOD+1))
        ENDIF
        ALLOCATE(ISKWP(NUMSKW+1))
        ALLOCATE(NSKWP(NSPMD))
        ALLOCATE(ISENSP(2*SENSORS%NSENSOR))
        ALLOCATE(NSENSP(NSPMD))
        ALLOCATE(IACCP(NACCELM))
        ALLOCATE(NACCP(NSPMD))
        ALLOCATE(IGAUP(NBGAUGE))
        ALLOCATE(NGAUP(NSPMD))
        ALLOCATE(TAG_SKN(NUMSKW+NSUBMOD+1))
        ALLOCATE(MULTIPLE_SKEW(NUMSKW+1))
        ISKWP(1:NUMSKW+1) = 0
        TAG_SKN(1:NUMSKW+NSUBMOD+1) = 0
        NSKWP(1:NSPMD) = 0
C
        IF(.NOT. ALLOCATED(IBVEL)) ALLOCATE(IBVEL(0)) ! Deprecated option?
        IF(.NOT. ALLOCATED(LBVEL)) ALLOCATE(LBVEL(0)) ! Deprecated option?

        CALL DOMDEC2(
     1    DD_IAD ,IPARI  ,IBCL    ,NPBY      ,
     2    LPBY   ,LRIVET ,IBVEL   ,LBVEL     ,
     3    IPARG  ,CEL     ,IXS    ,IXS10     ,IXS20   ,
     4    IXS16  ,IXQ     ,IXC    ,IXT       ,IXP     ,
     5    IXR    ,IXTG   ,IXTG1     ,T_MONVOL  ,
     6    IGRSURF,ADDCNE ,LCNE      ,GEO     ,
     7    NPRW   ,LPRW    ,LCNI2G ,ADDCNI2   ,CEPI2   ,
     8    CELI2  ,I2NSNT  ,ISKWN  ,ISKWP     ,NSKWP   ,
     9    ISENSP  ,NSENSP ,IACCP  ,NACCP     ,
     A    LACCELM,IBCV    ,IRBE3  ,LRBE3     ,FRONT_RM,
     B    IRBYM  ,LNRBYM  ,CEP    ,IBCR      ,IRBE2   ,
     C    LRBE2  ,CEPSP   ,CELSPH ,ILOADP    ,LLOADP  ,
     D    LGAUGE ,IGAUP   ,NGAUP  ,INTBUF_TAB,IBFFLUX ,
     E    ICNDS10,ITAGND  ,IGEO   ,TAG_SKN   ,MULTIPLE_SKEW,
     F    IBFV   ,IBCSCYC ,LBCSCYC,R_SKEW    ,IPM,
     G    SENSORS,SCEP    ,EBCS_TAB,LOADS,IFRAME)

C      ELSE
C       mise a 1 de front i.e. tous les noeuds sur Proc1, poids 1
C en SMP FRONT => WEIGHT a la meme addresse initialise a 1
C        SFR_IAD = 0
C        SDD_FRONT = 0
C        ALLOCATE(FR_IAD(SFR_IAD))
C        ALLOCATE(DD_FRONT(SDD_FRONT))
C        SWEIGHT = NUMNOD
C        ALLOCATE(WEIGHT(SWEIGHT))
C        WEIGHT = 1
C      ENDIF
C--------------------------------------------
C     traitement for DOMDEC, P/ON w/ Itet2 of S10
C--------------------------------------------
      IF(NS10E>0) THEN
       IF(IPARI0/=0) THEN
        ALLOCATE(CELCND(NS10E))
        ALLOCATE(CEPCND(NS10E))
        ALLOCATE(ADDCNCND(0:NUMNOD+1))
        CALL PRE_CNDPON(ICNDS10,ADDCNCND,CEPCND,CELCND ,ITAGND )
        LCNCND = ADDCNCND(NUMNOD+1)-ADDCNCND(1)
        IF(LCNCND>0) THEN
         ALLOCATE(CNCND(LCNCND))
         CNCND(1:LCNCND)=0
         CALL FILLCNCND(CNCND  ,ADDCNCND,ICNDS10,ITAGND)
        END IF
       END IF
      END IF
C   Preparation traitement SPMD des ecoulements par BEM
      CALL TRACE_OUT1()
      ERR_MSG='PROCESS BEM FOR SPMD'
      ERR_CATEGORY='PROCESS BEM FOR SPMD'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(NFLOW>0 .AND. NSPMD > 1) CALL FLOWDEC(IFLOW)
      CALL TRACE_OUT1()
C--------------------------------------------
C     FERMETURE DU FICHIER INPUT TMP
C--------------------------------------------
      ERR_MSG='CLOSING TMP INPUT FILE'
      ERR_CATEGORY='INTERNAL'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(IPID/=0) CLOSE (UNIT=IIN)
      CALL TRACE_OUT1()
C--------------------------------------------
C     ECRITURE FICHIER DESSIN
C--------------------------------------------
      ERR_MSG='ANIMATION FILE WRITING'
      ERR_CATEGORY='ANIMATION FILE WRITING'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      WRITE(ISTDO,'(A)')TITRE(46)
      IF(IOUTPUT>0) CALL DESOUT(
     .         X      ,IXS    ,IXQ    ,IXC    ,IXT    ,
     .         IXP    ,IXR    ,IXTG   ,ITAB   ,PM     ,
     .         GEO    ,MS     ,IXS10  ,IGEO  ,IPM    ,
     .         KXSP    ,IPART  ,IPARTSP )
      MWA = ZERO
C--------------------------------------------
C     ECRITURE FICHIER ANIM
C--------------------------------------------
      IFVANI=0
      IF(ANIM_VERS>=40.AND.(DSANIM==1.OR.
     .                         DECANI==1.OR.
     .                         NMANIM>0.OR.IFVANI>0))   THEN
         NEL3D = NUMELS + NUMSPH + 3*NUMELS16 + 27*NUMELIG3D
         NEL2D = NUMELC + NUMELTG + NUMELQ 
         NEL1D = NUMELT + NUMELP  + 2*NUMELR
         NEL = MAX(NEL1D,NEL2D,NEL3D)
C
         SIAD=NPART+1
         SWAFT=MAX(3*NUMNOD,6*NEL3D,3*NEL2D,9*NEL1D)
         SMAS=NEL+3*NUMELS16
         SWA4=3*NUMNOD+2*NUMELS16
         SMATER=NPART
         SEL2FA=NEL+1
         SXNORM=3*NUMNOD+2*NUMELS16
         SINVERT=NEL2D
         IF(NUMELX>0) THEN
            SNFACPTX=NPART
            SIXEDGE=2*NANIM1D
            SOFFX1=NANIM1D
            SNUMX1=NANIM1D
            SFUNC1=10*NANIM1D
         ELSE
            SNFACPTX=1
            SIXEDGE=1
            SOFFX1=1
            SNUMX1=1
            SFUNC1=1
         ENDIF
C
         IANIM=0
         NELEM=NUMELC+NUMELTG+NUMELS+NUMELR +
     .         NUMELP+NUMELT +NUMELQ+NUMELX
C
         CALL MY_ALLOC(DNULL,3*NUMNOD)
         DO I=1,3*NUMNOD
            DNULL(I)=ZERO
         ENDDO
C
         DO I=1,MX_ANI
            ANIM_N(I)=0
            ANIM_V(I)=0
            ANIM_CE(I)=0
            ANIM_CT(I)=0
            ANIM_SE(I)=0
            ANIM_ST(I)=0
            ANIM_FE(I)=0
            ANIM_FT(I)=0
         ENDDO
         ANIM_M=1
         NN_ANI=0
         NV_ANI=NMANIM
         NCE_ANI=9*NMANIM
         NCT_ANI=2*NMANIM
         NSE_ANI=9*NMANIM
         NST_ANI=1*NMANIM
         NFE_ANI=8*NMANIM
C
         CALL GENANI1(
     1      X        ,ELBUF    ,IXS     ,IXQ      ,IXC      ,
     2      IXT      ,IXP      ,IXR     ,IXTG     ,SWAFT    ,
     3      IPARG    ,PM       ,GEO     ,SKEW     ,ITAB     ,
     4      LPBY     ,NPBY     ,NSTRF   ,RWBUF    ,NPRW     ,
     5      IPART    ,IPARTS  ,IPARTQ   ,IPARTC   ,
     6      IPARTT   ,IPARTP   ,IPARTR  ,IPARTG   ,
     7      RBY      ,SWA4    ,
     8      IGRSURF  ,BUFSF    ,IPARTX  ,KXSP     ,IXSP     ,
     9      IPARTSP  ,SPBUF    ,IXS10   ,IXS20    ,IXS16    ,
     A      IPM,     IGEO,    SMATER, SEL2FA,  SNFACPTX,
     B      SIXEDGE, SOFFX1,  SNUMX1, SXNORM,  SINVERT,
     C      SFUNC1,  SIAD  ,  NMANIM, DNULL,   SMAS,
     D      MS       ,FXANI    ,MBUFEL  ,MDEPL    ,NSLEVEL  ,
     E      ELSUB,   DSANIM,  NELEM,  CEP,     CEPSP,
     F      NOM_OPT  ,INOM_OPT(5),INOM_OPT(8),
     G      ELBUF_TAB,SPH2SOL  ,SUBSETS )
      DEALLOCATE(DNULL)

      ENDIF
C----------------------------------------------
C     Driver to reader of so-called "engine cards"
C----------------------------------------------
C      NGINE = 0 ! Number of Engine "cards", to be counted in contrl.F
      IF((IS_DYNA /= 0 .OR. NB_DYNA_INCLUDE /= 0).AND.(NGINE+NANIM_ENG/=0)) 
     .          CALL READ_ENGINE_DRIVER(IGRPART,IS_DYNA,NB_DYNA_INCLUDE)
C----------------------------------------------
C     Driver to QAPRINT
C----------------------------------------------
      CALL ST_QAPRINT_DRIVER(
     1        IGEO        ,GEO        ,BUFGEO          ,IPM           ,PM               ,
     2        BUFMAT      ,NOM_OPT    ,INOM_OPT(1)     ,NUMLOADP      ,ILOADP           ,
     3        LLOADP      ,LOADP      ,IBCL            ,FORC          ,IPRES            ,
     4        PRES        ,NPBY       ,LPBY            ,RBY           ,IBCR             ,
     5        FRADIA      ,IBCV       ,FCONV           ,IBFTEMP       ,FBFTEMP          ,
     6        IGRV        ,LGRAV      ,GRAV            ,IBFFLUX       ,FBFFLUX          ,
     7        ITAB        ,V          , VR             ,W             ,ICODE            ,
     8        ISKEW       ,ICFIELD    ,LCFIELD         ,CFIELD        ,DAMPR            ,
     9        TEMP        ,IBCSLAG    ,IPARI           ,INTBUF_TAB    ,CLUSTERS         ,
     A        IBOX        ,IPMAS      ,IBFV            ,VEL           ,NIMPACC          ,
     B        LACCELM     ,ACCELM     ,NOM_SECT        ,NSTRF         ,SECBUF           ,
     C        SKEW        ,ISKWN      ,XFRAME          ,T_MONVOL      ,T_MONVOL_METADATA,
     D        I2RUPT      ,AREASL     ,INTBUF_FRIC_TAB ,NPFRICORTH    ,MAT_ELEM         ,
     E        PFRICORTH   ,IREPFORTH  ,PHIFORTH        ,VFORTH        ,XREFC            ,
     F        XREFTG      ,XREFS      ,TAGXREF         ,IXS           ,IXC              ,
     G        IXTG        ,RWBUF      ,NPRW            ,LPRW          ,ITHVAR           ,
     H        IPART      ,SUBSETS     ,IPARTTH         ,NTHGRPMX      ,NIMPDISP         ,
     M        NIMPVEL     ,DETONATORS ,IBCSCYC         ,NPC           ,TF               ,
     N        TABLE       ,NPTS       ,IRBE3           ,LRBE3         ,FRBE3            ,
     P        MGRBY       ,IXS10      ,ISOLNOD         ,IXR           ,R_SKEW           ,
     O        IXP         ,IXT        ,X               ,THKE          ,SH4ANG           ,
     Q        THKEC       ,SH3ANG     ,SET             ,LSUBMODEL     ,IGRNOD           ,
     R        IGRPART     ,IGRBRIC    ,IGRSH4N         ,IGRSH3N       ,IGRQUAD          ,
     S        IGRBEAM     ,IGRTRUSS   ,IGRSPRING       ,IGRSURF       ,IGRSLIN          ,
     T        IXQ         ,ISPCOND    ,RTRANS          ,IRAND         ,ALEA             ,
     U        XSEED       ,XLAS       ,ILAS            ,IRBE2         ,LRBE2            ,
     V        KXSP        ,IPARTSP    ,DRAPE           ,IXR_KJ        ,IACTIV           ,
     W        FACTIV      ,UNITAB     ,NPBYL           ,LPBYL         ,RBYL             ,
     X        XYZREF      ,SENSORS    ,FUNC2D          ,
     Y        INICRACK    ,IPRELOAD   ,PRELOAD         ,IFLAG_BPRELOAD,IBMPC            ,
     Z        IBMPC2      ,IBMPC3     ,IBMPC4          ,RBMPC         ,LJOINT           ,
     A        NNLINK      ,LNLINK     ,BUFSF           ,SBUFSF        ,STACK%PM         ,
     B        STACK%GEO   ,STACK%IGEO ,IPARG           ,IPADMESH      ,PADMESH          ,
     C        LIFLOW      ,LRFLOW     ,IFLOW           ,RFLOW         ,
     D        SH4TREE     ,SH3TREE    ,SH4TRIM         ,SH3TRIM       ,QP_IPERTURB      ,
     E        QP_RPERTURB ,LLINAL     ,LINALE          ,FVM_INIVEL    ,GJBUFI           ,
     F        GJBUFR      ,MS         ,IN              ,LGAUGE        ,GAUGE            ,
     G        KXX         ,IXX        ,IPARTX          ,LRIVET        ,IXS16            ,
     H        ICONX       ,FXBIPM     ,FXBFILE_TAB     ,EIGIPM        ,EIGRPM           ,
     I        ISPHIO      ,VSPHIO     ,EBCS_TAB        ,INIMAP1D      ,INIMAP2D         ,
     J        NSIGSH      ,SIGSH      ,NSIGI           ,SIGSP         ,NSIGS            ,
     K        SIGI        ,NSIGBEAM   ,SIGBEAM         ,NSIGTRUSS     ,SIGTRUSS         ,
     L        NSIGRS      ,SIGRS      ,MERGE_NODE_TAB  ,MERGE_NODE_TOL,
     M        IMERGE      ,NMERGE_TOT ,IEXLNK ,DRAPEG  ,USER_WINDOWS  ,OUTPUT )
!
      DEALLOCATE(SIGI)
      DEALLOCATE(SIGSH)
      DEALLOCATE(SIGSP)
      DEALLOCATE(SIGRS)
      DEALLOCATE(SIGBEAM)
      DEALLOCATE(SIGTRUSS)
C--------------------------------------------
C     DELETE HM_MODEL IN MEMORY
C--------------------------------------------
      CALL CPP_DELETE_MODEL()
C
      IF(NINTER > 0) THEN
        DEALLOCATE(I2RUPT)
        DEALLOCATE(AREASL)
      ENDIF
      IF(NRBMERGE > 0) THEN
        DEALLOCATE(MGRBY)
      ENDIF
C     -------------------
C     Memory deallocation
C     -------------------
      IF(NFUNC2D > 0) THEN
         DO KK = 1, NFUNC2D
            DEALLOCATE(FUNC2D(KK)%XVAL, FUNC2D(KK)%FVAL)
         ENDDO
         DEALLOCATE(FUNC2D)
      ENDIF
      IF(ALLOCATED(RNOISE))  DEALLOCATE(RNOISE)
      IF(ALLOCATED(PERTURB)) DEALLOCATE(PERTURB)
      IF(ALLOCATED(QP_IPERTURB)) DEALLOCATE(QP_IPERTURB)
      IF(ALLOCATED(QP_RPERTURB)) DEALLOCATE(QP_RPERTURB)

C--------FRICTION OROTHTROPIC DIRECTIONS dealloc now after qa print -----
      IF(NINTER > 0 .AND.NINTERFRIC >0.AND. IORTHFRICMAX > 0) THEN

        DEALLOCATE(PFRICORTH ,IREPFORTH , VFORTH ,PHIFORTH  )

      ENDIF
C
C----------------------------------------------
      CALL TRACE_OUT1()
      ERR_MSG='RESTART FILE(S) WRITING'
      ERR_CATEGORY='RESTART FILE(S) WRITING'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
      IF(IERR==0) THEN
C--------------------------------------------m
C     SPMD : SPLIT +  ECRITURE FICHIER RESTART PAR PROC
C--------------------------------------------
          IF(RESTART_FILE==1) WRITE(ISTDO,'(A)')TITRE(50)
          IF(RESTART_FILE==0) WRITE(ISTDO,'(A)')CHECK_MESSAGE(1)( 1:len_trim(CHECK_MESSAGE(1)) )
C--------------------------------------------
C Matrice de connectivite globale
C--------------------------------------------
          ILEN = MAX(NUMELS,NUMELQ,NUMELC,NUMELT,NUMELP,NUMELR,NUMELTG)
          IF(LCNE>0) ALLOCATE(CNE(LCNE),STAT=stat)
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,MSGTYPE=MSGERROR,C1='CNE')
          CALL FILLCNE(
     1      CNE    ,LCNE   ,IXS    ,IXS10   ,IXS20   ,
     2      IXS16  ,IXQ    ,IXC    ,IXT     ,IXP     ,
     3      IXR    ,IXTG   ,IXTG1   ,T_MONVOL  ,
     4      IGRSURF,IBCL   ,ADDCNE  ,CEP     ,
     5      ILEN   ,GEO    ,IBCV   ,IBCR    ,IBFFLUX ,
     6      ILOADP ,LLOADP ,CEL    ,EBCS_TAB,LOADS)
          IF(I2NSNT>0) THEN
            IF(LCNI2G>0) ALLOCATE(CNI2(LCNI2G))
            CALL FILLCNI2(
     1        CNI2   ,LCNI2G,ADDCNI2,IPARI, INTBUF_TAB )
          ENDIF
C--------------------------------------------
C     XDP ARRAY IN STARTER FOR SINGLE PRECISION
      ALLOCATE(XDP(1))

          LENVOLU = NIMV*NVOLU+LICBAG+LIBAGJET+LIBAGHOL+LIBAGALE
C
          LNOM_OPT=SNOM_OPT
          LENPOR = SNODPOR

          LENTHG = OUTPUT%TH%SITHBUF
          LENTHGR = SRTHBUF
Clongueur BUFMAT et BUFGEO
          LBUFMAT = SBUFMAT
          LBUFGEO = SBUFGEO
          LBUFSF  = SBUFSF
          PM1SHF = 1
          PM1SPH = 1

C----------------------------------------------------------------------
          ! Allocation and filling of specific ADDCNE and CNE for non-local
          IF(NLOC_DMG%IMOD>0) THEN
            ! Allocation of ADDCNE for non-local nodes
            IF(.NOT.ALLOCATED(NLOC_DMG%ADDCNE)) ALLOCATE(NLOC_DMG%ADDCNE(0:NLOC_DMG%NNOD+1))
            NLOC_DMG%ADDCNE(0:NLOC_DMG%NNOD+1) = 0
            ! Filling ADDCNE for non-local nodes
            CALL BUILD_ADDCNEL_SUB(ADDCNE  ,CNE      ,NLOC_DMG%ADDCNE,NLOC_DMG%INDX,NLOC_DMG%NNOD)
!             WRITE(*,*) "NLOC_DMG%ADDCNE(1:8) = ", NLOC_DMG%ADDCNE(1:8)
!             WRITE(*,*) "ADDCNE(1:8) = ", ADDCNE(1:8)
!             WRITE(*,*) "NLOC_DMG%ADDCNE(FIN) = ", NLOC_DMG%ADDCNE(NLOC_DMG%NNOD+1-8:NLOC_DMG%NNOD+1)
!             WRITE(*,*) "ADDCNE(FIN) = ", ADDCNE(NUMNOD+1-8:NUMNOD+1)
            ! Allocation of CNE for non-local nodes
            IF(.NOT.ALLOCATED(NLOC_DMG%CNE)) ALLOCATE(NLOC_DMG%CNE(NLOC_DMG%ADDCNE(NLOC_DMG%NNOD+1)-1))
            NLOC_DMG%CNE(1:NLOC_DMG%ADDCNE(NLOC_DMG%NNOD+1)-1) = 0
!             WRITE(*,*) 'SIZE NL = ', SIZE(NLOC_DMG%CNE)
!             WRITE(*,*) 'SIZE_L = ', SIZE(CNE)
            ! Filling CNE for non-local nodes
            CALL BUILD_CNEL_SUB(NLOC_DMG%CNE,NLOC_DMG%ADDCNE,CNE,ADDCNE,NLOC_DMG%INDX,NLOC_DMG%NNOD)
!             WRITE(*,*) 'NLOC_DMG%CNE(1:8) = ', NLOC_DMG%CNE(1:8)
!             WRITE(*,*) 'CNE(1:8) = ', CNE(ADDCNE(NLOC_DMG%INDX(1)):ADDCNE(NLOC_DMG%INDX(1))+7)
!             WRITE(*,*) 'NLOC_DMG%CNE(FIN) = ', NLOC_DMG%CNE(NLOC_DMG%ADDCNE(NLOC_DMG%NNOD+1)-1-8:
!      .                                         NLOC_DMG%ADDCNE(NLOC_DMG%NNOD+1)-1)
!             WRITE(*,*) 'CNE(FIN) = ', CNE(ADDCNE(NLOC_DMG%INDX(NLOC_DMG%NNOD)+1)-8-1:ADDCNE(NLOC_DMG%INDX(NLOC_DMG%NNOD)+1)-1)
          ENDIF
C--------------------------------------------
C     Multidomains -> transfert de la domdec
C--------------------------------------------
          IF((NSUBDOM>0).AND.(FLG_R2R_ERR==0)) THEN
            CALL R2R_CLEAN_INTER(IPARI,INTBUF_TAB,IPARTC,IPARTG,IPARTS,ISOLNOD)
            IF(IDDOM>0) THEN
           WRITE(ISTDO,'(A)')' .. MULTIDOMAINS DOMDEC SYNCHRONIZATION '
              CALL R2R_DOMDEC(IEXLNK,IGRNOD,FRONTB_R2R,DT_R2R,1)
            ELSE
              CALL R2R_DOMDEC(IEXLNK,IGRNOD,FRONTB_R2R,DT_R2R,2)
            ENDIF
          ENDIF

C    deallocation of arrays that are not needed anymore
C    The memory peak is in ddsplit: we need to deallocate everything that is not
C    needed anmyre before ddsplit.

       DEALLOCATE(IKINE1LAG)
       DEALLOCATE(IWCONT)
       DEALLOCATE(IWCIN2)
       DEALLOCATE(DSDOF)




C-------------------------------------------------------------
C INTERFACE ROUTINES CALLED BEFORE DOMAIN DECOMPOSITION
C-------------------------------------------------------------

C-------------------------------------------------------------
C         Set INTERCEP (for all INTERFACES except TYPE24) (flag=1)
C-------------------------------------------------------------

          CALL SET_INTERCEP(IPARI,INTERCEP,1,INTBUF_TAB,ITAB,CEP)

C ! this call is maintened here to avoid a bug
C-------------------------------------------------------------
          IF(NSPMD > 1 .AND. IDDLEVEL > 0) THEN
            CALL SET_FRONT8(IPARI,INTERCEP,INTBUF_TAB,INTERT8,NBT8,ITAB)
          ENDIF
C--------------------------------------------
C         /INTER/TYPE25 connectivit sommets => segments
C-------------------------------------------------------------
C
C         Dimensioning (computes NUMNOR == Nb of normals or vertices wrt ALL Interfaces TYPE25)
C         and Initialization of IRT>LM(3:4,1:NSN)
          CALL PREPARE_INT25(INTBUF_TAB, IPARI, INTERCEP, NRTMT_25)
          CALL PREPARE_SPLIT_I25E2E(NSPMD,INTBUF_TAB,IPARI,INTERCEP)

C
          ALLOCATE(ADDCSRECT(NUMNOR+1),CSRECT(4*NRTMT_25),STAT=stat)
          IF(STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO,
     .                               MSGTYPE=MSGERROR,
     .                          C1='CSRECT')
          ADDCSRECT(1:NUMNOR+1)=0
C
          IF(NINTER25 /= 0)
     .      CALL BUILD_CSRECT(INTBUF_TAB,IPARI,CSRECT,ADDCSRECT)
C
C--------------------------------------------
C LINES : SET A CPU for splitting
!       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        /\
!       /  \
!      / |  \
!     /  |   \
!    /   o    \
!   /__________\
!
!       /LINE are not used in the engine and the split is wrong in
!       case of useless line (ie. when a line is defined but not
!       used by an interface or other stuffs) -->
!       2 nodes (defining a segment) can be on 2 different processors
!       in this case, the segment is not written in the restart file
!       one could also define the nodes on the same processor but
!       it will increase the comm.
!       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
          CALL LINE_DECOMP(IGRSLIN)
!--------------------------------------------
C--------------------------------------------
C Calcul de variables globales SPMD
C--------------------------------------------
C CARE TO be computed right before DDSPLIT
C This routine computes array sizes for Animation file.
C There should not be any call to frontplus beside this point.
C--------------------------------------------
          CALL GLOBVARS(IGEO,IXR    ,NSTRF  )
          IF(NPLYMAX > 0)THEN
             CALL SPMD_ANIM_PLY_INIT(IGEO, GEO  ,IPARG  ,IXC  ,IXTG ,
     .                          IPARTC,IPARTQ,IPARTG ,STACK )
          ENDIF
C--------------------------------------------

c         start CPU timer for ddsplit
          CALL STARTIME(3,1)
!       compute the size of TAG_SCRATCH and check if /INTER/24 or /25 is used
          CALL GET_SIZE_INTER24(I24MAXNSNE2,NINTER,NPARI,IPARI,FLAG_24_25)
!       compute the local number of element
          CALL GET_SIZE_NUMNOD_LOCAL(NUMNOD,NUMNOD_L)

          ALLOCATE( ALE_ELM(NSPMD) )
          IF( (NUMELS>0).AND.(IALE+IEULER+ITHERM+IALELAG/=0) ) THEN
                CALL SPLIT_CFD_SOLIDE(NUMELS,ALE_CONNECTIVITY,IXS,ALE_ELM,SIZE_ALE_ELM)
          ELSE
                SIZE_ALE_ELM(1:NSPMD) = 0
          ENDIF
          ! -----------------------------------------
          ! reverse connectivity for FVM solver : useful to ensure the parith/on
          IF(IALE+IEULER+ITHERM+IALELAG/=0) THEN
                ALLOCATE( INDX_S(NUMELS) )
                ALLOCATE( INDX_Q(NUMELQ) )
                ALLOCATE( INDX_TG(NUMELTG) )
                ALLOCATE( FACE_ELM_S(6*NUMELS,2) )
                ALLOCATE( FACE_ELM_Q(4*NUMELQ,2) )
                ALLOCATE( FACE_ELM_TG(3*NUMELTG,2) )

                INDX_S(1:NUMELS) = 0
                INDX_Q(1:NUMELQ) = 0
                INDX_TG(1:NUMELTG) = 0
                FACE_ELM_S(1:6*NUMELS,1:2) = 0
                FACE_ELM_Q(1:4*NUMELQ,1:2) = 0
                FACE_ELM_TG(1:3*NUMELTG,1:2) = 0
                BOOL_ALE_TG = (N2D/=0.AND.MULTI_FVM%IS_USED)
                CALL MULTI_CONNECTIVITY( INDX_S,INDX_Q,INDX_TG,
     1                                   FACE_ELM_S,FACE_ELM_Q,FACE_ELM_TG,
     2                                   IXS,IXQ,IXTG,CEP,ALE_CONNECTIVITY,BOOL_ALE_TG)

            ELSE
                ALLOCATE( INDX_S(0) )
                ALLOCATE( INDX_Q(0) )
                ALLOCATE( INDX_TG(0) )
                ALLOCATE( FACE_ELM_S(0,0) )
                ALLOCATE( FACE_ELM_Q(0,0) )
                ALLOCATE( FACE_ELM_TG(0,0) )
            ENDIF
          ! -----------------------------------------

          ! -----------------------------------------
          ! split the LOADS structure on the different processors
            CALL SPLIT_PCYL(LOADS%NLOAD_CYL,LOADS,LOADS_PER_PROC)
          ! -----------------------------------------
C Parallel

!   -------------------------------------------------------------
!       RESTART FILE WRITING
!       if -norst cdl is used or if /RFILE/OFF is used, then
!       restart files are not generated
!   -------------------------------------
          IF(RESTART_FILE==1) THEN
C            CALL PREPARE_INT25_EDGE(INTBUF_TAB,INTERCEP,NSPMD,IPARI)
C----- create TAG_SKINS6 for /H3D/STRESS/TENS/OUTER
             ALLOCATE(TAG_SKINS6(NUMELS))
             CALL SURFEXT_TAGN(IXS    ,KNOD2ELS,NOD2ELS  ,IXS10   ,TAG_SKINS6,itab)
          P=0
          NP=P
          IF(.NOT. ALLOCATED(PARTSAV)) ALLOCATE(PARTSAV(0))
          IF(.NOT. ALLOCATED(ADMSMS)) ALLOCATE(ADMSMS(0))
          IF(.NOT. ALLOCATED(DMELC)) ALLOCATE(DMELC(0))
          IF(.NOT. ALLOCATED(DMELS)) ALLOCATE(DMELS(0))
          IF(.NOT. ALLOCATED(DMELTG)) ALLOCATE(DMELTG(0))
          IF(.NOT. ALLOCATED(DMELTR)) ALLOCATE(DMELTR(0))
          IF(.NOT. ALLOCATED(DMELP)) ALLOCATE(DMELP(0))
          IF(.NOT. ALLOCATED(DMELRT)) ALLOCATE(DMELRT(0))
          IF(.NOT. ALLOCATED(RES_SMS)) ALLOCATE(RES_SMS(0))
          IF(.NOT. ALLOCATED(DIAG_SMS)) ALLOCATE(DIAG_SMS(0))
          IF(.NOT. ALLOCATED(CNE_PXFEM)) ALLOCATE(CNE_PXFEM(0))
          IF(.NOT. ALLOCATED(CEL_PXFEM)) ALLOCATE(CEL_PXFEM(0))
          IF(.NOT. ALLOCATED(MSZ2)) ALLOCATE(MSZ2(0))
          IF(.NOT. ALLOCATED(XFEM_PHANTOM)) ALLOCATE(XFEM_PHANTOM(0))
!$OMP PARALLEL PRIVATE(ITASK,P,pMEMFLOW,INDX_NM,NINDX_NM,TAG_NM)
!$OMP+ PRIVATE(NINDX_SCRT,INDX_SCRT,TAG_SCRATCH)
          NINDX_NM = 0
          NINDX_SCRT = 0
          IF(NINTER>0) THEN
                ALLOCATE( TAG_NM(NUMNOD) )
                ALLOCATE( INDX_NM(NUMNOD) )
                ALLOCATE( TAG_SCRATCH(I24MAXNSNE2+NUMNOD+NUMELS+NUMFAKENODIGEO) )
                ALLOCATE(INDX_SCRT(I24MAXNSNE2+NUMNOD+NUMELS+NUMFAKENODIGEO) )
                TAG_NM(1:NUMNOD) = 0
                INDX_NM(1:NUMNOD) = 0
                TAG_SCRATCH(1:I24MAXNSNE2+NUMNOD+NUMELS+NUMFAKENODIGEO) = 0
                INDX_SCRT(1:I24MAXNSNE2+NUMNOD+NUMELS+NUMFAKENODIGEO) = 0
          ELSE
                ALLOCATE(TAG_NM(0))
                ALLOCATE(INDX_NM(0))
                ALLOCATE( TAG_SCRATCH(0) )
                ALLOCATE(INDX_SCRT(0) )
          ENDIF

 220      CONTINUE

!$OMP CRITICAL
          NP=NP+1
          P=NP
!$OMP END CRITICAL

#if defined(_OPENMP)
          ITASK = OMP_GET_THREAD_NUM()
#endif
c
          IF(P > NSPMD) GOTO 221
          !Sending clean addresses for unallocated arrays in case P=0
          NULLIFY(pMEMFLOW) ; IF(NSPMD  > 0)  pMEMFLOW => MEMFLOW(1,P)    !MEMFLOW(2,1:NSPMD)   ; NSPMD =0 => MEMFLOW(1,0) is undefined


          CALL DDSPLIT(
     1         P            ,CEP         ,CEL            ,IGEO        ,MAT_ELEM   ,
     2         IPM          ,ICODE       ,ISKEW          ,ISKWN       ,BID13      ,
     3         IBCSLAG      ,IPART       ,IPARTS         ,IPARTQ      ,IPARTC     ,
     4         IPARTT       ,IPARTP      ,IPARTR         ,IPARTG     ,
     5         IPARTX       ,NPC         ,IXTG           ,GROUP_PARAM_TAB,
     6         IXTG1        ,IXS         ,IXS10          ,IXS20       ,IXS16      ,
     7         IXQ          ,IXC         ,IXT            ,IXP         ,IXR        ,
     8         ITAB         ,ITABM1      ,GJBUFI         ,ALE_CONNECTIVITY%NALE   ,
     9         ALE_CONNECTIVITY,
     A         KXX          ,IXX         ,IBCL           ,IBFV        ,
     B         ILAS         ,LACCELM     ,NNLINK         ,LNLINK      ,    
     C         IPARG        ,IGRV        ,LGRAV          ,IBVEL       ,LBVEL      ,    
     D         IACTIV       ,FACTIV      ,KINET          ,IPARI       ,NPRW       ,              
     E         LPRW         ,ICONX       ,NPBY           ,    
     F         LPBY         ,LRIVET      ,NSTRF          ,LJOINT      ,NODPOR     ,    
     G         MONVOL       ,ICONTACT    ,LAGBUF         ,    
     H         FR_IAD       ,X           ,D              ,V           ,VR         ,    
     I         DR           ,THKE        ,DAMPR          ,DAMP        ,MS         ,    
     J         IN           ,TF          ,PM             ,SKEW        ,XFRAME     ,    
     K         GEO          ,EANI        ,BUFMAT         ,BUFGEO      ,BUFSF      ,    
     L         RBMPC        ,GJBUFR      ,W              ,VEUL        ,FILL       ,    
     M         DFILL        ,WB          ,DSAVE          ,ASAVE       ,MSNF       ,    
     N         SPBUF        ,FORC        ,VEL            ,FSAV        ,FZERO      ,    
     O         XLAS         ,ACCELM      ,FBVEL          ,GRAV       ,    
     P         FR_WAVE      ,FAILWAVE    ,PARTS0         ,ELBUF      ,                
     Q         RWBUF        ,RWSAV       ,RBY            ,RIVET       ,    
     R         SECBUF       ,VOLMON      ,RCONX          ,NLOC_DMG    ,                             
     S         FVMAIN       ,LIBAGALE    ,LENTHG         ,LBUFMAT     ,LBUFGEO    ,
     T         LBUFSF       ,SXLAS                       ,LNOM_OPT    ,SILAS      ,    
     U         LENVOLU                   ,NPTS           ,CNE         ,LCNE       ,    
     V         ADDCNE       ,CNI2        ,LCNI2G         ,ADDCNI2     ,CEPI2      ,    
     W         CELI2        ,I2NSNT      ,PROBINT        ,DDSTAT(1,P) ,PM1SHF,         
     X         DD_IAD       ,LSHSUB      ,DRSAVE         ,ARSAVE      ,STSAVE     ,    
     Y         STRSAVE      ,FSAVSAV     ,                                             
     Z                       KXSP        ,IXSP           ,NOD2SP      ,CEPSP      ,    
     a         NTHWA        ,NAIRWA      ,NMNT           ,L_MUL_LAG1  ,L_MUL_LAG  ,    
     b         LWASPIO      ,IPARTSP     ,ISPCOND        ,PM1SPH      ,                
     c         WMA          ,
     d         EIGIPM       ,EIGIBUF     ,EIGRPM         ,    
     e         IFLOW        ,RFLOW       ,pMEMFLOW       ,IEXLNK      ,FASOLFR    ,    
     f         IPARTTH      ,    
     j         FXBIPM       ,FXBRPM      ,FXBNOD         ,FXBMOD      ,FXBGLM     ,    
     k         FXBCPM       ,FXBCPS      ,FXBLM          ,FXBFLS      ,FXBDLS     ,    
     l         FXBDEP       ,FXBVIT      ,FXBACC         ,FXBELM      ,FXBSIG     ,    
     m         FXBGRVI      ,FXBGRVR     ,IADLL          ,LLL         ,IBMPC      ,    
     n         LAMBDA       ,LRBAGALE    ,ISKWP          ,NSKWP       ,ISENSP     ,    
     o         NSENSP       ,IACCP       ,NACCP          ,IPART_STATE ,MCP        ,    
     p         TEMP         ,UNITAB      ,INTSTAMP       ,IFRAME      ,CLUSTERS   ,    
     q                       PARTSAV     ,IBFTEMP        ,FBFTEMP     ,IBCV       ,       
     r         FCONV        ,IRBE3       ,LRBE3          ,FRBE3       ,FRONT_RM   ,    
     s         RBYM         ,IRBYM       ,LNRBYM         ,INOISE      ,FNOISE     ,    
     t         MS0          ,ADMSMS      ,NOM_SECT       ,ISPSYM      ,                
     u         SH4TREE      ,SH3TREE     ,IPADMESH       ,IBFFLUX     ,FBFFLUX    ,                             
     v         SH4TRIM      ,SH3TRIM     ,PADMESH        ,MSC         ,MSTG       ,    
     w         INC          ,INTG        ,PTG            ,MCPC        ,MCPTG      ,    
     x         RCONTACT     ,ACONTACT    ,PCONTACT       ,MSCND       ,INCND      ,    
     y         MSSA         ,MSTR        ,MSP            ,MSRT        ,IBCR       ,    
     z         FRADIA       ,DMELC       ,DMELTG         ,DMELS       ,DMELTR     ,    
     1         DMELP        ,DMELRT      ,RES_SMS                     ,ISPHIO     ,    
     2         LPRTSPH      ,LONFSPH     ,VSPHIO         ,SPHVELN     ,ALPH       ,    
     3         IFILL        ,IMS         ,IRBE2          ,LRBE2       ,    
     8         MS_PLY,
     9         ZI_PLY       ,INOD_PXFEM  ,IEL_PXFEM      ,ICODEP      ,ISKEWP     ,
     A         ADDCNE_PXFEM ,CNE_PXFEM   ,CEL_PXFEM      ,ITHVAR      ,XDP,TABLE  ,
     B         CELSPH       ,ICFIELD     ,LCFIELD        ,CFIELD      ,
     C         MSZ2         ,ITASK       ,DIAG_SMS,
     D         ILOADP       ,LLOADP      ,LOADP,
     E         INOD_CRKXFEM ,IEL_CRKXFEM ,ADDCNE_CRKXFEM ,CNE_CRKXFEM ,CEL_CRKXFEM,
     F         IBUFSSG_IO   ,INTERCEP    ,IBORDNODE      ,IEDGESH     ,IBORDEDGE  ,
     G         LINALE       ,NODEDGE     ,IEDGE          ,CEP_CRKXFEM ,IEDGE_TMP  ,
     H         CRKNODIAD    ,ELBUF_TAB   ,NOM_OPT        ,LGAUGE      ,GAUGE      ,
     I         IGAUP        ,NGAUP       ,NODLEVXF       ,FRONTB_R2R  ,DFLOW      ,
     J         VFLOW        ,WFLOW       ,SPH2SOL        ,SOL2SPH     ,IRST       ,
     K         ELCUTC       ,NODENR      ,KXFENOD2ELC    ,ENRTAG      ,INTBUF_TAB ,
     M         I11FLAG      ,XFEM_TAB    ,LENTHGR        ,RTHBUF      ,
     N         IXIG3D       ,KXIG3D      ,KNOT           ,IPARTIG3D   ,WIGE       ,
     O         NCRKPART     ,INDX_CRK    ,CRKLVSET       ,CRKSHELL    ,CRKSKY     ,
     P         CRKAVX       ,CRKEDGE     ,SENSORS        ,
     Q         STACK        ,XFEM_PHANTOM, INTERT8       ,TAB_UMP     ,POIN_UMP   ,
     R         SOL2SPH_TYP  ,ADDCSRECT   ,CSRECT         ,DRAPE       ,LOADS      ,
     S         ITAGND         ,ICNDS10     ,ADDCNCND   ,
     T         CEPCND       ,CELCND      ,CNCND          ,NATIV_SMS   ,I24MAXNSNE ,
     U         MULTI_FVM    ,SEGQUADFR   ,INTBUF_FRIC_TAB,SUBSETS     ,IGRNOD     ,
     V         IGRBRIC      ,IGRQUAD     ,IGRSH4N        ,IGRSH3N     ,IGRTRUSS   ,
     W         IGRBEAM      ,IGRSPRING   ,IGRPART        ,IGRSURF     ,IGRSLIN    ,
     X         POIN_PART_SHELL,POIN_PART_TRI,POIN_PART_SOL,MID_PID_SHELL,MID_PID_TRI ,
     Y         MID_PID_SOL  , TAG_NM      ,NINDX_NM       ,INDX_NM      ,TAG_SCRATCH ,
     Z         NINDX_SCRT   , INDX_SCRT   ,FLAG_24_25     ,NUMNOD_L(P)  ,TAG_SKN     ,
     A         MULTIPLE_SKEW, IGRSURF_PROC,KNOTLOCPC     ,KNOTLOCEL     ,ALE_ELM(P),
     B         SIZE_ALE_ELM(P),PINCH_DATA ,TAG_SKINS6    ,IBCSCYC      ,LBCSCYC   ,T_MONVOL,
     C         INDX_S,INDX_Q,INDX_TG,FACE_ELM_S,FACE_ELM_Q,FACE_ELM_TG,NBR_TH_MONVOL, EBCS_TAB,
     D         KLOADPINTER  ,LOADPINTER   ,DGAPLOADINT   ,S_LOADPINTER, SCEP,DYNAIN_DATA,
     E         DRAPEG       ,USER_WINDOWS ,OUTPUT        ,INTERFACES  ,NUMBER_LOAD_CYL ,
     F         LOADS_PER_PROC(P))
      GOTO 220
 221  CONTINUE

      DEALLOCATE(TAG_NM,TAG_SCRATCH)
      DEALLOCATE(INDX_NM,INDX_SCRT)
!$OMP END PARALLEL
      DEALLOCATE(TAG_SKINS6)
      ENDIF     ! <-- end of restart file writing
!   -------------------------------------------------------------
C
      CALL DEALLOCATE_IGRSURF_SPLIT(T_MONVOL,IGRSURF_PROC)
      DEALLOCATE( IGRSURF_PROC )

      IF( (NUMELS>0).AND.(IALE+IEULER+ITHERM+IALELAG/=0) ) CALL DEALLOCATE_SPLIT_CFD_SOLIDE(ALE_ELM)
      DEALLOCATE( ALE_ELM )

      DEALLOCATE( INDX_S )
      DEALLOCATE( INDX_Q )
      DEALLOCATE( INDX_TG )
      DEALLOCATE( FACE_ELM_S )
      DEALLOCATE( FACE_ELM_Q )
      DEALLOCATE( FACE_ELM_TG )
c     stop CPU timer for ddsplit
      CALL STOPTIME(3,1)

      CALL TRACE_OUT1()
      ERR_MSG='CLOSING STARTER'
      CALL TRACE_IN1(ERR_MSG,LEN_TRIM(ERR_MSG))
C----
C
C Deallocation
C
          DEALLOCATE(ICONTACT)
C
          IF(NELEM+NCONLD+NUMCONV+NUMRADIA+NFXFLUX+SLLOADP>0)THEN
            DEALLOCATE(CEP)
            DEALLOCATE(CEL)
          END IF
          IF(LCNE>0)THEN
            DEALLOCATE(CNE)
          END IF
C
          IF(I2NSNT>0) THEN
            DEALLOCATE(CELI2)
            DEALLOCATE(CEPI2)
            DEALLOCATE(ADDCNI2)
          END IF
          DEALLOCATE(ISKWP)
          DEALLOCATE(NSKWP)
          DEALLOCATE(ISENSP)
          DEALLOCATE(NSENSP)
          DEALLOCATE(IACCP)
          DEALLOCATE(NACCP)
          DEALLOCATE(IGAUP)
          DEALLOCATE(NGAUP)
          DEALLOCATE(IPART_STATE)
          DEALLOCATE(EIGIPM, EIGIBUF, EIGRPM)
          DEALLOCATE(TAG_SKN)
          DEALLOCATE(MULTIPLE_SKEW)
          IF(IDDLEVEL ==1 .OR. ((NINTER == 0).AND.(ISMS == 0))) THEN
            DEALLOCATE(TAGXREF)
            DEALLOCATE(TAGREFSTA)
          ENDIF
C
          IF(NUMSPH>0) THEN
            DEALLOCATE(CEPSP)
          END IF
          IF(NSPHIO>0)THEN
            DEALLOCATE(IBUFSSG_IO)
            DEALLOCATE(RESERVEP)
          ENDIF
          DEALLOCATE(CELSPH)
C
          IF(LAG_NCF > 0) THEN
            DEALLOCATE(IADLL)
            DEALLOCATE(LLL)
          END IF
C
C          IF(ALLOCATED(FVSPMD)) DEALLOCATE(FVSPMD)
C
          DEALLOCATE(ADDCSRECT)
          DEALLOCATE(CSRECT)
          DEALLOCATE(IGEO_STACK,GEO_STACK)
          IF(ALLOCATED(PLY_INFO))DEALLOCATE(PLY_INFO)
          IF(ALLOCATED(FXBFILE_TAB)) DEALLOCATE(FXBFILE_TAB)

          IF(ALLOCATED(TAB_UMP)) DEALLOCATE(TAB_UMP)
          IF(ALLOCATED(TAB_UMP_OLD)) DEALLOCATE(TAB_UMP_OLD)
          IF(ALLOCATED(POIN_UMP)) DEALLOCATE(POIN_UMP)
          IF(ALLOCATED(POIN_UMP_OLD)) DEALLOCATE(POIN_UMP_OLD)

          DEALLOCATE( POIN_PART_SHELL )
          DEALLOCATE( POIN_PART_TRI )
          DEALLOCATE( POIN_PART_SOL )
          DO I=1,NUMMAT
                IF(ALLOCATED(MID_PID_SHELL(I)%PID1D))DEALLOCATE( MID_PID_SHELL(I)%PID1D )
                IF(ALLOCATED(MID_PID_SHELL(I)%COST1D))DEALLOCATE( MID_PID_SHELL(I)%COST1D )

                IF(ALLOCATED(MID_PID_SHELL(I)%PID1D))DEALLOCATE( MID_PID_TRI(I)%PID1D )
                IF(ALLOCATED(MID_PID_TRI(I)%COST1D))DEALLOCATE( MID_PID_TRI(I)%COST1D )
                DO J=1,7
                        IF(ALLOCATED(MID_PID_SOL(I,J)%PID1D)) DEALLOCATE( MID_PID_SOL(I,J)%PID1D )
                        IF(ALLOCATED(MID_PID_SOL(I,J)%COST1D)) DEALLOCATE( MID_PID_SOL(I,J)%COST1D )
                ENDDO
          ENDDO
          DEALLOCATE( MID_PID_SHELL,MID_PID_TRI )
          DEALLOCATE( MID_PID_SOL )


          IF(ALLOCATED(IXIG3D)) DEALLOCATE(IXIG3D)
          IF(ALLOCATED(KXIG3D)) DEALLOCATE(KXIG3D)
          IF(ALLOCATED(MSIG3D)) DEALLOCATE(MSIG3D)
          IF(NS10E>0.AND.IPARI0/=0) THEN
            DEALLOCATE(CELCND)
            DEALLOCATE(CEPCND)
            DEALLOCATE(ADDCNCND)
            IF(LCNCND>0) DEALLOCATE(CNCND)
          END IF
          IF(ALLOCATED(ITAGND)) DEALLOCATE(ITAGND)
          IF(NS10E>0) DEALLOCATE(ICNDS10)
          CALL MONVOL_DEALLOCATE(NVOLU, T_MONVOL)
          IF(ALLOCATED(T_MONVOL)) DEALLOCATE(T_MONVOL)
          DEALLOCATE(IBCSCYC,LBCSCYC)
          IF(NBCSCYC>0) DEALLOCATE(ITAGCYC)
          IF(ALLOCATED(FVM_INIVEL)) DEALLOCATE(FVM_INIVEL)
C
C--       Seatblet structures deallocation
          IF(N_SEATBELT > 0) THEN
            DO I=1,N_SEATBELT
              DEALLOCATE(SEATBELT_TAB(I)%SPRING)
            ENDDO
            DEALLOCATE(SEATBELT_TAB)
          ENDIF
C
          IF(NSLIPRING > 0) THEN
            DO I=1,NSLIPRING
              DEALLOCATE(SLIPRING(I)%FRAM)
            ENDDO
            DEALLOCATE(SLIPRING)
          ENDIF
C
          IF(NRETRACTOR > 0) THEN
            DO I=1,NRETRACTOR
              DEALLOCATE(RETRACTOR(I)%INACTI_NODE)
            ENDDO
            DEALLOCATE(RETRACTOR)
          ENDIF
C --------------------------------------
C Starter Memory Printout
C --------------------------------------
          CALL PRINTSTSZ(DETONATORS)
C --------------------------------------
C Stat domdec + evaluation memoire SPMD
C --------------------------------------
C
          IF(RESTART_FILE==1) THEN
            CALL DDPRINT(DDSTAT, MEMFLOW)
          ELSE
            WRITE(IOUT,*)
            WRITE(IOUT,*) CHECK_MESSAGE(3)(1:len_trim(CHECK_MESSAGE(3)))
          ENDIF
C
c        ENDIF
      ELSE
        WRITE(ISTDO,'(A)')TITRE(48)
      ENDIF
C
      DEALLOCATE(KNOD2ELS,KNOD2ELC,KNOD2ELTG,
     .           NOD2ELS ,NOD2ELC ,NOD2ELTG,
     .           KNOD2EL1D,NOD2EL1D,KNOD2ELIG3D,
     .           NOD2ELIG3D,KNOD2ELQ,NOD2ELQ)
C
      DEALLOCATE(MSC,MSTG,MSSA,MSTR,MSP,MSRT)
C
      DEALLOCATE(MCP,TEMP)
      DEALLOCATE(IBCV, FCONV, IBCR, FRADIA, IBFTEMP, FBFTEMP, IBFFLUX, FBFFLUX)
C
      DEALLOCATE(RBYM  ,IRBYM    ,LNRBYM,WEIGHT_RM)
      DEALLOCATE(MS_PLY,ZI_PLY,ICODE,ISKEW)
c
      IF(ALLOCATED(KNOTLOCPC))DEALLOCATE(KNOTLOCPC)
      IF(ALLOCATED(KNOTLOCEL))DEALLOCATE(KNOTLOCEL)
c
      CALL TRACE_OUT1()
      IF(ALLOCATED(IPMAS))DEALLOCATE(IPMAS)
c---------------------------
      IF(ICRACK3D > 0) THEN
        DEALLOCATE(INOD_CRKXFEM,IEL_CRKXFEM)
        DEALLOCATE(CNE_CRKXFEM)
        DEALLOCATE(CEL_CRKXFEM)
        DEALLOCATE(CEP_CRKXFEM)
        DEALLOCATE(IEDGESH)
        DEALLOCATE(IBORDEDGE)
        DEALLOCATE(NODEDGE)
        DEALLOCATE(IEDGE)
        DEALLOCATE(IBORDNODE)
      END IF
      IF(ALLOCATED(IEDGE_TMP))     DEALLOCATE(IEDGE_TMP)
      IF(ALLOCATED(ELCUTC))        DEALLOCATE(ELCUTC)
      IF(ALLOCATED(NODENR))        DEALLOCATE(NODENR)
      IF(ALLOCATED(KXFENOD2ELC))   DEALLOCATE(KXFENOD2ELC)
      IF(ALLOCATED(ENRTAG))        DEALLOCATE(ENRTAG)
      IF(ALLOCATED(ADDCNE_CRKXFEM))DEALLOCATE(ADDCNE_CRKXFEM)
C----------------------------------------------
C     ALLOCATION TO REDUCE STACKSIZE
C----------------------------------------------
      DEALLOCATE(ADDCNE)
      DEALLOCATE(ADDCNE_PXFEM)
      DEALLOCATE(FXBTAG)
C
      DEALLOCATE(ISOLNOD)
      DEALLOCATE(ISOLOFF)
      DEALLOCATE(ISHEOFF)
      DEALLOCATE(ITRUOFF)
      DEALLOCATE(IPOUOFF)
      DEALLOCATE(IRESOFF)
      DEALLOCATE(ITRIOFF)
      DEALLOCATE(IGRNRBY)
      DEALLOCATE(IQUAOFF)
C
      DEALLOCATE(XREFC)
      DEALLOCATE(XREFTG)
      DEALLOCATE(XREFS)
      DEALLOCATE(IFRONT%P,IFRONT%IENTRY,IENTRY2)
      DEALLOCATE(DFLOW,VFLOW,WFLOW)
      DEALLOCATE(PERMUTATION%SOLID)
      IF(ALLOCATED(FILLSOL)) DEALLOCATE(FILLSOL)
      IF(ALLOCATED(SH3ANG))  DEALLOCATE(SH3ANG)
      IF(ALLOCATED(SH4ANG))  DEALLOCATE(SH4ANG)
      IF(ALLOCATED(NATIV_SMS))  DEALLOCATE(NATIV_SMS)
      IF(ALLOCATED(MULTI_FVM%VEL)) DEALLOCATE(MULTI_FVM%VEL)
      CALL ALE_CONNECTIVITY%ALE_DEALLOCATE_CONNECTIVITY()
!!      DEALLOCATE(STACK)
      IF(IPART_STACK > 0 .OR. IPART_PCOMPP > 0)DEALLOCATE(STACK_INFO)
      IF(ALLOCATED(IPRELOAD)) DEALLOCATE(IPRELOAD)
      IF(ALLOCATED(PRELOAD))  DEALLOCATE(PRELOAD)
      IF(ALLOCATED(IFLAG_BPRELOAD))  DEALLOCATE(IFLAG_BPRELOAD)
      IF(ALLOCATED(EOS_TAG))DEALLOCATE(EOS_TAG)
      CALL C_DELETE_HASH(H_NODE)
      CALL DEALLOCATE_DETONATORS(DETONATORS)
      IF(ALLOCATED(XSEED)) DEALLOCATE(XSEED)
      IF(ALLOCATED(ALEA))  DEALLOCATE(ALEA)
      IF(ALLOCATED(IRAND)) DEALLOCATE(IRAND)
      IF(ALLOCATED(SENSORS%SENSOR_TAB)) DEALLOCATE(SENSORS%SENSOR_TAB )
      IF(ALLOCATED(LOGICAL_SENSORS_LIST)) DEALLOCATE(LOGICAL_SENSORS_LIST)
C
      CALL EBCS_TAB%destroy()
      IF(NINIMAP1D > 0 .AND. .NOT. MULTI_FVM%IS_USED) THEN
         DO KK = 1, NINIMAP1D
            DEALLOCATE(INIMAP1D(KK)%TAGNODE)
         ENDDO
      ENDIF
      DEALLOCATE(INIMAP1D)

      IF(NINIMAP2D > 0 .AND. .NOT. MULTI_FVM%IS_USED) THEN
         DO KK = 1, NINIMAP2D
            DEALLOCATE(INIMAP2D(KK)%TAGNODE)
         ENDDO
      ENDIF
      DEALLOCATE(INIMAP2D)


      CALL INVERTED_GROUP_DEALLOC(INV_GROUP)

      CALL DEALLOCATE_JOINT( )

      IF(NFXBODY>0) THEN
         CLOSE(IFXM)
         CLOSE(IFXS) 
      ENDIF

      IF(ALLOCATED( DGAPINT ))  DEALLOCATE(DGAPINT)

C----------------------------------------------

      RETURN
      END
Chd|====================================================================
Chd|  SET_IBUFSSG_IO                source/starter/lectur.F       
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|====================================================================
      SUBROUTINE SET_IBUFSSG_IO(ISPHIO, IGRSURF, IBUFSSG_IO)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ISPHIO(NISPHIO,*),
     .        IBUFSSG_IO(SIBUFSSG_IO),
     .        N,J,NSEG,IN1,IN2,IN3,IN4,
     .        ISU,PROC,IAD2,ITYPE
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      IAD2 = 1

      DO N=1,NSPHIO
        ITYPE = ISPHIO(1,N)
        IF(ISPHIO(12,N)==0) THEN
          ISU = ISPHIO(3,N)
          NSEG= IGRSURF(ISU)%NSEG
          ISPHIO(10,N) = NSEG
          ISPHIO(11,N) = IAD2
          DO J=0,NSEG-1
            IN1=IGRSURF(ISU)%NODES(J+1,1)
            IN2=IGRSURF(ISU)%NODES(J+1,2)
            IN3=IGRSURF(ISU)%NODES(J+1,3)
            IN4=IGRSURF(ISU)%NODES(J+1,4)
            IBUFSSG_IO(IAD2+NIBSPH*J)   = IN1
            IBUFSSG_IO(IAD2+NIBSPH*J+1) = IN2
            IBUFSSG_IO(IAD2+NIBSPH*J+2) = IN3
            IBUFSSG_IO(IAD2+NIBSPH*J+3) = IN4
            DO PROC=1,NSPMD
              CALL IFRONTPLUS(IN1,PROC)
              CALL IFRONTPLUS(IN2,PROC)
              CALL IFRONTPLUS(IN3,PROC)
              CALL IFRONTPLUS(IN4,PROC)
            ENDDO
          ENDDO
          IAD2 = IAD2 + 4*NSEG
        ELSEIF(ISPHIO(12,N)==2) THEN
          IN1 = ISPHIO(13,N)
          IN2 = ISPHIO(14,N)
          IN3 = ISPHIO(15,N)
          DO PROC=1,NSPMD
            CALL IFRONTPLUS(IN1,PROC)
            CALL IFRONTPLUS(IN2,PROC)
            CALL IFRONTPLUS(IN3,PROC)
          ENDDO
        ENDIF
      ENDDO
C=======================================================================
      RETURN
      END
Chd|====================================================================
Chd|  INIT_PERMUTATION              source/starter/lectur.F       
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        REORDER_MOD                   share/modules1/reorder_mod.F  
Chd|====================================================================
      SUBROUTINE INIT_PERMUTATION()
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE REORDER_MOD

C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I

        DO I=1,NUMELS
           PERMUTATION%SOLID(I)=I
        ENDDO
        DO I=1,NUMELC
           PERMUTATION%SHELL(I)=I
        ENDDO
        DO I=1,NUMELTG
           PERMUTATION%TRIANGLE(I)=I
        ENDDO

      RETURN
      END
Chd|====================================================================
Chd|  TET4_10                       source/starter/lectur.F       
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE TET4_10(IGEO,ITET4_10)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IGEO(NPROPGI,*),ITET4_10
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,IGTYP,ITET4

        DO I=1,NUMGEO
         IGTYP =IGEO(11,I)
         ITET4 =IGEO(20,I)
         IF((IGTYP==14.OR.IGTYP==6).AND.ITET4==1) ITET4_10 = 1
        ENDDO

      RETURN
      END
