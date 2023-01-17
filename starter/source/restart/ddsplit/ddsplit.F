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
Chd|  DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        CLOSE_C                       source/output/tools/write_routines.c
Chd|        COMPRESS_I_NNZ                ../common_source/comm/compress_nnz.F
Chd|        COMPRESS_R_NNZ                ../common_source/comm/compress_nnz.F
Chd|        CUR_FIL_C                     source/output/tools/write_routines.c
Chd|        C_BUFEL                       source/restart/ddsplit/c_bufel.F
Chd|        C_CLUSTER                     source/restart/ddsplit/w_cluster.F
Chd|        C_CRKADD                      source/restart/ddsplit/c_crkadd.F
Chd|        C_CRKEDGE                     source/restart/ddsplit/c_crkedge.F
Chd|        C_CRKXFEM                     source/restart/ddsplit/c_crkxfem.F
Chd|        C_EIG                         source/restart/ddsplit/c_eig.F
Chd|        C_ELIG3D                      source/restart/ddsplit/c_elig3d.F
Chd|        C_ELMX                        source/restart/ddsplit/c_elmx.F
Chd|        C_ELTLOC                      source/restart/ddsplit/c_eltloc.F
Chd|        C_FASOLFR                     source/restart/ddsplit/c_idglob.F
Chd|        C_FRONT                       source/restart/ddsplit/c_front.F
Chd|        C_FVBAG                       source/airbag/c_fvbag.F       
Chd|        C_FXBODY1                     source/restart/ddsplit/c_fxbody.F
Chd|        C_FXBODY2                     source/restart/ddsplit/c_fxbody.F
Chd|        C_FXBODY3                     source/restart/ddsplit/c_fxbody.F
Chd|        C_FXBODY4                     source/restart/ddsplit/c_fxbody.F
Chd|        C_GROUP_STR                   source/restart/ddsplit/c_group_str.F
Chd|        C_IBCL                        source/restart/ddsplit/c_ibcl.F
Chd|        C_IBCSCYC                     source/restart/ddsplit/c_ibcscyc.F
Chd|        C_IBFT                        source/restart/ddsplit/c_ibft.F
Chd|        C_IBFV                        source/restart/ddsplit/c_ibfv.F
Chd|        C_IBMPC                       source/restart/ddsplit/c_ibmpc.F
Chd|        C_IBVEL                       source/restart/ddsplit/c_ibvel.F
Chd|        C_ICFIELD                     source/restart/ddsplit/c_icfield.F
Chd|        C_ICNDS10                     source/restart/ddsplit/c_icnds10.F
Chd|        C_ICONV                       source/restart/ddsplit/c_iconv.F
Chd|        C_IDGLOB                      source/restart/ddsplit/c_idglob.F
Chd|        C_IEBCS                       source/restart/ddsplit/c_iebcs.F
Chd|        C_IGRAV                       source/restart/ddsplit/c_igrav.F
Chd|        C_ILOADP                      source/restart/ddsplit/c_iloadp.F
Chd|        C_IRADIA                      source/restart/ddsplit/c_iradia.F
Chd|        C_IRBE2                       source/restart/ddsplit/c_irbe2.F
Chd|        C_IRBE3                       source/restart/ddsplit/c_irbe3.F
Chd|        C_ISHSUB                      source/restart/ddsplit/c_ishsub.F
Chd|        C_ISLIN_STR                   source/restart/ddsplit/c_islin_str.F
Chd|        C_ISPH                        source/restart/ddsplit/c_isph.F
Chd|        C_ISURF_STR                   source/restart/ddsplit/c_isurf_str.F
Chd|        C_ITHFLUX                     source/restart/ddsplit/c_ithflux.F
Chd|        C_IXFLOC                      source/restart/ddsplit/c_ixfloc.F
Chd|        C_JOINT_SMS                   source/constraints/general/cyl_joint/write_count_joint_sms.F
Chd|        C_LAG                         source/restart/ddsplit/c_lag.F
Chd|        C_LLINK                       source/restart/ddsplit/c_llink.F
Chd|        C_MAD                         source/restart/ddsplit/c_mad.F
Chd|        C_MULTI_VEL                   source/restart/ddsplit/c_multi_vel.F
Chd|        C_NCRKXFEM                    source/restart/ddsplit/c_ncrkxfem.F
Chd|        C_NGROUP                      source/restart/ddsplit/c_ngroup.F
Chd|        C_PON                         source/restart/ddsplit/c_pon.F
Chd|        C_PORO                        source/restart/ddsplit/c_poro.F
Chd|        C_PXFEM                       source/restart/ddsplit/c_pxfem.F
Chd|        C_RBYK                        source/restart/ddsplit/c_rbyk.F
Chd|        C_RBYMK                       source/restart/ddsplit/c_rbymk.F
Chd|        C_RWALL                       source/restart/ddsplit/c_rwall.F
Chd|        C_SEATBELTS                   source/restart/ddsplit/c_seatbelts.F
Chd|        C_SECTIO                      source/restart/ddsplit/c_sectio.F
Chd|        C_SEGQUADFR                   source/restart/ddsplit/c_segquadfr.F
Chd|        C_SPMD_NE_CONNECT             source/ale/spmd_ne_connect.F  
Chd|        C_VOIS                        source/restart/ddsplit/c_vois.F
Chd|        EIGWRESTP                     source/restart/ddsplit/wrrest.F
Chd|        ELAPSTIME_OMP                 source/system/timer.F         
Chd|        FILE_SIZE_C                   source/output/tools/write_routines.c
Chd|        FVWRESTP                      source/restart/ddsplit/fvwrestp.F
Chd|        FXBWRESTP                     source/restart/ddsplit/wrrest.F
Chd|        F_NODLOC2                     source/restart/ddsplit/f_nodloc2.F
Chd|        INTBUF_INI_STARTER            source/interfaces/intbuf/intbuf_ini_starter.F
Chd|        INTFRIC_WRESTI                source/interfaces/intbuf/intbufFric_ini_starter.F
Chd|        INTFRIC_WRESTR                source/interfaces/intbuf/intbufFric_ini_starter.F
Chd|        INTSTAMP_WRESTI               share/modules1/intstamp_mod.F 
Chd|        INTSTAMP_WRESTR               share/modules1/intstamp_mod.F 
Chd|        IPARI_L_INI                   source/restart/ddsplit/ipari_l_ini.F
Chd|        M_FLOW                        source/restart/ddsplit/m_flow.F
Chd|        NFWRESTP                      source/restart/ddsplit/wrrest.F
Chd|        PBLAST_WRITE_STARTER          ../common_source/modules/pblast_mod.F
Chd|        SETLENWA                      source/restart/ddsplit/setlenwa.F
Chd|        SPLIT_EBCS                    source/boundary_conditions/ebcs/split_ebcs.F
Chd|        SPLIT_INTERFACES              source/restart/ddsplit/split_interfaces.F
Chd|        SPLIT_SKEW                    source/restart/ddsplit/split_skew.F
Chd|        SPMD_NE_CONNECT               source/ale/spmd_ne_connect.F  
Chd|        SPMD_USERWI_REST              source/user_interface/userwis_ini_spmd.F
Chd|        STARTIME                      source/system/timer.F         
Chd|        STOPTIME                      source/system/timer.F         
Chd|        TABLE_WRESTI                  source/tools/curve/table_tools.F
Chd|        TABLE_WRESTR                  source/tools/curve/table_tools.F
Chd|        WRCOMIP                       source/restart/ddsplit/wrcommp.F
Chd|        WRCOMR                        source/restart/ddsplit/wrcomm.F
Chd|        WRITE_C_C                     source/output/tools/write_routines.c
Chd|        WRITE_DB                      source/restart/ddsplit/wrrest.F
Chd|        WRITE_DPDB                    source/restart/ddsplit/wrrest.F
Chd|        WRITE_ELGROUP_PARAM           source/restart/ddsplit/write_elgroup_param.F
Chd|        WRITE_I_C                     source/output/tools/write_routines.c
Chd|        WRITE_JOINT                   source/constraints/general/cyl_joint/write_joint.F
Chd|        WRITE_MATPARAM                source/materials/mat/write_matparam.F
Chd|        WRITE_NLOC_STRUCT             source/restart/ddsplit/write_nloc_struct.F
Chd|        WRITE_SENSOR_TAB              source/tools/sensor/write_sensor_tab.F
Chd|        WRITE_UNITS                   ../common_source/comm/write_units.F
Chd|        WRWEIGHT_RM                   source/restart/ddsplit/wrweight_rm.F
Chd|        W_ALELAG                      source/restart/ddsplit/w_alelag.F
Chd|        W_ANIM_CRK                    source/restart/ddsplit/w_anim_crk.F
Chd|        W_ANIM_PLY                    source/restart/ddsplit/w_anim_ply.F
Chd|        W_AVUPLY                      source/restart/ddsplit/w_avuply.F
Chd|        W_BID                         source/restart/ddsplit/w_bid.F
Chd|        W_BUFBRIC_22                  source/interfaces/inter3d1/w_bufbric_22.F
Chd|        W_BUFEL                       source/restart/ddsplit/w_bufel.F
Chd|        W_CLUSTER                     source/restart/ddsplit/w_cluster.F
Chd|        W_CRKXFEM                     source/restart/ddsplit/w_crkxfem.F
Chd|        W_DRAPE                       source/restart/ddsplit/w_drape.F
Chd|        W_ELBUF_STR                   source/restart/ddsplit/w_elbuf_str.F
Chd|        W_ELIG3D                      source/restart/ddsplit/w_elig3d.F
Chd|        W_ELMX                        source/restart/ddsplit/w_elmx.F
Chd|        W_FAC                         source/restart/ddsplit/w_fac.F
Chd|        W_FAILWAVE                    source/restart/ddsplit/w_failwave.F
Chd|        W_FASOLFR                     source/restart/ddsplit/w_fasolfr.F
Chd|        W_FBFT                        source/restart/ddsplit/w_fbft.F
Chd|        W_FCONV                       source/restart/ddsplit/w_fconv.F
Chd|        W_FI                          source/restart/ddsplit/w_fi.F 
Chd|        W_FIXVEL                      source/restart/ddsplit/w_fixvel.F
Chd|        W_FRADIA                      source/restart/ddsplit/w_fradia.F
Chd|        W_FRBE3                       source/restart/ddsplit/w_frbe3.F
Chd|        W_FRONT                       source/restart/ddsplit/w_front.F
Chd|        W_FSPH                        source/restart/ddsplit/w_fsph.F
Chd|        W_FTHFLUX                     source/restart/ddsplit/w_fthflux.F
Chd|        W_GEO                         source/restart/ddsplit/w_geo.F
Chd|        W_GJOINT                      source/restart/ddsplit/w_gjoint.F
Chd|        W_GROUP_STR                   source/restart/ddsplit/w_group_str.F
Chd|        W_IBCL                        source/restart/ddsplit/w_ibcl.F
Chd|        W_IBCSCYC                     source/restart/ddsplit/w_ibcscyc.F
Chd|        W_IBFT                        source/restart/ddsplit/w_ibft.F
Chd|        W_IBFV                        source/restart/ddsplit/w_ibfv.F
Chd|        W_IBMPC                       source/restart/ddsplit/w_ibmpc.F
Chd|        W_IBVEL                       source/restart/ddsplit/w_ibvel.F
Chd|        W_ICFIELD                     source/restart/ddsplit/w_icfield.F
Chd|        W_ICNDS10                     source/restart/ddsplit/w_icnds10.F
Chd|        W_ICONV                       source/restart/ddsplit/w_iconv.F
Chd|        W_IELOC                       source/restart/ddsplit/w_ieloc.F
Chd|        W_IGRAV                       source/restart/ddsplit/w_igrav.F
Chd|        W_ILOADP                      source/restart/ddsplit/w_iloadp.F
Chd|        W_ING2LOC                     source/restart/ddsplit/w_ing2loc.F
Chd|        W_INLOC                       source/restart/ddsplit/w_inloc.F
Chd|        W_IPARG                       source/restart/ddsplit/w_iparg.F
Chd|        W_IRADIA                      source/restart/ddsplit/w_iradia.F
Chd|        W_IRBE2                       source/restart/ddsplit/w_irbe2.F
Chd|        W_IRBE3                       source/restart/ddsplit/w_irbe3.F
Chd|        W_IRIVET                      source/restart/ddsplit/w_irivet.F
Chd|        W_ISHSUB                      source/restart/ddsplit/w_ishsub.F
Chd|        W_ISKEWSP                     source/restart/ddsplit/w_iskewsp.F
Chd|        W_ISKN                        source/restart/ddsplit/w_iskn.F
Chd|        W_ISLIN_STR                   source/restart/ddsplit/w_islin_str.F
Chd|        W_ISPH                        source/restart/ddsplit/w_isph.F
Chd|        W_ISURF_STR                   source/restart/ddsplit/w_isurf_str.F
Chd|        W_ITABM1                      source/restart/ddsplit/w_itabm1.F
Chd|        W_ITHFLUX                     source/restart/ddsplit/w_ithflux.F
Chd|        W_IXALOC                      source/restart/ddsplit/w_ixaloc.F
Chd|        W_IXBLOC                      source/restart/ddsplit/w_ixbloc.F
Chd|        W_IXLOC                       source/restart/ddsplit/w_ixloc.F
Chd|        W_JOINT_SMS                   source/constraints/general/cyl_joint/write_count_joint_sms.F
Chd|        W_KNOTLOC                     source/restart/ddsplit/w_knotloc.F
Chd|        W_LACC                        source/restart/ddsplit/w_lacc.F
Chd|        W_LAS                         source/restart/ddsplit/w_las.F
Chd|        W_LGAU                        source/restart/ddsplit/w_lgau.F
Chd|        W_LLINK                       source/restart/ddsplit/w_llink.F
Chd|        W_LSENS                       source/restart/ddsplit/w_lsens.F
Chd|        W_MAD                         source/restart/ddsplit/w_mad.F
Chd|        W_MAIN_PROC_WEIGHT            source/restart/ddsplit/w_master_proc_weight.F
Chd|        W_MONVOL                      source/restart/ddsplit/w_monvol.F
Chd|        W_OUTMAXN                     source/restart/ddsplit/w_outmaxn.F
Chd|        W_PLY_INFO                    source/restart/ddsplit/w_ply_info.F
Chd|        W_PON                         source/restart/ddsplit/w_pon.F
Chd|        W_PORO                        source/restart/ddsplit/w_poro.F
Chd|        W_PXFEM                       source/restart/ddsplit/w_pxfem.F
Chd|        W_R3NLOC                      source/restart/ddsplit/w_r3nloc.F
Chd|        W_RBYK                        source/restart/ddsplit/w_rbyk.F
Chd|        W_RBYMK                       source/restart/ddsplit/w_rbymk.F
Chd|        W_RELOC                       source/restart/ddsplit/w_reloc.F
Chd|        W_RELOC2                      source/restart/ddsplit/w_reloc2.F
Chd|        W_RFILLOC                     source/restart/ddsplit/w_rfilloc.F
Chd|        W_RNLOC                       source/restart/ddsplit/w_rnloc.F
Chd|        W_RNNLOC                      source/restart/ddsplit/w_rnnloc.F
Chd|        W_RSPH                        source/restart/ddsplit/w_rsph.F
Chd|        W_RWALL                       source/restart/ddsplit/w_rwall.F
Chd|        W_RWAR                        source/restart/ddsplit/w_rwar.F
Chd|        W_SEATBELTS                   source/restart/ddsplit/w_seatbelts.F
Chd|        W_SECBUF                      source/restart/ddsplit/w_secbuf.F
Chd|        W_SECTIO                      source/restart/ddsplit/w_sectio.F
Chd|        W_SEGQUADFR                   source/restart/ddsplit/w_segquadfr.F
Chd|        W_SKWSPH                      source/restart/ddsplit/w_skwsph.F
Chd|        W_SKWSUB                      source/restart/ddsplit/w_skwsub.F
Chd|        W_STACK                       source/restart/ddsplit/w_stack.F
Chd|        W_SUBSET_STR                  source/restart/ddsplit/w_subset_str.F
Chd|        W_TABMAT_PROP                 source/restart/ddsplit/w_tabmat_prop.F
Chd|        W_TH                          source/restart/ddsplit/w_th.F 
Chd|        ALEFVM_MOD                    ../common_source/modules/alefvm_mod.F
Chd|        ALE_CONNECTIVITY_MOD          ../common_source/modules/ale_connectivity_mod.F
Chd|        ALE_EBCS_MOD                  ../common_source/modules/ale_ebcs_mod.F
Chd|        CLUSTER_MOD                   share/modules1/cluster_mod.F  
Chd|        DRAPE_MOD                     share/modules1/drape_mod.F    
Chd|        DSGRAPH_MOD                   share/modules1/dsgraph_mod.F  
Chd|        EBCS_MOD                      ../common_source/modules/ebcs_mod.F
Chd|        ELBUFDEF_MOD                  ../common_source/modules/elbufdef_mod.F
Chd|        FAILWAVE_MOD                  ../common_source/modules/failwave_mod.F
Chd|        FRONT_MOD                     share/modules1/front_mod.F    
Chd|        FVBAG_MOD                     share/modules1/fvbag_mod.F    
Chd|        FVMBAG_MESHCONTROL_MOD        ../common_source/modules/fvmbag_meshcontrol_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        INOUTFILE_MOD                 ../common_source/modules/inoutfile_mod.F
Chd|        INT8_MOD                      ../common_source/modules/int8_mod.F
Chd|        INTBUFDEF_MOD                 ../common_source/modules/intbufdef_mod.F
Chd|        INTBUF_FRIC_MOD               share/modules1/intbuf_fric_mod.F
Chd|        INTSTAMP_MOD                  share/modules1/intstamp_mod.F 
Chd|        JOINT_MOD                     share/modules1/joint_mod.F    
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MID_PID_MOD                   share/modules1/mid_pid_mod.F  
Chd|        MONVOL_STRUCT_MOD             share/modules1/monvol_struct_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        NLOCAL_REG_MOD                ../common_source/modules/nlocal_reg_mod.F
Chd|        OUTMAX_MOD                    ../common_source/modules/outmax_mod.F
Chd|        PBLAST_MOD                    ../common_source/modules/pblast_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        SEATBELT_MOD                  ../common_source/modules/seatbelt_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SKEW_MOD                      share/modules1/skew_mod.F     
Chd|        SPLIT_CFD_MOD                 share/modules1/split_cfd_mod.F
Chd|        STACK_MOD                     share/modules1/stack_mod.F    
Chd|        STACK_VAR_MOD                 share/modules1/stack_var_mod.F
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|        XFEM2DEF_MOD                  ../common_source/modules/xfem2def_mod.F
Chd|====================================================================
      SUBROUTINE DDSPLIT(
     1   P            ,CEP          ,CEL            ,IGEO         ,MATPARAM_TAB,
     2   IPM          ,ICODE        ,ISKEW          ,ISKN         ,INSEL       ,
     3   IBCSLAG      ,IPART        ,IPARTS         ,IPARTQ       ,IPARTC      ,
     4   IPARTT       ,IPARTP       ,IPARTR         ,IPARTUR      ,IPARTTG     ,
     5   IPARTX       ,NPC          ,IXUR           ,IXTG         ,GROUP_PARAM_TAB,
     6   IXTG6        ,IXS          ,IXS10          ,IXS20        ,IXS16       ,
     7   IXQ          ,IXC          ,IXT            ,IXP          ,IXR         ,
     8   ITAB         ,ITABM1       ,GJBUFI         ,NALE         ,
     9   ALE_CONNECTIVITY,
     A   KXX          ,IXX          ,IBCL           ,IBFV         ,
     B   LAS          ,LACCELM      ,LSENSOR        ,NNLINK       ,LLLINK      ,
     C   IPARG        ,IGRAV        ,LGRAV          ,IBVEL        ,LBVEL       ,
     D   IACTIV       ,FACTIV       ,KINET          ,IPARI        ,NPRW        ,
     E   LPRW         ,IEXMAD       ,NPBY        ,
     F   LPBY         ,IXRI         ,NSTRF          ,LJOINT       ,PORNOD      ,
     G   MONVOL       ,ICONTACT     ,LAGBUF         ,ITHGRP       ,ITHBUF      ,
     H   FR_IAD       ,X            ,D              ,V            ,VR          ,
     I   DR           ,THKE         ,DAMPR          ,DAMP         ,MS          ,
     J   IN           ,TF           ,PM             ,SKEW         ,XFRAME      ,
     K   GEO          ,EANI         ,BUFMAT         ,BUFGEO       ,BUFSF       ,
     L   BRMPC        ,GJBUFR       ,W              ,VEUL         ,FILL        ,
     M   DFILL        ,WB           ,DSAV           ,ASAV         ,MSNF        ,
     N   SPBUF        ,FAC          ,VEL            ,FSAV         ,FZERO       ,
     O   XLAS         ,ACCELM       ,SENSOR         ,FBVEL        ,GRAVFAC     ,
     P   FR_WAVE      ,FAILWAVE     ,PARTS0         ,USERW        ,ELBUF       ,
     Q   RWL          ,RWSAV        ,RBY            ,RIVET        ,
     R   SECBUF       ,RVOLU        ,RCONX          ,NLOC_DMG     ,
     S   FVMAIN     ,LIBAGALE     ,LENTHG         ,LBUFMAT      ,LBUFGEO     ,
     T   LBUFSF       ,LENXLAS                      ,LNOM_OPT     ,LENLAS      ,           
     U   LENVOLU                    ,NPTS           ,CNE          ,LCNE        ,           
     V   ADDCNE       ,CNI2         ,LCNI2G         ,ADDCNI2      ,CEPI2       ,           
     W   CELI2        ,I2NSNT       ,PROBINT        ,DDSTAT       ,PM1SHF      ,           
     X   DD_IAD       ,LSHSUB       ,DRSAV          ,ARSAV        ,STSAV       ,           
     y   STRSAV       ,FSAVSAV      ,
     Z                 KXSP         ,IXSP           ,NOD2SP       ,CEPSP       ,
     a   NTHWA        ,NAIRWA       ,NMNT           ,L_MUL_LAG1   ,L_MUL_LAG   ,
     b   LWASPIO      ,IPARTSP      ,ISPCOND        ,PM1SPH       ,
     c   WMA          ,
     d   EIGIPM       ,EIGIBUF      ,EIGRPM         ,
     e   IFLOW        ,RFLOW        ,MEMFLOW        ,IEXLNK       ,FASOLFR     ,
     f   ITHGRPA      ,ITHGRPB      ,ITHGRPC        ,ITHGRPD      ,ITHGRPE     ,
     g   ITHGRPF      ,ITHGRPG      ,ITHGRPH        ,ITHGRPI      ,ITHBUFA     ,
     h   ITHBUFB      ,ITHBUFC      ,ITHBUFD        ,ITHBUFE      ,ITHBUFF     ,
     i   ITHBUFG      ,ITHBUFH      ,ITHBUFI        ,IPARTH       ,
     j   FXBIPM       ,FXBRPM       ,FXBNOD         ,FXBMOD       ,FXBGLM      ,
     k   FXBCPM       ,FXBCPS       ,FXBLM          ,FXBFLS       ,FXBDLS      ,
     l   FXBDEP       ,FXBVIT       ,FXBACC         ,FXBELM       ,FXBSIG      ,
     m   FXBGRVI      ,FXBGRVR      ,IADLL          ,LLL          ,IBMPC       ,
     n   LAMBDA       ,LRBAGALE     ,ISKWP          ,NSKWP        ,ISENSP      ,
     o   NSENSP       ,IACCP        ,NACCP          ,IPART_STATE  ,MCP         ,
     p   TEMP         ,UNITAB       ,INTSTAMP       ,IFRAME       ,CLUSTERS    ,
     q                 PARTSAV      ,IBFT           ,FBFT         ,IBCV        ,
     r   FCONV        ,IRBE3        ,LRBE3          ,FRBE3        ,FRONT_RM    ,
     s   RBYM         ,IRBYM        ,LCRBYM         ,INOISE       ,FNOISE      ,
     t   MS0          ,ADMSMS       ,NOM_SECT       ,ISPSYM       ,
     u   SH4TREE      ,SH3TREE      ,IPADMESH       ,IBFFLUX      ,FBFFLUX     ,
     v   SH4TRIM      ,SH3TRIM      ,PADMESH        ,MSC          ,MSTG        ,
     w   INC          ,INTG         ,PTG            ,MCPC         ,MCPTG       ,
     x   RCONTACT     ,ACONTACT     ,PCONTACT       ,MSCND        ,INCND       ,
     y   MSSA         ,MSTR         ,MSP            ,MSRT         ,IBCR        ,
     z   FRADIA       ,DMELC        ,DMELTG         ,DMELS        ,DMELTR      ,
     1   DMELP        ,DMELRT       ,RES_SMS        ,              ISPHIO      ,
     2   LPRTSPH      ,LONFSPH      ,VSPHIO         ,SPHVELN      ,ALPH        ,
     3   IFILL        ,IMS          ,IRBE2          ,LRBE2        ,
     8   MS_PLY       ,
     9   ZI_PLY       ,INOD_PXFEM   ,IEL_PXFEM      ,ICODPLY      ,ISKWPLY     ,
     A   ADDCNE_PXFEM ,CNE_PXFEM    ,CEL_PXFEM      ,ITHVAR       ,XDP,TABLE   ,
     B   CELSPH       ,ICFIELD      ,LCFIELD        ,CFIELD       ,USERNODS    ,IUSERW   ,
     C   SUSERW       ,SIUSERW      ,MSZ2           ,ITASK        ,DIAG_SMS    ,
     D   ILOADP       ,LLOADP       ,LOADP,
     E   INOD_CRKXFEM ,IEL_CRKXFEM  ,ADDCNE_CRKXFEM ,CNE_CRKXFEM  ,CEL_CRKXFEM ,
     F   IBUFSSG_IO   ,INTERCEP     ,IBORDNODE      ,IEDGESH      ,IBORDEDGE   ,
     G   LINALE       ,NODEDGE,IEDGE,CEP_CRKXFEM    ,IEDGE_TMP    ,
     H   CRKNODIAD    ,ELBUF_TAB    ,NOM_OPT        ,LGAUGE       ,GAUGE       ,
     I   IGAUP        ,NGAUP        ,NODLEVXF       ,FRONTB_R2R   ,DFLOW       ,
     J   VFLOW        ,WFLOW        ,SPH2SOL        ,SOL2SPH      ,IRST        ,
     K   ELCUTC       ,NODENR       ,KXFENOD2ELC    ,ENRTAG       ,INTBUF_TAB,
     M   I11FLAG      ,XFEM_TAB     ,LENTHGR        ,RTHBUF       ,
     N   IXIG3D       ,KXIG3D       ,KNOT           ,IPARTIG3D    ,WIGE        ,
     O   NCRKPART     ,INDX_CRK     ,CRKLVSET       ,CRKSHELL     ,CRKSKY      ,
     P   CRKAVX       ,CRKEDGE      ,FBSAV6         ,TABSENSOR    ,SENSOR_TAB  ,
     Q   STACK        ,XFEM_PHANTOM ,T8             ,TAB_UMP      ,POIN_UMP    ,
     R   SOL2SPH_TYP  ,ADDCSRECT    ,CSRECT         ,DRAPE        ,
     S   ITAGND        ,ICNDS10      ,ADDCNCND     ,
     T   CEPCND       ,CELCND       ,CNCND          ,NATIV_SMS    ,I24MAXNSNE  ,
     U   MULTI_FVM    ,SEGQUADFR    ,INTBUF_FRIC_TAB,SUBSET       ,IGRNOD      ,
     V   IGRBRIC      ,IGRQUAD      ,IGRSH4N        ,IGRSH3N      ,IGRTRUSS    ,
     W   IGRBEAM      ,IGRSPRING    ,IGRPART        ,IGRSURF      ,IGRSLIN     ,
     X   POIN_PART_SHELL,POIN_PART_TRI,POIN_PART_SOL,MID_PID_SHELL,MID_PID_TRI ,
     Y   MID_PID_SOL  , TAG_NM      ,NINDX_NM       ,INDX_NM      ,TAG_SCRATCH ,
     Z   NINDX_SCRT   , INDX_SCRT   ,FLAG_24_25     ,NUMNOD_L     ,TAG_SKN     ,
     A   MULTIPLE_SKEW, IGRSURF_PROC,KNOTLOCPC      ,KNOTLOCEL    ,ALE_ELM,
     B   SIZE_ALE_ELM ,PINCH_DATA  ,TAG_SKINS6      ,IBCSCYC      ,LBCSCYC  ,T_MONVOL,
     C INDX_S,INDX_Q,INDX_TG,FACE_ELM_S,FACE_ELM_Q,FACE_ELM_TG,NBR_TH_MONVOL, EBCS_TAB,
     D   KLOADPINTER  , LOADPINTER ,DGAPLOADINT     ,S_LOADPINTER, LEN_CEP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE DSGRAPH_MOD
      USE FVBAG_MOD
      USE INTSTAMP_MOD
      USE TABLE_MOD
      USE ELBUFDEF_MOD
      USE MESSAGE_MOD
      USE FRONT_MOD
      USE CLUSTER_MOD
      USE INTBUFDEF_MOD
      USE XFEM2DEF_MOD
      USE STACK_MOD
      USE ALEFVM_MOD
      USE INT8_MOD
      USE FVMBAG_MESHCONTROL_MOD
      USE MULTI_FVM_MOD
      USE STACK_VAR_MOD
      USE INTBUF_FRIC_MOD
      USE PBLAST_MOD
      USE GROUPDEF_MOD
      USE MID_PID_MOD
      USE FAILWAVE_MOD
      USE NLOCAL_REG_MOD
      USE SKEW_MOD
      USE MATPARAM_DEF_MOD
      USE SPLIT_CFD_MOD
      USE PINCHTYPE_MOD
      USE INOUTFILE_MOD
      USE ALE_CONNECTIVITY_MOD
      USE MONVOL_STRUCT_MOD
      USE DRAPE_MOD
      USE SENSOR_MOD
      USE ALE_EBCS_MOD
      USE EBCS_MOD
      USE JOINT_MOD
      USE SEATBELT_MOD
      USE OUTMAX_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com02_c.inc"
#include      "com03_c.inc"
#include      "com04_c.inc"
#include      "com05_c.inc"
#include      "com10_c.inc"
#include      "com_xfem1.inc"
#include      "eigcom.inc"
#include      "flowcom.inc"
#include      "fxbcom.inc"
#include      "intstamp_c.inc"
#include      "lagmult.inc"
#include      "param_c.inc"
#include      "ale_param.inc"
#include      "rnur_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr15_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "scr23_c.inc"
#include      "sms_c.inc"
#include      "sphcom.inc"
#include      "sysunit.inc"
#include      "tabsiz_c.inc"
#include      "tablen_c.inc"
#include      "remesh_c.inc"
#include      "submod_c.inc"
#include      "r2r_c.inc"
#include      "elbuf_c.inc"
#include      "inter18.inc"
#include      "inter22.inc"
#include      "alefvm.inc"
#include      "drape_c.inc"
#include      "thermal_c.inc"
#include      "debug_rst.inc"
C-----------------------------------------------
      TYPE INTERMASURFEP
        INTEGER, DIMENSION(:), POINTER :: P
      END TYPE INTERMASURFEP
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      INTEGER, INTENT(IN) :: LEN_CEP ! size of CEP
      INTEGER P,
     .        LIBAGALE, LENTHG, LENLAS, NPTS,LEN,
     .        LBUFMAT, LBUFGEO, LBUFSF, LENXLAS, LNOM_OPT,
     .        LENVOLU, NTHWA, NAIRWA, NMNT, I2NSNT,
     .        L_MUL_LAG1, L_MUL_LAG, LWASPIO,PM1SPH,
     .        LCNI2G, PM1SHF, I11FLAG,LENTHGR,NCRKPART,
     .        CEP(LEN_CEP),CEL(*),IGEO(*), IPM(*),
     .        ICODE(*),ISKEW(*),ISKN(*),INSEL(*),IBCSLAG(*),
     .        IPART(*),IPARTS(*),IPARTQ(*),IPARTC(*),IPARTT(*),
     .        IPARTP(*),IPARTR(*),IPARTUR(*),IPARTTG(*),IPARTX(*),
     .        NPC(*),IXUR(*),IXTG(NIXTG,*),IXTG6(4,*),
     .        IXS(NIXS,*),IXS10(6,*),IXS20(12,*),IXS16(8,*),
     .        IXQ(NIXQ,*),IXC(*),IXT(*),IXP(*),IXR(*),
     .        ITAB(*),ITABM1(*),GJBUFI(*),
     .        NALE(*),IEXMAD(*),
     .        KXX(*),IXX(*),IBCL(*),IBFV(*),LAS(*),
     .        LACCELM(*),LSENSOR(*),NNLINK(*),LLLINK(*),
     .        IPARG(*),IGRAV(*),LGRAV(*),
     .        IBVEL(*),LBVEL(*),IACTIV(*),KINET(*),
     .        IPARI(*),NPRW(*),LPRW(*),
     .        NPBY(*),LPBY(*), IEXLNK(*),
     .        IXRI(*),NSTRF(*),LJOINT(*),PORNOD(*),MONVOL(*),FVMAIN(*),
     .        LAGBUF(*),ITHGRP(*),ITHBUF(*), FR_IAD(*),
     .        ICONTACT(*), IPARTSP(*), ISPCOND(*),
     .        DDSTAT(50), CNE(*), LCNE, ADDCNE(0:*),
     .        CNI2(*), ADDCNI2(0:*), CEPI2(*), CELI2(*),
     .        DD_IAD(*), LSHSUB(*),
     .        KXSP(*), IXSP(*), NOD2SP(*), CEPSP(*),
     .        EIGIPM(*), EIGIBUF(*), IFLOW(*), FASOLFR(2,*),
     .        IPARTH(*),
     .        ITHGRPA(*),ITHGRPB(*),ITHGRPC(*),ITHGRPD(*),ITHGRPE(*),
     .        ITHGRPF(*),ITHGRPG(*),ITHGRPH(*),ITHGRPI(*),ITHBUFA(*),
     .        ITHBUFB(*),ITHBUFC(*),ITHBUFD(*),ITHBUFE(*),ITHBUFF(*),
     .        ITHBUFG(*),ITHBUFH(*),ITHBUFI(*), FXBIPM(*), FXBNOD(*),
     .        FXBELM(*), FXBGRVI(*), IADLL(*), LLL(*),IBMPC(*),LRBAGALE,
     .        ISKWP(*), NSKWP(*), ISENSP(2,*), NSENSP(*),
     .        IACCP(P), NACCP(*), IPART_STATE(*), IFRAME(*),
     .        IBFT(*),IBCV(*),IRBE3(NRBE3L,*),LRBE3(*),
     .        IRBYM(*) ,LCRBYM(*),FRONT_RM(NRBYM,*),INOISE(*),
     .        NOM_SECT(*), ISPSYM(*), SH4TREE(*), SH3TREE(*),
     .        IPADMESH(*), SH4TRIM(*), SH3TRIM(*), IBCR(*), IBFFLUX(*),
     .        ISPHIO(*), LPRTSPH(*), LONFSPH(*), IFILL(*), IMS(*),
     .        IRBE2(NRBE2L,*),LRBE2(*),INOD_PXFEM(*) ,
     .        IEL_PXFEM(*) ,ICODPLY(*),ISKWPLY(*) ,ADDCNE_PXFEM(0:*),
     .        CNE_PXFEM(*),CEL_PXFEM(*),ITHVAR(*), CELSPH(NUMSPH),
     .        ICFIELD(*),LCFIELD(*),USERNODS(*),SUSERW,SIUSERW,
     .        IUSERW(*),ILOADP(*),LLOADP(*),INOD_CRKXFEM(*),
     .        IEL_CRKXFEM(*),ADDCNE_CRKXFEM(0:*),CNE_CRKXFEM(*),
     .        CEL_CRKXFEM(*),IBORDNODE(*),IEDGESH(*),IBORDEDGE(*),
     .        LINALE(*),NODEDGE(2,*),IEDGE(*),CEP_CRKXFEM(*),
     .        IEDGE_TMP(3,*),CRKNODIAD(*), LGAUGE(*), IGAUP(*),NGAUP(*),
     .        NODLEVXF(*),FRONTB_R2R(SFRONTB_R2R,*),
     .        SPH2SOL(*), SOL2SPH(*), IRST(*),ELCUTC(2,*),NODENR(*),
     .        KXFENOD2ELC(*),ENRTAG(*),IXIG3D(*),KXIG3D(*),
     .        IPARTIG3D(*),INDX_CRK(*),TABSENSOR(STABSEN),
     .        POIN_UMP(NUMMAT),SOL2SPH_TYP(*), ADDCSRECT(*), CSRECT(*),
     .        ITAGND(*),ICNDS10(*),
     .        ADDCNCND(0:*), CEPCND(*), CELCND(*),CNCND(*),NATIV_SMS(*),
     .        I24MAXNSNE,SEGQUADFR(2,*),TAG_SKINS6(*),IBCSCYC(*),LBCSCYC(*)
      INTEGER, INTENT(IN) :: S_LOADPINTER
      INTEGER, INTENT(IN) ::  KLOADPINTER(NINTER+1)  ,LOADPINTER(S_LOADPINTER)
      my_real , INTENT(IN) :: DGAPLOADINT(S_LOADPINTER ) 
      INTEGER NOM_OPT(LNOPT1,*)
      INTEGER(KIND=8) :: MEMFLOW(*)
      INTEGER, DIMENSION(2,NPART), INTENT(IN) :: POIN_PART_SHELL,POIN_PART_TRI
      INTEGER, DIMENSION(2,NPART,7), INTENT(IN) :: POIN_PART_SOL
      INTEGER :: NBR_TH_MONVOL ! number of /TH/MONV

      DOUBLE PRECISION
     .        BUFGEO(*)
      my_real
     .       X(*), D(*), V(*), VR(*), DR(*),
     .       THKE(*), DAMPR(*), DAMP(*), MS(*), IN(*),
     .       TF(*), PM(*), SKEW(*), XFRAME(*), GEO(*),
     .       EANI(*), BUFMAT(*),  BUFSF(*), BRMPC(*),
     .       GJBUFR(*), W(*), VEUL(*), FILL(*), DFILL(*),
     .       WB(*), ASAV(*), DSAV(*), MSNF(*),
     .       SPBUF(*), FAC(*), VEL(*), FACTIV(*),
     .       FSAV(*), FZERO(*), XLAS(*), ACCELM(*), SENSOR(*),
     .       FBVEL(*), GRAVFAC(*), FR_WAVE(*), USERW(*), ELBUF(*),
     .       RWL(*), RWSAV(*), RBY(*), RIVET(*),
     .       SECBUF(*), RVOLU(*), PARTS0(*), RCONX(*),
     .       DRSAV(*), ARSAV(*), STSAV(*), STRSAV(*), FSAVSAV(*),
     .       WMA(*), EIGRPM(*), RFLOW(*),
     .       PROBINT, FXBRPM(*), FXBMOD(*), FXBGLM(*), FXBCPM(*),
     .       FXBCPS(*), FXBLM(*), FXBFLS(*), FXBDLS(*), FXBDEP(*),
     .       FXBVIT(*), FXBACC(*), FXBSIG(*), FXBGRVR(*), LAMBDA(*),
     .       MCP(*), TEMP(*),  PARTSAV(*),FBFT(*), FCONV(*),
     .       FRBE3(*), RBYM(*),FNOISE(*), MS0(*), ADMSMS(*),
     .       PADMESH(*) ,MSC(*), MSTG(*),
     .       INC(*), INTG(*), PTG(*), MCPC(*), MCPTG(*),
     .       RCONTACT(*), ACONTACT(*), PCONTACT(*), MSCND(*), INCND(*),
     .       MSSA(*), MSTR(*), MSP(*), MSRT(*), FRADIA(*), FBFFLUX(*),
     .       DMELC(*), DMELTG(*), DMELS(*), DMELTR(*), DMELP(*),
     .       DMELRT(*), RES_SMS(*), VSPHIO(*), SPHVELN(*), ALPH(*),
     .       MS_PLY(*),ZI_PLY(*),CFIELD(*),MSZ2(*),
     .       DIAG_SMS(*),LOADP(*),GAUGE(*),DFLOW(*),VFLOW(*),WFLOW(*),
     .       RTHBUF(*),KNOT(*),WIGE(*),TAB_UMP(7,TAILLE),
     .       KNOTLOCPC(*),KNOTLOCEL(*)
      DOUBLE PRECISION, DIMENSION(12,6,SFSAV6) :: FBSAV6
      TYPE (INTSTAMP_DATA) INTSTAMP(*)
      TYPE (TTABLE) TABLE(*)
      TYPE (INTBUF_STRUCT_) INTBUF_TAB(*)
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP)   :: ELBUF_TAB
      TYPE (ELBUF_STRUCT_), DIMENSION(NGROUP,*) :: XFEM_TAB
      TYPE (CLUSTER_)     , DIMENSION(NCLUSTER) :: CLUSTERS
      TYPE (XFEM_SHELL_)  , DIMENSION(NLEVMAX)  :: CRKSHELL
      TYPE (XFEM_LVSET_)  , DIMENSION(NLEVMAX)  :: CRKLVSET
      TYPE (XFEM_SKY_)    , DIMENSION(NLEVMAX)  :: CRKSKY
      TYPE (XFEM_AVX_)    , DIMENSION(NLEVMAX)  :: CRKAVX
      TYPE (XFEM_EDGE_)   , DIMENSION(NXLAYMAX) :: CRKEDGE
      TYPE (XFEM_PHANTOM_), DIMENSION(NXLAYMAX) :: XFEM_PHANTOM
      TYPE (STACK_PLY) :: STACK
      TYPE (INT8_STRUCT_) :: T8(NSPMD,*)
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      TYPE(INTBUF_FRIC_STRUCT_) :: INTBUF_FRIC_TAB(*)
      TYPE(MID_PID_TYPE), DIMENSION(NUMMAT), INTENT(IN) :: MID_PID_SHELL,MID_PID_TRI
      TYPE(MID_PID_TYPE), DIMENSION(NUMMAT,7), INTENT(IN) :: MID_PID_SOL
      TYPE (FAILWAVE_STR_) :: FAILWAVE
      TYPE (NLOCAL_STR_)   :: NLOC_DMG
      TYPE (DRAPE_)   :: DRAPE (*)
      TYPE (SENSOR_STR_) ,DIMENSION(*) :: SENSOR_TAB
C-----------------------------------------------
      TYPE (SUBSET_) , DIMENSION(NSUBS)   :: SUBSET
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE (GROUP_)  , DIMENSION(NGRBRIC) :: IGRBRIC
      TYPE (GROUP_)  , DIMENSION(NGRQUAD) :: IGRQUAD
      TYPE (GROUP_)  , DIMENSION(NGRSHEL) :: IGRSH4N
      TYPE (GROUP_)  , DIMENSION(NGRSH3N) :: IGRSH3N
      TYPE (GROUP_)  , DIMENSION(NGRTRUS) :: IGRTRUSS
      TYPE (GROUP_)  , DIMENSION(NGRBEAM) :: IGRBEAM
      TYPE (GROUP_)  , DIMENSION(NGRSPRI) :: IGRSPRING
      TYPE (GROUP_)  , DIMENSION(NGRPART) :: IGRPART
      TYPE (SURF_)   , DIMENSION(NSURF)   :: IGRSURF
      TYPE (SURF_)   , DIMENSION(NSLIN)   :: IGRSLIN
      TYPE (PINCH) :: PINCH_DATA

      LOGICAL, INTENT(IN) :: FLAG_24_25
      INTEGER, INTENT(IN) :: NUMNOD_L
      INTEGER, INTENT(INOUT) :: NINDX_NM,NINDX_SCRT
      INTEGER, DIMENSION(*), INTENT(INOUT) :: INDX_NM,INDX_SCRT
      INTEGER, DIMENSION(*) :: TAG_NM,TAG_SCRATCH
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
!       NUMNOD_L : integer, number of local element
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
        INTEGER, DIMENSION(NUMSKW+1), INTENT(INOUT) :: TAG_SKN
        TYPE(SKEW_TYPE), DIMENSION(NUMSKW+1), INTENT(INOUT) :: MULTIPLE_SKEW
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       TAG_SKN : integer ; dimension=NUMSKW+1
!                 tag array --> tag the i SKEW if a SPRING uses it
!                 tag array=0 --> the SKEW is not used by a SPRING
!                 tag array=1 --> the SKEW is used by one SPRING
!                 tag array>1 --> the SKEW is used by several SPRING
!       MULTIPLE_SKEW : SKEW_TYPE ; dimension=NUMSKW+1
!                       MULTIPLE_SKEW(I)%PLIST(:) is a list of processor
!                       where the SKEW is stuck
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
        TYPE(SURF_), DIMENSION(NSURF,NSPMD), INTENT(IN) :: IGRSURF_PROC
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       IGRSURF_PROC : SURF_ ; dimension=NSURF*NSPMD
!                 local surface property array (=IGRSURF for each proc)
!                 %ELTYP --> type of element (shell, triangle...)
!                 %ELEM  --> element id
!                 %NSEG --> total element number
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
        INTEGER, INTENT(IN) :: SIZE_ALE_ELM
        TYPE(split_cfd_type), INTENT(IN) :: ALE_ELM
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       SIZE_ALE_ELM : integer ; dimension=NSPMD ; size of ALE_ELM%SOL_ID array
!       ALE_ELM : split_cfd_type ; dimension=NSPMD ; solid element ID used
!                 during the domain splitting (ALE part)
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
        TYPE(MONVOL_STRUCT_), DIMENSION(NVOLU), INTENT(IN) :: T_MONVOL


        INTEGER, DIMENSION(*), INTENT(in) :: INDX_S
        INTEGER, DIMENSION(*), INTENT(in) :: INDX_Q
        INTEGER, DIMENSION(*), INTENT(in) :: INDX_TG
        INTEGER, DIMENSION(6*NUMELS,*), INTENT(in) :: FACE_ELM_S
        INTEGER, DIMENSION(4*NUMELQ,*), INTENT(in) :: FACE_ELM_Q
        INTEGER, DIMENSION(3*NUMELTG,*), INTENT(in) :: FACE_ELM_TG
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
!       INDX_xxx : integer ; dimension=NUMELxxx ; index for the surface
!                  of the remote connected element
!       FACE_ELM_xxx : integer ; dimension=(6/4/3*NUMELxxx,2) ; surface
!                  of the remote connected element
!       -*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-**-*-*-*-*-*-*-*-*-*-*-*-*
        TYPE(t_ebcs_tab), INTENT(INOUT) :: EBCS_TAB
C-----------------------------------------------
C   F u n c t i o n
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IR_, IM,IFILNAM(2148),IFILNAM2(2148),
     .        NUMELS_L,NUMELS8_L,NUMELS10_L,NUMELS20_L,NUMELS16_L,
     .        NUMELQ_L ,NUMELC_L, NCONLD_L,NSKYLL_L,
     .        NUMELT_L,NUMELP_L,NUMELR_L,NUMELUR_L,NUMELTG_L,
     .        NUMELTG3,NUMELTG3_L,NUMELTG6_L,NUMELX_L,NUMPOR_L,
     .        NSVOIS,NQVOIS,NTGVOIS,NE_NSVOIS,NE_NQVOIS,NE_NTGVOIS,NRCVVOIS,NSNDVOIS,ILAW11,LJOINT_L,
     .        NSEGFL_L,NGROUP_L, NSTRF_L,
     .        NSLAW_L, NESLAW_L, LWSAV_L, NSLARB_L,
     .        LBUFEL_L, LENWA_L, INTBAG_L, ISUMNX_L, NUMEL_L,
     .        NLLINK_L, LLGRAV_L, LLBVEL_L, NFXVEL_L, NSHFRONT_L, OFF1,
     .        OFF, SHF, IUN, NUMEL, ISP0, NBDDACC, NBDDKIN, NBDDNOD,
     .        NBDDPROC, NBDDBOUN, NBDDNRB, NRBYKIN_L, NSKYRW_L,
     .        NSKYRBK_L, LENTHGT, NBDDI2M, NNMV_L, NNMVC_L, NBDDNCJ,
     .        LCNE_L, NISKY0_L, LCNI2_L, I2NSN_L, NISKYI2_L, NIR,
     .        NSKYRBM_L,NBDDNRBM, LSECBUF_L, NNODT_L, NNODL_L,
     .        NMADSH4_L, NMADSH3_L,NMADSOL_L,NMADNOD_L,MADCL_NMADNOD_L,
     .        IMAXIMP_L, I2NSN25_L,
     .        NUMSPH_L, LWASPH_L, NBI18_L, NSKYI18_L,
     .        NSNT_L,NMNT_L,NSNT2_L,NMNT2_L,
     .        NFASOLFR_L, NLAGF_L, PROC, INUM, IDDL, ISKW,
     .        LEN_IA, LEN_AM, I_EVAL,R_EVAL,LSKYI, NCMAX,
     .        NERVOIS, NESVOIS,NE_NERVOIS, NE_NESVOIS, NBCFD, NUMEL_L2,
     .        DD_RBY2(3,NRBYKIN),  DD_RBM2(3,NIBVEL),
     .        EIGIPM_L(NEIPM,NEIG), LEIBUF_L, EIGIBUF_L(LEIBUF),
     .        IFLOW_L(LIFLOW), DD_LAGF(3,NSPMD+1),MY_ILEN,MY_RLEN,
     .        SKIPPON_L,LLRBE3_L,NSKYRBE3_L,
     .        NRBE3_L,NBDDRBE3M,NSLARBM_L,NSKYRBMK,
     .        NRBYMKIN_L ,NBDDNRBYM,NSKTRBMK_L,NRBYMK_L,
     .        DD_RBYM2(3,NRBYM),NSKYRBMK_L, LAG_SEC,
     .        NRBE2_L,LLRBE2_L,NBDDRBE2,NUMELCPXFE_L,NUMNODPXFE_L,
     .        ITAG_PXFEM(NPLYXFE),INDEX_PXFEM(NPLYXFE),
     .        LCNEPXFEM_L,LLCFIELD_L,SFR_USERWI_L,SUSERW_L,SIUSERW_L,
     .        ITASK,LLLOADP_L,NUMELCRKXFE_L,NUMNODCRKXFE_L,LCNECRKXFEM_L,
     .        NUMELCCRKXFE_L,NUMELTGCRKXFE_L,
     .        NUMEDGES_L,NBDDEDGE_L,
     .        SDD_R2R_ELEM,NOD_XFE_L,
     .        NSPHSOL_L,FIRST_SPHSOL_L,NCLUSTER_L,IXEL,FLAG_ALLOCATE,
     .        FLAG_XFEM,NUMELIG3D_L,ISUMNIG3D_L, NUMNOR_L, NBDDNORT_L,
     .        NBDDNOR_MAX_L, NBCCFR25_L, NBCCNOR_L, NBDDEDGT_L, NBDDEDG_MAX_L, NRTMX25_L,
     .        NBDDCNDM,NS10E_L,LCNCND_L,
     .        NSEGQUADFR_L ,LINTFRIC, SIZ,LENIGRNOD_L,
     .        LENIGRBRIC_L,LENIGRQUAD_L,LENIGRSH4N_L,
     .        LENIGRTRUS_L,LENIGRBEAM_L,LENIGRSPRI_L,
     .        LENIGRSH3N_L,LENISURF_L,LENISLIN_L,NBCSCYC_L,LLBCSCYC_L,NS_DIFF,
     .        N_SLIPRING_L,N_RETRACTOR_L,N_ANCHOR_REMOTE_L,N_ANCHOR_REMOTE_SEND_L,
     .        N_SEATBELT_L,N_SEATBELT_2D_L
      INTEGER IPARI_L(NPARI,MAX(1,NINTER))
      INTEGER NCONV_L,NFXTEMP_L,NRADIA_L,NFXFLUX_L
      INTEGER , DIMENSION(:), ALLOCATABLE:: NUMLOCGROUP,NUMLOCCLUSTER
      INTEGER, DIMENSION(:),ALLOCATABLE :: NODLOCAL,IEPXFEM_L,INPXFEM_L,
     .        IELXFEMC_L,INCRKXFEM_L,ELCUTC_L,ELCUTTG_L,NODENR_L,
     .        KXFENOD2ELC_L,ENRTAG_L,
     .        IELXFEMTG_L,INDEX_CRKXFEM,NODLEVXF_L, NODLOCAL_MGM
      INTEGER, DIMENSION(:),ALLOCATABLE :: NODGLOB, IDGLOB_L, UIDGLOB_L,
     .        IXSF, IXQF, IXTGF, ITABI2M, ITABRBE3M, ITABRBE2M
      ! for parith/on + FVM solver : ID_GLOBAL_VOIS = user id ; FACE_VOIS= id of the remote connected element
      INTEGER, DIMENSION(:), ALLOCATABLE :: ID_GLOBAL_VOIS,FACE_VOIS
      INTEGER, DIMENSION(:),ALLOCATABLE :: ITABCNDM
      INTEGER LENNOD_L, LENMOD_L, LENELM_L, LENSIG_L,
     .        FXBIPM_L(NBIPM,NFXBODY), LENGLM_L, LENCP_L, LENLM_L,
     .        LENFLS_L, LENDLS_L, LENMCD_L, LENGRVI_L, LENGRVR_L
      INTEGER LINTSTAMP, LTABLE
      INTEGER, DIMENSION(:), ALLOCATABLE :: FXBNOD_L, FXBELM_L,
     .                                      FXBGRVI_L, MONVOL_L
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: FXBMOD_L, FXBSIG_L,
     .                                      FXBGLM_L, FXBCPM_L,
     .                                      FXBCPS_L, FXBLM_L,
     .                                      FXBFLS_L, FXBDLS_L,
     .                                      FXBGRVR_L
      my_real
     .      RDPARTSAV(NPSAV*NPART)
      my_real
     .       , DIMENSION(:), ALLOCATABLE :: FORNEQS

      CHARACTER*100 FILNAM
      CHARACTER*4 PROCNAM,CHRUN
      INTEGER NUM16SHIFT_L,SPHSHIFT_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: ISKWP_L
C SPH Modif Structures
      INTEGER ,DIMENSION(:,:), ALLOCATABLE :: SPH_SEND_ARR,SPH_RECV_ARR
      INTEGER ,DIMENSION(:), ALLOCATABLE :: SPHLOCAL
      INTEGER RECV_COUNT(NSPMD),SEND_COUNT(NSPMD)
      INTEGER, DIMENSION(:), ALLOCATABLE :: IUSERW_L,IEDGECRK_L,
     .       IBORDEDGE_L,NODEDGE_L,IEDGESH4_L,IEDGESH3_L,TAGEDGE,
     .       IEDGE_L,EDGELOCAL,EDGEGLOBAL

c SPH inlet spmd structure
      INTEGER SLONFSPH_L,SLPRTSPH_L,SSPHVELN_L
      INTEGER IBUFSSG_IO(SIBUFSSG_IO)
      my_real, DIMENSION(:), ALLOCATABLE :: RBUFGEO
      my_real
     .      MSNDP(NSPMD)
      INTEGER :: LEN_TMP_NAME
      CHARACTER(len=4096) :: TMP_NAME
C-----------------------------------------------
C structure de fichier v44 a modifier
C-----------------------------------------------
      INTEGER LHEADER,LVARINT,LVARREA,LMXVINT,LMXVREA,LEN_G,LEN_M,
     .        LEN_S,LEN_ADM,LEN_IG,LEN_PM
      PARAMETER (LHEADER=15)
      PARAMETER (LMXVINT=4000)
      PARAMETER (LMXVREA=1000+30*MAXLAW+30)
      INTEGER   TABHEAD(LHEADER),TABVINT(LMXVINT)
      my_real
     .   TABVREA(LMXVREA)
      DOUBLE PRECISION XDP(*)

      TYPE(INTERSURFP) :: INTERCEP(3,NINTER)
      INTEGER, DIMENSION(:),ALLOCATABLE :: NODGLOBXFE

      TYPE(INTBUF_STRUCT_),DIMENSION(:),ALLOCATABLE :: INTBUF_TAB_L
      TYPE(MATPARAM_STRUCT_)  , DIMENSION(NUMMAT)   :: MATPARAM_TAB
      TYPE(GRP_PARAM_STRUCT_) , DIMENSION(NGROUP)   :: GROUP_PARAM_TAB
C-----------------------------------------------
C     For Meshgems
C-----------------------------------------------
      INTEGER :: MAX_ID
C-----------------------------------------------
C     EBCS
C-----------------------------------------------
      INTEGER :: IEBCSLGTH, IEBCS_NELEM_L(NEBCS), IEBCS_TYPE(NEBCS)
      INTEGER, DIMENSION(:), ALLOCATABLE ::
     .     IEBCS_LISTELEM_L,
     .     IEBCS_LISTFAC_L
      my_real, DIMENSION(:), ALLOCATABLE ::    IEBCS_LISTDP0_L
C-----------------------------------------------
C     Multifluid law
C-----------------------------------------------
      my_real, DIMENSION(:,:), ALLOCATABLE :: MULTI_FVM_VEL_L

C-----------------------------------------------
C     FILE SIZE
C-----------------------------------------------
      INTEGER (KIND=8) :: FILE_SIZE
      real(kind=8) :: SECS
C
      INTEGER NPINCH_L
      TYPE(t_ale_connectivity), INTENT(INOUT) :: ALE_CONNECTIVITY
      INTEGER, DIMENSION(:), ALLOCATABLE :: ELEMID_L
      INTEGER, DIMENSION(:), ALLOCATABLE :: NE_LERCVOIS  !(NE_NERVOIS)
      INTEGER, DIMENSION(:), ALLOCATABLE :: NE_LESDVOIS  !(NE_NESVOIS)
      INTEGER :: II, JJ, NEBCS_FVM, IPT, ITMP
      INTEGER :: NEBCS_NRF,OTHER_NEBCS
      INTEGER, DIMENSION(NEBCS) :: LIST_EBCS_NRF,LIST_OTHER_EBCS
      TYPE(t_ebcs_tab) :: EBCS_TAB_LOC,EBCS_TAB_LOC_2
      TYPE(t_connectivity_ext1) :: ee_connect_l
C-----------------------------------------------
      TYPE(SEATBELT_REMOTE_NODES_STRUCT) ANCHOR_REMOTE_L,ANCHOR_REMOTE_SEND_L
C=======================================================================
C ALLOCATION
C ----------
      IF (MULTI_FVM%IS_USED) THEN
         ALLOCATE(MULTI_FVM_VEL_L(3, NUMELS + NUMELQ + NUMELTG))
      ELSE
         ALLOCATE(MULTI_FVM_VEL_L(1,1))
      ENDIF


      ALLOCATE(NODLOCAL(NUMNOD))

      IF (TETRAMESHER_USED) ALLOCATE(NODLOCAL_MGM(NB_TOTAL_NODE))
      ALLOCATE(IEPXFEM_L(NUMELC))
      ALLOCATE(INPXFEM_L(NUMNOD))
      ALLOCATE(IELXFEMC_L(NUMELC))
      ALLOCATE(IELXFEMTG_L(NUMELTG))
      ALLOCATE(INCRKXFEM_L(NUMNOD))
      ALLOCATE(NUMLOCGROUP(NGROUP)) !NUMLOCGROUP(NG) = NG_LOC or 0
      ALLOCATE(NUMLOCCLUSTER(NCLUSTER)) !NUMLOCCLUSTER(I) = NG_LOC or 0
      NUMLOCCLUSTER(1:NCLUSTER) = 0
      ALLOCATE(ISKWP_L(NUMSKW+1))
C--------------------------------------------
C Allocate and init MONVOL local
C--------------------------------------------
      ALLOCATE(MONVOL_L(LENVOLU))
      DO I = 1, LENVOLU
        MONVOL_L(I) = MONVOL(I)
      END DO
C--------------------------------------------
      IUN = 1
      ISP0 = 0
      IF(P==1) ISP0 = 1
C--------------------------------------------
C Init Frontiere LAG MULT
C--------------------------------------------
      DO PROC = 1, NSPMD+1
       DD_LAGF(1,PROC) = 0
       DD_LAGF(2,PROC) = 0
      END DO
C--------------------------------------------
C Tableaux numeration nodale
C--------------------------------------------
      IF(NUMNOD_L>0) ALLOCATE(NODGLOB(NUMNOD_L))
      CALL F_NODLOC2(NUMNOD,P  ,NODGLOB,NODLOCAL,IGEO ,
     2               IPARG,IXS   ,IXQ,IXC    ,IXTG    ,
     3               IXT  ,IXP   ,IXR,GEO    ,NUMNOD_L)
C--------------------------------------
C count local NGROUP 
C--------------------------------------
      CALL C_NGROUP(IPARG,P-1,NUMLOCGROUP,NGROUP_L)
C--------------------------------------
C count LSHSUB
C--------------------------------------
      NSHFRONT_L=0
      IF (MSHSUB>0)
     .  CALL C_ISHSUB(LSHSUB,NSHFRONT_L,P-1)
C--------------------------------------------
c count local elements 
C--------------------------------------------
      OFF = 0
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELS8,NUMELS8_L)
      OFF = OFF + NUMELS8
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELS10,NUMELS10_L)
      OFF = OFF + NUMELS10
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELS20,NUMELS20_L)
      OFF = OFF + NUMELS20
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELS16,NUMELS16_L)
      OFF = OFF + NUMELS16
      NUMELS_L = NUMELS8_L + NUMELS10_L + NUMELS20_L + NUMELS16_L
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELQ,NUMELQ_L)
      OFF = OFF + NUMELQ
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELC,NUMELC_L)
      OFF = OFF + NUMELC
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELT,NUMELT_L)
      OFF = OFF + NUMELT
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELP,NUMELP_L)
      OFF = OFF + NUMELP
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELR,NUMELR_L)
      OFF = OFF + NUMELR
      NUMELTG3=NUMELTG-NUMELTG6
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELTG3,NUMELTG_L)
      OFF = OFF + NUMELTG3
      NUMELTG3_L = NUMELTG_L
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELTG6,NUMELTG6_L)
      OFF = OFF + NUMELTG6
      NUMELTG_L = NUMELTG3_L+NUMELTG6_L
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELUR,NUMELUR_L)
      OFF = OFF + NUMELUR
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELX,NUMELX_L)
      OFF = OFF + NUMELX
      CALL C_ELTLOC(CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELIG3D,NUMELIG3D_L)
      OFF = OFF + NUMELIG3D
C
      IF (P==1) THEN
        NUM16SHIFT_L = 1
        PM1SHF = 2*NUMELS16_L + 1
      ELSE
        NUM16SHIFT_L = PM1SHF
        PM1SHF =  PM1SHF + 2*NUMELS16_L
      ENDIF
C--------------------------------------------
C Counting local cluster
C--------------------------------------------
      NCLUSTER_L = 0
      IF(NCLUSTER > 0) THEN
        CALL C_CLUSTER(CLUSTERS,P-1,CEP,NCLUSTER_L,NUMLOCCLUSTER)
      ENDIF
C--------------------------------------------
C count neighbo elements CFD/ALE
C--------------------------------------------
      NSVOIS   = 0
      NQVOIS   = 0
      NTGVOIS  = 0
      NE_NSVOIS   = 0
      NE_NQVOIS   = 0
      NE_NTGVOIS  = 0
      NRCVVOIS = 0
      NSNDVOIS = 0
      ILAW11   = 0
      NSEGFL_L = 0
      NERVOIS  = 0
      NESVOIS  = 0
      NE_NERVOIS  = 0
      NE_NESVOIS  = 0
      IF(IALE+IEULER+ITHERM+IALELAG/=0)THEN
        NUMEL = MAX(NUMELS,NUMELQ)
        NUMEL_L = MAX(NUMELS_L,NUMELQ_L)
        IF (MULTI_FVM%IS_USED .AND. N2D /= 0) THEN
           NUMEL = MAX(NUMEL, NUMELTG)
           NUMEL_L = MAX(NUMEL_L, NUMELTG_L)
        ENDIF
        CALL C_VOIS(NSVOIS  ,NQVOIS , NTGVOIS, NRCVVOIS,NSNDVOIS,ILAW11 ,
     2              NSEGFL_L,IPARG  ,CEP     ,ALE_CONNECTIVITY  ,NUMEL  ,
     3              P-1     ,IXS    ,IXQ     ,IXTG,
     4              NERVOIS ,NESVOIS,NODLOCAL,NUMNOD_L,NUMEL_L,
     5              CEL, MULTI_FVM,ALE_ELM,SIZE_ALE_ELM)
      ENDIF

!     Node element connectivity
        IF(ALE_CONNECTIVITY%has_ne_connect)THEN
          CALL C_SPMD_NE_CONNECT(ALE_CONNECTIVITY, P - 1, CEP, CEL,
     .                           NODGLOB, NODLOCAL,
     .                           NUMEL, NUMNOD, NUMEL_L, NUMNOD_L,
     .                           NUMELS_L, NUMELQ_L, NUMELTG_L,
     .                           NE_NERVOIS, NE_NESVOIS,
     .                           NE_NSVOIS, NE_NQVOIS, NE_NTGVOIS)
        ENDIF
C
C--------------------------------------
C     count ebcs (multifluid)
C--------------------------------------
      IEBCSLGTH = 0
      ALLOCATE(IEBCS_LISTELEM_L(0))
      ALLOCATE(IEBCS_LISTFAC_L(0))
      ALLOCATE(IEBCS_LISTDP0_L(0))
      EBCS_TAB_LOC%nebcs_fvm = 0
      NEBCS_FVM = 0
      NEBCS_NRF = 0
      OTHER_NEBCS = 0
      IF (NEBCS > 0) THEN
! Workaround for issues with Polymorphic types with some compilers
!$OMP CRITICAL
         DO II = 1, NEBCS
            IF (EBCS_TAB%tab(II)%poly%is_multifluid) THEN
               NEBCS_FVM = NEBCS_FVM + 1
            ELSEIF(EBCS_TAB%tab(II)%poly%type==10) THEN
                NEBCS_NRF = NEBCS_NRF + 1
                LIST_EBCS_NRF(NEBCS_NRF) = II
            ELSEIF(P==1) THEN
                OTHER_NEBCS = OTHER_NEBCS + 1
                LIST_OTHER_EBCS(OTHER_NEBCS) = II
            ENDIF
         ENDDO
         ! allocation of EBCS type FVM
         CALL EBCS_TAB_LOC%create(NEBCS_FVM)
         EBCS_TAB_LOC%nebcs_fvm = NEBCS_FVM

         ! allocation of EBCS type nrf
         EBCS_TAB_LOC_2%nebcs_nrf = NEBCS_NRF
         EBCS_TAB_LOC_2%nebcs_loc = OTHER_NEBCS
         CALL EBCS_TAB_LOC_2%create(NEBCS_NRF)
         EBCS_TAB_LOC_2%nebcs_fvm = 0
         ! --------------------
         ! mode 0 : count the number of entity/element
         CALL C_IEBCS(IXS, IXQ, IXTG,
     .        NUMELS, NUMELQ, NUMELTG,
     .        IPM, NEBCS, CEP, NUMELS + NUMELQ + NUMELTG, NUMELS_L + NUMELQ_L + NUMELTG_L,
     .        P-1, IEBCS_NELEM_L, IEBCS_TYPE, IEBCS_LISTELEM_L, IEBCS_LISTFAC_L,IEBCS_LISTDP0_L,IEBCSLGTH, N2D,
     .        MULTI_FVM%IS_USED,0,EBCS_TAB)
         ! --------------------

C        ALLOCATE
         DEALLOCATE(IEBCS_LISTELEM_L)
         DEALLOCATE(IEBCS_LISTFAC_L)
         DEALLOCATE(IEBCS_LISTDP0_L)
         ALLOCATE(IEBCS_LISTFAC_L(IEBCSLGTH))
         ALLOCATE(IEBCS_LISTELEM_L(IEBCSLGTH))
         ALLOCATE(IEBCS_LISTDP0_L(IEBCSLGTH))

         ! --------------------
         ! mode 1 : initialization
         ! FILL IEBCS_LISTELEM_L and LISTFAC_L
         IEBCSLGTH = 0
         CALL C_IEBCS(IXS, IXQ, IXTG,
     .        NUMELS, NUMELQ, NUMELTG,
     .        IPM, NEBCS, CEP, NUMELS + NUMELQ + NUMELTG, NUMELS_L + NUMELQ_L + NUMELTG_L,
     .        P-1, IEBCS_NELEM_L, IEBCS_TYPE, IEBCS_LISTELEM_L, IEBCS_LISTFAC_L,IEBCS_LISTDP0_L,IEBCSLGTH, N2D,
     .        MULTI_FVM%IS_USED,1,EBCS_TAB)
         ! --------------------


         NEBCS_FVM = 0
         NEBCS_NRF = 0
         IPT = 1
        ! --------------------
        ! loop over the ebcs, 3 cases :
        ! (1) multifluid ebcs : parallel 
        ! (2) non-reflecting ebcs (/EBCS/NRF) : parallel
        ! (3) other ebcs  : sequential --> all the elements/nodes of the ebcs are on main processor
        ! for cases (1) & (2) : need to convert global id into local processor id
         DO II = 1, NEBCS
            ! ------------
            ! multifluid ebcs
            IF (EBCS_TAB%tab(II)%poly%is_multifluid) THEN

               NEBCS_FVM = NEBCS_FVM + 1
               ALLOCATE(EBCS_TAB_LOC%tab(NEBCS_FVM)%poly, source = EBCS_TAB%tab(II)%poly)
               IF (ALLOCATED(EBCS_TAB_LOC%tab(NEBCS_FVM)%poly%ielem)) THEN
                  DEALLOCATE(EBCS_TAB_LOC%tab(NEBCS_FVM)%poly%ielem)
               ENDIF
               IF (ALLOCATED(EBCS_TAB_LOC%tab(NEBCS_FVM)%poly%iface)) THEN
                  DEALLOCATE(EBCS_TAB_LOC%tab(NEBCS_FVM)%poly%iface)
               ENDIF
               IF (ALLOCATED(EBCS_TAB_LOC%tab(NEBCS_FVM)%poly%dp0)) THEN
                  DEALLOCATE(EBCS_TAB_LOC%tab(NEBCS_FVM)%poly%dp0)
               ENDIF
               EBCS_TAB_LOC%tab(NEBCS_FVM)%poly%nb_elem = IEBCS_NELEM_L(II)
               ALLOCATE(EBCS_TAB_LOC%tab(NEBCS_FVM)%poly%ielem(IEBCS_NELEM_L(II)))
               ALLOCATE(EBCS_TAB_LOC%tab(NEBCS_FVM)%poly%iface(IEBCS_NELEM_L(II)))
                IF(EBCS_TAB_LOC%tab(NEBCS_FVM)%poly%has_dp0) ALLOCATE(EBCS_TAB_LOC%tab(NEBCS_FVM)%poly%dp0(IEBCS_NELEM_L(II)))
               DO JJ = 1, IEBCS_NELEM_L(II)
                  EBCS_TAB_LOC%tab(NEBCS_FVM)%poly%ielem(JJ) = IEBCS_LISTELEM_L(IPT)
                  EBCS_TAB_LOC%tab(NEBCS_FVM)%poly%iface(JJ) = IEBCS_LISTFAC_L(IPT)
                  IF(EBCS_TAB_LOC%tab(NEBCS_FVM)%poly%has_dp0)EBCS_TAB_LOC%tab(NEBCS_FVM)%poly%dp0(JJ) = IEBCS_LISTDP0_L(IPT)
                  IPT = IPT + 1
               ENDDO
            ! ------------
            ! /NRF ebcs
            ELSEIF(EBCS_TAB%tab(II)%poly%type==10) THEN
               NEBCS_NRF = NEBCS_NRF + 1
               ALLOCATE(EBCS_TAB_LOC_2%tab(NEBCS_NRF)%poly, source = EBCS_TAB%tab(II)%poly)   
               IF(EBCS_TAB%tab(II)%poly%has_ielem) THEN     
                    EBCS_TAB_LOC_2%tab(NEBCS_NRF)%poly%nb_elem = IEBCS_NELEM_L(II)
                    IF (ALLOCATED(EBCS_TAB_LOC_2%tab(NEBCS_NRF)%poly%iface)) THEN
                       DEALLOCATE(EBCS_TAB_LOC_2%tab(NEBCS_NRF)%poly%iface)
                    ENDIF
                    EBCS_TAB_LOC_2%tab(NEBCS_NRF)%poly%nb_elem = IEBCS_NELEM_L(II)
                    ALLOCATE(EBCS_TAB_LOC_2%tab(NEBCS_NRF)%poly%iface(IEBCS_NELEM_L(II)))
                    DO JJ = 1, IEBCS_NELEM_L(II)
                       EBCS_TAB_LOC_2%tab(NEBCS_NRF)%poly%ielem(JJ) = IEBCS_LISTELEM_L(IPT)
                       EBCS_TAB_LOC_2%tab(NEBCS_NRF)%poly%iface(JJ) = IEBCS_LISTFAC_L(IPT)
                       IF(EBCS_TAB_LOC_2%tab(NEBCS_NRF)%poly%has_dp0)EBCS_TAB_LOC_2%tab(NEBCS_NRF)%poly%dp0(JJ) = IEBCS_LISTDP0_L(IPT)
                       IPT = IPT + 1
                    ENDDO
                ENDIF
            ! ------------
            ! other ebcs
            ELSEIF(EBCS_TAB%tab(II)%poly%has_ielem .and. P==1) THEN
               EBCS_TAB%tab(II)%poly%nb_elem = IEBCS_NELEM_L(II)
               IF (ALLOCATED(EBCS_TAB%tab(II)%poly%ielem)) THEN
                  DEALLOCATE(EBCS_TAB%tab(II)%poly%ielem)
               ENDIF
               IF (ALLOCATED(EBCS_TAB%tab(II)%poly%iface)) THEN
                  DEALLOCATE(EBCS_TAB%tab(II)%poly%iface)
               ENDIF
               EBCS_TAB%tab(II)%poly%nb_elem = IEBCS_NELEM_L(II)
               ALLOCATE(EBCS_TAB%tab(II)%poly%ielem(IEBCS_NELEM_L(II)))
               ALLOCATE(EBCS_TAB%tab(II)%poly%iface(IEBCS_NELEM_L(II)))
               DO JJ = 1, IEBCS_NELEM_L(II)
                  EBCS_TAB%tab(II)%poly%ielem(JJ) = IEBCS_LISTELEM_L(IPT)
                  EBCS_TAB%tab(II)%poly%iface(JJ) = IEBCS_LISTFAC_L(IPT)
                  IF(EBCS_TAB%tab(II)%poly%has_dp0)EBCS_TAB%tab(II)%poly%dp0(JJ) = IEBCS_LISTDP0_L(IPT)
                  IPT = IPT + 1
               ENDDO
            ENDIF
            ! ------------

         ENDDO

        ! --------------------
        ! additional computation for /NRF ebcs
         CALL SPLIT_EBCS(P,NEBCS_NRF,LIST_EBCS_NRF,SCEL,CEP,IGRSURF,
     .                   CEL,NODLOCAL,EBCS_TAB,EBCS_TAB_LOC_2)
        ! --------------------
!$OMP END CRITICAL
      ENDIF
C--------------------------------------
C     Multifluid law velocity
C--------------------------------------
      IF (MULTI_FVM%IS_USED) THEN
         IF (N2D == 0) THEN
            CALL C_MULTI_VEL(CEP, NUMELS, NUMELS_L, P-1,
     .           MULTI_FVM_VEL_L, MULTI_FVM)
         ELSE
            CALL C_MULTI_VEL(CEP, NUMELQ + NUMELTG, NUMELQ_L + NUMELTG_L, P-1,
     .           MULTI_FVM_VEL_L, MULTI_FVM)
         ENDIF
      ENDIF
C--------------------------------------
c count buffer subset, groups, surfaces, lines
C--------------------------------------
      LENIGRNOD_L = 0
      LENIGRBRIC_L = 0
      LENIGRQUAD_L = 0
      LENIGRSH4N_L = 0
      LENIGRTRUS_L = 0
      LENIGRBEAM_L = 0
      LENIGRSPRI_L = 0
      LENIGRSH3N_L = 0
      LENISURF_L = 0
      LENISLIN_L = 0
      CALL C_GROUP_STR(
     1         IGRNOD      ,IGRBRIC     ,IGRQUAD     ,IGRSH4N    ,IGRSH3N      ,
     2         IGRTRUSS    ,IGRBEAM     ,IGRSPRING   ,IGRPART    ,CEP          ,
     3         CEL         ,NODLOCAL    ,P-1         ,LENIGRNOD_L,LENIGRBRIC_L ,
     4         LENIGRQUAD_L,LENIGRSH4N_L,LENIGRTRUS_L,LENIGRBEAM_L,LENIGRSPRI_L,
     5         LENIGRSH3N_L,FRONTB_R2R  ,NUMNOD_L)
      CALL C_ISURF_STR(IGRSURF ,LEN_IA    ,CEP  ,CEL  ,P-1,
     .                 NODLOCAL,LENISURF_L)
      CALL C_ISLIN_STR(IGRSLIN ,P-1, LENISLIN_L)
C--------------------------------------------
C count Buffer interface
C--------------------------------------------
      IMAXIMP_L= 0
      NBDDI2M  = 0
      I2NSN_L  = 0
      NBI18_L  = 0
      NSKYI18_L= 0
      NSNT_L = 0
      NMNT_L = 0
      NSNT2_L = 0
      NMNT2_L = 0
      I2NSN25_L = 0
      NUMNOR_L = 0
      IF(NINTER>0) THEN
c      start CPU timer for IPARI_L_INI
       CALL STARTIME(5,1)

       CALL IPARI_L_INI(IPARI  ,P-1    ,NUMNOD_L ,
     +                NBDDI2M  ,I2NSN_L,PROBINT  ,IMAXIMP_L,NBI18_L ,
     +                NSKYI18_L,NSNT_L ,NMNT_L   ,NSNT2_L  ,NMNT2_L ,
     +                CEP      ,IGRBRIC,IPARI_L  ,
     +                NODLOCAL ,I2NSN25_L, INTERCEP, INTBUF_TAB, NUMNOR_L,
     +                I24MAXNSNE, MULTI_FVM,TAG_SCRATCH,INDX_SCRT,NINDX_SCRT)

c      stop CPU timer for IPARI_L_INI
       CALL STOPTIME(5,1)
      ENDIF

C--------------------------------------------
C count nodes RW
C--------------------------------------------
      NSLAW_L  = 0
      NESLAW_L = 0
      LWSAV_L  = 0
      NSKYRW_L = 0
      IF(NRWALL > 0)
     1  CALL C_RWALL(NPRW    ,LPRW   ,RWL     ,
     2               CEP    ,P-1     ,NSLAW_L ,NESLAW_L,
     3               LWSAV_L,NSKYRW_L)
C--------------------------------------------
C count Stockage Madymo
C--------------------------------------------
      NMADSH4_L = 0
      NMADSH3_L = 0
      NMADSOL_L = 0
      NMADNOD_L = 0
      MADCL_NMADNOD_L = 0

      IF(NEXMAD/=0)
     +  CALL C_MAD(IEXMAD,NMADSH4_L,NMADSH3_L,NMADSOL_L,NMADNOD_L,
     +             MADCL_NMADNOD_L,CEP      ,P-1      )
C--------------------------------------------
C count Nodes RBY
C--------------------------------------------
      NSLARB_L = 0
      NSKYRBK_L = 0
      IF(NRBYKIN>0)
     +  CALL C_RBYK(NPBY,LPBY,P-1,NSLARB_L,NSKYRBK_L)
C--------------------------------------------
C count Rigid material nodes
C--------------------------------------------
      NSLARBM_L = 0
      NSKYRBMK_L = 0
      IF(NRBYM > 0)
     +  CALL C_RBYMK(IRBYM,LCRBYM,P-1,NSLARBM_L,NSKYRBMK_L)
C--------------------------------------------
C count Sections
C--------------------------------------------
      NSTRF_L = 0
      LSECBUF_L = 0
      NNODT_L = 0
      NNODL_L = 0
      IF(NSECT>0)
     +  CALL C_SECTIO(NSTRF    ,CEP    ,P-1,NSTRF_L,
     +                LSECBUF_L,NNODT_L,NNODL_L)
C--------------------------------------------
C count Joints Cylindriques : already done
C--------------------------------------------
      LJOINT_L = 0
      IF(JOINT_SMS) THEN
        IF(NJOINT>0.AND.P==1) CALL C_JOINT_SMS(LJOINT,P-1,LJOINT_L)
      ENDIF
C--------------------------------------------
C Porosite
C--------------------------------------------
      NUMPOR_L = 0
      IF(NUMPOR>0) THEN
        CALL C_PORO(PORNOD,P-1,NUMPOR_L,GEO)
      ENDIF
C--------------------------------------------
C count Buffer Element
C--------------------------------------------
      CALL C_BUFEL(IPARG,P-1,LBUFEL_L)
C--------------------------------------------
C count Airbags
C--------------------------------------------
      INTBAG_L = 0
      IF(NVOLU>0)THEN
        IF(INTBAG/=0)
     +    INTBAG_L = LENVOLU
      ELSEIF(INTBAG > 0 .AND. IALELAG > 0 ) THEN
        INTBAG_L = INTBAG
      ENDIF
C--------------------------------------
C counts  Elem multi-brins
C--------------------------------------
      ISUMNX_L = 0
      IF (NUMELX_L>0)
     .  CALL C_ELMX(KXX,CEP,P-1,ISUMNX_L)
C--------------------------------------
C count SPH Cells
C--------------------------------------
      NUMSPH_L = 0
      LWASPH_L = 0
      NSPHSOL_L =0
      FIRST_SPHSOL_L = 0
C--   SOL2SPH_FLAG : general flag for all CPUS for SOL2SPH - communications are required for proc having common nodes on elements having SOL2SPH
      IF (NSPHSOL > 0) THEN
        SOL2SPH_FLAG = 1
      ELSE
        SOL2SPH_FLAG = 0
      ENDIF
C
      IF (NUMSPH>0)THEN
        CALL C_ISPH(KXSP,CEPSP,P-1,NUMSPH_L,
     .           SLONFSPH_L,SLPRTSPH,SLPRTSPH_L,IPARTSP,SSPHVELN_L,
     .           NSPHSOL_L ,FIRST_SPHSOL_L)
      ENDIF

      IF (P==1) THEN
        SPHSHIFT_L = 1
        PM1SPH = 4*NUMSPH_L + 1
      ELSE
        SPHSHIFT_L = PM1SPH
        PM1SPH =  PM1SPH + 4*NUMSPH_L
      ENDIF
C--------------------------------------
C counts IBCL
C--------------------------------------
      NCONLD_L = 0
      IF (NCONLD>0)
     .  CALL C_IBCL(IBCL,CEP,P-1,NCONLD_L)
C--------------------------------------
C counts Fixvel
C--------------------------------------
      NFXVEL_L = 0
      IF (NFXVEL>0)
     .  CALL C_IBFV(IBFV,NFXVEL,NFXVEL_L,P-1,DD_LAGF)
C--------------------------------------
C counts LLINK
C--------------------------------------
      NLLINK_L=0
      NSKYLL_L=0
      IF (NLINK>0)
     .  CALL C_LLINK(NNLINK,LLLINK,P-1,NLLINK_L,NSKYLL_L)
C--------------------------------------
C counts LLGRAV
C--------------------------------------
      LLGRAV_L=0
      IF (NGRAV>0)
     .  CALL C_IGRAV(IGRAV,LGRAV,LLGRAV_L,P-1,NODLOCAL,NUMNOD_L)
C--------------------------------------
C counts IBVEL
C--------------------------------------
      LLBVEL_L=0
      NSKYRBM_L = 0
      IF (NIBVEL>0)
     .  CALL C_IBVEL(IBVEL,LBVEL,LLBVEL_L,NSKYRBM_L,P-1)
C--------------------------------------
C counts RBE2
C--------------------------------------
      NRBE2_L=0
      LLRBE2_L=0
      NBDDRBE2 = 0
      IF (NRBE2>0)
     .   CALL C_IRBE2(IRBE2,LRBE2,P-1   ,NRBE2_L,
     .                LLRBE2_L ,NBDDRBE2)
C--------------------------------------
C counts RBE3
C--------------------------------------
      NRBE3_L=0
      LLRBE3_L=0
      NBDDRBE3M = 0
      NSKYRBE3_L = 0
      IF (NRBE3>0)
     .   CALL C_IRBE3(IRBE3,LRBE3,NSKYRBE3_L,P-1,
     .                NRBE3_L,LLRBE3_L ,NBDDRBE3M)
C--------------------------------------
C counts MPC
C--------------------------------------
      IF (NUMMPC>0.AND.P==1)THEN
        INUM = NUMMPC+1
        IDDL = INUM  +LMPC
        ISKW = IDDL  +LMPC
        CALL C_IBMPC(IBMPC,IBMPC(INUM),IBMPC(IDDL),IBMPC(ISKW),DD_LAGF)
      END IF
C--------------------------------------
C counts nb of local elts 
C--------------------------------------
      NUMEL_L = NUMELS_L + NUMELQ_L + NUMELC_L + NUMELT_L + NUMELP_L
     +        + NUMELR_L + NUMELUR_L+ NUMELTG_L+ NUMELX_L + NUMSPH_L
     +        + NUMELIG3D_L
C--------------------------------------------
C counts buffer TH
C--------------------------------------------
      LENTHGT = NITHGR*(NTHGRP0+NBR_TH_MONVOL)
C--------------------------------------
C counts boundary (SPMD)
C--------------------------------------
      NLAGF_L = 0
      CALL C_FRONT(P        ,NBDDACC,NBDDKIN,NBDDNRB,
     2             NPBY     ,NRBYKIN_L,LJOINT ,NBDDNCJ,IBVEL  ,
     3             NBDDNRBM ,IADLL    ,LLL    ,NLAGF_L, FRONT_RM,
     4             NRBYMK_L ,NBDDNRBYM,USERNODS,SFR_USERWI_L,
     5             SDD_R2R_ELEM,ADDCSRECT,CSRECT  ,NBDDNORT_L,NBDDNOR_MAX_L,
     6             NBCCNOR_L,NBCCFR25_L,NBDDEDGT_L,NBDDEDG_MAX_L,NRTMX25_L ,
     7             IPARI    ,INTBUF_TAB,INTERCEP  ,NODGLOB      ,NODLOCAL  ,
     8             NUMNOD_L)
      NBDDNOD = NBDDACC+NBDDKIN
C--------------------------------------
C counts Parith/ON SPMD
C--------------------------------------
      IF (IPARI0/=0) THEN
        CALL C_PON(ADDCNE ,NUMNOD_L,NODGLOB ,LCNE_L ,I2NSNT ,
     2             ADDCNI2,LCNI2_L ,MONVOL  ,
     3             CEP    ,P-1     ,NNMV_L  ,NNMVC_L ,ADDCNE_PXFEM,
     4             LCNEPXFEM_L,INOD_PXFEM ,ADDCNE_CRKXFEM,LCNECRKXFEM_L,
     5             INOD_CRKXFEM,ADDCNCND,LCNCND_L,IGRSURF)
        NISKY0_L=8*NUMELS_L+6*NUMELS10_L+8*NUMELS16_L+12*NUMELS20_L+
     +         4*NUMELQ_L+4*NUMELC_L+2*NUMELT_L+2*NUMELP_L+
     +         3*NUMELR_L+3*NUMELTG_L+3*NUMELTG6_L+4*NUMELUR_L+
     +         4*NCONLD_L
        NIR = 4
        IF(N2D/=0) NIR = 2
        NISKYI2_L = I2NSN_L*NIR
      ELSE
        LCNI2_L = 0
        LCNE_L = 0
        NISKY0_L=0
        NISKYI2_L = 0
        NNMV_L = 0
        NNMVC_L = 0
        LCNEPXFEM_L = 0
        LCNECRKXFEM_L = 0
        LCNCND_L = 0
      ENDIF
C--------------------------------------------
C counts and split FLEXIBLE BODY
C--------------------------------------------
      IF (NFXBODY>0) THEN
C Split arrays 
         CALL C_FXBODY1(
     .     FXBIPM,    FXBNOD,   NODLOCAL, IPARG,    FXBELM,
     .     LENNOD_L,  LENMOD_L, LENELM_L, LENSIG_L, P-1,
     .     LENGRVI_L, FXBGRVI )
C
         ALLOCATE(FXBNOD_L(LENNOD_L*2), FXBMOD_L(LENMOD_L*6),
     .            FXBELM_L(LENELM_L), FXBSIG_L(LENSIG_L),
     .            FXBGRVI_L(LENGRVI_L))
C
         CALL C_FXBODY2(
     .     FXBIPM,    FXBNOD,   NODLOCAL, IPARG,    FXBELM,
     .     FXBNOD_L,  FXBMOD_L, FXBELM_L, FXBSIG_L, P-1   ,
     .     FXBMOD,    FXBSIG,   FXBIPM_L, FXBGRVI,
     .     FXBGRVI_L, LENNOD_L, ITASK)
C arrays pined to PMAIN
         CALL C_FXBODY3(
     .     LENGLM_L, LENCP_L,   LENLM_L,  LENFLS_L, LENDLS_L,
     .     LENMCD_L, LENGRVR_L, FXBIPM_L, P-1     )
C
         ALLOCATE(FXBGLM_L(LENGLM_L), FXBCPM_L(LENCP_L),
     .            FXBCPS_L(LENCP_L), FXBLM_L(LENLM_L),
     .            FXBFLS_L(LENFLS_L), FXBDLS_L(LENDLS_L),
     .            FXBGRVR_L(LENGRVR_L))
C
         CALL C_FXBODY4(
     .     FXBGLM,    FXBCPM,   FXBCPS,  FXBLM,    FXBFLS,
     .     FXBDLS,    FXBGRVR,  FXBIPM , P-1,      FXBGLM_L,
     .     FXBCPM_L,  FXBCPS_L, FXBLM_L, FXBFLS_L, FXBDLS_L,
     .     FXBGRVR_L, FXBIPM_L)
      ELSE
        LENNOD_L =0
        LENMOD_L =0
        LENELM_L =0
        LENSIG_L =0
        LENGRVI_L=0
        LENGLM_L =0
        LENCP_L  =0
        LENLM_L  =0
        LENFLS_L =0
        LENDLS_L =0
        LENMCD_L =0
        LENGRVR_L=0
      ENDIF
C--------------------------------------------
C counts Eig
C--------------------------------------------
      LEIBUF_L = 0
      IF (NEIG>0)
     .   CALL C_EIG(EIGIPM   , EIGIBUF , EIGRPM, NODLOCAL, EIGIPM_L,
     .              EIGIBUF_L, LEIBUF_L)
C--------------------------------------------
C count slipring and retractor
C--------------------------------------------
      N_SEATBELT_L = 0
      N_SEATBELT_2D_L = 0
      N_SLIPRING_L = 0
      N_RETRACTOR_L = 0
      N_ANCHOR_REMOTE_L = 0
      N_ANCHOR_REMOTE_SEND_L = 0
      OFF = NUMELS + NUMELQ + NUMELC + NUMELT + NUMELP
C
      IF (NSLIPRING + NRETRACTOR > 0) THEN
        CALL C_SEATBELTS(N_SLIPRING_L,N_RETRACTOR_L,P,NODLOCAL,ELBUF_TAB,
     .                   IPARG,N_ANCHOR_REMOTE_L,N_ANCHOR_REMOTE_SEND_L,ANCHOR_REMOTE_L,ANCHOR_REMOTE_SEND_L,
     .                   N_SEATBELT_L,N_SEATBELT_2D_L,CEP,OFF)
      ENDIF
C--------------------------------------------
      IF (NFLOW>0) CALL M_FLOW(IFLOW, IFLOW_L, NODLOCAL, P, MEMFLOW)
C--------------------------------------------
C Split airbags ALE
C--------------------------------------------
      IF (NFVBAG>0) THEN
         IF (TETRAMESHER_USED) THEN
            NODLOCAL_MGM(1:NUMNOD) = NODLOCAL(1:NUMNOD)
            MAX_ID = MAXVAL(NODLOCAL(1:NUMNOD))
!            IF (P == 1) THEN
               DO I = 1, NB_TOTAL_NODE - NUMNOD
                  MAX_ID = MAX_ID + 1
                  NODLOCAL_MGM(NUMNOD + I) = MAX_ID
               ENDDO
!            ELSE
!               DO I = 1, NB_TOTAL_NODE - NUMNOD
!                  NODLOCAL_MGM(NUMNOD + I) = 0
!               ENDDO
!            ENDIF
            CALL C_FVBAG(
     .           MONVOL,   NODLOCAL_MGM, IXS_TEMP,      P, NB_TOTAL_NODE, FVMAIN)
         ELSE
            CALL C_FVBAG(
     .           MONVOL,   NODLOCAL, IXS,      P, NUMNOD,FVMAIN)
         ENDIF
      ENDIF
C--------------------------------------------
C count external solids faces 
C--------------------------------------------
      NFASOLFR_L= 0
      IF(NFASOLFR/=0 .AND. NUMELS_L/=0)THEN
        CALL C_FASOLFR(FASOLFR, CEP, P-1, CEL, NFASOLFR_L)
      ENDIF
C--------------------------------------------
C Counting "externam segments of quads"
C--------------------------------------------
      NSEGQUADFR_L= 0
      IF(NSEGQUADFR/=0 .AND. NUMELQ_L/=0)THEN
        CALL C_SEGQUADFR(SEGQUADFR, CEP, P-1, CEL, NSEGQUADFR_L)
      ENDIF
C--------------------------------------
C counts flux convec
C--------------------------------------
      NCONV_L = 0
      IF (NUMCONV>0)
     .    CALL C_ICONV(IBCV,CEP,P-1,NCONV_L)
C--------------------------------------
C counts radiative fluxes              
C--------------------------------------
      NRADIA_L = 0
      IF (NUMRADIA>0)
     .    CALL C_IRADIA(IBCR,CEP,P-1,NRADIA_L)
C--------------------------------------
C counts imposed fluxes 
C--------------------------------------
      NFXFLUX_L = 0
      IF (NFXFLUX>0)
     .    CALL C_ITHFLUX(IBFFLUX,CEP,P-1,NFXFLUX_L)
C--------------------------------------
c counts Fixtemp
C--------------------------------------
      NFXTEMP_L = 0
      IF (NFXTEMP>0)
     .  CALL C_IBFT(IBFT,NFXTEMP,NFXTEMP_L,P-1,DD_LAGF)
C--------------------------------------
C count Lagrange multiplier
C--------------------------------------
      LAG_SEC=0
      IF(LAG_NCF+LAG_NCL > 0)
     .  CALL C_LAG(IPARI,NPRW,LAG_SEC)
C--------------------------------------------
C count Elements PLY X-FEM type SHEL 4-N
C--------------------------------------------
       NUMELCPXFE_L = 0
       NUMNODPXFE_L = 0
       IF(IPLYXFEM > 0 ) THEN
         OFF = NUMELS + NUMELQ
C plyxfem
         CALL C_PXFEM(CEP(MIN(OFF+1,LEN_CEP)),P-1 ,NODGLOB  ,IEL_PXFEM  ,INOD_PXFEM,
     .                 IEPXFEM_L,INPXFEM_L, NUMELC , NUMNOD_L ,NUMELC_L,
     .                 NUMELCPXFE_L,NUMNODPXFE_L,INDEX_PXFEM)
       ENDIF
C--------------------------------------------
C count Elements (local) CRACK X-FEM type SHELL 4-NODE (layered shell)
C--------------------------------------------
       NUMELCRKXFE_L = 0
       NUMELCCRKXFE_L = 0
       NUMELTGCRKXFE_L = 0
       NUMNODCRKXFE_L = 0
       NUMEDGES_L = 0
       NBDDEDGE_L= 0
       INCRKXFEM_L=0
       IELXFEMC_L=0
       IELXFEMTG_L=0
       NOD_XFE_L = 0
       IF(ICRACK3D > 0)THEN
         ALLOCATE(TAGEDGE(NUMEDGES))
         TAGEDGE = 0
C
         OFF = NUMELS + NUMELQ
         CALL C_CRKXFEM(
     .          CEP(MIN(OFF+1,LEN_CEP)),P-1,IEL_CRKXFEM,IELXFEMC_L,NUMELC,
     .          NUMELC_L,NUMELCCRKXFE_L,NUMEDGES_L,IEDGESH,4,TAGEDGE)
         OFF = OFF + NUMELC + NUMELT + NUMELP + NUMELR
         IF(NUMELTG > 0)THEN
         CALL C_CRKXFEM(
     .         CEP(MIN(OFF+1,LEN_CEP)),P-1,IEL_CRKXFEM(1+NUMELC),IELXFEMTG_L,NUMELTG,
     .         NUMELTG_L,NUMELTGCRKXFE_L,NUMEDGES_L,
     .         IEDGESH(1+4*ECRKXFEC),3,TAGEDGE)
         END IF
         NUMELCRKXFE_L = NUMELCCRKXFE_L + NUMELTGCRKXFE_L
C
         IF(NUMELCRKXFE_L>0)THEN
           ALLOCATE(NODGLOBXFE(4*NUMELCRKXFE_L*NLEVMAX))
           NODGLOBXFE = 0
         ENDIF
C
         ALLOCATE(INDEX_CRKXFEM(NCRKXFE))
         ALLOCATE(NODLEVXF_L(NCRKXFE))
         INDEX_CRKXFEM = 0
         NODLEVXF_L = 0
C
         CALL C_NCRKXFEM(NODGLOB ,INOD_CRKXFEM  ,INCRKXFEM_L  ,
     .                   NUMNOD_L,NUMNODCRKXFE_L,INDEX_CRKXFEM,P-1,
     .                   IXC,IXTG,CEP_CRKXFEM,NODLOCAL,NODLEVXF_L,
     .                   NODLEVXF,NODGLOBXFE,NOD_XFE_L,CRKSHELL)
C
         ALLOCATE(IEDGECRK_L(4*NUMELCCRKXFE_L+3*NUMELTGCRKXFE_L),
     .            IBORDEDGE_L(NUMEDGES_L),NODEDGE_L(2*NUMEDGES_L),
     .            IEDGESH4_L(4*NUMELCCRKXFE_L),
     .            IEDGESH3_L(3*NUMELTGCRKXFE_L),IEDGE_L(NUMEDGES_L))
         ALLOCATE(EDGELOCAL(NUMEDGES))
         ALLOCATE(EDGEGLOBAL(NUMEDGES))
         IEDGECRK_L   = 0
         IBORDEDGE_L = 0
         NODEDGE_L = 0
         IEDGESH4_L = 0
         IEDGESH3_L = 0
         IEDGE_L = 0
         EDGELOCAL = 0
         EDGEGLOBAL = 0
         CALL C_CRKEDGE(IELXFEMC_L,IELXFEMTG_L,IEDGECRK_L,NUMEDGES,
     .                  IEDGESH   ,IEDGESH(1+4*ECRKXFEC),CEP   ,P-1,
     .                  IBORDEDGE ,IBORDEDGE_L,NUMEDGES_L,NODEDGE,
     .                  NODEDGE_L ,NODLOCAL   ,IEDGESH4_L,IEDGESH3_L,
     .                  IEL_CRKXFEM,IEDGE_L   ,IEDGE     ,ECRKXFEC ,
     .                  EDGELOCAL  ,NBDDEDGE_L,IEDGE_TMP,EDGEGLOBAL)
       ELSE
         ALLOCATE(IEDGECRK_L(0))
         ALLOCATE(IBORDEDGE_L(0))
         ALLOCATE(NODEDGE_L(0))
         ALLOCATE(IEDGESH4_L(0),IEDGESH3_L(0))
         ALLOCATE(TAGEDGE(0))
         ALLOCATE(IEDGE_L(0))
         ALLOCATE(EDGELOCAL(0))
         ALLOCATE(EDGEGLOBAL(0))
         ALLOCATE(NODGLOBXFE(0))
         ALLOCATE(INDEX_CRKXFEM(0))
         ALLOCATE(NODLEVXF_L(0))
       ENDIF
      CALL SETLENWA(
     1      LENWA_L ,NTHWA    ,NAIRWA    ,NUMELS_L ,NUMELQ_L,
     2      NUMELC_L,NUMELTG_L,NUMELT_L  ,NUMELP_L ,NUMELR_L,
     3      NUMNOD_L,NMNT     ,L_MUL_LAG1,L_MUL_LAG,MAXNX   ,
     4      LWASPH_L,NUMSPH_L  ,LWASPIO, NRCVVOIS )
C--------------------------------------
C counts LLCFIELD
C--------------------------------------
      LLCFIELD_L=0
      LLLOADP_L=0
      IF (NLOADC>0)
     .  CALL C_ICFIELD(ICFIELD,LCFIELD,LLCFIELD_L,P-1)
      IF (NLOADP>0) THEN
        CALL C_ILOADP(ILOADP,LLOADP,LLLOADP_L,P-1)
        NISKY0_L=NISKY0_L+4*LLLOADP_L
      ENDIF
C--------------------------------------
C     counts ELEMENTS ISO GEOMETRIQUES
C--------------------------------------
      ISUMNIG3D_L = 0
      IF (NUMELIG3D>0)
     .  CALL C_ELIG3D(KXIG3D,P-1,ISUMNIG3D_L)
C--------------------------------------
C     counts ICNDS10 Itet=2 of S10
C--------------------------------------
      NS10E_L = 0
      NBDDCNDM = 0
      MSNDP(P) = ZERO
      IF (NS10E>0)
     .  CALL C_ICNDS10(ICNDS10,ITAGND,P-1,NS10E_L,NBDDCNDM,MS,MSNDP(P))
C--------------------------------------
C     counts /BCS/CYCLIC
C--------------------------------------
      NBCSCYC_L = 0
      LLBCSCYC_L = 0
      IF (NBCSCYC>0)
     .  CALL C_IBCSCYC(IBCSCYC,LBCSCYC,P-1  ,NBCSCYC_L,LLBCSCYC_L)
C--------------------------------------
C Userwindow - preparation
C--------------------------------------
      IF (P==1) THEN
        SIUSERW_L = SIUSERW
        SUSERW_L = SUSERW
      ELSE
        SIUSERW_L = 0
        SUSERW_L = 0
      ENDIF
C--------------------------------------------
C Open Restart FILE
C--------------------------------------------
      LEN_IA = 0
      IR_ = P
      WRITE(CHRUN,'(I4.4)')IRUN
      WRITE(PROCNAM,'(I4.4)')P
      FILNAM = ROOTNAM(1:ROOTLEN)//'_'//CHRUN//'_'//PROCNAM//'.rst'
      TMP_NAME = OUTFILE_NAME(1:OUTFILE_NAME_LEN)//FILNAM(1:LEN_TRIM(FILNAM))
      LEN_TMP_NAME = OUTFILE_NAME_LEN+ROOTLEN+14
C fichier de restart au format binaire ieee 64 bits
      DO I=1,LEN_TMP_NAME
          IFILNAM(I)=ICHAR(TMP_NAME(I:I))
      ENDDO
      DO I=1,ROOTLEN+14
          IFILNAM2(I)=ICHAR(FILNAM(I:I))
      ENDDO
      CALL CUR_FIL_C(1)
      CALL OPEN_C(IFILNAM,LEN_TMP_NAME,0)
#ifdef DEBUG_RST
      IF(FLUSH_RST_TO_TXT) THEN
        OPEN(UNIT=777+ITASK,FILE=TRIM(FILNAM)//'.txt'
     . ,STATUS='UNKNOWN',FORM='FORMATTED')
      ENDIF
#endif
      CALL WRITE_C_C(IFILNAM2,ROOTLEN+14)
      LEN_IA = LEN_IA + ROOTLEN+14
C--------------------------------------------
C  Skippon_l for obsolide "modif" option.
      SKIPPON_L = NUMNOD_L+1 + LCNE_L + LCNI2_L +
     .  8*NUMELS_L + 6*NUMELS10_L + 8*NUMELS16_L + 12*NUMELS20_L +
     .  4*NUMELQ_L + 4*NUMELC_L   + 2*NUMELT_L + 2*NUMELP_L +
     .  3*NUMELR_L + 3*NUMELTG_L  + 3*NUMELTG6_L + 4*NUMELUR_L +
     .  4*NNMV_L   + 4*NCONLD_L  + 4*NCONV_L  + NSKYRW_L + NSKYRBK_L +
     .  NISKYI2_L +
     .  NNMV_L     + NNMVC_L      + NSKYLL_L + NSKYRBM_L + NSKYI18_L +
     .  NSKYRBMK_L + 4*NRADIA_L + 4*NFXFLUX_L + 4*LLLOADP_L

       IF (I2NSNT>0) THEN
          SKIPPON_L = SKIPPON_L + NUMNOD_L +1
       ENDIF

C--------------------------------------------
C     SPMD: variables preparation 
C--------------------------------------------

      CALL WRCOMIP(
     1   LMXVINT   ,LVARINT   ,TABVINT   ,NGROUP_L  ,LBUFEL_L ,
     2   LENWA_L   ,INTBAG_L  ,NUMELQ_L  ,NUMELS_L  ,NUMELC_L ,
     3   NUMELT_L  ,NUMELR_L  ,NUMELP_L  ,NUMELX_L  ,ISUMNX_L ,
     4   NUMELS10_L,NUMELS20_L,NUMELS8_L ,NUMELS16_L,LNOM_OPT ,
     5   NUMELUR_L ,NUMELTG_L ,NUMELTG6_L,NSLARB_L  ,NUMNOD_L ,
     6   NUMEL_L   ,NSVOIS    ,NQVOIS    ,NTGVOIS,   LENLAS   ,
     7   NLLINK_L  ,LLGRAV_L  ,LLBVEL_L  ,NSLAW_L  ,
     8   NESLAW_L  ,NSKYLL_L  ,NSTRF_L   ,LJOINT_L ,
     9   LENVOLU   ,LENTHGT   ,LBUFMAT   ,LBUFGEO   ,LBUFSF   ,
     A   LENXLAS   ,LWSAV_L   ,LSECBUF_L ,NUMPOR_L ,
     B              NPTS      ,NBDDNOD   ,NFXVEL_L  ,NBDDNRB  ,
     C   LCNE_L    ,NSKYRW_L  ,NSKYRBK_L ,NBDDI2M   ,LCNI2_L  ,
     D   NISKYI2_L ,I2NSNT    ,NCONLD_L  ,NNMV_L    ,NNMVC_L  ,
     E   NBDDNCJ   ,NBDDNRBM  ,NSKYRBM_L ,NNODT_L   ,NNODL_L  ,
     F   NMADSH4_L ,NMADSH3_L ,NMADSOL_L ,NMADNOD_L ,IMAXIMP_L,
     G   ISP0   ,NUM16SHIFT_L ,NSHFRONT_L,NUMSPH_L  ,LWASPH_L ,
     H   SPHSHIFT_L,NRCVVOIS  ,NSNDVOIS  ,NERVOIS  ,
     I   NESVOIS   ,ILAW11    ,NSEGFL_L  ,
     J   NBI18_L   ,NSKYI18_L ,LEIBUF_L ,
     K   LENTHG    ,NFASOLFR_L,LENNOD_L  ,LENMOD_L  ,LENELM_L ,
     L   LENSIG_L  ,LENGLM_L  ,LENCP_L   ,LENLM_L   ,LENFLS_L ,
     M   LENDLS_L  ,LENMCD_L  ,LENGRVI_L ,LENGRVR_L ,NLAGF_L  ,
     N   LRBAGALE  ,SKIPPON_L ,NCONV_L   ,NFXTEMP_L ,NFXFLUX_L ,
     O   NRBE3_L   ,LLRBE3_L  ,NBDDRBE3M ,NSKYRBE3_L,NSLARBM_L ,
     N   NSKYRBMK_L,NBDDNRBYM ,MADCL_NMADNOD_L,NRADIA_L,NRBE2_L,
     Q   LLRBE2_L  ,NBDDRBE2  ,NUMELCPXFE_L,
     R   NUMNODPXFE_L,LCNEPXFEM_L,LLCFIELD_L,SFR_USERWI_L,SUSERW_L,
     S SIUSERW_L,LLLOADP_L,NUMELCRKXFE_L,NUMNODCRKXFE_L,LCNECRKXFEM_L,
     T   SLONFSPH_L,SLPRTSPH_L,SSPHVELN_L,I2NSN25_L,NUMELCCRKXFE_L,
     U   NUMELTGCRKXFE_L,NUMEDGES_L,NBDDEDGE_L,SDD_R2R_ELEM,NOD_XFE_L,
     V   NSPHSOL_L,FIRST_SPHSOL_L,NCLUSTER_L,LENTHGR,NUMNOR_L,
     W   NBDDNORT_L,NBDDNOR_MAX_L,NBCCNOR_L,NBCCFR25_L,NS10E_L,
     X   LCNCND_L,NBDDCNDM,NEBCS,IEBCSLGTH,MULTI_FVM,NSEGQUADFR_L,
     Y   NBDDEDGT_L,NBDDEDG_MAX_L,NRTMX25_L,LENIGRNOD_L,LENIGRBRIC_L,
     Z   LENIGRQUAD_L,LENIGRSH4N_L,LENIGRTRUS_L,LENIGRBEAM_L,LENIGRSPRI_L,
     1   LENIGRSH3N_L,LENISURF_L,LENISLIN_L, NE_NERVOIS, NE_NESVOIS,
     2   NE_NSVOIS, NE_NQVOIS, NE_NTGVOIS,NBCSCYC_L,LLBCSCYC_L,
     3   ALE_CONNECTIVITY,N_SLIPRING_L,N_RETRACTOR_L,N_ANCHOR_REMOTE_L,
     4   N_ANCHOR_REMOTE_SEND_L,N_SEATBELT_L,N_SEATBELT_2D_L)

      CALL WRCOMR(LMXVREA,LVARREA,TABVREA,MSNDP(P),P)
C--------------------------------------
C     write TEXT
C--------------------------------------
      CALL WRITE_I_C(TEXT,LCOM05)
      LEN_IA = LEN_IA + LCOM05
C--------------------------------------
C     Write HEADER.
C--------------------------------------
C     RADIOSS FILE R/W
      TABHEAD(1) =IRADIOS
C     RESTART FILE
      TABHEAD(2) =IRESFIL
C     ITESTV
      TABHEAD(3) =ITESTV
C     THIS RUN NUMBER
      TABHEAD(4) =IRUN
C     CODE FOR THIS RUN (-1:UNKNOWN,0:STARTER,1:ENGINE,2:MODIF)
      IF(IRUN==0)THEN
       ICODRUN =0
      ELSE
       ICODRUN =2
      ENDIF
      TABHEAD(5) =ICODRUN
C     CODVERS FOR THIS RUN
      TABHEAD(6) =CODVERS
C     MINOR VERSION CODE FOR THIS RUN
      TABHEAD(7) =IMINVER
C     SOURCE VERSION CODE FOR THIS RUN
      TABHEAD(8) =ISRCVER
C     INPUT VERSION FOR THIS RUN
      TABHEAD(9) =INVERS
C
      IF(IRUN==0)THEN
       SCODVER=CODVERS
       SMINVER=IMINVER
       SSRCVER=ISRCVER
      ENDIF
C     STARTER CODVERS
      TABHEAD(10)=SCODVER
C     STARTER MINOR VERSION CODE.
      TABHEAD(11)=SMINVER
C     STARTER SOURCE VERSION CODE.
      TABHEAD(12)=SSRCVER
C     STARTER INPUT VERSION
      TABHEAD(13)=INVSTR
C
      TABHEAD(14)=LVARINT
      TABHEAD(15)=LVARREA
C
      CALL WRITE_I_C(LHEADER, 1)
      CALL WRITE_I_C(TABHEAD, LHEADER)
      LEN_IA = LEN_IA + LHEADER + 1
C--------------------------------------
      CALL WRITE_I_C(TABVINT,LVARINT)
      LEN_IA = LEN_IA + LVARINT
C--------------------------------------
C     NODGLOB Deplacee
C--------------------------------------
      CALL WRITE_I_C(NODGLOB,NUMNOD_L)
      LEN_IA = LEN_IA + NUMNOD_L
      CALL W_MAIN_PROC_WEIGHT(NODGLOB,NUMNOD_L,NSPMD,P)
      LEN_IA = LEN_IA + 2*NUMNOD_L
C--------------------------------------
C     Integers 
C--------------------------------------
      LEN_G = NPROPGI*NUMGEO
      LEN_M = NPROPMI*NUMMAT
      CALL W_INLOC(ICODE,NODGLOB,NUMNOD_L,LEN_IA)
C--------------------------------------
C Ecriture LSHSUB
C--------------------------------------
      IF (MSHSUB>0)
     .  CALL W_ISHSUB(LSHSUB,NSHFRONT_L,P-1,NODLOCAL,LEN_IA)
C
      IF(NUMSPH>0.AND.NSPCOND>0)THEN
        CALL W_ISKEWSP(ISKEW,NODGLOB,NUMNOD_L,NUMSPH_L,CEPSP,P-1,LEN_IA)
      ELSE
        CALL W_INLOC(ISKEW,NODGLOB,NUMNOD_L,LEN_IA)
      END IF
      CALL W_ISKN(
     1  ISKN,NUMSKW,MIN(IUN,NSPCOND)*NUMSPH_L,IFRAME,NUMFRAM,NODLOCAL,
     2  P   ,NUMNOD,CEPSP    ,LEN_IA )
      CALL WRITE_I_C(IBCSLAG,5*NBCSLAG)
      LEN_IA = LEN_IA + 5*NBCSLAG
      CALL WRITE_I_C(IPART,LIPART1*(NPART+NTHPART))
      LEN_IA = LEN_IA + (NPART+NTHPART)
      CALL WRITE_I_C(IPARTH,2*9*(NPART+NTHPART))
      LEN_IA = LEN_IA + 2*9*(NPART+NTHPART)
C--------------------------------------------
C Parts Elements
C--------------------------------------------
      OFF = 0
      CALL W_IELOC(IPARTS,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELS,NUMELS_L,LEN_IA)
      OFF = OFF + NUMELS
      CALL W_IELOC(IPARTQ,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELQ,NUMELQ_L,LEN_IA)
      OFF = OFF + NUMELQ
      CALL W_IELOC(IPARTC,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELC,NUMELC_L,LEN_IA)
      OFF = OFF + NUMELC
      CALL W_IELOC(IPARTT,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELT,NUMELT_L,LEN_IA)
      OFF = OFF + NUMELT
      CALL W_IELOC(IPARTP,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELP,NUMELP_L,LEN_IA)
      OFF = OFF + NUMELP
      CALL W_IELOC(IPARTR,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELR,NUMELR_L,LEN_IA)
      OFF = OFF + NUMELR
      OFF1 = OFF
      OFF  = OFF + NUMELTG
      CALL W_IELOC(IPARTUR,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELUR,NUMELUR_L,LEN_IA)
C      OFF = OFF + NUMELUR
      OFF = OFF1
      CALL W_IELOC(IPARTTG,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELTG,NUMELTG_L,LEN_IA)
      OFF = OFF + NUMELTG + NUMELUR
      CALL W_IELOC(IPARTX,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELX,NUMELX_L,LEN_IA)
      OFF = OFF + NUMELX
      CALL W_IELOC(IPARTSP,CEPSP,P-1,NUMSPH,NUMSPH_L,LEN_IA)
c      OFF = OFF + NUMSPH
      CALL W_IELOC(IPARTIG3D,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELIG3D,NUMELIG3D_L,LEN_IA)
C
      IF(P==1)THEN
        CALL WRITE_I_C(NOM_OPT,LNOM_OPT)
        LEN_IA = LEN_IA + LNOM_OPT
      ENDIF
C
      CALL WRITE_I_C(NPC,3*NFUNCT+1)
        LEN_IA = LEN_IA + 3*NFUNCT+1
C--------------------------------------------
C contectivity for RNUR
C--------------------------------------------
      OFF = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR+NUMELTG
      CALL W_IXLOC(IXUR,NIXUR,4,CEP(MIN(OFF+1,LEN_CEP)),P-1,
     +             NUMELUR,NUMELUR_L,NODLOCAL,LEN_IA)
C--------------------------------------------
C Connectivity for Triangles
C--------------------------------------------
      OFF = NUMELS+NUMELQ+NUMELC+NUMELT+NUMELP+NUMELR
      CALL W_IXLOC(IXTG,NIXTG,3,CEP(MIN(OFF+1,LEN_CEP)),P-1,
     +             NUMELTG,NUMELTG_L,NODLOCAL,LEN_IA)
C--------------------------------------------
C boundary Elts CFD/ALE
C--------------------------------------------
      IF(IALE+IEULER+ITHERM+IALELAG/=0) THEN
        IF(NUMELS_L+NUMELQ_L+NUMELTG_L>0) THEN
          ALLOCATE(IDGLOB_L(NUMELS_L + NSVOIS + NUMELQ_L + NQVOIS + NUMELTG_L + NTGVOIS))
          ALLOCATE(UIDGLOB_L(NUMELS_L + NSVOIS + NUMELQ_L + NQVOIS + NUMELTG_L + NTGVOIS))
          ALLOCATE( ID_GLOBAL_VOIS(NUMELS_L*6+NUMELQ_L*4+3*NUMELTG_L) )
          ID_GLOBAL_VOIS(1:NUMELS_L*6+NUMELQ_L*4+3*NUMELTG_L) = 0
          ALLOCATE( FACE_VOIS(NUMELS_L*6+NUMELQ_L*4+3*NUMELTG_L) )
          FACE_VOIS(1:NUMELS_L*6+NUMELQ_L*4+3*NUMELTG_L) = -1
        ELSE
          ALLOCATE( ID_GLOBAL_VOIS(0) )
          ALLOCATE(IDGLOB_L(0))
          ALLOCATE(UIDGLOB_L(0))
          ALLOCATE( FACE_VOIS(0) )
        ENDIF
        NUMEL = MAX(NUMELS,NUMELQ,NUMELTG)
        IF(NSVOIS>0)
     +    ALLOCATE(IXSF(NSVOIS*NIXS))
        IF(NQVOIS>0)
     +    ALLOCATE(IXQF(NQVOIS*NIXQ))
        IF(NTGVOIS>0)
     +    ALLOCATE(IXTGF(NTGVOIS*NIXTG))
        CALL C_IXFLOC(NUMEL,IXSF,IXQF,IXTGF,NSVOIS,
     +                NQVOIS,NTGVOIS,P-1,IPARG,CEP,CEL,
     +                ALE_CONNECTIVITY,ee_connect_l,IXS,IXQ,IXTG,NODLOCAL,NUMELS_L,NUMELQ_L,NUMELTG_L,MULTI_FVM,
     +                ID_GLOBAL_VOIS,INDX_S,INDX_Q,INDX_TG,FACE_ELM_S,FACE_ELM_Q,FACE_ELM_TG,FACE_VOIS)

        CALL WRITE_I_C(IXTGF, NIXTG * NTGVOIS)
        LEN_IA = LEN_IA + NIXTG * NTGVOIS
        IF (NTGVOIS > 0)
     +       DEALLOCATE(IXTGF)
        IF (MULTI_FVM%NS_DIFF) THEN
           CALL C_IDGLOB(NUMEL, NUMELS_L, NUMELQ_L, NUMELTG_L,
     .          P - 1, CEL, CEP, IPARG, ALE_CONNECTIVITY, IXS, IDGLOB_L, UIDGLOB_L)
        ENDIF
      ENDIF
C--------------------------------------------
C Triangles
C--------------------------------------------
      OFF = OFF+NUMELTG3
      CALL W_IXBLOC(IXTG6,3,4,CEP(MIN(OFF+1,LEN_CEP)),P-1,
     +              NUMELTG6,NUMELTG6_L,NODLOCAL,LEN_IA)
C      OFF = OFF+NUMELTG6
C--------------------------------------------
C Solid: elements connectivity
C--------------------------------------------
      OFF = 0
      CALL W_IXLOC(IXS,NIXS,8,CEP(MIN(OFF+1,LEN_CEP)),P-1,
     +             NUMELS,NUMELS_L,NODLOCAL,LEN_IA)
C
      IF(IALE+IEULER+ITHERM+IALELAG/=0) THEN
         CALL WRITE_I_C(IXSF,NIXS*NSVOIS)
         LEN_IA = LEN_IA + NIXS*NSVOIS
         IF(NSVOIS>0)
     +        DEALLOCATE(IXSF)
      ENDIF
C--------------------------------------------
C Solids                                
C--------------------------------------------
      OFF = OFF+NUMELS8
      CALL W_IXALOC(IXS10,6,CEP(MIN(OFF+1,LEN_CEP)),P-1,
     +             NUMELS10,NUMELS10_L,NODLOCAL,LEN_IA)
      OFF = OFF+NUMELS10
      CALL W_IXALOC(IXS20,12,CEP(MIN(OFF+1,LEN_CEP)),P-1,
     +             NUMELS20,NUMELS20_L,NODLOCAL,LEN_IA)
      OFF = OFF+NUMELS20
      CALL W_IXALOC(IXS16,8,CEP(MIN(OFF+1,LEN_CEP)),P-1,
     +             NUMELS16,NUMELS16_L,NODLOCAL,LEN_IA)
      OFF = OFF+NUMELS16
      CALL W_IXLOC(IXQ,NIXQ,4,CEP(MIN(OFF+1,LEN_CEP)),P-1,
     +             NUMELQ,NUMELQ_L,NODLOCAL,LEN_IA)
      OFF = OFF+NUMELQ
C--------------------------------------------
C Boundary Elts CFD/ALE
C--------------------------------------------
      IF(IALE+IEULER+ITHERM+IALELAG/=0) THEN
        CALL WRITE_I_C(IXQF,NIXQ*NQVOIS)
        LEN_IA = LEN_IA + NIXQ*NQVOIS
        IF(NQVOIS>0)
     +    DEALLOCATE(IXQF)
      ENDIF
C--------------------------------------------
      CALL W_IXLOC(IXC,NIXC,4,CEP(MIN(OFF+1,LEN_CEP)),P-1,
     +             NUMELC,NUMELC_L,NODLOCAL,LEN_IA)
      OFF = OFF+NUMELC
      CALL W_IXLOC(IXT,NIXT,2,CEP(MIN(OFF+1,LEN_CEP)),P-1,
     +             NUMELT,NUMELT_L,NODLOCAL,LEN_IA)
      OFF = OFF+NUMELT
      CALL W_IXLOC(IXP,NIXP,3,CEP(MIN(OFF+1,LEN_CEP)),P-1,
     +             NUMELP,NUMELP_L,NODLOCAL,LEN_IA)
      OFF = OFF+NUMELP
      CALL W_IXLOC(IXR,NIXR,3,CEP(MIN(OFF+1,LEN_CEP)),P-1,
     +             NUMELR,NUMELR_L,NODLOCAL,LEN_IA)
C
C--------------------------------------
C ITAB
C--------------------------------------
      CALL W_INLOC(ITAB,NODGLOB,NUMNOD_L,LEN_IA)
      CALL W_ITABM1(ITABM1,ITABM1(NUMNOD+1),NODGLOB,NODLOCAL,NUMNOD_L,
     .              ITAB,LEN_IA)
C--------------------------------------
C GJBUFI
C--------------------------------------
      IF(NGJOINT/=0 .AND. P == 1) THEN
        CALL W_GJOINT(GJBUFI,NGJOINT,LKJNI,NODLOCAL,LEN_IA)
      END IF
C--------------------------------------
C NALE et voisin ALE
C--------------------------------------
      IF(IALE+IEULER+ITHERM+IALELAG/=0) THEN
        IF(IALE+IEULER+IALELAG/=0)
     .       CALL W_INLOC(NALE,NODGLOB,NUMNOD_L,LEN_IA)

        ITMP = SIZE(ee_connect_l%iad_connect)
        CALL WRITE_I_C(ITMP, 1)
        LEN_IA = LEN_IA + 1
        CALL WRITE_I_C(ee_connect_l%iad_connect, ITMP)
        LEN_IA = LEN_IA + ITMP

        ITMP = SIZE(ee_connect_l%connected)
        CALL WRITE_I_C(ITMP, 1)
        LEN_IA = LEN_IA + 1
        CALL WRITE_I_C(ee_connect_l%connected, ITMP)
        LEN_IA = LEN_IA + ITMP

        ITMP = SIZE(ee_connect_l%type)
        CALL WRITE_I_C(ITMP, 1)
        LEN_IA = LEN_IA + 1
        CALL WRITE_I_C(ee_connect_l%type, ITMP)
        LEN_IA = LEN_IA + ITMP

        ITMP = SIZE(ee_connect_l%iface2)
        CALL WRITE_I_C(ITMP, 1)
        LEN_IA = LEN_IA + 1
        CALL WRITE_I_C(ee_connect_l%iface2, ITMP)
        LEN_IA = LEN_IA + ITMP

        ! write the Global ID of neighbour
        CALL WRITE_I_C(ID_GLOBAL_VOIS,NUMELS_L*6+NUMELQ_L*4+3*NUMELTG_L)
        LEN_IA = LEN_IA + NUMELS_L*6+NUMELQ_L*4+3*NUMELTG_L
        ! write the Id of the remote connected element
        CALL WRITE_I_C(FACE_VOIS,NUMELS_L*6+NUMELQ_L*4+3*NUMELTG_L)
        LEN_IA = LEN_IA + NUMELS_L*6+NUMELQ_L*4+3*NUMELTG_L


        DEALLOCATE(ID_GLOBAL_VOIS)
        DEALLOCATE(FACE_VOIS)

        ALLOCATE(ELEMID_L(NUMEL))
C     ----------------------    C
C     Node node connectivity
C     ----------------------    C
          IF(ALE_CONNECTIVITY%has_ne_connect)THEN
            CALL SPMD_NE_CONNECT(ALE_CONNECTIVITY, P - 1, CEP, CEL,
     .                           NODGLOB, NODLOCAL,
     .                           NUMEL, NUMNOD, NUMEL_L, NUMNOD_L,
     .                           NUMELS_L, NUMELQ_L, NUMELTG_L,
     .                           NE_NERVOIS, NE_NESVOIS,
     .                           NE_NSVOIS, NE_NQVOIS, NE_NTGVOIS, ELEMID_L, LEN_IA, IXS)
          ENDIF

          IF(ALE_CONNECTIVITY%has_nn_connect .OR. ALE_CONNECTIVITY%has_ne_connect)THEN
            CALL W_ING2LOC(ALE_CONNECTIVITY,
     .            ELEMID_L,NUMNOD,NODGLOB,NODLOCAL,NUMNOD_L,LEN_IA,NWALE)
          ENDIF

        DEALLOCATE(ELEMID_L)
      ENDIF

      NS_DIFF = 0
      IF (MULTI_FVM%NS_DIFF) NS_DIFF = 1
      CALL WRITE_I_C(NS_DIFF, 1)
      LEN_IA = LEN_IA + 1
      IF (MULTI_FVM%NS_DIFF) THEN
         CALL WRITE_I_C(IDGLOB_L, NUMELS_L + NSVOIS + NUMELQ_L + NQVOIS + NUMELTG_L + NTGVOIS)
         LEN_IA = LEN_IA + NUMELS_L + NSVOIS + NUMELQ_L + NQVOIS + NUMELTG_L + NTGVOIS
         CALL WRITE_I_C(UIDGLOB_L, NUMELS_L + NSVOIS + NUMELQ_L + NQVOIS + NUMELTG_L + NTGVOIS)
         LEN_IA = LEN_IA + NUMELS_L + NSVOIS + NUMELQ_L + NQVOIS + NUMELTG_L + NTGVOIS
      ENDIF
C
C multi mat 2D mono SPMD
C
      IF(NMULT > 0) THEN
        CALL WRITE_I_C(IFILL,SIFILL)
        CALL WRITE_I_C(IMS,SIMS)
        LEN_IA = LEN_IA +SIFILL + SIMS
      END IF
C--------------------------------------
C SUBSET, GROUPES, SURFACES, LIGNES ET BUFFERS ASSOCIES
C--------------------------------------
      CALL W_SUBSET_STR(SUBSET,LEN_IA)
      CALL W_GROUP_STR(LEN_IA  ,IGRNOD   ,IGRBRIC ,IGRQUAD   ,IGRSH4N   ,
     .                 IGRSH3N ,IGRTRUSS ,IGRBEAM ,IGRSPRING ,IGRPART   ,
     .                 CEP     ,CEL      ,NODLOCAL,P-1       ,FRONTB_R2R,
     .                 NUMNOD_L )
      CALL W_ISURF_STR(IGRSURF ,LEN_IA ,CEP  ,CEL  ,P-1,
     .                 NODLOCAL,LENISURF_L)
      CALL W_ISLIN_STR(IGRSLIN ,LEN_IA , P-1, NODLOCAL)
C--------------------------------------
C Elem multi-brins
C--------------------------------------
      IF (NUMELX>0)
     .  CALL W_ELMX(KXX,NUMELX_L,ISUMNX_L,IXX,CEP,
     +              P-1,NODLOCAL,LEN_IA)

C--------------------------------------
C SPH Cells
C--------------------------------------
      IF (NUMSPH>0) THEN
          CALL W_ISPH(KXSP    ,IXSP    ,NUMSPH_L,CEPSP ,P-1    ,
     +                NODLOCAL,NUMNOD_L,ISPCOND ,IPARG ,ISPHIO ,
     +                LEN_IA,  SLONFSPH_L,SLPRTSPH_L,IPARTSP,
     +                LONFSPH,LPRTSPH, IBUFSSG_IO,CELSPH,
     +                NSPHSOL_L,FIRST_SPHSOL_L,SPH2SOL ,SOL2SPH,
     +                IRST     , NUMELS8_L,CEP ,CEL    ,SOL2SPH_TYP)
       ENDIF
C--------------------------------------
C IBCL
C--------------------------------------
      IF (NCONLD>0)THEN
        CALL W_IBCL(IBCL,CEP,P-1,NODLOCAL,NCONLD_L,LEN_IA)
      ENDIF
C--------------------------------------------
C LOAD/PRESSURE interfaces
C--------------------------------------------
      IF (NINTLOADP>0) THEN
         CALL WRITE_I_C(KLOADPINTER, NINTER+1)
         LEN_IA = LEN_IA + NINTER + 1
         CALL WRITE_I_C(LOADPINTER,S_LOADPINTER )
         LEN_IA = LEN_IA + S_LOADPINTER 
      ENDIF
C--------------------------------------
C IBFV
C--------------------------------------
      IF (NFXVEL>0)
     .  CALL W_IBFV(IBFV,NFXVEL,NODLOCAL,LEN_IA,
     .              NFXVEL_L,P-1)
C--------------------------------------
C LAS (renumerote directement)
C--------------------------------------
      IF (NLASER>0.AND.P==1)
     .  CALL W_LAS(LAS,NLASER,NODLOCAL,LEN_IA)
C--------------------------------------
C IEBCS
C--------------------------------------
      NEBCS_FVM = 0
      CALL WRITE_I_C(NEBCS, 1) ! total number of /EBCS
      CALL WRITE_I_C(EBCS_TAB_LOC%nebcs_fvm, 1) ! multifluid ebcs number
      CALL WRITE_I_C(NEBCS_NRF, 1)  ! /NRF ebcs number
      CALL WRITE_I_C(OTHER_NEBCS, 1) ! other ebcs
      ! ---------------
      ! common data 
      IF (NEBCS > 0)THEN
         CALL EBCS_TAB%write_type_data(LEN_IA, LEN_AM)
      ENDIF
      ! ---------------

      ! ---------------
      ! multifluid EBCS
      DO II = 1, NEBCS
         IF (EBCS_TAB%tab(ii)%poly%is_multifluid) THEN
            NEBCS_FVM = NEBCS_FVM + 1
            CALL EBCS_TAB_LOC%tab(NEBCS_FVM)%poly%write_common_data(LEN_IA, LEN_AM)
            CALL EBCS_TAB_LOC%tab(NEBCS_FVM)%poly%write_data(LEN_IA, LEN_AM)
         ENDIF
      ENDDO
      ! ---------------
      ! deallocation of local multifluid ebcs structure
      CALL EBCS_TAB_LOC%destroy()
      ! ---------------

      ! ---------------
      ! other ebcs
      NEBCS_NRF = 0
      DO II = 1, NEBCS
        ! ---------------
        ! NRF ebcs
        IF(.NOT.EBCS_TAB%tab(ii)%poly%is_multifluid.AND.EBCS_TAB%tab(ii)%poly%type==10) THEN
            NEBCS_NRF = NEBCS_NRF + 1
            DO JJ = 1, EBCS_TAB_LOC_2%tab(ii)%poly%nb_node
                IF( NODLOCAL(EBCS_TAB_LOC_2%tab(ii)%poly%node_list(jj))>0.AND.
     .              NODLOCAL(EBCS_TAB_LOC_2%tab(ii)%poly%node_list(jj))<=NUMNOD ) THEN
                    EBCS_TAB_LOC_2%tab(NEBCS_NRF)%poly%node_list(JJ) =
     .               NODLOCAL(EBCS_TAB_LOC_2%tab(NEBCS_NRF)%poly%node_list(JJ))                
                ENDIF
            ENDDO
            IF (.NOT. EBCS_TAB%tab(ii)%poly%is_multifluid) THEN
                CALL EBCS_TAB_LOC_2%tab(NEBCS_NRF)%poly%write_common_data(LEN_IA, LEN_AM)
                CALL EBCS_TAB_LOC_2%tab(NEBCS_NRF)%poly%write_data(LEN_IA, LEN_AM)
            ENDIF
        ! ---------------
        ! other ebcs
        ELSEIF (.NOT.(EBCS_TAB%tab(ii)%poly%is_multifluid).AND.P == 1) THEN
            DO JJ = 1, EBCS_TAB%tab(ii)%poly%nb_node
               EBCS_TAB%tab(ii)%poly%node_list(jj) = NODLOCAL(EBCS_TAB%tab(ii)%poly%node_list(jj))
            ENDDO
            IF (.NOT. EBCS_TAB%tab(ii)%poly%is_multifluid) THEN
               CALL EBCS_TAB%tab(ii)%poly%write_common_data(LEN_IA, LEN_AM)
               CALL EBCS_TAB%tab(ii)%poly%write_data(LEN_IA, LEN_AM)
            ENDIF
        ENDIF
        ! ---------------
      ENDDO
      ! ---------------
C--------------------------------------
C LACCELEM (renumerote directement)
C--------------------------------------
      IF (NACCELM>0)
     .  CALL W_LACC(LACCELM,NACCELM,NODLOCAL,LEN_IA,CEL,CEP,P-1)
C--------------------------------------
C LGAUGE (renumerote directement)
C--------------------------------------
      IF (NBGAUGE>0)
     .  CALL W_LGAU(LGAUGE,NBGAUGE,NODLOCAL,LEN_IA,CEL,CEP,P-1)
C--------------------------------------
C LSENSOR
C--------------------------------------
      IF (NSENSOR>0)
     .  CALL W_LSENS(LSENSOR,NSENSOR,NSENSI,NODLOCAL,LEN_IA)
C--------------------------------------
C LLINK
C--------------------------------------
      IF (NLINK>0)
     .  CALL W_LLINK(NNLINK,LLLINK,NODLOCAL,P-1,NLLINK_L,LEN_IA)
C--------------------------------------
C USERWINDOW Partie Entiere
C--------------------------------------
      IF(P==1.AND.IUSERWI/=0)THEN
        ALLOCATE(IUSERW_L(SIUSERW_L))
        CALL SPMD_USERWI_REST(IUSERW,IUSERW_L,NODLOCAL)
        CALL  WRITE_I_C(IUSERW_L,SIUSERW_L)
        DEALLOCATE(IUSERW_L)
      ENDIF
C--------------------------------------
C IPARG
C--------------------------------------
      CALL W_IPARG(IPARG,P-1,NGROUP_L,LEN_IA)
C--------------------------------------
C DD_IAD
C--------------------------------------
      CALL  WRITE_I_C(DD_IAD,(NSPMD+1)*NSPGROUP)
      LEN_IA = LEN_IA + (NSPMD+1)*NSPGROUP
C--------------------------------------
C IGRAV
C--------------------------------------
      IF (NGRAV>0)
     .  CALL W_IGRAV(IGRAV,LGRAV,NODLOCAL,P-1,LLGRAV_L,LEN_IA,NUMNOD_L)
C--------------------------------------
C IEXLNK (RAD2RAD)
C--------------------------------------
      IF (NR2RLNK>0) THEN
        CALL WRITE_I_C(IEXLNK,NR2R*NR2RLNK)
        LEN_IA = LEN_IA + NR2R*NR2RLNK
      END IF
C--------------------------------------
C IBVEL
C--------------------------------------
      IF (NIBVEL>0)
     .  CALL W_IBVEL(IBVEL,LBVEL,NODLOCAL,P-1,LLBVEL_L,LEN_IA)
C--------------------------------------
C IRBE2
C--------------------------------------
      IF (NRBE2>0) THEN
        IF(NBDDRBE2>0) THEN
         ALLOCATE(ITABRBE2M(NUMNOD_L))
        ELSE
         ALLOCATE(ITABRBE2M(1))
        ENDIF
       CALL W_IRBE2(IRBE2   ,LRBE2  ,NODLOCAL ,P-1     ,
     .              LLRBE2_L,ITABRBE2M,NBDDRBE2,NUMNOD_L,LEN_IA  )
      ELSE
       ALLOCATE(ITABRBE2M(1))
      ENDIF
C--------------------------------------
C IRBE3
C--------------------------------------
      IF (NRBE3>0) THEN
        IF(NBDDRBE3M>0) THEN
         ALLOCATE(ITABRBE3M(NUMNOD_L))
        ELSE
         ALLOCATE(ITABRBE3M(1))
        ENDIF
       CALL W_IRBE3(IRBE3   ,LRBE3  ,NODLOCAL ,P-1     ,
     .              LLRBE3_L,ITABRBE3M,NBDDRBE3M,NUMNOD_L,LEN_IA)
      ELSE
       ALLOCATE(ITABRBE3M(1))
      ENDIF
C--------------------------------------
C IACTIV
C--------------------------------------
      IF (NACTIV>0)
     .  CALL WRITE_I_C(IACTIV,LACTIV*NACTIV)
      LEN_IA = LEN_IA + LACTIV*NACTIV
C--------------------------------------
C MPC (entier)
C--------------------------------------
      IF (NUMMPC>0.AND.P==1)THEN
        CALL W_IBMPC(IBMPC,NUMMPC,LMPC,NODLOCAL,LAG_SEC,LEN_IA)
      END IF
C--------------------------------------
C KINET                                            
C--------------------------------------
      CALL W_INLOC(KINET,NODGLOB,NUMNOD_L,LEN_IA)
C--------------------------------------------
C FASOLFR : "external segments for solids"                           
C--------------------------------------------
      IF(NFASOLFR_L/=0)THEN
        CALL W_FASOLFR(FASOLFR ,CEP ,P-1 ,CEL, NFASOLFR_L,
     .                 LEN_IA  )
      ENDIF
C--------------------------------------------
C SEGQUADFR : "external segments for quads"
C--------------------------------------------
      IF(NSEGQUADFR_L/=0)THEN
        CALL W_SEGQUADFR(SEGQUADFR ,CEP ,P-1 ,CEL, NSEGQUADFR_L,
     .                 LEN_IA  )
      ENDIF
C--------------------------------------
C INTBUF_TAB
C--------------------------------------
      IF(NINTER>0) THEN
        IF(NBDDI2M>0)THEN
          ALLOCATE(ITABI2M(NUMNOD_L))
        ELSE
          ALLOCATE(ITABI2M(1))
        ENDIF

c       start CPU timer for INTBUF_INI
        CALL STARTIME(6,1)

        !allocate local structure INTBUF_TAB_L to stock local sizes for interfaces
        ALLOCATE(INTBUF_TAB_L(NINTER))

c       start CPU timer for INTBUF_INI
        CALL STARTIME(6,1)

        !need to allocate only once at first passage in lectur
        FLAG_ALLOCATE = 0

        !initialize local sizes in local structure
        CALL INTBUF_INI_STARTER(INTBUF_TAB_L, IPARI_L, NUMNOD_L,
     .                          I11FLAG, FLAG_ALLOCATE, P-1,INTBUF_FRIC_TAB)

        !write local IPARI_L
        CALL WRITE_I_C(IPARI_L,NPARI*NINTER)
c       stop CPU timer for INTBUF_INI
        CALL STOPTIME(6,1)

        IF (I22LEN_L>0)THEN
          !init tableau de 0
          CALL W_BUFBRIC_22()
        ENDIF

c       start CPU timer for SPLIT_INTERFACES
        CALL STARTIME(7,1)
        CALL SPLIT_INTERFACES(INTBUF_TAB, IPARI   , P-1     , INTBUF_TAB_L,
     .                        IPARI_L   , INTERCEP, NODLOCAL,
     .                        ITAB      , ITABI2M , NBDDI2M , NUMNOD_L    ,
     .                        CEP       , CEL     , IGRBRIC , T8, MULTI_FVM,
     .                        TAG_NM    ,NINDX_NM ,INDX_NM  ,TAG_SCRATCH,NINDX_SCRT,
     .                        INDX_SCRT ,FLAG_24_25,I24MAXNSNE,INTBUF_FRIC_TAB)


        DEALLOCATE(INTBUF_TAB_L)
c       stop CPU timer for SPLIT_INTERFACES
        CALL STOPTIME(7,1)
      ELSE
       ALLOCATE(ITABI2M(1))
      ENDIF

C--------------------------------------
C RW et RW ALE
C--------------------------------------
      IF(NRWALL > 0)
     +  CALL W_RWALL(NPRW    ,LPRW    ,RWL   ,
     +               CEP     ,CEL     ,NODLOCAL,P-1   ,
     +               NSLAW_L,NESLAW_L,NUMNOD_L,LEN_IA)
C--------------------------------------------
C Madymo
C--------------------------------------------
      IF(NCONX+NEXMAD/=0)
     +  CALL W_MAD(IEXMAD  ,NMADSH4_L,NMADSH3_L,NMADSOL_L,NMADNOD_L,
     +             MADCL_NMADNOD_L,CEP ,P-1      ,NODLOCAL ,CEL ,
     +             NUMELS_L,NUMELC_L ,NUMELTG_L,LEN_IA   )
C--------------------------------------------
C RBY
C--------------------------------------------
      IF(NRBYKIN+NRBYLAG>0)
     +  CALL W_RBYK(NPBY,LPBY,NODLOCAL,P-1,NSLARB_L,LEN_IA)
C--------------------------------------------
C Rivets
C--------------------------------------------
      IF(NRIVET>0)
     +  CALL W_IRIVET(IXRI,NODLOCAL,P-1,LEN_IA)
C--------------------------------------------
C Sections
C--------------------------------------------
      IF(NSECT>0)
     +  CALL W_SECTIO(NSTRF  ,CEP   ,CEL,P-1,
     2                NSTRF_L,NODLOCAL,LEN_IA)
      IF(NSECT>0) THEN
        CALL WRITE_I_C(NOM_SECT,SNOM_SECT)
        LEN_IA = LEN_IA + SNOM_SECT
      ENDIF
C--------------------------------------------
C cylindrical joints
C--------------------------------------------
      IF(NJOINT>0) THEN
        CALL WRITE_JOINT(LJOINT,CEP,CEL,P-1,
     .                    NODLOCAL,LJOINT_L,LEN_IA,NUMNOD_L)
        IF(JOINT_SMS.AND.P==1) CALL W_JOINT_SMS(LJOINT,CEP,CEL,P-1,
     +                                            NODLOCAL,LJOINT_L,LEN_IA)
      ENDIF
C--------------------------------------------
C Porosite
C--------------------------------------------
      IF(NUMPOR>0)THEN
        CALL W_PORO(PORNOD,P-1,NUMPOR_L,GEO,NODLOCAL,LEN_IA)
      ENDIF
C--------------------------------------------
C Airbags
C--------------------------------------------
      IF(NVOLU>0)THEN
C        LENVOLU = NIMV*NVOLU+LICBAG+LIBAGJET+LIBAGHOL+LIBAGALE
        CALL W_MONVOL(MONVOL,LENVOLU,NODLOCAL,LEN_IA,MONVOL_L,ITAB,T_MONVOL,P)
      ENDIF
C--------------------------------------------
C LAG MULT
C--------------------------------------------
      IF(LAG_NCF>0.AND.P==1) THEN
        CALL WRITE_I_C(LAGBUF,LAG_NHF+3*LAG_NCF+2)
        LEN_IA = LEN_IA + LAG_NHF+3*LAG_NCF+2
      END IF
C--------------------------------------
C     ECRITURE ICNDS10 Itet=2 of S10
C--------------------------------------
      IF (NS10E_L > 0) THEN
        IF(NBDDCNDM > 0) THEN
         ALLOCATE(ITABCNDM(NUMNOD_L))
        ELSE
         ALLOCATE(ITABCNDM(1))
        ENDIF
       CALL W_ICNDS10(ICNDS10,ITAGND,P-1,NODLOCAL,NS10E_L,
     .                ITABCNDM,NBDDCNDM,NUMNOD_L,LEN_IA ,itab)
      ELSE
       ALLOCATE(ITABCNDM(1))
      ENDIF
C--------------------------------------------
C TH Groups
C--------------------------------------------
      CALL W_TH(ITHGRP ,ITHBUF ,LENTHG,NODLOCAL,CEL   ,
     2          NTHGRP0,NTHGRP ,P       ,LEN_IA,
     3          CELSPH ,NUMLOCCLUSTER,NBR_TH_MONVOL)

      IF (NTHGRP01(1) > 0) THEN
        CALL W_TH(ITHGRPA    ,ITHBUFA    ,SITHBUFA,NODLOCAL,CEL   ,
     2            NTHGRP01(1),NTHGRP1(1) ,P       ,LEN_IA,
     3            CELSPH, NUMLOCCLUSTER ,NBR_TH_MONVOL)
      END IF

      IF (NTHGRP01(2) > 0) THEN
        CALL W_TH(ITHGRPB ,ITHBUFB ,SITHBUFB,NODLOCAL,CEL   ,
     2            NTHGRP01(2),NTHGRP1(2) ,P ,LEN_IA  ,
     3            CELSPH, NUMLOCCLUSTER ,NBR_TH_MONVOL)
      END IF

      IF (NTHGRP01(3) > 0) THEN
        CALL W_TH(ITHGRPC ,ITHBUFC ,SITHBUFC,NODLOCAL,CEL   ,
     2            NTHGRP01(3),NTHGRP1(3) ,P ,LEN_IA  ,
     3            CELSPH, NUMLOCCLUSTER ,NBR_TH_MONVOL)
      END IF

      IF (NTHGRP01(4) > 0) THEN
        CALL W_TH(ITHGRPD ,ITHBUFD ,SITHBUFD,NODLOCAL,CEL   ,
     2            NTHGRP01(4),NTHGRP1(4) ,P ,LEN_IA  ,
     3             CELSPH, NUMLOCCLUSTER ,NBR_TH_MONVOL)
      END IF

      IF (NTHGRP01(5) > 0) THEN
        CALL W_TH(ITHGRPE ,ITHBUFE ,SITHBUFE,NODLOCAL,CEL   ,
     2          NTHGRP01(5),NTHGRP1(5) ,P ,LEN_IA    ,
     3          CELSPH, NUMLOCCLUSTER  ,NBR_TH_MONVOL)
      END IF

      IF (NTHGRP01(6) > 0) THEN
        CALL W_TH(ITHGRPF ,ITHBUFF ,SITHBUFF,NODLOCAL,CEL   ,
     2            NTHGRP01(6),NTHGRP1(6) ,P ,LEN_IA  ,
     3            CELSPH, NUMLOCCLUSTER ,NBR_TH_MONVOL)
      END IF

      IF (NTHGRP01(7) > 0) THEN
        CALL W_TH(ITHGRPG ,ITHBUFG ,SITHBUFG,NODLOCAL,CEL   ,
     2            NTHGRP01(7),NTHGRP1(7) ,P ,LEN_IA  ,
     3            CELSPH, NUMLOCCLUSTER  ,NBR_TH_MONVOL)
      END IF

      IF (NTHGRP01(8) > 0) THEN
        CALL W_TH(ITHGRPH ,ITHBUFH ,SITHBUFH,NODLOCAL,CEL   ,
     2            NTHGRP01(8),NTHGRP1(8) ,P ,LEN_IA  ,
     3            CELSPH, NUMLOCCLUSTER  ,NBR_TH_MONVOL)
      END IF

      IF (NTHGRP01(9) > 0) THEN
        CALL W_TH(ITHGRPI ,ITHBUFI ,SITHBUFI,NODLOCAL,CEL   ,
     2            NTHGRP01(9),NTHGRP1(9) ,P ,LEN_IA  ,
     3            CELSPH, NUMLOCCLUSTER  ,NBR_TH_MONVOL)
      END IF
C--------------------------------------
C Frontieres SPMD
C--------------------------------------
      NUMEL = MAX(NUMELS,NUMELQ)
      NUMEL_L2 = MAX(NUMELS_L,NUMELQ_L)
      IF (MULTI_FVM%IS_USED .AND. N2D /= 0 .AND. NUMELTG /= 0) THEN
         NUMEL = MAX(NUMEL, NUMELTG)
         NUMEL_L2 = MAX(NUMEL_L2, NUMELTG_L)
      ENDIF
      NBCFD = MAX(NRCVVOIS,NSNDVOIS,NERVOIS,NESVOIS,NSEGFL_L)
      IF(NUMSKW>0) CALL SPLIT_SKEW(P,ISKWP_L,ISKWP,TAG_SKN,MULTIPLE_SKEW)
      CALL W_FRONT(
     1   P       ,NBDDACC ,NBDDKIN ,NODLOCAL,
     2   NBDDPROC,NBDDBOUN,NODGLOB ,NUMNOD_L,NBDDNRB ,
     3   NPBY    ,LPBY    ,NPRW    ,LPRW    ,LEN_IA  ,
     4   DD_RBY2 ,ITABI2M ,NBDDI2M ,CEP     ,MONVOL  ,
     5   NNLINK  ,LLLINK  ,LJOINT  ,
     6   NBDDNCJ ,IBVEL   ,LBVEL   ,NBDDNRBM,DD_RBM2 ,
     7   NSTRF   ,NNODT_L ,NNODL_L ,IEXMAD  ,ISP0    ,
     8   NRCVVOIS,NSNDVOIS,NERVOIS ,NESVOIS ,
     9   NSEGFL_L,IPARG   ,
     A   NUMEL   ,ALE_CONNECTIVITY  ,NBCFD   ,IXS     ,IXQ     ,IXTG,
     B   NUMELS_L,NUMELQ_L,NUMELTG_L,CEL     ,GEO     ,PORNOD  ,
     C   NUMPOR_L,NUMEL_L2,IPARI   ,INTBUF_TAB,NBI18_L ,
     D   IEXLNK  ,IGRNOD  ,DD_LAGF ,NLAGF_L ,IADLL   ,
     E   LLL     ,ISKWP   ,NSKWP   ,ISENSP  ,NSENSP  ,
     F   IACCP   ,NACCP   ,IRBE3   ,LRBE3   ,ITABRBE3M,
     G   NBDDRBE3M,IRBYM   ,LCRBYM  ,FRONT_RM ,DD_RBYM2,
     H   NBDDNRBYM,IRBE2   ,LRBE2   ,NBDDRBE2,ITABRBE2M,
     I   USERNODS,IEDGE_TMP,NODEDGE,EDGELOCAL,NBDDEDGE_L,
     J   IGAUP   ,NGAUP    ,FRONTB_R2R,SDD_R2R_ELEM,ADDCSRECT,
     K   CSRECT  ,NBDDNORT_L,NBDDNOR_MAX_L,NBCCFR25_L,NBCCNOR_L,
     L   NUMNOR_L ,NBDDEDGT_L,NBDDEDG_MAX_L,INTERCEP ,NBDDCNDM,
     M   ITABCNDM,MULTI_FVM,IGRSURF,ISKWP_L,ALE_ELM,
     N   SIZE_ALE_ELM  )
      DEALLOCATE(ITABI2M)
      DEALLOCATE(ITABRBE3M)
      DEALLOCATE(ITABRBE2M)
      DEALLOCATE(ITABCNDM)
      DEALLOCATE(ISKWP_L)
C--------------------------------------
C Parith/ON SPMD
C--------------------------------------
      IF(IPARI0/=0)
     +  CALL W_PON(
     1      ADDCNE   ,CNE      ,LCNE      ,NUMNOD_L  ,NODGLOB   ,
     2      LCNE_L   ,CEP      ,CEL       ,IXS       ,IXS10     ,
     3      IXS20    ,IXS16    ,IXQ       ,IXC       ,IXT       ,
     4      IXP      ,IXR      ,IXTG      ,IXUR      ,MONVOL    ,
     5      IBCL     ,GEO      ,IGEO      ,P         ,
     6      NUMELS_L ,NUMELS8_L,NUMELS10_L,NUMELS16_L,NUMELS20_L,
     7      NUMELQ_L ,NUMELC_L ,NUMELT_L  ,NUMELP_L  ,NUMELR_L  ,
     8      NUMELTG_L,NUMELUR_L,NSKYRW_L  ,NPRW      ,LPRW      ,
     9      NSKYRBK_L,NPBY      ,LPBY     ,DD_RBY2   ,
     A      I2NSNT   ,I2NSN_L  ,IPARI     ,NIR       ,
     B      LCNI2_L  ,NISKYI2_L,CEPI2     ,CELI2     ,CNI2      ,
     C      ADDCNI2  ,NBDDI2M  ,NCONLD_L  ,IXTG6     ,NUMELTG6_L,
     D      NNMV_L   ,NNMVC_L  ,NSKYLL_L  ,NNLINK    ,LLLINK    ,
     E      NSKYRBM_L,DD_RBM2  ,IBVEL     ,LBVEL     ,NBI18_L   ,
     F      NSKYI18_L,LEN_IA   ,NCONV_L   ,IBCV     ,NSKYRBE3_L,
     G      IRBE3    ,LRBE3    ,NSKYRBMK_L,IRBYM     ,LCRBYM    ,
     H      FRONT_RM ,DD_RBYM2 ,IBCR      ,NRADIA_L  ,ADDCNE_PXFEM,
     I      CNE_PXFEM ,CEL_PXFEM ,LCNEPXFEM_L,INOD_PXFEM,IEL_PXFEM,
     J      NUMELCPXFE_L , NUMNODPXFE_L   ,LLOADP   ,ILOADP   ,
     K      LLLOADP_L,ADDCNE_CRKXFEM,CNE_CRKXFEM,CEL_CRKXFEM,
     L      LCNECRKXFEM_L,INOD_CRKXFEM,IEL_CRKXFEM,NUMELCCRKXFE_L,
     M      NUMNODCRKXFE_L,NUMELTGCRKXFE_L,CEP_CRKXFEM,INCRKXFEM_L,
     N      CRKNODIAD ,INTBUF_TAB,NUMELIG3D_L,KXIG3D,IXIG3D,
     O      IBFFLUX   ,NFXFLUX_L ,CEPCND  ,CELCND   ,ADDCNCND    ,
     P      CNCND     ,NS10E_L   ,ICNDS10 ,LCNCND_L ,ITAGND ,IGRSURF,
     Q      IGRSURF_PROC ,NEBCS_NRF, EBCS_TAB_LOC_2)

      ! -------------------
      CALL EBCS_TAB_LOC_2%destroy()
      ! -------------------
      CALL COMPRESS_I_NNZ(IGEO,LEN_G)
      LEN_IA = LEN_IA + LEN_G
      CALL COMPRESS_I_NNZ(IPM,LEN_M)
      LEN_IA = LEN_IA + LEN_M
C--------------------------------------
C KCONTACT                                        
C--------------------------------------
      IF(KCONTACT/=0)THEN
        CALL W_INLOC(ICONTACT,NODGLOB,NUMNOD_L,LEN_IA)
      ENDIF
C--------------------------------------
      LEN_S=NPART
      CALL WRITE_I_C(IPART_STATE ,LEN_S)
      LEN_IA = LEN_IA + LEN_S
C--------------------------------------
C Adaptive meshing (integers)
C--------------------------------------
      IF(NADMESH /= 0)THEN
        CALL WRITE_I_C(SH4TREE,KSH4TREE*NUMELC)
        LEN_IA = LEN_IA + KSH4TREE*NUMELC
        CALL WRITE_I_C(SH3TREE,KSH3TREE*NUMELTG)
        LEN_IA = LEN_IA + KSH3TREE*NUMELTG
        CALL WRITE_I_C(IPADMESH,KIPADMESH*NPART)
        LEN_IA = LEN_IA + KIPADMESH*NPART
        LEN_ADM=ABS(LSH4TRIM)
        CALL WRITE_I_C(SH4TRIM,LEN_ADM)
        LEN_IA = LEN_IA + LEN_ADM
        LEN_ADM=ABS(LSH3TRIM)
        CALL WRITE_I_C(SH3TRIM,LEN_ADM)
        LEN_IA = LEN_IA + LEN_ADM
      END IF
C--------------------------------------
C Interfaces 
C--------------------------------------
        CALL STARTIME(8,1)

        CALL W_FI(IPARI,P-1,LEN_IA,
     1            INTERCEP  ,INTBUF_TAB,ITAB,MULTI_FVM,TAG_SCRATCH,
     2            NINDX_SCRT,INDX_SCRT ,NODLOCAL,NUMNOD_L)

c       stop CPU timer for W_FI
        CALL STOPTIME(8,1)

      IF(NUMSPH>0.AND.NSPMD>1)THEN
         CALL W_FSPH()
      ENDIF
C--------------------------------------
C structure int21 (partie entiere)
C--------------------------------------
      IF(NINTSTAMP /= 0)THEN
        CALL INTSTAMP_WRESTI(INTSTAMP, LINTSTAMP, NODLOCAL)
        LEN_IA=LEN_IA+LINTSTAMP
      END IF
C--------------------------------------
C interface friction (integers)
C--------------------------------------
       IF(NINTERFRIC > 0) THEN
          CALL INTFRIC_WRESTI(INTBUF_FRIC_TAB, LINTFRIC)
          LEN_IA=LEN_IA+LINTFRIC
       ENDIF
C--------------------------------------
C structure tables (partie entiere)
C--------------------------------------
      IF(NTABLE /= 0)THEN
        CALL TABLE_WRESTI(TABLE, LTABLE)
        LEN_IA=LEN_IA+LTABLE
      END IF
C-------------------------------------------
      ! WRITE(6,*) "SITHVAR= ",SITHVAR
      CALL WRITE_I_C(ITHVAR,SITHVAR)
      LEN_IA =  LEN_IA + SITHVAR
C--------------------------------------
C ICFIELD
C--------------------------------------
      IF (NLOADC>0)CALL W_ICFIELD(ICFIELD,LCFIELD,NODLOCAL,P-1,LLCFIELD_L,LEN_IA,P,NUMNOD_L)
      IF (NLOADP>0)CALL W_ILOADP(ILOADP,LLOADP,NODLOCAL,P-1,LLLOADP_L,LEN_IA,P,CEP,ITAB)
C--------------------------------------------
C LOAD/PBLAST
C--------------------------------------------
      IF(NLOADP_B>0)THEN
        CALL PBLAST_WRITE_STARTER(PBLAST_TAB,P-1,CEP)
      ENDIF
C
      IF(ICRACK3D > 0)THEN
C--------------------------------------
C Boundary nodes for xfem
C--------------------------------------
        CALL W_INLOC(IBORDNODE,NODGLOB,NUMNOD_L,LEN_IA)
C--------------------------------------------
C Shell common edges (SHELL 4-N + SHELL 3-N  XFEM)
C--------------------------------------------
        CALL WRITE_I_C(IEDGESH4_L,4*NUMELCCRKXFE_L)
        LEN_IA =  LEN_IA + 4*NUMELCCRKXFE_L
        CALL WRITE_I_C(IEDGESH3_L,3*NUMELTGCRKXFE_L)
        LEN_IA =  LEN_IA + 3*NUMELTGCRKXFE_L
C
        CALL WRITE_I_C(NODEDGE_L,2*NUMEDGES_L)
        LEN_IA = LEN_IA + 2*NUMEDGES_L
        CALL WRITE_I_C(IEDGE_L,NUMEDGES_L)
        LEN_IA = LEN_IA + NUMEDGES_L
C--------------------------------------
C       NODGLOB nodxfem
C--------------------------------------
        CALL WRITE_I_C(NODGLOBXFE,NOD_XFE_L)
        LEN_IA = LEN_IA + NOD_XFE_L
        IF(ALLOCATED(NODGLOBXFE))DEALLOCATE(NODGLOBXFE)
      END IF
C--------------------------------------
C     Isogeo                                      
C--------------------------------------
      IF (NUMELIG3D>0)
     .  CALL W_ELIG3D(KXIG3D,NUMELIG3D,ISUMNIG3D_L,IXIG3D,
     +              P-1,NODLOCAL,LEN_IA)
C--------------------------------------
C TAG_SKINS6 : used for /H3D/STRESS/TENS/OUTER
C--------------------------------------
      CALL W_IELOC(TAG_SKINS6,CEP,P-1,NUMELS,NUMELS_L,LEN_IA)
C--------------------------------------
C     /BCS/CYCLIC
C--------------------------------------
      IF (NBCSCYC_L > 0)
     .    CALL W_IBCSCYC(IBCSCYC,LBCSCYC,NODLOCAL ,P-1,LLBCSCYC_L,
     .                   NBCSCYC_L,LEN_IA )
C--------------------------------------
C     Write REAL variable                               
C--------------------------------------
      LEN_AM = 0
      CALL WRITE_DB(TABVREA, LVARREA)
      LEN_AM = LEN_AM + LVARREA
C--------------------------------------------
C X, D, V, VR, DR
C--------------------------------------------

C--------------------------------------------
      CALL W_R3NLOC(X,NODGLOB,NUMNOD_L,LEN_AM)
      IF(IALE+IEULER+ITHERM+IALELAG/=0)
     .  CALL W_BID(3*NRCVVOIS,LEN_AM)
      CALL W_R3NLOC(D,NODGLOB,NUMNOD_L,LEN_AM)
      IF(IALE+IEULER+ITHERM+IALELAG/=0)
     .  CALL W_BID(3*NRCVVOIS,LEN_AM)
      CALL W_R3NLOC(V,NODGLOB,NUMNOD_L,LEN_AM)
      IF(IALE+IEULER+ITHERM+IALELAG/=0)
     .  CALL W_BID(3*NRCVVOIS,LEN_AM)
      IF(IRODDL>0) THEN
        CALL W_R3NLOC(VR,NODGLOB,NUMNOD_L,LEN_AM)
        IF(ISECUT>0.OR.ISUB>0.OR.MSHSUB>0.OR.IISROT>0.OR.
     .     IMPOSE_DR/=0.OR.IDROT==1)
     .    CALL W_R3NLOC(DR,NODGLOB,NUMNOD_L,LEN_AM)
      ENDIF
C--------------------------------------------
C THKE SHELL 4-N + SHELL 3-N
C--------------------------------------------
      OFF = NUMELS + NUMELQ
      CALL W_RELOC(THKE,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELC,NUMELC_L,LEN_AM)
C      OFF = OFF + NUMELC + NUMELT + NUMELP + NUMELR + NUMELUR
      OFF = OFF + NUMELC + NUMELT + NUMELP + NUMELR
      CALL W_RELOC(
     .   THKE(NUMELC+1),CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELTG,NUMELTG_L,LEN_AM)
C--------------------------------------------
C DAMPING
C--------------------------------------------
      IF (NDAMP>0) THEN
        CALL WRITE_DB(DAMPR,NRDAMP*NDAMP)
        LEN_AM = LEN_AM + NRDAMP*NDAMP

        IF(IRODDL>0)THEN
          CALL W_RNNLOC(DAMP,NODGLOB,NUMNOD_L,6,LEN_AM)
        ELSE
          CALL W_R3NLOC(DAMP,NODGLOB,NUMNOD_L,LEN_AM)
        ENDIF
      ENDIF
C--------------------------------------------
C Mass et Inertia
C--------------------------------------------
      CALL W_RNLOC(MS,NODGLOB,NUMNOD_L,LEN_AM)

C------Mass in 2D : MS_2D table is written here (MS_2D = MS in starter)-----
      IF(N2D>0)THEN
         CALL W_RNLOC(MS,NODGLOB,NUMNOD_L,LEN_AM)
      ENDIF

      IF(IRODDL>0)
     .  CALL W_RNLOC(IN,NODGLOB,NUMNOD_L,LEN_AM)
C--------------------------------------------
C Functions
C--------------------------------------------
      IF(NFUNCT/=0)THEN
        CALL WRITE_DB(TF,NPTS)
      LEN_AM = LEN_AM + NPTS
      ENDIF
C--------------------------------------------
C Properties Mat
C--------------------------------------------
      CALL WRITE_DB(PM,NPROPM*NUMMAT)
      LEN_AM = LEN_AM + NPROPM*NUMMAT
c
      CALL WRITE_MATPARAM(MATPARAM_TAB,NUMMAT,LEN)
      LEN_AM = LEN_AM + LEN
C--------------------------------------
C ELGROUP_PARAM
C--------------------------------------
      CALL WRITE_ELGROUP_PARAM(GROUP_PARAM_TAB,IPARG,P-1,NGROUP_L,LEN)
      LEN_AM = LEN_AM + LEN
C--------------------------------------------
C Skew et Frames
C--------------------------------------------
      CALL WRITE_DB(SKEW,LSKEW*(NUMSKW+1))
      LEN_AM = LEN_AM + LSKEW*(NUMSKW+1)
      IF(NUMSPH>0.AND.NSPCOND>0)THEN
        CALL W_SKWSPH(
     .   SKEW(LSKEW*(NUMSKW+1)+1),NUMSPH_L,CEPSP,P-1,LEN_AM)
          IF(NSUBMOD>0)
     .        CALL W_SKWSUB(
     .        SKEW(LSKEW*(NUMSKW+1+NUMSPH_L)+1),LEN_AM)
      ELSEIF(NSUBMOD>0)THEN
        CALL W_SKWSUB(
     .    SKEW(LSKEW*(NUMSKW+1)+1),LEN_AM)
      ENDIF
      CALL WRITE_DB(XFRAME,NXFRAME*(NUMFRAM+1))
      LEN_AM = LEN_AM + NXFRAME*(NUMFRAM+1)
C--------------------------------------------
C Properties Geo
C--------------------------------------------
      IF(NUMPOR==0)THEN
        CALL COMPRESS_R_NNZ(GEO,NUMGEO*NPROPG)
      ELSE
        CALL W_GEO(GEO,NODLOCAL)
      END IF
      LEN_AM = LEN_AM + NPROPG*NUMGEO
C--------------------------------------------
C Eani solide+quad+shell+triangle
C--------------------------------------------
      OFF = 0
      CALL W_RELOC(EANI(OFF+1),CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELS,NUMELS_L,LEN_AM)
      OFF = OFF + NUMELS
      CALL W_RELOC(EANI(OFF+1),CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELQ,NUMELQ_L,LEN_AM)
      OFF = OFF + NUMELQ
      CALL W_RELOC(EANI(OFF+1),CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELC,NUMELC_L,LEN_AM)
      SHF = OFF + NUMELC
C      OFF = OFF + NUMELC + NUMELT + NUMELP + NUMELR + NUMELUR
      OFF = OFF + NUMELC + NUMELT + NUMELP + NUMELR
      CALL W_RELOC(EANI(SHF+1),CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELTG,NUMELTG_L,LEN_AM)
C--------------------------------------------
C BUFMAT BUFGEO BUFSF
C--------------------------------------------
      CALL WRITE_DB(BUFMAT,LBUFMAT)
      LEN_AM = LEN_AM + LBUFMAT
      IF(IRESP == 1)THEN
C In BUFGEO is a double precision Buffer, even in Single Precision
        ALLOCATE (RBUFGEO(LBUFGEO))
        RBUFGEO(1:LBUFGEO)=BUFGEO(1:LBUFGEO)
        CALL WRITE_DB(RBUFGEO,LBUFGEO)
      ELSE
        CALL WRITE_DB(BUFGEO,LBUFGEO)
      ENDIF
      LEN_AM = LEN_AM + LBUFGEO
      CALL WRITE_DB(BUFSF ,LBUFSF )
      LEN_AM = LEN_AM + LBUFSF
C LAG options
      IF(NUMMPC>0.AND.P==1)THEN
        CALL WRITE_DB(BRMPC,LMPC)
        LEN_AM = LEN_AM + LMPC
      END IF
      IF(NGJOINT>0.AND.P==1)THEN
        CALL WRITE_DB(GJBUFR,LKJNR*NGJOINT)
        LEN_AM = LEN_AM + LKJNR*NGJOINT
      END IF
C--------------------------------------------
C W, VEUL, WB, ASAV, DSAV,
C--------------------------------------------
      IF(IALE/=0) THEN
        CALL W_R3NLOC(W,NODGLOB,NUMNOD_L,LEN_AM)
      ENDIF
      IF(IEULER * (1 - IMULTI_FVM)/=0)THEN
        IF(N2D==0) THEN
          OFF = 0
          CALL W_RELOC2(
     .      VEUL,LVEUL,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELS,NUMELS_L,LEN_AM)
        ELSE
          OFF = NUMELS
          CALL W_RELOC2(VEUL,10,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELQ,NUMELQ_L,LEN_AM)
        ENDIF
      ENDIF
C--------------------------------------------
C     Multifluid law velocity
C--------------------------------------------
      IF (MULTI_FVM%IS_USED) THEN
         CALL WRITE_DB(MULTI_FVM%PRES_SHIFT, 1)
         IF (N2D == 0) THEN
            CALL WRITE_DB(MULTI_FVM_VEL_L(1, 1:NUMELS_L),
     .           NUMELS_L)
            CALL WRITE_DB(MULTI_FVM_VEL_L(2, 1:NUMELS_L),
     .           NUMELS_L)
            CALL WRITE_DB(MULTI_FVM_VEL_L(3, 1:NUMELS_L),
     .           NUMELS_L)
         ELSE
            CALL WRITE_DB(MULTI_FVM_VEL_L(1, 1:NUMELQ_L + NUMELTG_L),
     .           NUMELQ_L + NUMELTG_L)
            CALL WRITE_DB(MULTI_FVM_VEL_L(2, 1:NUMELQ_L + NUMELTG_L),
     .           NUMELQ_L + NUMELTG_L)
            CALL WRITE_DB(MULTI_FVM_VEL_L(3, 1:NUMELQ_L + NUMELTG_L),
     .           NUMELQ_L + NUMELTG_L)
         ENDIF
      ENDIF
C
C multi mat 2D: mono SPMD
C
      IF(NMULT > 0) THEN
        CALL W_RFILLOC(FILL,NODGLOB,NUMNOD,NUMNOD_L,NMULT,LEN_AM)
        CALL W_RFILLOC(DFILL,NODGLOB,NUMNOD,NUMNOD_L,NMULT,LEN_AM)
        CALL WRITE_DB(ALPH,SALPH)
        LEN_AM = LEN_AM + SALPH
      END IF
      IF(NWALE==2) THEN
        CALL W_R3NLOC(WB,NODGLOB,NUMNOD_L,LEN_AM)
      ELSEIF(NWALE==4) THEN
        CALL W_R3NLOC(WB,NODGLOB,NUMNOD_L,LEN_AM)
        CALL W_RNLOC(WMA,NODGLOB,NUMNOD_L,LEN_AM)
      ENDIF
      IF(MSHSUB==1)THEN
        CALL W_R3NLOC(DSAV ,NODGLOB,NUMNOD_L,LEN_AM)
        CALL W_R3NLOC(DRSAV,NODGLOB,NUMNOD_L,LEN_AM)
        CALL W_R3NLOC(ASAV ,NODGLOB,NUMNOD_L,LEN_AM)
        CALL W_R3NLOC(ARSAV,NODGLOB,NUMNOD_L,LEN_AM)
        CALL W_RNLOC(STSAV ,NODGLOB,NUMNOD_L,LEN_AM)
        CALL W_RNLOC(STRSAV,NODGLOB,NUMNOD_L,LEN_AM)
C       FSAVSAV
        CALL WRITE_DB(FSAVSAV,NTHVKI*NSECT)
        LEN_AM = LEN_AM + NTHVKI*NSECT
      ELSEIF(ILAG==1.AND.(IALE+IEULER)/=0)THEN
        CALL W_R3NLOC(DSAV,NODGLOB,NUMNOD_L,LEN_AM)
        CALL W_R3NLOC(ASAV,NODGLOB,NUMNOD_L,LEN_AM)
      ENDIF
C--------------------------------------
C SPH Cells partie reelle
C--------------------------------------
      IF (NUMSPH>0)THEN
          CALL W_RSPH(SPBUF ,NUMSPH_L,CEPSP,P-1,
     *                VSPHIO,LEN_AM, SSPHVELN_L  )
      ENDIF
C--------------------------------------
C Masses nodales fluides
C--------------------------------------
      IF(IALE==1.OR.IEULER==1.OR.IALELAG==1) THEN
        CALL W_RNLOC(MSNF,NODGLOB,NUMNOD_L,LEN_AM)
      ENDIF
C--------------------------------------------
C Pressure & concentrated loads
C--------------------------------------------
c      CALL WRITE_DB(FAC,NCONLD)
c      LEN_AM = LEN_AM + NCONLD
      IF(NCONLD>0)
     .  CALL W_FAC(FAC,CEP,P-1,NCONLD_L,LEN_AM)
C--------------------------------------------
C Fixvel real
C--------------------------------------------
      IF(NFXVEL_L>0)
     .  CALL W_FIXVEL(VEL,NFXVEL,IBFV,NFXVEL_L,LEN_AM,P-1)
C--------------------------------------------
C FSAV
C--------------------------------------------
      CALL WRITE_DB(FSAV,NTHVKI*
     .     (NINTER+NRWALL+NRBODY+NSECT+NJOINT+NRBAG+NVOLU+
     .      NFXBODY+NINTSUB))
      LEN_AM = LEN_AM + NTHVKI*
     .        (NINTER+NRWALL+NRBODY+NSECT+NJOINT+NRBAG+NVOLU+
     .         NFXBODY+NINTSUB)

      CALL WRITE_DB(FBSAV6,12*6*SFSAV6)

      CALL WRITE_I_C(TABSENSOR,STABSEN)

C--------------------------------------------
C TIMER MAT/PROP and WEIGHT
C--------------------------------------------
       CALL WRITE_I_C(TAB_UMP,TAILLE*7)
C
       CALL WRITE_I_C(POIN_UMP,NUMMAT)
c
       CALL W_TABMAT_PROP(IPARG,IXC,IXTG,IXS,P-1,NGROUP_L,
     .                         POIN_PART_SHELL,POIN_PART_TRI,POIN_PART_SOL,
     .                         MID_PID_SHELL,MID_PID_TRI,MID_PID_SOL,
     .                         IPARTC,IPARTTG,IPARTS)
C--------------------------------------------
C FZERO Equilibre
C--------------------------------------------
      IF(ISIGI==2.OR.ISIGI==4)
     .  CALL W_R3NLOC(FZERO,NODGLOB,NUMNOD_L,LEN_AM)
      IF(IABS(ISIGI)==5)THEN
        OFF = NUMELS+NUMELQ
        CALL W_RELOC2(FZERO,3*4,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELC,NUMELC_L,LEN_AM)
C        OFF = OFF + NUMELC + NUMELT + NUMELP + NUMELR + NUMELUR
        OFF = OFF + NUMELC + NUMELT + NUMELP + NUMELR
        SHF = NUMELC*3*4
        CALL W_RELOC2(FZERO(SHF+1),3*4,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELTG,
     .                NUMELTG_L,LEN_AM)
      ENDIF
C--------------------------------------------
C XLAS
C--------------------------------------------
      IF(NLASER>0.AND.P==1)THEN
        CALL WRITE_DB(XLAS,LENXLAS)
        LEN_AM = LEN_AM + LENXLAS
      ENDIF
C--------------------------------------------
C Accelerometres
C--------------------------------------------
      IF(NACCELM>0)THEN
        CALL WRITE_DB(ACCELM,NACCELM*LLACCELM)
        LEN_AM = LEN_AM + NACCELM*LLACCELM
      ENDIF
C--------------------------------------------
C Gauges
C--------------------------------------------
      IF(NBGAUGE>0)THEN
        CALL WRITE_DB(GAUGE,NBGAUGE*LLGAUGE)
        LEN_AM = LEN_AM + NBGAUGE*LLGAUGE
      ENDIF
C--------------------------------------------
C Sensors
C--------------------------------------------
      IF (NSENSOR > 0) THEN
        CALL WRITE_DB(SENSOR,NSENSOR*NSENSR)
        LEN_AM = LEN_AM + NSENSOR*NSENSR

        CALL WRITE_SENSOR_TAB(SENSOR_TAB,NSENSOR,NODLOCAL,LEN_AM)
      END IF
C--------------------------------------------
C RBM
C--------------------------------------------
      IF(NIBVEL>0)
     .  CALL WRITE_DB(FBVEL,3*NIBVEL)
      LEN_AM = LEN_AM + 3*NIBVEL
C--------------------------------------------
C RBE3
C--------------------------------------------
      IF(NRBE3>0)
     .  CALL W_FRBE3(IRBE3   ,LRBE3  ,P-1    ,LLRBE3_L,
     .               FRBE3   ,FRBE3(3*SLRBE3+1),LEN_AM  )
C--------------------------------------
C NACTIV
C--------------------------------------
      IF (NACTIV > 0) THEN
         CALL WRITE_DB(FACTIV,LRACTIV*NACTIV)
         LEN_AM = LEN_AM + LRACTIV*NACTIV
      ENDIF
C--------------------------------------------
C GRAVFAC
C--------------------------------------------
      IF(NGRAV>0)
     .  CALL WRITE_DB(GRAVFAC,LFACGRV*NGRAV)
      LEN_AM = LEN_AM + LFACGRV*NGRAV
C--------------------------------------------
C FR_WAV
C--------------------------------------------
      IF(IFRWV>0)
     .  CALL W_RNLOC(FR_WAVE,NODGLOB,NUMNOD_L,LEN_AM)
C--------------------------------------------
C FAILWAVE
C--------------------------------------------
      CALL W_FAILWAVE(FAILWAVE,NODGLOB,NUMNOD,NUMNOD_L,LEN_AM,ITAB)
c
C--------------------------------------------
C NONLOCAL DAMAGE
C--------------------------------------------
      CALL WRITE_NLOC_STRUCT(
     .     NLOC_DMG ,NUMNOD_L ,NODGLOB  ,NODLOCAL ,CEL      ,
     .     CEP      ,P        ,IXS      ,IXC      ,IXTG     ,
     .     NUMELS_L ,NUMELC_L ,NUMELTG_L)
c
C--------------------------------------------
C PARTS0
C--------------------------------------------
      CALL WRITE_DB(PARTS0,NPART)
      LEN_AM = LEN_AM + NPART
C--------------------------------------
C USERWINDOW (float)
C--------------------------------------
      IF(P==1.AND.IUSERWI/=0)THEN
        CALL  WRITE_DB(USERW,SUSERW_L)
      ENDIF
C--------------------------------------------
C Element Buffer
C--------------------------------------------
      CALL W_BUFEL(IPARG,P-1,LBUFEL_L,ELBUF,LEN_AM)   ! old buffer
c------------------
      FLAG_XFEM = 0
      CALL W_ELBUF_STR(P-1,IPARG,ELBUF_TAB,LEN_AM,FLAG_XFEM)
c------------------
      IF (ICRACK3D > 0) THEN   ! if XFEM
        DO IXEL=1,NXEL
          FLAG_XFEM = 1
          CALL W_ELBUF_STR(P-1,IPARG,XFEM_TAB(1:NGROUP,IXEL),LEN_AM,FLAG_XFEM)
        ENDDO
      ENDIF
C--------------------------------------------
C Element Clusters
C--------------------------------------------
      IF (NCLUSTER > 0) CALL W_CLUSTER(CLUSTERS,
     .                                 IPARG,NODLOCAL,
     .                                 NCLUSTER_L,CEP,P-1,
     .                                 NUMLOCGROUP,
     .                                 LEN_IA,LEN_AM)
C--------------------------------------------
C RWALL et RWALL ALE
C--------------------------------------------
      CALL W_RWAR(NPRW,LPRW ,RWL, RWSAV,
     +            LWSAV_L,CEP,P-1,LEN_AM)
C--------------------------------------------
C RBY
C--------------------------------------------
      CALL WRITE_DB(RBY,NRBY*NRBYKIN)
      LEN_AM = LEN_AM + NRBY*NRBYKIN
      IF(P == 1 .AND. NRBYLAG > 0) THEN
        CALL WRITE_DB(RBY(NRBY*NRBYKIN+1),NRBY*NRBYLAG)
        LEN_AM = LEN_AM + NRBY*NRBYLAG
      END IF
C--------------------------------------------
C RIVET
C--------------------------------------------
      CALL WRITE_DB(RIVET,NRIVET*NRIVF)
      LEN_AM = LEN_AM + NRIVET*NRIVF
C--------------------------------------------
C SECBUF
C--------------------------------------------
      IF(NSECT>0)
     +  CALL W_SECBUF(SECBUF,LSECBUF_L,NSTRF,P-1,LEN_AM)
C--------------------------------------------
C RVOLU
C--------------------------------------------
      CALL WRITE_DB(RVOLU,NRVOLU*NVOLU+LRCBAG+LRBAGJET+LRBAGHOL
     .                   +LRBAGALE)
      LEN_AM = LEN_AM + NRVOLU*NVOLU+LRCBAG+LRBAGJET+LRBAGHOL
     .                + LRBAGALE
C--------------------------------------------
C LAMBDA
C--------------------------------------------
      NCMAX = LAG_NCF + LAG_NCL
      IF(NCMAX>0.AND.P==1)THEN
        CALL WRITE_DB(LAMBDA,NCMAX)
        LEN_AM = LEN_AM + NCMAX
      END IF
C--------------------------------------------
C RCONX
C--------------------------------------------
      IF(NCONX>0.AND.P==1)THEN
        CALL WRITE_DB(RCONX,NCONX*NRCNX)
        LEN_AM = LEN_AM + NCONX*NRCNX
      END IF
C--------------------------------------------
C Adaptive meshing CONTACT vectors
C--------------------------------------------
      IF(NADMESH /= 0)THEN
        CALL W_RNLOC(RCONTACT,NODGLOB,NUMNOD_L,LEN_AM)
        CALL W_RNLOC(ACONTACT,NODGLOB,NUMNOD_L,LEN_AM)
        CALL W_RNLOC(PCONTACT,NODGLOB,NUMNOD_L,LEN_AM)
      END IF
C--------------------------------------
C    PARTSAV
C--------------------------------------
      IF (NPSAV>=25)THEN
        IF (P==1) THEN
           CALL WRITE_DB(PARTSAV,NPSAV*NPART)
           LEN_AM = LEN_AM + NPSAV*NPART
        ELSE
           RDPARTSAV= 0
           CALL WRITE_DB(RDPARTSAV,NPSAV*NPART)
           LEN_AM = LEN_AM + NPSAV*NPART
        ENDIF
      ENDIF
C--------------------------------------
C     Lengths for checkpoint restart
C--------------------------------------
      MY_ILEN = 0
      MY_RLEN = 0
      CALL WRITE_I_C(MY_ILEN,1)
      CALL WRITE_I_C(MY_RLEN,1)
      LEN_IA=LEN_IA+2
C--------------------------------------
C     Elem Masses 
C--------------------------------------
      IF(NADMESH /= 0 .OR. IREST_MSELT /= 0)THEN
        OFF  = NUMELS + NUMELQ
        CALL W_RELOC2(MSC,1,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELC,NUMELC_L,
     .                LEN_AM )
        CALL W_RELOC2(INC,1,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELC,NUMELC_L,
     .                LEN_AM )
        OFF = OFF + NUMELC + NUMELT + NUMELP + NUMELR
        CALL W_RELOC2(MSTG,1,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELTG,NUMELTG_L,
     .                LEN_AM )
        CALL W_RELOC2(INTG,1,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELTG,NUMELTG_L,
     .                LEN_AM )
        CALL W_RELOC2(PTG,3,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELTG,NUMELTG_L,
     .                LEN_AM )
       END IF

      IF(IREST_MSELT /= 0)THEN
        OFF = 0
        CALL W_RELOC2(MSSA,1,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELS,NUMELS_L,
     .                LEN_AM )
        OFF = NUMELS+NUMELQ+NUMELC
        CALL W_RELOC2(MSTR,1,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELT,NUMELT_L,
     .                LEN_AM )
        OFF = OFF+NUMELT
        CALL W_RELOC2(MSP,1,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELP,NUMELP_L,
     .                LEN_AM )
        OFF = OFF+NUMELP
        CALL W_RELOC2(MSRT,1,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELR,NUMELR_L,
     .                LEN_AM )
       END IF
C--------------------------------------
C Adaptive meshing (float)
C--------------------------------------
      IF(NADMESH /= 0)THEN
        CALL WRITE_DB(PADMESH,KPADMESH*NPART)
        LEN_AM = LEN_AM + KPADMESH*NPART
        IF(ITHERM_FE > 0)THEN
          CALL WRITE_DB(MCPC,NUMELC)
          LEN_AM = LEN_AM + NUMELC
          CALL WRITE_DB(MCPTG,NUMELTG)
          LEN_AM = LEN_AM + NUMELTG
      END IF
      END IF
C
C--------------------------------------------
C CBAPINCHING
C--------------------------------------------
      IF(NPINCH > 0 ) THEN
        CALL WRITE_DB(PINCH_DATA%VPINCH,3*NUMNOD_L)
        LEN_AM = LEN_AM + 3*NUMNOD_L
        DEALLOCATE(PINCH_DATA%VPINCH)

        CALL WRITE_DB(PINCH_DATA%DPINCH,3*NUMNOD_L)
        LEN_AM = LEN_AM + 3*NUMNOD_L
        DEALLOCATE(PINCH_DATA%DPINCH)

        CALL WRITE_DB(PINCH_DATA%XPINCH,3*NUMNOD_L)
        LEN_AM = LEN_AM + 3*NUMNOD_L
        DEALLOCATE(PINCH_DATA%XPINCH)

        CALL WRITE_DB(PINCH_DATA%MSPINCH,NUMNOD_L)
        LEN_AM = LEN_AM + NUMNOD_L
        DEALLOCATE(PINCH_DATA%MSPINCH)
      ENDIF
C
      IF(ISTATCND /= 0)THEN
        CALL W_RNLOC(MSCND,NODGLOB,NUMNOD_L,LEN_AM)
        CALL W_RNLOC(INCND,NODGLOB,NUMNOD_L,LEN_AM)
      END IF
C--------------------------------------
C structure int21
C--------------------------------------
      IF(NINTSTAMP /= 0)THEN
        CALL INTSTAMP_WRESTR(INTSTAMP,LINTSTAMP)
        LEN_AM=LEN_AM+LINTSTAMP
      END IF
C--------------------------------------
C FRICTION INTERFACE (REALS)
C--------------------------------------
       IF(NINTERFRIC > 0) THEN
          CALL INTFRIC_WRESTR(INTBUF_FRIC_TAB, LINTFRIC)
          LEN_AM=LEN_AM+LINTFRIC
       ENDIF
C-----------------------------------------------
      CALL W_RNLOC(MS0,NODGLOB,NUMNOD_L,LEN_AM)
      IF(SIN > 0) CALL W_RNLOC(IN,NODGLOB,NUMNOD_L,LEN_AM)
C--------------------------------------
C Advanced Mass Scaling
C--------------------------------------
      IF(IDTMINS==1)THEN
        CALL W_RNLOC (ADMSMS,NODGLOB,NUMNOD_L,LEN_AM)
        CALL W_R3NLOC(RES_SMS,NODGLOB,NUMNOD_L,LEN_AM)
      ELSEIF(IDTMINS==2)THEN
        CALL W_RNLOC (DMELC  ,NODGLOB,NUMELC_L,LEN_AM)
        CALL W_RNLOC (DMELTG ,NODGLOB,NUMELTG_L,LEN_AM)
        CALL W_RNLOC (DMELS  ,NODGLOB,NUMELS_L,LEN_AM)
        CALL W_RNLOC (DMELTR ,NODGLOB,NUMELT_L,LEN_AM)
        CALL W_RNLOC (DMELP  ,NODGLOB,NUMELP_L,LEN_AM)
        CALL W_RNLOC (DMELRT ,NODGLOB,NUMELR_L,LEN_AM)
        CALL W_R3NLOC(RES_SMS,NODGLOB,NUMNOD_L,LEN_AM)
      END IF
      IF(IDTMINS==2.OR.IDTMINS_INT/=0) THEN
        CALL W_RNLOC(DIAG_SMS,NODGLOB,NUMNOD_L,LEN_AM)
      END IF
      IF(ISMS_SELEC /= 0) THEN
        CALL W_INLOC(NATIV_SMS,NODGLOB,NUMNOD_L,LEN_IA)
      ENDIF
C--------------------------------------
C TABLE (float)                                
C--------------------------------------
      IF(NTABLE /= 0)THEN
        CALL TABLE_WRESTR(TABLE, LTABLE)
        LEN_AM=LEN_AM+LTABLE
      END IF
#ifdef DNC
C--------------------------------------
C MDS Restart information
C--------------------------------------
      CALL MDS_RESTART()
#endif
C--------------------------------------
C ALE LINKS
C--------------------------------------
      IF(NALELK>0.AND.SLINALE>0)THEN
        CALL WRITE_I_C(LINALE,SLINALE)
      ENDIF
C-----------------------------------------------
C FXBODY
C-----------------------------------------------
      IF (NFXBODY>0) THEN
         CALL FXBWRESTP(
     .     FXBIPM_L,  FXBRPM,    FXBNOD_L,  FXBMOD_L, FXBGLM_L,
     .     FXBCPM_L,  FXBCPS_L,  FXBLM_L,   FXBFLS_L, FXBDLS_L,
     .     FXBDEP,    FXBVIT,    FXBACC,    FXBELM_L, FXBSIG_L,
     .     FXBGRVI_L, FXBGRVR_L, LENNOD_L,  LENMOD_L, LENGLM_L,
     .     LENCP_L,   LENLM_L,   LENFLS_L,  LENDLS_L, LENELM_L,
     .     LENSIG_L,  LENGRVI_L, LENGRVR_L, LEN_AM,   ITASK)
C
         DEALLOCATE(FXBNOD_L, FXBMOD_L, FXBGLM_L, FXBCPM_L, FXBCPS_L,
     .              FXBLM_L, FXBFLS_L, FXBDLS_L, FXBELM_L, FXBSIG_L,
     .              FXBGRVI_L, FXBGRVR_L)
      ENDIF
C-----------------------------------------------
C EIG
C-----------------------------------------------
      IF (NEIG>0)
     .   CALL EIGWRESTP(EIGIPM_L, EIGIBUF_L, EIGRPM, LEIBUF_L,LEN_AM)
C--------------------------------------------
C SEATBELTS - SLIPRING / RETRACTOR
C--------------------------------------------
      IF (NSLIPRING + NRETRACTOR > 0) THEN
        CALL W_SEATBELTS(LEN_IA,LEN_AM,P,NODLOCAL,N_ANCHOR_REMOTE_L,
     .       N_ANCHOR_REMOTE_SEND_L,ANCHOR_REMOTE_L,ANCHOR_REMOTE_SEND_L)
      ENDIF
C--------------------------------------------
C /H3D/?/TMAX part noda
C--------------------------------------------
      IF (LMAX_DIS>0)CALL W_R3NLOC(D,NODGLOB,NUMNOD_L,LEN_AM)
      IF (LMAX_VEL>0)CALL W_R3NLOC(V,NODGLOB,NUMNOD_L,LEN_AM)
      CALL W_OUTMAXN(NUMNOD_L,LEN_AM)
C-----------------------------------------------
C FLOW
C-----------------------------------------------
      IF (NFLOW>0) CALL NFWRESTP(IFLOW_L,RFLOW,LEN_IA,LEN_AM)
C-----------------------------------------------
C ALEBAG                                 
C-----------------------------------------------
      CALL FVWRESTP(P)
C-----------------------------------------------
C thermique des coques + solides
C-----------------------------------------------
        IF(ITHERM_FE > 0  ) THEN
          CALL W_RNLOC(MCP,NODGLOB,NUMNOD_L,LEN_AM)
          CALL W_RNLOC(TEMP,NODGLOB,NUMNOD_L,LEN_AM)
        ENDIF
C-----------------------------------------------
C Nitsche Method
C-----------------------------------------------
      IF(NITSCHE > 0   ) THEN
        ALLOCATE(FORNEQS(3*NUMNOD_L))
        FORNEQS(1:3*NUMNOD_L) = ZERO
        CALL WRITE_DB(FORNEQS,3*NUMNOD_L)
        LEN_AM = LEN_AM + 3*NUMNOD_L
        DEALLOCATE(FORNEQS)
      ENDIF
C ----------------------------------------------
C Units
C-----------------------------------------------
        CALL WRITE_UNITS(UNITAB)
C--------------------------------------------
C rigid body material
C--------------------------------------------
      IF(NRBYM > 0)THEN
C float
        CALL WRITE_DB(RBYM,NFRBYM*NRBYM)
        LEN_AM = LEN_AM + NFRBYM*NRBYM
c integer
        CALL W_RBYMK(IRBYM,LCRBYM,FRONT_RM,NODLOCAL,P-1,
     .                   NSLARBM_L,LEN_IA)
       CALL WRWEIGHT_RM(FRONT_RM,NRBYM,P)
      ENDIF
C--------------------------------------
C ICONV + fconv
C--------------------------------------
      IF (NUMCONV > 0 )THEN
C float
        CALL W_FCONV(FCONV,CEP,P-1,NCONV_L,LEN_AM)
C integer
        CALL W_ICONV(IBCV,CEP,P-1,NODLOCAL,NCONV_L,LEN_IA,
     .                CEL,IPARG,NUMLOCGROUP)

      ENDIF
C--------------------------------------
C IRADIA + fradia
C--------------------------------------
      IF (NUMRADIA > 0 )THEN
C float
        CALL W_FRADIA(FRADIA,CEP,P-1,NRADIA_L,LEN_AM)
C integer
        CALL W_IRADIA(IBCR,CEP,P-1,NODLOCAL,NRADIA_L,LEN_IA,
     .                CEL,IPARG,NUMLOCGROUP)
      ENDIF
C--------------------------------------
C Imposed heat flux
C--------------------------------------
      IF (NFXFLUX > 0 )THEN
C float
        CALL W_FTHFLUX(FBFFLUX,CEP,P-1,NFXFLUX_L,LEN_AM)
C integer
        CALL W_ITHFLUX(IBFFLUX,CEP,P-1,NODLOCAL,NFXFLUX_L,LEN_IA,CEL,IXS)
      ENDIF
C ----------------------------------------------
C fix temp
C-----------------------------------------------
       IF(NFXTEMP > 0) THEN
C Fixtemp float
         CALL W_FBFT(FBFT,NFXTEMP,IBFT,NFXTEMP_L,LEN_AM,P-1)
C fixtemp integer
         CALL W_IBFT(IBFT,NFXTEMP,NODLOCAL,LEN_IA,
     .          NFXTEMP_L,P-1)
       ENDIF
C
C  ply xfem
C
       IF(IPLYXFEM > 0 ) THEN
         OFF = NUMELS + NUMELQ
C plyxfem float + integer
         CALL W_PXFEM(IEPXFEM_L,INPXFEM_L ,INDEX_PXFEM,
     .                MS_PLY,ZI_PLY ,NUMELC_L, NUMNOD_L   ,
     .                NUMELCPXFE_L,NUMNODPXFE_L,LEN_AM,LEN_IA,MSZ2)
C plyxfem integer
         IF(IPLYBCS > 0) THEN
          CALL W_INLOC(ICODPLY,NODGLOB,NUMNOD_L,LEN_IA)
          CALL W_INLOC(ISKWPLY,NODGLOB,NUMNOD_L,LEN_IA)
         ENDIF
C PlyXfem Anim
         OFF = NUMELS + NUMELQ
         CALL W_ANIM_PLY(IXC,      NUMELC_L,   NODLOCAL,
     *                   NUMNOD_L, CEL(OFF+1), CEP(MIN(OFF+1,LEN_CEP)), P-1)
C plyxfem velocity, displacement
         CALL W_AVUPLY(NUMNODPXFE_L,LEN_AM)
       ENDIF
C--------------------------------------------
C LOAD/C
C--------------------------------------------
      IF(NLOADC>0)
     .  CALL WRITE_DB(CFIELD,LFACLOAD*NLOADC)
      LEN_AM = LEN_AM + LFACLOAD*NLOADC
C--------------------------------------------
C LOAD/PFLUID
C--------------------------------------------
      IF(NLOADP>0)
     .  CALL WRITE_DB(LOADP,LFACLOAD*NLOADP)
      LEN_AM = LEN_AM + LFACLOAD*NLOADP
C--------------------------------------------
C LOAD/PRESSURE interfaces : Gaps
C--------------------------------------------
      IF (NINTLOADP>0) THEN
         CALL WRITE_DB(DGAPLOADINT, S_LOADPINTER )
         LEN_AM = LEN_AM + S_LOADPINTER
      ENDIF
C--------------------------------------------
C  crack xfem
C--------------------------------------------
       IF(ICRACK3D > 0)THEN
C  integer
         CALL W_CRKXFEM(IELXFEMC_L,IELXFEMTG_L,INCRKXFEM_L,NUMELC_L,
     .                  NUMELTG_L,NUMNOD_L,LEN_IA,NUMNODCRKXFE_L,
     .                  NODLEVXF_L)
C Crk_xfem Anim
         CALL W_ANIM_CRK(
     .               IXC      ,IXTG       ,NUMELC_L,NUMELTG_L,NODLOCAL,
     .               NUMNOD_L ,INCRKXFEM_L,CEL     ,CEP_CRKXFEM ,P-1  ,
     .               IEDGECRK_L,IBORDEDGE_L,NUMEDGES_L,INDEX_CRKXFEM  ,
     .               INOD_CRKXFEM,LCNECRKXFEM_L,EDGEGLOBAL,CEP,CRKLVSET,
     .               NCRKPART, INDX_CRK,CRKSHELL,CRKSKY,CRKAVX,
     .               CRKEDGE ,XFEM_PHANTOM,NUMNODCRKXFE_L)
c
         ALLOCATE(ELCUTC_L(2*NUMELC_L))
         ALLOCATE(ELCUTTG_L(2*NUMELTG_L))
         ALLOCATE(NODENR_L(NUMNODCRKXFE_L))
         ALLOCATE(KXFENOD2ELC_L(NUMNODCRKXFE_L))
         ALLOCATE(ENRTAG_L(NUMNOD_L*IENRNOD))
         ELCUTC_L = 0
         ELCUTTG_L = 0
         NODENR_L = 0
         KXFENOD2ELC_L = 0
         ENRTAG_L = 0
C
         CALL C_CRKADD(
     .        ELCUTC   ,NODENR    ,KXFENOD2ELC  ,ENRTAG  ,
     .        ELCUTC_L ,NODENR_L  ,KXFENOD2ELC_L,ENRTAG_L,
     .        NUMELC_L ,NUMELTG_L ,NUMNOD_L     ,NODGLOB ,INOD_CRKXFEM ,
     .        P-1      ,CEP       ,ELCUTTG_L ,INCRKXFEM_L,INDEX_CRKXFEM)
C--------------------------------------
C     ADDITIONAL XFEM WORKING TABLES
C--------------------------------------
         CALL WRITE_I_C(ELCUTC_L,2*NUMELC_L)
         LEN_IA = LEN_IA + 2*NUMELC_L
         CALL WRITE_I_C(ELCUTTG_L,2*NUMELTG_L)
         LEN_IA = LEN_IA + 2*NUMELTG_L
         CALL WRITE_I_C(NODENR_L,NUMNODCRKXFE_L)
         LEN_IA = LEN_IA + NUMNODCRKXFE_L
         CALL WRITE_I_C(KXFENOD2ELC_L,NUMNODCRKXFE_L)
         LEN_IA = LEN_IA + NUMNODCRKXFE_L
         CALL WRITE_I_C(ENRTAG_L,NUMNOD_L*IENRNOD)
         LEN_IA = LEN_IA + NUMNOD_L*IENRNOD
C
         DEALLOCATE(ELCUTC_L,ELCUTTG_L,NODENR_L,KXFENOD2ELC_L,ENRTAG_L)
         DEALLOCATE(IEDGECRK_L,IBORDEDGE_L,NODEDGE_L,TAGEDGE,IEDGE_L)
         DEALLOCATE(EDGELOCAL,EDGEGLOBAL)
         DEALLOCATE(IEDGESH4_L,IEDGESH3_L)
         DEALLOCATE(INDEX_CRKXFEM,NODLEVXF_L)
       ELSE
         ALLOCATE(ELCUTC_L(0))
         ALLOCATE(ELCUTTG_L(0))
         ALLOCATE(NODENR_L(0))
         ALLOCATE(KXFENOD2ELC_L(0))
         ALLOCATE(ENRTAG_L(0))
       ENDIF
C--------------------------------------
C     ALEFVM WORKING ARRAY : MUST BE INIT. FOR CYCLE 0
C--------------------------------------
       IF(IALEFVM > 0)CALL WRITE_DB(FCELL ,6*NUMELS)
C--------------------------------------------
C  for foam+air
       IF(IALELAG > 0) THEN
          CALL W_ALELAG(DFLOW,VFLOW,WFLOW,NODGLOB,NUMNOD_L,LEN_AM)
       ENDIF
C--------------------------------------------
C TH Groupes
C--------------------------------------------
       IF(LENTHGR > 0) THEN
         CALL WRITE_DB(RTHBUF,LENTHGR)
         LEN_AM = LEN_AM + LENTHGR
       ENDIF
C--------------------------------------------
C KNOT (ISO-GEOM ELEMENTS)
C--------------------------------------------
        IF(SKNOT > 0) CALL WRITE_DB(KNOT,SKNOT)
        IF(SKNOTLOCPC > 0 .AND. SKNOTLOCEL > 0) THEN
          CALL W_KNOTLOC(KXIG3D,NUMELIG3D_L,ISUMNIG3D_L,IXIG3D,CEP,
     .                   PROC,NODLOCAL,LEN_IA,KNOTLOCPC,KNOTLOCEL,NUMNOD_L)
        ENDIF
C--------------------------------------------
C WIGE (ISO-GEOM ELEMENTS)
C--------------------------------------------
       IF(NUMELIG3D > 0) CALL W_RNLOC(WIGE,NODGLOB,NUMNOD,LEN_AM)
C--------------------------------------------
C STACK proprerties
C--------------------------------------------
       IF(IPART_STACK > 0 .OR. IPART_PCOMPP > 0 ) THEN
          LEN_IG  = (3*NPT_STACK + 2)*NS_STACK
          LEN_G   = (6*NPT_STACK + 1)*NS_STACK
          LEN_PM  =  20*NS_STACK
         CALL W_STACK(STACK%IGEO,STACK%GEO,LEN_IG,LEN_G,LEN_IA,LEN_AM,
     .                STACK%PM, LEN_PM)
       ENDIF
C--------------------------------------------
C DRAPE option
C--------------------------------------------
       IF (NDRAPE > 0) THEN
C    write drape structure
         OFF  = NUMELS + NUMELQ
         CALL W_DRAPE(DRAPE,CEP(MIN(OFF+1,LEN_CEP)),P-1,NUMELC,NUMELC_L,LEN_IA,LEN_AM)
         OFF = OFF + NUMELC + NUMELT + NUMELP + NUMELR
         CALL W_DRAPE(DRAPE(1+NUMELC),CEP(MIN(OFF+1,LEN_CEP)),P-1,
     .                 NUMELTG,NUMELTG_L,LEN_IA,LEN_AM)
       ENDIF
C--------------------------------------------
C PLY_INFO ( need for animation file )
C--------------------------------------------
      IF (NUMPLY>0)
     .  CALL W_PLY_INFO(PLY_INFO,NUMPLY)
C
! -------------------------------------
! write the current elapsed time
! -------------------------------------
        CALL ELAPSTIME_OMP(SECS)
        CALL WRITE_DPDB(SECS,1)
C--------------------------------------------
C Close Restart FILE
C--------------------------------------------
C FILE_SIZE_C gives the value of the file pointer
C             which is the file size before close
      CALL FILE_SIZE_C(FILE_SIZE)
      DDSTAT(25)= FILE_SIZE
      CALL CLOSE_C
#ifdef DEBUG_RST
      IF(FLUSH_RST_TO_TXT) CLOSE(777+ITASK)
#endif
      IF(NUMNOD_L>0) DEALLOCATE(NODGLOB)
C Deallocate
      DEALLOCATE(MONVOL_L)
C--------------------------------------------
C Array Deallocation
      DEALLOCATE(NODLOCAL)
      DEALLOCATE(IEPXFEM_L)
      DEALLOCATE(INPXFEM_L)
      DEALLOCATE(IELXFEMC_L)
      DEALLOCATE(IELXFEMTG_L)
      DEALLOCATE(INCRKXFEM_L)
      DEALLOCATE(NUMLOCGROUP)
      DEALLOCATE(NUMLOCCLUSTER)

C--------------------------------------------
C Stats Domain Decomp
C--------------------------------------------
C DDSTAT
C 1 : NUMNOD Local
C 2 : NELEM Local
C 3 : NUMELS_L
C 4 : NUMELQ_L
C 5 : NUMELC_L
C 6 : NUMELP_L
C 7 : NUMELT_L
C 8 : NUMELR_L
C 9 : NUMELUR_L
C 10: NUMELTG_L
C 11: NUMELX_L
C 12: NBDDPROC :  nb of boundary proc 
C 13: NBDDBOUN :  nb of boundary node 
C 14: NBDDNOD :   size of comm:nb de nodes
C 15: NBDDNRB :   size of comm: nb de nodes main de rby
C 16: NRBYKIN_L : number of local rigid body main
C 17: NUMSPH_L : number of particles SPH (local) 
C 18: MEMI : size memory local int MA
C 19: MEMR : size memory local float AM
C 20: NSNT_L : number of nodes secnds (interface de contact 7,10,11)
C 21: NMNT_L : number of nodes main (interface de contact 7,10,11)
C 22: NSNT2_L : number of nodes secnds (interface type2)
C 23: NMNT2_L : number of nodes main (interface type2)
C 24: RESTSIZE : restart size in KB
C 24: NSLARB_L : number of nodes secnd rigid body
      DDSTAT(1) = NUMNOD_L
      DDSTAT(2) = NUMEL_L
      DDSTAT(3) = NUMELS_L
      DDSTAT(4) = NUMELQ_L
      DDSTAT(5) = NUMELC_L
      DDSTAT(6) = NUMELP_L
      DDSTAT(7) = NUMELT_L
      DDSTAT(8) = NUMELR_L
      DDSTAT(9) = NUMELUR_L
      DDSTAT(10)= NUMELTG_L
      DDSTAT(11)= NUMELX_L
      DDSTAT(12)= NBDDPROC
      DDSTAT(13)= NBDDBOUN
      DDSTAT(14)= NBDDNOD
      DDSTAT(15)= NBDDNRB
      DDSTAT(16)= NRBYKIN_L
      DDSTAT(17)= NUMSPH_L
      DDSTAT(20)= NSNT_L
      DDSTAT(21)= NMNT_L
      DDSTAT(22)= NSNT2_L
      DDSTAT(23)= NMNT2_L
      DDSTAT(24)= NSLARB_L
C
C Estimating memory usage
C
      IF(IPARI0/=0)THEN
        NISKY0_L=NISKY0_L+4*NCONV_L+4*NRADIA_L+4*NFXFLUX_L
      END IF
C
      I_EVAL = NUMNOD_L+2*NUMNOD_L*MAX(IALE,IEULER,IALELAG)+NUMNOD_L*IRODDL
      R_EVAL = LENWA_L+NFUNCT
      IF(IPARI0/=0)THEN
        LSKYI = NINT(5*MAX(4*NUMNOD_L,IMAXIMP_L)*PROBINT)+4*NUMSPH_L+NSKYI18_L
        R_EVAL = R_EVAL+8*NUMNOD_L+NPSAV*NPART
        R_EVAL = R_EVAL+8*LCNE_L
        IF(N2D==0.AND.IALE+IEULER+ITHERM+IALELAG/=0) R_EVAL = R_EVAL+ LCNE_L
C NFSKYI = 5
        R_EVAL = R_EVAL+5*LSKYI
        I_EVAL = I_EVAL+LSKYI+NUMNOD_L+1+LCNI2_L
     +         + NISKY0_L+NSKYRW_L+NSKYRBK_L+NISKYI2_L
     +         + NNMV_L+NNMVC_L+NSKYLL_L+NSKYRBM_L+NSKYRBE3_L
     +         + NNMV_L+NNMVC_L+NSKYLL_L+NSKYRBM_L
        IF(IVECTOR==1)I_EVAL = I_EVAL+ NUMNOD_L
        IF(I2NSNT>0)I_EVAL = I_EVAL+ NUMNOD_L+1
        IF(I2NSNT>0.AND.IVECTOR==1)I_EVAL = I_EVAL+ NUMNOD_L
      ELSE
        R_EVAL = R_EVAL+7*NUMNOD_L+NUMNOD_L*IRODDL+NPSAV*NPART
        R_EVAL = R_EVAL+2*MAX(IALE,ITHERM,IEULER)*(NUMELS_L+NUMELQ_L)
      END IF
C      IF(KDTINT/=0)
      R_EVAL = R_EVAL+NUMNOD_L
      R_EVAL = R_EVAL+
     +         LWASPH_L+7*NUMNOD_L*NSECT*ISECUT+5*NINTER+21*NIBVEL
      I_EVAL = I_EVAL+NUMNOD_L
      DDSTAT(18)= LEN_IA + I_EVAL
      DDSTAT(19)= LEN_AM + R_EVAL

C Size of Restart file:
C        LEN_AM has a size of 2*4 bytes
C        LEN_IA has a size of 4 bytes
C        The value is expressed in kb DDSTAT = (LEN_AM*2+LEN_IA)*4/1024
c      DDSTAT(25)= (LEN_AM*2+LEN_IA)/256
C


      DEALLOCATE(MULTI_FVM_VEL_L)
      DEALLOCATE(IEBCS_LISTELEM_L)
      DEALLOCATE(IEBCS_LISTFAC_L)


      RETURN
      END
