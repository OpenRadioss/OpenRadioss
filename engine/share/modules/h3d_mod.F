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
Chd|  H3D_MOD                       share/modules/h3d_mod.F       
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|        ANI_PCONT                     source/output/anim/generate/ani_pcont.F
Chd|        ANI_PCONT21                   source/output/anim/generate/ani_pcont.F
Chd|        ARRET                         source/system/arret.F         
Chd|        ASS18                         source/interfaces/int18/ass18.F
Chd|        CLUSTERF                      source/output/cluster/clusterf.F
Chd|        CREATE_H3D_1D_SCALAR          source/output/h3d/h3d_build_fortran/create_h3d_1d_scalar.F
Chd|        CREATE_H3D_1D_TENSOR          source/output/h3d/h3d_build_fortran/create_h3d_1d_tensor.F
Chd|        CREATE_H3D_1D_TORSOR          source/output/h3d/h3d_build_fortran/create_h3d_1d_torsor.F
Chd|        CREATE_H3D_1D_VECTOR          source/output/h3d/h3d_build_fortran/create_h3d_1d_vector.F
Chd|        CREATE_H3D_ARG_KEYWORD        source/output/h3d/h3d_build_fortran/create_h3d_arg_keyword.F
Chd|        CREATE_H3D_INPUT              source/output/h3d/h3d_build_fortran/create_h3d_input.F
Chd|        CREATE_H3D_NODAL_SCALAR       source/output/h3d/h3d_build_fortran/create_h3d_nodal_scalar.F
Chd|        CREATE_H3D_NODAL_TENSOR       source/output/h3d/h3d_build_fortran/create_h3d_nodal_tensor.F
Chd|        CREATE_H3D_NODAL_VECTOR       source/output/h3d/h3d_build_fortran/create_h3d_nodal_vector.F
Chd|        CREATE_H3D_PARTS              source/output/h3d/h3d_build_fortran/create_h3d_parts.F
Chd|        CREATE_H3D_PARTS_ALL          source/output/h3d/h3d_build_fortran/create_h3d_parts_all.F
Chd|        CREATE_H3D_QUAD_SCALAR        source/output/h3d/h3d_build_fortran/create_h3d_quad_scalar.F
Chd|        CREATE_H3D_QUAD_TENSOR        source/output/h3d/h3d_build_fortran/create_h3d_quad_tensor.F
Chd|        CREATE_H3D_QUAD_VECTOR        source/output/h3d/h3d_build_fortran/create_h3d_quad_vector.F
Chd|        CREATE_H3D_SHELL_SCALAR       source/output/h3d/h3d_build_fortran/create_h3d_shell_scalar.F
Chd|        CREATE_H3D_SHELL_TENSOR       source/output/h3d/h3d_build_fortran/create_h3d_shell_tensor.F
Chd|        CREATE_H3D_SHELL_VECTOR       source/output/h3d/h3d_build_fortran/create_h3d_shell_vector.F
Chd|        CREATE_H3D_SKIN_SCALAR        source/output/h3d/h3d_build_fortran/create_h3d_skin_scalar.F
Chd|        CREATE_H3D_SKIN_TENSOR        source/output/h3d/h3d_build_fortran/create_h3d_skin_tensor.F
Chd|        CREATE_H3D_SKIN_VECTOR        source/output/h3d/h3d_build_fortran/create_h3d_skin_vector.F
Chd|        CREATE_H3D_SOLID_SCALAR       source/output/h3d/h3d_build_fortran/create_h3d_solid_scalar.F
Chd|        CREATE_H3D_SOLID_TENSOR       source/output/h3d/h3d_build_fortran/create_h3d_solid_tensor.F
Chd|        CREATE_H3D_SOLID_VECTOR       source/output/h3d/h3d_build_fortran/create_h3d_solid_vector.F
Chd|        CREATE_H3D_SPH_SCALAR         source/output/h3d/h3d_build_fortran/create_h3d_sph_scalar.F
Chd|        CREATE_H3D_SPH_TENSOR         source/output/h3d/h3d_build_fortran/create_h3d_sph_tensor.F
Chd|        DMI_RBE3                      source/constraints/general/rbe3/rbe3f.F
Chd|        DTNODA                        source/time_step/dtnoda.F     
Chd|        DTNODAMS                      source/time_step/dtnodams.F   
Chd|        DYNA_INA                      source/implicit/imp_dyna.F    
Chd|        DYNA_WEX                      source/implicit/imp_dyna.F    
Chd|        ECRIT                         source/output/ecrit.F         
Chd|        EIG                           stub/eig.F                    
Chd|        EIG1                          stub/eig1.F                   
Chd|        EIGCOND                       stub/eigcond.F                
Chd|        EIGP                          stub/eigp.F                   
Chd|        FORANI1                       source/output/anim/generate/forani1.F
Chd|        FORANI2                       source/output/anim/generate/forani2.F
Chd|        FORANI3                       source/output/anim/generate/forani3.F
Chd|        FORCEFINGEO                   source/loads/general/forcefingeo.F
Chd|        FORCEPINCH                    source/loads/general/forcepinch.F
Chd|        FORCE_IMP                     source/loads/general/force_imp.F
Chd|        FORINT                        source/elements/forint.F      
Chd|        FREFORM                       source/input/freform.F        
Chd|        FVBAG0                        source/airbag/fvbag0.F        
Chd|        FVBAG1                        source/airbag/fvbag1.F        
Chd|        FVBAG2                        source/airbag/fvbag2.F        
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|        H3D_CREATE_DATATYPE           source/output/h3d/h3d_build_fortran/h3d_create_datatype.F
Chd|        H3D_GENE_KEYWORD              source/output/h3d/input_list/h3d_gene_keyword.F
Chd|        H3D_INI                       source/output/h3d/h3d_build_fortran/h3d_ini.F
Chd|        H3D_LIST_1D_SCALAR            source/output/h3d/input_list/h3d_list_1d_scalar.F
Chd|        H3D_LIST_1D_TENSOR            source/output/h3d/input_list/h3d_list_1d_tensor.F
Chd|        H3D_LIST_1D_TORSOR            source/output/h3d/input_list/h3d_list_1d_torsor.F
Chd|        H3D_LIST_1D_VECTOR            source/output/h3d/input_list/h3d_list_1d_vector.F
Chd|        H3D_LIST_NODAL_SCALAR         source/output/h3d/input_list/h3d_list_noda_scalar.F
Chd|        H3D_LIST_NODAL_TENSOR         source/output/h3d/input_list/h3d_list_noda_tensor.F
Chd|        H3D_LIST_NODAL_VECTOR         source/output/h3d/input_list/h3d_list_noda_vector.F
Chd|        H3D_LIST_QUAD_SCALAR          source/output/h3d/input_list/h3d_list_quad_scalar.F
Chd|        H3D_LIST_QUAD_TENSOR          source/output/h3d/input_list/h3d_list_quad_tensor.F
Chd|        H3D_LIST_QUAD_VECTOR          source/output/h3d/input_list/h3d_list_quad_vector.F
Chd|        H3D_LIST_SHELL_SCALAR         source/output/h3d/input_list/h3d_list_shell_scalar.F
Chd|        H3D_LIST_SHELL_TENSOR         source/output/h3d/input_list/h3d_list_shell_tensor.F
Chd|        H3D_LIST_SHELL_VECTOR         source/output/h3d/input_list/h3d_list_shell_vector.F
Chd|        H3D_LIST_SOLID_SCALAR         source/output/h3d/input_list/h3d_list_solid_scalar.F
Chd|        H3D_LIST_SOLID_TENSOR         source/output/h3d/input_list/h3d_list_solid_tensor.F
Chd|        H3D_LIST_SOLID_VECTOR         source/output/h3d/input_list/h3d_list_solid_vector.F
Chd|        H3D_LIST_SPH_SCALAR           source/output/h3d/input_list/h3d_list_sph_scalar.F
Chd|        H3D_LIST_SPH_TENSOR           source/output/h3d/input_list/h3d_list_sph_tensor.F
Chd|        H3D_NODAL_SCALAR              source/output/h3d/h3d_results/h3d_nodal_scalar.F
Chd|        H3D_PRE_SKIN_SCALAR           source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|        H3D_READ                      source/output/h3d/h3d_build_fortran/h3d_read.F
Chd|        H3D_SKIN_SCALAR               source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|        H3D_SKIN_VECTOR               source/output/h3d/h3d_results/h3d_skin_vector.F
Chd|        H3D_SOLID_SCALAR              source/output/h3d/h3d_results/h3d_solid_scalar.F
Chd|        H3D_SOLID_SCALAR_1            source/output/h3d/h3d_results/h3d_solid_scalar_1.F
Chd|        H3D_SOLID_VECTOR              source/output/h3d/h3d_results/h3d_solid_vector.F
Chd|        I10FOR3                       source/interfaces/int10/i10for3.F
Chd|        I10MAINF                      source/interfaces/int10/i10mainf.F
Chd|        I10MAIN_TRI                   source/interfaces/intsort/i10main_tri.F
Chd|        I11ASS3                       source/interfaces/int11/i11ass3.F
Chd|        I11MAINF                      source/interfaces/int11/i11mainf.F
Chd|        I14CMP                        source/interfaces/int14/i14cmp.F
Chd|        I14FRT                        source/interfaces/int14/i14frt.F
Chd|        I15ASS                        source/interfaces/int15/i15ass.F
Chd|        I15CMP                        source/interfaces/int15/i15cmp.F
Chd|        I17FOR3                       source/interfaces/int17/i17for3.F
Chd|        I17LLL4_PENA                  source/interfaces/int17/i17for3.F
Chd|        I17LLL_PENA                   source/interfaces/int17/i17for3.F
Chd|        I17MAIN_PENA                  source/interfaces/int17/i17main_pena.F
Chd|        I18FOR3                       source/interfaces/int18/i18for3.F
Chd|        I18KINE_S                     source/interfaces/int18/i18main_kine.F
Chd|        I18MAIN_KINE_1                source/interfaces/int18/i18main_kine.F
Chd|        I18MAIN_KINE_2                source/interfaces/int18/i18main_kine.F
Chd|        I18MAIN_KINE_S                source/interfaces/int18/i18main_kine.F
Chd|        I20BUCE_CRIT                  source/interfaces/intsort/i20buce_crit.F
Chd|        I20FOR3                       source/interfaces/int20/i20for3.F
Chd|        I20FOR3E                      source/interfaces/int20/i20for3.F
Chd|        I20MAINF                      source/interfaces/int20/i20mainf.F
Chd|        I20MAIN_CRIT_TRI              source/interfaces/intsort/i20main_crit_tri.F
Chd|        I20MAIN_TRI                   source/interfaces/intsort/i20main_tri.F
Chd|        I21ASS3                       source/interfaces/int21/i21ass3.F
Chd|        I21MAINF                      source/interfaces/int21/i21mainf.F
Chd|        I22FOR3                       source/interfaces/int22/i22for3.F
Chd|        I22MAINF                      source/interfaces/int22/i22mainf.F
Chd|        I22MAIN_TRI                   source/interfaces/intsort/i22main_tri.F
Chd|        I23FOR3                       source/interfaces/int23/i23for3.F
Chd|        I23MAINF                      source/interfaces/int23/i23mainf.F
Chd|        I23MAIN_TRI                   source/interfaces/intsort/i23main_tri.F
Chd|        I24DST3E                      source/interfaces/int24/i24dst3e.F
Chd|        I24FOR3                       source/interfaces/int24/i24for3.F
Chd|        I24MAINF                      source/interfaces/int24/i24main.F
Chd|        I24MAIN_TRI                   source/interfaces/intsort/i24main_tri.F
Chd|        I25FOR3                       source/interfaces/int25/i25for3.F
Chd|        I25FOR3E                      source/interfaces/int25/i25for3e.F
Chd|        I25FOR3_E2S                   source/interfaces/int25/i25for3_e2s.F
Chd|        I25MAINF                      source/interfaces/int25/i25mainf.F
Chd|        I25MAIN_SLID                  source/interfaces/int25/i25main_slid.F
Chd|        I25MAIN_TRI                   source/interfaces/intsort/i25main_tri.F
Chd|        I2CURVF                       source/interfaces/interf/i2curvf.F
Chd|        I2CURVFP                      source/interfaces/interf/i2curvfp.F
Chd|        I2FOMO3                       source/interfaces/interf/i2for3.F
Chd|        I2FOMO3P                      source/interfaces/interf/i2for3p.F
Chd|        I2FOR25                       source/interfaces/interf/i2for25.F
Chd|        I2FOR25P                      source/interfaces/interf/i2for25p.F
Chd|        I2FOR26                       source/interfaces/interf/i2for26.F
Chd|        I2FOR26P                      source/interfaces/interf/i2for26p.F
Chd|        I2FOR27                       source/interfaces/interf/i2for27.F
Chd|        I2FOR27P                      source/interfaces/interf/i2for27p.F
Chd|        I2FOR27P_CIN                  source/interfaces/interf/i2for27p_cin.F
Chd|        I2FOR27P_PEN                  source/interfaces/interf/i2for27p_pen.F
Chd|        I2FOR27_CIN                   source/interfaces/interf/i2for27_cin.F
Chd|        I2FOR27_PEN                   source/interfaces/interf/i2for27_pen.F
Chd|        I2FOR28                       source/interfaces/interf/i2for28.F
Chd|        I2FOR28P                      source/interfaces/interf/i2for28p.F
Chd|        I2FOR28P_CIN                  source/interfaces/interf/i2for28p_cin.F
Chd|        I2FOR28P_PEN                  source/interfaces/interf/i2for28p_pen.F
Chd|        I2FOR28_CIN                   source/interfaces/interf/i2for28_cin.F
Chd|        I2FOR28_PEN                   source/interfaces/interf/i2for28_pen.F
Chd|        I2FOR3                        source/interfaces/interf/i2for3.F
Chd|        I2FOR3N                       source/interfaces/interf/i2for3.F
Chd|        I2FOR3O                       source/interfaces/interf/i2for3.F
Chd|        I2FOR3P                       source/interfaces/interf/i2for3p.F
Chd|        I2FOR3PN                      source/interfaces/interf/i2for3p.F
Chd|        I2FOR3PO                      source/interfaces/interf/i2for3p.F
Chd|        I2FORCES                      source/interfaces/interf/i2forces.F
Chd|        I2FORCES_2D                   source/interfaces/interf/i2forces_2D.F
Chd|        I2MOM27P_CIN                  source/interfaces/interf/i2mom27p_cin.F
Chd|        I2MOM27_CIN                   source/interfaces/interf/i2mom27_cin.F
Chd|        I2MOM3N                       source/interfaces/interf/i2for3.F
Chd|        I2MOM3PN                      source/interfaces/interf/i2for3p.F
Chd|        I2RUPT                        source/interfaces/interf/int2rupt.F
Chd|        I3FOR2                        source/interfaces/inter2d/i3for2.F
Chd|        I3FOR3                        source/interfaces/inter3d/i3for3.F
Chd|        I3FRI3                        source/interfaces/inter3d/i3fri3.F
Chd|        I5FOR3                        source/interfaces/inter3d/i5for3.F
Chd|        I5FRI3                        source/interfaces/inter3d/i3fri3.F
Chd|        I6ASS3                        source/interfaces/inter3d/i6ass3.F
Chd|        I6MAIN                        source/interfaces/inter3d/i6main.F
Chd|        I7ASS33                       source/interfaces/int07/i7ass3.F
Chd|        I7ASSIGEO0                    source/interfaces/int07/i7ass3.F
Chd|        I7FOR3                        source/interfaces/int07/i7for3.F
Chd|        I7MAINF                       source/interfaces/int07/i7mainf.F
Chd|        I7MAIN_TRI                    source/interfaces/intsort/i7main_tri.F
Chd|        I8FOR3                        source/interfaces/inter3d/i8for3.F
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_FANIE                     source/implicit/imp_solv.F    
Chd|        IMP_FANII                     source/implicit/imp_solv.F    
Chd|        IMP_FOUT                      source/implicit/imp_solv.F    
Chd|        IMP_INTTD0                    source/implicit/imp_int_k.F   
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        IMP_TRIPI                     source/implicit/imp_int_k.F   
Chd|        INIRESA                       source/output/restart/rdresa.F
Chd|        INT2RUPT                      source/interfaces/interf/int2rupt.F
Chd|        INTAL1                        source/ale/inter/intal1.F     
Chd|        INTAL2                        source/ale/inter/intal2.F     
Chd|        INTER_DEALLOCATE_WAIT         source/interfaces/generic/inter_deallocate_wait.F
Chd|        INTER_SORT                    source/interfaces/generic/inter_sort.F
Chd|        INTER_SORT_07                 source/interfaces/int07/inter_sort_07.F
Chd|        INTFOP1                       source/interfaces/interf/intfop1.F
Chd|        INTFOP2                       source/interfaces/interf/intfop2.F
Chd|        INTFOP8                       source/interfaces/interf/intfop8.F
Chd|        INTTI1                        source/interfaces/interf/intti1.F
Chd|        INTTI2F                       source/interfaces/interf/intti2f.F
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|        INTVO2                        source/interfaces/inter2d/intvo2.F
Chd|        INTVO3                        source/interfaces/inter3d/intvo3.F
Chd|        INTVO8                        source/interfaces/inter3d/intvo8.F
Chd|        IQELA1                        source/ale/inter/iqela1.F     
Chd|        LAG_ANITH                     source/tools/lagmul/lag_anith.F
Chd|        LAG_ANITHP                    source/tools/lagmul/lag_anith.F
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|        LAG_MULTP                     source/tools/lagmul/lag_mult.F
Chd|        LECH3D                        source/output/h3d/h3d_build_fortran/lech3d.F
Chd|        LECINP                        source/input/lecinp.F         
Chd|        LECTUR                        source/input/lectur.F         
Chd|        LOAD_PRESSURE                 source/loads/general/load_pressure/load_pressure.F
Chd|        MANCTR                        source/input/manctr.F         
Chd|        MONVOL0                       source/airbag/monvol0.F       
Chd|        MULTI_ALLOCATE                source/multifluid/multi_allocate.F
Chd|        PBILAN                        source/elements/beam/pbilan.F 
Chd|        PBLAST                        source/loads/pblast/pblast.F  
Chd|        PBLAST_1                      source/loads/pblast/pblast_1.F
Chd|        PBLAST_2                      source/loads/pblast/pblast_2.F
Chd|        PBLAST_3                      source/loads/pblast/pblast_3.F
Chd|        PFLUID                        source/loads/general/pfluid/pfluid.F
Chd|        PFORC3                        source/elements/beam/pforc3.F 
Chd|        PRESSURE_CYL                  source/loads/general/load_pcyl/pressure_cyl.F
Chd|        R1TORS                        source/elements/spring/r1tors.F
Chd|        R23FORC3                      source/elements/spring/r23forc3.F
Chd|        R23LAW108                     source/elements/spring/r23law108.F
Chd|        R23LAW113                     source/elements/spring/r23law113.F
Chd|        R23LAW114                     source/elements/spring/r23law114.F
Chd|        R2TORS                        source/elements/spring/r2tors.F
Chd|        R3TORS                        source/elements/spring/r3tors.F
Chd|        R4TORS                        source/elements/spring/r4tors.F
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RANIM33                       source/elements/joint/ranim33.F
Chd|        RBE3T1                        source/constraints/general/rbe3/rbe3f.F
Chd|        RDCOMI                        source/output/restart/rdcomm.F
Chd|        RDRESA                        source/output/restart/rdresa.F
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|        REACTION_FORCES_CHECK_FOR_REQUESTED_OUTPUTsource/output/reactions.F     
Chd|        REALLOCATE_FI2                source/mpi/interfaces/spmd_i25slide.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_HEAD                    source/engine/resol_head.F    
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|        RESTALLOC                     source/output/restart/arralloc.F
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|        RGJOINT                       source/elements/joint/rgjoint.F
Chd|        RUPTINT2                      source/interfaces/interf/ruptint2.F
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|        SPMD_EXCH2_A_PON              source/mpi/forces/spmd_exch2_a_pon.F
Chd|        SPMD_EXCH_A                   source/mpi/forces/spmd_exch_a.F
Chd|        SPMD_EXCH_A_AMS_POFF          source/mpi/forces/spmd_exch_a_ams_poff.F
Chd|        SPMD_EXCH_A_INT2              source/mpi/forces/spmd_exch_a_int2.F
Chd|        SPMD_EXCH_A_INT2H             source/mpi/forces/spmd_exch_a_int2h.F
Chd|        SPMD_EXCH_A_INT2H_AMS         source/mpi/forces/spmd_exch_a_int2h_ams.F
Chd|        SPMD_EXCH_A_INT2_AMS          source/mpi/forces/spmd_exch_a_int2_ams.F
Chd|        SPMD_EXCH_A_INT2_PON          source/mpi/forces/spmd_exch_a_int2_pon.F
Chd|        SPMD_EXCH_EFRIC               source/mpi/interfaces/spmd_exch_efric.F
Chd|        SPMD_EXCH_SORTING_EFRIC       source/mpi/interfaces/spmd_exch_sorting_efric.F
Chd|        SPMD_FIADD11_POFF             source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD11_PON              source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD17_POFF             source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD17_PON              source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD20E_POFF            source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD20FE_PON            source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD20F_PON             source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD20_POFF             source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD25E_POFF            source/mpi/interfaces/spmd_fiadd25e_poff.F
Chd|        SPMD_FIADD25E_PON             source/mpi/interfaces/spmd_fiadd25e_pon.F
Chd|        SPMD_FIADD_POFF               source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_FIADD_PON                source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_I18KINE_PENE_COM_POFF    source/mpi/interfaces/spmd_i18kine_pene_com_poff.F
Chd|        SPMD_I25_SLIDE_GAT            source/mpi/interfaces/spmd_i25slide.F
Chd|        SPMD_I7FCOM_POFF              source/mpi/forces/spmd_i7fcom_poff.F
Chd|        SPMD_I7FCOM_PON               source/mpi/forces/spmd_i7fcom_pon.F
Chd|        SPMD_INITFI                   source/mpi/interfaces/spmd_i7tool.F
Chd|        SPMD_TRI10GAT                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI20GAT                 source/mpi/interfaces/spmd_i7crit.F
Chd|        SPMD_TRI24GAT                 source/mpi/interfaces/spmd_int.F
Chd|        SPMD_TRI25GAT                 source/mpi/interfaces/spmd_tri25gat.F
Chd|        SPMD_TRI7GAT                  source/mpi/interfaces/spmd_int.F
Chd|        STOP_SENSOR                   source/tools/sensor/stop_sensor.F
Chd|        TBILAN                        source/elements/truss/tbilan.F
Chd|        TFORC3                        source/elements/truss/tforc3.F
Chd|        TMAX_IPART                    source/output/tmax_ipart.F    
Chd|        VOLPRE                        source/airbag/volpres.F       
Chd|        VOLPREP                       source/airbag/volpresp.F      
Chd|        WRCOMI                        source/output/restart/wrcomm.F
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE H3D_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
c
      LOGICAL :: IS_H3D_USED = .FALSE. 
C-----------------------------------------------
C   H3D_KEYWORD : LIST OF AVAILABLE /H3D OPTIONS
C-----------------------------------------------
      TYPE H3D_KEYWORD
          CHARACTER(LEN=100) ::  KEY3 = ''
          CHARACTER(LEN=100) ::  KEY4 = ''
          CHARACTER(LEN=100) ::  KEY5 = ''
          CHARACTER(LEN=100) ::  KEY6 = ''
          CHARACTER(LEN=100) ::  KEY7 = ''
          CHARACTER(LEN=100) ::  KEY8 = ''
          INTEGER ::  ID = 0 
          CHARACTER(LEN=50) ::   TEXT1 = ''
          CHARACTER(LEN=50) ::   TEXT2 = ''
          CHARACTER(LEN=80) ::   COMMENT = ''
c
          INTEGER ::  IS_PLY = 0 
          INTEGER ::  IS_PLY_ALL = 0 
          INTEGER ::  IS_LAYER = 0 
          INTEGER ::  IS_LAYER_ALL = 0 
          INTEGER ::  IS_IPT = 0 
          INTEGER ::  IS_IPT_ALL = 0 
          INTEGER ::  IS_GAUSS = 0 
          INTEGER ::  IS_GAUSS_ALL = 0 
          INTEGER ::  IS_UVAR = 0 
          INTEGER ::  IS_UVAR_ALL = 0 
          INTEGER ::  IS_IR = 0 
          INTEGER ::  IS_IR_ALL = 0 
          INTEGER ::  IS_IS = 0 
          INTEGER ::  IS_IS_ALL = 0 
          INTEGER ::  IS_IT = 0 
          INTEGER ::  IS_IT_ALL = 0 
          INTEGER ::  IS_ID = 0 
          INTEGER ::  IS_ID_ALL = 0 
          INTEGER ::  IS_INTER = 0 
          INTEGER ::  IS_INTER_ALL = 0 
          INTEGER ::  IS_SKIN = 0 
          INTEGER ::  IS_CORNER_DATA = 0
          INTEGER ::  IS_MDSVAR = 0 
          INTEGER ::  IS_MDSVAR_ALL = 0 
          INTEGER ::  IS_MDSVAR_DEF = 0  
          INTEGER ::  IS_MODE = 0 
          INTEGER ::  IS_MODE_ALL = 0 
c
      END TYPE H3D_KEYWORD
C-----------------------------------------------
C   H3D_INPUT_LIST : LIST OF /H3D KEYWORDS READ IN 1.rad
C-----------------------------------------------
      TYPE H3D_INPUT
          CHARACTER(LEN=20) :: KEY2 = ''
          CHARACTER(LEN=20) ::  KEY3 = ''
          CHARACTER(LEN=20) ::  KEY4 = ''
          CHARACTER(LEN=20) ::  KEY5 = ''
          CHARACTER(LEN=20) ::  KEY6 = ''
          CHARACTER(LEN=20) ::  KEY7 = ''
          CHARACTER(LEN=20) ::  KEY8 = ''
          INTEGER ::  NB_PART=0
          INTEGER, DIMENSION(:), ALLOCATABLE :: PART_LIST
      END TYPE H3D_INPUT
C-----------------------------------------------
C   H3D_PARTS : LIST OF OUTPUTTED PARTS
C-----------------------------------------------
      TYPE H3D_PART_LIST
          INTEGER :: NB_PART=0
          INTEGER, DIMENSION(:), ALLOCATABLE :: PART_LIST
          INTEGER, DIMENSION(:), ALLOCATABLE :: NODES
          INTEGER, DIMENSION(:), ALLOCATABLE :: PART
      END TYPE H3D_PART_LIST
C-----------------------------------------------
C   H3D_OUTPUT_LIST : OUTPUT DATABASE
C-----------------------------------------------
      TYPE H3D_OUTPUT
          INTEGER ::  ID = 0
          INTEGER ::  OK = 0
          INTEGER ::  N_OUTP = 0
          CHARACTER(LEN=100) :: KEYWORD = ''
          INTEGER ::  ETYPE = 0
          INTEGER ::  OUTP_TYPE = 0
          INTEGER ::  IS_INFO1 = 0
          INTEGER ::  INFO1 = 0
          INTEGER ::  IS_INFO2 = 0
          INTEGER ::  INFO2 = 0
          INTEGER ::  PLY = 0
          INTEGER ::  LAYER = 0
          INTEGER ::  LAYER_UPPER = 0
          INTEGER ::  LAYER_LOWER = 0
          INTEGER ::  LAYER_MEMB = 0
          INTEGER ::  LAYER_BEND = 0
          INTEGER ::  IPT = 0
          INTEGER ::  IS_IPT_UPPER = 0
          INTEGER ::  IS_IPT_LOWER = 0
          INTEGER ::  IS_IPT_MEMB = 0
          INTEGER ::  IS_IPT_BEND = 0
          INTEGER ::  IUVAR = 0
          INTEGER ::  IMDSVAR = 0
          INTEGER ::  IDMDS = 0
          INTEGER ::  IDMATMDS = 0
          CHARACTER(LEN=64) ::  MDSVAR_NAME = ''
          INTEGER ::  SMDSVAR_NAME = 0
          INTEGER ::  GAUSS = 0
          INTEGER ::  IR = 0
          INTEGER ::  IS = 0
          INTEGER ::  IT = 0
          INTEGER ::  OBJECT_ID = 0
          INTEGER ::  MODE = 0
          INTEGER ::  IS_CORNER_DATA = 0
          CHARACTER(LEN=80) ::  STRING1 = ''
          INTEGER ::   S_STRING1 = 0
          CHARACTER(LEN=80) ::  STRING2 = ''
          INTEGER ::   S_STRING2 = 0
          CHARACTER(LEN=120) ::  COMMENT = ''
          INTEGER ::   S_COMMENT = 0
          INTEGER ::  N_H3D_PART_LIST = 0
          INTEGER ::  INTER = 0
          INTEGER, DIMENSION(:), ALLOCATABLE :: PART_LIST
          INTEGER, DIMENSION(:), ALLOCATABLE :: PART
      END TYPE H3D_OUTPUT
C-----------------------------------------------
C   H3D_DATABASE 
C-----------------------------------------------
      TYPE H3D_DATABASE

          
        INTEGER ::  N_TITLE = 0
        INTEGER ::  N_SENS_H3D = 0
        INTEGER ::  N_OUTP_H3D = 0
        INTEGER ::  IH3D = 0
        INTEGER ::  IH3D_RUN = 0
        INTEGER ::  N_INPUT_H3D = 0
        INTEGER ::  IPART_SELECT = 0
        INTEGER ::  MH3D = 0

        INTEGER ::  N_SCAL_DT = 0               !ANIM_N(1)
        INTEGER ::  N_SCAL_DMAS   = 0           !ANIM_N(2)
        INTEGER ::  N_SCAL_DINER = 0            !ANIM_N(12)
        INTEGER ::  N_SCAL_DAMA2 = 0            !ANIM_N(15&16)
        INTEGER ::  N_SCAL_SKID = 0               
        INTEGER ::  N_SCAL_STIFR = 0            !ANIM_N(18)
        INTEGER ::  N_SCAL_STIFN = 0            !ANIM_N(19)
        INTEGER ::  N_SCAL_CSE_FRIC = 0      
        INTEGER ::  N_SCAL_CSE_FRICINT = 0          

        INTEGER ::  N_VECT_CONT = 0             !ANIM_V(4)
        INTEGER ::  N_VECT_FINT = 0             !ANIM_V(5)
        INTEGER ::  N_VECT_FEXT = 0             !ANIM_V(6)
        INTEGER ::  N_VECT_PCONT = 0            !ANIM_V(12)
        INTEGER ::  N_VECT_CONT2 = 0            !ANIM_V(13)
        INTEGER ::  N_VECT_DROT = 0             !ANIM_V(14)
        INTEGER ::  N_VECT_DXANC  = 0           !ANIM_V(15)
        INTEGER ::  N_VECT_FREAC = 0            !ANIM_V(17)
        INTEGER ::  N_VECT_MREAC = 0            !ANIM_V(18)
        INTEGER ::  N_VECT_CLUST_FORCE = 0      !ANIM_V(19)
        INTEGER ::  N_VECT_CLUST_MOM = 0        !ANIM_V(20)
        INTEGER ::  N_VECT_CONT_MAX  = 0        !ANIM_V(26)
        INTEGER ::  N_VECT_PCONT2 = 0           !ANIM_V(27)
        INTEGER ::  N_VECT_PCONT_MAX = 0     
        INTEGER ::  N_VECT_CONT2_MAX = 0             
        INTEGER ::  N_VECT_PCONT2_MAX = 0     
        INTEGER ::  N_VECT_CONT2_MIN  = 0            
        INTEGER ::  N_VECT_PCONT2_MIN  = 0
        INTEGER ::  N_VECT_CONT2M  = 0
        INTEGER ::  N_VECT_ACC = 0  !FVM acceleration (law151, colocated scheme)

        INTEGER ::  SH_SCAL_ERR_THK = 0         !ANIM_CE(2156)

        INTEGER ::  SOL_SCAL_VORTX = 0          !ANIM_SE(10)
        INTEGER ::  SOL_SCAL_VORTY = 0          !ANIM_SE(4960)
        INTEGER ::  SOL_SCAL_VORTZ = 0          !ANIM_SE(4961)

        INTEGER ::  SH_IEPSDOT  = 0             !ANIM_C(...)
        INTEGER ::  UND_FORC = 0                !ANIM_FT(1)
        INTEGER ::  STRAIN   = 0                !ANIM STRAIN TENSOR

        INTEGER ::  IS_CORNER_DATA = 0
        INTEGER ::  RBODY_SINGLE = 0
        INTEGER ::  RBE2_SINGLE = 0
        INTEGER ::  RBE3_SINGLE = 0

        INTEGER ::  COMP_LEVEL = 0

        my_real  :: TH3D  = 0.
        my_real  :: DTH3D= 0.
        my_real  :: TH3D_STOP= 0.
        my_real  :: TH3D0= 0.
        my_real  :: DTH3D0= 0.
        my_real  :: TH3D_STOP0= 0.
        my_real  :: PERCENTAGE_ERROR= 0.

        INTEGER, DIMENSION(:), ALLOCATABLE :: N_SKID_INTER   
        INTEGER, DIMENSION(:), ALLOCATABLE :: ITITLE 
        INTEGER, DIMENSION(:), ALLOCATABLE :: LSENS_H3D      
        INTEGER, DIMENSION(:), ALLOCATABLE :: N_CSE_FRIC_INTER   

        CHARACTER(LEN=80), DIMENSION(:), ALLOCATABLE ::TITLE        

        TYPE (H3D_INPUT), DIMENSION(:), ALLOCATABLE :: INPUT_LIST

        TYPE (H3D_PART_LIST), DIMENSION(:), ALLOCATABLE :: PARTS

        TYPE (H3D_OUTPUT), DIMENSION(:), ALLOCATABLE :: OUTPUT_LIST

      END TYPE H3D_DATABASE
      
C-----------------------------------------------
      TYPE H3D_NUMBER_OF_KEY
        INTEGER :: NODAL_SCALAR =0
        INTEGER ::  NODAL_VECTOR =0
        INTEGER ::  NODAL_TENSOR =0
        INTEGER ::  ONED_SCALAR =0
        INTEGER ::  ONED_VECTOR =0
        INTEGER ::  ONED_TENSOR =0
        INTEGER ::  ONED_TORSOR =0
        INTEGER ::  SHELL_SCALAR =0
        INTEGER ::  SHELL_VECTOR =0
        INTEGER ::  SHELL_TENSOR =0
        INTEGER ::  SOLID_SCALAR =0
        INTEGER ::  SOLID_VECTOR =0
        INTEGER ::  SOLID_TENSOR =0
        INTEGER ::  SPH_SCALAR =0
        INTEGER ::  SPH_TENSOR =0
        INTEGER ::  QUAD_SCALAR =0
        INTEGER ::  QUAD_VECTOR =0
        INTEGER ::  QUAD_TENSOR =0
      END TYPE H3D_NUMBER_OF_KEY
C-----------------------------------------------
      
      END MODULE H3D_MOD
