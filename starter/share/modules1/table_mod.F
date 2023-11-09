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
Chd|  TABLE_MOD                     share/modules1/table_mod.F    
Chd|-- called by -----------
Chd|        TABLE4D_MOD                   ../common_source/modules/table4d_mod.F
Chd|        CALC_UNIAX                    source/materials/mat/mat098/lossfun_98.F
Chd|        CALC_UNIAX_2                  source/materials/mat/mat098/lossfun_98.F
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|        FCT_FIBER                     source/materials/mat/mat098/lossfun_98.F
Chd|        FCT_FIBER_2                   source/materials/mat/mat098/lossfun_98.F
Chd|        FSDCOD                        source/system/fsdcod.F        
Chd|        FUNC_INTERS_C                 source/tools/curve/func_inters.F
Chd|        FUNC_INTERS_SHEAR             source/tools/curve/func_inters.F
Chd|        FUNC_MAXY                     source/tools/curve/func_maxy.F
Chd|        FUNC_SLOPE                    source/tools/curve/func_slope.F
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|        HM_READ_FAIL_GENE1            source/materials/fail/gene1/hm_read_fail_gene1.F
Chd|        HM_READ_FAIL_INIEVO           source/materials/fail/inievo/hm_read_fail_inievo.F
Chd|        HM_READ_FAIL_TAB2             source/materials/fail/tabulated/hm_read_fail_tab2.F
Chd|        HM_READ_FUNCT                 source/tools/curve/hm_read_funct.F
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|        HM_READ_MAT190                source/materials/mat/mat190/hm_read_mat190.F
Chd|        HM_READ_MAT76                 source/materials/mat/mat076/hm_read_mat76.F
Chd|        HM_READ_MAT80                 source/materials/mat/mat080/hm_read_mat80.F
Chd|        HM_READ_MAT90                 source/materials/mat/mat090/hm_read_mat90.F
Chd|        HM_READ_MOVE_FUNCT            source/tools/curve/hm_read_move_funct.F
Chd|        HM_READ_PCYL                  source/loads/general/load_pcyl/hm_read_pcyl.F
Chd|        HM_READ_TABLE1                source/tools/curve/hm_read_table.F
Chd|        HM_READ_TABLE1_0              source/tools/curve/hm_read_table1_0.F
Chd|        HM_READ_TABLE1_1              source/tools/curve/hm_read_table1_1.F
Chd|        HM_READ_TABLE2                source/tools/curve/hm_read_table.F
Chd|        HM_READ_TABLE2_0              source/tools/curve/hm_read_table2_0.F
Chd|        HM_READ_TABLE2_1              source/tools/curve/hm_read_table2_1.F
Chd|        HM_READ_VISC                  source/materials/visc/hm_read_visc.F
Chd|        HM_READ_VISC_PRONY            source/materials/visc/hm_read_visc_prony.F
Chd|        INTER_DCOD_FUNCTION           source/interfaces/reader/inter_dcod_function.F
Chd|        LAW100_UPD_AB                 source/materials/mat/mat100/law100_upd.F
Chd|        LAW100_UPD_NHT                source/materials/mat/mat100/law100_upd.F
Chd|        LAW108_UPD                    source/materials/mat/mat108/law108_upd.F
Chd|        LAW111_UPD                    source/materials/mat/mat111/law111_upd.F
Chd|        LAW112_UPD                    source/materials/mat/mat112/law112_upd.F
Chd|        LAW113_UPD                    source/materials/mat/mat113/law113_upd.F
Chd|        LAW114_UPD                    source/materials/mat/mat114/law114_upd.F
Chd|        LAW119_UPD                    source/materials/mat/mat119/law119_upd.F
Chd|        LAW120_UPD                    source/materials/mat/mat120/law120_upd.F
Chd|        LAW158_UPD                    source/materials/mat/mat158/law158_upd.F
Chd|        LAW36_UPD                     source/materials/mat/mat036/law36_upd.F
Chd|        LAW42_UPD                     source/materials/mat/mat042/law42_upd.F
Chd|        LAW58_UPD                     source/materials/mat/mat058/law58_upd.F
Chd|        LAW62_UPD                     source/materials/mat/mat062/law62_upd.F
Chd|        LAW69_UPD                     source/materials/mat/mat069/law69_upd.F
Chd|        LAW77_UPD                     source/materials/mat/mat077/law77_upd.F
Chd|        LAW87_UPD                     source/materials/mat/mat087/law87_upd.F
Chd|        LAW88_UPD                     source/materials/mat/mat088/law88_upd.F
Chd|        LAW90_UPD                     source/materials/mat/mat090/law90_upd.F
Chd|        LAW92_UPD                     source/materials/mat/mat092/law92_upd.F
Chd|        LM_LEAST_SQUARE_PRONY         source/materials/visc/hm_read_visc_prony.F
Chd|        LM_LEAST_SQUARE_PRONY_2       source/materials/visc/hm_read_visc_prony.F
Chd|        LOSSFUN_98                    source/materials/mat/mat098/lossfun_98.F
Chd|        MATFUN_USR2SYS                source/materials/tools/matfun_usr2sys.F
Chd|        MATTAB_USR2SYS                source/materials/tools/mattab_usr2sys.F
Chd|        READ_EBCS                     source/boundary_conditions/ebcs/read_ebcs.F
Chd|        READ_MATERIAL_MODELS          source/materials/read_material_models.F
Chd|        SPLIT_EBCS                    source/boundary_conditions/ebcs/split_ebcs.F
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|        ST_QAPRINT_MODEL_TOOLS        source/output/qaprint/st_qaprint_model_tools.F
Chd|        TABLE2D_INTERSECT             source/tools/curve/table2d_intersect.F
Chd|        TABLE_INTERP                  source/tools/curve/table_tools.F
Chd|        TABLE_INTERS                  source/tools/curve/func_inters.F
Chd|        TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|        TABLE_WRESTI                  source/tools/curve/table_tools.F
Chd|        TABLE_WRESTR                  source/tools/curve/table_tools.F
Chd|        TABLE_ZERO                    source/tools/curve/table_tools.F
Chd|        UNIFY_X                       source/tools/curve/func_slope.F
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|        YMAX                          source/materials/mat/mat100/law100_upd.F
Chd|        GET_U_NUMTABLE                source/user_interface/utable.F
Chd|        TABLE_GLOB_MOD                share/modules1/table_glob_mod.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE TABLE_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
      TYPE TTABLE_XY
        my_real, 
     .          DIMENSION(:), POINTER :: VALUES
      END TYPE TTABLE_XY
C---------
      TYPE TTABLE
          INTEGER :: NOTABLE
          INTEGER :: NDIM
          TYPE(TTABLE_XY), DIMENSION(:), POINTER :: X
          TYPE(TTABLE_XY), POINTER :: Y
      END TYPE TTABLE
C-----------------------------------------------
      END MODULE TABLE_MOD
Chd|====================================================================
Chd|  INTERFACE_TABLE_MOD           share/modules1/table_mod.F    
Chd|-- called by -----------
Chd|        GET_U_TABLE                   source/user_interface/utable.F
Chd|        GET_U_VTABLE                  source/user_interface/utable.F
Chd|        HM_READ_TABLE2                source/tools/curve/hm_read_table.F
Chd|        HM_READ_TABLE2_0              source/tools/curve/hm_read_table2_0.F
Chd|        HM_READ_TABLE2_1              source/tools/curve/hm_read_table2_1.F
Chd|        LAW190_UPD                    source/materials/mat/mat190/law190_upd.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE INTERFACE_TABLE_MOD
C-----------------------------------------------
      INTERFACE
C
        SUBROUTINE TABLE_INTERP(TABLE,XX,YY)
        USE TABLE_MOD
        TYPE(TTABLE) TABLE
        my_real, 
     .         DIMENSION(:) :: XX
        my_real
     .         YY
        END SUBROUTINE TABLE_INTERP
C
        SUBROUTINE TABLE_VINTERP(TABLE,NEL,IPOS,XX,YY,DYDX)
        USE TABLE_MOD
        TYPE(TTABLE) TABLE
        INTEGER NEL
        INTEGER, DIMENSION(:,:) :: IPOS
        my_real, 
     .         DIMENSION(:,:) :: XX
        my_real
     .         YY(*), DYDX(*)
        END SUBROUTINE TABLE_VINTERP
C
        SUBROUTINE TABLE_INTERP_LAW76(TABLE,IPOS2,XX,R2,DYDX,YY)
        USE TABLE_MOD
        TYPE(TTABLE) TABLE
        my_real, 
     .       DIMENSION(:) :: XX
        my_real
     .       YY, R2,DYDX
        INTEGER IPOS2
        END SUBROUTINE TABLE_INTERP_LAW76
C
      END INTERFACE
      END MODULE INTERFACE_TABLE_MOD


