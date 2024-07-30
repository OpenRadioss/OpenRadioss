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
      !||    table_mod                 ../starter/share/modules1/table_mod.F
      !||--- called by ------------------------------------------------------
      !||    calc_uniax                ../starter/source/materials/mat/mat098/lossfun_98.F
      !||    calc_uniax_2              ../starter/source/materials/mat/mat098/lossfun_98.F
      !||    ddsplit                   ../starter/source/restart/ddsplit/ddsplit.F
      !||    fail_tab2sys              ../starter/source/materials/tools/fail_tab2sys.F
      !||    fct_fiber                 ../starter/source/materials/mat/mat098/lossfun_98.F
      !||    fct_fiber_2               ../starter/source/materials/mat/mat098/lossfun_98.F
      !||    fsdcod                    ../starter/source/system/fsdcod.F
      !||    func_inters_c             ../starter/source/tools/curve/func_inters.F
      !||    func_inters_shear         ../starter/source/tools/curve/func_inters.F
      !||    func_maxy                 ../starter/source/tools/curve/func_maxy.F
      !||    func_slope                ../starter/source/tools/curve/func_slope.F
      !||    get_u_numtable            ../starter/source/user_interface/utable.F
      !||    hm_read_fail              ../starter/source/materials/fail/hm_read_fail.F
      !||    hm_read_fail_tab1         ../starter/source/materials/fail/tabulated/hm_read_fail_tab1.F
      !||    hm_read_funct             ../starter/source/tools/curve/hm_read_funct.F
      !||    hm_read_mat               ../starter/source/materials/mat/hm_read_mat.F
      !||    hm_read_mat190            ../starter/source/materials/mat/mat190/hm_read_mat190.F
      !||    hm_read_mat76             ../starter/source/materials/mat/mat076/hm_read_mat76.F
      !||    hm_read_mat80             ../starter/source/materials/mat/mat080/hm_read_mat80.F
      !||    hm_read_mat90             ../starter/source/materials/mat/mat090/hm_read_mat90.F
      !||    hm_read_move_funct        ../starter/source/tools/curve/hm_read_move_funct.F
      !||    hm_read_pcyl              ../starter/source/loads/general/load_pcyl/hm_read_pcyl.F
      !||    hm_read_table1            ../starter/source/tools/curve/hm_read_table.F
      !||    hm_read_table1_0          ../starter/source/tools/curve/hm_read_table1_0.F
      !||    hm_read_table1_1          ../starter/source/tools/curve/hm_read_table1_1.F
      !||    hm_read_table2            ../starter/source/tools/curve/hm_read_table.F
      !||    hm_read_table2_0          ../starter/source/tools/curve/hm_read_table2_0.F
      !||    hm_read_table2_1          ../starter/source/tools/curve/hm_read_table2_1.F
      !||    hm_read_visc              ../starter/source/materials/visc/hm_read_visc.F
      !||    hm_read_visc_prony        ../starter/source/materials/visc/hm_read_visc_prony.F
      !||    inter_dcod_function       ../starter/source/interfaces/reader/inter_dcod_function.F
      !||    interface_table_mod       ../starter/share/modules1/table_mod.F
      !||    law100_upd_ab             ../starter/source/materials/mat/mat100/law100_upd.F
      !||    law100_upd_nht            ../starter/source/materials/mat/mat100/law100_upd.F
      !||    law108_upd                ../starter/source/materials/mat/mat108/law108_upd.F
      !||    law111_upd                ../starter/source/materials/mat/mat111/law111_upd.F
      !||    law112_upd                ../starter/source/materials/mat/mat112/law112_upd.F
      !||    law113_upd                ../starter/source/materials/mat/mat113/law113_upd.F
      !||    law114_upd                ../starter/source/materials/mat/mat114/law114_upd.F
      !||    law119_upd                ../starter/source/materials/mat/mat119/law119_upd.F
      !||    law120_upd                ../starter/source/materials/mat/mat120/law120_upd.F
      !||    law158_upd                ../starter/source/materials/mat/mat158/law158_upd.F
      !||    law36_upd                 ../starter/source/materials/mat/mat036/law36_upd.F
      !||    law42_upd                 ../starter/source/materials/mat/mat042/law42_upd.F
      !||    law58_upd                 ../starter/source/materials/mat/mat058/law58_upd.F
      !||    law62_upd                 ../starter/source/materials/mat/mat062/law62_upd.F
      !||    law69_upd                 ../starter/source/materials/mat/mat069/law69_upd.F
      !||    law77_upd                 ../starter/source/materials/mat/mat077/law77_upd.F
      !||    law87_upd                 ../starter/source/materials/mat/mat087/law87_upd.F
      !||    law88_upd                 ../starter/source/materials/mat/mat088/law88_upd.F
      !||    law90_upd                 ../starter/source/materials/mat/mat090/law90_upd.F
      !||    law92_upd                 ../starter/source/materials/mat/mat092/law92_upd.F
      !||    lm_least_square_prony     ../starter/source/materials/visc/hm_read_visc_prony.F
      !||    lm_least_square_prony_2   ../starter/source/materials/visc/hm_read_visc_prony.F
      !||    lossfun_98                ../starter/source/materials/mat/mat098/lossfun_98.F
      !||    matfun_usr2sys            ../starter/source/materials/tools/matfun_usr2sys.F
      !||    mattab_usr2sys            ../starter/source/materials/tools/mattab_usr2sys.F
      !||    read_ebcs                 ../starter/source/boundary_conditions/ebcs/read_ebcs.F
      !||    read_material_models      ../starter/source/materials/read_material_models.F
      !||    split_ebcs                ../starter/source/boundary_conditions/ebcs/split_ebcs.F
      !||    st_qaprint_driver         ../starter/source/output/qaprint/st_qaprint_driver.F
      !||    st_qaprint_model_tools    ../starter/source/output/qaprint/st_qaprint_model_tools.F
      !||    table2d_intersect         ../starter/source/tools/curve/table2d_intersect.F
      !||    table_glob_mod            ../starter/share/modules1/table_glob_mod.F
      !||    table_interp              ../starter/source/tools/curve/table_tools.F
      !||    table_inters              ../starter/source/tools/curve/func_inters.F
      !||    table_vinterp             ../starter/source/tools/curve/table_tools.F
      !||    table_wresti              ../starter/source/tools/curve/table_tools.F
      !||    table_wrestr              ../starter/source/tools/curve/table_tools.F
      !||    table_zero                ../starter/source/tools/curve/table_tools.F
      !||    unify_x                   ../starter/source/tools/curve/func_slope.F
      !||    updfail                   ../starter/source/materials/updfail.F90
      !||    updmat                    ../starter/source/materials/updmat.F
      !||    ymax                      ../starter/source/materials/mat/mat100/law100_upd.F
      !||====================================================================
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
      !||====================================================================
      !||    interface_table_mod   ../starter/share/modules1/table_mod.F
      !||--- called by ------------------------------------------------------
      !||    get_u_table           ../starter/source/user_interface/utable.F
      !||    get_u_vtable          ../starter/source/user_interface/utable.F
      !||    hm_read_table2        ../starter/source/tools/curve/hm_read_table.F
      !||    hm_read_table2_0      ../starter/source/tools/curve/hm_read_table2_0.F
      !||    hm_read_table2_1      ../starter/source/tools/curve/hm_read_table2_1.F
      !||    law190_upd            ../starter/source/materials/mat/mat190/law190_upd.F
      !||--- uses       -----------------------------------------------------
      !||    table_mod             ../starter/share/modules1/table_mod.F
      !||====================================================================
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
        SUBROUTINE TABLE_VINTERP(TABLE,DIMX,NEL,IPOS,XX,YY,DYDX)
        USE TABLE_MOD
        TYPE(TTABLE) TABLE
        INTEGER ,INTENT(IN)                  :: NEL
        INTEGER ,VALUE ,INTENT(IN)           :: DIMX
        INTEGER ,DIMENSION(DIMX,TABLE%NDIM)  :: IPOS
        my_real ,DIMENSION(DIMX,TABLE%NDIM)  :: XX
        my_real ,DIMENSION(NEL)              :: YY, DYDX
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


