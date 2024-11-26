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
      !||    table_mod                  ../engine/share/modules/table_mod.F
      !||--- called by ------------------------------------------------------
      !||    alemain                    ../engine/source/ale/alemain.F
      !||    bforc2                     ../engine/source/ale/bimat/bforc2.F
      !||    c3forc3                    ../engine/source/elements/sh3n/coque3n/c3forc3.F
      !||    c3forc3_crk                ../engine/source/elements/xfem/c3forc3_crk.F
      !||    cbaforc3                   ../engine/source/elements/shell/coqueba/cbaforc3.F
      !||    cdk6forc3                  ../engine/source/elements/sh3n/coquedk6/cdk6forc3.F
      !||    cdkforc3                   ../engine/source/elements/sh3n/coquedk/cdkforc3.F
      !||    cforc3                     ../engine/source/elements/shell/coque/cforc3.F
      !||    cforc3_crk                 ../engine/source/elements/xfem/cforc3_crk.F
      !||    cmain3                     ../engine/source/materials/mat_share/cmain3.F
      !||    cmain3pinch                ../engine/source/elements/shell/coqueba/cmain3pinch.F
      !||    czforc3                    ../engine/source/elements/shell/coquez/czforc3.F
      !||    czforc3_crk                ../engine/source/elements/xfem/czforc3_crk.F
      !||    fail_gene1_c               ../engine/source/materials/fail/gene1/fail_gene1_c.F
      !||    fail_gene1_s               ../engine/source/materials/fail/gene1/fail_gene1_s.F
      !||    fail_inievo_c              ../engine/source/materials/fail/inievo/fail_inievo_c.F
      !||    fail_inievo_s              ../engine/source/materials/fail/inievo/fail_inievo_s.F
      !||    fail_tab2_c                ../engine/source/materials/fail/tabulated/fail_tab2_c.F
      !||    fail_tab2_s                ../engine/source/materials/fail/tabulated/fail_tab2_s.F
      !||    fail_tab_c                 ../engine/source/materials/fail/tabulated/fail_tab_c.F
      !||    fail_tab_s                 ../engine/source/materials/fail/tabulated/fail_tab_s.F
      !||    fail_tab_xfem              ../engine/source/materials/fail/tabulated/fail_tab_xfem.F
      !||    forint                     ../engine/source/elements/forint.F
      !||    forintc                    ../engine/source/elements/forintc.F
      !||    forintp                    ../engine/source/elements/forintp.F
      !||    genh3d                     ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    get_u_numtable             ../engine/source/user_interface/utable.F
      !||    h3d_pre_skin_scalar        ../engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
      !||    h3d_skin_scalar            ../engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
      !||    h3d_skin_vector            ../engine/source/output/h3d/h3d_results/h3d_skin_vector.F
      !||    ig3duforc3                 ../engine/source/elements/ige3d/ig3duforc3.F
      !||    interface_table_mod        ../engine/share/modules/table_mod.F
      !||    law119_membrane            ../engine/source/materials/mat/mat119/law119_membrane.F
      !||    lecfun                     ../engine/source/tools/curve/lecfun.F
      !||    lectur                     ../engine/source/input/lectur.F
      !||    mat107_newton              ../engine/source/materials/mat/mat107/mat107_newton.F
      !||    mat107_nice                ../engine/source/materials/mat/mat107/mat107_nice.F
      !||    mat107c_newton             ../engine/source/materials/mat/mat107/mat107c_newton.F
      !||    mat107c_nice               ../engine/source/materials/mat/mat107/mat107c_nice.F
      !||    mat112_xia_newton          ../engine/source/materials/mat/mat112/mat112_xia_newton.F
      !||    mat112_xia_nice            ../engine/source/materials/mat/mat112/mat112_xia_nice.F
      !||    mat112c_xia_newton         ../engine/source/materials/mat/mat112/mat112c_xia_newton.F
      !||    mat112c_xia_nice           ../engine/source/materials/mat/mat112/mat112c_xia_nice.F
      !||    mmain                      ../engine/source/materials/mat_share/mmain.F90
      !||    mmain8                     ../engine/source/materials/mat_share/mmain8.F
      !||    mulaw                      ../engine/source/materials/mat_share/mulaw.F90
      !||    mulawglc                   ../engine/source/materials/mat_share/mulawglc.F
      !||    mulawglcpinch              ../engine/source/elements/shell/coqueba/mulawglcpinch.F
      !||    press_seg3                 ../engine/source/loads/general/load_pcyl/press_seg3.F
      !||    pressure_cyl               ../engine/source/loads/general/load_pcyl/pressure_cyl.F
      !||    q4forc2                    ../engine/source/elements/solid_2d/quad4/q4forc2.F
      !||    qforc2                     ../engine/source/elements/solid_2d/quad/qforc2.F
      !||    r23forc3                   ../engine/source/elements/spring/r23forc3.F
      !||    r23law108                  ../engine/source/elements/spring/r23law108.F
      !||    r23law113                  ../engine/source/elements/spring/r23law113.F
      !||    r23law114                  ../engine/source/elements/spring/r23law114.F
      !||    r3def3                     ../engine/source/elements/spring/r3def3.F
      !||    resol                      ../engine/source/engine/resol.F
      !||    retractor_table_inv        ../engine/source/tools/seatbelts/retractor_table_inv.F90
      !||    rforc3                     ../engine/source/elements/spring/rforc3.F
      !||    s10forc3                   ../engine/source/elements/solid/solide10/s10forc3.F
      !||    s16forc3                   ../engine/source/elements/thickshell/solide16/s16forc3.F
      !||    s20forc3                   ../engine/source/elements/solid/solide20/s20forc3.F
      !||    s4forc3                    ../engine/source/elements/solid/solide4/s4forc3.F
      !||    s6cforc3                   ../engine/source/elements/thickshell/solide6c/s6cforc3.F
      !||    s8cforc3                   ../engine/source/elements/thickshell/solide8c/s8cforc3.F
      !||    s8eforc3                   ../engine/source/elements/solid/solide8e/s8eforc3.F
      !||    s8forc3                    ../engine/source/elements/solid/solide8/s8forc3.F
      !||    s8sforc3                   ../engine/source/elements/solid/solide8s/s8sforc3.F
      !||    s8zforc3                   ../engine/source/elements/solid/solide8z/s8zforc3.F
      !||    scforc3                    ../engine/source/elements/thickshell/solidec/scforc3.F
      !||    seatbelt_mod               ../common_source/modules/seatbelt_mod.F
      !||    sforc3                     ../engine/source/elements/solid/solide/sforc3.F
      !||    sigeps107                  ../engine/source/materials/mat/mat107/sigeps107.F
      !||    sigeps107c                 ../engine/source/materials/mat/mat107/sigeps107c.F
      !||    sigeps109                  ../engine/source/materials/mat/mat109/sigeps109.F
      !||    sigeps109c                 ../engine/source/materials/mat/mat109/sigeps109c.F
      !||    sigeps110c                 ../engine/source/materials/mat/mat110/sigeps110c.F
      !||    sigeps110c_lite_newton     ../engine/source/materials/mat/mat110/sigeps110c_lite_newton.F
      !||    sigeps110c_lite_nice       ../engine/source/materials/mat/mat110/sigeps110c_lite_nice.F
      !||    sigeps110c_newton          ../engine/source/materials/mat/mat110/sigeps110c_newton.F
      !||    sigeps110c_nice            ../engine/source/materials/mat/mat110/sigeps110c_nice.F
      !||    sigeps112                  ../engine/source/materials/mat/mat112/sigeps112.F
      !||    sigeps112c                 ../engine/source/materials/mat/mat112/sigeps112c.F
      !||    sigeps119c                 ../engine/source/materials/mat/mat119/sigeps119c.F
      !||    sigeps120                  ../engine/source/materials/mat/mat120/sigeps120.F
      !||    sigeps120_connect_main     ../engine/source/materials/mat/mat120/sigeps120_connect_main.F
      !||    sigeps120_connect_tab_dp   ../engine/source/materials/mat/mat120/sigeps120_connect_tab_dp.F
      !||    sigeps120_connect_tab_vm   ../engine/source/materials/mat/mat120/sigeps120_connect_tab_vm.F
      !||    sigeps120_tab_dp           ../engine/source/materials/mat/mat120/sigeps120_tab_dp.F
      !||    sigeps120_tab_vm           ../engine/source/materials/mat/mat120/sigeps120_tab_vm.F
      !||    sigeps52                   ../engine/source/materials/mat/mat052/sigeps52.F
      !||    sigeps52c                  ../engine/source/materials/mat/mat052/sigeps52c.F
      !||    sigeps58c                  ../engine/source/materials/mat/mat058/sigeps58c.F
      !||    sigeps73c                  ../engine/source/materials/mat/mat073/sigeps73c.F
      !||    sigeps74                   ../engine/source/materials/mat/mat074/sigeps74.F
      !||    sigeps80                   ../engine/source/materials/mat/mat080/sigeps80.F
      !||    sigeps80c                  ../engine/source/materials/mat/mat080/sigeps80c.F
      !||    sortie_main                ../engine/source/output/sortie_main.F
      !||    spstres                    ../engine/source/elements/sph/spstres.F
      !||    suforc3                    ../engine/source/user_interface/suforc3.F
      !||    suser43                    ../engine/source/elements/solid/sconnect/suser43.F
      !||    szforc3                    ../engine/source/elements/solid/solidez/szforc3.F
      !||    table2d_vinterp_log        ../engine/source/tools/curve/table2d_vinterp_log.F
      !||    table4d_mod                ../common_source/modules/table4d_mod.F
      !||    table_glob_mod             ../engine/share/modules/table_glob_mod.F
      !||    table_interp               ../engine/source/tools/curve/table_tools.F
      !||    table_interp_dydx          ../engine/source/tools/curve/table_tools.F
      !||    table_interp_law76         ../engine/source/tools/curve/table_tools.F
      !||    table_rresti               ../engine/source/tools/curve/table_tools.F
      !||    table_rrestr               ../engine/source/tools/curve/table_tools.F
      !||    table_vinterp              ../engine/source/tools/curve/table_tools.F
      !||    table_wresti               ../engine/source/tools/curve/table_tools.F
      !||    table_wrestr               ../engine/source/tools/curve/table_tools.F
      !||    table_zero                 ../engine/source/tools/curve/table_tools.F
      !||    usermat_shell              ../engine/source/materials/mat_share/usermat_shell.F
      !||    usermat_solid              ../engine/source/materials/mat_share/usermat_solid.F
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
      TYPE TTABLE
C---------
          INTEGER NOTABLE, NDIM
C---------
          TYPE(TTABLE_XY), DIMENSION(:), POINTER :: X
          TYPE(TTABLE_XY), POINTER :: Y
      END TYPE TTABLE
C-----------------------------------------------
      END MODULE TABLE_MOD
      !||====================================================================
      !||    interface_table_mod        ../engine/share/modules/table_mod.F
      !||--- called by ------------------------------------------------------
      !||    fail_gene1_c               ../engine/source/materials/fail/gene1/fail_gene1_c.F
      !||    fail_gene1_s               ../engine/source/materials/fail/gene1/fail_gene1_s.F
      !||    fail_inievo_c              ../engine/source/materials/fail/inievo/fail_inievo_c.F
      !||    fail_inievo_s              ../engine/source/materials/fail/inievo/fail_inievo_s.F
      !||    fail_tab2_c                ../engine/source/materials/fail/tabulated/fail_tab2_c.F
      !||    fail_tab2_s                ../engine/source/materials/fail/tabulated/fail_tab2_s.F
      !||    fail_tab_c                 ../engine/source/materials/fail/tabulated/fail_tab_c.F
      !||    fail_tab_s                 ../engine/source/materials/fail/tabulated/fail_tab_s.F
      !||    fail_tab_xfem              ../engine/source/materials/fail/tabulated/fail_tab_xfem.F
      !||    get_table_value            ../engine/source/user_interface/utable.F
      !||    get_table_value_dydx       ../engine/source/user_interface/utable.F
      !||    get_u_table                ../engine/source/user_interface/utable.F
      !||    get_u_vtable               ../engine/source/user_interface/utable.F
      !||    get_vtable_value           ../engine/source/user_interface/utable.F
      !||    law119_membrane            ../engine/source/materials/mat/mat119/law119_membrane.F
      !||    mat107_newton              ../engine/source/materials/mat/mat107/mat107_newton.F
      !||    mat107_nice                ../engine/source/materials/mat/mat107/mat107_nice.F
      !||    mat107c_newton             ../engine/source/materials/mat/mat107/mat107c_newton.F
      !||    mat107c_nice               ../engine/source/materials/mat/mat107/mat107c_nice.F
      !||    mat112_xia_newton          ../engine/source/materials/mat/mat112/mat112_xia_newton.F
      !||    mat112_xia_nice            ../engine/source/materials/mat/mat112/mat112_xia_nice.F
      !||    mat112c_xia_newton         ../engine/source/materials/mat/mat112/mat112c_xia_newton.F
      !||    mat112c_xia_nice           ../engine/source/materials/mat/mat112/mat112c_xia_nice.F
      !||    r3def3                     ../engine/source/elements/spring/r3def3.F
      !||    resol                      ../engine/source/engine/resol.F
      !||    sigeps107                  ../engine/source/materials/mat/mat107/sigeps107.F
      !||    sigeps107c                 ../engine/source/materials/mat/mat107/sigeps107c.F
      !||    sigeps109                  ../engine/source/materials/mat/mat109/sigeps109.F
      !||    sigeps109c                 ../engine/source/materials/mat/mat109/sigeps109c.F
      !||    sigeps110c                 ../engine/source/materials/mat/mat110/sigeps110c.F
      !||    sigeps110c_lite_newton     ../engine/source/materials/mat/mat110/sigeps110c_lite_newton.F
      !||    sigeps110c_lite_nice       ../engine/source/materials/mat/mat110/sigeps110c_lite_nice.F
      !||    sigeps110c_newton          ../engine/source/materials/mat/mat110/sigeps110c_newton.F
      !||    sigeps110c_nice            ../engine/source/materials/mat/mat110/sigeps110c_nice.F
      !||    sigeps112                  ../engine/source/materials/mat/mat112/sigeps112.F
      !||    sigeps112c                 ../engine/source/materials/mat/mat112/sigeps112c.F
      !||    sigeps119c                 ../engine/source/materials/mat/mat119/sigeps119c.F
      !||    sigeps120                  ../engine/source/materials/mat/mat120/sigeps120.F
      !||    sigeps120_connect_main     ../engine/source/materials/mat/mat120/sigeps120_connect_main.F
      !||    sigeps120_connect_tab_dp   ../engine/source/materials/mat/mat120/sigeps120_connect_tab_dp.F
      !||    sigeps120_connect_tab_vm   ../engine/source/materials/mat/mat120/sigeps120_connect_tab_vm.F
      !||    sigeps120_tab_dp           ../engine/source/materials/mat/mat120/sigeps120_tab_dp.F
      !||    sigeps120_tab_vm           ../engine/source/materials/mat/mat120/sigeps120_tab_vm.F
      !||    sigeps52                   ../engine/source/materials/mat/mat052/sigeps52.F
      !||    sigeps52c                  ../engine/source/materials/mat/mat052/sigeps52c.F
      !||    sigeps58c                  ../engine/source/materials/mat/mat058/sigeps58c.F
      !||    sigeps73c                  ../engine/source/materials/mat/mat073/sigeps73c.F
      !||    sigeps74                   ../engine/source/materials/mat/mat074/sigeps74.F
      !||    sigeps80                   ../engine/source/materials/mat/mat080/sigeps80.F
      !||    sigeps80c                  ../engine/source/materials/mat/mat080/sigeps80c.F
      !||--- uses       -----------------------------------------------------
      !||    table_mod                  ../engine/share/modules/table_mod.F
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
        SUBROUTINE TABLE2D_VINTERP_LOG(TABLE,ISMOOTH,DIMX,NEL,IPOS,XX,YY,DYDX1,DYDX2)
        USE TABLE_MOD
        TYPE(TTABLE) TABLE
        INTEGER ,INTENT(IN)                  :: ISMOOTH
        INTEGER ,INTENT(IN)                  :: NEL
        INTEGER ,VALUE ,INTENT(IN)           :: DIMX
        INTEGER ,DIMENSION(DIMX,TABLE%NDIM)  :: IPOS
        my_real ,DIMENSION(DIMX,TABLE%NDIM)  :: XX
        my_real ,DIMENSION(NEL)              :: YY, DYDX1, DYDX2
        END SUBROUTINE TABLE2D_VINTERP_LOG
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


