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
Chd|  TABLE_MOD                     share/modules/table_mod.F     
Chd|-- called by -----------
Chd|        MATPARAM_DEF_MOD              ../common_source/modules/matparam_def_mod.F
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|        ASSO_PLAS76                   source/materials/mat/mat076/asso_plas76.F
Chd|        ASSO_QPLAS76C                 source/materials/mat/mat076/asso_qplas76c.F
Chd|        BFORC2                        source/ale/bimat/bforc2.F     
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|        CDK6FORC3                     source/elements/sh3n/coquedk6/cdk6forc3.F
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|        CMAIN3                        source/materials/mat_share/cmain3.F
Chd|        CMAIN3PINCH                   source/elements/shell/coqueba/cmain3pinch.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|        FAIL_GENE1_C                  source/materials/fail/gene1/fail_gene1_c.F
Chd|        FAIL_GENE1_S                  source/materials/fail/gene1/fail_gene1_s.F
Chd|        FAIL_INIEVO_C                 source/materials/fail/inievo/fail_inievo_c.F
Chd|        FAIL_INIEVO_S                 source/materials/fail/inievo/fail_inievo_s.F
Chd|        FAIL_TAB2_C                   source/materials/fail/tabulated/fail_tab2_c.F
Chd|        FAIL_TAB2_S                   source/materials/fail/tabulated/fail_tab2_s.F
Chd|        FAIL_TAB_C                    source/materials/fail/tabulated/fail_tab_c.F
Chd|        FAIL_TAB_S                    source/materials/fail/tabulated/fail_tab_s.F
Chd|        FAIL_TAB_XFEM                 source/materials/fail/tabulated/fail_tab_xfem.F
Chd|        FORINT                        source/elements/forint.F      
Chd|        FORINTC                       source/elements/forintc.F     
Chd|        FORINTP                       source/elements/forintp.F     
Chd|        IG3DUFORC3                    source/elements/ige3d/ig3duforc3.F
Chd|        LAW119_MEMBRANE               source/materials/mat/mat119/law119_membrane.F
Chd|        LECFUN                        source/tools/curve/lecfun.F   
Chd|        LECTUR                        source/input/lectur.F         
Chd|        MAT107C_NEWTON                source/materials/mat/mat107/mat107c_newton.F
Chd|        MAT107C_NICE                  source/materials/mat/mat107/mat107c_nice.F
Chd|        MAT107_NEWTON                 source/materials/mat/mat107/mat107_newton.F
Chd|        MAT107_NICE                   source/materials/mat/mat107/mat107_nice.F
Chd|        MAT112C_XIA_NEWTON            source/materials/mat/mat112/mat112c_xia_newton.F
Chd|        MAT112C_XIA_NICE              source/materials/mat/mat112/mat112c_xia_nice.F
Chd|        MAT112_XIA_NEWTON             source/materials/mat/mat112/mat112_xia_newton.F
Chd|        MAT112_XIA_NICE               source/materials/mat/mat112/mat112_xia_nice.F
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|        MMAIN8                        source/materials/mat_share/mmain8.F
Chd|        MULAW                         source/materials/mat_share/mulaw.F
Chd|        MULAW8                        source/materials/mat_share/mulaw8.F
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        MULAWGLC                      source/materials/mat_share/mulawglc.F
Chd|        MULAWGLCPINCH                 source/elements/shell/coqueba/mulawglcpinch.F
Chd|        NO_ASSO_LPLAS76C              source/materials/mat/mat076/no_asso_lplas76c.F
Chd|        NO_ASSO_PLAS76                source/materials/mat/mat076/no_asso_plas76.F
Chd|        NO_ASSO_QPLAS76C              source/materials/mat/mat076/no_asso_qplas76c.F
Chd|        Q4FORC2                       source/elements/solid_2d/quad4/q4forc2.F
Chd|        QFORC2                        source/elements/solid_2d/quad/qforc2.F
Chd|        R23FORC3                      source/elements/spring/r23forc3.F
Chd|        R23LAW108                     source/elements/spring/r23law108.F
Chd|        R23LAW113                     source/elements/spring/r23law113.F
Chd|        R23LAW114                     source/elements/spring/r23law114.F
Chd|        R3DEF3                        source/elements/spring/r3def3.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S16FORC3                      source/elements/thickshell/solide16/s16forc3.F
Chd|        S20FORC3                      source/elements/solid/solide20/s20forc3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8FORC3                       source/elements/solid/solide8/s8forc3.F
Chd|        S8SFORC3                      source/elements/solid/solide8s/s8sforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SIGEPS107                     source/materials/mat/mat107/sigeps107.F
Chd|        SIGEPS107C                    source/materials/mat/mat107/sigeps107c.F
Chd|        SIGEPS109                     source/materials/mat/mat109/sigeps109.F
Chd|        SIGEPS109C                    source/materials/mat/mat109/sigeps109c.F
Chd|        SIGEPS110C                    source/materials/mat/mat110/sigeps110c.F
Chd|        SIGEPS110C_LITE_NEWTON        source/materials/mat/mat110/sigeps110c_lite_newton.F
Chd|        SIGEPS110C_LITE_NICE          source/materials/mat/mat110/sigeps110c_lite_nice.F
Chd|        SIGEPS110C_NEWTON             source/materials/mat/mat110/sigeps110c_newton.F
Chd|        SIGEPS110C_NICE               source/materials/mat/mat110/sigeps110c_nice.F
Chd|        SIGEPS112                     source/materials/mat/mat112/sigeps112.F
Chd|        SIGEPS112C                    source/materials/mat/mat112/sigeps112c.F
Chd|        SIGEPS119C                    source/materials/mat/mat119/sigeps119c.F
Chd|        SIGEPS120                     source/materials/mat/mat120/sigeps120.F
Chd|        SIGEPS120_CONNECT_MAIN        source/materials/mat/mat120/sigeps120_connect_main.F
Chd|        SIGEPS120_CONNECT_TAB_DP      source/materials/mat/mat120/sigeps120_connect_tab_dp.F
Chd|        SIGEPS120_CONNECT_TAB_VM      source/materials/mat/mat120/sigeps120_connect_tab_vm.F
Chd|        SIGEPS120_TAB_DP              source/materials/mat/mat120/sigeps120_tab_dp.F
Chd|        SIGEPS120_TAB_VM              source/materials/mat/mat120/sigeps120_tab_vm.F
Chd|        SIGEPS52                      source/materials/mat/mat052/sigeps52.F
Chd|        SIGEPS52C                     source/materials/mat/mat052/sigeps52c.F
Chd|        SIGEPS58C                     source/materials/mat/mat058/sigeps58c.F
Chd|        SIGEPS73C                     source/materials/mat/mat073/sigeps73c.F
Chd|        SIGEPS74                      source/materials/mat/mat074/sigeps74.F
Chd|        SIGEPS76                      source/materials/mat/mat076/sigeps76.F
Chd|        SIGEPS76C                     source/materials/mat/mat076/sigeps76c.F
Chd|        SIGEPS80                      source/materials/mat/mat080/sigeps80.F
Chd|        SIGEPS80C                     source/materials/mat/mat080/sigeps80c.F
Chd|        SPSTRES                       source/elements/sph/spstres.F 
Chd|        SUFORC3                       source/user_interface/suforc3.F
Chd|        SUSER43                       source/elements/solid/sconnect/suser43.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|        TABLE2D_VINTERP_LOG           source/tools/curve/table2d_vinterp_log.F
Chd|        TABLE_INTERP                  source/tools/curve/table_tools.F
Chd|        TABLE_INTERP_LAW76            source/tools/curve/table_tools.F
Chd|        TABLE_RRESTI                  source/tools/curve/table_tools.F
Chd|        TABLE_RRESTI_MAT              source/tools/curve/table_tools.F
Chd|        TABLE_RRESTR                  source/tools/curve/table_tools.F
Chd|        TABLE_RRESTR_MAT              source/tools/curve/table_tools.F
Chd|        TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|        TABLE_WRESTI                  source/tools/curve/table_tools.F
Chd|        TABLE_WRESTI_MAT              source/tools/curve/table_tools.F
Chd|        TABLE_WRESTR                  source/tools/curve/table_tools.F
Chd|        TABLE_WRESTR_MAT              source/tools/curve/table_tools.F
Chd|        TABLE_ZERO                    source/tools/curve/table_tools.F
Chd|        USERMAT_SHELL                 source/materials/mat_share/usermat_shell.F
Chd|        USERMAT_SOLID                 source/materials/mat_share/usermat_solid.F
Chd|        GET_U_NUMTABLE                source/user_interface/utable.F
Chd|        TABLE_GLOB_MOD                share/modules/table_glob_mod.F
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
      TYPE TTABLE
C---------
          INTEGER NOTABLE, NDIM
C---------
          TYPE(TTABLE_XY), DIMENSION(:), POINTER :: X
          TYPE(TTABLE_XY), POINTER :: Y
      END TYPE TTABLE
C-----------------------------------------------
      END MODULE TABLE_MOD
Chd|====================================================================
Chd|  INTERFACE_TABLE_MOD           share/modules/table_mod.F     
Chd|-- called by -----------
Chd|        ASSO_PLAS76                   source/materials/mat/mat076/asso_plas76.F
Chd|        ASSO_QPLAS76C                 source/materials/mat/mat076/asso_qplas76c.F
Chd|        FAIL_GENE1_C                  source/materials/fail/gene1/fail_gene1_c.F
Chd|        FAIL_GENE1_S                  source/materials/fail/gene1/fail_gene1_s.F
Chd|        FAIL_INIEVO_C                 source/materials/fail/inievo/fail_inievo_c.F
Chd|        FAIL_INIEVO_S                 source/materials/fail/inievo/fail_inievo_s.F
Chd|        FAIL_TAB2_C                   source/materials/fail/tabulated/fail_tab2_c.F
Chd|        FAIL_TAB2_S                   source/materials/fail/tabulated/fail_tab2_s.F
Chd|        FAIL_TAB_C                    source/materials/fail/tabulated/fail_tab_c.F
Chd|        FAIL_TAB_S                    source/materials/fail/tabulated/fail_tab_s.F
Chd|        FAIL_TAB_XFEM                 source/materials/fail/tabulated/fail_tab_xfem.F
Chd|        GET_TABLE_VALUE               source/user_interface/utable.F
Chd|        GET_U_TABLE                   source/user_interface/utable.F
Chd|        GET_U_VTABLE                  source/user_interface/utable.F
Chd|        GET_VTABLE_VALUE              source/user_interface/utable.F
Chd|        LAW119_MEMBRANE               source/materials/mat/mat119/law119_membrane.F
Chd|        MAT107C_NEWTON                source/materials/mat/mat107/mat107c_newton.F
Chd|        MAT107C_NICE                  source/materials/mat/mat107/mat107c_nice.F
Chd|        MAT107_NEWTON                 source/materials/mat/mat107/mat107_newton.F
Chd|        MAT107_NICE                   source/materials/mat/mat107/mat107_nice.F
Chd|        MAT112C_XIA_NEWTON            source/materials/mat/mat112/mat112c_xia_newton.F
Chd|        MAT112C_XIA_NICE              source/materials/mat/mat112/mat112c_xia_nice.F
Chd|        MAT112_XIA_NEWTON             source/materials/mat/mat112/mat112_xia_newton.F
Chd|        MAT112_XIA_NICE               source/materials/mat/mat112/mat112_xia_nice.F
Chd|        NO_ASSO_LPLAS76C              source/materials/mat/mat076/no_asso_lplas76c.F
Chd|        NO_ASSO_PLAS76                source/materials/mat/mat076/no_asso_plas76.F
Chd|        NO_ASSO_QPLAS76C              source/materials/mat/mat076/no_asso_qplas76c.F
Chd|        R3DEF3                        source/elements/spring/r3def3.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        SIGEPS107                     source/materials/mat/mat107/sigeps107.F
Chd|        SIGEPS107C                    source/materials/mat/mat107/sigeps107c.F
Chd|        SIGEPS109                     source/materials/mat/mat109/sigeps109.F
Chd|        SIGEPS109C                    source/materials/mat/mat109/sigeps109c.F
Chd|        SIGEPS110C                    source/materials/mat/mat110/sigeps110c.F
Chd|        SIGEPS110C_LITE_NEWTON        source/materials/mat/mat110/sigeps110c_lite_newton.F
Chd|        SIGEPS110C_LITE_NICE          source/materials/mat/mat110/sigeps110c_lite_nice.F
Chd|        SIGEPS110C_NEWTON             source/materials/mat/mat110/sigeps110c_newton.F
Chd|        SIGEPS110C_NICE               source/materials/mat/mat110/sigeps110c_nice.F
Chd|        SIGEPS112                     source/materials/mat/mat112/sigeps112.F
Chd|        SIGEPS112C                    source/materials/mat/mat112/sigeps112c.F
Chd|        SIGEPS119C                    source/materials/mat/mat119/sigeps119c.F
Chd|        SIGEPS120                     source/materials/mat/mat120/sigeps120.F
Chd|        SIGEPS120_CONNECT_MAIN        source/materials/mat/mat120/sigeps120_connect_main.F
Chd|        SIGEPS120_CONNECT_TAB_DP      source/materials/mat/mat120/sigeps120_connect_tab_dp.F
Chd|        SIGEPS120_CONNECT_TAB_VM      source/materials/mat/mat120/sigeps120_connect_tab_vm.F
Chd|        SIGEPS120_TAB_DP              source/materials/mat/mat120/sigeps120_tab_dp.F
Chd|        SIGEPS120_TAB_VM              source/materials/mat/mat120/sigeps120_tab_vm.F
Chd|        SIGEPS52                      source/materials/mat/mat052/sigeps52.F
Chd|        SIGEPS52C                     source/materials/mat/mat052/sigeps52c.F
Chd|        SIGEPS58C                     source/materials/mat/mat058/sigeps58c.F
Chd|        SIGEPS73C                     source/materials/mat/mat073/sigeps73c.F
Chd|        SIGEPS74                      source/materials/mat/mat074/sigeps74.F
Chd|        SIGEPS76                      source/materials/mat/mat076/sigeps76.F
Chd|        SIGEPS76C                     source/materials/mat/mat076/sigeps76c.F
Chd|        SIGEPS80                      source/materials/mat/mat080/sigeps80.F
Chd|        SIGEPS80C                     source/materials/mat/mat080/sigeps80c.F
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
        INTEGER ,DIMENSION(:,:) :: IPOS
        my_real ,DIMENSION(:,:) :: XX
        my_real
     .         YY(*), DYDX(*)
        END SUBROUTINE TABLE_VINTERP
C
        SUBROUTINE TABLE2D_VINTERP_LOG(TABLE,ISMOOTH,NEL,IPOS,XX,YY,DYDX1,DYDX2)
        USE TABLE_MOD
        TYPE(TTABLE) TABLE
        INTEGER ISMOOTH,NEL
        INTEGER ,DIMENSION(:,:) :: IPOS
        my_real ,DIMENSION(:,:) :: XX
        my_real ,DIMENSION(*)   :: YY,DYDX1,DYDX2
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


