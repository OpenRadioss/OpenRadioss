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
Chd|  FAIL_PARAM_MOD                modules/mat_elem/fail_param_mod.F
Chd|-- called by -----------
Chd|        MATPARAM_DEF_MOD              common_source/modules/mat_elem/matparam_def_mod.F
Chd|        FAIL_FUN2SYS                  starter/source/materials/tools/fail_fun2sys.F
Chd|        FAIL_TAB2SYS                  starter/source/materials/tools/fail_tab2sys.F
Chd|        HM_READ_FAIL_ALTER            starter/source/materials/fail/windshield_alter/hm_read_fail_alter.F
Chd|        HM_READ_FAIL_BIQUAD           starter/source/materials/fail/biquad/hm_read_fail_biquad.F
Chd|        HM_READ_FAIL_CHANG            starter/source/materials/fail/changchang/hm_read_fail_chang.F
Chd|        HM_READ_FAIL_COCKCROFT        starter/source/materials/fail/cockroft_latham/hm_read_fail_cockcroft.F
Chd|        HM_READ_FAIL_CONNECT          starter/source/materials/fail/connect/hm_read_fail_connect.F
Chd|        HM_READ_FAIL_EMC              starter/source/materials/fail/emc/hm_read_fail_emc.F
Chd|        HM_READ_FAIL_ENERGY           starter/source/materials/fail/energy/hm_read_fail_energy.F
Chd|        HM_READ_FAIL_FABRIC           starter/source/materials/fail/fabric/hm_read_fail_fabric.F
Chd|        HM_READ_FAIL_FLD              starter/source/materials/fail/fld/hm_read_fail_fld.F
Chd|        HM_READ_FAIL_GENE1            starter/source/materials/fail/gene1/hm_read_fail_gene1.F
Chd|        HM_READ_FAIL_GURSON           starter/source/materials/fail/gurson/hm_read_fail_gurson.F
Chd|        HM_READ_FAIL_HASHIN           starter/source/materials/fail/hashin/hm_read_fail_hashin.F
Chd|        HM_READ_FAIL_HC_DSSE          starter/source/materials/fail/hc_dsse/hm_read_fail_hc_dsse.F
Chd|        HM_READ_FAIL_HOFFMAN          starter/source/materials/fail/hoffman/hm_read_fail_hoffman.F
Chd|        HM_READ_FAIL_INIEVO           starter/source/materials/fail/inievo/hm_read_fail_inievo.F
Chd|        HM_READ_FAIL_JOHNSON          starter/source/materials/fail/johnson_cook/hm_read_fail_johnson.F
Chd|        HM_READ_FAIL_LADEVEZE         starter/source/materials/fail/ladeveze/hm_read_fail_ladeveze.F
Chd|        HM_READ_FAIL_MAXSTRAIN        starter/source/materials/fail/max_strain/hm_read_fail_maxstrain.F
Chd|        HM_READ_FAIL_NXT              starter/source/materials/fail/nxt/hm_read_fail_nxt.F
Chd|        HM_READ_FAIL_ORTHBIQUAD       starter/source/materials/fail/orthbiquad/hm_read_fail_orthbiquad.F
Chd|        HM_READ_FAIL_ORTHENERG        starter/source/materials/fail/orthenerg/hm_read_fail_orthenerg.F
Chd|        HM_READ_FAIL_ORTHSTRAIN       starter/source/materials/fail/orthstrain/hm_read_fail_orthstrain.F
Chd|        HM_READ_FAIL_PUCK             starter/source/materials/fail/puck/hm_read_fail_puck.F
Chd|        HM_READ_FAIL_RTCL             starter/source/materials/fail/rtcl/hm_read_fail_rtcl.F
Chd|        HM_READ_FAIL_SAHRAEI          starter/source/materials/fail/sahraei/hm_read_fail_sahraei.F
Chd|        HM_READ_FAIL_SNCONNECT        starter/source/materials/fail/snconnect/hm_read_fail_snconnect.F
Chd|        HM_READ_FAIL_SPALLING         starter/source/materials/fail/spalling/hm_read_fail_spalling.F
Chd|        HM_READ_FAIL_SYAZWAN          starter/source/materials/fail/syazwan/hm_read_fail_syazwan.F
Chd|        HM_READ_FAIL_TAB1             starter/source/materials/fail/tabulated/hm_read_fail_tab1.F
Chd|        HM_READ_FAIL_TAB2             starter/source/materials/fail/tabulated/hm_read_fail_tab2.F
Chd|        HM_READ_FAIL_TAB_OLD          starter/source/materials/fail/tabulated/hm_read_fail_tab_old.F
Chd|        HM_READ_FAIL_TBUTCHER         starter/source/materials/fail/tuler_butcher/hm_read_fail_tbutcher.F
Chd|        HM_READ_FAIL_TENSSTRAIN       starter/source/materials/fail/tensstrain/hm_read_fail_tensstrain.F
Chd|        HM_READ_FAIL_TSAIHILL         starter/source/materials/fail/tsaihill/hm_read_fail_tsaihill.F
Chd|        HM_READ_FAIL_TSAIWU           starter/source/materials/fail/tsaiwu/hm_read_fail_tsaiwu.F
Chd|        HM_READ_FAIL_USER             starter/source/materials/fail/failuser/hm_read_fail_user.F
Chd|        HM_READ_FAIL_VISUAL           starter/source/materials/fail/visual/hm_read_fail_visual.F
Chd|        HM_READ_FAIL_WIERZBICKI       starter/source/materials/fail/wierzbicki/hm_read_fail_wierzbicki.F
Chd|        HM_READ_FAIL_WILKINS          starter/source/materials/fail/wilkins/hm_read_fail_wilkins.F
Chd|        HM_READ_MULLINS_OR            starter/source/materials/fail/mullins_or/hm_read_fail_mullins_or.F
Chd|        WRITE_FAILPARAM               starter/source/materials/fail/write_failparam.F
Chd|        DELM01LAW                     engine/source/properties/composite_options/stack/delm01law.F
Chd|        DELM02LAW                     engine/source/properties/composite_options/stack/delm02law.F
Chd|        DELM24LAW                     engine/source/properties/composite_options/stack/delm24law.F
Chd|        H3D_FLD_TSH                   engine/source/output/h3d/h3d_results/h3d_fld_tsh.F
Chd|        READ_FAILPARAM                engine/source/output/restart/read_failparam.F
Chd|        WRITE_FAILPARAM               engine/source/output/restart/write_failparam.F
Chd|-- calls ---------------
Chd|        NAMES_AND_TITLES_MOD          modules/names_and_titles_mod.F
Chd|        TABLE4D_MOD                   modules/table4d_mod.F         
Chd|====================================================================
      MODULE FAIL_PARAM_MOD
c
      USE TABLE4D_MOD
      USE NAMES_AND_TITLES_MOD
C----------------------------------------------------------------------- 
#include "my_real.inc"
c=======================================================================      
c define type FAIL_ for general failure model data storage
c=======================================================================      
      
      TYPE FAIL_PARAM_
        character(len=nchartitle) :: KEYWORD  ! failure model type name
        INTEGER     :: IRUPT                  ! failure model type (number)
        INTEGER     :: FAIL_ID
        INTEGER     :: NUPARAM
        INTEGER     :: NIPARAM
        INTEGER     :: NUVAR
        INTEGER     :: NFUNC
        INTEGER     :: NTABLE
        INTEGER     :: NMOD                   !number of rupture/damage modes
        INTEGER     :: FAIL_IP                
        my_real     :: PTHK
        
        character(len=nchartitle) ,DIMENSION(:) ,ALLOCATABLE :: MODE   ! size NMOD
        my_real ,DIMENSION(:) ,ALLOCATABLE :: UPARAM  ! NUPARAM
        INTEGER ,DIMENSION(:) ,ALLOCATABLE :: IPARAM  ! NIPARAM
        INTEGER ,DIMENSION(:) ,ALLOCATABLE :: IFUNC   ! function index
        INTEGER ,DIMENSION(:) ,ALLOCATABLE :: TABLE   ! function table index
      
      END TYPE FAIL_PARAM_   
c
c---------------
      END MODULE FAIL_PARAM_MOD
