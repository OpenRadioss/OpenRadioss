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
Chd|  HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|-- called by -----------
Chd|        CONTRL                        source/starter/contrl.F       
Chd|        HM_CONVERT_2D_ELEMENTS_SEATBELTsource/devtools/hm_reader/hm_convert_2d_elements_seatbelt.F
Chd|        HM_CONVERT_FAIL_TAB           source/devtools/hm_reader/hm_convert_fail_tab.F
Chd|        HM_CONVERT_INTER_TYPE19       source/devtools/hm_reader/hm_convert_inter_type19.F
Chd|        HM_COUNT_2D_ELEMENT_SEATBELT  source/devtools/hm_reader/hm_count_2d_element_seatbelt.F
Chd|        HM_DEBUG_PRINT_OPTION         source/devtools/hm_reader/hm_debug_print_option.F
Chd|        HM_GET_MAX_ID                 source/devtools/hm_reader/hm_get_max_id.F
Chd|        HM_MESSAGES                   source/devtools/hm_reader/hm_messages.F
Chd|        HM_PREREAD_BEM                source/loads/bem/hm_read_bem.F
Chd|        HM_PREREAD_IMPACC             source/constraints/general/impvel/hm_preread_impacc.F
Chd|        HM_PREREAD_IMPDISP            source/constraints/general/impvel/hm_preread_impdisp.F
Chd|        HM_PREREAD_IMPVEL             source/constraints/general/impvel/hm_preread_impvel.F
Chd|        HM_PREREAD_IMPVEL0            source/constraints/general/impvel/hm_preread_impvel0.F
Chd|        HM_READ_ALE_GRID              source/general_controls/ale_grid/hm_read_ale_grid.F
Chd|        HM_READ_ALE_MUSCL             source/general_controls/ale_cfd/hm_read_ale_muscl.F
Chd|        HM_READ_ALE_SOLVER            source/general_controls/ale_cfd/hm_read_ale_solver.F
Chd|        HM_READ_ANALY                 source/general_controls/computation/hm_read_analy.F
Chd|        HM_READ_BEM                   source/loads/bem/hm_read_bem.F
Chd|        HM_READ_BOX                   source/model/box/hm_read_box.F
Chd|        HM_READ_CAA                   source/general_controls/computation/hm_read_caa.F
Chd|        HM_READ_DEFSHELL              source/general_controls/default_values/hm_read_defshell.F
Chd|        HM_READ_DEFSOLID              source/general_controls/default_values/hm_read_defsolid.F
Chd|        HM_READ_EOS                   source/materials/eos/hm_read_eos.F
Chd|        HM_READ_FAIL                  source/materials/fail/hm_read_fail.F
Chd|        HM_READ_FUNCT                 source/tools/curve/hm_read_funct.F
Chd|        HM_READ_GAUGE                 source/output/gauge/hm_read_gauge.F
Chd|        HM_READ_IMPACC                source/constraints/general/impvel/hm_read_impacc.F
Chd|        HM_READ_IMPLICIT              source/general_controls/computation/hm_read_implicit.F
Chd|        HM_READ_IMPVEL                source/constraints/general/impvel/hm_read_impvel.F
Chd|        HM_READ_INISTA                source/initial_conditions/inista/hm_read_inista.F
Chd|        HM_READ_INISTATE_D00          source/elements/initia/hm_read_inistate_d00.F
Chd|        HM_READ_INIVEL                source/initial_conditions/general/inivel/hm_read_inivel.F
Chd|        HM_READ_INTSUB                source/output/subinterface/hm_read_intsub.F
Chd|        HM_READ_IOFLAG                source/general_controls/inputoutput/hm_read_ioflag.F
Chd|        HM_READ_LAGMUL                source/tools/lagmul/hm_read_lagmul.F
Chd|        HM_READ_LEAK                  source/airbag/hm_read_leak.F  
Chd|        HM_READ_MOVE_FUNCT            source/tools/curve/hm_read_move_funct.F
Chd|        HM_READ_NONLOCAL              source/materials/nonlocal/hm_read_nonlocal.F
Chd|        HM_READ_PCYL                  source/loads/general/load_pcyl/hm_read_pcyl.F
Chd|        HM_READ_PERTURB               source/general_controls/computation/hm_read_perturb.F
Chd|        HM_READ_PERTURB_FAIL          source/general_controls/computation/hm_read_perturb_fail.F
Chd|        HM_READ_PRETHGROU             source/output/th/hm_read_prethgrou.F
Chd|        HM_READ_REFSTA                source/loads/reference_state/refsta/hm_read_refsta.F
Chd|        HM_READ_SENSORS               source/tools/sensor/hm_read_sensors.F
Chd|        HM_READ_SMS                   source/general_controls/computation/hm_read_sms.F
Chd|        HM_READ_SPHCEL                source/elements/reader/hm_read_sphcel.F
Chd|        HM_READ_SPHGLO                source/general_controls/computation/hm_read_sphglo.F
Chd|        HM_READ_SPMD                  source/general_controls/computation/hm_read_spmd.F
Chd|        HM_READ_SUBMODEL              source/model/assembling/hm_read_submodel.F
Chd|        HM_READ_TABLE1                source/tools/curve/hm_read_table.F
Chd|        HM_READ_TABLE2                source/tools/curve/hm_read_table.F
Chd|        HM_READ_THERM                 source/materials/therm/hm_read_therm.F
Chd|        HM_READ_THERM_STRESS          source/materials/therm/hm_read_therm_stress.F
Chd|        HM_READ_THGROU                source/output/th/hm_read_thgrou.F
Chd|        HM_READ_UNIT                  source/general_controls/computation/hm_read_unit.F
Chd|        HM_READ_UPWIND                source/general_controls/computation/hm_read_upwind.F
Chd|        HM_READ_VISC                  source/materials/visc/hm_read_visc.F
Chd|        HM_YCTRL                      source/elements/initia/hm_yctrl.F
Chd|        LECFILL                       source/elements/initia/lecfill.F
Chd|        LECSUBMOD                     source/model/submodel/lecsubmod.F
Chd|        LECTRANS                      source/model/transformation/lectrans.F
Chd|        LECTRANSSUB                   source/model/submodel/lectranssub.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        NBFUNCT                       source/tools/curve/nbfunc.F   
Chd|        NBSPH                         source/elements/sph/nbsph.F   
Chd|        R2R_PRELEC                    source/coupling/rad2rad/r2r_prelec.F
Chd|        READ_ALE_MAT                  source/materials/ale/read_ale_mat.F
Chd|        READ_DETONATORS               source/initial_conditions/detonation/read_detonators.F
Chd|        READ_ENGINE_DRIVER            source/general_controls/engine/read_engine_driver.F
Chd|        READ_EULER_MAT                source/materials/ale/read_euler_mat.F
Chd|        READ_RWALL                    source/constraints/general/rwall/read_rwall.F
Chd|-- calls ---------------
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_OPTION_COUNT(ENTITY_TYPE, HM_OPTION_NUMBER)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   COUNT NUMBER OF /ENTITY_TYPE KEYWORDS USING HM_READER
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME               DESCRIPTION                         
C
C     ENTITY_TYPE        KEYWORD YOU WANT TO FIND NUMBER OF OCCURRENCES 
C          'MATERIAL'
C          'PROPERTY'
C          'PART'
C          'TRANSFORM'
C          'SUBSET'
C          'SENSOR'
C          'INTER'
C          'RWALL'
C          'FAILURE'
C          'DAMP'
C          'GAUGE'
C          'RBODY'
C          'ACCEL'
C          'BCS'
C          'AIRBAG'
C          'FRICTION'
C          'UNIT'
C          ...........
C     HM_OPTION_NUMBER   NUMBER OF OCCURRENCES
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD       
      USE SUBMODEL_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      CHARACTER*(*),INTENT(IN)::ENTITY_TYPE
C OUTPUT ARGUMENTS
      INTEGER,INTENT(OUT)::HM_OPTION_NUMBER
C-----------------------------------------------
      CALL CPP_OPTION_COUNT(ENTITY_TYPE(1:LEN_TRIM(ENTITY_TYPE)),LEN_TRIM(ENTITY_TYPE),HM_OPTION_NUMBER) 
      RETURN
C
      END
