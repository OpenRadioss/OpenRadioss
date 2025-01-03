Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||    hm_option_count                   ../starter/source/devtools/hm_reader/hm_option_count.F
      !||--- called by ------------------------------------------------------
      !||    contrl                            ../starter/source/starter/contrl.F
      !||    hm_convert_2d_elements_seatbelt   ../starter/source/devtools/hm_reader/hm_convert_2d_elements_seatbelt.F
      !||    hm_convert_fail_tab               ../starter/source/devtools/hm_reader/hm_convert_fail_tab.F
      !||    hm_convert_inter_type19           ../starter/source/devtools/hm_reader/hm_convert_inter_type19.F
      !||    hm_count_2d_element_seatbelt      ../starter/source/devtools/hm_reader/hm_count_2d_element_seatbelt.F
      !||    hm_debug_print_option             ../starter/source/devtools/hm_reader/hm_debug_print_option.F
      !||    hm_get_max_id                     ../starter/source/devtools/hm_reader/hm_get_max_id.F
      !||    hm_messages                       ../starter/source/devtools/hm_reader/hm_messages.F
      !||    hm_preread_bem                    ../starter/source/loads/bem/hm_read_bem.F
      !||    hm_preread_impacc                 ../starter/source/constraints/general/impvel/hm_preread_impacc.F
      !||    hm_preread_impdisp                ../starter/source/constraints/general/impvel/hm_preread_impdisp.F
      !||    hm_preread_impvel                 ../starter/source/constraints/general/impvel/hm_preread_impvel.F
      !||    hm_preread_impvel0                ../starter/source/constraints/general/impvel/hm_preread_impvel0.F
      !||    hm_read_ale_grid                  ../starter/source/general_controls/ale_grid/hm_read_ale_grid.F
      !||    hm_read_ale_muscl                 ../starter/source/general_controls/ale_cfd/hm_read_ale_muscl.F
      !||    hm_read_ale_solver                ../starter/source/general_controls/ale_cfd/hm_read_ale_solver.F
      !||    hm_read_analy                     ../starter/source/general_controls/computation/hm_read_analy.F
      !||    hm_read_bem                       ../starter/source/loads/bem/hm_read_bem.F
      !||    hm_read_box                       ../starter/source/model/box/hm_read_box.F
      !||    hm_read_caa                       ../starter/source/general_controls/computation/hm_read_caa.F
      !||    hm_read_defshell                  ../starter/source/general_controls/default_values/hm_read_defshell.F
      !||    hm_read_defsolid                  ../starter/source/general_controls/default_values/hm_read_defsolid.F
      !||    hm_read_eos                       ../starter/source/materials/eos/hm_read_eos.F
      !||    hm_read_fail                      ../starter/source/materials/fail/hm_read_fail.F
      !||    hm_read_funct                     ../starter/source/tools/curve/hm_read_funct.F
      !||    hm_read_funct_python              ../starter/source/tools/curve/hm_read_funct_python.F90
      !||    hm_read_gauge                     ../starter/source/output/gauge/hm_read_gauge.F
      !||    hm_read_impacc                    ../starter/source/constraints/general/impvel/hm_read_impacc.F
      !||    hm_read_implicit                  ../starter/source/general_controls/computation/hm_read_implicit.F
      !||    hm_read_impvel                    ../starter/source/constraints/general/impvel/hm_read_impvel.F
      !||    hm_read_inista                    ../starter/source/initial_conditions/inista/hm_read_inista.F
      !||    hm_read_inistate_d00              ../starter/source/elements/initia/hm_read_inistate_d00.F
      !||    hm_read_inivel                    ../starter/source/initial_conditions/general/inivel/hm_read_inivel.F
      !||    hm_read_intsub                    ../starter/source/output/subinterface/hm_read_intsub.F
      !||    hm_read_ioflag                    ../starter/source/general_controls/inputoutput/hm_read_ioflag.F
      !||    hm_read_lagmul                    ../starter/source/tools/lagmul/hm_read_lagmul.F
      !||    hm_read_leak                      ../starter/source/airbag/hm_read_leak.F
      !||    hm_read_move_funct                ../starter/source/tools/curve/hm_read_move_funct.F
      !||    hm_read_nonlocal                  ../starter/source/materials/nonlocal/hm_read_nonlocal.F
      !||    hm_read_pcyl                      ../starter/source/loads/general/load_pcyl/hm_read_pcyl.F
      !||    hm_read_perturb                   ../starter/source/general_controls/computation/hm_read_perturb.F
      !||    hm_read_perturb_fail              ../starter/source/general_controls/computation/hm_read_perturb_fail.F
      !||    hm_read_prethgrou                 ../starter/source/output/th/hm_read_prethgrou.F
      !||    hm_read_refsta                    ../starter/source/loads/reference_state/refsta/hm_read_refsta.F
      !||    hm_read_sensors                   ../starter/source/tools/sensor/hm_read_sensors.F
      !||    hm_read_sms                       ../starter/source/general_controls/computation/hm_read_sms.F
      !||    hm_read_sphcel                    ../starter/source/elements/reader/hm_read_sphcel.F
      !||    hm_read_sphglo                    ../starter/source/general_controls/computation/hm_read_sphglo.F
      !||    hm_read_spmd                      ../starter/source/general_controls/computation/hm_read_spmd.F
      !||    hm_read_submodel                  ../starter/source/model/assembling/hm_read_submodel.F
      !||    hm_read_table1                    ../starter/source/tools/curve/hm_read_table.F
      !||    hm_read_table2                    ../starter/source/tools/curve/hm_read_table.F
      !||    hm_read_therm                     ../starter/source/materials/therm/hm_read_therm.F
      !||    hm_read_therm_stress              ../starter/source/materials/therm/hm_read_therm_stress.F90
      !||    hm_read_thgrou                    ../starter/source/output/th/hm_read_thgrou.F
      !||    hm_read_unit                      ../starter/source/general_controls/computation/hm_read_unit.F
      !||    hm_read_upwind                    ../starter/source/general_controls/computation/hm_read_upwind.F
      !||    hm_read_visc                      ../starter/source/materials/visc/hm_read_visc.F
      !||    hm_yctrl                          ../starter/source/elements/initia/hm_yctrl.F
      !||    lecfill                           ../starter/source/elements/initia/lecfill.F
      !||    lecsubmod                         ../starter/source/model/submodel/lecsubmod.F
      !||    lectrans                          ../starter/source/model/transformation/lectrans.F
      !||    lectranssub                       ../starter/source/model/submodel/lectranssub.F
      !||    lectur                            ../starter/source/starter/lectur.F
      !||    nbfunct                           ../starter/source/tools/curve/nbfunc.F
      !||    nbsph                             ../starter/source/elements/sph/nbsph.F
      !||    r2r_prelec                        ../starter/source/coupling/rad2rad/r2r_prelec.F
      !||    read_ale_mat                      ../starter/source/materials/ale/read_ale_mat.F
      !||    read_detonators                   ../starter/source/initial_conditions/detonation/read_detonators.F
      !||    read_engine_driver                ../starter/source/general_controls/engine/read_engine_driver.F
      !||    read_euler_mat                    ../starter/source/materials/ale/read_euler_mat.F
      !||    read_rwall                        ../starter/source/constraints/general/rwall/read_rwall.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    message_mod                       ../starter/share/message_module/message_mod.F
      !||    submodel_mod                      ../starter/share/modules1/submodel_mod.F
      !||====================================================================
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
