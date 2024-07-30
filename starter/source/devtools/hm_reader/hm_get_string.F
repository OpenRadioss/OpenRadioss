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
      !||    hm_get_string                ../starter/source/devtools/hm_reader/hm_get_string.F
      !||--- called by ------------------------------------------------------
      !||    contrl                       ../starter/source/starter/contrl.F
      !||    hm_preread_impvel0           ../starter/source/constraints/general/impvel/hm_preread_impvel0.F
      !||    hm_read_alebcs               ../starter/source/constraints/ale/hm_read_alebcs.F
      !||    hm_read_cload                ../starter/source/loads/general/cload/hm_read_cload.F
      !||    hm_read_eig                  ../starter/source/general_controls/computation/hm_read_eig.F
      !||    hm_read_eos                  ../starter/source/materials/eos/hm_read_eos.F
      !||    hm_read_eos_sesame           ../starter/source/materials/eos/hm_read_eos_sesame.F
      !||    hm_read_frm                  ../starter/source/tools/skew/hm_read_frm.F
      !||    hm_read_fxb1                 ../starter/source/constraints/fxbody/hm_read_fxb.F
      !||    hm_read_fxb2                 ../starter/source/constraints/fxbody/hm_read_fxb.F
      !||    hm_read_grav                 ../starter/source/loads/general/grav/hm_read_grav.F
      !||    hm_read_impacc               ../starter/source/constraints/general/impvel/hm_read_impacc.F
      !||    hm_read_inimap1d             ../starter/source/initial_conditions/inimap/hm_read_inimap1d.F
      !||    hm_read_inimap2d             ../starter/source/initial_conditions/inimap/hm_read_inimap2d.F
      !||    hm_read_inista               ../starter/source/initial_conditions/inista/hm_read_inista.F
      !||    hm_read_inivel               ../starter/source/initial_conditions/general/inivel/hm_read_inivel.F
      !||    hm_read_load_centri          ../starter/source/loads/general/load_centri/hm_read_load_centri.F
      !||    hm_read_load_pressure        ../starter/source/loads/general/load_pressure/hm_read_load_pressure.F
      !||    hm_read_mat02                ../starter/source/materials/mat/mat002/hm_read_mat02.F
      !||    hm_read_mat26                ../starter/source/materials/mat/mat026/hm_read_mat26.F
      !||    hm_read_matgas               ../starter/source/materials/mat/matgas/hm_read_matgas.F
      !||    hm_read_perturb_fail         ../starter/source/general_controls/computation/hm_read_perturb_fail.F
      !||    hm_read_perturb_part_shell   ../starter/source/general_controls/computation/hm_read_perturb_part_shell.F
      !||    hm_read_perturb_part_solid   ../starter/source/general_controls/computation/hm_read_perturb_part_solid.F
      !||    hm_read_pfluid               ../starter/source/loads/general/pfluid/hm_read_pfluid.F
      !||    hm_read_refsta               ../starter/source/loads/reference_state/refsta/hm_read_refsta.F
      !||    hm_read_skw                  ../starter/source/tools/skew/hm_read_skw.F
      !||    hm_read_spcnd                ../starter/source/constraints/sph/hm_read_spcnd.F
      !||    hm_read_unit                 ../starter/source/general_controls/computation/hm_read_unit.F
      !||    hm_set                       ../starter/source/model/sets/hm_set.F
      !||    hm_setfxrbyon                ../starter/source/constraints/fxbody/hm_setfxrbyon.F
      !||    lecsec42                     ../starter/source/tools/sect/hm_read_sect.F
      !||    lecsec4bolt                  ../starter/source/tools/sect/lecsec4bolt.F
      !||    read_impdisp                 ../starter/source/constraints/general/impvel/read_impdisp.F
      !||    read_impvel                  ../starter/source/constraints/general/impvel/read_impvel.F
      !||    read_impvel_lagmul           ../starter/source/constraints/general/impvel/read_impvel_lagmul.F
      !||    read_sensor_acc              ../starter/source/tools/sensor/read_sensor_acc.F
      !||    read_sensor_contact          ../starter/source/tools/sensor/read_sensor_contact.F
      !||    read_sensor_hic              ../starter/source/tools/sensor/read_sensor_hic.F
      !||    read_sensor_nic              ../starter/source/tools/sensor/read_sensor_nic.F
      !||    read_sensor_rbody            ../starter/source/tools/sensor/read_sensor_rbody.F
      !||    read_sensor_rwall            ../starter/source/tools/sensor/read_sensor_rwall.F
      !||    read_sensor_sect             ../starter/source/tools/sensor/read_sensor_sect.F
      !||    sort_set                     ../starter/source/model/sets/sort_sets.F
      !||    st_qaprint_set               ../starter/source/output/qaprint/st_qaprint_set.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||    message_mod                  ../starter/share/message_module/message_mod.F
      !||====================================================================
      SUBROUTINE HM_GET_STRING(NAME,SVAL,SIZE,IS_AVAILABLE)
C-----------------------------------------------
C   ROUTINE DESCRIPTION :
C   ===================
C   REQUEST DATA INTO MODEL NEUTRAL OBJECT DATABASE USING HM_READER
C   PICK VALUE
C   ASK STRING VALUE OF 'NAME' FIELD DEFINED IN .cfg FILE 
C-----------------------------------------------
C   DUMMY ARGUMENTS DESCRIPTION:
C   ===================
C
C     NAME         DESCRIPTION              
C
C     NAME         FIELD NAME         
C     SVAL         STRING VALUE OF THE FIELD 
C     IS_AVAILABLE VALUE AVAILABLE IN MODEL OR NOT
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD    
      USE, INTRINSIC :: ISO_C_BINDING, ONLY: C_BOOL 
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C INPUT ARGUMENTS
      CHARACTER*(*),INTENT(IN)::NAME
C OUTPUT ARGUMENTS
      CHARACTER*(*),INTENT(OUT)::SVAL
      INTEGER,INTENT(IN)::SIZE
      LOGICAL,INTENT(OUT)::IS_AVAILABLE
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,VALUE_TYPE,SUB_ID
      LOGICAL(KIND=C_BOOL) C_IS_AVAILABLE
C-----------------------------------------------
      SVAL = ''
C--------------------------------------------------
      C_IS_AVAILABLE = .FALSE.
      CALL CPP_GET_STRING(NAME(1:LEN_TRIM(NAME)),LEN_TRIM(NAME),SVAL,SIZE,C_IS_AVAILABLE)
      IS_AVAILABLE = C_IS_AVAILABLE
C--------------------------------------------------
      RETURN
C
      END
