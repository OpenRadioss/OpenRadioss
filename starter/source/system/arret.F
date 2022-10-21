Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  ARRET                         source/system/arret.F         
Chd|-- called by -----------
Chd|        INTBUF_INI                    ../common_source/interf/intbuf_ini.F
Chd|        PBLAST_LOAD                   ../common_source/modules/pblast_mod.F
Chd|        UPGRADE_CAND_OPT              ../common_source/interf/upgrade_multimp.F
Chd|        UPGRADE_LCAND_E2S             ../common_source/interf/upgrade_multimp.F
Chd|        UPGRADE_LCAND_EDG             ../common_source/interf/upgrade_multimp.F
Chd|        UPGRADE_MULTIMP               ../common_source/interf/upgrade_multimp.F
Chd|        ANCMSG                        source/output/message/message.F
Chd|        ANI_FASOLFR1                  source/output/anim/ani_fasolfr.F
Chd|        ARRET_MESSAGE                 source/interfaces/int24/arret_message.F
Chd|        ARRET_MESSAGE_SLID            source/interfaces/int24/arret_message.F
Chd|        C3GRTAILS                     source/elements/sh3n/coque3n/c3grtails.F
Chd|        CGRTAILS                      source/elements/shell/coque/cgrtails.F
Chd|        CONTRL                        source/starter/contrl.F       
Chd|        C_TRICALL                     stub/fvmbags_stub.F           
Chd|        ERROR_ALLOC                   source/system/error_alloc.F   
Chd|        FACEPOLY                      source/airbag/facepoly.F      
Chd|        FREDEC2IM                     source/output/message/fredec2im.F
Chd|        FREDEC3M                      source/output/message/fredec3m.F
Chd|        FVBRIC0                       source/airbag/fvbric0.F       
Chd|        FVBRIC01                      source/airbag/fvbric01.F      
Chd|        HM_PREREAD_BEM                source/loads/bem/hm_read_bem.F
Chd|        HM_PREREAD_PBLAST             source/loads/pblast/hm_preread_pblast.F
Chd|        HM_READ_BEM                   source/loads/bem/hm_read_bem.F
Chd|        HM_READ_FAIL_USER             source/materials/fail/failuser/hm_read_fail_user.F
Chd|        HM_READ_MAT                   source/materials/mat/hm_read_mat.F
Chd|        HM_READ_MAT29_31              source/materials/mat/matuser/hm_read_mat_user29_31.F
Chd|        HM_READ_MAT_99                source/materials/mat/matuser/hm_read_mat_user_99.F
Chd|        HM_READ_PBLAST                source/loads/pblast/hm_read_pblast.F
Chd|        HM_READ_PROP06                source/properties/solid/hm_read_prop06.F
Chd|        HM_READ_PROP14                source/properties/solid/hm_read_prop14.F
Chd|        HM_READ_PROP19                source/properties/shell/hm_read_prop19.F
Chd|        HM_READ_PROP_USER             source/properties/user_spring_solid/hm_read_prop_user.F
Chd|        HM_READ_RETRACTOR             source/tools/seatbelts/hm_read_retractor.F
Chd|        HM_READ_SLIPRING              source/tools/seatbelts/hm_read_slipring.F
Chd|        HM_READ_WINDOW_USER           source/tools/userwi/hm_read_window_user.F
Chd|        INITIA                        source/elements/initia/initia.F
Chd|        LAW111_UPD                    source/materials/mat/mat111/law111_upd.F
Chd|        LAW69_UPD                     source/materials/mat/mat069/law69_upd.F
Chd|        LAW88_UPD                     source/materials/mat/mat088/law88_upd.F
Chd|        LECSTACK_PLY                  source/properties/composite_options/stack/lecstack_ply.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        M20DCOD                       source/system/fsdcod.F        
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|        MONVOL_ORIENT_SURF            share/modules1/monvol_struct_mod.F
Chd|        POLYHEDR1                     source/airbag/polyhedr1.F     
Chd|        RBE3CL                        source/constraints/general/kinchk.F
Chd|        READ_MSGFILE                  source/output/message/read_msgfile.F
Chd|        READ_SENSOR_USER              source/tools/sensor/read_sensor_user.F
Chd|        SGRTAILS                      source/elements/solid/solide/sgrtails.F
Chd|        SMS_INI_JAD_3                 source/ams/sms_init.F         
Chd|        SORT_SET                      source/model/sets/sort_sets.F 
Chd|        SPTRI                         source/elements/sph/sptri.F   
Chd|        T3GRTAILS                     source/elements/solid_2d/tria/t3grtails.F
Chd|        TABLE_INTERP                  source/tools/curve/table_tools.F
Chd|        TABLE_VINTERP                 source/tools/curve/table_tools.F
Chd|        TRIBOX3                       stub/fvmbags_stub.F           
Chd|        TRITRI3                       stub/fvmbags_stub.F           
Chd|        W_BUFBRIC_22                  source/interfaces/inter3d1/w_bufbric_22.F
Chd|        W_FRONT                       source/restart/ddsplit/w_front.F
Chd|        W_INTBUF_SIZE                 source/restart/ddsplit/inter_tools.F
Chd|-- calls ---------------
Chd|        MY_EXIT                       source/output/analyse/analyse.c
Chd|        SUMMSG                        source/output/message/summsg.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|====================================================================
      SUBROUTINE ARRET (NN)
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NN
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "titr_c.inc"
#include       "xtimscr_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER N
      INTEGER EXIT_VALUE
      CALL starter_lic_release()
      N=NN
      !-------------------------!
      !     ARRET DU CALCUL     !
      !-------------------------!
      IF(N==2 .OR. N==3) THEN
        WRITE(ISTDO,1000)
        WRITE(IOUT,1000)
 1000   FORMAT(//,' RADIOSS STOP DUE TO INPUT ERROR')
      ENDIF
      
      IF(N/=3 .AND. IERR>0)N=2
      
C-----------------------------------------------
C Summary Errors/Warnings
C-----------------------------------------------
      CALL SUMMSG()
C-----------------------------------------------

      IF(N==2) THEN
        IERR=MAX0(IERR,1)
           WRITE(ISTDO,'(///A/A/,5X,I10,A)')TITRE(7)(1:40),TITRE(5),IERR,TITRE(7)(41:60)
           WRITE(IOUT ,'(///A/A/,5X,I10,A)')TITRE(7)(1:40),TITRE(5),IERR,TITRE(7)(41:60)
      ENDIF
      
      IF(N==1.OR.IWARN>0) THEN
        IF(IWARN==0) THEN
           WRITE(ISTDO,'(///A/A)')TITRE(6),TITRE(5)
           WRITE(IOUT ,'(///A/A)')TITRE(6),TITRE(5)
        ELSE
           WRITE(ISTDO,'(///A/A/,5X,I10,A)')TITRE(8)(1:40),TITRE(5),IWARN,TITRE(8)(41:60)
           WRITE(IOUT ,'(///A/A/,5X,I10,A)')TITRE(8)(1:40),TITRE(5),IWARN,TITRE(8)(41:60)
        ENDIF
      ENDIF

      CLOSE (UNIT=IOUT)

      IF(N == 2.OR. N ==3 )THEN
        CALL MY_EXIT(N)
      ELSE
        EXIT_VALUE=0
        CALL MY_EXIT(EXIT_VALUE)
      ENDIF
      END
