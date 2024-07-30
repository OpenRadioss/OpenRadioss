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
      !||    arret                   ../starter/source/system/arret.F
      !||--- called by ------------------------------------------------------
      !||    ancmsg                  ../starter/source/output/message/message.F
      !||    ani_fasolfr1            ../starter/source/output/anim/ani_fasolfr.F
      !||    arret_message           ../starter/source/interfaces/int24/arret_message.F
      !||    arret_message_slid      ../starter/source/interfaces/int24/arret_message.F
      !||    c3grtails               ../starter/source/elements/sh3n/coque3n/c3grtails.F
      !||    c_tricall               ../starter/stub/fvmbags_stub.F
      !||    cgrtails                ../starter/source/elements/shell/coque/cgrtails.F
      !||    contrl                  ../starter/source/starter/contrl.F
      !||    error_alloc             ../starter/source/system/error_alloc.F
      !||    facepoly                ../starter/source/airbag/facepoly.F
      !||    fredec2im               ../starter/source/output/message/fredec2im.F
      !||    fredec3m                ../starter/source/output/message/fredec3m.F
      !||    fvbric0                 ../starter/source/airbag/fvbric0.F
      !||    fvbric01                ../starter/source/airbag/fvbric01.F
      !||    hm_preread_bem          ../starter/source/loads/bem/hm_read_bem.F
      !||    hm_preread_pblast       ../starter/source/loads/pblast/hm_preread_pblast.F
      !||    hm_read_bem             ../starter/source/loads/bem/hm_read_bem.F
      !||    hm_read_fail_user       ../starter/source/materials/fail/failuser/hm_read_fail_user.F
      !||    hm_read_mat             ../starter/source/materials/mat/hm_read_mat.F
      !||    hm_read_mat29_31        ../starter/source/materials/mat/matuser/hm_read_mat_user29_31.F
      !||    hm_read_mat_99          ../starter/source/materials/mat/matuser/hm_read_mat_user_99.F
      !||    hm_read_pblast          ../starter/source/loads/pblast/hm_read_pblast.F
      !||    hm_read_prop06          ../starter/source/properties/solid/hm_read_prop06.F
      !||    hm_read_prop14          ../starter/source/properties/solid/hm_read_prop14.F
      !||    hm_read_prop19          ../starter/source/properties/shell/hm_read_prop19.F
      !||    hm_read_prop_user       ../starter/source/properties/user_spring_solid/hm_read_prop_user.F
      !||    hm_read_retractor       ../starter/source/tools/seatbelts/hm_read_retractor.F
      !||    hm_read_slipring        ../starter/source/tools/seatbelts/hm_read_slipring.F
      !||    hm_read_window_user     ../starter/source/tools/userwi/hm_read_window_user.F
      !||    i24fic_getn             ../starter/source/interfaces/inter3d1/i24surfi.F
      !||    initia                  ../starter/source/elements/initia/initia.F
      !||    law111_upd              ../starter/source/materials/mat/mat111/law111_upd.F
      !||    law69_upd               ../starter/source/materials/mat/mat069/law69_upd.F
      !||    law88_upd               ../starter/source/materials/mat/mat088/law88_upd.F
      !||    lecstack_ply            ../starter/source/properties/composite_options/stack/lecstack_ply.F
      !||    lectur                  ../starter/source/starter/lectur.F
      !||    m20dcod                 ../starter/source/system/fsdcod.F
      !||    matini                  ../starter/source/materials/mat_share/matini.F
      !||    monvol_orient_surf      ../starter/share/modules1/monvol_struct_mod.F
      !||    polyhedr1               ../starter/source/airbag/polyhedr1.F
      !||    rbe3cl                  ../starter/source/constraints/general/kinchk.F
      !||    read_msgfile            ../starter/source/output/message/read_msgfile.F
      !||    read_sensor_user        ../starter/source/tools/sensor/read_sensor_user.F
      !||    sgrtails                ../starter/source/elements/solid/solide/sgrtails.F
      !||    sms_ini_jad_3           ../starter/source/ams/sms_init.F
      !||    sort_set                ../starter/source/model/sets/sort_sets.F
      !||    sptri                   ../starter/source/elements/sph/sptri.F
      !||    t3grtails               ../starter/source/elements/solid_2d/tria/t3grtails.F
      !||    table_interp            ../starter/source/tools/curve/table_tools.F
      !||    table_vinterp           ../starter/source/tools/curve/table_tools.F
      !||    tribox3                 ../starter/stub/fvmbags_stub.F
      !||    tritri3                 ../starter/stub/fvmbags_stub.F
      !||    w_bufbric_22            ../starter/source/interfaces/inter3d1/w_bufbric_22.F
      !||    w_front                 ../starter/source/restart/ddsplit/w_front.F
      !||    w_intbuf_size           ../starter/source/restart/ddsplit/inter_tools.F
      !||--- calls      -----------------------------------------------------
      !||    my_exit                 ../starter/source/output/analyse/analyse.c
      !||    summsg                  ../starter/source/output/message/summsg.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod             ../starter/share/message_module/message_mod.F
      !||====================================================================
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
#include      "titr_c.inc"
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
