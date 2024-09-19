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
C
      !||====================================================================
      !||    fredec0                ../starter/source/starter/freform.F
      !||--- called by ------------------------------------------------------
      !||    lecig3d                ../starter/source/elements/ige3d/lecig3d.F
      !||    nbadigemesh            ../starter/source/elements/ige3d/nbadigemesh.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||    freerr                 ../starter/source/starter/freform.F
      !||    my_exit                ../starter/source/output/analyse/analyse.c
      !||--- uses       -----------------------------------------------------
      !||    format_mod             ../starter/share/modules1/format_mod.F90
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||    reader_old_mod         ../starter/share/modules1/reader_old_mod.F90
      !||====================================================================
      SUBROUTINE FREDEC0(ID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARFIELD
      USE FORMAT_MOD , ONLY : LFIELD, FMT_I
      USE READER_OLD_MOD , ONLY : KLINE
      USE USER_ID_MOD , ONLY : ID_LIMIT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
      INTEGER IOP,ID
      CHARACTER(LEN=NCHARFIELD) :: MOT1
      INTEGER I,J1,J2
C-----------------------------------------------
C   /KEYW/int_id
C-----------------------------------------------
      I=2
      DO WHILE(KLINE(I:I)/='/')
        I=I+1
        IF(I>ncharline)CALL FREERR(0)
      ENDDO
      I=I+1
      IF(I>ncharline)CALL FREERR(0)
      J1=I
      MOT1=KLINE(J1:J1-1+LFIELD)
      READ(MOT1,ERR=999,FMT=FMT_I)ID
      IF (ID>ID_LIMIT%GLOBAL.OR.ID<=0) THEN
        CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              I1=ID,C1=KLINE)
      ENDIF
C
      RETURN
 999  CALL FREERR(0)
      CALL MY_EXIT(2)
      END
C
      !||====================================================================
      !||    fredec4                ../starter/source/starter/freform.F
      !||--- calls      -----------------------------------------------------
      !||    freerr                 ../starter/source/starter/freform.F
      !||--- uses       -----------------------------------------------------
      !||    reader_old_mod         ../starter/share/modules1/reader_old_mod.F90
      !||====================================================================
      SUBROUTINE FREDEC4(COPT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARFIELD, NCHARLINE
      USE READER_OLD_MOD , ONLY : KLINE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C
      CHARACTER(LEN=NCHARFIELD) :: COPT
C
      INTEGER I,J1,J2,J
C-----------------------------------------------
C   /KEYW/int_type/int_id
C-----------------------------------------------
      I=2
      DO WHILE(KLINE(I:I)/='/')
        I=I+1

        IF(I>ncharline)CALL FREERR(0)
      ENDDO
      I=I+1

      IF(I>ncharline)CALL FREERR(0)
      J1=I
      DO WHILE(KLINE(I:I)/='/')
        I=I+1
        IF(I>ncharline)CALL FREERR(0)

      ENDDO
      J2=I-1
      COPT=KLINE(J1:J2)
C
      RETURN
      END
      !||====================================================================
      !||    fredec5                ../starter/source/starter/freform.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||    my_exit                ../starter/source/output/analyse/analyse.c
      !||--- uses       -----------------------------------------------------
      !||    format_mod             ../starter/share/modules1/format_mod.F90
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||    reader_old_mod         ../starter/share/modules1/reader_old_mod.F90
      !||====================================================================
      SUBROUTINE FREDEC5(COPT,ID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE READER_OLD_MOD , ONLY : KLINE
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARKEY, NCHARFIELD,NCHARLINE
      USE FORMAT_MOD , ONLY : LFIELD, FMT_I
      USE USER_ID_MOD , ONLY : ID_LIMIT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
      INTEGER ID
      CHARACTER(LEN=NCHARKEY) ::  COPT
C-----------------------------------------------
      CHARACTER(LEN=NCHARFIELD) :: MOT1
      INTEGER I,J1,J2,J
C-----------------------------------------------
C   /KEYW    ou
C   /KEYW/KEYW2/int_id
C-----------------------------------------------
      I=2

      DO WHILE(KLINE(I:I)/='/'.AND.I<ncharline)
        I=I+1
      ENDDO
      COPT=' '
      I=I+1

      IF(I>ncharline)RETURN
      J1=I

      DO WHILE(KLINE(I:I)/='/'.AND.I<ncharline)
        I=I+1
      ENDDO
      J2=I-1
      COPT=KLINE(J1:J2)
C
      ID=0
      I=I+1
      I=MIN(I,ncharline)
      IF(I>ncharline-LFIELD+1)RETURN
      J1=I
      DO WHILE(KLINE(I:I)/='/')
        I=I+1
                IF(I>ncharline)EXIT
      ENDDO
      J2=I-1

      J2=MIN(I-1+LFIELD,J2)
      MOT1=KLINE(J1:J2)

      READ(MOT1,ERR=999,FMT=FMT_I)ID
C
      IF (ID>ID_LIMIT%GLOBAL.OR.ID<=0) THEN
        CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              I1=ID,C1=KLINE)
      ENDIF

C
      RETURN
 999  CALL FREERR(0)
      CALL MY_EXIT(2)
      END
      !||====================================================================
      !||    fredec6                ../starter/source/starter/freform.F
      !||--- calls      -----------------------------------------------------
      !||    my_exit                ../starter/source/output/analyse/analyse.c
      !||--- uses       -----------------------------------------------------
      !||    reader_old_mod         ../starter/share/modules1/reader_old_mod.F90
      !||====================================================================
      SUBROUTINE FREDEC6(COPT,COPT2)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARKEY,NCHARLINE
      USE READER_OLD_MOD , ONLY : KLINE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
      CHARACTER(LEN=NCHARKEY) :: COPT,COPT2
C-----------------------------------------------
      INTEGER I,J1,J2,J
C-----------------------------------------------
C   /KEYW/KEYW2/KEYW3
C-----------------------------------------------
      I=2

      DO WHILE(KLINE(I:I)/='/'.AND.I<ncharline)
        I=I+1
      ENDDO
      COPT=' '
      I=I+1

      IF(I>ncharline)RETURN
      J1=I

      DO WHILE(KLINE(I:I)/='/'.AND.I<ncharline)
        I=I+1
      ENDDO
      J2=I-1
      COPT=KLINE(J1:J2)
C
      COPT2=' '
      I=I+1
      I=MIN(I,ncharline)
      J1=I

      DO WHILE(KLINE(I:I)/='/'.AND.I<ncharline)
        I=I+1
      ENDDO
    
      IF(I>ncharline)RETURN
      J2=I-1
      COPT2=KLINE(J1:J2)
C
      RETURN
 999  CALL FREERR(0)
      CALL MY_EXIT(2)
      END
      !||====================================================================
      !||    nodgrnr5               ../starter/source/starter/freform.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_bcs_wall       ../starter/source/boundary_conditions/hm_read_bcs_wall.F90
      !||    hm_read_cload          ../starter/source/loads/general/cload/hm_read_cload.F
      !||    hm_read_cyljoint       ../starter/source/constraints/general/cyl_joint/hm_read_cyljoint.F
      !||    hm_read_grav           ../starter/source/loads/general/grav/hm_read_grav.F
      !||    hm_read_impacc         ../starter/source/constraints/general/impvel/hm_read_impacc.F
      !||    hm_read_imptemp        ../starter/source/constraints/thermic/hm_read_imptemp.F
      !||    hm_read_load_centri    ../starter/source/loads/general/load_centri/hm_read_load_centri.F
      !||    hm_read_merge          ../starter/source/constraints/general/merge/hm_read_merge.F
      !||    hm_read_rbe3           ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
      !||    hm_read_xelem          ../starter/source/elements/reader/hm_read_xelem.F
      !||    lecig3d                ../starter/source/elements/ige3d/lecig3d.F
      !||    lecsec42               ../starter/source/tools/sect/hm_read_sect.F
      !||    lecsec4bolt            ../starter/source/tools/sect/lecsec4bolt.F
      !||    read_dfs_detcord       ../starter/source/initial_conditions/detonation/read_dfs_detcord.F
      !||    read_dfs_detline       ../starter/source/initial_conditions/detonation/read_dfs_detline.F
      !||    read_dfs_detplan       ../starter/source/initial_conditions/detonation/read_dfs_detplan.F
      !||    read_dfs_detpoint      ../starter/source/initial_conditions/detonation/read_dfs_detpoint.F
      !||    read_dfs_wave_shaper   ../starter/source/initial_conditions/detonation/read_dfs_wave_shaper.F
      !||    read_impdisp           ../starter/source/constraints/general/impvel/read_impdisp.F
      !||    read_impvel            ../starter/source/constraints/general/impvel/read_impvel.F
      !||    read_impvel_lagmul     ../starter/source/constraints/general/impvel/read_impvel_lagmul.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||====================================================================
      INTEGER FUNCTION NODGRNR5(IGU    ,IGS    ,IBUF,IGRNOD,
     .                          ITABM1 ,MESS   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
      INTEGER IGU,IGS,IBUF(*),ITABM1(*)
      CHARACTER MESS*40
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
      INTEGER I
C=======================================================================
      NODGRNR5 = 0
      IF (IGU > 0) THEN
        IGS=0
        DO I=1,NGRNOD
          IF(IGRNOD(I)%ID == IGU) THEN
            IGS=I
            NODGRNR5 = IGRNOD(IGS)%NENTITY
            EXIT
          ENDIF
        ENDDO
C
        IF (IGS == 0)THEN
           CALL ANCMSG(MSGID=53,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 C1= MESS,
     .                 I1=IGU)
           RETURN
        ENDIF
C
        DO I=1,NODGRNR5
          IBUF(I)=IGRNOD(IGS)%ENTITY(I)
        ENDDO
      ENDIF
C---
      RETURN
      END
      !||====================================================================
      !||    nodgrnr6               ../starter/source/starter/freform.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_rbe2           ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    hm_read_rbody          ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    hm_read_rbody_lagmul   ../starter/source/constraints/general/rbody/hm_read_rbody_lagmul.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||====================================================================
      INTEGER FUNCTION NODGRNR6(M      ,IGU    ,IGS    ,IBUF,IGRNOD,
     .                          ITABM1 ,MESS   ,ID     )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C  THIS FUNCTION REMOVES main NODE FROM SECND NODAL SET
C  IN THE KINEMATIC CONSTRAINT DEFINITION 
C-----------------------------------------------
      INTEGER M,IGU,IGS,ID,IBUF(*),ITABM1(*)
      CHARACTER MESS*40
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
      INTEGER I, MFLAG
C=======================================================================
      NODGRNR6 = 0
      IF (IGU > 0) THEN
        IGS=0
        DO I=1,NGRNOD
          IF(IGRNOD(I)%ID == IGU) THEN
            IGS=I
            NODGRNR6 = IGRNOD(IGS)%NENTITY
            EXIT
          ENDIF
        ENDDO
C
        IF (IGS == 0)THEN
           CALL ANCMSG(MSGID=53,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO,
     .                 C1= MESS,
     .                 I1=IGU)
           RETURN
        ENDIF
C
        MFLAG=0
        DO I=1,NODGRNR6
          IF(IGRNOD(IGS)%ENTITY(I)==M) THEN
            MFLAG=1
          ELSE
            IBUF(I-MFLAG)=IGRNOD(IGS)%ENTITY(I)
          ENDIF
        ENDDO
        IF(MFLAG==1) THEN 
          NODGRNR6=NODGRNR6-1
          CALL ANCMSG(MSGID=1624,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID)
        ENDIF
      ENDIF
C---
      RETURN
      END
      !||====================================================================
      !||    grfind         ../starter/source/starter/freform.F
      !||--- called by ------------------------------------------------------
      !||    lecextlnk      ../starter/source/coupling/rad2rad/lecextlnk.F
      !||    r2r_check      ../starter/source/coupling/rad2rad/r2r_check.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg         ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod    ../starter/share/message_module/message_mod.F
      !||====================================================================
      INTEGER FUNCTION GRFIND(IGU,IGRNOD,MESS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
      INTEGER IGU
      CHARACTER MESS*40
C-----------------------------------------------
      TYPE (GROUP_)  ,DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
      INTEGER I,IG
C-----------------------------------------------
      IG=0
      DO I=1,NGRNOD
        IF(IGRNOD(I)%ID==IGU)THEN
          IG=I
        ENDIF
      ENDDO
C
      IF(IG==0)THEN
        CALL ANCMSG(MSGID=53,
     .              MSGTYPE=MSGERROR,
     .              ANMODE=ANINFO,
     .              I1=IGU,
     .              C1='IN NODE GROUP SEARCH')
        RETURN                  
      ENDIF
      GRFIND = IG
C----
      RETURN
      END
C
C
      !||====================================================================
      !||    freerr                  ../starter/source/starter/freform.F
      !||--- called by ------------------------------------------------------
      !||    fredec0                 ../starter/source/starter/freform.F
      !||    fredec4                 ../starter/source/starter/freform.F
      !||    fredec_2key_4id         ../starter/source/starter/freform.F
      !||    fredec_2key_4id_t       ../starter/source/starter/freform.F
      !||    fredec_key_3id_t        ../starter/source/starter/freform.F
      !||    hm_read_inivel          ../starter/source/initial_conditions/general/inivel/hm_read_inivel.F
      !||    hm_read_monvol_type1    ../starter/source/airbag/hm_read_monvol_type1.F
      !||    hm_read_monvol_type10   ../starter/source/airbag/hm_read_monvol_type10.F
      !||    hm_read_monvol_type11   ../starter/source/airbag/hm_read_monvol_type11.F
      !||    hm_read_monvol_type2    ../starter/source/airbag/hm_read_monvol_type2.F
      !||    hm_read_monvol_type3    ../starter/source/airbag/hm_read_monvol_type3.F
      !||    hm_read_monvol_type4    ../starter/source/airbag/hm_read_monvol_type4.F
      !||    hm_read_monvol_type5    ../starter/source/airbag/hm_read_monvol_type5.F
      !||    hm_read_monvol_type6    ../starter/source/airbag/hm_read_monvol_type6.F
      !||    hm_read_monvol_type7    ../starter/source/airbag/hm_read_monvol_type7.F
      !||    hm_read_monvol_type8    ../starter/source/airbag/hm_read_monvol_type8.F
      !||    hm_read_monvol_type9    ../starter/source/airbag/hm_read_monvol_type9.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                  ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod             ../starter/share/message_module/message_mod.F
      !||    reader_old_mod          ../starter/share/modules1/reader_old_mod.F90
      !||====================================================================
      SUBROUTINE FREERR(IT)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE READER_OLD_MOD , ONLY : KLINE, LINE, KEY0, KCUR, IREC
      USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IT
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IT1
C=======================================================================
          IT1 = IT
      IF(IT1==3)THEN
        IT1=0
        READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
        IT1=1
 999    CONTINUE
      ENDIF
      IF(IT1==0)THEN
          CALL ANCMSG(MSGID=54,
     .                ANMODE=ANINFO,
     .                MSGTYPE=MSGERROR,
     .                C1=KLINE)
      ELSEIF(IT1==1)THEN
          CALL ANCMSG(MSGID=55,
     .                ANMODE=ANINFO,
     .                MSGTYPE=MSGERROR,
     .                C1=KEY0(KCUR),
     .                C2=KLINE,
     .                C3=LINE)
      ENDIF
      END
      !||====================================================================
      !||    fretitl                      ../starter/source/starter/freform.F
      !||--- called by ------------------------------------------------------
      !||    hm_preread_bcscyc            ../starter/source/constraints/general/bcs/lecbcscyc.F
      !||    hm_preread_sphio             ../starter/source/loads/sph/hm_preread_sphio.F
      !||    hm_read_bcs                  ../starter/source/constraints/general/bcs/hm_read_bcs.F
      !||    hm_read_cluster              ../starter/source/output/cluster/hm_read_cluster.F
      !||    hm_read_cyljoint             ../starter/source/constraints/general/cyl_joint/hm_read_cyljoint.F
      !||    hm_read_friction             ../starter/source/interfaces/friction/reader/hm_read_friction.F
      !||    hm_read_frm                  ../starter/source/tools/skew/hm_read_frm.F
      !||    hm_read_funct                ../starter/source/tools/curve/hm_read_funct.F
      !||    hm_read_fxb1                 ../starter/source/constraints/fxbody/hm_read_fxb.F
      !||    hm_read_gauge                ../starter/source/output/gauge/hm_read_gauge.F
      !||    hm_read_gjoint               ../starter/source/constraints/general/gjoint/hm_read_gjoint.F
      !||    hm_read_inter_fsi            ../starter/source/interfaces/reader/hm_read_inter_fsi.F
      !||    hm_read_inter_lagdt_type07   ../starter/source/interfaces/int07/hm_read_inter_lagdt_type07.F
      !||    hm_read_inter_lagmul         ../starter/source/interfaces/reader/hm_read_inter_lagmul.F
      !||    hm_read_inter_struct         ../starter/source/interfaces/reader/hm_read_inter_struct.F
      !||    hm_read_intsub               ../starter/source/output/subinterface/hm_read_intsub.F
      !||    hm_read_link                 ../starter/source/constraints/rigidlink/hm_read_rlink.F
      !||    hm_read_mat                  ../starter/source/materials/mat/hm_read_mat.F
      !||    hm_read_merge                ../starter/source/constraints/general/merge/hm_read_merge.F
      !||    hm_read_mpc                  ../starter/source/constraints/general/mpc/hm_read_mpc.F
      !||    hm_read_nbcs                 ../starter/source/constraints/general/bcs/hm_read_nbcs.F
      !||    hm_read_part                 ../starter/source/model/assembling/hm_read_part.F
      !||    hm_read_properties           ../starter/source/properties/hm_read_properties.F
      !||    hm_read_rbe2                 ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    hm_read_rbe3                 ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
      !||    hm_read_rbody                ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    hm_read_rbody_lagmul         ../starter/source/constraints/general/rbody/hm_read_rbody_lagmul.F
      !||    hm_read_retractor            ../starter/source/tools/seatbelts/hm_read_retractor.F
      !||    hm_read_rwall_cyl            ../starter/source/constraints/general/rwall/hm_read_rwall_cyl.F
      !||    hm_read_rwall_lagmul         ../starter/source/constraints/general/rwall/hm_read_rwall_lagmul.F
      !||    hm_read_rwall_paral          ../starter/source/constraints/general/rwall/hm_read_rwall_paral.F
      !||    hm_read_rwall_plane          ../starter/source/constraints/general/rwall/hm_read_rwall_plane.F
      !||    hm_read_rwall_spher          ../starter/source/constraints/general/rwall/hm_read_rwall_spher.F
      !||    hm_read_rwall_therm          ../starter/source/constraints/general/rwall/hm_read_rwall_therm.F
      !||    hm_read_skw                  ../starter/source/tools/skew/hm_read_skw.F
      !||    hm_read_slipring             ../starter/source/tools/seatbelts/hm_read_slipring.F
      !||    hm_read_spcnd                ../starter/source/constraints/sph/hm_read_spcnd.F
      !||    hm_read_thgrne               ../starter/source/output/th/hm_read_thgrne.F
      !||    hm_read_thgrns               ../starter/source/output/th/hm_read_thgrns.F
      !||    hm_read_thgrou               ../starter/source/output/th/hm_read_thgrou.F
      !||    hm_read_thgrsens             ../starter/source/output/th/hm_read_thgrsens.F
      !||    hm_read_thgrsurf             ../starter/source/output/th/hm_read_thgrsurf.F
      !||    hm_read_thpart               ../starter/source/output/thpart/hm_read_thpart.F
      !||    hm_thgrki_vent               ../starter/source/output/th/hm_thgrki_vent.F
      !||    lecacc                       ../starter/source/tools/accele/lecacc.F
      !||    lecsec42                     ../starter/source/tools/sect/hm_read_sect.F
      !||    lecstack_ply                 ../starter/source/properties/composite_options/stack/lecstack_ply.F
      !||    prelecsec                    ../starter/source/tools/sect/prelecsec.F
      !||    prelecsec4bolt               ../starter/source/tools/sect/prelecsec4bolt.F
      !||    r2r_void                     ../starter/source/coupling/rad2rad/r2r_void.F
      !||    read_impdisp                 ../starter/source/constraints/general/impvel/read_impdisp.F
      !||    read_impdisp_fgeo            ../starter/source/constraints/general/impvel/read_impdisp_fgeo.F
      !||    read_impvel                  ../starter/source/constraints/general/impvel/read_impvel.F
      !||    read_impvel_fgeo             ../starter/source/constraints/general/impvel/read_impvel_fgeo.F
      !||    read_impvel_lagmul           ../starter/source/constraints/general/impvel/read_impvel_lagmul.F
      !||    read_monvol                  ../starter/source/airbag/read_monvol.F
      !||    w_gr_entity_e                ../starter/source/restart/ddsplit/w_gr_entity.F
      !||    w_gr_entity_n                ../starter/source/restart/ddsplit/w_gr_entity.F
      !||    w_gr_entity_p                ../starter/source/restart/ddsplit/w_gr_entity.F
      !||    w_islin_str                  ../starter/source/restart/ddsplit/w_islin_str.F
      !||    w_isurf_str                  ../starter/source/restart/ddsplit/w_isurf_str.F
      !||    w_subset_str                 ../starter/source/restart/ddsplit/w_subset_str.F
      !||    write_sensors                ../starter/source/tools/sensor/write_sensors.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      SUBROUTINE FRETITL(TITR,IASC,L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARLINE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y  A r g u m e  n t s
C-----------------------------------------------
      INTEGER L,IASC(*)
      CHARACTER TITR*(*)
C-----------------------------------------------
C   L o c a l    V a r i a b  l e s
C-----------------------------------------------
      INTEGER I,J, JMAX
C-----------------------------------------------
      J=1
      JMAX=MIN(ncharline,LEN(TITR))
      DO I=1,L
        IASC(I)=  ICHAR(TITR(J:J))*65536
        J = J+1
        IF(J>JMAX) EXIT                          ! Replacing ncharline by JMAX
        IASC(I)=  IASC(I) + ICHAR(TITR(J:J))*256
        J = J+1
        IF(J>JMAX) EXIT
        IASC(I)=  IASC(I) + ICHAR(TITR(J:J))
        J = J+1
        IF(J>JMAX) EXIT
      ENDDO
      RETURN
      END
      !||====================================================================
      !||    fretitl2                      ../starter/source/starter/freform.F
      !||--- called by ------------------------------------------------------
      !||    addmaspart                    ../starter/source/tools/admas/addmaspart.F
      !||    ale_euler_init                ../starter/source/materials/ale/ale_euler_init.F
      !||    bsigini                       ../starter/source/elements/beam/bsigini.F
      !||    buserini                      ../starter/source/elements/beam/buserini.F
      !||    c3grhead                      ../starter/source/elements/sh3n/coque3n/c3grhead.F
      !||    c3grtails                     ../starter/source/elements/sh3n/coque3n/c3grtails.F
      !||    c3init3                       ../starter/source/elements/sh3n/coque3n/c3init3.F
      !||    c3inmas                       ../starter/source/elements/sh3n/coque3n/c3inmas.F
      !||    cbainit3                      ../starter/source/elements/shell/coqueba/cbainit3.F
      !||    cgrtails                      ../starter/source/elements/shell/coque/cgrtails.F
      !||    checkmp                       ../starter/source/elements/initia/initia.F
      !||    checkrby                      ../starter/source/constraints/general/rbody/checkrby.F
      !||    chekmp2                       ../starter/source/elements/initia/initia.F
      !||    chkfunct                      ../starter/source/tools/curve/lecfun.F
      !||    chktyp2                       ../starter/source/interfaces/interf1/chktyp2.F
      !||    cinit3                        ../starter/source/elements/shell/coque/cinit3.F
      !||    cinmas                        ../starter/source/elements/shell/coque/cinmas.F
      !||    cm27in3                       ../starter/source/materials/mat/mat027/cm27in3.F
      !||    cmatini4                      ../starter/source/materials/mat_share/cmatini4.F
      !||    corthini                      ../starter/source/elements/shell/coque/corthini.F
      !||    csigini                       ../starter/source/elements/shell/coque/csigini.F
      !||    csigini4                      ../starter/source/elements/shell/coqueba/scigini4.F
      !||    cuserini4                     ../starter/source/elements/shell/coqueba/cuserini4.F
      !||    desout                        ../starter/source/output/outp/desout.F
      !||    ecrsub2                       ../starter/source/model/assembling/hm_read_subset.F
      !||    fsdcod                        ../starter/source/system/fsdcod.F
      !||    genani1                       ../starter/source/output/anim/genani1.F
      !||    hireorbe3                     ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
      !||    hm_read_fxb1                  ../starter/source/constraints/fxbody/hm_read_fxb.F
      !||    hm_read_inistate_d00          ../starter/source/elements/initia/hm_read_inistate_d00.F
      !||    hm_read_intsub                ../starter/source/output/subinterface/hm_read_intsub.F
      !||    hm_read_merge                 ../starter/source/constraints/general/merge/hm_read_merge.F
      !||    hm_read_nonlocal              ../starter/source/materials/nonlocal/hm_read_nonlocal.F
      !||    hm_read_part                  ../starter/source/model/assembling/hm_read_part.F
      !||    hm_read_prop04                ../starter/source/properties/spring/hm_read_prop04.F
      !||    hm_read_prop09                ../starter/source/properties/shell/hm_read_prop09.F
      !||    hm_read_prop10                ../starter/source/properties/shell/hm_read_prop10.F
      !||    hm_read_prop11                ../starter/source/properties/shell/hm_read_prop11.F
      !||    hm_read_prop16                ../starter/source/properties/shell/hm_read_prop16.F
      !||    hm_read_prop17                ../starter/source/properties/shell/hm_read_prop17.F
      !||    hm_read_prop19                ../starter/source/properties/shell/hm_read_prop19.F
      !||    hm_read_prop26                ../starter/source/properties/spring/hm_read_prop26.F
      !||    hm_read_prop_generic          ../starter/source/properties/hm_read_prop_generic.F
      !||    hm_read_properties            ../starter/source/properties/hm_read_properties.F
      !||    hm_read_rbody                 ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    hm_read_sphcel                ../starter/source/elements/reader/hm_read_sphcel.F
      !||    hm_read_subset                ../starter/source/model/assembling/hm_read_subset.F
      !||    hm_read_therm_stress          ../starter/source/materials/therm/hm_read_therm_stress.F
      !||    hm_read_thgrki                ../starter/source/output/th/hm_read_thgrki.F
      !||    hm_read_thgrki_rbody          ../starter/source/output/th/hm_read_thgrki_rbody.F
      !||    hm_read_thgrne                ../starter/source/output/th/hm_read_thgrne.F
      !||    hm_read_thgrns                ../starter/source/output/th/hm_read_thgrns.F
      !||    hm_read_thgrsens              ../starter/source/output/th/hm_read_thgrsens.F
      !||    hm_read_thgrsurf              ../starter/source/output/th/hm_read_thgrsurf.F
      !||    hm_read_xref                  ../starter/source/loads/reference_state/xref/hm_read_xref.F
      !||    hm_thgrki_vent                ../starter/source/output/th/hm_thgrki_vent.F
      !||    ini_fxbody                    ../starter/source/constraints/fxbody/ini_fxbody.F
      !||    iniguser                      ../starter/source/system/iniguser.F
      !||    inintr                        ../starter/source/interfaces/interf1/inintr.F
      !||    inintr2                       ../starter/source/interfaces/inter3d1/inintr2.F
      !||    inintr_thkvar                 ../starter/source/interfaces/interf1/inintr_thkvar.F
      !||    inintsub                      ../starter/source/interfaces/interf1/inintsub.F
      !||    inintsub_11                   ../starter/source/output/subinterface/inintsub_11.F
      !||    inintsub_25                   ../starter/source/output/subinterface/inintsub_25.F
      !||    inintsub_7                    ../starter/source/output/subinterface/inintsub_7.F
      !||    inirbe2                       ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    inirbe3                       ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
      !||    initia                        ../starter/source/elements/initia/initia.F
      !||    inivoid                       ../starter/source/elements/initia/inivoid.F
      !||    inter_dcod_friction           ../starter/source/interfaces/reader/inter_dcod_friction.F
      !||    inter_dcod_function           ../starter/source/interfaces/reader/inter_dcod_function.F
      !||    inter_dcod_sensor             ../starter/source/interfaces/reader/inter_dcod_sensor.F
      !||    itagsl2                       ../starter/source/interfaces/inter3d1/itagsl2.F
      !||    kinchk                        ../starter/source/constraints/general/kinchk.F
      !||    lcgeo19                       ../starter/source/elements/shell/coque/lcgeo19.F
      !||    lecins                        ../starter/source/interfaces/interf1/lecins.F
      !||    lecint                        ../starter/source/interfaces/interf1/lecint.F
      !||    lecstack_ply                  ../starter/source/properties/composite_options/stack/lecstack_ply.F
      !||    lecstamp                      ../starter/source/interfaces/interf1/lecstamp.F
      !||    lgmini_bc                     ../starter/source/tools/lagmul/lgmini_bc.F
      !||    lgmini_fxv                    ../starter/source/tools/lagmul/lgmini_fxv.F
      !||    lgmini_gj                     ../starter/source/tools/lagmul/lgmini_gj.F
      !||    lgmini_i2                     ../starter/source/tools/lagmul/lgmini_i2.F
      !||    lgmini_i7                     ../starter/source/tools/lagmul/lgmini_i7.F
      !||    lgmini_mpc                    ../starter/source/tools/lagmul/lgmini_mpc.F
      !||    lgmini_rby                    ../starter/source/tools/lagmul/lgmini_rby.F
      !||    lgmini_rwl                    ../starter/source/tools/lagmul/lgmini_rwl.F
      !||    m20dcod                       ../starter/source/system/fsdcod.F
      !||    matini                        ../starter/source/materials/mat_share/matini.F
      !||    multifluid_init3              ../starter/source/multifluid/multifluid_init3.F
      !||    newdbl2                       ../starter/source/system/sysfus.F
      !||    outpart                       ../starter/source/elements/initia/initia.F
      !||    outpart5                      ../starter/source/elements/initia/initia.F
      !||    pgrtails                      ../starter/source/elements/beam/pgrtails.F
      !||    pornod                        ../starter/source/ale/pornod.F
      !||    q4init2                       ../starter/source/elements/solid_2d/quad4/q4init2.F
      !||    qgrhead                       ../starter/source/elements/solid_2d/quad/qgrhead.F
      !||    qgrtails                      ../starter/source/elements/solid_2d/quad/qgrtails.F
      !||    qinit2                        ../starter/source/elements/solid_2d/quad/qinit2.F
      !||    qmorth2                       ../starter/source/elements/solid_2d/quad/qmorth2.F
      !||    rcheckmass                    ../starter/source/elements/spring/rcheckmass.F
      !||    remn_i2op                     ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    remn_i2op_edg25               ../starter/source/interfaces/int25/i25remlin.F
      !||    remn_self24                   ../starter/source/interfaces/inter3d1/remn_self24.F
      !||    ri2_int24p_ini                ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    rigid_mat                     ../starter/source/materials/mat/mat019/rigid_mat.F
      !||    rini33_rb                     ../starter/source/elements/joint/rjoint/rini33_rb.F
      !||    rini45_rb                     ../starter/source/elements/joint/rjoint/rini45_rb.F
      !||    rinit3                        ../starter/source/elements/spring/rinit3.F
      !||    rkini3                        ../starter/source/elements/spring/rkini3.F
      !||    s10init3                      ../starter/source/elements/solid/solide10/s10init3.F
      !||    s16init3                      ../starter/source/elements/thickshell/solide16/s16init3.F
      !||    s20init3                      ../starter/source/elements/solid/solide20/s20init3.F
      !||    s4init3                       ../starter/source/elements/solid/solide4/s4init3.F
      !||    s4refsta3                     ../starter/source/elements/solid/solide4/s4refsta3.F
      !||    s6cinit3                      ../starter/source/elements/thickshell/solide6c/s6cinit3.F
      !||    s8cinit3                      ../starter/source/elements/thickshell/solide8c/s8cinit3.F
      !||    s8zinit3                      ../starter/source/elements/solid/solide8z/s8zinit3.F
      !||    scaleini                      ../starter/source/elements/initia/scaleini.F
      !||    scinit3                       ../starter/source/elements/thickshell/solidec/scinit3.F
      !||    scmorth3                      ../starter/source/elements/thickshell/solidec/scmorth3.F
      !||    sgrtails                      ../starter/source/elements/solid/solide/sgrtails.F
      !||    sinit3                        ../starter/source/elements/solid/solide/sinit3.F
      !||    smorth3                       ../starter/source/elements/solid/solide/smorth3.F
      !||    sms_init                      ../starter/source/ams/sms_init.F
      !||    spgrtails                     ../starter/source/elements/sph/spgrtails.F
      !||    sphdcod                       ../starter/source/elements/sph/sphdcod.F
      !||    st_qaprint_clusters           ../starter/source/output/qaprint/st_qaprint_clusters.F
      !||    st_qaprint_constraints        ../starter/source/output/qaprint/st_qaprint_constraints.F
      !||    st_qaprint_friction           ../starter/source/output/qaprint/st_qaprint_friction.F
      !||    st_qaprint_interfaces         ../starter/source/output/qaprint/st_qaprint_interfaces.F
      !||    st_qaprint_loads              ../starter/source/output/qaprint/st_qaprint_loads.F
      !||    st_qaprint_materials          ../starter/source/output/qaprint/st_qaprint_materials.F
      !||    st_qaprint_model_tools        ../starter/source/output/qaprint/st_qaprint_model_tools.F
      !||    st_qaprint_output_databases   ../starter/source/output/qaprint/st_qaprint_output_databases.F
      !||    st_qaprint_properties         ../starter/source/output/qaprint/st_qaprint_properties.F
      !||    st_qaprint_thgrou             ../starter/source/output/qaprint/st_qaprint_time_histories.F
      !||    suinit3                       ../starter/source/elements/elbuf_init/suinit3.F
      !||    t3grhead                      ../starter/source/elements/solid_2d/tria/t3grhead.F
      !||    t3grtails                     ../starter/source/elements/solid_2d/tria/t3grtails.F
      !||    thprin                        ../starter/source/output/th/thprin.F
      !||    tinit3                        ../starter/source/elements/truss/tinit3.F
      !||    updmat                        ../starter/source/materials/updmat.F
      !||    write_i_c_debug               ../starter/source/output/tools/write_debug.F
      !||    xinit3                        ../starter/source/elements/xelem/xinit3.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      SUBROUTINE FRETITL2(TITR,IASC,L)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y  A r g u m e  n t s
C-----------------------------------------------
      INTEGER L,IASC(*)
      CHARACTER TITR*(*)
C-----------------------------------------------
C   L o c a l    V a r i a b  l e s
C-----------------------------------------------
      INTEGER I,J
C-----------------------------------------------
      J=1
      DO I=1,L
        TITR(J:J)=CHAR(IASC(I)/65536)
        J=J+1
        IF(J>nchartitle) EXIT
        TITR(J:J)=CHAR(MOD(IASC(I),65536)/256)
        J=J+1
        IF(J>nchartitle) EXIT
        TITR(J:J)=CHAR(MOD(IASC(I),256))
        J=J+1
        IF(J>nchartitle) EXIT
      ENDDO
      RETURN
      END
      !||====================================================================
      !||    nextsla          ../starter/source/starter/freform.F
      !||--- called by ------------------------------------------------------
      !||    sz_r2r           ../starter/source/coupling/rad2rad/routines_r2r.F
      !||--- calls      -----------------------------------------------------
      !||    my_exit          ../starter/source/output/analyse/analyse.c
      !||--- uses       -----------------------------------------------------
      !||    reader_old_mod   ../starter/share/modules1/reader_old_mod.F90
      !||====================================================================
      SUBROUTINE NEXTSLA
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE READER_OLD_MOD , ONLY : LINE, KLINE, KEY0, KCUR, LKEY0, IREC
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
      IF (IREC<=0) THEN
        IREC=1
        READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
      ELSE
        READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
        DO WHILE(LINE(1:1)/='/')
          IREC=IREC+1
          READ(IIN,REC=IREC,ERR=999,FMT='(A)')LINE
        ENDDO
      END IF
      IF(LINE(2:1+LKEY0(KCUR))/=KEY0(KCUR)(1:LKEY0(KCUR)))GOTO 999
      KLINE=LINE
      RETURN
 999  CALL FREERR(1)
      CALL MY_EXIT(2)
      END
      
      !||====================================================================
      !||    fredec_2key_id_or_key_id   ../starter/source/starter/freform.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                     ../starter/source/output/message/message.F
      !||    my_exit                    ../starter/source/output/analyse/analyse.c
      !||--- uses       -----------------------------------------------------
      !||    format_mod                 ../starter/share/modules1/format_mod.F90
      !||    message_mod                ../starter/share/message_module/message_mod.F
      !||    reader_old_mod             ../starter/share/modules1/reader_old_mod.F90
      !||====================================================================
      SUBROUTINE FREDEC_2KEY_ID_OR_KEY_ID(KEY2,KEY3,UID,SUB_ID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY, NCHARFIELD, NCHARLINE
      USE FORMAT_MOD , ONLY : FMT_I
      USE READER_OLD_MOD , ONLY : KLINE
      USE USER_ID_MOD , ONLY : ID_LIMIT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
      INTEGER UID,SUB_ID
      CHARACTER(LEN=NCHARTITLE) :: TITR
      CHARACTER(LEN=NCHARKEY) :: KEY2,KEY3
C-----------------------------------------------
      INTEGER I,J1,J2,J3,J, JMAX
      CHARACTER(LEN=NCHARFIELD) :: MOT1
C-----------------------------------------------
C   /KEYW/KEY2/KEY3/u_id
C   /KEYW/KEY2/u_id
C   /KEYW/u_id
C-----------------------------------------------

      JMAX = ncharline

C     Pass KEY1
      UID=0
      SUB_ID = 0
      I=2
      DO WHILE(KLINE(I:I)/='/'.AND.I<ncharline)
        I=I+1
      ENDDO
      KEY2=' '
      KEY3=' '
      I=I+1
      IF(I>ncharline)RETURN
C     Read KEY2
      J1=I
      DO WHILE(KLINE(I:I)/='/'.AND.I<ncharline)
        I=I+1
      ENDDO
      IF (I>=ncharline) THEN
        MOT1=KLINE(J1:I)
          IF (KLINE(J1:MIN(JMAX,J1+2))/='AUX'     .AND.
     .        KLINE(J1:MIN(JMAX,J1+3))/='EPSP'    .AND.
     .        KLINE(J1:MIN(JMAX,J1+5))/='EPSP_F'  .AND.
     .        KLINE(J1:MIN(JMAX,J1+4))/='ORTHO'   .AND.
     .        KLINE(J1:MIN(JMAX,J1+5))/='STRA_F'  .AND.
     .        KLINE(J1:MIN(JMAX,J1+5))/='STRS_F'  .AND.
     .        KLINE(J1:MIN(JMAX,J1+4))/='THICK'   .AND.
     .        KLINE(J1:MIN(JMAX,J1+7))/='ORTH_LOC'.AND.
     .        KLINE(J1:MIN(JMAX,J1+5))/='STRESS'  .AND.  
     .        KLINE(J1:MIN(JMAX,J1+9))/='SCALE_YLD'.AND.
     .        KLINE(J1:MIN(JMAX,J1+4))/='FAIL'    .AND.  
     .        KLINE(J1:MIN(JMAX,J1+4))/='FILL'    .AND.
     .        KLINE(J1:MIN(JMAX,J1+4))/='FULL'    .AND.
     .        KLINE(J1:MIN(JMAX,J1+3))/='DENS'    .AND.
     .        KLINE(J1:MIN(JMAX,J1+3))/='EREF'    .AND.
     .        KLINE(J1:MIN(JMAX,J1+3))/='ENER'    ) THEN
           READ(MOT1,ERR=999,FMT=FMT_I)UID
           IF (UID > ID_LIMIT%GLOBAL .OR. UID < 0) THEN
             CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1=UID,C1=KLINE)
             RETURN
           ENDIF
           RETURN
          ENDIF  
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+2))=='AUX') THEN
        J2=J1+3
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+3))=='EPSP') THEN
        J2=J1+4
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+5))=='EPSP_F') THEN
        J2=J1+6
      ENDIF 
      IF (KLINE(J1:MIN(JMAX,J1+4))=='ORTHO') THEN
        J2=J1+5
      ENDIF   
      IF (KLINE(J1:MIN(JMAX,J1+8))=='STRA_FGLO') THEN
        J2=J1+6
      ELSEIF (KLINE(J1:MIN(JMAX,J1+5))=='STRA_F') THEN
        J2=J1+6
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+8))=='STRS_FGLO') THEN
        J2=J1+9
      ELSEIF (KLINE(J1:MIN(JMAX,J1+5))=='STRS_F') THEN
        J2=J1+6
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+4))=='THICK') THEN
        J2=J1+5
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+7))=='ORTH_LOC') THEN
        J2=J1+8
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+5))=='STRESS') THEN
        J2=J1+6
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+9))=='SCALE_YLD') THEN
        J2=J1+9
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+5))=='FAIL') THEN
        J2=J1+4
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+5))=='FILL') THEN
        J2=J1+4
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+5))=='FULL') THEN
        J2=J1+4
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+3))=='DENS') THEN
        J2=J1+4
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+3))=='ENER') THEN
        J2=J1+4
      ENDIF
      IF (KLINE(J1:MIN(JMAX,J1+3))=='EREF') THEN
        J2=J1+4
      ENDIF
      KEY2=KLINE(J1:MIN(JMAX,J2-1))
C     Read KEY3
      DO WHILE(KLINE(I:I)/='/'.AND.I<ncharline)
        I=I+1
      ENDDO
      J2 = I+1
      J2 = MIN(JMAX,J2)
      IF (I>=ncharline) THEN
        MOT1=KLINE(J2:I)
        IF (KLINE(J2:MIN(JMAX,J2+5))/='STRA_F'.AND.
     .      KLINE(J2:MIN(JMAX,J2+5))/='STRS_F') THEN
          READ(MOT1,ERR=999,FMT=FMT_I)UID
          IF (UID > ID_LIMIT%GLOBAL .OR. UID < 0) THEN
            CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=UID,C1=KLINE)
            RETURN
          ENDIF
          RETURN
        ENDIF
      ENDIF
      IF (KLINE(J2:MIN(JMAX,J2+3))=='GLOB') THEN
        J3=J2+3
        J3 = MIN(JMAX,J3)
        I = J3
        KEY3=KLINE(J2:J3)
        DO WHILE(KLINE(I:I)/='/' .AND. I<ncharline)
          I=I+1
        ENDDO
        I=I+1
        I=MIN(ncharline,I)
      ELSEIF (KLINE(J2:MIN(JMAX,J2+8))=='STRA_FGLO' .OR.
     .        KLINE(J2:MIN(JMAX,J2+8))=='STRS_FGLO'  ) THEN
        J3=J2+8
        J3 = MIN(JMAX,J3)
        I = J3
        KEY3=KLINE(J2:J3)
        DO WHILE(KLINE(I:I)/='/' .AND. I<ncharline)
          I=I+1
        ENDDO
        I=I+1
        I=MIN(JMAX,I)
      ELSEIF (KLINE(J2:MIN(JMAX,J2+5))=='STRA_F' .OR.
     .        KLINE(J2:MIN(JMAX,J2+5))=='STRS_F'  ) THEN
        J3=J2+5
        J3 = MIN(JMAX,J3)
        I = J3
        KEY3=KLINE(J2:J3)
        DO WHILE(KLINE(I:I)/='/' .AND. I<ncharline)
          I=I+1
        ENDDO
        I=I+1
        I = MIN(JMAX,I)
      ELSE
        J3=J2
        I=I+1
        I = MIN(JMAX,I)
        KEY3=' '
      ENDIF
C---  read UID
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I<ncharline)
        I=I+1
      ENDDO
      MOT1=KLINE(J1:I-1)
      READ(MOT1,ERR=999,FMT=FMT_I)UID
      IF (UID > ID_LIMIT%GLOBAL .OR. UID < 0) THEN
        CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              I1=UID,C1=KLINE)
      ENDIF
C---  read SUB_ID
      I=I+1
      I = MIN(JMAX,I)
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I<ncharline)
        I=I+1
      ENDDO
      MOT1=KLINE(J1:I-1)
      READ(MOT1,ERR=999,FMT=FMT_I)SUB_ID
C---
      RETURN
 999  CALL FREERR(0)
      CALL MY_EXIT(2)
      END   
      !||====================================================================
      !||    fredec_2key_4id_t      ../starter/source/starter/freform.F
      !||--- called by ------------------------------------------------------
      !||    nbadigemesh            ../starter/source/elements/ige3d/nbadigemesh.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||    freerr                 ../starter/source/starter/freform.F
      !||    my_exit                ../starter/source/output/analyse/analyse.c
      !||--- uses       -----------------------------------------------------
      !||    format_mod             ../starter/share/modules1/format_mod.F90
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||    reader_old_mod         ../starter/share/modules1/reader_old_mod.F90
      !||====================================================================
      SUBROUTINE FREDEC_2KEY_4ID_T(KEY2,ID,UID,VERS,SUB_ID,TITR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE READER_OLD_MOD , ONLY : KLINE, IREC
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY, NCHARFIELD
      USE FORMAT_MOD , ONLY : FMT_I
      USE USER_ID_MOD , ONLY : ID_LIMIT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
      INTEGER ID,UID,VERS,SUB_ID
      CHARACTER(LEN=NCHARTITLE) :: TITR
      CHARACTER(LEN=NCHARKEY) :: KEY2
C-----------------------------------------------
      INTEGER I,J1,J2,J
      CHARACTER(LEN=NCHARFIELD) :: MOT1
C-----------------------------------------------
C   /KEYW/KEY2/int_id/u_id/VERS
C + char_title
C-----------------------------------------------
C     Pass KEY1
      I=2
      DO WHILE(KLINE(I:I)/='/')
        I=I+1
        IF(I>ncharline)CALL FREERR(0)
      ENDDO
      I=I+1
C     Read KEY2
      J1=I
      DO WHILE(KLINE(I:I)/='/')
        I=I+1
        IF(I>ncharline)CALL FREERR(0)
      ENDDO
      J2=I-1
      KEY2=KLINE(J1:J2)
      I=I+1
C---  read ID
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      IF (I > ncharline) CALL FREERR(0)
      J2 = I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I)ID
      IF (ID > ID_LIMIT%GLOBAL .OR. ID <= 0) THEN
        CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1=ID,C1=KLINE)
      ENDIF
      I =I+1
C---  read UID
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      IF (I > ncharline) CALL FREERR(0)
      J2 = I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I)UID
      IF (UID > ID_LIMIT%GLOBAL .OR. UID < 0) THEN
        CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1=UID,C1=KLINE)
      ENDIF
      I =I+1
C---  read VERS
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      J2 = I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I)VERS
C---  read SUB_ID
      I =I+1
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      J2 = I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I)SUB_ID
C---  read TITR
      IREC=IREC+1                                
      READ(IIN,REC=IREC,ERR=999,FMT='(A)') TITR  
C---
      RETURN
 999  CALL FREERR(0)
      CALL MY_EXIT(2)
      END
      !||====================================================================
      !||    fredec_key_3id_t       ../starter/source/starter/freform.F
      !||--- called by ------------------------------------------------------
      !||    nbadigemesh            ../starter/source/elements/ige3d/nbadigemesh.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||    freerr                 ../starter/source/starter/freform.F
      !||    my_exit                ../starter/source/output/analyse/analyse.c
      !||--- uses       -----------------------------------------------------
      !||    format_mod             ../starter/share/modules1/format_mod.F90
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||    reader_old_mod         ../starter/share/modules1/reader_old_mod.F90
      !||====================================================================
      SUBROUTINE FREDEC_KEY_3ID_T(ID,UID,VERS,TITR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARFIELD
      USE FORMAT_MOD , ONLY : FMT_I
      USE READER_OLD_MOD , ONLY : KLINE, IREC
      USE USER_ID_MOD , ONLY : ID_LIMIT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
      INTEGER IOP,ID,UID,VERS
      CHARACTER(LEN=NCHARFIELD) :: MOT1
      CHARACTER(LEN=NCHARTITLE) :: TITR
      INTEGER I,J1,J2
C-----------------------------------------------
C   /KEYW/int_id/uid/VERS
C-----------------------------------------------
      I=2
      DO WHILE(KLINE(I:I)/='/')
        I=I+1
        IF (I > ncharline)CALL FREERR(0)
      ENDDO
      I=I+1
      IF (I > ncharline)CALL FREERR(0)
C---  read ID
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      IF (I > ncharline) CALL FREERR(0)
      J2 = I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I)ID
      IF (ID > ID_LIMIT%GLOBAL .OR. ID <= 0) THEN
        CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1=ID,C1=KLINE)
      ENDIF
      I =I+1
C---  read UID
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      IF (I > ncharline) CALL FREERR(0)
      J2 = I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I)UID
      IF (UID > ID_LIMIT%GLOBAL .OR. UID < 0) THEN
        CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,I1=UID,C1=KLINE)
      ENDIF
      I =I+1
C---  read VERS
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      J2 = I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I)VERS
C---  read TITR
      IREC=IREC+1                                
      READ(IIN,REC=IREC,ERR=999,FMT='(A)') TITR
C---
      RETURN
 999  CALL FREERR(0)
      CALL MY_EXIT(2)
      END
      !||====================================================================
      !||    fredec_2key_4id        ../starter/source/starter/freform.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||    freerr                 ../starter/source/starter/freform.F
      !||    my_exit                ../starter/source/output/analyse/analyse.c
      !||--- uses       -----------------------------------------------------
      !||    format_mod             ../starter/share/modules1/format_mod.F90
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||    reader_old_mod         ../starter/share/modules1/reader_old_mod.F90
      !||====================================================================
      SUBROUTINE FREDEC_2KEY_4ID(KEY2,ID,UID,VERS,SUB_ID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY, NCHARFIELD
      USE FORMAT_MOD , ONLY : FMT_I
      USE READER_OLD_MOD , ONLY : KLINE
      USE USER_ID_MOD , ONLY : ID_LIMIT
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
      INTEGER ID,UID,VERS,SUB_ID
      CHARACTER(LEN=NCHARTITLE) :: TITR
      CHARACTER(LEN=NCHARKEY) :: KEY2
C-----------------------------------------------
      INTEGER I,J1,J2,J
      CHARACTER(LEN=NCHARFIELD) :: MOT1
C-----------------------------------------------
C   /KEYW/KEY2/int_id/u_id/VERS
C-----------------------------------------------
C     Pass KEY1
      I=2
      DO WHILE(KLINE(I:I)/='/')
        I=I+1
        IF(I>ncharline)CALL FREERR(0)
      ENDDO
      I=I+1
C     Read KEY2
      J1=I
      DO WHILE(KLINE(I:I)/='/')
        I=I+1
        IF(I>ncharline)CALL FREERR(0)
      ENDDO
      J2=I-1
      KEY2=KLINE(J1:J2)
      I=I+1
C---  read ID
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      IF (I > ncharline) CALL FREERR(0)
      J2 = I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I)ID
      IF (ID > ID_LIMIT%GLOBAL .OR. ID <= 0) THEN
        CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              I1=ID,C1=KLINE)
      ENDIF
      I =I+1
C---  read UID
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      IF (I > ncharline) CALL FREERR(0)
      J2 = I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I)UID
      IF (UID > ID_LIMIT%GLOBAL .OR. UID < 0) THEN
        CALL ANCMSG(MSGID=510,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              I1=UID,C1=KLINE)
      ENDIF
C---  read VERS
      I=I+1
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      J2=I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I) VERS
C---  read SUB_ID
      I=I+1
      J1=I
      DO WHILE(KLINE(I:I)/='/' .AND. I-J1 <= ncharfield)
        I=I+1
      ENDDO
      J2=I-1
      MOT1=KLINE(J1:J2)
      READ(MOT1,ERR=999,FMT=FMT_I) SUB_ID
C---
      RETURN
 999  CALL FREERR(0)
      CALL MY_EXIT(2)
      END
