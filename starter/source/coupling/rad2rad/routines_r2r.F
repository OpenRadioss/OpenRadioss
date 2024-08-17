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
      !||    modif_tag               ../starter/source/coupling/rad2rad/routines_r2r.F
      !||--- called by ------------------------------------------------------
      !||    r2r_prelec              ../starter/source/coupling/rad2rad/r2r_prelec.F
      !||    tag_elem_void_r2r       ../starter/source/coupling/rad2rad/tagelem_r2r.F
      !||    tag_elem_void_r2r_lin   ../starter/source/coupling/rad2rad/tagelem_r2r.F
      !||====================================================================
      SUBROUTINE MODIF_TAG(TAG,NEW_TAG,MODIF)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER TAG,NEW_TAG,MODIF
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER OLD_TAG
C=======================================================================

        OLD_TAG = TAG
        TAG = NEW_TAG

        IF (OLD_TAG/=NEW_TAG) MODIF = MODIF+1

C-----------
        RETURN
      END SUBROUTINE MODIF_TAG

      !||====================================================================
      !||    r2r_sys          ../starter/source/coupling/rad2rad/routines_r2r.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_eref     ../starter/source/loads/reference_state/eref/hm_read_eref.F
      !||    hm_read_thgrne   ../starter/source/output/th/hm_read_thgrne.F
      !||    hm_read_xref     ../starter/source/loads/reference_state/xref/hm_read_xref.F
      !||    lecrefsta        ../starter/source/loads/reference_state/refsta/lecrefsta.F
      !||    usr2sys          ../starter/source/system/sysfus.F
      !||    usr2sys2         ../starter/source/system/sysfus.F
      !||--- calls      -----------------------------------------------------
      !||    r2r_sys2         ../starter/source/coupling/rad2rad/routines_r2r.F
      !||====================================================================
      INTEGER FUNCTION R2R_SYS(IU,ITABM1,MESS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IU
        CHARACTER MESS*40
        INTEGER ITABM1(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "r2r_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER JINF, JSUP, J,SAUV,NN
        INTEGER, DIMENSION(:), POINTER :: ITABM2
        TARGET :: ITABM1
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
        INTEGER R2R_SYS2
C-----------------------------------------------

        JINF=1
        JSUP=NUMNOD
        J=MAX(1,NUMNOD/2)

   10   IF(JSUP<=JINF.AND.(IU-ITABM1(J))/=0) THEN
          R2R_SYS=0
C------------Check of the list of removed nodes-------------
          ITABM2  => ITABM1(2*NUMNOD+1:2*(NUMNOD+NODSUPR))
          SAUV = NUMNOD
          NUMNOD = NODSUPR
          NN=R2R_SYS2(IU,ITABM2,MESS)
          IF (NN==0) R2R_SYS=-1
          NUMNOD = SAUV
C-----------------------------------------------------------
          RETURN
        ENDIF

        IF((IU-ITABM1(J))==0)THEN
C     >IU=TABM - end of the search
          R2R_SYS=ITABM1(J+NUMNOD)
          RETURN
        ELSE IF (IU-ITABM1(J)<0) THEN
C     >IU<TABM
          JSUP=J-1
        ELSE
C     >IU>TABM
          JINF=J+1
        ENDIF
        J=(JSUP+JINF)/2
        IF (J > 0) THEN
          GO TO 10
        ELSE
          R2R_SYS=0
        ENDIF
C
      end function R2R_SYS

      !||====================================================================
      !||    r2r_nin          ../starter/source/coupling/rad2rad/routines_r2r.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_thgrne   ../starter/source/output/th/hm_read_thgrne.F
      !||====================================================================
      INTEGER FUNCTION R2R_NIN(IEXT,NTN,M,N)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IEXT, M, N
        INTEGER NTN(M,N)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I
C-----------------------------------------------
        DO I=1,N
          IF(NTN(M,I)==IEXT)THEN
            R2R_NIN=I
            RETURN
          ENDIF
        ENDDO
        R2R_NIN=0
C-------------------------------------------
        RETURN
      end function R2R_NIN

      !||====================================================================
      !||    nodgr_r2r       ../starter/source/coupling/rad2rad/routines_r2r.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_cload   ../starter/source/loads/general/cload/hm_read_cload.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg          ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod     ../starter/share/message_module/message_mod.F
      !||    r2r_mod         ../starter/share/modules1/r2r_mod.F
      !||====================================================================
      INTEGER FUNCTION NODGR_R2R(IGU,IGS,IBUF,IGRNOD,
     .              ITABM1 ,MESS   )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE GROUPDEF_MOD
        USE MESSAGE_MOD
        USE R2R_MOD
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
        INTEGER I,NNCPL,COMPT
C=======================================================================
        NODGR_R2R = 0
        IF (IGU > 0) THEN
          IGS=0
          DO I=1,NGRNOD
            IF(IGRNOD(I)%ID == IGU) THEN
              IGS=I
              NODGR_R2R = IGRNOD(IGS)%NENTITY
              EXIT
            ENDIF
          ENDDO
C
          IF (IGS == 0)THEN
            CALL ANCMSG(MSGID=53,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  C1= MESS,
     .                  I1=IGU)
            RETURN
          ENDIF
C
          COMPT = 0
          DO I=1,NODGR_R2R
            IF (TAGNO(IGRNOD(IGS)%ENTITY(I)+NPART)/=2) THEN
              COMPT = COMPT + 1
              IBUF(COMPT)=IGRNOD(IGS)%ENTITY(I)
            ENDIF
          ENDDO
!
          NODGR_R2R = NODGR_R2R - IGRNOD(IGS)%R2R_SHARE
        ENDIF
C---
        RETURN
      end function NODGR_R2R

      !||====================================================================
      !||    sz_r2r             ../starter/source/coupling/rad2rad/routines_r2r.F
      !||--- called by ------------------------------------------------------
      !||    hm_pre_read_link   ../starter/source/constraints/rigidlink/hm_pre_read_rlink.F
      !||    hm_read_gauge      ../starter/source/output/gauge/hm_read_gauge.F
      !||    hm_read_link       ../starter/source/constraints/rigidlink/hm_read_rlink.F
      !||--- calls      -----------------------------------------------------
      !||    nextsla            ../starter/source/starter/freform.F
      !||--- uses       -----------------------------------------------------
      !||    reader_old_mod     ../starter/share/modules1/reader_old_mod.F90
      !||====================================================================
      SUBROUTINE SZ_R2R(TAG,VAL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE READER_OLD_MOD , ONLY : IREC, NSLASH
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER VAL,TAG(*)
C-----------------------------------------------

        CALL NEXTSLA
        DO WHILE (TAG(VAL) == 0)
          VAL=VAL+1
          IREC=IREC+1
          CALL NEXTSLA
        END DO

        RETURN
      END SUBROUTINE SZ_R2R

      !||====================================================================
      !||    hm_sz_r2r              ../starter/source/coupling/rad2rad/routines_r2r.F
      !||--- called by ------------------------------------------------------
      !||    hm_prelecjoi           ../starter/source/constraints/general/cyl_joint/hm_prelecjoi.F
      !||    hm_preread_rbe2        ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    hm_preread_rbe3        ../starter/source/constraints/general/rbe3/hm_preread_rbe3.F
      !||    hm_preread_rbody       ../starter/source/constraints/general/rbody/hm_preread_rbody.F
      !||    hm_read_cyljoint       ../starter/source/constraints/general/cyl_joint/hm_read_cyljoint.F
      !||    hm_read_gjoint         ../starter/source/constraints/general/gjoint/hm_read_gjoint.F
      !||    hm_read_inivol         ../starter/source/initial_conditions/inivol/hm_read_inivol.F
      !||    hm_read_interfaces     ../starter/source/interfaces/reader/hm_read_interfaces.F
      !||    hm_read_intsub         ../starter/source/output/subinterface/hm_read_intsub.F
      !||    hm_read_mpc            ../starter/source/constraints/general/mpc/hm_read_mpc.F
      !||    hm_read_rbe2           ../starter/source/constraints/general/rbe2/hm_read_rbe2.F
      !||    hm_read_rbe3           ../starter/source/constraints/general/rbe3/hm_read_rbe3.F
      !||    hm_read_rbody          ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    hm_read_rbody_lagmul   ../starter/source/constraints/general/rbody/hm_read_rbody_lagmul.F
      !||    hm_read_spcnd          ../starter/source/constraints/sph/hm_read_spcnd.F
      !||    lecsec42               ../starter/source/tools/sect/hm_read_sect.F
      !||    prelecsec              ../starter/source/tools/sect/prelecsec.F
      !||    preread_rbody_lagmul   ../starter/source/constraints/general/rbody/preread_rbody_lagmul.F
      !||    read_monvol            ../starter/source/airbag/read_monvol.F
      !||    setrbyon               ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||--- calls      -----------------------------------------------------
      !||    hm_option_read_key     ../starter/source/devtools/hm_reader/hm_option_read_key.F
      !||--- uses       -----------------------------------------------------
      !||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
      !||    submodel_mod           ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE HM_SZ_R2R(TAG,VAL,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE SUBMODEL_MOD
        USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER VAL,TAG(*)
        TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C
        DO WHILE (TAG(VAL) == 0)
          CALL HM_OPTION_READ_KEY(LSUBMODEL)
          VAL=VAL+1
        END DO
C
        RETURN
      END SUBROUTINE HM_SZ_R2R

      !||====================================================================
      !||    r2r_exist              ../starter/source/coupling/rad2rad/routines_r2r.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_thgrki         ../starter/source/output/th/hm_read_thgrki.F
      !||    hm_read_thgrki_rbody   ../starter/source/output/th/hm_read_thgrki_rbody.F
      !||    hm_read_thgrpa         ../starter/source/output/th/hm_read_thgrpa.F
      !||    hm_read_thgrpa_sub     ../starter/source/output/th/hm_read_thgrpa.F
      !||    hm_read_thgrsens       ../starter/source/output/th/hm_read_thgrsens.F
      !||    hm_read_thgrsurf       ../starter/source/output/th/hm_read_thgrsurf.F
      !||    hm_thgrki_vent         ../starter/source/output/th/hm_thgrki_vent.F
      !||    r2r_listcnt            ../starter/source/coupling/rad2rad/routines_r2r.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    group_mod              ../starter/share/modules1/group_mod.F
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||    r2r_mod                ../starter/share/modules1/r2r_mod.F
      !||    reader_old_mod         ../starter/share/modules1/reader_old_mod.F90
      !||    restmod                ../starter/share/modules1/restart_mod.F
      !||====================================================================
      INTEGER FUNCTION R2R_EXIST(TYP,ID)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE R2R_MOD
        USE RESTMOD
        USE MESSAGE_MOD
        USE GROUPDEF_MOD
        USE GROUP_MOD
        USE READER_OLD_MOD , ONLY : KINTER, NSLASH
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
        INTEGER ID,TYP
        INTEGER I,CURS
C--------------------------------------------------------
C------ --> TH : check if corresponding option is kept---
C--------------------------------------------------------

        R2R_EXIST=0
        CURS = 0

        IF (TYP==107) THEN
C-----------MONVOL------------------
          DO I=1,NVOLU
            CURS=CURS+1
            DO WHILE (TAGMON(CURS)==0)
              CURS=CURS+1
            END DO
            IF (TAGMON(CURS)==ID) R2R_EXIST=1
          END DO
        ELSEIF (TYP==101) THEN
C-----------INTER------------------
          DO I=1,HM_NINTER+NSLASH(KINTER)
            CURS=CURS+1
            DO WHILE (TAGINT(CURS)==0)
              CURS=CURS+1
            END DO
            IF (TAGINT(CURS)==ID) R2R_EXIST=1
          END DO
        ELSEIF (TYP==103) THEN
C-----------RBY------------------
          DO I=1,NRBODY
            CURS=CURS+1
            DO WHILE (TAGRBY(CURS)==0)
              CURS=CURS+1
            END DO
            IF (TAGRBY(CURS)==ID) R2R_EXIST=1
          END DO
        ELSEIF (TYP==105) THEN
C-----------CYL_JOIN--------------
          DO I=1,NJOINT
            CURS=CURS+1
            DO WHILE (TAGCYL(CURS)==0)
              CURS=CURS+1
            END DO
            IF (TAGCYL(CURS)==ID) R2R_EXIST=1
          END DO
        ELSEIF (TYP==1001) THEN
C-----------PART------------------
          DO I=1,NPART
            IF (IPART(LIPART1*(I-1)+4)==ID) CURS = I
          END DO
          IF (CURS == 0) THEN
            CALL ANCMSG(MSGID=258,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  C1="PART",
     .                  I1=ID)
          ENDIF
          IF (TAG_PART(CURS)>0) R2R_EXIST=1
        ELSEIF (TYP==1002) THEN
C-----------SUBSET------------------
          DO I=1,NSUBS
            IF (SUBSETS(I)%ID==ID) CURS = I
          END DO
          IF (CURS == 0) THEN
            CALL ANCMSG(MSGID=258,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO_BLIND_1,
     .                  C1="SUBSET",
     .                  I1=ID)
          ENDIF
          R2R_EXIST=1
        ELSEIF (TYP==102) THEN
C-----------RWALL-------------------
          R2R_EXIST=1
        ELSEIF (TYP==104) THEN
C-----------SECTION-----------------
          DO I=1,NSECT
            CURS=CURS+1
            DO WHILE (TAGSEC(CURS)==0)
              CURS=CURS+1
            END DO
            IF (TAGSEC(CURS)==ID) R2R_EXIST=1
          END DO
        ELSEIF (TYP==108) THEN
C-----------ACCELEROMETER-----------
          R2R_EXIST=1
        ELSEIF (TYP==110) THEN
C-----------FRAMES------------------
          R2R_EXIST=1
        ELSEIF (TYP==113) THEN
C-----------GAUGES------------------
          DO I=1,NBGAUGE
            CURS=CURS+1
            DO WHILE (TAGGAU(CURS)==0)
              CURS=CURS+1
            END DO
            IF (TAGGAU(CURS)==ID) R2R_EXIST=1
          END DO
        ENDIF

        RETURN
      end function R2R_EXIST

      !||====================================================================
      !||    r2r_listcnt        ../starter/source/coupling/rad2rad/routines_r2r.F
      !||--- called by ------------------------------------------------------
      !||    hm_read_thgrsurf   ../starter/source/output/th/hm_read_thgrsurf.F
      !||    hm_thgrki_vent     ../starter/source/output/th/hm_thgrki_vent.F
      !||--- calls      -----------------------------------------------------
      !||    my_exit            ../starter/source/output/analyse/analyse.c
      !||    r2r_exist          ../starter/source/coupling/rad2rad/routines_r2r.F
      !||--- uses       -----------------------------------------------------
      !||    format_mod         ../starter/share/modules1/format_mod.F90
      !||    r2r_mod            ../starter/share/modules1/r2r_mod.F
      !||    reader_old_mod     ../starter/share/modules1/reader_old_mod.F90
      !||====================================================================
      INTEGER FUNCTION R2R_LISTCNT(NVAR,TYP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE R2R_MOD
        USE FORMAT_MOD , ONLY : FMT_10I
        USE READER_OLD_MOD , ONLY : LINE, IREC
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
        INTEGER NVAR,TYP
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
        INTEGER R2R_EXIST
C-----------------------------------------------
        INTEGER I,JREC,J10(10),NVAR_TMP
C-----------------------------------------------------------
C------ --> TH : re-count of nb of entities in TH groups----
C-----------------------------------------------------------

        R2R_LISTCNT=0
        NVAR=0
        JREC=IREC
        JREC=JREC+1
        READ(IIN,REC=JREC,ERR=999,FMT='(A)')LINE
        DO WHILE(LINE(1:1)/='/')
          NVAR_TMP = NVAR
          READ(LINE,ERR=999,FMT=FMT_10I) J10
          DO I=1,10
            IF(J10(I)/=0) THEN
C-----------entity is counted if it is kept-------------------
              IF (R2R_EXIST(TYP,J10(I))==1) NVAR=NVAR+1
C-------------------------------------------------------------
            ENDIF
          ENDDO
          R2R_LISTCNT=R2R_LISTCNT+1
          JREC=JREC+1
          READ(IIN,REC=JREC,ERR=999,FMT='(A)')LINE
        ENDDO
        RETURN
 999    CALL FREERR(1)
        CALL MY_EXIT(2)
      end function R2R_LISTCNT

C
      !||====================================================================
      !||    grsize_r2r       ../starter/source/coupling/rad2rad/routines_r2r.F
      !||--- called by ------------------------------------------------------
      !||    prelecsec        ../starter/source/tools/sect/prelecsec.F
      !||    prelecsec4bolt   ../starter/source/tools/sect/prelecsec4bolt.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      INTEGER FUNCTION GRSIZE_R2R(IGU,IGRELEM,GRLEN,TYP)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE GROUPDEF_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IGU,GRLEN,TYP
C-----------------------------------------------
        TYPE (GROUP_)  , DIMENSION(GRLEN)  :: IGRELEM
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,IGS
C-----------------------------------------------
        GRSIZE_R2R = 0
        IF (IGU > 0) THEN
          DO I=1,GRLEN
            IF (IGU == IGRELEM(I)%ID) THEN
              IF (TYP == 8) THEN ! before split
                GRSIZE_R2R = IGRELEM(I)%R2R_ALL
              ELSEIF (TYP == 9) THEN ! shared
                GRSIZE_R2R = IGRELEM(I)%R2R_SHARE
              ENDIF
              IGS = I
              EXIT
            ENDIF
          ENDDO
        ENDIF
C-----------
        RETURN
      end function GRSIZE_R2R

      !||====================================================================
      !||    r2r_sys2      ../starter/source/coupling/rad2rad/routines_r2r.F
      !||--- called by ------------------------------------------------------
      !||    r2r_sys       ../starter/source/coupling/rad2rad/routines_r2r.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg        ../starter/source/output/message/message.F
      !||--- uses       -----------------------------------------------------
      !||    message_mod   ../starter/share/message_module/message_mod.F
      !||====================================================================
      INTEGER FUNCTION R2R_SYS2(IU,ITABM1,MESS)
        USE MESSAGE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IU
        CHARACTER MESS*40
        INTEGER ITABM1(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER JINF, JSUP, J
C-----------------------------------------------
C-- Same routine as USR2SYS -> used to avoid infinite loop in R2R_SYS

        JINF=1
        JSUP=NUMNOD
        J=MAX(1,NUMNOD/2)
   10   IF(JSUP<=JINF.AND.(IU-ITABM1(J))/=0) THEN
          CALL ANCMSG(MSGID=78,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                C1=MESS,
     .                I1=IU)
          R2R_SYS2=0
          RETURN
        ENDIF
        IF((IU-ITABM1(J))==0)THEN
C     >IU=TABM - end of search
          R2R_SYS2=ITABM1(J+NUMNOD)
          RETURN
        ELSE IF (IU-ITABM1(J)<0) THEN
C     >IU<TABM
          JSUP=J-1
        ELSE
C     >IU>TABM
          JINF=J+1
        ENDIF
        J=(JSUP+JINF)/2
        IF (J > 0) THEN
          GO TO 10
        ELSE
          R2R_SYS2=0
        ENDIF
      end function R2R_SYS2

      !||====================================================================
      !||    r2r_nom_opt    ../starter/source/coupling/rad2rad/routines_r2r.F
      !||--- called by ------------------------------------------------------
      !||    lectur         ../starter/source/starter/lectur.F
      !||--- uses       -----------------------------------------------------
      !||    r2r_mod        ../starter/share/modules1/r2r_mod.F
      !||    submodel_mod   ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE R2R_NOM_OPT(NOM_OPT,INOM_OPT,IN10,IN20,SNOM_OPT_OLD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE R2R_MOD
      USE SUBMODEL_MOD , ONLY : NSUBMOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER NOM_OPT(*),INOM_OPT(*),IN10,IN20,SNOM_OPT_OLD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J
C=======================================================================
C-- Split of NOM_OPT

        ALLOCATE (NOM_OPT_TEMP(SNOM_OPT_OLD))
        DO I=1,SNOM_OPT_OLD
          NOM_OPT_TEMP(I) = NOM_OPT(I)
          NOM_OPT(I) = 0
        ENDDO

C---  FUNCTIONS / TABLES --
        DO I=1,LNOPT1*NFUNCT
          NOM_OPT(LNOPT1*INOM_OPT(20)+I)=NOM_OPT_TEMP(LNOPT1*IN20+I)
        END DO
C---  FRAMES --
        DO I=1,LNOPT1*(NUMSKW+1+NUMFRAM+1+NSUBMOD)
          NOM_OPT(LNOPT1*INOM_OPT(10)+I)=NOM_OPT_TEMP(LNOPT1*IN10+I)
        END DO

        DEALLOCATE (NOM_OPT_TEMP)

C-----------
        RETURN
      END SUBROUTINE R2R_NOM_OPT

      !||====================================================================
      !||    chk_flg_fsi   ../starter/source/coupling/rad2rad/routines_r2r.F
      !||--- called by ------------------------------------------------------
      !||    r2r_group     ../starter/source/coupling/rad2rad/r2r_group.F
      !||--- uses       -----------------------------------------------------
      !||    r2r_mod       ../starter/share/modules1/r2r_mod.F
      !||====================================================================
      SUBROUTINE CHK_FLG_FSI(IXS,PM,IPARTS,ALE_EULER,IGEO)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE R2R_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
#include      "r2r_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER IXS(NIXS,SIXS/NIXS),IPARTS(*),ALE_EULER
        INTEGER,INTENT(IN) :: IGEO(NPROPGI,NUMGEO)
        my_real PM(NPROPM,NUMMAT)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER M,JALE,ID_PART,IMAT0,IPROP0,ELEM_VOID,JALE_FROM_MAT, JALE_FROM_PROP
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
        FLG_FSI = 0
        ALE_EULER = 0
        DO M=1,NUMELS
          ID_PART=IPARTS(M)
C---------------id of the original material -----------C
          IMAT0=IPART_R2R(1,ID_PART)
          JALE=NINT(PM(72,IMAT0))
C
          ELEM_VOID = 0
          IF ((TAGNO(ID_PART)==0).AND.(TAG_ELS(M)>0)) ELEM_VOID=1
          IF ((JALE > 0).AND.(TAGNO(ID_PART) > 0)) ALE_EULER = 1
          IF ((JALE == 0).OR.(ELEM_VOID == 0)) CYCLE
          FLG_FSI = 1
        END DO
C-------------------------------------------
        RETURN
      END SUBROUTINE CHK_FLG_FSI

      !||====================================================================
      !||    r2r_check_seg     ../starter/source/coupling/rad2rad/routines_r2r.F
      !||--- called by ------------------------------------------------------
      !||    r2r_clean_inter   ../starter/source/coupling/rad2rad/r2r_clean_inter.F
      !||--- uses       -----------------------------------------------------
      !||    nod2el_mod        ../starter/share/modules1/nod2el_mod.F
      !||    r2r_mod           ../starter/share/modules1/r2r_mod.F
      !||    restmod           ../starter/share/modules1/restart_mod.F
      !||====================================================================
      SUBROUTINE R2R_CHECK_SEG(ELTAG,FACE,IPARTC,IPARTG,IPARTS,ISOLNOD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE RESTMOD
        USE NOD2EL_MOD
        USE R2R_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
        INTEGER ELTAG,FACE(4),IPARTC(*),IPARTG(*),IPARTS(*),ISOLNOD(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER CUR_ID,CUR_10,CUR_20,CUR_16,FLG_T4,L,K
        INTEGER ITAGL(NUMNOD),NF,SUM,OFFSET
C-----------------------------------------------

        NF = FACE(1)
        ELTAG = 0

C-->  check of shell elements <---
        DO L = KNOD2ELC(NF)+1,KNOD2ELC(NF+1)
          CUR_ID = NOD2ELC(L)
          FLG_T4 = 0
          DO K = 1,4
            ITAGL(FACE(K)) = 0
          END DO
          DO K = 2,5
            ITAGL(IXC(NIXC*(CUR_ID-1)+K)) = 1
            IF (TAGNO(NPART+IXC(NIXC*(CUR_ID-1)+K))==2) FLG_T4 = 1
          END DO
          SUM=ITAGL(FACE(1))+ITAGL(FACE(2))+ITAGL(FACE(3))+ITAGL(FACE(4))
          IF ((SUM==4).AND.((TAGNO(IPARTC(CUR_ID))==1).OR.(FLG_T4==0))) ELTAG = 1
        END DO

C-->  check of sh3n elements <---
        DO L = KNOD2ELTG(NF)+1,KNOD2ELTG(NF+1)
          CUR_ID = NOD2ELTG(L)
          FLG_T4 = 0
          DO K = 1,4
            ITAGL(FACE(K)) = 0
          END DO
          DO K = 2,4
            ITAGL(IXTG(NIXTG*(CUR_ID-1)+K)) = 1
            IF (TAGNO(NPART+IXTG(NIXTG*(CUR_ID-1)+K))==2) FLG_T4 = 1
          END DO
          SUM=ITAGL(FACE(1))+ITAGL(FACE(2))+ITAGL(FACE(3))+ITAGL(FACE(4))
          IF (SUM==4) ELTAG = 1
          IF ((SUM==4).AND.((TAGNO(IPARTG(CUR_ID))==1).OR.(FLG_T4==0))) ELTAG = 1
        END DO

C-->  check of solid elements  <---
        DO L = KNOD2ELS(NF)+1,KNOD2ELS(NF+1)
          CUR_ID = NOD2ELS(L)
          FLG_T4 = 0
          DO K = 1,4
            ITAGL(FACE(K)) = 0
          END DO
          DO K = 2,9
            ITAGL(IXS(NIXS*(CUR_ID-1)+K)) = 1
            IF (TAGNO(NPART+IXS(NIXS*(CUR_ID-1)+K))==2) FLG_T4 = 1
          END DO
          IF (ISOLNOD(CUR_ID)==10) THEN
            OFFSET = NIXS*NUMELS
            CUR_10 = CUR_ID-NUMELS8
            DO K=1,6
              ITAGL(IXS(OFFSET+6*(CUR_10-1)+K)) = 1
              IF (TAGNO(NPART+IXS(OFFSET+6*(CUR_10-1)+K))==2) FLG_T4 = 1
            ENDDO
          ELSEIF (ISOLNOD(CUR_ID)==20) THEN
            OFFSET = NIXS*NUMELS+6*NUMELS10
            CUR_20 = CUR_ID-(NUMELS8+NUMELS10)
            DO K=1,12
              ITAGL(IXS(OFFSET+12*(CUR_20-1)+K)) = 1
              IF (TAGNO(NPART+IXS(OFFSET+12*(CUR_20-1)+K))==2) FLG_T4 = 1
            ENDDO
          ELSEIF (ISOLNOD(CUR_ID)==16) THEN
            OFFSET = NIXS*NUMELS+6*NUMELS10+12*NUMELS20
            CUR_16 = CUR_ID-(NUMELS8+NUMELS10+NUMELS20)
            DO K=1,8
              ITAGL(IXS(OFFSET+8*(CUR_16-1)+K)) = 1
              IF (TAGNO(NPART+IXS(OFFSET+8*(CUR_16-1)+K))==2) FLG_T4 = 1
            ENDDO
          ENDIF
          SUM=ITAGL(FACE(1))+ITAGL(FACE(2))+ITAGL(FACE(3))+ITAGL(FACE(4))
          IF (SUM==4) ELTAG = 1
          IF ((SUM==4).AND.((TAGNO(IPARTS(CUR_ID))==1).OR.(FLG_T4==0))) ELTAG = 1
        END DO

C-----------
        RETURN
      END SUBROUTINE R2R_CHECK_SEG
