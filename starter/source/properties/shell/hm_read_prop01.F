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
Chd|  HM_READ_PROP01                source/properties/shell/hm_read_prop01.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        MULTI_FVM_MOD                 ../common_source/modules/multi_fvm_mod.F
Chd|        PINCHTYPE_MOD                 ../common_source/modules/pinchtype_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP01(GEO       ,IGEO     ,PROP_TAG ,MULTI_FVM,IGTYP    ,
     .                          IG        ,IDTITL   ,
     .                          UNITAB    ,LSUBMODEL)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD
      USE MESSAGE_MOD
      USE MULTI_FVM_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE PINCHTYPE_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "scr03_c.inc"
#include      "scr05_c.inc"
#include      "scr16_c.inc"
#include      "scr17_c.inc"
#include      "scr21_c.inc"
#include      "tablen_c.inc"
#include      "fluid_scr_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      INTEGER IGEO(*)
      INTEGER IGTYP,IG,UID,SUB_ID
      my_real
     .   GEO(*)
      CHARACTER IDTITL*nchartitle
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(MULTI_FVM_STRUCT) :: MULTI_FVM
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,  ISMSTR, NIP, J,
     .   ISHEAR, IP, ISTRAIN,
     .   IHBE,IPLAST,ITHK,IBID,IHBEOUTP,K,N,
     .   NSHELL, NSHSUP, NSHINF,
     .   NSST_D, NSST_DS, NPSH,ISEN,ISORTH,
     .   IHBE_OLD,  ISH3N,ISROT,IPINCH,I1,I2

      my_real
     .    PUN,CVIS,DN_P,PTHK
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
      DATA NSHELL /0/, NSHSUP /0/, NSHINF /0/
C-----------------------------------------------
      DATA PUN/0.1/
C=======================================================================
        IS_ENCRYPTED = .FALSE.
        IS_AVAILABLE = .FALSE.

        IHBE=0
        ISMSTR=0
        ISROT=0
        IPINCH=0
        ISORTH=0
        ISEN=0
        NPSH=0
        NSST_D = 0
        NSST_DS= 0
        PTHK = ZERO
C
C       Double stockage temporaire - supprimer GEO(12)=IGTYP apres tests
        IGEO( 1)=IG
        IGEO(11)=IGTYP
        GEO(12) =IGTYP+PUN
C----------------------
C HIDDEN FLAGS
C----------------------
C ISHEAR NEVER USED
        ISHEAR = 0
C CVIS USED in coquez routines
        CVIS = ZERO
C ISEN USED IN ENGINE call csens3.F routine
        ISEN = 0
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('Ishell',IHBE,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ismstr',ISMSTR,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ish3',ISH3N,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Idrill',ISROT,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ipinch',IPINCH,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('NIP',NIP,IS_AVAILABLE,LSUBMODEL)
C      CALL HM_GET_INTV('ISTRAIN',ISTRAIN,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ITHICK',ITHK,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('IPLAS',IPLAST,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('P_Thick_Fail',PTHK,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Hm',GEO(13),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Hf',GEO(14),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Hr',GEO(15),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Dm',GEO(16),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Dn',GEO(17),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('THICK',GEO(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('AREA_SHEAR',GEO(38),IS_AVAILABLE,LSUBMODEL,UNITAB)
C----------------------
C For example : apply submodel offsets units submodel transform to V (VX,VY,VZ) if needed
C
c      IF (SUB_INDEX /= 0)
c     .         CALL SUBROTVECT(VX,VY,VZ,RTRANS,SUB_INDEX,LSUBMODEL)
C----------------------
C
        PTHK = MIN(PTHK, ONE-EM06)
        GEO(42) = PTHK
C----------------------
C fixing flag and removed from input
        ISTRAIN=1
        IF(IHBE==0)IHBE=IHBE_D
        IHBEOUTP=IHBE
        IF (IHBE == 4 .AND. ISH3N==0 .AND. ISH3N_D == 1) THEN
          CALL ANCMSG(MSGID=680,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                 I1=IG,
     .                 C1=IDTITL)
        ENDIF
        IF (IHBE==22.OR.IHBE==23) THEN
          CALL ANCMSG(MSGID=539,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=IG,
     .                C1=IDTITL)
          IHBE=24
        ENDIF
        IF(ISH3N==0) ISH3N = ISH3N_D
        IGEO(18) = ISH3N
        IF (GEO(16) == ZERO) IGEO(31) = 1

        IF (IHBE==24) THEN
          IF (CVIS==ZERO) CVIS=ONE
          IF (GEO(17)==ZERO) GEO(17)=ZEP015
        ENDIF
C---
        IF(ISMSTR==0)ISMSTR=ISST_D
        IF (ISST_D == -2) ISMSTR = -1
        IF(IHBE==3)THEN
          IF(GEO(13)==ZERO)GEO(13)=EM01
          IF(GEO(14)==ZERO)GEO(14)=EM01
          IF(GEO(15)==ZERO)GEO(15)=EM02
        ELSE
          IF(GEO(13)==ZERO)GEO(13)=EM02
          IF(GEO(14)==ZERO)GEO(14)=EM02
          IF(GEO(15)==ZERO)GEO(15)=EM02
        ENDIF
        IF(ISROT==0)ISROT=IDRIL_D
        IF(ISROT==2) ISROT = 0
        IGEO(20)=ISROT
        IGEO(51)=IPINCH
        IF(IPINCH /= 1 .AND. IPINCH /=0) THEN
          CALL ANCMSG(MSGID=1700,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1=IG, C1=IDTITL, I2=IPINCH)
        ENDIF
        IF(IPINCH == 1) THEN
          NPINCH = NUMNOD
        ENDIF
C-------to have DR----  to do it in cgrtais when Ismstr=10 is recommended one day        
        IF (ISMSTR==10.AND.ISROT>0.AND.IDROT==0) IDROT = 1
C
        IF (IHBE>11.AND.IHBE<29) THEN
          GEO(13)=GEO(17)
          GEO(17)=CVIS
        ENDIF
C----------------------
        IF(ISMSTR==0)ISMSTR=2
        IF(ISMSTR==3.AND.IHBE/=0.AND.IHBE/=2) THEN
          ISMSTR = 2
          CALL ANCMSG(MSGID=319,
     .                MSGTYPE=MSGWARNING,
     .                ANMODE=ANINFO_BLIND_2,
     .                I1=IG,
     .                C1=IDTITL)
        ENDIF
        GEO(3)=ISMSTR
C----------------------
C
        IF(GEO(38)==ZERO) GEO(38)=FIVE_OVER_6
        IF(NIP==-1)NIP=NPTS_D
        IF(NIP==1)THEN
          IF(IHBE==0.OR.IHBE==2) THEN
             IHBE_OLD=IHBE
             IHBE = 1
             CALL ANCMSG(MSGID=322,
     .                   MSGTYPE=MSGWARNING,
     .                   ANMODE=ANINFO_BLIND_2,
     .                   I1=IG,
     .                   C1=IDTITL,
     .                   I2=IHBE_OLD,
     .                   I3=IHBE)
          ENDIF
          GEO(38)= ZERO
        ENDIF
        IF(NIP>10) THEN
          CALL ANCMSG(MSGID=788,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                 I1=IG,
     .                 C1=IDTITL)

        ENDIF
        IF(ITHK==0)   ITHK=ITHK_D
        IF(ITHK_D==-2)  ITHK=-1
        IF(ISHEAR==0) ISHEAR=ISHEA_D
        IF(IPLAST==0) IPLAST=IPLA_D
        IF(IPLA_D==-2) IPLAST=-1
C        IF(ISTRAIN==0)ISTRAIN=ISTR_D
        IF(.NOT. IS_ENCRYPTED)THEN
          IF (IHBE>11.AND.IHBE<29.OR.
     .     (IHBE==0.AND.(ISH3N==1.OR.ISH3N==2))) THEN
C-------not change default dn of DKT with old define (ishel=12)     
            DN_P = GEO(13)
            IF (IHBE==12.AND.DN_P==ZERO) DN_P=EM03
            WRITE(IOUT,1112)IG,NIP,ISTRAIN,GEO(1),ISMSTR,IHBEOUTP,
     .            ISH3N,ISROT,IPINCH,GEO(16),DN_P,GEO(38),
     .            PTHK,ISHEAR,ITHK,IPLAST
          ELSE
            WRITE(IOUT,1110)IG,NIP,ISTRAIN,GEO(1),ISMSTR,IHBEOUTP,
     .            ISH3N,
     .            GEO(13),GEO(14),GEO(15),GEO(16),GEO(38),
     .            PTHK,ISHEAR,ITHK,IPLAST
          ENDIF
        ELSE
          WRITE(IOUT,1199)IG
        ENDIF
        NSHELL = NSHELL + 1
        IF (GEO(1)>=9) NSHSUP = NSHSUP +1
        IF (GEO(1)<FOURTH) NSHINF = NSHINF +1
        GEO(6)  =NIP+PUN
        IGEO(4) =NIP

        GEO(35) =ITHK
        GEO(39) =IPLAST
        IGEO(3) =ISEN
C-----------------------------
C       IHBE IPLA ISTRAIN
C-----------------------------
C       Double stockage temporaire
        GEO(171)=IHBE
        IGEO(10)=IHBE
        IF(IHBE==0)THEN
          GEO(171)=0
        ELSEIF(IHBE==1)THEN
          GEO(171)=1
        ELSEIF(IHBE==2)THEN
          GEO(171)=0
        ELSEIF(IHBE>=3.AND.IHBE<100.AND.IHBE/=4)THEN
          GEO(171)=IHBE-1
        ENDIF

        GEO(11) =ISTRAIN

        GEO(37) =ISHEAR
        IF(ISHEAR==0)THEN
          GEO(37)=0
        ELSEIF(ISHEAR==1)THEN
          GEO(37)=1
        ELSEIF(ISHEAR==2)THEN
          GEO(37)=0
        ENDIF

        IGEO(17)=ISORTH ! == 0 Isotropic property
C-------------------------------
C       Double stockage temporaire
        IGEO( 5)=NINT(GEO( 3))
        IGEO( 9)=NINT(GEO(39))
C-----------------------------
C       PROPERTY BUFFER 
C-----------------------------
        PROP_TAG(IGTYP)%G_SIG = 0
        PROP_TAG(IGTYP)%G_FOR = 5
        PROP_TAG(IGTYP)%G_MOM = 3
        PROP_TAG(IGTYP)%G_THK = 1
        PROP_TAG(IGTYP)%G_EINT= 2
        PROP_TAG(IGTYP)%G_EINS= 0
        PROP_TAG(IGTYP)%L_SIG = 5
        PROP_TAG(IGTYP)%L_THK = 0
        PROP_TAG(IGTYP)%L_EINT= 2
        PROP_TAG(IGTYP)%L_EINS= 0
        PROP_TAG(IGTYP)%G_VOL = 0
        PROP_TAG(IGTYP)%L_VOL = 0
        PROP_TAG(IGTYP)%LY_DMG = 2
        PROP_TAG(IGTYP)%LY_PLAPT = 1
        PROP_TAG(IGTYP)%LY_SIGPT = 5
        PROP_TAG(IGTYP)%G_FORPG  = 5
        PROP_TAG(IGTYP)%G_MOMPG  = 3
        PROP_TAG(IGTYP)%G_STRPG  = 8
C pinching for batoz shells         
        IF (IGTYP == 1 .AND. IHBE == 12 .AND. IPINCH == 1) THEN
          PROP_TAG(IGTYP)%G_FORPGPINCH  = 1
          PROP_TAG(IGTYP)%G_MOMPGPINCH  = 2
          PROP_TAG(IGTYP)%G_EPGPINCHXZ  = 1
          PROP_TAG(IGTYP)%G_EPGPINCHYZ  = 1
          PROP_TAG(IGTYP)%G_EPGPINCHZZ  = 1
        ENDIF
C-------------------------------
      RETURN
C-----------
 1110 FORMAT(
     & 5X,'ISOTROPIC SHELL PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'NUMBER OF INTEGRATION POINTS. . . . . .=',I10/,
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SHELL THICKNESS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'SHELL FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'3NODE SHELL FORMULATION FLAG. . . . . .=',I10/,
     & 5X,'SHELL HOURGLASS MEMBRANE DAMPING. . . .=',1PG20.13/,
     & 5X,'SHELL HOURGLASS FLEXURAL DAMPING. . . .=',1PG20.13/,
     & 5X,'SHELL HOURGLASS ROTATIONAL DAMPING. . .=',1PG20.13/,
     & 5X,'SHELL MEMBRANE DAMPING. . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'% OF INTEG POINT THICKNESS TO FAIL. . .=',1PG20.13/,
     & 5X,'SHEAR FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'THICKNESS VARIATION FLAG. . . . . . . .=',I10/,
     & 5X,'PLASTICITY FORMULATION FLAG . . . . . .=',I10//)
 1112 FORMAT(
     & 5X,'ISOTROPIC SHELL PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'NUMBER OF INTEGRATION POINTS. . . . . .=',I10/,
     & 5X,'POST PROCESSING STRAIN FLAG . . . . . .=',I10/,
     & 5X,'SHELL THICKNESS . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SMALL STRAIN FLAG . . . . . . . . . . .=',I10/,
     & 5X,'SHELL FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'3NODE SHELL FORMULATION FLAG. . . . . .=',I10/,
     & 5X,'DRILLING D.O.F. FLAG  . . . . . . . . .=',I10/,
     & 5X,'PINCHING D.O.F. FLAG  . . . . . . . . .=',I10/,
     & 5X,'SHELL MEMBRANE DAMPING. . . . . . . . .=',1PG20.13/,
     & 5X,'SHELL NUMERICAL DAMPING . . . . . . . .=',1PG20.13/,
     & 5X,'SHEAR AREA REDUCTION FACTOR . . . . . .=',1PG20.13/,
     & 5X,'% OF INTEG POINT THICKNESS TO FAIL. . .=',1PG20.13/,
     & 5X,'SHEAR FORMULATION FLAG. . . . . . . . .=',I10/,
     & 5X,'THICKNESS VARIATION FLAG. . . . . . . .=',I10/,
     & 5X,'PLASTICITY FORMULATION FLAG . . . . . .=',I10//)
 1199 FORMAT(
     & 5X,'ISOTROPIC SHELL PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'CONFIDENTIAL DATA'//)
      END
