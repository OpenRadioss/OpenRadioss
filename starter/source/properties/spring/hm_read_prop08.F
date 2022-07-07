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
Chd|  HM_READ_PROP08                source/properties/spring/hm_read_prop08.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_CRYPTED          source/devtools/hm_reader/hm_option_is_crypted.F
Chd|        NL                            source/output/message/message.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP08(GEO,IGEO,PROP_TAG ,IGTYP,IG,
     .                          ISKN,UNITAB,IUNIT,IDTITL,LSUBMODEL,SUB_ID)
C-----------------------------------------------
      USE UNITAB_MOD
      USE ELBUFTAG_MOD  
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
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
#include      "scr17_c.inc"
#include      "scr03_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "warn_c.inc"
#include      "com04_c.inc"
#include      "submod_c.inc"
#include      "sphcom.inc"
#include      "tablen_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER  IGEO(NPROPGI),ISKN(LISKN,*),IGTYP,IUNIT,SUB_ID
C     REAL
      my_real
     .   GEO(NPROPG)
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      CHARACTER IDTITL*nchartitle
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER IFUNC, IFUNC2, IFUNC3, IECROU, IFV, ISK, IG,
     .         ISENS,IFL,IFAIL,IEQUIL,IFAIL2,ISRATE,K
C     REAL
      my_real
     .   A, B, D, E, F, XM, XIN, XK, XC, DN, DX, PUN,
     .   ASRATE, LSCALE, GF3, CRIT_SCALE,FAC_M,FAC_L,FAC_T,
     .   A_WITHOUT_UNIT
      LOGICAL IS_AVAILABLE, IS_CRYPTED
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      DATA PUN/0.1/
C=======================================================================
C
C----------------------
      PUN = EM01
      IEQUIL=0
      IFAIL2 = 0
      ISRATE = 0
      ASRATE = ZERO
C
      IS_CRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
      FAC_M = UNITAB%FAC_M(IUNIT)
      FAC_L = UNITAB%FAC_L(IUNIT)
      FAC_T = UNITAB%FAC_T(IUNIT)
C
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_CRYPTED(IS_CRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('SKEW_CSID',ISK,IS_AVAILABLE,LSUBMODEL)
      IF(ISK == 0 .AND. SUB_ID .NE. 0 ) ISK = LSUBMODEL(SUB_ID)%SKEW
      CALL HM_GET_INTV('ISENSOR',ISENS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ISFLAG',IFL,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ifail',IFAIL,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ifail2',IFAIL2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Iequil',IEQUIL,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ISRATE',ISRATE,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C-------------------------------------------------
      CALL HM_GET_FLOATV('MASS',XM,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('INERTIA',XIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Asrate',ASRATE,IS_AVAILABLE,LSUBMODEL,UNITAB)
C----------------------
C
C  double stockage temporaire - supprimer GEO(12)=IGTYP apres tests
      IGEO( 1)=IG
      IGEO(11)=IGTYP
      GEO(12) =IGTYP+PUN
C----
      IF(XIN <= EM20) THEN
        XIN = EM20
        CALL ANCMSG(MSGID=445,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=IDTITL)
      ENDIF
C
      IF (IFL == 1) ISENS=-ISENS
C----
      DO K=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
        IF (ISK == ISKN(4,K+1)) THEN
          ISK=K+1
          GO TO 100
        ENDIF
      ENDDO
      CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .            C1='PROPERTY',
     .            C2='PROPERTY',
     .            I1=IGEO(1),I2=ISK,C3=IDTITL)
100   CONTINUE
C
      IF (IFAIL2 /= 1 .AND. IFAIL2 /= 2) IFAIL2 = 0
      GEO(1) =XM
      GEO(2) =ISK+PUN
      IGEO(2)=ISK
      GEO(8) =2.1
      GEO(9) =XIN
      IGEO(3)=ISENS
      GEO(79)=IFAIL
      GEO(80)=IFL
      GEO(94)=IEQUIL
      GEO(95)=IFAIL2
C----
      IF(.NOT. IS_CRYPTED)THEN
        WRITE(IOUT,1800)IG,XM,XIN,ISKN(4,ISK),ISENS,IFL,IFAIL,IFAIL2
      ELSE
        WRITE(IOUT,1000)IG
 1000   FORMAT(
     &    5X,'SPRING PROPERTY SET'/,
     &    5X,'-------------------'/,
     &    5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     &    5X,'CONFIDENTIAL DATA'//)
      ENDIF
!-------------------------------------------------------
!                      Translations
!-------------------------------------------------------
!-----------------
      ! Traction X
!-----------------
C-- Int - Trans X
      CALL HM_GET_INTV('FUN_A1',IFUNC,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('HFLAG1',IECROU,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_B1',IFV,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_C1',IFUNC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_D1',IFUNC3,IS_AVAILABLE,LSUBMODEL)
C-- Real - Trans X
      CALL HM_GET_FLOATV('STIFF1',XK,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('DAMP1',XC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Acoeft1',A,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Bcoeft1',B,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Dcoeft1',D,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MIN_RUP1',DN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAX_RUP1',DX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Prop_X_F',F,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Prop_X_E',E,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('scale1',LSCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('ffac',GF3,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV_DIM('MIN_RUP1',CRIT_SCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      IF (IECROU == 4 .AND. (IFUNC == 0 .OR. IFUNC2 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 4 .AND. GEO(2) == ZERO) THEN
         CALL ANCMSG(MSGID=230,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 5 .AND. (IFUNC ==0 .OR. IFUNC2 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 6 .AND. (IFUNC == 0 .OR. IFUNC2 == 0)) THEN
         CALL ANCMSG(MSGID=1057,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 7 .AND. IFUNC == 0) THEN
         CALL ANCMSG(MSGID=1058,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ELSEIF (IECROU == 7 .AND. IFUNC2 == 0) THEN
         CALL ANCMSG(MSGID=1059,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL,
     .               I2=IECROU)
        IECROU = 2
      ENDIF
C
      A_WITHOUT_UNIT = A / (FAC_M * FAC_L / (FAC_T **2))
      IF (IFUNC == 0 .AND. A /= ZERO .AND. A_WITHOUT_UNIT /= ONE) THEN
        CALL ANCMSG(MSGID=663,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=IDTITL)
      ENDIF
C----
      IF (A == ZERO) A = ONE * (FAC_M * FAC_L / (FAC_T **2))
      IF (D == ZERO) D = ONE * (FAC_L / FAC_T)
      IF (E == ZERO) E = ONE * (FAC_M * FAC_L / (FAC_T **2))
      IF (F == ZERO) F = ONE * (FAC_L / FAC_T)
      IF (GF3 == ZERO) GF3 = ONE * (FAC_M * FAC_L / (FAC_T **2))
      IF (LSCALE == ZERO) LSCALE = ONE * FAC_L
      IF (IFUNC == 0) THEN
        A = ONE
        B = ZERO
        E = ZERO
      ENDIF
C
      IF (IFAIL2 == 0) THEN
        DN = DN * LSCALE / FAC_L
        DX = DX * LSCALE / FAC_L
      ENDIF
      IF (DN == ZERO) DN=-EP30* CRIT_SCALE
      IF (DX == ZERO) DX= EP30* CRIT_SCALE
C----
      GEO(41) = A
      GEO(42) = B
      GEO(43) = D
      GEO(40) = E
      GEO(132)= GF3                       
      GEO(44) = ONE / F
      GEO(39) = ONE / LSCALE
      GEO(65) = DN
      GEO(66) = DX
      GEO(3)  = XK / A
      GEO(4)  = XC
      GEO(7)  = IECROU+PUN
C--   If H=6 - addditional internal variables must be stored in UVAR -  recommandation - GEO(25) = NUVAR
      IF (IECROU == 6) THEN
        GEO(25) = 6
      ENDIF
C
      IGEO(101) = IFUNC
      IGEO(102) = IFV
      IGEO(103) = IFUNC2
      IGEO(119) = IFUNC3
C----
      IF(.NOT. IS_CRYPTED)THEN
        IF (IECROU /= 5) THEN
          WRITE(IOUT,1810)'X',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                  A,B,D,E,GF3,IFV,IFUNC3,DN,DX
        ELSE
          WRITE(IOUT,1820)'X',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                  A,B,D,E,GF3,IFV,IFUNC3,DN,DX
        ENDIF
      ENDIF
!-----------------
      ! Traction Y
!-----------------
C-- Int - Trans Y
      CALL HM_GET_INTV('FUN_A2',IFUNC,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('HFLAG2',IECROU,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_B2',IFV,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_C2',IFUNC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_D2',IFUNC3,IS_AVAILABLE,LSUBMODEL)
C-- Real - Trans Y
      CALL HM_GET_FLOATV('STIFF2',XK,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('DAMP2',XC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Acoeft2',A,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Bcoeft2',B,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Dcoeft2',D,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MIN_RUP2',DN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAX_RUP2',DX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Prop_Y_F',F,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Prop_Y_E',E,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('scale2',LSCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('df',GF3,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV_DIM('MIN_RUP2',CRIT_SCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      IF (IECROU == 4 .AND. (IFUNC == 0 .OR. IFUNC2 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 4 .AND. GEO(2) == ZERO) THEN
         CALL ANCMSG(MSGID=230,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 5 .AND. (IFUNC == 0 .OR. IFUNC2 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 6 .AND. (IFUNC == 0 .OR. IFUNC2 == 0)) THEN
         CALL ANCMSG(MSGID=1057,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 7 .AND. IFUNC == 0) THEN
         CALL ANCMSG(MSGID=1058,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      
      ELSEIF (IECROU == 7 .AND. IFUNC2 == 0) THEN
         CALL ANCMSG(MSGID=1059,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL,
     .               I2=IECROU)
        IECROU = 2
      ENDIF
C
      A_WITHOUT_UNIT = A / (FAC_M * FAC_L / (FAC_T **2))
      IF (IFUNC == 0 .AND. A /= ZERO .AND. A_WITHOUT_UNIT /= ONE) THEN
        CALL ANCMSG(MSGID=663,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=IDTITL)
      ENDIF
C----
      IF (A == ZERO) A = ONE * (FAC_M * FAC_L / (FAC_T **2))
      IF (D == ZERO) D = ONE * (FAC_L / FAC_T)
      IF (E == ZERO) E = ONE * (FAC_M * FAC_L / (FAC_T **2))
      IF (F == ZERO) F = ONE * (FAC_L / FAC_T)
      IF (GF3 == ZERO) GF3 = ONE * (FAC_M * FAC_L / (FAC_T **2))
      IF (LSCALE == ZERO) LSCALE = ONE * FAC_L
      IF (IFUNC == 0) THEN
        A = ONE
        B = ZERO
        E = ZERO
      ENDIF
C
      IF (IFAIL2 == 0) THEN
        DN = DN * LSCALE / FAC_L
        DX = DX * LSCALE / FAC_L
      ENDIF
      IF (DN == ZERO) DN=-EP30* CRIT_SCALE
      IF (DX == ZERO) DX= EP30* CRIT_SCALE
C----
      GEO(45) = A
      GEO(46) = B
      GEO(47) = D
      GEO(180)= E
      GEO(133)= GF3                       
      GEO(48) = ONE / F
      GEO(174)= ONE / LSCALE
      GEO(67 )= DN
      GEO(68) = DX
      GEO(10) = XK / A
      GEO(11) = XC
      GEO(14) = IECROU+PUN
C--   If H=6 - addditional internal variables must be stored in UVAR -  recommandation - GEO(25) = NUVAR
      IF (IECROU == 6) THEN
        GEO(25) = 6
      ENDIF
C
      IGEO(104) = IFUNC
      IGEO(105) = IFV
      IGEO(106) = IFUNC2
      IGEO(120) = IFUNC3
C----
      IF(.NOT. IS_CRYPTED)THEN
        IF (IECROU /= 5) THEN
          WRITE(IOUT,1810)'Y',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                  A,B,D,E,GF3,IFV,IFUNC3,DN,DX
        ELSE
          WRITE(IOUT,1820)'Y',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                  A,B,D,E,GF3,IFV,IFUNC3,DN,DX
        ENDIF
      ENDIF
!-----------------
      ! Traction Z
!-----------------
C-- Int - Trans Z
      CALL HM_GET_INTV('FUN_A3',IFUNC,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('HFLAG3',IECROU,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_B3',IFV,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_C3',IFUNC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_D3',IFUNC3,IS_AVAILABLE,LSUBMODEL)
C-- Real - Trans Z
      CALL HM_GET_FLOATV('STIFF3',XK,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('DAMP3',XC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Acoeft3',A,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Bcoeft3',B,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Dcoeft3',D,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MIN_RUP3',DN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAX_RUP3',DX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Prop_Z_F',F,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Prop_Z_E',E,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('scale3',LSCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('D2',GF3,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV_DIM('MIN_RUP3',CRIT_SCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      IF (IECROU == 4 .AND. (IFUNC == 0 .OR. IFUNC2 == 0))THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 4 .AND. GEO(2) == ZERO)THEN
         CALL ANCMSG(MSGID=230,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 5 .AND. (IFUNC == 0 .OR. IFUNC2 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 6 .AND. (IFUNC == 0 .OR. IFUNC2 == 0)) THEN
         CALL ANCMSG(MSGID=1057,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 7 .AND. IFUNC == 0) THEN
         CALL ANCMSG(MSGID=1058,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ELSEIF (IECROU == 7 .AND. IFUNC2 == 0) THEN
         CALL ANCMSG(MSGID=1059,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL,
     .               I2=IECROU)
        IECROU = 2
      ENDIF
C
      A_WITHOUT_UNIT = A / (FAC_M * FAC_L / (FAC_T **2))
      IF (IFUNC == 0 .AND. A /= ZERO .AND. A_WITHOUT_UNIT /= ONE) THEN
        CALL ANCMSG(MSGID=663,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=IDTITL)
      ENDIF
C----
      IF (A == ZERO) A = ONE * (FAC_M * FAC_L / (FAC_T **2))
      IF (D == ZERO) D = ONE * (FAC_L / FAC_T)
      IF (E == ZERO) E = ONE * (FAC_M * FAC_L / (FAC_T **2))
      IF (F == ZERO) F = ONE * (FAC_L / FAC_T)
      IF (GF3 == ZERO) GF3 = ONE * (FAC_M * FAC_L / (FAC_T **2))
      IF (LSCALE == ZERO) LSCALE = ONE * FAC_L
      IF (IFUNC == 0) THEN
        A = ONE
        B = ZERO
        E = ZERO
      ENDIF
C
      IF (IFAIL2 == 0) THEN
        DN = DN * LSCALE / FAC_L
        DX = DX * LSCALE / FAC_L
      ENDIF
      IF (DN == ZERO) DN=-EP30* CRIT_SCALE
      IF (DX == ZERO) DX= EP30* CRIT_SCALE
C----
      GEO(49) = A
      GEO(50) = B
      GEO(51) = D
      GEO(181)= E
      GEO(134)= GF3                       
      GEO(52) = ONE / F
      GEO(175)= ONE / LSCALE
      GEO(69) = DN
      GEO(77) = DX
      GEO(15) = XK / A
      GEO(16) = XC
      GEO(18) = IECROU+PUN
C--   If H=6 - addditional internal variables must be stored in UVAR -  recommandation - GEO(25) = NUVAR
      IF (IECROU == 6) THEN
        GEO(25) = 6
      ENDIF
C
      IGEO(107) = IFUNC
      IGEO(108) = IFV
      IGEO(109) = IFUNC2
      IGEO(121) = IFUNC3
C----
      IF(.NOT. IS_CRYPTED)THEN
        IF (IECROU /= 5) THEN
          WRITE(IOUT,1810)'Z',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                  A,B,D,E,GF3,IFV,IFUNC3,DN,DX
        ELSE
          WRITE(IOUT,1820)'Z',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                  A,B,D,E,GF3,IFV,IFUNC3,DN,DX
        ENDIF
      ENDIF
!-------------------------------------------------------
!                      Rotations
!-------------------------------------------------------
!-----------------
      ! Rotation X
!-----------------
C-- Int - Rota X
      CALL HM_GET_INTV('FUN_A4',IFUNC,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('HFLAG4',IECROU,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_B4',IFV,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_C4',IFUNC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_D4',IFUNC3,IS_AVAILABLE,LSUBMODEL)
C-- Real - Rota X
      CALL HM_GET_FLOATV('STIFF4',XK,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('DAMP4',XC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Acoeft4',A,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Bcoeft4',B,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Dcoeft4',D,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MIN_RUP4',DN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAX_RUP4',DX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Prop_Tor_F',F,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Prop_Tor_E',E,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('scale4',LSCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Y0',GF3,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV_DIM('MIN_RUP4',CRIT_SCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      IF (IECROU == 4 .AND. (IFUNC == 0 .OR. IFUNC2 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 4 .AND. GEO(2) == ZERO) THEN
         CALL ANCMSG(MSGID=230,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 5 .AND. (IFUNC == 0 .OR. IFUNC2 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 6 .AND. (IFUNC == 0 .OR. IFUNC2 == 0)) THEN
         CALL ANCMSG(MSGID=1057,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 7 .AND. IFUNC == 0) THEN
         CALL ANCMSG(MSGID=1058,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ELSEIF (IECROU == 7 .AND. IFUNC2 == 0) THEN
         CALL ANCMSG(MSGID=1059,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL,
     .               I2=IECROU)
        IECROU = 2
      ENDIF
C
      A_WITHOUT_UNIT = A / (FAC_M * FAC_L**2 / FAC_T**2)
      IF (IFUNC == 0 .AND. A /= ZERO .AND. A_WITHOUT_UNIT /= ONE) THEN
        CALL ANCMSG(MSGID=663,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=IDTITL)
      ENDIF
C----
      IF (A == ZERO) A = ONE * (FAC_M * FAC_L**2 / (FAC_T **2))
      IF (D == ZERO) D = ONE / FAC_T
      IF (E == ZERO) E = ONE * (FAC_M * FAC_L**2 / (FAC_T **2))
      IF (F == ZERO) F = ONE / FAC_T
      IF (GF3 == ZERO) GF3 = ONE * (FAC_M * FAC_L**2 / (FAC_T **2))
      IF (LSCALE == ZERO) LSCALE = ONE
      IF (IFUNC == 0) THEN
        A = ONE
        B = ZERO
        E = ZERO
      ENDIF
C
      IF (IFAIL2 == 0) THEN
        DN = DN * LSCALE
        DX = DX * LSCALE
      ENDIF
      IF (DN == ZERO) DN=-EP30* CRIT_SCALE
      IF (DX == ZERO) DX= EP30* CRIT_SCALE
C----
      GEO(53)  = A
      GEO(54)  = B
      GEO(55)  = D
      GEO(182) = E
      GEO(135) = GF3                       
      GEO(56)  = ONE / F
      GEO(176) = ONE / LSCALE
      GEO(71)  = DN
      GEO(72)  = DX
      GEO(19)  = XK / A
      GEO(20)  = XC
      GEO(22)  = IECROU+PUN
C--   If H=6 - addditional internal variables must be stored in UVAR -  recommandation - GEO(25) = NUVAR
      IF (IECROU == 6) THEN
        GEO(25) = 6
      ENDIF
C
      IGEO(110) = IFUNC
      IGEO(111) = IFV
      IGEO(112) = IFUNC2
      IGEO(122) = IFUNC3
C----
      IF(.NOT. IS_CRYPTED)THEN
        IF (IECROU /= 5) THEN
          WRITE(IOUT,1830)'X',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                  A,B,D,E,GF3,IFV,IFUNC3,DN,DX
        ELSE
          WRITE(IOUT,1840)'X',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                  A,B,D,E,GF3,IFV,IFUNC3,DN,DX
        ENDIF
      ENDIF
!-----------------
      ! Rotation Y
!-----------------
C-- Int - Rota Y
      CALL HM_GET_INTV('FUN_A5',IFUNC,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('HFLAG5',IECROU,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_B5',IFV,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_C5',IFUNC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_D5',IFUNC3,IS_AVAILABLE,LSUBMODEL)
C-- Real - Rota Y
      CALL HM_GET_FLOATV('STIFF5',XK,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('DAMP5',XC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Acoeft5',A,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Bcoeft5',B,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Dcoeft5',D,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MIN_RUP5',DN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAX_RUP5',DX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Prop_FlxY_F',F,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Prop_FlxY_E',E,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('scale5',LSCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Z0',GF3,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV_DIM('MIN_RUP5',CRIT_SCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      IF (IECROU == 4 .AND. (IFUNC == 0 .OR. IFUNC2 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 4 .AND. GEO(2) == ZERO) THEN
         CALL ANCMSG(MSGID=230,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 5 .AND. (IFUNC == 0 .OR. IFUNC2 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 6 .AND. (IFUNC == 0 .OR. IFUNC2 == 0)) THEN
         CALL ANCMSG(MSGID=1057,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 7 .AND. IFUNC == 0) THEN
         CALL ANCMSG(MSGID=1058,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ELSEIF (IECROU == 7 .AND. IFUNC2 == 0) THEN
         CALL ANCMSG(MSGID=1059,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL,
     .               I2=IECROU)
        IECROU = 2
      ENDIF
C
      A_WITHOUT_UNIT = A / (FAC_M * FAC_L**2 / FAC_T**2)
      IF (IFUNC == 0 .AND. A /= ZERO .AND. A_WITHOUT_UNIT /= ONE) THEN
        CALL ANCMSG(MSGID=663,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=IDTITL)
      ENDIF
C----
      IF (A == ZERO) A = ONE * (FAC_M * FAC_L**2 / (FAC_T **2))
      IF (D == ZERO) D = ONE / FAC_T
      IF (E == ZERO) E = ONE * (FAC_M * FAC_L**2 / (FAC_T **2))
      IF (F == ZERO) F = ONE / FAC_T
      IF (GF3 == ZERO) GF3 = ONE * (FAC_M * FAC_L**2 / (FAC_T **2))
      IF (LSCALE == ZERO) LSCALE = ONE
      IF (IFUNC == 0) THEN
        A = ONE
        B = ZERO
        E = ZERO
      ENDIF
C
      IF (IFAIL2 == 0) THEN
        DN = DN * LSCALE
        DX = DX * LSCALE
      ENDIF
      IF (DN == ZERO) DN=-EP30* CRIT_SCALE
      IF (DX == ZERO) DX= EP30* CRIT_SCALE
C----
      GEO(57)  = A
      GEO(58)  = B
      GEO(59)  = D
      GEO(183) = E
      GEO(136) = GF3                       
      GEO(60)  = ONE / F
      GEO(177) = ONE / LSCALE
      GEO(73)  = DN
      GEO(74)  = DX
      GEO(23)  = XK / A
      GEO(24)  = XC
      GEO(26)  = IECROU+PUN
C--   If H=6 - addditional internal variables must be stored in UVAR -  recommandation - GEO(25) = NUVAR
      IF (IECROU == 6) THEN
        GEO(25) = 6
      ENDIF
C
      IGEO(113) = IFUNC
      IGEO(114) = IFV
      IGEO(115) = IFUNC2
      IGEO(123) = IFUNC3
C----
      IF(.NOT. IS_CRYPTED)THEN
        IF (IECROU /= 5) THEN
          WRITE(IOUT,1830)'Y',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                  A,B,D,E,GF3,IFV,IFUNC3,DN,DX
        ELSE
          WRITE(IOUT,1840)'Y',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                  A,B,D,E,GF3,IFV,IFUNC3,DN,DX
        ENDIF
      ENDIF
!-----------------
      ! Rotation Z
!-----------------
C-- Int - Rota Z
      CALL HM_GET_INTV('FUN_A6',IFUNC,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('HFLAG6',IECROU,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_B6',IFV,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_C6',IFUNC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_D6',IFUNC3,IS_AVAILABLE,LSUBMODEL)
C-- Real - Rota Z
      CALL HM_GET_FLOATV('STIFF6',XK,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('DAMP6',XC,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Acoeft6',A,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Bcoeft6',B,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Dcoeft6',D,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MIN_RUP6',DN,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('MAX_RUP6',DX,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Prop_FlxZ_F',F,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Prop_FlxZ_E',E,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('scale6',LSCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Hscale6',GF3,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV_DIM('MIN_RUP6',CRIT_SCALE,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      IF (IECROU == 4 .AND. (IFUNC == 0 .OR. IFUNC2 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 4 .AND. GEO(2) == ZERO) THEN
         CALL ANCMSG(MSGID=230,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 5 .AND. (IFUNC == 0 .OR. IFUNC2 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 6 .AND. (IFUNC == 0 .OR. IFUNC2 == 0)) THEN
         CALL ANCMSG(MSGID=1057,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 7 .AND. IFUNC == 0) THEN
         CALL ANCMSG(MSGID=1058,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      
      ELSEIF (IECROU == 7 .AND. IFUNC2 == 0) THEN
         CALL ANCMSG(MSGID=1059,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL,
     .               I2=IECROU)
        IECROU = 2
      ENDIF
C
      A_WITHOUT_UNIT = A / (FAC_M * FAC_L**2 / FAC_T**2)
      IF (IFUNC == 0 .AND. A /= ZERO .AND. A_WITHOUT_UNIT /= ONE) THEN
        CALL ANCMSG(MSGID=663,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=IDTITL)
      ENDIF
C----
      IF (A == ZERO) A = ONE * (FAC_M * FAC_L**2 / (FAC_T **2))
      IF (D == ZERO) D = ONE / FAC_T
      IF (E == ZERO) E = ONE * (FAC_M * FAC_L**2 / (FAC_T **2))
      IF (F == ZERO) F = ONE / FAC_T
      IF (GF3 == ZERO) GF3 = ONE * (FAC_M * FAC_L**2 / (FAC_T **2))
      IF (LSCALE == ZERO) LSCALE = ONE
      IF (IFUNC == 0) THEN
        A = ONE
        B = ZERO
        E = ZERO
      ENDIF
C
      IF (IFAIL2 == 0) THEN
        DN = DN * LSCALE
        DX = DX * LSCALE
      ENDIF
      IF (DN == ZERO) DN=-EP30* CRIT_SCALE
      IF (DX == ZERO) DX= EP30* CRIT_SCALE
C----
      GEO(61)  = A
      GEO(62)  = B
      GEO(63)  = D
      GEO(184) = E
      GEO(137) = GF3                       
      GEO(64)  = ONE / F
      GEO(178) = ONE / LSCALE
      GEO(75)  = DN
      GEO(76)  = DX
      GEO(27)  = XK / A
      GEO(28)  = XC
      GEO(30)  = IECROU+PUN
C--   If H=6 - addditional internal variables must be stored in UVAR -  recommandation - GEO(25) = NUVAR
      IF (IECROU == 6) THEN
        GEO(25) = 6
      ENDIF
C
      IGEO(116) = IFUNC
      IGEO(117) = IFV
      IGEO(118) = IFUNC2
      IGEO(124) = IFUNC3
C----
      IF(.NOT. IS_CRYPTED)THEN
        IF (IECROU /= 5) THEN
          WRITE(IOUT,1830)'Z',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                  A,B,D,E,GF3,IFV,IFUNC3,DN,DX
        ELSE
          WRITE(IOUT,1840)'Z',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                  A,B,D,E,GF3,IFV,IFUNC3,DN,DX
        ENDIF
      ENDIF
C------------------------
      IF (ASRATE == ZERO) ASRATE=EP30 / FAC_T
      GEO(96) = ISRATE
      GEO(97) = ASRATE
      IF(.NOT. IS_CRYPTED)THEN
        WRITE(IOUT, 1850) ISRATE, ASRATE
      ENDIF
C
C----------------------
C FROM LECGEO - GENERAL
C----------------------
C
      IF(GEO(39)/=ZERO.AND.IGEO( 9)== 0) IGEO( 9)=NINT(GEO(39))
C
C-----------------------------
C       PROPERTY BUFFER 
C-----------------------------
C
       PROP_TAG(IGTYP)%G_FOR  = 3     ! (FX,FY,FZ)
       PROP_TAG(IGTYP)%G_MOM = 3  !  (XMOM,YMOM,ZMOM)
       PROP_TAG(IGTYP)%G_LENGTH = 3   ! X0 (AL0,Y0,Z0)
       PROP_TAG(IGTYP)%G_TOTDEPL = 3  ! DX (DY,DZ) - total deformation (translation)
       PROP_TAG(IGTYP)%G_TOTROT = 3   ! RX (RY,RZ) - total deformation (rotation)
       PROP_TAG(IGTYP)%G_FOREP = 3    ! FORCE - (ELASTO PLASTIQUE (ISOTROPE))
       PROP_TAG(IGTYP)%G_MOMEP = 3    ! MOMENT - (ELASTO PLASTIQUE (ISOTROPE))
       PROP_TAG(IGTYP)%G_DEP_IN_TENS = 3   ! DPX  (DPY,DPZ) - max displacement in tension
       PROP_TAG(IGTYP)%G_DEP_IN_COMP = 3   ! DPX2 (DPY2,DPZ2) - max displacelent in compresion
       PROP_TAG(IGTYP)%G_ROT_IN_TENS = 3   ! RPX (RPY,RPZ) - max rotation in tension
       PROP_TAG(IGTYP)%G_ROT_IN_COMP = 3   ! RPX2 (RPY2,RPY2) - max rotation in compresion
       PROP_TAG(IGTYP)%G_POSX = 5
       PROP_TAG(IGTYP)%G_POSY = 5
       PROP_TAG(IGTYP)%G_POSZ = 5
       PROP_TAG(IGTYP)%G_POSXX = 5
       PROP_TAG(IGTYP)%G_POSYY = 5
       PROP_TAG(IGTYP)%G_POSZZ = 5
       PROP_TAG(IGTYP)%G_YIELD = 6
       PROP_TAG(IGTYP)%G_LENGTH_ERR = 3
       PROP_TAG(IGTYP)%G_E6 = 6
       PROP_TAG(IGTYP)%G_RUPTCRIT = 1
       PROP_TAG(IGTYP)%G_NUVAR = MAX(PROP_TAG(IGTYP)%G_NUVAR,NINT(GEO(25))) ! addtional internal variables for h=6
       PROP_TAG(IGTYP)%G_DEFINI = 6
       PROP_TAG(IGTYP)%G_FORINI = 6
       PROP_TAG(IGTYP)%G_SKEW_ID = 1
C
C------------------------
      RETURN
C------------------------
 1800 FORMAT(
     & 5X,'SPRING PROPERTY SET'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'SPRING MASS . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING INERTIA. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SKEW FRAME NUMBER (0:GLOBAL). . . . . .=',I10/,
     & 5X,'SENSOR NUMBER (0:NOT USED). . . . . . .=',I10/,
     & 5X,'SENSOR FLAG (0:ACTIV 1:DISACT 2:BOTH) .=',I10/,
     & 5X,'FAILURE FLAG (0:UNCOUPLED 1:COUPLED). .=',I10/,
     & 5X,'FAILURE CRITERIUM (DISPL/FORCE/ENERGY).=',I10/,
     & 5X,'      0:DISPLACEMENT  1:FORCE  2:ENERGY '  ,/)
 1810 FORMAT(
     & 5X,A1,' TRANSLATION'/,
     & 5X,'SPRING STIFFNESS. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING DAMPING. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR LOADING ',/,
     & 5X,'FORCE-DISPLACEMENT CURVE. . . . . . . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR UNLOADING ',/,
     & 5X,'FORCE-DISPLACEMENT CURVE (H=4,5,7). . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'HARDENING FLAG H. . . . . . . . . . . .=',I10/,
     & 5X,'0:ELASTIC   1:ISOTROPIC 2:UNCOUPLED',/,
     & 5X,'4:KINEMATIC 5:UNCOUPLED NL (UN/RE)LOADING',/,
     & 5X,'6:ELASTO PLASTIC WITH HARDENING 7: ELASTIC HYSTERESIS',/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR A. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR B. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR D. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR E. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR IGF3 . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'NEGATIVE FAILURE DISPLACEMENT . . . . .=',1PG20.13/,
     & 5X,'POSITIVE FAILURE DISPLACEMENT . . . . .=',1PG20.13/)
 1820 FORMAT(
     & 5X,A1,' TRANSLATION'/,
     & 5X,'SPRING STIFFNESS. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING DAMPING. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR LOADING ',/,
     & 5X,'FORCE-DISPLACEMENT CURVE. . . . . . . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'PERMANENT DISPL./MAX. DISPL. CURVE(H=5)=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'HARDENING FLAG H. . . . . . . . . . . .=',I10/,
     & 5X,'0:ELASTIC   1:ISOTROPIC 2:UNCOUPLED',/,
     & 5X,'4:KINEMATIC 5:UNCOUPLED NL (UN/RE)LOADING',/,
     & 5X,'6:ELASTO PLASTIC WITH HARDENING 7: ELASTIC HYSTERESIS',/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR A. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR B. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR D. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR E. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR IGF3 . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'NEGATIVE FAILURE DISPLACEMENT . . . . .=',1PG20.13/,
     & 5X,'POSITIVE FAILURE DISPLACEMENT . . . . .=',1PG20.13/)
 1830 FORMAT(
     & 5X,A1,' ROTATION'/,
     & 5X,'SPRING STIFFNESS. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING DAMPING. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR LOADING ',/,
     & 5X,'FORCE-DISPLACEMENT CURVE. . . . . . . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR UNLOADING ',/,
     & 5X,'FORCE-DISPLACEMENT CURVE (H=4,5,7). . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'HARDENING FLAG H. . . . . . . . . . . .=',I10/,
     & 5X,'0:ELASTIC   1:ISOTROPIC 2:UNCOUPLED',/,
     & 5X,'4:KINEMATIC 5:UNCOUPLED NL (UN/RE)LOADING',/,
     & 5X,'6:ELASTO PLASTIC WITH HARDENING 7: ELASTIC HYSTERESIS',/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR A. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR B. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR D. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR E. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR IGF3 . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'NEGATIVE FAILURE ROTATION . . . . . . .=',1PG20.13/,
     & 5X,'POSITIVE FAILURE ROTATION . . . . . . .=',1PG20.13/)
 1840 FORMAT(
     & 5X,A1,' ROTATION'/,
     & 5X,'SPRING STIFFNESS. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING DAMPING. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR LOADING ',/,
     & 5X,'FORCE-DISPLACEMENT CURVE. . . . . . . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'PERMANENT ROT./MAX. ROT. CURVE (H=5). .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'HARDENING FLAG H. . . . . . . . . . . .=',I10/,
     & 5X,'0:ELASTIC   1:ISOTROPIC 2:UNCOUPLED',/,
     & 5X,'4:KINEMATIC 5:UNCOUPLED NL (UN/RE)LOADING',/,
     & 5X,'6:ELASTO PLASTIC WITH HARDENING 7: ELASTIC HYSTERESIS',/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR A. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR B. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR D. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR E. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR IGF3 . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'NEGATIVE FAILURE ROTATION . . . . . . .=',1PG20.13/,
     & 5X,'POSITIVE FAILURE ROTATION . . . . . . .=',1PG20.13/)
1850  FORMAT(
     & 5X,'SMOOTH STRAIN RATE OPTION . . .. . . . =',I10/,
     & 5X,'STRAIN RATE CUTTING FREQUENCY .. . . . =',1PG20.13/)
c-----------
 999  CALL FREERR(1)
c-----------
      RETURN
      END
