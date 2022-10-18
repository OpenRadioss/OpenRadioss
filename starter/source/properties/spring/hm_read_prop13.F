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
Chd|  HM_READ_PROP13                source/properties/spring/hm_read_prop13.F
Chd|-- called by -----------
Chd|        HM_READ_PROPERTIES            source/properties/hm_read_properties.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        NL                            source/output/message/message.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP13(GEO, IGEO, IG, UNITAB,ISKN,
     .                          IDTITL, IGTYP, PROP_TAG,LSUBMODEL)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE ELBUFTAG_MOD   
      USE SUBMODEL_MOD         
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
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IGEO(NPROPGI),ISKN(LISKN,*),IGTYP,IG
C     REAL
      my_real GEO(NPROPG)
      my_real FAC_M, FAC_L, FAC_T
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
      CHARACTER IDTITL*nchartitle
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER J, IFUNC, IFUNC2,IFUNC3, IECROU, IFV, ISK,
     .         ISENS,IFL,IFAIL,ILENG,IFAIL2,ISRATE,K
C     REAL
      my_real
     .   A, B, D, E, F,  XM, XIN, XK, XC, DN, DX, FWV, LSCALE,
     .   PUN,VT0, VR0, CC(6), CN(6), XA(6), XB(6),ASRATE,GF3,
     .   A_UNIT,B_UNIT,D_UNIT,E_UNIT,F_UNIT,
     .   LSCALE_UNIT,GF3_UNIT,VT0_UNIT,VR0_UNIT,ASR_UNIT,CRIT_SCALE(6)
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C=======================================================================
      DATA PUN/0.1/
C=======================================================================
C
      PUN = EM01
      FWV = ZERO
      IFAIL2 = 0
      ISRATE = 0
      ASRATE = ZERO
C
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.

C     Double stockage temporaire - supprimer GEO(12,I)=IGTYP apres tests
      IGEO( 1)=IG
      IGEO(11)=IGTYP
      GEO(12) =IGTYP+PUN
C
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('SKEW_CSID',ISK,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ISENSOR',ISENS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ISFLAG',IFL,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ifail',IFAIL,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ileng',ILENG,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('Ifail2',IFAIL2,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('MASS',XM,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('INERTIA',XIN,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CC(1:6) = ZERO
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
      IF (IFAIL2 /= 1 .AND. IFAIL2 /= 2 .AND. IFAIL2 /= 3) IFAIL2 = 0
      GEO(1) =XM
      GEO(2) =ISK+PUN
      IGEO(2)=ISK
      GEO(8) =4
      GEO(9) =XIN
      IGEO(3)=ISENS
      GEO(79)=IFAIL
      GEO(80)=IFL
      GEO(93)=ILENG
      GEO(95)=IFAIL2
C----
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,1000)IG
 1000   FORMAT(
     &    5X,'SPRING PROPERTY SET'/,
     &    5X,'-------------------'/,
     &    5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     &    5X,'CONFIDENTIAL DATA'//)
      ELSE
        WRITE(IOUT,1801)IG,XM,XIN,ISKN(4,ISK),ISENS,IFL,IFAIL,IFAIL2,
     .                  ILENG
      ENDIF
!-------------------------------------------------------
!                      Translations
!-------------------------------------------------------
!-----------------
      ! Traction X
!-----------------
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('FUN_A1',IFUNC,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('HFLAG1',IECROU,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_B1',IFV,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_C1',IFUNC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_D1',IFUNC3,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
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
      CALL HM_GET_FLOATV('Prop_X_H',GF3,IS_AVAILABLE,LSUBMODEL,UNITAB)
      !units for default values
      CALL HM_GET_FLOATV_DIM('Acoeft1',A_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('Bcoeft1',B_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('Dcoeft1',D_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('Prop_X_F',F_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('Prop_X_E',E_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('scale1',LSCALE_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('Prop_X_H',GF3_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('MIN_RUP1',CRIT_SCALE(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
C----
!      IF (IFUNC /= 0 .AND. IECROU >= 1 .AND. XK == ZERO) THEN
!         CALL ANCMSG(MSGID=230,
!     .               MSGTYPE=MSGERROR,
!     .               ANMODE=ANINFO_BLIND_1,
!     .               I1=IG,
!     .               C1=IDTITL)
!      ENDIF
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
      IF (IECROU == 5. AND. (IFUNC == 0 .OR. IFUNC2 == 0)) THEN
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
      IF (IFUNC == 0 .AND. A /= ZERO .AND. A /= ONE) THEN
        CALL ANCMSG(MSGID=663,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=IDTITL)
      ENDIF
C----
      IF (A == ZERO) A = ONE * A_UNIT
      IF (D == ZERO) D = ONE * D_UNIT
      IF (E == ZERO) E = ONE * E_UNIT
      IF (F == ZERO) F = ONE * F_UNIT
      IF (LSCALE == ZERO) LSCALE = ONE * LSCALE_UNIT
      IF (GF3 == ZERO) GF3 = ONE * GF3_UNIT
      IF (IFUNC == 0) THEN
        A = ONE
        B = ZERO
        E = ZERO
      ENDIF
C
      IF ((IFAIL2 == 0).OR.(IFAIL2 ==1)) THEN
        DN = DN * LSCALE / CRIT_SCALE(1)
        DX = DX * LSCALE / CRIT_SCALE(1)
      ENDIF
      IF (DN == ZERO) DN=-EP30* CRIT_SCALE(1)
      IF (DX == ZERO) DX= EP30* CRIT_SCALE(1)
      IF ((IFAIL2 == 0).OR.(IFAIL2 ==1)) CRIT_SCALE(1) = LSCALE / CRIT_SCALE(1)
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
      GEO(87) = FWV
      GEO(3)  = XK / A
      GEO(4)  = XC
      GEO(7)  = IECROU+PUN
C--   If H=6 - addditional internal variables must be stored in UVAR -  - GEO(25) = NUVAR
      IF (IECROU == 6) THEN
        GEO(25) = 6
      ENDIF
C
      IGEO(101) = IFUNC
      IGEO(102) = IFV
      IGEO(103) = IFUNC2
      IGEO(119) = IFUNC3
C----
      IF (.NOT. IS_ENCRYPTED) THEN
        IF (IECROU /= 5) THEN
          IF (IFAIL2 == 3) THEN
            WRITE(IOUT,1813)'TENSION',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DX
          ELSEIF (IFAIL2 == 2) THEN
            WRITE(IOUT,1812)'TENSION',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ELSE
            WRITE(IOUT,1810)'TENSION',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ENDIF ! IF (IFAIL2 == 3)
        ELSE
          IF (IFAIL2 == 3) THEN
            WRITE(IOUT,1823)'TENSION',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DX
          ELSEIF (IFAIL2 == 2) THEN
            WRITE(IOUT,1822)'TENSION',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ELSE
            WRITE(IOUT,1820)'TENSION',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ENDIF ! IF (IFAIL2 == 3)
        ENDIF ! IF (IECROU /= 5)
      ENDIF
!-----------------
      ! Shear XY
!-----------------
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('FUN_A2',IFUNC,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('HFLAG2',IECROU,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_B2',IFV,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_C2',IFUNC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_D2',IFUNC3,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
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
      CALL HM_GET_FLOATV('Prop_Y_H',GF3,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV_DIM('MIN_RUP2',CRIT_SCALE(2),IS_AVAILABLE,LSUBMODEL,UNITAB)

C----
!      IF (IFUNC /= 0 .AND. IECROU >= 1 .AND. XK == ZERO) THEN
!         CALL ANCMSG(MSGID=230,
!     .               MSGTYPE=MSGERROR,
!     .               ANMODE=ANINFO_BLIND_1,
!     .               I1=IG,
!     .               C1=IDTITL)
!      ENDIF
      IF (IECROU == 4 .AND. (IFUNC == 0 .OR. IFUNC2 == 0)) THEN
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
      IF (IFUNC == 0 .AND. A /= ZERO .AND. A /= ONE) THEN
        CALL ANCMSG(MSGID=663,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=IDTITL)
      ENDIF
C----
      IF (A == ZERO) A = ONE * A_UNIT
      IF (D == ZERO) D = ONE * D_UNIT
      IF (E == ZERO) E = ONE * E_UNIT
      IF (F == ZERO) F = ONE * F_UNIT
      IF (LSCALE == ZERO) LSCALE = ONE * LSCALE_UNIT
      IF (GF3 == ZERO) GF3 = ONE * GF3_UNIT
      IF (IFUNC == 0) THEN
        A = ONE
        B = ZERO
        E = ZERO
      ENDIF 
C
      IF ((IFAIL2 == 0).OR.(IFAIL2 ==1)) THEN
        DN = DN * LSCALE / CRIT_SCALE(2)
        DX = DX * LSCALE / CRIT_SCALE(2)
      ENDIF
      IF (DN == ZERO) DN=-EP30* CRIT_SCALE(2)
      IF (DX == ZERO) DX= EP30* CRIT_SCALE(2)
      IF ((IFAIL2 == 0).OR.(IFAIL2 ==1)) CRIT_SCALE(2) = LSCALE / CRIT_SCALE(2)
C----
      GEO(45) = A
      GEO(46) = B
      GEO(47) = D
      GEO(180)= E
      GEO(133)= GF3                       
      GEO(48) = ONE / F
      GEO(174)= ONE / LSCALE
      GEO(67) = DN
      GEO(68) = DX
      GEO(88) = FWV
      GEO(10) = XK / A
      GEO(11) = XC
      GEO(14) = IECROU+PUN
C--   If H=6 - addditional internal variables must be stored in UVAR -  - GEO(25) = NUVAR
      IF (IECROU == 6) THEN
        GEO(25) = 6
      ENDIF
C
      IGEO(104) = IFUNC
      IGEO(105) = IFV
      IGEO(106) = IFUNC2
      IGEO(120) = IFUNC3
C----
      IF (.NOT. IS_ENCRYPTED) THEN
        IF (IECROU /= 5) THEN
          IF (IFAIL2 == 3) THEN
            WRITE(IOUT,1813)'Y SHEAR',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DX
          ELSEIF (IFAIL2 == 2) THEN
            WRITE(IOUT,1812)'Y SHEAR',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ELSE
            WRITE(IOUT,1810)'Y SHEAR',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ENDIF ! IF (IFAIL2 == 3)
        ELSE
          IF (IFAIL2 == 3) THEN
            WRITE(IOUT,1823)'Y SHEAR',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DX
          ELSEIF (IFAIL2 == 2) THEN
            WRITE(IOUT,1822)'Y SHEAR',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ELSE
            WRITE(IOUT,1820)'Y SHEAR',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ENDIF ! IF (IFAIL2 == 3)
        ENDIF ! IF (IECROU /= 5)
      ENDIF
!-----------------
      ! Shear XZ
!-----------------
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('FUN_A3',IFUNC,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('HFLAG3',IECROU,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_B3',IFV,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_C3',IFUNC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_D3',IFUNC3,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
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
      CALL HM_GET_FLOATV('Prop_Z_H',GF3,IS_AVAILABLE,LSUBMODEL,UNITAB)
C
      CALL HM_GET_FLOATV_DIM('MIN_RUP3',CRIT_SCALE(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
C
C----
!      IF (IFUNC /= 0 .AND. IECROU >= 1 .AND. XK == ZERO) THEN
!         CALL ANCMSG(MSGID=230,
!     .               MSGTYPE=MSGERROR,
!     .               ANMODE=ANINFO_BLIND_1,
!     .               I1=IG,
!     .               C1=IDTITL)
!      ENDIF
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
      IF (IFUNC == 0 .AND. A /= ZERO .AND. A /= ONE) THEN
        CALL ANCMSG(MSGID=663,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=IDTITL)
      ENDIF
C----
      IF (A == ZERO) A = ONE * A_UNIT
      IF (D == ZERO) D = ONE * D_UNIT
      IF (E == ZERO) E = ONE * E_UNIT
      IF (F == ZERO) F = ONE * F_UNIT
      IF (LSCALE == ZERO) LSCALE = ONE * LSCALE_UNIT
      IF (GF3 == ZERO) GF3 = ONE * GF3_UNIT
      IF (IFUNC == 0) THEN
        A = ONE
        B = ZERO
        E = ZERO
      ENDIF
C
      IF ((IFAIL2 == 0).OR.(IFAIL2 ==1)) THEN
        DN = DN * LSCALE / CRIT_SCALE(3)
        DX = DX * LSCALE / CRIT_SCALE(3)
      ENDIF
      IF (DN == ZERO) DN=-EP30* CRIT_SCALE(3)
      IF (DX == ZERO) DX= EP30* CRIT_SCALE(3)
      IF ((IFAIL2 == 0).OR.(IFAIL2 ==1)) CRIT_SCALE(3) = LSCALE / CRIT_SCALE(3)
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
      GEO(89) = FWV
      GEO(15) = XK / A
      GEO(16) = XC
      GEO(18) = IECROU+PUN
C--   If H=6 - addditional internal variables must be stored in UVAR -  - GEO(25) = NUVAR
      IF (IECROU == 6) THEN
        GEO(25) = 6
      ENDIF
C
      IGEO(107) = IFUNC
      IGEO(108) = IFV
      IGEO(109) = IFUNC2
      IGEO(121) = IFUNC3
C----
      IF (.NOT. IS_ENCRYPTED) THEN
        IF (IECROU /= 5) THEN
          IF (IFAIL2 == 3) THEN
            WRITE(IOUT,1813)'Z SHEAR',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DX
          ELSEIF (IFAIL2 == 2) THEN
            WRITE(IOUT,1812)'Z SHEAR',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ELSE
            WRITE(IOUT,1810)'Z SHEAR',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ENDIF ! IF (IFAIL2 == 3)
        ELSE
          IF (IFAIL2 == 3) THEN
            WRITE(IOUT,1823)'Z SHEAR',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DX
          ELSEIF (IFAIL2 == 2) THEN
            WRITE(IOUT,1822)'Z SHEAR',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ELSE
            WRITE(IOUT,1820)'Z SHEAR',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ENDIF ! IF (IFAIL2 == 3)
        ENDIF ! IF (IECROU /= 5)
      ENDIF
      IF (XM <= EM20) THEN
         GEO(1) = EM20
         CALL ANCMSG(MSGID=444,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
!-------------------------------------------------------
!                      Rotations
!-------------------------------------------------------
!-----------------
      ! Torsion X
!-----------------
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('FUN_A4',IFUNC,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('HFLAG4',IECROU,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_B4',IFV,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_C4',IFUNC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_D4',IFUNC3,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
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
      CALL HM_GET_FLOATV('Prop_Tor_H',GF3,IS_AVAILABLE,LSUBMODEL,UNITAB)
      !units for default values
      CALL HM_GET_FLOATV_DIM('Acoeft4',A_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('Bcoeft4',B_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('Dcoeft4',D_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('Prop_Tor_F',F_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('Prop_Tor_E',E_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('scale4',LSCALE_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('Prop_Tor_H',GF3_UNIT,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('MIN_RUP4',CRIT_SCALE(4),IS_AVAILABLE,LSUBMODEL,UNITAB)
C
C----
!      IF(IFUNC/=0.AND.IECROU>=1.AND.XK == ZERO)THEN
!         CALL ANCMSG(MSGID=230,
!     .               MSGTYPE=MSGERROR,
!     .               ANMODE=ANINFO_BLIND_1,
!     .               I1=IG,
!     .               C1=IDTITL)
!      ENDIF
      IF (IECROU == 4 .AND. (IFUNC == 0 .OR. IFUNC2 == 0)) THEN
         CALL ANCMSG(MSGID=231,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=IDTITL)
      ENDIF
      IF (IECROU == 4. AND. GEO(2) == ZERO) THEN
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
      IF (IFUNC == 0 .AND. A /= ZERO .AND. A /= ONE) THEN
        CALL ANCMSG(MSGID=663,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=IDTITL)
      ENDIF
C----
      IF (A == ZERO) A = ONE * A_UNIT
      IF (D == ZERO) D = ONE * D_UNIT
      IF (E == ZERO) E = ONE * E_UNIT
      IF (F == ZERO) F = ONE * F_UNIT
      IF (LSCALE == ZERO) LSCALE = ONE * LSCALE_UNIT
      IF (GF3 == ZERO) GF3 = ONE * GF3_UNIT
      IF (IFUNC == 0) THEN
        A = ONE
        B = ZERO
        E = ZERO
      ENDIF
C
      IF ((IFAIL2 == 0).OR.(IFAIL2 ==1)) THEN
        DN = DN * LSCALE / CRIT_SCALE(4)
        DX = DX * LSCALE / CRIT_SCALE(4)
      ENDIF
      IF (DN == ZERO) DN=-EP30* CRIT_SCALE(4)
      IF (DX == ZERO) DX= EP30* CRIT_SCALE(4)
      IF ((IFAIL2 == 0).OR.(IFAIL2 ==1)) CRIT_SCALE(4) = LSCALE / CRIT_SCALE(4)
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
C--   If H=6 - addditional internal variables must be stored in UVAR -  - GEO(25) = NUVAR
      IF (IECROU == 6) THEN
        GEO(25) = 6
      ENDIF
C
      IGEO(110) = IFUNC
      IGEO(111) = IFV
      IGEO(112) = IFUNC2
      IGEO(122) = IFUNC3
C----
      IF (.NOT. IS_ENCRYPTED) THEN
        IF (IECROU /= 5) THEN
          IF (IFAIL2 == 3) THEN
            WRITE(IOUT,1833)'TORSION',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DX
          ELSEIF (IFAIL2 == 2) THEN
            WRITE(IOUT,1832)'TORSION',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ELSE
            WRITE(IOUT,1830)'TORSION',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ENDIF ! IF (IFAIL2 == 3)
        ELSE
          IF (IFAIL2 == 3) THEN
            WRITE(IOUT,1843)'TORSION',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DX
          ELSEIF (IFAIL2 == 2) THEN
            WRITE(IOUT,1842)'TORSION',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ELSE
            WRITE(IOUT,1840)'TORSION',XK,XC,IFUNC,LSCALE,IFUNC2,F,IECROU,
     .                       A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ENDIF ! IF (IFAIL2 == 3)
        ENDIF ! IF (IECROU /= 5)
      ENDIF
!-----------------
      ! Rotation Y
!-----------------
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('FUN_A5',IFUNC,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('HFLAG5',IECROU,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_B5',IFV,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_C5',IFUNC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_D5',IFUNC3,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
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
      CALL HM_GET_FLOATV('Prop_FlxY_H',GF3,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('MIN_RUP5',CRIT_SCALE(5),IS_AVAILABLE,LSUBMODEL,UNITAB)
C
C----
!      IF (IFUNC /= 0 .AND. IECROU >= 1 .AND. XK == ZERO) THEN
!         CALL ANCMSG(MSGID=230,
!     .               MSGTYPE=MSGERROR,
!     .               ANMODE=ANINFO_BLIND_1,
!     .               I1=IG,
!     .               C1=IDTITL)
!      ENDIF
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
      IF (IFUNC == 0 .AND. A /= ZERO .AND. A /= ONE) THEN
        CALL ANCMSG(MSGID=663,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=IDTITL)
      ENDIF
C----
      IF (A == ZERO) A = ONE * A_UNIT
      IF (D == ZERO) D = ONE * D_UNIT
      IF (E == ZERO) E = ONE * E_UNIT
      IF (F == ZERO) F = ONE * F_UNIT
      IF (LSCALE == ZERO) LSCALE = ONE * LSCALE_UNIT
      IF (GF3 == ZERO) GF3 = ONE * GF3_UNIT
      IF (IFUNC == 0) THEN
        A = ONE
        B = ZERO
        E = ZERO
      ENDIF
C
      IF ((IFAIL2 == 0).OR.(IFAIL2 ==1)) THEN
        DN = DN * LSCALE / CRIT_SCALE(5)
        DX = DX * LSCALE / CRIT_SCALE(5)
      ENDIF
      IF (DN == ZERO) DN=-EP30* CRIT_SCALE(5)
      IF (DX == ZERO) DX= EP30* CRIT_SCALE(5)
      IF ((IFAIL2 == 0).OR.(IFAIL2 ==1)) CRIT_SCALE(5) = LSCALE / CRIT_SCALE(5)
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
C--   If H=6 - addditional internal variables must be stored in UVAR -  - GEO(25) = NUVAR
      IF (IECROU == 6) THEN
        GEO(25) = 6
      ENDIF
C
      IGEO(113) = IFUNC
      IGEO(114) = IFV
      IGEO(115) = IFUNC2
      IGEO(123) = IFUNC3
C----
      IF (.NOT. IS_ENCRYPTED) THEN
        IF (IECROU /= 5) THEN
          IF (IFAIL2 == 3) THEN
            WRITE(IOUT,1833)'Y FLEXION',XK,XC,IFUNC,LSCALE,IFUNC2,F,
     .                       IECROU,A,B,D,E,GF3,IFV,IFUNC3,DX
          ELSEIF (IFAIL2 == 2) THEN
            WRITE(IOUT,1832)'Y FLEXION',XK,XC,IFUNC,LSCALE,IFUNC2,F,
     .                       IECROU,A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ELSE
            WRITE(IOUT,1830)'Y FLEXION',XK,XC,IFUNC,LSCALE,IFUNC2,F,
     .                       IECROU,A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ENDIF ! IF (IFAIL2 == 3)
        ELSE
          IF (IFAIL2 == 3) THEN
            WRITE(IOUT,1843)'Y FLEXION',XK,XC,IFUNC,LSCALE,IFUNC2,F,
     .                       IECROU,A,B,D,E,GF3,IFV,IFUNC3,DX
          ELSEIF (IFAIL2 == 2) THEN
            WRITE(IOUT,1842)'Y FLEXION',XK,XC,IFUNC,LSCALE,IFUNC2,F,
     .                       IECROU,A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ELSE
            WRITE(IOUT,1840)'Y FLEXION',XK,XC,IFUNC,LSCALE,IFUNC2,F,
     .                       IECROU,A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ENDIF ! IF (IFAIL2 == 3)
        ENDIF ! IF (IECROU /= 5)
      ENDIF
!-----------------
      ! Rotation Z
!-----------------
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('FUN_A6',IFUNC,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('HFLAG6',IECROU,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_B6',IFV,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_C6',IFUNC2,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('FUN_D6',IFUNC3,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
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
      CALL HM_GET_FLOATV('Prop_FlxZ_H',GF3,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV_DIM('MIN_RUP6',CRIT_SCALE(6),IS_AVAILABLE,LSUBMODEL,UNITAB)
C
C----
!      IF (IFUNC /= 0 .AND. IECROU >= 1 .AND. XK == ZERO) THEN
!         CALL ANCMSG(MSGID=230,
!     .               MSGTYPE=MSGERROR,
!     .               ANMODE=ANINFO_BLIND_1,
!     .               I1=IG,
!     .               C1=IDTITL)
!      ENDIF
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
      IF (IFUNC == 0 .AND. A /= ZERO .AND. A /= ONE) THEN
        CALL ANCMSG(MSGID=663,
     .              MSGTYPE=MSGWARNING,
     .              ANMODE=ANINFO_BLIND_1,
     .              I1=IG,
     .              C1=IDTITL)
      ENDIF
C----
      IF (A == ZERO) A = ONE * A_UNIT
      IF (D == ZERO) D = ONE * D_UNIT
      IF (E == ZERO) E = ONE * E_UNIT
      IF (F == ZERO) F = ONE * F_UNIT
      IF (LSCALE == ZERO) LSCALE = ONE * LSCALE_UNIT
      IF (GF3 == ZERO) GF3 = ONE * GF3_UNIT
      IF (IFUNC == 0) THEN
        A = ONE
        B = ZERO
        E = ZERO
      ENDIF
C
      IF ((IFAIL2 == 0).OR.(IFAIL2 ==1)) THEN
        DN = DN * LSCALE / CRIT_SCALE(6)
        DX = DX * LSCALE / CRIT_SCALE(6)
      ENDIF
      IF (DN == ZERO) DN=-EP30* CRIT_SCALE(6)
      IF (DX == ZERO) DX= EP30* CRIT_SCALE(6)
      IF ((IFAIL2 == 0).OR.(IFAIL2 ==1)) CRIT_SCALE(6) = LSCALE / CRIT_SCALE(6)
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
C--   If H=6 - addditional internal variables must be stored in UVAR -  - GEO(25) = NUVAR
      IF (IECROU == 6) THEN
        GEO(25) = 6
      ENDIF
C
      IGEO(116) = IFUNC
      IGEO(117) = IFV
      IGEO(118) = IFUNC2
      IGEO(124) = IFUNC3
C----
      IF (.NOT. IS_ENCRYPTED) THEN
        IF (IECROU /= 5) THEN
          IF (IFAIL2 == 3) THEN
            WRITE(IOUT,1833)'Z FLEXION',XK,XC,IFUNC,LSCALE,IFUNC2,F,
     .                       IECROU,A,B,D,E,GF3,IFV,IFUNC3,DX
          ELSEIF (IFAIL2 == 2) THEN
            WRITE(IOUT,1832)'Z FLEXION',XK,XC,IFUNC,LSCALE,IFUNC2,F,
     .                       IECROU,A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ELSE
            WRITE(IOUT,1830)'Z FLEXION',XK,XC,IFUNC,LSCALE,IFUNC2,F,
     .                       IECROU,A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ENDIF ! IF (IFAIL2 == 3)
        ELSE
          IF (IFAIL2 == 3) THEN
            WRITE(IOUT,1843)'Z FLEXION',XK,XC,IFUNC,LSCALE,IFUNC2,F,
     .                       IECROU,A,B,D,E,GF3,IFV,IFUNC3,DX
          ELSEIF (IFAIL2 == 2) THEN
            WRITE(IOUT,1842)'Z FLEXION',XK,XC,IFUNC,LSCALE,IFUNC2,F,
     .                       IECROU,A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ELSE
            WRITE(IOUT,1840)'Z FLEXION',XK,XC,IFUNC,LSCALE,IFUNC2,F,
     .                       IECROU,A,B,D,E,GF3,IFV,IFUNC3,DN,DX
          ENDIF ! IF (IFAIL2 == 3)
        ENDIF ! IF (IECROU /= 5)
      ENDIF
!-------------------------------------------------------
c---   New uni/multiaxial failure criteria
!-------------------------------------------------------
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('ISRATE',ISRATE,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
      CALL HM_GET_FLOATV('TRANS_VEL0',VT0,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('ROT_VEL0',VR0,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Asrate',ASRATE,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('C1',CC(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('REL_VEL_EXP1',CN(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('ALPHA1',XA(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('BETA1',XB(1),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('C2',CC(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('REL_VEL_EXP2',CN(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('ALPHA2',XA(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('BETA2',XB(2),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('C3',CC(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('REL_VEL_EXP3',CN(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('ALPHA3',XA(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('BETA3',XB(3),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('C4',CC(4),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('REL_VEL_EXP4',CN(4),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('ALPHA4',XA(4),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('BETA4',XB(4),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('C5',CC(5),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('REL_VEL_EXP5',CN(5),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('ALPHA5',XA(5),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('BETA5',XB(5),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('C6',CC(6),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('REL_VEL_EXP6',CN(6),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('ALPHA6',XA(6),IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('BETA6',XB(6),IS_AVAILABLE,LSUBMODEL,UNITAB)
      !units for default values
      CALL HM_GET_FLOATV_DIM('TRANS_VEL0',VT0_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('ROT_VEL0',VR0_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
      CALL HM_GET_FLOATV_DIM('Asrate',ASR_UNIT,IS_AVAILABLE, LSUBMODEL, UNITAB)
C----
      IF (ASRATE == ZERO) ASRATE=EP30*ASR_UNIT
      IF (VT0 == ZERO) VT0 = ONE * VT0_UNIT
      IF (VR0 == ZERO) VR0 = ONE * VR0_UNIT
C
      DO J = 1,6
        IF (CN(J) == ZERO) CN(J) = ONE
        IF (XA(J) == ZERO) XA(J) = ONE
        IF (XB(J) == ZERO) XB(J) = TWO
      ENDDO
C----
      IF ((IFAIL2 == 0).OR.(IFAIL2 == 1)) THEN
        DO J = 1,6
          CC(J) = CC(J) * CRIT_SCALE(J)
        ENDDO
      ENDIF
C----
      GEO(101) = VT0
      GEO(102) = VR0
      GEO(103) = CC(1)
      GEO(104) = CC(2)
      GEO(105) = CC(3)
      GEO(106) = CC(4)
      GEO(107) = CC(5)
      GEO(108) = CC(6)
      GEO(109) = CN(1)
      GEO(110) = CN(2)
      GEO(111) = CN(3)
      GEO(112) = CN(4)
      GEO(113) = CN(5)
      GEO(114) = CN(6)
      GEO(115) = XA(1)
      GEO(116) = XA(2)
      GEO(117) = XA(3)
      GEO(118) = XA(4)
      GEO(119) = XA(5)
      GEO(120) = XA(6)
      GEO(121) = XB(1)
      GEO(122) = XB(2)
      GEO(123) = XB(3)
      GEO(124) = XB(4)
      GEO(125) = XB(5)
      GEO(126) = XB(6)
      GEO(127) = ISRATE
      GEO(128) = ASRATE
C
      IF (.NOT. IS_ENCRYPTED) THEN
        WRITE(IOUT,1850) VT0,VR0,
     .      (CC(J),J=1,6),(CN(J),J=1,6),(XA(J),J=1,6),(XB(J),J=1,6)
        WRITE(IOUT,1900) ISRATE,ASRATE
      ENDIF
C
      PROP_TAG(IGTYP)%G_EINT = 1
      PROP_TAG(IGTYP)%G_FOR  = 3
      PROP_TAG(IGTYP)%G_MOM = 3
      PROP_TAG(IGTYP)%G_LENGTH = 3
      PROP_TAG(IGTYP)%G_TOTDEPL = 3
      PROP_TAG(IGTYP)%G_TOTROT = 3
      PROP_TAG(IGTYP)%G_FOREP = 3
      PROP_TAG(IGTYP)%G_MOMEP = 3
      PROP_TAG(IGTYP)%G_DEP_IN_TENS = 3
      PROP_TAG(IGTYP)%G_DEP_IN_COMP = 3
      PROP_TAG(IGTYP)%G_ROT_IN_TENS = 3
      PROP_TAG(IGTYP)%G_ROT_IN_COMP = 3
      PROP_TAG(IGTYP)%G_POSX = 5
      PROP_TAG(IGTYP)%G_POSY = 5
      PROP_TAG(IGTYP)%G_POSZ = 5
      PROP_TAG(IGTYP)%G_POSXX = 5
      PROP_TAG(IGTYP)%G_POSYY = 5
      PROP_TAG(IGTYP)%G_POSZZ = 5
      PROP_TAG(IGTYP)%G_YIELD = 6
      PROP_TAG(IGTYP)%G_LENGTH_ERR = 3
      PROP_TAG(IGTYP)%G_SKEW = 3
      PROP_TAG(IGTYP)%G_SKEW_ERR = 3
      PROP_TAG(IGTYP)%G_E6 = 6
      PROP_TAG(IGTYP)%G_RUPTCRIT = 1
      PROP_TAG(IGTYP)%G_NUVAR = MAX(PROP_TAG(IGTYP)%G_NUVAR,NINT(GEO(25))) ! additional internal variables for h=6
      PROP_TAG(IGTYP)%G_DEFINI = 6
      PROP_TAG(IGTYP)%G_FORINI = 6
C
C------------------------
      RETURN
c-----------
 1800 FORMAT(
     & 5X,'SPRING PROPERTY SET (BEAM TYPE)'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'SPRING MASS . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING INERTIA. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SKEW FRAME NUMBER (0:GLOBAL). . . . . .=',I10/,
     & 5X,'SENSOR NUMBER (0:NOT USED). . . . . . .=',I10/,
     & 5X,'SENSOR FLAG (0:ACTIV 1:DEACT 2:BOTH). .=',I10/,
     & 5X,'FAILURE FLAG (0:UNCOUPLED 1:COUPLED). .=',I10/,
     & 5X,'UNIT LENGTH FLAG. . . . . . . . . . . .=',I10/,
     & 5X,'IF=1 UNIT LENGTH MASS,STIFFNESS AND INPUT',/,
     & 5X,'     CURVE ARE STRAIN DEPENDING',/)
 1810 FORMAT(
     & 5X,A,/,
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
     & 5X,'DYNAMIC AMPLIFICATION FACTOR GF3  . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'NEGATIVE FAILURE DISPLACEMENT . . . . .=',1PG20.13/,
     & 5X,'POSITIVE FAILURE DISPLACEMENT . . . . .=',1PG20.13/)
 1820 FORMAT(
     & 5X,A,/,
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
     & 5X,'DYNAMIC AMPLIFICATION FACTOR GF3  . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'NEGATIVE FAILURE DISPLACEMENT . . . . .=',1PG20.13/,
     & 5X,'POSITIVE FAILURE DISPLACEMENT . . . . .=',1PG20.13/)
 1830 FORMAT(
     & 5X,A,/,
     & 5X,'SPRING STIFFNESS. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING DAMPING. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR LOADING ',/,
     & 5X,'MOMENT-ROTATION CURVE . . . . . . . . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR UNLOADING ',/,
     & 5X,'MOMENT-ROTATION CURVE (H=4,5,7). . . . =',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'HARDENING FLAG H. . . . . . . . . . . .=',I10/,
     & 5X,'0:ELASTIC   1:ISOTROPIC 2:UNCOUPLED',/,
     & 5X,'4:KINEMATIC 5:UNCOUPLED NL (UN/RE)LOADING',/,
     & 5X,'6:ELASTO PLASTIC WITH HARDENING 7: ELASTIC HYSTERESIS',/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR A. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR B. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR D. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR E. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR GF3  . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'NEGATIVE FAILURE ROTATION . . . . . . .=',1PG20.13/,
     & 5X,'POSITIVE FAILURE ROTATION . . . . . . .=',1PG20.13/)
 1840 FORMAT(
     & 5X,A,/,
     & 5X,'SPRING STIFFNESS. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING DAMPING. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR LOADING ',/,
     & 5X,'MOMENT/ROTATION CURVE . . . . . . . . .=',I10/,
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
     & 5X,'DYNAMIC AMPLIFICATION FACTOR GF3  . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'NEGATIVE FAILURE ROTATION . . . . . . .=',1PG20.13/,
     & 5X,'POSITIVE FAILURE ROTATION . . . . . . .=',1PG20.13/)
 1801 FORMAT(
     & 5X,'SPRING PROPERTY SET (BEAM TYPE)'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'SPRING MASS . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING INERTIA. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SKEW FRAME NUMBER (0:GLOBAL). . . . . .=',I10/,
     & 5X,'SENSOR NUMBER (0:NOT USED). . . . . . .=',I10/,
     & 5X,'SENSOR FLAG (0:ACTIV 1:DEACT 2:BOTH). .=',I10/,
     & 5X,'FAILURE FLAG (0:UNCOUPLED 1:COUPLED). .=',I10/,
     & 5X,'FAILURE CRITERIUM (DISPL/FORCE/ENERGY).=',I10/,
     & 5X,'      1:DISPLACEMENT  2:FORCE  3:ENERGY '  ,/,
     & 5X,'UNIT LENGTH FLAG. . . . . . . . . . . .=',I10/,
     & 5X,'IF=1 UNIT LENGTH MASS,STIFFNESS AND INPUT',/,
     & 5X,'     CURVE ARE STRAIN DEPENDING',/)
 1812 FORMAT(
     & 5X,A,/,
     & 5X,'SPRING STIFFNESS. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING DAMPING. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR LOADING ',/,
     & 5X,'MOMENT/ROTATION CURVE. . . . . . . . . =',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR UNLOADING ',/,
     & 5X,'MOMENT/ROTATION CURVE (H=4,5,7). . . . =',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'HARDENING FLAG H. . . . . . . . . . . .=',I10/,
     & 5X,'0:ELASTIC   1:ISOTROPIC 2:UNCOUPLED',/,
     & 5X,'4:KINEMATIC 5:UNCOUPLED NL (UN/RE)LOADING',/,
     & 5X,'6:ELASTO PLASTIC WITH HARDENING 7: ELASTIC HYSTERESIS',/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR A. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR B. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR D. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR E. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR GF3  . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'NEGATIVE FAILURE FORCE. . . . . . . . .=',1PG20.13/,
     & 5X,'POSITIVE FAILURE FORCE. . . . . . . . .=',1PG20.13/)
 1813 FORMAT(
     & 5X,A,/,
     & 5X,'SPRING STIFFNESS. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING DAMPING. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR LOADING ',/,
     & 5X,'MOMENT/ROTATION CURVE . . . . . . . . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR UNLOADING ',/,
     & 5X,'MOMENT/ROTATION CURVE (H=4,5,7). . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'HARDENING FLAG H. . . . . . . . . . . .=',I10/,
     & 5X,'0:ELASTIC   1:ISOTROPIC 2:UNCOUPLED',/,
     & 5X,'4:KINEMATIC 5:UNCOUPLED NL (UN/RE)LOADING',/,
     & 5X,'6:ELASTO PLASTIC WITH HARDENING 7: ELASTIC HYSTERESIS',/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR A. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR B. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR D. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR E. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR GF3  . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FAILURE ENERGY. . . . . . . . . . . . .=',1PG20.13/)
 1822 FORMAT(
     & 5X,A,/,
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
     & 5X,'DYNAMIC AMPLIFICATION FACTOR GF3  . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'NEGATIVE FAILURE FORCE. . . . . . . . .=',1PG20.13/,
     & 5X,'POSITIVE FAILURE FORCE. . . . . . . . .=',1PG20.13/)
 1823 FORMAT(
     & 5X,A,/,
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
     & 5X,'DYNAMIC AMPLIFICATION FACTOR GF3  . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FAILURE ENERGY. . . . . . . . . . . . .=',1PG20.13/)
 1832 FORMAT(
     & 5X,A,/,
     & 5X,'SPRING STIFFNESS. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING DAMPING. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR LOADING ',/,
     & 5X,'MOMENT-ROTATION CURVE . . . . . . . . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR UNLOADING ',/,
     & 5X,'MOMENT-ROTATION CURVE (H=4,5,7). . . . =',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'HARDENING FLAG H. . . . . . . . . . . .=',I10/,
     & 5X,'0:ELASTIC   1:ISOTROPIC 2:UNCOUPLED',/,
     & 5X,'4:KINEMATIC 5:UNCOUPLED NL (UN/RE)LOADING',/,
     & 5X,'6:ELASTO PLASTIC WITH HARDENING 7: ELASTIC HYSTERESIS',/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR A. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR B. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR D. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR E. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR GF3  . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'NEGATIVE FAILURE MOMENT . . . . . . . .=',1PG20.13/,
     & 5X,'POSITIVE FAILURE MOMENT . . . . . . . .=',1PG20.13/)
 1833 FORMAT(
     & 5X,A,/,
     & 5X,'SPRING STIFFNESS. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING DAMPING. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR LOADING ',/,
     & 5X,'MOMENT-ROTATION CURVE . . . . . . . . .=',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR UNLOADING ',/,
     & 5X,'MOMENT-ROTATION CURVE (H=4,5,7). . . . =',I10/,
     & 5X,'ABSCISSA SCALE FACTOR ON CURVE . . . . =',1PG20.13/,
     & 5X,'HARDENING FLAG H. . . . . . . . . . . .=',I10/,
     & 5X,'0:ELASTIC   1:ISOTROPIC 2:UNCOUPLED',/,
     & 5X,'4:KINEMATIC 5:UNCOUPLED NL (UN/RE)LOADING',/,
     & 5X,'6:ELASTO PLASTIC WITH HARDENING 7: ELASTIC HYSTERESIS',/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR A. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR B. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR D. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR E. . . . .=',1PG20.13/,
     & 5X,'DYNAMIC AMPLIFICATION FACTOR GF3  . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FAILURE ENERGY. . . . . . . . . . . . .=',1PG20.13/)
 1842 FORMAT(
     & 5X,A,/,
     & 5X,'SPRING STIFFNESS. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING DAMPING. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR LOADING ',/,
     & 5X,'MOMENT-ROTATION CURVE . . . . . . . . .=',I10/,
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
     & 5X,'DYNAMIC AMPLIFICATION FACTOR GF3  . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'NEGATIVE FAILURE MOMENT . . . . . . . .=',1PG20.13/,
     & 5X,'POSITIVE FAILURE MOMENT . . . . . . . .=',1PG20.13/)
 1843 FORMAT(
     & 5X,A,/,
     & 5X,'SPRING STIFFNESS. . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING DAMPING. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR LOADING ',/,
     & 5X,'MOMENT-ROTATION CURVE . . . . . . . . .=',I10/,
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
     & 5X,'DYNAMIC AMPLIFICATION FACTOR GF3  . . .=',1PG20.13/,
     & 5X,'FUNCTION IDENTIFIER FOR  ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FUNCTION IDENTIFIER FOR THE ADDITIONAL ',/,
     & 5X,'FORCE-VELOCITY CURVE. . . . . . . . . .=',I10/,
     & 5X,'FAILURE ENERGY. . . . . . . . . . . . .=',1PG20.13/)
 1850 FORMAT(
     & 5X,'TRANSL. REF. DEFORMATION VELOCITY . . .=',1PG20.13/,
     & 5X,'ROT. REF. DEFORMATION VELOCITY. . . . .=',1PG20.13/,
     & 5X,'C1 COEFFICIENT. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C2 COEFFICIENT. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C3 COEFFICIENT. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C4 COEFFICIENT. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C5 COEFFICIENT. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'C6 COEFFICIENT. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'N1 EXPONENT . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'N2 EXPONENT . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'N3 EXPONENT . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'N4 EXPONENT . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'N5 EXPONENT . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'N6 EXPONENT . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'A1 COEFFICIENT. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'A2 COEFFICIENT. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'A3 COEFFICIENT. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'A4 COEFFICIENT. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'A5 COEFFICIENT. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'A6 COEFFICIENT. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'B1 EXPONENT . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'B2 EXPONENT . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'B3 EXPONENT . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'B4 EXPONENT . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'B5 EXPONENT . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'B6 EXPONENT . . . . . . . . . . . . . .=',1PG20.13/)
 1900 FORMAT(
     & 5X,'SMOOTH STRAIN RATE OPTION . . .. . . . =',I10/,
     & 5X,'STRAIN RATE CUTTING FREQUENCY .. . . . =',1PG20.13/)
c-----------
      RETURN
      END
