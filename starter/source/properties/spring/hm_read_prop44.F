Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  HM_READ_PROP44                source/properties/spring/hm_read_prop44.F
Chd|-- called by -----------
Chd|        HM_READ_PROP_GENERIC          source/properties/hm_read_prop_generic.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        SET_U_GEO                     source/user_interface/uaccess.F
Chd|        SET_U_PNU                     source/user_interface/uaccess.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP44(IOUT  ,IG   ,NUVAR ,PARGEO,
     .                          UNITAB,ISKN  ,IGEO ,TITR  ,IGTYP ,
     .                          PROP_TAG,LSUBMODEL ,SUB_ID,IUNIT)
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE ELBUFTAG_MOD
      USE SUBMODEL_MOD 
C----------+---------+---+---+--------------------------------------------
C  Crushable frame spring property (old uer type spring)
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C IG       |  1      | I | R | PROPERTY NUMBER
C NUVAR    |  1      | I | W | NUMBER OF USER ELEMENT VARIABLES
C----------+---------+---+---+--------------------------------------------
C PARGEO   |  *      | F | W | 1)SKEW NUMBER
C          |         |   |   | 2)STIFNESS FOR INTERFACE
C          |         |   |   | 3)FRONT WAVE OPTION
C          |         |   |   | 4)... not yet used
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "submod_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "tablen_c.inc"
C----------+---------+---+---+--------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IOUT,IG,NUVAR,IGEO(NPROPGI),ISKN(LISKN,*),
     .        IGTYP,SUB_ID,IUNIT
      my_real   
     .        PARGEO(*)
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      CHARACTER*nchartitle,
     .   TITR
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IERROR,ISK,
     .        IFUN_XP,IFUN_XMI,IFUN_XXPI,IFUN_XXMI,IFUN_YY1PI,
     .        IFUN_YY1MI,IFUN_YY2PI,IFUN_YY2MI,IFUN_ZZ1PI,
     .        IFUN_ZZ1MI,IFUN_ZZ2PI,IFUN_ZZ2MI,
     .        IFUN_XMR,IFUN_XXPR,IFUN_XXMR,IFUN_YY1PR,
     .        IFUN_YY1MR,IFUN_YY2PR,IFUN_YY2MR,IFUN_ZZ1PR,
     .        IFUN_ZZ1MR,IFUN_ZZ2PR,IFUN_ZZ2MR,ICO,K,
     .        IFUN_DAMP_X,IFUN_DAMP_Y,IFUN_DAMP_Z,IFUN_DAMP_XX,
     .        IFUN_DAMP_YY,IFUN_DAMP_ZZ,NC_FILTER,IDAMP
      my_real 
     .        FSCAL_X,FSCAL_RX,FSCAL_RY1,FSCAL_RY2 ,FSCAL_RZ1,FSCAL_RZ2,
     .        AMAS,INER,XK,XCO,K11,K44,K55,K66,K5B,K6C,
     .        XLIMG,XLIM,XXLIM,YY1LIM,YY2LIM,ZZ1LIM,ZZ2LIM,
     .        FSCAL_DAMP_X,FSCAL_DAMP_Y,FSCAL_DAMP_Z,
     .        FSCAL_DAMP_XX,FSCAL_DAMP_YY,FSCAL_DAMP_ZZ,
     .        F_X,F_Y,F_Z,F_XX,F_YY,F_ZZ,RNC_FILTER,RDAMP,
     .        FSCAL_X_DIM,FSCAL_RX_DIM,FSCAL_RY1_DIM,
     .        FSCAL_RY2_DIM,FSCAL_RZ1_DIM,FSCAL_RZ2_DIM,
     .        INER_DIM,
     .        FSCAL_DAMP_X_DIM,FSCAL_DAMP_Y_DIM,FSCAL_DAMP_Z_DIM,
     .        FSCAL_DAMP_XX_DIM,FSCAL_DAMP_YY_DIM,FSCAL_DAMP_ZZ_DIM,
     .        F_X_DIM,F_Y_DIM,F_Z_DIM,F_XX_DIM,F_YY_DIM,F_ZZ_DIM
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C-----------------------------------------------
      INTEGER    KFUNC
      PARAMETER (KFUNC=29)
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER   SET_U_PNU,SET_U_GEO
      EXTERNAL  SET_U_PNU,SET_U_GEO
C=======================================================================
C
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C
      NUVAR = 42   ! damping (linear or function) + strain rate filtering
      RNC_FILTER = ZERO
      IDAMP = 0
      RDAMP = ZERO
      IFUN_DAMP_X = 0
      IFUN_DAMP_Y = 0
      IFUN_DAMP_Z = 0
      IFUN_DAMP_XX = 0 
      IFUN_DAMP_YY = 0 
      IFUN_DAMP_ZZ = 0 
      FSCAL_DAMP_X = 0 
      FSCAL_DAMP_Y = 0 
      FSCAL_DAMP_Z = 0 
      FSCAL_DAMP_XX = ZERO 
      FSCAL_DAMP_YY = ZERO 
      FSCAL_DAMP_ZZ = ZERO 
      F_X  = ZERO
      F_Y  = ZERO 
      F_Z  = ZERO 
      F_XX = ZERO 
      F_YY = ZERO 
      F_ZZ = ZERO 
C
Card1   
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('SKEW_CSID',ISK,IS_AVAILABLE,LSUBMODEL)
        IF(ISK == 0 .AND. SUB_ID /= 0 ) ISK = LSUBMODEL(SUB_ID)%SKEW
        CALL HM_GET_INTV('Icoupling',ICO,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('Ifiltr',NC_FILTER,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('MASS',AMAS,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('INERTIA',INER,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('INERTIA',INER_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('STIFF1',XK,IS_AVAILABLE,LSUBMODEL,UNITAB)
Card2   - stiffness in traction, torsion, flex_y, flex_z
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('IDAMP',IDAMP,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('K11',K11,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('K44',K44,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('K55',K55,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('K66',K66,IS_AVAILABLE,LSUBMODEL,UNITAB)
Card3   stiffness in flexion yy and zz (off diagonal)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('K5b',K5B,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('K6c',K6C,IS_AVAILABLE,LSUBMODEL,UNITAB)
Card4   trans_x (traction)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('FUN_A1',IFUN_XP,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('FUN_B1',IFUN_XMI,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('FUN_A2',IFUN_XMR,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('FScale11',FSCAL_X,IS_AVAILABLE,LSUBMODEL,UNITAB)            
        CALL HM_GET_FLOATV_DIM('FScale11',FSCAL_X_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)            
Card5   rot_x (torsion)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('FUN_B2',IFUN_XXPI,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('FUN_A3',IFUN_XXMI,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('FUN_B3',IFUN_XXPR,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('FUN_A4',IFUN_XXMR,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('FScale22',FSCAL_RX,IS_AVAILABLE,LSUBMODEL,UNITAB)             
        CALL HM_GET_FLOATV_DIM('FScale22',FSCAL_RX_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)             
Card6   rot_y1  (flexion, torsion)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('FUN_B4',IFUN_YY1PI,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('FUN_A5',IFUN_YY1MI,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('FUN_B5',IFUN_YY1PR,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('FUN_A6',IFUN_YY1MR,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('FScale33',FSCAL_RY1,IS_AVAILABLE,LSUBMODEL,UNITAB)   
        CALL HM_GET_FLOATV_DIM('FScale33',FSCAL_RY1_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)   
Card7   rot_z1  (flexion, torsion)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('FUN_B6',IFUN_ZZ1PI,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('FUN_C1',IFUN_ZZ1MI,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('FUN_C2',IFUN_ZZ1PR,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('FUN_C3',IFUN_ZZ1MR,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('FScale12',FSCAL_RZ1,IS_AVAILABLE,LSUBMODEL,UNITAB)  
        CALL HM_GET_FLOATV_DIM('FScale12',FSCAL_RZ1_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)  
Card8   rot_y2  (flexion, torsion)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('FUN_C4',IFUN_YY2PI,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('FUN_C5',IFUN_YY2MI,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('FUN_C6',IFUN_YY2PR,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('FUN_D1',IFUN_YY2MR,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('FScale23',FSCAL_RY2,IS_AVAILABLE,LSUBMODEL,UNITAB)         
        CALL HM_GET_FLOATV_DIM('FScale23',FSCAL_RY2_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)         
Card9   rot_z2  (flexion, torsion)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('FUN_D2',IFUN_ZZ2PI,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('FUN_D3',IFUN_ZZ2MI,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('FUN_D4',IFUN_ZZ2PR,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('FUN_D5',IFUN_ZZ2MR,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('FScale13',FSCAL_RZ2,IS_AVAILABLE,LSUBMODEL,UNITAB)   
        CALL HM_GET_FLOATV_DIM('FScale13',FSCAL_RZ2_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)   
Card10
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('STRAIN1',XLIMG,IS_AVAILABLE,LSUBMODEL,UNITAB)      
        CALL HM_GET_FLOATV('STRAIN2',XLIM,IS_AVAILABLE,LSUBMODEL,UNITAB)      
        CALL HM_GET_FLOATV('STRAIN3',XXLIM,IS_AVAILABLE,LSUBMODEL,UNITAB)      
Card11
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('STRAIN4',YY1LIM,IS_AVAILABLE,LSUBMODEL,UNITAB)  
        CALL HM_GET_FLOATV('STRAIN5',ZZ1LIM,IS_AVAILABLE,LSUBMODEL,UNITAB)  
        CALL HM_GET_FLOATV('STRAIN6',YY2LIM,IS_AVAILABLE,LSUBMODEL,UNITAB)  
        CALL HM_GET_FLOATV('STRAIN7',ZZ2LIM,IS_AVAILABLE,LSUBMODEL,UNITAB)  
!-----------------------------------------------------------------------
      IF (IDAMP > 0) THEN
Card12
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('FCT_D_X',IFUN_DAMP_X,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('DSCALE_X',FSCAL_DAMP_X,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('F_X',F_X,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('DSCALE_X',FSCAL_DAMP_X_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('F_X',F_X_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)
Card13
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('FCT_D_Y',IFUN_DAMP_Y,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('DSCALE_Y',FSCAL_DAMP_Y,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('F_Y',F_Y,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('DSCALE_Y',FSCAL_DAMP_Y_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('F_Y',F_Y_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)
Card14
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('FCT_D_Z',IFUN_DAMP_Z,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('DSCALE_Z',FSCAL_DAMP_Z,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('F_Z',F_Z,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('DSCALE_Z',FSCAL_DAMP_Z_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('F_Z',F_Z_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)
Card15
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('FCT_D_XX',IFUN_DAMP_XX,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('DSCALE_XX',FSCAL_DAMP_XX,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('F_XX',F_XX,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('DSCALE_XX',FSCAL_DAMP_XX_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('F_XX',F_XX_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)
Card16
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('FCT_D_YY',IFUN_DAMP_YY,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('DSCALE_YY',FSCAL_DAMP_YY,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('F_YY',F_YY,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('DSCALE_YY',FSCAL_DAMP_YY_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('F_YY',F_YY_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)
Card17
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('FCT_D_ZZ',IFUN_DAMP_ZZ,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('DSCALE_ZZ',FSCAL_DAMP_ZZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('F_ZZ',F_ZZ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('DSCALE_ZZ',FSCAL_DAMP_ZZ_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('F_ZZ',F_ZZ_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)
!
      ENDIF ! IF (IDAMP > 0)
!-----------------------------------------------------------------------
      IF (FSCAL_X   == ZERO) FSCAL_X   = ONE * FSCAL_X_DIM
      IF (FSCAL_RX  == ZERO) FSCAL_RX  = ONE * FSCAL_RX_DIM
      IF (FSCAL_RY1 == ZERO) FSCAL_RY1 = ONE * FSCAL_RY1_DIM
      IF (FSCAL_RY2 == ZERO) FSCAL_RY2 = ONE * FSCAL_RY2_DIM
      IF (FSCAL_RZ1 == ZERO) FSCAL_RZ1 = ONE * FSCAL_RZ1_DIM
      IF (FSCAL_RZ2 == ZERO) FSCAL_RZ2 = ONE * FSCAL_RZ2_DIM
!
      IF(INER < EM20) THEN                   
         INER = EM20 * INER_DIM                        
         CALL ANCMSG(MSGID=445,
     .               MSGTYPE=MSGWARNING,
     .               ANMODE=ANINFO_BLIND_1,
     .               I1=IG,
     .               C1=TITR)
      ENDIF                                  
C--------------------------------------------
! strain rate filtering (by default = 12 cycles):
      IF (NC_FILTER > 0) NC_FILTER = 12
C--------------------------------------------
       DO K=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
         IF(ISK == ISKN(4,K+1)) THEN
           ISK=K+1
           GO TO 100
         ENDIF
       ENDDO
       CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .             C1='PROPERTY',
     .             C2='PROPERTY',
     .             I1=IGEO(1),I2=ISK,C3=TITR)
100    CONTINUE
      PARGEO(1) = ISK
      PARGEO(2) = XK
C     front wave = 1
      PARGEO(3) = 1
C--------------------------------------------
      IF (IFUN_XP <= 0)WRITE(IOUT,*)
     .         ' **ERROR ZERO FUNCTION NUMBER X+'
      IF (IFUN_XMI <= 0)WRITE(IOUT,*)
     .         ' **ERROR ZERO FUNCTION NUMBER Xi-'
      IF (IFUN_XXPI <= 0)WRITE(IOUT,*)
     .         ' **ERROR ZERO FUNCTION NUMBER XXi+'
      IF (IFUN_XXMI <= 0)WRITE(IOUT,*)
     .         ' **ERROR ZERO FUNCTION NUMBER XXi-'
      IF (IFUN_YY1PI <= 0)WRITE(IOUT,*)
     .         ' **ERROR ZERO FUNCTION NUMBER YY1i+'
      IF (IFUN_YY1MI <= 0)WRITE(IOUT,*)
     .         ' **ERROR ZERO FUNCTION NUMBER YY1i-'
!
      IF (IDAMP > 0) THEN
        IF (IFUN_DAMP_X <= 0)WRITE(IOUT,*)
     .           ' **ERROR ZERO FUNCTION NUMBER DAMP_X   ---> Linear damping is used'
        IF (IFUN_DAMP_Y <= 0)WRITE(IOUT,*)
     .           ' **ERROR ZERO FUNCTION NUMBER DAMP_Y   ---> Linear damping is used'
        IF (IFUN_DAMP_Z <= 0)WRITE(IOUT,*)
     .           ' **ERROR ZERO FUNCTION NUMBER DAMP_Z   ---> Linear damping is used'
        IF (IFUN_DAMP_XX <= 0)WRITE(IOUT,*)
     .           ' **ERROR ZERO FUNCTION NUMBER DAMP_XX  ---> Linear damping is used'
        IF (IFUN_DAMP_YY <= 0)WRITE(IOUT,*)
     .           ' **ERROR ZERO FUNCTION NUMBER DAMP_YY  ---> Linear damping is used'
        IF (IFUN_DAMP_ZZ <= 0)WRITE(IOUT,*)
     .           ' **ERROR ZERO FUNCTION NUMBER DAMP_ZZ  ---> Linear damping is used'
      ENDIF ! IF (IDAMP > 0)
!
      IF (IFUN_XMR   <= 0)IFUN_XMR  =IFUN_XMI
      IF (IFUN_XXPR  <= 0)IFUN_XXPR =IFUN_XXPI
      IF (IFUN_XXMR  <= 0)IFUN_XXMR =IFUN_XXMI
      IF (IFUN_YY1PR <= 0)IFUN_YY1PR=IFUN_YY1PI
      IF (IFUN_YY2PI <= 0)IFUN_YY2PI=IFUN_YY1PI
      IF (IFUN_YY2PR <= 0)IFUN_YY2PR=IFUN_YY1PR
      IF (IFUN_ZZ1PI <= 0)IFUN_ZZ1PI=IFUN_YY1PI
      IF (IFUN_ZZ1PR <= 0)IFUN_ZZ1PR=IFUN_ZZ1PI
      IF (IFUN_ZZ2PI <= 0)IFUN_ZZ2PI=IFUN_ZZ1PI
      IF (IFUN_ZZ2PR <= 0)IFUN_ZZ2PR=IFUN_ZZ1PR
      IF (IFUN_YY1MR <= 0)IFUN_YY1MR=IFUN_YY1MI
      IF (IFUN_YY2MI <= 0)IFUN_YY2MI=IFUN_YY1MI
      IF (IFUN_YY2MR <= 0)IFUN_YY2MR=IFUN_YY1MR
      IF (IFUN_ZZ1MI <= 0)IFUN_ZZ1MI=IFUN_YY1MI
      IF (IFUN_ZZ1MR <= 0)IFUN_ZZ1MR=IFUN_ZZ1MI
      IF (IFUN_ZZ2MI <= 0)IFUN_ZZ2MI=IFUN_ZZ1MI
      IF (IFUN_ZZ2MR <= 0)IFUN_ZZ2MR=IFUN_ZZ1MR
C---
      IERROR = SET_U_GEO(1,XLIMG)
      IERROR = SET_U_GEO(2,XLIM)
      IERROR = SET_U_GEO(3,XXLIM)
      IERROR = SET_U_GEO(4,YY1LIM)
      IERROR = SET_U_GEO(5,ZZ1LIM)
      IERROR = SET_U_GEO(6,YY2LIM)
      IERROR = SET_U_GEO(7,ZZ2LIM)
      IERROR = SET_U_GEO(8,AMAS)
      IERROR = SET_U_GEO(9,INER)
      IERROR = SET_U_GEO(10,K11)
      IERROR = SET_U_GEO(11,K44)
      IERROR = SET_U_GEO(12,K55)
      IERROR = SET_U_GEO(13,K66)
      IERROR = SET_U_GEO(14,K5B)
      IERROR = SET_U_GEO(15,K6C)
      XCO = ICO
      IERROR = SET_U_GEO(16,XCO)
      IERROR = SET_U_GEO(17,FSCAL_X)
      IERROR = SET_U_GEO(18,FSCAL_RX)
      IERROR = SET_U_GEO(19,FSCAL_RY1)
      IERROR = SET_U_GEO(20,FSCAL_RY2)
      IERROR = SET_U_GEO(21,FSCAL_RZ1)
      IERROR = SET_U_GEO(22,FSCAL_RZ2)
C
      IERROR = SET_U_PNU(1,IFUN_XMI,KFUNC)
      IERROR = SET_U_PNU(2,IFUN_XXMI,KFUNC)
      IERROR = SET_U_PNU(3,IFUN_YY1MI,KFUNC)
      IERROR = SET_U_PNU(4,IFUN_ZZ1MI,KFUNC)
      IERROR = SET_U_PNU(5,IFUN_YY2MI,KFUNC)
      IERROR = SET_U_PNU(6,IFUN_ZZ2MI,KFUNC)
      IERROR = SET_U_PNU(7,IFUN_XP,KFUNC)
      IERROR = SET_U_PNU(8,IFUN_XXPI,KFUNC)
      IERROR = SET_U_PNU(9,IFUN_YY1PI,KFUNC)

      IERROR = SET_U_PNU(10,IFUN_ZZ1PI,KFUNC)
      IERROR = SET_U_PNU(11,IFUN_YY2PI,KFUNC)
      IERROR = SET_U_PNU(12,IFUN_ZZ2PI,KFUNC)
      IERROR = SET_U_PNU(13,IFUN_XMR,KFUNC)
      IERROR = SET_U_PNU(14,IFUN_XXMR,KFUNC)
      IERROR = SET_U_PNU(15,IFUN_YY1MR,KFUNC)
      IERROR = SET_U_PNU(16,IFUN_ZZ1MR,KFUNC)
      IERROR = SET_U_PNU(17,IFUN_YY2MR,KFUNC)
      IERROR = SET_U_PNU(18,IFUN_ZZ2MR,KFUNC)
      IERROR = SET_U_PNU(19,IFUN_XXPR,KFUNC)
      IERROR = SET_U_PNU(20,IFUN_YY1PR,KFUNC)
      IERROR = SET_U_PNU(21,IFUN_ZZ1PR,KFUNC)
      IERROR = SET_U_PNU(22,IFUN_YY2PR,KFUNC)
      IERROR = SET_U_PNU(23,IFUN_ZZ2PR,KFUNC)
!--------------------------------------------
      IF (IDAMP > 0) THEN
! function damping
        IERROR = SET_U_PNU(24,IFUN_DAMP_X,KFUNC)
        IERROR = SET_U_PNU(25,IFUN_DAMP_Y,KFUNC)
        IERROR = SET_U_PNU(26,IFUN_DAMP_Z,KFUNC)
        IERROR = SET_U_PNU(27,IFUN_DAMP_XX,KFUNC)
        IERROR = SET_U_PNU(28,IFUN_DAMP_YY,KFUNC)
        IERROR = SET_U_PNU(29,IFUN_DAMP_ZZ,KFUNC)
! damping
        IF (F_X  == ZERO)  F_X  = ONE * F_X_DIM
        IF (F_Y  == ZERO)  F_Y  = ONE * F_Y_DIM
        IF (F_Z  == ZERO)  F_Z  = ONE * F_Z_DIM
        IF (F_XX == ZERO)  F_XX = ONE * F_XX_DIM
        IF (F_YY == ZERO)  F_YY = ONE * F_YY_DIM
        IF (F_ZZ == ZERO)  F_ZZ = ONE * F_ZZ_DIM
!
        IF (FSCAL_DAMP_X  == ZERO)  FSCAL_DAMP_X  = ONE * FSCAL_DAMP_X_DIM
        IF (FSCAL_DAMP_Y  == ZERO)  FSCAL_DAMP_Y  = ONE * FSCAL_DAMP_Y_DIM
        IF (FSCAL_DAMP_Z  == ZERO)  FSCAL_DAMP_Z  = ONE * FSCAL_DAMP_Z_DIM
        IF (FSCAL_DAMP_XX == ZERO)  FSCAL_DAMP_XX = ONE * FSCAL_DAMP_XX_DIM
        IF (FSCAL_DAMP_YY == ZERO)  FSCAL_DAMP_YY = ONE * FSCAL_DAMP_YY_DIM
        IF (FSCAL_DAMP_ZZ == ZERO)  FSCAL_DAMP_ZZ = ONE * FSCAL_DAMP_ZZ_DIM
!
        IERROR = SET_U_GEO(23,FSCAL_DAMP_X)
        IERROR = SET_U_GEO(24,FSCAL_DAMP_Y)
        IERROR = SET_U_GEO(25,FSCAL_DAMP_Z)
        IERROR = SET_U_GEO(26,FSCAL_DAMP_XX)
        IERROR = SET_U_GEO(27,FSCAL_DAMP_YY)
        IERROR = SET_U_GEO(28,FSCAL_DAMP_ZZ)
!
        IERROR = SET_U_GEO(29,F_X)
        IERROR = SET_U_GEO(30,F_Y)
        IERROR = SET_U_GEO(31,F_Z)
        IERROR = SET_U_GEO(32,F_XX)
        IERROR = SET_U_GEO(33,F_YY)
        IERROR = SET_U_GEO(34,F_ZZ)
      ENDIF ! IF (IDAMP > 0)
!
        RNC_FILTER = NC_FILTER
        IERROR = SET_U_GEO(35,RNC_FILTER)
        RDAMP = IDAMP
        IERROR = SET_U_GEO(36,RDAMP)
C--------------------------------------------
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,'(5X,A,//)')'CONFIDENTIAL DATA'
      ELSE
        WRITE(IOUT,1000)
     .        AMAS,INER,XK,ISK,ICO,K11,K44,K55,K66,K5B,K6C,
     .        IFUN_XP,IFUN_XMI,IFUN_XMR,                                          
     .        IFUN_XXPI,IFUN_XXMI,IFUN_XXPR,IFUN_XXMR,                            
     .        IFUN_YY1PI,IFUN_YY1MI,IFUN_YY1PR,IFUN_YY1MR,                        
     .        IFUN_ZZ1PI,IFUN_ZZ1MI,IFUN_ZZ1PR,IFUN_ZZ1MR,                        
     .        IFUN_YY2PI,IFUN_YY2MI,IFUN_YY2PR,IFUN_YY2MR,                        
     .        IFUN_ZZ2PI,IFUN_ZZ2MI,IFUN_ZZ2PR,IFUN_ZZ2MR,                        
     .        FSCAL_X,FSCAL_RX,FSCAL_RY1,FSCAL_RY2 ,FSCAL_RZ1,FSCAL_RZ2,          
     .        XLIMG,XLIM,XXLIM,YY1LIM,ZZ1LIM,YY2LIM,ZZ2LIM,
     .        NC_FILTER,IFUN_DAMP_X,IFUN_DAMP_Y,IFUN_DAMP_Z,
     .        IFUN_DAMP_XX,IFUN_DAMP_YY,IFUN_DAMP_ZZ,
     .        FSCAL_DAMP_X,FSCAL_DAMP_Y,FSCAL_DAMP_Z,
     .        FSCAL_DAMP_XX,FSCAL_DAMP_YY,FSCAL_DAMP_ZZ,
     .        F_X,F_Y,F_Z,F_XX,F_YY,F_ZZ
      ENDIF
C
      PROP_TAG(IGTYP)%G_EINT = 1
      PROP_TAG(IGTYP)%G_FOR  = 3
      PROP_TAG(IGTYP)%G_MOM = 5
      PROP_TAG(IGTYP)%G_SKEW = 6
      PROP_TAG(IGTYP)%G_MASS = 1
      PROP_TAG(IGTYP)%G_V_REPCVT  = 3 ! -- VITESSES REPERE CONVECTEE (V_REPCVT)
      PROP_TAG(IGTYP)%G_VR_REPCVT = 3  ! -- VITESSES REPERE CONVECTEE (VR_REPCVT)
      PROP_TAG(IGTYP)%G_NUVAR = NUVAR
C
C-----
 1000 FORMAT(
     & 5X,'MASS PER UNIT LENGTH. . . . . . . . . .=',E12.4/,
     & 5X,'INERTIA PER UNIT LENGTH . . . . . . . .=',E12.4/,
     & 5X,'STIFFNESS FOR INTERFACE K=E*A/L . . . .=',E12.4/,
     & 5X,'SKEW FRAME ID . . . . . . . . . . . . .=',I8/,
     & 5X,'COUPLING FLAG . . . . . . . . . . . . .=',I8/,
     & 5X,'STIFFNESS K11=E*A (TENSION COMPRESSION)=',E12.4/,
     & 5X,'STIFFNESS K44=G*Ix (TORSION). . . . . .=',E12.4/,
     & 5X,'STIFFNESS K55=4*E*Iy (Y11 BENDING). . .=',E12.4/,
     & 5X,'STIFFNESS K66=4*E*Iz (Z11 BENDING). . .=',E12.4/,
     & 5X,'STIFFNESS K5B=2*E*Iy (Y12 BENDING). . .=',E12.4/,
     & 5X,'STIFFNESS K6C=2*E*Iz (Z12 BENDING). . .=',E12.4/,
     & 5X,'TRACTION USER FUNCTION ID . . . . . . .=',I8/,
     & 5X,'INITIAL COMPRESSION USER FUNCTION ID. .=',I8/,
     & 5X,'FINAL COMPRESSION USER FUNCTION ID. . .=',I8/,
     & 5X,'INITIAL MAX TORSION USER FUNCTION ID. .=',I8/,
     & 5X,'INITIAL MIN TORSION USER FUNCTION ID. .=',I8/,
     & 5X,'FINAL MAX TORSION USER FUNCTION ID. . .=',I8/,
     & 5X,'FINAL MIN TORSION USER FUNCTION ID. . .=',I8/,
     & 5X,'INITIAL MAX Y BENDING NODE 1 FUNC. ID .=',I8/,
     & 5X,'INITIAL MIN Y BENDING NODE 1 FUNC. ID .=',I8/,
     & 5X,'FINAL MAX Y BENDING NODE 1 FUNC. ID . .=',I8/,
     & 5X,'FINAL MIN Y BENDING NODE 1 FUNC. ID . .=',I8/,
     & 5X,'INITIAL MAX Z BENDING NODE 1 FUNC. ID .=',I8/,
     & 5X,'INITIAL MIN Z BENDING NODE 1 FUNC. ID .=',I8/,
     & 5X,'FINAL MAX Z BENDING NODE 1 FUNC. ID . .=',I8/,
     & 5X,'FINAL MIN Z BENDING NODE 1 FUNC. ID . .=',I8/,
     & 5X,'INITIAL MAX Y BENDING NODE 2 FUNC. ID .=',I8/,
     & 5X,'INITIAL MIN Y BENDING NODE 2 FUNC. ID .=',I8/,
     & 5X,'FINAL MAX Y BENDING NODE 2 FUNC. ID . .=',I8/,
     & 5X,'FINAL MIN Y BENDING NODE 2 FUNC. ID . .=',I8/,
     & 5X,'INITIAL MAX Z BENDING NODE 2 FUNC. ID .=',I8/,
     & 5X,'INITIAL MIN Z BENDING NODE 2 FUNC. ID .=',I8/,
     & 5X,'FINAL MAX Z BENDING NODE 2 FUNC. ID . .=',I8/,
     & 5X,'FINAL MIN Z BENDING NODE 2 FUNC. ID . .=',I8/,
     & 5X,'SCALE FACTOR FOR TRACTION FUNCTIONS . .=',E12.4/,
     & 5X,'SCALE FACTOR FOR TORSION X FUNCTIONS. .=',E12.4/,
     & 5X,'SCALE FACTOR FOR BENDING Y AT NODE 1. .=',E12.4/,
     & 5X,'SCALE FACTOR FOR BENDING Z AT NODE 1. .=',E12.4/,
     & 5X,'SCALE FACTOR FOR BENDING Y AT NODE 2. .=',E12.4/,
     & 5X,'SCALE FACTOR FOR BENDING Z AT NODE 2. .=',E12.4/,
     & 5X,'GLOBAL COMPRESSION TRANSITION DEF.. . .=',E12.4/,
     & 5X,'LOCAL COMPRESSION TRANSITION DEF. . . .=',E12.4/,
     & 5X,'LOCAL TORSION TRANSITION DEFORMATION. .=',E12.4/,
     & 5X,'LOCAL Y BENDING NODE 1 TRANSITION DEF..=',E12.4/,
     & 5X,'LOCAL Z BENDING NODE 1 TRANSITION DEF..=',E12.4/,
     & 5X,'LOCAL Y BENDING NODE 2 TRANSITION DEF..=',E12.4/,
     & 5X,'LOCAL Z BENDING NODE 2 TRANSITION DEF..=',E12.4/,
!---
! filtering
     & 5X,'SMOOTH STRAIN RATE FILTERING. .. . . . =',I10/,
! damping
     & 5X,'DAMPING FORCE-ELONGATION RATE CURVE FOR X  DOF . . . .=',I10/,
     & 5X,'DAMPING FORCE-ELONGATION RATE CURVE FOR Y  DOF . . . .=',I10/,
     & 5X,'DAMPING FORCE-ELONGATION RATE CURVE FOR Z  DOF . . . .=',I10/,
     & 5X,'DAMPING MOMENT-ROTATION  RATE CURVE FOR XX DOF . . . .=',I10/,
     & 5X,'DAMPING MOMENT-ROTATION  RATE CURVE FOR YY DOF . . . .=',I10/,
     & 5X,'DAMPING MOMENT-ROTATION  RATE CURVE FOR ZZ DOF . . . .=',I10/,
     & 5X,'LINEAR/FUNCTION DAMPING SCALE FACTOR FOR X  DOF. . . .=',1PG20.13/,
     & 5X,'LINEAR/FUNCTION DAMPING SCALE FACTOR FOR Y  DOF. . . .=',1PG20.13/,
     & 5X,'LINEAR/FUNCTION DAMPING SCALE FACTOR FOR Z  DOF. . . .=',1PG20.13/,
     & 5X,'LINEAR/FUNCTION DAMPING SCALE FACTOR FOR XX DOF. . . .=',1PG20.13/,
     & 5X,'LINEAR/FUNCTION DAMPING SCALE FACTOR FOR YY DOF. . . .=',1PG20.13/,
     & 5X,'LINEAR/FUNCTION DAMPING SCALE FACTOR FOR ZZ DOF. . . .=',1PG20.13/,
     & 5X,'ABSCISSA SCALE FACTOR ON FORCE-ELONGATION RATE CURVE X  DOF. . =',1PG20.13/,
     & 5X,'ABSCISSA SCALE FACTOR ON FORCE-ELONGATION RATE CURVE Y  DOF. . =',1PG20.13/,
     & 5X,'ABSCISSA SCALE FACTOR ON FORCE-ELONGATION RATE CURVE Z  DOF. . =',1PG20.13/,
     & 5X,'ABSCISSA SCALE FACTOR ON MOMENT-ROTATION  RATE CURVE XX DOF. . =',1PG20.13/,
     & 5X,'ABSCISSA SCALE FACTOR ON MOMENT-ROTATION  RATE CURVE YY DOF. . =',1PG20.13/,
     & 5X,'ABSCISSA SCALE FACTOR ON MOMENT-ROTATION  RATE CURVE ZZ DOF. . =',1PG20.13//)
!---
C-----------      
      RETURN
      END SUBROUTINE HM_READ_PROP44
