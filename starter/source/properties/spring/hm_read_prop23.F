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
!||    hm_read_prop23           ../starter/source/properties/spring/hm_read_prop23.F
!||--- called by ------------------------------------------------------
!||    hm_read_properties       ../starter/source/properties/hm_read_properties.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||--- uses       -----------------------------------------------------
!||    elbuftag_mod             ../starter/share/modules1/elbuftag_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_PROP23(GEO,IGEO,IG,IGTYP,UNITAB,
     .                          ISKN,TITR,LSUBMODEL,PROP_TAG,SUB_INDEX)
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD   
      USE ELBUFTAG_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "tablen_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IGEO(NPROPGI),ISKN(LISKN,*),IG,IGTYP
      INTEGER, INTENT(IN) :: SUB_INDEX
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
C     REAL
      my_real
     .   GEO(NPROPG)
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
      CHARACTER(LEN=NCHARTITLE)::TITR
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
       INTEGER ITYPE,ID_SKEW,ID_SENS,ISFLAG,K
C     REAL
      my_real
     .  INERTIA,VOL_AIR,PUN
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      DATA PUN/0.1/
C=======================================================================
C
      IS_ENCRYPTED = .FALSE.
      IS_AVAILABLE = .FALSE.
C
      IGEO(1)=IG
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
      CALL HM_GET_INTV('Imass',ITYPE,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('SKEW_CSID',ID_SKEW,IS_AVAILABLE,LSUBMODEL)
      IF(ID_SKEW == 0 .AND. SUB_INDEX /= 0 ) ID_SKEW = LSUBMODEL(SUB_INDEX)%SKEW
      CALL HM_GET_INTV('ISENSOR',ID_SENS,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('ISFLAG',ISFLAG,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
C     Default value of Imass is 2
      IF (ITYPE==0) ITYPE = 2
C      
      CALL HM_GET_FLOATV('INERTIA',INERTIA,IS_AVAILABLE,LSUBMODEL,UNITAB)
      IF (ITYPE == 1) THEN
        CALL HM_GET_FLOATV('AREA',VOL_AIR,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ELSE
        CALL HM_GET_FLOATV('Volume',VOL_AIR,IS_AVAILABLE,LSUBMODEL,UNITAB)
      ENDIF  
C
!----------------
!  verification
! -----------       
C   
      DO K = 0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
        IF (ID_SKEW == ISKN(4,K+1)) THEN
          ID_SKEW = K+1
          GO TO 100
        ENDIF
      ENDDO
      CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .            C1='PROPERTY',
     .            C2='PROPERTY',
     .            I1=IGEO(1),I2=ID_SKEW,C3=TITR)
100   CONTINUE      
C      
      IF (ISFLAG == 1) ID_SENS=-ID_SENS
C      
      IGEO(2)= ID_SKEW
      IGEO(3)= ID_SENS 
      IGEO(4)= ITYPE
      IGEO(5)= ISFLAG
C      
      GEO(1) = VOL_AIR
      GEO(2) = INERTIA
C
      PROP_TAG(IGTYP)%G_EINT = 1
      PROP_TAG(IGTYP)%G_FOR  = 3
      PROP_TAG(IGTYP)%G_MOM = 3
      PROP_TAG(IGTYP)%G_LENGTH = 3
      PROP_TAG(IGTYP)%G_FOREP = 3
      PROP_TAG(IGTYP)%G_MOMEP = 3
      PROP_TAG(IGTYP)%G_LENGTH_ERR = 3
      PROP_TAG(IGTYP)%G_SKEW = 3
      PROP_TAG(IGTYP)%G_SKEW_ERR = 3
      PROP_TAG(IGTYP)%G_E6 = 6
      PROP_TAG(IGTYP)%G_DEFINI = 6
      PROP_TAG(IGTYP)%G_FORINI = 6
      PROP_TAG(IGTYP)%G_RUPTCRIT = 1
C----
      IF(IS_ENCRYPTED)THEN
        WRITE(IOUT,1000)IG
 1000   FORMAT(
     &    5X,'SPRING PROPERTY SET'/,
     &    5X,'-------------------'/,
     &    5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     &    5X,'CONFIDENTIAL DATA'//)
      ELSE
        IF(ITYPE == 1) THEN
          WRITE(IOUT,1700)IG,VOL_AIR,INERTIA,ISKN(4,ID_SKEW),ABS(ID_SENS),ISFLAG
        ELSE
          WRITE(IOUT,1800)IG,VOL_AIR,INERTIA,ISKN(4,ID_SKEW),ABS(ID_SENS),ISFLAG
        ENDIF  
      ENDIF
C------------------------
      RETURN
c-----------
 1700 FORMAT(
     & 5X,'SPRING PROPERTY SET (BEAM TYPE)'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'SPRING AREA . . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING INERTIA. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SKEW FRAME ID . . . . . . . . . . . . .=',I10/,
     & 5X,'SENSOR NUMBER (0:NOT USED). . . . . . .=',I10/,
     & 5X,'SENSOR FLAG (0:ACTIV 1:DEACT 2:BOTH). .=',I10/) 
 1800 FORMAT(
     & 5X,'SPRING PROPERTY SET (BEAM TYPE)'/,
     & 5X,'PROPERTY SET NUMBER . . . . . . . . . .=',I10/,
     & 5X,'SPRING VOLUME . . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SPRING INERTIA. . . . . . . . . . . . .=',1PG20.13/,
     & 5X,'SKEW FRAME ID . . . . . . . . . . . . .=',I10/,
     & 5X,'SENSOR NUMBER (0:NOT USED). . . . . . .=',I10/,
     & 5X,'SENSOR FLAG (0:ACTIV 1:DEACT 2:BOTH). .=',I10/)
c-----------
      RETURN
      END
