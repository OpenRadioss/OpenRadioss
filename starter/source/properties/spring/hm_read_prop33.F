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
Chd|  HM_READ_PROP33                source/properties/spring/hm_read_prop33.F
Chd|-- called by -----------
Chd|        HM_READ_PROP_GENERIC          source/properties/hm_read_prop_generic.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FREERR                        source/starter/freform.F      
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_READ_PROP33_CYL_JNT        source/properties/spring/hm_read_prop33_cyl_jnt.F
Chd|        HM_READ_PROP33_FIX_JNT        source/properties/spring/hm_read_prop33_fix_jnt.F
Chd|        HM_READ_PROP33_FREE_JNT       source/properties/spring/hm_read_prop33_free_jnt.F
Chd|        HM_READ_PROP33_OLD_JNT        source/properties/spring/hm_read_prop33_old_jnt.F
Chd|        HM_READ_PROP33_PLAN_JNT       source/properties/spring/hm_read_prop33_plan_jnt.F
Chd|        HM_READ_PROP33_REV_JNT        source/properties/spring/hm_read_prop33_rev_jnt.F
Chd|        HM_READ_PROP33_SPH_JNT        source/properties/spring/hm_read_prop33_sph_jnt.F
Chd|        HM_READ_PROP33_TRANS_JNT      source/properties/spring/hm_read_prop33_trans_jnt.F
Chd|        HM_READ_PROP33_UNIV_JNT       source/properties/spring/hm_read_prop33_univ_jnt.F
Chd|        ELBUFTAG_MOD                  share/modules1/elbuftag_mod.F 
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_PROP33(IOUT ,NUVAR ,PARGEO ,UNITAB,
     .                  IUNIT,ID    ,TITR   ,PROP_TAG,
     .                  IGTYP,LSUBMODEL)
C-----------------------------------------------
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
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "tablen_c.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C----------+---------+---+---+--------------------------------------------
C VAR      | SIZE    |TYP| RW| DEFINITION
C----------+---------+---+---+--------------------------------------------
C IOUT     |  1      | I | R | OUTPUT FILE UNIT (L00 file)
C NUVAR    |  1      | I | W | NUMBER OF USER ELEMENT VARIABLES
C----------+---------+---+---+--------------------------------------------
C PARGEO   |  *      | F | W | 1)SKEW NUMBER
C          |         |   |   | 2)STIFNESS FOR INTERFACE
C          |         |   |   | 3)FRONT WAVE OPTION
C          |         |   |   | 4)... not yet used
C----------+---------+---+---+--------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IOUT,NUVAR,IUNIT
      my_real PARGEO(*)
      INTEGER ID, IGTYP
      CHARACTER*nchartitle,
     .   TITR
      TYPE(PROP_TAG_) , DIMENSION(0:MAXPROP) :: PROP_TAG
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER ITYP,SKFLAG
      LOGICAL IS_AVAILABLE, IS_ENCRYPTED
C=======================================================================
C EXTRACT DATA (IS OPTION CRYPTED)
C--------------------------------------------------
      CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
      CALL HM_GET_INTV('type',ITYP,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV('SkewFlag',SKFLAG,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C
      NUVAR = 39
C
      IF (ITYP==1) THEN
        CALL HM_READ_PROP33_SPH_JNT(IOUT,ITYP,SKFLAG,PARGEO,IS_ENCRYPTED,UNITAB,IUNIT,
     .                   ID,TITR,LSUBMODEL)
      ELSEIF (ITYP==2) THEN
        CALL HM_READ_PROP33_REV_JNT(IOUT,ITYP,SKFLAG,PARGEO,IS_ENCRYPTED,UNITAB,IUNIT,
     .                   ID,TITR,LSUBMODEL)
      ELSEIF (ITYP==3) THEN
        CALL HM_READ_PROP33_CYL_JNT(IOUT,ITYP,SKFLAG,PARGEO,IS_ENCRYPTED,UNITAB,IUNIT,
     .                   ID,TITR,LSUBMODEL)
      ELSEIF (ITYP==4) THEN
        CALL HM_READ_PROP33_PLAN_JNT(IOUT,ITYP,SKFLAG,PARGEO,IS_ENCRYPTED,UNITAB,IUNIT,
     .                   ID,TITR,LSUBMODEL)
      ELSEIF (ITYP==5) THEN
        CALL HM_READ_PROP33_UNIV_JNT(IOUT,ITYP,PARGEO,IS_ENCRYPTED,UNITAB,IUNIT,
     .                   ID,TITR,LSUBMODEL)
      ELSEIF (ITYP==6) THEN
        CALL HM_READ_PROP33_TRANS_JNT(IOUT,ITYP,SKFLAG,PARGEO,IS_ENCRYPTED,UNITAB,IUNIT,
     .                   ID,TITR,LSUBMODEL)
      ELSEIF (ITYP==7) THEN
        CALL HM_READ_PROP33_OLD_JNT(IOUT,ITYP,SKFLAG,PARGEO,IS_ENCRYPTED,UNITAB,IUNIT,
     .                   ID,TITR,LSUBMODEL)
      ELSEIF (ITYP==8) THEN
        CALL HM_READ_PROP33_FIX_JNT(IOUT,ITYP,SKFLAG,PARGEO,IS_ENCRYPTED,UNITAB,IUNIT,
     .                   ID,TITR,LSUBMODEL)
      ELSEIF (ITYP==9) THEN
        CALL HM_READ_PROP33_FREE_JNT(IOUT,ITYP,SKFLAG,PARGEO,IS_ENCRYPTED,UNITAB,IUNIT,
     .                   ID,TITR,LSUBMODEL)
      ELSE
C        WRITE(IOUT,*)' **ERROR WRONG JOINT TYPE'
         CALL ANCMSG(MSGID=409,
     .               MSGTYPE=MSGERROR,
     .               ANMODE=ANINFO_BLIND_2,
     .               I1=ID,
     .               C1=TITR,
     .               I2=ITYP)
      END IF
C
      PROP_TAG(IGTYP)%G_EINT = 1
      PROP_TAG(IGTYP)%G_FOR  = 3
      PROP_TAG(IGTYP)%G_MOM = 3
      PROP_TAG(IGTYP)%G_TOTDEPL = 3
      PROP_TAG(IGTYP)%G_TOTROT = 3
      PROP_TAG(IGTYP)%G_SKEW = 3
      PROP_TAG(IGTYP)%G_MASS = 1
      PROP_TAG(IGTYP)%G_NUVAR = NUVAR
      PROP_TAG(IGTYP)%G_LENGTH_ERR = 3
C
      RETURN
      END

