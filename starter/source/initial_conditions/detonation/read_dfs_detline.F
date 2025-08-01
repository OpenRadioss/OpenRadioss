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
!||    read_dfs_detline         ../starter/source/initial_conditions/detonation/read_dfs_detline.F
!||--- called by ------------------------------------------------------
!||    read_detonators          ../starter/source/initial_conditions/detonation/read_detonators.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    hm_option_read_key       ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start          ../starter/source/devtools/hm_reader/hm_option_start.F
!||    nintri                   ../starter/source/system/nintrr.F
!||    nodgrnr5                 ../starter/source/starter/freform.F
!||    unused_mat_detonator     ../starter/source/initial_conditions/detonation/unused_mat_detonator.F
!||    usr2sys                  ../starter/source/system/sysfus.F
!||--- uses       -----------------------------------------------------
!||    detonators_mod           ../starter/share/modules1/detonators_mod.F
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE READ_DFS_DETLINE(DETONATORS,X,IPM,ITABM1,UNITAB,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE DETONATORS_MOD
      USE GROUPDEF_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN)           :: UNITAB
      INTEGER,INTENT(IN)                     :: ITABM1(SITABM1)
      INTEGER,INTENT(IN)                     :: IPM(NPROPMI,NUMMAT)
      my_real,INTENT(IN)                     :: X(3,NUMNOD)
      TYPE(SUBMODEL_DATA),INTENT(IN)         :: LSUBMODEL(NSUBMOD)
      TYPE(DETONATORS_STRUCT_),INTENT(INOUT) :: DETONATORS
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER              :: I, MAT, J, NPEM,NPCM,K,IGU,IGS,JJ,MDET,DET_ID,IDET
      INTEGER              :: IBID,  NODE_ID1, NODE_ID2,uID1,uID2, IOPT, IUNIT, UID
      INTEGER              :: FLAG_FMT,IMAT,IFLAGUNIT,UNUSED
      INTEGER              :: STAT
      my_real              :: XC, YC, ZC, ALT, XC1, YC1, ZC1, XC2, YC2, ZC2, NX, NY, NZ, BID, VCJ
      CHARACTER*40         :: MESS
      CHARACTER*64         :: chain1,chain2
      CHARACTER(LEN=NCHARKEY) :: KEY, KEY2
      CHARACTER(LEN=NCHARTITLE) :: TITR
      LOGICAL              :: IS_ENCRYPTED, IS_AVAILABLE
      LOGICAL              :: IS_NODE_DEFINED
      INTEGER              :: NDETPS,NDETSG,NECRAN,NDETPL,NDETCORD
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER,EXTERNAL     :: NODGRNR5, USR2SYS, NINTRI
      INTEGER              :: UNUSED_MAT_DETONATOR
      DATA MESS/'DETONATORS DEFINITION                   '/      
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------      

      CALL HM_OPTION_START('/DFS/DETLINE')

      DO IDET=1,DETONATORS%N_DET_LINE

        CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID=DET_ID, UNIT_ID=UID,KEYWORD2=KEY,KEYWORD3=KEY2)
        IF (LEN_TRIM(KEY) > 0) KEY = KEY(1:7)
        IF (LEN_TRIM(KEY2) > 0) KEY2 = KEY2(1:4)

        IS_ENCRYPTED= .FALSE.
        IS_AVAILABLE = .FALSE.
        IS_NODE_DEFINED = .FALSE.
        IF(KEY2(1:4) == 'NODE')IS_NODE_DEFINED = .TRUE.
        CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
        !---------------------------------!
        !             READING             !
        !---------------------------------!
        IF(IS_NODE_DEFINED)THEN
          CALL HM_GET_FLOATV('rad_det_time', ALT, IS_AVAILABLE, LSUBMODEL,UNITAB)
          CALL HM_GET_INTV('rad_det_materialid', MAT, IS_AVAILABLE, LSUBMODEL )
          CALL HM_GET_INTV('rad_det_node1', uID1, IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_INTV('rad_det_node2', uID2, IS_AVAILABLE, LSUBMODEL)
          XC=ZERO
          YC=ZERO
          ZC=ZERO
        ELSE
          CALL HM_GET_FLOATV('rad_det_locationA_X', XC1, IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('rad_det_locationA_Y', YC1, IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('rad_det_locationA_Z', ZC1, IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('rad_det_locationB_X', XC2, IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('rad_det_locationB_Y', YC2, IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('rad_det_locationB_Z', ZC2, IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('rad_det_time', ALT, IS_AVAILABLE, LSUBMODEL,UNITAB)
          CALL HM_GET_INTV('rad_det_materialid', MAT, IS_AVAILABLE, LSUBMODEL)
        ENDIF

        ! COORDINATES
        IF(IS_NODE_DEFINED)THEN
          NODE_ID1=USR2SYS(uID1,ITABM1,MESS,DET_ID)
          IF(NODE_ID1 > 0)THEN
            XC1 = X(1,NODE_ID1)
            YC1 = X(2,NODE_ID1)
            ZC1 = X(3,NODE_ID1)
          ENDIF
          NODE_ID2=USR2SYS(uID2,ITABM1,MESS,DET_ID)
          IF(NODE_ID2 > 0)THEN
            XC2 = X(1,NODE_ID2)
            YC2 = X(2,NODE_ID2)
            ZC2 = X(3,NODE_ID2)
          ENDIF
          IF(NODE_ID1 == 0 .AND. NODE_ID2 == 0)THEN
            CALL ANCMSG(MSGID = 104,MSGTYPE = MSGERROR,ANMODE  = ANINFO,
     .                  C1='/DFS/DETLINE/NODE',
     .                  I1=DET_ID,
     .                  C2='INVALID NODE_ID')
          ENDIF
        ENDIF
        !---------------------------------!
        !      CHECKING USER FLAGS        !
        !      +INTERNAL ID               !
        !---------------------------------!
        MDET=MAT !bak
        IF (ALT >  INFINITY)ALT= INFINITY
        IF (ALT < -INFINITY)ALT=-INFINITY
        UNUSED=0
        IF(MAT > 0)UNUSED=UNUSED_MAT_DETONATOR(MAT,NUMMAT,IPM)
        IF (MAT < 0) THEN
          CALL ANCMSG(MSGID=102,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                I1=DET_ID,
     .                C1='DETONATOR IS REFERRING TO A NEGATIVE MATERIAL ID',
     .                C2='/DFS/DETLINE',
     .                I2=MDET)
        ELSEIF (UNUSED == 1) THEN
          CALL ANCMSG(MSGID=102,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                I1=DET_ID,
     .                C1='DETONATOR IS REFERRING TO AN UNKNOWN MATERIAL ID',
     .                C2='/DFS/DETLINE',
     .                I2=MDET)
        ELSEIF (UNUSED == 2) THEN
          CALL ANCMSG(MSGID=102,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                I1=DET_ID,
     .                C1='DETONATOR MUST REFER TO A JWL MATERIAL LAW (LAWS 5, 51, 97, 151)',
     .                C2='/DFS/DETLINE',
     .                I2=MDET)
        ELSE
          DETONATORS%LINE(IDET)%IS_MAT_VALID = .TRUE.
          !---------------------------------!
          !        LISTING PRINTOUT         !
          !---------------------------------!
          IF(IS_NODE_DEFINED)THEN
            IF(.NOT.IS_ENCRYPTED)WRITE(IOUT,1501) DET_ID,NODE_ID1,XC1,YC1,ZC1,NODE_ID2,XC2,YC2,ZC2,ALT,MDET
          ELSE
            IF(.NOT.IS_ENCRYPTED)WRITE(IOUT,1500) DET_ID,XC1,YC1,ZC1,XC2,YC2,ZC2,ALT,MDET
          ENDIF
          IF(IS_ENCRYPTED)WRITE(IOUT,1001)

          DETONATORS%LINE(IDET)%TDET = ALT
          DETONATORS%LINE(IDET)%MAT = MAT
          DETONATORS%LINE(IDET)%XDET_1 = XC1
          DETONATORS%LINE(IDET)%YDET_1 = YC1
          DETONATORS%LINE(IDET)%ZDET_1 = ZC1
          DETONATORS%LINE(IDET)%XDET_2 = XC2
          DETONATORS%LINE(IDET)%YDET_2 = YC2
          DETONATORS%LINE(IDET)%ZDET_2 = ZC2
        END IF

      ENDDO

C-----------------------------------------------
C   O u t p u t   F o r m a t
C-----------------------------------------------      
 1001 FORMAT(///5X,
     &          'DETONATION LINE           ',I10,/5X,
     &          '---------------           ',/5X,
     &          'CONFIDENTIAL DATA')    
 1500 FORMAT(///5X,
     &          'DETONATION LINE            ',I10,/5X,
     &          '---------------            ',/5X,
     &          'X-COORDINATE FIRST POINT  =',1PG20.13,/5X,
     &          'Y-COORDINATE FIRST POINT  =',1PG20.13,/5X,
     &          'Z-COORDINATE FIRST POINT  =',1PG20.13,/5X,
     &          'X-COORDINATE SECOND POINT =',1PG20.13,/5X,
     &          'Y-COORDINATE SECOND POINT =',1PG20.13,/5X,
     &          'Z-COORDINATE SECOND POINT =',1PG20.13,/5X,
     &          'LIGHTING TIME             =',1PG20.13,/5X,
     &          'EXPLOSIVE MATERIAL NUMBER =',I10 )
 1501 FORMAT(///5X,
     &          'DETONATION LINE            ',I10,/5X,
     &          '---------------            ',/5X,
     &          'FIRST NODE ID             =',I10     ,/5X,     
     &          ' X-COORDINATE FIRST POINT =',1PG20.13,/5X,
     &          ' Y-COORDINATE FIRST POINT =',1PG20.13,/5X,
     &          ' Z-COORDINATE FIRST POINT =',1PG20.13,/5X,
     &          'SECOND NODE ID            =',I10     ,/5X,        
     &          ' X-COORDINATE SECOND POINT=',1PG20.13,/5X,
     &          ' Y-COORDINATE SECOND POINT=',1PG20.13,/5X,
     &          ' Z-COORDINATE SECOND POINT=',1PG20.13,/5X,
     &          'LIGHTING TIME             =',1PG20.13,/5X,
     &          'EXPLOSIVE MATERIAL NUMBER =',I10 )     
C-----------------------------------------------      
                 
      END SUBROUTINE
