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
      !||    read_dfs_detpoint                ../starter/source/initial_conditions/detonation/read_dfs_detpoint.F
      !||--- called by ------------------------------------------------------
      !||    read_detonators                  ../starter/source/initial_conditions/detonation/read_detonators.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                           ../starter/source/output/message/message.F
      !||    hm_get_floatv                    ../starter/source/devtools/hm_reader/hm_get_floatv.F
      !||    hm_get_intv                      ../starter/source/devtools/hm_reader/hm_get_intv.F
      !||    hm_option_is_encrypted           ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
      !||    hm_option_read_key               ../starter/source/devtools/hm_reader/hm_option_read_key.F
      !||    hm_option_start                  ../starter/source/devtools/hm_reader/hm_option_start.F
      !||    material_is_high_explosive       ../starter/source/materials/material_is_high_explosive.F90
      !||    nintri                           ../starter/source/system/nintrr.F
      !||    nodgrnr5                         ../starter/source/starter/freform.F
      !||    unused_mat_detonator             ../starter/source/initial_conditions/detonation/unused_mat_detonator.F
      !||    usr2sys                          ../starter/source/system/sysfus.F
      !||--- uses       -----------------------------------------------------
      !||    detonators_mod                   ../starter/share/modules1/detonators_mod.F
      !||    hm_option_read_mod               ../starter/share/modules1/hm_option_read_mod.F
      !||    material_is_high_explosive_mod   ../starter/source/materials/material_is_high_explosive.F90
      !||    message_mod                      ../starter/share/message_module/message_mod.F
      !||    submodel_mod                     ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE READ_DFS_DETPOINT(DETONATORS,X,IPM,PM,ITABM1,UNITAB,LSUBMODEL,IGRNOD)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE DETONATORS_MOD , ONLY : DETONATORS_STRUCT_
      USE GROUPDEF_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARKEY
      USE MATERIAL_IS_HIGH_EXPLOSIVE_MOD , ONLY : MATERIAL_IS_HIGH_EXPLOSIVE
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
      TYPE (UNIT_TYPE_),INTENT(IN)               :: UNITAB
      INTEGER,INTENT(IN)                         :: ITABM1(SITABM1)
      INTEGER,INTENT(IN)                         :: IPM(NPROPMI,NUMMAT)
      my_real,INTENT(IN)                         :: X(3,NUMNOD)
      TYPE(SUBMODEL_DATA),INTENT(IN)             :: LSUBMODEL(NSUBMOD)
      TYPE(DETONATORS_STRUCT_),INTENT(INOUT)     :: DETONATORS
      TYPE (GROUP_),DIMENSION(NGRNOD),INTENT(IN) :: IGRNOD
      my_real,INTENT(INOUT)                      :: PM(NPROPM,NUMMAT)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER              :: I,MAT,IGU,IGS,MDET,DET_ID,IDET
      INTEGER              :: NODE_ID1,uID1,UID,internal_ID,NNOD
      INTEGER              :: UNUSED
      my_real              :: XC, YC, ZC, TDET
      CHARACTER*40         :: MESS
      CHARACTER(LEN=NCHARTITLE) :: TITR
      CHARACTER*32         :: MSG_C2
      LOGICAL              :: IS_ENCRYPTED, IS_AVAILABLE, IS_AVAILABLE_SHADOW
      LOGICAL              :: IS_NODE_DEFINED,IS_GRNOD_DEFINED
      INTEGER              :: I_SHADOW
      INTEGER              :: MLW
      CHARACTER(LEN=NCHARKEY) :: KEY, KEY2
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER,EXTERNAL     :: NODGRNR5, USR2SYS, NINTRI
      INTEGER              :: UNUSED_MAT_DETONATOR
      DATA MESS/'DETONATORS DEFINITION                   '/
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      CALL HM_OPTION_START('/DFS/DETPOIN')

      DO IDET=1,DETONATORS%N_DET_POINT

       CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID=DET_ID, UNIT_ID=UID,KEYWORD2=KEY,KEYWORD3=KEY2)
       IF (LEN_TRIM(KEY) > 0) KEY = KEY(1:7)
       IF (LEN_TRIM(KEY2) > 0) KEY2 = KEY2(1:5)

        IS_ENCRYPTED= .FALSE.
        IS_AVAILABLE = .FALSE.
        IS_AVAILABLE_SHADOW = .FALSE.
        IS_NODE_DEFINED = .FALSE.
        IS_GRNOD_DEFINED = .FALSE.
        I_SHADOW = 0
        IF(KEY2(1:4)=='NODE')IS_NODE_DEFINED = .TRUE.
        IF(KEY2(1:3)=='SET')IS_GRNOD_DEFINED = .TRUE.
        CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
        !---------------------------------!
        !             READING             !
        !---------------------------------!
        IF(IS_NODE_DEFINED)THEN
          MSG_C2 = '/DFS/DETPOINT/NODE'
          CALL HM_GET_INTV('rad_det_Ishadow', I_SHADOW, IS_AVAILABLE_SHADOW, LSUBMODEL)
          CALL HM_GET_FLOATV('rad_det_time', TDET, IS_AVAILABLE, LSUBMODEL,UNITAB)
          CALL HM_GET_INTV('rad_det_materialid', MAT, IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_INTV('rad_det_node1', uID1, IS_AVAILABLE, LSUBMODEL)
          XC=ZERO
          YC=ZERO
          ZC=ZERO
        ELSEIF(IS_GRNOD_DEFINED)THEN
          MSG_C2 = '/DFS/DETPOINT/SET'
          CALL HM_GET_INTV('rad_det_Ishadow', I_SHADOW, IS_AVAILABLE_SHADOW, LSUBMODEL)
          CALL HM_GET_FLOATV('rad_det_time', TDET, IS_AVAILABLE, LSUBMODEL,UNITAB)
          CALL HM_GET_INTV('rad_det_materialid', MAT, IS_AVAILABLE, LSUBMODEL)
          CALL HM_GET_INTV('rad_det_grnod1', uID1, IS_AVAILABLE, LSUBMODEL)
          XC=ZERO
          YC=ZERO
          ZC=ZERO
        ELSE
          I_SHADOW = 0
          MSG_C2 = '/DFS/DETPOINT'
          CALL HM_GET_FLOATV('rad_det_locationA_X', XC, IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('rad_det_locationA_Y', YC, IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('rad_det_locationA_Z', ZC, IS_AVAILABLE, LSUBMODEL, UNITAB)
          CALL HM_GET_FLOATV('rad_det_time', TDET, IS_AVAILABLE, LSUBMODEL,UNITAB)
          CALL HM_GET_INTV('rad_det_materialid', MAT, IS_AVAILABLE, LSUBMODEL)
        ENDIF

        MDET = MAT

        IF (TDET > INFINITY) TDET= INFINITY
        IF (TDET < -INFINITY)TDET=-INFINITY

        !default value for shadowing flag
        IF(IS_AVAILABLE_SHADOW)THEN
          IF(I_SHADOW < 0 .OR. I_SHADOW > 1)THEN
            I_SHADOW = 0
            CALL ANCMSG(MSGID=102,MSGTYPE=MSGWARNING,ANMODE=ANINFO_BLIND_1,
     .                  I1=DET_ID,
     .                  C1='INVALID I_SHADOW FLAG. FLAG IS SET TO 0',
     .                  C2=MSG_C2,
     .                  I2=MDET)
          END IF
        ENDIF

        IF(I_SHADOW == 1) DETONATORS%IS_SHADOWING_REQUIRED = .TRUE.

        IF(I_SHADOW == 1) THEN
          IF(MDET > 0)THEN
            PM(96,MDET) = 1 !SHADOW FLAG SET ON
          ELSE
            !enable flag for all compatible material laws
            DO I=1,NUMMAT
              MLW = IPM(2,I)
              IF(MATERIAL_IS_HIGH_EXPLOSIVE(MLW))PM(96,I)=1 !SHADOW FLAG SET ON
            ENDDO
           ENDIF
        ENDIF

        IF(IS_NODE_DEFINED)THEN
          NODE_ID1=USR2SYS(uID1,ITABM1,MESS,DET_ID)
          DETONATORS%POINT(IDET)%NNOD = 1
          ALLOCATE(DETONATORS%POINT(IDET)%NODLIST(1))
          DETONATORS%POINT(IDET)%NODLIST(1) = NODE_ID1 !internal id
          IF(NODE_ID1 > 0)THEN
            XC = X(1,NODE_ID1)
            YC = X(2,NODE_ID1)
            ZC = X(3,NODE_ID1)
          ENDIF
          IF(NODE_ID1==0)THEN
            CALL ANCMSG(MSGID=104, MSGTYPE=MSGERROR, ANMODE=ANINFO, C1='/DFS/DETPOINT/NODE', I1=DET_ID, C2='INVALID NODE_ID')
          ENDIF
        ENDIF

        ! /DFS/DETPOINT/GRNOD
        ! check if provided group identifier does exist
        internal_ID = 0
        IF(IS_GRNOD_DEFINED)THEN
          NNOD = 0
          DO I=1,NGRNOD
            IF (IGRNOD(I)%ID == uID1) THEN
              internal_ID = I
              NNOD = IGRNOD(I)%NENTITY
              DETONATORS%POINT(IDET)%NNOD = NNOD
              ALLOCATE(DETONATORS%POINT(IDET)%NODLIST(NNOD))
              !filling nodlist
              NNOD=NODGRNR5(uID1,internal_ID,DETONATORS%POINT(IDET)%NODLIST,IGRNOD,ITABM1,MESS)
              EXIT
            ENDIF
          ENDDO
          IF(internal_ID == 0)THEN
            CALL ANCMSG(MSGID=104, MSGTYPE=MSGERROR, ANMODE=ANINFO, C1='/DFS/DETPOINT/SET', I1=DET_ID, C2='INVALID SET_ID')
          ENDIF
        ENDIF

        !---------------------------------!
        !      CHECKING USER FLAGS        !
        !      +INTERNAL ID               !
        !---------------------------------!
        MDET=MAT !bak
        UNUSED=0
        IF(MAT > 0)UNUSED=UNUSED_MAT_DETONATOR(MAT,NUMMAT,IPM)
        IF (MAT < 0) THEN
          CALL ANCMSG(MSGID=102,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                I1=DET_ID,
     .                C1='DETONATOR IS REFERRING TO A NEGATIVE MATERIAL ID',
     .                C2='/DFS/DETPOINT',
     .                I2=MDET)
        ELSEIF (UNUSED == 1) THEN
          CALL ANCMSG(MSGID=102,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                I1=DET_ID,
     .                C1='DETONATOR IS REFERRING TO AN UNKNOWN MATERIAL ID',
     .                C2='/DFS/DETPOINT',
     .                I2=MDET)
        ELSEIF (UNUSED == 2) THEN
          CALL ANCMSG(MSGID=102,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                I1=DET_ID,
     .                C1='DETONATOR MUST REFER TO A JWL MATERIAL LAW (LAWS 5, 51, 97, 151)',
     .                C2='/DFS/DETPOINT',
     .                I2=MDET)
        ELSE
          ! VALID MATERIAL ID
          DETONATORS%POINT(IDET)%IS_MAT_VALID = .TRUE.
          ! STARTER LISTING FILEetpoint
          IF(IS_ENCRYPTED)THEN
            WRITE(IOUT,1001)
          ELSEIF(IS_NODE_DEFINED)THEN
            WRITE(IOUT,1401) DET_ID,I_SHADOW,uID1,XC,YC,ZC,TDET,MDET
          ELSEIF(IS_GRNOD_DEFINED)THEN
            WRITE(IOUT,1402) DET_ID,I_SHADOW,uID1,NNOD,TDET,MDET
          ELSE
            WRITE(IOUT,1400) DET_ID,XC,YC,ZC,TDET,MDET
          ENDIF
          ! STORAGE
          DETONATORS%POINT(IDET)%SHADOW = I_SHADOW
          DETONATORS%POINT(IDET)%TDET = TDET
          DETONATORS%POINT(IDET)%MAT = MAT
          DETONATORS%POINT(IDET)%XDET = XC
          DETONATORS%POINT(IDET)%YDET = YC
          DETONATORS%POINT(IDET)%ZDET = ZC
          DETONATORS%POINT(IDET)%GRNOD_ID = internal_ID
        END IF

      ENDDO! next IDET

C-----------------------------------------------
C   O u t p u t   F o r m a t
C-----------------------------------------------
 1001 FORMAT(///'DETONATION POINT           ',I10,/5X,
     &          '----------------           ',/5X,
     &          'CONFIDENTIAL DATA')
 1400 FORMAT(///'DETONATION POINT           ',I10,/5X,
     &          '----------------           ',/5X,
     &          'X-COORDINATE              =',1PG20.13,/5X,
     &          'Y-COORDINATE              =',1PG20.13,/5X,
     &          'Z-COORDINATE              =',1PG20.13,/5X,
     &          'DETONATION TIME           =',1PG20.13,/5X,
     &          'EXPLOSIVE MATERIAL NUMBER =',I10 )
 1401 FORMAT(///'DETONATION POINT (NODE)    ',I10,/5X,
     &          '----------------           ',/5X,
     &          'SHADOWING FLAG            =',I10     ,/5X,
     &          'NODE ID                   =',I10     ,/5X,
     &          ' X-COORDINATE             =',1PG20.13,/5X,
     &          ' Y-COORDINATE             =',1PG20.13,/5X,
     &          ' Z-COORDINATE             =',1PG20.13,/5X,
     &          'DETONATION TIME           =',1PG20.13,/5X,
     &          'EXPLOSIVE MATERIAL NUMBER =',I10 )
 1402 FORMAT(///'DETONATION POINT (SET)     ',I10,/5X,
     &          '----------------           ',/5X,
     &          'SHADOWING FLAG            =',I10     ,/5X,
     &          'SET ID                    =',I10     ,/5X,
     &          ' NUMBER OF NODES          =',I10,/5X,
     &          'DETONATION TIME           =',1PG20.13,/5X,
     &          'EXPLOSIVE MATERIAL NUMBER =',I10 )
C-----------------------------------------------            
      END SUBROUTINE
