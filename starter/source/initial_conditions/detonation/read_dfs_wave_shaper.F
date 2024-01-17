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
Chd|====================================================================
Chd|  READ_DFS_WAVE_SHAPER          source/initial_conditions/detonation/read_dfs_wave_shaper.F
Chd|-- called by -----------
Chd|        READ_DETONATORS               source/initial_conditions/detonation/read_detonators.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_IS_ENCRYPTED        source/devtools/hm_reader/hm_option_is_encrypted.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        IFRONTPLUS                    source/spmd/node/frontplus.F  
Chd|        NODGRNR5                      source/starter/freform.F      
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE READ_DFS_WAVE_SHAPER(DETONATORS,IGRNOD,IPM,ITABM1,UNITAB,LSUBMODEL,ITAB)
C-----------------------------------------------
C   M o d u l e s
C----------------------------------------------- 
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE DETONATORS_MOD
      USE GROUPDEF_MOD
      USE HM_OPTION_READ_MOD
      USE SUBMODEL_MOD
      USE DETONATORS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN)                  :: UNITAB
      INTEGER,INTENT(IN)                            :: ITABM1(SITABM1),ITAB(NUMNOD)
      INTEGER,INTENT(IN)                            :: IPM(NPROPMI,NUMMAT)
      TYPE(SUBMODEL_DATA),INTENT(IN)                :: LSUBMODEL(NSUBMOD)
      TYPE(DETONATORS_STRUCT_),INTENT(INOUT),TARGET :: DETONATORS
      TYPE (GROUP_),DIMENSION(NGRNOD),INTENT(IN)    :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER              :: I, MAT, J, K,IGU,IGS,JJ,MDET,DET_ID,IDET
      INTEGER              :: IBID,  NODE_ID1, NODE_ID2,uID1,uID2, IOPT, IUNIT, UID
      INTEGER              :: FLAG_FMT,IMAT,IFLAGUNIT,UNUSED
      INTEGER              :: STAT,NPE
      my_real              :: XC, YC, ZC, ALT, XC1, YC1, ZC1, XC2, YC2, ZC2, NX, NY, NZ, BID, VCJ, VDET
      CHARACTER*40         :: MESS
      CHARACTER*64         :: chain1,chain2
      CHARACTER*ncharkey   :: KEY, KEY2
      CHARACTER*nchartitle :: TITR
      LOGICAL              :: IS_ENCRYPTED, IS_AVAILABLE
      INTEGER,POINTER,DIMENSION(:) :: IECRAN
      my_real,POINTER,DIMENSION(:) :: DECRAN
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

      CALL HM_OPTION_START('/DFS/WAV_SHA')

      DO IDET=1,DETONATORS%N_DET_WAVE_SHAPER

        CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID=DET_ID, UNIT_ID=UID,KEYWORD2=KEY,KEYWORD3=KEY2)
        IF (LEN_TRIM(KEY) > 0) KEY = KEY(1:7)
        IF (LEN_TRIM(KEY2) > 0) KEY2 = KEY2(1:4)

        IECRAN => DETONATORS%WAVE_SHAPER(IDET)%NODES(1:)
        DECRAN => DETONATORS%WAVE_SHAPER(IDET)%TIME(1:)

        IS_ENCRYPTED= .FALSE.
        IS_AVAILABLE = .FALSE.
        CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
        !---------------------------------!
        !             READING             !
        !---------------------------------!
        CALL HM_GET_FLOATV('rad_det_locationA_X', VDET, IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('rad_det_locationA_Y', YC1, IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('rad_det_locationA_Z', ZC1, IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('rad_det_time', ALT, IS_AVAILABLE, LSUBMODEL,UNITAB)
        CALL HM_GET_INTV('rad_det_materialid', MAT, IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INTV('entityid', IGU, IS_AVAILABLE, LSUBMODEL)
        !---------------------------------!
        !      CHECKING USER FLAGS        !
        !      +INTERNAL ID               !
        !---------------------------------!
        MDET=MAT !bak
        IF (ALT > INFINITY) ALT=INFINITY
        IF(MAT > 0)UNUSED=UNUSED_MAT_DETONATOR(MAT,NUMMAT,IPM)
        UNUSED=0
        IF (MAT < 0) THEN
          CALL ANCMSG(MSGID=102,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                I1=DET_ID,
     .                C1='DETONATOR IS REFERRING TO A NEGATIVE MATERIAL ID',
     .                C2='/DFS/WAV_SHA',
     .                I2=MDET)
        ELSEIF (UNUSED==1) THEN
          CALL ANCMSG(MSGID=102,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                I1=DET_ID,
     .                C1='DETONATOR IS REFERRING TO AN UNKNOWN MATERIAL ID',
     .                C2='/DFS/WAV_SHA',
     .                I2=MDET)
        ELSEIF (UNUSED==2) THEN
          CALL ANCMSG(MSGID=102,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                I1=DET_ID,
     .                C1='DETONATOR MUST REFER TO A JWL MATERIAL LAW (LAWS 5, 51, 97, 151)',
     .                C2='/DFS/WAV_SHA',
     .                I2=MDET)
        ELSE
          !---------------------------------!
          !        LISTING PRINTOUT         !
          !---------------------------------!
          NPE = NODGRNR5(IGU ,IGS,IECRAN(1),IGRNOD ,ITABM1 ,MESS)
          !Nodes in group are ordered from 1 to NPE. last point is the nearest from the detonation origin. Points are composing the screen lines (guard lines). Screen lines is the boundary of the obstacle (shadow area
          IF(IS_ENCRYPTED) WRITE(IOUT,1001)
          IF(.NOT.IS_ENCRYPTED)WRITE(IOUT,1550) DET_ID,VDET,YC1,ZC1,ALT,MDET,IGU,NPE
          IF(.NOT.IS_ENCRYPTED)WRITE(IOUT,FMT=FMW_10I) (ITAB(IECRAN(I)),I=1,NPE)
          DO I=1,NPE
            CALL IFRONTPLUS(IECRAN(I),1)
          END DO
          DETONATORS%WAVE_SHAPER(IDET)%TDET = ALT
          DETONATORS%WAVE_SHAPER(IDET)%MAT = MAT
          DETONATORS%WAVE_SHAPER(IDET)%VDET = VDET
          DETONATORS%WAVE_SHAPER(IDET)%XDET = ZERO
          DETONATORS%WAVE_SHAPER(IDET)%YDET = YC1
          DETONATORS%WAVE_SHAPER(IDET)%ZDET = ZC1
          DETONATORS%WAVE_SHAPER(IDET)%NUMNOD = NPE
        END IF

      ENDDO !next IDET
C-----------------------------------------------
C   O u t p u t   F o r m a t
C-----------------------------------------------
 1001 FORMAT(///'SHADOW LINE DETONATION    ',I10,/5X,
     &          '----------------------    ',/5X,
     &          'CONFIDENTIAL DATA')    
 1550 FORMAT(///'SHADOW LINE DETONATION    =',I10,/5X,
     &          '----------------------     ',/5X,
     &          'OPTIONAL VELOCITY         =',1PG20.13,/5X,
     &          'Y-COORDINATE              =',1PG20.13,/5X,
     &          'Z-COORDINATE              =',1PG20.13,/5X,
     &          'LIGHTING TIME             =',1PG20.13,/5X,
     &          'EXPLOSIVE MATERIAL NUMBER =',I10/
     &          'SHADOW LINE NODE GROUP ID =',I10/
     &          'NUMBER OF POINTS(SHADOW)  =',I10/
     &          'SHADOW LINE DEFINITION :   ')

C-----------------------------------------------
      END SUBROUTINE
