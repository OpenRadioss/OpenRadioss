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
!||    read_dfs_detcord         ../starter/source/initial_conditions/detonation/read_dfs_detcord.F
!||--- called by ------------------------------------------------------
!||    read_detonators          ../starter/source/initial_conditions/detonation/read_detonators.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                   ../starter/source/output/message/message.F
!||    detcord0                 ../starter/source/initial_conditions/detonation/detcord0.F
!||    hm_get_floatv            ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_intv              ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_option_is_encrypted   ../starter/source/devtools/hm_reader/hm_option_is_encrypted.F
!||    hm_option_read_key       ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start          ../starter/source/devtools/hm_reader/hm_option_start.F
!||    ifrontplus               ../starter/source/spmd/node/frontplus.F
!||    nintri                   ../starter/source/system/nintrr.F
!||    nodgrnr5                 ../starter/source/starter/freform.F
!||    unused_mat_detonator     ../starter/source/initial_conditions/detonation/unused_mat_detonator.F
!||    usr2sys                  ../starter/source/system/sysfus.F
!||--- uses       -----------------------------------------------------
!||    detonators_mod           ../starter/share/modules1/detonators_mod.F
!||    format_mod               ../starter/share/modules1/format_mod.F90
!||    hm_option_read_mod       ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod              ../starter/share/message_module/message_mod.F
!||    submodel_mod             ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE READ_DFS_DETCORD(DETONATORS,X,IGRNOD,IPM,ITABM1,UNITAB,LSUBMODEL,ITAB)
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
      USE FORMAT_MOD , ONLY : FMW_10I
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
      INTEGER,INTENT(IN)                         :: ITABM1(SITABM1),ITAB(NUMNOD)
      INTEGER,INTENT(IN)                         :: IPM(NPROPMI,NUMMAT)
      my_real,INTENT(IN)                         :: X(3,NUMNOD)
      TYPE(SUBMODEL_DATA),INTENT(IN)             :: LSUBMODEL(NSUBMOD)
      TYPE(DETONATORS_STRUCT_),INTENT(INOUT)     :: DETONATORS
      TYPE (GROUP_),DIMENSION(NGRNOD),INTENT(IN) :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER              :: I, MAT, J, NPEM, K,IGU,IGS,JJ,MDET,DET_ID,IDET
      INTEGER              :: IBID,  NODE_ID1, NODE_ID2,uID1,uID2, IOPT, IUNIT, UID
      INTEGER              :: FLAG_FMT,IMAT,IFLAGUNIT,UNUSED,NNOD
      INTEGER              :: STAT,NPE,NPE2
      my_real              :: XC, YC, ZC, ALT, XC1, YC1, ZC1, XC2, YC2, ZC2, NX, NY, NZ, BID, VCJ
      CHARACTER*40         :: MESS
      CHARACTER*64         :: chain1,chain2
      CHARACTER(LEN=NCHARKEY) :: KEY, KEY2
      CHARACTER(LEN=NCHARTITLE) :: TITR
      LOGICAL              :: IS_ENCRYPTED, IS_AVAILABLE
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
      CALL HM_OPTION_START('/DFS/DETCORD')

      DO IDET=1,DETONATORS%N_DET_CORD

        CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID=DET_ID, UNIT_ID=UID,KEYWORD2=KEY,KEYWORD3=KEY2)
        IF (LEN_TRIM(KEY) > 0) KEY = KEY(1:7)
        IF (LEN_TRIM(KEY2) > 0) KEY2 = KEY2(1:4)

        IS_ENCRYPTED= .FALSE.
        IS_AVAILABLE = .FALSE.
        CALL HM_OPTION_IS_ENCRYPTED(IS_ENCRYPTED)
        !---------------------------------!
        !             READING             !
        !---------------------------------!
        CALL HM_GET_FLOATV('magnitude', VCJ, IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_FLOATV('rad_det_time', ALT, IS_AVAILABLE, LSUBMODEL, UNITAB)
        CALL HM_GET_INTV('rad_det_iopt', IOPT, IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INTV('rad_det_materialid', MAT, IS_AVAILABLE, LSUBMODEL)
        CALL HM_GET_INTV('entityid', IGU, IS_AVAILABLE, LSUBMODEL)

        !---------------------------------!
        !      CHECKING USER FLAGS        !
        !      +INTERNAL ID               !
        !---------------------------------!
        MDET=MAT !bak
        IF (ALT > INFINITY) ALT=INFINITY
        IF (ALT < -INFINITY)ALT=-INFINITY
        NNOD = NODGRNR5(IGU ,IGS,DETONATORS%CORD(IDET)%NODES,IGRNOD,ITABM1,MESS)
        IF(IGRNOD(IGS)%SORTED /= 1)THEN
            CALL ANCMSG(MSGID   = 104,MSGTYPE = MSGERROR,ANMODE  = ANINFO,
     .                  C1      = '/DFS/DETCORD',
     .                  I1      = DET_ID,
     .                  C2      = 'ORDERED GROUP OF NODES IS REQUIRED')
        ENDIF
        UNUSED = 0
        IF(MAT > 0)UNUSED=UNUSED_MAT_DETONATOR(MAT,NUMMAT,IPM)
        IF (MAT < 0) THEN
          CALL ANCMSG(MSGID=102,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                I1=DET_ID,
     .                C1='DETONATOR IS REFERRING TO A NEGATIVE MATERIAL ID',
     .                C2='/DFS/DETCORD',
     .                I2=MDET)
        ELSEIF (UNUSED==1) THEN
          CALL ANCMSG(MSGID=102,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO,
     .                I1=DET_ID,
     .                C1='DETONATOR IS REFERRING TO AN UNKNOWN MATERIAL ID',
     .                C2='/DFS/DETCORD',
     .                I2=MDET)
        ELSEIF (UNUSED==2) THEN
          CALL ANCMSG(MSGID=102,MSGTYPE=MSGERROR,ANMODE=ANINFO,
     .                I1=DET_ID,
     .                C1='DETONATOR MUST REFER TO A JWL MATERIAL LAW (LAWS 5, 51, 97, 151)',
     .                C2='/DFS/DETCORD',
     .                I2=MDET)
        ELSEIF (NNOD == 0) THEN

        ELSE
          DETONATORS%CORD(IDET)%IS_MAT_VALID = .TRUE.
          !---------------------------------!
          !        LISTING PRINTOUT         !
          !---------------------------------!
          !IOPT=0  : def=3
          !IOPT=1  : Vdet  - multiple segments (experimental / osbslete)
          !IOPT=2  : instantaneous - multiple segments (experimental / obsolete)
          !IOPT=3  : Centripetal-Catmull-Rom SPLINE interpolation + projection along neutral fiber
          IF(IS_ENCRYPTED) WRITE(IOUT,1001)
          IF(IOPT == 0)IOPT=3
          IF(IOPT == 2)THEN
            IF(.NOT.IS_ENCRYPTED)WRITE(IOUT,1700) DET_ID,IGU,NNOD,ALT,MDET
          ELSEIF(IOPT == 1)THEN
            IF(.NOT.IS_ENCRYPTED)WRITE(IOUT,1701) DET_ID,IGU,NNOD,ALT,VCJ,MDET
          ELSEIF(IOPT==3)THEN
            IF(.NOT.IS_ENCRYPTED)WRITE(IOUT,1700) DET_ID,IGU,NNOD,ALT,MDET
          ENDIF

          IF(.NOT.IS_ENCRYPTED)WRITE(IOUT,FMT=FMW_10I) (ITAB(DETONATORS%CORD(IDET)%NODES(I)),I=1,NNOD)
          DO I=1,NNOD
            CALL IFRONTPLUS(DETONATORS%CORD(IDET)%NODES(I),1)
          END DO
          IF(IOPT == 1 .AND. VCJ <= ZERO)IOPT=2

          CALL DETCORD0(DETONATORS%CORD(IDET),ALT,X,VCJ,IOPT)

          !---------------------------------!
          DETONATORS%CORD(IDET)%TDET= ALT
          DETONATORS%CORD(IDET)%MAT = MAT
          DETONATORS%CORD(IDET)%VDET = VCJ
          DETONATORS%CORD(IDET)%IOPT = IOPT
        END IF

      END DO!next IDET

C-----------------------------------------------
C   O u t p u t   F o r m a t
C-----------------------------------------------      
 1001 FORMAT(///5X,
     &          'DETONATING CORD           ',I10,/5X,
     &          '---------------           ',/5X,
     &          'CONFIDENTIAL DATA')    
 1700 FORMAT(///5X,
     &          'DETONATING CORD            ',I10,/5X,
     &          '----------------           ',/5X,
     &          'NODE GROUP IDENTIFIER     =',I10     ,/5X,
     &          'NUMBER OF POINTS(CORD)    =',I10     ,/5X,         
     &          'LIGHTING TIME             =',1PG20.13,/5X,
     &          'EXPLOSIVE MATERIAL NUMBER =',I10     ,/5X,
     &          'CORD DEFINITION :          ')     
 1701 FORMAT(///5X,
     &          'DETONATING CORD            ',I10,/5X,
     &          '----------------           ',/5X,
     &          'NODE GROUP IDENTIFIER     =',I10     ,/5X,
     &          'NUMBER OF POINTS(CORD)    =',I10     ,/5X,         
     &          'LIGHTING TIME             =',1PG20.13,/5X,
     &          'DETONATION VELOCITY       =',1PG20.13,/5X,     
     &          'EXPLOSIVE MATERIAL NUMBER =',I10     ,/5X,
     &          'CORD DEFINITION :          ')     
C-----------------------------------------------
                 
      END SUBROUTINE
