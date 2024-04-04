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
Chd|  PRELECDET                     source/initial_conditions/detonation/prelecdet.F
Chd|-- called by -----------
Chd|        READ_DETONATORS               source/initial_conditions/detonation/read_detonators.F
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        NGR2USRN                      source/system/nintrr.F        
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE PRELECDET(IGRNOD, LSUBMODEL, DETONATORS)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE DETONATORS_MOD
      USE SUBMODEL_MOD         
      USE HM_OPTION_READ_MOD
      USE DETONATORS_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(NSUBMOD)
      TYPE(DETONATORS_STRUCT_) :: DETONATORS
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER SIZ_ECRAN, SIZ_DETCORD
      INTEGER              :: I,J,K,ID,USER_ID,INTERNAL_ID,NN,STAT
      INTEGER              :: NNOD_DETCORD, NNOD_WAV_SHA,NUM
      CHARACTER*ncharkey   :: KEY,KEY2,KEY3
      CHARACTER*nchartitle :: TITR
      LOGICAL              :: IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NGR2USRN
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------      

! Allocation for /DFS/WAV_SHA & /DFS/DETCORD

      !ALLOCATE NUMBER OF DETONATORS IN DATA STRUCTURE
      DETONATORS%N_DET = 0 !total number of detonators
      !
      NUM = DETONATORS%N_DET_POINT
      DETONATORS%N_DET = DETONATORS%N_DET + NUM
      IF(NUM > 0) ALLOCATE(DETONATORS%POINT(NUM))
      !
      NUM = DETONATORS%N_DET_LINE
      DETONATORS%N_DET = DETONATORS%N_DET + NUM
      IF(NUM > 0) ALLOCATE(DETONATORS%LINE(NUM))
      !
      NUM = DETONATORS%N_DET_PLANE
      DETONATORS%N_DET = DETONATORS%N_DET + NUM
      IF(NUM > 0) ALLOCATE(DETONATORS%PLANE(NUM))
      !
      NUM = DETONATORS%N_DET_WAVE_SHAPER
      DETONATORS%N_DET = DETONATORS%N_DET + NUM
      IF(NUM > 0) ALLOCATE(DETONATORS%WAVE_SHAPER(NUM))
      !
      NUM = DETONATORS%N_DET_CORD
      DETONATORS%N_DET = DETONATORS%N_DET + NUM
      IF(NUM > 0) ALLOCATE(DETONATORS%CORD(NUM))

      !---SPECIFIC ALLOCATIONS (WAV_SHA & DETCORD)
      !- /dfs/wav_sha
      CALL HM_OPTION_START('/DFS/WAV_SHA')
      DO K=1,DETONATORS%N_DET_WAVE_SHAPER
        CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID=ID,KEYWORD2= KEY)
        CALL HM_GET_INTV('entityid', USER_ID, IS_AVAILABLE, LSUBMODEL)
        INTERNAL_ID = NGR2USRN(USER_ID,IGRNOD,NGRNOD,NNOD_WAV_SHA)
        DETONATORS%WAVE_SHAPER(K)%NUMNOD = NNOD_WAV_SHA
        ALLOCATE(DETONATORS%WAVE_SHAPER(K)%NODES(NNOD_WAV_SHA))
        ALLOCATE(DETONATORS%WAVE_SHAPER(K)%IORDR(NNOD_WAV_SHA))
        ALLOCATE(DETONATORS%WAVE_SHAPER(K)%FLAG(NNOD_WAV_SHA))
        ALLOCATE(DETONATORS%WAVE_SHAPER(K)%TIME(NNOD_WAV_SHA))
        DETONATORS%WAVE_SHAPER(K)%NODES(:) = 0
        DETONATORS%WAVE_SHAPER(K)%IORDR(:) = 0
        DETONATORS%WAVE_SHAPER(K)%FLAG(:) = 0 
        DETONATORS%WAVE_SHAPER(K)%TIME(:) = ZERO
      END DO

      CALL HM_OPTION_START('/DFS/DETCORD')
      DO K=1,DETONATORS%N_DET_CORD
        CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID=ID,KEYWORD2= KEY)
        CALL HM_GET_INTV('entityid', USER_ID, IS_AVAILABLE, LSUBMODEL)
        INTERNAL_ID = NGR2USRN(USER_ID,IGRNOD,NGRNOD,NNOD_DETCORD)
        DETONATORS%CORD(K)%NUMNOD = NNOD_DETCORD
        ALLOCATE(DETONATORS%CORD(K)%NODES(NNOD_DETCORD))
        ALLOCATE(DETONATORS%CORD(K)%TDET_PATH(NNOD_DETCORD))
        DETONATORS%CORD(K)%NODES(:) = 0
        DETONATORS%CORD(K)%TDET_PATH(:) = ZERO
      ENDDO




C-------------------------------------------
      RETURN
      END SUBROUTINE PRELECDET

