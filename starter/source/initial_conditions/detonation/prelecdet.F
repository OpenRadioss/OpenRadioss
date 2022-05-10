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
Chd|  PRELECDET                     source/initial_conditions/detonation/prelecdet.F
Chd|-- called by -----------
Chd|        READ_DETONATORS               source/initial_conditions/detonation/read_detonators.F
Chd|-- calls ---------------
Chd|        FREERR                        source/starter/freform.F      
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
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "tabsiz_c.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(NSUBMOD)
      TYPE(DETONATOR_STRUCT_) :: DETONATORS
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER SIZ_ECRAN, SIZ_DETCORD
      INTEGER              :: I,J,K,ID,IGU,IGS,NN,STAT
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

      SIZ_ECRAN  = 0
      SIZ_DETCORD= 0      

      !---SPECIFIC SIZING (WAV_SHA & DETCORD) 
      CALL HM_OPTION_START('/DFS')
      DO K=1,DETONATORS%NDET
        CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID=ID,KEYWORD2= KEY)
        IF(KEY(1:7).EQ.'WAV_SHA')THEN
          CALL HM_GET_INTV('entityid', IGU, IS_AVAILABLE, LSUBMODEL)        
          IGS          = NGR2USRN(IGU,IGRNOD,NGRNOD,NN)
          SIZ_ECRAN    = MAX0(SIZ_ECRAN,NN)
        ELSEIF(KEY(1:7).EQ.'DETCORD')THEN
          CALL HM_GET_INTV('entityid', IGU, IS_AVAILABLE, LSUBMODEL)                 
          IGS          = NGR2USRN(IGU,IGRNOD,NGRNOD,NN)
          SIZ_DETCORD  = SIZ_DETCORD + NN               !IGS:internal_grnod_id    IGU:user_grnod_id     NN:Number of Nodes
        ENDIF
      ENDDO
      
      !---SPECIFIC ALLOCATIONS (WAV_SHA & DETCORD)  
      !- /dfs/wav_sha    
      DETONATORS%SIECRAN = SIZ_ECRAN*3
      DETONATORS%SDECRAN = SIZ_ECRAN
      ALLOCATE (DETONATORS%IECRAN(DETONATORS%SIECRAN)  ,STAT=stat)
      ALLOCATE (DETONATORS%DECRAN(DETONATORS%SDECRAN)  ,STAT=stat)
      IF (DETONATORS%SIECRAN > 0) DETONATORS%IECRAN = 0
      IF (DETONATORS%SDECRAN > 0) DETONATORS%DECRAN = ZERO
      !- /dfs/detcord
      DETONATORS%SICORD  = SIZ_DETCORD
      ALLOCATE (DETONATORS%ICORD(DETONATORS%SICORD)  ,STAT=stat)
      ALLOCATE (DETONATORS%DCORD(DETONATORS%SICORD)  ,STAT=stat)
      IF (DETONATORS%SICORD > 0) DETONATORS%ICORD = 0
      IF (DETONATORS%SICORD > 0) DETONATORS%DCORD = ZERO      
C-------------------------------------------
      RETURN
 999  CALL FREERR(3)
      RETURN
      END SUBROUTINE PRELECDET

