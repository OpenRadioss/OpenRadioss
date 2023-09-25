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
Chd|  READ_DETONATORS               source/initial_conditions/detonation/read_detonators.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        PRELECDET                     source/initial_conditions/detonation/prelecdet.F
Chd|        READ_DFS_DETCORD              source/initial_conditions/detonation/read_dfs_detcord.F
Chd|        READ_DFS_DETLINE              source/initial_conditions/detonation/read_dfs_detline.F
Chd|        READ_DFS_DETPLAN              source/initial_conditions/detonation/read_dfs_detplan.F
Chd|        READ_DFS_DETPOINT             source/initial_conditions/detonation/read_dfs_detpoint.F
Chd|        READ_DFS_WAVE_SHAPER          source/initial_conditions/detonation/read_dfs_wave_shaper.F
Chd|        DETONATORS_MOD                share/modules1/detonators_mod.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE READ_DETONATORS(ITABM1     ,ITAB   ,IGRNOD,
     .                           IPM        ,X      ,IXQ    ,IXS    ,UNITAB, 
     .                           LSUBMODEL  ,DETONATORS)
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
#include      "sysunit.inc"
#include      "tabsiz_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN)   :: UNITAB 
      INTEGER                        :: ITABM1(SITABM1),ITAB(NUMNOD)
      INTEGER                        :: MDET,IPM(NPROPMI,NUMMAT),IXQ(NIXQ,NUMELQ),IXS(NIXS,NUMELS)
      my_real                        :: X(3,NUMNOD)
      TYPE(SUBMODEL_DATA),INTENT(IN) :: LSUBMODEL(NSUBMOD)
      TYPE(DETONATOR_STRUCT_),TARGET :: DETONATORS
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER              :: I, MAT, J, NPCM, NPEM, N1,N2,ID,I1,I2,I3,I4,I5,I6,K,IGU,IGS,JJ
      INTEGER              :: IUNIT, UID
      INTEGER              :: IFLAGUNIT
      INTEGER              :: DET_ID,STAT
      CHARACTER*40         :: MESS
      CHARACTER*ncharkey   :: KEY, KEY2
      INTEGER              :: NDETPS,NDETSG,NECRAN,NDETPL,NDETCORD,NDET, SBURN
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      DATA MESS/'DETONATORS DEFINITION                   '/      
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
      NDET = DETONATORS%NDET
      IF (NDET <= 0) RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      NDETPS = DETONATORS%NDETPS
      NDETSG = DETONATORS%NDETSG
      NECRAN = DETONATORS%NECRAN
      NDETPL = DETONATORS%NDETPL
      NDETCORD = DETONATORS%NDETCORD

      ALLOCATE (DETONATORS%BURN(8,NDET)    ,STAT=stat)
      IF (STAT /= 0) CALL ANCMSG(MSGID=268,ANMODE=ANINFO, MSGTYPE=MSGERROR,C1='BURN')
      IF (NDET > 0) DETONATORS%BURN = ZERO
      
      !---SIZING & ALLOCATION
      CALL PRELECDET(IGRNOD, LSUBMODEL, DETONATORS)
      
      !---INITIALIZATIONS
      I1   = 0
      I2   = NDETPS
      I3   = NDETPS+NDETSG
      I4   = NDETPS+NDETSG+NECRAN
      I5   = NDETPS+NDETSG+NECRAN+NDETPL
      I6   = NDETPS+NDETSG+NECRAN+NDETPL+NDETCORD      
      N1   = 1
      N2   = 1
      NPCM = 0
      NPEM = 0
      
      !---READING OPTIONS
      CALL HM_OPTION_START('/DFS')
      WRITE(IOUT,1000)
      
      DO K=1,NDET+NLASER
        CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID=DET_ID, UNIT_ID=UID,KEYWORD2=KEY,KEYWORD3=KEY2)
        IF (LEN_TRIM(KEY) > 0) KEY = KEY(1:7)
        IF (LEN_TRIM(KEY2) > 0) KEY2 = KEY2(1:4)                     
        IFLAGUNIT=0
        DO IUNIT=1,NUNITS                           
          IF (UNITAB%UNIT_ID(IUNIT) == UID) THEN    
            IFLAGUNIT = 1               
            EXIT                                
          ENDIF                                 
        ENDDO                                              
        IF (UID /= 0 .AND. IFLAGUNIT == 0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I2=UID,
     .                I1=DET_ID,
     .                C1='DETONATORS',
     .                C2='DETONATORS',
     .                C3='/DFS')                             
        ENDIF  
c-------        
        SELECT CASE(KEY(1:7))
c-------
          CASE ('DETPOIN')
            CALL READ_DFS_DETPOINT(DET_ID,DETONATORS,X,IGRNOD,IPM,ITABM1,UNITAB,LSUBMODEL,KEY,KEY2,I1,I2,I3,I4,I5)
c-------
          CASE ('DETLINE')
            CALL READ_DFS_DETLINE(DET_ID,DETONATORS,X,IGRNOD,IPM,ITABM1,UNITAB,LSUBMODEL,KEY,KEY2,I1,I2,I3,I4,I5)      
c-------
          CASE ('WAV_SHA')
            CALL READ_DFS_WAVE_SHAPER(DET_ID,DETONATORS,X,IGRNOD,IPM,ITABM1,UNITAB,LSUBMODEL,KEY,KEY2,I1,I2,I3,I4,I5,
     .                                N1,NPEM,ITAB)
c-------
          CASE ('DETPLAN')
            CALL READ_DFS_DETPLAN(DET_ID,DETONATORS,X,IGRNOD,IPM,ITABM1,UNITAB,LSUBMODEL,KEY,KEY2,I1,I2,I3,I4,I5)               
c-------
          CASE ('DETCORD')
            CALL READ_DFS_DETCORD(DET_ID,DETONATORS,X,IGRNOD,IPM,ITABM1,UNITAB,LSUBMODEL,KEY,KEY2,I1,I2,I3,I4,I5,
     .                            N2,NPCM,ITAB)               
c-------      
          CASE ('LASERS')
            !SKIP IT               
c-------         
          END SELECT !CASE(KEY)
c-------
      ENDDO !K=1,NDET
c-------
      WRITE(IOUT,2000)
C-------------------------------------------
 999  RETURN
C-----------------------------------------------
C   O u t p u t   F o r m a t
C-----------------------------------------------

 1000 FORMAT(////'    DETONATORS' /
     .           '    ----------'/)
 2000 FORMAT(//)
      END



