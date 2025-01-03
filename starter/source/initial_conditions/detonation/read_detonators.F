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
      !||    read_detonators        ../starter/source/initial_conditions/detonation/read_detonators.F
      !||--- called by ------------------------------------------------------
      !||    lectur                 ../starter/source/starter/lectur.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../starter/source/output/message/message.F
      !||    hm_option_count        ../starter/source/devtools/hm_reader/hm_option_count.F
      !||    hm_option_read_key     ../starter/source/devtools/hm_reader/hm_option_read_key.F
      !||    hm_option_start        ../starter/source/devtools/hm_reader/hm_option_start.F
      !||    prelecdet              ../starter/source/initial_conditions/detonation/prelecdet.F
      !||    read_dfs_detcord       ../starter/source/initial_conditions/detonation/read_dfs_detcord.F
      !||    read_dfs_detline       ../starter/source/initial_conditions/detonation/read_dfs_detline.F
      !||    read_dfs_detplan       ../starter/source/initial_conditions/detonation/read_dfs_detplan.F
      !||    read_dfs_detpoint      ../starter/source/initial_conditions/detonation/read_dfs_detpoint.F
      !||    read_dfs_wave_shaper   ../starter/source/initial_conditions/detonation/read_dfs_wave_shaper.F
      !||--- uses       -----------------------------------------------------
      !||    detonators_mod         ../starter/share/modules1/detonators_mod.F
      !||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
      !||    message_mod            ../starter/share/message_module/message_mod.F
      !||    submodel_mod           ../starter/share/modules1/submodel_mod.F
      !||====================================================================
      SUBROUTINE READ_DETONATORS(ITABM1     ,ITAB   ,IGRNOD,
     .                           IPM        ,X      ,UNITAB,
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
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARKEY
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
      TYPE (UNIT_TYPE_),INTENT(IN)                  :: UNITAB
      INTEGER,INTENT(IN)                            :: ITABM1(SITABM1),ITAB(NUMNOD)
      INTEGER,INTENT(IN)                            :: IPM(NPROPMI,NUMMAT)
      my_real,INTENT(IN)                            :: X(3,NUMNOD)
      TYPE(SUBMODEL_DATA),INTENT(IN)                :: LSUBMODEL(NSUBMOD)
      TYPE(DETONATORS_STRUCT_),TARGET,INTENT(INOUT) :: DETONATORS
      TYPE (GROUP_),DIMENSION(NGRNOD),INTENT(IN)    :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER              :: I, MAT, J, NPCM, NPEM, N1,N2,ID,I1,I2,K
      INTEGER              :: IUNIT, UID
      INTEGER              :: IFLAGUNIT
      INTEGER              :: DET_ID,STAT
      CHARACTER*40         :: MESS
      CHARACTER(LEN=NCHARKEY) :: KEY, KEY2
      INTEGER              :: NDET,NDFS
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      DATA MESS/'DETONATORS DEFINITION                   '/
C-----------------------------------------------
C   P r e - C o n d i t i o n
C-----------------------------------------------
      NDET = DETONATORS%N_DET
      IF (NDET <= 0) RETURN
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------

      !---READING DFS OPTIONS & CHECK UNIT ID
      CALL HM_OPTION_COUNT('/DFS', NDFS)
      CALL HM_OPTION_START('/DFS')
      WRITE(IOUT,1000)
      DO K=1,NDFS
        CALL HM_OPTION_READ_KEY(LSUBMODEL,OPTION_ID=DET_ID, UNIT_ID=UID,KEYWORD2=KEY,KEYWORD3=KEY2)
        IF (LEN_TRIM(KEY) > 0) KEY = KEY(1:7)
        IF (LEN_TRIM(KEY2) > 0) KEY2 = KEY2(1:4)
        !CHECK UNIT SYSTEM ID (IF DEFINED)
        IF(UID > 0)THEN
          IFLAGUNIT=0
          DO IUNIT=1,UNITAB%NUNITS
            IF (UNITAB%UNIT_ID(IUNIT) == UID) THEN
              IFLAGUNIT = 1
              EXIT
            ENDIF
          ENDDO
          IF (IFLAGUNIT == 0) THEN
            CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I2=UID,I1=DET_ID,C1='DETONATORS',C2='DETONATORS',C3='/DFS')
          ENDIF
        ENDIF
      ENDDO !K=1,NDFS

      !---SIZING & ALLOCATION
      CALL PRELECDET(IGRNOD, LSUBMODEL, DETONATORS)

      ! --- READING ALL DETONATOR TYPES
      CALL READ_DFS_DETPOINT(DETONATORS,X,IPM,ITABM1,UNITAB,LSUBMODEL,IGRNOD)
      CALL READ_DFS_DETLINE(DETONATORS,X,IPM,ITABM1,UNITAB,LSUBMODEL)
      CALL READ_DFS_WAVE_SHAPER(DETONATORS,IGRNOD,IPM,ITABM1,UNITAB,LSUBMODEL,ITAB)
      CALL READ_DFS_DETPLAN(DETONATORS,X,IPM,ITABM1,UNITAB,LSUBMODEL)
      CALL READ_DFS_DETCORD(DETONATORS,X,IGRNOD,IPM,ITABM1,UNITAB,LSUBMODEL,ITAB)
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



