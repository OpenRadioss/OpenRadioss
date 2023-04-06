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
Chd|  HM_PREREAD_MERGE              source/constraints/general/merge/hm_preread_merge.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        GRSIZEN                       source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_PREREAD_MERGE(SMGRBY, SLPBY, IGRNOD, LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SMGRBY, SLPBY
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,IGU,NN,UID,
     .        IMAIN,M_TYPE,ISECONDARY,S_TYPE,FLAGG_OPT,NOBJ
      CHARACTER KEY2*ncharkey,TITR*nchartitle
      LOGICAL   IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER GRSIZEN
C=======================================================================
C Comptage du nombre de ligne dans les options /MERGE/RBODY
C Augmente la taille de LPBY en fonction des merge de NODE et GRNOD
C=======================================================================
      SMGRBY = 0
      NXTRA_NODE = 0
      NOBJ = 0
C--------------------------------------------------
C START BROWSING MODEL MERGE
C--------------------------------------------------
      IS_AVAILABLE = .FALSE.
      CALL HM_OPTION_START('/MERGE/RBODY')
C--------------------------------------------------
      DO I=1,NRBMERGE
C--------------------------------------------------
C EXTRACT DATAS OF /RBODY/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       UNIT_ID = UID,
     .                       OPTION_TITR = TITR,
     .                       KEYWORD2 = KEY2)

C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('NB_SUBOBJVE',NOBJ,IS_AVAILABLE,LSUBMODEL)
c
        DO J=1,NOBJ
            CALL HM_GET_INT_ARRAY_INDEX('Main_ID',IMAIN,J,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('M_type',M_TYPE,J,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('Secon_ID',ISECONDARY,J,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('S_type',S_TYPE,J,IS_AVAILABLE,LSUBMODEL)
            CALL HM_GET_INT_ARRAY_INDEX('Iflag',FLAGG_OPT,J,IS_AVAILABLE,LSUBMODEL)
            IF (IMAIN /= 0) THEN
              SMGRBY = SMGRBY + 1
              IF(S_TYPE == 2) THEN
                SLPBY = SLPBY + 1
                NXTRA_NODE = NXTRA_NODE + 1 
              ELSEIF(S_TYPE == 3) THEN
                IGU=ISECONDARY
                NN = GRSIZEN(IGU,IGRNOD,NGRNOD) 
                SLPBY = SLPBY + NN   
                NXTRA_NODE = NXTRA_NODE + NN                
              ENDIF
            ENDIF  ! IMAIN /= 0
        ENDDO

      ENDDO
C-----------
      RETURN
      END 
C

