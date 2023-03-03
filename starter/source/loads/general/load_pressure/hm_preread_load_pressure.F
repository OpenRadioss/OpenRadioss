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
Chd|  HM_PREREAD_LOAD_PRESSURE      source/loads/general/load_pressure/hm_preread_load_pressure.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_PREREAD_LOAD_PRESSURE(NUMLOADP ,IGRSURF,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
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
      INTEGER,INTENT(INOUT) :: NUMLOADP
C-----------------------------------------------
      TYPE (SURF_)   ,TARGET, DIMENSION(NSURF)   :: IGRSURF
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER               :: I, ID, NINTERS, IDINT
      INTEGER               :: ISU, IS, J ,NINTERP,NN, NIP
      CHARACTER             :: TITR*nchartitle
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C START BROWSING MODEL /LOAD/PRESSURE
C--------------------------------------------------
      CALL HM_OPTION_START('/LOAD/PRESSURE')
C-----------------------------------------------
      DO I=1,NLOADP_HYD
C--------------------------------------------------
C EXTRACT DATAS OF /LOAD/PRESSURE... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       OPTION_TITR = TITR)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
       CALL HM_GET_INTV  ('surf_ID',ISU,IS_AVAILABLE,LSUBMODEL)
C EXTRACT DATAS (INTEGER VALUES) : Number of interfaces
       CALL HM_GET_INTV('N_inter_P',NINTERP,IS_AVAILABLE,LSUBMODEL)
       NINTERS = 0
       IF(NINTERP > 0) THEN
         DO NIP=1,NINTERP

C EXTRACT DATAS (INTEGER VALUES) 

           CALL HM_GET_INT_ARRAY_INDEX('Inter_IDs',IDINT,NIP,IS_AVAILABLE,LSUBMODEL)

           IF(IDINT > 0 ) NINTERS = NINTERS + 1
          ENDDO
       ENDIF

C-------
        IS = 0                                              
        DO J=1,NSURF
          IF(ISU==IGRSURF(J)%ID)IS=J
        ENDDO 
        IF(IS == 0)THEN
          CALL ANCMSG(MSGID=931,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR,
     .                I2=ISU)
        ENDIF   
C-----------                                      
        NUMLOADP = NUMLOADP + 4*IGRSURF(IS)%NSEG
        NINTLOADP = NINTLOADP + NINTERS
      ENDDO
C-----------------------------------------------
      RETURN
      END SUBROUTINE HM_PREREAD_LOAD_PRESSURE
