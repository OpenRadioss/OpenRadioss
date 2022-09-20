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
Chd|  HM_PREREAD_IMPVEL             source/constraints/general/impvel/hm_preread_impvel.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        NGR2USRN                      source/system/nintrr.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_PREREAD_IMPVEL(
     .           NIMPVEL  ,IGRNOD  ,IPART   ,IPARTR   ,   NFVLAG,
     .           UNITAB   ,LSUBMODEL)
C============================================================================
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
#include      "param_c.inc"
#include      "scr03_c.inc"
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(OUT)   :: NIMPVEL
      INTEGER ,INTENT(INOUT) :: NFVLAG
      INTEGER IPART(LIPART1,*), IPARTR(*)
      my_real            , DIMENSION(LUNIT,*) ,INTENT(IN) :: UNITAB
      TYPE (GROUP_)      , DIMENSION(NGRNOD)  ,INTENT(IN) :: IGRNOD
      TYPE(SUBMODEL_DATA), DIMENSION(*)       ,INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,N,OPTID,NFVEL,NFGEO,IGS,GRNOD_ID,PART_ID,NNOD,JPART,SYS_TYPE
      CHARACTER(ncharkey)    :: KEY
      CHARACTER(nchartitle)  :: TITR
      LOGICAL                :: IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NGR2USRN
c--------------------------------------------------
c     Count number of nodes with imposed velocity => NUMVEL
C======================================================================|
      IS_AVAILABLE = .FALSE.
c
      NIMPVEL = 0   ! initialize counter of nodes
c--------------------------------------------------
c     /IMPVEL and /IMPVEL/LAGMUL
c--------------------------------------------------
      CALL HM_OPTION_COUNT('/IMPVEL',NFVEL)
c
      CALL HM_OPTION_START('/IMPVEL')
c
      DO I=1,NFVEL
c
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = OPTID,
     .                          OPTION_TITR = TITR,
     .                          KEYWORD2    = KEY)
c
        IF (KEY(1:4) /= 'FGEO') THEN
          CALL HM_GET_INTV('rad_system_input_type'   ,SYS_TYPE ,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('entityid'                ,GRNOD_ID ,IS_AVAILABLE,LSUBMODEL)
          IGS = NGR2USRN(GRNOD_ID,IGRNOD,NGRNOD,NNOD)
          IF (IGS > 0) THEN
            NIMPVEL = NIMPVEL + NNOD                                         
            IF (KEY(1:6) == 'LAGMUL') NFVLAG = NFVLAG + NNOD
          END IF
        ENDIF
c
      ENDDO ! DO I=1,NFVEL
c--------------------------      
c     /IMPVEL/FGEO    
c--------------------------         
      CALL HM_OPTION_COUNT('/IMPVEL/FGEO',NFGEO)
c
      CALL HM_OPTION_START('/IMPVEL/FGEO')
c
      
      DO I=1,NFGEO
c
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = OPTID,
     .                          OPTION_TITR = TITR,
     .                          KEYWORD2    = KEY)
        IF (KEY(1:4) == 'FGEO') THEN
          CALL HM_GET_INTV('rad_spring_part' ,PART_ID ,IS_AVAILABLE,LSUBMODEL)
          IF (PART_ID > 0) THEN
            JPART = 0
            DO N=1,NPART
              IF (IPART(4,N) == PART_ID) JPART = N
            ENDDO
            IF (JPART == 0) THEN
             CALL ANCMSG(MSGID=1077, MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,
     .                   I1=OPTID,
     .                   C1=TITR,
     .                   I2=PART_ID)
            ENDIF
            DO N=1,NUMELR
              IF (IPARTR(N) == JPART) NIMPVEL = NIMPVEL + 1
            ENDDO
          ENDIF
c
          CALL HM_GET_INTV('distribution_table_count' ,NNOD ,IS_AVAILABLE,LSUBMODEL)
          NIMPVEL = NIMPVEL + NNOD
        ENDIF
      ENDDO ! DO I=1,NFGEO
c-----------
      RETURN
      END
