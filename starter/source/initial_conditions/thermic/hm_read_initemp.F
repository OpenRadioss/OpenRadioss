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
Chd|  HM_READ_INITEMP               source/initial_conditions/thermic/hm_read_initemp.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOAT_ARRAY_INDEX      source/devtools/hm_reader/hm_get_float_array_index.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_INT_ARRAY_INDEX        source/devtools/hm_reader/hm_get_int_array_index.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        USR2SYS                       source/system/sysfus.F        
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_INITEMP(TEMP  ,ITAB   ,ITABM1  ,IGRNOD  ,INITIDS ,UNITAB ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   A n a l y s e   M o d u l e
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr03_c.inc"
#include      "submod_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      INTEGER ITAB(*), ITABM1(*),INITIDS(*)
C     REAL
      my_real
     .   TEMP(*)
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,N,NNOD,NOSYS,ITYPE,ID,ISK,IGR,IGRS,NBTEMP,BID,
     .        FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP,UID,TYP,NOD,NODSYS,NB_LINE
C     REAL
      my_real
     .  TEMP0
      CHARACTER MESS*40,LLINE*ncharline,TITR*nchartitle,KEY*ncharkey
      LOGICAL IS_AVAILABLE
      my_real, DIMENSION(:), ALLOCATABLE :: LIST_TEMP0
      INTEGER, DIMENSION(:), ALLOCATABLE :: LIST_NOD
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER USR2SYS, USRTOS
      DATA MESS/'INITIAL TEMPERATURE DEFINITION           '/
C=======================================================================
      BID =0
      NBTEMP = 0
      IS_AVAILABLE = .FALSE.   
      NB_LINE = 0                      
C--------------------------------------------------
C START BROWSING MODEL INITEMP
C--------------------------------------------------
      CALL HM_OPTION_START('/INITEMP')
C--------------------------------------------------
C BROWSING /INITEMP OPTIONS 1->NRADIA 
C--------------------------------------------------
      DO I=1,NINTEMP
        TITR = ''
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                      UNIT_ID = UID,
     .                      OPTION_ID = ID,
     .                      OPTION_TITR = TITR)

        IF (ITHERM_FE == 0) THEN
          CALL ANCMSG(MSGID=858,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO)
        ENDIF
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('distribution',TYP,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('entityid',IGR,IS_AVAILABLE,LSUBMODEL)
        IF (TYP == 1) THEN
          CALL HM_GET_INTV('grnd_ID',IGR,IS_AVAILABLE,LSUBMODEL)
          CALL HM_GET_INTV('distribution_table_count',NB_LINE,IS_AVAILABLE,LSUBMODEL)
          IF(.NOT.ALLOCATED(LIST_NOD)) ALLOCATE(LIST_NOD(NB_LINE))
          LIST_NOD(1:NB_LINE) = 0
          IF(.NOT.ALLOCATED(LIST_TEMP0)) ALLOCATE(LIST_TEMP0(NB_LINE))
          LIST_TEMP0(1:NB_LINE) = ZERO
          DO J=1,NB_LINE
            CALL HM_GET_INT_ARRAY_INDEX('location_unit_node',LIST_NOD(J),J,IS_AVAILABLE,LSUBMODEL)
          ENDDO
        ENDIF
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        IF (TYP == 0) THEN
          CALL HM_GET_FLOATV('magnitude',TEMP0,IS_AVAILABLE,LSUBMODEL,UNITAB)
        ELSEIF (TYP == 1) THEN
          CALL HM_GET_FLOAT_ARRAY_INDEX('T0',TEMP0,J,IS_AVAILABLE,LSUBMODEL,UNITAB)
          DO J=1,NB_LINE
            CALL HM_GET_FLOAT_ARRAY_INDEX('magnitude',LIST_TEMP0(J),J,IS_AVAILABLE,LSUBMODEL,UNITAB)
          ENDDO
        ENDIF
C--------------------------------------------------  
        NBTEMP = NBTEMP+1
        INITIDS(NBTEMP)=ID              
        IGRS=0     
        IF (TYP == 0) THEN                                           
          IF (IGR == 0)THEN 
            CALL ANCMSG(MSGID=668,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  C1='/INITEM',
     .                  C2='/INITEM',
     .                  C3=TITR,
     .                  I1=ID)
          ENDIF                                                       
          DO J=1,NGRNOD                                               
            IF (IGR == IGRNOD(J)%ID) IGRS=J
          ENDDO                                                       
          IF(IGRS /= 0)THEN                                           
            DO J=1,IGRNOD(IGRS)%NENTITY
              NOSYS=IGRNOD(IGRS)%ENTITY(J)
              TEMP(NOSYS)= TEMP0
            ENDDO                                                     
            NNOD=IGRNOD(IGRS)%NENTITY
          ELSE
            CALL ANCMSG(MSGID=53,
     .                  MSGTYPE=MSGERROR,
     .                  ANMODE=ANINFO,
     .                  C1='IN /INITEM OPTION',
     .                  I1=IGR)
          ENDIF   
        ELSE                                                   
          DO J=1,NGRNOD                                               
            IF (IGR == IGRNOD(J)%ID) IGRS=J
          ENDDO                                                       
          IF(IGRS /= 0)THEN                                           
            DO J=1,IGRNOD(IGRS)%NENTITY
              NOSYS=IGRNOD(IGRS)%ENTITY(J)
              TEMP(NOSYS)= TEMP0
            ENDDO                                                     
            NNOD=IGRNOD(IGRS)%NENTITY
          ENDIF      
          DO J=1,NB_LINE
            NODSYS=USR2SYS(LIST_NOD(J),ITABM1,MESS,ID)
            IF (LIST_NOD(J) == 0) THEN
              CALL ANCMSG(MSGID=78,
     .                    MSGTYPE=MSGERROR,
     .                    ANMODE=ANINFO,
     .                    C1='/INITEM',
     .                    I1=ID,  
     .                    I2=NOD)
            ENDIF
            IF (NODSYS /= 0) TEMP(NODSYS)= LIST_TEMP0(J)
          ENDDO 
        ENDIF 
        IF(ALLOCATED(LIST_TEMP0)) DEALLOCATE(LIST_TEMP0) 
        IF(ALLOCATED(LIST_NOD)) DEALLOCATE(LIST_NOD) 
       ENDDO  
C---                                                           
      CALL UDOUBLE(INITIDS,1,NBTEMP,MESS,0,BID)
C--------------------------------------------------
C     PRINT
C--------------------------------------------------
      J=0
      RETURN
C
2000  FORMAT(/, ' INITIAL TEMPERATURE ',/' -------------------',//
     + 6X,'NODE',17X,'TEMP ' )
      RETURN
      END
