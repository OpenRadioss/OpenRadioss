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
Chd|  HM_READ_IMPTEMP               source/constraints/thermic/hm_read_imptemp.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        NODGRNR5                      source/starter/freform.F      
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_IMPTEMP(IBFT   ,FAC   ,NUM   ,ITABM1,
     .                     IGRNOD ,NWORK ,ITAB  ,UNITAB,LSUBMODEL)
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
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER NUM
      INTEGER IBFT(NIFT,*), ITAB(*), ITABM1(*), NWORK(*)
      my_real
     .   FAC(LFACTHER,*)
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, L, NOD, NCUR, SENS_ID, ID, UID
      INTEGER K, IGU, IGRS, NN, J, NUM0,
     .        IFLAGUNIT
      INTEGER FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP
      my_real
     .   FAC1,FAC2,FACX,FACY,FAC2_DIM,FACX_DIM,FACY_DIM
      CHARACTER MESS*40, TITR*nchartitle
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER NODGRNR5
C
      DATA MESS/'IMPOSED TEMPERATURE DEFINITION  '/
C======================================================================|
      IS_AVAILABLE = .FALSE. 
      NUM = 0    
      I = 0
C--------------------------------------------------
C START BROWSING MODEL IMPTEMP
C--------------------------------------------------
      CALL HM_OPTION_START('/IMPTEMP')
C--------------------------------------------------
C BROWSING /IMPTEMP OPTIONS 1->NIMTEMP 
C--------------------------------------------------
      DO K=1,NIMTEMP
        TITR = ''
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       UNIT_ID = UID,
     .                       OPTION_ID = ID,
     .                       OPTION_TITR = TITR) 
        IFLAGUNIT = 0                                           
        DO J=1,NUNITS                           
          IF (UNITAB%UNIT_ID(J) == UID) THEN                     
            IFLAGUNIT = 1                       
            EXIT                                
          ENDIF                                 
        ENDDO                                  
        IF (UID /= 0.AND.IFLAGUNIT == 0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I2=UID,I1=ID,C1='IMPOSED TEMPERATURE',
     .                C2='IMPOSED TEMPERATURE',
     .                C3=TITR)                              
        ENDIF 
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('curveid',NCUR,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('rad_sensor_id',SENS_ID,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('entityid',IGU,IS_AVAILABLE,LSUBMODEL)    
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('xscale',FACX,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('xscale',FACX_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('magnitude',FACY,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('magnitude',FACY_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('rad_tstart',FAC1,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('rad_tstop',FAC2,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('rad_tstop',FAC2_DIM,IS_AVAILABLE,LSUBMODEL,UNITAB)
C--------------------------------------------------
        IF (FACY == ZERO) FACY=FACY_DIM
        IF (FACX == ZERO) FACX=FACX_DIM
        IF (FAC2 == ZERO) FAC2=EP30 * FAC2_DIM
        FACX = ONE / FACX
C--------------------------------------------------
        NUM0 = NUM
        NN = NODGRNR5(IGU    ,IGRS   ,NWORK(1+NIFT*NUM0),IGRNOD ,
     .                ITABM1 ,MESS   )
        NUM = NUM + NN
C
        DO J=NN,1,-1
C       !!  IBFV ET NWORK ONT LA MEME ADRESSE
C       !!  IBFV ECRASE PROGRESSIVEMENT NWORK
C            IBFV(1,I+J)=NWORK(J+5*NUM0)
            NWORK(1+NIFT*(I+J-1))=NWORK(J+NIFT*NUM0)
        ENDDO
C        
        IF(NN > 0 )WRITE(IOUT, 2001)
        DO J=1,NN
            I=I+1
            NOD = ITAB(IABS(IBFT(1,I)))
            IBFT(2,I)= NCUR
            IBFT(3,I)= SENS_ID
            IBFT(4,I) = 0
C            
            FAC(1,I)= FAC1
            FAC(2,I)= FAC2
            FAC(3,I)= FACX
            FAC(4,I)= FACY
C
            WRITE (IOUT,'(3X,I10,3X,I10,3X,I10,2X,
     .       1PG20.13,2X,1PG20.13,2X,G20.13,2X,G20.13)')
     .       ITAB(IABS(IBFT(1,I))),
     .       IBFT(2,I),SENS_ID,FAC(4,I),1/FACX,FAC(1,I),FAC(2,I)
C
        ENDDO
      ENDDO  
C
      RETURN
C          
 2001 FORMAT(//
     .'     IMPOSED TEMPERATURE   '/
     .'     -------------------   '/
     . 9X,'NODE   LOAD_CURVE       SENSOR            FSCALE       ',
     . 9X,'ASCALE                START_TIME             STOP_TIME ')
      END
