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
Chd|  HM_READ_LOAD_CENTRI           source/loads/general/load_centri/hm_read_load_centri.F
Chd|-- called by -----------
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        NODGRNR5                      source/starter/freform.F      
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_LOAD_CENTRI(ICFIELD,LCFIELD  ,CFIELD  ,ITAB     ,ITABM1 ,
     .                               IGRNOD ,NPC      ,SENSOR_TAB ,UNITAB   ,IFRAME ,
     .                               LSUBMODEL)
C
C-----------------------------------------------
C FAC_M FACL FAC_T : enable to convert (custom) input unit to working unit system
C FAC_MASS, FAC_LENGTH, FAC_TIME : enable to convert working unit system into International Unit system
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE R2R_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER ICFIELD(SIZFIELD,*), LCFIELD(*), ITAB(*), ITABM1(*), 
     .  NPC(*),IFRAME(LISKN,*)
      my_real CFIELD(LFACLOAD,*)
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
      TYPE (SENSOR_STR_) ,DIMENSION(NSENSOR) :: SENSOR_TAB
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .        FCX,FCY,FAC_FCX,FAC_FCY
      INTEGER I,J,K,K1,K2,NOD, NCUR, IFRA, ISENS,NN,IGU,IGS,
     .        UID,IAD,ID,IFLAGUNIT,NCURS,TSENS,IFRAS, IVAR, ISU, IDIR,
     .        SUB_INDEX, SUB_ID
      INTEGER FLAG_FMT,FLAG_FMT_TMP,IFIX_TMP
      CHARACTER MESS*40, XYZ*ncharfield, 
     .          char_X*1, char_Y*1, char_Z*1, char_XX*2, char_YY*2, char_ZZ*2
      CHARACTER TITR*nchartitle
      LOGICAL IS_AVAILABLE

      DATA char_X/'X'/
      DATA char_Y/'Y'/
      DATA char_Z/'Z'/
      DATA char_XX/'XX'/
      DATA char_YY/'YY'/
      DATA char_ZZ/'ZZ'/
      DATA MESS/'CENTRIFUGAL LOAD DEFINITION           '/
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NODGRNR5
      EXTERNAL NODGRNR5
C-----------------------------------------------
C   C o m m e n t s
C-----------------------------------------------      
C
C                         /LOAD/CENTRI              
C
C     ICFIELD(SIZFIELD,NLOADC)
C     ICFIELD(1,K) :   NN           Nb of nodes                                            
C     ICFIELD(2,K) :   IDIR         Direction of the load               
C     ICFIELD(3,K) :   NCURS        Velocity function internal number                              
C     ICFIELD(4,K) :   IAD          Address of nodes in skyline vector LCFIELD
C     ICFIELD(5,K) :   1            ! Type /LOAD/CENTRI   
C     ICFIELD(6,K) :   TSENS        Sensor internal number                                          
C     ICFIELD(7,K) :   IFRAS        Frame internal number                 
C     ICFIELD(8,K) :   IVAR         Flag for vel variation 
C     ICFIELD(9,K) :   ID           User ID of this /LOAD/CENTRI 
C     ICFIELD(10,K):                                                                               
C     ICFIELD(11,K):                                                     
C     ICFIELD(12,K):                                                     
C     ICFIELD(13,K):                                                        
C-----------------------------------------------
C     LCFIELD(SLCFIELD) skyline vector of nodes vs all /LOAD/CENTRI
C-----------------------------------------------
C     CFIELD(LFACLOAD,NLOADC)
C     CFIELD(1,K) : FCY
C     CFIELD(2,K) : ONE/FCX
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------      
      IS_AVAILABLE = .FALSE.

      IAD          = 1
      NN           = 0 
      
C--------------------------------------------------
C START BROWSING MODEL CLOAD
C--------------------------------------------------
      CALL HM_OPTION_START('/LOAD/CENTRI')
C--------------------------------------------------
C BROWSING MODEL 1..NLOADC
C--------------------------------------------------
      DO K=1,NLOADC
C--------------------------------------------------
C EXTRACT DATAS OF /LOAD/CENTRI... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       UNIT_ID = UID,
     .                       SUBMODEL_ID = SUB_ID,
     .                       SUBMODEL_INDEX = SUB_INDEX,
     .                       OPTION_TITR = TITR)
C--------------------------------------------------
C EXTRACT DATAS (STRING VALUES)
C--------------------------------------------------
        XYZ = ''
        CALL HM_GET_STRING('rad_dir',XYZ,ncharfield,IS_AVAILABLE)
C--------------------------------------------------
C EXTRACT DATAS (INTEGER VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('curveid',NCUR,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('inputsystem',IFRA,IS_AVAILABLE,LSUBMODEL)
        IF(IFRA == 0 .AND. SUB_ID /= 0 ) THEN
          CALL ANCMSG(MSGID   = 1712,
     .                ANMODE  = ANINFO,
     .                MSGTYPE = MSGWARNING,
     .                I1      = ID,
     .                I2      = SUB_ID,
     .                C1      = '/LOAD/CENTRI - frame_ID=0',
     .                C2      = TITR)  
        END IF
        CALL HM_GET_INTV('rad_sensor_id',ISENS,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('entityid',IGU,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('rad_ivar_flag',IVAR,IS_AVAILABLE,LSUBMODEL)
C--------------------------------------------------
C EXTRACT DATAS (REAL VALUES)
C--------------------------------------------------
        CALL HM_GET_FLOATV('xscale',FCX,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('xscale',FAC_FCX,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('magnitude',FCY,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('magnitude',FAC_FCY,IS_AVAILABLE,LSUBMODEL,UNITAB)
C--------------------------------------------------
        IFLAGUNIT = 0
        DO J=1,NUNITS
          IF (UNITAB%UNIT_ID(J) == UID) THEN
            IFLAGUNIT = 1
            EXIT
          ENDIF
        ENDDO
        IF (UID/=0.AND.IFLAGUNIT==0) THEN
          CALL ANCMSG(MSGID   = 659,
     .                ANMODE  = ANINFO,
     .                MSGTYPE = MSGERROR,
     .                I2      = UID,
     .                I1      = ID,
     .                C1      = 'CENTRIFUGAL LOAD',
     .                C2      = 'CENTRIFUGAL LOAD',
     .                C3      = TITR)  
        ENDIF
C----------- 
        IDIR=0       
        IF(XYZ(1:1)==char_X) IDIR=1
        IF(XYZ(1:1)==char_Y) IDIR=2
        IF(XYZ(1:1)==char_Z) IDIR=3
        IF(XYZ(1:2)==char_XX)IDIR=4
        IF(XYZ(1:2)==char_YY)IDIR=5
        IF(XYZ(1:2)==char_ZZ)IDIR=6
        IF(IDIR==0) THEN
           CALL ANCMSG(MSGID=881,
     .                 MSGTYPE=MSGERROR,
     .                 ANMODE=ANINFO_BLIND_1,
     .                 I1=ID,
     .                 C1=XYZ)
        ENDIF
C-----------        
        NCURS=0
        DO J=1,NFUNCT
          IF(NPC(NFUNCT+J+1)==NCUR)NCURS=J
        ENDDO
        IF(NCURS==0)THEN
          CALL ANCMSG(MSGID=883,
     .                MSGTYPE=MSGERROR,
     .                ANMODE=ANINFO_BLIND_1,
     .                I1=ID,
     .                C1=TITR,
     .                I2=NCUR)
        ENDIF
C-----------        
        IF (FCX == ZERO) FCX = FAC_FCX
        IF (FCY == ZERO) FCY = FAC_FCY
C-----------        
        IF(IFRA /=0)THEN
          DO J=0,NUMFRAM
            IF(IFRA==IFRAME(4,J+1)) THEN
              IFRAS=J
              EXIT
            ENDIF
            IF (J==NUMFRAM) THEN
              CALL ANCMSG(MSGID=490,
     .             MSGTYPE=MSGERROR,
     .             ANMODE=ANINFO_BLIND_1,
     .             C1='/LOAD/CENTRI',
     .             I1=ID,
     .             C2='/LOAD/CENTRI',
     .             C3=TITR,
     .             I2=IFRA)
            ENDIF
          ENDDO
        ELSE
          IFRAS=0
        ENDIF
C-----------        
        TSENS = 0
	       IF (ISENS/=0) THEN
          DO J=1,NSENSOR
	           IF (ISENS == SENSOR_TAB(J)%SENS_ID) TSENS=J
	         ENDDO
	         IF (TSENS==0) THEN
             CALL ANCMSG(MSGID=884, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     .            I1=ID,
     .            C1=TITR,
     .            I2=ISENS)
	         ENDIF
	       ENDIF
C-----------        
        NN = NODGRNR5(IGU,IGS,LCFIELD(IAD),IGRNOD,ITABM1,MESS)
C-----------        
        IF (IVAR == 0) IVAR = 1
C-----------        
	       ICFIELD(1,K)  = NN
	       ICFIELD(2,K)  = IDIR
        ICFIELD(3,K)  = NCURS
        ICFIELD(4,K)  = IAD
        ICFIELD(5,K)  = 1 ! Type /LOAD/CENTRI
        ICFIELD(6,K)  = TSENS
        ICFIELD(7,K)  = IFRAS
        ICFIELD(8,K)  = IVAR
        ICFIELD(9,K)  = ID
        CFIELD(1,K)   = FCY
        CFIELD(2,K)   = ONE/FCX
C-----------
        IF (IVAR == 1)THEN
          WRITE (IOUT,2000) ID
        ELSE
          WRITE (IOUT,2001) ID
        ENDIF
        WRITE (IOUT,'(I10,2X,I10,4X,A2,2X,I10,2X,I10,2X,
     .              1PG20.13,2X,1PG20.13)')
     .     IGU,IFRA,XYZ(1:2),
     .     NCUR,ISENS,FCX,FCY
C---------
        IAD = IAD + NN  !  NUMCENTRI
     
      ENDDO ! next K (next /LOAD/CENTRI)
C--------------------------------------------------------------------------------
      RETURN
C--------------------------------------------------------------------------------     
 2000 FORMAT(//
     .'     CENTRIFUGAL LOAD. . . . . . . . . . . . . . .  . . .',I10/
     .'     VARIATION OF VELOCITY FUNCTION NOT TAKEN INTO ACCOUNT'/
     .'     ------------------  '/
     .'NODE GROUP       FRAME   DIR  LOAD_CURVE      SENSOR',
     .'           SCALE_X               SCALE_Y')

 2001 FORMAT(//
     .'     CENTRIFUGAL LOAD. . . . . . . . . . . . . . .  . . .',I10/
     .'     VARIATION OF VELOCITY FUNCTION TAKEN INTO ACCOUNT'/
     .'     ------------------  '/
     .'NODE GROUP       FRAME   DIR  LOAD_CURVE      SENSOR',
     .'           SCALE_X               SCALE_Y')
C-----------------------------------------------
     
      END
