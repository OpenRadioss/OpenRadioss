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
Chd|  HM_READ_CLOAD                 source/loads/general/cload/hm_read_cload.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        NODGRNR5                      source/starter/freform.F      
Chd|        NODGR_R2R                     source/coupling/rad2rad/routines_r2r.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        R2R_MOD                       share/modules1/r2r_mod.F      
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_CLOAD(IBCL   ,FORC    ,NUM    ,ITAB   ,ITABM1 ,
     .                         IGRNOD ,NWORK   ,UNITAB ,ISKN   ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE R2R_MOD
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
#include      "scr17_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "warn_c.inc"
#include      "sysunit.inc"
#include      "submod_c.inc"
#include      "r2r_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB
      INTEGER NUM
      INTEGER IBCL(NIBCLD,*), ITAB(*), ITABM1(*),NWORK(*),
     .        ISKN(LISKN,*)
C     REAL
      my_real
     .   FORC(LFACCLD,*)
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(*)
C-----------------------------------------------
      TYPE (GROUP_)  ,DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .        FCX,FCY,FAC_FCX,FAC_FCY
      INTEGER I,J,K,K1,K2,NOD, NCUR, NOSKEW, ISENS,NLD0,NN,IGU,IGS,
     .        UID,IAD,NS,IWA,ID,NUM0,IFLAGUNIT,COMPT,SUB_INDEX,IDIR
      INTEGER NNB
      CHARACTER MESS*40, XYZ*ncharfield,
     .          X*1, Y*1, Z*1, XX*2, YY*2, ZZ*2
      CHARACTER TITR*nchartitle
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NODGRNR5,NODGR_R2R
      EXTERNAL NODGRNR5,NODGR_R2R
C-----------------------------------------------
C     IBCL(NIBCLD,NUMCLD+NUMPRES), NUMCLD = Total nb of (cloads * nodes)
C                             NUMPRES = Total nb of (ploads * segments)
C     IBCL(1:NIBCLD,1:NUMCLD)              IPRES = IBCL(1:NIBCLD,NUMCCLD+1,NUMCLD+NUMPRES)         
C     1: Node Number                       1st node number of the segment
C     2: NS = 10*Noskew+Idir               2nd node number of the segment
C     3: Function internal number          3rd node number of the segment
C     4: -1 <=> CLOAD                      4th node number of the segment
C     5: UNUSED                            Function internal number
C     6: ISENS Sensor User ID              ISENS Sensor User ID
C     7: User ID                           User ID
C-----------------------------------------------
C     FORC(LFACCLD,NUMCLD+NUMPRES)
C     FORC(LFACCLD,NUMCLD)                 PRES = FORC(LFACCLD,NUMCLD+1:NUMCLD+NUMPRES)
C     1: Fscale_y                          Fscale_y  
C     2: 1/Ascale_x                        1/Ascale_x
C     3 : UNUSED                           /=0 <=> Pinching pressure
C=======================================================================
      DATA X/'X'/
      DATA Y/'Y'/
      DATA Z/'Z'/
      DATA XX/'XX'/
      DATA YY/'YY'/
      DATA ZZ/'ZZ'/
      DATA MESS/'CONCENTRATED LOADS DEFINITION           '/
C=======================================================================
      IS_AVAILABLE = .FALSE.
C
      WRITE (IOUT,2000)
      NLD0=NUM
      NUM=0
      I=0
C--------------------------------------------------
C START BROWSING MODEL CLOAD
C--------------------------------------------------
      CALL HM_OPTION_START('/CLOAD')
C--------------------------------------------------
C BROWSING MODEL PARTS 1->NLD0 (=NCONLD)
C--------------------------------------------------
      DO K=1,NLD0
        IF(NSUBDOM>0)THEN
          IF(NNCL(K)==0)CYCLE
        END IF
        TITR = ''
C--------------------------------------------------
C EXTRACT DATAS OF /CLOAD/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       UNIT_ID = UID,
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
        CALL HM_GET_INTV('inputsystem',NOSKEW,IS_AVAILABLE,LSUBMODEL)
        IF(NOSKEW == 0 .AND. SUB_INDEX /= 0 ) NOSKEW = LSUBMODEL(SUB_INDEX)%SKEW
        CALL HM_GET_INTV('rad_sensor_id',ISENS,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('entityid',IGU,IS_AVAILABLE,LSUBMODEL)
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
c
        IF (UID/=0.AND.IFLAGUNIT==0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I2=UID,I1=ID,C1='CONCENTRED LOAD',
     .                 C2='CONCENTRED LOAD',
     .                 C3=TITR)
        ENDIF
        DO J=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
          IF(NOSKEW == ISKN(4,J+1)) THEN
            NOSKEW=J+1
            GO TO 100
          ENDIF
        ENDDO
        CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              C1='CONCENTRED LOAD',
     .              C2='CONCENTRED LOAD',
     .              I2=NOSKEW,I1=ID,C3=TITR)
 100    CONTINUE
C
        IF (FCX == ZERO) FCX = FAC_FCX
        IF (FCY == ZERO) FCY = FAC_FCY
        NOSKEW=10*NOSKEW
        NS=0

        IDIR = 0
        IF(XYZ(1:1)==X) IDIR=1
        IF(XYZ(1:1)==Y) IDIR=2
        IF(XYZ(1:1)==Z) IDIR=3
        IF(XYZ(1:2)==XX) IDIR=4
        IF(XYZ(1:2)==YY) IDIR=5
        IF(XYZ(1:2)==ZZ) IDIR=6

        IF(IDIR == 1) NS=1+NOSKEW
        IF(IDIR == 2) NS=2+NOSKEW
        IF(IDIR == 3) NS=3+NOSKEW
        IF(IDIR == 4) NS=4+NOSKEW
        IF(IDIR == 5) NS=5+NOSKEW
        IF(IDIR == 6) NS=6+NOSKEW


        IF(IDIR == 0) THEN
           CALL ANCMSG(MSGID=149,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                 C2=XYZ,I1=ID,C1=TITR)
        ENDIF
        IF(IDIR >= 4) THEN
           IF (IRODDL==0) THEN
             CALL ANCMSG(MSGID=845,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                   C2=XYZ,I1=ID,C1=TITR)
           END IF
        END IF
C !!  IBCL ET NWORK ONT LA MEME ADRESSE
        NUM0=NUM
C-----------
        IF (IDDOM==0) THEN
        NN = NODGRNR5(IGU    ,IGS    ,NWORK(1+NIBCLD*NUM0),IGRNOD ,
     .                ITABM1 ,MESS   )
        ELSE
C-----------Multidomaines : on enleve les noeuds communs qui sont deja trait   s dans le fomain full-------------
        NN = NODGR_R2R(IGU    ,IGS    ,NWORK(1+NIBCLD*NUM0),IGRNOD   ,
     .                ITABM1 ,MESS      )
        ENDIF
C-----------
        IF (NN==0) THEN
         CALL ANCMSG(MSGID=54,
     .               ANMODE=ANINFO,
     .               MSGTYPE=MSGERROR,
     .               C1=KLINE)
        ENDIF
        NUM=NUM+NN
        DO J=NN,1,-1
C       !!  IBCL ET NWORK ONT LA MEME ADRESSE
C          IBCL(1,I+J)=NWORK(J+6*NUM0)
            NWORK(1+NIBCLD*(J+I-1))=NWORK(J+NIBCLD*NUM0)
        ENDDO

        DO J=1,NN
          I=I+1
          IBCL(2,I)      = NS
          IBCL(3,I)      = NCUR
          IBCL(4,I)      = -1
          IBCL(6,I)      = ISENS
          IBCL(7,I)      = 0
          IBCL(8,I)      = 0
          FORC(1,I) = FCY
          FORC(2,I) = ONE/FCX
          IF (IDIR <= 3) THEN
             WRITE (IOUT,'(I10,2X,I10,5X,A,2X,I10,2X,I10,2X,
     .                  1PG20.13,2X,1PG20.13)')
     .       ITAB(IBCL(1,I)),ISKN(4,NOSKEW/10),XYZ(1:1),
     .       IBCL(3,I),ISENS,FCX,FCY
          ELSEIF (IDIR <= 6) THEN
             WRITE (IOUT,'(I10,2X,I10,4X,A2,2X,I10,2X,I10,2X,
     .                  1PG20.13,2X,1PG20.13)')
     .       ITAB(IBCL(1,I)),ISKN(4,NOSKEW/10),XYZ(1:2),
     .       IBCL(3,I),ISENS,FCX,FCY
          ENDIF
        ENDDO
      ENDDO

C
 2000 FORMAT(//
     .'     CONCENTRATED LOADS  '/
     .'     ------------------  '/
     .'      NODE        SKEW   DIR  LOAD_CURVE      SENSOR',
     .'           SCALE_X               SCALE_Y')
      RETURN
      END
