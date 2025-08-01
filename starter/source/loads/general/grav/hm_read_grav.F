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
!||    hm_read_grav           ../starter/source/loads/general/grav/hm_read_grav.F
!||--- called by ------------------------------------------------------
!||    lectur                 ../starter/source/starter/lectur.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    hm_get_floatv          ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim      ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_get_string          ../starter/source/devtools/hm_reader/hm_get_string.F
!||    hm_option_read_key     ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start        ../starter/source/devtools/hm_reader/hm_option_start.F
!||    nodgrnr5               ../starter/source/starter/freform.F
!||    remove_nd              ../starter/source/elements/solid/solide10/dim_s10edg.F
!||    usr2sys                ../starter/source/system/sysfus.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    reader_old_mod         ../starter/share/modules1/reader_old_mod.F90
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE HM_READ_GRAV(IGRV   ,LGRAV   ,GRAV      ,ITAB    ,ITABM1  ,
     .                        IGRNOD ,NPC     ,SENSORS   ,UNITAB  ,ISKN    ,
     .                        ITAGND ,LSUBMODEL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD , ONLY : NSUBMOD, SUBMODEL_DATA
      USE HM_OPTION_READ_MOD
      USE SENSOR_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARFIELD
      USE READER_OLD_MOD , ONLY : IREC
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "units_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER IGRV(NIGRV,NGRAV), LGRAV(*), ITAB(*), ITABM1(*),
     .        NPC(*), ISKN(LISKN,*), ITAGND(*)
      my_real
     .   GRAV(LFACGRV,NGRAV)
      TYPE(SUBMODEL_DATA),INTENT(IN)::LSUBMODEL(NSUBMOD)
      TYPE (SENSORS_) ,INTENT(IN) :: SENSORS
C-----------------------------------------------
C     IGRV(LIBRV,NGRAV) 
C-----------------------------------------------
C     IGRV(1,K) : NN Nb of Nodes
C     IGRV(2,K) : NS=10*NOSKEW+IDIR, IDIR=1 (X),2 (Y),3 (Z)
C     IGRV(3,K) : Function (internal) number
C     IGRV(4,K) : IAD Address of nodes in skyline vector LGRAV(IAD:IAD+NN-1)
C     IGRV(5,K) : Option ID
C     IGRV(6,K) : Sensor (internal) number
C-----------------------------------------------
C     LGRAV(1:SLGRAV) skyline vector of nodes numbers (one /GRAV option after the other)
C-----------------------------------------------
C     GRAV(LFACGRV,NGRAV)   
C     GRAV(1,K) : FCY
C     GRAV(2,K) : ONE/FCX
C-----------------------------------------------
      TYPE (GROUP_)  , DIMENSION(NGRNOD)  :: IGRNOD
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real FCX,FCY,FAC_FCX,FAC_FCY
      INTEGER I, NCUR, NOSKEW,NSKW, ISENS,NN,IGU,IGS,UID,IAD,NS,J,K,ID,NCURS,TSENS, IFLAGUNIT, SUB_INDEX
      CHARACTER(LEN=NCHARFIELD) :: XYZ
      CHARACTER :: X*1, Y*1, Z*1, XX*2, YY*2, ZZ*2, MESS*40
      CHARACTER(LEN=NCHARTITLE)::TITR
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  USR2SYS,NODGRNR5
      EXTERNAL USR2SYS,NODGRNR5
C-----------------------------------------------
      DATA X/'X'/
      DATA Y/'Y'/
      DATA Z/'Z'/
      DATA XX/'XX'/
      DATA YY/'YY'/
      DATA ZZ/'ZZ'/
      DATA MESS/'GRAVITY LOADS DEFINITION                '/
C=======================================================================
      IS_AVAILABLE = .FALSE.
c
C
      IAD=1
      IF(NGRAV==0)RETURN   
C
      WRITE (IOUT,2000)
C--------------------------------------------------
C START BROWSING MODEL GRAV
C--------------------------------------------------
      CALL HM_OPTION_START('/GRAV')
C--------------------------------------------------
C BROWSING MODEL PARTS 1->NGRAV
C--------------------------------------------------
      DO K=1,NGRAV
        TITR = ''
C--------------------------------------------------
C EXTRACT DATAS OF /PART/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = ID,
     .                       UNIT_ID = UID,
     .                       SUBMODEL_INDEX = SUB_INDEX,
     .                       OPTION_TITR = TITR)
C--------------------------------------------------
C EXTRACT DATAS (STRING VALUES)
C--------------------------------------------------
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
        DO J=1,UNITAB%NUNITS
          IF (UNITAB%UNIT_ID(J) == UID) THEN
            IFLAGUNIT = 1
            EXIT
          ENDIF
        ENDDO
        IF (UID/=0.AND.IFLAGUNIT==0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I2=UID,I1=ID,C1='GRAVITY LOAD',
     .                 C2='GRAVITY LOAD',
     .                 C3=TITR) 
        ENDIF
        DO J=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
          IF(NOSKEW == ISKN(4,J+1)) THEN
            NOSKEW=J+1
            GO TO 100
          ENDIF
        ENDDO
        CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              C1='GRAVITY LOAD',
     .              C2='GRAVITY LOAD',
     .              I2=NOSKEW,I1=ID,C3=TITR)
 100  CONTINUE

        IF (FCX == ZERO) FCX = FAC_FCX
        IF (FCY == ZERO) FCY = FAC_FCY

        NSKW=10*NOSKEW
        NS=0
        IF (XYZ(1:1)==X)   THEN
          NS=1+NSKW
        ELSEIF(XYZ(1:1)==Y)THEN
          NS=2+NSKW
        ELSEIF(XYZ(1:1)==Z)THEN
          NS=3+NSKW
        ELSE
          XYZ='Z'
          NS=3+NSKW
c          CALL ANCMSG(MSGID=153,ANMODE=ANINFO,MSGTYPE=MSGERROR,
c     .                C2=XYZ,I1=ID,C1=TITR) 
        ENDIF
C
        IF (IGU /= 0) THEN
          NN = NODGRNR5(IGU,IGS,LGRAV(IAD),IGRNOD,ITABM1,MESS)
        ELSE
          NN = NUMNOD
          DO I=1,NUMNOD
            LGRAV(IAD-1+I)=I
          ENDDO
        ENDIF
        IF (NS10E > 0 ) CALL REMOVE_ND(NN,LGRAV(IAD),ITAGND)
C
        IREC = IREC + 1
        IGRV(1,K)=NN
        IGRV(2,K)=NS
        NCURS=0
        IF(NCUR/=0) THEN
          DO J=1,NFUNCT
            IF(NPC(NFUNCT+J+1)==NCUR)NCURS=J
          ENDDO
          IF(NCURS==0)THEN
            CALL ANCMSG(MSGID=154,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I2=NCUR,I1=ID,C1=TITR) 
          ENDIF
        ENDIF
        IGRV(3,K)=NCURS
        IGRV(4,K)=IAD
                 IGRV(5,K)=ID
        IGRV(6,K)=ISENS
        GRAV(1,K) = FCY
        GRAV(2,K) = ONE/FCX
        TSENS=0
        DO J=1,SENSORS%NSENSOR
          IF(ISENS/=0) THEN
            IF (IGRV(6,K) == SENSORS%SENSOR_TAB(J)%SENS_ID) TSENS=J
          ENDIF
        ENDDO
        IF((TSENS==0).AND.(IGRV(6,K)/=0))THEN
          CALL ANCMSG(MSGID=521,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .               I2=IGRV(6,K),I1=ID,C1=TITR) 
        ENDIF
        IF (NOSKEW > 0)   NOSKEW = ISKN(4,NOSKEW)
        WRITE (IOUT,3000) NOSKEW,XYZ(1:1),
     .                    NCUR,ISENS,FCX,FCY
        WRITE (IOUT,'(10I10)') (ITAB(LGRAV(J+IAD-1)),J=1,NN)
        IAD=IAD+NN
      ENDDO
C-----------
      RETURN
C-----------
 2000 FORMAT(//
     .'     GRAVITY LOADS  '/
     .'     -------------  '/
     .'        SKEW   DIRECTION    LOAD CURVE',
     .'      SENSOR         SCALE_X             SCALE_Y     ')
 3000 FORMAT(2X,I10,10X,A2,4X,I10,2X,I10,2X,1P2G20.13)
C-----------
      RETURN
      END
