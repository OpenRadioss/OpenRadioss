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
!||    read_impvel            ../starter/source/constraints/general/impvel/read_impvel.F
!||--- called by ------------------------------------------------------
!||    hm_read_impvel         ../starter/source/constraints/general/impvel/hm_read_impvel.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                 ../starter/source/output/message/message.F
!||    fretitl                ../starter/source/starter/freform.F
!||    hm_get_floatv          ../starter/source/devtools/hm_reader/hm_get_floatv.F
!||    hm_get_floatv_dim      ../starter/source/devtools/hm_reader/hm_get_floatv_dim.F
!||    hm_get_intv            ../starter/source/devtools/hm_reader/hm_get_intv.F
!||    hm_get_string          ../starter/source/devtools/hm_reader/hm_get_string.F
!||    hm_option_read_key     ../starter/source/devtools/hm_reader/hm_option_read_key.F
!||    hm_option_start        ../starter/source/devtools/hm_reader/hm_option_start.F
!||    kinset                 ../starter/source/constraints/general/kinset.F
!||    nodgrnr5               ../starter/source/starter/freform.F
!||    usr2sys                ../starter/source/system/sysfus.F
!||--- uses       -----------------------------------------------------
!||    hm_option_read_mod     ../starter/share/modules1/hm_option_read_mod.F
!||    message_mod            ../starter/share/message_module/message_mod.F
!||    submodel_mod           ../starter/share/modules1/submodel_mod.F
!||====================================================================
      SUBROUTINE READ_IMPVEL(
     .           NFVEL    ,INUM     ,IOPT     ,FBFVEL   ,IBFVEL   ,
     .           ITAB     ,ITABM1   ,IKINE    ,IKINE1LAG,NOM_OPT  ,
     .           IGRNOD   ,ISKN     ,UNITAB   ,LSUBMODEL)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE UNITAB_MOD
      USE NAMES_AND_TITLES_MOD , ONLY : NCHARTITLE, NCHARFIELD, NCHARKEY
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN)    :: NFVEL
      INTEGER ,INTENT(INOUT) :: INUM,IOPT
      INTEGER ,DIMENSION(*)  :: ITAB,ITABM1,IKINE,IKINE1LAG
      INTEGER ,DIMENSION(LISKN,*)        ,INTENT(IN)  :: ISKN
      INTEGER ,DIMENSION(LNOPT1,*)       ,INTENT(OUT) :: NOM_OPT
      INTEGER ,DIMENSION(NIFV,NFXVEL)    ,INTENT(OUT) :: IBFVEL
      my_real ,DIMENSION(LFXVELR,NFXVEL) ,INTENT(OUT) :: FBFVEL
      TYPE (UNIT_TYPE_)  ,INTENT(IN) ::  UNITAB
      TYPE (GROUP_)      ,DIMENSION(NGRNOD) ,INTENT(IN) :: IGRNOD
      TYPE(SUBMODEL_DATA),DIMENSION(*)      ,INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,JJ,NN,IVEL,IDIS,INOD,NODID,NOD,NOSKEW,NOFRAME,SENS_ID,
     .   OPTID,SYS_TYPE,UID,FCT_ID,SKEW_ID,FRAME_ID,GRN,IGS,LEN,
     .   ILAGM,FGEO,ICOOR,IUNIT,FLAGUNIT,SUBID,NOSUB,NN_FM(3)
      INTEGER ,DIMENSION(NFXVEL)   :: NODENUM
      INTEGER ,DIMENSION(3*NUMNOD) :: IKINE1
      my_real :: YSCALE,TSTART,TSTOP,XSCALE,FSCAL_T,FSCAL_V
      CHARACTER(LEN=2)           :: X,Y,Z,XX,YY,ZZ
      CHARACTER(LEN=NCHARFIELD)  :: XYZ
      CHARACTER(LEN=NCHARKEY)    :: KEY
      CHARACTER(LEN=NCHARTITLE)  :: TITR,MESS
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NODGRNR5,USR2SYS
      EXTERNAL NODGRNR5,USR2SYS
C-----------------------------------------------
C   D a t a
C-----------------------------------------------
      DATA X  /'X'/
      DATA Y  /'Y'/
      DATA Z  /'Z'/
      DATA XX /'XX'/
      DATA YY /'YY'/
      DATA ZZ /'ZZ'/
      DATA MESS/'IMPOSED VELOCITY DEFINITION  '/
C======================================================================|
      IS_AVAILABLE = .FALSE.
c
      IKINE1(:) = 0
      NN_FM(1:3)=0
c--------------------------------------------------
c     START browsing /IMPVEL options
c--------------------------------------------------
c
      CALL HM_OPTION_START('/IMPVEL')
c
      WRITE (IOUT,1000)
c
c--------------------------------------------------
      DO IVEL = 1,NFVEL
c--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = OPTID,
     .                          UNIT_ID     = UID,
     .                          SUBMODEL_ID = SUBID,
     .                          SUBMODEL_INDEX = NOSUB,
     .                          OPTION_TITR = TITR,
     .                          KEYWORD2    = KEY)
c--------------------------------------------------
        IF (KEY(1:4) == 'FGEO')   CYCLE
        IF (KEY(1:6) == 'LAGMUL') CYCLE
c        
        IOPT = IOPT + 1
        NOM_OPT(1,IOPT) = OPTID
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,IOPT),LTITR)
c        
        FRAME_ID = 0
        SYS_TYPE = 1  ! skew = 1 ,frame =2
        ICOOR    = 0  ! ICOOR = 1 => axial coordinates
        FGEO     = 0
        ILAGM    = 0
        IDIS     = 1
        LEN      = 1
        TSTART   = ZERO
        TSTOP    = INFINITY
c--------------------------------------------------
c       READ STRING VALUES from /IMPVEL
c--------------------------------------------------
        CALL HM_GET_INTV('rad_system_input_type'   ,SYS_TYPE ,IS_AVAILABLE,LSUBMODEL)
c        
        CALL HM_GET_INTV  ('curveid'       ,FCT_ID   ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_STRING('rad_dir'       ,XYZ      ,ncharfield,IS_AVAILABLE)
        CALL HM_GET_INTV  ('skew_ID'       ,SKEW_ID  ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV  ('rad_sensor_id' ,SENS_ID  ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV  ('entityid'      ,GRN      ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV  ('frame_ID'      ,FRAME_ID ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV  ('rad_icoor'     ,ICOOR    ,IS_AVAILABLE,LSUBMODEL)
c
        CALL HM_GET_FLOATV('xscale'        ,XSCALE ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('magnitude'     ,YSCALE ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('rad_tstart'    ,TSTART ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('rad_tstop'     ,TSTOP  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c
c--------------------------------------------------
c       CHECK IF Unit_ID exists
c--------------------------------------------------
        FLAGUNIT = 0
        DO IUNIT=1,UNITAB%NUNITS
          IF (UNITAB%UNIT_ID(IUNIT) == UID) THEN
            FLAGUNIT = 1
            EXIT
          ENDIF
        ENDDO
        IF (UID > 0 .and. FLAGUNIT == 0) THEN
          CALL ANCMSG(MSGID=659,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1= OPTID,
     .                I2= UID,
     .                C1='IMPDISP',
     .                C2='IMPDISP',
     .                C3= TITR)                              
        ENDIF
c--------------------------------------------------
c       Check skew and frame IDs
c--------------------------------------------------
        NOSKEW  = 0
        NOFRAME = 0
c----
        IF ((SKEW_ID == 0).AND.(FRAME_ID == 0).AND.(SUBID /= 0)) THEN
          SKEW_ID = LSUBMODEL(NOSUB)%SKEW
        ENDIF
c----
        IF ((SYS_TYPE == 0).OR.(SYS_TYPE == 1)) THEN
          DO J=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
            IF (SKEW_ID == ISKN(4,J+1)) THEN               
              NOSKEW = J+1                         
              EXIT                           
            ENDIF                                   
          ENDDO
          IF (SKEW_ID > 0 .and. NOSKEW == 0)
     .      CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1= OPTID,
     .                  I2= SKEW_ID,
     .                  C1='IMPOSED VELOCITY',
     .                  C2='IMPOSED VELOCITY',
     .                  C3= TITR)
c----
        ELSEIF (SYS_TYPE == 2) THEN
          JJ = (NUMSKW+1) + MIN(1,NSPCOND)*NUMSPH+1 + NSUBMOD        
          DO J=1,NUMFRAM                    
            JJ = JJ+1
            IF (FRAME_ID == ISKN(4,JJ)) THEN               
              NOFRAME = J+1                         
              NN_FM(1:3) = ISKN(1:3,JJ) 
              EXIT                           
            ENDIF                                   
          ENDDO            
          IF (FRAME_ID > 0 .and. NOFRAME == 0)
     .      CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                  I1= OPTID,
     .                  I2= FRAME_ID,
     .                  C1='IMPOSED VELOCITY',
     .                  C2='IMPOSED VELOCITY',
     .                  C3= TITR)
        ENDIF
c
c----
        IF (NOSKEW > 0 .AND. NOFRAME > 0) THEN
           CALL ANCMSG(MSGID=491,ANMODE=ANINFO_BLIND_1,
     .                 MSGTYPE=MSGERROR,
     .                 I1= OPTID,
     .                 I2= NOSKEW,
     .                 I3= NOFRAME,
     .                 C1= TITR)
        ENDIF
c--------------------------------------------------
c       Default scale factors
c--------------------------------------------------
        CALL HM_GET_FLOATV_DIM('xscale'   ,FSCAL_T ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('magnitude',FSCAL_V ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c
        IF (XSCALE == ZERO) XSCALE = ONE * FSCAL_T
        XSCALE = ONE / XSCALE
        IF (YSCALE == ZERO) YSCALE = ONE * FSCAL_V
        IF (TSTOP == ZERO)  TSTOP  = INFINITY
c
        IF (xyz(1:2) == XX .OR. xyz(1:2) == YY .OR. xyz(1:2) == ZZ) THEN
          LEN = 2
c          YSCALE = YSCALE / (FSCAL_V * FSCAL_T)
        ENDIF
c--------------------------------------------------
c       Read NODE numbers from the group
c
        NN = NODGRNR5(GRN,IGS,NODENUM,IGRNOD,ITABM1,MESS)
c
c--------------------------------------------------
        DO J=1,NN
          INUM  = INUM + 1
          IBFVEL(1, INUM) = NODENUM(J)
          IBFVEL(2 ,INUM) = 0
          IBFVEL(3 ,INUM) = FCT_ID
          IBFVEL(4 ,INUM) = SENS_ID
          IBFVEL(5 ,INUM) = 0
          IBFVEL(6 ,INUM) = 0  ! init dans lecrby (si vitesse de rotation sur main)
          IBFVEL(7 ,INUM) = IDIS
          IBFVEL(8 ,INUM) = ILAGM
          IBFVEL(9 ,INUM) = NOFRAME
          IBFVEL(10,INUM) = ICOOR
          IBFVEL(11,INUM) = 0
          IBFVEL(12,INUM) = IOPT
          IBFVEL(13,INUM) = FGEO
          IBFVEL(14,INUM) = 0
c
          FBFVEL(1,INUM)  = YSCALE
          FBFVEL(2,INUM)  = TSTART
          FBFVEL(3,INUM)  = TSTOP
          FBFVEL(4,INUM)  = ZERO
          FBFVEL(5,INUM)  = XSCALE
          FBFVEL(6,INUM)  = ZERO
c
          INOD  = IABS(NODENUM(J))
          NODID = ITAB(INOD)
c
c         ! SET DIRECTIONS AND TAG NODES WITH KINEMATIC CONDITIONS
c
          IF (NOFRAME > 0) THEN
            IF(XYZ(1:2) == XX)THEN
              IBFVEL(2,INUM) = 4
              CALL KINSET(16,NODID,IKINE(INOD),4,NOFRAME,IKINE1(INOD))
            ELSEIF(XYZ(1:2) == YY)THEN
              IBFVEL(2,INUM) = 5
              CALL KINSET(16,NODID,IKINE(INOD),5,NOFRAME,IKINE1(INOD))
            ELSEIF(XYZ(1:2) == ZZ)THEN
              IBFVEL(2,INUM) = 6
              CALL KINSET(16,NODID,IKINE(INOD),6,NOFRAME,IKINE1(INOD))
            ELSEIF (XYZ(1:1) == X)THEN
              IBFVEL(2,INUM) = 1
              CALL KINSET(16,NODID,IKINE(INOD),1,NOFRAME,IKINE1(INOD))
            ELSEIF(XYZ(1:1) == Y)THEN
              IBFVEL(2,INUM) = 2
              CALL KINSET(16,NODID,IKINE(INOD),2,NOFRAME,IKINE1(INOD))
            ELSEIF(XYZ(1:1) == Z)THEN
              IBFVEL(2,INUM) = 3
              CALL KINSET(16,NODID,IKINE(INOD),3,NOFRAME,IKINE1(INOD))
            ELSE
              CALL ANCMSG(MSGID=164, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .                    I1=OPTID,
     .                    C1=TITR,
     .                    C2=XYZ)
            ENDIF
c
            WRITE (IOUT, 3000) NODID,NOSKEW,FRAME_ID,XYZ(1:LEN),FCT_ID,SENS_ID,
     .                         YSCALE,ONE/XSCALE,TSTART,TSTOP,ICOOR
c
c---------------
c         check for N1,N2,N3 of frame not the imposed node
c---------------
            IF (INOD==NN_FM(1) .OR. INOD==NN_FM(2) .OR. INOD==NN_FM(3)) THEN
              CALL ANCMSG(MSGID=3091, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .                    I1=OPTID,
     .                    C1=TITR,
     .                    I2=NODID,
     .                    I3=FRAME_ID)
            END IF
c
          ELSE  ! SKEW
            IF(XYZ(1:2) == XX)THEN
              IBFVEL(2,INUM) = 4 + NOSKEW*10
              CALL KINSET(16,NODID,IKINE(INOD),4,NOSKEW,IKINE1(INOD))
            ELSEIF(XYZ(1:2) == YY)THEN
              IBFVEL(2,INUM) = 5 + NOSKEW*10
              CALL KINSET(16,NODID,IKINE(INOD),5,NOSKEW,IKINE1(INOD))
            ELSEIF(XYZ(1:2) == ZZ)THEN
              IBFVEL(2,INUM) = 6 + NOSKEW*10
              CALL KINSET(16,NODID,IKINE(INOD),6,NOSKEW,IKINE1(INOD))
            ELSEIF (XYZ(1:1) == X)THEN
              IBFVEL(2,INUM)=1 + NOSKEW*10
              CALL KINSET(16,NODID,IKINE(INOD),1,NOSKEW,IKINE1(INOD))
            ELSEIF(XYZ(1:1) == Y)THEN
              IBFVEL(2,INUM) = 2 + NOSKEW*10
              CALL KINSET(16,NODID,IKINE(INOD),2,NOSKEW,IKINE1(INOD))
            ELSEIF(XYZ(1:1) == Z)THEN
              IBFVEL(2,INUM) = 3 + NOSKEW*10
              CALL KINSET(16,NODID,IKINE(INOD),3,NOSKEW,IKINE1(INOD))
            ELSE
               CALL ANCMSG(MSGID=164, MSGTYPE=MSGERROR, ANMODE=ANINFO,
     .                    I1=OPTID,
     .                    C1=TITR,
     .                    C2=XYZ)
            ENDIF
c
            WRITE (IOUT,4000) NODID,ISKN(4,NOSKEW),0,XYZ(1:LEN),FCT_ID,SENS_ID,
     .         YSCALE,ONE/XSCALE,TSTART,TSTOP,ICOOR
c
          ENDIF
c-----------------------------------------------------------
c
        ENDDO ! NN
c
      ENDDO ! DO I=1,NFVEL
c
c-----------
      RETURN
c--------------------------------------------------
 1000 FORMAT(//
     .'     IMPOSED VELOCITIES    '/
     .'     -------------------    '/
     .'         NODE         SKEW        FRAME  DIRECTION   LOAD_CURVE',
     .'       SENSOR            FSCALE                ASCALE',
     .'            START_TIME                 STOP_TIME',
     .'         COORDINATE SYSTEM')
c--------------------------------------------------
 2000 FORMAT(//
     .'     IMPOSED VELOCITIES BY LAGRANGE MULTIPLIERS  '/
     .'     ------------------------------------------  '/
     .'         NODE         SKEW        FRAME  DIRECTION   LOAD_CURVE',
     .'       SENSOR            FSCALE                ASCALE',
     .'            START_TIME                 STOP_TIME',
     .'         COORDINATE SYSTEM')
 3000 FORMAT(3X,I10,3X,I10,3X,I10,9X,A2,3X,I10,3X,I10,
     .       2X,1PG20.13,2X,1PG20.13,2X,G20.13,2X,G20.13,16X,I10)
 4000 FORMAT(3X,I10,3X,I10,3X,I10,9X,A2,3X,I10,3X,I10,
     .       2X,1PG20.13,2X,1PG20.13,2X,G20.13,2X,G20.13,16X,I10)
c--------------------------------------------------
      END
