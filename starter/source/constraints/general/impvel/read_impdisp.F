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
Chd|  READ_IMPDISP                  source/constraints/general/impvel/read_impdisp.F
Chd|-- called by -----------
Chd|        HM_READ_IMPVEL                source/constraints/general/impvel/hm_read_impvel.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        FRETITL                       source/starter/freform.F      
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        KINSET                        source/constraints/general/kinset.F
Chd|        NODGRNR5                      source/starter/freform.F      
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE READ_IMPDISP(
     .           NDISP    ,INUM     ,IOPT     ,FBFVEL   ,IBFVEL   ,
     .           ITAB     ,ITABM1   ,IKINE    ,IGRNOD   ,NOM_OPT  ,
     .           ISKN     ,UNITAB   ,LSUBMODEL)
C============================================================================
C   M o d u l e s
C-----------------------------------------------
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE UNITAB_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr03_c.inc"
#include      "scr17_c.inc"
#include      "param_c.inc"
#include      "sphcom.inc"
#include      "submod_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN)    :: NDISP
      INTEGER ,INTENT(INOUT) :: INUM,IOPT
      INTEGER ,DIMENSION(*)  :: ITAB,ITABM1,IKINE
      INTEGER ,DIMENSION(LISKN,*)        ,INTENT(IN)  :: ISKN
      INTEGER ,DIMENSION(LNOPT1,*)       ,INTENT(OUT) :: NOM_OPT
      INTEGER ,DIMENSION(NIFV,NFXVEL)    ,INTENT(OUT) :: IBFVEL
      my_real ,DIMENSION(LFXVELR,NFXVEL) ,INTENT(OUT) :: FBFVEL
      TYPE (UNIT_TYPE_)  ,INTENT(IN) ::  UNITAB
      TYPE (GROUP_)      ,DIMENSION(NGRNOD)  ,INTENT(IN) :: IGRNOD
      TYPE(SUBMODEL_DATA),DIMENSION(*)       ,INTENT(IN) :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,IDISP,NN,NFVEL,IDIS,INOD,NODID,NOD,NOSKEW,NOFRAME,LEN,
     .   IUNIT,FLAGUNIT,SENS_ID,OPTID,UID,SKEW_ID,FCT_ID,GRN_ID,IGS,
     .   IFGEO,ICOOR,ILAGM,SUBID,NOSUB
      INTEGER ,DIMENSION(NFXVEL)   :: NODENUM
      INTEGER ,DIMENSION(3*NUMNOD) :: IKINE1
      my_real :: YSCALE,TSTART,TSTOP,XSCALE,FSCAL_T,FSCAL_L
      CHARACTER(2)           :: X,Y,Z,XX,YY,ZZ
      CHARACTER(ncharfield)  :: XYZ
      CHARACTER(ncharkey)    :: KEY
      CHARACTER(nchartitle)  :: TITR,MESS
      LOGICAL IS_AVAILABLE
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NODGRNR5
      EXTERNAL NODGRNR5
C-----------------------------------------------
C   D a t a
C-----------------------------------------------
      DATA X  /'X'/
      DATA Y  /'Y'/
      DATA Z  /'Z'/
      DATA XX /'XX'/
      DATA YY /'YY'/
      DATA ZZ /'ZZ'/
      DATA MESS/'IMPOSED DISPLACEMENT DEFINITION  '/
C======================================================================|
      IS_AVAILABLE = .FALSE.
c
      IKINE1(:)= 0
c--------------------------------------------------
c     READ /IMPDISP cards
c--------------------------------------------------
c
      CALL HM_OPTION_START('/IMPDISP')
c
      WRITE (IOUT,1000)
c
c--------------------------------------------------
      DO IDISP = 1,NDISP
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
c       
        IOPT = IOPT + 1
        NOM_OPT(1,IOPT) = OPTID
        CALL FRETITL(TITR,NOM_OPT(LNOPT1-LTITR+1,IOPT),LTITR)
c        
        ICOOR    = 0  ! ICOOR = 1 => axial coordinates
        IFGEO    = 0
        ILAGM    = 0
        IDIS     = 2
        LEN      = 1
        NOFRAME  = 0
c--------------------------------------------------
c       READ STRING VALUES from /IMPDISP
c--------------------------------------------------
        CALL HM_GET_INTV  ('curveid'       ,FCT_ID  ,IS_AVAILABLE,LSUBMODEL)  
        CALL HM_GET_STRING('rad_dir'       ,XYZ     ,ncharfield,IS_AVAILABLE) 
        CALL HM_GET_INTV  ('inputsystem'   ,SKEW_ID ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV  ('rad_sensor_id' ,SENS_ID ,IS_AVAILABLE,LSUBMODEL)  
        CALL HM_GET_INTV  ('entityid'      ,GRN_ID  ,IS_AVAILABLE,LSUBMODEL)  
        CALL HM_GET_INTV  ('rad_icoor'     ,ICOOR   ,IS_AVAILABLE,LSUBMODEL)  
c
        CALL HM_GET_FLOATV('xscale'        ,XSCALE  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('magnitude'     ,YSCALE  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('rad_tstart'    ,TSTART  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('rad_tstop'     ,TSTOP   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c
        IF (xyz(1:2) == XX .OR. xyz(1:2) == YY .OR. xyz(1:2) == ZZ) LEN = 2
c--------------------------------------------------
c       CHECK IF Unit_ID exists
c--------------------------------------------------
        FLAGUNIT = 0
        DO IUNIT=1,NUNITS
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
c       Check skew IDs
c-------------------------------------------------- 
        IF ((SKEW_ID == 0).AND.(SUBID /= 0)) THEN
          SKEW_ID = LSUBMODEL(NOSUB)%SKEW
        ENDIF
c----
        NOSKEW = 0
        DO I=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
          IF (SKEW_ID == ISKN(4,I+1)) THEN               
            NOSKEW = I+1                         
            EXIT                           
          ENDIF                                   
        ENDDO
        IF (SKEW_ID > 0 .and. NOSKEW == 0)
     .    CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1= OPTID,
     .                I2= NOSKEW,
     .                C1='IMPOSED DISPLACEMENT',
     .                C2='IMPOSED DISPLACEMENT',
     .                C3= TITR)
c--------------------------------------------------
c       Default scale factors
c--------------------------------------------------
        CALL HM_GET_FLOATV_DIM('xscale'   ,FSCAL_T ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('magnitude',FSCAL_L ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c
        IF (XSCALE == ZERO) XSCALE  = ONE * FSCAL_T
        XSCALE = ONE / XSCALE
        IF (YSCALE == ZERO)  YSCALE = ONE * FSCAL_L
c        IF (LEN == 2) THEN  ! FSCALE = rad - to be checked in cfg file ! 
c          YSCALE = YSCALE / FSCAL_L
c        ENDIF 
        IF (TSTOP == ZERO)  TSTOP  = INFINITY
c--------------------------------------------------
c       Read NODE numbers from the group
c
        NN = NODGRNR5(GRN_ID,IGS,NODENUM,IGRNOD,ITABM1,MESS)
c--------------------------------------------------
        DO I=1,NN
          INUM  = INUM + 1
          IBFVEL(1, INUM) = NODENUM(I)
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
          IBFVEL(13,INUM) = IFGEO
          IBFVEL(14,INUM) = 0
c
          FBFVEL(1,INUM)  = YSCALE
          FBFVEL(2,INUM)  = TSTART
          FBFVEL(3,INUM)  = TSTOP
          FBFVEL(4,INUM)  = ZERO
          FBFVEL(5,INUM)  = XSCALE
          FBFVEL(6,INUM)  = ZERO
c
          INOD  = IABS(NODENUM(I))
          NODID = ITAB(INOD)
c 
c---------------
c         TAG NODES WITH KINEMATIC CONDITIONS
c---------------
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
     .                  I1=OPTID,
     .                  C1=TITR,
     .                  C2=XYZ)
          ENDIF
c---------------
c         OUTPUT
c---------------
          WRITE (IOUT,2000) NODID,ISKN(4,NOSKEW),0,XYZ(1:LEN),FCT_ID,SENS_ID,
     .       YSCALE,ONE/XSCALE,TSTART,TSTOP
c-----------------------------------------------------------
c
        ENDDO ! NN
c
      ENDDO ! DO I=1,NFDISP
c
c--------------------------------------------------
 1000 FORMAT(//
     .'     IMPOSED DISPLACEMENTS    '/
     .'     -------------------    '/
     .'         NODE         SKEW        FRAME  DIRECTION   LOAD_CURVE',
     .'       SENSOR            FSCALE                ASCALE',
     .'            START_TIME                 STOP_TIME')
 2000 FORMAT(3X,I10,3X,I10,3X,I10,9X,A2,3X,I10,3X,I10,
     .       2X,1PG20.13,2X,1PG20.13,2X,G20.13,2X,G20.13)
c--------------------------------------------------
      RETURN
      END
