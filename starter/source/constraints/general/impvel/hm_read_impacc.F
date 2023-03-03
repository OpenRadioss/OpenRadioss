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
Chd|  HM_READ_IMPACC                source/constraints/general/impvel/hm_read_impacc.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        KINSET                        source/constraints/general/kinset.F
Chd|        UDOUBLE                       source/system/sysfus.F        
Chd|        NODGRNR5                      source/starter/freform.F      
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE HM_READ_IMPACC(
     .           FAC      ,IBFV     ,NFXVEL0  ,ITAB     ,ITABM1   ,
     .           IKINE    ,IGRNOD   ,ISKN     ,UNITAB   ,LSUBMODEL,
     .           NUM      ,NIMPACC  )
C============================================================================
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
#include      "param_c.inc"
#include      "scr17_c.inc"
#include      "sphcom.inc"
#include      "submod_c.inc"
#include      "units_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (UNIT_TYPE_),INTENT(IN) ::UNITAB 
      INTEGER ,INTENT(INOUT) :: NIMPACC,NUM
      INTEGER ,INTENT(IN)    :: NFXVEL0
      INTEGER ,DIMENSION(NIFV,NFXVEL0)   :: IBFV
      INTEGER ,DIMENSION(LISKN,*)        :: ISKN
      INTEGER ,DIMENSION(*)              :: ITAB,ITABM1,IKINE
      my_real            ,DIMENSION(LFXVELR,*) ,INTENT(INOUT) :: FAC

      TYPE (GROUP_)      ,DIMENSION(NGRNOD)    ,INTENT(IN)    :: IGRNOD
      TYPE(SUBMODEL_DATA),DIMENSION(*)         ,INTENT(IN)    :: LSUBMODEL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J,ID,UID,IACC,FCT_ID,INP_ID,SENS_ID,GRN,NACC,
     .        NOSKEW,NOFRAME,NUM0,NN,I_VDA,INOD,NODID,IGS,
     .        L_XYZ,SUBID,NOSUB,NODENUM(NFXVEL0)
      INTEGER ,DIMENSION(3*NUMNOD) :: IKINE1
      LOGICAL IS_AVAILABLE
      CHARACTER(ncharfield)  :: XYZ
      CHARACTER(nchartitle)  :: TITR,MESS
      INTEGER ,DIMENSION(:),ALLOCATABLE  :: IACCIDS
      CHARACTER(2)           :: X,Y,Z,XX,YY,ZZ
      my_real :: FAC1,FAC2,FAC3,FACX,FSCAL_T,FSCAL_A,BID
C-----------------------------------------------
C   D a t a
C-----------------------------------------------
      DATA X  /'X'/
      DATA Y  /'Y'/
      DATA Z  /'Z'/
      DATA XX /'XX'/
      DATA YY /'YY'/
      DATA ZZ /'ZZ'/
      DATA MESS/'IMPOSED ACCELERATION DEFINITION  '/
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NODGRNR5
      EXTERNAL NODGRNR5
C======================================================================|
!---
      WRITE (IOUT,1000)
!---
!---
      I_VDA = NUM ! (VDA ---> velocities + desplacements + accelerations)
!---
      IKINE1(1:3*NUMNOD) = 0
      NODENUM(1:NFXVEL0) = 0
      BID  = ZERO
      NACC = 0
c--------------------------------------------------
c     COUNT /IMPACC Options
c--------------------------------------------------
      CALL HM_OPTION_COUNT('/IMPACC',NIMPACC)
!
      ALLOCATE(IACCIDS(NIMPACC))
      IACCIDS(1:NIMPACC) = 0
!
      IS_AVAILABLE = .FALSE.
C--------------------------------------------------
C START BROWSING MODEL /IMPACC
C--------------------------------------------------
      CALL HM_OPTION_START('/IMPACC')
C--------------------------------------------------
C BROWSING MODEL IMPACC 1-> NIMPACC
C--------------------------------------------------
      DO IACC=1,NIMPACC
        TITR = ''
C--------------------------------------------------
C EXTRACT DATAS OF /IMPACC/... LINE
C--------------------------------------------------
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                          OPTION_ID   = ID,
     .                          UNIT_ID     = UID,
     .                          SUBMODEL_ID = SUBID,
     .                          SUBMODEL_INDEX = NOSUB,
     .                          OPTION_TITR = TITR)
!
        IACCIDS(IACC) = ID
!
C--------------------------------------------------
C EXTRACT DATA (STRING VALUES)
C--------------------------------------------------
        CALL HM_GET_INTV('curveid'        ,FCT_ID  ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_STRING('rad_dir'      ,XYZ     ,ncharfield,IS_AVAILABLE)
        CALL HM_GET_INTV('inputsystem'    ,INP_ID  ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('rad_sensor_id'  ,SENS_ID ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('entityid'       ,GRN     ,IS_AVAILABLE,LSUBMODEL)
!
        CALL HM_GET_FLOATV('xscale'       ,FACX,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('magnitude'    ,FAC1,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('rad_tstart'   ,FAC2,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('rad_tstop'    ,FAC3,IS_AVAILABLE,LSUBMODEL,UNITAB)
c--------------------------------------------------
c       Check skew and frame IDs
c--------------------------------------------------
        IF ((INP_ID == 0).AND.(SUBID /= 0)) THEN
          INP_ID = LSUBMODEL(NOSUB)%SKEW
        ENDIF
c----
        NOSKEW  = 0
        NOFRAME = 0
c----
        DO J=0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
          IF (INP_ID == ISKN(4,J+1)) THEN               
            NOSKEW = J+1                         
            EXIT                           
          ENDIF                                   
        ENDDO
        IF (INP_ID > 0 .and. NOSKEW == 0)
     .    CALL ANCMSG(MSGID=137,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .                I1= ID,
     .                I2= NOSKEW,
     .                C1='IMPOSED ACCELERATION',
     .                C2='IMPOSED ACCELERATION',
     .                C3= TITR)
c----
c--------------------------------------------------
c       Default scale factors
c--------------------------------------------------
        CALL HM_GET_FLOATV_DIM('xscale'   ,FSCAL_T ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('magnitude',FSCAL_A ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        IF (FACX == ZERO) FACX = ONE * FSCAL_T
        FACX = ONE / FACX
        IF (FAC1 == ZERO) FAC1 = ONE * FSCAL_A
        IF (FAC3 == ZERO) FAC3 = EP20
c--------------------------------------------------
        IF (XYZ(1:2) == XX .OR. XYZ(1:2) == YY .OR. XYZ(1:2) == ZZ) THEN
          FAC1 = FAC1 /  (FSCAL_A * FSCAL_T * FSCAL_T)
        ENDIF
!---
        NUM0 = NUM
        NN   = NODGRNR5(GRN,IGS,NODENUM,IGRNOD,ITABM1,MESS)
        NUM  = NUM  + NN
        NACC = NACC + NN
!
        DO J=1,NN
          I_VDA = I_VDA + 1
          IBFV(1,I_VDA)  = NODENUM(J)
          IBFV(2,I_VDA)  = 0
          IBFV(3,I_VDA)  = FCT_ID
          IBFV(4,I_VDA)  = SENS_ID
          IBFV(5,I_VDA)  = 0
          IBFV(6,I_VDA)  = 0  ! init dans lecrby (si vitesse de rotation sur main)
          IBFV(7,I_VDA)  = 0
          IBFV(8,I_VDA)  = 0
          IBFV(9,I_VDA)  = NOFRAME
          IBFV(10,I_VDA) = 0
          IBFV(11,I_VDA) = 0
          IBFV(12,I_VDA) = IACC
          IBFV(13,I_VDA) = 0
          IBFV(14,I_VDA) = 0
!
          FAC(1,I_VDA)= FAC1
          FAC(2,I_VDA)= FAC2
          FAC(3,I_VDA)= FAC3
          FAC(4,I_VDA)= ZERO
          FAC(5,I_VDA)= FACX
          FAC(6,I_VDA)= ZERO
!
          INOD  = IABS(NODENUM(J))
          NODID = ITAB(INOD)
!---
!  PRINT OUT
!---
          L_XYZ = 0
          IF (XYZ(1:2) == XX .OR. XYZ(1:2) == YY .OR. XYZ(1:2) == ZZ) THEN
            L_XYZ = 2
          ELSEIF (XYZ(1:1) == X .OR. XYZ(1:1) == Y .OR. XYZ(1:1) == Z) THEN
            L_XYZ = 1
          ENDIF
!
          WRITE (IOUT,'(3X,I10,3X,I10,3X,I10,9X,A2,3X,I10,3X,I10,2X,
     .       1PG20.13,2X,1PG20.13,2X,G20.13,2X,G20.13,16X,I10)')
     .       NODID,ISKN(4,NOSKEW),0,XYZ(1:L_XYZ),IBFV(3,I_VDA),SENS_ID,
     .       FAC(1,I_VDA),ONE/FACX,FAC(2,I_VDA),FAC(3,I_VDA),IBFV(10,I_VDA)
!
          IF (XYZ(1:2) == XX) THEN
            IBFV(2,I_VDA) = 4 + NOSKEW*10
            CALL KINSET(16,NODID,IKINE(INOD),4,NOSKEW,IKINE1(INOD))
          ELSEIF (XYZ(1:2) == YY) THEN
            IBFV(2,I_VDA) = 5 + NOSKEW*10
            CALL KINSET(16,NODID,IKINE(INOD),5,NOSKEW,IKINE1(INOD))
          ELSEIF (XYZ(1:2) == 'ZZ') THEN
            IBFV(2,I_VDA) = 6 + NOSKEW*10
            CALL KINSET(16,NODID,IKINE(INOD),6,NOSKEW,IKINE1(INOD))
          ELSEIF (XYZ(1:1) == X) THEN
            IBFV(2,I_VDA)= 1 + NOSKEW*10
            CALL KINSET(16,NODID,IKINE(INOD),1,NOSKEW,IKINE1(INOD))
          ELSEIF (XYZ(1:1) == Y) THEN
            IBFV(2,I_VDA) = 2 + NOSKEW*10
            CALL KINSET(16,NODID,IKINE(INOD),2,NOSKEW,IKINE1(INOD))
          ELSEIF (XYZ(1:1) == 'Z') THEN
            IBFV(2,I_VDA) = 3 + NOSKEW*10
            CALL KINSET(16,NODID,IKINE(INOD),3,NOSKEW,IKINE1(INOD))
          ELSE
             CALL ANCMSG(MSGID=164,
     .                   MSGTYPE=MSGERROR,
     .                   ANMODE=ANINFO,I1=ID,
     .                   C1=TITR,
     .                   C2=XYZ)
          ENDIF ! IF (XYZ(1:1) == X)
        ENDDO ! DO J=1,NN
!---------------------------
      ENDDO ! DO IACC=1,NIMPACC
c-----------
c     TEST DOUBLE IDs
c-----------
!
      CALL UDOUBLE(IACCIDS,1,NIMPACC,MESS,0,BID)
c
      NIMPACC = NACC
!
C-----
      DEALLOCATE(IACCIDS)
C-----
 1000 FORMAT(//
     .'     IMPOSED ACCELERATIONS   '/
     .'     ---------------------   '/
     .'         NODE         SKEW        FRAME  DIRECTION   LOAD_CURVE',
     .'       SENSOR            FSCALE                ASCALE',
     .'            START_TIME                 STOP_TIME')
C-----
      RETURN
      END
