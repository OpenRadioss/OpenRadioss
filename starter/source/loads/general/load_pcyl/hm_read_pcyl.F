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
Chd|  HM_READ_PCYL                  source/loads/general/load_pcyl/hm_read_pcyl.F
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_FLOATV_DIM             source/devtools/hm_reader/hm_get_floatv_dim.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_OPTION_COUNT               source/devtools/hm_reader/hm_option_count.F
Chd|        HM_OPTION_READ_KEY            source/devtools/hm_reader/hm_option_read_key.F
Chd|        HM_OPTION_START               source/devtools/hm_reader/hm_option_start.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        LOADS_MOD                     ../common_source/modules/loads/loads_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|        TABLE_MOD                     share/modules1/table_mod.F    
Chd|====================================================================
      SUBROUTINE HM_READ_PCYL(LOADS    ,IGRSURF  ,NSENSOR  ,SENSOR_TAB,TABLE  ,
     .                        IFRAME   ,UNITAB   ,LSUBMODEL, NUMBER_LOAD_CYL)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
      USE SUBMODEL_MOD
      USE HM_OPTION_READ_MOD
      USE LOADS_MOD
      USE TABLE_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "scr17_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "tabsiz_c.inc"
#include      "sysunit.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER   ,INTENT(IN) :: NSENSOR
      INTEGER   ,DIMENSION(LISKN,NUMFRAM+1)   ,INTENT(IN) :: IFRAME
      TYPE (SURF_)         ,DIMENSION(NSURF)  ,INTENT(IN) :: IGRSURF
      TYPE (TTABLE)        ,DIMENSION(NTABLE) ,INTENT(IN) :: TABLE
      TYPE (SENSOR_STR_)   ,DIMENSION(NSENSOR),INTENT(IN) :: SENSOR_TAB
      TYPE (SUBMODEL_DATA) ,DIMENSION(*)      ,INTENT(IN) :: LSUBMODEL
      TYPE (UNIT_TYPE_)    ,INTENT(IN)  :: UNITAB 
      TYPE (LOADS_)        ,INTENT(INOUT) :: LOADS
      INTEGER, INTENT(INOUT) :: NUMBER_LOAD_CYL ! total number of contribution (1 per node per segment) of /LOAD/CYL
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
        INTEGER I,J,LOAD_ID,TABLE_ID,SURF_ID,SENS_ID,FRAME_ID,UID,ISENS,ISS,
     .          NOFRA,SUB_INDX,NSEG,ITABLE,STAT,NLOAD_CYL,IMOV
        my_real :: X_R,X_T,YFAC,FAC_R,FAC_T,FAC_P
        CHARACTER MESS*40,TITR*nchartitle
        LOGICAL IS_AVAILABLE
        DATA MESS/'CYLINDRICAL PRESSURE LOADS DEFINITION   '/
C-----------------------------------------------
C   E x t e r n a l   F u n c t i o n s
C-----------------------------------------------
      INTEGER  NGR2USR
      EXTERNAL NGR2USR
C----------------------------------------------------------------------------------        
C   C o m m e n t s
C----------------------------------------------------------------------------------        
C  /LOAD/PCYL : imposed pressure in function of radial coordinate around an axis and time       
C---------------------------------------------------------------------------------- 
C     LOAD_CYL
c         -> PCYL_ID
c         -> NSEG
c         -> SEGNOD(NSEG,4)  (N1,N2,NB3,N4) by segment
c         -> AXIS(2)         (M1,M2)
c         -> SENS_ID
c         -> TABLE_ID
c         -> XSCALE_R
c         -> XSCALE_T
c         -> YSCALE_P
c         -> SURFBOX(xmin,ymin,zmin,xmax,ymax,zmax)
C=======================================================================
      IS_AVAILABLE = .FALSE.
      NUMBER_LOAD_CYL = 0
C--------------------------------------------------
C     START BROWSING MODEL /PCYL
C--------------------------------------------------
      CALL HM_OPTION_COUNT('/LOAD/PCYL',NLOAD_CYL)
      LOADS%NLOAD_CYL = NLOAD_CYL
      ALLOCATE(LOADS%LOAD_CYL(NLOAD_CYL))

      CALL HM_OPTION_START('/LOAD/PCYL')
C--------------------------------------------------
      DO I=1,NLOAD_CYL     

        TITR = ''                                           
        CALL HM_OPTION_READ_KEY(LSUBMODEL,
     .                       OPTION_ID = LOAD_ID,
     .                       UNIT_ID = UID,
     .                       SUBMODEL_INDEX = SUB_INDX,
     .                       OPTION_TITR = TITR)
c---------------------------------------------------------------------------
card1
        CALL HM_GET_INTV('surf_ID'   ,SURF_ID  ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('sens_ID'   ,SENS_ID  ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_INTV('frame_ID'  ,FRAME_ID ,IS_AVAILABLE,LSUBMODEL)
c
card2
        CALL HM_GET_INTV('table_ID'  ,TABLE_ID ,IS_AVAILABLE,LSUBMODEL)
        CALL HM_GET_FLOATV('xscale_r',X_R      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('xscale_t',X_T      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV('yscale_p',YFAC     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c
c       read units
        CALL HM_GET_FLOATV_DIM('xscale_r' ,FAC_R ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('xscale_t' ,FAC_T ,IS_AVAILABLE,LSUBMODEL,UNITAB)
        CALL HM_GET_FLOATV_DIM('yscale_p' ,FAC_P ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c---------------------------------------------------------------------------
        IF (X_R  == ZERO) X_R  = FAC_R
        IF (X_T  == ZERO) X_T  = FAC_T
        IF (YFAC == ZERO) YFAC = FAC_P
        LOADS%LOAD_CYL(I)%XSCALE_R  = X_R
        LOADS%LOAD_CYL(I)%XSCALE_T  = X_T
        LOADS%LOAD_CYL(I)%YSCALE    = YFAC
c
c       read surface segments
c
c        internal_SURF_ID =  NGR2USR(SURF_ID,INGR2USR,NSURF)

        NSEG = 0
        IF (SURF_ID > 0) THEN
          DO J=1,NSURF
            IF (SURF_ID == IGRSURF(J)%ID) THEN
              ISS = J
               NSEG = IGRSURF(ISS)%NSEG
              EXIT
            ENDIF                             
          ENDDO 
          LOADS%LOAD_CYL(I)%ID   = LOAD_ID
          LOADS%LOAD_CYL(I)%NSEG = NSEG
          CALL MY_ALLOC(LOADS%LOAD_CYL(I)%SEGNOD,NSEG,4)
          DO J=1,NSEG
            LOADS%LOAD_CYL(I)%SEGNOD(J,1) = IGRSURF(ISS)%NODES(J,1)
            LOADS%LOAD_CYL(I)%SEGNOD(J,2) = IGRSURF(ISS)%NODES(J,2)
            LOADS%LOAD_CYL(I)%SEGNOD(J,3) = IGRSURF(ISS)%NODES(J,3)
            LOADS%LOAD_CYL(I)%SEGNOD(J,4) = IGRSURF(ISS)%NODES(J,4)
            IF (IGRSURF(ISS)%ELTYP(J)==7) LOADS%LOAD_CYL(I)%SEGNOD(J,4) = 0
          ENDDO 
          NUMBER_LOAD_CYL = NUMBER_LOAD_CYL + 4*NSEG
        ENDIF
c
c---------------------------------------------------------------------------
        ITABLE = 0
        IF (TABLE_ID > 0) THEN
          DO J=1,NTABLE
            IF (TABLE_ID == TABLE(J)%NOTABLE) THEN
              ITABLE = J
              EXIT
            ENDIF
           ENDDO
        ENDIF
           IF (ITABLE == 0) THEN
           CALL ANCMSG(MSGID=488,ANMODE=ANINFO,MSGTYPE=MSGERROR,
     .              C1='LOAD PCYL',
     .              C2='LOAD PCYL',
     .              I2=TABLE_ID,I1=LOAD_ID,C3=TITR)
        END IF
c
c---------------------------------------------------------------------------
c       check input sensor
c
        ISENS = 0
        IF (SENS_ID > 0) THEN
          DO J=1,NSENSOR
            IF (SENS_ID == SENSOR_TAB(J)%SENS_ID) THEN
              ISENS = J
              EXIT
            ENDIF
          ENDDO
        ENDIF
c
c       check local frame
c
        NOFRA = 0
        IMOV  = 0
        IF (FRAME_ID > 0) THEN
          DO J=0,NUMFRAM
            IF (FRAME_ID == IFRAME(4,J+1)) THEN
              NOFRA = J
              IMOV  = IFRAME(5,J+1)
              EXIT
            ENDIF
          ENDDO
        ENDIF
        IF (NOFRA == 0) THEN
            CALL ANCMSG(MSGID=490, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     .           C1='/LOAD/PCYL',
     .           I1=LOAD_ID,
     .           C2='/LOAD/PCYL',
     .           C3=TITR,
     .           I2=FRAME_ID)
        ELSE IF (IMOV == 0) THEN
            CALL ANCMSG(MSGID=3011, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_1,
     .           C1='/LOAD/PCYL',
     .           I1=LOAD_ID,
     .           C2='/LOAD/PCYL',
     .           C3=TITR)
        ENDIF
c
        LOADS%LOAD_CYL(I)%ID = LOAD_ID
        LOADS%LOAD_CYL(I)%IFRAME = NOFRA
        LOADS%LOAD_CYL(I)%ITABLE = ITABLE
        LOADS%LOAD_CYL(I)%ISENS  = ISENS
c---------------------------------------------------------------------------
c       OUTPUT
c---------------------------------------------------------------------------
        WRITE (IOUT,1000) LOAD_ID,FRAME_ID,SENS_ID,TABLE_ID,SURF_ID,NSEG,
     .                    X_R,X_T,YFAC
      ENDDO
c-----------
 1000 FORMAT(
     & 5X,'                         '/,
     & 5X,'CYLINDRICAL PRESSURE LOAD'/,
     & 5X,'-------------------------'/,
     & 5X,'LOAD ID. . . . . . . . . . . . . . . . .=',I10/,
     & 5X,'FRAME ID . . . . . . . . . . . . . . . .=',I10/,
     & 5X,'SENSOR ID. . . . . . . . . . . . . . . .=',I10/,
     & 5X,'TABLE ID . . . . . . . . . . . . . . . .=',I10/,
     & 5X,'SURFACE ID . . . . . . . . . . . . . . .=',I10/,
     & 5X,'NUMBER OF SEGMENTS . . . . . . . . . . .=',I10/,
     & 5X,'RADIUS SCALE FACTOR FOR ABSCISSA . . . .=',1PG20.13/,
     & 5X,'TIME   SCALE FACTOR FOR ABSCISSA . . . .=',1PG20.13/,
     & 5X,'PRESSURE SCALE FACTOR. . . . . . . . . .=',1PG20.13/)
c-----------
      RETURN
      END
