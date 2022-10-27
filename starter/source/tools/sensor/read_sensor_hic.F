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
Chd|  READ_SENSOR_HIC               source/tools/sensor/read_sensor_hic.F
Chd|-- called by -----------
Chd|        HM_READ_SENSORS               source/tools/sensor/hm_read_sensors.F
Chd|-- calls ---------------
Chd|        ANCMSG                        source/output/message/message.F
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        ID                            source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE READ_SENSOR_HIC(SENSOR_PTR ,SENS_ID  ,TITR      ,
     .           LSENSOR ,SENSOR  ,LACCELM  ,UNITAB   ,LSUBMODEL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE SUBMODEL_MOD
      USE SENSOR_MOD
      USE HM_OPTION_READ_MOD
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
#include      "scr_usens_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: SENS_ID
      INTEGER ,DIMENSION(3,*) ,INTENT(IN) :: LACCELM
      INTEGER ,DIMENSION(NSENSI)  ,INTENT(OUT) :: LSENSOR
      my_real ,DIMENSION(NSENSR)  ,INTENT(OUT) :: SENSOR
      CHARACTER (len=nchartitle) TITR
      TYPE (SENSOR_STR_)  :: SENSOR_PTR
      TYPE (SUBMODEL_DATA) ,DIMENSION(NSUBMOD) :: LSUBMODEL
      TYPE (UNIT_TYPE_)    ,INTENT(IN) ::UNITAB 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: K,SENS_TYPE,INP_FLAG,ACC_ID,NOD_ID,IACC,IDIR,
     .   NPOINT,NPARIS,NPARRS,NVAR
      my_real :: TDEL,PERIOD,HIC_CRIT,DEF_GRAVITY,GRAVITY,TMIN,
     .   FAC_GRAV,Time_Unit
      CHARACTER(len=ncharkey) :: DIR
      LOGICAL :: IS_AVAILABLE
      DATA DEF_GRAVITY/9.80665/
C--------------------------------
C     SENSOR BASED ON HEAD INJURY INDEX CRITERION 
C=======================================================================
      IS_AVAILABLE = .FALSE.
c
      SENS_TYPE = 16
      INP_FLAG    = 1       ! input is accelerometer ID (instead of node ID)
      NPOINT      = 200     ! default value
c--------------------------------------------------
card1
      CALL HM_GET_FLOATV('Tdelay' ,TDEL   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
card2
      CALL HM_GET_INTV  ('IACC1'  ,ACC_ID ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_STRING('DIR '   ,DIR    ,ncharfield,IS_AVAILABLE)
card3
      CALL HM_GET_FLOATV('HIC_Period'  ,PERIOD   ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('HIC_Value'   ,HIC_CRIT ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('HIC_Gravity' ,GRAVITY  ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Tmin'        ,TMIN     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
c--------------------------------------------------
c     Check input data
c--------------------------------------------------
      IACC = 0                                                              
      IF (ACC_ID > 0) THEN                                                  
        DO K =1,NACCELM                                                     
          IF (ACC_ID == LACCELM(2,K))THEN                                   
            IACC = K                                                        
            EXIT                                                            
          ENDIF                                                             
        ENDDO                                                               
      END IF
      IF (IACC == 0) THEN                                                 
        CALL ANCMSG(MSGID=45, MSGTYPE=MSGERROR, ANMODE=ANINFO_BLIND_2,
     .              I1=SENS_ID,                                         
     .              C1=TITR,                                              
     .              I2=IACC)                                              
      END IF
c--------------------             
      IDIR = 0  
      DO K = 1,LFIELD                                            
        IF (DIR(K:K) == 'R' .OR .DIR(K:K) == 'r') THEN
          IDIR = 1
          DIR(1:1) = 'R'
        ELSE IF (DIR(K:K) == 'X' .OR .DIR(K:K) == 'x') THEN
          IDIR = 2
          DIR(1:1) = 'X'
        ELSE IF (DIR(K:K) == 'Y' .OR .DIR(K:K) == 'y') THEN
          IDIR = 3
          DIR(1:1) = 'Y'            
        ELSE IF (DIR(K:K) == 'Z' .OR .DIR(K:K) == 'z') THEN
          IDIR = 4
          DIR(1:1) = 'Z'            
        END IF
        IF (IDIR > 0) THEN
          DIR(1:1) =  DIR(K:K)
          EXIT         
        END IF
      ENDDO
      IF (IDIR == 0) THEN
        DIR(1:1) =  'R'
        IDIR = 1
      END IF 
c--------------------             
      IF (GRAVITY == ZERO) THEN
        FAC_GRAV  = FAC_T_WORK**2 / FAC_L_WORK
        GRAVITY   = DEF_GRAVITY * FAC_GRAV
      END IF
      IF (HIC_CRIT == ZERO) HIC_CRIT = INFINITY
      IF (PERIOD == ZERO) THEN
        PERIOD = 0.036     ! m/s
        PERIOD = PERIOD / FAC_T_WORK
      END IF
c
      Time_Unit = FAC_T_WORK
c-------------------------------                          
      SENSOR_PTR%TYPE    = SENS_TYPE
      SENSOR_PTR%SENS_ID = SENS_ID
      SENSOR_PTR%STATUS  = 0            ! status = deactivated
      SENSOR_PTR%TSTART  = INFINITY
      SENSOR_PTR%TCRIT   = INFINITY
      SENSOR_PTR%TMIN    = TMIN
      SENSOR_PTR%TDELAY  = TDEL         ! time delay before activation
      SENSOR_PTR%VALUE   = ZERO
c
      NPARIS = 4
      NPARRS = 4
      NVAR   = 4 + NPOINT
c
      SENSOR_PTR%NPARI  = NPARIS
      SENSOR_PTR%NPARR  = NPARRS
      SENSOR_PTR%NVAR   = NVAR
c
      ALLOCATE (SENSOR_PTR%IPARAM(NPARIS))
      ALLOCATE (SENSOR_PTR%RPARAM(NPARRS))
      ALLOCATE (SENSOR_PTR%VAR(NVAR))
      SENSOR_PTR%VAR(:) = ZERO
c
      SENSOR_PTR%IPARAM(1) = INP_FLAG
      SENSOR_PTR%IPARAM(2) = IACC
      SENSOR_PTR%IPARAM(3) = NPOINT
      SENSOR_PTR%IPARAM(4) = IDIR
c
      SENSOR_PTR%RPARAM(1) = PERIOD
      SENSOR_PTR%RPARAM(2) = HIC_CRIT
      SENSOR_PTR%RPARAM(3) = GRAVITY          
      SENSOR_PTR%RPARAM(4) = Time_Unit          
c---------------------------
c     old sensor data tables
c---------------------------
      LSENSOR(1) = SENS_ID
      LSENSOR(2) = SENS_TYPE
      LSENSOR(3:NSENSI) = 0
      SENSOR(:) = ZERO
c-----------------------------------------------------------------------
      WRITE (IOUT, 1000) SENS_ID,TDEL
      WRITE (IOUT, 2000) ACC_ID,DIR(1:1),TDEL,PERIOD,HIC_CRIT,TMIN,GRAVITY
c-----------------------------------------------------------------------
 1000 FORMAT(
     . 5X,'    SENSOR TYPE 16: HIC   '/,
     . 5X,'    -------------------   '/,
     . 5X,'SENSOR ID. . . . . . . . . . . . . . . . .=',I10/
     . 5X,'TIME DELAY BEFORE ACTIVATION . . . . . . .=',E12.4)
 2000 FORMAT(
     . 5X,'ACCELEROMETER ID . . . . . . . . . . . . =',I10/
     . 5X,'DIRECTION. . . . . . . . . . . . . . . . =',A10/
     . 5X,'TIME DELAY BEFORE ACTIVATION . . . . . . =',E12.4/
     . 5X,'HIC PERIOD . . . . . . . . . . . . . . . =',E12.4/,
     . 5X,'HIC CRITERION. . . . . . . . . . . . . . =',E12.4/,
     . 5X,'HIC DURATION TO ACTIVATE . . . . . . . . =',E12.4/,
     . 5X,'GRAVITY VALUE. . . . . . . . . . . . . . =',E12.4//)
c-----------
      RETURN
      END
