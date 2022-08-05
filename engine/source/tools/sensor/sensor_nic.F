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
C=======================================================================
Chd|====================================================================
Chd|  SENSOR_NIC                    source/tools/sensor/sensor_nic.F
Chd|-- called by -----------
Chd|        SENSOR1                       source/tools/sensor/sensor1.F 
Chd|-- calls ---------------
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|====================================================================
      SUBROUTINE SENSOR_NIC(SENSOR)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE GROUPDEF_MOD
      USE SENSOR_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "comlock.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "units_c.inc"
#include      "com08_c.inc"
#include      "scr05_c.inc"
#include      "scr17_c.inc"
#include      "task_c.inc"
#include      "parit_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      TYPE (SENSOR_STR_) ,TARGET :: SENSOR
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
      INTEGER I,J,IDIR,ISENSOR_TYPE,NPOINT,NPH,NVAR,CFC,
     .        SKEW_SENS,SKEW_SPRING,IDIR1,IDIR2
c
      my_real :: TIME_UNIT,TIME,DT
      my_real :: NIC_PERIOD,NIJMAX,NIC,NIC_NF,DTIME
      my_real :: TIME_PREC,TEST_TIME,DELTA_T
      my_real :: PERIOD_TMP,OPT_PERIOD,
     .           FTENS,FCOMP,MFLEX,MEXT,ALPHA,BETA
      my_real :: NIJ,FOR,MOM
      my_real ,DIMENSION(:) ,ALLOCATABLE :: FX,FY,FZ,MX,MY,MZ
      my_real ,DIMENSION(3) :: CA
      my_real ,DIMENSION(2) :: CB
      my_real ,DIMENSION(:) ,POINTER :: TABLE_F,TABLE_M
c
      DATA NVAR/4/
c-----------------------------------------------------------------------
c     Sensor state variables
c        SENSOR%VAR(1) :  TIME_PREC   = beginning of current time interval
c        SENSOR%VAR(2) :  Integral of acceleration over time within current time interval
c        SENSOR%VAR(3) :  Index of current time interval in data table
c        SENSOR%VAR(4) :  Previous NIC value
c        SENSOR%VAR(5...5+NPOINT)  : average acceleration data table by time intervals
C=======================================================================
      IF (SENSOR%STATUS == 3) RETURN   ! already activated
c
      ISENSOR_TYPE = SENSOR%IPARAM(1)
      SKEW_SENS    = SENSOR%IPARAM(3)
      SKEW_SPRING  = SENSOR%IPARAM(4)
      IDIR1        = SENSOR%IPARAM(5)
      IDIR2        = SENSOR%IPARAM(6)
      NPH          = SENSOR%IPARAM(7)
      CFC          = SENSOR%IPARAM(8)
c
      NIJMAX = SENSOR%RPARAM(1)
      FTENS  = SENSOR%RPARAM(2)
      FCOMP  = SENSOR%RPARAM(3)
      MFLEX  = SENSOR%RPARAM(4)
      MEXT   = SENSOR%RPARAM(5)
      CA(1)  = SENSOR%RPARAM(6)
      CA(2)  = SENSOR%RPARAM(7)
      CA(3)  = SENSOR%RPARAM(8)
      CB(1)  = SENSOR%RPARAM(9)
      CB(2)  = SENSOR%RPARAM(10)
      DTIME  = SENSOR%RPARAM(11)
      ALPHA  = SENSOR%RPARAM(12)
      
c     actual normalized spring force and moment
      FOR = SENSOR%VAR(1)

!      MOM = SENSOR%VAR(2)
      TIME_PREC = SENSOR%VAR(3)  ! bidon
      
      
!     1) fill Force and moment tables from previous time steps  
!     2) call filtering routine for all components
!     3) Calculate Nij criterion for each combination of normalized 
!        tension-compression-flex-ext combinations : N11,N12,N21,N22
!     4) test crit value over Tmin and deactivate sensor
!
 
!      NPOINT = 2*NPH + 1
      NPOINT = 7
      DT = DT2
c
      TIME = TT
      NVAR = 5
c      TABLE_F(1:NPOINT) => SENSOR%VAR(NVAR+1:NVAR+NPOINT)
c      TABLE_M(1:NPOINT) => SENSOR%VAR(NVAR+NPOINT+1:NVAR+NPOINT*2)
c     shift data to make place for new time interval when NIC_PERIOD is reached
      DO I = 1,NPOINT-1
        TABLE_F(I) = TABLE_F(I+1)   
      END DO                                
      TABLE_F(1:NPOINT) = FOR
      DO I = 1,NPOINT-1
        TABLE_M(I) = TABLE_M(I+1)   
      END DO                                
      TABLE_M(1:NPOINT) = MOM
      

c      CALL FILTER_SAE211(NPOINT    ,SENSOR%VAR   ,CA    ,CB   )

c      CALL FILTER_SAE211(TABLE_M   ,DT   ,NPOINT    ,CA    ,CB   )

      NIC = FOR + MOM
c      SENSOR%VAR(5) = NIC
c      SENSOR%VAR(3) = TABLE_F(NPH+1)
c      SENSOR%VAR(4) = TABLE_M(NPH+1)
      
c      NIC = SENSOR%VAR(3) + SENSOR%VAR(4)
      SENSOR%VALUE = NIC
      
c-----------------------------------------
c     Check Sensor State
c-----------------------------------------
      IF (NIC >= NIJMAX) THEN
        IF (SENSOR%STATUS == 0) THEN
          SENSOR%STATUS = 1
          SENSOR%TCRIT  = TIME + SENSOR%TMIN
        END IF
        IF (SENSOR%TCRIT <= TIME) THEN
          SENSOR%STATUS = 2
        END IF
        IF (SENSOR%STATUS == 2 .and. 
     .      SENSOR%TCRIT + SENSOR%TDELAY <= TIME) THEN
          SENSOR%STATUS = 3   ! activate
          SENSOR%TSTART = TIME
        END IF
      ENDIF ! Fin test NIC
c-----------------------------------------------------------------------      
      IF (SENSOR%STATUS == 3 .and. ISPMD == 0) THEN
#include "lockon.inc"
        WRITE (ISTDO,1100 ) SENSOR%SENS_ID,SENSOR%TSTART
        WRITE (IOUT ,1100 ) SENSOR%SENS_ID,SENSOR%TSTART
#include "lockoff.inc"
      ENDIF
c-----------------------------------------------------------------------      
1100  FORMAT(' SENSOR NUMBER ',I10,' ,ACTIVATED AT TIME ',1PE12.5,
     .       '      NIC VALUE  = ',1PE12.5)           
c-----------------------------------------------------------------------      
      RETURN
      END SUBROUTINE
