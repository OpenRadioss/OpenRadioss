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
Chd|  READ_SENSOR_NIC               source/tools/sensor/read_sensor_nic.F
Chd|-- called by -----------
Chd|        HM_READ_SENSORS               source/tools/sensor/hm_read_sensors.F
Chd|-- calls ---------------
Chd|        HM_GET_FLOATV                 source/devtools/hm_reader/hm_get_floatv.F
Chd|        HM_GET_INTV                   source/devtools/hm_reader/hm_get_intv.F
Chd|        HM_GET_STRING                 source/devtools/hm_reader/hm_get_string.F
Chd|        GROUPDEF_MOD                  ../common_source/modules/groupdef_mod.F
Chd|        HM_OPTION_READ_MOD            share/modules1/hm_option_read_mod.F
Chd|        MESSAGE_MOD                   share/message_module/message_mod.F
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|        SUBMODEL_MOD                  share/modules1/submodel_mod.F 
Chd|====================================================================
      SUBROUTINE READ_SENSOR_NIC(
     .           SENSOR_PTR,SENS_ID  ,TITR     ,
     .           LSENSOR   ,SENSOR   ,ISKN     ,UNITAB   ,LSUBMODEL )
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------  
      USE UNITAB_MOD
      USE MESSAGE_MOD
      USE GROUPDEF_MOD
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
#include      "tabsiz_c.inc"
#include      "com04_c.inc"
#include      "sphcom.inc"
#include      "scr_usens_c.inc"
#include      "warn_c.inc"
#include      "units_c.inc"
#include      "sysunit.inc"
#include      "submod_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ,INTENT(IN) :: SENS_ID
      INTEGER ,DIMENSION(NSENSI)       ,INTENT(OUT) :: LSENSOR
      my_real ,DIMENSION(NSENSR)       ,INTENT(OUT) :: SENSOR
      INTEGER ,DIMENSION(LISKN,SISKWN) ,INTENT(IN)  :: ISKN
      CHARACTER TITR*nchartitle
      TYPE (SENSOR_STR_) :: SENSOR_PTR
      TYPE (SUBMODEL_DATA) ,DIMENSION(NSUBMOD) :: LSUBMODEL
      TYPE (SUBSET_)       ,DIMENSION(NSUBS)   :: SUBSET
      TYPE (UNIT_TYPE_)    ,INTENT(IN) :: UNITAB 
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: I,SENS_TYPE,SPRING_ID,SKEW_ID,ISK,NPOINT,ICFC,IDIR1,IDIR2,
     .   NPARIS,NPARRS,NVAR
      my_real :: TDEL,TMIN,NIJMAX,FTENS,FCOMP,MFLEX,MEXT,ALPHA,CFC,
     .           FAC_FORC,FAC_MOM
      CHARACTER F_DIR*ncharfield, M_DIR*ncharfield
      LOGICAL :: IS_AVAILABLE
c--------------------------------
c     SENSOR BASED ON NECK INJURY CRITERION FOR SAFETY
c=======================================================================
      IS_AVAILABLE = .FALSE.
      SENS_TYPE    = 19
      IDIR1 = 0
      IDIR2 = 0
c--------------------------------------------------
card1
      CALL HM_GET_FLOATV('Tdelay'    ,TDEL     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
card2
      CALL HM_GET_FLOATV('NIJ_max'   ,NIJMAX    ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Fint_tens' ,FTENS     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Fint_comp' ,FCOMP     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Mint_flex' ,MFLEX     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Mint_ext'  ,MEXT      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
card3
      CALL HM_GET_INTV  ('Spring_Id' ,SPRING_ID ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_INTV  ('Skew_Id'   ,SKEW_ID   ,IS_AVAILABLE,LSUBMODEL)
      CALL HM_GET_STRING('Ax_Dir'    ,F_DIR       ,ncharfield,IS_AVAILABLE)
      CALL HM_GET_STRING('Bend_Dir'  ,M_DIR      ,ncharfield,IS_AVAILABLE)
card4
      CALL HM_GET_FLOATV('Tmin'      ,TMIN      ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_FLOATV('Alpha'     ,ALPHA     ,IS_AVAILABLE,LSUBMODEL,UNITAB)
      CALL HM_GET_INTV  ('CFC'       ,ICFC      ,IS_AVAILABLE,LSUBMODEL)
c
c--------------------------------------------------
c     Check input and defaul values
c--------------------------------------------------
      IF (F_DIR(1:1) == 'X' .or. F_DIR(1:1) == 'x') IDIR1 = 1
      IF (F_DIR(1:1) == 'Y' .or. F_DIR(1:1) == 'y') IDIR1 = 2
      IF (F_DIR(1:1) == 'Z' .or. F_DIR(1:1) == 'z') IDIR1 = 3
      IF (M_DIR(1:1) == 'X' .or. M_DIR(1:1) == 'x') IDIR2 = 1
      IF (M_DIR(1:1) == 'Y' .or. M_DIR(1:1) == 'y') IDIR2 = 2
      IF (M_DIR(1:1) == 'Z' .or. M_DIR(1:1) == 'z') IDIR2 = 3
      IF (IDIR1 == 0) THEN
        IDIR1 = 3
        F_DIR = 'Z'
      END IF
      IF (IDIR2 == 0) THEN
        IDIR1 = 2
        M_DIR  = 'Y'
      END IF
c
      ISK = 0
      IF (SKEW_ID > 0) THEN      
        DO I = 0,NUMSKW+MIN(1,NSPCOND)*NUMSPH+NSUBMOD
          IF (SKEW_ID == ISKN(4,I+1)) THEN
            ISK = I+1
            EXIT
          ENDIF
        ENDDO
      END IF
      FAC_FORC = FAC_M_WORK * FAC_L_WORK / FAC_T_WORK**2
      FAC_MOM  = FAC_FORC * FAC_L_WORK
      CFC = ICFC
      IF (FTENS == 0) FTENS = 6806.0 / FAC_FORC 
      IF (FCOMP == 0) FCOMP = 6160.0 / FAC_FORC 
      IF (MFLEX == 0) MFLEX = 310.0  / FAC_MOM
      IF (MEXT  == 0) MEXT  = 135.0  / FAC_MOM
      IF (ALPHA == 0) ALPHA = 2.0775
      IF (CFC == ZERO) CFC  = 600 * FAC_T_WORK
c--------------------------------------------------
c     sensor data structure
c--------------------------------------------------
      NPOINT = 5      
      NPARIS = 6             ! int  value parameters
      NPARRS = 7             ! real value parameters
      NVAR   = NPOINT*4 + 5  ! internal variables
c
      ALLOCATE (SENSOR_PTR%IPARAM(NPARIS))
      ALLOCATE (SENSOR_PTR%RPARAM(NPARRS))
      ALLOCATE (SENSOR_PTR%VAR(NVAR))
      SENSOR_PTR%VAR(:) = ZERO

      SENSOR_PTR%NVAR    = NVAR
      SENSOR_PTR%NPARI   = NPARIS
      SENSOR_PTR%NPARR   = NPARRS
      SENSOR_PTR%TYPE    = SENS_TYPE
      SENSOR_PTR%SENS_ID = SENS_ID
      SENSOR_PTR%STATUS  = 0          ! status = deactivated
      SENSOR_PTR%TSTART  = INFINITY   ! start time
      SENSOR_PTR%TCRIT   = INFINITY   ! global activation time
      SENSOR_PTR%TDELAY  = TDEL       ! time delay before activation
      SENSOR_PTR%TMIN    = TMIN       ! min time duration to validate criterion
      SENSOR_PTR%VALUE   = ZERO
c
      SENSOR_PTR%IPARAM(1) = SPRING_ID
      SENSOR_PTR%IPARAM(2) = 0        ! spring number - initialized in inisens()
      SENSOR_PTR%IPARAM(3) = ISK
      SENSOR_PTR%IPARAM(4) = 0        ! spring skew number - initialized in inisens()
      SENSOR_PTR%IPARAM(5) = IDIR1
      SENSOR_PTR%IPARAM(6) = IDIR2
c
      SENSOR_PTR%RPARAM(1) = NIJMAX
      SENSOR_PTR%RPARAM(2) = FTENS
      SENSOR_PTR%RPARAM(3) = FCOMP
      SENSOR_PTR%RPARAM(4) = MFLEX
      SENSOR_PTR%RPARAM(5) = MEXT
      SENSOR_PTR%RPARAM(6) = ALPHA
      SENSOR_PTR%RPARAM(7) = CFC   
c---------------------------
c     old sensor data tables                                           
c---------------------------                                           
      LSENSOR(1) = SENS_ID                                             
      LSENSOR(2) = SENS_TYPE
c
      SENSOR(1)  = TDEL        ! time delay
      SENSOR(2)  = INFINITY    ! start time
      SENSOR(3)  = INFINITY    ! stop time
      SENSOR(4)  = INFINITY    ! global activation time
c------------------------------------------------------------
      WRITE (IOUT ,1000) SENS_ID,TDEL,TMIN
      WRITE (IOUT ,2000) NIJMAX,FTENS,FCOMP,MFLEX,MEXT,SPRING_ID,SKEW_ID,
     .                   F_DIR,M_DIR,CFC,ALPHA
c------------------------------------------------------------
 1000 FORMAT(
     . 5X,'    SENSOR : NECK INJURY CRITERION          '/,
     . 5X,'    ------------------------------          '/,
     . 5X,'SENSOR ID. . . . . . . . . . . . . . . . . =',I10/
     . 5X,'TIME DELAY BEFORE ACTIVATION . . . . . . . =',E12.4/
     . 5X,'MINIMUM TIME DURATION. . . . . . . . . . . =',E12.4)
 2000 FORMAT(
     . 5X,'  NIJ LIMIT VALUE. . . . . . . . . . . . . =',E12.4/
     . 5X,'  CRITICAL AXIAL FORCE IN TENSION. . . . . =',E12.4/
     . 5X,'  CRITICAL AXIAL FORCE IN COMPRESSION. . . =',E12.4/
     . 5X,'  CRITICAL BENDING MOMENT IN FLEXION . . . =',E12.4/
     . 5X,'  CRITICAL BENDING MOMENT IN EXTENSION . . =',E12.4/
     . 5X,'  SPRING ELEMENT ID. . . . . . . . . . . . =',I10/
     . 5X,'  LOCAL SKEW SYSTEM ID . . . . . . . . . . =',I10/
     . 5X,'  DIRECTION OF AXIAL FORCE MONITORING. . . =',9X,A1/
     . 5X,'  DIRECTION OF BENDING MOMENT MONITORING . =',9X,A1/
     . 5X,'  CFC CHANNEL FREQUENCY CLASS. . . . . . . =',E12.4/
     . 5X,'  FILTER CONSTANT. . . . . . . . . . . . . =',E12.4/)
c-----------
      RETURN
      END
