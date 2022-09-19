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
Chd|  SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|-- called by -----------
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|        DOMDEC2                       source/spmd/domdec2.F         
Chd|        HM_READ_PRELOAD               source/loads/general/preload/hm_read_preload.F
Chd|        HM_READ_SENSORS               source/tools/sensor/hm_read_sensors.F
Chd|        INISEN                        source/tools/sensor/inisen.F  
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        READ_SENSOR_ACC               source/tools/sensor/read_sensor_acc.F
Chd|        READ_SENSOR_AND               source/tools/sensor/read_sensor_and.F
Chd|        READ_SENSOR_CONTACT           source/tools/sensor/read_sensor_contact.F
Chd|        READ_SENSOR_DISP              source/tools/sensor/read_sensor_disp.F
Chd|        READ_SENSOR_DIST_SURF         source/tools/sensor/read_sensor_dist_surf.F
Chd|        READ_SENSOR_ENERGY            source/tools/sensor/read_sensor_energy.F
Chd|        READ_SENSOR_GAUGE             source/tools/sensor/read_sensor_gauge.F
Chd|        READ_SENSOR_HIC               source/tools/sensor/read_sensor_hic.F
Chd|        READ_SENSOR_NIC               source/tools/sensor/read_sensor_nic.F
Chd|        READ_SENSOR_NOT               source/tools/sensor/read_sensor_not.F
Chd|        READ_SENSOR_OR                source/tools/sensor/read_sensor_or.F
Chd|        READ_SENSOR_RBODY             source/tools/sensor/read_sensor_rbody.F
Chd|        READ_SENSOR_RWALL             source/tools/sensor/read_sensor_rwall.F
Chd|        READ_SENSOR_SECT              source/tools/sensor/read_sensor_sect.F
Chd|        READ_SENSOR_SENS              source/tools/sensor/read_sensor_sens.F
Chd|        READ_SENSOR_TEMP              source/tools/sensor/read_sensor_temp.F
Chd|        READ_SENSOR_TIME              source/tools/sensor/read_sensor_time.F
Chd|        READ_SENSOR_USER              source/tools/sensor/read_sensor_user.F
Chd|        READ_SENSOR_VEL               source/tools/sensor/read_sensor_vel.F
Chd|        READ_SENSOR_WORK              source/tools/sensor/read_sensor_work.F
Chd|        SENSOR_TAB_INIT               source/tools/sensor/sensor_tab_init.F
Chd|        SENSOR_USER_ALLOC             source/tools/sensor/sensor_user_alloc.F
Chd|        SENSOR_USER_INIT              source/tools/sensor/sensor_user_init.F
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|        ST_QAPRINT_MODEL_TOOLS        source/output/qaprint/st_qaprint_model_tools.F
Chd|        WRITE_SENSOR_TAB              source/tools/sensor/write_sensor_tab.F
Chd|        SET_U_SENS_SPMD_NODE_LIST     source/tools/sensor/set_u_sens_spmd_node_list.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE SENSOR_MOD
!$COMMENT
!       SENSOR_MOD :
!               * SENSOR_STR   : general sensor structure with dynamic allocation
!$ENDCOMMENT
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------
      TYPE SENSOR_STR_
        INTEGER :: TYPE        !   sensor type
        INTEGER :: SENS_ID     !   sensor ID
        INTEGER :: STATUS      !   sensor status (activated / deactivated)
        my_real :: TCRIT       !   time when activation criterion is met
        my_real :: TMIN        !   time duration of crit value before activation
        my_real :: TDELAY      !   time delay before activation (after Tmin)
        my_real :: TSTART      !   time when sensor is finally activated (for output)
        my_real :: VALUE       !   actual value of sensor criterion
        INTEGER :: NPARI       !   number of constant integer parameters
        INTEGER :: NPARR       !   number of constant real value parameters
        INTEGER :: NVAR        !   number of internal variables
        INTEGER ,DIMENSION(:) ,ALLOCATABLE :: IPARAM  !  integer parameter array
        my_real ,DIMENSION(:) ,ALLOCATABLE :: RPARAM  !  real parameter array
        my_real ,DIMENSION(:) ,ALLOCATABLE :: VAR     !  internal variables array
      END TYPE SENSOR_STR_


      TYPE SENSOR_USER_STRUCT_
        LOGICAL :: IS_USED          ! boolean, true if a user sensor is activated
        INTEGER :: POINTER_NODE,NUMBER_NODE ! nbr of node of user sensor + index
        INTEGER :: POINTER_PART,NUMBER_PART ! nbr of part of user sensor + index
        INTEGER :: POINTER_NODE_PER_PART,NUMBER_NODE_PER_PART ! nbr of node defined in a part of user sensor + index
        INTEGER, DIMENSION(:), ALLOCATABLE :: NODE_LIST ! list of node
        INTEGER, DIMENSION(:), ALLOCATABLE :: PART_LIST ! list of part
        INTEGER, DIMENSION(:), ALLOCATABLE :: NODE_PER_PART_LIST ! list of node defined in a part
      END TYPE SENSOR_USER_STRUCT_

      TYPE(SENSOR_USER_STRUCT_) :: SENSOR_USER_STRUCT
c
c--------------------------
      END MODULE SENSOR_MOD
