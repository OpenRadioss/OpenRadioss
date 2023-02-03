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
Chd|  SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|-- called by -----------
Chd|        CREATE_SEATBELT               source/tools/seatbelts/create_seatbelt.F
Chd|        DDSPLIT                       source/restart/ddsplit/ddsplit.F
Chd|        DOMDEC2                       source/spmd/domdec2.F         
Chd|        FSDCOD                        source/system/fsdcod.F        
Chd|        HM_READ_ACTIV                 source/tools/activ/hm_read_activ.F
Chd|        HM_READ_GRAV                  source/loads/general/grav/hm_read_grav.F
Chd|        HM_READ_INTERFACES            source/interfaces/reader/hm_read_interfaces.F
Chd|        HM_READ_INTER_STRUCT          source/interfaces/reader/hm_read_inter_struct.F
Chd|        HM_READ_INTER_TYPE05          source/interfaces/int05/hm_read_inter_type05.F
Chd|        HM_READ_INTER_TYPE07          source/interfaces/int07/hm_read_inter_type07.F
Chd|        HM_READ_INTER_TYPE11          source/interfaces/int11/hm_read_inter_type11.F
Chd|        HM_READ_INTER_TYPE21          source/interfaces/int21/hm_read_inter_type21.F
Chd|        HM_READ_INTER_TYPE24          source/interfaces/int24/hm_read_inter_type24.F
Chd|        HM_READ_INTER_TYPE25          source/interfaces/int25/hm_read_inter_type25.F
Chd|        HM_READ_LOAD_CENTRI           source/loads/general/load_centri/hm_read_load_centri.F
Chd|        HM_READ_LOAD_PRESSURE         source/loads/general/load_pressure/hm_read_load_pressure.F
Chd|        HM_READ_MONVOL_TYPE11         source/airbag/hm_read_monvol_type11.F
Chd|        HM_READ_MONVOL_TYPE4          source/airbag/hm_read_monvol_type4.F
Chd|        HM_READ_MONVOL_TYPE5          source/airbag/hm_read_monvol_type5.F
Chd|        HM_READ_MONVOL_TYPE6          source/airbag/hm_read_monvol_type6.F
Chd|        HM_READ_MONVOL_TYPE7          source/airbag/hm_read_monvol_type7.F
Chd|        HM_READ_MONVOL_TYPE8          source/airbag/hm_read_monvol_type8.F
Chd|        HM_READ_MONVOL_TYPE9          source/airbag/hm_read_monvol_type9.F
Chd|        HM_READ_PFLUID                source/loads/general/pfluid/hm_read_pfluid.F
Chd|        HM_READ_PRELOAD               source/loads/general/preload/hm_read_preload.F
Chd|        HM_READ_RBODY                 source/constraints/general/rbody/hm_read_rbody.F
Chd|        HM_READ_SENSORS               source/tools/sensor/hm_read_sensors.F
Chd|        HM_READ_THGROU                source/output/th/hm_read_thgrou.F
Chd|        HM_READ_THGRSENS              source/output/th/hm_read_thgrsens.F
Chd|        INIEBCS                       source/boundary_conditions/ebcs/iniebcs.F
Chd|        INIPARSEN                     source/tools/sensor/iniparsen.F
Chd|        INISEN                        source/tools/sensor/inisen.F  
Chd|        INIT_MONVOL                   source/airbag/init_monvol.F   
Chd|        INTER_DCOD_SENSOR             source/interfaces/reader/inter_dcod_sensor.F
Chd|        LAW58_UPD                     source/materials/mat/mat058/law58_upd.F
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        READ_MONVOL                   source/airbag/read_monvol.F   
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
Chd|        SORT_LOGICAL_SENSORS          source/tools/sensor/sort_logical_sensors.F
Chd|        ST_QAPRINT_DRIVER             source/output/qaprint/st_qaprint_driver.F
Chd|        ST_QAPRINT_MODEL_TOOLS        source/output/qaprint/st_qaprint_model_tools.F
Chd|        UPDMAT                        source/materials/updmat.F     
Chd|        WRITE_SENSOR_TAB              source/tools/sensor/write_sensor_tab.F
Chd|        SET_U_SENS_FPAR               source/user_interface/uaccess.F
Chd|        SET_U_SENS_IPAR               source/user_interface/uaccess.F
Chd|        SET_U_SENS_SPMD_NODE_LIST     source/tools/sensor/set_u_sens_spmd_node_list.F
Chd|        SENSOR_VARIABLE_MOD           share/modules1/sensor_mod.F   
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
      INTEGER KSENS_CUR
      INTEGER ISENBUF
      INTEGER LSENBUF
      INTEGER NSENPARI
      INTEGER NSENPARR
      INTEGER ISENPARI
      INTEGER ISENPARR
      PARAMETER (ISENBUF=20)
      PARAMETER (LSENBUF=101)
      PARAMETER (NSENPARI=12)
      PARAMETER (NSENPARR=20)
      PARAMETER (ISENPARI=3)
      PARAMETER (ISENPARR=203)

      TYPE SENSOR_STR_
        INTEGER :: TYPE        !   sensor type 
        INTEGER :: SENS_ID     !   sensor ID
        INTEGER :: STATUS      !   sensor status
                               !          = 0   : deactivated
                               !          = 1   : activated at TSTART
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
        INTEGER ,DIMENSION(:) ,ALLOCATABLE :: INTEGER_USERBUF    !  Buffer to store integer variables
        my_real ,DIMENSION(:) ,ALLOCATABLE :: FLOAT_USERBUF      !  Buffer to store user variables.
        INTEGER ,DIMENSION(:) ,ALLOCATABLE :: INTEGER_USERPARAM  !  Buffer to store integer variables
        my_real ,DIMENSION(:) ,ALLOCATABLE :: FLOAT_USERPARAM    !  Buffer to store user variables.
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
      INTEGER LOGICAL_SENSOR_COUNT
      INTEGER, DIMENSION(:),ALLOCATABLE :: LOGICAL_SENSORS_LIST
c--------------------------
      END MODULE SENSOR_MOD


Chd|====================================================================
Chd|  SENSOR_VARIABLE_MOD           share/modules1/sensor_mod.F   
Chd|-- called by -----------
Chd|        LECTUR                        source/starter/lectur.F       
Chd|        SORT_LOGICAL_SENSORS          source/tools/sensor/sort_logical_sensors.F
Chd|        SET_U_SENS_FPAR               source/user_interface/uaccess.F
Chd|        SET_U_SENS_IPAR               source/user_interface/uaccess.F
Chd|-- calls ---------------
Chd|        SENSOR_MOD                    share/modules1/sensor_mod.F   
Chd|====================================================================
      MODULE SENSOR_VARIABLE_MOD
C-----------------------------------------------
         USE SENSOR_MOD
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include      "my_real.inc"
         TYPE (SENSOR_STR_)       ,DIMENSION(:) , ALLOCATABLE :: SENSOR_TAB       ! NSENSOR

         ! Logical sensors : and, or,not,sens sorted list
      END MODULE SENSOR_VARIABLE_MOD
      

