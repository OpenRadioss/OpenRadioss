Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      !||    sensor_mod                  ../starter/share/modules1/sensor_mod.F
      !||--- called by ------------------------------------------------------
      !||    create_seatbelt             ../starter/source/tools/seatbelts/create_seatbelt.F
      !||    ddsplit                     ../starter/source/restart/ddsplit/ddsplit.F
      !||    domdec2                     ../starter/source/spmd/domdec2.F
      !||    fsdcod                      ../starter/source/system/fsdcod.F
      !||    hm_read_activ               ../starter/source/tools/activ/hm_read_activ.F
      !||    hm_read_bcs_wall            ../starter/source/boundary_conditions/hm_read_bcs_wall.F90
      !||    hm_read_grav                ../starter/source/loads/general/grav/hm_read_grav.F
      !||    hm_read_inivel              ../starter/source/initial_conditions/general/inivel/hm_read_inivel.F
      !||    hm_read_inter_struct        ../starter/source/interfaces/reader/hm_read_inter_struct.F
      !||    hm_read_inter_type05        ../starter/source/interfaces/int05/hm_read_inter_type05.F
      !||    hm_read_inter_type07        ../starter/source/interfaces/int07/hm_read_inter_type07.F
      !||    hm_read_inter_type11        ../starter/source/interfaces/int11/hm_read_inter_type11.F
      !||    hm_read_inter_type21        ../starter/source/interfaces/int21/hm_read_inter_type21.F
      !||    hm_read_inter_type24        ../starter/source/interfaces/int24/hm_read_inter_type24.F
      !||    hm_read_inter_type25        ../starter/source/interfaces/int25/hm_read_inter_type25.F
      !||    hm_read_interfaces          ../starter/source/interfaces/reader/hm_read_interfaces.F
      !||    hm_read_load_centri         ../starter/source/loads/general/load_centri/hm_read_load_centri.F
      !||    hm_read_load_pressure       ../starter/source/loads/general/load_pressure/hm_read_load_pressure.F
      !||    hm_read_monvol_type11       ../starter/source/airbag/hm_read_monvol_type11.F
      !||    hm_read_monvol_type4        ../starter/source/airbag/hm_read_monvol_type4.F
      !||    hm_read_monvol_type5        ../starter/source/airbag/hm_read_monvol_type5.F
      !||    hm_read_monvol_type6        ../starter/source/airbag/hm_read_monvol_type6.F
      !||    hm_read_monvol_type7        ../starter/source/airbag/hm_read_monvol_type7.F
      !||    hm_read_monvol_type8        ../starter/source/airbag/hm_read_monvol_type8.F
      !||    hm_read_monvol_type9        ../starter/source/airbag/hm_read_monvol_type9.F
      !||    hm_read_pcyl                ../starter/source/loads/general/load_pcyl/hm_read_pcyl.F
      !||    hm_read_pfluid              ../starter/source/loads/general/pfluid/hm_read_pfluid.F
      !||    hm_read_preload             ../starter/source/loads/general/preload/hm_read_preload.F
      !||    hm_read_preload_axial       ../starter/source/loads/general/preload/hm_read_preload_axial.F90
      !||    hm_read_rbody               ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    hm_read_sensors             ../starter/source/tools/sensor/hm_read_sensors.F
      !||    hm_read_thgrou              ../starter/source/output/th/hm_read_thgrou.F
      !||    hm_read_thgrsens            ../starter/source/output/th/hm_read_thgrsens.F
      !||    iniebcs                     ../starter/source/boundary_conditions/ebcs/iniebcs.F
      !||    iniparsen                   ../starter/source/tools/sensor/iniparsen.F
      !||    inisen                      ../starter/source/tools/sensor/inisen.F
      !||    init_monvol                 ../starter/source/airbag/init_monvol.F
      !||    initia                      ../starter/source/elements/initia/initia.F
      !||    inter_dcod_sensor           ../starter/source/interfaces/reader/inter_dcod_sensor.F
      !||    law158_upd                  ../starter/source/materials/mat/mat158/law158_upd.F
      !||    law19_upd                   ../starter/source/materials/mat/mat019/law19_upd.F90
      !||    law58_upd                   ../starter/source/materials/mat/mat058/law58_upd.F
      !||    lectur                      ../starter/source/starter/lectur.F
      !||    read_monvol                 ../starter/source/airbag/read_monvol.F
      !||    read_sensor_acc             ../starter/source/tools/sensor/read_sensor_acc.F
      !||    read_sensor_and             ../starter/source/tools/sensor/read_sensor_and.F
      !||    read_sensor_contact         ../starter/source/tools/sensor/read_sensor_contact.F
      !||    read_sensor_disp            ../starter/source/tools/sensor/read_sensor_disp.F
      !||    read_sensor_dist_surf       ../starter/source/tools/sensor/read_sensor_dist_surf.F
      !||    read_sensor_energy          ../starter/source/tools/sensor/read_sensor_energy.F
      !||    read_sensor_gauge           ../starter/source/tools/sensor/read_sensor_gauge.F
      !||    read_sensor_hic             ../starter/source/tools/sensor/read_sensor_hic.F
      !||    read_sensor_nic             ../starter/source/tools/sensor/read_sensor_nic.F
      !||    read_sensor_not             ../starter/source/tools/sensor/read_sensor_not.F
      !||    read_sensor_or              ../starter/source/tools/sensor/read_sensor_or.F
      !||    read_sensor_rbody           ../starter/source/tools/sensor/read_sensor_rbody.F
      !||    read_sensor_rwall           ../starter/source/tools/sensor/read_sensor_rwall.F
      !||    read_sensor_sect            ../starter/source/tools/sensor/read_sensor_sect.F
      !||    read_sensor_sens            ../starter/source/tools/sensor/read_sensor_sens.F
      !||    read_sensor_temp            ../starter/source/tools/sensor/read_sensor_temp.F
      !||    read_sensor_time            ../starter/source/tools/sensor/read_sensor_time.F
      !||    read_sensor_user            ../starter/source/tools/sensor/read_sensor_user.F
      !||    read_sensor_vel             ../starter/source/tools/sensor/read_sensor_vel.F
      !||    read_sensor_work            ../starter/source/tools/sensor/read_sensor_work.F
      !||    sensor_tab_init             ../starter/source/tools/sensor/sensor_tab_init.F
      !||    sensor_user_alloc           ../starter/source/tools/sensor/sensor_user_alloc.F
      !||    sensor_user_init            ../starter/source/tools/sensor/sensor_user_init.F
      !||    set_u_sens_fpar             ../starter/source/user_interface/uaccess.F
      !||    set_u_sens_ipar             ../starter/source/user_interface/uaccess.F
      !||    set_u_sens_spmd_node_list   ../starter/source/tools/sensor/set_u_sens_spmd_node_list.F
      !||    sort_logical_sensors        ../starter/source/tools/sensor/sort_logical_sensors.F
      !||    st_qaprint_driver           ../starter/source/output/qaprint/st_qaprint_driver.F
      !||    st_qaprint_model_tools      ../starter/source/output/qaprint/st_qaprint_model_tools.F
      !||    updmat                      ../starter/source/materials/updmat.F
      !||    user_interface_mod          ../starter/share/modules1/user_interface_mod.F
      !||    user_sensor_mod             ../starter/share/modules1/user_interface_mod.F
      !||    wrcomip                     ../starter/source/restart/ddsplit/wrcommp.F
      !||    write_sensors               ../starter/source/tools/sensor/write_sensors.F
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      MODULE SENSOR_MOD
!$COMMENT
!       SENSOR_MOD :
!               * SENSOR_STR   : general sensor structure with dynamic allocation
!$ENDCOMMENT
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
        USE NAMES_AND_TITLES_MOD , ONLY : nchartitle
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include "my_real.inc"
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
        INTEGER :: SENS_ID     !   sensor User ID
        INTEGER :: STATUS      !   sensor status
                               !          = 0   : deactivated
                               !          = 1   : activated at TSTART
        CHARACTER(LEN = nchartitle) :: TITLE
        my_real :: TCRIT       !   time when activation criterion is met
        my_real :: TMIN        !   time duration of crit value before activation
        my_real :: TDELAY      !   time delay before activation (after Tmin)
        my_real :: TSTART      !   time when sensor is finally activated (for output)
        my_real :: VALUE       !   actual sensor value
        INTEGER :: NPARI       !   number of constant integer parameters
        INTEGER :: NPARR       !   number of constant real value parameters
        INTEGER :: NVAR        !   number of internal variables
        INTEGER ,DIMENSION(:) ,ALLOCATABLE :: IPARAM  !  integer parameter array
        my_real ,DIMENSION(:) ,ALLOCATABLE :: RPARAM  !  real parameter array
        my_real ,DIMENSION(:) ,ALLOCATABLE :: VAR     !  internal variables array
        ! User Sensor buffers
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
      
      
      ! -----------------------------------
      !  Main sensor data structure    
      ! -----------------------------------
      TYPE SENSORS_
      
        INTEGER :: NSENSOR
        INTEGER :: STABSEN
        INTEGER :: SFSAV
        INTEGER :: NSTOP
        INTEGER :: NSTAT
        INTEGER :: NOUTP
        INTEGER :: NANIM


        TYPE (SENSOR_STR_) ,DIMENSION(:)     ,ALLOCATABLE :: SENSOR_TAB
            
        DOUBLE PRECISION   ,DIMENSION(:,:,:) ,ALLOCATABLE :: FSAV  ! smpd communication array for "force" sensors
        INTEGER            ,DIMENSION(:)     ,ALLOCATABLE :: TABSENSOR  

      END TYPE SENSORS_

      
      
      
c--------------------------
      END MODULE SENSOR_MOD



      

