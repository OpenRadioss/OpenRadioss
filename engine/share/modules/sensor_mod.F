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
Chd|  SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|-- called by -----------
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|        CBILAN                        source/elements/shell/coque/cbilan.F
Chd|        ECRIT                         source/output/ecrit.F         
Chd|        FILTER_SAE211                 source/tools/sensor/filter_sae211.F
Chd|        FORINT                        source/elements/forint.F      
Chd|        FORINTC                       source/elements/forintc.F     
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|        READ_SENSOR_TAB               source/output/restart/read_sensor_tab.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_HEAD                    source/engine/resol_head.F    
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|        RSENS_NIC                     source/tools/sensor/rsens_nic.F
Chd|        SENSOR0                       source/tools/sensor/sensor0.F 
Chd|        SENSOR1                       source/tools/sensor/sensor1.F 
Chd|        SENSOR2                       source/tools/sensor/sensor2.F 
Chd|        SENSOR_DIST_SURF              source/tools/sensor/sensor_dist_surf.F
Chd|        SENSOR_DIST_SURF0             source/tools/sensor/sensor_dist_surf0.F
Chd|        SENSOR_ENERGY                 source/tools/sensor/sensor_energy.F
Chd|        SENSOR_ENERGY_BILAN           source/tools/sensor/sensor_energy_bilan.F
Chd|        SENSOR_ENERGY_PART            source/tools/sensor/sensor_energy_part.F
Chd|        SENSOR_HIC                    source/tools/sensor/sensor_hic.F
Chd|        SENSOR_INIT                   source/tools/sensor/sensor_init.F
Chd|        SENSOR_NIC                    source/tools/sensor/sensor_nic.F
Chd|        SENSOR_TEMP                   source/tools/sensor/sensor_temp.F
Chd|        SENSOR_TEMP0                  source/tools/sensor/sensor_temp0.F
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|        WRITE_SENSOR_TAB              source/output/restart/write_sensor_tab.F
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE SENSOR_MOD
!$COMMENT
!       SENSOR_MOD :
!              create the structur for the sensor energy
!               * SUB_SENSOR_TYPE & SENSOR_TYPE : structure for the parith/on accumulation
!               * SENSOR_GROUP : structure for the element group
!               * SENSOR_COMM  : structuer for the mpi communication
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
        INTEGER :: STATUS      !   sensor status
                               !          = 0   : desactivated
                               !          = 1   : criterium reached, testing duration time Tmin
                               !          = 2   : criterium reached, waiting Tdelay before activation
                               !          = 3   : activated at TSTART
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
c
      ! -----------------------------------
      TYPE SUB_SENSOR_TYPE
        INTEGER :: PART     !   id of the part
        INTEGER :: NUM_GROUP_PART   !   number of element group per part
        REAL(KIND=8), DIMENSION(:,:,:), ALLOCATABLE :: FBSAV6_SENS      !   FBSAV6 double precision array (parith/on array)
      END TYPE SUB_SENSOR_TYPE
      ! -----------------------------------
      TYPE SENSOR_TYPE
        INTEGER :: TYP      !   type of energy sensor : 1 --> only 1 part ; 2 --> several subparts
        INTEGER :: PART     !   id of the part
        INTEGER :: NB_SUB   !   number of subpart only useful for typ=2
        INTEGER :: NUM_GROUP_PART   !   number of element group per part
        type(sub_sensor_type), DIMENSION(:), ALLOCATABLE :: SUB
        REAL(KIND=8), DIMENSION(:,:,:), ALLOCATABLE :: FBSAV6_SENS      !   FBSAV6 double precision array (parith/on array)
      END TYPE SENSOR_TYPE
      ! -----------------------------------
      TYPE SENSOR_GROUP
        INTEGER :: NUM_PART     !   number of part per element group
        INTEGER, DIMENSION(:,:), ALLOCATABLE :: PART   !   size = (num_part,3)
!       structur of SENS_GROUP%PART :
!       (1:num_part,1): id of the part
!       (1:num_part,2): type of energy sensor (1=part / 2=subset)
!       (1:num_part,3): sensor linked to the current part
      END TYPE SENSOR_GROUP
      ! -----------------------------------
      TYPE SENSOR_COMM
        LOGICAL :: BOOL         !   boolean : true if there is one or more energy sensors
        INTEGER :: NUM_SENS   !   number of sensor
        INTEGER :: BUFFER_SIZE_MEAN   !   buffer size for MPI_SUM reduction
        INTEGER :: BUFFER_SIZE_MIN_MAX   !   buffer size for MIN/MAX reduction
        INTEGER, DIMENSION(:), ALLOCATABLE :: ID_SENS   !   id of the sensor
      END TYPE SENSOR_COMM
      ! -----------------------------------
      type(sensor_comm) :: COMM_SENS14 ! struture for mpi communication : sensor typ14
      type(sensor_comm) :: COMM_SENS16 ! struture for mpi communication : sensor typ16
      type(sensor_comm) :: COMM_SENS17 ! struture for mpi communication : sensor typ17 --> sensor temperature
      type(sensor_type), DIMENSION(:), ALLOCATABLE :: SENSOR_STRUCT     !   structure of energy sensor
      type(sensor_group), DIMENSION(:), ALLOCATABLE :: SENS_GROUP       !   structure of energy sensor
      INTEGER :: NGR_SENSOR
!$OMP THREADPRIVATE (NGR_SENSOR)
      END MODULE SENSOR_MOD
