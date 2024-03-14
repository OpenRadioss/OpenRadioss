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
Chd|====================================================================
Chd|  SENSOR_MOD                    share/modules/sensor_mod.F    
Chd|-- called by -----------
Chd|        AIRBAGA                       source/airbag/airbag1.F       
Chd|        AIRBAGA1                      source/airbag/airbaga1.F      
Chd|        ALEFVM_GRAV_INIT              source/ale/alefvm/alefvm_grav_init.F
Chd|        ALEMAIN                       source/ale/alemain.F          
Chd|        ANIM_BUILD_INDEX_ALL          source/output/anim/reader/anim_build_index_all.F
Chd|        ANIM_SET2ZERO_STRUCT          source/output/anim/reader/anim_set2zero_struct.F
Chd|        BOLTST                        source/elements/solid/solide/boltst.F
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|        CBILAN                        source/elements/shell/coque/cbilan.F
Chd|        CDK6FORC3                     source/elements/sh3n/coquedk6/cdk6forc3.F
Chd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
Chd|        CFIELD_1                      source/loads/general/load_centri/cfield.F
Chd|        CFIELD_IMP                    source/loads/general/load_centri/cfield_imp.F
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|        CMAIN3                        source/materials/mat_share/cmain3.F
Chd|        CONVEC                        source/constraints/thermic/convec.F
Chd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
Chd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
Chd|        DAASOLV                       source/fluid/daasolv.F        
Chd|        DAASOLVP                      source/fluid/daasolvp.F       
Chd|        DESACTI                       source/elements/desacti.F     
Chd|        DYNA_INA                      source/implicit/imp_dyna.F    
Chd|        DYNA_WEX                      source/implicit/imp_dyna.F    
Chd|        ECRIT                         source/output/ecrit.F         
Chd|        ENG_QAPRINT_ANIMINPUT         source/output/qaprint/eng_qaprint_animinput.F
Chd|        ENG_QAPRINT_DRIVER            source/output/qaprint/eng_qaprint_driver.F
Chd|        ENG_QAPRINT_GENERALCONTROLSINPUTsource/output/qaprint/eng_qaprint_generalcontrolsinput.F
Chd|        FILTER_SAE211                 source/tools/sensor/filter_sae211.F
Chd|        FIXFINGEO                     source/constraints/general/impvel/fixfingeo.F
Chd|        FIXFLUX                       source/constraints/thermic/fixflux.F
Chd|        FIXTEMP                       source/constraints/thermic/fixtemp.F
Chd|        FIXVEL                        source/constraints/general/impvel/fixvel.F
Chd|        FLOW0                         source/fluid/flow0.F          
Chd|        FORCE                         source/loads/general/force.F  
Chd|        FORCEFINGEO                   source/loads/general/forcefingeo.F
Chd|        FORCEPINCH                    source/loads/general/forcepinch.F
Chd|        FORCE_IMP                     source/loads/general/force_imp.F
Chd|        FORINT                        source/elements/forint.F      
Chd|        FORINTC                       source/elements/forintc.F     
Chd|        FREANIM                       source/output/anim/reader/freanim.F
Chd|        FREFORM                       source/input/freform.F        
Chd|        FREOUTP                       source/input/freoutp.F        
Chd|        FRESTAT                       source/input/frestat.F        
Chd|        FVBAG0                        source/airbag/fvbag0.F        
Chd|        FVBAG1                        source/airbag/fvbag1.F        
Chd|        FVINJT6                       source/airbag/fvinjt6.F       
Chd|        FVINJT8                       source/airbag/fvinjt8.F       
Chd|        FV_FINT0                      source/constraints/general/impvel/fv_imp0.F
Chd|        FV_IMP                        source/constraints/general/impvel/fv_imp0.F
Chd|        FV_UP_SWITCH                  source/airbag/fv_up_switch.F  
Chd|        FXBODFP2                      source/constraints/fxbody/fxbodfp.F
Chd|        FXBYFOR                       source/constraints/fxbody/fxbyfor.F
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|        GRAVIT                        source/loads/general/grav/gravit.F
Chd|        GRAVIT_FVM_FEM                source/loads/general/grav/gravit_fvm_fem.F
Chd|        GRAVIT_IMP                    source/loads/general/grav/gravit_imp.F
Chd|        H3D_PRE_SKIN_SCALAR           source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|        H3D_READ                      source/output/h3d/h3d_build_fortran/h3d_read.F
Chd|        H3D_SKIN_SCALAR               source/output/h3d/h3d_results/h3d_skin_scalar.F
Chd|        H3D_SKIN_VECTOR               source/output/h3d/h3d_results/h3d_skin_vector.F
Chd|        HIST2                         source/output/th/hist2.F      
Chd|        I21_ICRIT                     source/interfaces/intsort/i21_icrit.F
Chd|        I25COMP_1                     source/interfaces/int25/i25comp_1.F
Chd|        I25MAIND_2                    source/interfaces/int25/i25maind_2.F
Chd|        I25MAIN_NORM                  source/interfaces/int25/i25main_norm.F
Chd|        I25MAIN_SLID                  source/interfaces/int25/i25main_slid.F
Chd|        IMP_BUCK                      source/implicit/imp_buck.F    
Chd|        IMP_CHKM                      source/implicit/imp_solv.F    
Chd|        IMP_COMPAB                    source/implicit/imp_solv.F    
Chd|        IMP_COMPABP                   source/implicit/imp_solv.F    
Chd|        IMP_DTKIN                     source/implicit/imp_int_k.F   
Chd|        IMP_ICOMCRIT                  source/implicit/imp_int_k.F   
Chd|        IMP_INTDT                     source/implicit/imp_int_k.F   
Chd|        IMP_INTTD0                    source/implicit/imp_int_k.F   
Chd|        IMP_INT_K                     source/implicit/imp_int_k.F   
Chd|        IMP_KPRES                     source/implicit/imp_glob_k.F  
Chd|        IMP_SOLV                      source/implicit/imp_solv.F    
Chd|        IMP_TRIPI                     source/implicit/imp_int_k.F   
Chd|        INCPFLOW                      source/fluid/incpflow.F       
Chd|        INTCRIT                       source/interfaces/intsort/intcrit.F
Chd|        INTER_CHECK_SORT              source/interfaces/generic/inter_check_sort.F
Chd|        INTER_DEALLOCATE_WAIT         source/interfaces/generic/inter_deallocate_wait.F
Chd|        INTER_SORT                    source/interfaces/generic/inter_sort.F
Chd|        INTFOP1                       source/interfaces/interf/intfop1.F
Chd|        INTFOP2                       source/interfaces/interf/intfop2.F
Chd|        INTFOP8                       source/interfaces/interf/intfop8.F
Chd|        INTTRI                        source/interfaces/intsort/inttri.F
Chd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
Chd|        LECINP                        source/input/lecinp.F         
Chd|        LECTUR                        source/input/lectur.F         
Chd|        LOAD_PRESSURE                 source/loads/general/load_pressure/load_pressure.F
Chd|        MANCTR                        source/input/manctr.F         
Chd|        MATERIAL_FLOW                 source/tools/seatbelts/material_flow.F
Chd|        MONVOL0                       source/airbag/monvol0.F       
Chd|        MPP_INIT                      source/mpi/interfaces/spmd_i7tool.F
Chd|        MULAWC                        source/materials/mat_share/mulawc.F
Chd|        PFLUID                        source/loads/general/pfluid/pfluid.F
Chd|        PRESSURE_CYL                  source/loads/general/load_pcyl/pressure_cyl.F
Chd|        R1SENS3                       source/elements/spring/r1sens3.F
Chd|        R23FORC3                      source/elements/spring/r23forc3.F
Chd|        R23L114DEF3                   source/elements/spring/r23l114def3.F
Chd|        R23LAW108                     source/elements/spring/r23law108.F
Chd|        R23LAW113                     source/elements/spring/r23law113.F
Chd|        R23LAW114                     source/elements/spring/r23law114.F
Chd|        R23SENS3                      source/elements/spring/r23sens3.F
Chd|        R2SENS3                       source/elements/spring/r2sens3.F
Chd|        RADIATION                     source/constraints/thermic/radiation.F
Chd|        RADIOSS2                      source/engine/radioss2.F      
Chd|        RBYSENS                       source/constraints/general/rbody/rbyonf.F
Chd|        RDCOMI                        source/output/restart/rdcomm.F
Chd|        RDRESA                        source/output/restart/rdresa.F
Chd|        RDRESB                        source/output/restart/rdresb.F
Chd|        READ_SENSORS                  source/output/restart/read_sensors.F
Chd|        RESOL                         source/engine/resol.F         
Chd|        RESOL_HEAD                    source/engine/resol_head.F    
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|        RFORC3                        source/elements/spring/rforc3.F
Chd|        RGJOINT                       source/elements/joint/rgjoint.F
Chd|        RSENS_NIC                     source/tools/sensor/rsens_nic.F
Chd|        RSKEW33                       source/elements/joint/rskew33.F
Chd|        RUSER32                       source/elements/spring/ruser32.F
Chd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
Chd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
Chd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
Chd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
Chd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
Chd|        SENSOR_ACC                    source/tools/sensor/sensor_acc.F
Chd|        SENSOR_AND                    source/tools/sensor/sensor_and.F
Chd|        SENSOR_BASE                   source/tools/sensor/sensor_base.F
Chd|        SENSOR_CONTACT                source/tools/sensor/sensor_contact.F
Chd|        SENSOR_DIST                   source/tools/sensor/sensor_dist.F
Chd|        SENSOR_DIST_SURF              source/tools/sensor/sensor_dist_surf.F
Chd|        SENSOR_DIST_SURF0             source/tools/sensor/sensor_dist_surf0.F
Chd|        SENSOR_ENERGY                 source/tools/sensor/sensor_energy.F
Chd|        SENSOR_ENERGY_BILAN           source/tools/sensor/sensor_energy_bilan.F
Chd|        SENSOR_ENERGY_PART            source/tools/sensor/sensor_energy_part.F
Chd|        SENSOR_ENERGY_TOTAL           source/tools/sensor/sensor_energy_total.F
Chd|        SENSOR_ENER_SAV               source/tools/sensor/sensor_ener_sav.F
Chd|        SENSOR_GAUGE                  source/tools/sensor/sensor_gauge.F
Chd|        SENSOR_HIC                    source/tools/sensor/sensor_hic.F
Chd|        SENSOR_INIT                   source/tools/sensor/sensor_init.F
Chd|        SENSOR_LOGICAL                source/tools/sensor/sensor_logical.F
Chd|        SENSOR_NIC                    source/tools/sensor/sensor_nic.F
Chd|        SENSOR_NOT                    source/tools/sensor/sensor_not.F
Chd|        SENSOR_OR                     source/tools/sensor/sensor_or.F
Chd|        SENSOR_RBODY                  source/tools/sensor/sensor_rbody.F
Chd|        SENSOR_RWALL                  source/tools/sensor/sensor_rwall.F
Chd|        SENSOR_SECTION                source/tools/sensor/sensor_section.F
Chd|        SENSOR_SENS                   source/tools/sensor/sensor_sens.F
Chd|        SENSOR_SPMD                   source/tools/sensor/sensor_spmd.F
Chd|        SENSOR_TEMP                   source/tools/sensor/sensor_temp.F
Chd|        SENSOR_TEMP0                  source/tools/sensor/sensor_temp0.F
Chd|        SENSOR_TIME                   source/tools/sensor/sensor_time.F
Chd|        SENSOR_VEL                    source/tools/sensor/sensor_vel.F
Chd|        SENSOR_WORK                   source/tools/sensor/sensor_work.F
Chd|        SFORC3                        source/elements/solid/solide/sforc3.F
Chd|        SIGEPS158C                    source/materials/mat/mat158/sigeps158c.F
Chd|        SIGEPS19C                     source/materials/mat/mat019/sigeps19c.F
Chd|        SIGEPS58C                     source/materials/mat/mat058/sigeps58c.F
Chd|        SMS_ENCIN_2                   source/ams/sms_encin_2.F      
Chd|        SMS_FIXVEL                    source/ams/sms_fixvel.F       
Chd|        SMS_GRAVIT                    source/ams/sms_gravit.F       
Chd|        SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F 
Chd|        SMS_PCG                       source/ams/sms_pcg.F          
Chd|        SORTIE_MAIN                   source/output/sortie_main.F   
Chd|        SPMD_I21FTHECOM               source/mpi/interfaces/send_cand.F
Chd|        SPMD_I21TEMPCOM               source/mpi/interfaces/send_cand.F
Chd|        SPMD_I7XVCOM2                 source/mpi/interfaces/spmd_i7xvcom2.F
Chd|        SPMD_IFRONT                   source/mpi/interfaces/spmd_ifront.F
Chd|        SPMD_IFRONT_STAMP             source/mpi/interfaces/send_cand.F
Chd|        SPMD_SAVEFI                   source/mpi/interfaces/spmd_i7tool.F
Chd|        STOP_SENSOR                   source/tools/sensor/stop_sensor.F
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|        THSENS                        source/output/th/thsens.F     
Chd|        TH_TIME_OUTPUT                source/output/th/th_time_output.F
Chd|        VOLPRE                        source/airbag/volpres.F       
Chd|        VOLPREP                       source/airbag/volpresp.F      
Chd|        WFV_IMP                       source/constraints/general/impvel/fv_imp0.F
Chd|        WRCOMI                        source/output/restart/wrcomm.F
Chd|        WRITE_SENSORS                 source/output/restart/write_sensors.F
Chd|        WRRESTP                       source/output/restart/wrrestp.F
Chd|        GET_U_NUMSENS                 source/user_interface/usensor.F
Chd|        GET_U_SENS                    source/user_interface/usensor.F
Chd|        GET_U_SENS_ACTI               source/user_interface/usensor.F
Chd|        GET_U_SENS_DELAY              source/user_interface/usensor.F
Chd|        GET_U_SENS_FPAR               source/user_interface/usensor.F
Chd|        GET_U_SENS_ID                 source/user_interface/usensor.F
Chd|        GET_U_SENS_IPAR               source/user_interface/usensor.F
Chd|        GET_U_SENS_VALUE              source/user_interface/usensor.F
Chd|        SET_U_SENS_ACTI               source/user_interface/usensor.F
Chd|        SET_U_SENS_DEACTI             source/user_interface/usensor.F
Chd|        SET_U_SENS_MAXVALUE           source/user_interface/usensor.F
Chd|        SET_U_SENS_VALUE              source/user_interface/usensor.F
Chd|        USER_INTERFACE_MOD            share/modules/user_interface_mod.F
Chd|-- calls ---------------
Chd|====================================================================
      MODULE SENSOR_MOD
      USE NAMES_AND_TITLES_MOD, ONLY:NCHARTITLE
!$COMMENT
!       SENSOR_MOD :
!              create the structure for the sensor energy
!               * SUB_SENSOR_TYPE & SENSOR_TYPE : structure for the parith/on accumulation
!               * SENSOR_GROUP : structure for the element group
!               * SENSOR_COMM  : structuer for the mpi communication
!               * SENSOR_STR   : general sensor structure with dynamic allocation
!$ENDCOMMENT
C-----------------------------------------------
C   m y _ r e a l
C-----------------------------------------------
#include "my_real.inc"
C-----------------------------------------------
C     fixed array sizes
C-----------------------------------------------
      INTEGER ,PARAMETER :: ISENBUF  = 20
      INTEGER ,PARAMETER :: LSENBUF  = 101
      INTEGER ,PARAMETER :: NSENPARI = 12
      INTEGER ,PARAMETER :: NSENPARR = 20
      INTEGER ,PARAMETER :: ISENPARI = 3
      INTEGER ,PARAMETER :: ISENPARR = 203
      INTEGER ,PARAMETER :: MX_SENS  = 50000 !used only in sensor list in engine options /STOP, /OUTP...

C-----------------------------------------------
C   D e r i v e d   T y p e   D e f i n i t i o n s
C-----------------------------------------------

      TYPE SENSOR_STR_
        INTEGER :: TYPE        !   sensor type 
        INTEGER :: SENS_ID     !   sensor User ID
        INTEGER :: STATUS      !   sensor status
                               !          = 0   : deactivated
                               !          = 1   : activated at TSTART
        CHARACTER(LEN = NCHARTITLE) :: TITLE
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
!       structure of SENS_GROUP%PART :
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
      type(sensor_comm) :: COMM_SENS14 ! structure for mpi communication : sensor typ14
      type(sensor_comm) :: COMM_SENS16 ! structure for mpi communication : sensor typ16
      type(sensor_comm) :: COMM_SENS17 ! structure for mpi communication : sensor typ17 --> sensor temperature
      type(sensor_type), DIMENSION(:), ALLOCATABLE :: SENSOR_STRUCT     !   structure of energy sensor
      type(sensor_group), DIMENSION(:), ALLOCATABLE :: SENS_GROUP       !   structure of energy sensor
      INTEGER :: NGR_SENSOR
!$OMP THREADPRIVATE (NGR_SENSOR)
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
        INTEGER :: NRESET
        INTEGER :: ANIM_ID
        INTEGER :: STOP_NSTH      !< /STOP/LSENSOR - Write time history file
        INTEGER :: STOP_NSANIM    !< /STOP/LSENSOR - Write animation file
        INTEGER :: STOP_NSSTAT    !< /STOP/LSENSOR - Write State file
        INTEGER :: STOP_NSOUTP    !< /STOP/LSENSOR - Write State file   
        INTEGER :: STOP_NSH3D     !< /STOP/LSENSOR - Write h3d state
        INTEGER :: STOP_NSABF     !< /STOP/LSENSOR - Write abf file
        my_real :: ANIM_DT

        TYPE (SENSOR_STR_) ,DIMENSION(:) ,ALLOCATABLE :: SENSOR_TAB

        INTEGER            ,DIMENSION(:) ,ALLOCATABLE :: STOP
        INTEGER            ,DIMENSION(:) ,ALLOCATABLE :: STAT
        INTEGER            ,DIMENSION(:) ,ALLOCATABLE :: OUTP
        INTEGER            ,DIMENSION(:) ,ALLOCATABLE :: ANIM
        INTEGER            ,DIMENSION(:) ,ALLOCATABLE :: RESET
        INTEGER            ,DIMENSION(:) ,ALLOCATABLE :: STOP_TMP
        INTEGER            ,DIMENSION(:) ,ALLOCATABLE :: OUTP_TMP
        INTEGER            ,DIMENSION(:) ,ALLOCATABLE :: STAT_TMP
        INTEGER            ,DIMENSION(:) ,ALLOCATABLE :: ANIM_TMP

        INTEGER, DIMENSION(:) ,ALLOCATABLE :: TABSENSOR  
        DOUBLE PRECISION ,DIMENSION(:,:,:) ,ALLOCATABLE :: FSAV  ! smpd communication array for "force" sensors
            
      END TYPE SENSORS_

      ! -----------------------------------
     

      END MODULE SENSOR_MOD

