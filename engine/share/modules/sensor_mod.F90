!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
!Copyright>
!Copyright>        This program is free software: you can redistribute it and/or modify
!Copyright>        it under the terms of the GNU Affero General Public License as published by
!Copyright>        the Free Software Foundation, either version 3 of the License, or
!Copyright>        (at your option) any later version.
!Copyright>
!Copyright>        This program is distributed in the hope that it will be useful,
!Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
!Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!Copyright>        GNU Affero General Public License for more details.
!Copyright>
!Copyright>        You should have received a copy of the GNU Affero General Public License
!Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
!Copyright>
!Copyright>
!Copyright>        Commercial Alternative: Altair Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
!Copyright>        software under a commercial license.  Contact Altair to discuss further if the
!Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
!hd|====================================================================
!hd|  SENSOR_MOD                    share/modules/sensor_mod.F
!hd|-- called by -----------
!hd|        AIRBAGA                       source/airbag/airbag1.F
!hd|        AIRBAGA1                      source/airbag/airbaga1.F
!hd|        ALEFVM_GRAV_INIT              source/ale/alefvm/alefvm_grav_init.F
!hd|        ALEMAIN                       source/ale/alemain.F
!hd|        ANIM_BUILD_INDEX_ALL          source/output/anim/reader/anim_build_index_all.F
!hd|        ANIM_SET2ZERO_STRUCT          source/output/anim/reader/anim_set2zero_struct.F
!hd|        BOLTST                        source/elements/solid/solide/boltst.F
!hd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
!hd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
!hd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
!hd|        CBILAN                        source/elements/shell/coque/cbilan.F
!hd|        CDK6FORC3                     source/elements/sh3n/coquedk6/cdk6forc3.F
!hd|        CDKFORC3                      source/elements/sh3n/coquedk/cdkforc3.F
!hd|        CFIELD_1                      source/loads/general/load_centri/cfield.F
!hd|        CFIELD_IMP                    source/loads/general/load_centri/cfield_imp.F
!hd|        CFORC3                        source/elements/shell/coque/cforc3.F
!hd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
!hd|        CMAIN3                        source/materials/mat_share/cmain3.F
!hd|        CONVEC                        source/constraints/thermic/convec.F
!hd|        CZFORC3                       source/elements/shell/coquez/czforc3.F
!hd|        CZFORC3_CRK                   source/elements/xfem/czforc3_crk.F
!hd|        DAASOLV                       source/fluid/daasolv.F
!hd|        DAASOLVP                      source/fluid/daasolvp.F
!hd|        DESACTI                       source/elements/desacti.F
!hd|        DYNA_INA                      source/implicit/imp_dyna.F
!hd|        DYNA_WEX                      source/implicit/imp_dyna.F
!hd|        ECRIT                         source/output/ecrit.F
!hd|        ENG_QAPRINT_ANIMINPUT         source/output/qaprint/eng_qaprint_animinput.F
!hd|        ENG_QAPRINT_DRIVER            source/output/qaprint/eng_qaprint_driver.F
!hd|        ENG_QAPRINT_GENERALCONTROLSINPUTsource/output/qaprint/eng_qaprint_generalcontrolsinput.F
!hd|        FILTER_SAE211                 source/tools/sensor/filter_sae211.F
!hd|        FIXFINGEO                     source/constraints/general/impvel/fixfingeo.F
!hd|        FIXFLUX                       source/constraints/thermic/fixflux.F
!hd|        FIXTEMP                       source/constraints/thermic/fixtemp.F
!hd|        FIXVEL                        source/constraints/general/impvel/fixvel.F
!hd|        FLOW0                         source/fluid/flow0.F
!hd|        FORCEFINGEO                   source/loads/general/forcefingeo.F
!hd|        FORCEPINCH                    source/loads/general/forcepinch.F
!hd|        FORCE_IMP                     source/loads/general/force_imp.F
!hd|        FORINT                        source/elements/forint.F
!hd|        FORINTC                       source/elements/forintc.F
!hd|        FREANIM                       source/output/anim/reader/freanim.F
!hd|        FREFORM                       source/input/freform.F
!hd|        FREOUTP                       source/input/freoutp.F
!hd|        FRESTAT                       source/input/frestat.F
!hd|        FVBAG0                        source/airbag/fvbag0.F
!hd|        FVBAG1                        source/airbag/fvbag1.F
!hd|        FVINJT6                       source/airbag/fvinjt6.F
!hd|        FVINJT8                       source/airbag/fvinjt8.F
!hd|        FV_FINT0                      source/constraints/general/impvel/fv_imp0.F
!hd|        FV_IMP                        source/constraints/general/impvel/fv_imp0.F
!hd|        FV_UP_SWITCH                  source/airbag/fv_up_switch.F
!hd|        FXBODFP2                      source/constraints/fxbody/fxbodfp.F
!hd|        FXBYFOR                       source/constraints/fxbody/fxbyfor.F
!hd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
!hd|        GRAVIT                        source/loads/general/grav/gravit.F
!hd|        GRAVIT_FVM_FEM                source/loads/general/grav/gravit_fvm_fem.F
!hd|        GRAVIT_IMP                    source/loads/general/grav/gravit_imp.F
!hd|        H3D_PRE_SKIN_SCALAR           source/output/h3d/h3d_results/h3d_skin_scalar.F
!hd|        H3D_READ                      source/output/h3d/h3d_build_fortran/h3d_read.F
!hd|        H3D_SKIN_SCALAR               source/output/h3d/h3d_results/h3d_skin_scalar.F
!hd|        H3D_SKIN_VECTOR               source/output/h3d/h3d_results/h3d_skin_vector.F
!hd|        HIST2                         source/output/th/hist2.F
!hd|        I21_ICRIT                     source/interfaces/intsort/i21_icrit.F
!hd|        I25COMP_1                     source/interfaces/int25/i25comp_1.F
!hd|        I25MAIND_2                    source/interfaces/int25/i25maind_2.F
!hd|        I25MAIN_NORM                  source/interfaces/int25/i25main_norm.F
!hd|        I25MAIN_SLID                  source/interfaces/int25/i25main_slid.F
!hd|        IMP_BUCK                      source/implicit/imp_buck.F
!hd|        IMP_CHKM                      source/implicit/imp_solv.F
!hd|        IMP_COMPAB                    source/implicit/imp_solv.F
!hd|        IMP_COMPABP                   source/implicit/imp_solv.F
!hd|        IMP_DTKIN                     source/implicit/imp_int_k.F
!hd|        IMP_ICOMCRIT                  source/implicit/imp_int_k.F
!hd|        IMP_INTDT                     source/implicit/imp_int_k.F
!hd|        IMP_INTTD0                    source/implicit/imp_int_k.F
!hd|        IMP_INT_K                     source/implicit/imp_int_k.F
!hd|        IMP_KPRES                     source/implicit/imp_glob_k.F
!hd|        IMP_SOLV                      source/implicit/imp_solv.F
!hd|        IMP_TRIPI                     source/implicit/imp_int_k.F
!hd|        INCPFLOW                      source/fluid/incpflow.F
!hd|        INTCRIT                       source/interfaces/intsort/intcrit.F
!hd|        INTER_CHECK_SORT              source/interfaces/generic/inter_check_sort.F
!hd|        INTER_DEALLOCATE_WAIT         source/interfaces/generic/inter_deallocate_wait.F
!hd|        INTER_SORT                    source/interfaces/generic/inter_sort.F
!hd|        INTFOP1                       source/interfaces/interf/intfop1.F
!hd|        INTFOP2                       source/interfaces/interf/intfop2.F
!hd|        INTFOP8                       source/interfaces/interf/intfop8.F
!hd|        INTTRI                        source/interfaces/intsort/inttri.F
!hd|        LAG_MULT                      source/tools/lagmul/lag_mult.F
!hd|        LECINP                        source/input/lecinp.F
!hd|        LECTUR                        source/input/lectur.F
!hd|        LOAD_PRESSURE                 source/loads/general/load_pressure/load_pressure.F
!hd|        MANCTR                        source/input/manctr.F
!hd|        MATERIAL_FLOW                 source/tools/seatbelts/material_flow.F
!hd|        MONVOL0                       source/airbag/monvol0.F
!hd|        MPP_INIT                      source/mpi/interfaces/spmd_i7tool.F
!hd|        MULAWC                        source/materials/mat_share/mulawc.F
!hd|        PFLUID                        source/loads/general/pfluid/pfluid.F
!hd|        PRESSURE_CYL                  source/loads/general/load_pcyl/pressure_cyl.F
!hd|        R1SENS3                       source/elements/spring/r1sens3.F
!hd|        R23FORC3                      source/elements/spring/r23forc3.F
!hd|        R23L114DEF3                   source/elements/spring/r23l114def3.F
!hd|        R23LAW108                     source/elements/spring/r23law108.F
!hd|        R23LAW113                     source/elements/spring/r23law113.F
!hd|        R23LAW114                     source/elements/spring/r23law114.F
!hd|        R23SENS3                      source/elements/spring/r23sens3.F
!hd|        R2SENS3                       source/elements/spring/r2sens3.F
!hd|        RADIATION                     source/constraints/thermic/radiation.F
!hd|        RADIOSS2                      source/engine/radioss2.F
!hd|        RBYSENS                       source/constraints/general/rbody/rbyonf.F
!hd|        RDCOMI                        source/output/restart/rdcomm.F
!hd|        RDRESA                        source/output/restart/rdresa.F
!hd|        RDRESB                        source/output/restart/rdresb.F
!hd|        READ_SENSORS                  source/output/restart/read_sensors.F
!hd|        RESOL                         source/engine/resol.F
!hd|        RESOL_HEAD                    source/engine/resol_head.F
!hd|        RESOL_INIT                    source/engine/resol_init.F
!hd|        RFORC3                        source/elements/spring/rforc3.F
!hd|        RGJOINT                       source/elements/joint/rgjoint.F
!hd|        RSENS_NIC                     source/tools/sensor/rsens_nic.F
!hd|        RSKEW33                       source/elements/joint/rskew33.F
!hd|        RUSER32                       source/elements/spring/ruser32.F
!hd|        S10FORC3                      source/elements/solid/solide10/s10forc3.F
!hd|        S4FORC3                       source/elements/solid/solide4/s4forc3.F
!hd|        S8CFORC3                      source/elements/thickshell/solide8c/s8cforc3.F
!hd|        S8EFORC3                      source/elements/solid/solide8e/s8eforc3.F
!hd|        S8ZFORC3                      source/elements/solid/solide8z/s8zforc3.F
!hd|        SENSOR_ACC                    source/tools/sensor/sensor_acc.F
!hd|        SENSOR_AND                    source/tools/sensor/sensor_and.F
!hd|        SENSOR_BASE                   source/tools/sensor/sensor_base.F
!hd|        SENSOR_CONTACT                source/tools/sensor/sensor_contact.F
!hd|        SENSOR_DIST                   source/tools/sensor/sensor_dist.F
!hd|        SENSOR_DIST_SURF              source/tools/sensor/sensor_dist_surf.F
!hd|        SENSOR_DIST_SURF0             source/tools/sensor/sensor_dist_surf0.F
!hd|        SENSOR_ENERGY                 source/tools/sensor/sensor_energy.F
!hd|        SENSOR_ENERGY_BILAN           source/tools/sensor/sensor_energy_bilan.F
!hd|        SENSOR_ENERGY_PART            source/tools/sensor/sensor_energy_part.F
!hd|        SENSOR_ENERGY_TOTAL           source/tools/sensor/sensor_energy_total.F
!hd|        SENSOR_ENER_SAV               source/tools/sensor/sensor_ener_sav.F
!hd|        SENSOR_GAUGE                  source/tools/sensor/sensor_gauge.F
!hd|        SENSOR_HIC                    source/tools/sensor/sensor_hic.F
!hd|        SENSOR_INIT                   source/tools/sensor/sensor_init.F
!hd|        SENSOR_LOGICAL                source/tools/sensor/sensor_logical.F
!hd|        SENSOR_NIC                    source/tools/sensor/sensor_nic.F
!hd|        SENSOR_NOT                    source/tools/sensor/sensor_not.F
!hd|        SENSOR_OR                     source/tools/sensor/sensor_or.F
!hd|        SENSOR_RBODY                  source/tools/sensor/sensor_rbody.F
!hd|        SENSOR_RWALL                  source/tools/sensor/sensor_rwall.F
!hd|        SENSOR_SECTION                source/tools/sensor/sensor_section.F
!hd|        SENSOR_SENS                   source/tools/sensor/sensor_sens.F
!hd|        SENSOR_SPMD                   source/tools/sensor/sensor_spmd.F
!hd|        SENSOR_TEMP                   source/tools/sensor/sensor_temp.F
!hd|        SENSOR_TEMP0                  source/tools/sensor/sensor_temp0.F
!hd|        SENSOR_TIME                   source/tools/sensor/sensor_time.F
!hd|        SENSOR_VEL                    source/tools/sensor/sensor_vel.F
!hd|        SENSOR_WORK                   source/tools/sensor/sensor_work.F
!hd|        SFORC3                        source/elements/solid/solide/sforc3.F
!hd|        SIGEPS158C                    source/materials/mat/mat158/sigeps158c.F
!hd|        SIGEPS19C                     source/materials/mat/mat019/sigeps19c.F
!hd|        SIGEPS58C                     source/materials/mat/mat058/sigeps58c.F
!hd|        SMS_ENCIN_2                   source/ams/sms_encin_2.F
!hd|        SMS_FIXVEL                    source/ams/sms_fixvel.F
!hd|        SMS_GRAVIT                    source/ams/sms_gravit.F
!hd|        SMS_MASS_SCALE_2              source/ams/sms_mass_scale_2.F
!hd|        SMS_PCG                       source/ams/sms_pcg.F
!hd|        SORTIE_MAIN                   source/output/sortie_main.F
!hd|        SPMD_I21FTHECOM               source/mpi/interfaces/send_cand.F
!hd|        SPMD_I21TEMPCOM               source/mpi/interfaces/send_cand.F
!hd|        SPMD_I7XVCOM2                 source/mpi/interfaces/spmd_i7xvcom2.F
!hd|        SPMD_IFRONT                   source/mpi/interfaces/spmd_ifront.F
!hd|        SPMD_IFRONT_STAMP             source/mpi/interfaces/send_cand.F
!hd|        SPMD_SAVEFI                   source/mpi/interfaces/spmd_i7tool.F
!hd|        STOP_SENSOR                   source/tools/sensor/stop_sensor.F
!hd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
!hd|        THSENS                        source/output/th/thsens.F
!hd|        TH_TIME_OUTPUT                source/output/th/th_time_output.F
!hd|        VOLPRE                        source/airbag/volpres.F
!hd|        VOLPREP                       source/airbag/volpresp.F
!hd|        WFV_IMP                       source/constraints/general/impvel/fv_imp0.F
!hd|        WRCOMI                        source/output/restart/wrcomm.F
!hd|        WRITE_SENSORS                 source/output/restart/write_sensors.F
!hd|        WRRESTP                       source/output/restart/wrrestp.F
!hd|        GET_U_NUMSENS                 source/user_interface/usensor.F
!hd|        GET_U_SENS                    source/user_interface/usensor.F
!hd|        GET_U_SENS_ACTI               source/user_interface/usensor.F
!hd|        GET_U_SENS_DELAY              source/user_interface/usensor.F
!hd|        GET_U_SENS_FPAR               source/user_interface/usensor.F
!hd|        GET_U_SENS_ID                 source/user_interface/usensor.F
!hd|        GET_U_SENS_IPAR               source/user_interface/usensor.F
!hd|        GET_U_SENS_VALUE              source/user_interface/usensor.F
!hd|        SET_U_SENS_ACTI               source/user_interface/usensor.F
!hd|        SET_U_SENS_DEACTI             source/user_interface/usensor.F
!hd|        SET_U_SENS_MAXVALUE           source/user_interface/usensor.F
!hd|        SET_U_SENS_VALUE              source/user_interface/usensor.F
!hd|        USER_INTERFACE_MOD            share/modules/user_interface_mod.F
!hd|-- calls ---------------
!hd|====================================================================
      MODULE SENSOR_MOD
        USE NAMES_AND_TITLES_MOD, ONLY:NCHARTITLE
        !       COMMENT
        !       SENSOR_MOD :
        !              create the structure for the sensor energy
        !               * SUB_SENSOR_TYPE & SENSOR_TYPE : structure for the parith/on accumulation
        !               * SENSOR_GROUP : structure for the element group
        !               * SENSOR_COMM  : structuer for the mpi communication
        !               * SENSOR_STR   : general sensor structure with dynamic allocation
        !       ENDCOMMENT
!-----------------------------------------------
!   m y _ r e a l
!-----------------------------------------------
#include "my_real.inc"
!-----------------------------------------------
!     fixed array sizes
!-----------------------------------------------
        INTEGER ,PARAMETER :: ISENBUF  = 20
        INTEGER ,PARAMETER :: LSENBUF  = 101
        INTEGER ,PARAMETER :: NSENPARI = 12
        INTEGER ,PARAMETER :: NSENPARR = 20
        INTEGER ,PARAMETER :: ISENPARI = 3
        INTEGER ,PARAMETER :: ISENPARR = 203
        INTEGER ,PARAMETER :: MX_SENS  = 50000 !used only in sensor list in engine options /STOP, /OUTP...

!-----------------------------------------------
!   D e r i v e d   T y p e   D e f i n i t i o n s
!-----------------------------------------------

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
!
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

