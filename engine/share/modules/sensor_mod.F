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
      !||    sensor_mod                         ../engine/share/modules/sensor_mod.F
      !||--- called by ------------------------------------------------------
      !||    airbaga                            ../engine/source/airbag/airbag1.F
      !||    airbaga1                           ../engine/source/airbag/airbaga1.F
      !||    alefvm_grav_init                   ../engine/source/ale/alefvm/alefvm_grav_init.F
      !||    alemain                            ../engine/source/ale/alemain.F
      !||    anim_build_index_all               ../engine/source/output/anim/reader/anim_build_index_all.F
      !||    anim_set2zero_struct               ../engine/source/output/anim/reader/anim_set2zero_struct.F
      !||    bcs_wall_trigger                   ../engine/source/boundary_conditions/bcs_wall_trigger.F90
      !||    boltst                             ../engine/source/elements/solid/solide/boltst.F
      !||    c3forc3                            ../engine/source/elements/sh3n/coque3n/c3forc3.F
      !||    c3forc3_crk                        ../engine/source/elements/xfem/c3forc3_crk.F
      !||    cbaforc3                           ../engine/source/elements/shell/coqueba/cbaforc3.F
      !||    cbilan                             ../engine/source/elements/shell/coque/cbilan.F
      !||    cdk6forc3                          ../engine/source/elements/sh3n/coquedk6/cdk6forc3.F
      !||    cdkforc3                           ../engine/source/elements/sh3n/coquedk/cdkforc3.F
      !||    cfield_1                           ../engine/source/loads/general/load_centri/cfield.F
      !||    cfield_imp                         ../engine/source/loads/general/load_centri/cfield_imp.F
      !||    cforc3                             ../engine/source/elements/shell/coque/cforc3.F
      !||    cforc3_crk                         ../engine/source/elements/xfem/cforc3_crk.F
      !||    cmain3                             ../engine/source/materials/mat_share/cmain3.F
      !||    convec                             ../engine/source/constraints/thermic/convec.F
      !||    czforc3                            ../engine/source/elements/shell/coquez/czforc3.F
      !||    czforc3_crk                        ../engine/source/elements/xfem/czforc3_crk.F
      !||    daasolv                            ../engine/source/fluid/daasolv.F
      !||    daasolvp                           ../engine/source/fluid/daasolvp.F
      !||    desacti                            ../engine/source/elements/desacti.F
      !||    dyna_ina                           ../engine/source/implicit/imp_dyna.F
      !||    dyna_wex                           ../engine/source/implicit/imp_dyna.F
      !||    ecrit                              ../engine/source/output/ecrit.F
      !||    eng_qaprint_animinput              ../engine/source/output/qaprint/eng_qaprint_animinput.F
      !||    eng_qaprint_driver                 ../engine/source/output/qaprint/eng_qaprint_driver.F
      !||    eng_qaprint_generalcontrolsinput   ../engine/source/output/qaprint/eng_qaprint_generalcontrolsinput.F
      !||    fixfingeo                          ../engine/source/constraints/general/impvel/fixfingeo.F
      !||    fixflux                            ../engine/source/constraints/thermic/fixflux.F
      !||    fixtemp                            ../engine/source/constraints/thermic/fixtemp.F
      !||    fixvel                             ../engine/source/constraints/general/impvel/fixvel.F
      !||    flow0                              ../engine/source/fluid/flow0.F
      !||    force                              ../engine/source/loads/general/force.F90
      !||    force_imp                          ../engine/source/loads/general/force_imp.F
      !||    forcefingeo                        ../engine/source/loads/general/forcefingeo.F
      !||    forcepinch                         ../engine/source/loads/general/forcepinch.F
      !||    forint                             ../engine/source/elements/forint.F
      !||    forintc                            ../engine/source/elements/forintc.F
      !||    freanim                            ../engine/source/output/anim/reader/freanim.F
      !||    freform                            ../engine/source/input/freform.F
      !||    freoutp                            ../engine/source/input/freoutp.F
      !||    frestat                            ../engine/source/input/frestat.F
      !||    fv_fint0                           ../engine/source/constraints/general/impvel/fv_imp0.F
      !||    fv_imp                             ../engine/source/constraints/general/impvel/fv_imp0.F
      !||    fv_up_switch                       ../engine/source/airbag/fv_up_switch.F
      !||    fvbag0                             ../engine/source/airbag/fvbag0.F
      !||    fvbag1                             ../engine/source/airbag/fvbag1.F
      !||    fvinjt6                            ../engine/source/airbag/fvinjt6.F
      !||    fvinjt8                            ../engine/source/airbag/fvinjt8.F
      !||    fxbodfp2                           ../engine/source/constraints/fxbody/fxbodfp.F
      !||    fxbyfor                            ../engine/source/constraints/fxbody/fxbyfor.F
      !||    genh3d                             ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    get_preload_axial                  ../engine/source/elements/spring/preload_axial.F90
      !||    get_u_numsens                      ../engine/source/user_interface/usensor.F
      !||    get_u_sens                         ../engine/source/user_interface/usensor.F
      !||    get_u_sens_acti                    ../engine/source/user_interface/usensor.F
      !||    get_u_sens_delay                   ../engine/source/user_interface/usensor.F
      !||    get_u_sens_fpar                    ../engine/source/user_interface/usensor.F
      !||    get_u_sens_id                      ../engine/source/user_interface/usensor.F
      !||    get_u_sens_ipar                    ../engine/source/user_interface/usensor.F
      !||    get_u_sens_value                   ../engine/source/user_interface/usensor.F
      !||    gravit                             ../engine/source/loads/general/grav/gravit.F
      !||    gravit_fvm_fem                     ../engine/source/loads/general/grav/gravit_fvm_fem.F
      !||    gravit_imp                         ../engine/source/loads/general/grav/gravit_imp.F
      !||    h3d_pre_skin_scalar                ../engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
      !||    h3d_read                           ../engine/source/output/h3d/h3d_build_fortran/h3d_read.F
      !||    h3d_skin_scalar                    ../engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
      !||    h3d_skin_vector                    ../engine/source/output/h3d/h3d_results/h3d_skin_vector.F
      !||    hist2                              ../engine/source/output/th/hist2.F
      !||    i21_icrit                          ../engine/source/interfaces/intsort/i21_icrit.F
      !||    i25comp_1                          ../engine/source/interfaces/int25/i25comp_1.F
      !||    i25main_norm                       ../engine/source/interfaces/int25/i25main_norm.F
      !||    i25main_slid                       ../engine/source/interfaces/int25/i25main_slid.F
      !||    i25maind_2                         ../engine/source/interfaces/int25/i25maind_2.F
      !||    imp_buck                           ../engine/source/implicit/imp_buck.F
      !||    imp_chkm                           ../engine/source/implicit/imp_solv.F
      !||    imp_compab                         ../engine/source/implicit/imp_solv.F
      !||    imp_compabp                        ../engine/source/implicit/imp_solv.F
      !||    imp_dtkin                          ../engine/source/implicit/imp_int_k.F
      !||    imp_icomcrit                       ../engine/source/implicit/imp_int_k.F
      !||    imp_int_k                          ../engine/source/implicit/imp_int_k.F
      !||    imp_intdt                          ../engine/source/implicit/imp_int_k.F
      !||    imp_inttd0                         ../engine/source/implicit/imp_int_k.F
      !||    imp_kpres                          ../engine/source/implicit/imp_glob_k.F
      !||    imp_solv                           ../engine/source/implicit/imp_solv.F
      !||    imp_tripi                          ../engine/source/implicit/imp_int_k.F
      !||    incpflow                           ../engine/source/fluid/incpflow.F
      !||    inivel_dt2                         ../engine/source/loads/general/inivel/inivel_dt2.F90
      !||    inivel_init                        ../engine/source/loads/general/inivel/inivel_init.F90
      !||    inivel_start                       ../engine/source/loads/general/inivel/inivel_start.F90
      !||    intcrit                            ../engine/source/interfaces/intsort/intcrit.F
      !||    inter_check_sort                   ../engine/source/interfaces/generic/inter_check_sort.F
      !||    inter_deallocate_wait              ../engine/source/interfaces/generic/inter_deallocate_wait.F
      !||    inter_sort                         ../engine/source/interfaces/generic/inter_sort.F
      !||    intfop1                            ../engine/source/interfaces/interf/intfop1.F
      !||    intfop2                            ../engine/source/interfaces/interf/intfop2.F
      !||    intfop8                            ../engine/source/interfaces/interf/intfop8.F
      !||    inttri                             ../engine/source/interfaces/intsort/inttri.F
      !||    lag_mult                           ../engine/source/tools/lagmul/lag_mult.F
      !||    lecinp                             ../engine/source/input/lecinp.F
      !||    lectur                             ../engine/source/input/lectur.F
      !||    load_pressure                      ../engine/source/loads/general/load_pressure/load_pressure.F
      !||    manctr                             ../engine/source/input/manctr.F
      !||    material_flow                      ../engine/source/tools/seatbelts/material_flow.F
      !||    monvol0                            ../engine/source/airbag/monvol0.F
      !||    mpp_init                           ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    mulawc                             ../engine/source/materials/mat_share/mulawc.F
      !||    pfluid                             ../engine/source/loads/general/pfluid/pfluid.F
      !||    preload_axial                      ../engine/source/elements/spring/preload_axial.F90
      !||    pressure_cyl                       ../engine/source/loads/general/load_pcyl/pressure_cyl.F
      !||    r1sens3                            ../engine/source/elements/spring/r1sens3.F
      !||    r23forc3                           ../engine/source/elements/spring/r23forc3.F
      !||    r23l114def3                        ../engine/source/elements/spring/r23l114def3.F
      !||    r23law108                          ../engine/source/elements/spring/r23law108.F
      !||    r23law113                          ../engine/source/elements/spring/r23law113.F
      !||    r23law114                          ../engine/source/elements/spring/r23law114.F
      !||    r23sens3                           ../engine/source/elements/spring/r23sens3.F
      !||    r2sens3                            ../engine/source/elements/spring/r2sens3.F
      !||    radiation                          ../engine/source/constraints/thermic/radiation.F
      !||    radioss2                           ../engine/source/engine/radioss2.F
      !||    rbysens                            ../engine/source/constraints/general/rbody/rbyonf.F
      !||    rdcomi                             ../engine/source/output/restart/rdcomm.F
      !||    rdresa                             ../engine/source/output/restart/rdresa.F
      !||    rdresb                             ../engine/source/output/restart/rdresb.F
      !||    read_sensors                       ../engine/source/output/restart/read_sensors.F
      !||    resol                              ../engine/source/engine/resol.F
      !||    resol_head                         ../engine/source/engine/resol_head.F
      !||    resol_init                         ../engine/source/engine/resol_init.F
      !||    rforc3                             ../engine/source/elements/spring/rforc3.F
      !||    rgjoint                            ../engine/source/elements/joint/rgjoint.F
      !||    rsens_nic                          ../engine/source/tools/sensor/rsens_nic.F
      !||    rskew33                            ../engine/source/elements/joint/rskew33.F
      !||    ruser32                            ../engine/source/elements/spring/ruser32.F
      !||    s10forc3                           ../engine/source/elements/solid/solide10/s10forc3.F
      !||    s4forc3                            ../engine/source/elements/solid/solide4/s4forc3.F
      !||    s8cforc3                           ../engine/source/elements/thickshell/solide8c/s8cforc3.F
      !||    s8eforc3                           ../engine/source/elements/solid/solide8e/s8eforc3.F
      !||    s8zforc3                           ../engine/source/elements/solid/solide8z/s8zforc3.F
      !||    sensor_acc                         ../engine/source/tools/sensor/sensor_acc.F
      !||    sensor_and                         ../engine/source/tools/sensor/sensor_and.F
      !||    sensor_base                        ../engine/source/tools/sensor/sensor_base.F
      !||    sensor_contact                     ../engine/source/tools/sensor/sensor_contact.F
      !||    sensor_dist                        ../engine/source/tools/sensor/sensor_dist.F
      !||    sensor_dist_surf                   ../engine/source/tools/sensor/sensor_dist_surf.F
      !||    sensor_dist_surf0                  ../engine/source/tools/sensor/sensor_dist_surf0.F
      !||    sensor_ener_sav                    ../engine/source/tools/sensor/sensor_ener_sav.F
      !||    sensor_energy                      ../engine/source/tools/sensor/sensor_energy.F
      !||    sensor_energy_bilan                ../engine/source/tools/sensor/sensor_energy_bilan.F
      !||    sensor_energy_part                 ../engine/source/tools/sensor/sensor_energy_part.F
      !||    sensor_energy_total                ../engine/source/tools/sensor/sensor_energy_total.F
      !||    sensor_gauge                       ../engine/source/tools/sensor/sensor_gauge.F
      !||    sensor_hic                         ../engine/source/tools/sensor/sensor_hic.F
      !||    sensor_init                        ../engine/source/tools/sensor/sensor_init.F
      !||    sensor_logical                     ../engine/source/tools/sensor/sensor_logical.F
      !||    sensor_nic                         ../engine/source/tools/sensor/sensor_nic.F
      !||    sensor_not                         ../engine/source/tools/sensor/sensor_not.F
      !||    sensor_or                          ../engine/source/tools/sensor/sensor_or.F
      !||    sensor_rbody                       ../engine/source/tools/sensor/sensor_rbody.F
      !||    sensor_rwall                       ../engine/source/tools/sensor/sensor_rwall.F
      !||    sensor_section                     ../engine/source/tools/sensor/sensor_section.F
      !||    sensor_sens                        ../engine/source/tools/sensor/sensor_sens.F
      !||    sensor_spmd                        ../engine/source/tools/sensor/sensor_spmd.F
      !||    sensor_temp                        ../engine/source/tools/sensor/sensor_temp.F
      !||    sensor_temp0                       ../engine/source/tools/sensor/sensor_temp0.F
      !||    sensor_time                        ../engine/source/tools/sensor/sensor_time.F
      !||    sensor_vel                         ../engine/source/tools/sensor/sensor_vel.F
      !||    sensor_work                        ../engine/source/tools/sensor/sensor_work.F
      !||    set_u_sens_acti                    ../engine/source/user_interface/usensor.F
      !||    set_u_sens_deacti                  ../engine/source/user_interface/usensor.F
      !||    set_u_sens_maxvalue                ../engine/source/user_interface/usensor.F
      !||    set_u_sens_value                   ../engine/source/user_interface/usensor.F
      !||    sforc3                             ../engine/source/elements/solid/solide/sforc3.F
      !||    sigeps158c                         ../engine/source/materials/mat/mat158/sigeps158c.F
      !||    sigeps19c                          ../engine/source/materials/mat/mat019/sigeps19c.F
      !||    sigeps58c                          ../engine/source/materials/mat/mat058/sigeps58c.F
      !||    sms_encin_2                        ../engine/source/ams/sms_encin_2.F
      !||    sms_fixvel                         ../engine/source/ams/sms_fixvel.F
      !||    sms_gravit                         ../engine/source/ams/sms_gravit.F
      !||    sms_mass_scale_2                   ../engine/source/ams/sms_mass_scale_2.F
      !||    sms_pcg                            ../engine/source/ams/sms_pcg.F
      !||    sortie_main                        ../engine/source/output/sortie_main.F
      !||    spmd_i21fthecom                    ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_i21tempcom                    ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_i7xvcom2                      ../engine/source/mpi/interfaces/spmd_i7xvcom2.F
      !||    spmd_ifront                        ../engine/source/mpi/interfaces/spmd_ifront.F
      !||    spmd_ifront_stamp                  ../engine/source/mpi/interfaces/send_cand.F
      !||    spmd_savefi                        ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    stop_sensor                        ../engine/source/tools/sensor/stop_sensor.F
      !||    szforc3                            ../engine/source/elements/solid/solidez/szforc3.F
      !||    th_time_output                     ../engine/source/output/th/th_time_output.F
      !||    thsens                             ../engine/source/output/th/thsens.F
      !||    user_interface_mod                 ../engine/share/modules/user_interface_mod.F
      !||    volpre                             ../engine/source/airbag/volpres.F
      !||    volprep                            ../engine/source/airbag/volpresp.F
      !||    wfv_imp                            ../engine/source/constraints/general/impvel/fv_imp0.F
      !||    wrcomi                             ../engine/source/output/restart/wrcomm.F
      !||    write_sensors                      ../engine/source/output/restart/write_sensors.F
      !||    wrrestp                            ../engine/source/output/restart/wrrestp.F
      !||--- calls      -----------------------------------------------------
      !||    nvar                               ../engine/source/input/nvar.F
      !||--- uses       -----------------------------------------------------
      !||    names_and_titles_mod               ../common_source/modules/names_and_titles_mod.F
      !||====================================================================
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

