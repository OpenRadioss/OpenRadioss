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
!hd|  MULTI_FVM_MOD                 modules/ale/multi_fvm_mod.F
!hd|-- called by -----------
!hd|        EBCS_MOD                      common_source/modules/boundary_conditions/ebcs_mod.F
!hd|        ALELEC                        starter/source/ale/alelec.F
!hd|        C3GRHEAD                      starter/source/elements/sh3n/coque3n/c3grhead.F
!hd|        CONTRL                        starter/source/starter/contrl.F
!hd|        C_IXFLOC                      starter/source/restart/ddsplit/c_ixfloc.F
!hd|        C_MULTI_VEL                   starter/source/restart/ddsplit/c_multi_vel.F
!hd|        C_VOIS                        starter/source/restart/ddsplit/c_vois.F
!hd|        DDSPLIT                       starter/source/restart/ddsplit/ddsplit.F
!hd|        FREFORM                       starter/source/starter/freform.F
!hd|        HM_READ_EBCS_FLUXOUT          starter/source/boundary_conditions/ebcs/hm_read_ebcs_fluxout.F
!hd|        HM_READ_EBCS_GRADP0           starter/source/boundary_conditions/ebcs/hm_read_ebcs_gradp0.F
!hd|        HM_READ_EBCS_INIP             starter/source/boundary_conditions/ebcs/hm_read_ebcs_inip.F
!hd|        HM_READ_EBCS_INIV             starter/source/boundary_conditions/ebcs/hm_read_ebcs_iniv.F
!hd|        HM_READ_EBCS_INLET            starter/source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
!hd|        HM_READ_EBCS_MONVOL           starter/source/boundary_conditions/ebcs/hm_read_ebcs_monvol.F
!hd|        HM_READ_EBCS_NORMV            starter/source/boundary_conditions/ebcs/hm_read_ebcs_normv.F
!hd|        HM_READ_EBCS_NRF              starter/source/boundary_conditions/ebcs/hm_read_ebcs_nrf.F
!hd|        HM_READ_EBCS_PRES             starter/source/boundary_conditions/ebcs/hm_read_ebcs_pres.F
!hd|        HM_READ_EBCS_VALVIN           starter/source/boundary_conditions/ebcs/hm_read_ebcs_valvin.F
!hd|        HM_READ_EBCS_VALVOUT          starter/source/boundary_conditions/ebcs/hm_read_ebcs_valvout.F
!hd|        HM_READ_EBCS_VEL              starter/source/boundary_conditions/ebcs/hm_read_ebcs_vel.F
!hd|        HM_READ_INIMAP1D              starter/source/initial_conditions/inimap/hm_read_inimap1d.F
!hd|        HM_READ_INIVEL                starter/source/initial_conditions/general/inivel/hm_read_inivel.F
!hd|        HM_READ_INIVOL                starter/source/initial_conditions/inivol/hm_read_inivol.F
!hd|        HM_READ_INTERFACES            starter/source/interfaces/reader/hm_read_interfaces.F
!hd|        HM_READ_INTER_FSI             starter/source/interfaces/reader/hm_read_inter_fsi.F
!hd|        HM_READ_INTER_TYPE18          starter/source/interfaces/int18/hm_read_inter_type18.F
!hd|        HM_READ_MAT                   starter/source/materials/mat/hm_read_mat.F
!hd|        HM_READ_MAT151                starter/source/materials/mat/mat151/hm_read_mat151.F
!hd|        HM_READ_PART                  starter/source/model/assembling/hm_read_part.F
!hd|        HM_READ_PROP01                starter/source/properties/shell/hm_read_prop01.F
!hd|        HM_READ_PROP06                starter/source/properties/solid/hm_read_prop06.F
!hd|        HM_READ_PROP14                starter/source/properties/solid/hm_read_prop14.F
!hd|        HM_READ_PROP14F               starter/source/properties/solid/hm_read_prop14.F
!hd|        HM_READ_PROP20                starter/source/properties/thickshell/hm_read_prop20.F
!hd|        HM_READ_PROP22                starter/source/properties/thickshell/hm_read_prop22.F
!hd|        HM_READ_PROPERTIES            starter/source/properties/hm_read_properties.F
!hd|        INGRBRIC_DX                   starter/source/interfaces/interf1/ingrbric_dx.F
!hd|        INIFILL                       starter/source/initial_conditions/inivol/inifill.F
!hd|        INIGRAV_LOAD                  starter/source/initial_conditions/inigrav/inigrav_load.F
!hd|        ININTR                        starter/source/interfaces/interf1/inintr.F
!hd|        INIPHASE                      starter/source/initial_conditions/inivol/iniphase.F
!hd|        INITIA                        starter/source/elements/initia/initia.F
!hd|        INIVEL                        starter/source/initial_conditions/general/inivel/inivel.F
!hd|        INIVOL_SET                    starter/source/initial_conditions/inivol/inivol_set.F
!hd|        INI_FVMINIVEL                 starter/source/elements/initia/ini_fvminivel.F
!hd|        INI_INIMAP1D                  starter/source/initial_conditions/inimap/ini_inimap1d.F
!hd|        INI_INIMAP2D                  starter/stub/ini_inimap2d.F
!hd|        INT18_LAW151_ALLOC            starter/source/interfaces/int18/int18_law151_alloc.F
!hd|        INT18_LAW151_INIT             starter/source/interfaces/int18/int18_law151_init.F
!hd|        IPARI_L_INI                   starter/source/restart/ddsplit/ipari_l_ini.F
!hd|        LECINS                        starter/source/interfaces/interf1/lecins.F
!hd|        LECINT                        starter/source/interfaces/interf1/lecint.F
!hd|        LECTUR                        starter/source/starter/lectur.F
!hd|        LEC_INIMAP1D_FILE             starter/source/initial_conditions/inimap/lec_inimap1d_file.F
!hd|        MULTIFLUID_GLOBAL_TDET        starter/source/multifluid/multifluid_global_tdet.F
!hd|        MULTIFLUID_INIT2T             starter/source/multifluid/multifluid_init2t.F
!hd|        MULTIFLUID_INIT3              starter/source/multifluid/multifluid_init3.F
!hd|        MULTI_CHECK_EOS               starter/source/multifluid/multi_check_eos.F
!hd|        MULTI_CHECK_PSH               starter/source/multifluid/multi_check_psh.F
!hd|        MULTI_CONNECTIVITY            starter/source/multifluid/multi_connectivity.F
!hd|        PREPARE_SPLIT_I7              starter/source/restart/ddsplit/inter_tools.F
!hd|        READ_EBCS                     starter/source/boundary_conditions/ebcs/read_ebcs.F
!hd|        READ_MATERIAL_MODELS          starter/source/materials/read_material_models.F
!hd|        SPLIT_EBCS                    starter/source/boundary_conditions/ebcs/split_ebcs.F
!hd|        SPLIT_INTERFACES              starter/source/restart/ddsplit/split_interfaces.F
!hd|        STARTER0                      starter/source/starter/starter0.F
!hd|        ST_QAPRINT_DRIVER             starter/source/output/qaprint/st_qaprint_driver.F
!hd|        ST_QAPRINT_INITIAL_CONDITIONS starter/source/output/qaprint/st_qaprint_initial_conditions.F
!hd|        T3GRHEAD                      starter/source/elements/solid_2d/tria/t3grhead.F
!hd|        WRCOMIP                       starter/source/restart/ddsplit/wrcommp.F
!hd|        W_FI                          starter/source/restart/ddsplit/w_fi.F
!hd|        W_FRONT                       starter/source/restart/ddsplit/w_front.F
!hd|        ALEMAIN                       engine/source/ale/alemain.F
!hd|        ALEWDX                        engine/source/ale/grid/alewdx.F
!hd|        BUILD_CONNECTIVITY            engine/source/multifluid/connectivity.F
!hd|        DFUNC0                        engine/source/output/anim/generate/dfunc0.F
!hd|        DFUNCC                        engine/source/output/anim/generate/dfuncc.F
!hd|        DFUNCS                        engine/source/output/anim/generate/dfunc6.F
!hd|        EBCS10                        engine/source/boundary_conditions/ebcs/ebcs10.F
!hd|        EBCS_MAIN                     engine/source/boundary_conditions/ebcs/ebcs_main.F
!hd|        ECRIT                         engine/source/output/ecrit.F
!hd|        EIG                           engine/stub/eig.F
!hd|        EIG1                          engine/stub/eig1.F
!hd|        EIGCOND                       engine/stub/eigcond.F
!hd|        EIGP                          engine/stub/eigp.F
!hd|        GENANI                        engine/source/output/anim/generate/genani.F
!hd|        GENH3D                        engine/source/output/h3d/h3d_results/genh3d.F
!hd|        GENSTAT                       engine/source/output/sta/genstat.F
!hd|        H3D_GENE_KEYWORD              engine/source/output/h3d/input_list/h3d_gene_keyword.F
!hd|        H3D_LIST_QUAD_SCALAR          engine/source/output/h3d/input_list/h3d_list_quad_scalar.F
!hd|        H3D_LIST_SHELL_SCALAR         engine/source/output/h3d/input_list/h3d_list_shell_scalar.F
!hd|        H3D_LIST_SOLID_SCALAR         engine/source/output/h3d/input_list/h3d_list_solid_scalar.F
!hd|        H3D_NODAL_SCALAR              engine/source/output/h3d/h3d_results/h3d_nodal_scalar.F
!hd|        H3D_QUAD_SCALAR               engine/source/output/h3d/h3d_results/h3d_quad_scalar.F
!hd|        H3D_QUAD_VECTOR               engine/source/output/h3d/h3d_results/h3d_quad_vector.F
!hd|        H3D_SHELL_SCALAR              engine/source/output/h3d/h3d_results/h3d_shell_scalar.F
!hd|        H3D_SHELL_SCALAR_1            engine/source/output/h3d/h3d_results/h3d_shell_scalar_1.F
!hd|        H3D_SHELL_VECTOR              engine/source/output/h3d/h3d_results/h3d_shell_vector.F
!hd|        H3D_SHELL_VECTOR_1            engine/source/output/h3d/h3d_results/h3d_shell_vector_1.F
!hd|        H3D_SKIN_SCALAR               engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
!hd|        H3D_SOLID_SCALAR              engine/source/output/h3d/h3d_results/h3d_solid_scalar.F
!hd|        H3D_SOLID_SCALAR_1            engine/source/output/h3d/h3d_results/h3d_solid_scalar_1.F
!hd|        H3D_SOLID_VECTOR              engine/source/output/h3d/h3d_results/h3d_solid_vector.F
!hd|        HIST2                         engine/source/output/th/hist2.F
!hd|        I18DST3                       engine/source/interfaces/int18/i18dst3.F
!hd|        I18FOR3                       engine/source/interfaces/int18/i18for3.F
!hd|        I18MAIN_KINE_1                engine/source/interfaces/int18/i18main_kine.F
!hd|        I18MAIN_KINE_I                engine/source/interfaces/int18/i18main_kine.F
!hd|        I22MAIN_TRI                   engine/source/interfaces/intsort/i22main_tri.F
!hd|        I23MAIN_TRI                   engine/source/interfaces/intsort/i23main_tri.F
!hd|        I7MAINF                       engine/source/interfaces/int07/i7mainf.F
!hd|        I7MAIN_TRI                    engine/source/interfaces/intsort/i7main_tri.F
!hd|        IMP_BUCK                      engine/source/implicit/imp_buck.F
!hd|        IMP_INTTD0                    engine/source/implicit/imp_int_k.F
!hd|        IMP_SOLV                      engine/source/implicit/imp_solv.F
!hd|        IMP_TRIPI                     engine/source/implicit/imp_int_k.F
!hd|        INT18_ALLOC                   engine/source/interfaces/int18/int18_alloc.F
!hd|        INT18_LAW151_INIT             engine/source/interfaces/int18/int18_law151_init.F
!hd|        INT18_LAW151_NSV_SHIFT        engine/source/interfaces/int18/int18_law151_nsv_shift.F
!hd|        INT18_LAW151_OMP_ACCUMULATION engine/source/interfaces/int18/int18_law151_omp_accumulation.F
!hd|        INT18_LAW151_UPDATE           engine/source/interfaces/int18/int18_law151_update.F
!hd|        INTER_COLOR_VOXEL             engine/source/interfaces/generic/inter_color_voxel.F
!hd|        INTER_DEALLOCATE_WAIT         engine/source/interfaces/generic/inter_deallocate_wait.F
!hd|        INTER_SORT                    engine/source/interfaces/generic/inter_sort.F
!hd|        INTER_SORT_07                 engine/source/interfaces/int07/inter_sort_07.F
!hd|        INTFOP2                       engine/source/interfaces/interf/intfop2.F
!hd|        INTTRI                        engine/source/interfaces/intsort/inttri.F
!hd|        LECH3D                        engine/source/output/h3d/h3d_build_fortran/lech3d.F
!hd|        LECTUR                        engine/source/input/lectur.F
!hd|        MULTI_ALLOCATE                engine/source/multifluid/multi_allocate.F
!hd|        MULTI_BILAN                   engine/source/multifluid/multi_bilan.F
!hd|        MULTI_BUF2VAR                 engine/source/multifluid/multi_buf2var.F
!hd|        MULTI_COMPUTE_DT              engine/source/multifluid/multi_compute_dt.F
!hd|        MULTI_DEALLOCATE              engine/source/multifluid/multi_deallocate.F
!hd|        MULTI_EBCS                    engine/source/multifluid/multi_ebcs.F
!hd|        MULTI_EVOLVE_GLOBAL           engine/source/multifluid/multi_evolve_global.F
!hd|        MULTI_EVOLVE_PARTIAL          engine/source/multifluid/multi_evolve_partial.F
!hd|        MULTI_FACE_ELEM_DATA          engine/source/multifluid/multi_face_data_elem.F
!hd|        MULTI_FLUXES_COMPUTATION      engine/source/multifluid/multi_fluxes_computation.F
!hd|        MULTI_FLUXOUT_EBCS            engine/source/multifluid/multi_fluxout_ebcs.F
!hd|        MULTI_FVM2FEM                 engine/source/multifluid/multi_fvm2fem.F
!hd|        MULTI_GLOBALIZE               engine/source/multifluid/multi_globalize.F
!hd|        MULTI_I18_FORCE_POFF          engine/source/interfaces/int18/multi_i18_force_poff.F
!hd|        MULTI_I18_FORCE_PON           engine/source/interfaces/int18/multi_i18_force_pon.F
!hd|        MULTI_INLET_EBCS              engine/source/multifluid/multi_inlet_ebcs.F
!hd|        MULTI_MUSCL_GRADIENTS         engine/source/multifluid/multi_muscl_gradients.F
!hd|        MULTI_NRF_EBCS                engine/source/multifluid/multi_nrf_ebcs.F
!hd|        MULTI_PRESSURE_EQUILIBRIUM    engine/source/multifluid/multi_pressure_equilibrium.F
!hd|        MULTI_TIMEEVOLUTION           engine/source/multifluid/multi_timeevolution.F
!hd|        MULTI_UPDATE_GLOBAL           engine/source/multifluid/multi_update_global.F
!hd|        MULTI_UPDATE_PARTIAL          engine/source/multifluid/multi_update_partial.F
!hd|        MULTI_VAR2BUF                 engine/source/multifluid/multi_var2buf.F
!hd|        MULTI_VELOCITY_BACKUP         engine/source/multifluid/multi_velocity_backup.F
!hd|        NODALSSP                      engine/source/output/anim/generate/nodalssp.F
!hd|        RADIOSS2                      engine/source/engine/radioss2.F
!hd|        RDCOMI                        engine/source/output/restart/rdcomm.F
!hd|        RDRESA                        engine/source/output/restart/rdresa.F
!hd|        RDRESB                        engine/source/output/restart/rdresb.F
!hd|        RESOL                         engine/source/engine/resol.F
!hd|        RESOL_HEAD                    engine/source/engine/resol_head.F
!hd|        RESTALLOC                     engine/source/output/restart/arralloc.F
!hd|        SCHLIEREN_BUFFER_GATHERING    engine/source/output/anim/generate/schlieren_buffer_gathering.F
!hd|        SORTIE_MAIN                   engine/source/output/sortie_main.F
!hd|        SPMD_CELL_EXCHANGE            engine/source/mpi/generic/spmd_cell_exchange.F
!hd|        SPMD_COLLECT_MULTI_FVM        engine/source/mpi/output/spmd_collect_multi_fvm.F
!hd|        SPMD_EXCH_INTER_18            engine/source/mpi/interfaces/spmd_exch_inter_18.F
!hd|        SPMD_FIADD25E_POFF            engine/source/mpi/interfaces/spmd_fiadd25e_poff.F
!hd|        SPMD_FIADD_POFF               engine/source/mpi/interfaces/spmd_i7tool.F
!hd|        SPMD_I7FCOM_POFF              engine/source/mpi/forces/spmd_i7fcom_poff.F
!hd|        SPMD_I7FCOM_PON               engine/source/mpi/forces/spmd_i7fcom_pon.F
!hd|        SPMD_I7XVCOM2                 engine/source/mpi/interfaces/spmd_i7xvcom2.F
!hd|        SPMD_INT18_LAW151_PON         engine/source/mpi/forces/spmd_int18_law151_pon.F
!hd|        SPMD_TRI18_151VOX             engine/source/mpi/interfaces/spmd_int.F
!hd|        SPMD_TRI7GAT                  engine/source/mpi/interfaces/spmd_int.F
!hd|        STAT_INIMAP1D_FILE_SPMD       engine/source/output/sta/stat_inimap1d_file_spmd.F
!hd|        STAT_INIMAP1D_SPMD            engine/source/output/sta/stat_inimap1d_spmd.F
!hd|        STAT_INIMAP2D_FILE_SPMD       engine/source/output/sta/stat_inimap2d_file_spmd.F
!hd|        STAT_INIMAP2D_SPMD            engine/source/output/sta/stat_inimap2d_spmd.F
!hd|        THQUAD                        engine/source/output/th/thquad.F
!hd|        THSOL                         engine/source/output/th/thsol.F
!hd|        WRCOMI                        engine/source/output/restart/wrcomm.F
!hd|        WRRESTP                       engine/source/output/restart/wrrestp.F
!hd|        MULTI_MUSCL_FLUXES_COMPUTATIONengine/source/multifluid/multi_muscl_fluxes_computation.F
!hd|-- calls ---------------
!hd|        ELBUFDEF_MOD                  modules/mat_elem/elbufdef_mod.F
!hd|====================================================================
      MODULE MULTI_FVM_MOD
        USE ELBUFDEF_MOD
#include "my_real.inc"

        ! --------------------------------------------------
        ! /INT18 + LAW 151
        ! ----------------
        ! force accumulation (dt x fx) for remote nodes
        ! size : parith/on  : 3,6,NSN*NTHREADS
        !        parith/off : 0
        TYPE REMOTE_MULTI_FVM
          INTEGER :: NODFI
          REAL(kind=8), DIMENSION(:,:,:), ALLOCATABLE :: R_FORCE_INT
        END TYPE REMOTE_MULTI_FVM
        ! --------------------------------------------------

        TYPE FACE_DATA_STRUCT
          my_real, DIMENSION(:, :), ALLOCATABLE :: SURF
          my_real, DIMENSION(:, :, :), ALLOCATABLE :: NORMAL, WFAC
          my_real, DIMENSION(:, :, :), ALLOCATABLE :: CENTROID
        END TYPE FACE_DATA_STRUCT

        TYPE ELEM_DATA_STRUCT
          my_real, DIMENSION(:, :), ALLOCATABLE :: CENTROID
        END TYPE ELEM_DATA_STRUCT

        TYPE FVM_INLET_DATA_STRUCT
          INTEGER :: FORMULATION
          INTEGER :: VECTOR_VELOCITY
          INTEGER, DIMENSION(3) :: FUNC_VEL
          INTEGER, DIMENSION(21) :: FUNC_ALPHA,FUNC_RHO,FUNC_PRES
          my_real, DIMENSION(3) :: VAL_VEL
          my_real, DIMENSION(21) :: VAL_ALPHA,VAL_RHO,VAL_PRES
        END TYPE FVM_INLET_DATA_STRUCT

        TYPE FVM_CONNECTIVITY_STRUCT
          INTEGER, DIMENSION(:), ALLOCATABLE :: KVOIS
        END TYPE FVM_CONNECTIVITY_STRUCT

        TYPE MULTI_FVM_STRUCT
          TYPE(FACE_DATA_STRUCT) :: FACE_DATA
          TYPE(ELEM_DATA_STRUCT) :: ELEM_DATA
          INTEGER :: IEBCSLGTH
          INTEGER :: NBMAT
          LOGICAL :: IS_USED                  !card /MAT/LAW151 used in input file
          LOGICAL :: IS_ASSOCIATED_TO_A_PART  !card /MAT/LAW151 can be used in input file but not necessarily associated to a given PART
          LOGICAL :: ARE_ALL_PARTS_151        !check if all PART are associated with law151 (otherwise mixed scheme)
          LOGICAL :: IS_RESTART
          INTEGER :: MUSCL
!     Compression coefficient for phase advection
          my_real :: BETA
          my_real :: PRES_SHIFT
          INTEGER :: NELEM
          INTEGER, DIMENSION(:, :), ALLOCATABLE :: N4_VOIS
          my_real, DIMENSION(:, :, :), ALLOCATABLE :: FLUXES, SUBVOL_FLUXES, SUBMASS_FLUXES, SUBENER_FLUXES
          my_real, DIMENSION(:, :), ALLOCATABLE :: VEL
          my_real, DIMENSION(:, :), ALLOCATABLE :: ACC
          my_real, DIMENSION(:), ALLOCATABLE :: SOUND_SPEED
          my_real, DIMENSION(:), ALLOCATABLE :: RHO, EINT, PRES
          my_real, DIMENSION(:), ALLOCATABLE :: TBURN, VOL
          my_real, DIMENSION(:, :), ALLOCATABLE :: BFRAC
          INTEGER, POINTER, DIMENSION(:) :: pCNEL, pADDCNEL, pADDTMPL

          ! Indicates whether we run in 3D (SYM = 0), or 2D (SYM = 1 planar case, SYM = 2 cylindrical case)
          INTEGER :: SYM
          ! Low mach options for WATER / AIR applications
          LOGICAL :: LOWMACH_OPT
          ! MUSCL Variables
          my_real, DIMENSION(:, :), ALLOCATABLE :: GRAD_RHO, GRAD_U, GRAD_V, GRAD_W, GRAD_PRES
          my_real, DIMENSION(:, :), ALLOCATABLE :: PHASE_ALPHA, PHASE_PRES, PHASE_RHO, PHASE_EINT
          my_real, DIMENSION(:, :, :), ALLOCATABLE :: PHASE_GRAD_RHO, PHASE_GRAD_ALPHA, PHASE_GRAD_PRES
          ! EBCS
          TYPE(FVM_CONNECTIVITY_STRUCT) :: FVM_CONNECTIVITY
          ! --------------------------------------------------
          ! /INT18 + LAW 151
          ! ----------------
          ! IS_INT18_LAW151 : boolean, true if /INT18 + LAW 151
          ! NUMBER_INT18 : integer, number of interface /INT18
          ! INT18_LIST : integer, dimension = NUMBER_INT18, list of interface 18
          ! INT18_GLOBAL_LIST : boolean, dimension=NINTER, true if /INT18 + LAW151 for the NIN interface
          !                     used for the mpi comm spmd_i7fcom_poff/pon
          ! S_APPEND_ARRAY : integer, size of X_APPEND,V_APPEND,MASS_APPEND and KINET_APPEND arrays
          LOGICAL :: IS_INT18_LAW151
          INTEGER :: NUMBER_INT18
          INTEGER, DIMENSION(:), ALLOCATABLE :: INT18_LIST
          LOGICAL, DIMENSION(:), ALLOCATABLE :: INT18_GLOBAL_LIST

          INTEGER :: S_APPEND_ARRAY ! size of X_APPEND,V_APPEND,MASS_APPEND and KINET_APPEND arrays
          ! extended position/velocity/mass/kinet for /INT18 + LAW 151
          ! size : NUMNOD + NUMELS
          my_real, DIMENSION(:), ALLOCATABLE :: X_APPEND
          my_real, DIMENSION(:), ALLOCATABLE :: V_APPEND
          my_real, DIMENSION(:), ALLOCATABLE :: MASS_APPEND
          INTEGER, DIMENSION(:), ALLOCATABLE :: KINET_APPEND
          ! force accumulation (dt x fx)
          ! size : parith/on  : 3,0
          !        parith/off : 3,NUMELS*NTHREADS
          INTEGER :: SIZE_FORCE_INT_1   ! --> 1srt dimension = 3
          INTEGER :: SIZE_FORCE_INT_2   ! --> 2ns dimension  = NUMELS*NTHREADS or 0
          my_real, DIMENSION(:,:), ALLOCATABLE :: FORCE_INT

          INTEGER :: SIZE_FORCE_INT_PON   ! --> 3rd dimension = NUMELSxNTHREADS (parith/on) or 0 (parith/off)
          REAL(kind=8), DIMENSION(:,:,:), ALLOCATABLE :: FORCE_INT_PON

          ! force accumulation (dt x fx) for remote nodes
          ! size : parith/on  : number of /TYPE18+/LAW151
          !        parith/off : 0
          TYPE(REMOTE_MULTI_FVM), DIMENSION(:), ALLOCATABLE :: R_AFI
          ! --------------------------------------------------
          ! Navier-Stokes diffusion activated with law 6
          LOGICAL :: NS_DIFF
        END TYPE MULTI_FVM_STRUCT

        TYPE LBUF_PTR
          TYPE(L_BUFEL_), POINTER :: LBUF
        END TYPE LBUF_PTR

        TYPE EBUF_PTR
          TYPE(BUF_EOS_), POINTER :: EBUF
        END TYPE EBUF_PTR

        TYPE FVM_INIVEL_STRUCT
          LOGICAL :: FLAG
          INTEGER :: GRBRICID, GRQUADID, GRSH3NID
          my_real :: VX, VY, VZ
        END TYPE FVM_INIVEL_STRUCT



      CONTAINS


      END MODULE MULTI_FVM_MOD
