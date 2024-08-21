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
      !||    multi_fvm_mod                    ../common_source/modules/ale/multi_fvm_mod.F
      !||--- called by ------------------------------------------------------
      !||    alelec                           ../starter/source/ale/alelec.F
      !||    alemain                          ../engine/source/ale/alemain.F
      !||    alewdx                           ../engine/source/ale/grid/alewdx.F
      !||    anim_nodal_ssp_elems             ../engine/source/output/anim/generate/anim_nodal_ssp_elems.F90
      !||    build_connectivity               ../engine/source/multifluid/connectivity.F
      !||    c3grhead                         ../starter/source/elements/sh3n/coque3n/c3grhead.F
      !||    c_ixfloc                         ../starter/source/restart/ddsplit/c_ixfloc.F
      !||    c_multi_vel                      ../starter/source/restart/ddsplit/c_multi_vel.F
      !||    c_vois                           ../starter/source/restart/ddsplit/c_vois.F
      !||    contrl                           ../starter/source/starter/contrl.F
      !||    ddsplit                          ../starter/source/restart/ddsplit/ddsplit.F
      !||    dfunc0                           ../engine/source/output/anim/generate/dfunc0.F
      !||    dfuncc                           ../engine/source/output/anim/generate/dfuncc.F
      !||    dfuncs                           ../engine/source/output/anim/generate/dfunc6.F
      !||    ebcs10                           ../engine/source/boundary_conditions/ebcs/ebcs10.F
      !||    ebcs_main                        ../engine/source/boundary_conditions/ebcs/ebcs_main.F
      !||    ebcs_mod                         ../common_source/modules/boundary_conditions/ebcs_mod.F
      !||    ecrit                            ../engine/source/output/ecrit.F
      !||    eig                              ../engine/stub/eig.F
      !||    eig1                             ../engine/stub/eig1.F
      !||    eigcond                          ../engine/stub/eigcond.F
      !||    eigp                             ../engine/stub/eigp.F
      !||    funct_python_update_elements     ../engine/source/tools/curve/funct_python_update_elements.F90
      !||    genani                           ../engine/source/output/anim/generate/genani.F
      !||    genh3d                           ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    genstat                          ../engine/source/output/sta/genstat.F
      !||    h3d_gene_keyword                 ../engine/source/output/h3d/input_list/h3d_gene_keyword.F
      !||    h3d_list_quad_scalar             ../engine/source/output/h3d/input_list/h3d_list_quad_scalar.F
      !||    h3d_list_shell_scalar            ../engine/source/output/h3d/input_list/h3d_list_shell_scalar.F
      !||    h3d_list_solid_scalar            ../engine/source/output/h3d/input_list/h3d_list_solid_scalar.F
      !||    h3d_nodal_scalar                 ../engine/source/output/h3d/h3d_results/h3d_nodal_scalar.F
      !||    h3d_quad_scalar                  ../engine/source/output/h3d/h3d_results/h3d_quad_scalar.F
      !||    h3d_quad_scalar_1                ../engine/source/output/h3d/h3d_results/h3d_quad_scalar_1.F90
      !||    h3d_quad_vector                  ../engine/source/output/h3d/h3d_results/h3d_quad_vector.F
      !||    h3d_shell_scalar                 ../engine/source/output/h3d/h3d_results/h3d_shell_scalar.F
      !||    h3d_shell_scalar_1               ../engine/source/output/h3d/h3d_results/h3d_shell_scalar_1.F
      !||    h3d_shell_vector                 ../engine/source/output/h3d/h3d_results/h3d_shell_vector.F
      !||    h3d_shell_vector_1               ../engine/source/output/h3d/h3d_results/h3d_shell_vector_1.F
      !||    h3d_skin_scalar                  ../engine/source/output/h3d/h3d_results/h3d_skin_scalar.F
      !||    h3d_solid_scalar                 ../engine/source/output/h3d/h3d_results/h3d_solid_scalar.F
      !||    h3d_solid_scalar_1               ../engine/source/output/h3d/h3d_results/h3d_solid_scalar_1.F
      !||    h3d_solid_vector                 ../engine/source/output/h3d/h3d_results/h3d_solid_vector.F
      !||    hist2                            ../engine/source/output/th/hist2.F
      !||    hm_read_bcs_wall                 ../starter/source/boundary_conditions/hm_read_bcs_wall.F90
      !||    hm_read_ebcs_fluxout             ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_fluxout.F
      !||    hm_read_ebcs_gradp0              ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_gradp0.F
      !||    hm_read_ebcs_inip                ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_inip.F
      !||    hm_read_ebcs_iniv                ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_iniv.F
      !||    hm_read_ebcs_inlet               ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_inlet.F
      !||    hm_read_ebcs_monvol              ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_monvol.F
      !||    hm_read_ebcs_normv               ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_normv.F
      !||    hm_read_ebcs_nrf                 ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_nrf.F
      !||    hm_read_ebcs_pres                ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_pres.F
      !||    hm_read_ebcs_valvin              ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_valvin.F
      !||    hm_read_ebcs_valvout             ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_valvout.F
      !||    hm_read_ebcs_vel                 ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_vel.F
      !||    hm_read_inimap1d                 ../starter/source/initial_conditions/inimap/hm_read_inimap1d.F
      !||    hm_read_inivel                   ../starter/source/initial_conditions/general/inivel/hm_read_inivel.F
      !||    hm_read_inivol                   ../starter/source/initial_conditions/inivol/hm_read_inivol.F
      !||    hm_read_inter_fsi                ../starter/source/interfaces/reader/hm_read_inter_fsi.F
      !||    hm_read_inter_type18             ../starter/source/interfaces/int18/hm_read_inter_type18.F
      !||    hm_read_interfaces               ../starter/source/interfaces/reader/hm_read_interfaces.F
      !||    hm_read_mat                      ../starter/source/materials/mat/hm_read_mat.F
      !||    hm_read_mat151                   ../starter/source/materials/mat/mat151/hm_read_mat151.F
      !||    hm_read_part                     ../starter/source/model/assembling/hm_read_part.F
      !||    hm_read_prop01                   ../starter/source/properties/shell/hm_read_prop01.F
      !||    hm_read_prop06                   ../starter/source/properties/solid/hm_read_prop06.F
      !||    hm_read_prop14                   ../starter/source/properties/solid/hm_read_prop14.F
      !||    hm_read_prop14f                  ../starter/source/properties/solid/hm_read_prop14.F
      !||    hm_read_prop20                   ../starter/source/properties/thickshell/hm_read_prop20.F
      !||    hm_read_prop22                   ../starter/source/properties/thickshell/hm_read_prop22.F
      !||    hm_read_properties               ../starter/source/properties/hm_read_properties.F
      !||    i18dst3                          ../engine/source/interfaces/int18/i18dst3.F
      !||    i18for3                          ../engine/source/interfaces/int18/i18for3.F
      !||    i18main_kine_1                   ../engine/source/interfaces/int18/i18main_kine.F
      !||    i18main_kine_i                   ../engine/source/interfaces/int18/i18main_kine.F
      !||    i22main_tri                      ../engine/source/interfaces/intsort/i22main_tri.F
      !||    i23main_tri                      ../engine/source/interfaces/intsort/i23main_tri.F
      !||    i7main_tri                       ../engine/source/interfaces/intsort/i7main_tri.F
      !||    i7mainf                          ../engine/source/interfaces/int07/i7mainf.F
      !||    imp_buck                         ../engine/source/implicit/imp_buck.F
      !||    imp_inttd0                       ../engine/source/implicit/imp_int_k.F
      !||    imp_solv                         ../engine/source/implicit/imp_solv.F
      !||    imp_tripi                        ../engine/source/implicit/imp_int_k.F
      !||    ingrbric_dx                      ../starter/source/interfaces/interf1/ingrbric_dx.F
      !||    ini_fvminivel                    ../starter/source/elements/initia/ini_fvminivel.F
      !||    ini_inimap1d                     ../starter/source/initial_conditions/inimap/ini_inimap1d.F
      !||    ini_inimap2d                     ../starter/stub/ini_inimap2d.F
      !||    inifill                          ../starter/source/initial_conditions/inivol/inifill.F
      !||    inigrav_load                     ../starter/source/initial_conditions/inigrav/inigrav_load.F
      !||    inintr                           ../starter/source/interfaces/interf1/inintr.F
      !||    iniphase                         ../starter/source/initial_conditions/inivol/iniphase.F
      !||    init_bcs_wall                    ../starter/source/boundary_conditions/init_bcs_wall.F90
      !||    init_inivol                      ../starter/source/initial_conditions/inivol/init_inivol.F90
      !||    init_inivol_2d_polygons          ../starter/source/initial_conditions/inivol/init_inivol_2D_polygons.F90
      !||    initia                           ../starter/source/elements/initia/initia.F
      !||    inivel                           ../starter/source/initial_conditions/general/inivel/inivel.F
      !||    inivol_interf_mod                ../starter/share/modules1/inivol_mod.F
      !||    inivol_set                       ../starter/source/initial_conditions/inivol/inivol_set.F
      !||    int18_alloc                      ../engine/source/interfaces/int18/int18_alloc.F
      !||    int18_law151_alloc               ../starter/source/interfaces/int18/int18_law151_alloc.F
      !||    int18_law151_init                ../engine/source/interfaces/int18/int18_law151_init.F
      !||    int18_law151_nsv_shift           ../engine/source/interfaces/int18/int18_law151_nsv_shift.F
      !||    int18_law151_omp_accumulation    ../engine/source/interfaces/int18/int18_law151_omp_accumulation.F
      !||    int18_law151_update              ../engine/source/interfaces/int18/int18_law151_update.F
      !||    inter_color_voxel                ../engine/source/interfaces/generic/inter_color_voxel.F
      !||    inter_deallocate_wait            ../engine/source/interfaces/generic/inter_deallocate_wait.F
      !||    inter_sort                       ../engine/source/interfaces/generic/inter_sort.F
      !||    inter_sort_07                    ../engine/source/interfaces/int07/inter_sort_07.F
      !||    intfop2                          ../engine/source/interfaces/interf/intfop2.F
      !||    inttri                           ../engine/source/interfaces/intsort/inttri.F
      !||    ipari_l_ini                      ../starter/source/restart/ddsplit/ipari_l_ini.F
      !||    lec_inimap1d_file                ../starter/source/initial_conditions/inimap/lec_inimap1d_file.F
      !||    lech3d                           ../engine/source/output/h3d/h3d_build_fortran/lech3d.F
      !||    lecins                           ../starter/source/interfaces/interf1/lecins.F
      !||    lecint                           ../starter/source/interfaces/interf1/lecint.F
      !||    lectur                           ../engine/source/input/lectur.F
      !||    multi_allocate                   ../engine/source/multifluid/multi_allocate.F
      !||    multi_bilan                      ../engine/source/multifluid/multi_bilan.F
      !||    multi_buf2var                    ../engine/source/multifluid/multi_buf2var.F
      !||    multi_check_eos                  ../starter/source/multifluid/multi_check_eos.F
      !||    multi_check_psh                  ../starter/source/multifluid/multi_check_psh.F
      !||    multi_compute_dt                 ../engine/source/multifluid/multi_compute_dt.F
      !||    multi_connectivity               ../starter/source/multifluid/multi_connectivity.F
      !||    multi_deallocate                 ../engine/source/multifluid/multi_deallocate.F
      !||    multi_ebcs                       ../engine/source/multifluid/multi_ebcs.F
      !||    multi_evolve_global              ../engine/source/multifluid/multi_evolve_global.F
      !||    multi_evolve_partial             ../engine/source/multifluid/multi_evolve_partial.F
      !||    multi_face_elem_data             ../engine/source/multifluid/multi_face_data_elem.F
      !||    multi_fluxes_computation         ../engine/source/multifluid/multi_fluxes_computation.F
      !||    multi_fluxout_ebcs               ../engine/source/multifluid/multi_fluxout_ebcs.F
      !||    multi_fvm2fem                    ../engine/source/multifluid/multi_fvm2fem.F
      !||    multi_globalize                  ../engine/source/multifluid/multi_globalize.F
      !||    multi_i18_force_poff             ../engine/source/interfaces/int18/multi_i18_force_poff.F
      !||    multi_i18_force_pon              ../engine/source/interfaces/int18/multi_i18_force_pon.F
      !||    multi_inlet_ebcs                 ../engine/source/multifluid/multi_inlet_ebcs.F
      !||    multi_muscl_fluxes_computation   ../engine/source/multifluid/multi_muscl_fluxes_computation.F
      !||    multi_muscl_gradients            ../engine/source/multifluid/multi_muscl_gradients.F
      !||    multi_nrf_ebcs                   ../engine/source/multifluid/multi_nrf_ebcs.F
      !||    multi_pressure_equilibrium       ../engine/source/multifluid/multi_pressure_equilibrium.F
      !||    multi_timeevolution              ../engine/source/multifluid/multi_timeevolution.F
      !||    multi_update_global              ../engine/source/multifluid/multi_update_global.F
      !||    multi_update_partial             ../engine/source/multifluid/multi_update_partial.F
      !||    multi_var2buf                    ../engine/source/multifluid/multi_var2buf.F
      !||    multi_velocity_backup            ../engine/source/multifluid/multi_velocity_backup.F
      !||    multifluid_global_tdet           ../starter/source/multifluid/multifluid_global_tdet.F
      !||    multifluid_init2t                ../starter/source/multifluid/multifluid_init2t.F
      !||    multifluid_init3                 ../starter/source/multifluid/multifluid_init3.F
      !||    nodalssp                         ../engine/source/output/anim/generate/nodalssp.F
      !||    ns_fvm_diffusion                 ../engine/source/multifluid/ns_fvm_diffusion.F
      !||    prepare_split_i7                 ../starter/source/restart/ddsplit/inter_tools.F
      !||    radioss2                         ../engine/source/engine/radioss2.F
      !||    rdcomi                           ../engine/source/output/restart/rdcomm.F
      !||    rdresa                           ../engine/source/output/restart/rdresa.F
      !||    rdresb                           ../engine/source/output/restart/rdresb.F
      !||    read_ebcs                        ../starter/source/boundary_conditions/ebcs/read_ebcs.F
      !||    read_material_models             ../starter/source/materials/read_material_models.F
      !||    resol                            ../engine/source/engine/resol.F
      !||    resol_head                       ../engine/source/engine/resol_head.F
      !||    restalloc                        ../engine/source/output/restart/arralloc.F
      !||    schlieren_buffer_gathering       ../engine/source/output/anim/generate/schlieren_buffer_gathering.F
      !||    sortie_main                      ../engine/source/output/sortie_main.F
      !||    split_ebcs                       ../starter/source/boundary_conditions/ebcs/split_ebcs.F
      !||    split_interfaces                 ../starter/source/restart/ddsplit/split_interfaces.F
      !||    spmd_cell_exchange               ../engine/source/mpi/generic/spmd_cell_exchange.F
      !||    spmd_collect_multi_fvm           ../engine/source/mpi/output/spmd_collect_multi_fvm.F
      !||    spmd_exch_inter_18               ../engine/source/mpi/interfaces/spmd_exch_inter_18.F
      !||    spmd_fiadd25e_poff               ../engine/source/mpi/interfaces/spmd_fiadd25e_poff.F
      !||    spmd_fiadd_poff                  ../engine/source/mpi/interfaces/spmd_i7tool.F
      !||    spmd_i7fcom_poff                 ../engine/source/mpi/forces/spmd_i7fcom_poff.F
      !||    spmd_i7fcom_pon                  ../engine/source/mpi/forces/spmd_i7fcom_pon.F
      !||    spmd_i7xvcom2                    ../engine/source/mpi/interfaces/spmd_i7xvcom2.F
      !||    spmd_int18_law151_pon            ../engine/source/mpi/forces/spmd_int18_law151_pon.F
      !||    spmd_tri18_151vox                ../engine/source/mpi/interfaces/spmd_int.F
      !||    spmd_tri7gat                     ../engine/source/mpi/interfaces/spmd_int.F
      !||    st_qaprint_driver                ../starter/source/output/qaprint/st_qaprint_driver.F
      !||    st_qaprint_initial_conditions    ../starter/source/output/qaprint/st_qaprint_initial_conditions.F
      !||    starter0                         ../starter/source/starter/starter0.F
      !||    stat_inimap1d_file_spmd          ../engine/source/output/sta/stat_inimap1d_file_spmd.F
      !||    stat_inimap1d_spmd               ../engine/source/output/sta/stat_inimap1d_spmd.F
      !||    stat_inimap2d_file_spmd          ../engine/source/output/sta/stat_inimap2d_file_spmd.F
      !||    stat_inimap2d_spmd               ../engine/source/output/sta/stat_inimap2d_spmd.F
      !||    t3grhead                         ../starter/source/elements/solid_2d/tria/t3grhead.F
      !||    thquad                           ../engine/source/output/th/thquad.F
      !||    thsol                            ../engine/source/output/th/thsol.F
      !||    w_fi                             ../starter/source/restart/ddsplit/w_fi.F
      !||    w_front                          ../starter/source/restart/ddsplit/w_front.F
      !||    wrcomi                           ../engine/source/output/restart/wrcomm.F
      !||    wrcomip                          ../starter/source/restart/ddsplit/wrcommp.F
      !||    wrrestp                          ../engine/source/output/restart/wrrestp.F
      !||--- uses       -----------------------------------------------------
      !||    elbufdef_mod                     ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||====================================================================
      MODULE MULTI_FVM_MOD
      USE ELBUFDEF_MOD ,ONLY: L_BUFEL_,BUF_EOS_
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
C     Compression coefficient for phase advection
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
