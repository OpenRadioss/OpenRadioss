!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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
      !||====================================================================
      !||    multi_fvm_mod                    ../common_source/modules/ale/multi_fvm_mod.F90
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
      !||    ebcs11                           ../engine/source/boundary_conditions/ebcs/ebcs11.F90
      !||    ebcs_main                        ../engine/source/boundary_conditions/ebcs/ebcs_main.F
      !||    ebcs_mod                         ../common_source/modules/boundary_conditions/ebcs_mod.F90
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
      !||    hm_read_ebcs_propergol           ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_propergol.F90
      !||    hm_read_ebcs_valvin              ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_valvin.F
      !||    hm_read_ebcs_valvout             ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_valvout.F
      !||    hm_read_ebcs_vel                 ../starter/source/boundary_conditions/ebcs/hm_read_ebcs_vel.F
      !||    hm_read_inimap1d                 ../starter/source/initial_conditions/inimap/hm_read_inimap1d.F
      !||    hm_read_inivel                   ../starter/source/initial_conditions/general/inivel/hm_read_inivel.F
      !||    hm_read_inivol                   ../starter/source/initial_conditions/inivol/hm_read_inivol.F90
      !||    hm_read_inter_fsi                ../starter/source/interfaces/reader/hm_read_inter_fsi.F
      !||    hm_read_inter_type18             ../starter/source/interfaces/int18/hm_read_inter_type18.F
      !||    hm_read_interfaces               ../starter/source/interfaces/reader/hm_read_interfaces.F
      !||    hm_read_mat                      ../starter/source/materials/mat/hm_read_mat.F90
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
      !||    inivel_start                     ../engine/source/loads/general/inivel/inivel_start.F90
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
      !||    multi_propergol_ebcs             ../engine/source/multifluid/multi_propergol_ebcs.F90
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
      !||    prelech3d                        ../engine/source/output/h3d/h3d_build_fortran/prelech3d.F90
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
      module multi_fvm_mod
        use elbufdef_mod ,only: l_bufel_,buf_eos_
#include "my_real.inc"

        ! --------------------------------------------------
        ! /INT18 + LAW 151
        ! ----------------
        ! force accumulation (dt x fx) for remote nodes
        ! size : parith/on  : 3,6,NSN*NTHREADS
        !        parith/off : 0
        type remote_multi_fvm
          integer :: nodfi
          real(kind=8), dimension(:,:,:), allocatable :: r_force_int
        end type remote_multi_fvm
        ! --------------------------------------------------

        type face_data_struct
          my_real, dimension(:, :), allocatable :: surf
          my_real, dimension(:, :, :), allocatable :: normal, wfac
          my_real, dimension(:, :, :), allocatable :: centroid
        end type face_data_struct

        type elem_data_struct
          my_real, dimension(:, :), allocatable :: centroid
        end type elem_data_struct

        type fvm_inlet_data_struct
          integer :: formulation
          integer :: vector_velocity
          integer, dimension(3) :: func_vel
          integer, dimension(21) :: func_alpha,func_rho,func_pres
          my_real, dimension(3) :: val_vel
          my_real, dimension(21) :: val_alpha,val_rho,val_pres
        end type fvm_inlet_data_struct

        type fvm_connectivity_struct
          integer, dimension(:), allocatable :: kvois
        end type fvm_connectivity_struct

        type multi_fvm_struct
          type(face_data_struct) :: face_data
          type(elem_data_struct) :: elem_data
          integer :: iebcslgth
          integer :: nbmat
          logical :: is_used                  !< card /mat/law151 used in input file
          logical :: is_associated_to_a_part  !< card /mat/law151 can be used in input file but not necessarily associated to a given part
          logical :: are_all_parts_151        !< check if all part are associated with law151 (otherwise mixed scheme)
          logical :: is_restart
          integer :: muscl
!     compression coefficient for phase advection
          my_real :: beta
          my_real :: pres_shift
          integer :: nelem
          integer, dimension(:, :), allocatable :: n4_vois
          my_real, dimension(:, :, :), allocatable :: fluxes, subvol_fluxes, submass_fluxes, subener_fluxes
          my_real, dimension(:, :), allocatable :: vel
          my_real, dimension(:, :), allocatable :: acc
          my_real, dimension(:), allocatable :: sound_speed
          my_real, dimension(:), allocatable :: rho, eint, pres
          my_real, dimension(:), allocatable :: tburn, vol
          my_real, dimension(:, :), allocatable :: bfrac
          integer, pointer, dimension(:) :: pcnel, paddcnel, paddtmpl

          ! indicates whether we run in 3d (sym = 0), or 2d (sym = 1 planar case, sym = 2 cylindrical case)
          integer :: sym
          ! low mach options for water / air applications
          logical :: lowmach_opt
          ! muscl variables
          my_real, dimension(:, :), allocatable :: grad_rho, grad_u, grad_v, grad_w, grad_pres
          my_real, dimension(:, :), allocatable :: phase_alpha, phase_pres, phase_rho, phase_eint
          my_real, dimension(:, :, :), allocatable :: phase_grad_rho, phase_grad_alpha, phase_grad_pres
          ! ebcs
          type(fvm_connectivity_struct) :: fvm_connectivity
          ! --------------------------------------------------
          ! /int18 + law 151
          ! ----------------
          ! is_int18_law151 : boolean, true if /int18 + law 151
          ! number_int18 : integer, number of interface /int18
          ! int18_list : integer, dimension = number_int18, list of interface 18
          ! int18_global_list : boolean, dimension=ninter, true if /int18 + law151 for the nin interface
          !                     used for the mpi comm spmd_i7fcom_poff/pon
          ! s_append_array : integer, size of x_append,v_append,mass_append and kinet_append arrays
          logical :: is_int18_law151
          integer :: number_int18
          integer, dimension(:), allocatable :: int18_list
          logical, dimension(:), allocatable :: int18_global_list

          integer :: s_append_array ! size of x_append,v_append,mass_append and kinet_append arrays
          ! extended position/velocity/mass/kinet for /int18 + law 151
          ! size : numnod + numels
          my_real, dimension(:), allocatable :: x_append
          my_real, dimension(:), allocatable :: v_append
          my_real, dimension(:), allocatable :: mass_append
          integer, dimension(:), allocatable :: kinet_append
          ! force accumulation (dt x fx)
          ! size : parith/on  : 3,0
          !        parith/off : 3,numels*nthreads
          integer :: size_force_int_1   ! --> 1srt dimension = 3
          integer :: size_force_int_2   ! --> 2ns dimension  = numels*nthreads or 0
          my_real, dimension(:,:), allocatable :: force_int

          integer :: size_force_int_pon   ! --> 3rd dimension = numelsxnthreads (parith/on) or 0 (parith/off)
          real(kind=8), dimension(:,:,:), allocatable :: force_int_pon

          ! force accumulation (dt x fx) for remote nodes
          ! size : parith/on  : number of /type18+/law151
          !        parith/off : 0
          type(remote_multi_fvm), dimension(:), allocatable :: r_afi
          ! --------------------------------------------------
          ! navier-stokes diffusion activated with law 6
          logical :: ns_diff
        end type multi_fvm_struct

        type lbuf_ptr
          type(l_bufel_), pointer :: lbuf
        end type lbuf_ptr

        type ebuf_ptr
          type(buf_eos_), pointer :: ebuf
        end type ebuf_ptr

        type fvm_inivel_struct
          logical :: flag
          integer :: grbricid, grquadid, grsh3nid
          my_real :: vx, vy, vz
        end type fvm_inivel_struct



      contains


      end module multi_fvm_mod
