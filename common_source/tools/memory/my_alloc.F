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
      !||    my_alloc_mod                       ../common_source/tools/memory/my_alloc.F
      !||--- called by ------------------------------------------------------
      !||    add_mass_stat                      ../starter/source/tools/admas/add_mass_stat.F
      !||    allocbuf_auto                      ../engine/source/elements/elbuf/allocbuf_auto.F
      !||    c3grhead                           ../starter/source/elements/sh3n/coque3n/c3grhead.F
      !||    c3grtails                          ../starter/source/elements/sh3n/coque3n/c3grtails.F
      !||    c3init3                            ../starter/source/elements/sh3n/coque3n/c3init3.F
      !||    cbainit3                           ../starter/source/elements/shell/coqueba/cbainit3.F
      !||    cgrhead                            ../starter/source/elements/shell/coque/cgrhead.F
      !||    cgrtails                           ../starter/source/elements/shell/coque/cgrtails.F
      !||    create_ellipse_clause              ../starter/source/model/sets/create_ellipse_clause.F
      !||    create_line_from_element           ../starter/source/model/sets/create_line_from_element.F
      !||    create_line_from_surface_all       ../starter/source/model/sets/create_line_from_surface_all.F
      !||    create_line_from_surface_ext       ../starter/source/model/sets/create_line_from_surface_ext.F
      !||    create_line_from_surface_ext_all   ../starter/source/model/sets/create_line_from_ext_surface_ext_all.F
      !||    create_seatbelt                    ../starter/source/tools/seatbelts/create_seatbelt.F
      !||    create_surface_from_element        ../starter/source/model/sets/create_surface_from_element.F
      !||    elbuf_ini                          ../engine/source/elements/elbuf/elbuf_ini.F
      !||    failwave_init                      ../starter/source/materials/fail/failwave_init.F
      !||    fill_gr                            ../starter/source/model/sets/fill_gr.F
      !||    fill_line                          ../starter/source/model/sets/fill_gr.F
      !||    fill_surf                          ../starter/source/model/sets/fill_gr.F
      !||    fill_surf_ellipse                  ../starter/source/model/sets/fill_gr_surf_ellipse.F
      !||    find_dt_engine                     ../starter/source/coupling/rad2rad/r2r_speedup.F
      !||    friction_parts_search              ../starter/source/interfaces/inter3d1/i7sti3.F
      !||    genh3d                             ../engine/source/output/h3d/h3d_results/genh3d.F
      !||    hm_grogro                          ../starter/source/groups/hm_grogro.F
      !||    hm_grogronod                       ../starter/source/groups/hm_grogronod.F
      !||    hm_lecgre                          ../starter/source/groups/hm_lecgre.F
      !||    hm_lecgrn                          ../starter/source/groups/hm_lecgrn.F
      !||    hm_lines_of_lines                  ../starter/source/groups/hm_lines_of_lines.F
      !||    hm_prelecgrns                      ../starter/source/groups/hm_prelecgrns.F
      !||    hm_preread_rbody                   ../starter/source/constraints/general/rbody/hm_preread_rbody.F
      !||    hm_read_admas                      ../starter/source/tools/admas/hm_read_admas.F
      !||    hm_read_box                        ../starter/source/model/box/hm_read_box.F
      !||    hm_read_grpart                     ../starter/source/groups/hm_read_grpart.F
      !||    hm_read_inicrack                   ../starter/source/initial_conditions/inicrack/hm_read_inicrack.F
      !||    hm_read_inivol                     ../starter/source/initial_conditions/inivol/hm_read_inivol.F
      !||    hm_read_lines                      ../starter/source/groups/hm_read_lines.F
      !||    hm_read_pcyl                       ../starter/source/loads/general/load_pcyl/hm_read_pcyl.F
      !||    hm_read_rbody                      ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    hm_read_retractor                  ../starter/source/tools/seatbelts/hm_read_retractor.F
      !||    hm_read_sensors                    ../starter/source/tools/sensor/hm_read_sensors.F
      !||    hm_read_slipring                   ../starter/source/tools/seatbelts/hm_read_slipring.F
      !||    hm_read_subset                     ../starter/source/model/assembling/hm_read_subset.F
      !||    hm_read_surf                       ../starter/source/groups/hm_read_surf.F
      !||    hm_read_surfsurf                   ../starter/source/groups/hm_read_surfsurf.F
      !||    hm_read_thgrou                     ../starter/source/output/th/hm_read_thgrou.F
      !||    hm_read_window_user                ../starter/source/tools/userwi/hm_read_window_user.F
      !||    hm_thvarvent                       ../starter/source/output/th/hm_thvarent.F
      !||    i24gapm                            ../starter/source/interfaces/inter3d1/i24sti3.F
      !||    i24sti3                            ../starter/source/interfaces/inter3d1/i24sti3.F
      !||    i25gapm                            ../starter/source/interfaces/inter3d1/i25sti3.F
      !||    i25neigh                           ../starter/source/interfaces/inter3d1/i25neigh.F
      !||    i25sti3                            ../starter/source/interfaces/inter3d1/i25sti3.F
      !||    i7remnode                          ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    i7sti3                             ../starter/source/interfaces/inter3d1/i7sti3.F
      !||    ini_seatbelt                       ../starter/source/tools/seatbelts/ini_seatbelt.F
      !||    init_bcs_wall                      ../starter/source/boundary_conditions/init_bcs_wall.F90
      !||    init_monvol                        ../starter/source/airbag/init_monvol.F
      !||    insert_clause_in_set               ../starter/source/model/sets/insert_clause_in_set.F
      !||    intbuf_fric_ini_starter            ../starter/source/interfaces/intbuf/intbufFric_ini_starter.F
      !||    intbuf_ini_starter                 ../starter/source/interfaces/intbuf/intbuf_ini_starter.F
      !||    lectur                             ../engine/source/input/lectur.F
      !||    monvol_check_delete_duplicated     ../starter/source/airbag/monvol_check_delete_duplicated.F
      !||    nloc_dmg_init                      ../starter/source/materials/fail/nloc_dmg_init.F
      !||    outri                              ../starter/source/materials/time_step/outri.F
      !||    outrin                             ../starter/source/materials/time_step/outri.F
      !||    pre_i2                             ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    r2r_group                          ../starter/source/coupling/rad2rad/r2r_group.F
      !||    r2r_speedup                        ../starter/source/coupling/rad2rad/r2r_speedup.F
      !||    r2r_split                          ../starter/source/coupling/rad2rad/r2r_split.F
      !||    r2r_void                           ../starter/source/coupling/rad2rad/r2r_void.F
      !||    r2r_void_1d                        ../starter/source/coupling/rad2rad/r2r_void.F
      !||    rcheckmass                         ../starter/source/elements/spring/rcheckmass.F
      !||    read_box_box                       ../starter/source/model/box/read_box_box.F
      !||    read_viscparam                     ../engine/source/output/restart/read_viscparam.F
      !||    remn_i2_edg                        ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    remn_i2_edgop                      ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    remn_i2op                          ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    remn_i2op_edg25                    ../starter/source/interfaces/int25/i25remlin.F
      !||    remn_self24                        ../starter/source/interfaces/inter3d1/remn_self24.F
      !||    restalloc                          ../engine/source/output/restart/arralloc.F
      !||    ri2_int24p_ini                     ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    rm_cand24                          ../starter/source/interfaces/inter3d1/i7remnode.F
      !||    set_user_window_nodes              ../starter/source/user_interface/user_windows_tools.F
      !||    seteloff                           ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    setrbyon                           ../starter/source/constraints/general/rbody/hm_read_rbody.F
      !||    sgrhead                            ../starter/source/elements/solid/solide/sgrhead.F
      !||    sgrtails                           ../starter/source/elements/solid/solide/sgrtails.F
      !||    spmd_userwi_rest                   ../starter/source/user_interface/user_windows_tools.F
      !||    st_qaprint_element                 ../starter/source/output/qaprint/st_qaprint_element.F
      !||    st_qaprint_reference_state         ../starter/source/output/qaprint/st_qaprint_reference_state.F
      !||    t3grhead                           ../starter/source/elements/solid_2d/tria/t3grhead.F
      !||    t3grtails                          ../starter/source/elements/solid_2d/tria/t3grtails.F
      !||    th_surf_load_pressure              ../starter/source/output/th/th_surf_load_pressure.F
      !||    userwis_front                      ../starter/source/user_interface/user_windows_tools.F
      !||    w_th_surf_loadp                    ../starter/source/restart/ddsplit/w_th_surf_loadp.F
      !||    w_th_surf_pload                    ../starter/source/restart/ddsplit/w_th_surf_pload.F
      !||====================================================================
      module my_alloc_mod
        implicit none
        integer, parameter :: len_error_message = 100
        ! lengths : integer
        private :: my_alloc_real_1d
        private :: my_alloc_real_2d
        private :: my_alloc_real_3d
        private :: my_alloc_integer_1d
        private :: my_alloc_integer_2d
        private :: my_alloc_integer_3d
        private :: my_alloc_double_1d
        private :: my_alloc_double_2d
        private :: my_alloc_double_3d
        private :: my_alloc_preal_1d
        private :: my_alloc_preal_2d
        private :: my_alloc_preal_3d
        private :: my_alloc_pinteger_1d
        private :: my_alloc_pinteger_2d
        private :: my_alloc_pinteger_3d
        private :: my_alloc_pdouble_1d
        private :: my_alloc_pdouble_2d
        private :: my_alloc_pdouble_3d
        !lengths : integer(8)
        private :: my_alloc_8_real_1d
        private :: my_alloc_8_real_2d
        private :: my_alloc_8_real_3d
        private :: my_alloc_8_integer_1d
        private :: my_alloc_8_integer_2d
        private :: my_alloc_8_integer_3d
        private :: my_alloc_8_double_1d
        private :: my_alloc_8_double_2d
        private :: my_alloc_8_double_3d
        private :: my_alloc_8_preal_1d
        private :: my_alloc_8_preal_2d
        private :: my_alloc_8_preal_3d
        private :: my_alloc_8_pinteger_1d
        private :: my_alloc_8_pinteger_2d
        private :: my_alloc_8_pinteger_3d
        private :: my_alloc_8_pdouble_1d
        private :: my_alloc_8_pdouble_2d
        private :: my_alloc_8_pdouble_3d

        public :: my_alloc

        interface my_alloc
          module procedure my_alloc_real_1d
          module procedure my_alloc_real_2d
          module procedure my_alloc_real_3d
          module procedure my_alloc_integer_1d
          module procedure my_alloc_integer_2d
          module procedure my_alloc_integer_3d
          module procedure my_alloc_double_1d
          module procedure my_alloc_double_2d
          module procedure my_alloc_double_3d
          module procedure my_alloc_preal_1d
          module procedure my_alloc_preal_2d
          module procedure my_alloc_preal_3d
          module procedure my_alloc_pinteger_1d
          module procedure my_alloc_pinteger_2d
          module procedure my_alloc_pinteger_3d
          module procedure my_alloc_pdouble_1d
          module procedure my_alloc_pdouble_2d
          module procedure my_alloc_pdouble_3d

          module procedure my_alloc_8_real_1d
          module procedure my_alloc_8_real_2d
          module procedure my_alloc_8_real_3d
          module procedure my_alloc_8_integer_1d
          module procedure my_alloc_8_integer_2d
          module procedure my_alloc_8_integer_3d
          module procedure my_alloc_8_double_1d
          module procedure my_alloc_8_double_2d
          module procedure my_alloc_8_double_3d
          module procedure my_alloc_8_preal_1d
          module procedure my_alloc_8_preal_2d
          module procedure my_alloc_8_preal_3d
          module procedure my_alloc_8_pinteger_1d
          module procedure my_alloc_8_pinteger_2d
          module procedure my_alloc_8_pinteger_3d
          module procedure my_alloc_8_pdouble_1d
          module procedure my_alloc_8_pdouble_2d
          module procedure my_alloc_8_pdouble_3d
        end interface my_alloc

      contains

! ======================================================================================================================
!                                                     TOOLS
! ======================================================================================================================
      !||====================================================================
      !||    build_msg      ../common_source/tools/memory/my_alloc.F
      !||--- called by ------------------------------------------------------
      !||    execargcheck   ../engine/source/engine/execargcheck.F
      !||    radioss2       ../engine/source/engine/radioss2.F
      !||    starter0       ../starter/source/starter/starter0.F
      !||====================================================================
        function build_msg(str) result(error_message)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          character(len=*), intent(in) :: str
          character(len=len_error_message) :: error_message
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          if(len_trim(str) > len_error_message) then
            error_message = str(1:len_error_message)
          else
            error_message = adjustl(str) // repeat(" ", len_error_message - len_trim(str))
          end if
        end function build_msg

!! \brief Check if the allocation was successful and print an error message if it was noti
      !||====================================================================
      !||    check_error_and_write    ../common_source/tools/memory/my_alloc.F
      !||--- called by ------------------------------------------------------
      !||    my_alloc_8_double_1d     ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_8_double_2d     ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_8_double_3d     ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_8_integer_1d    ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_8_integer_2d    ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_8_integer_3d    ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_8_pdouble_1d    ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_8_pdouble_2d    ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_8_pdouble_3d    ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_8_pinteger_1d   ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_8_pinteger_2d   ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_8_pinteger_3d   ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_8_preal_1d      ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_8_preal_2d      ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_8_preal_3d      ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_8_real_1d       ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_8_real_2d       ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_8_real_3d       ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_double_1d       ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_double_2d       ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_double_3d       ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_integer_1d      ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_integer_2d      ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_integer_3d      ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_pdouble_1d      ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_pdouble_2d      ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_pdouble_3d      ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_pinteger_1d     ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_pinteger_2d     ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_pinteger_3d     ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_preal_1d        ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_preal_2d        ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_preal_3d        ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_real_1d         ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_real_2d         ../common_source/tools/memory/my_alloc.F
      !||    my_alloc_real_3d         ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    arret                    ../engine/source/system/arret.F
      !||====================================================================
        subroutine check_error_and_write(stat,msg)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: stat
          character(len=len_error_message), optional,  intent(in) :: msg

! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          if (stat /= 0) then
            write(6, "(a,i10,a)") 'Error in memory allocation'
            if(present(msg)) then
              write(6, "(a)") msg
            endif
            call arret(2)
          end if
        end subroutine check_error_and_write

! ======================================================================================================================
!                                           REAL ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of real numbers
      !||====================================================================
      !||    my_alloc_real_1d        ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_real_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_real_1d

!! \brief Allocate a 2D array of real numbers
      !||====================================================================
      !||    my_alloc_real_2d        ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_real_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_real_2d

      !||====================================================================
      !||    my_alloc_real_3d        ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_real_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:,:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_real_3d
! ======================================================================================================================
!                                           double ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of double numbers
      !||====================================================================
      !||    my_alloc_double_1d      ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_double_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_double_1d

!! \brief Allocate a 2D array of double numbers
      !||====================================================================
      !||    my_alloc_double_2d      ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_double_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_double_2d

      !||====================================================================
      !||    my_alloc_double_3d      ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_double_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:,:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_double_3d


! ======================================================================================================================
!                                           INTEGER ALLOCATION ROUTINES
! ======================================================================================================================
        !! \brief Allocate a 1D array of integer numbers
      !||====================================================================
      !||    my_alloc_integer_1d     ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_integer_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_integer_1d

!! \brief Allocate a 2D array of integers
      !||====================================================================
      !||    my_alloc_integer_2d     ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_integer_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_integer_2d

!! \brief Allocate a 3D array of integers
      !||====================================================================
      !||    my_alloc_integer_3d     ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_integer_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:,:), allocatable, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr
        end subroutine my_alloc_integer_3d
! ======================================================================================================================
!                                           REAL ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of real numbers
      !||====================================================================
      !||    my_alloc_preal_1d       ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_preal_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_preal_1d

!! \brief Allocate a 2D array of real numbers
      !||====================================================================
      !||    my_alloc_preal_2d       ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_preal_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_preal_2d

      !||====================================================================
      !||    my_alloc_preal_3d       ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_preal_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:,:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_preal_3d
! ======================================================================================================================
!                                           double ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of double numbers
      !||====================================================================
      !||    my_alloc_pdouble_1d     ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_pdouble_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_pdouble_1d

!! \brief Allocate a 2D array of double numbers
      !||====================================================================
      !||    my_alloc_pdouble_2d     ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_pdouble_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_pdouble_2d

      !||====================================================================
      !||    my_alloc_pdouble_3d     ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_pdouble_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:,:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_pdouble_3d


! ======================================================================================================================
!                                           INTEGER ALLOCATION ROUTINES
! ======================================================================================================================
        !! \brief Allocate a 1D array of integer numbers
      !||====================================================================
      !||    my_alloc_pinteger_1d    ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_pinteger_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_pinteger_1d

!! \brief Allocate a 2D array of integers
      !||====================================================================
      !||    my_alloc_pinteger_2d    ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_pinteger_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: n !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_pinteger_2d

!! \brief Allocate a 3D array of integers
      !||====================================================================
      !||    my_alloc_pinteger_3d    ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_pinteger_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:,:), pointer, intent(inout) :: a !< The allocated array
          integer, intent(in) :: l !< The first dimension of the array
          integer, intent(in) :: m !< The second dimension of the array
          integer, intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr
        end subroutine my_alloc_pinteger_3d

! ======================
! ====================== INTEGER*8
! ======================
! ======================================================================================================================
!                                           REAL ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of real numbers
      !||====================================================================
      !||    my_alloc_8_real_1d      ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_8_real_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_real_1d

!! \brief Allocate a 2D array of real numbers
      !||====================================================================
      !||    my_alloc_8_real_2d      ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_8_real_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_real_2d

      !||====================================================================
      !||    my_alloc_8_real_3d      ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_8_real_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:,:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_real_3d
! ======================================================================================================================
!                                           double ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of double numbers
      !||====================================================================
      !||    my_alloc_8_double_1d    ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_8_double_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_double_1d

!! \brief Allocate a 2D array of double numbers
      !||====================================================================
      !||    my_alloc_8_double_2d    ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_8_double_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_double_2d

      !||====================================================================
      !||    my_alloc_8_double_3d    ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_8_double_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:,:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_double_3d


! ======================================================================================================================
!                                           INTEGER ALLOCATION ROUTINES
! ======================================================================================================================
        !! \brief Allocate a 1D array of integer numbers
      !||====================================================================
      !||    my_alloc_8_integer_1d   ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_8_integer_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_integer_1d

!! \brief Allocate a 2D array of integers
      !||====================================================================
      !||    my_alloc_8_integer_2d   ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_8_integer_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_integer_2d

!! \brief Allocate a 3D array of integers
      !||====================================================================
      !||    my_alloc_8_integer_3d   ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_8_integer_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:,:), allocatable, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr
        end subroutine my_alloc_8_integer_3d
! ======================================================================================================================
!                                           REAL ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of real numbers
      !||====================================================================
      !||    my_alloc_8_preal_1d     ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_8_preal_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_preal_1d

!! \brief Allocate a 2D array of real numbers
      !||====================================================================
      !||    my_alloc_8_preal_2d     ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_8_preal_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_preal_2d

      !||====================================================================
      !||    my_alloc_8_preal_3d     ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_8_preal_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          real, dimension(:,:,:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_preal_3d
! ======================================================================================================================
!                                           double ALLOCATION ROUTINES
! ======================================================================================================================

!! \brief Allocate a 1D array of double numbers
      !||====================================================================
      !||    my_alloc_8_pdouble_1d   ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_8_pdouble_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_pdouble_1d

!! \brief Allocate a 2D array of double numbers
      !||====================================================================
      !||    my_alloc_8_pdouble_2d   ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_8_pdouble_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_pdouble_2d

      !||====================================================================
      !||    my_alloc_8_pdouble_3d   ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write   ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_8_pdouble_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(:,:,:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_pdouble_3d


! ======================================================================================================================
!                                           INTEGER ALLOCATION ROUTINES
! ======================================================================================================================
        !! \brief Allocate a 1D array of integer numbers
      !||====================================================================
      !||    my_alloc_8_pinteger_1d   ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write    ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_8_pinteger_1d(a, n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The size of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_pinteger_1d

!! \brief Allocate a 2D array of integers
      !||====================================================================
      !||    my_alloc_8_pinteger_2d   ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write    ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_8_pinteger_2d(a, n,m, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: n !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          allocate(a(n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr

        end subroutine my_alloc_8_pinteger_2d

!! \brief Allocate a 3D array of integers
      !||====================================================================
      !||    my_alloc_8_pinteger_3d   ../common_source/tools/memory/my_alloc.F
      !||--- calls      -----------------------------------------------------
      !||    check_error_and_write    ../common_source/tools/memory/my_alloc.F
      !||====================================================================
        subroutine my_alloc_8_pinteger_3d(a,l,m,n, msg, stat)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, dimension(:,:,:), pointer, intent(inout) :: a !< The allocated array
          integer(8), intent(in) :: l !< The first dimension of the array
          integer(8), intent(in) :: m !< The second dimension of the array
          integer(8), intent(in) :: n !< The third dimension of the array
          character(len=len_error_message), optional, intent(in) :: msg !< The error message to print if the allocation fails
          integer, optional, intent(out) :: stat !< The error code returned by the allocation
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ierr
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------

          allocate(a(l,n,m), stat=ierr)
          if(.not. present(stat)) then
            if(present(msg)) then
              call check_error_and_write(ierr, msg=msg)
            else
              call check_error_and_write(ierr)
            end if
          endif
          if(present(stat)) stat = ierr
        end subroutine my_alloc_8_pinteger_3d
      end module my_alloc_mod

