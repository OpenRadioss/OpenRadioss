#define NOT_FUSED 0 
#define FUSED 1
#define TARGET_FUSED 2
#define PROBLEMATIC 3
#define PROBLEMATIC_UNSOLVED 4

module multicutcell_solver_mod
  use FV_fluxes, only: FV_flux_hllc_Euler
  use precision_mod, only : wp                            !provides kind for eigther single or double precision (wp means working precision)
  implicit none
  contains

  
  pure function max_nb_edges_in_cell(NUMELQ, NUMELTG)
    integer, intent(in) :: NUMELQ, NUMELTG
    integer(kind=8) ::max_nb_edges_in_cell

    if (NUMELQ>0) then
      max_nb_edges_in_cell = 4
    elseif (NUMELTG>0) then
      max_nb_edges_in_cell = 3
    end if
  end function max_nb_edges_in_cell

  pure function max_nb_points_in_cell(NUMELQ, NUMELTG)
    integer, intent(in) :: NUMELQ, NUMELTG
    integer(kind=8) :: max_nb_points_in_cell

    if (NUMELQ>0) then
      max_nb_points_in_cell = 4
    elseif (NUMELTG>0) then
      max_nb_points_in_cell = 3
    end if
  end function max_nb_points_in_cell

!! \brief Flags, in grid, the cells close to the interface.
  subroutine compute_close_cells(NUMELQ, NUMELTG, NUMNOD, IXQ, IXTG, grid)
    use polygon_cutcell_mod
    use grid2D_struct_multicutcell_mod
  
    IMPLICIT NONE
  
    ! INPUT argument
    integer, intent(in) :: NUMELQ, NUMELTG, NUMNOD
    integer, dimension(:,:), intent(in) :: IXQ, IXTG
    ! IN/OUTPUT argument
    type(grid2D_struct_multicutcell), dimension(:, :), intent(inout) :: grid

    !Local variables
    logical, dimension(NUMNOD) :: is_narrowband_pt
    integer :: nb_cell, nb_regions, nb_pts_in_cell, i, k

    nb_cell = size(grid, 1)
    nb_regions = size(grid, 2)
    if (NUMELQ>0) then
      nb_pts_in_cell = 4
    elseif (NUMELTG>0) then
      nb_pts_in_cell = 3
    end if
    !First : all points in contact with a cell in a narrowband are marked as true.
    is_narrowband_pt(:) = .false.
    if (NUMELQ>0) then
      do i = 1,nb_cell
        do k=2,2+nb_pts_in_cell-1
          is_narrowband_pt(IXQ(k, i)) = (is_narrowband_pt(IXQ(k, i)) .or. grid(i, 1)%is_narrowband)
        end do
      end do
    elseif (NUMELTG>0) then
      do i = 1,nb_cell
        do k=2,2+nb_pts_in_cell-1
          is_narrowband_pt(IXTG(k, i)) = (is_narrowband_pt(IXTG(k, i)) .or. grid(i, 1)%is_narrowband)
        end do
      end do
    end if

    !Second : all cells that have at least one point marked true are close cells
    grid(:,:)%close_cells = .false.
    if (NUMELQ>0) then
      do i = 1,nb_cell
        do k=2,2+nb_pts_in_cell-1
          grid(i,1)%close_cells = (grid(i,1)%close_cells .or. is_narrowband_pt(IXQ(k, i)))
          grid(i,2:nb_regions)%close_cells = grid(i,1)%close_cells
        end do
      end do
    elseif (NUMELTG>0) then
      do i = 1,nb_cell
        do k=2,2+nb_pts_in_cell-1
          grid(i,1)%close_cells = (grid(i,1)%close_cells .or. is_narrowband_pt(IXTG(k, i)))
          grid(i,2:nb_regions)%close_cells = grid(i,1)%close_cells
        end do
      end do
    end if
  end subroutine compute_close_cells

!! \brief Compute cell occupancies for each phase in grid.
  subroutine multicutcell_compute_lambdas(NUMELQ, NUMELTG, NUMNOD, IXQ, IXTG, X, grid, dt, &
                                          rho, vely, velz, p, gamma, nb_edges_clipped, wave_type)
    use polygon_cutcell_mod
    use grid2D_struct_multicutcell_mod
    use riemann_solver_mod
  
    IMPLICIT NONE
  
    ! INPUT argument
    integer, intent(in) :: NUMELQ, NUMELTG, NUMNOD
    integer, dimension(:,:), intent(in) :: IXQ, IXTG
    real(kind=wp), dimension(:,:), intent(in) :: X
    real(kind=wp), intent(in) :: dt
    real(kind=wp), dimension(:,:), intent(in) :: rho
    real(kind=wp), dimension(:,:), intent(in) :: vely
    real(kind=wp), dimension(:,:), intent(in) :: velz
    real(kind=wp), dimension(:,:), intent(in) :: p
    real(kind=wp), dimension(:), intent(in) :: gamma
    integer(kind=8), intent(in) :: nb_edges_clipped
    integer, intent(in) :: wave_type
    ! IN/OUTPUT argument
    type(grid2D_struct_multicutcell), dimension(:, :), intent(inout) :: grid

    !Local variables
    integer(kind=8), parameter:: max_length_array=1000 !I hope 1000 is enough... But it is reasonable.
    integer(kind=8) :: nb_cell, nb_regions, nb_edges
    integer(kind=8) :: i, j, k
    real(kind=wp), dimension(:), allocatable :: ptr_lambdas_arr
    real(kind=wp), dimension(:), allocatable :: ptr_big_lambda_n, ptr_big_lambda_np1
    real(kind=wp), dimension(max_length_array) :: normals_y 
    real(kind=wp), dimension(max_length_array) :: normals_z
    real(kind=wp), dimension(max_length_array) :: normals_t
    integer(kind=8) :: nb_normals
    real(kind=wp), dimension(:), allocatable :: mean_area 
    real(kind=wp), dimension(:), allocatable :: pressure_face
    integer(kind=8) :: is_narrowband
    real(kind=wp), dimension(4) :: ys, zs
    integer(kind=8), dimension(4) :: pt_indices
    integer :: print_nb_cell
    real(kind=wp) :: us, vsL, vsR, ps, ny, nz, norm
    integer(kind=8), dimension(max_length_array) :: local_index_edge
    integer(kind=8), dimension(:,:), allocatable :: index_edge
    type(Point3D), dimension(:,:), allocatable :: normals_clipped
    real(kind=wp) :: mean_pressure, mean_A
    type(Point3D) :: mean_normal

    nb_cell = size(grid, 1)
    nb_regions = size(grid, 2)
    if (NUMELQ>0) then
      nb_edges = 4
    else
      nb_edges = 3
    end if

    allocate(ptr_lambdas_arr(4*nb_regions))
    allocate(ptr_big_lambda_n(nb_regions))
    allocate(ptr_big_lambda_np1(nb_regions))
    allocate(index_edge(nb_cell, max_length_array))
    allocate(normals_clipped(nb_cell, max_length_array))
    allocate(mean_area(nb_edges_clipped))
    allocate(pressure_face(nb_edges_clipped))

    index_edge(:,:) = -1
    do j=1,nb_edges_clipped
      mean_area(j) = 0.0
      pressure_face(j) = 0.0
    end do

    print_nb_cell = nb_cell/10
    write(*,*) "Doing cell number ", 1, "/", nb_cell
    do i = 1,nb_cell
      if (i>print_nb_cell-1) then 
        write(*,*) "Doing cell number ", i, "/", nb_cell
        print_nb_cell = print_nb_cell + nb_cell/10
        call system('sync')
      end if

      if (grid(i,1)%close_cells) then
        pt_indices = IXQ(2:2+nb_edges-1, i)
        ys(1:nb_edges) = X(2, pt_indices)
        zs(1:nb_edges) = X(3, pt_indices)
        call build_grid_from_points_fortran(ys, zs, pt_indices, nb_edges) 
        call compute_lambdas2d_fortran(dt, ptr_lambdas_arr, ptr_big_lambda_n, ptr_big_lambda_np1, &
                                      normals_y, normals_z, normals_t, local_index_edge, max_length_array, nb_normals,&
                                      is_narrowband)

        nb_normals = min(nb_normals, max_length_array) !if there is more than max_length_array normals, we only keep the first max_length_array of them...
        do j=1,nb_normals
          k = local_index_edge(j)
          index_edge(i,j) = k 

          norm = sqrt(normals_y(j)*normals_y(j) + normals_z(j)*normals_z(j))
          normals_clipped(i,j)%y = normals_y(j)
          normals_clipped(i,j)%z = normals_z(j)
          normals_clipped(i,j)%t = normals_t(j)
          if (norm > 0.0) then
            ny = normals_y(j) / norm
            nz = normals_z(j) / norm
            call solve_riemann_problem(gamma(1), gamma(2), &
                                        rho(i,1), rho(i,2), &
                                        vely(i,1), vely(i,2), &
                                        velz(i,1), velz(i,2), &
                                        p(i,1), p(i,2), wave_type, &
                                        ny, nz, &
                                        us, vsL, vsR, ps)
            mean_area(k) = mean_area(k) + norm
            pressure_face(k) = pressure_face(k) + ps*norm
          end if
        end do

        do j=1,nb_regions
          grid(i,j)%lambdan_per_cell = ptr_big_lambda_n(j)
          grid(i,j)%lambdanp1_per_cell = ptr_big_lambda_np1(j)
          grid(i,j)%is_narrowband = (is_narrowband>0)
          do k=1,nb_edges
            grid(i,j)%lambda_per_edge(k) = ptr_lambdas_arr((k-1)*nb_regions + j)
          end do
        end do
      end if
    end do

    do i = 1,nb_cell
      if (grid(i,1)%close_cells) then
        mean_A = 0.0_wp
        mean_pressure = 0.0_wp
        mean_normal%y = 0.0_wp
        mean_normal%z = 0.0_wp
        mean_normal%t = 0.0_wp
        do j=1,max_length_array
          k = index_edge(i,j)
          if (k<0) then 
            exit   !break inner j loop
          end if

          mean_A = mean_A + mean_area(k)
          mean_pressure = mean_pressure + pressure_face(k)
          mean_normal%y = mean_normal%y + normals_clipped(i,j)%y
          mean_normal%z = mean_normal%z + normals_clipped(i,j)%z
          mean_normal%t = mean_normal%t + normals_clipped(i,j)%t
        end do

        do j=1,nb_regions
          if (mean_A > 0) then
            grid(i,j)%normal_intern_face_space%y = mean_normal%y
            grid(i,j)%normal_intern_face_space%z = mean_normal%z
            grid(i,j)%normal_intern_face_time    = mean_normal%t
            grid(i,j)%p_normal_intern_face_space%y = (mean_pressure/mean_A)*mean_normal%y
            grid(i,j)%p_normal_intern_face_space%z = (mean_pressure/mean_A)*mean_normal%z
            grid(i,j)%p_normal_intern_face_time    = (mean_pressure/mean_A)*mean_normal%t
          else 
            grid(i,j)%normal_intern_face_space%y = 0.0_wp
            grid(i,j)%normal_intern_face_space%z = 0.0_wp
            grid(i,j)%normal_intern_face_time    = 0.0_wp
            grid(i,j)%p_normal_intern_face_space%y = 0.0_wp
            grid(i,j)%p_normal_intern_face_space%z = 0.0_wp
            grid(i,j)%p_normal_intern_face_time    = 0.0_wp
          end if
        end do
      end if
    end do

    deallocate(index_edge)
    deallocate(ptr_lambdas_arr)
    deallocate(ptr_big_lambda_n)
    deallocate(ptr_big_lambda_np1)
    deallocate(normals_clipped)
    deallocate(mean_area)
    deallocate(pressure_face)

    call compute_close_cells(NUMELQ, NUMELTG, NUMNOD, IXQ, IXTG, grid)

  end subroutine multicutcell_compute_lambdas

!! \brief Compute normals to the cell boundary.
  subroutine multicutcell_compute_normals(NUMELQ, NUMELTG, IXQ, IXTG, X, i_cell, normals, nb_normals)
    use polygon_cutcell_mod

    IMPLICIT NONE

    integer(kind=8), intent(in) :: i_cell
    integer, intent(in) :: NUMELQ, NUMELTG
    integer, dimension(:,:), intent(in) :: IXQ, IXTG
    real(kind=wp), dimension(:,:), intent(in) :: X
    type(Point2D), dimension(4) :: normals
    integer(kind=8), intent(out) :: nb_normals

    integer(kind=8) :: j 
    type(Point2D) :: P1, P2, P3, P4
    real(kind=wp) :: norm
    
    if (NUMELQ > 0) then
      nb_normals = 4

      P1%y = X(2, IXQ(2, i_cell))
      P1%z = X(3, IXQ(2, i_cell))
      
      P2%y = X(2, IXQ(3, i_cell))
      P2%z = X(3, IXQ(3, i_cell))

      P3%y = X(2, IXQ(4, i_cell))
      P3%z = X(3, IXQ(4, i_cell))

      P4%y = X(2, IXQ(5, i_cell))
      P4%z = X(3, IXQ(5, i_cell))

      normals(1)%y = (P2%z-P1%z)
      normals(1)%z =-(P2%y-P1%y)
      normals(2)%y = (P3%z-P2%z)
      normals(2)%z =-(P3%y-P2%y)
      normals(3)%y = (P4%z-P3%z)
      normals(3)%z =-(P4%y-P3%y)
      normals(4)%y = (P1%z-P4%z)
      normals(4)%z =-(P1%y-P4%y)
    elseif (NUMELTG > 0) then
      nb_normals = 3

      P1%y = X(2, IXTG(2, i_cell))
      P1%z = X(3, IXTG(2, i_cell))
      
      P2%y = X(2, IXTG(3, i_cell))
      P2%z = X(3, IXTG(3, i_cell))

      P3%y = X(2, IXTG(4, i_cell))
      P3%z = X(3, IXTG(4, i_cell))

      normals(1)%y = (P2%z-P1%z)
      normals(1)%z =-(P2%y-P1%y)
      normals(2)%y = (P3%z-P2%z)
      normals(2)%z =-(P3%y-P2%y)
      normals(3)%y = (P1%z-P3%z)
      normals(3)%z =-(P1%y-P3%y)
    end if

    do j=1,nb_normals
      !Normalize
      norm = sqrt(normals(j)%y*normals(j)%y + normals(j)%z*normals(j)%z)
      normals(j)%y = normals(j)%y / norm
      normals(j)%z = normals(j)%z / norm
    end do
  end subroutine multicutcell_compute_normals

  subroutine build_full_states(grid, rho, vely, velz, p, gamma, &
                              full_rho, full_pres, full_vel, full_etot)
    use grid2D_struct_multicutcell_mod
    implicit none
    type(grid2D_struct_multicutcell), dimension(:, :), intent(in) :: grid
    real(kind=wp), dimension(:,:), intent(in) :: vely
    real(kind=wp), dimension(:,:), intent(in) :: velz
    real(kind=wp), dimension(:,:), intent(in) :: rho
    real(kind=wp), dimension(:,:), intent(in) :: p
    real(kind=wp), dimension(:), intent(in) :: gamma
    ! OUTPUT arguments
    real(kind=wp), dimension(:), intent(out) :: full_rho, full_pres, full_etot
    real(kind=wp), dimension(:, :), intent(out) :: full_vel 

    full_rho = (grid(:,1)%lambdanp1_per_cell*rho(:,1) + &
                        grid(:,2)%lambdanp1_per_cell*rho(:,2))/grid(:,1)%area
    full_pres = (grid(:,1)%lambdanp1_per_cell*p(:,1) + &
                        grid(:,2)%lambdanp1_per_cell*p(:,2))/grid(:,1)%area
    full_vel(2,:) = (grid(:,1)%lambdanp1_per_cell*vely(:,1) + &
                        grid(:,2)%lambdanp1_per_cell*vely(:,2))/grid(:,1)%area
    full_vel(3,:) = (grid(:,1)%lambdanp1_per_cell*velz(:,1) + &
                        grid(:,2)%lambdanp1_per_cell*velz(:,2))/grid(:,1)%area


    full_etot = (0.5*(full_vel(2,:)*full_vel(2,:)+full_vel(3,:)*full_vel(3,:)) &
                          + (grid(:,1)%lambdanp1_per_cell*(p(:,1)/((gamma(1)*rho(:,1)))) + &
                              grid(:,2)%lambdanp1_per_cell*(p(:,2)/((gamma(2)*rho(:,2))))) &
                          + (grid(:,1)%lambdanp1_per_cell*(p(:,1)/((gamma(1)*rho(:,1)))) + &
                              grid(:,2)%lambdanp1_per_cell*(p(:,2)/((gamma(2)*rho(:,2))))))/grid(:,1)%area

  end subroutine build_full_states
 
!! \brief Updates state of fluid in each phase.
!! \details Update the velocity, density and pressure of each phase of the fluid, and compute the average of each quantity
!!          given the occupancy of each cell.
!! \param grid information on each cell (occupancy for each phase in the cell and on its boundary, ... See grid2D_struct_multicutcell)
!! \param vel* 2D array of velocity for each phase
!! \param rho 2D array of density for each phase
!! \param p 2D array of pressure for each phase
!! \param full_vel 2D array of velocity averaged by cell occupancy
!! \param full_rho 1D array of density averaged by cell occupancy
!! \param full_p 1D array of pressure averaged by cell occupancy
!! \param dt_next time step of next time integration
!! \param i_print integer used for printing a vtk for reading the polygonal interface.
  subroutine update_fluid_multicutcell(N2D, NUMELQ, NUMELTG, NUMNOD, IXQ, IXTG, X, ALE_CONNECT, &
                          grid, vely, velz, rho, p, gamma, dt, threshold, sign, ebcs_tab, &
                          full_rho, full_pres, full_vel, full_etot, dt_next, sound_speed, i_print)
    use polygon_cutcell_mod
    use grid2D_struct_multicutcell_mod
    use ALE_CONNECTIVITY_MOD
    use ebcs_mod, only : t_ebcs_tab
  
    IMPLICIT NONE
  
    ! INPUT arguments
    integer, intent(in) :: N2D, NUMELQ, NUMELTG, NUMNOD
    integer, dimension(:,:), intent(in) :: IXQ, IXTG
    real(kind=wp), dimension(:,:), intent(in) :: X
    TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
    real(kind=wp), intent(in) :: threshold, dt
    real(kind=wp), dimension(:), intent(in) :: gamma
    integer, intent(in) :: sign
    type(t_ebcs_tab), target, intent(in) :: ebcs_tab              !< data structure for user boundary conditions
    integer(kind=8), intent(in) :: i_print    !Used for debugging : output the polygon after update that can be plotted using gnuplot.
    ! IN/OUTPUT arguments
    type(grid2D_struct_multicutcell), dimension(:, :), intent(inout) :: grid
    real(kind=wp), dimension(:,:) :: vely
    real(kind=wp), dimension(:,:) :: velz
    real(kind=wp), dimension(:,:) :: rho
    real(kind=wp), dimension(:,:) :: p
    ! OUTPUT arguments
    real(kind=wp), intent(out) :: dt_next
    real(kind=wp), dimension(:), intent(out) :: sound_speed 
    real(kind=wp), dimension(:), intent(out) :: full_rho, full_pres, full_etot
    real(kind=wp), dimension(:, :), intent(out) :: full_vel 
  
    !Local variables
    integer(kind=8) :: i, j, k
    integer(kind=8) :: nb_cell, nb_regions, nb_edges
    integer(kind=8) :: ind_targ
    type(Point2D) :: pressure_mean_normal
    real(kind=wp) :: mean_normal_time
    !real(kind=wp) :: lambdan
    real(kind=wp) :: lambdanp1
    type(ConservativeState2D), dimension(:, :), allocatable :: W
    type(ConservativeState2D), dimension(:, :), allocatable :: dW
    type(ConservativeFlux2D), dimension(:, :), allocatable :: fx
    real(kind=wp), dimension(:,:), allocatable :: lambdan_prev
    !real(kind=wp), dimension(:), allocatable :: areas
    integer(kind=8), dimension(:, :), allocatable :: target_cells
    integer(kind=8), dimension(:, :), allocatable :: cell_type
    real(kind=wp), dimension(:), allocatable :: normalVecy
    real(kind=wp), dimension(:), allocatable :: normalVecz
    real(kind=wp) :: min_pos_Se
    real(kind=wp), dimension(:), allocatable :: vec_move_clippedy
    real(kind=wp), dimension(:), allocatable :: vec_move_clippedz
    integer(kind=8) :: nb_pts_clipped
    integer(kind=8) :: nb_edge_clipped, new_nb_edges
    real(kind=wp) :: dx
    real(kind=wp) :: pressure_mean_normal_time
    type(Point2D) :: mean_normal
    integer(kind=2) :: odd_k
    integer(kind=8), dimension(:), allocatable :: id_pt_cell
    real(kind=wp) :: minimal_length, maximal_length, minimal_angle, largest_speed_wave
    integer :: wave_type = 2 !1 for rarefaction, 2 for contact, 3 for shock waves

    if (N2D < 0) then
      print *, "Error: no 2D?"
      !stop
    end if
    if (sign<0) then
    end if
    
    dx = sqrt(minval(grid(:, 1)%area))
    nb_cell = NUMELQ+NUMELTG !size(vely, 1)
    nb_regions = size(vely, 2)
    minimal_length = 0.5*dx
    maximal_length = dx
    minimal_angle = ATAN(1.d0) !pi/4
  
    call nb_pts_clipped_fortran(nb_pts_clipped)
    call nb_edge_clipped_fortran(nb_edge_clipped)
    allocate(lambdan_prev(nb_cell, nb_regions))
    allocate(fx(nb_cell, nb_regions))
    !allocate(areas(nb_cell))
    allocate(target_cells(nb_cell, nb_regions))
    allocate(cell_type(nb_cell, nb_regions))
    allocate(W(nb_cell, nb_regions))
    allocate(dW(nb_cell, nb_regions))
    allocate(normalVecy(nb_pts_clipped))
    allocate(normalVecz(nb_pts_clipped))
    allocate(vec_move_clippedy(nb_pts_clipped))
    allocate(vec_move_clippedz(nb_pts_clipped))
    allocate(id_pt_cell(nb_pts_clipped))
  
    do i = 1,nb_cell
      do k = 1,nb_regions
        call primal_to_conservative(gamma(k), vely(i, k), velz(i, k), rho(i, k), p(i, k), W(i, k))
        lambdan_prev(i, k) = grid(i, k)%lambdanp1_per_cell
      end do 
    end do
  
    normalVecy(:) = 0.0
    normalVecz(:) = 0.0
    call compute_normals_clipped_fortran(normalVecy, normalVecz, min_pos_Se)


    call compute_all_id_pt_cell(NUMELQ, NUMELTG, IXQ, IXTG, X, grid, nb_pts_clipped, id_pt_cell)
    call compute_vec_move_clipped(gamma, rho, vely, velz, p, nb_pts_clipped, id_pt_cell, &
                                normalVecy, normalVecz, &
                                vec_move_clippedy, vec_move_clippedz, wave_type)
  
    call smooth_vel_clipped_fortran(vec_move_clippedy, vec_move_clippedz, min_pos_Se, dt)
    call update_clipped_fortran(vec_move_clippedy, vec_move_clippedz, dt, new_nb_edges, &
                                minimal_length, maximal_length, minimal_angle)
                                
    nb_edge_clipped = max(nb_edge_clipped, new_nb_edges)
    call print_clipped_fortran(i_print)
  
    call multicutcell_compute_lambdas(NUMELQ, NUMELTG, NUMNOD, IXQ, IXTG, X, grid, dt,&
                                        rho, vely, velz, p, gamma, nb_edge_clipped, wave_type) 
    !TODO exchange lambdas between procs on neighbouring cells
    call fuse_cells(NUMELQ, NUMELTG, ALE_CONNECT, grid, threshold, target_cells, cell_type) 

    call multicutcell_compute_fluxes(NUMELQ, NUMELTG, IXQ, IXTG, X, ALE_CONNECT, grid, target_cells, gamma, rho, vely, velz, p, fx)
    call multicutcell_compute_fluxes_boundary(NUMELQ, NUMELTG, IXQ, IXTG, X, gamma, &
                                              rho, vely, velz, p, ebcs_tab, fx)
    !Compute right hand side for non-fused or target cells
    grid(:, :)%lambdanp1_per_cell_target = 0.
    grid(:, :)%lambdan_per_cell_target = 0.
    
    odd_k = 1
    do k=1,nb_regions
      do i = 1,nb_cell
        dW(i, k)%rho   = 0.0
        dW(i, k)%rhovy = 0.0
        dW(i, k)%rhovz = 0.0
        dW(i, k)%rhoE  = 0.0
      end do
    end do

    nb_edges = max_nb_edges_in_cell(NUMELQ, NUMELTG)
    do k=1,nb_regions
      do i = 1,nb_cell
        ind_targ = target_cells(i, k) !TODO problem here when parallel: what if the target cell is in an other region?
        dW(ind_targ, k)%rho = dW(ind_targ, k)%rho + grid(i, k)%lambdan_per_cell * W(i, k)%rho  !Add explicit term of Euler scheme
        dW(ind_targ, k)%rhovy = dW(ind_targ, k)%rhovy + grid(i, k)%lambdan_per_cell * W(i, k)%rhovy  
        dW(ind_targ, k)%rhovz = dW(ind_targ, k)%rhovz + grid(i, k)%lambdan_per_cell * W(i, k)%rhovz  
        dW(ind_targ, k)%rhoE = dW(ind_targ, k)%rhoE + grid(i, k)%lambdan_per_cell * W(i, k)%rhoE  
    
        !Add numerical fluxes
        do j = 1,nb_edges
          dW(ind_targ, k)%rho   = dW(ind_targ, k)%rho   - dt * fx(i, k)%rho(j)   * grid(i, k)%lambda_per_edge(j) 
          dW(ind_targ, k)%rhovy = dW(ind_targ, k)%rhovy - dt * fx(i, k)%rhovy(j) * grid(i, k)%lambda_per_edge(j) 
          dW(ind_targ, k)%rhovz = dW(ind_targ, k)%rhovz - dt * fx(i, k)%rhovz(j) * grid(i, k)%lambda_per_edge(j) 
          dW(ind_targ, k)%rhoE  = dW(ind_targ, k)%rhoE  - dt * fx(i, k)%rhoE(j)  * grid(i, k)%lambda_per_edge(j) 
        end do
    
        !Add swept quantities
        mean_normal = grid(i, 1)%normal_intern_face_space
        mean_normal_time = grid(i, 1)%normal_intern_face_time
        pressure_mean_normal = grid(i, 1)%p_normal_intern_face_space
        pressure_mean_normal_time = grid(i, 1)%p_normal_intern_face_time

        dW(ind_targ, k)%rhovy = dW(ind_targ, k)%rhovy - odd_k*pressure_mean_normal%y
        dW(ind_targ, k)%rhovz = dW(ind_targ, k)%rhovz - odd_k*pressure_mean_normal%z
        dW(ind_targ, k)%rhoE  = dW(ind_targ, k)%rhoE  + odd_k*pressure_mean_normal_time

        !Update size of fluid part
        grid(ind_targ, k)%lambdanp1_per_cell_target = grid(ind_targ, k)%lambdanp1_per_cell_target + grid(i, k)%lambdanp1_per_cell
        grid(ind_targ, k)%lambdan_per_cell_target = grid(ind_targ, k)%lambdan_per_cell_target + grid(i, k)%lambdan_per_cell
        !areas(ind_targ) = areas(ind_targ) + grid(i, 1)%area
      end do
      odd_k = -1
    end do
    
    !Update non-fused or target cells
    do k = 1,nb_regions
        do i = 1,nb_cell
          if (i == target_cells(i, k)) then
            lambdanp1 = grid(i, k)%lambdanp1_per_cell_target
            !lambdan = grid(i)%lambdan_per_cell_target(k)
            if (lambdanp1 > 0.5 * threshold * grid(i, k)%area) then
                W(i, k)%rho   = dW(i, k)%rho / lambdanp1
                W(i, k)%rhovy = dW(i, k)%rhovy / lambdanp1
                W(i, k)%rhovz = dW(i, k)%rhovz / lambdanp1
                W(i, k)%rhoE  = dW(i, k)%rhoE / lambdanp1
            end if
          end if
        end do
    end do
    
    !Broadcast value
    do i = 1,nb_cell
      do k = 1, nb_regions
        if (i /= target_cells(i, k)) then
          W(i, k)%rho   = W(target_cells(i, k), k)%rho
          W(i, k)%rhovy = W(target_cells(i, k), k)%rhovy
          W(i, k)%rhovz = W(target_cells(i, k), k)%rhovz
          W(i, k)%rhoE  = W(target_cells(i, k), k)%rhoE
        end if
        call conservative_to_primal(gamma(k), vely(i, k), velz(i, k), rho(i, k), p(i, k), W(i, k))
      end do
    end do

    !Compute global variables for output
    call build_full_states(grid, rho, vely, velz, p, gamma, &
                              full_rho, full_pres, full_vel, full_etot)
    
    !Compute next dt
    largest_speed_wave = -1.
    do i = 1,nb_cell
      sound_speed(i) = (grid(i,1)%lambdanp1_per_cell*sqrt(gamma(1) * p(i,1) / rho(i,1)) + &
                    grid(i,2)%lambdanp1_per_cell*sqrt(gamma(2) * p(i,2) / rho(i,2)))/&
                    (grid(i,1)%lambdanp1_per_cell + grid(i,2)%lambdanp1_per_cell)
      do k = 1,nb_regions
        largest_speed_wave = max(largest_speed_wave, max(abs(vely(i, k)), abs(velz(i, k))) + sound_speed(i))
      end do
    end do
    dt_next = threshold * dx / largest_speed_wave

    !Deallocate local memory
    deallocate(lambdan_prev)
    deallocate(fx)
    !deallocate(areas)
    deallocate(target_cells)
    deallocate(cell_type)
    deallocate(W)
    deallocate(dW)
    deallocate(vec_move_clippedy)
    deallocate(vec_move_clippedz)
    deallocate(id_pt_cell)
  
    contains   
    !! Goes from primal variable to conservative variables
    subroutine primal_to_conservative(gamma, vely, velz, rho, p, W)
      use grid2D_struct_multicutcell_mod
  
      implicit none
      real(kind=wp) :: gamma
      real(kind=wp), intent(in) :: vely, velz, rho, p 
      type(ConservativeState2D), intent(out) :: W
  
      W%rho = rho
      W%rhovy = rho*vely
      W%rhovz = rho*velz
      W%rhoE = 0.5*rho*(vely*vely + velz*velz) + p/(gamma-1)
    end subroutine primal_to_conservative

    !! Goes from conservative variable to primal variables
    subroutine conservative_to_primal(gamma, vely, velz, rho, p, W)
      use grid2D_struct_multicutcell_mod
  
      implicit none
      real(kind=wp) :: gamma
      real(kind=wp), intent(out) :: vely, velz, rho, p 
      type(ConservativeState2D), intent(in) :: W
  
      rho = W%rho
      vely = W%rhovy/rho
      velz = W%rhovz/rho
      p =  (gamma - 1) * (W%rhoE - 0.5*(W%rhovy*W%rhovy + W%rhovz*W%rhovz)/W%rho)
    end subroutine conservative_to_primal
  
    !! For cell i_cell and local edge j_edge, finds the neighbouring other_cell and its local index for the edge (other_edge)
    subroutine adjacency_edge(ALE_CONNECT, i_cell, j_edge, other_cell, other_edge)
      use ALE_CONNECTIVITY_MOD

      implicit none
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      integer(kind=8), intent(in) :: i_cell, j_edge
      integer(kind=8), intent(out) :: other_cell, other_edge

      integer(kind=8) :: IAD2, LGTH, J, IV

      IAD2 = ALE_CONNECT%ee_connect%iad_connect(i_cell)
      other_cell = ALE_CONNECT%ee_connect%connected(IAD2 + j_edge - 1)

      !Look for correct edge on the other side
      other_edge = -1
      if (other_cell>0) then
        IAD2 = ALE_CONNECT%ee_connect%iad_connect(other_cell)
        LGTH = ALE_CONNECT%ee_connect%iad_connect(other_cell+1) - IAD2
        do J=1,LGTH
          IV = ALE_CONNECT%ee_connect%connected(IAD2 + J - 1)
          if (IV == i_cell) then
            other_edge = J 
          end if
        end do
      end if
    end subroutine adjacency_edge

    !! Identify all cut cells that are too small and makes a correspondance array (final_target_cells)
    !! with which every small cell will be fused 
    !! (all quantities of the small cell will be transfered for this time step in the target cell).
    subroutine fuse_cells(NUMELQ, NUMELTG, ALE_CONNECT, grid, threshold, final_target_cells, cell_type)
      use grid2D_struct_multicutcell_mod
      use integer_LL_mod
      use ALE_CONNECTIVITY_MOD

      implicit none
      integer :: NUMELQ, NUMELTG
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      type(grid2D_struct_multicutcell), dimension(:, :) :: grid
      real(kind=wp) :: threshold
      integer(kind=8), dimension(:, :) :: final_target_cells
      integer(kind=8), dimension(:, :) :: cell_type

      type(ptr_to_integer_LL_), dimension(:, :), allocatable :: target_cells
      logical :: narrowBand
      type(integer_LL_), pointer :: cells_to_be_merged
      type(integer_LL_), pointer :: copy_to_be_merged
      type(integer_LL_), pointer :: targ_c
      type(integer_LL_), pointer :: curr_ptr_int, curr_targ
      integer(kind=8) :: nb_cell, nb_regions, nb_edges
      integer(kind=8) :: i, k, j, c
      integer(kind=8) :: other_face, other_edge
      logical :: global_fusion_happened, fusion_happened
      integer(kind=8) :: size_list
      real(kind=wp) :: max_lambda
      integer(kind=8) :: i_max_lambd, rand_cell

      nb_cell = size(grid, 1)
      nb_regions = 2

      allocate(target_cells(nb_cell, nb_regions))
      !allocate(targ_c)
      !allocate(cells_to_be_merged)
      nullify(targ_c)
      nullify(cells_to_be_merged)
      nullify(copy_to_be_merged)
      nullify(curr_ptr_int)
      nullify(curr_targ)

      do k = 1,nb_regions
        call integer_LL_destroy(cells_to_be_merged)
        do i = 1,nb_cell !First : detect all regions needing some fusion, and list the possible neighbours
          nullify(target_cells(i, k)%ptr)
          narrowBand = ((((0 < grid(i,k)%lambdan_per_cell / grid(i,k)%area) .and. &
                            (grid(i,k)%lambdan_per_cell / grid(i,k)%area < threshold)) .or. &
                          ((0 < grid(i,k)%lambdanp1_per_cell / grid(i, k)%area) .and. &
                            (grid(i,k)%lambdanp1_per_cell / grid(i, k)%area < threshold))) .and. &
                          grid(i, k)%is_narrowband)
          if (narrowBand) then
              call integer_LL_insert_unique(cells_to_be_merged, i)
              cell_type(i, k) = PROBLEMATIC_UNSOLVED
              nb_edges = max_nb_edges_in_cell(NUMELQ, NUMELTG)
              do j = 1,nb_edges
                if (grid(i, k)%lambda_per_edge(j) > 0) then
                  call adjacency_edge(ALE_CONNECT, i, j, other_face, other_edge) 
                  if  (other_face > 0) then
                    if ((grid(other_face, k)%lambdan_per_cell > 0.0) .or. (grid(other_face, k)%lambdanp1_per_cell > 0.0)) then
                      call integer_LL_insert_unique(target_cells(i, k)%ptr, other_face)
                    end if
                  end if
                end if
              end do
          else
              cell_type(i, k) = NOT_FUSED
              call integer_LL_insert_after(target_cells(i, k)%ptr, i)
          end if
        end do

        !Do fusion: choose one neighbour among acceptable ones (acceptable /= possible), treat pathologic cases, do it again and again until nothing happens.
        global_fusion_happened = .true.
        do while (global_fusion_happened)
          global_fusion_happened = .false.

          fusion_happened = .true.
          do while (fusion_happened)
            fusion_happened = .false.
            call integer_LL_copy(cells_to_be_merged, copy_to_be_merged)

            !Iterate over copy_to_be_merged
            curr_ptr_int => copy_to_be_merged
            do while (associated(curr_ptr_int))
              !Build targ_c, a list of acceptable neighbours among possible ones.
              call integer_LL_destroy(targ_c)
              size_list = 0
              c = curr_ptr_int%val
              if (associated(target_cells(c, k)%ptr)) then
                curr_targ => target_cells(c, k)%ptr
                do while (associated(curr_targ))
                  if ((cell_type(curr_targ%val, k) == NOT_FUSED .and. &
                        ((grid(curr_targ%val, k)%lambdan_per_cell >= threshold*grid(curr_targ%val, k)%area) .or. &
                        (grid(curr_targ%val, k)%lambdanp1_per_cell >= threshold*grid(curr_targ%val, k)%area))) &
                        .or. (cell_type(curr_targ%val, k) == FUSED)) then
                    call integer_LL_insert_after(targ_c, curr_targ%val)
                    size_list = size_list + 1
                  end if
                  curr_targ => curr_targ%next
                end do
              end if

              !if targ_c is not empty: fuse with the biggest Lambda.
              if (size_list > 0) then
                curr_targ => targ_c
                max_lambda = grid(curr_targ%val, k)%lambdan_per_cell / grid(curr_targ%val, k)%area
                i_max_lambd = curr_targ%val
                curr_targ => curr_targ%next
                do while(associated(curr_targ)) !looking for maximum Lambda
                  if (max_lambda < grid(curr_targ%val, k)%lambdan_per_cell / grid(curr_targ%val, k)%area) then
                    max_lambda = grid(curr_targ%val, k)%lambdan_per_cell / grid(curr_targ%val, k)%area
                    i_max_lambd = curr_targ%val
                  end if
                  curr_targ => curr_targ%next
                end do

                call integer_LL_destroy(target_cells(c, k)%ptr)
                call integer_LL_insert_after(target_cells(c, k)%ptr, i_max_lambd)

                call integer_LL_delete_value(cells_to_be_merged, c)
                cell_type(c, k) = FUSED
                cell_type(i_max_lambd, k) = FUSED
                fusion_happened = .true.
                global_fusion_happened = .true.
              end if
            
              curr_ptr_int => curr_ptr_int%next
            end do
          end do

          do while (integer_LL_size(cells_to_be_merged) > 0) !Problem : some cells can't be merged with a cell big enough.
            rand_cell = integer_LL_pop(cells_to_be_merged)
            call integer_LL_destroy(target_cells(rand_cell, k)%ptr)
            call integer_LL_insert_after(target_cells(rand_cell, k)%ptr, rand_cell)
            cell_type(rand_cell, k) = PROBLEMATIC

            fusion_happened = .true.
            do while (fusion_happened)
              fusion_happened = .false.
              call integer_LL_destroy(copy_to_be_merged)
              call integer_LL_copy(cells_to_be_merged, copy_to_be_merged)

              curr_ptr_int => copy_to_be_merged
              do while (associated(curr_ptr_int))
                !Build targ_c, a list of acceptable neighbours among possible ones.
                call integer_LL_destroy(targ_c)
                size_list = 0
                c = curr_ptr_int%val
                curr_targ => target_cells(c, k)%ptr
                do while (associated(curr_targ))
                  if (cell_type(curr_targ%val, k) == PROBLEMATIC) then
                    call integer_LL_insert_after(targ_c, curr_targ%val)
                    size_list = size_list + 1
                  end if
                  curr_targ => curr_targ%next
                end do

                !if targ_c is not empty: fuse with the biggest Lambda.
                if (size_list > 0) then
                  curr_targ => targ_c
                  max_lambda = grid(curr_targ%val, k)%lambdan_per_cell / grid(curr_targ%val, k)%area
                  i_max_lambd = curr_targ%val
                  curr_targ => curr_targ%next
                  do while(associated(curr_targ)) !looking for maximum Lambda
                    if (max_lambda < grid(curr_targ%val, k)%lambdan_per_cell / grid(curr_targ%val, k)%area) then
                      max_lambda = grid(curr_targ%val, k)%lambdan_per_cell / grid(curr_targ%val, k)%area
                      i_max_lambd = curr_targ%val
                    end if
                    curr_targ => curr_targ%next
                  end do

                  call integer_LL_destroy(target_cells(c, k)%ptr)
                  call integer_LL_insert_after(target_cells(c, k)%ptr, i_max_lambd)
                  call integer_LL_delete_value(cells_to_be_merged, c)
                  cell_type(c, k) = PROBLEMATIC
                  fusion_happened = .true.
                end if
                curr_ptr_int => curr_ptr_int%next
              end do
            end do
          end do

          do i = 1,nb_cell
              !final_target_cells(i, k) = integer_LL_pop(target_cells(i, k)%ptr)
              final_target_cells(i, k) = target_cells(i, k)%ptr%val
          end do

          do i = 1,nb_cell
            j = final_target_cells(i, k)
            do while (j /= final_target_cells(j, k))
                j = final_target_cells(j, k)
            end do
            final_target_cells(i, k) = j
            if ((i == j) .and. (cell_type(i, k) == FUSED)) then
                cell_type(i, k) = TARGET_FUSED
            end if
          end do
        end do
      end do

      !Deallocate all temp variables
      do k = 1,nb_regions 
        do i = 1,nb_cell
          call integer_LL_destroy(target_cells(i,k)%ptr)
        enddo
      enddo
      deallocate(target_cells)

      call integer_LL_destroy(targ_c)
      call integer_LL_destroy(cells_to_be_merged)
    end subroutine fuse_cells

    !! Compute flux between two adjacent cells
    subroutine multicutcell_compute_fluxes(NUMELQ, NUMELTG, IXQ, IXTG, X, ALE_CONNECT, grid, target_cells, gamma, &
                                            rho, vely, velz, p, fx)
      use grid2D_struct_multicutcell_mod
      use ALE_CONNECTIVITY_MOD

      implicit none

      !Dummy arguments
      integer, intent(in) :: NUMELQ, NUMELTG
      integer, dimension(:,:), intent(in) :: IXQ, IXTG
      real(kind=wp), dimension(:,:), intent(in) :: X
      TYPE(t_ale_connectivity), INTENT(IN) :: ALE_CONNECT
      type(grid2D_struct_multicutcell), dimension(:, :), intent(in) :: grid
      integer(kind=8), dimension(:, :), intent(in) :: target_cells
      real(kind=wp), dimension(:), INTENT(IN) :: gamma
      real(kind=wp), dimension(:,:), intent(in) :: vely
      real(kind=wp), dimension(:,:), intent(in) :: velz
      real(kind=wp), dimension(:,:), intent(in) :: rho
      real(kind=wp), dimension(:,:), intent(in) :: p
      type(ConservativeFlux2D), dimension(:, :), intent(out) :: fx

      !Local variables
      integer(kind=8) :: i, j, k
      integer(kind=8) :: nb_cell, nb_regions, nb_edges
      integer(kind=8) :: other_edge, other_face
      type(Point2D), dimension(4) :: normals
      integer(kind=8), dimension(:), allocatable :: fused_i

      nb_cell = size(vely, 1)
      nb_regions = size(vely, 2)

      allocate(fused_i(nb_regions))

      do i=1,nb_cell
        call multicutcell_compute_normals(NUMELQ, NUMELTG, IXQ, IXTG, X, i, normals, nb_edges)
        do k = 1,nb_regions
          if ((grid(i,k)%lambdan_per_cell > 1e-16))  then
            fused_i(k) = i
          else
            fused_i(k) = target_cells(i, k)
          end if
        end do

        do j=1,nb_edges
          call adjacency_edge(ALE_CONNECT, i, j, other_face, other_edge) 
          do k = 1,nb_regions
            fx(i, k)%rho(j) = 0.
            fx(i, k)%rhovy(j) = 0.
            fx(i, k)%rhovz(j) = 0.
            fx(i, k)%rhoE(j) = 0.
            if (other_face > 0) then
              call FV_flux_hllc_Euler(gamma(k), rho(fused_i(k), k), rho(other_face, k), &
                                  vely(fused_i(k), k), vely(other_face, k), velz(fused_i(k), k), velz(other_face, k), &
                                  p(fused_i(k), k), p(other_face, k), &
                                  normals(j), &
                                  fx(i, k)%rho(j), fx(i, k)%rhovy(j), fx(i, k)%rhovz(j), fx(i, k)%rhoE(j))
            else !There is no neighbour!
              !Exact flux
              fx(i, k)%rho(j) = 0.
              fx(i, k)%rhovy(j) = p(i, k)*normals(j)%y
              fx(i, k)%rhovz(j) = p(i, k)*normals(j)%z
              fx(i, k)%rhoE(j) = 0.
            end if
          end do
        end do
      end do
    end subroutine multicutcell_compute_fluxes
  
    !! Correction on fluxed on the boundary
    subroutine multicutcell_compute_fluxes_boundary(NUMELQ, NUMELTG, IXQ, IXTG, X, &
                                                    gamma, rho, vely, velz, p, ebcs_tab, fx)
      use grid2D_struct_multicutcell_mod
      use ALE_CONNECTIVITY_MOD
      use ebcs_mod !, only : t_ebcs_tab, t_ebcs

      implicit none
      !Dummy arguments
      integer, intent(in) :: NUMELQ, NUMELTG
      integer, dimension(:,:), intent(in) :: IXQ, IXTG
      real(kind=wp), dimension(:,:), intent(in) :: X
      real(kind=wp), dimension(:), INTENT(IN) :: gamma
      real(kind=wp), dimension(:,:), intent(in) :: vely
      real(kind=wp), dimension(:,:), intent(in) :: velz
      real(kind=wp), dimension(:,:), intent(in) :: rho
      real(kind=wp), dimension(:,:), intent(in) :: p
      type(t_ebcs_tab), target, intent(in) :: ebcs_tab              !< data structure for user boundary conditions
      type(ConservativeFlux2D), dimension(:, :), intent(out) :: fx

      !Local variables
      integer :: IBC, k, i_edge, nb_regions, NELEM, ielem
      integer(kind=8) :: ii, jj, nb_edges
      class (t_ebcs), pointer :: EBCS !< pointer to ebcs data structure (in order to retrieve list of elems and related faces)
      integer :: ebcs_ityp !< boundary condition type
      real(kind=wp) :: rhoii, velyii, velzii, pii
      real(kind=wp) :: rhojj, velyjj, velzjj, pjj
      real(kind=wp) :: gammaii
      type(Point2D), dimension(4) :: normals
      type(Point2D) :: normal

      nb_regions = size(vely, 2)

      DO k=1,nb_regions
        gammaii = gamma(k)
        DO IBC = 1, EBCS_TAB%nebcs_fvm
          EBCS => EBCS_TAB%tab(IBC)%poly
          ebcs_ityp = EBCS%type  ! ebcs can be identified by member %type (9) or by its class 'TYPE IS(t_ebcs_fluxout)'
          NELEM = EBCS%nb_elem ! number of element to treat (elems which have a user BC)

          SELECT TYPE (twf => EBCS)
            TYPE IS(t_ebcs_fluxout)
              DO ielem=1,NELEM
                ii = twf%ielem(ielem)
                rhoii = rho(ii, k)
                velyii = vely(ii, k)
                velzii = velz(ii, k)
                pii = p(ii, k)
                i_edge = EBCS%iface(ielem)
                call multicutcell_compute_normals(NUMELQ, NUMELTG, IXQ, IXTG, X, ii, normals, nb_edges)
                normal = normals(i_edge)

                jj = twf%ielem(ielem)
                rhojj = rho(jj, k)
                velyjj = vely(jj, k)
                velzjj = velz(jj, k)
                pjj = p(jj, k)

                call FV_flux_hllc_Euler(gammaii, rhoii, rhojj, &
                                    velyii, velyjj, velzii, velzjj, pii, pjj, normal, &
                                    fx(ii, k)%rho(i_edge), fx(ii, k)%rhovy(i_edge), &
                                    fx(ii, k)%rhovz(i_edge), fx(ii, k)%rhoE(i_edge))
              ENDDO
            TYPE IS(t_ebcs_nrf)
              DO ielem=1,NELEM
                ii = twf%ielem(ielem)
                rhoii = rho(ii, k)
                velyii = vely(ii, k)
                velzii = velz(ii, k)
                pii = p(ii, k)
                i_edge = EBCS%iface(ielem)
                call multicutcell_compute_normals(NUMELQ, NUMELTG, IXQ, IXTG, X, ii, normals, nb_edges)
                normal = normals(i_edge)

                jj = twf%ielem(ielem)
                rhojj = rho(jj, k)
                pjj = p(jj, k)
                velyjj = -(vely(jj, k)*normal%y + velz(jj, k)*normal%z)*normal%y &
                         - (-vely(jj, k)*normal%z + velz(jj, k)*normal%y)*normal%z
                velzjj = -(vely(jj, k)*normal%y + velz(jj, k)*normal%z)*normal%z &
                         + (-vely(jj, k)*normal%z + velz(jj, k)*normal%y)*normal%y

                call FV_flux_hllc_Euler(gammaii, rhoii, rhojj, &
                                    velyii, velyjj, velzii, velzjj, pii, pjj, normal, &
                                    fx(ii, k)%rho(i_edge), fx(ii, k)%rhovy(i_edge), &
                                    fx(ii, k)%rhovz(i_edge), fx(ii, k)%rhoE(i_edge))
              ENDDO
            TYPE IS (t_ebcs_inlet)
              write(6,*) 'MULTI_EBCS: Inlet EBCS not yet implemented'
              write(6,*) "NUMELQ=",NUMELQ,"NUMELTG=",NUMELTG
              CALL ARRET(2)
            TYPE IS(t_ebcs_propellant)
              write(6,*) 'MULTI_EBCS: Propellant EBCS not yet implemented'
              write(6,*) "NUMELQ=",NUMELQ,"NUMELTG=",NUMELTG
              CALL ARRET(2)
            class default
              DO ielem=1,NELEM
                ii = twf%ielem(ielem)
                rhoii = rho(ii, k)
                velyii = vely(ii, k)
                velzii = velz(ii, k)
                pii = p(ii, k)
                i_edge = EBCS%iface(ielem)
                call multicutcell_compute_normals(NUMELQ, NUMELTG, IXQ, IXTG, X, ii, normals, nb_edges)
                normal = normals(i_edge)

                jj = twf%ielem(ielem)
                rhojj = rho(jj, k)
                pjj = p(jj, k)
                velyjj = -(vely(jj, k)*normal%y + velz(jj, k)*normal%z)*normal%y &
                         - (-vely(jj, k)*normal%z + velz(jj, k)*normal%y)*normal%z
                velzjj = -(vely(jj, k)*normal%y + velz(jj, k)*normal%z)*normal%z &
                         + (-vely(jj, k)*normal%z + velz(jj, k)*normal%y)*normal%y

                call FV_flux_hllc_Euler(gammaii, rhoii, rhojj, &
                                    velyii, velyjj, velzii, velzjj, pii, pjj, normal, &
                                    fx(ii, k)%rho(i_edge), fx(ii, k)%rhovy(i_edge), &
                                    fx(ii, k)%rhovz(i_edge), fx(ii, k)%rhoE(i_edge))
              ENDDO
          END SELECT

        ENDDO
      ENDDO
    end subroutine multicutcell_compute_fluxes_boundary
  
    !! Identify in which cell is each point of the polygonal interface.
    subroutine compute_all_id_pt_cell(NUMELQ, NUMELTG, IXQ, IXTG, X, grid, nb_pts_clipped, id_pt_cell)
      use grid2D_struct_multicutcell_mod
      use polygon_cutcell_mod
      USE CONSTANT_MOD , ONLY : EM8
  
      IMPLICIT NONE
  
      ! INPUT argument
      integer :: NUMELQ, NUMELTG
      integer, dimension(:,:) :: IXQ, IXTG
      real(kind=wp), dimension(:,:) :: X
      type(grid2D_struct_multicutcell), dimension(:, :) :: grid
      integer(kind=8) :: nb_pts_clipped
      ! OUTPUT argument
      integer(kind=8), dimension(:) :: id_pt_cell

      !Dummy arguments
      type(Point2D), dimension(4) :: normals
      type(Point2D) :: pt, pt_grid
      real(kind=wp) :: d
      integer(kind=8) :: nb_normals
      integer(kind=8) :: nb_cell
      integer(kind=8) :: i, j, k
      logical:: is_inside

      nb_cell = NUMELQ+NUMELTG !size(vely, 1)
      id_pt_cell(:) = -1
      do i = 1,nb_pts_clipped
        call get_clipped_ith_vertex_fortran(i, pt) 
        is_inside = .true.

        do j = 1,nb_cell
          if (any(grid(j, :)%close_cells)) then
            call multicutcell_compute_normals(NUMELQ, NUMELTG, IXQ, IXTG, X, j, normals, nb_normals)

            is_inside = .true.
            do k=1,nb_normals
              pt_grid%y = X(2, IXQ(k+1, j))
              pt_grid%z = X(3, IXQ(k+1, j))
              d = normals(k)%y*(pt_grid%y-pt%y) + normals(k)%z*(pt_grid%z-pt%z)
              !if ((abs(pt%y - pt_grid%y)<epsilon(d)) .and. ((abs(pt%z - pt_grid%z)<epsilon(d)))) then
              !  is_inside = .true.
              !  exit
              !end if
              !d = d/sqrt(normals(k)%y*normals(k)%y + normals(k)%z*normals(k)%z)
              if (d<0) then
                is_inside = .false.
                exit
              endif
            enddo

            if (is_inside) then
              id_pt_cell(i) = j
              exit
            endif
          endif
        enddo
      
      end do

    end subroutine compute_all_id_pt_cell

    !! Compute the advection velocity field for each point of the polygonal interface.
    subroutine compute_vec_move_clipped(gamma, rho, vely, velz, p, nb_pts_clipped, id_pt_cell,&
                                      normalVecy, normalVecz, &
                                      vec_move_clippedy, vec_move_clippedz, wave_type)
      use polygon_cutcell_mod
      use riemann_solver_mod

      implicit none
      real(kind=wp), dimension(:) :: gamma
      real(kind=wp), dimension(:,:)       :: rho
      real(kind=wp), dimension(:,:)       :: vely
      real(kind=wp), dimension(:,:)       :: velz
      real(kind=wp), dimension(:,:)       :: p
      integer(kind=8)               :: nb_pts_clipped
      integer(kind=8), dimension(:) :: id_pt_cell
      real(kind=wp), dimension(:)         :: normalVecy
      real(kind=wp), dimension(:)         :: normalVecz
      real(kind=wp), dimension(:)         :: vec_move_clippedy
      real(kind=wp), dimension(:)         :: vec_move_clippedz
      integer, intent(in) :: wave_type

      integer(kind=8) :: eL, eR, k, i
      type(Point2D) :: pt
      real(kind=wp) :: rhoL, velyL, velzL, pL
      real(kind=wp) :: rhoR, velyR, velzR, pR
      real(kind=wp) :: us, vsL, vsR, ps

      do k = 1,nb_pts_clipped
        call get_clipped_ith_vertex_fortran(k, pt)
        call get_clipped_edges_ith_vertex_fortran(k, eR, eL) 
        eR = eR+1 !C is 0-indexed, Fortran is 1 indexed...
        eL = eL+1
        if ((eR>0) .and. (eL>0)) then !point k is part of an edge
          i = id_pt_cell(k)
          rhoL = rho(i, 1)
          velyL = vely(i, 1)
          velzL = velz(i, 1)
          pL = p(i, 1)
          rhoR = rho(i, 2)
          velyR = vely(i, 2)
          velzR = velz(i, 2)
          pR = p(i, 2)

          call solve_riemann_problem(gamma(1), gamma(2), rhoL, rhoR, velyL, velyR, velzL, velzR, pL, pR, wave_type, &
                                      normalVecy(k), normalVecz(k), &
                                      us, vsL, vsR, ps)
        
          vec_move_clippedy(k) = us * normalVecy(k) - 0.5 * (vsL + vsR) * normalVecz(k) !Choice of the mean of left and right tangential velocities, another choice could be made!
          vec_move_clippedz(k) = us * normalVecz(k) + 0.5 * (vsL + vsR) * normalVecy(k) !Choice of the mean of left and right tangential velocities, another choice could be made!
        else
            vec_move_clippedy(k) = 0.0
            vec_move_clippedz(k) = 0.0
        end if
      end do
  
      !return vec_move_clipped
    end subroutine compute_vec_move_clipped
 end subroutine update_fluid_multicutcell

!! \brief Allocate and initialize structures used for multiphase simulation.
!! \details (y_polygon, z_polygon) are the coordinates of successive points forming the polygonal interface.
  subroutine initialize_solver_multicutcell(N2D, NUMELQ, NUMELTG, NUMNOD, IXQ, IXTG, ngroup, nparg, iparg, X, elbuf, & !gamma, &
                              nb_id_polygon, list_id_polygon, ngrnod, igrnode, & !multi_cutcell, 
                              grid, num_mixed,list_mixed)
    use grid2D_struct_multicutcell_mod
    use groupdef_mod , only : group_
    use polygon_cutcell_mod, only : Point3D
    use multi_cutcell_mod, only : multi_cutcell_struct
    use elbufdef_mod

    implicit none
    integer, intent(in) :: N2D, NUMELQ, NUMELTG, NUMNOD
    integer, intent(in), dimension(:,:) :: IXQ, IXTG
    integer,intent(in) :: nparg  !< size IPARG
    integer,intent(in) :: ngroup !< number of group
    integer,intent(in) :: iparg(nparg,ngroup)
    real(kind=wp), intent(in), dimension(:,:) :: X
    type(elbuf_struct_),dimension(:) :: elbuf
    !real(kind=wp), dimension(:), intent(in) :: gamma
    integer(kind=8), intent(in) :: nb_id_polygon
    integer, intent(in) :: list_id_polygon(nb_id_polygon)
    integer,intent(in) :: ngrnod                                  !< number of group of nodes(array size for igrnod)
    type(group_), dimension(ngrnod), intent(in)  :: igrnode
    !type(multi_cutcell_struct), intent(inout) :: multi_cutcell        !< element buffer (storage for output files : pressure, density, velocity, ...)
    type(grid2D_struct_multicutcell), dimension(:, :), allocatable :: grid 
    integer,intent(in) :: num_mixed,list_mixed(num_mixed)
    
    !DUMMY ARGUMENTS
    integer nb_cell, nb_regions, nb_edges
    integer(kind=8) i, k, j, nb_pts_poly, old_nb_pts_poly, nb_edges_poly
    real(kind=wp), dimension(:), allocatable :: vec_move_clippedy, vec_move_clippedz
    real(kind=wp) :: dt, dx, minimal_length, minimal_angle, maximal_length
    real(kind=wp), dimension(:), allocatable :: y_polygon, z_polygon
    integer(kind=8) :: limits_polygon(nb_id_polygon + 1)
    integer :: polyg_id, nb_pts
    integer, dimension(:), allocatable :: entities
    real(kind=wp) :: x_pt1, x_pt2, y_pt1, y_pt2
    integer :: ng, nel, nft, elem_iid
    
    if (N2D < 0) then
      print *, "Error: no 2D?"
      !stop
    end if

    if (NUMELQ>0) then
      nb_edges = 4
    else
      nb_edges = 3
    end if

    dt = 1.0

    nb_cell = NUMELQ+NUMELTG !size(vely, 1)
    nb_regions = 2
    allocate(grid(nb_cell, nb_regions))

    !Create grid, initialize volume fraction and identify cut cells
    call launch_grb() !C call
    do i=1,nb_cell
      do k=1,nb_regions
        grid(i,k) = makegrid(NUMELQ, NUMELTG, IXQ, IXTG, X, i )
      end do
    end do

    do ng=1,ngroup
      nel = iparg(2,ng)
      nft = iparg(3,ng) ! shift
      do i=1,nel
        elem_iid = i+nft
        do k=1,nb_regions
          grid(elem_iid,k)%lambdan_per_cell = elbuf(ng)%bufly(k)%lbuf(1,1,1)%vol(i)
          grid(elem_iid,k)%lambdanp1_per_cell = elbuf(ng)%bufly(k)%lbuf(1,1,1)%vol(i)

          if (grid(elem_iid,k)%lambdan_per_cell > 0.0) then 
            do j=1,nb_edges-1
              x_pt1 = X(2, IXQ(2+j-1, elem_iid))
              x_pt2 = X(2, IXQ(2+j, elem_iid))
              y_pt1 = X(3, IXQ(2+j-1, elem_iid))
              y_pt2 = X(3, IXQ(2+j, elem_iid))
              grid(elem_iid,k)%lambda_per_edge(j) = sqrt((x_pt2-x_pt1)*(x_pt2-x_pt1) + (y_pt2-y_pt1)*(y_pt2-y_pt1))
            end do
            j=nb_edges
            x_pt1 = X(2, IXQ(2+j-1, elem_iid))
            x_pt2 = X(2, IXQ(2, elem_iid))
            y_pt1 = X(3, IXQ(2+j-1, elem_iid))
            y_pt2 = X(3, IXQ(2, elem_iid))
            grid(elem_iid,k)%lambda_per_edge(j) = sqrt((x_pt2-x_pt1)*(x_pt2-x_pt1) + (y_pt2-y_pt1)*(y_pt2-y_pt1))
          else
            grid(elem_iid,k)%lambda_per_edge(:) = 0.0
          end if
        end do
      end do
    end do
    
    do j=1,num_mixed
      i = list_mixed(j)
      do k=1,nb_regions
        grid(i,k)%is_narrowband = .true.
        grid(i,k)%close_cells = .true.
      end do
    end do

    !Create polygon
    if (nb_id_polygon > 0) then
      nb_pts_poly = 0
      do i=1,nb_id_polygon
        polyg_id = list_id_polygon(i)
        nb_pts_poly = nb_pts_poly + igrnode(polyg_id)%nentity
      end do
      allocate(y_polygon(nb_pts_poly))
      allocate(z_polygon(nb_pts_poly))

      limits_polygon(1) = 0
      do i=1,nb_id_polygon
        polyg_id = list_id_polygon(i)
        nb_pts = igrnode(polyg_id)%nentity
        allocate(entities(nb_pts))
        entities = igrnode(polyg_id)%entity(1:nb_pts)
        limits_polygon(i+1) = limits_polygon(i) + nb_pts
        y_polygon(limits_polygon(i)+1:limits_polygon(i+1)) = X(2, entities)
        z_polygon(limits_polygon(i)+1:limits_polygon(i+1)) = X(3, entities)
        deallocate(entities)
      end do
    end if

    call build_clipped_from_pts_fortran(y_polygon, z_polygon, limits_polygon, nb_id_polygon)
    i = 0
    call print_clipped_fortran(i)
    
    !Update polygon to meet length criteria
    allocate(vec_move_clippedy(nb_pts_poly))
    allocate(vec_move_clippedz(nb_pts_poly))
    vec_move_clippedy(:) = 0.
    vec_move_clippedz(:) = 0.
    
    dx = sqrt(minval(grid(:, 1)%area))
    minimal_length = 0.5*dx
    minimal_angle = -1._wp
    maximal_length = dx

    call nb_pts_clipped_fortran(old_nb_pts_poly)
    call update_clipped_fortran(vec_move_clippedy, vec_move_clippedz, dt, nb_edges_poly, &
                                minimal_length, maximal_length, minimal_angle) !initialize clipped3D in C.
    call print_clipped_fortran(i)
    call nb_pts_clipped_fortran(nb_pts_poly)
    do while (nb_pts_poly /= old_nb_pts_poly)
      old_nb_pts_poly = nb_pts_poly
      deallocate(vec_move_clippedy)
      deallocate(vec_move_clippedz)
      allocate(vec_move_clippedy(nb_pts_poly))
      allocate(vec_move_clippedz(nb_pts_poly))
      vec_move_clippedy(:) = 0.
      vec_move_clippedz(:) = 0.

      call update_clipped_fortran(vec_move_clippedy, vec_move_clippedz, dt, nb_edges_poly, &
                                  minimal_length, maximal_length, minimal_angle)
      call nb_pts_clipped_fortran(nb_pts_poly)
    end do
    call print_clipped_fortran(i)
    
    !Identify cells close to the interface
    call compute_close_cells(NUMELQ, NUMELTG, NUMNOD, IXQ, IXTG, grid)
    !write(*,*) "Computing lambdas for the first time: this may take a while..."
    !call multicutcell_compute_lambdas(NUMELQ, NUMELTG, NUMNOD, IXQ, IXTG, X, grid, dt,&
    !                                  multi_cutcell%phase_rho, multi_cutcell%phase_vely,&
    !                                  multi_cutcell%phase_velz, multi_cutcell%phase_pres, gamma) !initialize lambda fields, close_cell and is_narrowband in grid

    deallocate(vec_move_clippedy)
    deallocate(vec_move_clippedz)
    deallocate(y_polygon)
    deallocate(z_polygon)
  end subroutine initialize_solver_multicutcell

!! \brief Deallocate some C structures used for multiphase simulation.
  subroutine deallocate_solver_multicutcell()
    call end_grb() !C call
  end subroutine deallocate_solver_multicutcell

!! \brief List all points of the clipped polygon into y_polygon and z_polygon arrays.
!! \details The limits_polygons array gives the start index of each polygon in the clipped solid. 
!!          Its size is nb_polygons+1, with limits_polygons[1]=0 and limits_polygons[nb_polygons+1]=total number of points in the clipped solid.
!!          For polygon i, its points are stored from index limits_polygons[i]+1 to limits_polygons[i+1] in the y_polygon and z_polygon arrays.
!! \param y_polygon y coordinates of clipped polygon points
!! \param z_polygon z coordinates of clipped polygon points
!! \param limits_polygons Index limits of each polygon in the clipped solid
  subroutine output_clipped(nb_polygon, limits_polygon, y_polygon, z_polygon)
    implicit none
    integer(kind=8), intent(out) :: nb_polygon
    integer(kind=8), dimension(:), allocatable :: limits_polygon
    real(kind=wp), dimension(:), allocatable :: y_polygon
    real(kind=wp), dimension(:), allocatable :: z_polygon

    integer(kind=8) :: nb_pts

    call nb_edge_clipped_fortran(nb_polygon)
    call nb_pts_clipped_fortran(nb_pts)
    
    if(allocated(y_polygon)) deallocate(y_polygon)
    if(allocated(z_polygon)) deallocate(z_polygon)
    if(allocated(limits_polygon)) deallocate(limits_polygon)

    allocate(y_polygon(nb_pts))
    allocate(z_polygon(nb_pts))
    allocate(limits_polygon(nb_polygon + 1))

    call output_clipped_fortran(y_polygon, z_polygon, limits_polygon)
  end subroutine output_clipped

   !initial state
  subroutine multicutcell_initial_state(ngroup, elbuf, nparg, iparg, IGRQUAD, multi_cutcell)
    use elbufdef_mod
    use multi_cutcell_mod, only : multi_cutcell_struct
    use ale_mod , only : ALE
    use groupdef_mod , only : GROUP_

    implicit none

    !DUMMY ARGUMENTS
    integer, intent(in) :: ngroup,nparg,iparg(nparg,ngroup)
    type(elbuf_struct_),dimension(ngroup) :: elbuf
    TYPE(GROUP_),INTENT(IN),DIMENSION(:) :: IGRQUAD
    type(multi_cutcell_struct), intent(inout) :: multi_cutcell        !< element buffer (storage for output files : pressure, density, velocity, ...)

    !LOCAL VARIABLE
    integer :: imat,elem_iid,nft,nel,ii,ng,mlw

    do ng=1,ngroup
      nel = iparg(2,ng)
      nft = iparg(3,ng) ! shift
      mlw = iparg(1,ng)
      if(mlw /= 20)cycle
      do ii=1,nel
        elem_iid = ii+nft
        do imat=1,2
          multi_cutcell%phase_rho(elem_iid,imat) = elbuf(ng)%BUFLY(imat)%LBUF(1,1,1)%rho(ii)
          multi_cutcell%phase_pres(elem_iid,imat) = -elbuf(ng)%BUFLY(imat)%LBUF(1,1,1)%sig(ii)
        end do
      end do
    enddo

    mlw = ALE%CUTCELL%NINIVEL_CUT_CELL
    DO ii=1,mlw
      nft = ALE%CUTCELL%FVM_VEL(ii)%grquadid
      imat = ALE%CUTCELL%FVM_VEL(ii)%isubmat
      nel = IGRQUAD(nft)%nentity
      DO ng=1,nel
        elem_iid = IGRQUAD(nft)%entity(ng)
        multi_cutcell%phase_vely(elem_iid, imat) = ALE%CUTCELL%FVM_VEL(ii)%vy
        multi_cutcell%phase_velz(elem_iid, imat) = ALE%CUTCELL%FVM_VEL(ii)%vz
      END DO
    END DO

  end subroutine multicutcell_initial_state


  !identify mixed cells
  subroutine multicutcell_mixed_cell(ngroup, elbuf, nparg, iparg, numelq, num_mixed, list_mixed)
    use elbufdef_mod
    use precision_mod , only : WP
    use constant_mod , only : em10, one, zero

    implicit none

    !DUMMY ARGUMENTS
    integer, intent(in) :: ngroup,nparg,iparg(nparg,ngroup),numelq
    integer,intent(inout) :: num_mixed
    integer,intent(inout),dimension(:),allocatable :: list_mixed
    type(elbuf_struct_),dimension(ngroup) :: elbuf

    !LOCAL VARIABLE
    integer :: imat,elem_iid,nft,nel,ii,ng,mlw
    real(kind=WP) :: subvol, vol, tol, bound1, bound2
    integer :: itag(numelq), kk
    real(kind=WP) :: vf
    integer,dimension(numelq) :: list

    itag(1:numelq)=0

    tol=em10
    bound1= tol
    bound2= one-tol
    num_mixed = 0
    do ng=1,ngroup
      nel = iparg(2,ng)
      nft = iparg(3,ng) ! shift
      mlw = iparg(1,ng)
      if(mlw /= 20)cycle
      do ii=1,nel
        elem_iid = ii+nft
        subvol = elbuf(ng)%BUFLY(1)%LBUF(1,1,1)%vol(ii)
        vol = elbuf(ng)%gbuf%vol(ii)
        vf = subvol / vol
        if(vf > bound1 .and. vf < bound2)then
          itag(ii+nft) = 1
          num_mixed = num_mixed +1
        end if
      end do
    enddo

    allocate(list_mixed(num_mixed))
    kk = 0
    do ii=1,numelq
      if(itag(ii)==1)then
        kk = kk +1
        list_mixed(kk) = ii
      end if
    end do

  end subroutine multicutcell_mixed_cell


end module multicutcell_solver_mod

#undef NOT_FUSED 
#undef FUSED
#undef TARGET_FUSED
#undef PROBLEMATIC
#undef PROBLEMATIC_UNSOLVED