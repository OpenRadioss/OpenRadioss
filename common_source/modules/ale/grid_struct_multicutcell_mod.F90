module grid2D_struct_multicutcell_mod
  use polygon_cutcell_mod, only : Point2D
  use precision_mod  , only : wp

  type grid2D_struct_multicutcell
    logical                     :: close_cells !Determine whether a cell is close to the interface
    logical                     :: is_narrowband
    !real(kind=wp), dimension(4)       :: length_edge
    real(kind=wp), dimension(4)       :: lambda_per_edge
    real(kind=wp)                     :: lambdan_per_cell
    real(kind=wp)                     :: lambdan_per_cell_target
    real(kind=wp)                     :: lambdanp1_per_cell
    real(kind=wp)                     :: lambdanp1_per_cell_target
    type(Point2D)               :: normal_intern_face_space
    real(kind=wp)                     :: normal_intern_face_time
    type(Point2D)               :: p_normal_intern_face_space
    real(kind=wp)                     :: p_normal_intern_face_time
    real(kind=wp)                     :: area
  end type grid2D_struct_multicutcell

  type ConservativeState2D
    real(kind=wp) :: rho
    real(kind=wp) :: rhoE
    real(kind=wp) :: rhovz
    real(kind=wp) :: rhovy 
  end type ConservativeState2D

  type ConservativeFlux2D
    real(kind=wp), dimension(4) :: rho
    real(kind=wp), dimension(4) :: rhoE
    real(kind=wp), dimension(4) :: rhovz
    real(kind=wp), dimension(4) :: rhovy 
  end type ConservativeFlux2D

  contains 
  function compute_area(N,X) result(area)  !Area enclosed by polygon P, by the "shoelace" method.
      implicit none
      integer :: N !The number of points.
      real(kind=wp) :: X(2, N) !The points.
      real(kind=wp) :: area

      area = sum(X(1,1:N - 1)*X(2,2:N)) + X(1,N)*X(2,1) &
             - sum(X(1,2:N)*X(2,1:N - 1)) - X(1,1)*X(2,N)
      area = abs(area/2.0)  !The midpoint formula requires a halving.
  end function compute_area

  function makegrid(NUMELQ, NUMELTG, IXQ, IXTG, X, i_cell) result(grid)
    implicit none
    integer :: N2D, NUMELQ, NUMELTG
    integer(kind=8) :: i_cell
    integer, dimension(:,:) :: IXQ, IXTG
    real(kind=wp), dimension(:,:) :: X
    type(grid2D_struct_multicutcell) :: grid

    grid%is_narrowband = .false.
    grid%close_cells = .false.

    if (NUMELQ > 0) then
      grid%area = compute_area(4, X(2:3, IXQ(2:5, i_cell)))
    elseif (NUMELTG>0) then
      grid%area = compute_area(3, X(2:3, IXTG(2:4, i_cell)))
    end if

  end function makegrid

end module grid2D_struct_multicutcell_mod
