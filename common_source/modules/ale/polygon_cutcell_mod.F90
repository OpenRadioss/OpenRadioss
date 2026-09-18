module polygon_cutcell_mod

  use precision_mod  , only : wp

  type Point2D
    SEQUENCE 
    real(kind=wp) :: y
    real(kind=wp) :: z
  end type Point2D

  type Point3D
    SEQUENCE 
    real(kind=wp) :: y
    real(kind=wp) :: z
    real(kind=wp) :: t
  end type Point3D
end module polygon_cutcell_mod
