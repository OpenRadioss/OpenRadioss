!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2026 Altair Engineering Inc.
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
!! \brief S6RCOOR3_2 - Coordinate transformation for 6-node solid elements
!! \details Transforms nodal coordinates and velocities to corotational frame
!!          for 6-node solid elements (pentahedral elements)

!||====================================================================
!||    s6zrcoor3_mod   ../engine/source/elements/solid/solide6z/s6zrcoor3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3        ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||====================================================================
      module s6zrcoor3_mod
      contains
!||====================================================================
!||    s6zrcoor3        ../engine/source/elements/solid/solide6z/s6zrcoor3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3         ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||--- calls      -----------------------------------------------------
!||    sorthdir3        ../engine/source/elements/solid/solide/sorthdir3.F
!||    sortho3          ../engine/source/elements/solid/solide/sortho3.F
!||    srepiso3         ../engine/source/elements/solid/solide/srepiso3.F
!||    srrota3          ../engine/source/elements/solid/solide/srrota3.F
!||--- uses       -----------------------------------------------------
!||    constant_mod     ../common_source/modules/constant_mod.F
!||    precision_mod    ../common_source/modules/precision_mod.F90
!||    prop_param_mod   ../common_source/modules/mat_elem/prop_param_mod.F90
!||====================================================================
      subroutine s6zrcoor3(                                                   &
        numnod  , x       , ixs     , v       , gama0   , gama    ,           &
        x1      , x2      , x3      , x4      , x5      , x6      ,           &
        y1      , y2      , y3      , y4      , y5      , y6      ,           &
        z1      , z2      , z3      , z4      , z5      , z6      ,           &
        vx1     , vx2     , vx3     , vx4     , vx5     , vx6     ,           &
        vy1     , vy2     , vy3     , vy4     , vy5     , vy6     ,           &
        vz1     , vz2     , vz3     , vz4     , vz5     , vz6     ,           &
        vd2     , vis     , offg    , off     , sav     , rho     , rhoo    , &
        r11     , r12     , r13     , r21     , r22     , r23     , r31     , &
        r32     , r33     ,                                                   &
        nc1     , nc2     , nc3     , nc4     , nc5     , nc6     , ngl     , &
        mxt     , ngeo    , ioutprt , vgax    , vgay    , vgaz    , vga2    , &
        nel     , xgax    , xgay    , xgaz    , xgxa2   , xgya2   , xgza2   , &
        xgxya   , xgyza   , xgzxa   , iparg   , gama_r  , nixs    ,           &
        irep    , ismstr  , isorth  , jlag    ,                               & 
        xd1     , xd2     , xd3     , xd4     , xd5     , xd6     ,           &
        yd1     , yd2     , yd3     , yd4     , yd5     , yd6     ,           &
        zd1     , zd2     , zd3     , zd4     , zd5     , zd6     ,           &
        JHBE    , IDTMIN  , x0      , y0      , z0      ,iresp    ,xdp)  
!
!-------------------------------------------------------------------------------
!   m o d u l e s
!-------------------------------------------------------------------------------
          use precision_mod, only : wp
          use constant_mod , only : zero, one, two, one_over_6, em20
          use prop_param_mod
          use MVSIZ_MOD, only : MVSIZ
!-------------------------------------------------------------------------------
!    i m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
          implicit none
!-------------------------------------------------------------------------------
!    d u m m y   a r g u m e n t s
!-------------------------------------------------------------------------------
      integer,                             intent(in)    :: numnod   !< total number of nodes
      integer,dimension(102) :: idtmin
      integer,                             intent(in)    :: jhbe !< total number of nodes 
      integer,                             intent(in)    :: nixs     !< number of columns in connectivity array
      integer,                             intent(in)    :: irep     !< iteration flag for coordinate system
      integer,                             intent(in)    :: ismstr   !< small strain flag
      integer,                             intent(in)    :: isorth   !< orthotropic material flag  
      integer,                             intent(in)    :: jlag     !< lagrangian flag
      integer,                             intent(in)    :: nel      !< number of elements
      real(kind=wp), dimension(3,numnod),  intent(in)    :: x        !< nodal coordinates
      integer, dimension(nixs,nel),        intent(in)    :: ixs      !< element connectivity
      real(kind=wp), dimension(3,numnod),  intent(in)    :: v        !< nodal velocities
      real(kind=wp), dimension(mvsiz,6),     intent(inout) :: gama     !< material orientation matrix
      real(kind=wp), dimension(nel,6),     intent(in)    :: gama0    !< initial material orientation
      real(kind=wp), dimension(nel),       intent(out)   :: x1       !< local x-coordinates node 1
      real(kind=wp), dimension(nel),       intent(out)   :: x2       !< local x-coordinates node 2
      real(kind=wp), dimension(nel),       intent(out)   :: x3       !< local x-coordinates node 3
      real(kind=wp), dimension(nel),       intent(out)   :: x4       !< local x-coordinates node 4
      real(kind=wp), dimension(nel),       intent(out)   :: x5       !< local x-coordinates node 5
      real(kind=wp), dimension(nel),       intent(out)   :: x6       !< local x-coordinates node 6
      real(kind=wp), dimension(nel),       intent(out)   :: y1       !< local y-coordinates node 1
      real(kind=wp), dimension(nel),       intent(out)   :: y2       !< local y-coordinates node 2
      real(kind=wp), dimension(nel),       intent(out)   :: y3       !< local y-coordinates node 3
      real(kind=wp), dimension(nel),       intent(out)   :: y4       !< local y-coordinates node 4
      real(kind=wp), dimension(nel),       intent(out)   :: y5       !< local y-coordinates node 5
      real(kind=wp), dimension(nel),       intent(out)   :: y6       !< local y-coordinates node 6
      real(kind=wp), dimension(nel),       intent(out)   :: z1       !< local z-coordinates node 1
      real(kind=wp), dimension(nel),       intent(out)   :: z2       !< local z-coordinates node 2
      real(kind=wp), dimension(nel),       intent(out)   :: z3       !< local z-coordinates node 3
      real(kind=wp), dimension(nel),       intent(out)   :: z4       !< local z-coordinates node 4
      real(kind=wp), dimension(nel),       intent(out)   :: z5       !< local z-coordinates node 5
      real(kind=wp), dimension(nel),       intent(out)   :: z6       !< local z-coordinates node 6
      real(kind=wp), dimension(nel),       intent(out)   :: vx1      !< local x-velocities node 1
      real(kind=wp), dimension(nel),       intent(out)   :: vx2      !< local x-velocities node 2
      real(kind=wp), dimension(nel),       intent(out)   :: vx3      !< local x-velocities node 3
      real(kind=wp), dimension(nel),       intent(out)   :: vx4      !< local x-velocities node 4
      real(kind=wp), dimension(nel),       intent(out)   :: vx5      !< local x-velocities node 5
      real(kind=wp), dimension(nel),       intent(out)   :: vx6      !< local x-velocities node 6
      real(kind=wp), dimension(nel),       intent(out)   :: vy1      !< local y-velocities node 1
      real(kind=wp), dimension(nel),       intent(out)   :: vy2      !< local y-velocities node 2
      real(kind=wp), dimension(nel),       intent(out)   :: vy3      !< local y-velocities node 3
      real(kind=wp), dimension(nel),       intent(out)   :: vy4      !< local y-velocities node 4
      real(kind=wp), dimension(nel),       intent(out)   :: vy5      !< local y-velocities node 5
      real(kind=wp), dimension(nel),       intent(out)   :: vy6      !< local y-velocities node 6
      real(kind=wp), dimension(nel),       intent(out)   :: vz1      !< local z-velocities node 1
      real(kind=wp), dimension(nel),       intent(out)   :: vz2      !< local z-velocities node 2
      real(kind=wp), dimension(nel),       intent(out)   :: vz3      !< local z-velocities node 3
      real(kind=wp), dimension(nel),       intent(out)   :: vz4      !< local z-velocities node 4
      real(kind=wp), dimension(nel),       intent(out)   :: vz5      !< local z-velocities node 5
      real(kind=wp), dimension(nel),       intent(out)   :: vz6      !< local z-velocities node 6
      real(kind=wp), dimension(nel),       intent(out)   :: vd2      !< viscous damping coefficient
      real(kind=wp), dimension(nel),       intent(inout) :: vis      !< viscosity
      real(kind=wp), dimension(nel),       intent(in)    :: offg     !< global offset flag
      real(kind=wp), dimension(nel),       intent(out)   :: off      !< local offset flag
      real(kind=wp), dimension(nel),       intent(in)    :: rho      !< density
      real(kind=wp), dimension(nel),       intent(out)   :: rhoo     !< initial density
      real(kind=wp), dimension(nel),       intent(out)   :: r11      !< rotation matrix component (1,1)
      real(kind=wp), dimension(nel),       intent(out)   :: r12      !< rotation matrix component (1,2)
      real(kind=wp), dimension(nel),       intent(out)   :: r13      !< rotation matrix component (1,3)
      real(kind=wp), dimension(nel),       intent(out)   :: r21      !< rotation matrix component (2,1)
      real(kind=wp), dimension(nel),       intent(out)   :: r22      !< rotation matrix component (2,2)
      real(kind=wp), dimension(nel),       intent(out)   :: r23      !< rotation matrix component (2,3)
      real(kind=wp), dimension(nel),       intent(out)   :: r31      !< rotation matrix component (3,1)
      real(kind=wp), dimension(nel),       intent(out)   :: r32      !< rotation matrix component (3,2)
      real(kind=wp), dimension(nel),       intent(out)   :: r33      !< rotation matrix component (3,3)
      integer, dimension(nel),             intent(out)   :: nc1      !< node connectivity index 1
      integer, dimension(nel),             intent(out)   :: nc2      !< node connectivity index 2
      integer, dimension(nel),             intent(out)   :: nc3      !< node connectivity index 3
      integer, dimension(nel),             intent(out)   :: nc4      !< node connectivity index 4
      integer, dimension(nel),             intent(out)   :: nc5      !< node connectivity index 5
      integer, dimension(nel),             intent(out)   :: nc6      !< node connectivity index 6
      integer, dimension(nel),             intent(out)   :: ngl      !< global element number
      integer, dimension(nel),             intent(out)   :: mxt      !< material type
      integer, dimension(nel),             intent(out)   :: ngeo     !< geometry number
      integer,                             intent(in)    :: ioutprt  !< output print flag
      real(kind=wp), dimension(nel),       intent(out)   :: vgax     !< global velocity x-component
      real(kind=wp), dimension(nel),       intent(out)   :: vgay     !< global velocity y-component
      real(kind=wp), dimension(nel),       intent(out)   :: vgaz     !< global velocity z-component
      real(kind=wp), dimension(nel),       intent(out)   :: vga2     !< velocity squared
      real(kind=wp), dimension(nel),       intent(out)   :: xgax     !< global coordinate x-component
      real(kind=wp), dimension(nel),       intent(out)   :: xgay     !< global coordinate y-component
      real(kind=wp), dimension(nel),       intent(out)   :: xgaz     !< global coordinate z-component
      real(kind=wp), dimension(nel),       intent(out)   :: xgxa2    !< x-coordinate squared term
      real(kind=wp), dimension(nel),       intent(out)   :: xgya2    !< y-coordinate squared term
      real(kind=wp), dimension(nel),       intent(out)   :: xgza2    !< z-coordinate squared term
      real(kind=wp), dimension(nel),       intent(out)   :: xgxya    !< xy cross product term
      real(kind=wp), dimension(nel),       intent(out)   :: xgyza    !< yz cross product term
      real(kind=wp), dimension(nel),       intent(out)   :: xgzxa    !< zx cross product term
      integer,dimension(n_var_iparg),      intent(in)    :: iparg
      real(kind=wp), dimension(nel,6),     intent(out)   :: gama_r   !< rotation matrix storage
!c    force wp = 8 to ensure double-precision (64-bit) floating-point calculations, even when compiling in single-precision mode.      
      real(kind=8),  dimension(nel,15),    intent(inout)    :: sav      !< saved variables array sav must be in double precision, so kind = 8
      real(kind=8), dimension(nel),       intent(out)   :: xd1       !< local x-coordinates node 1 in double precision
      real(kind=8), dimension(nel),       intent(out)   :: xd2       !< local x-coordinates node 2 in double precision
      real(kind=8), dimension(nel),       intent(out)   :: xd3       !< local x-coordinates node 3 in double precision
      real(kind=8), dimension(nel),       intent(out)   :: xd4       !< local x-coordinates node 4 in double precision
      real(kind=8), dimension(nel),       intent(out)   :: xd5       !< local x-coordinates node 5 in double precision
      real(kind=8), dimension(nel),       intent(out)   :: xd6       !< local x-coordinates node 6 in double precision
      real(kind=8), dimension(nel),       intent(out)   :: yd1       !< local y-coordinates node 1 in double precision
      real(kind=8), dimension(nel),       intent(out)   :: yd2       !< local y-coordinates node 2 in double precision
      real(kind=8), dimension(nel),       intent(out)   :: yd3       !< local y-coordinates node 3 in double precision
      real(kind=8), dimension(nel),       intent(out)   :: yd4       !< local y-coordinates node 4 in double precision
      real(kind=8), dimension(nel),       intent(out)   :: yd5       !< local y-coordinates node 5 in double precision
      real(kind=8), dimension(nel),       intent(out)   :: yd6       !< local y-coordinates node 6 in double precision
      real(kind=8), dimension(nel),       intent(out)   :: zd1       !< local z-coordinates node 1 in double precision
      real(kind=8), dimension(nel),       intent(out)   :: zd2       !< local z-coordinates node 2 in double precision
      real(kind=8), dimension(nel),       intent(out)   :: zd3       !< local z-coordinates node 3 in double precision
      real(kind=8), dimension(nel),       intent(out)   :: zd4       !< local z-coordinates node 4 in double precision
      real(kind=8), dimension(nel),       intent(out)   :: zd5       !< local z-coordinates node 5 in double precision
      real(kind=8), dimension(nel),       intent(out)   :: zd6       !< local z-coordinates node 6 in double precision
      
      real(kind=8), dimension(mvsiz,6),       intent(inout)   :: X0
      real(kind=8), dimension(mvsiz,6),       intent(inout)   :: Y0
      real(kind=8), dimension(mvsiz,6),       intent(inout)   :: Z0

      integer,                            intent(in)    :: iresp     !< response parameter
      real(kind=8), dimension(3,numnod),  intent(in)    :: xdp       

!-------------------------------------------------------------------------------
!    l o c a l   v a r i a b l e s
!-------------------------------------------------------------------------------
      integer :: i                                                   !< loop counter
      real(kind=wp) :: off_l                                         !< local offset flag
!c    force wp = 8 to ensure double-precision (64-bit) floating-point calculations, even when compiling in single-precision mode.
      real(kind=8) :: xl, yl, zl                                    !< local centroid coordinates
      real(kind=8), dimension(nel) :: xdl1, xdl2, xdl3, xdl4         !< local degenerated x-coordinates 1-4 double precision
      real(kind=8), dimension(nel) :: xdl5, xdl6, xdl7, xdl8         !< local degenerated x-coordinates 5-8 double precision
      real(kind=8), dimension(nel) :: ydl1, ydl2, ydl3, ydl4         !< local degenerated y-coordinates 1-4 double precision
      real(kind=8), dimension(nel) :: ydl5, ydl6, ydl7, ydl8         !< local degenerated y-coordinates 5-8 double precision
      real(kind=8), dimension(nel) :: zdl1, zdl2, zdl3, zdl4         !< local degenerated z-coordinates 1-4 double precision
      real(kind=8), dimension(nel) :: zdl5, zdl6, zdl7, zdl8         !< local degenerated z-coordinates 5-8 double precision
      real(kind=8) :: xdl_a, ydl_a, zdl_a                            !< local degenerated coordinates in double precison   
      real(kind=wp), dimension(nel) :: vxd1, vxd2, vxd3, vxd4        !< degenerated x-velocities 1-4
      real(kind=wp), dimension(nel) :: vxd5, vxd6, vxd7, vxd8        !< degenerated x-velocities 5-8
      real(kind=wp), dimension(nel) :: vyd1, vyd2, vyd3, vyd4        !< degenerated y-velocities 1-4
      real(kind=wp), dimension(nel) :: vyd5, vyd6, vyd7, vyd8        !< degenerated y-velocities 5-8
      real(kind=wp), dimension(nel) :: vzd1, vzd2, vzd3, vzd4        !< degenerated z-velocities 1-4
      real(kind=wp), dimension(nel) :: vzd5, vzd6, vzd7, vzd8        !< degenerated z-velocities 5-8
      real(kind=wp), dimension(mvsiz) :: rx, ry, rz                    !< rotation matrix row 1
      real(kind=wp), dimension(mvsiz) :: sx, sy, sz                    !< rotation matrix row 2
      real(kind=wp), dimension(mvsiz) :: tx, ty, tz                    !< rotation matrix row 3
      real(kind=wp) :: G11, G21, G31                                   !< metric tensor components
      real(kind=wp) :: G12, G22, G32
      real(kind=wp) :: G13, G23, G33
      real(kind=wp) :: T11, T21, T31                                   !< transformation matrix components
      real(kind=wp) :: T12, T22, T32
      real(kind=wp) :: T13, T23, T33


!===============================================================================
!     s o u r c e  l i n e s
!===============================================================================
      off_l  = zero 
      !< recover nodal connectivity and material properties 
      do i = 1,nel
        vis(i)  = zero
        ngeo(i) = ixs(10,i)
        ngl(i)  = ixs(11,i)
        mxt(i)  = ixs(1,i)
        nc1(i)  = ixs(2,i)
        nc2(i)  = ixs(3,i)
        nc3(i)  = ixs(4,i)
        nc4(i)  = ixs(6,i)
        nc5(i)  = ixs(7,i)
        nc6(i)  = ixs(8,i)
        rhoo(i) = rho(i)
      enddo
! 
!c----------------------------
!c     nodal coordinates     |
!c----------------------------

        IF(IRESP==1)THEN 
        DO i = 1,nel
        xd1(i)=xdp(1,nc1(i))
        yd1(i)=xdp(2,nc1(i))
        zd1(i)=xdp(3,nc1(i))
        xd2(i)=xdp(1,nc2(i))
        yd2(i)=xdp(2,nc2(i))
        zd2(i)=xdp(3,nc2(i))
        xd3(i)=xdp(1,nc3(i))
        yd3(i)=xdp(2,nc3(i))
        zd3(i)=xdp(3,nc3(i))
        xd4(i)=xdp(1,nc4(i))
        yd4(i)=xdp(2,nc4(i))
        zd4(i)=xdp(3,nc4(i))
        xd5(i)=xdp(1,nc5(i))
        yd5(i)=xdp(2,nc5(i))
        zd5(i)=xdp(3,nc5(i))
        xd6(i)=xdp(1,nc6(i))
        yd6(i)=xdp(2,nc6(i))
        zd6(i)=xdp(3,nc6(i))  

            xdl1(i)=xd1(i)
            ydl1(i)=yd1(i)
            zdl1(i)=zd1(i)
!
            xdl2(i)=xd2(i)
            ydl2(i)=yd2(i)
            zdl2(i)=zd2(i)
!
            xdl3(i)=xd3(i)
            ydl3(i)=yd3(i)
            zdl3(i)=zd3(i)
!
            xdl4(i)=xd3(i)
            ydl4(i)=yd3(i)
            zdl4(i)=zd3(i)
!
            xdl5(i)=xd4(i)
            ydl5(i)=yd4(i)
            zdl5(i)=zd4(i)
!
            xdl6(i)=xd5(i)
            ydl6(i)=yd5(i)
            zdl6(i)=zd5(i)
!
            xdl7(i)=xd6(i)
            ydl7(i)=yd6(i)
            zdl7(i)=zd6(i)
!
            xdl8(i)=xd6(i)  
            ydl8(i)=yd6(i)  
            zdl8(i)=zd6(i)        

        ENDDO
      ELSE
      do i = 1,nel

        xd1(i)=x(1,nc1(i))
        yd1(i)=x(2,nc1(i))
        zd1(i)=x(3,nc1(i))
        xd2(i)=x(1,nc2(i))
        yd2(i)=x(2,nc2(i))
        zd2(i)=x(3,nc2(i))
        xd3(i)=x(1,nc3(i))
        yd3(i)=x(2,nc3(i))
        zd3(i)=x(3,nc3(i))
        xd4(i)=x(1,nc4(i))
        yd4(i)=x(2,nc4(i))
        zd4(i)=x(3,nc4(i))
        xd5(i)=x(1,nc5(i))
        yd5(i)=x(2,nc5(i))
        zd5(i)=x(3,nc5(i))
        xd6(i)=x(1,nc6(i))
        yd6(i)=x(2,nc6(i))
        zd6(i)=x(3,nc6(i))

            xdl1(i)=xd1(i)
            ydl1(i)=yd1(i)
            zdl1(i)=zd1(i)
!
            xdl2(i)=xd2(i)
            ydl2(i)=yd2(i)
            zdl2(i)=zd2(i)
!
            xdl3(i)=xd3(i)
            ydl3(i)=yd3(i)
            zdl3(i)=zd3(i)
!
            xdl4(i)=xd3(i)
            ydl4(i)=yd3(i)
            zdl4(i)=zd3(i)
!
            xdl5(i)=xd4(i)
            ydl5(i)=yd4(i)
            zdl5(i)=zd4(i)
!
            xdl6(i)=xd5(i)
            ydl6(i)=yd5(i)
            zdl6(i)=zd5(i)
!
            xdl7(i)=xd6(i)
            ydl7(i)=yd6(i)
            zdl7(i)=zd6(i)
!
            xdl8(i)=xd6(i)  
            ydl8(i)=yd6(i)  
            zdl8(i)=zd6(i) 

      enddo
    endif
!
      !< nodal velocities
      do i = 1,nel

        vx1(i)=v(1,nc1(i))
        vy1(i)=v(2,nc1(i))
        vz1(i)=v(3,nc1(i))
        vx2(i)=v(1,nc2(i))
        vy2(i)=v(2,nc2(i))
        vz2(i)=v(3,nc2(i))
        vx3(i)=v(1,nc3(i))
        vy3(i)=v(2,nc3(i))
        vz3(i)=v(3,nc3(i))
        vx4(i)=v(1,nc4(i))
        vy4(i)=v(2,nc4(i))
        vz4(i)=v(3,nc4(i))
        vx5(i)=v(1,nc5(i))
        vy5(i)=v(2,nc5(i))
        vz5(i)=v(3,nc5(i))
        vx6(i)=v(1,nc6(i))
        vy6(i)=v(2,nc6(i))
        vz6(i)=v(3,nc6(i))
        vxd1(i)=vx1(i)
        vyd1(i)=vy1(i)
        vzd1(i)=vz1(i)
        vxd2(i)=vx2(i)
        vyd2(i)=vy2(i)
        vzd2(i)=vz2(i)
        vxd3(i)=vx3(i)
        vyd3(i)=vy3(i)
        vzd3(i)=vz3(i)
        vxd4(i)=vx3(i)  
        vyd4(i)=vy3(i)  
        vzd4(i)=vz3(i)  
        vxd5(i)=vx4(i)  
        vyd5(i)=vy4(i)  
        vzd5(i)=vz4(i)  
        vxd6(i)=vx5(i)  
        vyd6(i)=vy5(i)  
        vzd6(i)=vz5(i)  
        vxd7(i)=vx6(i)
        vyd7(i)=vy6(i)
        vzd7(i)=vz6(i)
        vxd8(i)=vx6(i)  
        vyd8(i)=vy6(i)  
        vzd8(i)=vz6(i) 
      enddo

!C-----------
!C     prepares outputs by part.
!C-----------
      if (ioutprt/=0) then
        do i = 1,nel
          vgax(i)=vx1(i)+vx2(i)+vx3(i)+vx4(i)+vx5(i)+vx6(i)
          vgay(i)=vy1(i)+vy2(i)+vy3(i)+vy4(i)+vy5(i)+vy6(i)
          vgaz(i)=vz1(i)+vz2(i)+vz3(i)+vz4(i)+vz5(i)+vz6(i)
          vga2(i)=vx1(i)*vx1(i)+vx2(i)*vx2(i)+vx3(i)*vx3(i)+vx4(i)*vx4(i) &
                +vx5(i)*vx5(i)+vx6(i)*vx6(i) &
                +vy1(i)*vy1(i)+vy2(i)*vy2(i)+vy3(i)*vy3(i)+vy4(i)*vy4(i) &
                +vy5(i)*vy5(i)+vy6(i)*vy6(i) &
                +vz1(i)*vz1(i)+vz2(i)*vz2(i)+vz3(i)*vz3(i)+vz4(i)*vz4(i) &
                +vz5(i)*vz5(i)+vz6(i)*vz6(i)
            enddo
            if(iparg(80)==1) then
              do i = 1,nel
                xgax(i)=xd1(i)+xd2(i)+xd3(i)+xd4(i)+xd5(i)+xd6(i)
                xgay(i)=yd1(i)+yd2(i)+yd3(i)+yd4(i)+yd5(i)+yd6(i)
                xgaz(i)=zd1(i)+zd2(i)+zd3(i)+zd4(i)+zd5(i)+zd6(i)
                xgxa2(i)=xd1(i)**2+xd2(i)**2+xd3(i)**2+xd4(i)**2 &
                  +xd5(i)**2+xd6(i)**2
                xgya2(i)=yd1(i)**2+yd2(i)**2+yd3(i)**2+yd4(i)**2 &
                  +yd5(i)**2+yd6(i)**2
                xgza2(i)=zd1(i)**2+zd2(i)**2+zd3(i)**2+zd4(i)**2 &
                  +zd5(i)**2+zd6(i)**2
                xgxya(i)=xd1(i)*yd1(i)+xd2(i)*yd2(i)+xd3(i)*yd3(i)+xd4(i)*yd4(i) &
                  +xd5(i)*yd5(i)+xd6(i)*yd6(i)
                xgyza(i)=yd1(i)*zd1(i)+yd2(i)*zd2(i)+yd3(i)*zd3(i)+yd4(i)*zd4(i) &
                  +yd5(i)*zd5(i)+yd6(i)*zd6(i)
                xgzxa(i)=zd1(i)*xd1(i)+zd2(i)*xd2(i)+zd3(i)*xd3(i)+zd4(i)*xd4(i) &
                  +zd5(i)*xd5(i)+zd6(i)*xd6(i)
              enddo
            endif
          endif
!OK
!C-----------
!C     convected frame (ITERATIONS).
!C-----------
      !< rotation matrix in the element frame
      call srepiso3( &
          xdl1,     xdl2,     xdl3,     xdl4, &
          xdl5,     xdl6,     xdl7,     xdl8, &
          ydl1,     ydl2,     ydl3,     ydl4, &
          ydl5,     ydl6,     ydl7,     ydl8, &
          zdl1,     zdl2,     zdl3,     zdl4, &
          zdl5,     zdl6,     zdl7,     zdl8, &
          rx,      ry,      rz,      sx, &
          sy,      sz,      tx,      ty, &
          tz,      nel)    


      call sortho3(                       &
          rx,      ry,      rz,      sx,  &
          sy,      sz,      tx,      ty,  &
          tz,      r12,     r13,     r11, &
          r22,     r23,     r21,     r32, &
          r33,     r31,     nel)

!
!C------stocker [R] in %GAMA_R--------------  
      gama_r(1:nel,1) = r11(1:nel) ! dir1_x                            
      gama_r(1:nel,2) = r21(1:nel) ! dir1_y                            
      gama_r(1:nel,3) = r31(1:nel) ! dir1_z                            
      gama_r(1:nel,4) = r12(1:nel) ! dir2_x                            
      gama_r(1:nel,5) = r22(1:nel) ! dir2_y                           
      gama_r(1:nel,6) = r32(1:nel) ! dir2_z     
!C-------sauf thick shells --------------     
      !< isotropic property
      if (isorth == 0) then
        do i = 1,nel                                
          gama(i,1) = one                             
          gama(i,2) = zero                              
          gama(i,3) = zero
          gama(i,4) = zero                              
          gama(i,5) = one                              
          gama(i,6) = zero
        enddo      
      !< orthotropic property
      else    
        call sorthdir3( &
          rx, ry, rz, sx, &
          sy, sz, tx, ty, &
          tz, r11, r12, r13, &
          r21, r22, r23, r31, &
          r32, r33, gama0, gama, &
          nel, irep)
      endif
!C-----
!C     Change to the convected frame.
!C-----------       
      !< rotate in the element frame 
      if ((ISMSTR<=4.OR.(ISMSTR==12.AND.IDTMIN(1)==3)).AND.JLAG>0) then
        do i = 1,nel
          if (ABS(OFFG(I)) > ONE) then
    
            xd1(i) = sav(i,1)
            yd1(i) = sav(i,2)
            zd1(i) = sav(i,3)
            xd2(i) = sav(i,4)
            yd2(i) = sav(i,5)
            zd2(i) = sav(i,6)
            xd3(i) = sav(i,7)
            yd3(i) = sav(i,8)
            zd3(i) = sav(i,9)
            xd4(i) = sav(i,10)
            yd4(i) = sav(i,11)
            zd4(i) = sav(i,12)
            xd5(i) = sav(i,13)
            yd5(i) = sav(i,14)
            zd5(i) = sav(i,15)
            xd6(i) = zero
            yd6(i) = zero
            zd6(i) = zero

            xdl1(i)=xd1(i)
            ydl1(i)=yd1(i)
            zdl1(i)=zd1(i)
!
            xdl2(i)=xd2(i)
            ydl2(i)=yd2(i)
            zdl2(i)=zd2(i)
!
            xdl3(i)=xd3(i)
            ydl3(i)=yd3(i)
            zdl3(i)=zd3(i)
!
            xdl4(i)=xd3(i)
            ydl4(i)=yd3(i)
            zdl4(i)=zd3(i)
!
            xdl5(i)=xd4(i)
            ydl5(i)=yd4(i)
            zdl5(i)=zd4(i)
!
            xdl6(i)=xd5(i)
            ydl6(i)=yd5(i)
            zdl6(i)=zd5(i)
!
            xdl7(i)=xd6(i)
            ydl7(i)=yd6(i)
            zdl7(i)=zd6(i)
!
            xdl8(i)=xd6(i)  
            ydl8(i)=yd6(i)  
            zdl8(i)=zd6(i) 

            OFF(I) = ABS(OFFG(I))-ONE
            OFF_L  = MIN(OFF_L,OFFG(I))
!
          else
 
            xdl_a = r11(i)*xdl1(i)+r21(i)*ydl1(i)+r31(i)*zdl1(i)
            ydl_a = r12(i)*xdl1(i)+r22(i)*ydl1(i)+r32(i)*zdl1(i)
            zdl_a = r13(i)*xdl1(i)+r23(i)*ydl1(i)+r33(i)*zdl1(i)
            xdl1(i) = xdl_a
            ydl1(i) = ydl_a
            zdl1(i) = zdl_a
            xdl_a = r11(i)*xdl2(i)+r21(i)*ydl2(i)+r31(i)*zdl2(i)
            ydl_a = r12(i)*xdl2(i)+r22(i)*ydl2(i)+r32(i)*zdl2(i)
            zdl_a = r13(i)*xdl2(i)+r23(i)*ydl2(i)+r33(i)*zdl2(i)
            xdl2(i) = xdl_a
            ydl2(i) = ydl_a
            zdl2(i) = zdl_a
            xdl_a = r11(i)*xdl3(i)+r21(i)*ydl3(i)+r31(i)*zdl3(i)
            ydl_a = r12(i)*xdl3(i)+r22(i)*ydl3(i)+r32(i)*zdl3(i)
            zdl_a = r13(i)*xdl3(i)+r23(i)*ydl3(i)+r33(i)*zdl3(i)
            xdl3(i) = xdl_a
            ydl3(i) = ydl_a
            zdl3(i) = zdl_a
            xdl_a = r11(i)*xdl4(i)+r21(i)*ydl4(i)+r31(i)*zdl4(i)
            ydl_a = r12(i)*xdl4(i)+r22(i)*ydl4(i)+r32(i)*zdl4(i)
            zdl_a = r13(i)*xdl4(i)+r23(i)*ydl4(i)+r33(i)*zdl4(i)
            xdl4(i) = xdl_a
            ydl4(i) = ydl_a
            zdl4(i) = zdl_a
            xdl_a = r11(i)*xdl5(i)+r21(i)*ydl5(i)+r31(i)*zdl5(i)
            ydl_a = r12(i)*xdl5(i)+r22(i)*ydl5(i)+r32(i)*zdl5(i)
            zdl_a = r13(i)*xdl5(i)+r23(i)*ydl5(i)+r33(i)*zdl5(i)
            xdl5(i) = xdl_a
            ydl5(i) = ydl_a
            zdl5(i) = zdl_a
            xdl_a = r11(i)*xdl6(i)+r21(i)*ydl6(i)+r31(i)*zdl6(i)
            ydl_a = r12(i)*xdl6(i)+r22(i)*ydl6(i)+r32(i)*zdl6(i)
            zdl_a = r13(i)*xdl6(i)+r23(i)*ydl6(i)+r33(i)*zdl6(i)
            xdl6(i) = xdl_a
            ydl6(i) = ydl_a
            zdl6(i) = zdl_a
            xdl_a = r11(i)*xdl7(i)+r21(i)*ydl7(i)+r31(i)*zdl7(i)
            ydl_a = r12(i)*xdl7(i)+r22(i)*ydl7(i)+r32(i)*zdl7(i)
            zdl_a = r13(i)*xdl7(i)+r23(i)*ydl7(i)+r33(i)*zdl7(i)
            xdl7(i) = xdl_a
            ydl7(i) = ydl_a
            zdl7(i) = zdl_a
            xdl_a = r11(i)*xdl8(i)+r21(i)*ydl8(i)+r31(i)*zdl8(i)
            ydl_a = r12(i)*xdl8(i)+r22(i)*ydl8(i)+r32(i)*zdl8(i)
            zdl_a = r13(i)*xdl8(i)+r23(i)*ydl8(i)+r33(i)*zdl8(i)
            xdl8(i) = xdl_a
            ydl8(i) = ydl_a
            zdl8(i) = zdl_a              
            
            OFF(I) = ABS(OFFG(I))
            OFF_L  = MIN(OFF_L,OFFG(I))
          endif !offg(i)>one
        enddo
!OK        
        IF((ISMSTR==12.AND.IDTMIN(1)==3).AND.JLAG>0) THEN !//IDTMIN 
         DO I = 1,nel  
          IF(ABS(OFFG(I)) > ONE)THEN

            xdl_a = r11(i)*xdl1(i)+r21(i)*ydl1(i)+r31(i)*zdl1(i)
            ydl_a = r12(i)*xdl1(i)+r22(i)*ydl1(i)+r32(i)*zdl1(i)
            zdl_a = r13(i)*xdl1(i)+r23(i)*ydl1(i)+r33(i)*zdl1(i)
            xdl1(i) = xdl_a
            ydl1(i) = ydl_a
            zdl1(i) = zdl_a
            xdl_a = r11(i)*xdl2(i)+r21(i)*ydl2(i)+r31(i)*zdl2(i)
            ydl_a = r12(i)*xdl2(i)+r22(i)*ydl2(i)+r32(i)*zdl2(i)
            zdl_a = r13(i)*xdl2(i)+r23(i)*ydl2(i)+r33(i)*zdl2(i)
            xdl2(i) = xdl_a
            ydl2(i) = ydl_a
            zdl2(i) = zdl_a
            xdl_a = r11(i)*xdl3(i)+r21(i)*ydl3(i)+r31(i)*zdl3(i)
            ydl_a = r12(i)*xdl3(i)+r22(i)*ydl3(i)+r32(i)*zdl3(i)
            zdl_a = r13(i)*xdl3(i)+r23(i)*ydl3(i)+r33(i)*zdl3(i)
            xdl3(i) = xdl_a
            ydl3(i) = ydl_a
            zdl3(i) = zdl_a
            xdl_a = r11(i)*xdl4(i)+r21(i)*ydl4(i)+r31(i)*zdl4(i)
            ydl_a = r12(i)*xdl4(i)+r22(i)*ydl4(i)+r32(i)*zdl4(i)
            zdl_a = r13(i)*xdl4(i)+r23(i)*ydl4(i)+r33(i)*zdl4(i)
            xdl4(i) = xdl_a
            ydl4(i) = ydl_a
            zdl4(i) = zdl_a
            xdl_a = r11(i)*xdl5(i)+r21(i)*ydl5(i)+r31(i)*zdl5(i)
            ydl_a = r12(i)*xdl5(i)+r22(i)*ydl5(i)+r32(i)*zdl5(i)
            zdl_a = r13(i)*xdl5(i)+r23(i)*ydl5(i)+r33(i)*zdl5(i)
            xdl5(i) = xdl_a
            ydl5(i) = ydl_a
            zdl5(i) = zdl_a
            xdl_a = r11(i)*xdl6(i)+r21(i)*ydl6(i)+r31(i)*zdl6(i)
            ydl_a = r12(i)*xdl6(i)+r22(i)*ydl6(i)+r32(i)*zdl6(i)
            zdl_a = r13(i)*xdl6(i)+r23(i)*ydl6(i)+r33(i)*zdl6(i)
            xdl6(i) = xdl_a
            ydl6(i) = ydl_a
            zdl6(i) = zdl_a
            xdl_a = r11(i)*xdl7(i)+r21(i)*ydl7(i)+r31(i)*zdl7(i)
            ydl_a = r12(i)*xdl7(i)+r22(i)*ydl7(i)+r32(i)*zdl7(i)
            zdl_a = r13(i)*xdl7(i)+r23(i)*ydl7(i)+r33(i)*zdl7(i)
            xdl7(i) = xdl_a
            ydl7(i) = ydl_a
            zdl7(i) = zdl_a
            xdl_a = r11(i)*xdl8(i)+r21(i)*ydl8(i)+r31(i)*zdl8(i)
            ydl_a = r12(i)*xdl8(i)+r22(i)*ydl8(i)+r32(i)*zdl8(i)
            zdl_a = r13(i)*xdl8(i)+r23(i)*ydl8(i)+r33(i)*zdl8(i)
            xdl8(i) = xdl_a
            ydl8(i) = ydl_a
            zdl8(i) = zdl_a                   
          END IF  
        end do 
      end if
       

      else  ! Ismstr > 4

            do i = 1,nel
            xdl_a = r11(i)*xdl1(i)+r21(i)*ydl1(i)+r31(i)*zdl1(i)
            ydl_a = r12(i)*xdl1(i)+r22(i)*ydl1(i)+r32(i)*zdl1(i)
            zdl_a = r13(i)*xdl1(i)+r23(i)*ydl1(i)+r33(i)*zdl1(i)
            xdl1(i) = xdl_a
            ydl1(i) = ydl_a
            zdl1(i) = zdl_a
            xdl_a = r11(i)*xdl2(i)+r21(i)*ydl2(i)+r31(i)*zdl2(i)
            ydl_a = r12(i)*xdl2(i)+r22(i)*ydl2(i)+r32(i)*zdl2(i)
            zdl_a = r13(i)*xdl2(i)+r23(i)*ydl2(i)+r33(i)*zdl2(i)
            xdl2(i) = xdl_a
            ydl2(i) = ydl_a
            zdl2(i) = zdl_a
            xdl_a = r11(i)*xdl3(i)+r21(i)*ydl3(i)+r31(i)*zdl3(i)
            ydl_a = r12(i)*xdl3(i)+r22(i)*ydl3(i)+r32(i)*zdl3(i)
            zdl_a = r13(i)*xdl3(i)+r23(i)*ydl3(i)+r33(i)*zdl3(i)
            xdl3(i) = xdl_a
            ydl3(i) = ydl_a
            zdl3(i) = zdl_a
            xdl_a = r11(i)*xdl4(i)+r21(i)*ydl4(i)+r31(i)*zdl4(i)
            ydl_a = r12(i)*xdl4(i)+r22(i)*ydl4(i)+r32(i)*zdl4(i)
            zdl_a = r13(i)*xdl4(i)+r23(i)*ydl4(i)+r33(i)*zdl4(i)
            xdl4(i) = xdl_a
            ydl4(i) = ydl_a
            zdl4(i) = zdl_a
            xdl_a = r11(i)*xdl5(i)+r21(i)*ydl5(i)+r31(i)*zdl5(i)
            ydl_a = r12(i)*xdl5(i)+r22(i)*ydl5(i)+r32(i)*zdl5(i)
            zdl_a = r13(i)*xdl5(i)+r23(i)*ydl5(i)+r33(i)*zdl5(i)
            xdl5(i) = xdl_a
            ydl5(i) = ydl_a
            zdl5(i) = zdl_a
            xdl_a = r11(i)*xdl6(i)+r21(i)*ydl6(i)+r31(i)*zdl6(i)
            ydl_a = r12(i)*xdl6(i)+r22(i)*ydl6(i)+r32(i)*zdl6(i)
            zdl_a = r13(i)*xdl6(i)+r23(i)*ydl6(i)+r33(i)*zdl6(i)
            xdl6(i) = xdl_a
            ydl6(i) = ydl_a
            zdl6(i) = zdl_a
            xdl_a = r11(i)*xdl7(i)+r21(i)*ydl7(i)+r31(i)*zdl7(i)
            ydl_a = r12(i)*xdl7(i)+r22(i)*ydl7(i)+r32(i)*zdl7(i)
            zdl_a = r13(i)*xdl7(i)+r23(i)*ydl7(i)+r33(i)*zdl7(i)
            xdl7(i) = xdl_a
            ydl7(i) = ydl_a
            zdl7(i) = zdl_a
            xdl_a = r11(i)*xdl8(i)+r21(i)*ydl8(i)+r31(i)*zdl8(i)
            ydl_a = r12(i)*xdl8(i)+r22(i)*ydl8(i)+r32(i)*zdl8(i)
            zdl_a = r13(i)*xdl8(i)+r23(i)*ydl8(i)+r33(i)*zdl8(i)
            xdl8(i) = xdl_a
            ydl8(i) = ydl_a
            zdl8(i) = zdl_a              
            !off(i) = offg(i)
            OFF(I) = ABS(OFFG(I))
            OFF_L  = MIN(OFF_L,OFFG(I))
             enddo
      endif  ! ISMSTR   

!C-----------
!C     PASSAGE AU ORTHOTROPIC FRAME.
!C-----------

      IF (ISORTH/=0 .AND. JHBE/=24 .AND. JHBE/=222 .AND. JHBE/=14 &
                   .AND. JHBE/=15) THEN
!C------save for SAV update     
       IF(ISMSTR <= 3) THEN

        DO I=1,nel
         X0(I,1) = XDl1(I)
         Y0(I,1) = YDl1(I)
         Z0(I,1) = ZDl1(I)
         X0(I,2) = XDl2(I)
         Y0(I,2) = YDl2(I)
         Z0(I,2) = ZDl2(I)
         X0(I,3) = XDl3(I)
         Y0(I,3) = YDl3(I)
         Z0(I,3) = ZDl3(I)
         X0(I,4) = XDl5(I)
         Y0(I,4) = YDl5(I)
         Z0(I,4) = ZDl5(I)
         X0(I,5) = XDl6(I)
         Y0(I,5) = YDl6(I)
         Z0(I,5) = ZDl6(I)
         X0(I,6) = XDl7(I)
         Y0(I,6) = YDl7(I)
         Z0(I,6) = ZDl7(I)
        END DO
       END IF
!      C
        DO I=1,nel
!C         Extration de G tq Xortho=Transpose(G) Xcvt
!C                                 =Transpose(G) Transpose(R) Xglobal.

          G11=GAMA(I,1)
          G21=GAMA(I,2)
          G31=GAMA(I,3)
          G12=GAMA(I,4)
          G22=GAMA(I,5)
          G32=GAMA(I,6)
          G13=G21*G32-G31*G22
          G23=G31*G12-G11*G32
          G33=G11*G22-G21*G12
!C
          XDL_a=G11*XDl1(I)+G21*YDl1(I)+G31*ZDl1(I)
          YDL_a=G12*XDl1(I)+G22*YDl1(I)+G32*ZDl1(I)
          ZDL_a=G13*XDl1(I)+G23*YDl1(I)+G33*ZDl1(I)
          XDl1(I)=XDL_a
          YDl1(I)=YDL_a
          ZDl1(I)=ZDL_a
          XDL_a=G11*XDl2(I)+G21*YDl2(I)+G31*ZDl2(I)
          YDL_a=G12*XDl2(I)+G22*YDl2(I)+G32*ZDl2(I)
          ZDL_a=G13*XDl2(I)+G23*YDl2(I)+G33*ZDl2(I)
          XDl2(I)=XDL_a
          YDl2(I)=YDL_a
          ZDl2(I)=ZDL_a
          XDL_a=G11*XDl3(I)+G21*Ydl3(I)+G31*ZDl3(I)
          YDL_a=G12*XDl3(I)+G22*YDl3(I)+G32*ZDl3(I)
          ZDL_a=G13*XDl3(I)+G23*YDl3(I)+G33*ZDl3(I)
          XDl3(I)=XDL_a
          YDl3(I)=YDL_a
          ZDl3(I)=ZDL_a
          XDL_a=G11*XDl4(I)+G21*YDl4(I)+G31*ZDl4(I)
          YDL_a=G12*XDl4(I)+G22*YDl4(I)+G32*ZDl4(I)
          ZDL_a=G13*XDl4(I)+G23*YDl4(I)+G33*ZDl4(I)
          XDl4(I)=XDL_a
          YDl4(I)=YDL_a
          ZDl4(I)=ZDL_a
          XDL_a=G11*XDl5(I)+G21*YDl5(I)+G31*ZDl5(I)
          YDL_a=G12*XDl5(I)+G22*YDl5(I)+G32*ZDl5(I)
          ZDL_a=G13*XDl5(I)+G23*YDl5(I)+G33*ZDl5(I)
          XDl5(I)=XDL_a
          YDl5(I)=YDL_a
          ZDl5(I)=ZDL_a
          XDL_a=G11*XDl6(I)+G21*YDl6(I)+G31*ZDl6(I)
          YDL_a=G12*XDl6(I)+G22*YDl6(I)+G32*ZDl6(I)
          ZDL_a=G13*XDl6(I)+G23*YDl6(I)+G33*ZDl6(I)
          XDl6(I)=XDL_a
          YDl6(I)=YDL_a
          ZDl6(I)=ZDL_a
          XDL_a=G11*XDl7(I)+G21*YDl7(I)+G31*ZDl7(I)
          YDL_a=G12*XDl7(I)+G22*YDl7(I)+G32*ZDl7(I)
          ZDL_a=G13*XDl7(I)+G23*YDl7(I)+G33*ZDl7(I)
          XDl7(I)=XDL_a
          YDl7(I)=YDL_a
          ZDl7(I)=ZDL_a
          XDL_a=G11*XDl8(I)+G21*YDl8(I)+G31*ZDl8(I)
          YDL_a=G12*XDl8(I)+G22*YDl8(I)+G32*ZDl8(I)
          ZDL_a=G13*XDl8(I)+G23*YDl8(I)+G33*ZDl8(I)
          XDl8(I)=XDL_a
          YDl8(I)=YDL_a
          ZDl8(I)=ZDL_a
!C         MATRICE DE PASSAGE GLOBAL -> ORTHOTROPE.
           T11=R11(I)*G11+R12(I)*G21+R13(I)*G31
           T12=R11(I)*G12+R12(I)*G22+R13(I)*G32
           T13=R11(I)*G13+R12(I)*G23+R13(I)*G33
           T21=R21(I)*G11+R22(I)*G21+R23(I)*G31
           T22=R21(I)*G12+R22(I)*G22+R23(I)*G32
           T23=R21(I)*G13+R22(I)*G23+R23(I)*G33
           T31=R31(I)*G11+R32(I)*G21+R33(I)*G31
           T32=R31(I)*G12+R32(I)*G22+R33(I)*G32
           T33=R31(I)*G13+R32(I)*G23+R33(I)*G33
           R11(I)=T11
           R12(I)=T12
           R13(I)=T13
           R21(I)=T21
           R22(I)=T22
           R23(I)=T23
           R31(I)=T31
           R32(I)=T32
           R33(I)=T33
        ENDDO
      ENDIF   

!ok

        do i = 1,nel

            xd1(i) = xdl1(i)
            yd1(i) = ydl1(i)
            zd1(i) = zdl1(i)
!
            xd2(i) = xdl2(i)
            yd2(i) = ydl2(i)
            zd2(i) = zdl2(i)
!
            xd3(i) = xdl3(i)
            yd3(i) = ydl3(i)
            zd3(i) = zdl3(i)
!
            xd4(i) = xdl5(i)
            yd4(i) = ydl5(i)
            zd4(i) = zdl5(i)
!
            xd5(i) = xdl6(i)
            yd5(i) = ydl6(i)
            zd5(i) = zdl6(i)
!
            xd6(i) = xdl7(i) 
            yd6(i) = ydl7(i) 
            zd6(i) = zdl7(i) 

            x1(i) = xd1(i) 
            y1(i) = yd1(i) 
            z1(i) = zd1(i)
!
            x2(i) = xd2(i)
            y2(i) = yd2(i)
            z2(i) = zd2(i)
!
            x3(i) = xd3(i)
            y3(i) = yd3(i)
            z3(i) = zd3(i)
!
            x4(i) = xd4(i)
            y4(i) = yd4(i)
            z4(i) = zd4(i)
!
            x5(i) = xd5(i)
            y5(i) = yd5(i)
            z5(i) = zd5(i)
!
            x6(i) = xd6(i) 
            y6(i) = yd6(i) 
            z6(i) = zd6(i) 
      ENDDO  
!ok      
      !< apply rotation transformation to degenerated velocities
      call srrota3(                                                           &
        r11     , r12     , r13     , r21     ,                               &
        r22     , r23     , r31     , r32     ,                               &
        r33     , vxd1    , vxd2    , vxd3    ,                               &
        vxd4    , vxd5    , vxd6    , vxd7    ,                               &
        vxd8    , vyd1    , vyd2    , vyd3    ,                               &
        vyd4    , vyd5    , vyd6    , vyd7    ,                               &
        vyd8    , vzd1    , vzd2    , vzd3    ,                               &
        vzd4    , vzd5    , vzd6    , vzd7    ,                               &
        vzd8    , nel     )
!

          DO I= 1,nel
!
            vx1(i) = vxd1(i)
            vy1(i) = vyd1(i)
            vz1(i) = vzd1(i)
!
            vx2(i) = vxd2(i)
            vy2(i) = vyd2(i)
            vz2(i) = vzd2(i)
!
            vx3(i) = vxd3(i)
            vy3(i) = vyd3(i)
            vz3(i) = vzd3(i)
!
            vx4(i) = vxd5(i)
            vy4(i) = vyd5(i)
            vz4(i) = vzd5(i)
!
            vx5(i) = vxd6(i)
            vy5(i) = vyd6(i)
            vz5(i) = vzd6(i)
!
            vx6(i) = vxd7(i)
            vy6(i) = vyd7(i)
            vz6(i) = vzd7(i)
!
          enddo


          IF(OFF_L < ZERO)THEN
            DO I= 1,nel
              IF(OFFG(I) < ZERO)THEN
                VX1(I)=ZERO
                VY1(I)=ZERO
                VZ1(I)=ZERO
                VX2(I)=ZERO
                VY2(I)=ZERO
                VZ2(I)=ZERO
                VX3(I)=ZERO
                VY3(I)=ZERO
                VZ3(I)=ZERO
                VX4(I)=ZERO
                VY4(I)=ZERO
                VZ4(I)=ZERO
                VX5(I)=ZERO
                VY5(I)=ZERO
                VZ5(I)=ZERO
                VX6(I)=ZERO
                VY6(I)=ZERO
                VZ6(I)=ZERO
              ENDIF
            ENDDO
          ENDIF


          do i = 1,nel
            vd2(i) = zero
          enddo

        end subroutine s6zrcoor3
      end module s6zrcoor3_mod

