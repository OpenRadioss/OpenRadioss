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
!||    s6zrcoor3       ../engine/source/elements/solid/solide6z/s6zrcoor3.F90
!||--- called by ------------------------------------------------------
!||    s6zforc3        ../engine/source/elements/solid/solide6z/s6zforc3.F90
!||--- calls      -----------------------------------------------------
!||    sorthdir3       ../engine/source/elements/solid/solide/sorthdir3.F
!||    sortho3         ../engine/source/elements/solid/solide/sortho3.F
!||    srepiso3        ../engine/source/elements/solid/solide/srepiso3.F
!||    srrota3         ../engine/source/elements/solid/solide/srrota3.F
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    precision_mod   ../common_source/modules/precision_mod.F90
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
        irep    , ismstr  , isorth  , jlag    )
!
!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
      use precision_mod, only : wp
      use constant_mod , only : zero, one, two, one_over_6, em20
!-------------------------------------------------------------------------------
!    I m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
      implicit none
!-------------------------------------------------------------------------------
!    D u m m y   a r g u m e n t s
!-------------------------------------------------------------------------------
      integer,                             intent(in)    :: numnod   !< Total number of nodes
      integer,                             intent(in)    :: nixs     !< Number of columns in connectivity array
      integer,                             intent(in)    :: irep     !< Iteration flag for coordinate system
      integer,                             intent(in)    :: ismstr   !< Small strain flag
      integer,                             intent(in)    :: isorth   !< Orthotropic material flag  
      integer,                             intent(in)    :: jlag     !< Lagrangian flag
      integer,                             intent(in)    :: nel      !< Number of elements
      real(kind=WP), dimension(3,numnod),  intent(in)    :: x        !< Nodal coordinates
      integer, dimension(nixs,nel),        intent(in)    :: ixs      !< element connectivity
      real(kind=WP), dimension(3,numnod),  intent(in)    :: v        !< Nodal velocities
      real(kind=WP), dimension(nel,6),     intent(inout) :: gama     !< Material orientation matrix
      real(kind=WP), dimension(nel,6),     intent(in)    :: gama0    !< Initial material orientation
      real(kind=WP), dimension(nel),       intent(out)   :: x1       !< Local x-coordinates node 1
      real(kind=WP), dimension(nel),       intent(out)   :: x2       !< Local x-coordinates node 2
      real(kind=WP), dimension(nel),       intent(out)   :: x3       !< Local x-coordinates node 3
      real(kind=WP), dimension(nel),       intent(out)   :: x4       !< Local x-coordinates node 4
      real(kind=WP), dimension(nel),       intent(out)   :: x5       !< Local x-coordinates node 5
      real(kind=WP), dimension(nel),       intent(out)   :: x6       !< Local x-coordinates node 6
      real(kind=WP), dimension(nel),       intent(out)   :: y1       !< Local y-coordinates node 1
      real(kind=WP), dimension(nel),       intent(out)   :: y2       !< Local y-coordinates node 2
      real(kind=WP), dimension(nel),       intent(out)   :: y3       !< Local y-coordinates node 3
      real(kind=WP), dimension(nel),       intent(out)   :: y4       !< Local y-coordinates node 4
      real(kind=WP), dimension(nel),       intent(out)   :: y5       !< Local y-coordinates node 5
      real(kind=WP), dimension(nel),       intent(out)   :: y6       !< Local y-coordinates node 6
      real(kind=WP), dimension(nel),       intent(out)   :: z1       !< Local z-coordinates node 1
      real(kind=WP), dimension(nel),       intent(out)   :: z2       !< Local z-coordinates node 2
      real(kind=WP), dimension(nel),       intent(out)   :: z3       !< Local z-coordinates node 3
      real(kind=WP), dimension(nel),       intent(out)   :: z4       !< Local z-coordinates node 4
      real(kind=WP), dimension(nel),       intent(out)   :: z5       !< Local z-coordinates node 5
      real(kind=WP), dimension(nel),       intent(out)   :: z6       !< Local z-coordinates node 6
      real(kind=WP), dimension(nel),       intent(out)   :: vx1      !< Local x-velocities node 1
      real(kind=WP), dimension(nel),       intent(out)   :: vx2      !< Local x-velocities node 2
      real(kind=WP), dimension(nel),       intent(out)   :: vx3      !< Local x-velocities node 3
      real(kind=WP), dimension(nel),       intent(out)   :: vx4      !< Local x-velocities node 4
      real(kind=WP), dimension(nel),       intent(out)   :: vx5      !< Local x-velocities node 5
      real(kind=WP), dimension(nel),       intent(out)   :: vx6      !< Local x-velocities node 6
      real(kind=WP), dimension(nel),       intent(out)   :: vy1      !< Local y-velocities node 1
      real(kind=WP), dimension(nel),       intent(out)   :: vy2      !< Local y-velocities node 2
      real(kind=WP), dimension(nel),       intent(out)   :: vy3      !< Local y-velocities node 3
      real(kind=WP), dimension(nel),       intent(out)   :: vy4      !< Local y-velocities node 4
      real(kind=WP), dimension(nel),       intent(out)   :: vy5      !< Local y-velocities node 5
      real(kind=WP), dimension(nel),       intent(out)   :: vy6      !< Local y-velocities node 6
      real(kind=WP), dimension(nel),       intent(out)   :: vz1      !< Local z-velocities node 1
      real(kind=WP), dimension(nel),       intent(out)   :: vz2      !< Local z-velocities node 2
      real(kind=WP), dimension(nel),       intent(out)   :: vz3      !< Local z-velocities node 3
      real(kind=WP), dimension(nel),       intent(out)   :: vz4      !< Local z-velocities node 4
      real(kind=WP), dimension(nel),       intent(out)   :: vz5      !< Local z-velocities node 5
      real(kind=WP), dimension(nel),       intent(out)   :: vz6      !< Local z-velocities node 6
      real(kind=WP), dimension(nel),       intent(out)   :: vd2      !< Viscous damping coefficient
      real(kind=WP), dimension(nel),       intent(inout) :: vis      !< Viscosity
      real(kind=WP), dimension(nel),       intent(in)    :: offg     !< Global offset flag
      real(kind=WP), dimension(nel),       intent(out)   :: off      !< Local offset flag
      real(kind=8),  dimension(nel,15),    intent(in)    :: sav      !< Saved variables array sav must be in double precision, so kind = 8
      real(kind=WP), dimension(nel),       intent(in)    :: rho      !< Density
      real(kind=WP), dimension(nel),       intent(out)   :: rhoo     !< Initial density
      real(kind=WP), dimension(nel),       intent(out)   :: r11      !< Rotation matrix component (1,1)
      real(kind=WP), dimension(nel),       intent(out)   :: r12      !< Rotation matrix component (1,2)
      real(kind=WP), dimension(nel),       intent(out)   :: r13      !< Rotation matrix component (1,3)
      real(kind=WP), dimension(nel),       intent(out)   :: r21      !< Rotation matrix component (2,1)
      real(kind=WP), dimension(nel),       intent(out)   :: r22      !< Rotation matrix component (2,2)
      real(kind=WP), dimension(nel),       intent(out)   :: r23      !< Rotation matrix component (2,3)
      real(kind=WP), dimension(nel),       intent(out)   :: r31      !< Rotation matrix component (3,1)
      real(kind=WP), dimension(nel),       intent(out)   :: r32      !< Rotation matrix component (3,2)
      real(kind=WP), dimension(nel),       intent(out)   :: r33      !< Rotation matrix component (3,3)
      integer, dimension(nel),             intent(out)   :: nc1      !< Node connectivity index 1
      integer, dimension(nel),             intent(out)   :: nc2      !< Node connectivity index 2
      integer, dimension(nel),             intent(out)   :: nc3      !< Node connectivity index 3
      integer, dimension(nel),             intent(out)   :: nc4      !< Node connectivity index 4
      integer, dimension(nel),             intent(out)   :: nc5      !< Node connectivity index 5
      integer, dimension(nel),             intent(out)   :: nc6      !< Node connectivity index 6
      integer, dimension(nel),             intent(out)   :: ngl      !< Global element number
      integer, dimension(nel),             intent(out)   :: mxt      !< Material type
      integer, dimension(nel),             intent(out)   :: ngeo     !< Geometry number
      integer,                             intent(in)    :: ioutprt  !< Output print flag
      real(kind=WP), dimension(nel),       intent(out)   :: vgax     !< Global velocity x-component
      real(kind=WP), dimension(nel),       intent(out)   :: vgay     !< Global velocity y-component
      real(kind=WP), dimension(nel),       intent(out)   :: vgaz     !< Global velocity z-component
      real(kind=WP), dimension(nel),       intent(out)   :: vga2     !< Velocity squared
      real(kind=WP), dimension(nel),       intent(out)   :: xgax     !< Global coordinate x-component
      real(kind=WP), dimension(nel),       intent(out)   :: xgay     !< Global coordinate y-component
      real(kind=WP), dimension(nel),       intent(out)   :: xgaz     !< Global coordinate z-component
      real(kind=WP), dimension(nel),       intent(out)   :: xgxa2    !< X-coordinate squared term
      real(kind=WP), dimension(nel),       intent(out)   :: xgya2    !< Y-coordinate squared term
      real(kind=WP), dimension(nel),       intent(out)   :: xgza2    !< Z-coordinate squared term
      real(kind=WP), dimension(nel),       intent(out)   :: xgxya    !< XY cross product term
      real(kind=WP), dimension(nel),       intent(out)   :: xgyza    !< YZ cross product term
      real(kind=WP), dimension(nel),       intent(out)   :: xgzxa    !< ZX cross product term
      integer      , dimension(nel),       intent(in)    :: iparg    !< Integer parameters
      real(kind=WP), dimension(nel,6),     intent(out)   :: gama_r   !< Rotation matrix storage
!-------------------------------------------------------------------------------
!    L o c a l   v a r i a b l e s
!-------------------------------------------------------------------------------
      integer :: i                                                   !< Loop counter
      real(kind=WP) :: xl, yl, zl                                    !< Local centroid coordinates
      real(kind=WP) :: off_l                                         !< Local offset flag
      real(kind=WP), dimension(nel) :: xd1, xd2, xd3, xd4            !< Degenerated x-coordinates 1-4
      real(kind=WP), dimension(nel) :: xd5, xd6, xd7, xd8            !< Degenerated x-coordinates 5-8
      real(kind=WP), dimension(nel) :: yd1, yd2, yd3, yd4            !< Degenerated y-coordinates 1-4
      real(kind=WP), dimension(nel) :: yd5, yd6, yd7, yd8            !< Degenerated y-coordinates 5-8
      real(kind=WP), dimension(nel) :: zd1, zd2, zd3, zd4            !< Degenerated z-coordinates 1-4
      real(kind=WP), dimension(nel) :: zd5, zd6, zd7, zd8            !< Degenerated z-coordinates 5-8
      real(kind=WP), dimension(nel) :: vxd1, vxd2, vxd3, vxd4        !< Degenerated x-velocities 1-4
      real(kind=WP), dimension(nel) :: vxd5, vxd6, vxd7, vxd8        !< Degenerated x-velocities 5-8
      real(kind=WP), dimension(nel) :: vyd1, vyd2, vyd3, vyd4        !< Degenerated y-velocities 1-4
      real(kind=WP), dimension(nel) :: vyd5, vyd6, vyd7, vyd8        !< Degenerated y-velocities 5-8
      real(kind=WP), dimension(nel) :: vzd1, vzd2, vzd3, vzd4        !< Degenerated z-velocities 1-4
      real(kind=WP), dimension(nel) :: vzd5, vzd6, vzd7, vzd8        !< Degenerated z-velocities 5-8
      real(kind=WP) :: xdl, ydl, zdl                                 !< Local degenerated coordinates
      real(kind=WP), dimension(nel) :: rx, ry, rz                    !< Rotation matrix row 1
      real(kind=WP), dimension(nel) :: sx, sy, sz                    !< Rotation matrix row 2
      real(kind=WP), dimension(nel) :: tx, ty, tz                    !< Rotation matrix row 3
!===============================================================================
!     S o u r c e  l i n e s
!===============================================================================

      off_l  = zero 
      !< Recover nodal connectivity and material properties 
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
      !< Nodal coordinates
      do i = 1,nel
        x1(i)=x(1,nc1(i))
        y1(i)=x(2,nc1(i))
        z1(i)=x(3,nc1(i))
        x2(i)=x(1,nc2(i))
        y2(i)=x(2,nc2(i))
        z2(i)=x(3,nc2(i))
        x3(i)=x(1,nc3(i))
        y3(i)=x(2,nc3(i))
        z3(i)=x(3,nc3(i))
        x4(i)=x(1,nc4(i))
        y4(i)=x(2,nc4(i))
        z4(i)=x(3,nc4(i))
        x5(i)=x(1,nc5(i))
        y5(i)=x(2,nc5(i))
        z5(i)=x(3,nc5(i))
        x6(i)=x(1,nc6(i))
        y6(i)=x(2,nc6(i))
        z6(i)=x(3,nc6(i))
        off(i) = min(one,abs(offg(i)))
        off_l  = min(off_l,offg(i))
!
        xd1(i)=x1(i)
        yd1(i)=y1(i)
        zd1(i)=z1(i)
!
        xd2(i)=x2(i)
        yd2(i)=y2(i)
        zd2(i)=z2(i)
!
        xd3(i)=x3(i)
        yd3(i)=y3(i)
        zd3(i)=z3(i)
!
        xd4(i)=x3(i)  
        yd4(i)=y3(i)  
        zd4(i)=z3(i)  
!
        xd5(i)=x4(i)  
        yd5(i)=y4(i)  
        zd5(i)=z4(i)  
!
        xd6(i)=x5(i)  
        yd6(i)=y5(i)  
        zd6(i)=z5(i)  
!
        xd7(i)=x6(i)
        yd7(i)=y6(i)
        zd7(i)=z6(i)
!
        xd8(i)=x6(i)  
        yd8(i)=y6(i)  
        zd8(i)=z6(i)
      enddo
!
      !< Nodal velocities
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
!
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
!
      if(off_l<zero)then
        do i = 1,nel
          if (offg(i)<zero) then
            vx1(i)=zero
            vy1(i)=zero
            vz1(i)=zero
            vx2(i)=zero
            vy2(i)=zero
            vz2(i)=zero
            vx3(i)=zero
            vy3(i)=zero
            vz3(i)=zero
            vx4(i)=zero
            vy4(i)=zero
            vz4(i)=zero
            vx5(i)=zero
            vy5(i)=zero
            vz5(i)=zero
            vx6(i)=zero
            vy6(i)=zero
            vz6(i)=zero
          endif
        enddo
      endif
!
      !< Prepare PART output
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
           xgax(i)=x1(i)+x2(i)+x3(i)+x4(i)+x5(i)+x6(i)
           xgay(i)=y1(i)+y2(i)+y3(i)+y4(i)+y5(i)+y6(i)
           xgaz(i)=z1(i)+z2(i)+z3(i)+z4(i)+z5(i)+z6(i)
           xgxa2(i)=x1(i)**2+x2(i)**2+x3(i)**2+x4(i)**2 &
                   +x5(i)**2+x6(i)**2
           xgya2(i)=y1(i)**2+y2(i)**2+y3(i)**2+y4(i)**2 &
                   +y5(i)**2+y6(i)**2
           xgza2(i)=z1(i)**2+z2(i)**2+z3(i)**2+z4(i)**2 &
                   +z5(i)**2+z6(i)**2
           xgxya(i)=x1(i)*y1(i)+x2(i)*y2(i)+x3(i)*y3(i)+x4(i)*y4(i) &
                   +x5(i)*y5(i)+x6(i)*y6(i)
           xgyza(i)=y1(i)*z1(i)+y2(i)*z2(i)+y3(i)*z3(i)+y4(i)*z4(i) &
                   +y5(i)*z5(i)+y6(i)*z6(i)
           xgzxa(i)=z1(i)*x1(i)+z2(i)*x2(i)+z3(i)*x3(i)+z4(i)*x4(i) &
                   +z5(i)*x5(i)+z6(i)*x6(i)
          enddo
        endif
      endif
!
      !< Rotation matrix in the element frame
      call srepiso3( &
          xd1,     xd2,     xd3,     xd4, &
          xd5,     xd6,     xd7,     xd8, &
          yd1,     yd2,     yd3,     yd4, &
          yd5,     yd6,     yd7,     yd8, &
          zd1,     zd2,     zd3,     zd4, &
          zd5,     zd6,     zd7,     zd8, &
          rx,      ry,      rz,      sx, &
          sy,      sz,      tx,      ty, &
          tz,      nel)
      call sortho3( &
          rx,      ry,      rz,      sx, &
          sy,      sz,      tx,      ty, &
          tz,      r12,     r13,     r11, &
          r22,     r23,     r21,     r32, &
          r33,     r31,     nel)
!
      !< Store rotation matrix
      gama_r(1:nel,1) = r11(1:nel) ! dir1_x                            
      gama_r(1:nel,2) = r21(1:nel) ! dir1_y                            
      gama_r(1:nel,3) = r31(1:nel) ! dir1_z                            
      gama_r(1:nel,4) = r12(1:nel) ! dir2_x                            
      gama_r(1:nel,5) = r22(1:nel) ! dir2_y                           
      gama_r(1:nel,6) = r32(1:nel) ! dir2_z     
!
      !< Isotropic property
      if (isorth == 0) then
        do i = 1,nel                                    
          gama(i,1) = one                             
          gama(i,2) = zero                              
          gama(i,3) = zero
          gama(i,4) = zero                              
          gama(i,5) = one                              
          gama(i,6) = zero
        enddo      
      !< Orthotropic property
      else    
        call sorthdir3( &
          rx, ry, rz, sx, &
          sy, sz, tx, ty, &
          tz, r11, r12, r13, &
          r21, r22, r23, r31, &
          r32, r33, gama0, gama, &
          nel, irep)
      endif
!                   
      !< Rotate in the element frame
      if (ismstr<=4.and.jlag>0) then
        do i = 1,nel
          if (offg(i)>one) then
            x1(i) = sav(i,1)
            y1(i) = sav(i,2)
            z1(i) = sav(i,3)
            x2(i) = sav(i,4)
            y2(i) = sav(i,5)
            z2(i) = sav(i,6)
            x3(i) = sav(i,7)
            y3(i) = sav(i,8)
            z3(i) = sav(i,9)
            x4(i) = sav(i,10)
            y4(i) = sav(i,11)
            z4(i) = sav(i,12)
            x5(i) = sav(i,13)
            y5(i) = sav(i,14)
            z5(i) = sav(i,15)
            x6(i) = zero
            y6(i) = zero
            z6(i) = zero
            off(i) = offg(i) -one
            xl    = one_over_6*(x1(i)+x2(i)+x3(i)+x4(i)+x5(i)+x6(i))
            yl    = one_over_6*(y1(i)+y2(i)+y3(i)+y4(i)+y5(i)+y6(i))
            zl    = one_over_6*(z1(i)+z2(i)+z3(i)+z4(i)+z5(i)+z6(i))
            x1(i) = x1(i)-xl
            y1(i) = y1(i)-yl
            z1(i) = z1(i)-zl
            x2(i) = x2(i)-xl
            y2(i) = y2(i)-yl
            z2(i) = z2(i)-zl
            x3(i) = x3(i)-xl
            y3(i) = y3(i)-yl
            z3(i) = z3(i)-zl
            x4(i) = x4(i)-xl
            y4(i) = y4(i)-yl
            z4(i) = z4(i)-zl
            x5(i) = x5(i)-xl
            y5(i) = y5(i)-yl
            z5(i) = z5(i)-zl
            x6(i) = x6(i)-xl
            y6(i) = y6(i)-yl
            z6(i) = z6(i)-zl
          else
            xdl = r11(i)*xd1(i)+r21(i)*yd1(i)+r31(i)*zd1(i)
            ydl = r12(i)*xd1(i)+r22(i)*yd1(i)+r32(i)*zd1(i)
            zdl = r13(i)*xd1(i)+r23(i)*yd1(i)+r33(i)*zd1(i)
            xd1(i) = xdl
            yd1(i) = ydl
            zd1(i) = zdl
            xdl = r11(i)*xd2(i)+r21(i)*yd2(i)+r31(i)*zd2(i)
            ydl = r12(i)*xd2(i)+r22(i)*yd2(i)+r32(i)*zd2(i)
            zdl = r13(i)*xd2(i)+r23(i)*yd2(i)+r33(i)*zd2(i)
            xd2(i) = xdl
            yd2(i) = ydl
            zd2(i) = zdl
            xdl = r11(i)*xd3(i)+r21(i)*yd3(i)+r31(i)*zd3(i)
            ydl = r12(i)*xd3(i)+r22(i)*yd3(i)+r32(i)*zd3(i)
            zdl = r13(i)*xd3(i)+r23(i)*yd3(i)+r33(i)*zd3(i)
            xd3(i) = xdl
            yd3(i) = ydl
            zd3(i) = zdl
            xdl = r11(i)*xd4(i)+r21(i)*yd4(i)+r31(i)*zd4(i)
            ydl = r12(i)*xd4(i)+r22(i)*yd4(i)+r32(i)*zd4(i)
            zdl = r13(i)*xd4(i)+r23(i)*yd4(i)+r33(i)*zd4(i)
            xd4(i) = xdl
            yd4(i) = ydl
            zd4(i) = zdl
            xdl = r11(i)*xd5(i)+r21(i)*yd5(i)+r31(i)*zd5(i)
            ydl = r12(i)*xd5(i)+r22(i)*yd5(i)+r32(i)*zd5(i)
            zdl = r13(i)*xd5(i)+r23(i)*yd5(i)+r33(i)*zd5(i)
            xd5(i) = xdl
            yd5(i) = ydl
            zd5(i) = zdl
            xdl = r11(i)*xd6(i)+r21(i)*yd6(i)+r31(i)*zd6(i)
            ydl = r12(i)*xd6(i)+r22(i)*yd6(i)+r32(i)*zd6(i)
            zdl = r13(i)*xd6(i)+r23(i)*yd6(i)+r33(i)*zd6(i)
            xd6(i) = xdl
            yd6(i) = ydl
            zd6(i) = zdl
            xdl = r11(i)*xd7(i)+r21(i)*yd7(i)+r31(i)*zd7(i)
            ydl = r12(i)*xd7(i)+r22(i)*yd7(i)+r32(i)*zd7(i)
            zdl = r13(i)*xd7(i)+r23(i)*yd7(i)+r33(i)*zd7(i)
            xd7(i) = xdl
            yd7(i) = ydl
            zd7(i) = zdl
            xdl = r11(i)*xd8(i)+r21(i)*yd8(i)+r31(i)*zd8(i)
            ydl = r12(i)*xd8(i)+r22(i)*yd8(i)+r32(i)*zd8(i)
            zdl = r13(i)*xd8(i)+r23(i)*yd8(i)+r33(i)*zd8(i)
            xd8(i) = xdl
            yd8(i) = ydl
            zd8(i) = zdl              
            off(i) = offg(i)
          endif
        enddo
      endif
!
      do i = 1,nel
        vd2(i) = zero
      enddo
!
      !< Apply rotation transformation to degenerated velocities
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
      do i = 1,nel
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
        x4(i) = xd5(i) 
        y4(i) = yd5(i) 
        z4(i) = zd5(i) 
!
        x5(i) = xd6(i) 
        y5(i) = yd6(i)
        z5(i) = zd6(i) 
!
        x6(i) = xd7(i) 
        y6(i) = yd7(i) 
        z6(i) = zd7(i) 
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
      end subroutine s6zrcoor3
      end module s6zrcoor3_mod

