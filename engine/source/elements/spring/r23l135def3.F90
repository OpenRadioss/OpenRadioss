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
!||====================================================================
!||    r23l135def3_mod   ../engine/source/elements/spring/r23l135def3.F90
!||--- called by ------------------------------------------------------
!||    r23law135         ../engine/source/elements/spring/r23law135.F90
!||====================================================================
      module r23l135def3_mod
      contains
!||====================================================================
!||    r23l135def3        ../engine/source/elements/spring/r23l135def3.F90
!||--- called by ------------------------------------------------------
!||    r23law135          ../engine/source/elements/spring/r23law135.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod       ../common_source/modules/constant_mod.F
!||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    mvsiz_mod          ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod      ../common_source/modules/precision_mod.F90
!||    redef3_mod         ../engine/source/elements/spring/redef3.F90
!||====================================================================
        subroutine r23l135def3( &
          pid      ,skew     ,  &
          geo      ,fx       ,fy       ,fz       ,      &
          dx       ,dy       ,dz       ,      &
          x0       ,y0       ,z0       ,xmom     ,      &
          ymom     ,zmom     ,rx       ,ry       ,      &
          rz       ,v        ,nel      ,      &
          x0_err   ,x1dp     ,x2dp     ,xkr      ,      &
          exx      ,eyx      ,ezx      ,exy      ,      &
          eyy      ,ezy      ,exz      ,eyz      ,      &
          ezz      ,xcr      ,rx1      ,ry1      ,      &
          rz1      ,rx2      ,ry2      ,rz2      ,      &
          xin      ,xm       ,xkm      ,      &
          xcm      ,nc1      ,nc2      ,nuvar    ,      &
          uvar     ,mass     ,dx0      ,dy0      ,      &
          dz0      ,rx0      ,ry0      ,rz0      ,      &
          iequil   ,skew_id  ,mat_param,      &
          lskew    ,npropg   ,                &
          inispri  ,scodver  ,ismdisp  ,numelr   ,      &
          tt       ,dt1      ,                          &
          anim_fe  ,numskw   ,numgeo  ,numnod ) 

!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
          use precision_mod,    only : wp
          use constant_mod,     only : zero, half, one
          use mvsiz_mod,        only : mvsiz
          use redef3_mod
          use matparam_def_mod


!-------------------------------------------------------------------------------
!   I m p l i c i t   t y p e s
!-------------------------------------------------------------------------------
          implicit none

!-------------------------------------------------------------------------------
!   D u m m y   A r g u m e n t s
!-------------------------------------------------------------------------------
          integer,                          intent(in)    :: nel       !< Number of elements
          integer,                          intent(in)    :: nuvar     !< Number of user variables
          
          integer,                          intent(in)    :: lskew     !< Number of skew definitions
          integer,                          intent(in)    :: npropg    !< Number of real geometry parameters
          integer,                          intent(in)    :: inispri   !< Initial spring flag
          integer,                          intent(in)    :: scodver   !< Code version
          integer,                          intent(in)    :: ismdisp   !< Mass damping flag
          integer,                          intent(in)    :: numelr    !< Number of elements for result output
          integer,                          intent(in)    :: numskw    !< Number of skews
          integer,                          intent(in)    :: numgeo     !< Number of geometry parameters
          integer,                          intent(in)    :: numnod     !< Number of nodes
         
          real(kind=wp),                    intent(in)    :: tt        !< Time
          real(kind=wp),                    intent(in)    :: dt1       !< Time step
          integer, dimension(20),           intent(in)    :: anim_fe   !< Animation flag array

          integer, dimension(nel),            intent(in)    :: pid       !< Property ID array
          integer, dimension(nel),            intent(in)    :: nc1       !< Node 1 connectivity
          integer, dimension(nel),            intent(in)    :: nc2       !< Node 2 connectivity
          integer, dimension(nel),            intent(inout) :: iequil    !< Equilibrium flag
          integer, dimension(nel),            intent(in)    :: skew_id   !< Skew ID array

          real(kind=wp), dimension(lskew,numskw),intent(in)    :: skew      !< Skew frames
          real(kind=wp), dimension(npropg,numgeo),intent(in)   :: geo       !< Geometry parameters
          real(kind=wp), dimension(3,numnod),    intent(in)    :: v         !< Velocities      
          real(kind=wp), dimension(nel),      intent(in)    :: mass      !< Mass
          real(kind=wp), dimension(nel),      intent(in)    :: dx0       !< Initial DX
          real(kind=wp), dimension(nel),      intent(in)    :: dy0       !< Initial DY
          real(kind=wp), dimension(nel),      intent(in)    :: dz0       !< Initial DZ
          real(kind=wp), dimension(nel),      intent(in)    :: rx0       !< Initial RX
          real(kind=wp), dimension(nel),      intent(in)    :: ry0       !< Initial RY
          real(kind=wp), dimension(nel),      intent(in)    :: rz0       !< Initial RZ

          real(kind=wp), dimension(nel),      intent(inout) :: fx        !< Force X
          real(kind=wp), dimension(nel),      intent(inout) :: fy        !< Force Y
          real(kind=wp), dimension(nel),      intent(inout) :: fz        !< Force Z
          real(kind=wp), dimension(nel),      intent(inout) :: dx        !< Displacement X
          real(kind=wp), dimension(nel),      intent(inout) :: dy        !< Displacement Y
          real(kind=wp), dimension(nel),      intent(inout) :: dz        !< Displacement Z
          real(kind=wp), dimension(nel),      intent(inout) :: x0        !< Coordinate X
          real(kind=wp), dimension(nel),      intent(inout) :: y0        !< Coordinate Y
          real(kind=wp), dimension(nel),      intent(inout) :: z0        !< Coordinate Z
          real(kind=wp), dimension(nel),      intent(inout) :: xmom      !< Moment X
          real(kind=wp), dimension(nel),      intent(inout) :: ymom      !< Moment Y
          real(kind=wp), dimension(nel),      intent(inout) :: zmom      !< Moment Z
          real(kind=wp), dimension(nel),      intent(inout) :: rx        !< Rotation X
          real(kind=wp), dimension(nel),      intent(inout) :: ry        !< Rotation Y
          real(kind=wp), dimension(nel),      intent(inout) :: rz        !< Rotation Z

          real(kind=wp), dimension(3,nel),    intent(inout) :: x0_err    !< Double to WP error tracking

          real(kind=wp), dimension(nel),  intent(inout) :: exx       !< Skew X-X
          real(kind=wp), dimension(nel),  intent(inout) :: eyx       !< Skew Y-X
          real(kind=wp), dimension(nel),  intent(inout) :: ezx       !< Skew Z-X
          real(kind=wp), dimension(nel),  intent(inout) :: exy       !< Skew X-Y
          real(kind=wp), dimension(nel),  intent(inout) :: eyy       !< Skew Y-Y
          real(kind=wp), dimension(nel),  intent(inout) :: ezy       !< Skew Z-Y
          real(kind=wp), dimension(nel),  intent(inout) :: exz       !< Skew X-Z
          real(kind=wp), dimension(nel),  intent(inout) :: eyz       !< Skew Y-Z
          real(kind=wp), dimension(nel),  intent(inout) :: ezz       !< Skew Z-Z
          real(kind=wp), dimension(nel),  intent(inout) :: xcr       !< Critical property
          real(kind=wp), dimension(nel),  intent(inout) :: rx1       !< Node 1 rotation X
          real(kind=wp), dimension(nel),  intent(inout) :: ry1       !< Node 1 rotation Y
          real(kind=wp), dimension(nel),  intent(inout) :: rz1       !< Node 1 rotation Z
          real(kind=wp), dimension(nel),  intent(inout) :: rx2       !< Node 2 rotation X
          real(kind=wp), dimension(nel),  intent(inout) :: ry2       !< Node 2 rotation Y
          real(kind=wp), dimension(nel),  intent(inout) :: rz2       !< Node 2 rotation Z
          real(kind=wp), dimension(nel),  intent(inout) :: xin       !< Input geometry mapping
          real(kind=wp), dimension(nel),  intent(inout) :: xm        !< Mass mapping
          real(kind=wp), dimension(nel),  intent(inout) :: xkm       !< Max stiffness
          real(kind=wp), dimension(nel),  intent(inout) :: xcm       !< Max damping
          real(kind=wp), dimension(nel),  intent(inout) :: xkr       !< Max rotational stiffness

          real(kind=8),  dimension(3,nel),    intent(in)    :: x1dp      !< Node 1 coordinates (Double precision)
          real(kind=8),  dimension(3,nel),    intent(in)    :: x2dp      !< Node 2 coordinates (Double precision)

          real(kind=wp), dimension(nuvar,nel),intent(inout), target :: uvar  !< User variables (Target for pointers)

          type(matparam_struct_),           intent(in),    target :: mat_param !< Material parameter structure

!-------------------------------------------------------------------------------
!   L o c a l   V a r i a b l e s
!-------------------------------------------------------------------------------
          integer :: i, j, isk, kk, nindx, israte
          integer :: i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, i14, i15
          integer :: nupar, iadbuf, if1, if2, if3, if4

          integer, dimension(nel) :: ifunc2, iecrou, ifunc, ifv, indx, ifunc3
          integer, dimension(nel) :: ifail, ifail2

          real(kind=wp), dimension(nel) :: xk, yk, zk, xc, yc, zc, xhr, xh
          real(kind=wp), dimension(nel) :: dxold, dyold, dzold, dv, epla, xl0, rscale
          real(kind=wp), dimension(nel) :: b, d, dmn, dmx, crit, x21, y21, z21, lscale, ee
          real(kind=wp), dimension(nel) :: gf3, hx, hy, hz, x0_ini, y0_ini, z0_ini
          real(kind=wp), dimension(nel) :: max_slope
          real(kind=wp), dimension(nel) :: vx, vy, vz, vrx, vry, vrz

          real(kind=wp) :: sx, sy, sz, xx, yy, zz, xka, yka, zka, asrate, dlim, ms_tmp, in_tmp, dt_tmp
          real(kind=wp), dimension(2), target :: not_used

          real(kind=8), dimension(nel) :: x21dp, y21dp, z21dp, x0dp, y0dp, z0dp

!===============================================================================
!   B o d y
!===============================================================================
          not_used = zero
          
          nupar = 4
          i1  = nupar
          i2  = i1 + 6 
          i3  = i2 + 6 
          i4  = i3 + 6 
          i5  = i4 + 6 
          i6  = i5 + 6 
          i7  = i6 + 6 
          i8  = i7 + 6 
          i9  = i8 + 6 
          i10 = i9 + 6 
          i11 = i10 + 6 
          i12 = i11 + 6 
          i13 = i12 + 6
          i14 = i13 + 6
          i15 = i14 + 6 + 2 
          nupar = nupar + 84

          !---------------------------------------------------------------------
          ! Element initialization mapping
          !---------------------------------------------------------------------
          do i = 1, nel
          
            epla(i) = zero
            xm(i) = mass(i)


            xkm(i) = uvar(17,i)
            xcm(I) = 0.

            isk = skew_id(i)

            exx(i) = skew(1,isk)
            eyx(i) = skew(2,isk)
            ezx(i) = skew(3,isk)
            exy(i) = skew(4,isk)
            eyy(i) = skew(5,isk)
            ezy(i) = skew(6,isk)
            exz(i) = skew(7,isk)
            eyz(i) = skew(8,isk)
            ezz(i) = skew(9,isk)



            xl0(i) = one
            iequil(i) = 1 
          end do

          !---------------------
          ! TRANSLATIONS
          !---------------------
          do i = 1, nel
            dxold(i) = dx(i)
            dyold(i) = dy(i)
            dzold(i) = dz(i)
          end do

          if (inispri /= 0 .and. tt == zero) then 
            do i = 1, nel
              dxold(i) = dx0(i)
              dyold(i) = dy0(i)
              dzold(i) = dz0(i)
            end do
          end if

          if (inispri /= 0 .and. tt == zero) then
            do i = 1, nel
              x0_ini(i) = x0(i)
              y0_ini(i) = y0(i)
              z0_ini(i) = z0(i)
            end do
          end if

          do i = 1, nel
            x21dp(i) = x2dp(1,i) - x1dp(1,i)
            y21dp(i) = x2dp(2,i) - x1dp(2,i)
            z21dp(i) = x2dp(3,i) - x1dp(3,i)
            x21(i) = x21dp(i)
            y21(i) = y21dp(i)
            z21(i) = z21dp(i)
          end do

          if (tt == zero) then
            do i = 1, nel
              x0dp(i) = x21dp(i)*exx(i) + y21dp(i)*eyx(i) + z21dp(i)*ezx(i)
              y0dp(i) = x21dp(i)*exy(i) + y21dp(i)*eyy(i) + z21dp(i)*ezy(i)
              z0dp(i) = x21dp(i)*exz(i) + y21dp(i)*eyz(i) + z21dp(i)*ezz(i)
              x0(i) = x0dp(i) ! cast double to WP
              y0(i) = y0dp(i) ! cast double to WP
              z0(i) = z0dp(i) ! cast double to WP
            end do

            if (inispri /= 0) then
              ! Condition needed for spring type 8, not concerned by /INISPRI,
              ! and having initial length /= 0 
              do i = 1, nel
                if (x0_ini(i) == zero .and. dx0(i) == zero) x0_ini(i) = x0dp(i)
                if (y0_ini(i) == zero .and. dy0(i) == zero) y0_ini(i) = y0dp(i)
                if (z0_ini(i) == zero .and. dz0(i) == zero) z0_ini(i) = z0dp(i)
              end do
            end if
          end if

          if (scodver >= 101) then
            if (tt == zero) then
              do i = 1, nel
                x0_err(1,i) = x0dp(i) - x0(i) ! difference between double and WP
                x0_err(2,i) = y0dp(i) - y0(i) 
                x0_err(3,i) = z0dp(i) - z0(i) 
              end do
            end if
          end if

          if (inispri /= 0 .and. tt == zero) then
            do i = 1, nel
              x0(i) = x0_ini(i)
              y0(i) = y0_ini(i)
              z0(i) = z0_ini(i)
            end do
          end if

          do i = 1, nel
            x0dp(i) = x0(i) ! cast WP to double
            y0dp(i) = y0(i)
            z0dp(i) = z0(i)
          end do

          if (scodver >= 101) then
            do i = 1, nel
              ! AL_DP must be recomputed to be sure of absolute consistency
              x0dp(i) = x0dp(i) + x0_err(1,i) 
              y0dp(i) = y0dp(i) + x0_err(2,i) 
              z0dp(i) = z0dp(i) + x0_err(3,i) 
            end do
          end if

          if (ismdisp > 0) then
            do i = 1, nel
              if (iequil(i) == 1) then
                sx = half * (rx2(i) + rx1(i))
                sy = half * (ry2(i) + ry1(i))
                sz = half * (rz2(i) + rz1(i))
                xx = y21(i)*sz - z21(i)*sy
                yy = z21(i)*sx - x21(i)*sz
                zz = x21(i)*sy - y21(i)*sx
                xx = (v(1,nc2(i)) - v(1,nc1(i)) + xx) * dt1
                yy = (v(2,nc2(i)) - v(2,nc1(i)) + yy) * dt1
                zz = (v(3,nc2(i)) - v(3,nc1(i)) + zz) * dt1
              else
                xx = (v(1,nc2(i)) - v(1,nc1(i))) * dt1
                yy = (v(2,nc2(i)) - v(2,nc1(i))) * dt1
                zz = (v(3,nc2(i)) - v(3,nc1(i))) * dt1
              end if
              dx(i) = dxold(i) + xx*exx(i) + yy*eyx(i) + zz*ezx(i)
              dy(i) = dyold(i) + xx*exy(i) + yy*eyy(i) + zz*ezy(i)
              dz(i) = dzold(i) + xx*exz(i) + yy*eyz(i) + zz*ezz(i)
              crit(i) = zero
            end do
          else
            do i = 1, nel
              if (iequil(i) == 1) then
                sx = half * (rx2(i) + rx1(i))
                sy = half * (ry2(i) + ry1(i))
                sz = half * (rz2(i) + rz1(i))
                xx = y21(i)*sz - z21(i)*sy
                yy = z21(i)*sx - x21(i)*sz
                zz = x21(i)*sy - y21(i)*sx
                xx = (v(1,nc2(i)) - v(1,nc1(i)) + xx) * dt1
                yy = (v(2,nc2(i)) - v(2,nc1(i)) + yy) * dt1
                zz = (v(3,nc2(i)) - v(3,nc1(i)) + zz) * dt1
                dx(i) = dxold(i) + xx*exx(i) + yy*eyx(i) + zz*ezx(i)
                dy(i) = dyold(i) + xx*exy(i) + yy*eyy(i) + zz*ezy(i)
                dz(i) = dzold(i) + xx*exz(i) + yy*eyz(i) + zz*ezz(i)
              else
                dx(i) = x21dp(i)*exx(i) + y21dp(i)*eyx(i) + z21dp(i)*ezx(i) - x0dp(i)
                dy(i) = x21dp(i)*exy(i) + y21dp(i)*eyy(i) + z21dp(i)*ezy(i) - y0dp(i)
                dz(i) = x21dp(i)*exz(i) + y21dp(i)*eyz(i) + z21dp(i)*ezz(i) - z0dp(i)
              end if
              crit(i) = zero
            end do
          end if

          !-------------------------------
          nindx = 0 
          if1 = 0
          if2 = 6
          if3 = 12
          if4 = 18

          do i = 1, nel
            dxold(i) = dx(i)
            dyold(i) = dy(i)
            dzold(i) = dz(i)
          end do

          !------------------------------- FX     



          do i = 1, nel  
            if (mat_param%iparam(1) == 1) then
              xk(i) = uvar(17,i)*mat_param%uparam(1)    
            else ! if (mat_param%iparam(1) == 0) then 
              xk(i) = zero
            end if  
            fx(i) = xk(i) * dx(i) 
          end do


          !------------------------------- FY     
          
          kk = 1 + numelr * anim_fe(11)


          do i = 1, nel      
            if (mat_param%iparam(2) == 1) then
              yk(i) = uvar(17,i)*mat_param%uparam(1)       
            else !  if (mat_param%iparam(2) == 0) then 
              yk(i) = zero
            end if  
            fy(i) = yk(i) * dy(i)    
          end do
          
          !------------------------------- FZ      
          
          kk = 1 + numelr * (anim_fe(11) + anim_fe(12))
          


          do i = 1, nel      
            if (mat_param%iparam(3) == 1) then
              zk(i) = uvar(17,i)*mat_param%uparam(1)      
            else !if (mat_param%iparam(3) == 0) then 
              zk(i) = zero
            end if  
            fz(i) = zk(i) * dz(i) 
          end do

 

          !---------------------
          ! ROTATIONS
          !---------------------
          !------------------------------- XMOM 
          do i = 1, nel
            xin(i) = geo(2,pid(i))
            xkr(i) = uvar(18,i)   
            XCR(I) = 0.

          end do

          do i = 1, nel
            dxold(i) = rx(i)
            dyold(i) = ry(i)
            dzold(i) = rz(i)
          end do

          if (inispri /= 0 .and. tt == zero) then
            do i = 1, nel
              dxold(i) = rx0(i)
              dyold(i) = ry0(i)
              dzold(i) = rz0(i)
            end do
          end if

          do i = 1, nel
            x21(i) = (rx2(i) - rx1(i)) * dt1
            y21(i) = (ry2(i) - ry1(i)) * dt1
            z21(i) = (rz2(i) - rz1(i)) * dt1
            rx(i)  = dxold(i) + x21(i)*exx(i) + y21(i)*eyx(i) + z21(i)*ezx(i)
            ry(i)  = dyold(i) + x21(i)*exy(i) + y21(i)*eyy(i) + z21(i)*ezy(i)
            rz(i)  = dzold(i) + x21(i)*exz(i) + y21(i)*eyz(i) + z21(i)*ezz(i)
          end do

          !-------------------------------

          do i = 1, nel      
            if (mat_param%iparam(4) == 1) then
              xk(i) = uvar(18,i)*mat_param%uparam(2)       
            else if (mat_param%iparam(4) == 0) then 
              xk(i) = zero
            end if  
            xmom(i) = xk(i) * rx(i) 
          end do



          !-------------------------------------
          !------------------------------- YMOM 



          do i = 1, nel      
            if (mat_param%iparam(5) == 1) then
              yk(i) = uvar(18,i)*mat_param%uparam(2)       
            else if (mat_param%iparam(5) == 0) then 
              yk(i) = zero
            end if  
            ymom(i) = yk(i) * ry(i)  
          end do


          !-----------------------------------
          !------------------------------- ZMOM 


          do i = 1, nel      
            if (mat_param%iparam(6) == 1) then
              zk(i) = uvar(18,i)*mat_param%uparam(2)       
            else if (mat_param%iparam(6) == 0) then 
              zk(i) = zero
            end if  
            zmom(i) = zk(i) * rz(i)  
          end do


        end subroutine r23l135def3

      end module r23l135def3_mod