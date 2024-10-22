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
      !||====================================================================
      !||    s6chour_ctl_mod   ../engine/source/elements/thickshell/solide6c/s6chour_ctl.F90
      !||--- called by ------------------------------------------------------
      !||    s6cforc3          ../engine/source/elements/thickshell/solide6c/s6cforc3.F
      !||====================================================================
      module s6chour_ctl_mod
      contains
! ======================================================================================================================
! \brief new penta6 thick hourglass formulation for distordtion control
! ======================================================================================================================
      !||====================================================================
      !||    s6chour_ctl     ../engine/source/elements/thickshell/solide6c/s6chour_ctl.F90
      !||--- called by ------------------------------------------------------
      !||    s6cforc3        ../engine/source/elements/thickshell/solide6c/s6cforc3.F
      !||--- calls      -----------------------------------------------------
      !||    shour_ctl       ../engine/source/elements/solid/solidez/shour_ctl.F90
      !||--- uses       -----------------------------------------------------
      !||    constant_mod    ../common_source/modules/constant_mod.F
      !||    shour_ctl_mod   ../engine/source/elements/solid/solidez/shour_ctl.F90
      !||====================================================================
        subroutine s6chour_ctl(                                  &
            x1,      x2,       x3,      x5,       x6,      x7,   &
            y1,      y2,       y3,      y5,       y6,      y7,   &
            z1,      z2,       z3,      z5,       z6,      z7,   &
           vx1,     vx2,      vx3,     vx5,      vx6,     vx7,   &
           vy1,     vy2,      vy3,     vy5,      vy6,     vy7,   &
           vz1,     vz2,      vz3,     vz5,      vz6,     vz7,   &
           f11,     f12,      f13,     f15,      f16,     f17,   &
           f21,     f22,      f23,     f25,      f26,     f27,   &
           f31,     f32,      f33,     f35,      f36,     f37,   &
            pm, npropm ,   nummat,    mtn,       mxt,      dn,   &
           rho,    vol ,      ssp,   fhour,      off,    vol0,   &
          eint,     dt1,      sti,     nel)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
      use constant_mod,   only : zero,one_over_8,one_over_64
      use shour_ctl_mod,  only : shour_ctl
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
#include "mvsiz_p.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                           d u m m y   a r g u m e n t s
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                              :: nel              !< number of elements
          integer, intent(in)                              :: mtn              !< material type
          integer, intent(in)                              :: npropm           !< number of properties
          integer, intent(in)                              :: nummat           !< number of laws
          integer, dimension(mvsiz), intent(in   )         :: mxt              !< material id
          my_real, dimension(npropm,nummat) ,intent(in)    :: pm               !< material data
          my_real, dimension(nel,12)        ,intent(inout) :: fhour            !< hourglass stress 
          my_real, dimension(nel),   intent(in   )         :: rho              !< density
          my_real, dimension(mvsiz), intent(in   )         :: ssp              !< speed sound 
          my_real, dimension(mvsiz), intent(in   )         :: off              !< off value 
          my_real, dimension(mvsiz), intent(in   )         :: vol              !< volume 
          my_real, dimension(mvsiz), intent(in   )         :: vol0             !< initial volume 
          my_real, dimension(mvsiz) ,intent(in   )         ::             &
                           x1,x2,x3,x7,x5,x6,                             &
                           y1,y2,y3,y7,y5,y6,                             &
                           z1,z2,z3,z7,z5,z6                                   !< nodal coordinates
          my_real, dimension(mvsiz) ,intent(in   )         ::             &
                           vx1,vx2,vx3,vx7,vx5,vx6,                       &
                           vy1,vy2,vy3,vy7,vy5,vy6,                       &
                           vz1,vz2,vz3,vz7,vz5,vz6                             !< nodal velocity
          my_real, dimension(mvsiz) ,intent(inout)         ::             &
                           f11,f12,f13,f15,f16,f17,                       &
                           f21,f22,f23,f25,f26,f27,                       &
                           f31,f32,f33,f35,f36,f37                             !< nodal internal force
         my_real, dimension(mvsiz)  ,intent(inout)         :: eint             !< internal energy (hourglass)
         my_real, dimension(mvsiz)  ,intent(inout)         :: sti              !< nodal stiffness
         my_real, intent(in   )                            :: dt1              !< time step
         my_real, intent(in   )                            :: dn               !< coef of viscous hourglass 
! ----------------------------------------------------------------------------------------------------------------------
!                                        l o c a l   v a r i a b l e s
! ----------------------------------------------------------------------------------------------------------------------
      integer i, j
!
      my_real    f14(mvsiz), f18(mvsiz), f24(mvsiz), f28(mvsiz),          &
                 f34(mvsiz), f38(mvsiz)
      my_real  dett ,jaci1, jaci2, jaci3,jaci4, jaci5, jaci6,             &
               jaci7, jaci8, jaci9, jaci12, jaci45, jaci78,               &
               x_17_46 , x_28_35 ,y_17_46 , y_28_35 ,z_17_46 , z_28_35 ,  &
               x17(mvsiz) , x28(mvsiz) , x35(mvsiz) , x46(mvsiz),         &
               y17(mvsiz) , y28(mvsiz) , y35(mvsiz) , y46(mvsiz),         &
               z17(mvsiz) , z28(mvsiz) , z35(mvsiz) , z46(mvsiz),         &
               hx,hy,hz,h1x,h1y,h1z,h2x,h2y,h2z,h3x,h3y,h3z,h4x,h4y,h4z,  &
               px1(mvsiz), px2(mvsiz), px3(mvsiz), px4(mvsiz),            &
               py1(mvsiz), py2(mvsiz), py3(mvsiz), py4(mvsiz),            &
               pz1(mvsiz), pz2(mvsiz), pz3(mvsiz), pz4(mvsiz),            &
               px1h1(mvsiz),px2h1(mvsiz),px3h1(mvsiz),px4h1(mvsiz),       &
               px1h2(mvsiz),px2h2(mvsiz),px3h2(mvsiz),px4h2(mvsiz),       &
               px1h3(mvsiz),px2h3(mvsiz),px3h3(mvsiz),px4h3(mvsiz),       &
               jac_59_68(mvsiz), jac_67_49(mvsiz), jac_48_57(mvsiz),      &
               jac_19_37(mvsiz),jac1(mvsiz),jac2(mvsiz),jac3(mvsiz),      &
               jac4(mvsiz),jac5(mvsiz),jac6(mvsiz),                       &
               jac7(mvsiz),jac8(mvsiz),jac9(mvsiz),det(mvsiz)
! ----------------------------------------------------------------------------------------------------------------------
      do i=1,nel          ! 4=3,8=7
        x17(i)=x7(i)-x1(i)
        x28(i)=x7(i)-x2(i)
        x35(i)=x5(i)-x3(i)
        x46(i)=x6(i)-x3(i)
        y17(i)=y7(i)-y1(i)
        y28(i)=y7(i)-y2(i)
        y35(i)=y5(i)-y3(i)
        y46(i)=y6(i)-y3(i)
        z17(i)=z7(i)-z1(i)
        z28(i)=z7(i)-z2(i)
        z35(i)=z5(i)-z3(i)
        z46(i)=z6(i)-z3(i)
      end do
! jacobian matrix
      do i=1,nel
!  -------ri.xi-----------
        jac3(i)=x17(i)+x28(i)-x35(i)-x46(i)
        jac1(i)=y17(i)+y28(i)-y35(i)-y46(i)
        jac2(i)=z17(i)+z28(i)-z35(i)-z46(i)
!-------
        x_17_46=x17(i)+x46(i)
        x_28_35=x28(i)+x35(i)
        y_17_46=y17(i)+y46(i)
        y_28_35=y28(i)+y35(i)
        z_17_46=z17(i)+z46(i)
        z_28_35=z28(i)+z35(i)
!  -------si.xi-----------
        jac6(i)=x_17_46+x_28_35
        jac4(i)=y_17_46+y_28_35
        jac5(i)=z_17_46+z_28_35
!  -------ti.xi-----------
        jac9(i)=x_17_46-x_28_35
        jac7(i)=y_17_46-y_28_35
        jac8(i)=z_17_46-z_28_35
        jac_59_68(i)=jac5(i)*jac9(i)-jac6(i)*jac8(i)
        jac_67_49(i)=jac6(i)*jac7(i)-jac4(i)*jac9(i)
        jac_19_37(i)=jac1(i)*jac9(i)-jac3(i)*jac7(i)
        jac_48_57(i)=jac4(i)*jac8(i)-jac5(i)*jac7(i)
        det(i)=one_over_64*(jac1(i)*jac_59_68(i)+jac2(i)*jac_67_49(i)+jac3(i)*jac_48_57(i))
      enddo
! jacobian matrix inverse 
      do i=1,nel
        dett=one_over_64/det(i)
        jaci1=dett*jac_59_68(i)
        jaci4=dett*jac_67_49(i)
        jaci7=dett*jac_48_57(i)
        jaci2=dett*(-jac2(i)*jac9(i)+jac3(i)*jac8(i))
        jaci5=dett*jac_19_37(i)
        jaci8=dett*(-jac1(i)*jac8(i)+jac2(i)*jac7(i))
        jaci3=dett*( jac2(i)*jac6(i)-jac3(i)*jac5(i))
        jaci6=dett*(-jac1(i)*jac6(i)+jac3(i)*jac4(i))
        jaci9=dett*( jac1(i)*jac5(i)-jac2(i)*jac4(i))
!
        jaci12=jaci1-jaci2
        jaci45=jaci4-jaci5
        jaci78=jaci7-jaci8
!
        py3(i)= jaci12+jaci3
        pz3(i)= jaci45+jaci6
        px3(i)= jaci78+jaci9
        py4(i)= jaci12-jaci3
        pz4(i)= jaci45-jaci6
        px4(i)= jaci78-jaci9

        jaci12=jaci1+jaci2
        jaci45=jaci4+jaci5
        jaci78=jaci7+jaci8

        py1(i)=-jaci12-jaci3
        pz1(i)=-jaci45-jaci6
        px1(i)=-jaci78-jaci9
        py2(i)=-jaci12+jaci3
        pz2(i)=-jaci45+jaci6
        px2(i)=-jaci78+jaci9
      enddo
!   h1
! 1 1 -1 -1 -1 -1 1 1
       do i=1,nel
         h1x=x1(i)+x2(i)-x3(i)-x3(i)-x5(i)-x6(i)+x7(i)+x7(i)
         h1y=y1(i)+y2(i)-y3(i)-y3(i)-y5(i)-y6(i)+y7(i)+y7(i)
         h1z=z1(i)+z2(i)-z3(i)-z3(i)-z5(i)-z6(i)+z7(i)+z7(i)
         hx=one_over_8*h1x
         hy=one_over_8*h1y
         hz=one_over_8*h1z
         px1h1(i)=px1(i)*hx+ py1(i)*hy+pz1(i)*hz
         px2h1(i)=px2(i)*hx+ py2(i)*hy+pz2(i)*hz
         px3h1(i)=px3(i)*hx+ py3(i)*hy+pz3(i)*hz
         px4h1(i)=px4(i)*hx+ py4(i)*hy+pz4(i)*hz
       end do
!   h2
! 1 -1 -1 1 -1 1 1 -1
       do i=1,nel
         h2x=x1(i)-x2(i)-x5(i)+x6(i)
         h2y=y1(i)-y2(i)-y5(i)+y6(i)
         h2z=z1(i)-z2(i)-z5(i)+z6(i)
         hx=one_over_8*h2x
         hy=one_over_8*h2y
         hz=one_over_8*h2z   
         px1h2(i)=px1(i)*hx+ py1(i)*hy+pz1(i)*hz
         px2h2(i)=px2(i)*hx+ py2(i)*hy+pz2(i)*hz
         px3h2(i)=px3(i)*hx+ py3(i)*hy+pz3(i)*hz
         px4h2(i)=px4(i)*hx+ py4(i)*hy+pz4(i)*hz
       end do
!   h3
! 1 -1 1 -1 1 -1 1 -1 mode 3
        do i=1,nel
          h3x=x1(i)-x2(i)+x5(i)-x6(i)
          h3y=y1(i)-y2(i)+y5(i)-y6(i)
          h3z=z1(i)-z2(i)+z5(i)-z6(i)
          hx=one_over_8*h3x
          hy=one_over_8*h3y
          hz=one_over_8*h3z   
          px1h3(i)=px1(i)*hx+ py1(i)*hy+pz1(i)*hz
          px2h3(i)=px2(i)*hx+ py2(i)*hy+pz2(i)*hz
          px3h3(i)=px3(i)*hx+ py3(i)*hy+pz3(i)*hz
          px4h3(i)=px4(i)*hx+ py4(i)*hy+pz4(i)*hz
        end do
!------------------------------------------------
        f14(1:nel) =zero
        f24(1:nel) =zero
        f34(1:nel) =zero
        f18(1:nel) =zero
        f28(1:nel) =zero
        f38(1:nel) =zero
        call shour_ctl(                                                         &
                      pm,      rho,     off,     vx1,                           &
                      vx2,     vx3,     vx3,     vx5,                           &
                      vx6,     vx7,     vx7,     vy1,                           &
                      vy2,     vy3,     vy3,     vy5,                           &
                      vy6,     vy7,     vy7,     vz1,                           &
                      vz2,     vz3,     vz3,     vz5,                           &
                      vz6,     vz7,     vz7,     f11,                           &
                      f21,     f31,     f12,     f22,                           &
                      f32,     f13,     f23,     f33,                           &
                      f14,     f24,     f34,     f15,                           &
                      f25,     f35,     f16,     f26,                           &
                      f36,     f17,     f27,     f37,                           &
                      f18,     f28,     f38,     px1h1,                         &
                      px1h2,   px1h3,   px2h1,   px2h2,                         &
                      px2h3,   px3h1,   px3h2,   px3h3,                         &
                      px4h1,   px4h2,   px4h3,    vol,                          &
                      fhour,   mtn,      dt1 ,    mxt,                          &
                        ssp,  eint,    npropm, nummat,                          &
                       vol0,   dn ,     sti  ,   nel )
        f13(1:nel) =f13(1:nel)+f14(1:nel)
        f17(1:nel) =f17(1:nel)+f18(1:nel)
        f23(1:nel) =f23(1:nel)+f24(1:nel)
        f27(1:nel) =f27(1:nel)+f28(1:nel)
        f33(1:nel) =f33(1:nel)+f34(1:nel)
        f37(1:nel) =f37(1:nel)+f38(1:nel)
!
       end subroutine s6chour_ctl
!-------------------
      end module s6chour_ctl_mod
