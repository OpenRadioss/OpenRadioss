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
!----s---1----+----2----+----3----+----4----+----5----+----6----+----7--
!||====================================================================
!||    redef3_mod       ../engine/source/elements/spring/redef3.F90
!||--- called by ------------------------------------------------------
!||    fixtemp          ../engine/source/constraints/thermic/fixtemp.F
!||    r1def3           ../engine/source/elements/spring/r1def3.F
!||    r23l108def3      ../engine/source/elements/spring/r23l108def3.F
!||    r23l113def3      ../engine/source/elements/spring/r23l113def3.F
!||    r23l114def3      ../engine/source/elements/spring/r23l114def3.F
!||    r2def3           ../engine/source/elements/spring/r2def3.F
!||    r3def3           ../engine/source/elements/spring/r3def3.F
!||    r4def3           ../engine/source/elements/spring/r4def3.F
!||    r6def3           ../engine/source/elements/spring/r6def3.F
!||    redef_seatbelt   ../engine/source/tools/seatbelts/redef_seatbelt.F90
!||====================================================================
      module redef3_mod
      implicit none
      contains
!! for performance reasons, this function must inlined, because it is called in a loop
!!      \brief return .true. if the function id corresponds to a python function
        pure integer function get_python_funct_id(nfunct, funct_id, npc,snpc) result(id)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     module
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: nfunct
          integer, intent(in) :: funct_id !< the id of the function
          integer, intent(in) :: snpc
          integer, intent(in) :: npc(snpc)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i
! ----------------------------------------------------------------------------------------------------------------------
!                                                      body
! ----------------------------------------------------------------------------------------------------------------------
          i = 0
          id = 0
          if (funct_id> 0 .and. 2*nfunct+funct_id+1 <= snpc) i = npc(2*nfunct+funct_id+1)
          if(i < 0) id = -i
        end function get_python_funct_id

!||====================================================================
!||    redef3                 ../engine/source/elements/spring/redef3.F90
!||--- called by ------------------------------------------------------
!||    r1def3                 ../engine/source/elements/spring/r1def3.F
!||    r23l108def3            ../engine/source/elements/spring/r23l108def3.F
!||    r23l113def3            ../engine/source/elements/spring/r23l113def3.F
!||    r23l114def3            ../engine/source/elements/spring/r23l114def3.F
!||    r2def3                 ../engine/source/elements/spring/r2def3.F
!||    r3def3                 ../engine/source/elements/spring/r3def3.F
!||    r4def3                 ../engine/source/elements/spring/r4def3.F
!||    r6def3                 ../engine/source/elements/spring/r6def3.F
!||--- calls      -----------------------------------------------------
!||    python_solve           ../common_source/modules/python_mod.F90
!||    vinter2                ../engine/source/tools/curve/vinter.F
!||    vinter2dp              ../engine/source/tools/curve/vinter.F
!||    vinter_mixed           ../engine/source/tools/curve/vinter_mixed.F90
!||--- uses       -----------------------------------------------------
!||    constant_mod           ../common_source/modules/constant_mod.F
!||    mvsiz_mod              ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod          ../common_source/modules/precision_mod.F90
!||    python_funct_mod       ../common_source/modules/python_mod.F90
!||    vinter_mixed_mod       ../engine/source/tools/curve/vinter_mixed.F90
!||====================================================================
        subroutine redef3(python,&
        &fx,      xk,      dx,      fxep,&
        &dxold,   dpx,     tf,      npf,&
        &xc,      off,     e,       dpx2,&
        &anim,    iani,    pos,&
        &xl0,     dmn,     dmx,     dvx,&
        &ff,      lscale,  ee,      gf3,&
        &ifunc3,  yield,   aldp,    ak,&
        &b,       d,       iecrou,  ifunc,&
        &ifv,     ifunc2,  epla,    xx_old,&
        &nel, nft, stf,    sanin,   dt1,&
        &iresp,   impl_s,  idyna,   snpc,&
        &max_slope,fx_max, yieldc,  xx_oldc,&
        &fx0,     ifunc4,  pos6)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
          use python_funct_mod
          use vinter_mixed_mod
          use constant_mod
          use precision_mod, only : WP
          use mvsiz_mod, only : mvsiz
! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_), intent(inout) :: python
          integer, intent(in) :: stf    !< size of tf
          integer, intent(in) :: sanin  !< size of anim
          integer, intent(in) :: iresp  !< single precision flag
          integer, intent(in) :: impl_s !< implicit solver flag
          integer, intent(in) :: idyna  !< dynamic condensaton flag
          integer, intent(in) :: snpc   !< size of npf
          integer, intent(in) :: nel
          integer, intent(in) :: nft
          integer, intent(in) ::  iani
          integer, dimension(snpc),intent(in) :: npf
          integer, dimension(mvsiz),intent(in) :: iecrou  !< spring hardening flag
!                0: elastic spring.
!                1: nonlinear elastic plastic spring with isotropic hardening.
!                2: nonlinear elastic plastic spring with uncoupled hardening.
!                4: nonlinear elastic plastic spring with kinematic hardening.
!                5: nonlinear elastic plastic spring with nonlinear unloading.
!                6: nonlinear elastic plastic spring with isotropic hardening and nonlinear unloading.
!                7: nonlinear elastic spring with elastic hystersis.
!                8: nonlinear elastic spring with total length function
          integer, dimension(mvsiz),intent(inout) ::  ifunc
          integer, dimension(mvsiz),intent(inout) ::  ifunc2
          integer, dimension(mvsiz),intent(in) ::  ifv
          integer, dimension(mvsiz),intent(in), optional ::  ifunc4

          ! real(kind=WP) arrays
          real(kind=WP),intent(in) ::  dt1                                        !< time step
          real(kind=WP), dimension(stf),intent(inout) :: tf                       !< functions
          real(kind=WP), dimension(sanin),intent(inout) :: anim                   !< animation
          real(kind=WP), dimension(5,nel),intent(inout) :: pos                    !< gbuf%posx
          real(kind=WP), dimension(nel),intent(inout) :: fx                       !< gbuf%for
          real(kind=WP), dimension(nel),intent(inout) :: dx                       !< gbuf%totdepl
          real(kind=WP), dimension(nel),intent(inout) :: fxep                     !< gbuf%forep
          real(kind=WP), dimension(nel),intent(inout) :: dpx                      !< gbuf%dep_in_tens
          real(kind=WP), dimension(nel),intent(inout) :: e                        !< gbuf%eint (internal energy)
          real(kind=WP), dimension(nel),intent(inout) :: dpx2                     !< gbuf%dep_in_comp
          real(kind=WP), dimension(nel),intent(inout) :: yield                    !< gbuf%yield(ii(1))
          real(kind=WP), dimension(nel),intent(inout) :: xx_old                   !< uvar(1,1:nel)
          real(kind=WP), dimension(mvsiz),intent(inout) :: xk
          real(kind=WP), dimension(mvsiz),intent(inout) :: dxold                  !< previous dx
          real(kind=WP), dimension(mvsiz),intent(inout) :: xc                     !<
          real(kind=WP), dimension(mvsiz),intent(inout) :: off                    !< element activated/deactivated (local buffer)
          real(kind=WP), dimension(mvsiz),intent(inout) :: xl0                    !< length
          real(kind=WP), dimension(mvsiz),intent(inout) :: dmn                    !< geo(15,mgn(i))
          real(kind=WP), dimension(mvsiz),intent(inout) :: dmx                    !< geo(16,mgn(i))
          real(kind=WP), dimension(mvsiz),intent(inout) :: dvx                    !< working array
          real(kind=WP), dimension(mvsiz),intent(inout) :: ff                     !< geo(18 ,mgn(i))
          real(kind=WP), dimension(mvsiz),intent(inout) :: lscale                 !< geo(39 ,mgn(i))
          real(kind=WP), dimension(mvsiz),intent(inout) :: ee                     !< geo(40 ,mgn(i))
          real(kind=WP), dimension(mvsiz),intent(inout) :: ak                     !<
          real(kind=WP), dimension(mvsiz),intent(inout) :: b                      !<
          real(kind=WP), dimension(mvsiz),intent(inout) :: d                      !<
          real(kind=WP), dimension(mvsiz),intent(inout) :: epla                   !<
          real(kind=WP), dimension(mvsiz),intent(inout), optional :: max_slope    !<
          real(kind=WP), dimension(mvsiz),intent(inout), optional :: fx_max       !<
          real(kind=WP), dimension(nel)  ,intent(inout), optional :: yieldc       !<
          real(kind=WP), dimension(nel)  ,intent(inout), optional :: xx_oldc      !<
          real(kind=WP), intent(in), optional ::  fx0
          real(kind=WP), dimension(nel),intent(inout), optional :: pos6           !< gbuf%posx(6)
          double precision, dimension(mvsiz),intent(inout) :: aldp          !<
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: jpos(mvsiz)
          integer :: jlen(mvsiz)
          integer :: jad(mvsiz)
          integer :: jpos2(mvsiz)
          integer :: jlen2(mvsiz)
          integer :: jpos3(mvsiz)
          integer :: jlen3(mvsiz)
          integer :: jad2(mvsiz)
          integer :: jfunc
          integer :: jfunc2
          integer :: jdmp
          integer :: jecrou(-1:12)
          integer :: j2dmp
          integer :: k1
          integer :: np2
          integer :: i
          integer :: ii
          integer :: interp
          integer :: k
!     integer ic1(mvsiz)
          integer :: ic2(mvsiz)
          integer :: fund
          integer :: ifunc3(mvsiz)
          integer :: j2pos(mvsiz)
          integer :: j2len(mvsiz)
          integer :: j2ad(mvsiz)
          integer :: j2func
          integer :: j3pos(mvsiz)
          integer :: j3len(mvsiz)
          integer :: j3ad(mvsiz)
          integer :: j3func
          integer :: j2k
!     real or real*8
          real(kind=WP) :: b1
          real(kind=WP) :: ddx(mvsiz)
          real(kind=WP) :: fold(mvsiz)
          real(kind=WP) :: gx(mvsiz)
          real(kind=WP) :: dxela(mvsiz)
          real(kind=WP) :: dydx(mvsiz)
          real(kind=WP) :: xx(mvsiz)
          real(kind=WP) :: xx2(mvsiz)
          real(kind=WP) :: xx3(mvsiz)
          real(kind=WP) :: yy(mvsiz)
          real(kind=WP) :: yy2(mvsiz)
          real(kind=WP) :: yy3(mvsiz)
          real(kind=WP) :: dydx2(mvsiz)
          real(kind=WP) :: dydx3(mvsiz)
          real(kind=WP) :: dydxv(mvsiz)
          real(kind=WP) :: dperm(mvsiz)
          real(kind=WP) :: dvv
          real(kind=WP) :: dfac
          real(kind=WP) :: dt11
          real(kind=WP) :: damp
          real(kind=WP) :: damm
          real(kind=WP) :: fmax(mvsiz)
          real(kind=WP) :: dvxs(mvsiz)
          real(kind=WP) :: gf3(mvsiz)
          real(kind=WP) :: dydxv2(mvsiz)
          real(kind=WP) :: fmin(mvsiz)
          real(kind=WP) :: gx2(mvsiz)
          real(kind=WP) :: xi1
          real(kind=WP) :: xi2
          real(kind=WP) :: yi1
          real(kind=WP) :: yi2
          real(kind=WP) :: x1
          real(kind=WP) :: x2
          real(kind=WP) :: y1
          real(kind=WP) :: y2
          real(kind=WP) :: an3y0(mvsiz)
          real(kind=WP) :: fxb
          real(kind=WP) :: x1s,x2s,xxb
          real(kind=WP) :: xk_tansav(mvsiz)
          real(kind=WP) :: xn3fy0(mvsiz)
          real(kind=WP) :: kx2(mvsiz)
          real(kind=WP) :: dkdx2(mvsiz)

          integer :: nfunct !< total number of functions
          integer :: pyid1, pyid2 !< python function id
          logical :: any_python_func !< any python function?
! ----------------------------------------------------------------------------------------------------------------------
          nfunct = python%funct_offset + python%nb_functs -python%nb_sensors! offset = nb of non-python functions
          any_python_func = .false.
          dt11 = dt1
          x1s = zero
          x2s = zero
          fxb = zero
          if(dt11==zero)dt11 = ep30
          do i=1,nel
            dx(i)=dx(i)/xl0(i)
            dxold(i)=dxold(i)/xl0(i)
            dpx(i)=dpx(i)/xl0(i)
            dpx2(i)=dpx2(i)/xl0(i)
            e(i)=e(i)/xl0(i)
          end do
!
          do i=1,nel
            fold(i)=fx(i)
            ddx(i)= (dx(i)-dxold(i))
            dvx(i)= ddx(i)/ dt11
            dvxs(i)= dvx(i)*ff(i)
          end do
!
!
          if(iani/=0)then
            do i=1,nel
              ii=i+nft
              damp=dx(i)/max(dmx(i),em15) !
              damm=dx(i)/min(dmn(i),-em15)
              anim(ii)=max(anim(ii),damp,damm)
              anim(ii)=min(anim(ii),one)
            end do
          end if
!-------------------------------------
!        vector interpolation (address)
!-------------------------------------
          jecrou(-1)  = 0
          jecrou(0)  = 0
          jecrou(1)  = 0
          jecrou(2)  = 0
          jecrou(3)  = 0
          jecrou(4)  = 0
          jecrou(5)  = 0
          jecrou(6)  = 0
          jecrou(7)  = 0
          jecrou(8)  = 0
          jecrou(9)  = 0
          jecrou(10) = 0
          jecrou(11) = 0
          jecrou(12) = 0
          interp = 0
          jdmp = 0
          j2dmp = 0
          j2k = 0
!
          do i=1,nel
            if(iecrou(i) == 12)then
              jecrou(12) = jecrou(12) + 1
            else if(ifunc(i)==0)then  ! ifunc =igeo(101)-fct_id1
              jecrou(-1) = jecrou(-1) + 1
!  vectorisation
            else if(iecrou(i)==0)then
              jecrou(0) = jecrou(0) + 1
              interp = 1
            else if(iecrou(i)==1)then
              jecrou(1) = jecrou(1) + 1
              interp = 1
            else if(iecrou(i)==2)then
              jecrou(2) = jecrou(2) + 1
              interp = 1
            else if(iecrou(i)==3)then
              jecrou(3) = jecrou(3) + 1
              interp = 1
            else if(iecrou(i)==4)then
              jecrou(4) = jecrou(4) + 1
              interp = 1
            else if(iecrou(i)==5)then
              jecrou(5) = jecrou(5) + 1
              interp = 1
            else if(iecrou(i)==6)then
              jecrou(6) = jecrou(6) + 1
              interp = 1
            else if(iecrou(i)==7)then
              jecrou(7) = jecrou(7) + 1
              interp = 1
            else if(iecrou(i) == 8)then
              jecrou(8) = jecrou(8) + 1
              interp = 1
            else if(iecrou(i) == 9)then
              jecrou(9) = jecrou(9) + 1
              interp = 1
            end if
            if(ifv(i)/=0) jdmp = jdmp + 1
            if(ifunc3(i)/=0) j2dmp = j2dmp + 1
            if(present(ifunc4)) then
              if(ifunc4(i)/=0) j2k = j2k + 1
            end if
          end do
!
          if(interp>0)then
            do i=1,nel
              jpos(i)  = nint(pos(1,i))
              jpos2(i) = nint(pos(2,i))
              jpos3(i) = nint(pos(3,i))
              pyid1 = get_python_funct_id(nfunct, ifunc(i),npf,snpc)
              pyid2 = get_python_funct_id(nfunct, ifunc2(i),npf,snpc)
              if(pyid1 > 0) then
                jad(i) = -pyid1
                jlen(i) = -pyid1
                jlen3(i) = -pyid1
                ifunc(i) = pyid1
                any_python_func = .true.
              else
                jfunc =max(1,ifunc(i))
                jad(i)   = npf(jfunc) / 2  + 1
                jlen(i)  = npf(jfunc+1) / 2  - jad(i)  - jpos(i)
                jlen3(i) = npf(jfunc+1) / 2  - jad(i)  - jpos3(i)
              end if

              if(pyid2 > 0) then
                jad2(i) = -pyid2
                jlen2(i) = -pyid2
                ifunc2(i) =  pyid2
                any_python_func = .true.
              else
                jfunc2=max(1,ifunc2(i))
                jad2(i)  = npf(jfunc2) / 2 + 1
                jlen2(i) = npf(jfunc2+1) / 2 - jad2(i) - jpos2(i)
              end if
              xx(i) =zero
              xx2(i)=zero
              xx3(i)=zero
            end do
          else
            do i=1,nel
              xx(i) = zero
              xx2(i)= zero
              xx3(i)= zero
            end do
          end if
!-------------------------------------
!        non linear elastic, f=f(total length)
!-------------------------------------
          if (jecrou(8) > 0) then
            do i=1,nel
              if (iecrou(i) == 8) then
                xx(i) = aldp(i)
              else
                xx(i) = dx(i)
              end if
            end do
          end if
!-------------------------------------
!        non linear elastic
!        nl elasto plastic (tension compression decoupled)
!-------------------------------------
          if(jecrou(0)+jecrou(2)>0)then
            do i=1,nel
              if(ifunc(i) /= 0.and.(iecrou(i)==0.or.iecrou(i)==2))then
                xx(i)=dx(i)
              end if
            end do
          end if
!-------------------------------------
!        elasto plastic (isotropic)
!        elasto plastic (tension compression coupled 6 dof couples)
!-------------------------------------
          if(jecrou(1)+jecrou(3)>0)then
            do i=1,nel
              if(ifunc(i) /= 0.and.(iecrou(i)==1.or.iecrou(i)==3))then
                fx(i)=fxep(i)+xk(i)*ddx(i)
                if(fx(i)>=zero)then
                  xx(i)=dpx(i)+fx(i)/xk(i)
                else
                  xx(i)=-dpx(i)+fx(i)/xk(i)
                end if
              end if
            end do
          end if
!-------------------------------------
!        elasto plastic (kinematic hardening)
!-------------------------------------
          if(jecrou(4)>0)then
            do i=1,nel
              if(ifunc(i) /= 0.and.iecrou(i)==4)then
                interp = max(2,interp)
                xx(i) =dx(i)
                xx2(i)=dx(i)
              end if
            end do
          end if
!-------------------------------------
!        elasto plastic (tension compression decoupled)
!         (d/r)nonlinear reloading
!         dpx = maximum displacement (and not plastic)
!-------------------------------------
          if(jecrou(5)>0)then
            do i=1,nel
              if(ifunc(i) /= 0.and.iecrou(i)==5)then
                xx(i)=dx(i)
                if(dx(i)>zero)then
                  interp = max(3,interp)
                  xx2(i)=dpx(i)
                  xx3(i)=dpx(i)
                else if(dx(i)<zero)then
                  interp = max(3,interp)
                  xx2(i)=dpx2(i)
                  xx3(i)=dpx2(i)
                else
                  interp = max(1,interp)
                end if
              end if
            end do
          end if
!-------------------------------------
!        elasto plastic (isotropic hardening)
!-------------------------------------

          if(jecrou(6)>0)then
            if(any_python_func) then
              do i=1,nel
                if(ifunc(i) /= 0.and.iecrou(i)== 6 .and. jad2(i) < 0 )then
                  fund = -jad2(i)           ! curve id
                  ! obtain the derivative directly at fxep(i)
                  call python_deriv_funct1d(python, fund, fxep(i), an3y0(i))
                  ! calculate fx(i) and xx(i) based on the derivative
                  fx(i) = fxep(i) + an3y0(i) * ddx(i)
                  xx(i) = sign(abs(xx_old(i)), fx(i))
                  xx(i) = xx(i) + ddx(i)
                end if
              end do
            end if
            do i=1,nel
              !if not a python function
              if(ifunc(i) /= 0.and.iecrou(i)== 6 .and. jad2(i) >= 0 )then
                fund = ifunc2(i)
                np2  = (npf(fund+1)-npf(fund))/2
                an3y0(i)= zero
                do  k=2,np2
                  k1=2*(k-2)
                  x1=tf(npf(fund)+k1)
                  x2=tf(npf(fund)+k1+2)
                  y1=tf(npf(fund)+k1+1)
                  y2=tf(npf(fund)+k1+3)
                  if((fxep(i)< y2.and.fxep(i)>=y1))then
                    an3y0(i)=(y2-y1)/ (x2-x1)
                    exit
                  end if
                end do
                if (an3y0(i)== zero)then
                  x1=tf(npf(fund)+(np2-2)*2)
                  x2=tf(npf(fund)+(np2-2)*2+2)
                  y1=tf(npf(fund)+(np2-2)*2+1)
                  y2=tf(npf(fund)+(np2-2)*2+3)
!
                  xi1=tf(npf(fund))
                  xi2=tf(npf(fund)+2)
                  yi1=tf(npf(fund)+1)
                  yi2=tf(npf(fund)+3)
                  if(fxep(i)>y2)an3y0(i)=(y2-y1)/ (x2-x1)
                  if(fxep(i)<yi1)an3y0(i)=(yi2-yi1)/ (xi2-xi1)
                end if
                fx(i)=fxep(i)+an3y0(i)*ddx(i)
                xx(i)=sign(abs(xx_old(i)),fx(i))
                xx(i)=xx(i)+ddx(i)
              end if
            end do
            interp = 0
          end if
!-------------------------------------
!      elasto plastic two curves for load and unload
!-------------------------------------
          if(jecrou(7)>0)then
            interp = max(2,interp)
            do i=1,nel
              if(ifunc(i) /= 0.and.iecrou(i)==7)then
                xx(i) =dx(i)
                xx2(i)=dx(i)
              end if
            end do
          end if
!-------------------------------------
!        elasto plastic - isotropic uncoupled hardening in traction and compression
!-------------------------------------
          if(jecrou(9)>0)then
            if(any_python_func) then
              do i=1,nel
                if (ifunc2(i) /= 0 .and. iecrou(i) == 9 .and. jad2(i) < 0) then
                  fund = -jad2(i) ! curve identifier
                  an3y0(i) = zero
                  xn3fy0(i) = zero
                  dxela(i) = dx(i) - dpx(i)
                  call python_deriv_funct1d(python, fund, fxep(i), an3y0(i))
                  call python_solve(python, fund, xn3fy0(i), fxep(i))
                  xxb = xn3fy0(i) + ddx(i)
                  xk_tansav(i)=an3y0(i)
                  if (fxep(i)==yield(i)) dpx(i) = xx_old(i)- abs(xn3fy0(i))
                  if (fxep(i)==-yieldc(i)) dpx2(i) = xx_oldc(i)- abs(xn3fy0(i))
                  fx(i)=fxep(i)+an3y0(i)*ddx(i)
                  if ((fxep(i) < yield(i)).and.(fx(i) > yield(i))) then
!----               crossing of the traction yield line
                    xx(i)=dpx(i) + xn3fy0(i) + ddx(i)
                  else if ((-fxep(i)< yieldc(i)).and.(-fx(i) > yieldc(i))) then
!----               crossing of the compression yield line
                    xx(i)=-dpx2(i) + xn3fy0(i) + ddx(i)
                  else
                    if (fx(i) >= zero) then
                      xx(i)= xx_old(i) + ddx(i)
                    else
                      xx(i)= -xx_oldc(i) + ddx(i)
                    end if
                  end if
                end if
              end do
            end if
            do i=1,nel
              if(ifunc2(i)/= 0.and.iecrou(i)== 9 .and. jad2(i) > 0)then
                fund = ifunc2(i)     ! courbe n3 de unload
                np2  = (npf(fund+1)-npf(fund))/2
                an3y0(i)= zero
                dxela(i)=dx(i)-dpx(i)
                xxb = 0
!--- tension - load curve is used
                do  k=2,np2
                  k1=2*(k-2)
                  x1=tf(npf(fund)+k1)
                  x2=tf(npf(fund)+k1+2)
                  y1=tf(npf(fund)+k1+1)
                  y2=tf(npf(fund)+k1+3)
                  if((fxep(i)< y2.and.fxep(i)>=y1))then
                    x1s = x1
                    x2s = x2
                    an3y0(i)=(y2-y1)/ (x2-x1)
                    xn3fy0(i)=(fxep(i)-y1)/an3y0(i) + x1   !abs de n3
                    exit
                  end if
                end do
                !---        extrapolation (outside of input curve points)
                if (an3y0(i)== zero)then !
                  x1=tf(npf(fund)+(np2-2)*2)
                  x2=tf(npf(fund)+(np2-2)*2+2)
                  y1=tf(npf(fund)+(np2-2)*2+1)
                  y2=tf(npf(fund)+(np2-2)*2+3)
                  xi1=tf(npf(fund))
                  xi2=tf(npf(fund)+2)
                  yi1=tf(npf(fund)+1)
                  yi2=tf(npf(fund)+3)
                  if(fxep(i)>y2) then
                    an3y0(i)=(y2-y1)/ (x2-x1)
                    xn3fy0(i)=(fxep(i)-y1)/an3y0(i) + x1
                    x1s = x2
                    x2s = ep20
                  else if(fxep(i)<yi1) then
                    an3y0(i)=(yi2-yi1)/ (xi2-xi1)
                    xn3fy0(i)=(fxep(i)-yi1)/an3y0(i) + xi1
                    x1s = -ep20
                    x2s = xi1
                  end if
                  xk_tansav(i)=an3y0(i)
                end if
                xxb =xn3fy0(i)+ddx(i)
                xk_tansav(i)=an3y0(i)
                if (fxep(i)==yield(i)) dpx(i) = xx_old(i)- abs(xn3fy0(i))
                if (fxep(i)==-yieldc(i)) dpx2(i) = xx_oldc(i)- abs(xn3fy0(i))
                !---         next point is in another part of the curve
                if (xxb< x1s.or.xxb>x2s) then
                  xk_tansav(i)=zero
                  do  k=2,np2
                    k1=2*(k-2)
                    x1=tf(npf(fund)+k1)
                    x2=tf(npf(fund)+k1+2)
                    y1=tf(npf(fund)+k1+1)
                    y2=tf(npf(fund)+k1+3)
                    if((xxb < x2.and.xxb >=x1))then
                      xk_tansav(i)=(y2-y1)/ (x2-x1)
                      fxb = y1 + ((y2-y1)/(x2-x1))*(xxb-x1)
                      an3y0(i)= (fxep(i)-fxb)/ (xn3fy0(i)-xxb)
                      exit
                    end if
                  end do
                  if (xk_tansav(i)== zero)then ! extrapolation (outside of input curve points)
                    x1=tf(npf(fund)+(np2-2)*2)
                    x2=tf(npf(fund)+(np2-2)*2+2)
                    y1=tf(npf(fund)+(np2-2)*2+1)
                    y2=tf(npf(fund)+(np2-2)*2+3)
                    xi1=tf(npf(fund))
                    xi2=tf(npf(fund)+2)
                    yi1=tf(npf(fund)+1)
                    yi2=tf(npf(fund)+3)
                    if(xxb>x2) then
                      xk_tansav(i)=(y2-y1)/ (x2-x1)
                      fxb = y2 + xk_tansav(i)*(xxb-x2)
                    else if(xxb<xi1) then
                      xk_tansav(i)=(yi2-yi1)/ (xi2-xi1)
                      fxb = yi1 + xk_tansav(i)*(xxb-xi1)
                    end if
                    an3y0(i)= (fxep(i)-fxb)/ (xn3fy0(i)-xxb)
                  end if
                end if
                fx(i)=fxep(i)+an3y0(i)*ddx(i)
                if ((fxep(i) < yield(i)).and.(fx(i) > yield(i))) then
!----             crossing of the traction yield line
                  xx(i)=dpx(i) + xn3fy0(i) + ddx(i)
                else if ((-fxep(i)< yieldc(i)).and.(-fx(i) > yieldc(i))) then
!----             crossing of the compression yield line
                  xx(i)=-dpx2(i) + xn3fy0(i) + ddx(i)
                else
                  if (fx(i) >= fx0) then
                    xx(i)= xx_old(i) + ddx(i)
                  else
                    xx(i)= -xx_oldc(i) + ddx(i)
                  end if
                end if
              end if
            end do
            interp = 0
          end if
!-------------------------------------
!        elasto perfeclty plastic (isotropic hardening) - used only for seatbelts
!-------------------------------------
          if(jecrou(12)>0)then
            do i=1,nel
              if(iecrou(i)== 12) then
                an3y0(i)= zero
                if (abs(fxep(i)) > fx_max(i)) then
                  an3y0(i)= zero
                else
                  an3y0(i)= xk(i)
                end if
!
                fx(i)=fxep(i)+an3y0(i)*ddx(i)
                xx(i)=sign(abs(xx_old(i)),fx(i))
                xx(i)=xx(i)+ddx(i)
              end if
            end do
          end if
!-------------------------------------
!     vector interpolation
!-------------------------------------
          do i=1,nel
            xx(i)  = xx(i) *lscale(i)
            xx2(i) = xx2(i)*lscale(i)
            xx3(i) = xx3(i)*lscale(i)
          end do
!----s---1----+----2----+----3----+----4----+----5----+----6----+----7--
          if(any_python_func) then
            if(interp>=1) call vinter_mixed(python, tf,jad,jpos,jlen,nel,xx,dydx,yy)
            if(interp>=2) call vinter_mixed(python, tf,jad2,jpos2,jlen2,nel,xx2,dydx2,yy2)
            if(interp>=3) call vinter_mixed(python, tf,jad ,jpos3,jlen3,nel,xx3,dydx3,yy3)
          else
            if (iresp==1) then

              if(interp>=1) call vinter2dp(tf,jad ,jpos ,jlen ,nel,xx ,dydx ,yy)
              if(interp>=2) call vinter2dp(tf,jad2,jpos2,jlen2,nel,xx2,dydx2,yy2)
              if(interp>=3) call vinter2dp(tf,jad ,jpos3,jlen3,nel,xx3,dydx3,yy3)
            else
              if(interp>=1) call vinter2(tf,jad ,jpos ,jlen ,nel,xx ,dydx ,yy )
              if(interp>=2) call vinter2(tf,jad2,jpos2,jlen2,nel,xx2,dydx2,yy2)
              if(interp>=3) call vinter2(tf,jad ,jpos3,jlen3,nel,xx3,dydx3,yy3)
            end if
          end if

          if(interp>0)then
            do i=1,nel
              pos(1,i) = jpos(i)
              pos(2,i) = jpos2(i)
              pos(3,i) = jpos3(i)
            end do
          end if
!-------------------------------------
!        linear elastic
!-------------------------------------
          if(jecrou(-1)>0)then
            do i=1,nel
              if(ifunc(i)==0)then
                fx(i)=xk(i)*dx(i)
              end if
            end do
          end if
!-------------------------------------
!        non linear elastic, f = f(total length)
!-------------------------------------
          if (jecrou(8) > 0)then
            do i=1,nel
              if (iecrou(i) == 8) fx(i)=yy(i)
            end do
          end if
!-------------------------------------
!        linear elastic
!-------------------------------------
          if(jecrou(0)>0)then
            do i=1,nel
              if(ifunc(i) > 0 .and.iecrou(i)==0) fx(i)=yy(i)
            end do
          end if
!-------------------------------------
!        elasto plastic (isotrope)
!-------------------------------------
          if(jecrou(1)>0)then
            do i=1,nel
              if(ifunc(i) /= 0.and.iecrou(i)==1)then
                if(fx(i)>=zero.and.fx(i)>yy(i))then
                  dpx(i)=dpx(i)+(fx(i)-yy(i))/xk(i)
                  fx(i)=yy(i)
                else if(fx(i)<zero.and.fx(i)<yy(i))then
                  dpx(i)=dpx(i)+(yy(i)-fx(i))/xk(i)
                  fx(i)=yy(i)
                end if
                fxep(i)=fx(i)
              end if
            end do
          end if
!-------------------------------------
!        elasto plastic (decoupled compression tension)
!-------------------------------------
          if(jecrou(2)>0)then
            do i=1,nel
              if(ifunc(i) /= 0.and.iecrou(i)==2)then
                if(dx(i)>dpx(i))then
                  fx(i)  = xk(i) * (dx(i)-dpx(i))
                  fxep(i)= yy(i)
                  fx(i)  = min(fx(i),fxep(i))
                  dpx(i) = dx(i) - fx(i) / xk(i)
                else if(dx(i)<dpx2(i))then
                  fx(i)   = xk(i) * (dx(i)-dpx2(i))
                  fxep(i) = yy(i)
                  fx(i)   = max(fx(i),fxep(i))
                  dpx2(i) = dx(i) - fx(i) / xk(i)
                else
                  fx(i)   = zero
                end if
              end if
            end do
          end if
!-------------------------------------
!        elasto plastic (compression tension coupled with 6 dof)
!-------------------------------------
          if(jecrou(3)>0)then
            do i=1,nel
              if(ifunc(i) /= 0.and.iecrou(i)==3)then
                if(fx(i)>=zero.and.fx(i)>yy(i))then
                  epla(i)=epla(i)+abs(yy(i)*(fx(i)-yy(i))/xk(i))
                  fx(i)=yy(i)
                else if(fx(i)<zero.and.fx(i)<yy(i))then
                  epla(i)=epla(i)+abs(yy(i)*(yy(i)-fx(i))/xk(i))
                  fx(i)=yy(i)
                end if
                fxep(i)=fx(i)
              end if
            end do
          end if
!-------------------------------------
!        elasto plastic (ecouissage cinematique)
!-------------------------------------
          if(jecrou(4)>0)then
            do i=1,nel
              if(ifunc(i) /= 0.and.iecrou(i)==4)then
                fx(i) = fxep(i) + xk(i)*ddx(i)
                ic2(i)= 0
                if(fx(i)>yy(i))then
                  ic2(i)=1
                  fx(i) = yy(i)
                end if
                if(fx(i)<yy2(i))then
                  ic2(i)=2
                  fx(i) = yy2(i)
                end if
              end if
            end do
            do i=1,nel
              if(ifunc(i) /= 0.and.iecrou(i)==4)then
                fxep(i)=fx(i)
                dpx(i) = dx(i) - fx(i) / xk(i)
              end if
            end do
          end if
!-------------------------------------
!        elasto plastic (tension compression decoupled)
!         (d/r)nonlinear reloading
!         dpx = maximum displacement (and not plastic)
!-------------------------------------
          if(jecrou(5)>0)then
            do i=1,nel
              if(ifunc(i) /= 0.and.iecrou(i)==5)then
                if(dx(i)>dpx(i))then
                  fx(i)=yy(i)
                  dpx(i) = dx(i)
                else if(dx(i)>zero)then
                  dperm(i)=max(yy2(i),zero)
                  if(dx(i)>dperm(i).and.yy3(i)/=zero)then
                    fmax(i)=yy3(i)/lscale(i)
                    dperm(i)=min(dperm(i),dpx(i)- fmax(i) / xk(i))
!              y = a (x-x1)^b
                    b1 = (dpx(i)-dperm(i))*xk(i)/fmax(i)
                    fmin(i) = fmax(i) *&
                    &( (dx(i)-dperm(i))/(dpx(i)-dperm(i)) )**b1
                    fmax(i) = fmax(i)*(dx(i)-dperm(i))/(dpx(i)-dperm(i))
                    fx(i)=fxep(i)+xk(i)*ddx(i)
                    fx(i)=max(fx(i),fmin(i),zero)
                    fx(i)=min(fx(i),fmax(i),yy(i))
                  else
                    fx(i) = zero
                  end if
                else if(dx(i)<dpx2(i))then
                  fx(i)=yy(i)
                  dpx2(i) = dx(i)
                else if(dx(i)<zero)then
                  dperm(i)=yy2(i)
                  dperm(i)=min(dperm(i),zero)
                  if(dx(i)<dperm(i).and.yy3(i)/=zero)then
                    fmax(i)=yy3(i)/lscale(i)
                    dperm(i)=max(dperm(i),dpx2(i)- fmax(i) / xk(i))
!              y = a (x-x1)^b
                    b1 = (dpx2(i)-dperm(i))*xk(i)/fmax(i)
                    fmin(i) = fmax(i)*&
                    &( (-dx(i)+dperm(i))/(-dpx2(i)+dperm(i)) )**b1
                    fmax(i) = fmax(i)*(dx(i)-dperm(i))/(dpx2(i)-dperm(i))
                    fx(i)=fxep(i)+xk(i)*ddx(i)
                    fx(i)=min(fx(i),fmin(i),zero)
                    fx(i)=max(fx(i),fmax(i),yy(i))
                  else
                    fx(i) = zero
                  end if
                end if
                fxep(i)=fx(i)
              end if
            end do
          end if
!-------------------------------------
!        elasto plastic (isotropic hardening)
!-------------------------------------
          if(jecrou(6)>0)then
            if(any_python_func) then
              call vinter_mixed(python, tf,jad,jpos,jlen,nel,xx,dydx,yy)
            else
              call vinter2(tf,jad ,jpos ,jlen ,nel,xx ,dydx ,yy )
            end if
            do i=1,nel
              if(ifunc(i) /=  0.and.iecrou(i)== 6)then
                if(fx(i)>= zero.and.fx(i)>yield(i))then
                  pos(1,i) = jpos(i)
!-- compute plastic and elastic deformation (total)
                  dpx(i)=dpx(i)+(fx(i)-yy(i))/an3y0(i)
                  dxela(i)=dx(i)-dpx(i)
                  fx(i)=yy(i) !h1
                  yield(i)=fx(i)
!-- ecr variable for hardening/softening - always incremented with positive value
                  xx_old(i) = xx_old(i) + abs(ddx(i))
!---fx< o
                else if(fx(i)< zero.and.fx(i)< -yield(i))then
                  pos(1,i) = jpos(i)
!-- compute plastic and elastic deformation (total)
                  dpx(i)=dpx(i)+(yy(i)-fx(i))/an3y0(i)
                  dxela(i)=dx(i)-dpx(i)
                  fx(i)=yy(i)
                  yield(i)=-fx(i)
!-- ecr variable for hardening/softening - always incremented with positive value
                  xx_old(i) = xx_old(i) + abs(ddx(i))
                end if
                fxep(i)=fx(i)
              end if
            end do
          end if
!
!-------------------------------------
!        elasto plastic (!!)
!-------------------------------------
          if (jecrou(7) > 0) then
            do i=1,nel
              if (ifunc(i) /= 0 .and. iecrou(i)==7) then
                fx(i) = fxep(i) + xk(i)*ddx(i)
                if (dx(i)>= dxold(i) .and. dx(i)>=0) then
                  if (fx(i)>yy(i)) fx(i) = yy(i)
                else if (dx(i)< dxold(i) .and. dx(i)>= 0) then
                  if  (fx(i) < yy2(i)) fx(i) = yy2(i)
                else if (dx(i)>= dxold(i) .and. dx(i)<0) then
                  if (fx(i)> yy2(i)) fx(i) = yy2(i)
                else if (dx(i)< dxold(i) .and. dx(i)<0) then
                  if (fx(i)< yy(i))  fx(i) = yy(i)
                end if
                fxep(i) = fx(i)
                dpx(i)  = dx(i) - fx(i) / xk(i)
              end if
            end do
          end if
!-------------------------------------
!         elasto plastic - isotropic uncoupled hardening in traction and compression
!-------------------------------------
          if(jecrou(9)>0)then
            if(any_python_func) then
              call vinter_mixed(python, tf,jad,jpos,jlen,nel,xx,dydx,yy)
            else
              call vinter2(tf,jad ,jpos ,jlen ,nel,xx ,dydx ,yy )
            end if
            do i=1,nel
              if(ifunc(i)/= 0.and.iecrou(i)== 9)then
                if(fx(i)>=fx0.and.fx(i)>yield(i))then
                  pos(1,i) = jpos(i)
!--               compute plastic and elastic deformation (total)
                  fx(i)=yy(i)
                  yield(i)=fx(i)
!--               ecr variable for hardening/softening in traction - always incremented with positive value
                  xx_old(i) = xx_old(i) + abs(ddx(i))
                else if(fx(i)< fx0.and.fx(i)< -yieldc(i))then
                  pos(1,i) = jpos(i)
!--               compute plastic and elastic deformation (total)
                  fx(i)=yy(i)
                  yieldc(i)=-fx(i)
!--               ecr variable for hardening/softening in compression - always incremented with positive value
                  xx_oldc(i) = xx_oldc(i) + abs(ddx(i))
                end if
                fxep(i)=fx(i)
              end if
            end do
          end if
!-------------------------------------
!        elasto perfeclty plastic (isotropic hardening) - used only for seatbelts
!-------------------------------------
          if(jecrou(12)>0)then
            do i=1,nel
              if(iecrou(i)== 12)then
                if (abs(xk(i)*xx(i)) > fx_max(i)) then
                  yy(i) =sign(fx_max(i),xx(i))
                  dydx = 0
                else
                  yy(i) = xk(i)*xx(i)
                  dydx = xk(i)
                end if
                if(fx(i)>= zero.and.fx(i)>yield(i))then
!              pos(1,i) = jpos(i)
!-- compute plastic and elastic deformation (total)
                  dpx(i)=dpx(i)+(fx(i)-yy(i))/an3y0(i)
                  dxela(i)=dx(i)-dpx(i)
                  fx(i)=yy(i) !h1
                  yield(i)=fx(i)
!-- ecr variable for hardening/softening - always incremented with positive value
                  xx_old(i) = xx_old(i) + abs(ddx(i))
!---fx< o
                else if(fx(i)< zero.and.fx(i)< -yield(i))then
!              pos(1,i) = jpos(i)
!-- compute plastic and elastic deformation (total)
                  dpx(i)=dpx(i)+(yy(i)-fx(i))/an3y0(i)
                  dxela(i)=dx(i)-dpx(i)
                  fx(i)=yy(i)
                  yield(i)=-fx(i)
!-- ecr variable for hardening/softening - always incremented with positive value
                  xx_old(i) = xx_old(i) + abs(ddx(i))
                end if
                fxep(i)=fx(i)
              end if
            end do
          end if
!--------------------------------------------------------------------
!     non linear damping
!--------------------------------------------------------------------
          if(impl_s==0.or.idyna>0) then
!  enter a func4 (or 3)
            if(jdmp>0)then
              do i=1,nel
                jpos(i) = nint(pos(4,i))
                jfunc=max(ifv(i),1)
                pyid1 = get_python_funct_id(nfunct,jfunc,npf,snpc)
                if(pyid1>0)then
                  !python function
                  jad(i) = -pyid1
                  jlen(i)= -pyid1
                  any_python_func = .true.
                else
                  jad(i)  = npf(jfunc) / 2 + 1
                  jlen(i) = npf(jfunc+1) / 2 - jad(i) - jpos(i)
                end if
              end do

              if(any_python_func) then
                call vinter_mixed(python, tf,jad,jpos,jlen,nel,xx,dydxv,gx)
              else
                call vinter2(tf,jad,jpos,jlen,nel,dvxs,dydxv,gx)
              end if
!
              do i=1,nel
                pos(4,i) = jpos(i)
              end do
            end if
!---------------------------g * funct_id_4
            if(j2dmp>0)then
              do i=1,nel
                j2pos(i) = nint(pos(5,i))
                j2func=max(ifunc3(i),1)
                pyid2 = get_python_funct_id(nfunct,j2func,npf,snpc)
                if(pyid2>0)then
                  !python function
                  j2ad(i) = -pyid2
                  j2len(i)= -pyid2
                  any_python_func = .true.
                else
                  j2ad(i)  = npf(j2func) / 2 + 1
                  j2len(i) = npf(j2func+1) / 2 - j2ad(i) - j2pos(i)
                end if
              end do
              if(any_python_func) then
                call vinter_mixed(python, tf,j2ad,j2pos,j2len,nel,xx,dydxv2,gx2)
                if(present(max_slope)) then
                  do i=1,nel
                    j2pos(i) = nint(pos(5,i))
                    j2func=max(ifunc3(i),1)
                    pyid2 = get_python_funct_id(nfunct,j2func,npf,snpc)
                    if(pyid2>0) max_slope(i) = max(max_slope(i), two*abs(dydxv2(i)))
                  end do
                end if

              else
                call vinter2(tf,j2ad,j2pos,j2len,nel,dvxs,dydxv2,gx2)
              end if
              do i=1,nel
                pos(5,i) = j2pos(i)
              end do
            end if
!---------------------------k * funct_id_5
            if(j2k>0)then
              do i=1,nel
                j3pos(i) = nint(pos6(i))
                j3func=max(ifunc4(i),1)
                pyid2 = get_python_funct_id(nfunct,j3func,npf,snpc)
                if(pyid2>0)then
                  !python function
                  j3ad(i) = -pyid2
                  j3len(i)= -pyid2
                  any_python_func = .true.
                else
                  j3ad(i)  = npf(j3func) / 2 + 1
                  j3len(i) = npf(j3func+1) / 2 - j3ad(i) - j3pos(i)
                end if
              end do
              if(any_python_func) then
                call vinter_mixed(python, tf,j3ad,j3pos,j3len,nel,xx,dkdx2,kx2)
                if(present(max_slope)) then
                  do i=1,nel
                    j3pos(i) = nint(pos6(i))
                    j3func=max(ifunc4(i),1)
                    pyid2 = get_python_funct_id(nfunct,j3func,npf,snpc)
                    if(pyid2>0) max_slope(i) = max(max_slope(i), two*abs(dkdx2(i)))
                  end do
                end if
              else
                call vinter2(tf,j3ad,j3pos,j3len,nel,xx,dkdx2,kx2)
              end if
              do i=1,nel
                pos6(i) = j3pos(i)
              end do
            end if
!-------------------------
            if(j2k /= nel)then
              if (present(ifunc4)) then
                do i=1,nel
                  if(ifunc4(i) == 0) kx2(i) = one
                end do
              else
                kx2(1:nel) = one
              end if
            end if
            if(jdmp/=nel)then
              do i=1,nel
                if(ifv(i)==0) gx(i)=zero
              end do
            end if
            if(j2dmp /= nel)then
              do i=1,nel
                if(ifunc3(i) == 0) gx2(i) = zero
                gx2(i) = gx2(i) * kx2(i)
              end do
            else
              do i=1,nel
                gx2(i) = gx2(i) * kx2(i)
              end do
            end if

            do i=1,nel
              dvv  = max(one,abs(dvx(i)/d(i)))
              dfac = ak(i) + b(i) * log(dvv) + ee(i)*gx(i)
              fx(i)= ( dfac*fx(i) + xc(i)*dvx(i) + gf3(i)*gx2(i) ) *off(i)
              e(i) = e(i) + (dx(i)-dxold(i)) * (fx(i)+fold(i)) * half
            end do
          else
            do i=1,nel
              fx(i)= fx(i)  *ak(i)* off(i)
              e(i) = e(i) + (dx(i)-dxold(i)) * (fx(i)+fold(i)) * half
            end do
          end if
          do i=1,nel
            dx(i)=dx(i)*xl0(i)
            dxold(i)=dxold(i)*xl0(i)
            dpx(i)=dpx(i)*xl0(i)
            dpx2(i)=dpx2(i)*xl0(i)
            e(i)=e(i)*xl0(i)
          end do
!
!----
          return
        end subroutine redef3
      end module redef3_mod
