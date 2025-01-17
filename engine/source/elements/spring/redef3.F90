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
module redef3_mod
contains
!! for performance reasons, this function must inlined, because it is called in a loop
!!      \brief return .true. if the function id corresponds to a python function
   pure integer function get_python_funct_id(nfunct, funct_id, npc) result(id)
! ----------------------------------------------------------------------------------------------------------------------
!                                                     module
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     arguments
! ----------------------------------------------------------------------------------------------------------------------
      integer, intent(in) :: nfunct
      integer, intent(in) :: funct_id !< the id of the function
      integer, intent(in) :: npc(3*nfunct+1)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer :: i
! ----------------------------------------------------------------------------------------------------------------------
!                                                      body
! ----------------------------------------------------------------------------------------------------------------------
      i = 0
      id = 0
      if (funct_id> 0) i = npc(2*nfunct+funct_id+1)
      if(i < 0) id = -i
   end function get_python_funct_id

   subroutine redef3(python,&
   &                 fx,      xk,      dx,      fxep,  &
   &                 dxold,   dpx,     tf,      npf,   &
   &                 xc,      off,     e,       dpx2,  &
   &                 anim,    iani,    pos,            &
   &                 xl0,     dmn,     dmx,     dvx,   &
   &                 ff,      lscale,  ee,      gf3,   &
   &                 ifunc3,  yield,   aldp,    ak,    &
   &                 b,       d,       iecrou,  ifunc, &
   &                 ifv,     ifunc2,  epla,    xx_old,&
   &                 nel, nft, stf,    sanin,   dt1,   &
   &                 iresp,   impl_s,  idyna,   snpc,  &
   &                 max_slope,fx_max,  xkc )
! ----------------------------------------------------------------------------------------------------------------------
!                                                   modules
! ----------------------------------------------------------------------------------------------------------------------
   use python_funct_mod
      use vinter_mixed_mod
      use constant_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   implicit none
! ----------------------------------------------------------------------------------------------------------------------
      implicit none
#include "my_real.inc"
#include "mvsiz_p.inc"
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

      ! my_real arrays
      my_real,intent(in) ::  dt1                                        !< time step
      my_real, dimension(stf),intent(inout) :: tf                       !< functions
      my_real, dimension(sanin),intent(inout) :: anim                   !< animation
      my_real, dimension(5,nel),intent(inout) :: pos                    !< gbuf%posx
      my_real, dimension(nel),intent(inout) :: fx                       !< gbuf%for
      my_real, dimension(nel),intent(inout) :: dx                       !< gbuf%totdepl
      my_real, dimension(nel),intent(inout) :: fxep                     !< gbuf%forep
      my_real, dimension(nel),intent(inout) :: dpx                      !< gbuf%dep_in_tens
      my_real, dimension(nel),intent(inout) :: e                        !< gbuf%eint (internal energy)
      my_real, dimension(nel),intent(inout) :: dpx2                     !< gbuf%dep_in_comp
      my_real, dimension(nel),intent(inout) :: yield                    !< gbuf%yield(ii(1))
      my_real, dimension(nel),intent(inout) :: xx_old                   !< uvar(1,1:nel)
      my_real, dimension(mvsiz),intent(inout) :: xk
      my_real, dimension(mvsiz),intent(inout) :: dxold                  !< previous dx
      my_real, dimension(mvsiz),intent(inout) :: xc                     !<
      my_real, dimension(mvsiz),intent(inout) :: off                    !< element activated/deactivated (local buffer)
      my_real, dimension(mvsiz),intent(inout) :: xl0                    !< length
      my_real, dimension(mvsiz),intent(inout) :: dmn                    !< geo(15,mgn(i))
      my_real, dimension(mvsiz),intent(inout) :: dmx                    !< geo(16,mgn(i))
      my_real, dimension(mvsiz),intent(inout) :: dvx                    !< working array
      my_real, dimension(mvsiz),intent(inout) :: ff                     !< geo(18 ,mgn(i))
      my_real, dimension(mvsiz),intent(inout) :: lscale                 !< geo(39 ,mgn(i))
      my_real, dimension(mvsiz),intent(inout) :: ee                     !< geo(40 ,mgn(i))
      my_real, dimension(mvsiz),intent(inout) :: ak                     !<
      my_real, dimension(mvsiz),intent(inout) :: b                      !<
      my_real, dimension(mvsiz),intent(inout) :: d                      !<
      my_real, dimension(mvsiz),intent(inout) :: epla                   !<
      my_real, dimension(mvsiz),intent(inout), optional :: max_slope    !<
      my_real, dimension(mvsiz),intent(inout), optional :: fx_max       !<
      my_real, dimension(mvsiz),intent(inout), optional :: xkc          !<
      double precision, dimension(mvsiz),intent(inout) :: aldp          !<
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
      integer jpos(mvsiz)
      integer jlen(mvsiz)
      integer jad(mvsiz)
      integer jpos2(mvsiz)
      integer jlen2(mvsiz)
      integer jpos3(mvsiz)
      integer jlen3(mvsiz)
      integer jad2(mvsiz)
      integer jfunc
      integer jfunc2
      integer jdmp
      integer jecrou(-1:11)
      integer j2dmp
      integer k1
      integer np2
      integer i
      integer ii
      integer interp
      integer k
!     integer ic1(mvsiz)
      integer ic2(mvsiz)
      integer fund
      integer ifunc3(mvsiz)
      integer j2pos(mvsiz)
      integer j2len(mvsiz)
      integer j2ad(mvsiz)
      integer j2func
!     real ou real*8
      my_real b1
      my_real ddx(mvsiz)
      my_real fold(mvsiz)
      my_real gx(mvsiz)
      my_real dxela(mvsiz)
      my_real dydx(mvsiz)
      my_real xx(mvsiz)
      my_real xx2(mvsiz)
      my_real xx3(mvsiz)
      my_real yy(mvsiz)
      my_real yy2(mvsiz)
      my_real yy3(mvsiz)
      my_real dydx2(mvsiz)
      my_real dydx3(mvsiz)
      my_real dydxv(mvsiz)
      my_real dperm(mvsiz)
      my_real dvv
      my_real dfac
      my_real dt11
      my_real damp
      my_real damm
      my_real fmax(mvsiz)
      my_real dvxs(mvsiz)
      my_real gf3(mvsiz)
      my_real dydxv2(mvsiz)
      my_real fmin(mvsiz)
      my_real gx2(mvsiz)
      my_real xi1
      my_real xi2
      my_real yi1
      my_real yi2
      my_real x1
      my_real x2
      my_real y1
      my_real y2
      my_real an3y0(mvsiz)
      my_real ddxt
      my_real ddxc

      integer :: nfunct !< total number of functions
      integer :: pyid1, pyid2 !< python function id
      logical :: any_python_func !< any python function?
! ----------------------------------------------------------------------------------------------------------------------
      nfunct = python%funct_offset + python%nb_functs! offset = nb of non-python functions
      any_python_func = .false.
      dt11 = dt1
      if(dt11==zero)dt11 = ep30
      do i=1,nel
         dx(i)=dx(i)/xl0(i)
         dxold(i)=dxold(i)/xl0(i)
         dpx(i)=dpx(i)/xl0(i)
         dpx2(i)=dpx2(i)/xl0(i)
         e(i)=e(i)/xl0(i)
      enddo
!
      do i=1,nel
         fold(i)=fx(i)
         ddx(i)= (dx(i)-dxold(i))
         dvx(i)= ddx(i)/ dt11
         dvxs(i)= dvx(i)*ff(i)
      enddo
!
!
      if(iani/=0)then
         do i=1,nel
            ii=i+nft
            damp=dx(i)/max(dmx(i),em15) !
            damm=dx(i)/min(dmn(i),-em15)
            anim(ii)=max(anim(ii),damp,damm)
            anim(ii)=min(anim(ii),one)
         enddo
      endif
!-------------------------------------
!        vector interpolation (adress)
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
      interp = 0
      jdmp = 0
      j2dmp = 0
!
      do i=1,nel
         if(iecrou(i) == 9)then
            jecrou(9) = jecrou(9) + 1
         elseif(iecrou(i) == 11)then
            jecrou(11) = jecrou(11) + 1
         elseif(ifunc(i)==0)then  ! ifunc =igeo(101)-fct_id1
            jecrou(-1) = jecrou(-1) + 1
!  vectorisation
         elseif(iecrou(i)==0)then
            jecrou(0) = jecrou(0) + 1
            interp = 1
         elseif(iecrou(i)==1)then
            jecrou(1) = jecrou(1) + 1
            interp = 1
         elseif(iecrou(i)==2)then
            jecrou(2) = jecrou(2) + 1
            interp = 1
         elseif(iecrou(i)==3)then
            jecrou(3) = jecrou(3) + 1
            interp = 1
         elseif(iecrou(i)==4)then
            jecrou(4) = jecrou(4) + 1
            interp = 1
         elseif(iecrou(i)==5)then
            jecrou(5) = jecrou(5) + 1
            interp = 1
         elseif(iecrou(i)==6)then
            jecrou(6) = jecrou(6) + 1
            interp = 1
         elseif(iecrou(i)==7)then
            jecrou(7) = jecrou(7) + 1
            interp = 1
         elseif(iecrou(i) == 8)then
            jecrou(8) = jecrou(8) + 1
            interp = 1
         elseif(iecrou(i) == 10)then
            jecrou(10) = jecrou(10) + 1
            interp = 1
         endif
         if(ifv(i)/=0) jdmp = jdmp + 1
         if(ifunc3(i)/=0) j2dmp = j2dmp + 1
      enddo
!
      if(interp>0)then
         do i=1,nel
            jpos(i)  = nint(pos(1,i))
            jpos2(i) = nint(pos(2,i))
            jpos3(i) = nint(pos(3,i))
            pyid1 = get_python_funct_id(nfunct, ifunc(i),npf)
            pyid2 = get_python_funct_id(nfunct, ifunc2(i),npf)
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
            endif

            if(pyid2 > 0) then
               jad2(i) = -pyid2
               jlen2(i) = -pyid2
               ifunc2(i) =  pyid2
               any_python_func = .true.
            else
               jfunc2=max(1,ifunc2(i))
               jad2(i)  = npf(jfunc2) / 2 + 1
               jlen2(i) = npf(jfunc2+1) / 2 - jad2(i) - jpos2(i)
            endif
            xx(i) =zero
            xx2(i)=zero
            xx3(i)=zero
         enddo
      endif
!-------------------------------------
!        non linear elastic, f=f(total length)
!-------------------------------------
      if (jecrou(8) > 0) then
         do i=1,nel
            if (iecrou(i) == 8) then
               xx(i) = aldp(i)
            else
               xx(i) = dx(i)
            endif
         enddo
      endif
!-------------------------------------
!        non linear elastic
!        nl elasto plastic (tension compression decoupled)
!-------------------------------------
      if(jecrou(0)+jecrou(2)>0)then
         do i=1,nel
            if(ifunc(i) /= 0.and.(iecrou(i)==0.or.iecrou(i)==2))then
               xx(i)=dx(i)
            endif
         enddo
      endif
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
               endif
            endif
         enddo
      endif
!-------------------------------------
!        elasto plastic (kinematic hardening)
!-------------------------------------
      if(jecrou(4)>0)then
         do i=1,nel
            if(ifunc(i) /= 0.and.iecrou(i)==4)then
               interp = max(2,interp)
               xx(i) =dx(i)
               xx2(i)=dx(i)
            endif
         enddo
      endif
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
               elseif(dx(i)<zero)then
                  interp = max(3,interp)
                  xx2(i)=dpx2(i)
                  xx3(i)=dpx2(i)
               else
                  interp = max(1,interp)
               endif
            endif
         enddo
      endif
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
               endif
            enddo
         endif
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
                  endif
               enddo
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
               endif
               fx(i)=fxep(i)+an3y0(i)*ddx(i)
               xx(i)=sign(abs(xx_old(i)),fx(i))
               xx(i)=xx(i)+ddx(i)
            endif
         enddo
      endif
!-------------------------------------
!      elasto plastic two curves for load and unload
!-------------------------------------
      if(jecrou(7)>0)then
         interp = max(2,interp)
         do i=1,nel
            if(ifunc(i) /= 0.and.iecrou(i)==7)then
               xx(i) =dx(i)
               xx2(i)=dx(i)
            endif
         enddo
      endif
!-------------------------------------
!        elasto perfeclty plastic (isotropic hardening) - input by values
!-------------------------------------
      if(jecrou(9)>0)then
         do i=1,nel
            if(iecrou(i)== 9) then
               an3y0(i)= zero
               if (abs(fxep(i)) > fx_max(i)) then
                  an3y0(i)= zero
               else
                  an3y0(i)= xk(i)
               endif
!
               fx(i)=fxep(i)+an3y0(i)*ddx(i)
               xx(i)=sign(abs(xx_old(i)),fx(i))
               xx(i)=xx(i)+ddx(i)
            endif
         enddo
      endif
!-------------------------------------
!        elasto plastique (isotropic hardening) - perfeclty plastic in compression
!-------------------------------------


      if(jecrou(10)>0)then

         if(any_python_func) then
            do i=1,nel
               if(ifunc(i) /=0 .and.iecrou(i)== 10 .and. jad2(i) < 0)then
                  fund = -jad2(i)           ! curve id
                  an3y0(i) = zero
                  dxela(i) = dx(i) - dpx(i)
                  if (((dxela(i) >= zero) .or. (fxep(i) >= zero)) .and. (fund > 0)) then
                     call python_call_funct1d(python, fund, fxep(i), y1)
                     call python_deriv_funct1d(python, fund, fxep(i), an3y0(i))
                     if (an3y0(i) == zero) then
                        call python_call_funct1d(python, fund, fxep(i), y1)
                        if (fxep(i) > y1) then
                           call python_deriv_funct1d(python, fund, fxep(i) + ddx(i), an3y0(i))
                        else
                           call python_deriv_funct1d(python, fund, fxep(i) - ddx(i), an3y0(i))
                        end if
                     end if
                     if ((dxela(i) < zero) .and. (abs(ddx(i)) > zero)) then
                        ddxt = -fxep(i) / an3y0(i)
                        ddxc = ddx(i) - ddxt
                        an3y0(i) = (ddxt / ddx(i)) * an3y0(i) + (ddxc / ddx(i)) * xkc(i)
                     end if
                     if (dxela(i) >= zero) xx(i) = xx_old(i) + ddx(i)
                  else
                     an3y0(i) = xkc(i)
                  end if
                  fx(i) = fxep(i) + an3y0(i) * ddx(i)
               endif
            enddo
         endif
         do i=1,nel
            if(ifunc(i) /=0 .and.iecrou(i)== 10 .and. jad2(i) > 0)then
               fund = ifunc2(i)     ! unload curve n3
               np2  = (npf(fund+1)-npf(fund))/2
               an3y0(i)= zero
               dxela(i)=dx(i)-dpx(i)
               if (((dxela(i) >= zero).or.(fxep(i) >= zero)).and.(fund > 0)) then
!--- tension - load curve is used
                  do  k=2,np2
                     k1=2*(k-2)
                     x1=tf(npf(fund)+k1)
                     x2=tf(npf(fund)+k1+2)
                     y1=tf(npf(fund)+k1+1)
                     y2=tf(npf(fund)+k1+3)
                     if((fxep(i)< y2.and.fxep(i)>=y1))then
                        an3y0(i)=(y2-y1)/ (x2-x1)
                        exit
                     endif
                  enddo
                  if (an3y0(i)== zero)then ! extrapolation
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
                  endif
!----       crossing of compression/tension line - mix stiffness computed
                  if ((dxela(i) < zero).and.(abs(ddx(i)) > zero)) then
                     ddxt = -fxep(i)/an3y0(i)
                     ddxc = ddx(i) - ddxt
                     an3y0(i) = (ddxt/ddx(i))*an3y0(i) + (ddxc/ddx(i))*xkc(i)
                  endif
!
                  if (dxela(i) >= zero) xx(i)=xx_old(i)+ddx(i)
               else
!--- compression - perfectly plastic behavior
                  an3y0(i)= xkc(i)
               endif
               fx(i)=fxep(i)+an3y0(i)*ddx(i)
            endif
         enddo
      endif
!-------------------------------------
!        linear elastic in tension - perfleclty plastic in compression (same as 10 without curve)
!-------------------------------------
      if(jecrou(11)>0)then
         do i=1,nel
            if(iecrou(i)== 11) then
               an3y0(i)= zero
               dxela(i)=dx(i)-dpx(i)
               if ((dxela(i) >= zero).or.(fxep(i) >= zero)) then
                  an3y0(i)= xk(i)
!----       crossing of compression/tension line - mix stiffness computed
                  if ((dxela(i) < zero).and.(abs(ddx(i)) > zero)) then
                     ddxt = -fxep(i)/an3y0(i)
                     ddxc = ddx(i) - ddxt
                     an3y0(i) = (ddxt/ddx(i))*an3y0(i) + (ddxc/ddx(i))*xkc(i)
                  endif
               else
                  an3y0(i)= xkc(i)
               endif
               fx(i)=fxep(i)+an3y0(i)*ddx(i)
            endif
         enddo
      endif
!-------------------------------------
!     vector interpolation
!-------------------------------------
      do i=1,nel
         xx(i)  = xx(i) *lscale(i)
         xx2(i) = xx2(i)*lscale(i)
         xx3(i) = xx3(i)*lscale(i)
      enddo
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
         endif
      endif

      if(interp>0)then
         do i=1,nel
            pos(1,i) = jpos(i)
            pos(2,i) = jpos2(i)
            pos(3,i) = jpos3(i)
         enddo
      endif
!-------------------------------------
!        linear elastic
!-------------------------------------
      if(jecrou(-1)>0)then
         do i=1,nel
            if(ifunc(i)==0)then
               fx(i)=xk(i)*dx(i)
            endif
         enddo
      endif
!-------------------------------------
!        non linear elastic, f = f(total length)
!-------------------------------------
      if (jecrou(8) > 0)then
         do i=1,nel
            if (iecrou(i) == 8) fx(i)=yy(i)
         enddo
      endif
!-------------------------------------
!        linear elastic
!-------------------------------------
      if(jecrou(0)>0)then
         do i=1,nel
            if(ifunc(i) > 0 .and.iecrou(i)==0) fx(i)=yy(i)
         enddo
      endif
!-------------------------------------
!        elasto plastic (isotrope)
!-------------------------------------
      if(jecrou(1)>0)then
         do i=1,nel
            if(ifunc(i) /= 0.and.iecrou(i)==1)then
               if(fx(i)>=zero.and.fx(i)>yy(i))then
                  dpx(i)=dpx(i)+(fx(i)-yy(i))/xk(i)
                  fx(i)=yy(i)
               elseif(fx(i)<zero.and.fx(i)<yy(i))then
                  dpx(i)=dpx(i)+(yy(i)-fx(i))/xk(i)
                  fx(i)=yy(i)
               endif
               fxep(i)=fx(i)
            endif
         enddo
      endif
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
               elseif(dx(i)<dpx2(i))then
                  fx(i)   = xk(i) * (dx(i)-dpx2(i))
                  fxep(i) = yy(i)
                  fx(i)   = max(fx(i),fxep(i))
                  dpx2(i) = dx(i) - fx(i) / xk(i)
               else
                  fx(i)   = zero
               endif
            endif
         enddo
      endif
!-------------------------------------
!        elasto plastic (compression tension coupled with 6 dof)
!-------------------------------------
      if(jecrou(3)>0)then
         do i=1,nel
            if(ifunc(i) /= 0.and.iecrou(i)==3)then
               if(fx(i)>=zero.and.fx(i)>yy(i))then
                  epla(i)=epla(i)+abs(yy(i)*(fx(i)-yy(i))/xk(i))
                  fx(i)=yy(i)
               elseif(fx(i)<zero.and.fx(i)<yy(i))then
                  epla(i)=epla(i)+abs(yy(i)*(yy(i)-fx(i))/xk(i))
                  fx(i)=yy(i)
               endif
               fxep(i)=fx(i)
            endif
         enddo
      endif
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
               endif
               if(fx(i)<yy2(i))then
                  ic2(i)=2
                  fx(i) = yy2(i)
               endif
            endif
         enddo
         do i=1,nel
            if(ifunc(i) /= 0.and.iecrou(i)==4)then
               fxep(i)=fx(i)
               dpx(i) = dx(i) - fx(i) / xk(i)
            endif
         enddo
      endif
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
               elseif(dx(i)>zero)then
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
                  endif
               elseif(dx(i)<dpx2(i))then
                  fx(i)=yy(i)
                  dpx2(i) = dx(i)
               elseif(dx(i)<zero)then
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
                  endif
               endif
               fxep(i)=fx(i)
            endif
         enddo
      endif
!-------------------------------------
!        elasto plastic (isotropic hardening)
!-------------------------------------
      if(jecrou(6)>0)then
         if(any_python_func) then
            call vinter_mixed(python, tf,jad,jpos,jlen,nel,xx,dydx,yy)
         else
            call vinter2(tf,jad ,jpos ,jlen ,nel,xx ,dydx ,yy )
         endif
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
               elseif(fx(i)< zero.and.fx(i)< -yield(i))then
                  pos(1,i) = jpos(i)
!-- compute plastic and elastic deformation (total)
                  dpx(i)=dpx(i)+(yy(i)-fx(i))/an3y0(i)
                  dxela(i)=dx(i)-dpx(i)
                  fx(i)=yy(i)
                  yield(i)=-fx(i)
!-- ecr variable for hardening/softening - always incremented with positive value
                  xx_old(i) = xx_old(i) + abs(ddx(i))
               endif
               fxep(i)=fx(i)
            endif
         enddo
      endif
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
               elseif (dx(i)< dxold(i) .and. dx(i)>= 0) then
                  if  (fx(i) < yy2(i)) fx(i) = yy2(i)
               elseif (dx(i)>= dxold(i) .and. dx(i)<0) then
                  if (fx(i)> yy2(i)) fx(i) = yy2(i)
               elseif (dx(i)< dxold(i) .and. dx(i)<0) then
                  if (fx(i)< yy(i))  fx(i) = yy(i)
               endif
               fxep(i) = fx(i)
               dpx(i)  = dx(i) - fx(i) / xk(i)
            endif
         enddo
      endif
!-------------------------------------
!     seatbelt - elasto perfectly plastic (isotropic hardening) - input by values
!-------------------------------------
      if(jecrou(9)>0)then
         do i=1,nel
            if(iecrou(i)== 9)then
               if (abs(xk(i)*xx(i)) > fx_max(i)) then
                  yy(i) =sign(fx_max(i),xx(i))
                  dydx = 0
               else
                  yy(i) = xk(i)*xx(i)
                  dydx = xk(i)
               endif
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
               elseif(fx(i)< zero.and.fx(i)< -yield(i))then
!              pos(1,i) = jpos(i)
!-- compute plastic and elastic deformation (total)
                  dpx(i)=dpx(i)+(yy(i)-fx(i))/an3y0(i)
                  dxela(i)=dx(i)-dpx(i)
                  fx(i)=yy(i)
                  yield(i)=-fx(i)
!-- ecr variable for hardening/softening - always incremented with positive value
                  xx_old(i) = xx_old(i) + abs(ddx(i))
               endif
               fxep(i)=fx(i)
            endif
         enddo
      endif
!-------------------------------------
!     seatbelt - elasto plastique (ecouissage isotrope) in tension - perfleclty plastic in compression
!-------------------------------------
      if(jecrou(10)>0)then
         if(any_python_func) then
            call vinter_mixed(python, tf,jad,jpos,jlen,nel,xx,dydx,yy)
         else
            call vinter2(tf,jad ,jpos ,jlen ,nel,xx ,dydx ,yy )
         endif
         do i=1,nel
            if(ifunc(i) /=  0.and.iecrou(i)== 10)then
               if(fx(i)> zero.and.fx(i)>yield(i))then
                  pos(1,i) = jpos(i)
!-- compute plastic and elastic deformation (total)
                  dpx(i)=dpx(i)+(fx(i)-yy(i))/an3y0(i)
                  fx(i)=yy(i)
                  yield(i)=fx(i)
!-- ecr variable for hardening/softening - always incremented with positive value
                  xx_old(i) = xx_old(i) + abs(ddx(i))
               elseif(fx(i)<= -fx_max(i))then
                  yy(i) = -fx_max(i)
!-- compute plastic deformation (total)
                  dpx(i)=dpx(i)+(-yy(i)+fx(i))/an3y0(i)
                  fx(i)=yy(i)
               endif
               fxep(i)=fx(i)
            endif
         enddo
      endif
!-------------------------------------
!     seatbelt - linear elastic in tension - perfleclty plastic in compression
!-------------------------------------
      if(jecrou(11)>0)then
         do i=1,nel
            if(iecrou(i)== 11)then
               if(fx(i)<= -fx_max(i))then
                  yy(i) = -fx_max(i)
!-- compute plastic deformation (total)
                  dpx(i)=dpx(i)+(-yy(i)+fx(i))/an3y0(i)
                  fx(i)=yy(i)
               endif
               fxep(i)=fx(i)
            endif
         enddo
      endif
!--------------------------------------------------------------------
!     non linear damping
!--------------------------------------------------------------------
      if(impl_s==0.or.idyna>0) then
!  rentrer un func4 (ou 3)
         if(jdmp>0)then
            do i=1,nel
               jpos(i) = nint(pos(4,i))
               jfunc=max(ifv(i),1)
               pyid1 = get_python_funct_id(nfunct,jfunc,npf)
               if(pyid1>0)then
                  !python function
                  jad(i) = -pyid1
                  jlen(i)= -pyid1
                  any_python_func = .true.
               else
                  jad(i)  = npf(jfunc) / 2 + 1
                  jlen(i) = npf(jfunc+1) / 2 - jad(i) - jpos(i)
               endif
            enddo

            if(any_python_func) then
               call vinter_mixed(python, tf,jad,jpos,jlen,nel,xx,dydxv,gx)
            else
               call vinter2(tf,jad,jpos,jlen,nel,dvxs,dydxv,gx)
            endif
!
            do i=1,nel
               pos(4,i) = jpos(i)
            enddo
         endif
!---------------------------g * funct_id_4
         if(j2dmp>0)then
            do i=1,nel
               j2pos(i) = nint(pos(5,i))
               j2func=max(ifunc3(i),1)
               pyid2 = get_python_funct_id(nfunct,j2func,npf)
               if(pyid2>0)then
                  !python function
                  j2ad(i) = -pyid2
                  j2len(i)= -pyid2
                  any_python_func = .true.
               else
                  j2ad(i)  = npf(j2func) / 2 + 1
                  j2len(i) = npf(j2func+1) / 2 - j2ad(i) - j2pos(i)
               endif
            enddo
            if(any_python_func) then
               call vinter_mixed(python, tf,j2ad,j2pos,j2len,nel,xx,dydxv2,gx2)
               if(present(max_slope)) then
                  do i=1,nel
                     j2pos(i) = nint(pos(5,i))
                     j2func=max(ifunc3(i),1)
                     pyid2 = get_python_funct_id(nfunct,j2func,npf)
                     if(pyid2>0) max_slope(i) = max(max_slope(i), two*abs(dydxv2(i)))
                  enddo
               endif

            else
               call vinter2(tf,j2ad,j2pos,j2len,nel,dvxs,dydxv2,gx2)
            endif
            do i=1,nel
               pos(5,i) = j2pos(i)
            enddo
         endif
!-------------------------
         if(jdmp/=nel)then
            do i=1,nel
               if(ifv(i)==0) gx(i)=zero
            enddo
         endif
         if(j2dmp/=nel)then
            do i=1,nel
               if(ifunc3(i)==0) gx2(i)=zero
            enddo
         endif

         do i=1,nel
            dvv  = max(one,abs(dvx(i)/d(i)))
            dfac = ak(i) + b(i) * log(dvv) + ee(i)*gx(i)
            fx(i)= ( dfac*fx(i) + xc(i)*dvx(i) + gf3(i)*gx2(i) ) *off(i)
            e(i) = e(i) + (dx(i)-dxold(i)) * (fx(i)+fold(i)) * half
         enddo
      else
         do i=1,nel
            fx(i)= fx(i)  *ak(i)* off(i)
            e(i) = e(i) + (dx(i)-dxold(i)) * (fx(i)+fold(i)) * half
         enddo
      endif
      do i=1,nel
         dx(i)=dx(i)*xl0(i)
         dxold(i)=dxold(i)*xl0(i)
         dpx(i)=dpx(i)*xl0(i)
         dpx2(i)=dpx2(i)*xl0(i)
         e(i)=e(i)*xl0(i)
      enddo
!
!----
      return
   end
end module
