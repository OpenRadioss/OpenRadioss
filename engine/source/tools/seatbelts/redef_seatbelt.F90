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

!||====================================================================
!||    redef_seatbelt_mod   ../engine/source/tools/seatbelts/redef_seatbelt.F90
!||--- called by ------------------------------------------------------
!||    r23l114def3          ../engine/source/elements/spring/r23l114def3.F
!||====================================================================
      module redef_seatbelt_mod
      implicit none
      contains
!! \brief routine to compute seatbelt spring
!||====================================================================
!||    redef_seatbelt         ../engine/source/tools/seatbelts/redef_seatbelt.F90
!||--- called by ------------------------------------------------------
!||    r23l114def3            ../engine/source/elements/spring/r23l114def3.F
!||--- calls      -----------------------------------------------------
!||    python_solve           ../common_source/modules/python_mod.F90
!||    vinter2                ../engine/source/tools/curve/vinter.F
!||--- uses       -----------------------------------------------------
!||    constant_mod           ../common_source/modules/constant_mod.F
!||    mvsiz_mod              ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod          ../common_source/modules/precision_mod.F90
!||    python_funct_mod       ../common_source/modules/python_mod.F90
!||    redef3_mod             ../engine/source/elements/spring/redef3.F90
!||====================================================================
        subroutine redef_seatbelt(python,                                          &
        &                         fx,         xk,         dx,         fxep,        &
        &                         dxold,      dpx,        tf,         npf,         &
        &                         xc,         off,        e,          anim,        &
        &                         iani,       pos,        xl0,        dmn,         &
        &                         dmx,        lscale,     yield,      ak,          &
        &                         iecrou,     ifunc,      ifunc2,     xx_old,      &
        &                         fx_max,     xkc,        nel_loc,    indexa,      &
        &                         flag,       xk_tan,     eps_old,    fram_factor, &
        &                         nft,        snpc,       stf,        sanin,       &
        &                         dt1,        impl_s,     idyna,      buffer_siz)
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod
          use python_funct_mod
          use redef3_mod
          use precision_mod, only : WP
          use mvsiz_mod, only : mvsiz
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                       Arguments
! ----------------------------------------------------------------------------------------------------------------------
          ! integer arguments
          type(python_) :: python
          integer, intent(in) :: impl_s
          integer, intent(in) :: idyna
          integer, intent(in) :: snpc
          integer, intent(in) :: stf
          integer, intent(in) :: iani
          integer, intent(in) :: flag
          integer, intent(in) :: nft
          integer, intent(in) :: nel_loc
          integer, intent(in) :: sanin
          integer, intent(in) :: buffer_siz                              !< Siz of fx,dx,fxep : either NEL (element buffer) or MVSIZ
          integer, intent(in),dimension(mvsiz) :: ifunc
          integer, intent(inout),dimension(mvsiz) :: ifunc2
          integer, intent(in),dimension(mvsiz) :: iecrou
          integer, intent(in),dimension(mvsiz) :: indexa
          integer, intent(in),dimension(snpc) ::  npf


          ! real arguments
          real(kind=WP), intent(in) :: dt1                                     !< time step
          real(kind=WP), dimension(stf),intent(in) :: tf                       !< function array
          real(kind=WP), dimension(sanin),intent(inout) :: anim                !< animation
          real(kind=WP), dimension(buffer_siz),intent(inout) :: fx                !< gbuf%for
          real(kind=WP), dimension(buffer_siz),intent(inout) :: dx                !< gbuf%totdepl
          real(kind=WP), dimension(buffer_siz),intent(inout) :: fxep              !< gbuf%olddepl
          real(kind=WP), dimension(buffer_siz),intent(inout) :: dpx               !< gbuf%dep_in_tens
          real(kind=WP), dimension(buffer_siz),intent(inout) :: e                 !< gbuf%eint (internal energy)
          real(kind=WP), dimension(buffer_siz),intent(inout) :: xx_old            !< uvar(1,1:nel)
          real(kind=WP), dimension(buffer_siz),intent(inout) :: yield             !< gbuf%yield(ii(1))
          real(kind=WP), dimension(mvsiz),intent(in) :: xk                     !<
          real(kind=WP), dimension(mvsiz),intent(inout) :: dxold               !< previous dx
          real(kind=WP), dimension(mvsiz),intent(inout) :: off                 !< element activation/deactivation
          real(kind=WP), dimension(mvsiz),intent(inout) :: xl0                 !< length
          real(kind=WP), dimension(mvsiz),intent(inout) :: lscale              !< geo(39 ,mgn(i))
          real(kind=WP), dimension(mvsiz),intent(inout) :: ak                  !<
          real(kind=WP), dimension(mvsiz),intent(inout) :: fx_max              !<
          real(kind=WP), dimension(mvsiz),intent(inout) :: xkc                 !<
          real(kind=WP), dimension(mvsiz),intent(inout) :: dmn                 !< geo(15,mgn(i))
          real(kind=WP), dimension(mvsiz),intent(inout) :: dmx                 !< geo(16,mgn(i))
          real(kind=WP), dimension(mvsiz),intent(inout) :: pos                 !< posx
          real(kind=WP), dimension(mvsiz),intent(inout) :: xc                  !<
          real(kind=WP), dimension(mvsiz),intent(inout) :: xk_tan              !<
          real(kind=WP), dimension(mvsiz),intent(in) :: fram_factor            !<
          real(kind=WP), dimension(mvsiz),intent(inout) :: eps_old             !<
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j, k, np2, fund, k1, ii, jecrou(4), interp, jfunc
          integer,dimension(mvsiz) :: jpos
          integer,dimension(mvsiz) :: jlen
          integer,dimension(mvsiz) :: jad
          integer,dimension(mvsiz) :: jad2
          integer,dimension(mvsiz) :: jlen2
          integer :: pyid1,pyid2
          integer :: nfunct

          real(kind=WP) ::x1,x2,y1,y2,dt11
          real(kind=WP) :: xi1,xi2,yi1,yi2
          real(kind=WP) :: damp,damm,fxb,xxb,x1s,x2s
          real(kind=WP),dimension(mvsiz) :: an3y0
          real(kind=WP),dimension(mvsiz) :: xn3fy0
          real(kind=WP),dimension(mvsiz) :: dxela
          real(kind=WP),dimension(mvsiz) :: xx
          real(kind=WP),dimension(mvsiz) :: yy
          real(kind=WP),dimension(mvsiz) :: dydx
          real(kind=WP),dimension(mvsiz) :: ddx
          real(kind=WP),dimension(mvsiz) :: dvx
          real(kind=WP),dimension(mvsiz) :: fold
          real(kind=WP),dimension(mvsiz) :: xk_tansav(mvsiz)

          logical :: any_python_funct
! ======================================================================================================================
!     derived from redef3
!     can be launched on a reduced number of elements (defined by indexa)
!     --> flag = 1 : loop on all elements i = j
!     --> flag = 2 : loop only on 2nd strand of elements in slipring - i /= j
!        (all local arrays use j)
! ----------------------------------------------------------------------------------------------------------------------
          nfunct = python%funct_offset + python%nb_functs - python%nb_sensors! offset = nb of non-python functions

          x1s = -huge(x1s)
          x2s = -huge(x2s)
          fxb = -huge(fxb)
          dt11 = dt1
          if(dt11==zero)dt11 = ep30
!
          do j=1,nel_loc
            i = indexa(j)
            dx(i)=dx(i)/xl0(i)
            dxold(i)=dxold(i)/xl0(i)
            e(i)=e(i)/xl0(i)
            fold(j)=fx(i)
            ddx(j)= (dx(i)-eps_old(i))
            dvx(j)= (dx(i)-dxold(i))/ dt11
            eps_old(i) = dx(i)
          end do
!
          if ((iani/=0).and.(flag==1))then
            do i=1,nel_loc
              ii=i+nft
              damp=dx(i)/max(dmx(i),em15)
              damm=dx(i)/min(dmn(i),-em15)
              anim(ii)=max(anim(ii),damp,damm)
              anim(ii)=min(anim(ii),one)
            end do
          end if
!
! ----------------------------------------------------------------------------------------------------------------------
!        vector interpolation (address)
! ----------------------------------------------------------------------------------------------------------------------
!
          jecrou(1:4) = 0
          interp = 0
!
          do j=1,nel_loc
            i = indexa(j)
            if(iecrou(i) == 0)then
              jecrou(1) = jecrou(1) + 1
            else if(iecrou(i) == 10)then
              jecrou(2) = jecrou(2) + 1
              interp = 1
            else if(iecrou(i) == 11)then
              jecrou(3) = jecrou(3) + 1
            else if(iecrou(i) == 12)then
              jecrou(4) = jecrou(4) + 1
              interp = 1
            end if
          end do
!
          any_python_funct = .false.
          if(interp>0)then
            do j=1,nel_loc
              i = indexa(j)
              jpos(j)  = nint(pos(i))
              jfunc =max(1,ifunc(i))
              pyid1 = get_python_funct_id(nfunct, jfunc,npf,snpc)
              if(pyid1>0)then
                jad(j) = -pyid1
                jlen(j) = -pyid1
                any_python_funct = .true.
              else
                jad(j)   = npf(jfunc) / 2  + 1
                jlen(j)  = npf(jfunc+1) / 2  - jad(j)  - jpos(j)
              end if
              pyid2 = get_python_funct_id(nfunct, ifunc2(i),npf,snpc)
              if(pyid2 > 0) then
                jad2(j) = -pyid2
                jlen2(j) = -pyid2
                any_python_funct = .true.
              else
                jad2(j) = 1
              end if
              xx(j) =zero
            end do

          end if
!
! ----------------------------------------------------------------------------------------------------------------------
!        linear elastic
! ----------------------------------------------------------------------------------------------------------------------
          do j=1,nel_loc
            i = indexa(j)
            if(ifunc(i)==0)then
              fx(i)=xk(i)*dx(i)
              xk_tan(i) = xk(i)
            end if
          end do
!
! ----------------------------------------------------------------------------------------------------------------------
!        elasto plastic (isotropic hardening) - perfectly plastic in compression
! ----------------------------------------------------------------------------------------------------------------------
          if(jecrou(2)>0)then
            if(any_python_funct) then
              do j=1,nel_loc
                i = indexa(j)
                if(ifunc2(i)/=0.and.iecrou(i)== 10 .and. jad2(j) < 0 )then
                  fund = -jad2(j)           ! curve id
                  an3y0(j) = zero
                  dxela(j) = dx(i) - dpx(i)
                  if (((dxela(j) >= zero).or.(fxep(i)>zero).or.((fxep(i)==zero).and.(dxela(j)>=zero))).and.(fund > 0)) then
                    call python_call_funct1d(python, fund, fxep(i), y1)
                    call python_deriv_funct1d(python, fund, fxep(i), an3y0(j))
                    if (an3y0(j) == zero) then
                      call python_call_funct1d(python, fund, fxep(i), y1)
                      if (fxep(i) > y1) then
                        call python_deriv_funct1d(python, fund, fxep(i) + ddx(j), an3y0(j))
                      else
                        call python_deriv_funct1d(python, fund, fxep(i) - ddx(j), an3y0(j))
                      end if
                    end if
                    if (dxela(j) >= zero) xx(i) = xx_old(i) + ddx(i)
                  else
                    an3y0(j) = xkc(i)
                  end if
                  fx(i) = fxep(i) + an3y0(j) * ddx(j)
                end if
              end do
            end if
            do j=1,nel_loc
              i = indexa(j)
              if(ifunc2(i)/=0.and.iecrou(i)== 10 .and. jad2(j) >= 0)then
                fund = ifunc2(i)     ! n3 curve for unloading
                np2  = (npf(fund+1)-npf(fund))/2
                an3y0(j)= zero
                dxela(j)=dx(i)-dpx(i)
                if (((dxela(j) >= zero).or.(fxep(i) > zero).or.((fxep(i)==zero).and.(dxela(j) >= zero))).and.(fund > 0)) then
!--- tension - load curve is used
                  do  k=2,np2
                    k1=2*(k-2)
                    x1=tf(npf(fund)+k1)
                    x2=tf(npf(fund)+k1+2)
                    y1=tf(npf(fund)+k1+1)
                    y2=tf(npf(fund)+k1+3)
                    if((fxep(i)< y2.and.fxep(i)>=y1))then
                      an3y0(j)=(y2-y1)/ (x2-x1)
                      xn3fy0(i)=(fxep(i)-y1)/an3y0(j) + x1   !abs of n3
                      exit
                    end if
                  end do
!
                  if (an3y0(j)==zero) then ! extrapolation (outside of input curve points)
                    x1=tf(npf(fund)+(np2-2)*2)
                    x2=tf(npf(fund)+(np2-2)*2+2)
                    y1=tf(npf(fund)+(np2-2)*2+1)
                    y2=tf(npf(fund)+(np2-2)*2+3)
!
                    xi1=tf(npf(fund))
                    xi2=tf(npf(fund)+2)
                    yi1=tf(npf(fund)+1)
                    yi2=tf(npf(fund)+3)
                    if(fxep(i)>y2)an3y0(j)=(y2-y1)/ (x2-x1)
                    if(fxep(i)<yi1)an3y0(j)=(yi2-yi1)/ (xi2-xi1)
                  end if
!
                  xx(j)=xx_old(i)+ddx(j)
                else
!--- compression - perfectly plastic behavior
                  an3y0(j)= xkc(i)
                end if
                fx(i)=fxep(i)+an3y0(j)*ddx(j)
              end if
            end do
          end if
! ----------------------------------------------------------------------------------------------------------------------
!        linear elastic in tension - perfleclty palstic in compression (same as 10 without curve)
! ----------------------------------------------------------------------------------------------------------------------
          if(jecrou(3)>0)then
            do j=1,nel_loc
              i = indexa(j)
              if(iecrou(i)== 11) then
                an3y0(j)= zero
                dxela(j)=dx(i)-dpx(i)
                if ((dxela(j) >= zero).or.(fxep(i) > zero).or.((fxep(i)==zero).and.(dxela(j) >= zero))) then
                  an3y0(j)= xk(i)
                else
                  an3y0(j)= xkc(i)
                end if
                fx(i)=fxep(i)+an3y0(j)*ddx(j)
              end if
            end do
          end if
! ----------------------------------------------------------------------------------------------------------------------
!        non linear elastic in tension with compression and no plasticity in compression - for 2d seatblets only
! ----------------------------------------------------------------------------------------------------------------------
          if(jecrou(4)>0)then

            if(any_python_funct) then
              do j = 1, nel_loc
                i = indexa(j)
                if (ifunc(i) /= 0 .and. iecrou(i) == 12 .and. jad2(j) < 0) then
                  fund = -jad2(j) ! curve identifier
                  an3y0(j) = zero
                  dxela(j) = dx(i) - dpx(i)
                  call python_call_funct1d(python, fund, fxep(i), an3y0(j))
                  call python_solve(python, fund, xn3fy0(j), fxep(i))
                  xxb = xn3fy0(j) + ddx(j)
                  xk_tansav(j) = an3y0(j)
                  fx(i) = fxep(i) + an3y0(j) * ddx(j)
                  if ((fxep(i) < yield(i)) .and. (fx(i) > yield(i))) then
                    xx(j) = dpx(i) + xn3fy0(j) + ddx(j)
                  else
                    xx(j) = xx_old(i) + ddx(j)
                  end if
                end if
              end do
            end if
            do j=1,nel_loc
              i = indexa(j)
              if(ifunc2(i)/= 0.and.iecrou(i)== 12 .and. jad2(j) > 0)then
                fund = ifunc2(i)     ! courbe n3 de unload
                np2  = (npf(fund+1)-npf(fund)) / 2
                an3y0(j)= zero
                dxela(j)=dx(i)-dpx(i)
                xxb = 0
!---        tension - load curve is used
                do  k=2,np2
                  k1=2*(k-2)
                  x1=tf(npf(fund)+k1)
                  x2=tf(npf(fund)+k1+2)
                  y1=tf(npf(fund)+k1+1)
                  y2=tf(npf(fund)+k1+3)
                  if((fxep(i)< y2.and.fxep(i)>=y1))then
                    x1s = x1
                    x2s = x2
                    an3y0(j)=(y2-y1)/ (x2-x1)
                    xn3fy0(j)=(fxep(i)-y1)/an3y0(j) + x1   !abs de n3
                    exit
                  end if
                end do
!---        extrapolation (outside of input curve points)
                if (an3y0(j)== zero)then !
                  x1=tf(npf(fund)+(np2-2)*2)
                  x2=tf(npf(fund)+(np2-2)*2+2)
                  y1=tf(npf(fund)+(np2-2)*2+1)
                  y2=tf(npf(fund)+(np2-2)*2+3)
                  xi1=tf(npf(fund))
                  xi2=tf(npf(fund)+2)
                  yi1=tf(npf(fund)+1)
                  yi2=tf(npf(fund)+3)
                  if(fxep(i)>y2) then
                    an3y0(j)=(y2-y1)/ (x2-x1)
                    xn3fy0(j)=(fxep(i)-y1)/an3y0(j) + x1
                    x1s = x2
                    x2s = ep20
                  else if(fxep(i)<yi1) then
                    an3y0(j)=(yi2-yi1)/ (xi2-xi1)
                    xn3fy0(j)=(fxep(i)-yi1)/an3y0(j) + xi1
                    x1s = -ep20
                    x2s = xi1
                  end if
                  xk_tansav(j)=an3y0(j)
                end if
                xxb =xn3fy0(j)+ddx(j)
                xk_tansav(j)=an3y0(j)
                if (fxep(i)==yield(i)) dpx(i) = xx_old(i) - xn3fy0(j)
!---        next point is in another part of the curve
                if (xxb< x1s.or.xxb>x2s) then
                  xk_tansav(j)=zero
                  do  k=2,np2
                    k1=2*(k-2)
                    x1=tf(npf(fund)+k1)
                    x2=tf(npf(fund)+k1+2)
                    y1=tf(npf(fund)+k1+1)
                    y2=tf(npf(fund)+k1+3)
                    if((xxb < x2.and.xxb >=x1))then
                      xk_tansav(j)=(y2-y1)/ (x2-x1)
                      fxb = y1 + ((y2-y1)/(x2-x1))*(xxb-x1)
                      an3y0(j)= (fxep(i)-fxb)/ (xn3fy0(j)-xxb)
                      exit
                    end if
                  end do
                  if (xk_tansav(j)== zero)then ! extrapolation (outside of input curve points)
                    x1=tf(npf(fund)+(np2-2)*2)
                    x2=tf(npf(fund)+(np2-2)*2+2)
                    y1=tf(npf(fund)+(np2-2)*2+1)
                    y2=tf(npf(fund)+(np2-2)*2+3)
                    xi1=tf(npf(fund))
                    xi2=tf(npf(fund)+2)
                    yi1=tf(npf(fund)+1)
                    yi2=tf(npf(fund)+3)
                    if(xxb>x2) then
                      xk_tansav(j)=(y2-y1)/ (x2-x1)
                      fxb = y2 + xk_tansav(j)*(xxb-x2)
                    else if(xxb<xi1) then
                      xk_tansav(j)=(yi2-yi1)/ (xi2-xi1)
                      fxb = yi1 + xk_tansav(j)*(xxb-xi1)
                    end if
                    an3y0(j)= (fxep(i)-fxb)/ (xn3fy0(j)-xxb)
                  end if
                end if
                fx(i)=fxep(i)+an3y0(j)*ddx(j)
                if ((fxep(i) < yield(i)).and.(fx(i) > yield(i))) then
!----         crossing of the yield line
                  xx(j)=dpx(i) + xn3fy0(j) + ddx(j)
                else
                  xx(j)=xx_old(i)+ddx(j)
                end if
              end if
            end do
          end if
! ----------------------------------------------------------------------------------------------------------------------
!     vector interpolation
! ----------------------------------------------------------------------------------------------------------------------
          do j=1,nel_loc
            i = indexa(j)
            xx(j)  = xx(j) *lscale(i)
          end do
! ----------------------------------------------------------------------------------------------------------------------
!     seatbelt - elasto plastique (ecouissage isotrope) in tension - perfleclty plastic in compression
! ----------------------------------------------------------------------------------------------------------------------
          if(jecrou(2)>0)then
            call vinter2(tf,jad ,jpos ,jlen ,nel_loc,xx ,dydx ,yy )
            do j=1,nel_loc
              i = indexa(j)
              if(ifunc(i)/= 0.and.iecrou(i)== 10)then
                if(fx(i)> zero.and.fx(i)>yield(i))then
                  pos(i) = jpos(j)
!-- compute plastic and elastic deformation (total)
                  dpx(i)=dpx(i)+(fx(i)-yy(j))/max(em20,an3y0(j))
                  fx(i)=yy(j)
                  yield(i)=fx(i)
!-- ecr variable for hardening/softening - always incremented with positive value
                  xx_old(i) = xx_old(i) + abs(ddx(j))
                  xk_tan(i) = dydx(j)
                else if(fx(i)<= -fx_max(i))then
                  yy(j) = -fx_max(i)
!-- compute plastic deformation (total)
                  if (xkc(i) > zero) dpx(i)=dpx(i)+(-yy(j)+fx(i))/max(em20,an3y0(j))
                  fx(i)=yy(j)
                  xk_tan(i) = xk(i)
                else
                  xk_tan(i) = an3y0(j)
                end if
                fxep(i)=fx(i)
              end if
            end do
          end if
! ----------------------------------------------------------------------------------------------------------------------
!     seatbelt - linear elastic in tension - perfleclty plastic in compression
! ----------------------------------------------------------------------------------------------------------------------
          if(jecrou(3)>0)then
            do j=1,nel_loc
              i = indexa(j)
              if(iecrou(i)== 11)then
                if(fx(i)<= -fx_max(i))then
                  yy(j) = -fx_max(i)
!-- compute plastic deformation (total)
                  if (xkc(i) > zero) dpx(i)=dpx(i)+(-yy(j)+fx(i))/max(em20,an3y0(j))
                  fx(i)=yy(j)
                  xk_tan(i) = xk(i)
                else
                  xk_tan(i) = an3y0(j)
                end if
                fxep(i)=fx(i)
              end if
            end do
          end if
! ----------------------------------------------------------------------------------------------------------------------
!     non linear elastic in tension with compression and no plasticity in compression - for 2d seatblets only
! ----------------------------------------------------------------------------------------------------------------------
          if(jecrou(4)>0)then
            call vinter2(tf,jad ,jpos ,jlen ,nel_loc,xx ,dydx ,yy )
            do j=1,nel_loc
              i = indexa(j)
              if(ifunc(i)/= 0.and.iecrou(i)== 12)then
                if(fx(i)> zero.and.fx(i)>yield(i))then
                  pos(i) = jpos(j)
!-- compute plastic and elastic deformation (total)
                  fx(i)=yy(j)
                  yield(i)=fx(i)
!-- ecr variable for hardening/softening - always incremented with positive value
                  xx_old(i) = max(xx_old(i),abs(xx(j)))
                  xk_tan(i) = dydx(j)
                else
                  xk_tan(i) = xk_tansav(j)
                end if
                fxep(i)=fx(i)
              end if
            end do
          end if
! ----------------------------------------------------------------------------------------------------------------------
!     non linear damping
! ----------------------------------------------------------------------------------------------------------------------
          if(impl_s==0.or.idyna>0) then
            do j=1,nel_loc
              i = indexa(j)
              fx(i)= (ak(i)*fx(i) + xc(i)*dvx(j)) *off(i)
              e(i) = e(i) + (dx(i)-dxold(i)) * (fx(i)+fold(j)) / 2
            end do
          else
            do j=1,nel_loc
              i = indexa(j)
              fx(i)= fx(i)  *ak(i)* off(i)
              e(i) = e(i) + (dx(i)-dxold(i)) * (fx(i)+fold(j)) / 2
            end do
          end if
! ----------------------------------------------------------------------------------------------------------------------
          do j=1,nel_loc
            i = indexa(j)
            dx(i)=dx(i)*xl0(i)
            dxold(i)=dxold(i)*xl0(i)
            e(i)=e(i)*xl0(i)
            xk_tan(i) = xk_tan(i)*fram_factor(i)
          end do
!
! ----------------------------------------------------------------------------------------------------------------------
          return
        end subroutine redef_seatbelt
      end module redef_seatbelt_mod
