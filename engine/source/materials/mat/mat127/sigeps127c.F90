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
      !||    sigeps127c_mod   ../engine/source/materials/mat/mat127/sigeps127c.F90
      !||--- called by ------------------------------------------------------
      !||    mulawc           ../engine/source/materials/mat_share/mulawc.F
      !||====================================================================
      module sigeps127c_mod
        contains
  ! ======================================================================================================================
  ! \brief   material law /MAT/LAW127
  ! \details Material law  Dedicated to composite application. 
  ! ======================================================================================================================
      !||====================================================================
      !||    sigeps127c         ../engine/source/materials/mat/mat127/sigeps127c.F90
      !||--- called by ------------------------------------------------------
      !||    mulawc             ../engine/source/materials/mat_share/mulawc.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod       ../common_source/modules/constant_mod.F
      !||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||====================================================================
         SUBROUTINE sigeps127c(                             &
           nel     ,mat_param  , nuvar   ,uvar,                   &
           rho     ,thk       ,thkly     , shf ,                  &
           depsxx  ,depsyy     ,depsxy   ,depsyz   ,depszx ,      &    
           sigoxx  ,sigoyy     ,sigoxy  ,sigozx ,sigoyz ,         &
           signxx  ,signyy     ,signxy  ,signzx   ,signyz  ,      &
           off     ,sigy       ,etse    ,ssp   ) 
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
          use matparam_def_mod 
          use constant_mod      
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
          implicit none 
#include  "my_real.inc"
#include  "units_c.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          integer, intent(in) :: nel !< number of elements in the group
          integer, intent(in) :: nuvar !< number of user variables

          my_real, dimension(nel,nuvar), intent(inout) :: uvar !< user variables
          type(matparam_struct_), intent(in) :: mat_param !< material parameters data
          my_real, dimension(nel), intent(in) :: rho !< material density
          my_real, dimension(nel), intent(inout) :: sigy !< yield stress
          my_real, dimension(nel), intent(inout) :: shf !< shear factor correction 
          my_real, dimension(nel), intent(inout) :: thk !< shell thikness 
          my_real, dimension(nel), intent(in)    :: thkly !< ply thikness  
          my_real, dimension(nel), intent(inout) :: etse !< ratio of rigidity  
          my_real, dimension(nel), intent(in) :: depsxx !< incr strain xx 
          my_real, dimension(nel), intent(in) :: depsyy !< incr strain yy
          my_real, dimension(nel), intent(in) :: depsxy !< incr strain xy 
          my_real, dimension(nel), intent(in) :: depsyz !< incr strain yz 
          my_real, dimension(nel), intent(in) :: depszx !< incr strain zx 
          my_real, dimension(nel), intent(in) :: sigoxx !< old stress xx 
          my_real, dimension(nel), intent(in) :: sigoyy !< old stress yy
          my_real, dimension(nel), intent(in) :: sigoxy !< old stress xy 
          my_real, dimension(nel), intent(in) :: sigoyz !< old stress yz 
          my_real, dimension(nel), intent(in) :: sigozx !< old stress zx 
          my_real, dimension(nel), intent(out) :: signxx !< new stress xx 
          my_real, dimension(nel), intent(out) :: signyy !< new stress yy
          my_real, dimension(nel), intent(out) :: signxy !< new stress xy 
          my_real, dimension(nel), intent(out) :: signyz !< new stress yz 
          my_real, dimension(nel), intent(out) :: signzx !< new stress zx 
          my_real, dimension(nel), intent(inout) :: ssp !< sound speed
          my_real, dimension(nel), intent(inout) :: off !< element deletion flag
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer fs, i,damage,updat,updat1,updat2,nkey
      my_real                                                       &
       e1,e2,nu12,nu21,xt,slimt1,xc,slimc1,                         &
       yt,slimt2,em22c,yc,slimc2s,sc, d,                            &
       slims,invd,m2c,m2t,  slimc2,                                 &
       e21d,g12,limit_sig, eint, deint,a11,g13,g23,ycfac
      my_real , dimension(nel) ::  dezz,check,xc_r
      !
      logical :: abit_t,abit_c,abit_s,abit_check
!!======================================================================
          ! Material parameters
       e1    = mat_param%uparam(1)  
       e2    = mat_param%uparam(2)  
       g12   = mat_param%uparam(4)  
       g13   = mat_param%uparam(5) 
       g23   = mat_param%uparam(6)  
       nu12  = mat_param%uparam(7) 
       nu21  = mat_param%uparam(8) 
      ! Fiber direction
       xt     = mat_param%uparam(9) 
       slimt1 = mat_param%uparam(10) 
       xc     = mat_param%uparam(11)
       xc_r(1:nel) = xc
       slimc1 = mat_param%uparam(12)
      ! Matrix direction
       yt     = mat_param%uparam(13) 
       slimt2 = mat_param%uparam(14) 
       yc     = mat_param%uparam(15) 
       slimc2 = mat_param%uparam(16)  
      ! shear 
       sc    = mat_param%uparam(17)
       slims = mat_param%uparam(18)
       ycfac = mat_param%uparam(21)
       do i=1,nel
             d = (one - nu12*nu21)
             invd = one/d
             signxx(i) = sigoxx(i) + invd*(e1*depsxx(i) + nu21*e1*depsyy(i))
             signyy(i) = sigoyy(i) + invd*(nu12*e2*depsxx(i)+ e2*depsyy(i))
             signxy(i) = sigoxy(i) + g12*depsxy(i)
             signzx(i) = sigozx(i) + shf(i)*g13*depszx(i) 
             signyz(i) = sigoyz(i) + shf(i)*g23*depsyz(i) 
             !!
             nkey = nint(uvar(i,2))
            !! eint =  half*(epsxx(i)*signxx(i)+ epsyy(i)*signyy(i) + epsxy(i)*signxy(i))
             deint = half*(depsxx(i)*(signxx(i) + sigoxx(i)) +                                     &
                           depsyy(i)*(signyy(i) + sigoyy(i))) +                                    &
                           depsxy(i)*(signxy(i) + sigoxy(i)) 
             !!deint = eint - uvar(i,1)
             eint = uvar(i,1) + deint
             uvar(i,1) = eint
            !! deint = deint/max(em20,uvar(i,4)) 
            
             if(deint < ZERO ) then
           !! if(deint < zero .and. abs(deint) >= EM6) then 
               check(i) = -one
           !!  elseif(uvar(i,2) == -one) then
            !!    check(i) = -one 
            !!    if(uvar(i,3) /= zero .and. eint >= uvar(i,6)) check(i) = one
             else 
                check(i) = one  
             endif  
             etse(i)   = one
             a11       = max(e1,e2)/(one - nu12**2) 
             a11       = max(e1,e2)
             ssp(i) = sqrt(a11/rho(i))
             sigy(i)    = min(slimt1*xt,slimt2*yt, slimc1*xc,slimc2*yc)
            ! computation of the thickness variation 
              limit_sig=  zero
              abit_check =  btest(nkey,6)
              if(abit_check ) xc_r(i) = ycfac*yc ! to check
              if(check(i) >= zero) then ! loading 
                 ! dir 11
                 abit_t =  btest(nkey,1) 
                 abit_c =  btest(nkey,2) 
                 if(signxx(i) >= xt  .or. (abit_t  .and. signxx(i) >= slimt1*xt)) then
                    limit_sig = slimt1*xt 
                    signxx(i) = limit_sig
                    nkey = IBSET(nkey,1)
                    signyy(i) = sign(slimt1*abs(signyy(i)), signyy(i))
                    signxy(i) = sign(slimt1*abs(signxy(i)), signxy(i))
                  elseif(signxx(i) <=  -xc_r(i) .or. (abit_c .and. signxx(i)  <= - slimc1*xc_r(i))) then
                    signxx(i) = - slimc1*xc_r(i)
                    nkey = IBSET(nkey,2)
                    signyy(i) = sign(slimc1*abs(signyy(i)), signyy(i))
                    signxy(i) = sign(slimc1*abs(signxy(i)), signxy(i))
                  endif  
                ! dir 22
                  abit_t =  btest(nkey,3) 
                  abit_c =  btest(nkey,4) 
                  if(signyy(i) >= yt .or. (abit_t .and. signyy(i) >=  slimt2*yt)) then
                    signyy(i) = slimt2*yt
                    nkey = IBSET(nkey,3)
                    signxx(i) = sign(slimt2*abs(signxx(i)), signxx(i))
                    signxy(i) = sign(slimt2*abs(signxy(i)), signxy(i))
                  elseif(signyy(i) <= -yc .or. (abit_c .and. signyy(i) <= -slimc2*yc)) then
                    signyy(i) = - slimc2*yc
                    nkey = IBSET(nkey,6)
                    nkey = IBSET(nkey,4)
                    signxx(i) = sign(slimc2*abs(signxx(i)), signxx(i))
                    signxy(i) = sign(slimc2*abs(signxy(i)), signxy(i))
                  endif  
                  abit_s =  btest(nkey,5) 
                  if(abs(signxy(i)) >= sc  .or. (abit_s .and. abs(signxy(i)) >=  slims*sc) ) then
                    limit_sig = slims*sc
                    signxy(i) = sign(limit_sig, signxy(i))
                    nkey = IBSET(nkey,5)
                    signxx(i) = sign(slims*abs(signxx(i)), signxx(i))
                    signyy(i) = sign(slims*abs(signyy(i)), signyy(i))
                  endif  
                  nkey = IBSET(nkey,7) ! for loading/unloadinf if is needed
                  uvar(i,2) = nkey
              else ! unloading check < 0
                  signxx(i) = sigoxx(i) + invd*(e1*depsxx(i) + nu21*e1*depsyy(i))
                  signyy(i) = sigoyy(i) + invd*(nu12*e2*depsxx(i)+ e2*depsyy(i))
                  signxy(i) = sigoxy(i) + g12*depsxy(i)
                  signzx(i) = sigozx(i) + shf(i)*g13*depszx(i) 
                  signyz(i) = sigoyz(i) + shf(i)*g23*depsyz(i)
                 ! dir 11
                  if(signxx(i) >= xt .or. sigoxx(i) == slimt1*xt  ) then
                    limit_sig = slimt1*xt
                    signxx(i) = min(signxx(i),limit_sig)
                  elseif(signxx(i) <= -xc .or. sigoxx(i) == -slimc1*xc_r(i) )then
                    limit_sig= - slimc1*xc_r(i)
                    signxx(i)= max(signxx(i),limit_sig)
                  endif 
                ! dir 22
                  if(signyy(i) >= yt .or. sigoyy(i) == slimt2*yt ) then
                    limit_sig = slimt2*yt
                    signyy(i) = min(signyy(i),limit_sig)
                  elseif(signyy(i) <= -yc .or. sigoyy(i) == -slimc2*yc ) then
                    limit_sig= - slimc2*yc
                    signyy(i)= max(signyy(i),limit_sig)
                  endif  
                  if(abs(signxy(i)) >= sc .or. abs(sigoxy(i)) == slims*sc) then
                    limit_sig = slims*sc
                    if(signxy(i) >= zero) then
                       signxy(i) = min(limit_sig, signxy(i))
                    else
                      signxy(i) = max(-limit_sig, signxy(i))
                    endif
                  endif
              endif   
             dezz(i)  = -(nu12/e1)*(signxx(i)-sigoxx(i))-(nu12/e2)*(signyy(i)-sigoyy(i)) 
             thk(i)     = thk(i) + dezz(i)*thkly(i)*off(i) 
           enddo ! nel loop
!-------------------------------------------------------------------------------------------
         end subroutine sigeps127c
      end module sigeps127c_mod 