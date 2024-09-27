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
      !||    sigeps125_mod   ../engine/source/materials/mat/mat125/sigeps125.F90
      !||--- called by ------------------------------------------------------
      !||    mulaw           ../engine/source/materials/mat_share/mulaw.F90
      !||====================================================================
      module sigeps125_mod
        contains
  ! ======================================================================================================================
  ! \brief    Mat058 Lsdyna 
  ! \details Material law based on Mat058 Lsdyna. Dedicated to composite . 
  ! ======================================================================================================================
      !||====================================================================
      !||    sigeps125          ../engine/source/materials/mat/mat125/sigeps125.F90
      !||--- called by ------------------------------------------------------
      !||    mulaw              ../engine/source/materials/mat_share/mulaw.F90
      !||--- uses       -----------------------------------------------------
      !||    constant_mod       ../common_source/modules/constant_mod.F
      !||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||====================================================================
        subroutine sigeps125(                                         &
          nel      ,nuvar    ,uvar     ,matparam ,                    &
          rho0     ,                                                  &
          epsxx    ,epsyy    ,epszz    ,epsxy    ,epsyz    ,epszx   , &
          signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx  , &
          ssp            )
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
          type(matparam_struct_), intent(in) :: matparam !< material parameters data
          my_real, dimension(nel), intent(in) :: rho0 !< material density
          my_real, dimension(nel), intent(in) :: epsxx !< total strain  xx 
          my_real, dimension(nel), intent(in) :: epsyy !< total strain  yy
          my_real, dimension(nel), intent(in) :: epszz !< total strain  zz 
          my_real, dimension(nel), intent(in) :: epsxy !< total strain  xy 
          my_real, dimension(nel), intent(in) :: epsyz !< total strain  yz 
          my_real, dimension(nel), intent(in) :: epszx !< total strain  zx 
          
          my_real, dimension(nel), intent(out) :: signxx !< new stress xx 
          my_real, dimension(nel), intent(out) :: signyy !< new stress yy
          my_real, dimension(nel), intent(out) :: signzz !< new stress zz 
          my_real, dimension(nel), intent(out) :: signxy !< new stress xy 
          my_real, dimension(nel), intent(out) :: signyz !< new stress yz 
          my_real, dimension(nel), intent(out) :: signzx !< new stress zx 
          my_real, dimension(nel), intent(inout) :: ssp !< sound speed
!-----------------------------------------------
!  L o c a l   V a r i a b l e s
!-----------------------------------------------
 !-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer fs, i,damage,updat,updat1,updat2
      my_real                                                       &
       e1,e2,nu12,nu21,em11t,xt,slimt1,em11c,xc,slimc1,             &
       em22t,yt,slimt2,em22c,yc,slimc2,gamma,tau,ems,sc,            &
       slims,gammaf,gammar, tsdm, erods,tsize,ef11t,ef11c,          &
       m1t,m1c,al1t,al2t,al2c,als,mfs,ms,e1d,e2d,g12d,d,            &
       w11,w22,w12,al1c,ef22c,ef22t,e12d,efs,invd,m2c,m2t,          &
       e21d,g12,limit_sig, eint, deint,a11,tauxy, w33,w13,w23,      &
       m3t,m3c,al3t,al3c,als13,als23,mfs13,mfs23,ms13,ms23 ,        &
       g13,g23,nu13,nu31,nu23,nu32,slimt3,slimc3,tauzx,tau1,gamma1, &
       tau2, gamma2, tauyz,c11,c12,c13,c21,c22,c23,c31,c32,c33,     &
       e3, ef33c,ef33t, efs13,efs23, em33c,em33t, ems13,ems23,      &
       sc13,sc23,zc,zt, slims13,slims23
      my_real , dimension(nel) ::  dezz,check
!!======================================================================
!
       FS = 0 ! type of failure yield surface method
                              !  =  -1   
                              !  =  0  
                              !  =  1  
!--------------------------  
          ! Material parameters
       e1    = matparam%uparam(1)  
       e2    = matparam%uparam(2)  
       e3    = matparam%uparam(3)
       g12   = matparam%uparam(4)
       g13   = matparam%uparam(5)
       g23   = matparam%uparam(6)
       nu12  = matparam%uparam(8)  
       nu13  = matparam%uparam(9)  
       nu23  = matparam%uparam(10) 
       nu21  = matparam%uparam(78) 
       nu31  = matparam%uparam(79) 
       nu32  = matparam%uparam(80) 
      ! Fiber direction
       em11t  = matparam%uparam(11) 
       xt     = matparam%uparam(12) 
       slimt1 = matparam%uparam(13) 
       em11c  = matparam%uparam(14) 
       xc     = matparam%uparam(15)
       slimc1 = matparam%uparam(16)
      ! Matrix direction
       em22t  = matparam%uparam(17) 
       yt     = matparam%uparam(18) 
       slimt2 = matparam%uparam(19) 
       em22c  = matparam%uparam(20) 
       yc     = matparam%uparam(21) 
       slimc2 = matparam%uparam(22)  
       !
       ! Matrix direction
       em33t  = matparam%uparam(28) 
       zt     = matparam%uparam(29) 
       slimt3 = matparam%uparam(30) 
       em33c  = matparam%uparam(31) 
       zc     = matparam%uparam(32) 
       slimc3 = matparam%uparam(33)
      ! shear 
       gamma = matparam%uparam(23)
       tau   = matparam%uparam(24)
       ems   = matparam%uparam(25) 
       sc    = matparam%uparam(26)
       slims = matparam%uparam(27)
       ! transversal shear
         !
       gamma1 = matparam%uparam(34)  
       tau1   = matparam%uparam(35)  
       ems13  = matparam%uparam(36)  
       sc13   = matparam%uparam(37) 
       slims13= matparam%uparam(38)  
   !
       gamma2  = matparam%uparam(39)  
       tau2    = matparam%uparam(40)  
       ems23   = matparam%uparam(41)  
       sc23     = matparam%uparam(42)  
       slims23 = matparam%uparam(43) 
     !
       gammaf = matparam%uparam(44) 
       gammar = matparam%uparam(45)  
       tsdm   = matparam%uparam(46)  
      !
       erods = matparam%uparam(47) 
       tsize = matparam%uparam(48) 
      ! parameters of damage ex fon : exp(-(e/ef)**m/alpha)
       ef11t = matparam%uparam(49) 
       m1t   = matparam%uparam(50) 
       al1t  = matparam%uparam(51)
       ef11c = matparam%uparam(52)
       m1c   = matparam%uparam(53) 
       al1c  = matparam%uparam(54) 

       ef22t = matparam%uparam(55)
       m2t   = matparam%uparam(56)
       al2t  = matparam%uparam(57)
       ef22c = matparam%uparam(58)
       m2c   = matparam%uparam(59)
       al2c  = matparam%uparam(60)

       ef33t = matparam%uparam(61) 
       m3t   = matparam%uparam(62)
       al3t  = matparam%uparam(63) 
       ef33c = matparam%uparam(64) 
       m3c   = matparam%uparam(65) 
       al3c  = matparam%uparam(66) 
       ! dir 12
       efs  = matparam%uparam(67)
       ms   = matparam%uparam(68)
       als  = matparam%uparam(69)
       ! dir 13
       efs13  = matparam%uparam(70) 
       ms13   = matparam%uparam(71) 
       als13  = matparam%uparam(72) 
       ! dir 23
       efs23 = matparam%uparam(73) 
       ms23  = matparam%uparam(74) 
       als23 = matparam%uparam(75) 
       !
       fs = nint(matparam%uparam(76))
       damage = nint(matparam%uparam(77))
      ! membrane computing FS = 0, 1, -1 
       do i=1,nel
       ! computing damage by direction
        ! dir 
        w11 = one
        w22 = one 
        check(i) = one
        ! check unloading
         w11 = uvar(i,1)
         w22 = uvar(i,2)
         w33 = uvar(i,3)
         W12 = uvar(i,4)
         w23 = uvar(i,5)
         w13 = uvar(i,6) 
         d = (one - w11*w22*w33*nu12*nu23*nu31 - w11*w22*w33*nu21*nu32*nu13        &
                  - w11*w33*nu31*nu13 - w22*w33*nu23*nu32 - w11*w22*nu12*nu21 )
         c11 = (one - w22*w33*nu23*nu32)*w11*e1
         c22 = (one - w11*w33*nu13*nu31)*w22*e2
         c33 = (one - w11*w22*nu12*nu21)*w33*e3
         c12 = w11*w22*(nu21 + w33*nu23*nu31)*e1
         c21 = w11*w22*(nu12 + w33*nu13*nu32)*e2
         c31 = w11*w33*(nu13 + w22*nu12*nu23)*e3 
         c13 = w11*w33*(nu31 + w22*nu21*nu32)*e1 
         c32 = w22*w33*(nu23 + w11*nu13*nu21)*e3 
         c23 = w22*w33*(nu32 + w11*nu12*nu31)*e2 

         invd = one/(max(em20,d))
         signxx(i) = invd*(c11*epsxx(i) + c12*epsyy(i) + c13*epszz(i))
         signyy(i) = invd*(c21*epsxx(i) + c22*epsyy(i) + c23*epszz(i))
         signzz(i) = invd*(c31*epsxx(i) + c32*epsyy(i) + c33*epszz(i))
         signxy(i) = w12*g12*epsxy(i)
         signzx(i) = w13*g13*epszx(i)
         signyz(i) = w23*g23*epsyz(i)

         eint =  half*(epsxx(i)*signxx(i) + epsyy(i)*signyy(i) + epszz(i)*signzz(i)    & 
                     + epsxy(i)*signxy(i) + epszx(i)*signzx(i) + epsyz(i)*signyz(i))
         deint = eint - uvar(i,8)
         uvar(i,8) = eint
         if(deint < ZERO ) then
          check(i) = -one
         elseif(uvar(i,7) == -one) then
          check(i) = -one 
          if(uvar(i,9) /= zero .and. eint >= uvar(i,9)) check(i) = one
         else 
          check(i) = one  
         endif  
         if(check(i) >= zero ) then
           uvar(i,5) = one
          if(damage > 0 ) then
            if(epsxx(i) >= zero )then
              w11 = epsxx(i)/ef11t
              w11 = exp(m1t*log(w11))/al1t  ! (esp/epsf)^m/alpha
              w11 = exp(-w11)
            else
              w11 = abs(epsxx(i))/ef11c
              w11 = exp(m1c*log(w11))/al1c  ! (esp/epsf)^m/alpha
              w11 = exp(-w11)
            endif
        ! dir 22
            if(epsyy(i) >= zero )then
              w22 = epsyy(i)/ef22t
              w22 = exp(m2t*log(w22))/al2t  ! (esp/epsf)^m/alpha
              w22 = exp(-w22)
            else
              w22 = abs(epsyy(i))/ef22c
              w22 = exp(m2c*log(w22))/al2c  ! (esp/epsf)^m/alpha
              w22 = exp(-w22)
            endif    
            ! dir 33
            if(epszz(i) >= zero )then
              w33 = epszz(i)/ef33t
              w33 = exp(m3t*log(w33))/al3t  ! (esp/epsf)^m/alpha
              w33 = exp(-w33)
            else
              w33 = abs(epszz(i))/ef33c
              w33 = exp(m3c*log(w33))/al3c  ! (esp/epsf)^m/alpha
              w33 = exp(-w33)
            endif  
          endif   
         else ! unlaod
            w11 = uvar(i,1)
            w22 = uvar(i,2)
            w33 = uvar(i,3)
            W12 = uvar(i,4)
            w23 = uvar(i,5)
            w13 = uvar(i,6) 
            uvar(i,7) = -one
         endif  
         ! damage hook matrix
         d = (one - w11*w22*w33*nu12*nu23*nu31 - w11*w22*w33*nu21*nu32*nu13        &
                  - w11*w33*nu31*nu13 - w22*w33*nu23*nu32 - w11*w22*nu12*nu21 )
         c11 = (one - w22*w33*nu23*nu32)*w11*e1
         c22 = (one - w11*w33*nu13*nu31)*w22*e2
         c33 = (one - w11*w22*nu12*nu21)*w33*e3
         c12 = w11*w22*(nu21 + w33*nu23*nu31)*e1
         c21 = w11*w22*(nu12 + w33*nu13*nu32)*e2
         c31 = w11*w33*(nu13 + w22*nu12*nu23)*e3 
         c13 = w11*w33*(nu31 + w22*nu21*nu32)*e1 
         c32 = w22*w33*(nu23 + w11*nu13*nu21)*e3 
         c23 = w22*w33*(nu32 + w11*nu12*nu31)*e2 
         !
         invd = one/(max(em20,d))
         signxx(i) = invd*(c11*epsxx(i) + c12*epsyy(i) + c13*epszz(i))
         signyy(i) = invd*(c21*epsxx(i) + c22*epsyy(i) + c23*epszz(i))
         signzz(i) = invd*(c31*epsxx(i) + c32*epsyy(i) + c33*epszz(i))
         signxy(i) = w12*g12*epsxy(i)
         signzx(i) = w13*g13*epszx(i)
         signyz(i) = w23*g23*epsyz(i)
          
         !
         if( check(i) >= zero ) then
            limit_sig = zero
           if(epsxx(i) >= em11t  ) then
              limit_sig = slimt1*xt
              signxx(i) = max(limit_sig, signxx(i))
           elseif(abs(epsxx(i)) >= em11c)then 
              limit_sig = slimc1*xc
              signxx(i) = -max(limit_sig, abs(signxx(i)))
           endif  
           if(abs(signxx(i)) ==  limit_sig .and. limit_sig > zero) w11 = signxx(i) / epsxx(i)/e1
           if(epsyy(i) >= em22t)then 
              limit_sig = slimt2*yt
              signyy(i) = max(limit_sig, signyy(i))
           elseif(abs(epsyy(i)) >= em22c)then 
              limit_sig = slimc2*yc
              signyy(i) = - max(limit_sig, abs(signyy(i)))
           endif 
           if(abs(signyy(i)) ==  limit_sig .and. limit_sig > zero) w22 = signyy(i) / epsyy(i)/e2
            if(epsZZ(i) >= em33t)then 
              limit_sig = slimt3*zt
              signzz(i) = max(limit_sig, signzz(i))
           elseif(abs(epszz(i)) >= em33c)then 
              limit_sig = slimc3*zc
              signzz(i) = - max(limit_sig, abs(signzz(i)))
           endif 
           if(abs(signzz(i)) ==  limit_sig .and. limit_sig > zero) w33 = signzz(i) / epszz(i)/e3
            ! save w11 & w22 
           uvar(i,1)= w11
           uvar(i,2)= w22
           uvar(i,3) = w33
           uvar(i,9)= eint
         endif ! check 
         a11       = max(e1,e2,e3)  ! bulk + G*4/3 ????
         ssp(i) = sqrt(a11/rho0(i))
       end do ! nel loop 
      select  case (fs)
           case(-1)
              do i=1,nel
                ! shear w12
                 w12 = one
                 w13 = one
                 w23 = one
                 updat = 1
                 if(check(i) >= zero)then
                   w12 = abs(epsxy(i))/efs
                   w12 = exp(ms*log(w12))/als  ! (esp/epsf)^m/alpha
                   w12 = exp(-w12)
                   !
                   w13 = abs(epszx(i))/efs13
                   w13 = exp(ms13*log(w13))/als13 ! 
                   w13 = exp(-w13) 
                   !
                   w23 = abs(epsyz(i))/efs23
                   w23 = exp(ms23*log(w23))/als23 
                   w23 = exp(-w23)
                 else
                   w12 = uvar(i,4)
                   w13 = uvar(i,5)
                   w23 = uvar(i,6)
                   uvar(i,7) = -one 
                 endif    
                  signxy(i) = w12*g12*epsxy(i)
                  signzx(i) = w13*g13*epszx(i)
                  signyz(i) = w23*g23*epsyz(i)
                  if(abs(signxy(i)) >= tau ) then
                    tauxy = abs(epsxy(i)/gamma)
                    tauxy = tau + tauxy*(sc - tau)/(ems - gamma)
                    signxy(i) = sign(tauxy,signxy(i))
                    if(abs(signxy(i)) > sc) then
                       limit_sig =  slims*sc
                       signxy(i) = sign(limit_sig, signxy(i))
                       if(abs(signxy(i)) ==  limit_sig .and. check(i) >= zero) then
                         w12 = signxy(i)/epsxy(i)/g12
                       endif  
                    endif 
                  endif  
                   if(abs(signzx(i)) >= tau1 ) then
                    tauzx = abs(epszx(i)/gamma1)
                    tauzx = tau1 + tauzx*(sc13 - tau1)/(ems13 - gamma1)
                    signzx(i) = sign(tauzx,signzx(i))
                    if(abs(signzx(i)) > sc13) then
                       limit_sig =  slims13*sc13
                       signzx(i) = sign(limit_sig, signzx(i))
                       if(abs(signzx(i)) ==  limit_sig .and. check(i) >= zero) then
                         w13 = signzx(i)/epszx(i)/g13
                       endif  
                    endif 
                  endif   
                   if(abs(signyz(i)) >= tau2 ) then
                    tauyz = abs(epsyz(i)/gamma2)
                    tauyz = tau2 + tauyz*(sc23 - tau2)/(ems23 - gamma2)
                    signyz(i) = sign(tauyz,signyz(i))
                    if(abs(signyz(i)) > sc23) then
                       limit_sig =  slims23*sc23
                       signyz(i) = sign(limit_sig, signyz(i))
                       if(abs(signyz(i)) ==  limit_sig .and. check(i) >= zero) then
                         w23 = signyz(i)/epsyz(i)/g23
                       endif  
                    endif 
                  endif    
                  
                  uvar(i,4)= w12
                  uvar(i,5)= w13
                  uvar(i,6)= w23
               enddo ! nel loop
            case(0)
               do i=1,nel
                  w12 = one 
                  w13 = one
                  w23 = one
                  IF(check(i) < zero ) then
                    signxy(i) = uvar(i,4)*g12*epsxy(i)
                    signzx(i) = uvar(i,5)*g13*epszx(i)
                    signyz(i) = uvar(i,6)*g23*epsyz(i)
                    uvar(i,7) = -one 
                  else 
                     signxy(i) = g12*epsxy(i)
                     signzx(i) = g13*epszx(i)
                     signyz(i) = g23*epsyz(i)
                     if(abs(signxy(i)) > sc)then
                        limit_sig = slims*sc
                        signxy(i) = sign(limit_sig, signxy(i))
                        if(abs(signxy(i)) ==  limit_sig ) then
                           w12 = signxy(i)/epsxy(i)/g12
                        endif
                     endif 
                     if(abs(signzx(i)) > sc13)then
                        limit_sig = slims13*sc13
                        signzx(i) = sign(limit_sig, signzx(i))
                        if(abs(signzx(i)) ==  limit_sig ) then
                           w13 = signzx(i)/epszx(i)/g13
                        endif
                     endif 
                     if(abs(signyz(i)) > sc23)then
                        limit_sig = slims23*sc23
                        signyz(i) = sign(limit_sig, signyz(i))
                        if(abs(signyz(i)) ==  limit_sig ) then
                           w23 = signyz(i)/epszx(i)/g23
                        endif
                     endif 
                     uvar(i,4)= w12
                     uvar(i,5)= w13
                     uvar(i,6)= w23
                  endif   
               enddo ! nel loop
            case(1)
           do i=1,nel
                  w12 = one 
                  IF(check(i) < zero ) then
                    signxy(i) = uvar(i,3)*g12*epsxy(i)
                    signzx(i) = uvar(i,5)*g13*epszx(i)
                    signyz(i) = uvar(i,6)*g23*epsyz(i)
                    uvar(i,5) = -one 
                  else 
                     signxy(i) = g12*epsxy(i)
                     if(abs(signxy(i)) > sc)then
                        limit_sig = slims*sc
                        signxy(i) = sign(limit_sig, signxy(i))
                        if(abs(signxy(i)) ==  limit_sig ) then
                           w12 = signxy(i)/epsxy(i)/g12
                        endif
                     endif 
                     if(abs(signzx(i)) > sc13)then
                        limit_sig = slims13*sc13
                        signzx(i) = sign(limit_sig, signzx(i))
                        if(abs(signzx(i)) ==  limit_sig ) then
                           w13 = signzx(i)/epszx(i)/g13
                        endif
                     endif 
                     if(abs(signyz(i)) > sc23)then
                        limit_sig = slims23*sc23
                        signyz(i) = sign(limit_sig, signyz(i))
                        if(abs(signyz(i)) ==  limit_sig ) then
                           w23 = signyz(i)/epszx(i)/g23
                        endif
                     endif 
                     uvar(i,4)= w12
                     uvar(i,5)= w13
                     uvar(i,6)= w23
                  endif   
               enddo ! nel loop
            end select 
!-------------------------------------------------------------------------------------------
          end subroutine sigeps125
      end module sigeps125_mod   
