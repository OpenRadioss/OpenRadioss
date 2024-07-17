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
      module sigeps125c_mod
        contains
  ! ======================================================================================================================
  ! \brief   material law /MAT/LAW125
  ! \details Material law  Dedicated to composite application. 
  ! ======================================================================================================================
         SUBROUTINE sigeps125c(                                   &
           nel     ,mat_param  , nuvar    ,uvar,                  &
           rho     ,thk       ,thkly     , shf ,                  &
           epsxx   ,epsyy      ,epsxy   ,epsyz   ,epszx ,         &    
           sigoxx  ,sigoyy     ,                                  &
           signxx  ,signyy     ,signxy  ,signyz   ,signzx  ,      &
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
          my_real, dimension(nel), intent(in) :: sigoxx !< initial stress xx 
          my_real, dimension(nel), intent(in) :: sigoyy !< initial stress yy
          my_real, dimension(nel), intent(in) :: epsxx !< total strain xx 
          my_real, dimension(nel), intent(in) :: epsyy !< total strain yy
          my_real, dimension(nel), intent(in) :: epsxy !< total strain xy 
          my_real, dimension(nel), intent(in) :: epsyz !< total strain yz 
          my_real, dimension(nel), intent(in) :: epszx !< total strain zx 
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
      integer fs, i,damage,updat,updat1,updat2
      my_real                                                       &
       e1,e2,nu12,nu21,em11t,xt,slimt1,em11c,xc,slimc1,             &
       em22t,yt,slimt2,em22c,yc,slimc2,gamma,tau,ems,sc,            &
       slims,gammaf,gammar, tsdm, erods,tsize,ef11t,ef11c,          &
       m1t,m1c,al1t,al2t,al2c,als,mfs,ms,e1d,e2d,g12d,d,            &
       w11,w22,w12,al1c,ef22c,ef22t,e12d,efs,invd,m2c,m2t,          &
       e21d,g12,limit_sig, eint, deint,a11,tauxy,g13,g23
      my_real , dimension(nel) ::  dezz,check
!!======================================================================
!
       FS = 0 ! type of failure yield surface method
                              !  =  -1   
                              !  =  0  
                              !  =  1  
!--------------------------  
          ! Material parameters
       e1    = mat_param%uparam(1)  
       e2    = mat_param%uparam(2)  
       g12   = mat_param%uparam(4)  
       g13   = mat_param%uparam(5) 
       g23   = mat_param%uparam(6)  
       nu12  = mat_param%uparam(8) 
       nu21  = mat_param%uparam(78) 
      ! Fiber direction
       em11t  = mat_param%uparam(11) 
       xt     = mat_param%uparam(12) 
       slimt1 = mat_param%uparam(13) 
       em11c  = mat_param%uparam(14) 
       xc     = mat_param%uparam(15)
       slimc1 = mat_param%uparam(16)
      ! Matrix direction
       em22t  = mat_param%uparam(17) 
       yt     = mat_param%uparam(18) 
       slimt2 = mat_param%uparam(19) 
       em22c  = mat_param%uparam(20) 
       yc     = mat_param%uparam(21) 
       slimc2 = mat_param%uparam(22)  
      ! shear 
       gamma = mat_param%uparam(23)
       tau   = mat_param%uparam(24)
       ems   = mat_param%uparam(25) 
       sc    = mat_param%uparam(26)
       slims = mat_param%uparam(27)
     !
       gammaf = mat_param%uparam(44) 
       gammar = mat_param%uparam(45)  
       tsdm   = mat_param%uparam(46)  
      !
       erods = mat_param%uparam(47) 
       tsize = mat_param%uparam(48) 
      ! parameters of damage ex fon : exp(-(e/ef)**m/alpha)
       ef11t = mat_param%uparam(49) 
       m1t   = mat_param%uparam(50) 
       al1t  = mat_param%uparam(51)
       ef11c = mat_param%uparam(52)
       m1c   = mat_param%uparam(53) 
       al1c  = mat_param%uparam(54) 

       ef22t = mat_param%uparam(55)
       m2t   = mat_param%uparam(56)
       al2t  = mat_param%uparam(57)
       ef22c = mat_param%uparam(58)
       m2c   = mat_param%uparam(59)
       al2c  = mat_param%uparam(60)

       efs  = mat_param%uparam(67)
       ms   = mat_param%uparam(68)
       als  = mat_param%uparam(69)

       fs = nint(mat_param%uparam(76))
       damage = nint(mat_param%uparam(77))
       ! 
       select  case (damage)
         case(1) ! with damage 
      ! membrane computing FS = 0, 1, -1 
          do i=1,nel
      ! computing damage by direction
            ! dir 
             check(i) = one
            ! check unloading
             w11 = uvar(i,1)
             w22 = uvar(i,2)
             W12 = uvar(i,3)
             d = (one - w11*w22*nu12*nu21)
             e1d = w11*e1
             e2d = w22*e2
             e12d = w11*w22*nu12*e2
             e21d = w11*w22*nu21*e2
             invd = one/d
             signxx(i) = invd*(e1d*epsxx(i) + e12d*epsyy(i))
             signyy(i) = invd*(e21d*epsxx(i)+ e2d*epsyy(i))
             signxy(i) = w12*g12*epsxy(i)
             eint =  half*(epsxx(i)*signxx(i)+ epsyy(i)*signyy(i) + epsxy(i)*signxy(i))
             deint = eint - uvar(i,4)
             uvar(i,4) = eint
             if(deint < ZERO ) then
               check(i) = -one
             elseif(uvar(i,5) == -one) then
                check(i) = -one 
                if(uvar(i,6) /= zero .and. eint >= uvar(i,6)) check(i) = one
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
            ! dir 
               if(epsyy(i) >= zero )then
                 w22 = epsyy(i)/ef22t
                 w22 = exp(m2t*log(w22))/al2t  ! (esp/epsf)^m/alpha
                 w22 = exp(-w22)
               else
                 w22 = abs(epsyy(i))/ef22c
                 w22 = exp(m2c*log(w22))/al2c  ! (esp/epsf)^m/alpha
                 w22 = exp(-w22)
               endif    
              endif   
             else ! unlaod
               w11 = uvar(i,1)
               w22 = uvar(i,2)
               uvar(i,5) = -one
             endif  
             ! damage hook matrix
             d = (one - w11*w22*nu12*nu21)
             e1d = w11*e1
             e2d = w22*e2
             e12d = w11*w22*nu12*e1
             e21d = w11*w22*nu21*e2
             invd = one/d
             signxx(i) = invd*(e1d*epsxx(i) + e12d*epsyy(i))
             signyy(i) = invd*(e21d*epsxx(i)+ e2d*epsyy(i))
             signzx(i) = shf(i)*g12*epszx(i) 
             signyz(i) = shf(i)*g12*epsyz(i) 
             if( check(i) >= zero ) then
               limit_sig = zero
               if(epsxx(i) >= em11t  ) then
                 limit_sig = slimt1*xt
                 signxx(i) = max(limit_sig, signxx(i))
               elseif(abs(epsxx(i)) >= em11c)then 
                 limit_sig = slimc1*xc
                 signxx(i) = -max(limit_sig, abs(signxx(i)))
               endif  
               if(abs(signxx(i)) ==  limit_sig) w11 = signxx(i) / epsxx(i)/e1
               if(epsyy(i) >= em22t)then 
                 limit_sig = slimt2*yt
                 signyy(i) = max(limit_sig, signyy(i))
               elseif(abs(epsyy(i)) >= em22c)then 
                 limit_sig = slimc2*yc
                 signyy(i) = - max(limit_sig, abs(signyy(i)))
               endif 
               if(abs(signyy(i)) ==  limit_sig) w22 = signyy(i) / epsyy(i)/e2
              ! save w11 & w22
               uvar(i,1)= w11
               uvar(i,2)= w22
               uvar(i,6)= eint
            endif
            etse(i)   = one
            a11       = max(e1,e2)/(one - nu12**2) 
            a11       = max(e1,e2)
            ssp(i) = sqrt(a11/rho(i))
            sigy(i)    = min(slimt1*xt,slimt2*yt, slimc1*xc,slimc2*yc)
             ! computation of the thickness variation 
            dezz(i)  = -(nu12/e1)*(signxx(i)-sigoxx(i))-(nu12/e2)*(signyy(i)-sigoyy(i)) 
            thk(i)     = thk(i) + dezz(i)*thkly(i)*off(i) 
            enddo ! nel loop
           select  case (fs)
             case(-1)
               do i=1,nel
                ! shear w12
                 w12 = one
                 updat = 1
                 if(check(i) >= zero)then
                   w12 = abs(epsxy(i))/efs
                   w12 = exp(ms*log(w12))/als  ! (esp/epsf)^m/alpha
                   w12 = exp(-w12)
                 else
                   w12 = uvar(i,3)
                   uvar(i,5) = -one 
                 endif    
                  g12d = w12*g12
                  signxy(i) = g12d*epsxy(i)
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
                  uvar(i,3)= w12
               enddo ! nel loop
             case(0)  ! fs = 0 TO CHECK
               do i=1,nel
                  w12 = one 
                  IF(check(i) < zero ) then
                    signxy(i) = uvar(i,3)*g12*epsxy(i)
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
                     uvar(i,3)= w12
                  endif   
               enddo ! nel loop
             case(1)
               do i=1,nel
                  w12 = one 
                  IF(check(i) < zero ) then
                    signxy(i) = uvar(i,3)*g12*epsxy(i)
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
                     uvar(i,3)= w12
                  endif   
               enddo ! nel loop
            end select ! FS
         case(0) ! with out damage (lineare behavior)
            do i=1,nel
             d = (one - nu12*nu21)
             invd = one/d
             signxx(i) = invd*(e1*epsxx(i) + nu12*e1*epsyy(i))
             signyy(i) = invd*(nu21*e2*epsxx(i)+ e2*epsyy(i))
             signxy(i) = g12*epsxy(i)
             signzx(i) = shf(i)*g13*epszx(i) 
             signyz(i) = shf(i)*g23*epsyz(i) 
             etse(i)   = one
             a11       = max(e1,e2)/(one - nu12**2) 
             a11       = max(e1,e2)
             ssp(i) = sqrt(a11/rho(i))
             sigy(i)    = min(slimt1*xt,slimt2*yt, slimc1*xc,slimc2*yc)
            ! computation of the thickness variation 
             dezz(i)  = -(nu12/e1)*(signxx(i)-sigoxx(i))-(nu12/e2)*(signyy(i)-sigoyy(i)) 
             thk(i)     = thk(i) + dezz(i)*thkly(i)*off(i) 
           enddo ! nel loop
          end select ! damage
!-------------------------------------------------------------------------------------------
         end subroutine sigeps125c
      end module sigeps125c_mod  
