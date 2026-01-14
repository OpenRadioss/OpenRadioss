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
!||    fail_hashin_s_mod   ../engine/source/materials/fail/hashin/fail_hashin_s.F90
!||--- called by ------------------------------------------------------
!||    mmain               ../engine/source/materials/mat_share/mmain.F90
!||    mulaw               ../engine/source/materials/mat_share/mulaw.F90
!||    usermat_solid       ../engine/source/materials/mat_share/usermat_solid.F
!||====================================================================
      module fail_hashin_s_mod
      contains
!||====================================================================
!||    fail_hashin_s   ../engine/source/materials/fail/hashin/fail_hashin_s.F90
!||--- called by ------------------------------------------------------
!||    mmain           ../engine/source/materials/mat_share/mmain.F90
!||    mulaw           ../engine/source/materials/mat_share/mulaw.F90
!||    usermat_solid   ../engine/source/materials/mat_share/usermat_solid.F
!||--- uses       -----------------------------------------------------
!||    constant_mod    ../common_source/modules/constant_mod.F
!||    precision_mod   ../common_source/modules/precision_mod.F90
!||====================================================================
      subroutine fail_hashin_s(                                                &
        nel     ,nuparam ,nuvar   ,uparam  ,uvar    ,                          &
        time    ,ngl     ,ilay    ,ipg     ,npg     ,                          &
        signxx  ,signyy  ,signzz  ,signxy  ,signyz  ,signzx  ,                 &
        dmg_scale,lf_dammx,dfmax  ,tdel    ,timestep,                          &
        loff    ,off     ,noff    ,nipar   ,iparam  )
!
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use precision_mod, only : WP
      use constant_mod
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
      implicit none
!-----------------------------------------------
!   G l o b a l   P a r a m e t e r s
!-----------------------------------------------
#include "units_c.inc"
!-----------------------------------------------
!   A r g u m e n t s
!-----------------------------------------------
      integer,                               intent(in)    :: nel       !< Number of elements in the group
      integer,                               intent(in)    :: nuparam   !< Number of model parameters
      integer,                               intent(in)    :: nuvar     !< Number of state variables
      real(kind=WP), dimension(nuparam)  ,   intent(in)    :: uparam    !< Failure criterion real parameters
      real(kind=WP), dimension(nel,nuvar),   intent(inout) :: uvar      !< User variables array
      real(kind=WP),                         intent(in)    :: time      !< Current time
      integer,       dimension(nel),         intent(in)    :: ngl       !< Global element numbers
      integer,                               intent(in)    :: ilay      !< Layer number
      integer,                               intent(in)    :: ipg       !< Integration point number
      integer,                               intent(in)    :: npg       !< Number of integration points per element
      real(kind=WP), dimension(nel),         intent(in)    :: signxx    !< Current stress component xx
      real(kind=WP), dimension(nel),         intent(in)    :: signyy    !< Current stress component yy
      real(kind=WP), dimension(nel),         intent(in)    :: signzz    !< Current stress component zz
      real(kind=WP), dimension(nel),         intent(in)    :: signxy    !< Current stress component xy
      real(kind=WP), dimension(nel),         intent(in)    :: signyz    !< Current stress component yz
      real(kind=WP), dimension(nel),         intent(in)    :: signzx    !< Current stress component zx
      real(kind=WP), dimension(nel),         intent(inout) :: dmg_scale !< Damage scaling factor
      integer,                               intent(in)    :: lf_dammx  !< Size of damage variable array
      real(kind=WP), dimension(nel,lf_dammx),intent(inout) :: dfmax     !< Maximum damage variable
      real(kind=WP), dimension(nel),         intent(inout) :: tdel      !< Time of element deletion
      real(kind=WP),                         intent(in)    :: timestep  !< Time step size
      real(kind=WP), dimension(nel),         intent(inout) :: loff      !< Local off flag
      real(kind=WP), dimension(nel),         intent(inout) :: off       !< Element off flag
      integer,       dimension(nel),         intent(inout) :: noff      !< Number of broken integration points
      integer,                               intent(in)    :: nipar     !< Number of integer model parameters
      integer,       dimension(nipar),       intent(in)    :: iparam    !< Integer model parameters
!-----------------------------------------------
!   L o c a l   V a r i a b l e s
!-----------------------------------------------
      integer :: i,j,jj,nindx,nindx2,ifail_so,imodel
      integer ,dimension(nel) :: indx,indx2
      real(kind=WP) :: sigt1,sigt2,sigt3,sigc1,sigc2,csig,fsig12,msig12,msig23,&
        msig13,angle,sdel,tmax,ratio,f1,f2,f3,f4,f5,sig,p,f6,f7,tsig12,xsig12, &
        xsig13,xsig23,dammx,telem,k2m,k0,k1,k2,dtinv,km,epspref,filt,alpha,    &
        asrate,fcut
      real(kind=WP), dimension(nel) :: sxx,syy,szz,sxy,syz,szx
!-----------------------------------------------
!   S o u r c e   L i n e s
!-----------------------------------------------
      !=========================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=========================================================================
      !< Integer material parameter
      ifail_so = iparam(1)   !< Shear failure flag
      imodel   = iparam(3)   !< Failure model type
      !< Real material parameters
      sigt1    = uparam(1)   !< 
      sigt2    = uparam(2)   !< 
      sigt3    = uparam(3)   !<
      sigc1    = uparam(4)   !< 
      sigc2    = uparam(5)   !< 
      csig     = uparam(6)   !< Crushing strength
      fsig12   = uparam(7)   !< Shear strength in 1-2 plane
      msig12   = uparam(8)   !< Shear strength in 1-2 plane for matrix failure
      msig13   = uparam(9)   !< Shear strength in 1-3 plane for matrix failure
      msig23   = uparam(10)  !< Shear strength in 2-3 plane for matrix failure
      angle    = uparam(11)  !< Friction angle for matrix failure
      sdel     = uparam(12)  !< Delamination strength ratio
      tmax     = uparam(13)  !< Maximum relaxation time
      fcut     = uparam(14)  !< Frequency cut-off for stress smoothing
      if (fcut > zero) then
        asrate = two*pi*fcut*timestep
        asrate = asrate/(one+asrate)
      else
        asrate = one
      endif
!
      !< Stress filtering if activated
      do i=1,nel
        sxx(i) = asrate*signxx(i) + (one - asrate)*uvar(i,2)
        syy(i) = asrate*signyy(i) + (one - asrate)*uvar(i,3)
        szz(i) = asrate*signzz(i) + (one - asrate)*uvar(i,4)
        sxy(i) = asrate*signxy(i) + (one - asrate)*uvar(i,5)
        syz(i) = asrate*signyz(i) + (one - asrate)*uvar(i,6)
        szx(i) = asrate*signzx(i) + (one - asrate)*uvar(i,7)  
        uvar(i,2) = sxx(i)
        uvar(i,3) = syy(i)
        uvar(i,4) = szz(i)
        uvar(i,5) = sxy(i)
        uvar(i,6) = syz(i)
        uvar(i,7) = szx(i)
      enddo
!
      !=========================================================================
      ! - COMPUTATION OF THE DAMAGE VARIABLE EVOLUTION
      !=========================================================================
      nindx = 0  
      indx(1:nel) = 0
      nindx2 = 0
      indx2(1:nel) = 0
!
      !< Initialization of element failure index     
      select case (imodel)
!
        !<----------------------------------------------------------------------
        !< Unidirectional lamina model
        !<----------------------------------------------------------------------
        case(1)
!
          !< Loop over the elements
          do i = 1,nel
!
            !< If damage has not been reached yet
            if (dfmax(i,1) < one) then 
!
              !< Reset of local variables        
              f1 = zero
              f2 = zero
              f3 = zero
              f4 = zero
              f5 = zero              
!
              !-----------------------------------------------------------------
              !< Fiber criterion
              !-----------------------------------------------------------------
              !< Tensile/shear fiber mode
              f1  = (max(sxx(i),zero)/sigt1)**2 + (sxy(i)**2 + szx(i)**2)/fsig12**2
              dfmax(i,2) = max(dfmax(i,2),f1)
              dfmax(i,2) = min(dfmax(i,2),one)
              !< Compressive fiber mode
              sig = -sxx(i) + max(-half*(syy(i) + szz(i)),zero)
              if (sig > zero) then 
                f2 = (sig/sigc1)**2 
                dfmax(i,3) = max(dfmax(i,3),f2)
                dfmax(i,3) = min(dfmax(i,3),one)
              endif
!
              !-----------------------------------------------------------------
              !< Crush mode criterion
              !-----------------------------------------------------------------
              p = -third*(sxx(i) + syy(i) + szz(i))
              if (p > 0) then 
                f3 = (p/csig)**2
                dfmax(i,4) = max(dfmax(i,4),f3)
                dfmax(i,4) = min(dfmax(i,4),one)
              endif
!
              !-----------------------------------------------------------------
              !< Matrix failure criterion
              !-----------------------------------------------------------------
              !< Failure matrix mode
              if (syy(i) < zero) then                           
                xsig12 = msig12 - syy(i)*(tan(angle))           
                xsig23 = msig23 - syy(i)*(tan(angle))           
              else                                                 
                xsig12 = msig12                                    
                xsig23 = msig23                                    
              endif          
              f4  = (max(syy(i),zero)/sigt2)**2 + (syz(i)/xsig23)**2 +         &
                    (sxy(i)/xsig12)**2  
              dfmax(i,5) = max(dfmax(i,5),f4)
              dfmax(i,5) = min(dfmax(i,5),one)   
!              
              !< Delamination mode
              if (szz(i) < zero) then                           
                xsig13 = msig13 - szz(i)*(tan(angle))           
                xsig23 = msig23 - szz(i)*(tan(angle))           
              else                                                 
                xsig13 = msig13                                   
                xsig23 = msig23                                    
              endif              
              f5  = (max(szz(i),zero)/sigt3)**2 + (syz(i)/xsig23)**2 +         &
                    (szx(i)/xsig13)**2
              f5  = sdel*sdel*f5
              dfmax(i,6) = max(dfmax(i,6),f5)
              dfmax(i,6) = min(dfmax(i,6),one)
!
              !-----------------------------------------------------------------
              !< Update global damage variable
              !-----------------------------------------------------------------    
              dfmax(i,1) = max(dfmax(i,1),f1,f2,f3,f4,f5)
              dfmax(i,1) = min(one,dfmax(i,1))
!
              !< Check if criterion has been reached
              if (dfmax(i,1) >= one) then
                nindx = nindx+1                                    
                indx(nindx) = i   
                if (ifail_so > 0) then 
                  uvar(i,1) = time
                endif                                  
              endif                                                                                                    
            endif
!
            !< Stress relaxation in case of damage reached
            if ((uvar(i,1) > zero).and.(loff(i) /= zero).and.                  &
                (ifail_so > 0).and.(off(i) /= zero)) then 
              dmg_scale(i) = exp(-(time - uvar(i,1))/tmax)   
              if (dmg_scale(i) < em02) then
                loff(i) = zero
                tdel(i) = time
                dmg_scale(i) = zero
                if (ifail_so == 1) then 
                  off(i) = zero
                  nindx2 = nindx2 + 1 
                  indx2(nindx2) = i  
                elseif (ifail_so == 2) then 
                  noff(i) = noff(i) + 1
                  if (int(noff(i)) >= npg) then
                    off(i) = zero
                    nindx2 = nindx2 + 1 
                    indx2(nindx2) = i  
                  endif
                endif
              endif
            endif                                          
          enddo                   
!  
        !<----------------------------------------------------------------------
        !< Fabric failure model
        !<---------------------------------------------------------------------- 
        case (2)
          !< Loop over the elements
          do i = 1,nel
!
            !< If damage has not been reached yet
            if (dfmax(i,1) < one) then 
!
              !< Reset of local variables        
              f1 = zero
              f2 = zero
              f3 = zero
              f4 = zero
              f5 = zero    
              f6 = zero
              f7 = zero         
!
              !-----------------------------------------------------------------
              !< Fiber criterion
              !-----------------------------------------------------------------
              !< Tensile/shear fiber modes
              f1  = (max(sxx(i),zero)/sigt1)**2 + (sxy(i)**2+szx(i)**2)/fsig12**2
              dfmax(i,2) = max(dfmax(i,2),f1)
              dfmax(i,2) = min(dfmax(i,2),one)
!
              tsig12 = fsig12*sigt2/sigt1
              f2 = (max(syy(i),zero)/sigt2)**2 + (sxy(i)**2 + syz(i)**2)/tsig12**2      
              dfmax(i,3) = max(dfmax(i,3),f2)
              dfmax(i,3) = min(dfmax(i,3),one)
!
              !< Compressive fiber modes
              sig = -sxx(i) + max(-szz(i),zero)
              if (sig > zero) then
                f3 = (sig/sigc1)**2
                dfmax(i,4) = max(dfmax(i,4),f3)
                dfmax(i,4) = min(dfmax(i,4),one)
              endif
!
              sig = -syy(i) + max(-szz(i),zero)
              if (sig > zero) then
                f4 = (sig/sigc2)**2
                dfmax(i,5) = max(dfmax(i,5),f4)
                dfmax(i,5) = min(dfmax(i,5),one)
              endif
!
              !-----------------------------------------------------------------
              !< Crush mode criterion
              !-----------------------------------------------------------------
              p = -third*(sxx(i) + syy(i) + szz(i))
              if (p > zero) then
                f5 = (p/csig)**2
                dfmax(i,6) = max(dfmax(i,6),f5)
                dfmax(i,6) = min(dfmax(i,6),one)
              endif
!
              !-----------------------------------------------------------------
              !< Matrix failure criterion
              !-----------------------------------------------------------------
              !< Shear Matrix failure
              f6 = (sxy(i)/msig12)**2
              dfmax(i,7) = max(dfmax(i,7),f6)
              dfmax(i,7) = min(dfmax(i,7),one)
!
              !< Matrix failure
              if (szz(i) < zero) then                           
                xsig13 = msig13 - szz(i)*(tan(angle))           
                xsig23 = msig23 - szz(i)*(tan(angle))           
              else                                                 
                xsig13 = msig13                                   
                xsig23 = msig23                                    
              endif  
              f7 = (max(szz(i),zero)/sigt3)**2 + (syz(i)/xsig23)**2 + (szx(i)/xsig13)**2  
              f7 = sdel*sdel*f7
              dfmax(i,8) = max(dfmax(i,8),f7)
              dfmax(i,8) = min(dfmax(i,8),one)
!
              !-----------------------------------------------------------------
              !< Update global damage variable
              !-----------------------------------------------------------------
              dfmax(i,1) = max(dfmax(i,1),f1,f2,f3,f4,f5,f6,f7)
              dfmax(i,1) = min(one,dfmax(i,1))
!
              !< Check if criterion has been reached
              if (dfmax(i,1) >= one) then
                nindx = nindx+1                                    
                indx(nindx) = i   
                if (ifail_so > 0) then 
                  uvar(i,1) = time
                endif                                  
              endif                                                                                                   
            endif
!
            !< Stress relaxation in case of damage reached
            if ((uvar(i,1) > zero).and.(loff(i) /= zero).and.                  &
                (ifail_so > 0).and.(off(i) /= zero)) then 
              dmg_scale(i) = exp(-(time - uvar(i,1))/tmax)   
              if (dmg_scale(i) < em02) then
                loff(i) = zero
                tdel(i) = time
                dmg_scale(i) = zero
                if (ifail_so == 1) then 
                  off(i) = zero
                  nindx2 = nindx2 + 1 
                  indx2(nindx2) = i  
                elseif (ifail_so == 2) then 
                  noff(i) = noff(i) + 1
                  if (int(noff(i)) >= npg) then
                    off(i) = zero
                    nindx2 = nindx2 + 1 
                    indx2(nindx2) = i  
                  endif
                endif
              endif
            endif
          enddo     
      end select
!
      !====================================================================
      ! - PRINTOUT DATA ABOUT FAILED MODES
      !====================================================================    
      if (nindx > 0) then 
        do j = 1,nindx    
          i = indx(j)
          write(iout, 1000) ngl(i),ipg,ilay
          write(istdo,1000) ngl(i),ipg,ilay
          if (imodel == 1) then 
            if (dfmax(i,2) == one) write(iout, 2000) '1 - TENSILE/SHEAR FIBER'
            if (dfmax(i,3) == one) write(iout, 2000) '2 - COMPRESSION FIBER'
            if (dfmax(i,4) == one) write(iout, 2000) '3 - CRUSH'
            if (dfmax(i,5) == one) write(iout, 2000) '4 - MATRIX FAILURE'
            if (dfmax(i,6) == one) write(iout, 2000) '5 - DELAMINATION'
          elseif (imodel == 2) then
            if (dfmax(i,2) == one) write(iout, 2000) '1 - TENSILE/SHEAR FIBER 1'
            if (dfmax(i,3) == one) write(iout, 2000) '2 - TENSILE/SHEAR FIBER 2'
            if (dfmax(i,4) == one) write(iout, 2000) '3 - COMPRESSION FIBER 1'
            if (dfmax(i,5) == one) write(iout, 2000) '4 - COMPRESSION FIBER 2'
            if (dfmax(i,6) == one) write(iout, 2000) '5 - CRUSH'
            if (dfmax(i,7) == one) write(iout, 2000) '6 - SHEAR FAILURE MATRIX'
            if (dfmax(i,8) == one) write(iout, 2000) '7 - MATRIX FAILURE'
          endif
        enddo
      endif
!
      if (nindx2 > 0) then 
        do j=1,nindx2    
          i = indx2(j)
          write(iout, 3000) ngl(i),time
          write(istdo,3000) ngl(i),time
        enddo
      endif
!--------------------------------------------
 1000 format(1X,'FAILURE (HASHIN) IN SOLID ELEMENT ',I10,1X,',GAUSS PT', &
             I5,1X,',LAYER',I3)
 2000 format(1X,'MODE',1X,A)
 3000 format(1X,'-- RUPTURE OF SOLID ELEMENT : ',I10,1X,'AT TIME : ',1PE12.4)
!--------------------------------------------
      end subroutine fail_hashin_s
      end module fail_hashin_s_mod
