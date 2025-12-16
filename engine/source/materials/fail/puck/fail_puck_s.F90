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
      module fail_puck_s_mod
      contains
      subroutine fail_puck_s(                                                  &
        nel     ,nuparam ,nuvar   ,uparam  ,uvar    ,                          &
        time    ,ngl     ,ilay    ,ipg     ,npg     ,                          &
        signxx  ,signyy  ,signzz  ,signxy  ,signzx  ,                          &
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
      integer ::                                                               &
        i,j,ifail_so,indx(nel),nindx,indx2(nel),nindx2
      real(kind=WP) ::                                                         &
        sigt1,sigt2, sigc1,sigc2,fsig12,f1,fa,fb,fc,pn12,pp12,pn22,fac,tmax,   &       
        fa_2,fb_2,fc_2,fcut,asrate,sxx(nel),syy(nel),szz(nel),sxy(nel),        &
        szx(nel)
!-----------------------------------------------
!   S o u r c e   L i n e s
!-----------------------------------------------
      !=========================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=========================================================================        
      !< Recovering integer model parameters
      ifail_so = iparam(1)  !< Solid failure flag  
      !< Recovering real model parameters
      sigt1    = uparam(1)  !< Longitudinal tensile strength
      sigt2    = uparam(2)  !< Transverse tensile strength
      sigc1    = uparam(3)  !< Longitudinal compressive strength
      sigc2    = uparam(4)  !< Transverse compressive strength
      fsig12   = uparam(5)  !< In-plane shear strength
      pp12     = uparam(6)  !< Puck inclination parameter 
      pn12     = uparam(7)  !< Puck inclination parameter         
      pn22     = uparam(8)  !< Puck inclination parameter
      tmax     = uparam(9)  !< Softening characteristic time 
      fcut     = uparam(10) !< Frequency cut-off for stress smoothing 
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
        szx(i) = asrate*signzx(i) + (one - asrate)*uvar(i,6)
        uvar(i,2) = sxx(i)
        uvar(i,3) = syy(i)
        uvar(i,4) = szz(i)
        uvar(i,5) = sxy(i)
        uvar(i,6) = szx(i)
      enddo
!   
      !=========================================================================
      ! - COMPUTATION OF THE DAMAGE VARIABLE EVOLUTION
      !=========================================================================
      !< Initialization of element failure index     
      nindx = 0
      indx(1:nel) = 0
      nindx2 = 0
      indx2(1:nel) = 0
!
      !< Loop over the elements
      do i = 1,nel
!
        !< If damage has not been reached yet
        if (dfmax(i,1) < one) then  
!
          !< Reset of local variables               
          f1   = zero
          fa   = zero
          fb   = zero
          fc   = zero
          fa_2 = zero
          fb_2 = zero
          fc_2 = zero  
!            
          !---------------------------------------------------------------------
          !< Fiber criterion
          !---------------------------------------------------------------------
          !< Longitudinal fiber criterion
          if (sxx(i) >= zero) then
            f1 = sxx(i)/sigt1
            dfmax(i,2) = max(dfmax(i,2), f1)
            dfmax(i,2) = min(dfmax(i,2),one)
          else
            f1 = -sxx(i)/sigc1
            dfmax(i,3) = max(dfmax(i,3), f1)
            dfmax(i,3) = min(dfmax(i,3),one)
          endif
!
          !---------------------------------------------------------------------
          !< Matrix criterion direction 2
          !---------------------------------------------------------------------
          !< Inter fiber criterion A
          if (syy(i) >= zero) then
            fac = one - pp12*sigt2/fsig12
            fac = fac*syy(i)/sigt2
            fa = sqrt((sxy(i)/fsig12)**2 + fac*fac) + pp12*syy(i)/fsig12
            dfmax(i,4) = max(dfmax(i,4), fa)
            dfmax(i,4) = min(dfmax(i,4),one)
          else
            !< Inter fiber criterion B
            fb = sqrt(sxy(i)**2 + (pn12*syy(i))**2 ) + pn12*syy(i)
            fb = fb/fsig12
            fb = max(zero, fb)
            dfmax(i,5) = max(dfmax(i,5), fb)
            dfmax(i,5) = min(dfmax(i,5),one)
            !< Inter fiber criterion C
            fac = half/(one + pn22)/fsig12
            fc = (sxy(i)*fac)**2 + (syy(i)/sigc2)**2
            fc =-fc*sigc2/min(syy(i), -em02*sigc2)
            dfmax(i,6) = max(dfmax(i,6), fc)
            dfmax(i,6) = min(dfmax(i,6),one)                            
          endif
!
          !---------------------------------------------------------------------
          !< Matrix criterion direction 3
          !---------------------------------------------------------------------
          !< Inter fiber criterion A    
          if (szz(i) >= zero) then
            fac = one - pp12*sigt2/fsig12
            fac = fac*szz(i)/sigt2
            fa_2 = sqrt((szx(i)/fsig12)**2 + fac*fac) + pp12*szz(i)/fsig12
            dfmax(i,4) = max(dfmax(i,4),fa_2)
            dfmax(i,4) = min(dfmax(i,4), one)
          else
            !< Inter fiber criterion B
            fb_2 = sqrt(szx(i)**2 + (pn12*szz(i))**2 ) + pn12*szz(i)
            fb_2 = fb_2/fsig12 
            fb_2 = max(zero,fb_2)
            dfmax(i,5) = max(dfmax(i,5),fb_2)
            dfmax(i,5) = min(dfmax(i,5), one)
            !< Inter fiber criterion C    
            fac = half/(one + pn22)/fsig12      
            fc_2 = (szx(i)*fac)**2 + (szz(i)/sigc2)**2
            fc_2 =-fc_2*sigc2/min(szz(i), -em02*sigc2)
            dfmax(i,6) = max(dfmax(i,6),fc_2)
            dfmax(i,6) = min(dfmax(i,6), one)                      
          endif
!
          !---------------------------------------------------------------------
          !< Update global damage variable
          !---------------------------------------------------------------------
          dfmax(i,1) = max(dfmax(i,1),f1,fa,fb,fc,fa_2,fb_2,fc_2)
          dfmax(i,1) = min(one,dfmax(i,1))
!        
          !< Check if PUCK criterion has been reached
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
        if ((uvar(i,1) > zero).and.(loff(i) /= zero).and.                      &
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
      !====================================================================
      ! - PRINTOUT DATA ABOUT FAILED MODES
      !====================================================================    
      if (nindx > 0) then 
        do j = 1,nindx    
          i = indx(j)
          write(iout, 1000) ngl(i),ipg,ilay
          write(istdo,1000) ngl(i),ipg,ilay
          if (dfmax(i,2)  == 1) write(iout,2000) '1 - TENSILE FIBER FAILURE'
          if (dfmax(i,3)  == 1) write(iout,2000) '2 - COMPRESSION FIBER FAILURE'
          if (dfmax(i,4)  == 1) write(iout,2000) '3 - INTER-FIBER A'
          if (dfmax(i,5)  == 1) write(iout,2000) '4 - INTER-FIBER B'
          if (dfmax(i,6)  == 1) write(iout,2000) '5 - INTER-FIBER C'
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
 1000 format(1X,'FAILURE (PUCK) IN SOLID ELEMENT ',I10,1X,',GAUSS PT', &
             I5,1X,',LAYER',I3)
 2000 format(1X,'MODE',1X,A)
 3000 format(1X,'-- RUPTURE OF SOLID ELEMENT : ',I10,1X,'AT TIME : ',1PE12.4)
!--------------------------------------------
      end subroutine fail_puck_s
      end module fail_puck_s_mod
