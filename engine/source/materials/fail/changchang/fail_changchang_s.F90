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
      module fail_changchang_s_mod
      contains
      subroutine fail_changchang_s(                                            &
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
      integer :: i,j,jj,nindx,nindx2,failip
      integer ,dimension(nel) :: indx,indx2
      real(kind=WP) :: sigt1,sigt2,sigt12,sigc1,sigc2,beta,tmax,fcut,asrate,   &
        damft,damfc,dammt1,dammc1,dammt2,dammc2
      real(kind=WP), dimension(nel) :: sxx,syy,szz,sxy,szx
!-----------------------------------------------
!   S o u r c e   L i n e s
!-----------------------------------------------
      !=========================================================================
      ! - INITIALISATION OF COMPUTATION ON TIME STEP
      !=========================================================================
      !< Integer material parameter
      failip = iparam(1) !< Number of failed integration points prior to solid deletion
      failip = min(failip,npg)
      !< Real material parameters
      sigt1  = uparam(1) ! -> Longitudinal tensile strength    
      sigt2  = uparam(2) ! -> Transversal tensile strength    
      sigt12 = uparam(3) ! -> Shear strength     
      sigc1  = uparam(4) ! -> Longitudinal compressive strength   
      sigc2  = uparam(5) ! -> Transversal compressive strength     
      beta   = uparam(6) ! -> Shear coupling factor 
      tmax   = uparam(7) ! -> Relaxation time     
      fcut   = uparam(8) ! -> Stress tensor filtering frequency
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
      nindx = 0  
      indx(1:nel) = 0
      nindx2 = 0
      indx2(1:nel) = 0
!
      !< Loop over the elements
      do i = 1,nel
!
        !< Reset of local variables        
        damft = zero
        damfc = zero
        dammt1 = zero
        dammc1 = zero
        dammt2 = zero
        dammc2 = zero
!
        ! If damage has not been reached yet
        if (dfmax(i,1) < one) then
!
          !---------------------------------------------------------------------
          !< Fiber criterion
          !---------------------------------------------------------------------    
          !< Tensile fiber mode 
          if (sxx(i) >= zero) then
            damft = (sxx(i)/sigt1)**2 + beta*(sxy(i)/sigt12)**2                &
                  + beta*(szx(i)/sigt12)**2
            damfc = zero
            dfmax(i,2) = max(damft,dfmax(i,2))
            dfmax(i,2) = min(dfmax(i,2),one)
          !< Compressive fiber mode
          else 
            damfc = (sxx(i)/sigc1)**2
            damft = zero
            dfmax(i,3) = max(damfc,dfmax(i,3))
            dfmax(i,3) = min(dfmax(i,3),one)
          endif 
!
          !---------------------------------------------------------------------
          !< Matrix failure criterion
          !---------------------------------------------------------------------
          !< Direction 2
          if (syy(i) >= zero) then                                   
            dammt1 = (syy(i)/sigt2)**2 + (sxy(i)/sigt12)**2                    
            dammc1 = zero        
            dfmax(i,4) = max(dammt1,dfmax(i,4))
            dfmax(i,4) = min(dfmax(i,4),one)   
          else             
            dammc1 = (syy(i)/(two*sigt12))**2 + (sxy(i)/sigt12)**2             &                       
                    + syy(i)*((sigc2/(two*sigt12))**2 - one)/sigc2 
            dammt1 = zero
            dfmax(i,5) = max(dammc1,dfmax(i,5))
            dfmax(i,5) = min(dfmax(i,5),one)  
          endif
!
          !< Direction 3
          if (szz(i) >= zero) then                                   
            dammt2 = (szz(i)/sigt2)**2 + (szx(i)/sigt12)**2                    
            dammc2 = zero        
            dfmax(i,4) = max(dammt2,dfmax(i,4))
            dfmax(i,4) = min(dfmax(i,4),one)  
          else             
            dammc2 = (szz(i)/(two*sigt12))**2 + (szx(i)/sigt12)**2             &                           
                   + szz(i)*((sigc2/(two*sigt12))**2 - one)/sigc2 
            dammt2 = zero
            dfmax(i,5) = max(dammc2,dfmax(i,5))
            dfmax(i,5) = min(dfmax(i,5),one)  
          endif          
!
          !---------------------------------------------------------------------
          !< Update global damage variable
          !--------------------------------------------------------------------- 
          dfmax(i,1) = max(dfmax(i,1),dfmax(i,2),dfmax(i,3),dfmax(i,4),dfmax(i,5))
          dfmax(i,1) = min(one,dfmax(i,1))
          if (dfmax(i,1) >= one) then
            nindx = nindx+1                                    
            indx(nindx) = i
            uvar(i,1) = time
          endif
        endif
!
        !< Stress relaxation in case of damage reached
        if ((uvar(i,1) > zero).and.(loff(i) /= zero).and.                      &
            (failip > 0).and.(off(i) /= zero)) then 
          dmg_scale(i) = exp(-(time - uvar(i,1))/tmax)   
          if (dmg_scale(i) < em02) then
            loff(i) = zero
            tdel(i) = time
            dmg_scale(i) = zero
            noff(i) = noff(i) + 1
            if (int(noff(i)) >= failip) then
              off(i) = zero
              nindx2 = nindx2 + 1 
              indx2(nindx2) = i  
            endif
          endif
        endif 
      enddo
!
      !=========================================================================
      ! - PRINTOUT DATA ABOUT FAILED MODES
      !=========================================================================    
      if (nindx > 0) then 
        do j = 1,nindx    
          i = indx(j)
          write(iout, 1000) ngl(i),ipg,ilay
          write(istdo,1000) ngl(i),ipg,ilay
          if (dfmax(i,2) == one) write(iout, 2000) '1 - TENSILE FIBER'
          if (dfmax(i,3) == one) write(iout, 2000) '2 - COMPRESSION FIBER'
          if (dfmax(i,4) == one) write(iout, 2000) '3 - TENSILE MATRIX'
          if (dfmax(i,5) == one) write(iout, 2000) '4 - COMPRESSION MATRIX'
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
 1000 format(1X,'FAILURE (CHANG) IN SOLID ELEMENT ',I10,1X,',GAUSS PT', &
             I5,1X,',LAYER',I3)
 2000 format(1X,'MODE',1X,A)
 3000 format(1X,'-- RUPTURE OF SOLID ELEMENT : ',I10,1X,'AT TIME : ',1PE12.4)
!--------------------------------------------
      end subroutine fail_changchang_s
      end module fail_changchang_s_mod
