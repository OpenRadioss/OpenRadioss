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
!Chd|====================================================================
!Chd|  brokmann_crack_init          source/materials/fail/windshield_alter/brokmann_crack_init.F
!Chd|-- called by -----------
!Chd|-- calls ---------------
!Chd|====================================================================
      !||====================================================================
      !||    brokmann_crack_init_mod   ../starter/source/materials/fail/windshield_alter/brokmann_crack_init.F90
      !||--- called by ------------------------------------------------------
      !||    fail_windshield_init      ../starter/source/materials/fail/windshield_alter/fail_windshield_init.F
      !||====================================================================
      module brokmann_crack_init_mod
      contains
! ========================================================================================
! \brief initializes random crack in /fail/alter following Ch.Brokmann extension
! \details
! ========================================================================================

      !||====================================================================
      !||    brokmann_crack_init       ../starter/source/materials/fail/windshield_alter/brokmann_crack_init.F90
      !||--- called by ------------------------------------------------------
      !||    fail_windshield_init      ../starter/source/materials/fail/windshield_alter/fail_windshield_init.F
      !||--- calls      -----------------------------------------------------
      !||--- uses       -----------------------------------------------------
      !||====================================================================
      subroutine brokmann_crack_init(nel,ipt,npt,nuparam,nuvar,uparam,   &
                                     brokmann,uvar,indx,thk,aldt,ngl)

!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
      use brokmann_random_def_mod
      use newman_raju_mod
      use constant_mod ,only : zero,half,one,two,pi,em6,ep06
! ---------------------------------------------------------------------------------------------
          implicit none
! ---------------------------------------------------------------------------------------------
!     included files
! ---------------------------------------------------------------------------------------------

#include "my_real.inc"

!-----------------------------------------------
!    D u m m y   A r g u m e n t s
!-----------------------------------------------
      integer ,intent(in)  :: nel                             !< size of element group
      integer ,intent(in)  :: ipt                             !< current integration point in thickness
      integer ,intent(in)  :: npt                             !< number of integration points in thickness
      integer ,intent(in)  :: nuparam                         !< number of failure model parameters
      integer ,intent(in)  :: nuvar                           !< number of state variables
      integer ,dimension(nel)      ,intent(in)    :: indx     !< element index table
      integer ,dimension(nel)      ,intent(in)    :: ngl      !< element id table
      my_real ,dimension(nuparam)  ,intent(in)    :: uparam   !< failure model parameter table
      my_real ,dimension(nel,nuvar),intent(inout) :: uvar     !< state variables of failure model
      my_real ,dimension(nel) ,intent(in)         :: thk      !< element thickness
      my_real ,dimension(nel) ,intent(in)         :: aldt     !< element size
      type (brokmann_)        ,intent(in)         :: brokmann !< brokmann data structure
!-----------------------------------------------
!    L o c a l   v a r i a b l e s
!-----------------------------------------------
      integer :: i,ib,p_switch,iter,irand,idebug
      my_real :: v0,vm,randp,a_drb,exp_n,k_ic,kcm,af,afp,al,crleN
      my_real :: eta,eta1,eta2,beta1,beta2,tau1,tau2,eta_drb,beta_drb,tau_drb,betai
      my_real :: p_scale,ff,phi,facn,ns1,ns2,ns3,ns4,m1,m2,thkm,aldtm,yiter
      my_real :: yi1,yi2,py,pf,pw,p0,p1,p2,p3,p4,p5,y_drb,y_shift,depth_max,ytol,delta_y
      my_real :: dsig_drb,sig0,sig1,sig2,sig3,sig4,sig5,sig6,sshift,sig_mpa
      my_real :: fsize,fac_m,fac_l,fac_t,fac_v,fac_lenm,fac_mpa,fac_pi2
      my_real, dimension(nel) :: cr_len,cr_depth,cr_ang
      my_real, dimension(nel) :: sig_drb,y0
!-----------------------
!---  state variables for ch.brokmann extension
!     uvar(15) = fail_b   :  failure flag, set = 1 to allow alter test
!     uvar(16) = cr_len   :  crack length
!     uvar(17) = cr_depth :  crack width
!     uvar(18) = cr_ang   :  random crack angle
!     uvar(19) = thk      :  initial thickness of the shell (in micrometers)
!     uvar(20) = aldt0    :  initial width of the shell (in micrometers)
!     uvar(21) = sig_cos  :  crack opening stress (saved for filtering)
!=======================================================================
      idebug   = 0
      exp_n    = uparam(1)
      k_ic     = uparam(6)
      v0       = uparam(8)
      a_drb    = uparam(23)
      eta1     = uparam(24)
      beta1    = uparam(25)
      tau1     = uparam(26)
      eta2     = uparam(27)
      beta2    = uparam(28)
      tau2     = uparam(29)
      p_scale  = uparam(31)
      p_switch = nint(uparam(32))
      fac_m    = uparam(33) 
      fac_l    = uparam(34)
      fac_t    = uparam(35)
!--------------------------------------------------
!     parameter initialization and unit_conversions
!--------------------------------------------------
      fac_v    = fac_l / fac_t  ! conversion to (m/s)
      fac_lenm = ep06 * fac_l   ! conversion to micrometers
      fac_mpa  = em6  * fac_m / (fac_l * fac_t**2) ! stress conversion to MPa      
      vm  = v0 * fac_v          ! (m/s)
      kcm = k_ic * sqrt(fac_l) 
!
!     hard coded values for calculation of statistical distribution parameters
      sig0      = 75.0   ! [mpa]
      sshift    = 50.0   ! [mpa]
      dsig_drb  = 2.0    ! [mpa/s]
      depth_max = 100.0  ! maximal initial crack depth in micrometers
      yi1       = 0.663
      yi2       = 1.1
      ytol      = 0.0001
      delta_y   = ytol*two
      fac_pi2   = half
!
      sig1 = sig0 + sshift  ! 125 mpa
      sig2 = sig1 + sshift  ! 175 mpa
      sig3 = sig2 + sshift  ! 225 mpa
      sig4 = sig3 + sshift  ! 275 mpa
      sig5 = sig4 + sshift  ! 325 mpa
      sig6 = sig5 + sshift  ! 375 mpa
!------------------
      ns1  = exp_n
      ns2  = exp_n * ns1
      ns3  = exp_n * ns2
      ns4  = exp_n * ns3
      facn = (exp_n - two) / (two*(exp_n+one))
      m1   = exp_n / (exp_n - two)
      m2   = two   / (exp_n - two)
!
      cr_len(:) = zero
      cr_ang(:) = zero
!--------------------------------------------------------------------
!     weibull distribution parameters on top and bottom surfaces
!--------------------------------------------------------------------
      if (ipt == npt) then           ! top surface - last integration point
        eta      = eta1
        beta_drb = beta1
        tau_drb  = tau1
        irand    = 0
      else ! if (ipt == 1) then        ! bottom surface - first integration point
        eta      = eta2
        beta_drb = beta2
        tau_drb  = tau2
        irand    = 3
      end if
      betai = one / beta_drb
!
      pf = -HUGE(pf)
      if (p_switch == 1) then
        p0 = zero
        pf = one - p_scale
      else if (p_switch == 0) then
        p0 = p_scale
        pf = one
      end if
!--------------------------------------------
!       calculation of p variable - Weibull distribution
!--------------------------------------------      
      do i=1,nel
        fsize = aldt(i)*aldt(i) / a_drb
        thkm  = thk(i)  * fac_lenm   !  initial shell thickness in micrometers
        aldtm = aldt(i) * fac_lenm   !  initial shell width in micrometers
        uvar(i,19) = thkm
        uvar(i,20) = aldtm
!        
        ib = indx(i)
        if (ib > 0) then ! .and.ib < brokmann%nelem) then
          randp = brokmann%brokmann_elem(ib)%random(irand + 1)
          pw = p0 + randp * (pf - p0)

!         calculate fracture stress value
          eta_drb = eta * fsize**(-betai)
          ff      = log(one - pw) - (tau_drb / eta_drb)**beta_drb
          sig_drb(i) = -eta_drb*sign(abs(ff)**betai,ff)   ! fracture stress    
        end if
      end do
!------------------------------------------
      do i=1,nel
!------------------------------------------
!       initial geometry correction factor (random value between yi1 and yi2
!
        ib = indx(i) 
        if (ib > 0) then ! .and.ib < brokmann%nelem) then
          py = brokmann%brokmann_elem(ib)%random(irand + 2)
!
          y0(i) = yi1 + (yi2-yi1)*py
!
          sig_mpa = sig_drb(i) * fac_mpa                
!
          if (sig_mpa <  sig0) then 
            ! valid for p(0.663;1.1) at 50 mpa
            p1 = 3.854846e-04*ns4 -2.565276e-02*ns3                       &
               + 6.142425e-01*ns2 -6.200668e+00*ns1 + 2.450664e+01         
            p2 = -1.368162e-03*ns4 + 9.175738e-02*ns3                     &
                - 2.220515e+00*ns2 + 2.272542e+01*ns1 -9.012056e+01        
            p3 = 1.787600e-03*ns4 -1.208241e-01*ns3                       &
                + 2.955620e+00*ns2 -3.070972e+01*ns1 + 1.231156e+02        
            p4 = -1.020375e-03*ns4 + 6.954743e-02*ns3                     &
                - 1.721442e+00*ns2 + 1.820610e+01*ns1 -7.466173e+01        
            p5 = 2.130927e-04*ns4 -1.464948e-02*ns3                       &
                + 3.670756e-01*ns2 -3.957401e+00*ns1 + 1.785698e+01       
          else if (sig_mpa >= sig0 .and. sig_mpa < sig1) then 
            ! valid for p(0.663;1.1) at 100 mpa
            p1 = -8.014637e-05*ns4 + 6.234394e-03*ns3                     &
               - 1.757171e-01*ns2 + 2.165919e+00*ns1 -7.843583e+00         
            p2 = 1.196065e-04*ns4 -9.673449e-03*ns3                       &
                + 2.757151e-01*ns2 -3.450433e+00*ns1 + 9.667971e+00        
            p3 = -3.978835e-05*ns4 + 3.830078e-03*ns3                     &
                - 1.122047e-01*ns2 + 1.423220e+00*ns1 + 1.058530e+00       
            p4 = 2.550177e-06*ns4 -6.309084e-04*ns3                       &
                + 1.772278e-02*ns2 -1.612462e-01*ns1 -4.326689e+00         
            p5 = -1.793787e-05*ns4 + 1.431770e-03*ns3                     &
                - 3.888580e-02*ns2 + 4.324152e-01*ns1 + 5.858975e-01       
          else if (sig_mpa >= sig1 .and. sig_mpa < sig2) then               
            ! valid for p(0.663;1.1) at 150 mpa                            
            p1 = -2.446033e-04*ns4 + 1.828184e-02*ns3                     &
               -5.015157e-01*ns2 + 6.034614e+00*ns1 -2.490241e+01          
            p2 = 5.067851e-04*ns4 -3.734857e-02*ns3                       &
               + 1.003949e+00*ns2 -1.185853e+01*ns1 + 4.575989e+01         
            p3 = -3.258463e-04*ns4 + 2.332497e-02*ns3                     &
               -5.974454e-01*ns2 + 6.706583e+00*ns1 -2.038952e+01          
            p4 = 7.386059e-05*ns4 -4.882414e-03*ns3                       &
               + 1.054173e-01*ns2 -9.057073e-01*ns1 -2.098349e+00          
            p5 = -3.790448e-05*ns4 + 2.752424e-03*ns3                     &
               -7.083969e-02*ns2 + 7.759309e-01*ns1 -8.351173e-01          
          else if (sig_mpa >= sig2 .and. sig_mpa < sig3) then              
            ! valid for p(0.663;1.1) at 200 mpa                            
            p1 = -1.002426e-03*ns4 + 7.487536e-02*ns3                     &
               -2.056697e+00*ns2 + 2.464744e+01*ns1 -1.066757e+02          
            p2 = 3.118627e-03*ns4 -2.316144e-01*ns3                       &
               + 6.316496e+00*ns2 -7.506691e+01*ns1 + 3.214670e+02         
            p3 = -3.609067e-03*ns4 + 2.667937e-01*ns3                     &
               -7.231503e+00*ns2 + 8.529346e+01*ns1 -3.613532e+02          
            p4 = 1.870434e-03*ns4 -1.378567e-01*ns3                       &
               + 3.720583e+00*ns2 -4.361605e+01*ns1 + 1.826124e+02         
            p5 = -4.094512e-04*ns4 + 3.029387e-02*ns3                     &
               -8.210301e-01*ns2 + 9.660467e+00*ns1 -3.938020e+01          
          else if (sig_mpa >= sig3 .and. sig_mpa < sig4) then              
            ! valid for p(0.663;1.1) at 250 mpa                            
            p1 = -1.661365e-03*ns4 + 1.251400e-01*ns3                     &
               -3.471542e+00*ns2 + 4.204654e+01*ns1 -1.854847e+02          
            p2 = 5.550941e-03*ns4 -4.160687e-01*ns3                       &
               + 1.147311e+01*ns2 -1.379743e+02*ns1 + 6.037335e+02         
            p3 = -6.851821e-03*ns4 + 5.116860e-01*ns3                     &
               -1.404451e+01*ns2 + 1.679347e+02*ns1 -7.296794e+02          
            p4 = 3.731562e-03*ns4 -2.780340e-01*ns3                       &
               + 7.608298e+00*ns2 -9.060341e+01*ns1 + 3.911483e+02         
            p5 = -8.004821e-04*ns4 + 5.976458e-02*ns3                     &
               -1.639088e+00*ns2 + 1.955932e+01*ns1 -8.338423e+01          
          else if (sig_mpa >= sig4 .and. sig_mpa < sig5) then              
            ! valid for p(0.663;1.1) at 300 mpa                            
            p1 = -1.929460e-03*ns4 + 1.471697e-01*ns3                     &
               -4.141752e+00*ns2 + 5.098240e+01*ns1 -2.294842e+02          
            p2 = 6.697166e-03*ns4 -5.079334e-01*ns3                       &
               + 1.419800e+01*ns2 -1.733844e+02*ns1 + 7.736174e+02         
            p3 = -8.537941e-03*ns4 + 6.448618e-01*ns3                     &
               -1.793464e+01*ns2 + 2.176802e+02*ns1 -9.643518e+02          
            p4 = 4.765638e-03*ns4 -3.590089e-01*ns3                       &
               + 9.951988e+00*ns2 -1.202820e+02*ns1 + 5.297103e+02         
            p5 = -1.023363e-03*ns4 + 7.720270e-02*ns3                     &
               -2.143459e+00*ns2 + 2.594356e+01*ns1 -1.131898e+02          
          else if (sig_mpa >= sig5 .and. sig_mpa < sig6) then              
            ! valid for p(0.663;1.1) at 350 mpa                            
            p1 = -1.839328e-03*ns4 + 1.429385e-01*ns3                     &
               -4.107221e+00*ns2 + 5.173982e+01*ns1 -2.389880e+02          
            p2 = 6.608847e-03*ns4 -5.095222e-01*ns3                       &
               + 1.450735e+01*ns2 -1.808588e+02*ns1 + 8.261478e+02         
            p3 = -8.661755e-03*ns4 + 6.640298e-01*ns3                     &
               - 1.878160e+01*ns2 + 2.323316e+02*ns1 -1.052092e+03         
            p4 = 4.935803e-03*ns4 -3.770376e-01*ns3                       &
               + 1.061849e+01*ns2 -1.306599e+02*ns1 + 5.876649e+02         
            p5 = -1.067185e-03*ns4 + 8.159522e-02*ns3                     &
               - 2.300259e+00*ns2 + 2.832827e+01*ns1 -1.262924e+02         
          else ! if (sig_mpa >= sig6) then                                 
            ! valid for p(0.663;1.1) at 400 mpa                            
            p1 = -1.487196e-03*ns4 + 1.192492e-01*ns3                     &
               -3.542728e+00*ns2 + 4.624246e+01*ns1 -2.215987e+02          
            p2 = 5.590149e-03*ns4 -4.422194e-01*ns3                       &
               + 1.294667e+01*ns2 -1.663471e+02*ns1 + 7.845554e+02         
            p3 = -7.575999e-03*ns4 + 5.938425e-01*ns3                     &
               -1.720949e+01*ns2 + 2.186274e+02*ns1 -1.018855e+03          
            p4 = 4.421525e-03*ns4 -3.445888e-01*ns3                       &
               + 9.921046e+00*ns2 -1.250843e+02*ns1 + 5.777186e+02         
            p5 = -9.673316e-04*ns4 + 7.539378e-02*ns3                     &
               -2.170851e+00*ns2 + 2.736578e+01*ns1 -1.251497e+02      
          end if 
!      
          y_shift = p1 * y0(i)**4 + p2 * y0(i)**3 + p3 * y0(i)**2 + P4 * y0(I) + p5
          y_drb   = y0(i) * y_shift     ! geometry correction factor 
!
          af  = (kcm / (y_drb*sig_drb(i)))**2 / pi
          afp = af**m1
          al  = (facn * vm * sig_mpa / dsig_drb + af)**m2
          cr_len(i) = afp / al
          cr_len(i) = cr_len(i) * ep06 !  crack length in micrometers
!
          phi = brokmann%brokmann_elem(ib)%random(irand + 3)
          cr_ang(i) = phi * pi         !  random crack angle (0 - Pi)
        end if ! ib > 0
      enddo    ! nel
!-----------------------------------
!     crack depth calculation
!-----------------------------------
      do i=1,nel
        crlen = cr_len(i)
        if (crlen > zero) then
          iter  = 0
          yiter = zero
          cr_depth(i) = crlen*1.1
          thkm  = uvar(i,19)
          aldtm = uvar(i,20)
!        
          do while (delta_y >= ytol)
            iter = iter + 1
            call newman_raju(cr_depth(i),crlen,thkm,aldtm,fac_pi2,yiter)

            delta_y = abs(one-yiter/y0(i))

            if (yiter < y0(i)) then
              cr_depth(i) = cr_depth(i)*(one+delta_y)
            elseif (yiter > y0(i)) then
              cr_depth(i) = cr_depth(i)*(one-delta_y)
            end if

            if (cr_depth(i) < crlen) then
              cr_depth(i) = crlen*1.01
            end if

            if (iter >= 100) exit
          end do
          cr_depth(i) = max(cr_depth(i) ,depth_max)                   
!
        else
          cr_depth(i) = zero          
        end if ! cr_len > 0 
      enddo
!
      uvar(1:nel,15) = zero                ! brokmann model flag
      uvar(1:nel,16) = cr_len(1:nel)
      uvar(1:nel,17) = cr_depth(1:nel)
      uvar(1:nel,18) = cr_ang(1:nel)

      if (idebug == 1) then
        do i = 1,nel
          if (cr_len(i) > zero) print*,'i,len,angle=',i,ngl(i),cr_len(i),cr_ang(i)
        end do
      end if
!-----------
      return
      end subroutine brokmann_crack_init
!-----------
      end module brokmann_crack_init_mod
