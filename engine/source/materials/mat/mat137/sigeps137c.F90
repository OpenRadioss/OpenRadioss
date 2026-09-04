!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
      module sigeps137c_mod
      contains
      subroutine sigeps137c(                                                   &
        nel      ,matparam ,rho      ,nvartmp  ,vartmp   ,temp     ,           &
        epsxx    ,epsyy    ,epsxy    ,epsyz    ,epszx    ,                     &
        depsxx   ,depsyy   ,depsxy   ,depsyz   ,depszx   ,                     &
        sigoxx   ,sigoyy   ,signxx   ,signyy   ,signxy   ,signyz   ,signzx   , &
        soundsp  ,off      ,pla      ,dpla     ,seq      ,et       ,           &
        sigy     ,nuvar    ,uvar     ,l_sigb   ,sigb     ,time     ,           &
        forth    ,eintth   ,temp_init,shf      ,thk0     ,thkly    ,dt1      , &
        eint     ,area     ,thk      ,ipt      ,thklyl   )
!----------------------------------------------------------------
!   M o d u l e s
!----------------------------------------------------------------
        use matparam_def_mod
        use constant_mod
        use precision_mod, only : WP
        use table_mat_vinterp_mod
!----------------------------------------------------------------
!   I m p l i c i t   T y p e s
!----------------------------------------------------------------
        implicit none
!----------------------------------------------------------------
!  I n p u t   A r g u m e n t s
!----------------------------------------------------------------
        integer,                       intent(in)    :: nel        !< Number of elements in the group
        type(matparam_struct_),        intent(in)    :: matparam   !< Material parameters data
        real(kind=WP), dimension(nel), intent(in)    :: rho        !< Density at current time
        integer,                       intent(in)    :: nvartmp    !< Number of variables used in tabulated variables
        integer, dimension(nel,nvartmp), intent(inout) :: vartmp   !< Temporary variables for tabulated hardening
        real(kind=WP), dimension(nel), intent(inout) :: temp       !< Temperature
        real(kind=WP), dimension(nel), intent(inout) :: epsxx      !< Mechanical strain tensor xx
        real(kind=WP), dimension(nel), intent(inout) :: epsyy      !< Mechanical strain tensor yy
        real(kind=WP), dimension(nel), intent(inout) :: epsxy      !< Mechanical strain tensor xy
        real(kind=WP), dimension(nel), intent(inout) :: epsyz      !< Mechanical strain tensor yz
        real(kind=WP), dimension(nel), intent(inout) :: epszx      !< Mechanical strain tensor zx
        real(kind=WP), dimension(nel), intent(inout) :: depsxx     !< Mechanical strain increment xx (full cycle)
        real(kind=WP), dimension(nel), intent(inout) :: depsyy     !< Mechanical strain increment yy (full cycle)
        real(kind=WP), dimension(nel), intent(in)    :: depsxy     !< Mechanical strain increment xy (full cycle)
        real(kind=WP), dimension(nel), intent(in)    :: depsyz     !< Mechanical strain increment yz (full cycle)
        real(kind=WP), dimension(nel), intent(in)    :: depszx     !< Mechanical strain increment zx (full cycle)
        real(kind=WP), dimension(nel), intent(in)    :: sigoxx     !< Old stress xx
        real(kind=WP), dimension(nel), intent(in)    :: sigoyy     !< Old stress yy
        real(kind=WP), dimension(nel), intent(inout) :: signxx     !< Current stress xx
        real(kind=WP), dimension(nel), intent(inout) :: signyy     !< Current stress yy
        real(kind=WP), dimension(nel), intent(inout) :: signxy     !< Current stress xy
        real(kind=WP), dimension(nel), intent(inout) :: signyz     !< Current stress yz
        real(kind=WP), dimension(nel), intent(inout) :: signzx     !< Current stress zx
        real(kind=WP), dimension(nel), intent(inout) :: soundsp    !< Current sound speed
        real(kind=WP), dimension(nel), intent(inout) :: off        !< Element failure flag
        real(kind=WP), dimension(nel), intent(inout) :: pla        !< Accumulated plastic strain
        real(kind=WP), dimension(nel), intent(inout) :: dpla       !< Plastic strain increment
        real(kind=WP), dimension(nel), intent(inout) :: seq        !< Equivalent stress
        real(kind=WP), dimension(nel), intent(inout) :: et         !< Hourglass stabilization variable
        real(kind=WP), dimension(nel), intent(inout) :: sigy       !< Current yield stress
        integer,                       intent(in)    :: nuvar      !< Number of user variables
        real(kind=WP), dimension(nel,nuvar), intent(inout) :: uvar !< User variables
        integer,                       intent(in)    :: l_sigb     !< Size of backstress array
        real(kind=WP),dimension(nel,l_sigb),intent(inout) :: sigb  !< Backstress components for kinematic hardening
        real(kind=WP),                 intent(in)    :: time       !< Current time
        real(kind=WP), dimension(nel), intent(inout) :: forth      !< Total thermal strain (element level)
        real(kind=WP), dimension(nel), intent(inout) :: eintth     !< Thermal energy (element level)
        real(kind=WP), dimension(nel), intent(in)    :: temp_init  !< Initial temperature on the time step
        real(kind=WP), dimension(nel), intent(in)    :: shf        !< Shear correction factor
        real(kind=WP), dimension(nel), intent(inout) :: thk0       !< Initial element thickness
        real(kind=WP), dimension(nel), intent(in)    :: thkly      !< Integration point layer weight
        real(kind=WP),                 intent(in)    :: dt1        !< Time step size
        real(kind=WP), dimension(nel,2), intent(inout) :: eint     !< Internal energy
        real(kind=WP), dimension(nel), intent(in)    :: area       !< Element area
        real(kind=WP), dimension(nel), intent(inout) :: thk        !< Element current thickness
        integer,                       intent(in)    :: ipt        !< Integration point number
        real(kind=WP), dimension(nel), intent(in)    :: thklyl     !< Integration point layer weight for the current integration point
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        integer :: i,ii,iter(nel),nindx,indx(nel),nindx_1,indx_1(nel),         &
          ipos(nel,2),vartmp_i(nel,1:2)
        real(kind=WP), dimension(nel) :: pla0,normxx,normyy,normzz,normxy,     &
          normyz,normzx,phi,young,dlam,dsigxx_dlam,dsigyy_dlam,                &
          dsigzz_dlam,dsigxy_dlam,dsigyz_dlam,dsigzx_dlam,dseq_dlam,dpla_dlam, &
          dsigy_dlam,dphi_dseq,dphi_dsigy,dphi_dlam,sig_dseqdsig,c11,c12,c44,  &
          c55,dyoung,nu,dnu,p,sxx,syy,szz,sxy,syz,szx,inv_seq,dsigy,hr,        &
          dhr,re,tempfactor,temp0,temp_max,gamma,alpha,dalpha,                 &
          eth,sigkk,sigkkn,phasevar,sigy0,sigy_i,hr_i,cxx,cyy,cxy,cyz,czx,     &
          sigoxx_sub,sigoyy_sub,sigoxy_sub,sigoyz_sub,sigozx_sub,s13,          &
          ezz,epspxx,epspyy,epspzz,epspxy,epspyz,epspzx
        real(kind=WP) :: tastrt,taend,tlstrt,tlend,eghost,nughost,             &
          aghost,beta,factor,dtemp,tcutoff,t1phase,t2phase
        real(kind=WP), dimension(nel,2) :: xvec,xvec2
        real(kind=WP), dimension(:,:), allocatable :: dsigb_dlam
        logical :: converged
        !< Thermal + mechanical co-substepping (see DTEMP, matparam%uparam(12))
        integer :: nsub_max,isub
        real(kind=WP), dimension(nel) :: temp_start,dtemp_full,deps_sub_xx,    &
          deps_sub_yy,deps_sub_xy,deps_sub_yz,deps_sub_zx
        real(kind=WP) :: frac
        !< Increment of thermal energy
        real(kind=WP), dimension(nel) :: deintth
!
        integer, parameter :: nitermax = 20            !< Maximum number of plastic iterations
        real(kind=WP), parameter :: tol = 1.0d-6       !< Tolerance for plasticity convergence
        logical, dimension(nel) :: active_elements_mask
        integer, dimension(nel) :: temp_all_indices
!===============================================================================
!
        !< Allocate large arrays if not already allocated
        if (.not. allocated(dsigb_dlam)) allocate(dsigb_dlam(nel,l_sigb))
!
        !=======================================================================
        !< - Initialisation of computation on time step
        !=======================================================================
        !< Recover material parameters
        beta    = matparam%uparam(1)   !< Kinematic/Isotropic hardening factor  
        tastrt  = matparam%uparam(2)   !< Start of annealing temperature  
        taend   = matparam%uparam(3)   !< End of annealing temperature  
        tlstrt  = matparam%uparam(4)   !< Start of birth material temperature
        tlend   = matparam%uparam(5)   !< End of birth material temperature  
        eghost  = matparam%uparam(6)   !< Young modulus for ghost material  
        nughost = matparam%uparam(7)   !< Poisson ratio for ghost material  
        aghost  = matparam%uparam(8)   !< Coefficient for ghost material
        t1phase = matparam%uparam(10)  !< Phase change temperature 1 (transformation end) 
        t2phase = matparam%uparam(11)  !< Phase change temperature 2 (transformation start)
        dtemp   = matparam%uparam(12)  !< Maximum temperature increment for the time step
        tcutoff = matparam%uparam(13)  !< Cutoff temperature
!
        !< Initialisation of the hourglass control variable
        et(1:nel) = one
        !< Increment of cumulated plastic strain
        dpla(1:nel) = zero
        !< Update element status flag
        where (off(1:nel) < em01)
          off(1:nel) = zero
        end where
        where (off(1:nel) < one)
          off(1:nel) = off(1:nel)*four_over_5
        end where
        !< Recover plastic strain tensor
        epspxx(1:nel)   = uvar(1:nel,1)
        epspyy(1:nel)   = uvar(1:nel,2)
        epspzz(1:nel)   = uvar(1:nel,3)
        epspxy(1:nel)   = uvar(1:nel,4)
        temp_max(1:nel) = uvar(1:nel,5)
        temp0(1:nel)    = temp_init(1:nel)
        !< Re-initialize the thickness
        if (ipt == 1) thk(1:nel) = zero
!
        !=======================================================================
        !< - Strip the historical thermal strain out of the incoming total
        !    strain tensor
        !=======================================================================
        epsxx(1:nel) = epsxx(1:nel) - forth(1:nel)
        epsyy(1:nel) = epsyy(1:nel) - forth(1:nel)
!
        !=======================================================================
        !< - Setup for thermal + mechanical co-substepping
        !=======================================================================
        !< Evaluate the number of substeps
        temp_start(1:nel) = temp0(1:nel)
        dtemp_full(1:nel) = temp(1:nel) - temp_start(1:nel)
        if (dtemp > 0) then
          nsub_max = max(1,maxval(ceiling(abs(dtemp_full(1:nel))/dtemp)))
          nsub_max = min(nsub_max,1000)
        else
          nsub_max = 1
        endif
        frac = one/real(nsub_max,WP)
        !< Compute the mechanical strain total + increment for each substep
        epsxx(1:nel) = epsxx(1:nel) - depsxx(1:nel)
        epsyy(1:nel) = epsyy(1:nel) - depsyy(1:nel)
        epsxy(1:nel) = epsxy(1:nel) - depsxy(1:nel)
        epsyz(1:nel) = epsyz(1:nel) - depsyz(1:nel)
        epszx(1:nel) = epszx(1:nel) - depszx(1:nel)
        deps_sub_xx(1:nel) = depsxx(1:nel)*frac
        deps_sub_yy(1:nel) = depsyy(1:nel)*frac
        deps_sub_xy(1:nel) = depsxy(1:nel)*frac
        deps_sub_yz(1:nel) = depsyz(1:nel)*frac
        deps_sub_zx(1:nel) = depszx(1:nel)*frac
!
        !=======================================================================
        !< - Loop over substeps
        !=======================================================================        
        do isub = 1,nsub_max
!
          !< Ramp temperature in lockstep across substeps
          temp(1:nel) = temp_start(1:nel) + real(isub,WP)*frac*dtemp_full(1:nel)
          temp_max(1:nel) = max(temp_max(1:nel),temp(1:nel))
!
          !< Birth material factor
          if (abs(tlend - tlstrt) > em20) then
            gamma(1:nel) = min(one, max(zero, temp_max(1:nel)-tlstrt)/         &
                                                       (tlend-tlstrt))
          else
            gamma(1:nel) = one
          endif
!
          !< Save the initial cumulated plastic strain value
          pla0(1:nel) = pla(1:nel)
!
          !< Update the mechanical strain tensor for this substep
          epsxx(1:nel) = epsxx(1:nel) + deps_sub_xx(1:nel)
          epsyy(1:nel) = epsyy(1:nel) + deps_sub_yy(1:nel)
          epsxy(1:nel) = epsxy(1:nel) + deps_sub_xy(1:nel)
          epsyz(1:nel) = epsyz(1:nel) + deps_sub_yz(1:nel)
          epszx(1:nel) = epszx(1:nel) + deps_sub_zx(1:nel)
!
          !=====================================================================
          !< - Computation of temperature dependent material properties
          !=====================================================================
          !< Compute the elastic constants for each element
          xvec(1:nel,1) = temp(1:nel)
          xvec(1:nel,2) = temp_max(1:nel)
          ! -> Young modulus tabulated function
          call table_mat_vinterp(matparam%table(1),nel,nel,vartmp(1:nel,1),    &
            xvec(1:nel,1),young(1:nel),dyoung(1:nel),opt_extrapolate=.false.)
          ! -> Poisson ratio tabulated function
          call table_mat_vinterp(matparam%table(2),nel,nel,vartmp(1:nel,2),    &
            xvec(1:nel,1),nu(1:nel),dnu(1:nel),opt_extrapolate=.false.)
          ! -> Thermal expansion coefficient tabulated function
          xvec(1:nel,1) = min(temp(1:nel), tcutoff)
          if (matparam%table(3)%notable > 0) then 
            call table_mat_vinterp(matparam%table(3),nel,nel,vartmp(1:nel,3),  &
              xvec(1:nel,1),alpha(1:nel),dalpha(1:nel),opt_extrapolate=.false.)
          else
            alpha(1:nel)  = zero
            dalpha(1:nel) = zero
          endif
          ! -> Compute the elastic constants for each element with birth material 
          !    factor
          young(1:nel) = young(1:nel)*gamma(1:nel) + (one - gamma(1:nel))*eghost
          nu(1:nel) = nu(1:nel)*gamma(1:nel) + (one - gamma(1:nel))*nughost
          ! -> Compute the thermal expansion coefficient for each element with 
          !    birth material factor
          alpha(1:nel) = alpha(1:nel)*gamma(1:nel) + (one - gamma(1:nel))*aghost
          !< Plane stress reduced elastic constants (no elastic stiffness on the
          !  through-thickness direction: c13=c23=c33=0, hence dsigzz_dlam=0)
          c11(1:nel) = young(1:nel)/(one - nu(1:nel)*nu(1:nel))
          c12(1:nel) = nu(1:nel)*c11(1:nel)
          c44(1:nel) = young(1:nel)/(two*(one + nu(1:nel)))
          c55(1:nel) = c44(1:nel)*shf(1:nel)
          !< Compute the sound speed for each element
          soundsp(1:nel) = sqrt((matparam%young/(one - matparam%nu*matparam%nu)&
                                                                  /rho(1:nel)))
!
          !=====================================================================
          !< - Computation of thermal strain and thermal-only stress trace
          !=====================================================================
          ! -> Thermal strain increment for this substep
          eth(1:nel)    = alpha(1:nel)*(min( temp(1:nel),tcutoff) -            &
                                        min(temp0(1:nel),tcutoff))*off(1:nel)
          ! -> Total thermal strain (running total across substeps and cycles;
          !    amu is only updated ONCE, after the substep loop, see below)
          forth(1:nel)  = forth(1:nel)  + eth(1:nel)
          ! -> Update the mechanic strain tensor and the volumetric strain
          epsxx(1:nel)  = epsxx(1:nel)  - eth(1:nel)
          epsyy(1:nel)  = epsyy(1:nel)  - eth(1:nel)
          ! -> Update the mechanical strain increment
          depsxx(1:nel) = depsxx(1:nel) - eth(1:nel)
          depsyy(1:nel) = depsyy(1:nel) - eth(1:nel)
          ! -> Stress trace
          if (isub == 1) then
            sigkk(1:nel) = sigoxx(1:nel) + sigoyy(1:nel)
            sigoxx_sub(1:nel) = sigoxx(1:nel)
            sigoyy_sub(1:nel) = sigoyy(1:nel)
          else
            sigkk(1:nel) = signxx(1:nel) + signyy(1:nel)
            sigoxx_sub(1:nel) = signxx(1:nel)
            sigoyy_sub(1:nel) = signyy(1:nel)
          endif
!
          !=====================================================================
          !< - Computation of the elastic trial stress tensor
          !=====================================================================
          signxx(1:nel) = c11(1:nel)*(epsxx(1:nel) - epspxx(1:nel)) +          &
                          c12(1:nel)*(epsyy(1:nel) - epspyy(1:nel))
          signyy(1:nel) = c12(1:nel)*(epsxx(1:nel) - epspxx(1:nel)) +          &
                          c11(1:nel)*(epsyy(1:nel) - epspyy(1:nel))
          signxy(1:nel) = c44(1:nel)*(epsxy(1:nel) - epspxy(1:nel))
          signyz(1:nel) = c55(1:nel)*epsyz(1:nel)
          signzx(1:nel) = c55(1:nel)*epszx(1:nel)
!
          !=====================================================================
          !< - Computation of the yield stress
          !=====================================================================
          !< Tabulated initial yield stress and linear hardening modulus vs temp
          if (matparam%table(4)%ndim == 1) then 
            xvec(1:nel,1) = temp(1:nel)
            if (matparam%table(4)%notable > 0) then
              call table_mat_vinterp(matparam%table(4),nel,nel,vartmp(1:nel,5),&
                xvec(1:nel,1),re(1:nel),dsigy(1:nel))   
            else
              re(1:nel)  = infinity
            endif
            xvec(1:nel,1) = pla(1:nel)
            if (matparam%table(5)%notable > 0) then
              call table_mat_vinterp(matparam%table(5),nel,nel,vartmp(1:nel,6),&
                xvec(1:nel,1),hr(1:nel),dhr(1:nel))
            else
              hr(1:nel)  = zero
              dhr(1:nel) = zero
            endif
            sigy(1:nel) = re(1:nel) + beta*hr(1:nel)*pla(1:nel)
          !< Tabulated non-linear yield stress vs plastic strain vs temperature
          elseif (matparam%table(4)%ndim == 2) then
            xvec2(1:nel,1) = zero
            xvec2(1:nel,2) = temp(1:nel)
            if (matparam%table(4)%notable > 0) then
              ipos(1:nel,1:2) = 1
              call table_mat_vinterp(matparam%table(4),nel,nel,ipos(1:nel,1:2),&
                xvec2(1:nel,1:2),sigy0(1:nel),hr(1:nel),opt_extrapolate=.false.)
              xvec2(1:nel,1) = pla(1:nel)
              call table_mat_vinterp(matparam%table(4),nel,nel,vartmp(1:nel,5:6),&
                xvec2(1:nel,1:2),sigy(1:nel),hr(1:nel),opt_extrapolate=.false.)
            else
              sigy(1:nel)  = infinity
              sigy0(1:nel) = infinity
              hr(1:nel)    = zero
            endif
            sigy(1:nel) = beta*sigy(1:nel) + (one - beta)*sigy0(1:nel)
          endif
!
          !=====================================================================
          !< - Add backstress contribution to stress tensor
          !=====================================================================
          signxx(1:nel) = signxx(1:nel) - (sigb(1:nel,1) - sigb(1:nel,3))
          signyy(1:nel) = signyy(1:nel) - (sigb(1:nel,2) - sigb(1:nel,3))
          signxy(1:nel) = signxy(1:nel) -  sigb(1:nel,4)
!
          !=====================================================================
          !< - Computation of the trial equivalent stress and its 1st derivative
          !=====================================================================
          seq(1:nel) = signxx(1:nel)**2 + signyy(1:nel)**2 -                   &
                       signxx(1:nel)*signyy(1:nel) + three*(signxy(1:nel)**2)
          seq(1:nel) = sqrt(seq(1:nel))
!
          !=====================================================================
          !< - Computation of the trial yield function and count yielding elem.
          !=====================================================================
          phi(1:nel) = (seq(1:nel) / sigy(1:nel))**2 - one
          active_elements_mask(1:nel) = (phi(1:nel) >= zero.and.off(1:nel) == one)
          nindx = COUNT(active_elements_mask(1:nel))
          temp_all_indices(1:nel) = [(i, i=1,nel)]
!
          !=====================================================================
          !< - Return mapping algorithm using Cutting Plane Method (C.P.M)
          !=====================================================================
          if (nindx > 0) then
!
            ! Note : in this part, the purpose is to compute for each iteration
            ! a plastic multiplier (lambda) allowing to update internal
            ! variables to satisfy the consistency condition using the cutting
            ! plane algorithm.
            ! Its expression at each iteration is: dlam = - phi/dphi_dlam
            ! -> phi       : current value of yield function (known)
            ! -> dphi_dlam : derivative of phi with respect to dlam by taking
            !                into account of internal variables kinetic:
            !                plasticity, strain-rate ... (to be computed)
!  
            !< Extract the indices of the yielding elements
            indx(1:nindx) = PACK(temp_all_indices(1:nel),active_elements_mask(1:nel))
!  
            !< Initialisation of the iteration counter and convergence flag
            iter(1:nel) = 0
            converged = .false.
!
            !< Loop over yielding elements
            do while (.not. converged) 
!
              !< Loop over yielding elements
#include "vectorize.inc"
              do ii = 1,nindx
                i = indx(ii)
!
                !< 1 - Derivative of equivalent stress sigeq w.r.t stress tensor
                !< -------------------------------------------------------------
                inv_seq(ii) = one / max(seq(i), em20)
                normxx(ii) =  half*inv_seq(ii)*(two*signxx(i) - signyy(i))
                normyy(ii) =  half*inv_seq(ii)*(two*signyy(i) - signxx(i))
                normzz(ii) = - normxx(ii) - normyy(ii)
                normxy(ii) = three*inv_seq(ii)*signxy(i)
!  
                !< 2 - Derivative of eq. plastic strain w.r.t plastic multiplier
                !< -------------------------------------------------------------
                sig_dseqdsig(ii) = signxx(i)*normxx(ii) +                      &
                                   signyy(i)*normyy(ii) +                      &
                                   signxy(i)*normxy(ii)
                dpla_dlam(ii) = sig_dseqdsig(ii)/max(sigy(i),em20)
!  
                !< 3 - Derivative of equivalent stress sigeq w.r.t lambda
                !< -------------------------------------------------------------
                dsigxx_dlam(ii) = -(c11(i)*normxx(ii) + c12(i)*normyy(ii))
                dsigyy_dlam(ii) = -(c12(i)*normxx(ii) + c11(i)*normyy(ii))
                dsigzz_dlam(ii) = zero
                dsigxy_dlam(ii) = -(c44(i)*normxy(ii))          
!  
                !<  b) Derivatives of backstress tensor w.r.t lambda
                !<  ------------------------------------------------------------
                factor = (one - beta)*dpla_dlam(ii)*hr(i)
                dsigb_dlam(ii,1) = factor*(two_third*signxx(i) -               &
                                           third*signyy(i))/max(seq(i),em20)
                dsigb_dlam(ii,2) = factor*(two_third*signyy(i) -               &
                                           third*signxx(i))/max(seq(i),em20)
                dsigb_dlam(ii,3) = factor*(-third*signxx(i) -                  &
                                            third*signyy(i))/max(seq(i),em20)
                dsigb_dlam(ii,4) = factor*signxy(i)/max(seq(i),em20)
!  
                !<  c) Assembling derivative of eq. stress sigeq w.r.t lambda
                !<  ------------------------------------------------------------
                dseq_dlam(ii) = normxx(ii)*(dsigxx_dlam(ii)-dsigb_dlam(ii,1)) +&
                                normyy(ii)*(dsigyy_dlam(ii)-dsigb_dlam(ii,2)) +&
                                normzz(ii)*(dsigzz_dlam(ii)-dsigb_dlam(ii,3)) +&
                                normxy(ii)*(dsigxy_dlam(ii)-dsigb_dlam(ii,4))
!  
                !< 4 - Derivative of yield stress ystrs w.r.t lambda
                !< -------------------------------------------------------------
!  
                !<  a) Assembling derivative of ystrs w.r.t lambda
                !<  ------------------------------------------------------------
                dsigy_dlam(ii) = beta*hr(i)*dpla_dlam(ii)
!  
                !< 5 - Assembling the derivative of phi w.r.t lambda
                !< -------------------------------------------------------------
!  
                !<  a) Derivative of phi w.r.t eq. stress sigeq
                !<  ------------------------------------------------------------
                dphi_dseq(ii)  =  two*seq(i)/(sigy(i)**2)
!  
                !<  b) Derivative of phi w.r.t yield stress ystrs
                !<  ------------------------------------------------------------
                dphi_dsigy(ii) = -two*(seq(i)**2)/(sigy(i)**3)
!  
                !<  c) Derivative of phi w.r.t lambda
                !<  ------------------------------------------------------------
                dphi_dlam(ii) = dphi_dseq(ii)*dseq_dlam(ii) +                  &
                                dphi_dsigy(ii)*dsigy_dlam(ii)
                dphi_dlam(ii) = sign(max(abs(dphi_dlam(ii)),em20),dphi_dlam(ii))
!  
                !< 6 - Computation of plastic multiplier and variables update
                !< -------------------------------------------------------------
!  
                !<  a) Computation of the plastic multiplier increment dlam
                !<  ------------------------------------------------------------
                dlam(ii) = -phi(i)/dphi_dlam(ii)
!  
                !<  b) Stress tensor update
                !<  ------------------------------------------------------------
                signxx(i) = signxx(i) + (dsigxx_dlam(ii)-dsigb_dlam(ii,1))*dlam(ii)
                signyy(i) = signyy(i) + (dsigyy_dlam(ii)-dsigb_dlam(ii,2))*dlam(ii)
                signxy(i) = signxy(i) + (dsigxy_dlam(ii)-dsigb_dlam(ii,4))*dlam(ii)
!  
                !<  c) Update the plastic strain related variables
                !<  ------------------------------------------------------------
                !< Equivalent plastic strain increment
                dpla(i) = max(dpla(i) + dpla_dlam(ii)*dlam(ii),zero)
                !< Equivalent plastic strain
                pla(i)  = pla0(i) + dpla(i)
                !< Plastic strain tensor update
                epspxx(i) = epspxx(i) + dlam(ii)*normxx(ii)
                epspyy(i) = epspyy(i) + dlam(ii)*normyy(ii)
                epspzz(i) = epspzz(i) + dlam(ii)*normzz(ii)
                epspxy(i) = epspxy(i) + dlam(ii)*normxy(ii)
!  
                !<  d) Yield stress update
                !<  ------------------------------------------------------------
                if (matparam%table(4)%ndim == 1) then
                  sigy(i) = re(i) + beta*hr(i)*pla(i)
                endif
!  
                !<  e) Backstress tensor update
                !<  ------------------------------------------------------------
                sigb(i,1) = sigb(i,1) + dsigb_dlam(ii,1)*dlam(ii)
                sigb(i,2) = sigb(i,2) + dsigb_dlam(ii,2)*dlam(ii)
                sigb(i,3) = sigb(i,3) + dsigb_dlam(ii,3)*dlam(ii)
                sigb(i,4) = sigb(i,4) + dsigb_dlam(ii,4)*dlam(ii)
!  
                !<  f) Equivalent stress update
                !<  ------------------------------------------------------------
                seq(1:nel) = signxx(1:nel)**2 + signyy(1:nel)**2 -             &
                       signxx(1:nel)*signyy(1:nel) + three*(signxy(1:nel)**2)
                seq(1:nel) = sqrt(seq(1:nel))
!  
              enddo  
!
              !< g) Update the yield stress and hardening modulus for the next 
              !     iteration
              !  ---------------------------------------------------------------
              if (matparam%table(4)%ndim == 2) then
#include "vectorize.inc"
                do ii = 1,nindx
                  i = indx(ii) 
                  vartmp_i(ii,1:2) = vartmp(i,5:6)
                  xvec2(ii,1) = pla(i)
                  xvec2(ii,2) = temp(i)
                enddo
                if (matparam%table(4)%notable > 0) then
                  call table_mat_vinterp(matparam%table(4),nindx,nindx,        &
                    vartmp_i(1:nindx,1:2),xvec2(1:nindx,1:2),sigy_i(1:nindx),  &
                    hr_i(1:nindx),opt_extrapolate=.false.)
                else
                  sigy_i(1:nindx) = infinity
                  hr_i(1:nindx)   = zero
                endif
#include "vectorize.inc"
                do ii = 1,nindx
                  i = indx(ii)  
                  vartmp(i,5:6) = vartmp_i(ii,1:2)
                  sigy(i) = beta*sigy_i(ii) + (one - beta)*sigy0(i)
                  hr(i) = hr_i(ii)
                enddo
              endif
!
              !< h) Update the yield function and iteration number for the next 
              !     iteration
              !  ---------------------------------------------------------------
#include "vectorize.inc"
              do ii = 1,nindx
                i = indx(ii)
                !< Yield function
                phi(i) = (seq(i)/sigy(i))**2 - one
                !< Iteration number
                iter(i) = iter(i) + 1
              enddo
!
              !< i) Update the list of yielding elements 
              !<  --------------------------------------------------------------
              nindx_1 = 0
              do ii = 1, nindx
                i = indx(ii)  
                if ((abs(phi(i)) >= tol).and.(iter(i) < nitermax)) then 
                  nindx_1 = nindx_1 + 1 
                  indx_1(nindx_1) = i
                endif
              enddo
!
              !< j) Update the number of yielding elements for the next iteration 
              !     and check convergence
              !  ---------------------------------------------------------------
              nindx = nindx_1
              indx(1:nindx) = indx_1(1:nindx)
              if (nindx == 0) converged = .true.
!
            enddo
!
            !< Update the hourglass stabilization variable
            where (active_elements_mask(1:nel))
              et(1:nel) = beta*hr(1:nel) / (beta*hr(1:nel) + young(1:nel))
            end where
!
          endif
!
          !=====================================================================
          !< - Remove backstress contribution from the stress tensor
          !=====================================================================
          signxx(1:nel) = signxx(1:nel) + (sigb(1:nel,1) - sigb(1:nel,3))
          signyy(1:nel) = signyy(1:nel) + (sigb(1:nel,2) - sigb(1:nel,3))
          signxy(1:nel) = signxy(1:nel) +  sigb(1:nel,4)
!
          !=====================================================================
          !< - Update the thermal energy 
          !=====================================================================
          sigkkn(1:nel) = signxx(1:nel) + signyy(1:nel)
          deintth(1:nel) = - half*                                             &
              (sigkk(1:nel)+sigkkn(1:nel))*eth(1:nel)*area(1:nel)*thklyl(1:nel)
          eintth(1:nel) = eintth(1:nel) + deintth(1:nel)
          eint(1:nel,1) = eint(1:nel,1) + deintth(1:nel)
!
          !< Incremental (irreversible) annealing ratio
          if (abs(tastrt - taend) > em20) then
            tempfactor(1:nel) = max(zero,min(one, (temp(1:nel)-taend)/         &
                                                       (tastrt-taend)))
          else
            tempfactor(1:nel) = one
          endif
          pla(1:nel)    = pla(1:nel)    * tempfactor(1:nel)**frac
          epspxx(1:nel) = epspxx(1:nel) * tempfactor(1:nel)**frac
          epspyy(1:nel) = epspyy(1:nel) * tempfactor(1:nel)**frac
          epspzz(1:nel) = epspzz(1:nel) * tempfactor(1:nel)**frac
          epspxy(1:nel) = epspxy(1:nel) * tempfactor(1:nel)**frac
          sigb(1:nel,1:6) = sigb(1:nel,1:6)*spread(tempfactor(1:nel)**frac,2,6)
!
          !< Prepare temperature state for the next substep
          temp0(1:nel) = temp(1:nel)
!
          !=====================================================================
          !< - Phase change tracking 
          !=====================================================================
          if (t2phase > t1phase .and. t2phase > zero) then
            !< Recover the phase-tracking state variable
            !<   = 0 : T2PHASE not yet reached
            !<   < 0 (= -t2) : T2PHASE reached, T1PHASE not yet reached
            !<   > 0 (= cooling rate) : transformation completed, final result
            phasevar(1:nel) = uvar(1:nel,6)
            !< Step 1 - detect crossing of T2PHASE (transformation start)
            where (phasevar(1:nel) == zero .and. temp(1:nel) <= t2phase)
              phasevar(1:nel) = -(time + dt1*(real(isub,WP)*frac - one ))
            end where
            !< Step 2 - detect crossing of T1PHASE (transformation end) and
            !< compute the average cooling rate through the transformation window
            where (phasevar(1:nel) < zero .and. temp(1:nel) <= t1phase)
              phasevar(1:nel) = (t2phase - t1phase) /                          &
                  max(time + dt1*(real(isub,WP)*frac - one ) + phasevar(1:nel),&
                                                                         em20)
            end where
            !< Store back the updated state
            uvar(1:nel,6) = phasevar(1:nel)
          endif
!
        enddo
!
        !=======================================================================
        !< - Update the shell thickness
        !=======================================================================
        ezz(1:nel) = (-nu(1:nel)/(one - nu(1:nel)))*                           &
                       ((epsxx(1:nel) - epspxx(1:nel))  +                      &
                        (epsyy(1:nel) - epspyy(1:nel))) +                      &
                         epspzz(1:nel) + forth(1:nel)
        thk(1:nel)  = thk(1:nel) + thk0(1:nel)*thkly(1:nel)*(one + ezz(1:nel))
!
        !=======================================================================
        !< - User variable update
        !=======================================================================    
        uvar(1:nel,1) = epspxx(1:nel)
        uvar(1:nel,2) = epspyy(1:nel)
        uvar(1:nel,3) = epspzz(1:nel)
        uvar(1:nel,4) = epspxy(1:nel)
        uvar(1:nel,5) = temp_max(1:nel)
!
        !< Large array deallocation
        if (allocated(dsigb_dlam)) deallocate(dsigb_dlam)
!
       end subroutine sigeps137c
       end module sigeps137c_mod
