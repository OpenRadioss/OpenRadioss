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
!||    sigeps88c_mod   ../engine/source/materials/mat/mat088/sigeps88c.F90
!||--- called by ------------------------------------------------------
!||    mulawc          ../engine/source/materials/mat_share/mulawc.F90
!||====================================================================
      module sigeps88c_mod
      contains
!||====================================================================
!||    sigeps88c               ../engine/source/materials/mat/mat088/sigeps88c.F90
!||--- called by ------------------------------------------------------
!||    mulawc                  ../engine/source/materials/mat_share/mulawc.F90
!||--- calls      -----------------------------------------------------
!||    table_mat_vinterp       ../engine/source/materials/tools/table_mat_vinterp.F
!||--- uses       -----------------------------------------------------
!||    constant_mod            ../common_source/modules/constant_mod.F
!||    file_descriptor_mod     ../engine/source/modules/file_descriptor_mod.F90
!||    matparam_def_mod        ../common_source/modules/mat_elem/matparam_def_mod.F90
!||    mvsiz_mod               ../engine/share/spe_inc/mvsiz_mod.F90
!||    precision_mod           ../common_source/modules/precision_mod.F90
!||    table_mat_vinterp_mod   ../engine/source/materials/tools/table_mat_vinterp.F
!||====================================================================
      subroutine sigeps88c(                                                    &
        nel    ,matparam,nuvar   ,uvar    ,tstep  ,tt      ,                   &
        rho    ,soundsp ,off     ,ismstr  ,israte ,ngl     ,                   &
        epsxx  ,epsyy   ,epsxy   ,epspxx  ,epspyy ,epspxy  ,                   &
        depsxx ,depsyy  ,depsxy  ,depsyz  ,depszx ,                            &
        sigoxx ,sigoyy  ,sigoxy  ,sigoyz  ,sigozx ,                            &
        signxx ,signyy  ,signxy  ,signyz  ,signzx ,                            &
        asrate ,et      ,epsd    ,nvartmp ,vartmp ,dmg     ,                   &
        thkly  ,thk0    ,thkn    ,shf     ,ipg    ,npg     )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
        use matparam_def_mod 
        use constant_mod      
        use table_mat_vinterp_mod
        use file_descriptor_mod
        use precision_mod, only: WP
        use mvsiz_mod, only : mvsiz
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
        implicit none 
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
        integer, intent(in) :: nel                               !< number of elements
        type(matparam_struct_), intent(in) :: matparam           !< number of material parameters
        integer, intent(in) :: nuvar                             !< number of user variables
        real(kind=WP), dimension(nel,nuvar), intent(inout) :: uvar !< user variables
        real(kind=WP), intent(in) :: tstep                       !< current time step
        real(kind=WP), intent(in) :: tt                          !< current time
        real(kind=WP), dimension(nel), intent(in) :: rho         !< current density
        real(kind=WP), dimension(nel), intent(inout) :: soundsp  !< sound speed
        real(kind=WP), dimension(nel), intent(inout) :: off      !< element deletion status
        integer, intent(in) :: ismstr                            !< small strain option
        integer, intent(in) :: israte                            !< strain rate option
        real(kind=WP), dimension(nel), intent(in) :: epsxx       !< strain xx
        real(kind=WP), dimension(nel), intent(in) :: epsyy       !< strain yy
        real(kind=WP), dimension(nel), intent(in) :: epsxy       !< strain xy
        real(kind=WP), dimension(nel), intent(in) :: epspxx      !< strain rate xx
        real(kind=WP), dimension(nel), intent(in) :: epspyy      !< strain rate yy
        real(kind=WP), dimension(nel), intent(in) :: epspxy      !< strain rate xy
        real(kind=WP), dimension(nel), intent(in) :: depsxx      !< strain increment xx
        real(kind=WP), dimension(nel), intent(in) :: depsyy      !< strain increment yy
        real(kind=WP), dimension(nel), intent(in) :: depsxy      !< strain increment xy
        real(kind=WP), dimension(nel), intent(in) :: depsyz      !< strain increment yz
        real(kind=WP), dimension(nel), intent(in) :: depszx      !< strain increment zx
        real(kind=WP), dimension(nel), intent(in) :: sigoxx      !< previous stress xx
        real(kind=WP), dimension(nel), intent(in) :: sigoyy      !< previous stress yy
        real(kind=WP), dimension(nel), intent(in) :: sigoxy      !< previous stress xy
        real(kind=WP), dimension(nel), intent(in) :: sigoyz      !< previous stress yz
        real(kind=WP), dimension(nel), intent(in) :: sigozx      !< previous stress zx
        real(kind=WP), dimension(nel), intent(inout) :: signxx   !< stress xx
        real(kind=WP), dimension(nel), intent(inout) :: signyy   !< stress yy
        real(kind=WP), dimension(nel), intent(inout) :: signxy   !< stress xy
        real(kind=WP), dimension(nel), intent(inout) :: signyz   !< stress yz
        real(kind=WP), dimension(nel), intent(inout) :: signzx   !< stress zx
        real(kind=WP), intent(in) :: asrate                      !< strain rate filtering coefficient
        real(kind=WP), dimension(nel), intent(inout) :: et       !< hourglass control factor
        real(kind=WP), dimension(nel), intent(inout) :: epsd     !< strain rate
        integer, intent(in) :: nvartmp                           !< number of temporary variables
        integer, dimension(nel,nvartmp), intent(inout) :: vartmp !< temporary variables
        real(kind=WP), dimension(nel), intent(inout) :: dmg      !< damage variable
        integer, dimension(nel), intent(in) :: ngl               !< global element number
        real(kind=WP), dimension(nel), intent(in) :: thkly       !< shell element thickness at beginning of step
        real(kind=WP), dimension(nel), intent(inout) :: thkn     !< shell element thickness at end of step
        real(kind=WP), dimension(nel), intent(in) :: thk0        !< initial shell element thickness
        real(kind=WP), dimension(nel), intent(inout) :: shf      !< shell element shear factor
        integer, intent(in) :: ipg                               !< integration point number
        integer, intent(in) :: npg                               !< number of integration points
!----------------------------------------------------------------
!  L o c a l  V a r i a b l e s
!----------------------------------------------------------------
        !< Integer parameters
        integer :: itens,nload,iunl_for,rtype,failip,nv_base
        !< Real parameters
        real(kind=WP) :: nu,hys,shape,rbulk(nel),gs,gdamp,sigf,kfail,gam1,     &
          gam2,eh,beta
        !< Integer working variables
        integer :: i,j,n,ne_load,ne_unload,jj,nindx_dam,nindx_fail,           &
          iter2d
        integer, dimension (nel) :: indx_l,indx_unl,jdom,indx_dam,indx_fail
        integer, dimension(nel,6) :: ipos
        integer, parameter :: niter = 6    !< Number of recursive iterations
        integer, parameter :: niter2d = 3  !< Number of plane stress iterations
        !< Real working variables
        real(kind=WP) :: ldav,scale,lam_r,denom,fr,lam_tg,prev,sgnR,taper,xhat,&
          xmag,amax,xhat_raw,sgnR_dom,denom_dom,dlam_eff,gdir,lamj,dlam_tg,    &
          Rblend,dt3_dlam3,a11,rho0,young,dlamb1_dlam3,dlamb2_dlam3,dlamb3_dlam3
        real(kind=WP), dimension(nel) :: deint0,deint,p,epseq,emax,loadflg,    &
          sigdxx,sigdyy,sigdzz,sigdxy,sigdyz,sigdzx,sigdeff,erate,rv,rv_mth,   &
          gmax,ecurent,unl,dunldlam,loadflg_old,gunl,dgunldlam,ratioR,fcrit,   &
          i1,i2,dpdrv,trav,rootv,invrv,lam3_0,fJ,dfJdx,gJ,dgJdlam,dgJsqrdlam,  &
          gJsqr,xhat_dom,xfoam
        real(kind=WP), dimension(2,2) :: epsp
        real(kind=WP), dimension(nel,3) :: lam,evv,evvp,ee,eep,f,dfdlam,t,g,   &
          gsqr,dgdlam,dgsqrdlam
        real(kind=WP), dimension(nel,6) :: xvec
        real(kind=WP), dimension(nel,3,2) :: eigv
        real(kind=WP) :: vx, vy, nrm, alt1
!
        !=======================================================================
        !<                           /!\ WARNING
        !=======================================================================
        !  This material model is highly experimental and complex. It should be
        !  used with caution, and modified if necessary by experts only.
        !  Please refer to the documentation and relevant literature before 
        !  making any changes. Ask for review before committing any changes.
        !=======================================================================
!
        !=======================================================================
        !< - Initialisation of computation on time step
        !=======================================================================
        !< Recovering integer model parameter
        itens    = matparam%iparam(1) !< Strain rate effect type
        iunl_for = matparam%iparam(2) !< Unloading formulation
        nload    = matparam%iparam(3) !< Number of loading functions
        rtype    = matparam%iparam(4) !< Strain rate type
        failip   = matparam%iparam(5) !< Number of failed integration points prior to element deletion
        failip   = min(failip,npg)    !< Limit to max number of integration points
        nv_base  = matparam%iparam(6) !< Base number of user variables
        !< Recovering real model parameters
        rho0     = matparam%rho0      !< Initial density
        young    = matparam%young     !< Young's modulus
        rbulk(1:nel) = matparam%bulk  !< Bulk modulus
        nu       = matparam%nu        !< Poisson's ratio
        if (nu >= 0.49d0) nu = 0.5d0 
        gs       = matparam%shear     !< Shear modulus
        hys      = matparam%uparam(1) !< Hysteresis parameter
        shape    = matparam%uparam(2) !< Shape parameter
        gdamp    = matparam%uparam(3) !< Damping parameter
        sigf     = matparam%uparam(4) !< Cutoff stress
        kfail    = matparam%uparam(5) !< Failure criterion threshold
        gam1     = matparam%uparam(6) !< Failure criterion shape parameter 1
        gam2     = matparam%uparam(7) !< Failure criterion shape parameter 2
        eh       = matparam%uparam(8) !< Damage softening parameter
        beta     = matparam%uparam(9) !< Pressure damping parameter
!
        !=======================================================================
        !< - Recover user variables
        !=======================================================================
        emax(1:nel)    = uvar(1:nel,1) !< Maximum strain energy over loading time
        ecurent(1:nel) = uvar(1:nel,2) !< Current strain energy
        loadflg(1:nel) = uvar(1:nel,5) !< Load/Unload flag
        lam3_0(1:nel)  = uvar(1:nel,8) !< Previous third principal stretch
        loadflg_old(1:nel) = loadflg(1:nel)
        do i = 1, nel
          if (off(i) <  one) off(i) = off(i)*four_over_5
          if (off(i) < em01) off(i) = zero
          !< Old frictionnal damping stresses
          sigdxx(i) = uvar(i,6)
          sigdyy(i) = uvar(i,7)
          sigdxy(i) = uvar(i,9)
          !< Previous energie increment
          deint0(i) = half*(one/(max(one - dmg(i),em20)))*(                    &
                           (sigoxx(i) - sigdxx(i))*depsxx(i) +                 &
                           (sigoyy(i) - sigdyy(i))*depsyy(i) +                 &
                           (sigoxy(i) - sigdxy(i))*depsxy(i))
          !< Re-initialize the thickness
          if (ipg == 1) thkn(i) = zero
        enddo
!
        !< Specific user variables for tabulated unloading (iunl_for == 1)
        if (iunl_for == 1) then 
          do i = 1, nel
            do j = 1,3
              if (uvar(i,nv_base+9+j) <= zero) uvar(i,nv_base+9+j) = one
            enddo
            !< Previous hysteresis ratio R_prev = f_r/f_tg (initially 1)
            if (uvar(i,nv_base+13) <= zero) uvar(i,nv_base+13) = one 
          enddo
        endif
!
        !< Compute principal strains and directions (2D plane stress case)
        do i = 1,nel
!
          !< Principal strains
          trav(i)  = epsxx(i) + epsyy(i)
          rootv(i) = sqrt((epsxx(i)-epsyy(i))*(epsxx(i)-epsyy(i))              &
                                             + epsxy(i)*epsxy(i))
          evv(i,1) = half*(trav(i)+rootv(i))
          evv(i,2) = half*(trav(i)-rootv(i))
          evv(i,3) = zero
!
          !< Principal directions
          !< Primary eigenvector with fallback
          vx   = half*epsxy(i)
          vy   = evv(i,1) - epsxx(i)
          alt1 = abs(vx) + abs(vy)
          if (alt1 < em06) then
            vx = evv(i,1) - epsyy(i)
            vy = half*epsxy(i)
          end if
!
          !< Normalize / handle near-isotropy by freezing previous direction if available
          nrm = sqrt(vx*vx + vy*vy)
          if (nrm > em06) then
            vx = vx / nrm
            vy = vy / nrm
          else
            !< Quasi-isotropic: reuse previous if stored, else take x-axis
            if (abs(uvar(i,10)) + abs(uvar(i,11)) > zero) then
              vx = uvar(i,10)
              vy = uvar(i,11)
            else
              vx = one
              vy = zero
            end if
          end if
!
          !< Sign continuity: align with previous orientation if stored
          if (vx*uvar(i,10) + vy*uvar(i,11) < zero) then
            vx = -vx
            vy = -vy
          end if
!
          !< Store eigv1
          eigv(i,1,1) = vx
          eigv(i,2,1) = vy
          eigv(i,3,1) = zero
!     
          !< Orthonormal eigv2 = [-vy, vx]
          eigv(i,1,2) = -vy
          eigv(i,2,2) =  vx
          eigv(i,3,2) = zero
!
          !< Store principal direction in user variables for next step
          uvar(i,10) = eigv(i,1,1)
          uvar(i,11) = eigv(i,2,1)
!
        enddo
!
        !< Compute strain rates in principal reference directions
        do i = 1,nel
          evvp(i,1) = eigv(i,1,1)*eigv(i,1,1)*epspxx(i) +                      &
                      eigv(i,1,1)*eigv(i,2,1)*epspxy(i) +                      &
                      eigv(i,2,1)*eigv(i,2,1)*epspyy(i)
          evvp(i,2) = eigv(i,1,2)*eigv(i,1,2)*epspxx(i) +                      &
                      eigv(i,1,2)*eigv(i,2,2)*epspxy(i) +                      &
                      eigv(i,2,2)*eigv(i,2,2)*epspyy(i)
          !< Third principal strain rate
          if (nu > 0.49d0) then
            evvp(i,3) = - (evvp(i,1) + evvp(i,2))
          else
            evvp(i,3) = zero
          endif
        enddo
!
        !=======================================================================
        !< Compute principal stretches (λ_i) from strains
        !  (Depending on strain measure: logarithmic,Green-Lagrange,engineering)
        !=======================================================================
        if (ismstr == 0 .or. ismstr == 2 .or. ismstr == 4) then
          ! -> Logarithmic strains
          do i = 1, nel
            lam(i,1) = exp(evv(i,1))
            lam(i,2) = exp(evv(i,2))
            if (lam3_0(i) > zero) then
              lam(i,3) = lam3_0(i)
            else
              lam(i,3) = one/(lam(i,1)*lam(i,2))
            endif
          enddo
        elseif (ismstr == 10 .or. ismstr == 12) then
          !< Green-Lagrange strain -> λ = sqrt(1 + 2E) (with possible offset)
          do i = 1, nel
            lam(i,1) = sqrt(evv(i,1) + one)
            lam(i,2) = sqrt(evv(i,2) + one)
            if (lam3_0(i) > zero) then
              lam(i,3) = lam3_0(i)
            else
              lam(i,3) = one/(lam(i,1)*lam(i,2))
            endif
          enddo
        else
          !< Engineering strain -> λ = 1 + ε
          do i = 1, nel
            lam(i,1) = evv(i,1) + one
            lam(i,2) = evv(i,2) + one
            if (lam3_0(i) > zero) then
              lam(i,3) = lam3_0(i)
            else
              lam(i,3) = one/(lam(i,1)*lam(i,2))
            endif
          enddo
        endif
        !< Current principal engineering strains (ee = λ - 1)
        do i = 1, nel
          ee(i,1) = lam(i,1) - one
          ee(i,2) = lam(i,2) - one
          ee(i,3) = lam(i,3) - one
        enddo
!
        !=======================================================================
        ! - Compute equivalent strain rate (scalar)
        !=======================================================================
        if (rtype == 1) then
          ! Engineering strain rate (account for strain measure)
          do i = 1, nel
            !< Current principal engineering strain rates
            if (ismstr == 0 .or. ismstr == 2 .or. ismstr == 4) then
              eep(i,1) = exp(evv(i,1))*evvp(i,1)
              eep(i,2) = exp(evv(i,2))*evvp(i,2)
            !< Green-Lagrange strains
            elseif (ismstr == 10 .or. ismstr == 12) then
              eep(i,1) = half*(one/sqrt(evv(i,1) + one))*evvp(i,1)
              eep(i,2) = half*(one/sqrt(evv(i,2) + one))*evvp(i,2)
            !< Engineering strains
            else
              eep(i,1) = evvp(i,1)
              eep(i,2) = evvp(i,2)
            endif
            !< Current strain rate             
            erate(i) = sqrt(eep(i,1)**2 + eep(i,2)**2)
          enddo
        else
          !< True (logarithmic) strain rate
          do i = 1, nel
            erate(i) = sqrt(evvp(i,1)**2 + evvp(i,2)**2)
          end do
        end if
        !< Optional strain rate filtering
        if (israte > 0) then
          epsd(1:nel) = asrate * erate(1:nel) + (one - asrate) * epsd(1:nel)
        else
          epsd(1:nel) = erate(1:nel)
        endif
!
        !<======================================================================
        !< - Compute current loading stresses in principal directions
        !   (Interpolation of loading curves w.r.t stretches and strain rate)
        !<======================================================================
        !< Plane stress iterations
        do iter2d = 1, niter2d
!
          do i = 1,nel
            !< Relative volume computation 
            ! (rho0/rho) = def(F) = J with F = Grad(Strain)
            rv(i) = lam(i,1)*lam(i,2)*lam(i,3) 
            !< Relative volume to the power -1/3 : J^(-1/3)
            if (rv(i) > zero) then
              rv_mth(i) = exp((-third)*log(rv(i)))
            else
              rv_mth(i) = zero
            endif
            ! -> For compressible materials 
            if ((nu > zero) .and. (nu < 0.49d0)) then
              rv_mth(i) = one
            endif
            !< Normalized stretch (isochoric stretch)
            lam(i,1) = lam(i,1)*rv_mth(i)
            lam(i,2) = lam(i,2)*rv_mth(i)
            lam(i,3) = lam(i,3)*rv_mth(i)
          enddo
!  
          !< Compute the current and rate dependent loading stresses
          do i = 1,nel
            !< Interpolation abscissa (normalized stretch and strain rate)
            xvec(i,1) = lam(i,1)
            xvec(i,3) = lam(i,2)
            xvec(i,5) = lam(i,3)
            !< Strain rate
            xvec(i,2) = epsd(i)                
            xvec(i,4) = epsd(i)
            xvec(i,6) = epsd(i)
            !< Strain rate dependency type
            ! -> If itens = -1, no strain rate effect during unloading only, 
            !    strain rate effect during loading are always considered 
            !    (tension and compression)
            ! -> If itens = 0, strain rate effect in compression only
            if (itens == 0) then 
              if (rv(i) > one) then 
                xvec(i,2) = zero
                xvec(i,4) = zero
                xvec(i,6) = zero
              endif
            endif
            ! -> If itens = 1, strain rate effect in all cases 
            !    (loading and unloading + tension and compression)
          enddo
          !< Interpolate uniaxial loading stress functions g(λ, ẋ) for each 
          !  principal stretch
          call table_mat_vinterp(matparam%table(1),nel,nel,vartmp(1:nel,1),    &
                                  xvec(1:nel,1),g(1:nel,1),dgdlam(1:nel,1))     
          call table_mat_vinterp(matparam%table(1),nel,nel,vartmp(1:nel,3),    &
                                  xvec(1:nel,3),g(1:nel,2),dgdlam(1:nel,2)) 
          call table_mat_vinterp(matparam%table(1),nel,nel,vartmp(1:nel,5),    &
                                  xvec(1:nel,5),g(1:nel,3),dgdlam(1:nel,3))     
          !< Initialize loading function values f = λ * g, and derivatives 
          !  dfdlam = d(λ*g)/dλ
          do i = 1, nel
            !< Loading functions initialization
            f(i,1) = xvec(i,1)*g(i,1)
            f(i,2) = xvec(i,3)*g(i,2)
            f(i,3) = xvec(i,5)*g(i,3)
            !< Loading functions derivatives initialization
            dfdlam(i,1) = g(i,1) + xvec(i,1)*dgdlam(i,1)
            dfdlam(i,2) = g(i,2) + xvec(i,3)*dgdlam(i,2)
            dfdlam(i,3) = g(i,3) + xvec(i,5)*dgdlam(i,3)
          enddo
          !< Recursive integration for higher-order stretches (niter iterations)
          do n = 1, niter
            do i = 1,nel
              xvec(i,1) = lam(i,1)**((-nu)**n)
              xvec(i,3) = lam(i,2)**((-nu)**n)
              xvec(i,5) = lam(i,3)**((-nu)**n)
              ipos(i,1:6) = 1
            enddo
            !< Interpolation of the uniaxial loading stresses
            call table_mat_vinterp(matparam%table(1),nel,nel,ipos(1:nel,1),    & 
                            xvec(1:nel,1),gsqr(1:nel,1),dgsqrdlam(1:nel,1))
            call table_mat_vinterp(matparam%table(1),nel,nel,ipos(1:nel,3),    & 
                            xvec(1:nel,3),gsqr(1:nel,2),dgsqrdlam(1:nel,2))
            call table_mat_vinterp(matparam%table(1),nel,nel,ipos(1:nel,5),    & 
                            xvec(1:nel,5),gsqr(1:nel,3),dgsqrdlam(1:nel,3))
            do i = 1, nel
              !< Loading functions recursive update
              f(i,1) = f(i,1) + xvec(i,1)*gsqr(i,1)
              f(i,2) = f(i,2) + xvec(i,3)*gsqr(i,2)
              f(i,3) = f(i,3) + xvec(i,5)*gsqr(i,3)
              !< Loading functions derivatives recursiven update
              dfdlam(i,1) = dfdlam(i,1) +                                      &
                             ((-nu)**n)*(lam(i,1)**(((-nu)**n)-1))*            &
                             (gsqr(i,1) + lam(i,1)**((-nu)**n)*dgsqrdlam(i,1))
              dfdlam(i,2) = dfdlam(i,2) +                                      &
                             ((-nu)**n)*(lam(i,2)**(((-nu)**n)-1))*            &
                             (gsqr(i,2) + lam(i,2)**((-nu)**n)*dgsqrdlam(i,2))
              dfdlam(i,3) = dfdlam(i,3) +                                      &
                             ((-nu)**n)*(lam(i,3)**(((-nu)**n)-1))*            &
                             (gsqr(i,3) + lam(i,3)**((-nu)**n)*dgsqrdlam(i,3))
            enddo
          enddo
!        
          !<==================================================================== 
          !< Hydrostatic pressure computation
          !<====================================================================
          ! -> Viscous pressure
          if (beta > zero) then 
            do i = 1,nel
              !< Volume change rate
              ldav = epspxx(i) + epspyy(i)
              !< Viscous pressure
              p(i) = uvar(i,12)*exp(-beta*tstep) +                            &
                        rbulk(i)*ldav*((one - exp(-beta*tstep))/beta)
              !< Derivative of p w.r.t. rv
              dpdrv(i) = zero
            enddo
          ! -> Compressible pressure (Foam version)
          elseif ((nu > zero) .and. (nu < 0.49d0)) then
            !< Interpolation abscissa (based on relative volume and strain rate)
            do i = 1,nel 
              xfoam(i)  = rv(i)**((-nu/(one - two*nu)))
              xvec(i,1) = xfoam(i)
              xvec(i,2) = epsd(i)
              ipos(i,1:6) = 1
            enddo
            !< Interpolation of the uniaxial loading stresses
            call table_mat_vinterp(matparam%table(1),nel,nel,ipos(1:nel,1),      & 
                                   xvec(1:nel,1),gJ(1:nel),dgJdlam(1:nel))
            !< Initialize loading function value
            do i = 1, nel
              fJ(i) = gJ(i)*xvec(i,1)
              dfJdx(i) = gJ(i) + xvec(i,1)*dgJdlam(i)
            enddo
            !< Recursive integration for higher-order stretches (niter iterations)
            do n = 1, niter
              do i = 1,nel
                xvec(i,1) = xfoam(i)**((-nu)**n)
                xvec(i,2) = epsd(i) 
                ipos(i,1:6) = 1
              enddo
              !< Interpolation of the uniaxial loading stresses
              call table_mat_vinterp(matparam%table(1),nel,nel,ipos(1:nel,1),    & 
                                     xvec(1:nel,1),gJsqr(1:nel),dgJsqrdlam(1:nel))
              do i = 1, nel
                !< Loading functions recursive update
                fJ(i) = fJ(i) + xvec(i,1)*gJsqr(i)
                dfJdx(i) = dfJdx(i) + ((-nu)**n) *                               &
                           (xfoam(i)**(((-nu)**n) - one)) *                      &
                           (gJsqr(i) + xvec(i,1) * dgJsqrdlam(i))
              enddo
            enddo
            !< Effective bulk modulus computation
            do i = 1, nel
              rbulk(i) = (-nu/(one - two*nu)) * xfoam(i) * dfJdx(i)
              if (rbulk(i) < em12) rbulk(i) = em12
            enddo
          ! -> Incompressible pressure (Rubber version)
          else
            do i = 1,nel
              p(i) = rbulk(i)*(rv(i) - one)
              dpdrv(i) = rbulk(i)
            enddo
          endif
!
          !< Compute the derivarive of t3 w.r.t. λ3 from incompressibility
          do i = 1,nel
!
            !< Current true principal stresses in out-of-plane direction
            if (nu <= zero .or. nu >= 0.49d0) then
              t(i,3) = (two_third*f(i,3)-third*(f(i,1) + f(i,2)) + p(i))/rv(i)
            else
              t(i,3) = (f(i,3) - fJ(i))/rv(i)
            endif
!
            !< Derivative of t3 w.r.t. λ3
            if (nu <= zero .or. nu >= 0.49d0) then
              dt3_dlam3 = (one/rv(i))*(                                        &
                    (four/nine)*dfdlam(i,3)*lam(i,3)/(lam(i,3)/rv_mth(i)) +    &
                     (one/nine)*dfdlam(i,1)*lam(i,1)/(lam(i,3)/rv_mth(i)) +    &
                     (one/nine)*dfdlam(i,2)*lam(i,2)/(lam(i,3)/rv_mth(i)) +    &
              (lam(i,1)/rv_mth(i))*(lam(i,2)/rv_mth(i))*(dpdrv(i) - t(i,3)))
            else
              dt3_dlam3 = (one/rv(i))*dfdlam(i,3) + (one/lam(i,3))*dfJdx(i)*(  &
                      nu/(one - two*nu))*(rv(i)**((nu-one)/(one - two*nu))) -  &
                      (one/lam(i,3))*t(i,3)
            endif
            dt3_dlam3 = sign(max(abs(dt3_dlam3),em20) ,dt3_dlam3)
!              
            !< Update the third stretch to enforce incompressibility
            !  Unnormalize λ_i
            lam(i,1) = lam(i,1)/rv_mth(i)
            lam(i,2) = lam(i,2)/rv_mth(i)
            lam(i,3) = lam(i,3)/rv_mth(i) - t(i,3)/dt3_dlam3
!
          enddo  
        enddo
!
        !<======================================================================
        !< - Compute trial Cauchy stress (principal) assuming loading path
        !    (This will be adjusted for unloading elements next)
        !<======================================================================
        ne_load = 0
        ne_unload = 0
        indx_l(1:nel) = 0
        indx_unl(1:nel) = 0
        do i = 1, nel
          ! Re-normalize λ_i
          lam(i,1) = lam(i,1)*rv_mth(i)
          lam(i,2) = lam(i,2)*rv_mth(i)
          lam(i,3) = lam(i,3)*rv_mth(i)
          !< Only for non-deleted elements
          if (off(i) == one) then
            !< Compute equivalent total strain (for energy tracking)
            epseq(i) = sqrt(ee(i,1)**2 + ee(i,2)**2)
            !< Compute trial principal Cauchy stresses t(i,j) for each direction:
            ! -> Incompressible-like case (nu <= 0 or nu >= 0.49)
            if (nu <= zero .or. nu >= 0.49d0) then 
              t(i,1) = (two_third*f(i,1) - third*(f(i,2) + f(i,3)) + p(i))/rv(i)
              t(i,2) = (two_third*f(i,2) - third*(f(i,1) + f(i,3)) + p(i))/rv(i)
              t(i,3) = (two_third*f(i,3) - third*(f(i,1) + f(i,2)) + p(i))/rv(i)
            ! -> Compressible case (0 < nu < 0.49)
            else
              t(i,1) = (f(i,1) - fJ(i))/rv(i)
              t(i,2) = (f(i,2) - fJ(i))/rv(i)
              t(i,3) = (f(i,3) - fJ(i))/rv(i)
            endif
            !< Compute strain energy increment (using trial stresses)
            deint(i) = deint0(i) + half*(                                        &
                          t(i,1)*evvp(i,1) +                                     &
                          t(i,2)*evvp(i,2)) * tstep
            ecurent(i) = max(em20, ecurent(i) + deint(i))
            emax(i)    = max(emax(i), ecurent(i))
            !< Determine loading/unloading flag based on energy input
            if (loadflg(i) == -one) then
              !< Element was unloading
              if (deint(i)/max(ecurent(i),em20) >= em07) then
                !< Now gaining energy -> reload
                ne_load = ne_load + 1
                indx_l(ne_load) = i
                loadflg(i) = one
                emax(i) = ecurent(i)   !< reset energy threshold at reload
              else
                !< Continue unloading
                ne_unload = ne_unload + 1
                indx_unl(ne_unload) = i
                loadflg(i) = -one
              endif
            else
              !< Element was loading
              if (deint(i)/max(emax(i), em20) >= zero) then
                !< Still loading (or reloading)
                ne_load = ne_load + 1
                indx_l(ne_load) = i
                loadflg(i) = one
              else
                !< Switch to unloading
                ne_unload = ne_unload + 1
                indx_unl(ne_unload) = i
                loadflg(i) = -one
              endif
            endif
          endif
        enddo
!
        !<======================================================================
        !< - Special handling for tabulated unloading (iunl_for == 1)
        !<   Update state variables at load/unload switches for hysteresis loops
        !<======================================================================
        if (iunl_for == 1) then
          !< Update state at loading / unloading switch
          do i = 1, nel
            !< Switch loading -> unloading
            if (loadflg_old(i) == one .and. loadflg(i) == -one) then
              !< Loading -> Unloading: store state at reversal
              do j = 1, 3
                !< Stretch at unload start (λ_r)
                uvar(i,nv_base+j) = lam(i,j)
                !< Anchor stretch (λ_tg) (initially set to 1)
                if (abs(uvar(i,nv_base+3+j))<em10) uvar(i,nv_base+3+j) = one
                !< Loading function at switch (peak f)
                uvar(i,nv_base+6+j) = f(i,j)
                !< Normalized amplitude tracking (initially set to 1)
                uvar(i,nv_base+9+j) = one
              enddo
              !< Previous hysteresis ratio R_prev = f_r/f_tg (initially 1)
              uvar(i,nv_base+13) = one 
            !< Switch unloading -> re-loading
            elseif (loadflg_old(i) == -one .and. loadflg(i) == one) then
              !< Unloading->Reloading: update anchors for closed loop continuity
              do j = 1, 3
                !< Previous stretch at switch, new anchor stretch
                uvar(i,nv_base+3+j) = uvar(i,nv_base+j)
                !< Current stretch at switch
                uvar(i,nv_base+j) = lam(i,j)
                !< Loading function at switch
                uvar(i,nv_base+6+j) = f(i,j)
                !< Reset the abscissa amplitude
                uvar(i,nv_base+9+j) = one
              enddo
              !< Previous hysteresis ratio R_prev = f_r/f_tg (initially 1)
              uvar(i,nv_base+13) = one
            endif
          enddo
          !< Check for complete loop closure in unloading elements
          do i = 1, nel
            if (loadflg(i) == -one) then
              !< Reset the abscissa amplitude
              amax = zero
              do j = 1,3
                !< Recover the stretch at loading/unloading switch
                lam_r  = uvar(i,nv_base+j)
                !< Anchor stretch at loading/unloading switch
                lam_tg = uvar(i,nv_base+3+j)
                !< Compute the normalized progress of unloading
                denom  = max(abs(lam_r - lam_tg), em20)
                xhat   = (lam(i,j) - lam_tg) / denom
                xhat   = max(-one, min(one, xhat))
                !< Monotonicity: the amplitude should only decrease along 
                !  the unloading
                prev   = uvar(i,nv_base+9+j)
                if (prev <= zero) prev = one
                xmag   = min(abs(xhat), prev)
                uvar(i,nv_base+9+j) = xmag
                !< Save the maximum amplitude among directions
                amax = max(amax, xmag)
              enddo
              !< Closing criterion (unloading only if the amplitude is 
              !  small enough)
              if (amax <= em03) then
                !< Hysteresis loop closed (amplitude ~ 0): treat as reloading
                loadflg(i) = one
                do j = 1,3
                  !< Save the new anchor stretch
                  uvar(i,nv_base+3+j) = uvar(i,nv_base+j)
                  !< Update the stretch at loading/unloading switch
                  uvar(i,nv_base+j) = lam(i,j)
                  !< Loading function at switch
                  uvar(i,nv_base+6+j) = f(i,j)
                  !< Reset the abscissa amplitude
                  uvar(i,nv_base+9+j) = one
                enddo
                !< Previous hysteresis ratio R_prev = f_r/f_tg (initially 1)
                uvar(i,nv_base+13) = one 
              !< Otherwise, the element remains unloaded
              else
                loadflg(i) = -one
              endif
            endif
          enddo
          !< Rebuild loaded/unloaded lists after adjustments
          ne_load = 0
          ne_unload = 0
          indx_l(1:nel) = 0
          indx_unl(1:nel) = 0
          do i = 1, nel
            if (off(i) == one) then
              if (loadflg(i) == one) then
                ne_load = ne_load + 1
                indx_l(ne_load) = i
              else
                ne_unload = ne_unload + 1
                indx_unl(ne_unload) = i
              endif
            endif
          enddo
        endif
!
        !<======================================================================
        !< - Unloaded elements treatment
        !<======================================================================
        if (ne_unload > 0) then
          select case (iunl_for)
          !<--------------------------------------------------------------------
          !< Following unloading curve
          !<--------------------------------------------------------------------
          case (1)
!           
            !< Re-initialize variables for unloading elements
            xvec(1:nel,1:6) = zero
            !< Unnormalize λ_i
            do i = 1, nel
              lam(i,1:3) = lam(i,1:3) / rv_mth(i)
            enddo
!
            !< Plane stress iterations
            do iter2d = 1, niter2d
              do jj = 1, ne_unload
                i = indx_unl(jj)
!
                !< Relative volume computation 
                ! (rho0/rho) = def(F) = J with F = Grad(Strain)
                rv(i) = lam(i,1)*lam(i,2)*lam(i,3)
                !< Relative volume to the power -1/3 : J^(-1/3)
                if (rv(i) > zero) then
                  rv_mth(i) = exp((-third)*log(rv(i)))
                else
                  rv_mth(i) = zero
                endif
                ! -> For compressible materials 
                if ((nu > zero) .and. (nu < 0.49d0)) then
                  rv_mth(i) = one
                endif
!
                !< Normalized stretch (isochoric stretch)
                lam(i,1) = lam(i,1)*rv_mth(i)
                lam(i,2) = lam(i,2)*rv_mth(i)
                lam(i,3) = lam(i,3)*rv_mth(i)
!
                !< Compute the current and rate dependent loading stresses
                !< Interpolation abscissa (normalized stretch and strain rate)
                xvec(i,1) = lam(i,1)
                xvec(i,3) = lam(i,2)
                xvec(i,5) = lam(i,3)
                !< Strain rate
                xvec(i,2) = epsd(i)                
                xvec(i,4) = epsd(i)
                xvec(i,6) = epsd(i)
                !< Strain rate dependency type
                ! -> If itens = -1, no strain rate effect during unloading only, 
                !    strain rate effect during loading are always considered 
                !    (tension and compression)
                ! -> If itens = 0, strain rate effect in compression only
                if (itens == 0) then 
                  if (rv(i) > one) then 
                    xvec(i,2) = zero
                    xvec(i,4) = zero
                    xvec(i,6) = zero
                  endif
                endif
              enddo
!
              !< Interpolate uniaxial loading stress functions g(λ, ẋ) for each 
              !  principal stretch
              call table_mat_vinterp(matparam%table(1),nel,nel,                &
                     vartmp(1:nel,1),xvec(1:nel,1),g(1:nel,1),dgdlam(1:nel,1))     
              call table_mat_vinterp(matparam%table(1),nel,nel,                &
                     vartmp(1:nel,3),xvec(1:nel,3),g(1:nel,2),dgdlam(1:nel,2)) 
              call table_mat_vinterp(matparam%table(1),nel,nel,                &
                     vartmp(1:nel,5),xvec(1:nel,5),g(1:nel,3),dgdlam(1:nel,3))
!     
              !< Initialize loading function values f = λ * g, and derivatives 
              !  dfdlam = d(λ*g)/dλ
              do jj = 1, ne_unload
                i = indx_unl(jj)
                !< Loading functions initialization
                f(i,1) = xvec(i,1)*g(i,1)
                f(i,2) = xvec(i,3)*g(i,2)
                f(i,3) = xvec(i,5)*g(i,3)
                !< Loading functions derivatives initialization
                dfdlam(i,1) = g(i,1) + xvec(i,1)*dgdlam(i,1)
                dfdlam(i,2) = g(i,2) + xvec(i,3)*dgdlam(i,2)
                dfdlam(i,3) = g(i,3) + xvec(i,5)*dgdlam(i,3)
              enddo
!
              !< Recursive integration for higher-order stretches
              do n = 1, niter
                do jj = 1, ne_unload
                  i = indx_unl(jj)
                  xvec(i,1) = lam(i,1)**((-nu)**n)
                  xvec(i,3) = lam(i,2)**((-nu)**n)
                  xvec(i,5) = lam(i,3)**((-nu)**n)
                  ipos(i,1:6) = 1
                enddo
                !< Interpolation of the uniaxial loading stresses
                call table_mat_vinterp(matparam%table(1),nel,nel,ipos(1:nel,1),& 
                                xvec(1:nel,1),gsqr(1:nel,1),dgsqrdlam(1:nel,1))
                call table_mat_vinterp(matparam%table(1),nel,nel,ipos(1:nel,3),& 
                                xvec(1:nel,3),gsqr(1:nel,2),dgsqrdlam(1:nel,2))
                call table_mat_vinterp(matparam%table(1),nel,nel,ipos(1:nel,5),& 
                                xvec(1:nel,5),gsqr(1:nel,3),dgsqrdlam(1:nel,3))
                !< Update loading functions and derivatives
                do jj = 1, ne_unload
                  i = indx_unl(jj)
                  !< Loading functions recursive update
                  f(i,1) = f(i,1) + xvec(i,1)*gsqr(i,1)
                  f(i,2) = f(i,2) + xvec(i,3)*gsqr(i,2)
                  f(i,3) = f(i,3) + xvec(i,5)*gsqr(i,3)
                  !< Loading functions derivatives recursiven update
                  dfdlam(i,1) = dfdlam(i,1) +                                  &
                              ((-nu)**n)*(lam(i,1)**(((-nu)**n)-1))*           &
                              (gsqr(i,1) + lam(i,1)**((-nu)**n)*dgsqrdlam(i,1))
                  dfdlam(i,2) = dfdlam(i,2) +                                  &
                              ((-nu)**n)*(lam(i,2)**(((-nu)**n)-1))*           &
                              (gsqr(i,2) + lam(i,2)**((-nu)**n)*dgsqrdlam(i,2))
                  dfdlam(i,3) = dfdlam(i,3) +                                  &
                              ((-nu)**n)*(lam(i,3)**(((-nu)**n)-1))*           &
                              (gsqr(i,3) + lam(i,3)**((-nu)**n)*dgsqrdlam(i,3))
                enddo
              enddo
!
              !< Specific treatment for foam-like compressible materials
              if ((nu > zero) .and. (nu < 0.49d0)) then
                !< Interpolation abscissa (based on relative volume and strain rate)
                do jj = 1, ne_unload
                  i = indx_unl(jj) 
                  xfoam(i)  = rv(i)**((-nu/(one - two*nu)))
                  xvec(i,1) = xfoam(i)
                  xvec(i,2) = epsd(i)
                  ipos(i,1:6) = 1
                enddo
                !< Interpolation of the uniaxial loading stresses
                call table_mat_vinterp(matparam%table(1),nel,nel,ipos(1:nel,1),& 
                                       xvec(1:nel,1),gJ(1:nel),dgJdlam(1:nel))
                !< Initialize loading function value
                do jj = 1, ne_unload
                  i = indx_unl(jj)
                  fJ(i) = gJ(i)*xvec(i,1)
                  dfJdx(i) = gJ(i) + xvec(i,1)*dgJdlam(i)
                enddo
                !< Recursive integration for higher-order stretches (niter iterations)
                do n = 1, niter
                  do jj = 1, ne_unload
                    i = indx_unl(jj)
                    xvec(i,1) = xfoam(i)**((-nu)**n)
                    xvec(i,2) = epsd(i) 
                    ipos(i,1:6) = 1
                  enddo
                  !< Interpolation of the uniaxial loading stresses
                  call table_mat_vinterp(matparam%table(1),nel,nel,            & 
                          ipos(1:nel,1),xvec(1:nel,1),gJsqr(1:nel),            &
                                                 dgJsqrdlam(1:nel))
                  do jj = 1, ne_unload
                    i = indx_unl(jj)
                    !< Loading functions recursive update
                    fJ(i) = fJ(i) + xvec(i,1)*gJsqr(i)
                    dfJdx(i) = dfJdx(i) + ((-nu)**n) *                         &
                               (xfoam(i)**(((-nu)**n) - one)) *                &
                               (gJsqr(i) + xvec(i,1) * dgJsqrdlam(i))
                  enddo
                enddo
                !< Effective bulk modulus computation
                do jj = 1, ne_unload
                  i = indx_unl(jj)
                  rbulk(i) = (-nu/(one - two*nu)) * xfoam(i) * dfJdx(i)
                  if (rbulk(i) < em12) rbulk(i) = em12
                enddo
              endif
!
              !<================================================================
              !< - Find the dominant direction jdom(i) for each unloading element 
              !  i via the maximum normalized amplitude amax = max(|xhat|) among
              !  the 3 directions, with
              !  xhat = (λ - λ_tg) / |λ_r - λ_tg|  (in [-1,1])
              !  (lam_r = stretch at unload start, λ_tg = anchor stretch)
              !  (xhat = 0 at anchor, xhat = ±1 at unload start)
              !  (also enforce monotonicity |xhat| ≤ previous value)
              !<================================================================ 
              do jj = 1, ne_unload
                i = indx_unl(jj)
                !< Find dominant direction jdom(i) with max amplitude
                amax = -one
                xhat_dom(i) = zero
                jdom(i) = 1
                do j = 1,2
                  !< Stretch at unload start
                  lam_r  = uvar(i,nv_base+j)   
                  !< Stretch anchor                    
                  lam_tg = uvar(i,nv_base+3+j)                       
                  denom  = max(abs(lam_r - lam_tg), em20)
                  !< Normalized abscissa
                  xhat   = (lam(i,j) - lam_tg)/denom
                  xhat   = max(-one, min(one, xhat))
                  prev   = uvar(i,nv_base+9+j)
                  if (prev <= zero) prev = one
                  !< Enforce monotonicity
                  xmag   = min(abs(xhat),prev)
                  uvar(i,nv_base+9+j) = xmag
                  if (xmag > amax) then
                    amax = xmag
                    jdom(i) = j
                    xhat_dom(i) = sign(one,xhat)*xmag
                  end if
                end do
!
                !< Fill interpolation abscissa for dominant direction only
                xvec(i,1) = sign(one,xhat_dom(i))*min(abs(xhat_dom(i)),one - em08)
!
                !< Strain rate dependency type
                ! -> If itens = -1, no strain rate effect during unloading
                ! -> Strain rate effect in compression only (itens = 0)
                if (itens == 0) then
                  if (rv(i) < one) xvec(i,2) = epsd(i)
                ! -> Strain rate effect in all cases (itens = 1)
                elseif (itens == 1) then
                  xvec(i,2) = epsd(i)
                endif
              enddo
!
              !<================================================================
              !< - Interpolation of loading function g(xhat, ẋ), unloading 
              !    function gunl(xhat, ẋ), and their derivatives, for the  
              !    dominant direction of each unloading element. 
              !<================================================================
              ! --- Loading ---
              ipos(1:nel,1:6) = 1
              call table_mat_vinterp(matparam%table(3),nel,nel,ipos(1:nel,1),  &
                                    xvec(1:nel,1),g(1:nel,1),dgdlam(1:nel,1))
              ! --- Unloading ---
              ipos(1:nel,1:6) = 1
              call table_mat_vinterp(matparam%table(2),nel,nel,ipos(1:nel,1),  &
                                  xvec(1:nel,1),gunl(1:nel),dgunldlam(1:nel))
!
              !<================================================================
              !< - Scalar ratio R = gunl(xhat) / g(xhat,ẋ), bounded [0,1] then
              !    scaling of f(i,1:3) and dfdlam(i,1:3)
              !<================================================================
              do jj = 1, ne_unload
                i = indx_unl(jj)
                !< Compute ratioR = gunl / g for dominant direction
                ratioR(i) = zero
                if (abs(g(i,1)) > em20) ratioR(i) = gunl(i) / g(i,1)
                ratioR(i) = max(zero, min(one, ratioR(i)))
!
                !< Blend with previous ratioR for smoothness
                amax  = max(zero, min(one, uvar(i,nv_base+9 + jdom(i))))
                Rblend = (one - amax**3)*uvar(i,nv_base+13)+(amax**3)*ratioR(i)
                ratioR(i) = max(zero, min(one, Rblend))
!
                !< Scale loading functions f by ratioR
                f(i,1) = f(i,1) * ratioR(i)
                f(i,2) = f(i,2) * ratioR(i)
                f(i,3) = f(i,3) * ratioR(i)
!
                !< Scale loading function derivatives dfdlam by ratioR
                dfdlam(i,1) = dfdlam(i,1) * ratioR(i)
                dfdlam(i,2) = dfdlam(i,2) * ratioR(i)
                dfdlam(i,3) = dfdlam(i,3) * ratioR(i)
!
                !< Re-compute hydrostatic pressure and its derivative
                if (beta > zero) then
                  ldav = epspxx(i) + epspyy(i)
                  p(i) = uvar(i,12)*exp(-beta*tstep) +                         &
                         rbulk(i)*ldav*((one-exp(-beta*tstep))/beta)
                  dpdrv(i) = zero
                elseif (nu >= 0.49d0) then
                  p(i) = rbulk(i)*(rv(i)-one)
                  dpdrv(i) = rbulk(i)
                endif
!
                !< Compute current value of principal stresses with scaled 
                !  loading functions
                ! -> Incompressible-like case (nu <= 0 or nu >= 0.49)
                if (nu <= zero .or. nu >= 0.49d0) then 
                  t(i,1) = (two_third*f(i,1) - third *(f(i,2)+f(i,3)) + p(i))/rv(i)
                  t(i,2) = (two_third*f(i,2) - third *(f(i,1)+f(i,3)) + p(i))/rv(i)
                  t(i,3) = (two_third*f(i,3) - third *(f(i,1)+f(i,2)) + p(i))/rv(i)
                ! -> Compressible case (0 < nu < 0.49)
                else
                  t(i,1) = (f(i,1) - fJ(i)*ratioR(i))/rv(i)
                  t(i,2) = (f(i,2) - fJ(i)*ratioR(i))/rv(i)
                  t(i,3) = (f(i,3) - fJ(i)*ratioR(i))/rv(i)
                endif 
!
                !< Derivative of t3 w.r.t. λ3
                if (nu <= zero .or. nu >= 0.49d0) then 
                  dt3_dlam3 = (one/rv(i))*                                     &
                      ((four/nine)*dfdlam(i,3)*lam(i,3)/(lam(i,3)/rv_mth(i))   &
                      + (one/nine)*dfdlam(i,1)*lam(i,1)/(lam(i,3)/rv_mth(i))   &
                      + (one/nine)*dfdlam(i,2)*lam(i,2)/(lam(i,3)/rv_mth(i))   &
                      + (lam(i,1)/rv_mth(i))*(lam(i,2)/rv_mth(i))*             &
                                              (dpdrv(i) - t(i,3)))
                else
                  dt3_dlam3 = (one/rv(i))*dfdlam(i,3) + (one/lam(i,3))*        &
                      dfJdx(i)*(nu/(one - two*nu))*(rv(i)**((nu-one)/          &
                      (one - two*nu))) - (one/lam(i,3))*t(i,3) 
                endif
                dt3_dlam3 = sign(max(abs(dt3_dlam3),em20) ,dt3_dlam3)
!
                !< Update the third stretch to enforce incompressibility
                !  Unnormalize λ_i
                lam(i,1) = lam(i,1)/rv_mth(i)
                lam(i,2) = lam(i,2)/rv_mth(i)
                lam(i,3) = lam(i,3)/rv_mth(i) - t(i,3)/dt3_dlam3
!
              enddo
            enddo
!
            !< Re-normalize λ_i and save ratioR in state variables
            do jj = 1, ne_unload
              i = indx_unl(jj)
              uvar(i,nv_base+13) = ratioR(i)
              lam(i,1) = lam(i,1)*rv_mth(i)
              lam(i,2) = lam(i,2)*rv_mth(i)
              lam(i,3) = lam(i,3)*rv_mth(i)
            enddo
!
          !<--------------------------------------------------------------------
          !< Unloading with hysteretic model based on the energy 
          !<--------------------------------------------------------------------
          case (2)
!
            do jj = 1, ne_unload
              i = indx_unl(jj)
              ratioR(i) = one - (ecurent(i)/emax(i))**shape
              ratioR(i) = one - (one - hys) * ratioR(i)
              t(i,1) = ratioR(i) * t(i,1)
              t(i,2) = ratioR(i) * t(i,2)
            enddo
!
          end select
        endif
!
        !=======================================================================
        !< - Compute global Cauchy stress tensor from principal values
        !=======================================================================
        do i = 1, nel
!
          !< Transform principal stresses t(i,*) back to global coordinates
          signxx(i) = t(i,1)*eigv(i,1,1)*eigv(i,1,1) +                        &
                      t(i,2)*eigv(i,1,2)*eigv(i,1,2)
          signyy(i) = t(i,1)*eigv(i,2,1)*eigv(i,2,1) +                        &
                      t(i,2)*eigv(i,2,2)*eigv(i,2,2)
          signxy(i) = t(i,1)*eigv(i,1,1)*eigv(i,2,1) +                        &
                      t(i,2)*eigv(i,1,2)*eigv(i,2,2)
          signyz(i) = sigoyz(i) + gs*shf(i)*depsyz(i)
          signzx(i) = sigozx(i) + gs*shf(i)*depszx(i)
!
          !< Estimate effective directional shear modulus for stability 
          !  (bulk vs. curve slope)
          gmax(i) = gs
          do j = 1,2
            !< Unloading behavior case
            select case (iunl_for)
              case (1)
                dlam_eff = merge(max(dfdlam(i,j),dgdlam(i,j)),dgdlam(i,j),     &
                                                      loadflg(i) == -one )
                !< Keep the positive slope only
                dlam_eff = max(dlam_eff, zero)
              case default
                dlam_eff = max( dgdlam(i,j), zero )
            end select
            !< Safety boundary on the denominator
            lamj  = max(lam(i,j),tiny(one))
            denom = nine*rbulk(i) - dlam_eff*lamj
            if (denom < em12) then
              dlam_eff = min(dlam_eff,(0.98d0*nine*rbulk(i))/lamj)
              denom    = nine*rbulk(i) - dlam_eff*lamj
            endif
            if (denom > zero) then
              gdir = three*rbulk(i)*dlam_eff*lamj / denom
              gmax(i) = max(gmax(i), gdir)
            endif
          enddo
          !< Update the sound speed
          a11 = gmax(i) + gdamp
          a11 = four*a11*(a11 + three*rbulk(i))/(four*a11 + three*rbulk(i))
          soundsp(i) = sqrt(a11/min(rho(i),rho0))
          !< Hourglass control factor
          et(i) = (gmax(i) + gdamp) / gs
        enddo
!
        !=======================================================================
        !< - Update user variables for energy and flags
        !=======================================================================
        do i = 1, nel
          !< New energy increment
          deint(i) = deint0(i) +                                               &
             half*(signxx(i)*depsxx(i)+signyy(i)*depsyy(i)+signxy(i)*depsxy(i))
          !< Update current and maximum energies
          ecurent(i) = max(em20, uvar(i,2) + deint(i))
          emax(i)    = max(uvar(i,1), ecurent(i))
          !< Update user variables
          uvar(i,1)  = emax(i)
          uvar(i,2)  = ecurent(i)
          uvar(i,3)  = epseq(i)
          uvar(i,5)  = loadflg(i)
          uvar(i,8)  = lam(i,3)/rv_mth(i)
          if (beta > zero) uvar(i,12) = p(i)
          !< Update shell thickness for 2D elements
          thkn(i) = thkn(i) + thkly(i)*thk0(i)*lam(i,3)/rv_mth(i)
        enddo
!
        !=======================================================================
        !< Frictional deviatoric damping stress tensor computation
        !=======================================================================
        do i = 1, nel
          !< Initialize the damping stress tensor
          ldav = depsxx(i) + depsyy(i)
          sigdxx(i)  = uvar(i,6) + two * gdamp * (depsxx(i) - third*ldav)
          sigdyy(i)  = uvar(i,7) + two * gdamp * (depsyy(i) - third*ldav)
          sigdxy(i)  = uvar(i,9) +       gdamp * depsxy(i)
          sigdeff(i) = half * (sigdxx(i)**2 + sigdyy(i)**2 ) + sigdxy(i)**2
          sigdeff(i) = sqrt(three * sigdeff(i))
          !< Limit deviatoric damping stress to cutoff value sigf
          scale = min( sigf / max(sigdeff(i), em20), one )
          sigdxx(i) = scale * sigdxx(i)
          sigdyy(i) = scale * sigdyy(i)
          sigdxy(i) = scale * sigdxy(i)
          !< Add damping stress to Cauchy stress
          signxx(i) = signxx(i) + sigdxx(i)
          signyy(i) = signyy(i) + sigdyy(i)
          signxy(i) = signxy(i) + sigdxy(i)
          !< Store updated damping stress in user variables for next step
          uvar(i,6) = sigdxx(i)
          uvar(i,7) = sigdyy(i)
          uvar(i,9) = sigdxy(i)
        enddo
!
        !=======================================================================
        !< Failure and damage softening computation
        !=======================================================================
        if (kfail > zero) then 
          !< Reset lists of elements
          nindx_dam = 0
          indx_dam(1:nel) = 0
          nindx_fail = 0
          indx_fail(1:nel) = 0
          !< Listing of damaging elements
          do i = 1, nel
            !< Unnormalized principal stretches
            lam(i,1) = lam(i,1)/rv_mth(i)
            lam(i,2) = lam(i,2)/rv_mth(i)
            lam(i,3) = lam(i,3)/rv_mth(i)
            !< Invariants of the stretch tensor
            i1(i) = lam(i,1)**2 + lam(i,2)**2 + lam(i,3)**2
            i2(i) = lam(i,1)**2 * lam(i,2)**2 +                                &
                    lam(i,2)**2 * lam(i,3)**2 +                                &
                    lam(i,3)**2 * lam(i,1)**2
            !< Failure criterion
            fcrit(i) = (i1(i) - three) + gam1*(i1(i) - three)**2 +             &
                                         gam2*(i2(i) - three) 
            !< List of elements that can be damaged
            if ((dmg(i) < one).and.(off(i) == one)) then 
              nindx_dam = nindx_dam + 1
              indx_dam(nindx_dam) = i
            endif
          enddo
          !< Damage computation for failing elements only
          do jj = 1, nindx_dam
            i = indx_dam(jj)
            !< No damage
            if (fcrit(i) <= (one - eh)*kfail) then
              dmg(i) = zero
            !< Partial damage
            elseif (((one - eh)*kfail < fcrit(i)).and.(fcrit(i) < kfail)) then
              dmg(i) = half*(one + cos(pi*(fcrit(i) - kfail)/(eh*kfail)))
            !< Full damage
            elseif (fcrit(i) >= kfail) then
              dmg(i) = one
              uvar(i,4) = uvar(i,4) + one
              if ((failip > 0).and.(int(uvar(i,4)) == failip)) then 
                nindx_fail = nindx_fail + 1
                indx_fail(nindx_fail) = i
                off(i) = four_over_5
              endif
            endif
            dmg(i) = max(min(dmg(i),one),zero)
          enddo
          !< Apply damage to Cauchy stress
          do i = 1,nel
            signxx(i) = (one - dmg(i))*signxx(i)
            signyy(i) = (one - dmg(i))*signyy(i)
            signxy(i) = (one - dmg(i))*signxy(i)
          enddo
          !< Printing element failure messages
          do jj = 1,nindx_fail
            i = indx_fail(jj)
            write(iout ,1000) ngl(i),tt
            write(istdo,1000) ngl(i),tt
          enddo
        endif
!
        !< Element failure messages formats
1000    format(1X,'-- RUPTURE (TABULATED_HYPERELASTIC) OF SHELL ELEMENT :',I10,' AT TIME :',1PE12.4)
!
      end subroutine sigeps88c
      end module sigeps88c_mod
