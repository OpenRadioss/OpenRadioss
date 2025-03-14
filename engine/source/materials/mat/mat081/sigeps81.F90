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
      !||    sigeps81_mod   ../engine/source/materials/mat/mat081/sigeps81.F90
      !||--- called by ------------------------------------------------------
      !||    mulaw          ../engine/source/materials/mat_share/mulaw.F90
      !||====================================================================
      module sigeps81_mod
      contains
! ======================================================================================================================
! \brief   Drucker-Prager with cap hardening material law /MAT/LAW81 (DPRAG_CAP)
! \details Material law based on Drucker-Prager pressure dependent plastic model with cap hardening.
! ======================================================================================================================
      !||====================================================================
      !||    sigeps81           ../engine/source/materials/mat/mat081/sigeps81.F90
      !||--- called by ------------------------------------------------------
      !||    mulaw              ../engine/source/materials/mat_share/mulaw.F90
      !||--- calls      -----------------------------------------------------
      !||    vinter2            ../engine/source/tools/curve/vinter.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod       ../common_source/modules/constant_mod.F
      !||    matparam_def_mod   ../common_source/modules/mat_elem/matparam_def_mod.F90
      !||====================================================================
        subroutine sigeps81 (                                                  &
          nel     ,nuvar   ,uvar     ,matparam ,nfunc    ,ifunc   ,           &
          npf     ,tf      ,snpc     ,stf      ,rho0     ,rho     ,           &
          volume  ,amu     ,defp     ,soundsp  ,viscmax  ,dt1     ,           &
          depsxx  ,depsyy  ,depszz   ,depsxy   ,depsyz   ,depszx  ,           &
          sigoxx  ,sigoyy  ,sigozz   ,sigoxy   ,sigoyz   ,sigozx  ,           &
          signxx  ,signyy  ,signzz   ,signxy   ,signyz   ,signzx  ,           &
          sigvxx  ,sigvyy  ,sigvzz   ,nvartmp  ,vartmp   ,seq     ,           &
          et      )
!-------------------------------------------------------------------------------
!   M o d u l e s
!-------------------------------------------------------------------------------
          use matparam_def_mod
          use constant_mod
!-------------------------------------------------------------------------------
!   I m p l i c i t   T y p e s
!-------------------------------------------------------------------------------
          implicit none
#include  "my_real.inc"
!-------------------------------------------------------------------------------
!   D u m m y   A r g u m e n t s
!-------------------------------------------------------------------------------
          integer, intent(in) :: nel                           !< number of elements in the group
          integer, intent(in) :: nuvar                         !< number of user variables
          my_real, dimension(nel,nuvar), intent(inout) :: uvar !< user variables
          type(matparam_struct_), intent(in) :: matparam       !< material parameters data
          integer, intent(in) :: nfunc                         !< number of functions
          integer, dimension(nfunc), intent(in) :: ifunc       !< function index
          integer, intent(in) :: snpc                          !< number of parameters for each function
          integer, dimension(snpc), intent(in) :: npf          !< number of values for each function
          integer, intent(in) :: stf                           !< number of parameters
          my_real, dimension(stf), intent(in) :: tf            !< function parameters
          my_real, dimension(nel), intent(in) :: rho0          !< initial density
          my_real, dimension(nel), intent(in) :: rho           !< current density
          my_real, dimension(nel), intent(in) :: volume        !< integration point associated volume
          my_real, dimension(nel), intent(in) :: amu           !< element volumetric strain
          my_real, dimension(nel,2), intent(inout) :: defp     !< element plastic strain
          my_real, dimension(nel), intent(inout) :: soundsp    !< sound speed
          my_real, dimension(nel), intent(inout) :: viscmax    !< maximum viscosity
          my_real, intent(in) :: dt1                           !< time step
          my_real, dimension(nel), intent(in)  :: depsxx       !< strain increment component xx
          my_real, dimension(nel), intent(in)  :: depsyy       !< strain increment component yy
          my_real, dimension(nel), intent(in)  :: depszz       !< strain increment component zz
          my_real, dimension(nel), intent(in)  :: depsxy       !< strain increment component xy
          my_real, dimension(nel), intent(in)  :: depsyz       !< strain increment component yz
          my_real, dimension(nel), intent(in)  :: depszx       !< strain increment component zx
          my_real, dimension(nel), intent(in)  :: sigoxx       !< old stress component xx
          my_real, dimension(nel), intent(in)  :: sigoyy       !< old stress component yy
          my_real, dimension(nel), intent(in)  :: sigozz       !< old stress component zz
          my_real, dimension(nel), intent(in)  :: sigoxy       !< old stress component xy
          my_real, dimension(nel), intent(in)  :: sigoyz       !< old stress component yz
          my_real, dimension(nel), intent(in)  :: sigozx       !< old stress component zx
          my_real, dimension(nel), intent(out) :: signxx       !< new stress component xx
          my_real, dimension(nel), intent(out) :: signyy       !< new stress component yy
          my_real, dimension(nel), intent(out) :: signzz       !< new stress component zz
          my_real, dimension(nel), intent(out) :: signxy       !< new stress component xy
          my_real, dimension(nel), intent(out) :: signyz       !< new stress component yz
          my_real, dimension(nel), intent(out) :: signzx       !< new stress component zx
          my_real, dimension(nel), intent(out) :: sigvxx       !< viscous stress component xx
          my_real, dimension(nel), intent(out) :: sigvyy       !< viscous stress component yy
          my_real, dimension(nel), intent(out) :: sigvzz       !< viscous stress component zz
          integer, intent(in) :: nvartmp                       !< number of temporary variables
          integer, dimension(nel,nvartmp), intent(inout) :: vartmp !< temporary variables
          my_real, dimension(nel), intent(inout) :: seq        !< Von Mises equivalent stress
          my_real, dimension(nel), intent(out) :: et           !< for hourglass stiffness
!-------------------------------------------------------------------------------
!  L o c a l   V a r i a b l e s
!-------------------------------------------------------------------------------
          integer :: i,ii,soft_flag,iter,nindx,ntricomp
          integer, dimension(nel) :: indx,indxtricomp,ipos,iad,ilen
          integer, parameter :: niter = 3
          my_real :: kini,gini,tgphi,tgpsi,alpha,max_dilat,epspvol0,kwater,    &
            por0,sat0,u0,tolmu,viscfac,cini,capini
          my_real :: delta,depspd_dlam,depspv_dlam,df_dlam,dfdc,dfdp,dfdrc,    &
            dfdseq,dfdsig_dsigdlam,dfdsigxx,dfdsigyy,dfdsigzz,dfdsigxy,        &
            dfdsigyz,dfdsigzx,dgdp,dgdseq,dgdsigxx,dgdsigyy,dgdsigzz,dlam,     &
            dgdsigxy,dgdsigyz,dgdsigzx,drcdpa,drcdpb,dseqdsigxx,dseqdsigyy,    &
            dseqdsigzz,dseqdsigxy,dseqdsigyz,dseqdsigzx,dsigxxdlam,dsigyydlam, &
            dsigzzdlam,dsigxydlam,dsigyzdlam,dsigzxdlam,drcdp,ldav,trdgpds,    &
            fac,ddepspv,ddepspd,dpxx,dpyy,dpzz,dpxy,dpyz,dpzx,dfdpu,dgdpu,     &
            dftrcdepspv,ftrc,dmuwdpor,dpordepspv,drcdpu
          my_real, dimension(nel) :: k,g,lame,g2,c,pa,pb,p0,dcdepsp,dpbdepsp,  &
            deri,epspd,epspv,depspd,depspv,dcdepspd,dpbdepspv,dpadepspv,f,     &
            p,sxx,syy,szz,sxy,syz,szx,pu,rc,a,muw,u,dudmuw,por,epspv0,epspd0,  &
            young,dpudp,dpudu
!===============================================================================
!
          !=====================================================================
          ! - INITIALISATION OF COMPUTATION ON TIME STEP
          !=====================================================================
          !< Recovering integer model parameter
          soft_flag = matparam%iparam(1)  !< Softening flag
          !< Recovering real model parameter
          kini      = matparam%uparam(1)  !< Initial bulk modulus or scl. factor
          gini      = matparam%uparam(2)  !< Initial shear modulus or scl.factor
          tgphi     = matparam%uparam(3)  !< Friction angle
          tgpsi     = matparam%uparam(4)  !< Plastic flow potential angle
          cini      = matparam%uparam(5)  !< Initial material cohesion
          capini    = matparam%uparam(6)  !< Initial cap limit pressure
          alpha     = matparam%uparam(7)  !< Ratio Pa/Pb
          max_dilat = matparam%uparam(8)  !< Maximum dilatancy
          epspvol0  = matparam%uparam(9)  !< Initial volumetric plastic strain
          kwater    = matparam%uparam(10) !< Pore water bulk modulus
          por0      = matparam%uparam(11) !< Initial porosity
          sat0      = matparam%uparam(12) !< Initial saturation
          u0        = matparam%uparam(13) !< Initial pore water pressure
          tolmu     = matparam%uparam(14) !< Tolerance for capshift viscosity
          viscfac   = matparam%uparam(15) !< Viscosity factor
!
          !< Initialization of the volumetric plastic strain if needed
          if ((uvar(1,2) == zero).and.(epspvol0 /= zero).and.(sat0 > 0)) then
            defp(1:nel,2) = epspvol0
          endif
!
          !=====================================================================
          !< - RECOVERING USER VARIABLES AND STATE VARIABLES
          !=====================================================================
          epspd0(1:nel) = defp(1:nel,1) !< Initial deviatoric plastic strain
          epspv0(1:nel) = defp(1:nel,2) !< Initial volumetric plastic ptrain
          epspd(1:nel)  = epspd0(1:nel)
          epspv(1:nel)  = epspv0(1:nel)
          et(1:nel)     = one           !< Coefficient for hourglass
!
          !=====================================================================
          !< - RECOVERING ELASTIC PARAMETERS
          !=====================================================================
          !<  Bulk modulus computation
          !  -> Interpolated with volumetric plastic strain
          if (ifunc(1) > 0) then
            ipos(1:nel) = vartmp(1:nel,1)
            iad(1:nel)  = npf(ifunc(1)) / 2 + 1
            ilen(1:nel) = npf(ifunc(1)+1) / 2 - iad(1:nel) - ipos(1:nel)
            call vinter2(tf,iad,ipos,ilen,nel,epspv,deri,k)
            k(1:nel) = kini*k(1:nel)
            vartmp(1:nel,1) = ipos(1:nel)
            !  -> Constant value
          else
            k(1:nel) = kini
          endif
          !<  Shear modulus computation
          !   -> Interpolated with volumetric plastic strain
          if (ifunc(2) > 0) then
            ipos(1:nel) = vartmp(1:nel,2)
            iad(1:nel)  = npf(ifunc(2)) / 2 + 1
            ilen(1:nel) = npf(ifunc(2)+1) / 2 - iad(1:nel) - ipos(1:nel)
            call vinter2(tf,iad,ipos,ilen,nel,epspv,deri,g)
            g(1:nel) = gini*g(1:nel)
            vartmp(1:nel,2) = ipos(1:nel)
            !   -> Constant value
          else
            g(1:nel) = gini
          endif
          !< Young modulus
          do i = 1,nel
            young(i) = nine*k(i)*g(i)/(three*k(i)+g(i))
          enddo
          !< Two*shear modulus
          g2(1:nel) = two*g(1:nel)
          !< Lame coefficient
          lame(1:nel) = k(1:nel) - two_third*g(1:nel)
!
          !=====================================================================
          !< - COMPUTATION OF TRIAL STRESS TENSOR, VON MISES AND PRESSURE
          !=====================================================================
          do i=1,nel
            !< Trial Cauchy stress tensor
            ldav      = lame(i)*(depsxx(i) + depsyy(i) + depszz(i))
            signxx(i) = sigoxx(i) + g2(i)*depsxx(i) + ldav
            signyy(i) = sigoyy(i) + g2(i)*depsyy(i) + ldav
            signzz(i) = sigozz(i) + g2(i)*depszz(i) + ldav
            signxy(i) = sigoxy(i) +  g(i)*depsxy(i)
            signyz(i) = sigoyz(i) +  g(i)*depsyz(i)
            signzx(i) = sigozx(i) +  g(i)*depszx(i)
!
            !< Trial devriatoric stress tensor
            p(i)   = -third*(signxx(i) + signyy(i) + signzz(i))
            sxx(i) = signxx(i) + p(i)
            syy(i) = signyy(i) + p(i)
            szz(i) = signzz(i) + p(i)
            sxy(i) = signxy(i)
            syz(i) = signyz(i)
            szx(i) = signzx(i)
!
            !< Trial Von Mises stress
            seq(i) = three_half*(sxx(i)**2 + syy(i)**2 + szz(i)**2)            &
              +      three*(sxy(i)**2 + syz(i)**2 + szx(i)**2)
            seq(i) = sqrt(seq(i))
          enddo
!
          !=====================================================================
          !< - POROSITY COMPUTATION (IF ACTIVATED)
          !=====================================================================
          if (sat0 > zero) then
            do i = 1,nel
              !< Compute porosity according to volumetric plastic strain
              por(i) = one - (one - por0)*exp(epspv(i) - epspvol0)
              por(i) = max(por(i),em03)
              !< Compute water volume fraction
              muw(i) = (sat0*por0/max(por(i),em20))*amu(i)
              !< Compute pore water pressure and its derivative
              if (muw(i) >= tolmu) then
                u(i) = kwater*muw(i)
                dudmuw(i) = kwater
              elseif (muw(i) > -tolmu) then
                u(i) = (kwater/(four*tolmu))*(muw(i)+tolmu)**2
                dudmuw(i) = (kwater/(two*tolmu))*(muw(i)+tolmu)
              else
                u(i) = zero
                dudmuw(i) = zero
              endif
            enddo
          else
            muw(1:nel)    = -one
            u(1:nel)      = zero
            dudmuw(1:nel) = zero
          endif
!
          !=====================================================================
          !< - YIELD CRITERION VARIABLES COMPUTATION
          !=====================================================================
          !< Material cohesion
          !  -> Interpolated with deviatoric plastic strain
          if (ifunc(3) > 0) then
            ipos(1:nel) = vartmp(1:nel,3)
            iad(1:nel)  = npf(ifunc(3)) / 2 + 1
            ilen(1:nel) = npf(ifunc(3)+1) / 2 - iad(1:nel) - ipos(1:nel)
            call vinter2(tf,iad,ipos,ilen,nel,epspd,dcdepspd,c)
            c(1:nel) = cini*c(1:nel)
            dcdepspd(1:nel) = cini*dcdepspd(1:nel)
            vartmp(1:nel,3) = ipos(1:nel)
            !  -> Constant value
          else
            c(1:nel) = cini
            dcdepspd(1:nel) = zero
          endif
          !< Tri-traction return mapping (apex of the yield surface)
          do i = 1,nel
            if (p(i) <= -c(i)/tgphi) then
              depspv(i) = (p(i) + (c(i)/tgphi))/k(i)
              if (soft_flag == 1) depspv(i) = max(depspv(i),zero)
              epspv(i) = epspv0(i) + depspv(i)
              p(i) = -c(i)/tgphi
              signxx(i) = sxx(i) - p(i)
              signyy(i) = syy(i) - p(i)
              signzz(i) = szz(i) - p(i)
            endif
          enddo
!
          !< Cap limit pressure
          !  -> Interpolated with volumetric plastic strain
          if (ifunc(4) > 0) then
            ipos(1:nel) = vartmp(1:nel,4)
            iad(1:nel)  = npf(ifunc(4)) / 2 + 1
            ilen(1:nel) = npf(ifunc(4)+1) / 2 - iad(1:nel) - ipos(1:nel)
            call vinter2(tf,iad,ipos,ilen,nel,epspv,dpbdepspv,pb)
            pb(1:nel) = capini*pb(1:nel)
            dpbdepspv(1:nel) = capini*dpbdepspv(1:nel)
            vartmp(1:nel,4) = ipos(1:nel)
            !  -> Constant value
          else
            pb(1:nel) = capini
            dpbdepspv(1:nel) = zero
          endif
          !< Transition pressure yield surface to cap
          pa(1:nel) = alpha*pb(1:nel)
          dpadepspv(1:nel) = alpha*dpbdepspv(1:nel)
          !< Null criterion derivative pressure (dfdp = 0)
          do i = 1,nel
            delta = (pa(i)*tgphi + c(i))**2 + eight*((pb(i)-pa(i))**2)*(tgphi**2)
            if (tgphi > zero) then
              p0(i) = pa(i) + (-(pa(i)*tgphi+c(i)) + sqrt(delta))/(four*tgphi)
            else
              p0(i) = pa(i)
            endif
          enddo
!
          !=====================================================================
          !< - COMPUTATION OF YIELD FUNCTION AND CHECK ELEMENT BEHAVIOR
          !=====================================================================
          !< Initialisation of the element indexes
          ntricomp = 0
          indxtricomp(1:nel) = 0
          nindx = 0
          indx(1:nel) = 0
          !< Preliminary computation to know the location:
          ! - on the yield surface
          ! - on the cap hardening surface
          do i=1,nel
            !< Taking into account pore water pressure
            if (p(i) < p0(i)) then
              pu(i)    = p(i)
              dpudp(i) = one
              dpudu(i) = zero
            elseif (p(i) - u(i) <= p0(i)) then
              pu(i)    = p0(i)
              dpudp(i) = zero
              dpudu(i) = zero
            else
              pu(i)    = p(i) - u(i)
              dpudp(i) =  one
              dpudu(i) = -one
            endif
            !< Transition surface to cap hardening factor
            if (pu(i)<=pa(i)) then
              rc(i) = one
            elseif (pu(i) >= pb(i)) then
              rc(i) = zero
              ntricomp = ntricomp + 1
              indxtricomp(ntricomp) = i
            else
              rc(i) = one - ((pu(i)-pa(i))/(pb(i)-pa(i)))**2
              rc(i) = sqrt(max(rc(i),zero))
            endif
            !< Check the yield condition
            a(i) = max(zero,pu(i)*tgphi + c(i))
            f(i) = seq(i) - rc(i)*a(i)
            if (f(i) >= zero) then
              nindx = nindx + 1
              indx(nindx) = i
            endif
          enddo
!
          !=====================================================================
          ! - RETURN MAPPING PROCEDURES (PLASTIC CORRECTION)
          !=====================================================================
!
          !< Tri-compression return mapping (cap hardening)
          if (ntricomp > 0) then
            !< Initialisation of the volumetric plastic strain increment
            epspv0(1:nel) = epspv(1:nel)
            depspv(1:nel) = zero
            !< Cutting plane algorithm to compute the cap hardening pressure
            do iter = 1, niter
              do ii = 1, ntricomp
                i = indxtricomp(ii)
                !< Tri-compression yield function
                ftrc = pu(i) - pb(i)
                !< Derivative of the yield function w.r.t volum. pl. strain
                dftrcdepspv = - k(i) - dpbdepspv(i)
                !< Take into account porosity if needed
                if (sat0 > 0) then
                  dmuwdpor    = -(sat0*por0*amu(i))/max((por(i)**2),em20)
                  dpordepspv  = (por0 - one)*exp(epspv(i) - epspvol0)
                  dftrcdepspv = dftrcdepspv - dudmuw(i)*dmuwdpor*dpordepspv
                endif
                !< Volumetric plastic strain increment and update
                dftrcdepspv = sign(min(max(abs(dftrcdepspv),em20),ep20),     &
                  dftrcdepspv)
                ddepspv = -ftrc/dftrcdepspv
                depspv(i) = depspv(i) + ddepspv
                if (soft_flag == 1) depspv(i) = max(depspv(i),zero)
                epspv(i) = epspv0(i) + depspv(i)
                !< Update material pressure
                p(i) = p(i) - k(i)*ddepspv
                !< Update pore water pressure (if activated)
                if (sat0 > zero) then
                  por(i) = one - (one - por0)*exp(epspv(i) - epspvol0)
                  por(i) = max(por(i),em03)
                  muw(i) = (sat0*por0/max(por(i),em20))*amu(i)
                  !< Compute pore water pressure and its derivative
                  if (muw(i) >= tolmu) then
                    u(i) = kwater*muw(i)
                    dudmuw(i) = kwater
                  elseif (muw(i) > -tolmu) then
                    u(i) = (kwater/(four*tolmu))*(muw(i)+tolmu)**2
                    dudmuw(i) = (kwater/(two*tolmu))*(muw(i)+tolmu)
                  else
                    u(i) = zero
                    dudmuw(i) = zero
                  endif
                endif
              enddo
              !< Update the cap pressure limit Pb if tabulated
              if (ifunc(4) > 0) then
                ipos(1:nel) = vartmp(1:nel,4)
                iad(1:nel)  = npf(ifunc(4)) / 2 + 1
                ilen(1:nel) = npf(ifunc(4)+1) / 2 - iad(1:nel) - ipos(1:nel)
                call vinter2(tf,iad,ipos,ilen,nel,epspv,dpbdepspv,pb)
                pb(1:nel) = capini*pb(1:nel)
                dpbdepspv(1:nel) = capini*dpbdepspv(1:nel)
                vartmp(1:nel,4) = ipos(1:nel)
                pa(1:nel) = alpha*pb(1:nel)
                dpadepspv(1:nel) = alpha*dpbdepspv(1:nel)
              endif
              do ii = 1, ntricomp
                i = indxtricomp(ii)
                !< Update null criterion derivative pressure (dfdp = 0)
                delta = (pa(i)*tgphi + c(i))**2 +                              &
                  eight*((pb(i)-pa(i))**2)*(tgphi**2)
                if (tgphi > zero) then
                  p0(i) = pa(i) + (-(pa(i)*tgphi+c(i)) +                       &
                    sqrt(delta))/(four*tgphi)
                else
                  p0(i) = pa(i)
                endif
                !< Update the shifted pressure
                if (p(i) < p0(i)) then
                  pu(i)    = p(i)
                  dpudp(i) = one
                  dpudu(i) = zero
                elseif (p(i) - u(i) <= p0(i)) then
                  pu(i)    = p0(i)
                  dpudp(i) = zero
                  dpudu(i) = zero
                else
                  pu(i)    = p(i) - u(i)
                  dpudp(i) = one
                  dpudu(i) = -one
                endif
              enddo
            enddo
            !< Compute accordingly the new related variables
            do ii = 1, ntricomp
              i = indxtricomp(ii)
              !< Update transition pressure yield surface to cap
              pa(i) = alpha*pb(i)
              dpadepspv(i) = alpha*dpbdepspv(i)
              !< Update null criterion derivative pressure (dfdp = 0)
              delta = (pa(i)*tgphi + c(i))**2 + eight*((pb(i)-pa(i))**2)*(tgphi**2)
              if (tgphi > zero) then
                p0(i) = pa(i) + (-(pa(i)*tgphi+c(i)) + sqrt(delta))/(four*tgphi)
              else
                p0(i) = pa(i)
              endif
              !< Update of the hydtostatic stress
              signxx(i) = sxx(i) - p(i)
              signyy(i) = syy(i) - p(i)
              signzz(i) = szz(i) - p(i)
              !< Coefficient for hourglass control
              et(i) = max(et(i),dpbdepspv(i)/(dpbdepspv(i) + k(i)))
            enddo
          endif
!
          !< Regular return mapping (yield surface)
          !< Cutting plane algorithm to compute the plastic correction
          if (nindx > 0) then
            !< Initialisation of the plastic correction
            depspv(1:nel) = zero
            depspd(1:nel) = zero
            epspv0(1:nel) = epspv(1:nel)
!
            !< Loop over the iterations
            do iter = 1, niter
              !< Loop over yielding elements
              do ii = 1, nindx
                i = indx(ii)
!
                ! Note: in this part, the purpose is to compute for each iteration
                ! a plastic multiplier allowing to update internal variables to
                ! satisfy the consistency condition using the cutting plane method
                ! within an iterative procedure.
                ! Its expression at each iteration is : dlambda = - f/df_dlambda
                ! -> f       : current value of yield function (known)
                ! -> df_dlam : derivative of f with respect to dlambda by taking
                !              into account of internal variables kinetic :
                !              plasticity, damage ... (to be computed)
!
                !< 1 - Derivative of yield criterion w.r.t plastic multiplier
                !      Contribution of the stress tensor
                !---------------------------------------------------------------
                !< Derivative of Von Mises stress w.r.t stress tensor
                dseqdsigxx = three_half*sxx(i)/max(seq(i),em20)
                dseqdsigyy = three_half*syy(i)/max(seq(i),em20)
                dseqdsigzz = three_half*szz(i)/max(seq(i),em20)
                dseqdsigxy =      three*sxy(i)/max(seq(i),em20)
                dseqdsigyz =      three*syz(i)/max(seq(i),em20)
                dseqdsigzx =      three*szx(i)/max(seq(i),em20)
!
                !< Derivative of yield criterion with respect to pressure and
                !  Von Mises stress
                dfdpu = -rc(i)*tgphi
                if ((pu(i) > pa(i)).and.(rc(i) > zero)) then
                  drcdpu = -(pu(i) - pa(i))/(rc(i)*(pb(i) - pa(i))**2)
                  dfdpu  = dfdpu - drcdpu*a(i)
                endif
                dfdp   = dfdpu*dpudp(i)
                dfdseq = one
!
                !< Derivative of plastic potential with respect to pressure and
                !  Von Mises stress
                if (pu(i) <= pa(i)) then
                  dgdpu = -tgpsi
                elseif (pu(i) <= p0(i)) then
                  dgdpu = -tgpsi*(one - ((pu(i) - pa(i))/(p0(i) - pa(i))))
                elseif (pu(i) > p0(i)) then
                  dgdpu = dfdpu
                endif
                dgdp   = dgdpu*dpudp(i)
                dgdseq = one
!
                !< Check if maximum dilatancy is reached
                !  (maximum dilatancy always negative)
                if (rho(i) <= (one + max_dilat)*rho0(i)) then
                  dgdp = max(zero,dgdp)
                  dfdp = max(zero,dfdp)
                endif
!
                !< Assembling derivative of yield criterion w.r.t stress tensor
                dfdsigxx = dfdseq*dseqdsigxx - third*dfdp
                dfdsigyy = dfdseq*dseqdsigyy - third*dfdp
                dfdsigzz = dfdseq*dseqdsigzz - third*dfdp
                dfdsigxy = dfdseq*dseqdsigxy
                dfdsigyz = dfdseq*dseqdsigyz
                dfdsigzx = dfdseq*dseqdsigzx
!
                !< Assembling derivative of plastic potential w.r.t stress tensor
                dgdsigxx = dgdseq*dseqdsigxx - third*dgdp
                dgdsigyy = dgdseq*dseqdsigyy - third*dgdp
                dgdsigzz = dgdseq*dseqdsigzz - third*dgdp
                dgdsigxy = dgdseq*dseqdsigxy
                dgdsigyz = dgdseq*dseqdsigyz
                dgdsigzx = dgdseq*dseqdsigzx
!
                !< Derivative of stress tensor w.r.t plastic multiplier
                trdgpds    =   dgdsigxx + dgdsigyy + dgdsigzz
                dsigxxdlam = -(dgdsigxx*g2(i) + lame(i)*trdgpds)
                dsigyydlam = -(dgdsigyy*g2(i) + lame(i)*trdgpds)
                dsigzzdlam = -(dgdsigzz*g2(i) + lame(i)*trdgpds)
                dsigxydlam = - dgdsigxy* g(i)
                dsigyzdlam = - dgdsigyz* g(i)
                dsigzxdlam = - dgdsigzx* g(i)
!
                !< Contribution of the stress tensor to the derivative of
                !  the yield criterion with respect to the plastic multiplier
                dfdsig_dsigdlam = dfdsigxx*dsigxxdlam + dfdsigyy*dsigyydlam +  &
                  dfdsigzz*dsigzzdlam + dfdsigxy*dsigxydlam +  &
                  dfdsigyz*dsigyzdlam + dfdsigzx*dsigzxdlam
!
                !< 2 - Derivative of yield criterion w.r.t pressures Pa and Pb
                !---------------------------------------------------------------
                dfdrc = -one
                if ((pu(i) > pa(i)).and.(rc(i) > zero)) then
                  drcdpb =  ((pu(i) - pa(i))**2)/(rc(i)*(pb(i) - pa(i))**3)
                  drcdpa = -((pu(i) - pa(i))*(pu(i) - pb(i)))/                 &
                    (rc(i)*(pb(i) - pa(i))**3)
                else
                  drcdpa = zero
                  drcdpb = zero
                endif
!
                !< 3 - Derivative of yield criterion w.r.t material cohesion
                !---------------------------------------------------------------
                dfdc = -rc(i)
!
                !< 4 - Derivative of deviatoric and volumetric plastic strain
                !      w.r.t plastic multiplier
                !---------------------------------------------------------------
                depspd_dlam = (signxx(i)*dgdsigxx + signyy(i)*dgdsigyy +       &
                  signzz(i)*dgdsigzz + signxy(i)*dgdsigxy +       &
                  signyz(i)*dgdsigyz + signzx(i)*dgdsigzx)        &
                  /max(seq(i),em20)
                depspv_dlam = -(dgdsigxx + dgdsigyy + dgdsigzz)
!
                !< 5 - Derivative of yield criterion w.r.t plastic multiplier
                !---------------------------------------------------------------
                df_dlam =  dfdsig_dsigdlam +                                   &
                  dfdrc*drcdpa*dpadepspv(i)*depspv_dlam +             &
                  dfdrc*drcdpb*dpbdepspv(i)*depspv_dlam +             &
                  dfdc*dcdepspd(i)*depspd_dlam
                !< Take into account water pressure if needed
                if (sat0 > zero) then
                  dmuwdpor = -(sat0*por0*amu(i))/max((por(i)**2),em20)
                  dpordepspv = (por0 - one)*exp(epspv(i) - epspvol0)
                  df_dlam = df_dlam +                                          &
                    dfdpu*dpudu(i)*dudmuw(i)*dmuwdpor*dpordepspv*depspv_dlam
                endif
                df_dlam = sign(min(max(abs(df_dlam),em20),ep20),df_dlam)
!
                !< 6 - Computation of plastic multiplier
                !---------------------------------------------------------------
                dlam = -f(i)/df_dlam
!
                !< 7 - Update plastic strain related variables
                !---------------------------------------------------------------
                !< Plastic strain tensor increment (on the current iteration)
                dpxx = dlam * dgdsigxx
                dpyy = dlam * dgdsigyy
                dpzz = dlam * dgdsigzz
                dpxy = dlam * dgdsigxy
                dpyz = dlam * dgdsigyz
                dpzx = dlam * dgdsigzx
                !< Deviatoric plastic strain update
                ddepspd   = depspd_dlam*dlam
                depspd(i) = depspd(i) + ddepspd
                epspd(i)  = epspd0(i) + max(depspd(i),zero)
                !< Volumetric plastic strain update
                ddepspv   = depspv_dlam*dlam
                depspv(i) = depspv(i) + ddepspv
                if (soft_flag == 1) depspv(i) = max(depspv(i),zero)
                epspv(i)  = epspv0(i) + depspv(i)
!
                !< 8 - Update stress tensor, pressure and Von Mises stress
                !---------------------------------------------------------------
                signxx(i) = signxx(i) - (g2(i)*dpxx - lame(i)*depspv(i))
                signyy(i) = signyy(i) - (g2(i)*dpyy - lame(i)*depspv(i))
                signzz(i) = signzz(i) - (g2(i)*dpzz - lame(i)*depspv(i))
                signxy(i) = signxy(i) -   g(i)*dpxy
                signyz(i) = signyz(i) -   g(i)*dpyz
                signzx(i) = signzx(i) -   g(i)*dpzx
!
                !< New pressure and deviatoric stress tensor
                p(i)   = -(signxx(i) + signyy(i) + signzz(i))/three
                sxx(i) = signxx(i) + p(i)
                syy(i) = signyy(i) + p(i)
                szz(i) = signzz(i) + p(i)
                sxy(i) = signxy(i)
                syz(i) = signyz(i)
                szx(i) = signzx(i)
!
                !< New Von Mises stress
                seq(i) = three_half*(sxx(i)**2 + syy(i)**2 + szz(i)**2)        &
                  +      three*(sxy(i)**2 + syz(i)**2 + szx(i)**2)
                seq(i) = sqrt(seq(i))
!
                !< Update pore water pressure (if activated)
                if (sat0 > zero) then
                  por(i) = one - (one - por0)*exp(epspv(i) - epspvol0)
                  por(i) = max(por(i),em03)
                  muw(i) = (sat0*por0/max(por(i),em20))*amu(i)
                  !< Compute pore water pressure and its derivative
                  if (muw(i) >= tolmu) then
                    u(i) = kwater*muw(i)
                    dudmuw(i) = kwater
                  elseif (muw(i) > -tolmu) then
                    u(i) = (kwater/(four*tolmu))*(muw(i)+tolmu)**2
                    dudmuw(i) = (kwater/(two*tolmu))*(muw(i)+tolmu)
                  else
                    u(i) = zero
                    dudmuw(i) = zero
                  endif
                endif
              enddo
!
              !< 9 - Update yield function value
              !-----------------------------------------------------------------
              !< Update material cohesion
              if (ifunc(3) > 0) then
                ipos(1:nel) = vartmp(1:nel,3)
                iad(1:nel)  = npf(ifunc(3)) / 2 + 1
                ilen(1:nel) = npf(ifunc(3)+1) / 2 - iad(1:nel) - ipos(1:nel)
                call vinter2(tf,iad,ipos,ilen,nel,epspd,dcdepspd,c)
                c(1:nel) = cini*c(1:nel)
                dcdepspd(1:nel) = cini*dcdepspd(1:nel)
                vartmp(1:nel,3) = ipos(1:nel)
              endif
!
              !< Update cap limit pressure
              if (ifunc(4) > 0) then
                ipos(1:nel) = vartmp(1:nel,4)
                iad(1:nel)  = npf(ifunc(4)) / 2 + 1
                ilen(1:nel) = npf(ifunc(4)+1) / 2 - iad(1:nel) - ipos(1:nel)
                call vinter2(tf,iad,ipos,ilen,nel,epspv,dpbdepspv,pb)
                pb(1:nel) = capini*pb(1:nel)
                dpbdepspv(1:nel) = capini*dpbdepspv(1:nel)
                vartmp(1:nel,4) = ipos(1:nel)
                pa(1:nel) = alpha*pb(1:nel)
                dpadepspv(1:nel) = alpha*dpbdepspv(1:nel)
              endif
!
              !< Loop over yielding elements
              do ii = 1,nindx
                i = indx(ii)
!
                !< Update null criterion derivative pressure (dfdp = 0)
                delta = (pa(i)*tgphi+c(i))**2+eight*((pb(i)-pa(i))**2)*(tgphi**2)
                if (tgphi > zero) then
                  p0(i) = pa(i)+(-(pa(i)*tgphi+c(i))+sqrt(delta))/(four*tgphi)
                else
                  p0(i) = pa(i)
                endif
!
                !< Update the shifted pressure
                if (p(i) < p0(i)) then
                  pu(i)    = p(i)
                  dpudp(i) = one
                  dpudu(i) = zero
                elseif (p(i) - u(i) <= p0(i)) then
                  pu(i)    = p0(i)
                  dpudp(i) = zero
                  dpudu(i) = zero
                else
                  pu(i)    = p(i) - u(i)
                  dpudp(i) =  one
                  dpudu(i) = -one
                endif
!
                !< Transition yield surface to cap hardening factor
                if (pu(i)<=pa(i)) then
                  rc(i) = one
                elseif (pu(i) >= pb(i)) then
                  rc(i) = zero
                else
                  rc(i) = one - ((pu(i)-pa(i))/(pb(i)-pa(i)))**2
                  rc(i) = sqrt(max(rc(i),zero))
                endif
                !< Pressure dependent factor
                a(i) = max(zero,pu(i)*tgphi + c(i))
                !< New yield function value
                f(i) = seq(i) - rc(i)*a(i)
              enddo
            enddo
!
            !< Update coefficient for hourglass
            do ii = 1, nindx
              i = indx(ii)
              if (depspv(i) > zero) then
                et(i) = max(et(i),dpbdepspv(i)/(dpbdepspv(i) + k(i)))
              endif
              if (depspd(i) > zero) then
                et(i) = max(et(i),dcdepspd(i)/(dcdepspd(i) + young(i)))
              endif
            enddo
!
          endif
          !=====================================================================
          ! - END OF PLASTIC CORRECTION WITH CUTTING PLANE METHOD
          !=====================================================================
!
          !< Update porosity variable
          if (sat0 > zero) then
            do i=1,nel
              viscmax(i) = zero
              !< fp_poro adding viscosity close to saturation
              if (muw(i) > -tolmu) then
                viscmax(i) = viscfac*(sqrt(kwater*rho(i))*(volume(i)**third))
                u(i) = u(i) - viscmax(i)*(depsxx(i)+depsyy(i)+depszz(i))/dt1
              endif
              !< fp_poro the pore pressure is stored in the viscous stress
              !< for practical reasons including compatibility with ale
              sigvxx(i) = -u(i)
              sigvyy(i) = -u(i)
              sigvzz(i) = -u(i)
            enddo
          endif
!
          !< Update user variables and compute the sound speed
          do i = 1,nel
            !< User variables
            uvar(i,1)  = epspd(i)        !< Deviatoric equivalent plastic strain
            uvar(i,2)  = epspv(i)        !< Volumetric plastic strain
            uvar(i,3)  = c(i)            !< Material cohesion
            uvar(i,4)  = pb(i)           !< Cap limit pressure
            uvar(i,5)  = pa(i)           !< Transition pressure yield surface to cap
            uvar(i,6)  = p0(i)           !< Null criterion derivative pressure (dfdp = 0)
            uvar(i,7)  = u(i)            !< Pore water pressure
            uvar(i,8)  = por(i)          !< Porosity
            uvar(i,9)  = muw(i) + one    !< Saturation
            uvar(i,10) = pu(i)           !< Cap shift
            !< Standard variables
            defp(1:nel,1) = epspd(1:nel) !< Deviatoric Equivalent Plastic Strain
            defp(1:nel,2) = epspv(1:nel) !< Volumetric Plastic Strain
            !< Sound speed in the material
            soundsp(i) = sqrt((k(i) + four_over_3*g(i) + dudmuw(i))/rho0(i))
          enddo
!
        end subroutine sigeps81
      end module sigeps81_mod
