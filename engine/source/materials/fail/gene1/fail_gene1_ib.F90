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
!||    fail_gene1_ib_mod   ../engine/source/materials/fail/gene1/fail_gene1_ib.F90
!||--- called by ------------------------------------------------------
!||    fail_beam18         ../engine/source/elements/beam/fail_beam18.F
!||====================================================================
      module fail_gene1_ib_mod
      implicit none
      contains
! ======================================================================================================================
! \brief   gene1 failure criteria for type18 beam elements
! \details multiple failure models with different combinations with strain rate, thermal or mesh size dependency.
! ======================================================================================================================
!||====================================================================
!||    fail_gene1_ib         ../engine/source/materials/fail/gene1/fail_gene1_ib.F90
!||--- called by ------------------------------------------------------
!||    fail_beam18           ../engine/source/elements/beam/fail_beam18.F
!||--- calls      -----------------------------------------------------
!||    finter                ../engine/source/tools/curve/finter.F
!||    table2d_vinterp_log   ../engine/source/tools/curve/table2d_vinterp_log.F
!||    table_vinterp         ../engine/source/tools/curve/table_tools.F
!||--- uses       -----------------------------------------------------
!||    constant_mod          ../common_source/modules/constant_mod.F
!||    elbufdef_mod          ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    interface_table_mod   ../engine/share/modules/table_mod.F
!||    precision_mod         ../common_source/modules/precision_mod.F90
!||    table_mod             ../engine/share/modules/table_mod.F
!||====================================================================
        subroutine fail_gene1_ib(fail  ,                      &
          nel      ,nuvar    ,uvar     ,nvartmp  ,vartmp ,    &
          time     ,timestep ,ipg      ,                      &
          ngl      ,dt       ,epsp     ,                      &
          off      ,epsxx    ,epsxy    ,epszx    ,            &
          signxx   ,signxy   ,signzx   ,temp     ,            &
          dfmax    ,aldt     ,table    ,ntablf   ,itablf   ,  &
          lf_dammx ,ntable   ,foff     ,uelr , npg,  tdel)
!c-----------------------------------------------
!                                                    modules
!c-----------------------------------------------
          use table_mod
          use interface_table_mod
          use elbufdef_mod
          use constant_mod
          use precision_mod, only : WP
          use fail_param_mod
          use table_mat_vinterp_mod
!c-----------------------------------------------
!                                               c i m p l i c i t t y p e
!c-----------------------------------------------
          implicit none
#include      "units_c.inc"
!c-----------------------------------------------
!                                                  arguments s
!c-----------------------------------------------
          integer                     ,intent(in)     :: nel      ! size of element group
          integer                     ,intent(in)     :: nuvar    ! size of user variable array
          integer                     ,intent(in)     :: nvartmp
          integer                     ,intent(in)     :: ipg      ! current integration point
          integer                     ,intent(in)     :: ntablf   ! number of table functions
          integer                     ,intent(in)     :: lf_dammx ! maximum damage flag
          integer, dimension(ntablf)  ,intent(in)     :: itablf   ! table function identifiers
          integer, dimension(nel)     ,intent(in)     :: ngl      ! element identifiers
          integer, dimension(nel,nvartmp)   ,intent(inout)  :: vartmp
          real(kind=WP)                     ,intent(in)     :: time     ! current time
          real(kind=WP)                     ,intent(in)     :: timestep ! current timestep
          real(kind=WP), dimension(nel)     ,intent(in)     :: epsxx    ! strain component xx
          real(kind=WP), dimension(nel)     ,intent(in)     :: epsxy    ! strain component xy
          real(kind=WP), dimension(nel)     ,intent(in)     :: epszx    ! strain component zx
          real(kind=WP), dimension(nel)     ,intent(in)     :: dt       ! time increment
          real(kind=WP), dimension(nel)     ,intent(in)     :: epsp     ! strain rate (confirmed by the tensstrain_criterion in solid element and Beam3 element)
          real(kind=WP), dimension(nel)     ,intent(in)     :: aldt     !
          real(kind=WP), dimension(nel)     ,intent(in)     :: temp     ! temperature
          real(kind=WP), dimension(nel)     ,intent(inout)  :: signxx   ! stress component xx
          real(kind=WP), dimension(nel)     ,intent(inout)  :: signxy   ! stress component xy
          real(kind=WP), dimension(nel)     ,intent(inout)  :: signzx   ! stress component zx
          real(kind=WP), dimension(nel)     ,intent(inout)  :: off      ! offset
          integer, dimension(nel)     ,intent(inout)  :: foff     ! integration point deactivation flag
          integer                     ,intent(in)     :: npg
          real(kind=WP) ,dimension(nel)     ,intent(inout)  :: tdel     ! deactivation time

          real(kind=WP), dimension(nel, lf_dammx), intent(inout) :: dfmax      ! maximum damage
          real(kind=WP), dimension(nel, nuvar), intent(inout)    :: uvar       ! user variables
          type(ttable), dimension(ntable), intent(inout)   :: table      ! table data
          real(kind=WP), dimension(nel)     ,intent(inout)  :: uelr
          type (fail_param_) ,intent(in) :: fail    !< failure model data structure
          INTEGER ,INTENT(IN) :: NTABLE
!c-----------------------------------------------
!                                                  local variables
!c-----------------------------------------------
          integer :: i,ii,j, nindx, nindx_on,nstep, crit, nmod, &
            fct_ism, fct_ips, fct_idg12, fct_idg13, fct_ide1c, fct_idel, &
            ismooth, istrain, tab_idfld, itab, ncs, nindx3, nindx2,failip
          integer, dimension(nel) :: indx, indx_on,indx2, indx3, ncrit, ipmax, ipmin, &
            is1max, itmax, imindt, isigmax, isigth, iepsmax, ieffeps, ivoleps, &
            imineps, ishear, imix12, imix13, imxe1c, ifld, imaxtemp
          real(kind=WP) :: &
            minpres, maxpres, sigp1, tmax, dtmin, epsdot_sm, sigvm, sigth, &
            kf, epsdot_ps, maxeps, effeps, voleps, mineps, epssh, epsdot_fld, &
            volfrac, maxtemp, fscale_el, el_ref, fac, df, e52, e5, e5d, thin
          real(kind=WP) :: &
            e1, e2, e3, e4, e6, e42, e62, i1, i2, i3, sxx, syy, szz, &
            q, r, r_inter, phi, dav, e1d, e2d, e3d, e4d, e6d, psi, denom
          real(kind=WP), dimension(nel) :: p, svm, e11, e22, e33, vol_strain, s11, s22, s33, eff_strain, &
            epsmax, sigmax, facl, sh12, sh13, e1c, e1fld, dfld, triax, hardr
          real(kind=WP), dimension(nel, 2) :: xvec
          real(kind=WP) ,dimension(nel) :: xvec1,dydx
!c=======================================================================
!c=======================================================================
          !c user variables
          !c! user variable # 1,      regularization factors for length
          !c! user variable # 2,      t-butcher intg. value
          !c! user variable # 3,      not used in beam18
          !c! user variable # 4,      not used in beam18
          !c! user variable # 5,      for stress reduction with NSTEP
          !c! user variable # 6,      not used in beam18
          !c! user variable # 8,      aldt(1:nel), initial beam length
!c===============================================================================================
          !step1: recovering failure criterion parameters and initiation
          !=======================================================================
          ! - initialisation of computation on time step
          !=======================================================================
          ! recovering failure criterion parameters
          ! -> integer parameter, activated criteria
          crit       = fail%iparam(1)
          itab       = fail%iparam(2)    !> table dependency type (used only if tab_idfld is a table).
          nstep      = fail%iparam(3)    !> number of cycles for the stress reduction, default = 10 (integer)
          ncs        = fail%iparam(4)    !> number of conditions to reach before the element is deleted, default = 1 (integer)
          ismooth    = fail%iparam(5)    !> interpolation type (in case of tabulated yield function)
          istrain    = fail%iparam(6)    !> engineering / true input strain flag
          failip     = min(fail%iparam(7),npg) ! 	number of failed integration point prior to solid element deletion. default = 1 (integer)
          ! -> real parameters
          minpres    = fail%uparam(1)    !> minimum pressure (positive in compression)
          maxpres    = fail%uparam(2)    !> maximum pressure (positive in compression)
          sigp1      = fail%uparam(3)    !> maximum principal stress
          tmax       = fail%uparam(4)    !> failure time, default = 1e+20 (real)
          dtmin      = fail%uparam(5)    !> minimum time step
          epsdot_sm  = fail%uparam(6)    !> reference strain rate value for fct_idsm, default = 1 (real)
          sigvm      = fail%uparam(7)
          sigth      = fail%uparam(8)
          kf         = fail%uparam(9)    !> tuler-butcher
          epsdot_ps  = fail%uparam(10)   !> reference strain rate value for fct_idps, default = 1 (real)
          maxeps     = fail%uparam(11)   !> ordinate scale factor for fct_idps or maximum principal strain if fct_idps is not defined, default = 1
          effeps     = fail%uparam(12)   !> maximum effective strain
          voleps     = fail%uparam(13)   !> maximum volumetric strain
          mineps     = fail%uparam(14)   !> minimum principal strain
          epssh      = fail%uparam(15)   !> Tensorial shear strain
          epsdot_fld = fail%uparam(16)   !> Reference strain rate value for tab_IDfld.
          thin       = fail%uparam(17)   !> thinning failure value (real)
          volfrac    = fail%uparam(18)   !> damaged volume fraction to reach before the element is deleted (fully-integrated and higher order solid elements only), default = 0.5 (real)
          maxtemp    = fail%uparam(20)   !> maximum temperature.
          fscale_el  = fail%uparam(21)   !> element size function scale factor for fct_idel, tab_idfld (itab=2), fct_idg12, fct_idg23, fct_idg13 and fct_ide1c, default = 1.0 (real)
          el_ref     = fail%uparam(22)   !> reference element size for fct_idel, tab_idfld (itab=2), fct_idg12, fct_idg23, fct_idg13 and fct_ide1c, default = 1.0 (real)

          !c     function & tables
          fct_ism    = 1     !> function identifier of the maximum equivalent stress versus strain rate
          fct_ips    = 2     !> maximum principal strain vs strain-rate
          fct_idg12  = 3     !> in-plane shear strain vs element size
          fct_idg13  = 4     !> in-plane shear strain vs element size
          fct_ide1c  = 5     !> major in plane-strain vs element size
          fct_idel   = 6     !> element size regularization

          if (ntablf > 0) then
            tab_idfld  = itablf(1)
          else
            tab_idfld  = 0
          end if

          ! initialization of variable
          nindx  = 0  !flag for: damage at an integration point
          nindx2 = 0  !flag for: rupture of solid element at time
          nindx3 = 0  !flag for: damaged volume fraction > critical value
          indx(1:nel)     = 0 !Table for the initiation of element damage
          indx2(1:nel)    = 0 !Table for the element rupture
          indx3(1:nel)    = 0 !Table for the element rupture due to volime fraction
          ipmax(1:nel)    = 0  !flag for : hydrostatic pressure value > critical value
          ipmin(1:nel)    = 0  !flag for : hydrostatic pressure value < critical value
          is1max(1:nel)   = 0  !flag for : 1st principal stress value > critical value
          itmax(1:nel)    = 0  !flag for : time value > critical value
          imindt(1:nel)   = 0  !flag for : element timestep value < critical value
          isigmax(1:nel)  = 0  !flag for : equivalent stress value > critical value
          isigth(1:nel)   = 0  !flag for : t-butcher intg. value > critical value
          iepsmax(1:nel)  = 0  !flag for : 1st principal strain value > critical value
          ieffeps(1:nel)  = 0  !flag for : effective strain value > critical value
          ivoleps(1:nel)  = 0  !flag for : volumetric strain value > critical value
          imineps(1:nel)  = 0  !flag for : 3rd principal strain value < critical value
          ishear(1:nel)   = 0  !flag for : max. shear strain value > critical value
          imix12(1:nel)   = 0  !flag for : in-plane sh.strain 12 value > critical value
          imix13(1:nel)   = 0  !flag for : transv.  sh.strain 13 value > critical value
          imxe1c(1:nel)   = 0  !flag for : in-plane princ.strain value > critical value
          ifld(1:nel)     = 0  !flag for : 1st principal stress value > forming limit value
          imaxtemp(1:nel) = 0  !flag for : temperature value > critical value
          ncrit(1:nel)    = 0  !number of failure criterion
!c
!c
          ! at initial time, compute the element size regularization factor
          ! Initiation of the calculation
          if (uvar(1,1)==zero) then !for the first step
            if (fail%table4d(fct_idel)%notable > 0) then
              xvec1(:)  = aldt(1:nel)/el_ref
              call table_mat_vinterp(fail%table4d(fct_idel),nel,nel,vartmp(1:nel,1:1),xvec1,uvar(1:nel,1),dydx)
            else
              uvar(1:nel,1) = one
            end if
          end if

          if (uvar(1,5) == zero.and.(off(1) /= zero)) uvar(1:nel,5) = one
          if (uvar(1,8) == zero) uvar(1:nel,8) = aldt(1:nel)
!c
          ! checking element failure and recovering user variable
          do i=1,nel
            ! integration point failure
            if (uvar(i,5) < one .and. uvar(i,5) >= em08) then
              uvar(i,5) = uvar(i,5) - one/nstep
            end if

            if (uvar(i,5) <= em08) uvar(i,5) = zero
            signxx(i) = signxx(i)*uvar(i,5)
            signxy(i) = signxy(i)*uvar(i,5)
            signzx(i) = signzx(i)*uvar(i,5)
            ! regularization factors for length, surface and volume
            facl(i)   = uvar(i,1)
          end do
!c
          !step2: computation of stress and strain
          !====================================================================
          ! - loop over the element to compute the stresses and strains
          !====================================================================
          do i=1,nel
!c
            ! for active element or gauss point
            if ((off(i)==one).and.foff(i) == 1) then
              ! ----------------------------------------------------------------------------------------
              ! computation of volumetric strain, effective strain, shear strain and principal strains
              ! ----------------------------------------------------------------------------------------
              !  -> computation of tensiorial strain
              e1  = epsxx(i)
              e2  = 0.
              e3  = 0.
              e4  = epsxy(i)
              e5  = 0.
              e6  = epszx(i)
              !  -> computation of strain tensor invariants
              e42 = e4*e4
              e52 = e5*e5
              e62 = e6*e6
              i1  = e1 + e2 + e3
              i2  = e1*e2 + e2*e3 + e3*e1 - e4*e4 - e5*e5 - e6*e6
              i3  = e1*e2*e3 - e1*e52 - e2*e62 - e3*e42 + two*e4*e5*e6
              !  -> computation of principal strains
              q   = (three*i2 - i1*i1)/nine
              r   = (two*i1*i1*i1-nine*i1*i2+twenty7*i3)/54.0     ! (2*i3^3-9*i1*i2+27*i3)/54
              r_inter = min(r/sqrt(max(em20,(-q**3))),one)
              phi = acos(max(r_inter,-one))
              e11(i) = two*sqrt(-q)*cos(phi/three)+third*i1
              e22(i) = two*sqrt(-q)*cos((phi+two*pi)/three)+third*i1
              e33(i) = two*sqrt(-q)*cos((phi+four*pi)/three)+third*i1
              if (e11(i) < e22(i)) then
                r_inter = e11(i)
                e11(i)  = e22(i)
                e22(i)  = r_inter
              end if
              if (e22(i) < e33(i))then
                r_inter = e22(i)
                e22(i)  = e33(i)
                e33(i)  = r_inter
              end if
              if (e11(i) < e22(i))then
                r_inter = e11(i)
                e11(i)  = e22(i)
                e22(i)  = r_inter
              end if
              !  -> computation of volumetric strain
              vol_strain(i) = e11(i) + e22(i) + e33(i)
!c
              !  -> computation of effective strain
              dav = (epsxx(i)+0.+0.)*third
              e1d = epsxx(i) - dav
              e2d = 0. - dav
              e3d = 0. - dav
              e4d = epsxy(i)
              e5d = 0.
              e6d = epszx(i)
              eff_strain(i) = e1d**2 + e2d**2 + e3d**2 + two*(e4d**2 + e5d**2 + e6d**2)
              eff_strain(i) = sqrt(two_third*eff_strain(i))

!c******************************************************************about strain finished
              ! --------------------------------------------------------------------------
              ! computation of hydrostatic stress, von mises stress and principal stresses
              ! --------------------------------------------------------------------------
              !  -> pressure stress (positive in compression)
              p(i)   = -third*(signxx(i) + 0. + 0.)
              !  -> equivalent stress of von mises
              sxx    = signxx(i) + p(i)
              syy    =  p(i)
              szz    =  p(i)
              svm(i) = half*(sxx**2 + syy**2 + szz**2) &
                + signxy(i)**2 + signzx(i)**2
              svm(i) = sqrt(three*svm(i))
              triax(i) = -p(i)/max(svm(i),em20)
              !  -> computing the principal stresses
              i1 = signxx(i)
              i2 = -signxy(i)*signxy(i)-signzx(i)*signzx(i)
              !i3 = 0.
              q  = (three*i2 - i1*i1)/nine
              r  = (two*i1*i1*i1-nine*i1*i2)/54.0    ! (2*i3^3-9*i1*i2+27*i3)/54
              r_inter = min(r/sqrt(max(em20,(-q**3))),one)
              psi = acos(max(r_inter,-one))
              s11(i) = two*sqrt(-q)*cos(psi/three)+third*i1
              s22(i) = two*sqrt(-q)*cos((psi+two*pi)/three)+third*i1
              s33(i) = two*sqrt(-q)*cos((psi+four*pi)/three)+third*i1
              if (s11(i) < s22(i)) then
                r_inter = s11(i)
                s11(i)  = s22(i)
                s22(i)  = r_inter
              end if
              if (s22(i) < s33(i)) then
                r_inter = s22(i)
                s22(i)  = s33(i)
                s33(i)  = r_inter
              end if
              if (s11(i) < s22(i)) then
                r_inter = s11(i)
                s11(i)  = s22(i)
                s22(i)  = r_inter
              end if
!c
              ! for broken element or gauss point
            else
              e11(i) = zero
              e22(i) = zero
              e33(i) = zero
              vol_strain(i) = zero
              eff_strain(i) = zero
              p(i) = zero
              svm(i) = zero
              triax(i) = zero
              s11(i) = zero
              s22(i) = zero
              s33(i) = zero
            end if
!c
          end do
!c
          !  -> forming limit diagram
          !Flag for function
          if (ntablf > 0) then
            if (itab == 1) then
              ! diagram using true strains
              if (istrain == 0) then
                ! in-plane tabulation with strain-rate
                xvec(1:nel,1) = e22(1:nel)
                xvec(1:nel,2) = epsp(1:nel)/epsdot_fld
                !   -> tensile yield stress in direction 1 (md)
                call table2d_vinterp_log(table(tab_idfld),ismooth,nel,nel,vartmp(1:nel,7:8),xvec,e1fld,dfld,hardr)
                ! diagram using engineering strain
              else
                ! in-plane tabulation with strain-rate
                xvec(1:nel,1) = exp(e22(1:nel))-one
                xvec(1:nel,2) = epsp(1:nel)/epsdot_fld
                !   -> tensile yield stress in direction 1 (md)
                call table2d_vinterp_log(table(tab_idfld),ismooth,nel,nel,vartmp(1:nel,7:8),xvec,e1fld,dfld,hardr)
                e1fld = log(one + e1fld)
              end if
            else
              ! diagram using true strains
              if (istrain == 0) then
                ! in-plane tabulation with strain-rate
                xvec(1:nel,1) = e22(1:nel)
                xvec(1:nel,2) = aldt(1:nel)/el_ref
                !   -> tensile yield stress in direction 1 (md)
                call table_vinterp(table(tab_idfld),nel,nel,vartmp(1:nel,7:8),xvec,e1fld,dfld)
                ! diagram using engineering strains
              else
                ! in-plane tabulation with strain-rate
                xvec(1:nel,1) = exp(e22(1:nel))-one
                xvec(1:nel,2) = aldt(1:nel)/el_ref
                !   -> tensile yield stress in direction 1 (md)
                call table_vinterp(table(tab_idfld),nel,nel,vartmp(1:nel,7:8),xvec,e1fld,dfld)
                e1fld = log(one + e1fld)
              end if
            end if
          end if

!c    !step3: computation of stress and strain
!-------------------------------------------------------------------------------
          ! Tag active elements
          nindx_on = 0  
          do i=1,nel
            if (off(i) == one .and. foff(i)==one) then
              nindx_on = nindx_on + 1
              indx_on(nindx_on) = i  
            end if
          end do
          
          !====================================================================
          ! - loop over the element to check the erosion criteria
          !====================================================================
          nmod = 0

          if (btest(crit,1)) then
            nmod = nmod + 1
            do ii = 1,nindx_on
              i = indx_on(ii)
              dfmax(i,1+nmod) = max(p(i)/(minpres*facl(i)),dfmax(i,1+nmod))
              dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
              if (p(i) <= minpres*facl(i)) then
                ncrit(i) = ncrit(i) + 1
                ipmin(i) = 1
              end if
            end do
          end if
          !  -> maximum pressure
          if (btest(crit,2)) then
            nmod = nmod + 1
            do ii = 1,nindx_on
              i = indx_on(ii)
              dfmax(i,1+nmod) = max(p(i)/(maxpres*facl(i)),dfmax(i,1+nmod))
              dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
              if (p(i) >= maxpres*facl(i)) then
                ncrit(i) = ncrit(i) + 1
                ipmax(i) = 1
              end if
            end do
          end if
          !  -> maximal principal stress
          if (btest(crit,3)) then
            nmod = nmod + 1
            ! (unrestricted)
            do ii = 1,nindx_on
              i = indx_on(ii)
              if (sigp1 > zero) then
                dfmax(i,1+nmod) = max(s11(i)/(sigp1*facl(i)),dfmax(i,1+nmod))
                dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)

                if (s11(i) >= sigp1*facl(i)) then
                  ncrit(i)  = ncrit(i) + 1
                  is1max(i) = 1

                end if
                !     (restricted to positive stress triaxialities)
              else
                if (triax(i)>em10) then
                  dfmax(i,1+nmod) = max(s11(i)/(abs(sigp1)*facl(i)),dfmax(i,1+nmod))
                  dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
                  if (s11(i) >= abs(sigp1)*facl(i)) then
                    ncrit(i)  = ncrit(i) + 1
                    is1max(i) = 1
                  end if
                end if
              end if
            end do
          end if
          !  -> maximum time
          if (btest(crit,4)) then
            nmod = nmod + 1
            do ii = 1,nindx_on
              i = indx_on(ii)
              dfmax(i,1+nmod) = max(time/tmax,dfmax(i,1+nmod))
              dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
              if (time >= tmax) then
                ncrit(i) = ncrit(i) + 1
                itmax(i) = 1
              end if
            end do
          end if
          !  -> minimum timestep
          if (btest(crit,5)) then
            nmod = nmod + 1
            do ii = 1,nindx_on
              i = indx_on(ii)
              if (time > zero) then
                dfmax(i,1+nmod) = max(dtmin/dt(i),dfmax(i,1+nmod))
                dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
                if ((dt(i) <= dtmin).and.(time > zero)) then
                  ncrit(i)  = ncrit(i) + 1
                  imindt(i) = 1
                end if
              end if
            end do
          end if
          !  -> equivalent stress
          if (btest(crit,6)) then
            nmod = nmod + 1
            if (epsdot_sm /= zero) then
              xvec1(1:nel) = epsp(1:nel)/epsdot_sm
              call table_mat_vinterp(fail%table4d(fct_ism),nel,nel,vartmp(1:nel,2:2),xvec1,sigmax,dydx)
            else
              sigmax(1:nel) = sigvm
            end if
            do ii = 1,nindx_on
              i = indx_on(ii)
              dfmax(i,1+nmod) = max(svm(i)/(sigmax(i)*facl(i)),dfmax(i,1+nmod))
              dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
              if (svm(i) >= sigmax(i)*facl(i)) then
                ncrit(i)   = ncrit(i) + 1
                isigmax(i) = 1
              end if
            end do
          end if
          !  -> tuler-butcher
          if (btest(crit,7)) then
            nmod = nmod + 1
            do ii = 1,nindx_on
              i = indx_on(ii)
              dfmax(i,1+nmod) = max(uvar(i,2)/(kf*facl(i)),dfmax(i,1+nmod))
              dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
              if (s11(i) > sigth) then
                uvar(i,2) = uvar(i,2) + timestep*(s11(i) - sigth)**2
                if (uvar(i,2) >= kf*facl(i)) then
                  ncrit(i)  = ncrit(i) + 1
                  isigth(i) = 1
                end if
              end if
            end do
          end if
          !  -> maximal principal strain
          if (btest(crit,8)) then
            nmod = nmod + 1
            if (epsdot_ps /= zero) then
              xvec1(1:nel) = epsp(1:nel)/epsdot_ps
              call table_mat_vinterp(fail%table4d(fct_ips),nel,nel,vartmp(1:nel,3:3),xvec1,epsmax,dydx)
            else
              epsmax(1:nel) = maxeps
            end if
            do ii = 1,nindx_on
              i = indx_on(ii)
              dfmax(i,1+nmod) = max(e11(i)/(epsmax(i)*facl(i)),dfmax(i,1+nmod))
              dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
              if (e11(i) >= epsmax(i)*facl(i)) then
                ncrit(i)   = ncrit(i) + 1
                iepsmax(i) = 1
              end if
            end do
          end if

          !-> maximum effective strain
          if (btest(crit,9)) then
            nmod = nmod + 1
            do ii = 1,nindx_on
              i = indx_on(ii)
              dfmax(i,1+nmod) = max(eff_strain(i)/(effeps*facl(i)),dfmax(i,1+nmod))
              dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
              if (eff_strain(i) >= effeps*facl(i)) then
                ncrit(i)   = ncrit(i) + 1
                ieffeps(i) = 1
              end if
            end do
          end if
          !  -> maximum volumetric strain
          if (btest(crit,10)) then
            nmod = nmod + 1
            if (voleps > zero) then
              do ii = 1,nindx_on
                i = indx_on(ii)
                dfmax(i,1+nmod) = max(vol_strain(i)/(voleps*facl(i)),dfmax(i,1+nmod))
                dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
                if (vol_strain(i) >= voleps*facl(i)) then
                  ncrit(i)   = ncrit(i) + 1
                  ivoleps(i) = 1
                end if
              end do
            else
              do ii = 1,nindx_on
                i = indx_on(ii)
                dfmax(i,1+nmod) = max(vol_strain(i)/(voleps*facl(i)),dfmax(i,1+nmod))
                dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
                if (vol_strain(i) <= voleps*facl(i)) then
                  ncrit(i)   = ncrit(i) + 1
                  ivoleps(i) = 1
                end if
              end do
            end if
          end if
          !  -> minimum principal strain
          if (btest(crit,11)) then
            nmod = nmod + 1
            do ii = 1,nindx_on
              i = indx_on(ii)
              if (e33(i) /= zero) then
                dfmax(i,1+nmod) = max(mineps*facl(i)/(e33(i)),dfmax(i,1+nmod))
              end if
              dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
              if (e33(i) <= mineps*facl(i)) then
                ncrit(i) = ncrit(i) + 1
                imineps(i) = 1
              end if
            end do
          end if
          !  -> maximum tensorial shear strain
          if (btest(crit,12)) then
            nmod = nmod + 1
            do ii = 1,nindx_on
              i = indx_on(ii)
              dfmax(i,1+nmod) = max(((e11(i) - e33(i))/two)/(epssh*facl(i)),dfmax(i,1+nmod))
              dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
              if ((e11(i) - e33(i))/two >= epssh*facl(i)) then
                ncrit(i)  = ncrit(i) + 1
                ishear(i) = 1
              end if
            end do
          end if
          !  -> mixed mode
          if (btest(crit,13)) then
            nmod    = nmod + 1
            xvec1(1:nel)  = uvar(1:nel,8)/el_ref
            call table_mat_vinterp(fail%table4d(fct_idg12),nel,nel,vartmp(1:nel,4:4),xvec1,sh12,dydx)
            do ii = 1,nindx_on
              i = indx_on(ii)
              denom   = sign(max(abs(e11(i)),em20),e11(i))
              if (((e22(i)/denom)<=-half).and.((e22(i)/denom)>=-two)) then
                dfmax(i,1+nmod) = max(((e11(i) - e22(i))/two)/(sh12(i)),dfmax(i,1+nmod))
                dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
                if ((e11(i) - e22(i))/two >= sh12(i)) then
                  ncrit(i)  = ncrit(i) + 1
                  imix12(i) = 1
                end if
              end if
            end do
          end if
          if (btest(crit,14)) then
            nmod    = nmod + 1
            xvec1(1:nel)  = uvar(1:nel,8)/el_ref
            call table_mat_vinterp(fail%table4d(fct_idg13),nel,nel,vartmp(1:nel,5:5),xvec1,sh13,dydx)
            do ii = 1,nindx_on
              i = indx_on(ii)
              denom   = sign(max(abs(e11(i)),em20),e11(i))
              if (((e22(i)/denom)<=one).and.((e22(i)/denom)>=-half)) then
                dfmax(i,1+nmod) = max(((e11(i) - e33(i))/two)/(sh13(i)),dfmax(i,1+nmod))
                dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
                if ((e11(i) - e33(i))/two >= sh13(i)) then
                  ncrit(i)  = ncrit(i) + 1
                  imix13(i) = 1
                end if
              end if
            end do
          end if
          if (btest(crit,15)) then
            nmod   = nmod + 1
            xvec1(1:nel)  = uvar(1:nel,8)/el_ref
            call table_mat_vinterp(fail%table4d(fct_ide1c),nel,nel,vartmp(1:nel,6:6),xvec1,e1c,dydx)
            do ii = 1,nindx_on
              i = indx_on(ii)
              denom  = sign(max(abs(e11(i)),em20),e11(i))
              if (((e22(i)/denom)<=one).and.((e22(i)/denom)>=-half)) then
                dfmax(i,1+nmod) = max(e11(i)/e1c(i),dfmax(i,1+nmod))
                dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
                if (e11(i) >= e1c(i)) then
                  ncrit(i)  = ncrit(i) + 1
                  imxe1c(i) = 1
                end if
              end if
            end do
          end if
          !  -> forming limit diagram
          if (btest(crit,16)) then
            nmod = nmod + 1
            if (itab == 1) then
              do ii = 1,nindx_on
                i = indx_on(ii)
                dfmax(i,1+nmod) = max(e11(i)/(e1fld(i)*facl(i)),dfmax(i,1+nmod))
                dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
                if (e11(i) >= e1fld(i)*facl(i)) then
                  ncrit(i) = ncrit(i) + 1
                  ifld(i)  = 1
                end if
              end do
            else
              do ii = 1,nindx_on
                i = indx_on(ii)
                dfmax(i,1+nmod) = max(e11(i)/(e1fld(i)),dfmax(i,1+nmod))
                dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
                if (e11(i) >= e1fld(i)) then
                  ncrit(i) = ncrit(i) + 1
                  ifld(i)  = 1
                end if
              end do
            end if
          end if
          !  -> maximum temperature
          if (btest(crit,18)) then
            nmod = nmod + 1
            do ii = 1,nindx_on
              i = indx_on(ii)
              dfmax(i,1+nmod) = max(temp(i)/maxtemp,dfmax(i,1+nmod))
              dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
              if (temp(i) >= maxtemp) then
                ncrit(i)    = ncrit(i) + 1
                imaxtemp(i) = 1
              end if
            end do
          end if
!c
!---------------------------------------------------------------------
          !  -> checking failure
          do ii = 1,nindx_on
            i = indx_on(ii)
            do j = 1,nmod
              dfmax(i,1) = max(dfmax(i,1),dfmax(i,1+j))
            end do
            dfmax(i,1) = min(dfmax(i,1),one)
            if (ncrit(i) >= ncs) then
              uvar(i,5)   = uvar(i,5) - one/nstep
              signxx(i)   = signxx(i)*uvar(i,5)
              signxy(i)   = signxy(i)*uvar(i,5)
              signzx(i)   = signzx(i)*uvar(i,5)
              dfmax(i,1)  = one
              nindx       = nindx + 1
              indx(nindx) = i
              foff(i)=0
              tdel(I)     = time
              uelr(i)     = uelr(i) + one
              if (nint(uelr(i)) >= failip) then
                off(i)    = four_over_5
              end if
            end if
          end do
!c------------------------
!c------------------------
          if (nindx > 0) then
            do j=1,nindx
              i = indx(j)
              if (ncrit(i) == 1) then
                !message: failure start,  criterion has been reached
                write(iout, 1000) ngl(i),ipg,time,ncrit(i)
                write(istdo,1000) ngl(i),ipg,time,ncrit(i)
              else
                !message: failure start, criteria have been reached
                write(iout, 1001) ngl(i),ipg,time,ncrit(i)
                write(istdo,1001) ngl(i),ipg,time,ncrit(i)
              end if
              if (ipmax(i) == 1) then
                !message: hydrostatic pressure value > critical value
                write(iout, 1002) p(i),maxpres*facl(i)
                write(istdo,1002) p(i),maxpres*facl(i)
                if (off(i) == four_over_5) then
                  write(iout, 1111) ngl(i),time
                  write(istdo,1111) ngl(i),time
                end if
              end if
              if (ipmin(i) == 1) then
                !message: hydrostatic pressure value < critical value
                write(iout, 1003) p(i),minpres*facl(i)
                write(istdo,1003) p(i),minpres*facl(i)
                if (off(i) == four_over_5) then
                  write(iout, 1111) ngl(i),time
                  write(istdo,1111) ngl(i),time
                end if
              end if
              if (is1max(i) == 1) then
                !message: 1st principal stress value > critical value
                write(iout, 1004) s11(i),abs(sigp1)*facl(i)
                write(istdo,1004) s11(i),abs(sigp1)*facl(i)
                if (off(i) == four_over_5) then
                  write(iout, 1111) ngl(i),time
                  write(istdo,1111) ngl(i),time
                end if
              end if
              if (itmax(i) == 1) then
                !message: time value:                > critical value
                write(iout, 1005) time,tmax
                write(istdo,1005) time,tmax
                if (off(i) == four_over_5) then
                  write(iout, 1111) ngl(i),time
                  write(istdo,1111) ngl(i),time
                end if
              end if
              if (imindt(i) == 1) then
                !message: element timestep value:    < critical value
                write(iout, 1006) dt(i),dtmin
                write(istdo,1006) dt(i),dtmin
                if (off(i) == four_over_5) then
                  write(iout, 1111) ngl(i),time
                  write(istdo,1111) ngl(i),time
                end if
              end if
              if (isigmax(i) == 1) then
                !message: equivalent stress value:    > critical value
                write(iout, 1007) svm(i),sigmax(i)*facl(i)
                write(istdo,1007) svm(i),sigmax(i)*facl(i)
                if (off(i) == four_over_5) then
                  write(iout, 1111) ngl(i),time
                  write(istdo,1111) ngl(i),time
                end if
              end if
              if (isigth(i) == 1) then
                !message: t-butcher intg. value:      > critical value
                write(iout, 1008) uvar(i,2),kf*facl(i)
                write(istdo,1008) uvar(i,2),kf*facl(i)
                if (off(i) == four_over_5) then
                  write(iout, 1111) ngl(i),time
                  write(istdo,1111) ngl(i),time
                end if
              end if
              if (iepsmax(i) == 1) then
                !message: 1st principal strain value: > critical value
                write(iout, 1009) e11(i),epsmax(i)*facl(i)
                write(istdo,1009) e11(i),epsmax(i)*facl(i)
                if (off(i) == four_over_5) then
                  write(iout, 1111) ngl(i),time
                  write(istdo,1111) ngl(i),time
                end if
              end if
              if (ieffeps(i) == 1) then
                !message: effective strain value:     > critical value
                write(iout, 1010) eff_strain(i),effeps*facl(i)
                write(istdo,1010) eff_strain(i),effeps*facl(i)
                if (off(i) == four_over_5) then
                  write(iout, 1111) ngl(i),time
                  write(istdo,1111) ngl(i),time
                end if
              end if
              if (ivoleps(i) == 1) then
                if (voleps >= zero) then
                  !message: volumetric strain value:      > critical value
                  write(iout, 1011) vol_strain(i),voleps*facl(i)
                  write(istdo,1011) vol_strain(i),voleps*facl(i)
                  if (off(i) == four_over_5) then
                    write(iout, 1111) ngl(i),time
                    write(istdo,1111) ngl(i),time
                  end if
                else
                  !message: volumetric strain value:      < critical value
                  write(iout, 1012) vol_strain(i),voleps*facl(i)
                  write(istdo,1012) vol_strain(i),voleps*facl(i)
                  if (off(i) == four_over_5) then
                    write(iout, 1111) ngl(i),time
                    write(istdo,1111) ngl(i),time
                  end if
                end if
              end if
              if (imineps(i) == 1) then
                !message:  3rd principal strain value:   < critical value
                write(iout, 1013) e33(i),mineps*facl(i)
                write(istdo,1013) e33(i),mineps*facl(i)
                if (off(i) == four_over_5) then
                  write(iout, 1111) ngl(i),time
                  write(istdo,1111) ngl(i),time
                end if
              end if
              if (ishear(i) == 1) then
                !message:  max. shear strain value:      > critical value
                write(iout, 1014) (e11(i) - e33(i))/two,epssh*facl(i)
                write(istdo,1014) (e11(i) - e33(i))/two,epssh*facl(i)
                if (off(i) == four_over_5) then
                  write(iout, 1111) ngl(i),time
                  write(istdo,1111) ngl(i),time
                end if
              end if
              if (imix12(i) == 1) then
                !message:  in-plane sh.strain 12 value:  > critical value
                write(iout, 1015) (e11(i) - e22(i))/two,sh12(i)
                write(istdo,1015) (e11(i) - e22(i))/two,sh12(i)
                if (off(i) == four_over_5) then
                  write(iout, 1111) ngl(i),time
                  write(istdo,1111) ngl(i),time
                end if
              end if
              if (imix13(i) == 1) then
                !message:  transv.  sh.strain 13 value  > critical value
                write(iout, 1016) (e11(i) - e33(i))/two,sh13(i)
                write(istdo,1016) (e11(i) - e33(i))/two,sh13(i)
                if (off(i) == four_over_5) then
                  write(iout, 1111) ngl(i),time
                  write(istdo,1111) ngl(i),time
                end if
              end if
              if (imxe1c(i) == 1) then
                !message:  in-plane princ.strain value  > critical value
                write(iout, 1017) e11(i),e1c(i)
                write(istdo,1017) e11(i),e1c(i)
                if (off(i) == four_over_5) then
                  write(iout, 1111) ngl(i),time
                  write(istdo,1111) ngl(i),time
                end if
              end if
              if (ifld(i) == 1) then
                if (itab == 1) then
                  !message: 1st principal stress value > forming limit value
                  write(iout, 1018) e11(i),e1fld(i)*facl(i)
                  write(istdo,1018) e11(i),e1fld(i)*facl(i)
                  if (off(i) == four_over_5) then
                    write(iout, 1111) ngl(i),time
                    write(istdo,1111) ngl(i),time
                  end if
                else
                  !message: 1st principal stress value > forming limit value
                  write(iout, 1018) e11(i),e1fld(i)
                  write(istdo,1018) e11(i),e1fld(i)
                  if (off(i) == four_over_5) then
                    write(iout, 1111) ngl(i),time
                    write(istdo,1111) ngl(i),time
                  end if
                end if
              end if
              !endif
              if (imaxtemp(i) == 1) then
                !message: temperature value:      > critical value
                write(iout, 1020) temp(i),maxtemp
                write(istdo,1020) temp(i),maxtemp
                if (off(i) == four_over_5) then
                  write(iout, 1111) ngl(i),time
                  write(istdo,1111) ngl(i),time
                end if
              end if
            end do
          end if

!c------------------------
1000      format(1X,">> FOR BEAM ELEMENT NUMBER (GENE1) EL#",I10,", GAUSS POINT # ",I5, &
            ", FAILURE START AT TIME: ",1PE12.4,", ",I3," CRITERION HAS BEEN REACHED:")
1001      format(1X,">> FOR BEAM ELEMENT NUMBER (GENE1) EL#",I10,", GAUSS POINT # ",I5, &
            ", FAILURE START AT TIME: ",1PE12.4,", ",I3," CRITERIA HAVE BEEN REACHED:")
1002      format(1X,"HYDROSTATIC PRESSURE VALUE:  ",1PE12.4," > CRITICAL VALUE: ",1PE12.4)
1003      format(1X,"HYDROSTATIC PRESSURE VALUE:  ",1PE12.4," < CRITICAL VALUE: ",1PE12.4)
1004      format(1X,"1ST PRINCIPAL STRESS VALUE:  ",1PE12.4," > CRITICAL VALUE: ",1PE12.4)
1005      format(1X,"TIME VALUE:                  ",1PE12.4," > CRITICAL VALUE: ",1PE12.4)
1006      format(1X,"ELEMENT TIMESTEP VALUE:      ",1PE12.4," < CRITICAL VALUE: ",1PE12.4)
1007      format(1X,"EQUIVALENT STRESS VALUE:     ",1PE12.4," > CRITICAL VALUE: ",1PE12.4)
1008      format(1X,"T-BUTCHER INTG. VALUE:       ",1PE12.4," > CRITICAL VALUE: ",1PE12.4)
1009      format(1X,"1ST PRINCIPAL STRAIN VALUE:  ",1PE12.4," > CRITICAL VALUE: ",1PE12.4)
1010      format(1X,"EFFECTIVE STRAIN VALUE:      ",1PE12.4," > CRITICAL VALUE: ",1PE12.4)
1011      format(1X,"VOLUMETRIC STRAIN VALUE:     ",1PE12.4," > CRITICAL VALUE: ",1PE12.4)
1012      format(1X,"VOLUMETRIC STRAIN VALUE:     ",1PE12.4," < CRITICAL VALUE: ",1PE12.4)
1013      format(1X,"3RD PRINCIPAL STRAIN VALUE:  ",1PE12.4," < CRITICAL VALUE: ",1PE12.4)
1014      format(1X,"MAX. SHEAR STRAIN VALUE:     ",1PE12.4," > CRITICAL VALUE: ",1PE12.4)
1015      format(1X,"IN-PLANE SH.STRAIN 12 VALUE: ",1PE12.4," > CRITICAL VALUE: ",1PE12.4)
1016      format(1X,"TRANSV.  SH.STRAIN 13 VALUE: ",1PE12.4," > CRITICAL VALUE: ",1PE12.4)
1017      format(1X,"IN-PLANE PRINC.STRAIN VALUE: ",1PE12.4," > CRITICAL VALUE: ",1PE12.4)
1018      format(1X,"1ST PRINCIPAL STRESS VALUE:  ",1PE12.4," > FORMING LIMIT VALUE : ",1PE12.4)
1020      format(1X,"TEMPERATURE VALUE:           ",1PE12.4," > CRITICAL VALUE: ",1PE12.4)
1111      format(1X,"DELETED BEAM ELEMENT ",I10,1X,"AT TIME :",1PE12.4)
          return

        end subroutine fail_gene1_ib
      end module fail_gene1_ib_mod
