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
      !||    fail_gene1_b_mod   ../engine/source/materials/fail/gene1/fail_gene1_b.F90
      !||--- called by ------------------------------------------------------
      !||    fail_beam3         ../engine/source/elements/beam/fail_beam3.F
      !||====================================================================
    module fail_gene1_b_mod
    contains
! ======================================================================================================================
! \brief   gene1 failure criteria for type3 beam elements
! \details multiple failure models with different combinations with strain rate, thermal or mesh size dependency.
! ======================================================================================================================
      !||====================================================================
      !||    fail_gene1_b          ../engine/source/materials/fail/gene1/fail_gene1_b.F90
      !||--- called by ------------------------------------------------------
      !||    fail_beam3            ../engine/source/elements/beam/fail_beam3.F
      !||--- calls      -----------------------------------------------------
      !||    finter                ../engine/source/tools/curve/finter.F
      !||    table2d_vinterp_log   ../engine/source/tools/curve/table2d_vinterp_log.F
      !||    table_vinterp         ../engine/source/tools/curve/table_tools.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod          ../common_source/modules/constant_mod.F
      !||    elbufdef_mod          ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    interface_table_mod   ../engine/share/modules/table_mod.F
      !||    table_mod             ../engine/share/modules/table_mod.F
      !||====================================================================
    subroutine fail_gene1_b(                          &
           nel      ,nuparam  ,nuvar    ,nfunc    ,   &
           ifunc    ,npf      ,tf       ,             &
           time     ,timestep ,uparam   ,             &
           ngl      ,dt       ,epsp     ,uvar     ,   &
           off      ,                                 &
           area     ,f1       ,                       &
           epsxx    ,epsxy    ,epszx    ,             &
           temp     ,                                 &
           dfmax    ,aldt     ,table    ,             &
           ntablf   ,                                 &
           itablf   ,lf_dammx ,niparam  ,iparam   ,   &
           snpc     ,stf      ,ntable  )
!c-----------------------------------------------
!c   m o d u l e s
!c-----------------------------------------------
      use table_mod
      use interface_table_mod
      use elbufdef_mod 
      use constant_mod  
!c-----------------------------------------------
!c   i m p l i c i t   t y p e 
!c-----------------------------------------------    
      implicit none
!c-----------------------------------------------
!c   c o m m o n   b l o c k s
!c-----------------------------------------------
#include      "my_real.inc" 
#include      "units_c.inc" 
!c-----------------------------------------------
!c   d u m m y   a r g u m e n t s
!c-----------------------------------------------
      integer                     ,intent(in)     :: nel      ! size of element group
      integer                     ,intent(in)     :: nuparam  ! size of parameter array
      integer                     ,intent(in)     :: nuvar    ! size of user variable array
      integer                     ,intent(in)     :: nfunc    ! number of functions
      integer                     ,intent(in)     :: ntablf   ! number of table functions
      integer                     ,intent(in)     :: niparam  ! number of integer parameters
      integer                     ,intent(in)     :: lf_dammx ! maximum damage flag
      integer, dimension(nfunc)   ,intent(in)     :: ifunc    ! function identifiers
      integer, dimension(ntablf)  ,intent(in)     :: itablf   ! table function identifiers
      integer, dimension(nel)     ,intent(in)     :: ngl      ! element identifiers
      integer, dimension(niparam) ,intent(in)     :: iparam   ! integer parameters
      my_real                     ,intent(in)     :: time     ! current time
      my_real                     ,intent(in)     :: timestep ! current timestep
      my_real, dimension(nuparam) ,intent(in)     :: uparam   ! user parameters
      my_real, dimension(nel)     ,intent(in)     :: dt       ! time increment
      my_real, dimension(nel)     ,intent(in)     :: epsp     ! strain rate (confirmed by the tensstrain_criterion in solid element and beam3 element) 
      my_real, dimension(nel)     ,intent(in)     :: aldt     ! element size
      my_real, dimension(nel)     ,intent(in)     :: temp     ! temperature
      my_real ,dimension(nel)     ,intent(inout)    :: f1        ! force in local x direction
      my_real ,dimension(nel)     ,intent(in)    :: epsxx      !< strain increment component xx
      my_real ,dimension(nel)     ,intent(in)    :: epsxy      !< strain increment component xy
      my_real ,dimension(nel)     ,intent(in)    :: epszx      !< strain increment component xz
      my_real                     ,intent(in)    :: area       !< cross section area
      my_real, dimension(nel)     ,intent(inout)  :: off      ! offset
      my_real, dimension(nel, lf_dammx), intent(inout) :: dfmax      ! maximum damage
      my_real, dimension(nel, nuvar), intent(inout)    :: uvar       ! user variables
      type(ttable), dimension(ntable), intent(inout)   :: table      ! table data
      integer ,intent(in) :: snpc
      integer ,intent(in) :: stf
      integer ,intent(in) :: ntable
!c!-----------------------------------------------
!c!   variables for function interpolation 
!c!-----------------------------------------------
      integer, dimension(snpc), intent(in) :: npf
      my_real, dimension(stf), intent(in) :: tf
      my_real, external :: finter
!c-----------------------------------------------
!c   l o c a l   v a r i a b l e s
!c-----------------------------------------------
  integer :: i, j, nindx, nindx2,nstep, crit, nmod, &
     fct_ism, fct_ips, fct_idg12, fct_idg13, fct_ide1c, fct_idel, &
     ismooth, istrain, tab_idfld, itab, ncs, nindx3
  integer, dimension(nel) :: indx, indx2, indx3, ncrit, ipmax, ipmin, &
     is1max, itmax, imindt, isigmax, isigth, iepsmax, ieffeps, ivoleps, &
     imineps, ishear, imix12, imix13, imxe1c, ifld, imaxtemp
  integer, dimension(nel, 2) :: ipos
  my_real :: &
     minpres, maxpres, sigp1, tmax, dtmin, epsdot_sm, sigvm, sigth, &
     kf, epsdot_ps, maxeps, effeps, voleps, mineps, epssh, epsdot_fld, &
     volfrac, maxtemp, fscale_el, el_ref, lambda, fac, df, thin 
  my_real :: &
     e1, e4, e6, e42, e62, i1, i2, q, r, r_inter, phi, dav, e1d, e2d, e3d, e4d, e6d, denom,sxx,syy,szz
  my_real, dimension(nel) :: p, svm, e11, e22, e33, vol_strain, s11, s22, s33, eff_strain, &
     epsmax, sigmax, facl, sh12, sh13, e1c, e1fld, dfld, triax, signxx, signxy, signzx,hardr
  my_real, dimension(nel, 2) :: xvec
!c=======================================================================
!c=======================================================================
     !c user variables
     !c! user variable # 1,      regularization factors for length
     !c! user variable # 2,      t-butcher intg. value
     !c! user variable # 3,      epsxx
     !c! user variable # 4,      epsxy
     !c! user variable # 5,      not used for beam3
     !c! user variable # 6,      epszx
     !c! user variable # 8,      aldt(1:nel), initial beam length
!c===============================================================================================
 
!c=======================================================================
      !step1: recovering failure criterion parameters and initiation
      !=======================================================================
      ! - initialisation of computation on time step
      !=======================================================================
      ! recovering failure criterion parameters
      ! -> integer parameter, activated criteria
      crit       = iparam(1)
      itab       = iparam(2)    !> table dependency type (used only if tab_idfld is a table).
      nstep      = iparam(3)    !> number of cycles for the stress reduction, default = 10 (integer)
      ncs        = iparam(4)    !> number of conditions to reach before the element is deleted, default = 1 (integer)
      ismooth    = iparam(5)    !> interpolation type (in case of tabulated yield function) 
      istrain    = iparam(6)    !> engineering / true input strain flag
       ! -> real parameter, activated criteria     
      minpres    = uparam(1)    !> minimum pressure (positive in compression)
      maxpres    = uparam(2)    !> maximum pressure (positive in compression)
      sigp1      = uparam(3)    !> maximum principal stress
      tmax       = uparam(4)    !> failure time, default = 1e+20 (real)
      dtmin      = uparam(5)    !> minimum time step
      epsdot_sm  = uparam(6)    !> reference strain rate value for fct_idsm, default = 1 (real)
      sigvm      = uparam(7)   
      sigth      = uparam(8)
      kf         = uparam(9)    !>  -> tuler-butcher
      epsdot_ps  = uparam(10)   !> reference strain rate value for fct_idps, default = 1 (real)
      maxeps     = uparam(11)   !> ordinate scale factor for fct_idps or maximum principal strain if fct_idps is not defined, default = 1 
      effeps     = uparam(12)   !> maximum effective strain
      voleps     = uparam(13)   !> maximum volumetric strain
      mineps     = uparam(14)   !> minimum principal strain
      epssh      = uparam(15)   !> tensorial shear strain
      epsdot_fld = uparam(16)   !> reference strain rate value for tab_idfld.
      thin       = uparam(17)   !> thinning failure value (real)
      volfrac    = uparam(18)   !> damaged volume fraction to reach before the element is deleted (fully-integrated and higher order solid elements only), default = 0.5 (real)
      maxtemp    = uparam(20)   !> maximum temperature.
      fscale_el  = uparam(21)   !> element size function scale factor for fct_idel, tab_idfld (itab=2), fct_idg12, fct_idg23, fct_idg13 and fct_ide1c, default = 1.0 (real)
      el_ref     = uparam(22)   !> reference element size for fct_idel, tab_idfld (itab=2), fct_idg12, fct_idg23, fct_idg13 and fct_ide1c, default = 1.0 (real)

      !-> function & tables
      fct_ism    = ifunc(1)     !> function identifier of the maximum equivalent stress versus strain rate 
      fct_ips    = ifunc(2)     !> maximum principal strain vs strain-rate
      fct_idg12  = ifunc(3)     !> in-plane shear strain vs element size 
      fct_idg13  = ifunc(4)     !> in-plane shear strain vs element size
      fct_ide1c  = ifunc(5)     !> major in plane-strain vs element size
      fct_idel   = ifunc(6)     !> element size regularization

      if (ntablf > 0) then
        tab_idfld  = itablf(1)
      else
        tab_idfld  = 0
      end if


      ! initialization of variable
      nindx  = 0           !flag for: damage at an integration point
      nindx2 = 0           !flag for: rupture of solid element  at time
      nindx3 = 0           !flag for: damaged volume fraction > critical value
      indx(1:nel)     = 0  !table for the initiation of element damage
      indx2(1:nel)    = 0  !table for the element rupture
      indx3(1:nel)    = 0  !table for the element rupture due to volime fraction
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
      ! initiation of the calculation
      if (uvar(1,1)==zero) then !for the first step
        if (fct_idel > 0) then  !element size function scale, if needed 
          do i=1,nel   
            lambda      = aldt(i)/el_ref
            fac         = finter(fct_idel,lambda,npf,tf,df) 
            uvar(i,1)   = fac*fscale_el
          enddo
        else
          uvar(1:nel,1) = one
        endif
      endif  

      if (uvar(1,8) == zero) uvar(1:nel,8) = aldt(1:nel)
      if (uvar(1,5) == zero.and.(off(1) /= zero)) uvar(1:nel,5) = one

      ! checking element failure and recovering user variable
      do i=1,nel
        ! Integration point failure
        if (uvar(i,5) < one .and. uvar(i,5) >= em08) then 
          uvar(i,5) = uvar(i,5) - one/nstep
        endif
        if (uvar(i,5) <= em08) uvar(i,5) = zero
        if ((uvar(i,5) == zero) .and. off(i) == one) off(i) = zero
        ! integration point failure     
        f1(i) = uvar(i,5)*f1(i)
        signxx(i) = f1(i)/area   
        ! regularization factors for length, surface and volume
        facl(i)   = uvar(i,1)
      enddo
!c      
      !step2: computation of stress and strain 
      !====================================================================
      ! - loop over the element to compute the stresses and strains
      !====================================================================       
      do i=1,nel
!c              
        ! for active element 
        if ((uvar(i,5) == one).and.off(i)==one) then
          ! ----------------------------------------------------------------------------------------
          ! computation of volumetric strain, effective strain, shear strain and principal strains
          ! ----------------------------------------------------------------------------------------
          !  -> computation of tensiorial strain
          uvar(i,3) = uvar(i,3) + epsxx(i)
          uvar(i,4) = uvar(i,4) + epsxy(i)
          uvar(i,6) = uvar(i,6) + epszx(i)      
          !< for beam type 3 : epsyy = 0, epszz = 0, epsyz = 0  
          e1 = uvar(i,3) !epsxx
          e4 = uvar(i,4) !epsxy
          e6 = uvar(i,6) !epszx         

          !  -> computation of strain tensor invariants
          e42 = e4*e4
          e62 = e6*e6
          i1  = e1
          i2  = - e4*e4 - e6*e6
          !i3 = 0 

          ! -> computation of principal strains
          ! the value of e11 is verified, which is close to e1, as well as e22 and e33
          q   = (three*i2 - i1*i1)/nine
          r   = (two*i1*i1*i1-nine*i1*i2)/54.0     ! (2*i3^3-9*i1*i2+27*i3)/54  
          r_inter = min(r/sqrt(max(em20,(-q**3))),one)
          phi = acos(max(r_inter,-one))
          e11(i) = two*sqrt(-q)*cos(phi/three)+third*i1
          e22(i) = two*sqrt(-q)*cos((phi+two*pi)/three)+third*i1
          e33(i) = two*sqrt(-q)*cos((phi+four*pi)/three)+third*i1
          if (e11(i) < e22(i)) then 
            r_inter = e11(i)
            e11(i)  = e22(i)
            e22(i)  = r_inter
          endif 
          if (e22(i) < e33(i))then
            r_inter = e22(i)
            e22(i)  = e33(i)
            e33(i)  = r_inter
          endif
          if (e11(i) < e22(i))then
            r_inter = e11(i)
            e11(i)  = e22(i)
            e22(i)  = r_inter
          endif

          !  -> computation of volumetric strain
          vol_strain(i) = e11(i) + e22(i) + e33(i)
!c       
          !  -> computation of effective strain
          dav = epsxx(i)*third
          e1d = epsxx(i) - dav
          e2d = - dav
          e3d = - dav
          e4d = half*epsxy(i)

          e6d = half*epszx(i)
          eff_strain(i) = e1d**2 + e2d**2 + e3d**3 + two*(e4d**2 + e6d**2)
          eff_strain(i) = sqrt(two_third*eff_strain(i))

          ! --------------------------------------------------------------------------
          ! computation of hydrostatic stress, von mises stress and principal stresses
          ! --------------------------------------------------------------------------
          !  -> pressure stress (positive in compression)
          !  -> the stress calculation is good (confirmed by the test in the fail_beam3 subroutine)
          p(i)   = -third*(signxx(i))
          !  -> equivalent stress of von mises
          sxx    = signxx(i) + p(i)
          syy    = p(i)
          szz    = p(i)
          svm(i) = half*(sxx**2 + syy**2 + szz**2) &
               + signxy(i)**2 + signzx(i)**2 
          svm(i) = sqrt(three*svm(i)) !verified that it's identical to the one in the fail_beam3 subroutine             
          triax(i) = -p(i)/max(svm(i),em20)
          !  -> computing the principal stresses
          s11(i) = abs(signxx(i))
          s22(i) = 0.
          s33(i) = 0.
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
        endif
!c
      enddo
!c
      !  -> forming limit diagram
      !flag for function
      if (ntablf > 0) then 
        if (itab == 1) then 
          ! diagram using true strains
          if (istrain == 0) then 
            ! in-plane tabulation with strain-rate
            xvec(1:nel,1) = e22(1:nel)
            xvec(1:nel,2) = epsp(1:nel)/epsdot_fld
            !   -> tensile yield stress in direction 1 (md)
            ipos(1:nel,1:2) = 1
            call table2d_vinterp_log(table(tab_idfld),ismooth,nel,nel,ipos,xvec,e1fld,dfld,hardr)
          ! diagram using engineering strain
          else
            ! in-plane tabulation with strain-rate
            xvec(1:nel,1) = exp(e22(1:nel))-one
            xvec(1:nel,2) = epsp(1:nel)/epsdot_fld
            !   -> tensile yield stress in direction 1 (md)
            ipos(1:nel,1:2) = 1
            call table2d_vinterp_log(table(tab_idfld),ismooth,nel,nel,ipos,xvec,e1fld,dfld,hardr)
            e1fld = log(one + e1fld)
          endif
        else
          ! diagram using true strains
          if (istrain == 0) then 
            ! in-plane tabulation with strain-rate
            xvec(1:nel,1) = e22(1:nel)
            xvec(1:nel,2) = aldt(1:nel)/el_ref
            !   -> tensile yield stress in direction 1 (md)
            ipos(1:nel,1:2) = 1
            call table_vinterp(table(tab_idfld),nel,nel,ipos,xvec,e1fld,dfld)
          ! diagram using engineering strains
          else
            ! in-plane tabulation with strain-rate
            xvec(1:nel,1) = exp(e22(1:nel))-one
            xvec(1:nel,2) = aldt(1:nel)/el_ref
            !   -> tensile yield stress in direction 1 (md)
            ipos(1:nel,1:2) = 1
            call table_vinterp(table(tab_idfld),nel,nel,ipos,xvec,e1fld,dfld)
            e1fld = log(one + e1fld)
          endif
        endif
      endif 

!c    ! step4: check the criterion of failure 
      !====================================================================
      ! - loop over the element to check the erosion criteria
      !====================================================================    
      do i = 1,nel
        nmod = 0
        if ((uvar(i,5) == one).and.off(i)==one) then
          !  -> minimum pressure
          if (btest(crit,1)) then 
            nmod = nmod + 1
            dfmax(i,1+nmod) = max(p(i)/(minpres*facl(i)),dfmax(i,1+nmod))
            dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
            if (p(i) <= minpres*facl(i)) then
              ncrit(i) = ncrit(i) + 1
              ipmin(i) = 1
            endif
          endif
          !  -> maximum pressure
          if (btest(crit,2)) then 
            nmod = nmod + 1
            dfmax(i,1+nmod) = max(p(i)/(maxpres*facl(i)),dfmax(i,1+nmod))
            dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
            if (p(i) >= maxpres*facl(i)) then
              ncrit(i) = ncrit(i) + 1
              ipmax(i) = 1
            endif
          endif
          !  -> maximal principal stress
          if (btest(crit,3)) then 
            nmod = nmod + 1
            ! (unrestricted)
            if (sigp1 > zero) then
              dfmax(i,1+nmod) = max(s11(i)/(sigp1*facl(i)),dfmax(i,1+nmod))
              dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)      
              if (s11(i) >= sigp1*facl(i)) then   
                ncrit(i)  = ncrit(i) + 1
                is1max(i) = 1
              endif
            !     (restricted to positive stress triaxialities)
            else 
              if (triax(i)>em10) then 
                dfmax(i,1+nmod) = max(s11(i)/(abs(sigp1)*facl(i)),dfmax(i,1+nmod))
                dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
                if (s11(i) >= abs(sigp1)*facl(i)) then 
                  ncrit(i)  = ncrit(i) + 1
                  is1max(i) = 1
                endif
              endif
            endif
          endif
          !  -> maximum time       
          if (btest(crit,4)) then 
            nmod = nmod + 1
            dfmax(i,1+nmod) = max(time/tmax,dfmax(i,1+nmod))
            dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)   
            if (time >= tmax) then
              ncrit(i) = ncrit(i) + 1
              itmax(i) = 1
            endif
          endif
          !  -> minimum timestep      
          if (btest(crit,5)) then 
            nmod = nmod + 1
            if (time > zero) then 
              dfmax(i,1+nmod) = max(dtmin/dt(i),dfmax(i,1+nmod))
              dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)  
              if ((dt(i) <= dtmin).and.(time > zero)) then 
                ncrit(i)  = ncrit(i) + 1
                imindt(i) = 1
              endif 
            endif
          endif
          !  -> equivalent stress
          if (btest(crit,6)) then
            nmod = nmod + 1 
            if (epsdot_sm /= zero) then 
              lambda    = epsp(i)/epsdot_sm
              sigmax(i) = finter(fct_ism,lambda,npf,tf,df) 
              sigmax(i) = sigmax(i)*sigvm       
            else
              sigmax(i) = sigvm
            endif
            dfmax(i,1+nmod) = max(svm(i)/(sigmax(i)*facl(i)),dfmax(i,1+nmod))
            dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)   
            if (svm(i) >= sigmax(i)*facl(i)) then 
              ncrit(i)   = ncrit(i) + 1  
              isigmax(i) = 1
            endif
          endif
          !  -> tuler-butcher      
          if (btest(crit,7)) then
            nmod = nmod + 1 
            dfmax(i,1+nmod) = max(uvar(i,2)/(kf*facl(i)),dfmax(i,1+nmod))
            dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)  
            if (s11(i) > sigth) then 
              uvar(i,2) = uvar(i,2) + timestep*(s11(i) - sigth)**2
              if (uvar(i,2) >= kf*facl(i)) then 
                ncrit(i)  = ncrit(i) + 1 
                isigth(i) = 1
              endif
            endif
          endif
          !  -> maximal principal strain
          if (btest(crit,8)) then
            nmod = nmod + 1 
            if (epsdot_ps /= zero) then 
              lambda    = epsp(i)/epsdot_ps    
              epsmax(i) = finter(fct_ips,lambda,npf,tf,df)
              epsmax(i) = epsmax(i)*maxeps
            else
              epsmax(i) = maxeps
            endif
            dfmax(i,1+nmod) = max(e11(i)/(epsmax(i)*facl(i)),dfmax(i,1+nmod))
            dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one) 
            if (e11(i) >= epsmax(i)*facl(i)) then 
              ncrit(i)   = ncrit(i) + 1       
              iepsmax(i) = 1
            endif  
          endif

          !  -> maximum effective strain  
          if (btest(crit,9)) then
            nmod = nmod + 1 
            dfmax(i,1+nmod) = max(eff_strain(i)/(effeps*facl(i)),dfmax(i,1+nmod))
            dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one) 
            if (eff_strain(i) >= effeps*facl(i)) then
              ncrit(i)   = ncrit(i) + 1
              ieffeps(i) = 1
            endif
          endif
          !  -> maximum volumetric strain         
          if (btest(crit,10)) then
            nmod = nmod + 1 
            if (voleps > zero) then 
              dfmax(i,1+nmod) = max(vol_strain(i)/(voleps*facl(i)),dfmax(i,1+nmod))
              dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)  
              if (vol_strain(i) >= voleps*facl(i)) then 
                ncrit(i)   = ncrit(i) + 1
                ivoleps(i) = 1
              endif
            else
              dfmax(i,1+nmod) = max(vol_strain(i)/(voleps*facl(i)),dfmax(i,1+nmod))
              dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one) 
              if (vol_strain(i) <= voleps*facl(i)) then 
                ncrit(i)   = ncrit(i) + 1
                ivoleps(i) = 1
              endif
            endif
          endif
          !  -> minimum principal strain          
          if (btest(crit,11)) then
            nmod = nmod + 1 
            if (e33(i) /= zero) then 
              dfmax(i,1+nmod) = max(mineps*facl(i)/(e33(i)),dfmax(i,1+nmod))
            endif
            dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)  
            if (e33(i) <= mineps*facl(i)) then
              ncrit(i) = ncrit(i) + 1
              imineps(i) = 1
            endif
          endif
          !  -> maximum tensorial shear strain  
          if (btest(crit,12)) then
            nmod = nmod + 1 
            dfmax(i,1+nmod) = max(((e11(i) - e33(i))/two)/(epssh*facl(i)),dfmax(i,1+nmod))
            dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)  
            if ((e11(i) - e33(i))/two >= epssh*facl(i)) then 
              ncrit(i)  = ncrit(i) + 1
              ishear(i) = 1
            endif 
          endif
          !  -> mixed mode 
          if (btest(crit,13)) then
            lambda  = uvar(i,8)/el_ref
            sh12(i) = finter(fct_idg12,lambda,npf,tf,df) 
            denom   = sign(max(abs(e11(i)),em20),e11(i))  
            nmod    = nmod + 1 
            if (((e22(i)/denom)<=-half).and.((e22(i)/denom)>=-two)) then
              dfmax(i,1+nmod) = max(((e11(i) - e22(i))/two)/(sh12(i)),dfmax(i,1+nmod))
              dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one) 
              if ((e11(i) - e22(i))/two >= sh12(i)) then 
                ncrit(i)  = ncrit(i) + 1
                imix12(i) = 1
              endif
            endif
          endif 
          if (btest(crit,14)) then 
            lambda  = uvar(i,8)/el_ref
            sh13(i) = finter(fct_idg13,lambda,npf,tf,df)
            denom   = sign(max(abs(e11(i)),em20),e11(i))  
            nmod    = nmod + 1
            if (((e22(i)/denom)<=one).and.((e22(i)/denom)>=-half)) then
              dfmax(i,1+nmod) = max(((e11(i) - e33(i))/two)/(sh13(i)),dfmax(i,1+nmod))
              dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one) 
              if ((e11(i) - e33(i))/two >= sh13(i)) then 
                ncrit(i)  = ncrit(i) + 1
                imix13(i) = 1
              endif
            endif
          endif
          if (btest(crit,15)) then
            lambda = uvar(i,8)/el_ref
            e1c(i) = finter(fct_ide1c,lambda,npf,tf,df) 
            denom  = sign(max(abs(e11(i)),em20),e11(i))  
            nmod   = nmod + 1 
            if (((e22(i)/denom)<=one).and.((e22(i)/denom)>=-half)) then
              dfmax(i,1+nmod) = max(e11(i)/e1c(i),dfmax(i,1+nmod))
              dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one) 
              if (e11(i) >= e1c(i)) then 
                ncrit(i)  = ncrit(i) + 1
                imxe1c(i) = 1
              endif
            endif
          endif
          !  -> forming limit diagram 
          if (btest(crit,16)) then 
            nmod = nmod + 1 
            if (itab == 1) then 
              dfmax(i,1+nmod) = max(e11(i)/(e1fld(i)*facl(i)),dfmax(i,1+nmod))
              dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)
              if (e11(i) >= e1fld(i)*facl(i)) then 
                ncrit(i) = ncrit(i) + 1
                ifld(i)  = 1
              endif
            else
              dfmax(i,1+nmod) = max(e11(i)/(e1fld(i)),dfmax(i,1+nmod))
              dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one) 
              if (e11(i) >= e1fld(i)) then
                ncrit(i) = ncrit(i) + 1
                ifld(i)  = 1
              endif
            endif
          endif
          !  -> maximum temperature
          if (btest(crit,18)) then 
            nmod = nmod + 1 
            dfmax(i,1+nmod) = max(temp(i)/maxtemp,dfmax(i,1+nmod))
            dfmax(i,1+nmod) = min(dfmax(i,1+nmod),one)  
            if (temp(i) >= maxtemp) then 
              ncrit(i)    = ncrit(i) + 1 
              imaxtemp(i) = 1
            endif
          endif
!c        
          !  -> checking failure
          do j = 1,nmod 
            dfmax(i,1) = max(dfmax(i,1),dfmax(i,1+j))
          enddo
          dfmax(i,1) = min(dfmax(i,1),one)
          if (ncrit(i) >= ncs) then 
            dfmax(i,1)  = one
            nindx       = nindx + 1
            indx(nindx) = i
            uvar(i,5)   = one - one/nstep
            f1(i) = uvar(i,5)*f1(i)
          endif
        endif
!c
      enddo
!c
!c------------------------
!c------------------------
      if (nindx > 0) then
        do j=1,nindx
          i = indx(j)     
          if (ncrit(i) == 1) then
            ! message: failure start criterion has been reached
            write(iout, 1000) ngl(i),time,ncrit(i)
            write(istdo,1000) ngl(i),time,ncrit(i)
            write(iout, 2000) ngl(i),time
            write(istdo,2000) ngl(i),time
          else
            ! message: failure start criteria have been reached
            write(iout, 1001) ngl(i),time,ncrit(i)
            write(istdo,1001) ngl(i),time,ncrit(i)
            write(iout, 2000) ngl(i),time
            write(istdo,2000) ngl(i),time
          endif
          if (ipmax(i) == 1) then 
            !message: hydrostatic pressure value > critical value
            write(iout, 1002) p(i),maxpres*facl(i)
            write(istdo,1002) p(i),maxpres*facl(i)
          endif
          if (ipmin(i) == 1) then
            !message: hydrostatic pressure value < critical value 
            write(iout, 1003) p(i),minpres*facl(i)
            write(istdo,1003) p(i),minpres*facl(i)
          endif
          if (is1max(i) == 1) then
            !message: 1st principal stress value > critical value
            write(iout, 1004) s11(i),abs(sigp1)*facl(i)
            write(istdo,1004) s11(i),abs(sigp1)*facl(i)
          endif
          if (itmax(i) == 1) then 
            !message: time value > critical value
            write(iout, 1005) time,tmax
            write(istdo,1005) time,tmax    
          endif
          if (imindt(i) == 1) then 
            !message: element timestep value: < critical value
            write(iout, 1006) dt(i),dtmin
            write(istdo,1006) dt(i),dtmin        
          endif
          if (isigmax(i) == 1) then 
            !message: equivalent stress value: > critical value
            write(iout, 1007) svm(i),sigmax(i)*facl(i)
            write(istdo,1007) svm(i),sigmax(i)*facl(i)    
          endif
          if (isigth(i) == 1) then 
            !message: t-butcher intg. value > critical value
            write(iout, 1008) uvar(i,2),kf*facl(i)
            write(istdo,1008) uvar(i,2),kf*facl(i)              
          endif
          if (iepsmax(i) == 1) then 
            !message: 1st principal strain value > critical value
            write(iout, 1009) e11(i),epsmax(i)*facl(i)
            write(istdo,1009) e11(i),epsmax(i)*facl(i)
          endif
          if (ieffeps(i) == 1) then
            !message: effective strain value > critical value
            write(iout, 1010) eff_strain(i),effeps*facl(i)
            write(istdo,1010) eff_strain(i),effeps*facl(i)
          endif
          if (ivoleps(i) == 1) then
            if (voleps >= zero) then 
              !message: volumetric strain value > critical value
              write(iout, 1011) vol_strain(i),voleps*facl(i)
              write(istdo,1011) vol_strain(i),voleps*facl(i)
            else
              !message: volumetric strain value < critical value
              write(iout, 1012) vol_strain(i),voleps*facl(i)
              write(istdo,1012) vol_strain(i),voleps*facl(i) 
            endif
          endif
          if (imineps(i) == 1) then 
            !message: 3rd principal strain value < critical value
            write(iout, 1013) e33(i),mineps*facl(i)
            write(istdo,1013) e33(i),mineps*facl(i)
          endif
          if (ishear(i) == 1) then 
            !message: max. shear strain value > critical value
            write(iout, 1014) (e11(i) - e33(i))/two,epssh*facl(i)
            write(istdo,1014) (e11(i) - e33(i))/two,epssh*facl(i)
          endif
          if (imix12(i) == 1) then
            !message: 1in-plane sh.strain value > critical value
            write(iout, 1015) (e11(i) - e22(i))/two,sh12(i)
            write(istdo,1015) (e11(i) - e22(i))/two,sh12(i)
          endif
          if (imix13(i) == 1) then
            !message: transv.  sh.strain 13 value > critical value
            write(iout, 1016) (e11(i) - e33(i))/two,sh13(i)
            write(istdo,1016) (e11(i) - e33(i))/two,sh13(i)
          endif
          if (imxe1c(i) == 1) then 
            !message: in-plane princ.strain value: > critical value
            write(iout, 1017) e11(i),e1c(i)
            write(istdo,1017) e11(i),e1c(i)           
          endif
          if (ifld(i) == 1) then 
            if (itab == 1) then 
              !message: 1st principal stress value > forming limit value 
              write(iout, 1018) e11(i),e1fld(i)*facl(i)
              write(istdo,1018) e11(i),e1fld(i)*facl(i)   
            else
              !message: 1st principal stress value > forming limit value 
              write(iout, 1018) e11(i),e1fld(i)
              write(istdo,1018) e11(i),e1fld(i) 
            endif
          endif
          if (imaxtemp(i) == 1) then 
            !message: temperature value > critical value
            write(iout, 1020) temp(i),maxtemp
            write(istdo,1020) temp(i),maxtemp
          endif
        end do
      end if    

!C------------------------
 1000 format(1X,'>> FOR BEAM ELEMENT NUMBER (GENE1) EL#',I10, &
              ', FAILURE START AT TIME: ',1PE12.4,', ',I3,' CRITERION HAS BEEN REACHED:')     
 1001 format(1X,'>> FOR BEAM ELEMENT NUMBER (GENE1) EL#',I10, &
             ', FAILURE START AT TIME: ',1PE12.4,', ',I3,' CRITERIA HAVE BEEN REACHED:')
 1002 format(1X,'HYDROSTATIC PRESSURE VALUE:  ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4) 
 1003 format(1X,'HYDROSTATIC PRESSURE VALUE:  ',1PE12.4,' < CRITICAL VALUE: ',1PE12.4) 
 1004 format(1X,'1ST PRINCIPAL STRESS VALUE:  ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4) 
 1005 format(1X,'TIME VALUE:                  ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4) 
 1006 format(1X,'ELEMENT TIMESTEP VALUE:      ',1PE12.4,' < CRITICAL VALUE: ',1PE12.4) 
 1007 format(1X,'EQUIVALENT STRESS VALUE:     ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4) 
 1008 format(1X,'T-BUTCHER INTG. VALUE:       ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)
 1009 format(1X,'1ST PRINCIPAL STRAIN VALUE:  ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)
 1010 format(1X,'EFFECTIVE STRAIN VALUE:      ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)
 1011 format(1X,'VOLUMETRIC STRAIN VALUE:     ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)
 1012 format(1X,'VOLUMETRIC STRAIN VALUE:     ',1PE12.4,' < CRITICAL VALUE: ',1PE12.4)
 1013 format(1X,'3RD PRINCIPAL STRAIN VALUE:  ',1PE12.4,' < CRITICAL VALUE: ',1PE12.4)
 1014 format(1X,'MAX. SHEAR STRAIN VALUE:     ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)
 1015 format(1X,'IN-PLANE SH.STRAIN 12 VALUE: ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)
 1016 format(1X,'TRANSV.  SH.STRAIN 13 VALUE: ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)
 1017 format(1X,'IN-PLANE PRINC.STRAIN VALUE: ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)
 1018 format(1X,'1ST PRINCIPAL STRESS VALUE:  ',1PE12.4,' > FORMING LIMIT VALUE : ',1PE12.4)
 1019 format(1X,'THINNING VALUE:              ',1PE12.4,' < CRITICAL VALUE: ',1PE12.4)
 1020 format(1X,'TEMPERATURE VALUE:           ',1PE12.4,' > CRITICAL VALUE: ',1PE12.4)  
 2000 format(1X,'DELETED BEAM ELEMENT ',I10,1X,'AT TIME :',1PE12.4)
!c
      return

      end subroutine fail_gene1_b 
    end module fail_gene1_b_mod