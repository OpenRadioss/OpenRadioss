!copyright>        openradioss
!copyright>        copyright (c) 1986-2025 altair engineering inc.
!copyright>
!copyright>        this program is free software: you can redistribute it and/or modify
!copyright>        it under the terms of the gnu affero general public license as published by
!copyright>        the free software foundation, either version 3 of the license, or
!copyright>        (at your option) any later version.
!copyright>
!copyright>        this program is distributed in the hope that it will be useful,
!copyright>        but without any warranty; without even the implied warranty of
!copyright>        merchantability or fitness for a particular purpose.  see the
!copyright>        gnu affero general public license for more details.
!copyright>
!copyright>        you should have received a copy of the gnu affero general public license
!copyright>        along with this program.  if not, see <https://www.gnu.org/licenses/>.
!copyright>
!copyright>
!copyright>        commercial alternative: altair radioss software
!copyright>
!copyright>        as an alternative to this open-source version, altair also offers altair radioss
!copyright>        software under a commercial license.  contact altair to discuss further if the
!copyright>        commercial version may interest you: https://www.altair.com/radioss/.
      !||====================================================================
      !||    fail_tab2_ib_mod   ../engine/source/materials/fail/tabulated/fail_tab2_ib.F90
      !||--- called by ------------------------------------------------------
      !||    fail_beam18        ../engine/source/elements/beam/fail_beam18.F
      !||====================================================================
    module fail_tab2_ib_mod
    contains
! ======================================================================================================================
! \brief   tab2 failure criteria for type18 beam elements
! \details multiple failure models with different combinations with strain rate, thermal or mesh size dependency.
! ======================================================================================================================

      !||====================================================================
      !||    fail_tab2_ib          ../engine/source/materials/fail/tabulated/fail_tab2_ib.F90
      !||--- called by ------------------------------------------------------
      !||    fail_beam18           ../engine/source/elements/beam/fail_beam18.F
      !||--- calls      -----------------------------------------------------
      !||    finter                ../engine/source/tools/curve/finter.F
      !||    table_vinterp         ../engine/source/tools/curve/table_tools.F
      !||    vinter2               ../engine/source/tools/curve/vinter.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod          ../common_source/modules/constant_mod.F
      !||    elbufdef_mod          ../common_source/modules/mat_elem/elbufdef_mod.F90
      !||    interface_table_mod   ../engine/share/modules/table_mod.F
      !||    table_mod             ../engine/share/modules/table_mod.F
      !||====================================================================
    subroutine fail_tab2_ib(                                            &
             nel   ,nuparam   ,nuvar   ,nfunc   ,ifunc  ,               &
             npf   ,table     ,tf      ,time    ,uparam ,               &
             ngl   ,aldt      ,dpla    ,epsp    ,uvar   ,               & 
             signxx,signxy    ,signzx  ,                                &         
             temp  ,off       ,dfmax   ,tdele   ,dmg_scale ,            &
             ipg   ,npg       ,foff    ,ntablf    ,itablf,              &
             uelr  ,                                                    &
             snpc  ,stf       ,ntable      )
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
#include      "my_real.inc" 
#include      "units_c.inc" 

!c-----------------------------------------------
!c   i n p u t   a r g u m e n t s
!c-----------------------------------------------
      integer                     ,intent(in)     :: nel      ! size of element group
      integer                     ,intent(in)     :: nuparam  ! size of parameter array
      integer                     ,intent(in)     :: nuvar    ! size of user variable array
      integer, dimension(nel)     ,intent(in)     :: ngl      ! element identifiers
      integer                     ,intent(in)     :: npg      ! number of integration points
      integer                     ,intent(in)     :: ipg      ! current integration point
      integer                     ,intent(in)     :: ntablf   ! number of table functions
      integer, dimension(ntablf)  ,intent(in)     :: itablf   ! table function identifiers
      my_real, dimension(nel)     ,intent(inout)  :: uelr     ! integration point deactivation flag
      my_real                     ,intent(in)     :: time     ! current time
      my_real, dimension(nuparam) ,intent(in)     :: uparam   ! user parameters
      my_real, dimension(nel)     ,intent(in)     :: aldt     ! time increment
      my_real, dimension(nel)     ,intent(in)     :: dpla     ! plastic strain
      my_real, dimension(nel)     ,intent(in)     :: epsp     ! strain rate
      my_real, dimension(nel)     ,intent(in)     :: temp     ! temperature
      my_real, dimension(nel)     ,intent(in)     :: signxx   ! stress component xx
      my_real, dimension(nel)     ,intent(in)     :: signxy   ! stress component xy
      my_real, dimension(nel)     ,intent(in)     :: signzx   ! stress component zx
      my_real, dimension(nel, nuvar), intent(inout) :: uvar   ! user variables
      my_real, dimension(nel)     ,intent(inout)  :: dfmax    ! maximum damage
      my_real, dimension(nel)     ,intent(inout)  :: tdele    ! element deletion time
      my_real, dimension(nel)     ,intent(inout)  :: dmg_scale ! damage scale
      my_real, dimension(nel)     ,intent(inout)  :: off      ! offset
      integer, dimension(nel)     ,intent(inout)  :: foff     ! length offset
      type(ttable), dimension(ntable), intent(in) :: table    ! table data
      integer ,intent(in) :: snpc
      integer ,intent(in) :: stf
      integer ,intent(in) :: ntable
!c-----------------------------------------------
!c   variables for function interpolation 
!c-----------------------------------------------
      integer, dimension(snpc), intent(in) :: npf
      my_real, dimension(stf), intent(in) :: tf
      my_real, external :: finter
      integer                     ,intent(in)     :: nfunc    ! number of functions
      integer, dimension(nfunc)   ,intent(in)     :: ifunc    ! function identifiers

!c-----------------------------------------------
!c   l o c a l   v a r i a b l e s
!c-----------------------------------------------
      integer :: i, j, nindx, itab_epsf, failip, &
             itab_inst, itab_size, ireg, ndim, &
             log_scale1, log_scale2
      integer, dimension(nel) :: indx, ipos2, iad, ilen
      integer, dimension(nel, 3) :: ipos
      my_real :: fcrit, dn, dcrit, ecrit, exp_ref, expo, el_ref, &
             sr_ref1, fscale_el, shrf, biaxf, sr_ref2, &
             fscale_sr, cjc, fscale_dlim, temp_ref, fscale_temp
      my_real :: lambda, fac, df, dpl_def, cos3theta, det, p, svm, &
             sxx, syy, szz
      my_real, dimension(nel) :: inst, dc, l0, triax, xi, epsf, epsl, &
                 depsf, depsl, sizefac, ratefac, dsize, &
                 softexp, dlim, tempfac, tempfac2, dft, var
      my_real, dimension(nel, 3) :: xvec

!c=======================================================================
!c=======================================================================
     !c user variables
     !c! user variable # 1,      instability damage
     !c! user variable # 2,      necking critical damage 
     !c! user variable # 3,       element size 
!c===============================================================================================
      !step1: recovering failure criterion parameters and initiation
      !=======================================================================
      ! - initialisation of computation on time step
      !=======================================================================
      ! recovering failure criterion parameters
      fcrit         = uparam(1)                !> scale factor for failure plastic strain table
      failip        = min(nint(uparam(2)),npg) !> number of failed integration point prior to solid element deletion
      dn            = uparam(4)                !> damage accumulation exponent. default = 1.0 (real)
      dcrit         = uparam(5)                !> critical damage for stress softening triggering. default = 0.0 (real)
      ecrit         = uparam(6)                !> scale factor for necking plastic strain table identifier. (real)
      exp_ref       = uparam(7)                !> reference element size for stress softening exponent function. default = 1.0 (real)
      expo          = uparam(8)                !> scale factor for stress softening exponent function.default = 1.0 (real)
      ireg          = nint(uparam(9))          !> regularization flag for element size. default = 1 (integer)
      el_ref        = uparam(10)               !> reference element size for element size scaling table. default = 1.0 (real)
      sr_ref1       = uparam(11)               !> reference strain rate for size scaling table. default = 1.0 (real) 
      fscale_el     = uparam(12)               !> scale factor for element size scaling function. default = 1.0 (real)  
      shrf          = uparam(13)               !> lower stress triaxiality boundary for element size scaling. default = -1.0 (real)
      biaxf         = uparam(14)               !> upper stress triaxiality boundary for element size scaling. default = 1.0 (real)
      sr_ref2       = uparam(15)               !> reference strain rate for strain rate dependency function. default = 1.0 (real)
      fscale_sr     = uparam(16)               !> scale factor for strain rate dependency function. default = 1.0 (real)
      cjc           = uparam(17)               !> johnson-cook strain rate dependency factor.
      fscale_dlim   = uparam(18)               !> damage limit function scale factor. default = 1.0 (real)
      temp_ref      = uparam(19)               !> reference temperature for temperature dependency function. default = 0.0 (integer)
      fscale_temp   = uparam(20)               !> scale factor for temperature scaling function.
      log_scale1    = nint(uparam(21))
      log_scale2    = nint(uparam(22))

!c
      itab_epsf = itablf(1)                   !> plastic strain at failure table or function identifier
      itab_inst = itablf(2)                   !> instability (necking) plastic strain table or function identifier.
      itab_size = itablf(3)                   !> element size scaling table or function identifier.

  
!c  c
      ! checking element failure and recovering user variable
      do i=1,nel
        ! if necking control is activated
        if ((itab_inst > 0).or.(ecrit > zero)) then 
          ! instability damage
          inst(i) = uvar(i,1)
          ! necking critical damage 
          if (uvar(i,2) == zero) uvar(i,2) = one
          dc(i)   = uvar(i,2)
        endif
      end do   
!c      
      !c      
      !====================================================================
      ! - loop over the element to compute the stress state quantities
      !====================================================================       
      do i=1,nel
!c
        ! computation of hydrostatic stress, von mises stress, and stress triaxiality
        p   = third*(signxx(i))
        sxx = signxx(i) - p
        syy = 0. - p
        szz = 0. - p
        svm = half*(sxx**2 + syy**2 + szz**2)           &
             + signxy(i)**2 + signzx(i)**2 + 0.**2
        svm = sqrt(max(three*svm,zero))
        triax(i) = p/max(em20,svm)
        if (triax(i) < -one) triax(i) = -one
        if (triax(i) >  one) triax(i) = one
!c
        ! computation of lode parameter
        det  = sxx*syy*szz +0.                           &
           - 0.-szz*signxy(i)**2 - syy*signzx(i)**2
        cos3theta = half*twenty7*det/max(em20,svm**3)
        if (cos3theta < -one) cos3theta = -one
        if (cos3theta > one)  cos3theta = one
        xi(i) = one - two*acos(cos3theta)/pi
!c
      enddo
!c      
      !====================================================================
      ! - compute factors for element size, strain rate and temperature
      !====================================================================
      ! at initial time, save the element size 
      if (uvar(1,3) == zero) uvar(1:nel,3) = aldt(1:nel)
      l0(1:nel) = uvar(1:nel,3)
!c
      ! compute the softening exponent
      if (ifunc(1) > 0) then 
        do i=1,nel   
          lambda     = l0(i)/exp_ref
          softexp(i) = finter(ifunc(1),lambda,npf,tf,df) 
          softexp(i) = expo*softexp(i)
        enddo
      else
        softexp(1:nel) = expo
      endif
!c
      ! compute the temperature dependency factor
      if (ifunc(4) > 0) then 
        var(1:nel)   = temp(1:nel)/temp_ref
        ipos2(1:nel) = 1
        iad(1:nel)   = npf(ifunc(4)) / 2 + 1
        ilen(1:nel)  = npf(ifunc(4)+1) / 2 - iad(1:nel) - ipos2(1:nel)
        call vinter2(tf,iad,ipos2,ilen,nel,var,dft,tempfac)
        tempfac(1:nel) = fscale_temp*tempfac(1:nel)
        tempfac2(1:nel) = tempfac(1:nel)
      else
        tempfac(1:nel)  = one
        tempfac2(1:nel) = one
      endif
!c
      ! compute the element size regularization factor 
      if (itab_size > 0) then 
        ! element size scaling dependency
        ndim = table(itab_size)%ndim
        if (ireg == 1) then 
          select case (ndim)
            ! scale factor vs element size
            case(1)
              xvec(1:nel,1)   = l0(1:nel)/el_ref
              xvec(1:nel,2:3) = zero
              ipos(1:nel,1:3) = 1
            ! scale factor vs element size vs strain rate
            case(2)
              xvec(1:nel,1)   = l0(1:nel)/el_ref
              if (log_scale1 > 0) then 
                do i = 1,nel
                  xvec(i,2) = log(max(epsp(i),em20)/sr_ref1)
                enddo 
              else
                xvec(1:nel,2) = epsp(1:nel)/sr_ref1
              endif
              xvec(1:nel,3)   = zero
              ipos(1:nel,1:3) = 1
          end select
        elseif (ireg == 2) then 
          select case (ndim)
            ! scale factor vs element size
            case(1)
              xvec(1:nel,1)   = l0(1:nel)/el_ref
              xvec(1:nel,2:3) = zero
              ipos(1:nel,1:3) = 1
            ! scale factor vs element size vs triaxiality
            case(2)
              xvec(1:nel,1)   = l0(1:nel)/el_ref
              xvec(1:nel,2)   = triax(1:nel)
              xvec(1:nel,3)   = zero
              ipos(1:nel,1:3) = 1
              ! scale factor vs element size vs triaxiality vs lode parameter
            case(3)
              xvec(1:nel,1)   = l0(1:nel)/el_ref
              xvec(1:nel,2)   = triax(1:nel)
              xvec(1:nel,3)   = xi(1:nel)
              ipos(1:nel,1:3) = 1
          end select            
        endif
        call table_vinterp(table(itab_size),nel,nel,ipos,xvec,sizefac,dsize)
        sizefac(1:nel) = sizefac(1:nel)*fscale_el
        if (ireg == 1) then 
          do i = 1,nel
            if (triax(i) < shrf) then 
              sizefac(i) = one
            elseif (triax(i) > biaxf) then 
              sizefac(i) = one
            endif
          enddo
        endif
      else
        sizefac(1:nel) = one
      endif
!c
      ! compute the strain rate dependency factor
      if (ifunc(2) > 0) then 
        if (log_scale2 > 0) then
          do i = 1,nel 
            var(i) = log(max(epsp(i),em20)/sr_ref2)
          enddo 
        else
          var(1:nel) = epsp(1:nel)/sr_ref2
        endif
        ipos2(1:nel) = 1
        iad (1:nel) = npf(ifunc(2)) / 2 + 1
        ilen(1:nel) = npf(ifunc(2)+1) / 2 - iad(1:nel) - ipos2(1:nel)
        call vinter2(tf,iad,ipos2,ilen,nel,var,dft,ratefac)
        ratefac(1:nel) = fscale_sr*ratefac(1:nel)
      elseif (cjc > zero) then 
        do i=1,nel
          if (epsp(i) > sr_ref2) then 
            ratefac(i) = one + cjc*log(epsp(i)/sr_ref2)
          else
            ratefac(i) = one
          endif
        enddo
      else
        ratefac(1:nel) = one
      endif
!c
      ! compute the damage limit value
      if (ifunc(3) > 0) then 
        do i = 1,nel 
          lambda  = triax(i)
          dlim(i) = finter(ifunc(3),lambda,npf,tf,df) 
          dlim(i) = fscale_dlim*dlim(i)
          dlim(i) = min(dlim(i),one)
          dlim(i) = max(dlim(i),zero)
        enddo
      else
        dlim(1:nel) = one
      endif
!c
      !====================================================================
      ! - computation of plastic strain at failure
      !====================================================================   
      if (itab_epsf > 0) then 
        ! failure plastic strain map dependency
        ndim = table(itab_epsf)%ndim
        select case (ndim)
          ! failure plastic strain vs triaxiality
          case (1)
            xvec(1:nel,1)   = triax(1:nel)
            xvec(1:nel,2:3) = zero
            ipos(1:nel,1:3) = 1
          ! failure plastic strain vs triaxiality vs lode parameter
          case (2)
            xvec(1:nel,1)   = triax(1:nel)
            xvec(1:nel,2)   = xi(1:nel)
            xvec(1:nel,3)   = zero
            ipos(1:nel,1:3) = 1
          ! failure plastic strain vs triaxiality vs lode parameter vs temperature
          case (3)
            xvec(1:nel,1)   = triax(1:nel)
            xvec(1:nel,2)   = xi(1:nel)
            xvec(1:nel,3)   = temp(1:nel)/temp_ref
            ipos(1:nel,1:3) = 1
            tempfac(1:nel)  = one
        end select
        call table_vinterp(table(itab_epsf),nel,nel,ipos,xvec,epsf,depsf)
        epsf(1:nel) = epsf(1:nel)*fcrit
      else 
        epsf(1:nel) = fcrit
      endif
!c
      !====================================================================
      ! - computation of plastic strain at necking
      !====================================================================   
      if (itab_inst > 0) then 
        ! instability plastic strain map dependency
        ndim = table(itab_inst)%ndim
        select case (ndim)
          ! instability plastic strain vs triaxiality
          case(1)
            xvec(1:nel,1)   = triax(1:nel)
            xvec(1:nel,2:3) = zero
            ipos(1:nel,1:3) = 1
          ! instability plastic strain vs triaxiality vs lode 
          case(2)
            xvec(1:nel,1)   = triax(1:nel)
            xvec(1:nel,2)   = xi(1:nel)
            xvec(1:nel,3)   = zero
            ipos(1:nel,1:3) = 1
          ! instability plastic strain vs triaxiality vs lode vs temperature
          case(3)
            xvec(1:nel,1)   = triax(1:nel)
            xvec(1:nel,2)   = xi(1:nel)
            xvec(1:nel,3)   = temp(1:nel)/temp_ref
            ipos(1:nel,1:3) = 1         
            tempfac2(1:nel) = one            
        end select
        call table_vinterp(table(itab_inst),nel,nel,ipos,xvec,epsl,depsl)
        epsl(1:nel) = epsl(1:nel)*ecrit
      elseif (ecrit > zero) then 
        epsl(1:nel) = ecrit
      endif
!c
      !====================================================================
      ! - computation of the damage variable evolution
      !==================================================================== 
      ! initialization of element failure index
      nindx = 0  
      indx(1:nel) = 0
!c
      ! loop over the elements 
      do i=1,nel
!c
        ! if the element is not broken
        if (foff(i) == one .and. off(i) ==  one .and. dpla(i) > zero) then
!c
          ! needs to initialize damage at a very small value the first time
          if (dfmax(i) == zero) dfmax(i) = em20
          if (inst(i)  == zero) inst(i)  = em20
!c
          ! compute failure strain damage variable
          dpl_def  = dpla(i)/max(epsf(i)*ratefac(i)*sizefac(i)*tempfac(i),em20)
          dfmax(i) = dfmax(i) + dpl_def*dn*(dfmax(i)**(one-(one/dn)))
          dfmax(i) = min(dfmax(i),dlim(i))
          if (dfmax(i) >= one) then 
            nindx       = nindx + 1
            indx(nindx) = i
            uelr(i)     = uelr(i) + 1
            foff(i)     = zero
              if (uelr(i) >= failip) then 
                off(i)    = zero
                tdele(i)  = time
              endif
                     
          endif
!c
          ! compute the control necking instability damage
          if ((itab_inst > 0).or.(ecrit > zero)) then 
            dpl_def = dpla(i)/max(epsl(i)*ratefac(i)*sizefac(i)*tempfac2(i),em20)
            inst(i) = inst(i) + dpl_def*dn*(inst(i)**(one-(one/dn)))
            inst(i) = min(inst(i),one)
            if ((inst(i) >= one).and.(dc(i) == one)) then 
              dc(i) = dfmax(i)
            endif
          endif
!c
        endif
      enddo
!c
      !====================================================================
      ! - update uvar and the stress tensor
      !====================================================================
      do i = 1,nel 
        if ((itab_inst > 0).or.(ecrit > zero)) then 
          uvar(i,1) = inst(i)
          uvar(i,2) = dc(i)
          if (dfmax(i) >= dc(i)) then 
            if (dc(i) < one) then 
              dmg_scale(i) = one - ((dfmax(i)-dc(i))/max(one-dc(i),em20))**softexp(i)
            else
              dmg_scale(i) = zero
            endif
          else
            dmg_scale(i) = one
          endif
        else
          if (dfmax(i) >= dcrit) then 
            if (dcrit < one) then 
              dmg_scale(i) = one - ((dfmax(i)-dcrit)/max(one-dcrit,em20))**softexp(i)
            else
              dmg_scale(i) = zero
            endif
          else
            dmg_scale(i) = one
          endif
        endif
      enddo
!c
      !====================================================================
      ! - printout data about failed elements
      !====================================================================
      if (nindx > 0) then 
        do j=1,nindx
          i = indx(j)     
          write(iout, 1000) ngl(i),ipg,time
          write(istdo,1000) ngl(i),ipg,time
          if (off(i) == zero) then 
            write(iout, 2000) ngl(i),time
            write(istdo,2000) ngl(i),time
          endif
        end do
      end if                          
!c-----------------------------------------------------------------------  
 1000 format(1x,'FOR BEAM ELEMENT NUMBER EL#',i10,    &
                ' FAILURE (TAB2) AT GAUSS POINT ',i5, &
                ' AT TIME :',1pe12.4)     
 2000 format(1x,'-- RUPTURE OF BEAM ELEMENT :',i10,   &
                ' AT TIME :',1pe12.4)    
!c
      end subroutine fail_tab2_ib 
    end module fail_tab2_ib_mod