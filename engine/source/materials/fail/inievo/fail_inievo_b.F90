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
!||    fail_inievo_b_mod   ../engine/source/materials/fail/inievo/fail_inievo_b.F90
!||--- called by ------------------------------------------------------
!||    fail_beam3          ../engine/source/elements/beam/fail_beam3.F
!||====================================================================
      module fail_inievo_b_mod
      implicit none
      contains
! ======================================================================================================================
! \brief   inievo failure criteria for type3 beam elements
! ======================================================================================================================
!||====================================================================
!||    fail_inievo_b         ../engine/source/materials/fail/inievo/fail_inievo_b.F90
!||--- called by ------------------------------------------------------
!||    fail_beam3            ../engine/source/elements/beam/fail_beam3.F
!||--- calls      -----------------------------------------------------
!||    table_vinterp         ../engine/source/tools/curve/table_tools.F
!||--- uses       -----------------------------------------------------
!||    constant_mod          ../common_source/modules/constant_mod.F
!||    elbufdef_mod          ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    interface_table_mod   ../engine/share/modules/table_mod.F
!||    message_mod           ../engine/share/message_module/message_mod.F
!||    precision_mod         ../common_source/modules/precision_mod.F90
!||    table_mod             ../engine/share/modules/table_mod.F
!||====================================================================
        subroutine fail_inievo_b(                                        &
          nel     ,nuparam  ,nuvar    ,                                  &
          table   ,ntablf   ,itablf   ,time    ,uparam     ,             &
          ngl     ,aldt     ,dpla     ,epsp    ,uvar       ,             &
          f1      ,area,                                                 &
          pla     ,sigy     ,off      ,dfmax      ,                      &
          tdele   ,                                          &
          damini  ,                                                      &
          ntable  ,dmgscl )
!c-----------------------------------------------
!                                                    modules
!c-----------------------------------------------
          use table_mod
          use interface_table_mod
          use elbufdef_mod
          use constant_mod
          use message_mod
          use precision_mod, only : WP
!c-----------------------------------------------
!                                               c i m p l i c i t t y p e
!c-----------------------------------------------
          implicit none
#include      "units_c.inc"
!c-----------------------------------------------
!                                                  arguments s
!c-----------------------------------------------
          integer                     ,intent(in)     :: nel         ! size of element group
          integer                     ,intent(in)     :: nuparam     ! size of parameter array
          integer                     ,intent(in)     :: nuvar       ! size of user variable array
          type(ttable), dimension(ntable), intent(inout)   :: table  ! table data
          integer                     ,intent(in)     :: ntablf      ! number of table functions
          integer, dimension(ntablf)  ,intent(in)     :: itablf      ! table function identifiers
          real(kind=WP)                     ,intent(in)     :: time        ! current time
          real(kind=WP), dimension(nuparam) ,intent(in)     :: uparam      ! user parameters
          integer, dimension(nel)     ,intent(in)     :: ngl         ! element identifiers
          real(kind=WP), dimension(nel)     ,intent(in)     :: aldt        ! element size
          real(kind=WP), dimension(nel)     ,intent(in)     :: dpla        ! increment of plastic strain
          real(kind=WP), dimension(nel)     ,intent(in)     :: epsp        ! strain rate (confirmed by the tensstrain_criterion in solid element and beam3 element)
          real(kind=WP), dimension(nel, nuvar), intent(inout)    :: uvar   ! user variables
          real(kind=WP), dimension(nel)     ,intent(inout)  :: f1          ! force in direction x
          real(kind=WP)                     ,intent(in)     :: area        ! cross section area
          real(kind=WP), dimension(nel)     ,intent(in)     :: pla         ! the plastic strain of the current element
          real(kind=WP), dimension(nel)     ,intent(in)     :: sigy        ! yield stress
          real(kind=WP), dimension(nel)     ,intent(inout)  :: off         ! offset
          real(kind=WP), dimension(nel)     ,intent(inout)  :: dfmax       ! maximum damage
          real(kind=WP), dimension(nel)     ,intent(inout)  :: tdele       !
          real(kind=WP), dimension(nel)     ,intent(inout)  :: damini      !
          real(kind=WP), dimension(nel)     ,intent(inout)  :: dmgscl
          integer ,intent(in) :: ntable
!c-----------------------------------------------
!                                                  local variables
!c-----------------------------------------------
          integer :: i, j, nindx, ninievo, ilen
          integer, dimension(nel) :: indx
          integer, dimension(nel, 2) :: ipos
          integer, dimension(:), allocatable :: initype, evotype, evoshap, comptyp, tab_id, &
            tab_el, fcrit
          real(kind=WP), dimension(:), allocatable :: sr_ref, fscale, ini_p1, el_ref, elscal, disp, &
            ener, alpha2
          real(kind=WP) :: lambda, fac, df, sxx, syy, szz, plas_disp, r_inter, yld0, center, &
            devsp1, devsp2, radius, sigp1, sigp2
          real(kind=WP), dimension(nel) :: l0, triax, epsf, depsf, sizefac, epsmod, p, svm, &
            dmgmax, dmgmul, maxshear, sigpmaj, alpha, dsize
          real(kind=WP), dimension(nel, 2) :: xvec
          real(kind=WP), dimension(:,:), allocatable :: dmgini, dmgevo
!c=======================================================================
!c=======================================================================
          !c user variables
          !c! user variable # 1,              initial beam length aldt(1:nel)
          !c! user variable # 2,              positive stress triaxiality bounded plastic strain
          !c! user variable # 3+(j-1)*3,      damage initiation variable
          !c! user variable # 4+(j-1)*3,      damage evolution variable
          !c! user variable # 5,              pla or sigy
!c===============================================================================================
          !step1: recovering failure criterion parameters and initiation
          !=======================================================================
          ! - initialisation of computation on time step
          !=======================================================================
          ! recovering failure criterion parameters
          ninievo = uparam(1)
          allocate(initype(ninievo))
          allocate(evotype(ninievo))
          allocate(evoshap(ninievo))
          allocate(comptyp(ninievo))
          allocate(tab_id (ninievo))
          allocate(sr_ref (ninievo))
          allocate(fscale (ninievo))
          allocate(ini_p1 (ninievo))
          allocate(tab_el (ninievo))
          allocate(el_ref (ninievo))
          allocate(elscal (ninievo))
          allocate(disp   (ninievo))
          allocate(ener   (ninievo))
          allocate(alpha2 (ninievo))
!c
          tab_id(1:ninievo) = itablf(1:ninievo)
          tab_el(1:ninievo) = itablf(ninievo+1:ninievo*2)
!c
          do j = 1,ninievo
            initype(j) = uparam(6  + 14*(j-1)) !damage initiation tabulated criterion type
            evotype(j) = uparam(7  + 14*(j-1)) !damage evolution type.
            evoshap(j) = uparam(8  + 14*(j-1)) !shape of the damage evolution.
            comptyp(j) = uparam(9  + 14*(j-1)) !criterion combination type (only if ninievo > 0).
            sr_ref(j)  = uparam(11 + 14*(j-1)) !reference strain rate for table identifier.
            fscale(j)  = uparam(12 + 14*(j-1)) !scale factor for failure initiation criterion table.
            ini_p1(j)  = uparam(13 + 14*(j-1))
            el_ref(j)  = uparam(15 + 14*(j-1)) !reference element size for size scaling table.
            elscal(j)  = uparam(16 + 14*(j-1)) !scale factor for element size scaling function.
            disp(j)    = uparam(17 + 14*(j-1)) !plastic displacement at failure.
            alpha2(j)  = uparam(18 + 14*(j-1)) !exponential shape parameter (not applicable for exponential energy- based evolution).
            ener(j)    = uparam(19 + 14*(j-1)) !fracture energy.
          end do
!c
          ! element characteristic length computation
          !  -> initial values
          if (uvar(1,1) == zero) then
            ! -> critical timestep formulation
            uvar(1:nel,1) = aldt(1:nel)
            ! -> no geometric formulation
          end if
          l0(1:nel) = uvar(1:nel,1)
          ! positive stress triaxiality bounded plastic strain
          epsmod(1:nel) = uvar(1:nel,2)
          ! damage initiation and evolution variable
          allocate(dmgini(nel,ninievo))
          allocate(dmgevo(nel,ninievo))
          allocate(fcrit(nel))
          do j = 1,ninievo
            do i=1,nel
              ! initiation damage
              dmgini(i,j) = uvar(i,3+(j-1)*3)
              ! evolution damage
              dmgevo(i,j) = uvar(i,4+(j-1)*3)
            end do
          end do
          ! criterion number leading to element deletion
          fcrit(1:nel) = 0
          !====================================================================
          ! - loop over the element to compute the stress state quantities
          !====================================================================
          do i=1,nel
!c
            ! computation of hydrostatic stress, von mises stress, and stress triaxiality
            p(i)   = -third*(f1(i)/area   + 0. + 0.)
            sxx    = f1(i)/area   + p(i)
            syy    = 0. + p(i)
            szz    = 0. + p(i)
            svm(i) = half*(sxx**2 + syy**2 + szz**2)
            svm(i) = sqrt(three*svm(i))
            triax(i) = -p(i)/max(em20,svm(i))
            if (triax(i) < -one) triax(i) = -one
            if (triax(i) >  one) triax(i) = one
!c
            ! increase the modified plastic strain
            if (triax(i) > zero) epsmod(i) = epsmod(i) + dpla(i)
!c

!c
          end do

          do i = 1,nel
!c
            sigpmaj(i) = abs(f1(i)/area)
            maxshear(i) = 0.
!c
            ! compute the alpha parameter for fld/msfld
            center = half*(f1(i)/area)
            radius = sqrt((half*(f1(i)/area))**2)
            sigp1  = center + radius
            sigp2  = center - radius
            devsp1 = sigp1  - third*(sigp1+sigp2)
            devsp2 = sigp2  - third*(sigp1+sigp2)
            alpha(i) = devsp2/sign(max(abs(devsp1),em20),devsp1)
!c
          end do
!c
          !====================================================================
          ! - compute damage initiation and evolution
          !====================================================================
          do j = 1,ninievo
            ! damage initiation type selection
            select case(initype(j))
              ! plastic strain vs triaxiality
             case(1)
              xvec(1:nel,1) = triax(1:nel)
              ! plastic strain vs shear influence (theta)
             case(2)
              do i = 1,nel
                xvec(i,1) = (svm(i) + ini_p1(j)*p(i))/max(maxshear(i),em08)
              end do
              ! msfld / fld
             case(3,4)
              xvec(1:nel,1) = alpha(1:nel)
              ! normalized principal stress
             case(5)
              do i = 1,nel
                xvec(i,1) = (svm(i) + ini_p1(j)*p(i))/max(sigpmaj(i),em08)
              end do
            end select
            xvec(1:nel,2)   = epsp(1:nel)/sr_ref(j)
            ipos(1:nel,1:2) = 1
            call table_vinterp(table(tab_id(j)),nel,nel,ipos,xvec,epsf,depsf)
            epsf(1:nel) = epsf(1:nel)*fscale(j)
!c
            ! compute the element size regularization factor
            if (tab_el(j) > 0) then
              xvec(1:nel,1) = l0(1:nel)/el_ref(j)
              select case (initype(j))
               case(1)
                xvec(1:nel,2) = triax(1:nel)
               case(2)
                do i = 1,nel
                  xvec(i,2) = (svm(i) + ini_p1(j)*p(i))/max(maxshear(i),em08)
                end do
               case(3,4)
                xvec(1:nel,2) = alpha(1:nel)
               case(5)
                do i = 1,nel
                  xvec(i,2) = (svm(i) + ini_p1(j)*p(i))/max(sigpmaj(i),em08)
                end do
              end select
              ipos(1:nel,1:2) = 1
              call table_vinterp(table(tab_el(j)),nel,nel,ipos,xvec,sizefac,dsize)
              sizefac(1:nel) = sizefac(1:nel)*elscal(j)
              epsf(1:nel) = epsf(1:nel)*sizefac(1:nel)
            end if
!c
            ! update damage initiation
            select case (initype(j))
             case(1,2,5)
              do i = 1,nel
                if ((dpla(i) > zero).and.(dmgini(i,j)<one).and.(off(i) == one)) then
                  dmgini(i,j) = dmgini(i,j) + dpla(i)/max(epsf(i),em20)
                  dmgini(i,j) = min(dmgini(i,j),one)
                end if
              end do
             case(3)
              if (nint(ini_p1(j))>0) then
                do i = 1,nel
                  if (((epsmod(i)-uvar(i,2)) > zero).and.(dmgini(i,j)<one).and.(off(i) == one)) then
                    dmgini(i,j) = dmgini(i,j) + (epsmod(i)-uvar(i,2))/max(epsf(i),em20)
                    dmgini(i,j) = min(dmgini(i,j),one)
                  end if
                end do
              else
                do i = 1,nel
                  if (((epsmod(i)-uvar(i,2)) > zero).and.(dmgini(i,j)<one).and.(off(i) == one)) then
                    dmgini(i,j) = max(dmgini(i,j),epsmod(i)/max(epsf(i),em20))
                    dmgini(i,j) = min(dmgini(i,j),one)
                  end if
                end do
              end if
             case(4)
              if (nint(ini_p1(j))>0) then
                do i = 1,nel
                  if ((dpla(i) > zero).and.(dmgini(i,j)<one).and.(off(i) == one)) then
                    dmgini(i,j) = dmgini(i,j) + dpla(i)/max(epsf(i),em20)
                    dmgini(i,j) = min(dmgini(i,j),one)
                  end if
                end do
              else
                do i = 1,nel
                  if ((dpla(i) > zero).and.(dmgini(i,j)<one).and.(off(i) == one)) then
                    dmgini(i,j) = max(dmgini(i,j),pla(i)/max(epsf(i),em20))
                    dmgini(i,j) = min(dmgini(i,j),one)
                  end if
                end do
              end if
            end select
!c
            ! update damage evolution
            select case (evotype(j))
              ! plastic displacement at failure
             case(1)
              select case (evoshap(j))
                ! linear shape
               case(1)
                do i = 1,nel
                  if ((dmgini(i,j) >= one).and.(dpla(i)>zero).and.  &
                    (off(i) == one).and.(dmgevo(i,j)<one)) then
                    dmgevo(i,j) = dmgevo(i,j) + l0(i)*dpla(i)/disp(j)
                    dmgevo(i,j) = min(one,dmgevo(i,j))
                    if (dmgevo(i,j) >= one) fcrit(i) = j
                  end if
                end do
                ! exponential shape
               case(2)
                do i = 1,nel
                  if ((dmgini(i,j) >= one).and.(dpla(i)>zero).and.  &
                    (off(i) == one).and.(dmgevo(i,j)<one)) then
                    if (dmgevo(i,j) == zero) uvar(i,5+(j-1)*3) = pla(i)
                    plas_disp = (pla(i) - uvar(i,5+(j-1)*3))*l0(i)/disp(j)
                    dmgevo(i,j) = dmgevo(i,j) + (alpha2(j)/(one - exp(-alpha2(j))))* &
                      exp(-alpha2(j)*plas_disp)*  &
                      dpla(i)*l0(i)/disp(j)
                    if (dmgevo(i,j) > 0.999d0) dmgevo(i,j) = one
                    dmgevo(i,j) = min(one,dmgevo(i,j))
                    if (dmgevo(i,j) >= one) fcrit(i) = j
                  end if
                end do
              end select
              ! fracture energy failure
             case(2)
              select case (evoshap(j))
                ! linear shape
               case(1)
                do i = 1,nel
                  if ((dmgini(i,j) >= one).and.(dpla(i)>zero).and.  &
                    (off(i) == one).and.(dmgevo(i,j)<one)) then
                    if (dmgevo(i,j) == zero) uvar(i,5+(j-1)*3) = sigy(i)
                    yld0 = uvar(i,5+(j-1)*3)
                    dmgevo(i,j) = dmgevo(i,j) + dpla(i)*l0(i)*yld0/(two*ener(j))
                    dmgevo(i,j) = min(one,dmgevo(i,j))
                    if (dmgevo(i,j) >= one) fcrit(i) = j
                  end if
                end do
                ! exponential shape
               case(2)
                do i = 1,nel
                  if ((dmgini(i,j) >= one).and.(dpla(i)>zero).and.  &
                    (off(i) == one).and.(dmgevo(i,j)<one)) then
                    uvar(i,5+(j-1)*3) = uvar(i,5+(j-1)*3) + sigy(i)*l0(i)*dpla(i)
                    dmgevo(i,j) = one - exp(-(uvar(i,5+(j-1)*3))/ener(j))
                    if (dmgevo(i,j) > 0.999d0) dmgevo(i,j) = one
                    dmgevo(i,j) = min(one,dmgevo(i,j))
                    if (dmgevo(i,j) >= one) fcrit(i) = j
                  end if
                end do
              end select
              ! failure criterion approach
             case default
              do i = 1,nel
                if ((dmgini(i,j) >= one).and.(dpla(i)>zero).and.   &
                  (off(i) == one).and.(dmgevo(i,j)<one)) then
                  dmgevo(i,j) = dmgini(i,j)
                  dmgevo(i,j) = min(one,dmgevo(i,j))
                  if (dmgevo(i,j) >= one) fcrit(i) = j
                end if
              end do
            end select
          end do
!c
          !====================================================================
          ! - compute global damage variable and damage scaling
          !====================================================================
          dfmax(1:nel)  = zero
          dmgmax(1:nel) = zero
          dmgmul(1:nel) = one
          do j = 1,ninievo
            select case (comptyp(j))
              ! maximum damage
             case(1)
              do i = 1,nel
                dmgmax(i) = max(dmgmax(i),dmgevo(i,j))
              end do
              ! multiplicative damage
             case(2)
              do i = 1,nel
                dmgmul(i) = dmgmul(i)*(one-dmgevo(i,j))
              end do
            end select
          end do
          dmgmul(1:nel) = one - dmgmul(1:nel)
          nindx = 0
          indx(1:nel) = 0



          do i = 1,nel
            if (off(i)== ONE) then
              dfmax(i) = max(dmgmax(i),dmgmul(i))
              if (dfmax(i) >= one) then
                nindx       = nindx + 1
                indx(nindx) = i
                off(i)    = zero
                tdele(i)  = time
              end if
            end if
            !====================================================================
            ! - update the damage scaling factor
            !====================================================================
            dmgscl(i) = one - dfmax(i)
          end do

!c
          !====================================================================
          ! - update the user variable
          !====================================================================
          ! positive stress triaxiality bounded plastic strain
          uvar(1:nel,2) = epsmod(1:nel)
          damini(1:nel) = zero
          do j = 1,ninievo
            ! checking element failure and recovering user variable
            do i=1,nel
              ! damage initiation output
              damini(i) = max(dmgini(i,j),damini(i))
              ! initiation damage
              uvar(i,3+(j-1)*3) = dmgini(i,j)
              ! evolution damage
              uvar(i,4+(j-1)*3) = dmgevo(i,j)
            end do
          end do
!c
          !====================================================================
          ! - printout data about failed elements
          !====================================================================
          if (nindx > 0) then
            do j=1,nindx
              i = indx(j)
              write(iout, 2000) ngl(i),time
              write(istdo,2000) ngl(i),time
            end do
          end if
!c
          !====================================================================
          ! - tables deallocation
          !====================================================================
          if (allocated(initype)) deallocate(initype)
          if (allocated(evotype)) deallocate(evotype)
          if (allocated(evoshap)) deallocate(evoshap)
          if (allocated(comptyp)) deallocate(comptyp)
          if (allocated(tab_id))  deallocate(tab_id)
          if (allocated(sr_ref))  deallocate(sr_ref)
          if (allocated(fscale))  deallocate(fscale)
          if (allocated(ini_p1))  deallocate(ini_p1)
          if (allocated(tab_el))  deallocate(tab_el)
          if (allocated(el_ref))  deallocate(el_ref)
          if (allocated(elscal))  deallocate(elscal)
          if (allocated(disp))    deallocate(disp)
          if (allocated(ener))    deallocate(ener)
          if (allocated(alpha2))  deallocate(alpha2)
          if (allocated(dmgini))  deallocate(dmgini)
          if (allocated(dmgevo))  deallocate(dmgevo)
          if (allocated(fcrit))   deallocate(fcrit)
!c-----------------------------------------------------------------------
2000      format(1x,"-- RUPTURE OF BEAM ELEMENT :",i10,  &
            " AT TIME :",1pe12.4)

          return
        end subroutine fail_inievo_b
      end module fail_inievo_b_mod
