!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
      module sigeps126_mod
        contains
  ! ======================================================================================================================
  ! \brief Johnson-Holmquist 1 material law /MAT/LAW126
  ! \details Material law based on Johnson-Holmquist version 1 theory. Dedicated to concrete application. 
  ! ======================================================================================================================
        subroutine sigeps126(                                          &
          nel      ,nuvar    ,uvar     ,matparam ,tt       ,           &
          rho0     ,ngl      ,sigy     ,dpla     ,defp     ,amu      , &
          depsxx   ,depsyy   ,depszz   ,depsxy   ,depsyz   ,depszx   , &
          sigoxx   ,sigoyy   ,sigozz   ,sigoxy   ,sigoyz   ,sigozx   , &
          signxx   ,signyy   ,signzz   ,signxy   ,signyz   ,signzx   , &
          epsd     ,dmg      ,ssp      ,off      ,idel7nok ,inloc    , &
          varnl    ,l_planl  ,planl    )
!-----------------------------------------------
!   M o d u l e s
!-----------------------------------------------
          use matparam_def_mod 
          use constant_mod      
!-----------------------------------------------
!   I m p l i c i t   T y p e s
!-----------------------------------------------
          implicit none 
#include  "my_real.inc"
#include  "units_c.inc"
!-----------------------------------------------
!   D u m m y   A r g u m e n t s
!-----------------------------------------------
          integer, intent(in) :: nel !< number of elements in the group
          integer, intent(in) :: nuvar !< number of user variables
          my_real, dimension(nel,nuvar), intent(inout) :: uvar !< user variables
          type(matparam_struct_), intent(in) :: matparam !< material parameters data
          my_real, intent(in) :: tt !< current time
          my_real, dimension(nel), intent(in) :: rho0 !< material density
          integer, dimension(nel), intent(in) :: ngl !< element user IDs index table
          my_real, dimension(nel), intent(inout) :: sigy !< yield stress
          my_real, dimension(nel), intent(inout) :: dpla !< cumulated plastic strain increment 
          my_real, dimension(nel), intent(inout) :: defp !< cumulated plastic strain
          my_real, dimension(nel), intent(in) :: amu !< volumetric strain
          my_real, dimension(nel), intent(in) :: depsxx !< strain increment xx 
          my_real, dimension(nel), intent(in) :: depsyy !< strain increment yy
          my_real, dimension(nel), intent(in) :: depszz !< strain increment zz 
          my_real, dimension(nel), intent(in) :: depsxy !< strain increment xy 
          my_real, dimension(nel), intent(in) :: depsyz !< strain increment yz 
          my_real, dimension(nel), intent(in) :: depszx !< strain increment zx 
          my_real, dimension(nel), intent(in) :: sigoxx !< initial stress xx 
          my_real, dimension(nel), intent(in) :: sigoyy !< initial stress yy
          my_real, dimension(nel), intent(in) :: sigozz !< initial stress zz 
          my_real, dimension(nel), intent(in) :: sigoxy !< initial stress xy 
          my_real, dimension(nel), intent(in) :: sigoyz !< initial stress yz 
          my_real, dimension(nel), intent(in) :: sigozx !< initial stress zx 
          my_real, dimension(nel), intent(out) :: signxx !< new stress xx 
          my_real, dimension(nel), intent(out) :: signyy !< new stress yy
          my_real, dimension(nel), intent(out) :: signzz !< new stress zz 
          my_real, dimension(nel), intent(out) :: signxy !< new stress xy 
          my_real, dimension(nel), intent(out) :: signyz !< new stress yz 
          my_real, dimension(nel), intent(out) :: signzx !< new stress zx 
          my_real, dimension(nel), intent(in) :: epsd !< equivalent strain rate 
          my_real, dimension(nel), intent(inout) :: dmg !< damage variable 
          my_real, dimension(nel), intent(inout) :: ssp !< sound speed
          my_real, dimension(nel), intent(inout) :: off !< element deletion flag
          integer, intent(inout) :: idel7nok !< flag for element deletion in interface type 7
          integer, intent(in) :: inloc !< non-local method flag
          my_real, dimension(nel), intent(inout) :: varnl !< non-local variable increment
          integer, intent(in) :: l_planl !< size of the non-local plastic strain table
          my_real, dimension(l_planl*nel), intent(in) :: planl !< non-local plastic strain
!-----------------------------------------------
!  L o c a l   V a r i a b l e s
!-----------------------------------------------
          integer :: i,j,nindx,indx(nel),idel
          my_real                                                              &
            g,g2,aa,bb,nn,fc,t0,cc,eps0,sfmax,efmin,pc,muc,pl,mul,             &
            k0,k1,k2,k3,d1,d2,emax,h
          my_real                                                              &
            pold(nel),vm(nel),mup(nel),pnew(nel),dpdmu(nel),dmup(nel),         &
            pstar(nel),phard(nel),scale(nel),dav(nel)                     
          my_real                                                              &
            j2,kav,pmin,mubar,sigstar,epfail,ratio
!
          !=======================================================================
          ! - INITIALISATION OF COMPUTATION ON TIME STEP
          !=======================================================================
          ! Recovering integer model parameter
          idel  = matparam%iparam(1)
          ! Recovering real model parameters
          g     = matparam%uparam(1)
          g2    = matparam%uparam(2)
          aa    = matparam%uparam(3)
          bb    = matparam%uparam(4)
          nn    = matparam%uparam(5)
          fc    = matparam%uparam(6)
          t0    = matparam%uparam(7)
          cc    = matparam%uparam(8)
          eps0  = matparam%uparam(9)
          sfmax = matparam%uparam(10)
          efmin = matparam%uparam(11)
          pc    = matparam%uparam(12)
          muc   = matparam%uparam(13)
          pl    = matparam%uparam(14)
          mul   = matparam%uparam(15)
          k0    = matparam%uparam(16)
          k1    = matparam%uparam(17)
          k2    = matparam%uparam(18)
          k3    = matparam%uparam(19)
          d1    = matparam%uparam(20)
          d2    = matparam%uparam(21)
          emax  = matparam%uparam(22)
          h     = matparam%uparam(23)
          ! Recovering user variables
          do i=1,nel
            if (off(i) < em01) off(i) = zero
            if (off(i) <  one) off(i) = off(i)*four_over_5
            if (uvar(i,2) == zero) uvar(i,2) = pc
            if (tt == zero) uvar(i,4) = aa
            mup(i)   = uvar(i,1)
            phard(i) = uvar(i,2)
            dpla(i)  = zero 
            dmup(i)  = zero
          enddo 
! 
          !========================================================================
          ! - Computation of elastic deviatoric stresses and equivalent stress
          !========================================================================
          do i=1,nel
            dav(i)    =  (depsxx(i) + depsyy(i) + depszz(i))*third
            pold(i)   = -(sigoxx(i) + sigoyy(i) + sigozz(i))*third
            signxx(i) = sigoxx(i) + pold(i) + g2*(depsxx(i)-dav(i))
            signyy(i) = sigoyy(i) + pold(i) + g2*(depsyy(i)-dav(i))
            signzz(i) = sigozz(i) + pold(i) + g2*(depszz(i)-dav(i))
            signxy(i) = sigoxy(i) + g*depsxy(i)
            signyz(i) = sigoyz(i) + g*depsyz(i)
            signzx(i) = sigozx(i) + g*depszx(i)
            j2        = half*(signxx(i)**2+signyy(i)**2+signzz(i)**2)           &
                            + signxy(i)**2+signyz(i)**2+signzx(i)**2
            vm(i)     = sqrt(three*j2)
          enddo
!
          !========================================================================
          ! - Update plastic strain and damage in case of non-local regularisation
          !========================================================================
          if (inloc > 0) then 
            nindx = 0
            indx(1:nel) = 0
            do i=1,nel
              if (off(i) == one) then 
                ! compute plastic strain at failure
                if ((pold(i)/fc+t0/fc) >= zero) then
                  epfail = d1*(pold(i)/fc+t0/fc)**d2
                else
                  epfail = zero
                endif
                epfail = max(epfail,efmin)
                ! update plastic strain and damage 
                dmg(i) = dmg(i) + varnl(i)/epfail
                dmg(i) = min(dmg(i),one)
                ! check element deletion
                if (idel == 1) then 
                  if ((pold(i)/fc+t0/fc) <= zero) then 
                    off(i) = four_over_5
                    nindx  = nindx + 1
                    indx(nindx) = i
                    idel7nok = 1
                  endif 
                elseif (idel == 2) then 
                  if (planl(i) > emax) then 
                    off(i) = four_over_5
                    nindx  = nindx + 1
                    indx(nindx) = i
                    idel7nok = 1
                  endif
                elseif (idel == 3) then 
                  if (uvar(i,4) <= zero) then 
                    off(i) = four_over_5
                    nindx  = nindx + 1
                    indx(nindx) = i
                    idel7nok = 1
                  endif   
                elseif (idel == 4) then 
                  if (dmg(i) >= one) then 
                    off(i) = four_over_5
                    nindx  = nindx + 1
                    indx(nindx) = i
                    idel7nok = 1
                  endif          
                endif
              endif
            enddo    
          endif
!
          !========================================================================
          ! - Computation of the pressure
          !========================================================================
          do i=1,nel
            ! New pressure 
            ! -> Region I and II: elasticity + air voids crushing plasticity
            if (mup(i) < mul) then
              pmin    = -t0*(one-dmg(i)) 
              kav     = (k0 + (k1 - k0)*(mup(i)/mul))
              pnew(i) = pold(i) + kav*(amu(i) - uvar(i,3))
              if (pnew(i) > phard(i)) then 
                dmup(i)  = (pnew(i)-phard(i))/(kav + h)
                mup(i)   = mup(i) + dmup(i)
                mup(i)   = min(mup(i),mul)
                pnew(i)  = pnew(i) - kav*dmup(i)
                phard(i) = pnew(i)
              endif
              pnew(i)  = max(pnew(i),pmin)
              dpdmu(i) = kav
            endif
            ! -> Region III: fully dense concrete
            if (mup(i) >= mul) then 
              mubar    = (amu(i) - mul)/(one + mul)
              pnew(i)  = k1*mubar + k2*(mubar**2) + k3*(mubar**3)
              dpdmu(i) = (k1 + two*k2*mubar + three*k3*(mubar**2))/(one + mul)
            endif 
            ! Normalized pressure
            pstar(i) = pnew(i)/fc
          enddo
!
          !========================================================================
          ! - Computation of the deviatoric yield stress
          !========================================================================
          do i=1,nel
            if (pstar(i) > zero) then 
              sigy(i) = aa*(one-dmg(i)) + bb*exp(nn*log(pstar(i)))
              if (epsd(i) > eps0) then 
                sigy(i) = sigy(i)*(one + cc*log(epsd(i)/eps0))
              endif
              sigy(i) = min(sfmax,sigy(i))
            else
              sigy(i) = aa*(one + (pnew(i)/t0))*(one - dmg(i))
              if (epsd(i) > eps0) then 
                sigy(i) = sigy(i)*(one + cc*log(epsd(i)/eps0))
              endif
            endif
          enddo
!
          !========================================================================
          ! - Radial return mapping for deviatoric stress tensor
          !========================================================================
          do i=1,nel
            if (off(i) == one) then 
              sigstar = vm(i)/fc
              if (sigstar < sigy(i)) then
                scale(i) = one
              elseif (vm(i) > zero) then
                scale(i) = sigy(i)/sigstar
              else
                scale(i) = zero
              endif
              signxx(i) = scale(i)*signxx(i)
              signyy(i) = scale(i)*signyy(i)
              signzz(i) = scale(i)*signzz(i)
              signxy(i) = scale(i)*signxy(i)
              signyz(i) = scale(i)*signyz(i)
              signzx(i) = scale(i)*signzx(i)
              dpla(i)   = (one - scale(i))*vm(i)/(three*g)
              defp(i)   = defp(i) + dpla(i)  
            endif
          enddo
!
          !========================================================================
          ! - Update plastic strain and damage without non-local regularization
          !========================================================================
          if (inloc == 0) then 
            nindx = 0
            indx(1:nel) = 0
            do i=1,nel
              if (off(i) == one) then 
                ! compute plastic strain at failure
                if ((pstar(i)+t0/fc) >= zero) then
                  epfail = d1*(pstar(i)+t0/fc)**d2
                else
                  epfail = zero
                endif
                epfail = max(epfail,efmin)
                ! update plastic strain and damage
                dmg(i) = dmg(i) + (dpla(i) + dmup(i))/epfail
                dmg(i) = min(dmg(i),one)
                ! check element deletion
                if (idel == 1) then 
                  if ((pstar(i)+t0/fc) <= zero) then 
                    off(i) = four_over_5
                    nindx  = nindx + 1
                    indx(nindx) = i
                    idel7nok = 1
                  endif 
                elseif (idel == 2) then 
                  if (defp(i) > emax) then 
                    off(i) = four_over_5
                    nindx  = nindx + 1
                    indx(nindx) = i
                    idel7nok = 1
                  endif
                elseif (idel == 3) then 
                  if (fc*sigy(i) <= zero) then 
                    off(i) = four_over_5
                    nindx  = nindx + 1
                    indx(nindx) = i
                    idel7nok = 1
                  endif   
                elseif (idel == 4) then 
                  if (dmg(i) >= one) then 
                    off(i) = four_over_5
                    nindx  = nindx + 1
                    indx(nindx) = i
                    idel7nok = 1
                  endif          
                endif
              endif
            enddo    
          endif
!
          !========================================================================
          ! - Update stress tensor and sound speed
          !========================================================================
          do i=1,nel
            ! Yield stress 
            sigy(i) = fc*sigy(i)
            ! User variables
            uvar(i,1) = mup(i)
            uvar(i,2) = phard(i)
            uvar(i,3) = amu(i)
            uvar(i,4) = sigy(i)
            ! Add pressure to the stress tensor
            signxx(i) = signxx(i)-pnew(i)
            signyy(i) = signyy(i)-pnew(i)
            signzz(i) = signzz(i)-pnew(i)
            ! Sound speed
            ssp(i) = sqrt((dpdmu(i)+four_over_3*g)/rho0(i))
            ! Non-local variable to regularize
            if (inloc > 0) then 
              if (off(i) == one) then 
                varnl(i) = defp(i) + mup(i)
                if (dmg(i) >= one) then 
                  varnl(i) = zero
                endif
              else
                varnl(i) = zero
              endif 
            endif 
          enddo          
!     
          !========================================================================
          ! - Print out element deletion data
          !========================================================================
          if (nindx>0)then
            do j=1,nindx
              write(iout, 1000) ngl(indx(j)),tt
              write(istdo,1000) ngl(indx(j)),tt
            enddo
          endif
 1000 format(1x,'-- RUPTURE (JHC) OF SOLID ELEMENT :',I10,' AT TIME :',1PE12.4)     

        end subroutine sigeps126
      end module sigeps126_mod  