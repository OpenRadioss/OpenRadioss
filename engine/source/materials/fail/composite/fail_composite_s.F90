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
!||    fail_composite_s_mod   ../engine/source/materials/fail/composite/fail_composite_s.F90
!||--- called by ------------------------------------------------------
!||    mmain                  ../engine/source/materials/mat_share/mmain.F90
!||    mulaw                  ../engine/source/materials/mat_share/mulaw.F90
!||    usermat_solid          ../engine/source/materials/mat_share/usermat_solid.F
!||====================================================================
      module fail_composite_s_mod
      implicit none
      contains
!||====================================================================
!||    fail_composite_s   ../engine/source/materials/fail/composite/fail_composite_s.F90
!||--- called by ------------------------------------------------------
!||    mmain              ../engine/source/materials/mat_share/mmain.F90
!||    mulaw              ../engine/source/materials/mat_share/mulaw.F90
!||    usermat_solid      ../engine/source/materials/mat_share/usermat_solid.F
!||--- uses       -----------------------------------------------------
!||    constant_mod       ../common_source/modules/constant_mod.F
!||    fail_param_mod     ../common_source/modules/mat_elem/fail_param_mod.F90
!||====================================================================
        subroutine fail_composite_s(                                             &
          nel       ,fail      ,nuvar     ,uvar      ,time      ,ngl       ,  &
          ip        ,ilay      ,npg       ,tdele     ,off       ,loff      ,  &
          signxx    ,signyy    ,signzz    ,signxy    ,signyz    ,signzx    ,  &
          dmgscl    ,lf_dammx  ,dfmax     ,noff      )
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod
          use fail_param_mod
          use precision_mod , only : WP
! ----------------------------------------------------------------------------------------------------------------------
!                                                 implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
#include "units_c.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                              Arguments s
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in)                     :: nel      !< Number of elements
          type(fail_param_), intent(in)           :: fail     !< Failure parameters data structure
          integer, intent(in)                     :: nuvar    !< Number of user variables
          real(kind=WP), dimension(nel,nuvar), intent(inout) :: uvar !< User variables
          real(kind=WP), intent(in)                     :: time     !< Current time
          integer, dimension(nel), intent(in)     :: ngl      !< Global element numbers
          integer, intent(in)                     :: ip       !< Gauss point number
          integer, intent(in)                     :: ilay     !< Layer number
          integer, intent(in)                     :: npg      !< Number of Gauss points
          real(kind=WP), dimension(nel), intent(inout)  :: tdele    !< Deletion time
          real(kind=WP), dimension(nel), intent(inout)  :: off      !< Element failure flag
          real(kind=WP), dimension(nel), intent(inout)  :: loff     !< Integration point failure flag
          real(kind=WP), dimension(nel), intent(inout)  :: signxx   !< Stress xx
          real(kind=WP), dimension(nel), intent(inout)  :: signyy   !< Stress yy
          real(kind=WP), dimension(nel), intent(inout)  :: signzz   !< Stress zz
          real(kind=WP), dimension(nel), intent(inout)  :: signxy   !< Stress xy
          real(kind=WP), dimension(nel), intent(inout)  :: signyz   !< Stress yz
          real(kind=WP), dimension(nel), intent(inout)  :: signzx   !< Stress zx
          real(kind=WP), dimension(nel), intent(inout)  :: dmgscl   !< Damage softening scaling factor
          integer, intent(in)                     :: lf_dammx !< Flag for damage max value
          real(kind=WP), dimension(nel,lf_dammx), intent(inout) :: dfmax !< Damage variable
          integer, dimension(nel), intent(inout)  :: noff     !< Number of failed integration points
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,k,indx(nel),nindx,ifail_so,indx0(nel),nindx0
          real(kind=WP) :: sigt1,sigc1,sigt2,sigc2,sig12,sigt3,sigc3,sig23,sig31,beta,   &
            expn,tmax
!
          !=========================================================================
          !< Recover failure criterion parameters
          !=========================================================================
          !< Integer parameters
          ifail_so = fail%iparam(2) !< Shell element failure flag
          !< Real parameters
          sigt1 = fail%uparam(1)    !< Critical tensile stress in material direction 1
          sigc1 = fail%uparam(2)    !< Critical compression stress in material direction 1
          sigt2 = fail%uparam(3)    !< Critical tensile stress in material direction 2
          sigc2 = fail%uparam(4)    !< Critical compression stress in material direction 2
          sig12 = fail%uparam(5)    !< Critical shear stress in material plane 12
          sigt3 = fail%uparam(6)    !< Critical tensile stress in material direction 3
          sigc3 = fail%uparam(7)    !< Critical compression stress in material direction 3
          sig23 = fail%uparam(8)    !< Critical shear stress in material plane 23
          sig31 = fail%uparam(9)    !< Critical shear stress in material plane 31
          beta  = fail%uparam(10)   !< Shear scaling factor
          expn  = fail%uparam(11)   !< Exponent
          tmax  = fail%uparam(12)   !< Dynamic time relaxation time
!
          !====================================================================
          !< - COMPUTATION OF THE DAMAGE VARIABLE EVOLUTION
          !====================================================================
          !< Initialization of element failure index
          nindx = 0
          indx(1:nel) = 0
          nindx0 = 0
          indx0(1:nel) = 0
!
          !< Loop over the elements
          do i=1,nel
!
            !< If damage has not been reached yet
            if (dfmax(i,1) < one) then
!
              !< Mode 1: tensile in direction 1
              if (signxx(i) >= zero) then
                dfmax(i,2) = max((abs(signxx(i))/sigt1)**expn +                   &
                  beta*(abs(signxy(i))/sig12)**expn,dfmax(i,2))
                dfmax(i,2) = min(dfmax(i,2),one)
                !< Mode 2: compression in direction 1
              else
                dfmax(i,3) = max((abs(signxx(i))/sigc1)**expn +                    &
                  beta*(abs(signxy(i))/sig12)**expn,dfmax(i,3))
                dfmax(i,3) = min(dfmax(i,3),one)
              endif
!
              !< Mode 3: tensile in direction 2
              if (signyy(i) >= zero) then
                dfmax(i,4) = max((abs(signyy(i))/sigt2)**expn +                    &
                  beta*(abs(signxy(i))/sig12)**expn,dfmax(i,4))
                dfmax(i,4) = min(dfmax(i,4),one)
                !< Mode 4: compression in direction 2
              else
                dfmax(i,5) = max((abs(signyy(i))/sigc2)**expn +                    &
                  beta*(abs(signxy(i))/sig12)**expn,dfmax(i,5))
                dfmax(i,5) = min(dfmax(i,5),one)
              endif
!
              !< Mode 5: Shear in direction 12
              dfmax(i,6) = (abs(signxy(i))/sig12)**expn
              dfmax(i,6) = min(dfmax(i,6),one)
!
              !< Mode 6: tensile in direction 3
              if (signzz(i) >= zero) then
                dfmax(i,7) = max((abs(signzz(i))/sigt3)**expn +                    &
                  beta*(abs(signyz(i))/sig23)**expn +                    &
                  beta*(abs(signzx(i))/sig31)**expn,dfmax(i,7))
                dfmax(i,7) = min(dfmax(i,7),one)
                !< Mode 7: compression in direction 3
              else
                dfmax(i,8) = max((abs(signzz(i))/sigc3)**expn +                    &
                  beta*(abs(signyz(i))/sig23)**expn +                    &
                  beta*(abs(signzx(i))/sig31)**expn,dfmax(i,8))
                dfmax(i,8) = min(dfmax(i,8),one)
              endif
!
              !< Mode 8: Shear in direction 23
              dfmax(i,9) = (abs(signyz(i))/sig23)**expn
              dfmax(i,9) = min(dfmax(i,9),one)
!
              !< Mode 9: Shear in direction 31
              dfmax(i,10) = (abs(signzx(i))/sig31)**expn
              dfmax(i,10) = min(dfmax(i,10),one)
!
              !< Global failure index
              dfmax(i,1) = max(dfmax(i,1),dfmax(i,2),dfmax(i,3),                   &
                dfmax(i,4),dfmax(i,5),dfmax(i,6),                   &
                dfmax(i,7),dfmax(i,8),dfmax(i,9),dfmax(i,10))
              dfmax(i,1) = min(dfmax(i,1),one)
              if (dfmax(i,1) >= one) then
                nindx = nindx+1
                indx(nindx) = i
                if (ifail_so > 0) then
                  uvar(i,1) = time
                endif
              endif
            endif
!
            ! Stress relaxation in case of damage reached
            if ((uvar(i,1) > zero).and.(loff(i) /= zero).and.                      &
              (ifail_so > 0)    .and.( off(i) /= zero)) then
              dmgscl(i) = exp(-(time - uvar(i,1))/tmax)
              if (dmgscl(i) < em02) then
                loff(i)   = zero
                tdele(i)  = time
                dmgscl(i) = zero
                if (ifail_so == 1) then
                  off(i) = zero
                  nindx0 = nindx0 + 1
                  indx0(nindx0) = i
                elseif (ifail_so == 2) then
                  noff(i) = noff(i) + 1
                  if (noff(i) >= npg) then
                    off(i) = zero
                    nindx0 = nindx0 + 1
                    indx0(nindx0) = i
                  endif
                endif
              endif
            endif
          enddo
!
          !====================================================================
          !< - PRINTOUT DATA ABOUT FAILED INTEGRATION POINTS/ELEMENTS
          !====================================================================
          if (nindx > 0) then
            do j = 1,nindx
              i = indx(j)
              write(iout, 1000) ngl(i),ip,ilay
              write(istdo,1100) ngl(i),ip,ilay,time
              do k = 2,10
                if (dfmax(i,k) >= one) then
                  write(iout ,2000) k-1,trim(fail%mode(k-1))
                  write(istdo,2000) k-1,trim(fail%mode(k-1))
                endif
              enddo
            end do
          endif
!
          if (nindx0 > 0) then
            do j=1,nindx0
              i = indx0(j)
              write(iout, 1200) ngl(i),time
              write(istdo,1200) ngl(i),time
            end do
          endif
! ----------------------------------------------------------------------------------------------------------------------
1000      format(1X,'FAILURE (COMPOSITE) OF SOLID ELEMENT ',I10,1X,                &
            ',GAUSS PT',I5,1X,',LAYER',I5)
1100      format(1X,'FAILURE (COMPOSITE) OF SOLID ELEMENT ',I10,1X,                &
            ',GAUSS PT',I5,1X,',LAYER',I5,1X,'AT TIME :',1PE20.13)
1200      format(1X,'-- RUPTURE OF SOLID ELEMENT : ',I10,1X,                       &
            'AT TIME :',1PE20.13)
2000      format(1X,'---- MODE ',I2,':',1X,A)
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine fail_composite_s
      end module fail_composite_s_mod
