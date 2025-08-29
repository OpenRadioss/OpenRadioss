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
!||    fail_composite_c_mod   ../engine/source/materials/fail/composite/fail_composite_c.F90
!||--- called by ------------------------------------------------------
!||    mulawc                 ../engine/source/materials/mat_share/mulawc.F90
!||    usermat_shell          ../engine/source/materials/mat_share/usermat_shell.F
!||====================================================================
      module fail_composite_c_mod
      implicit none
      contains
!||====================================================================
!||    fail_composite_c   ../engine/source/materials/fail/composite/fail_composite_c.F90
!||--- called by ------------------------------------------------------
!||    mulawc             ../engine/source/materials/mat_share/mulawc.F90
!||    usermat_shell      ../engine/source/materials/mat_share/usermat_shell.F
!||--- uses       -----------------------------------------------------
!||    constant_mod       ../common_source/modules/constant_mod.F
!||    fail_param_mod     ../common_source/modules/mat_elem/fail_param_mod.F90
!||    precision_mod      ../common_source/modules/precision_mod.F90
!||====================================================================
        subroutine fail_composite_c(                                             &
          nel       ,fail      ,nuvar     ,uvar      ,time      ,ngl       ,  &
          ipg       ,ilay      ,ipt       ,ply_id    ,igtyp     ,tdel      ,  &
          signxx    ,signyy    ,signxy    ,foff      ,dmg_flag  ,dmgscl    ,  &
          lf_dammx  ,dfmax     )
! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use constant_mod
          use precision_mod , only : WP
          use fail_param_mod
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
          integer, intent(in)                     :: ipg      !< Gauss point number
          integer, intent(in)                     :: ilay     !< Layer number
          integer, intent(in)                     :: ipt      !< Integration point number
          integer, intent(in)                     :: ply_id   !< Ply ID
          integer, intent(in)                     :: igtyp    !< Property type
          real(kind=WP), dimension(nel), intent(inout)  :: signxx   !< Stress xx
          real(kind=WP), dimension(nel), intent(inout)  :: signyy   !< Stress yy
          real(kind=WP), dimension(nel), intent(inout)  :: signxy   !< Stress xy
          integer, dimension(nel), intent(inout)  :: foff     !< Integration point failure flag
          integer, intent(inout)                  :: dmg_flag !< Damage softening flag
          real(kind=WP), dimension(nel), intent(inout)  :: dmgscl   !< Damage softening scaling factor
          integer, intent(in)                     :: lf_dammx !< Flag for damage max value
          real(kind=WP), dimension(nel,lf_dammx), intent(inout) :: dfmax !< Damage variable
          real(kind=WP), dimension(nel), intent(inout)  :: tdel     !< Deletion time
! ----------------------------------------------------------------------------------------------------------------------
!                                                   local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j,k,indx(nel),nindx,ifail_sh
          real(kind=WP) :: sigt1,sigc1,sigt2,sigc2,sig12,beta,expn,tmax
!
          !=========================================================================
          !< Recover failure criterion parameters
          !=========================================================================
          !< Integer parameters
          ifail_sh = fail%iparam(1) !< Shell element failure flag
          !< Real parameters
          sigt1 = fail%uparam(1)    !< Critical tensile stress in material direction 1
          sigc1 = fail%uparam(2)    !< Critical compression stress in material direction 1
          sigt2 = fail%uparam(3)    !< Critical tensile stress in material direction 2
          sigc2 = fail%uparam(4)    !< Critical compression stress in material direction 2
          sig12 = fail%uparam(5)    !< Critical shear stress in material plane 12
          beta  = fail%uparam(10)   !< Shear scaling factor
          expn  = fail%uparam(11)   !< Exponent
          tmax  = fail%uparam(12)   !< Dynamic time relaxation time
!
          !< Stress softening activation
          dmg_flag = 1
!
          !====================================================================
          !< - COMPUTATION OF THE DAMAGE VARIABLE EVOLUTION
          !====================================================================
          !< Initialization of element failure index
          nindx = 0
          indx(1:nel) = 0
!
          !< Loop over the elements
          do i=1,nel
!
            !< If damage has not been reached yet
            if (dfmax(i,1) < one) then
!
              !< Mode 1: tensile in direction 1
              if (signxx(i) >= zero) then
                dfmax(i,2) = max((abs(signxx(i))/sigt1)**expn +                    &
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
                  beta*(abs(signxy(i))/sig12)**expn, dfmax(i,4))
                dfmax(i,4) = min(dfmax(i,4),one)
                !< Mode 4: compression in direction 2
              else
                dfmax(i,5) = max((abs(signyy(i))/sigc2)**expn +                    &
                  beta*(abs(signxy(i))/sig12)**expn, dfmax(i,5))
                dfmax(i,5) = min(dfmax(i,5),one)
              endif
!
              !< Mode 5: Shear in direction 12
              dfmax(i,6) = (abs(signxy(i))/sig12)**expn
              dfmax(i,6) = min(dfmax(i,6),one)
!
              !< Global failure index
              dfmax(i,1) = max(dfmax(i,1),dfmax(i,2),dfmax(i,3),                   &
                dfmax(i,4),dfmax(i,5),dfmax(i,6))
              dfmax(i,1) = min(dfmax(i,1),one)
              if (dfmax(i,1) >= one) then
                nindx = nindx+1
                indx(nindx) = i
                if (ifail_sh > 0) then
                  uvar(i,1) = time
                endif
              endif
            endif
!
            ! Stress relaxation in case of damage reached
            if ((uvar(i,1) > zero).and.(foff(i) /= 0).and.(ifail_sh > 0)) then
              dmgscl(i) = exp(-(time - uvar(i,1))/tmax)
              if (dmgscl(i) < em02) then
                foff(i) = 0
                tdel(i) = time
                dmgscl(i) = zero
              endif
            endif
          enddo
!
          !====================================================================
          !< - PRINTOUT DATA ABOUT FAILED INTEGRATION POINTS
          !====================================================================
          if (nindx > 0) then
            do j = 1,nindx
              i = indx(j)
              if (igtyp == 17 .or. igtyp == 51 .or. igtyp == 52) then
                write(iout ,1200) ngl(i),ipg,ply_id,ipt
                write(istdo,1200) ngl(i),ipg,ply_id,ipt
              elseif (igtyp == 1 .or. igtyp == 9) then
                write(iout ,1000) ngl(i),ipg,ipt
                write(istdo,1000) ngl(i),ipg,ipt
              else
                write(iout ,1100) ngl(i),ipg,ilay,ipt
                write(istdo,1100) ngl(i),ipg,ilay,ipt
              endif
              do k = 2,6
                if (dfmax(i,k) >= one) then
                  write(iout ,2000) k-1,trim(fail%mode(k-1))
                  write(istdo,2000) k-1,trim(fail%mode(k-1))
                endif
              enddo
            end do
          endif
! ----------------------------------------------------------------------------------------------------------------------
1000      format(1X,'FAILURE (COMPOSITE) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',    &
            I2,1X,',INTEGRATION PT',I3)
1100      format(1X,'FAILURE (COMPOSITE) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',    &
            I2,1X,',LAYER',I3,1X,',INTEGRATION PT',I3)
1200      format(1X,'FAILURE (COMPOSITE) OF SHELL ELEMENT ',I10,1X,',GAUSS PT',    &
            I2,1X,',PLY ID',I10,1X,',INTEGRATION PT',I3)
2000      format(1X,'---- MODE ',I2,':',1X,A)
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine fail_composite_c
      end module fail_composite_c_mod
