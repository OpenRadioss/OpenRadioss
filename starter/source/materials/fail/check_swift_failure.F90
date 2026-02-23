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
!||    check_swift_failure_mod   ../starter/source/materials/fail/check_swift_failure.F90
!||--- called by ------------------------------------------------------
!||    law02_upd                 ../starter/source/materials/mat/mat002/law02_upd.F90
!||====================================================================
      module check_swift_failure_mod
      implicit none
      contains
        ! ==========================================================================================
        ! \brief check if the failure models linked to material model are activate swift option
        ! ==========================================================================================
!||====================================================================
!||    check_swift_failure   ../starter/source/materials/fail/check_swift_failure.F90
!||--- called by ------------------------------------------------------
!||    law02_upd             ../starter/source/materials/mat/mat002/law02_upd.F90
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine check_swift_failure(fail, swift_reg)
! --------------------------------------------------------------------------------------------------
!         Modules
! --------------------------------------------------------------------------------------------------
          use fail_param_mod
! --------------------------------------------------------------------------------------------------
          implicit none
! --------------------------------------------------------------------------------------------------
!         Global arguments
! --------------------------------------------------------------------------------------------------
          type (fail_param_) ,intent(in)  :: fail        !< failure model data structure
          logical            ,intent(out) :: swift_reg
! --------------------------------------------------------------------------------------------------
!         Local variables
! --------------------------------------------------------------------------------------------------
          integer :: i,failure_model,ireg,ninievo,initype
! ==================================================================================================
          swift_reg = .false.        
          failure_model = fail%irupt
          select case (failure_model)
            case(30)                             ! biquad
              ireg = fail%iparam(1)
              if (ireg == 2) swift_reg = .true.        
            case(38)                             ! orth_biquad
              ireg = fail%iparam(1)
              if (ireg == 2) swift_reg = .true.        
            case(39)                             ! gene1

            case(41)                             ! fail_tab2
              ireg = fail%iparam(2)
              if (ireg == 3) swift_reg = .true.        
            case(42)                             ! inievo
              ninievo = nint(fail%uparam(1))
              do i = 1,ninievo
                initype = nint(fail%uparam(6+(i-1)*14))
                ireg = fail%iparam(2)
                if (initype == 1 .and. ireg == 2) swift_reg = .true.  
              end do

          end select
!-----------------------------------------------
          return 
          end subroutine check_swift_failure
          end module check_swift_failure_mod


