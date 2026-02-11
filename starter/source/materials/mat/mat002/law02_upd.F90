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
!||    law02_upd_mod   ../starter/source/materials/mat/mat02/law02_upd.F90
!||--- called by ------------------------------------------------------
!||    updmat           ../starter/source/materials/updmat.F
!||====================================================================
      module law02_upd_mod
      implicit none
      contains
        ! ==========================================================================================
        ! \brief Updating material parameters of /mat/law02
        ! \details create tabulated hardening function for failure models 
        ! ==========================================================================================
!||====================================================================
!||    law02_upd         ../starter/source/materials/mat/mat002/law02_upd.F90
!||--- called by ------------------------------------------------------
!||    updmat             ../starter/source/materials/updmat.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine law02_upd(mat_param)
! --------------------------------------------------------------------------------------------------
!                                                        Modules
! --------------------------------------------------------------------------------------------------
          use matparam_def_mod
          use mat_hardening_to_fail_mod
          use check_swift_failure_mod
          use constant_mod  ,only : one
          use precision_mod ,only : WP
! --------------------------------------------------------------------------------------------------
          implicit none
! --------------------------------------------------------------------------------------------------
!         Dummy arguments
! --------------------------------------------------------------------------------------------------
          type(matparam_struct_) ,intent(inout) :: mat_param !< Material parameters data structure
! --------------------------------------------------------------------------------------------------
!         Local variables
! --------------------------------------------------------------------------------------------------
          integer :: i,ifail
          integer, parameter :: npt = 200
          real(kind=WP) :: ca,cb,cn
          real(kind=WP) :: deps
          real(kind=WP) ,dimension(npt) :: eps,sig
          logical :: swift_reg
! ==================================================================================================
    ! Check connected failure models compatible with Swift diffuse instability regularization option
!
          do ifail = 1,mat_param%nfail
            call check_swift_failure(mat_param%fail(ifail), swift_reg)
!
            ! generate static tabulated hardening curve for failure model
!
            if (swift_reg) then
              ca = mat_param%uparam(1)
              cb = mat_param%uparam(2)
              cn = mat_param%uparam(3)
              deps = one / (npt-1)  ! plastic strain increment in the range = <0,1>
              do i=1,npt
                eps(i) = (i-1) * deps
                sig(i) = ca + cb*eps(i)**cn
              end do                  
!
              ! save generated hardening curve in failure model data structure
              
              call mat_hardening_to_fail(npt, eps(1:npt), sig(1:npt), mat_param%fail(ifail))
            end if
          end do  ! ifail = 1...nfail
!-------------------------------
          return
          end subroutine law02_upd
          end module law02_upd_mod
