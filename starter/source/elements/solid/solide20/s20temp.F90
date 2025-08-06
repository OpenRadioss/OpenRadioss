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
! ======================================================================================================================
!                                                      procedures
! ======================================================================================================================
!! \brief calculate element temperature in integration point from nodal values
!! \details
! ======================================================================================================================
!||====================================================================
!||    s20temp_mod   ../starter/source/elements/solid/solide20/s20temp.F90
!||--- called by ------------------------------------------------------
!||    s16init3      ../starter/source/elements/thickshell/solide16/s16init3.F
!||    s20init3      ../starter/source/elements/solid/solide20/s20init3.F
!||====================================================================
      module s20temp_mod
        implicit none
      contains


!||====================================================================
!||    s20temp         ../starter/source/elements/solid/solide20/s20temp.F90
!||--- called by ------------------------------------------------------
!||    s16init3        ../starter/source/elements/thickshell/solide16/s16init3.F
!||    s20init3        ../starter/source/elements/solid/solide20/s20init3.F
!||--- uses       -----------------------------------------------------
!||====================================================================
        subroutine s20temp(nel ,numnod ,mvsiz ,npe, nc, ni, temp ,tempel)

! ----------------------------------------------------------------------------------------------------------------------
!                                                        Modules
! ----------------------------------------------------------------------------------------------------------------------
          use precision_mod, only : WP
          use constant_mod, only : zero
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   arguments 
! ----------------------------------------------------------------------------------------------------------------------
          integer ,intent(in) :: nel
          integer ,intent(in) :: numnod
          integer ,intent(in) :: mvsiz
          integer ,intent(in) :: npe
          integer ,dimension(mvsiz,npe) ,intent(in)  :: nc       !< element connectivity
          real(kind=WP) ,dimension(npe)       ,intent(in)  :: ni       !< form functions
          real(kind=WP) ,dimension(numnod)    ,intent(in)  :: temp     !< nodal temperature
          real(kind=WP) ,dimension(nel)       ,intent(out) :: tempel   !< element temperature in Gauss point
! ----------------------------------------------------------------------------------------------------------------------
!                                              L o c a l v a r i a b l e s
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,j
! ======================================================================================================================
          tempel(1:nel) = zero
          do i=1,nel
            do j=1,npe
              tempel(i) = tempel(i) + ni(j) * temp(nc(i,j))
            enddo
          enddo
! ----------------------------------------------------------------------------------------------------------------------
          return
        end subroutine s20temp
      end module s20temp_mod
