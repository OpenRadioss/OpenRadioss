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
!||    retractor_table_inv_mod   ../engine/source/tools/seatbelts/retractor_table_inv.F90
!||--- called by ------------------------------------------------------
!||    material_flow             ../engine/source/tools/seatbelts/material_flow.F
!||====================================================================
      module retractor_table_inv_mod
      implicit none
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!
!=======================================================================================================================
!!\brief This subroutine find abcissa XX from ordinate YY in table
!=======================================================================================================================
!
!||====================================================================
!||    retractor_table_inv   ../engine/source/tools/seatbelts/retractor_table_inv.F90
!||--- called by ------------------------------------------------------
!||    material_flow         ../engine/source/tools/seatbelts/material_flow.F
!||--- calls      -----------------------------------------------------
!||    ancmsg                ../engine/source/output/message/message.F
!||    arret                 ../engine/source/system/arret.F
!||--- uses       -----------------------------------------------------
!||    constant_mod          ../common_source/modules/constant_mod.F
!||    message_mod           ../engine/share/message_module/message_mod.F
!||    precision_mod         ../common_source/modules/precision_mod.F90
!||    table_mod             ../engine/share/modules/table_mod.F
!||====================================================================
        subroutine retractor_table_inv(table,xx,yy)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use table_mod , only: TTABLE
          use constant_mod , only: one,zero
          use precision_mod, only : WP
          use message_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(TTABLE),                              intent(in) :: TABLE                       !< local table of retractor
          real(kind=WP),                                  intent(out) :: xx                          !< abcissa
          real(kind=WP),                                   intent(in) :: yy                          !< ordinate
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ndim, ipos, nxk, i
          real(kind=WP) :: r, dy2, unr
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!
!         works only for monotonic increasing tables - null slope ine table treated in starter for retractors
!
          ndim = table%ndim
          if (ndim > 1) then
            call ancmsg(msgid=36, anmode=aninfo, c1="table interpolation")
            call arret(2)
          end if
!
          ipos = 1
          r = one
          nxk = size(table%x(1)%values)
!
          do i = 2, nxk
            dy2 = table%y%values(i) - yy
            if (dy2 >= zero .or. i == nxk) then
              ipos = i - 1
              if (table%y%values(i) == table%y%values(i - 1)) then
                r = one
              else
                r = (table%y%values(i) - yy) / (table%y%values(i) - table%y%values(i - 1))
              end if
              exit
            end if
          end do
          unr = one - r
!
          xx = r * table%x(1)%values(ipos) + unr * table%x(1)%values(ipos + 1)
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine retractor_table_inv
      end module retractor_table_inv_mod
