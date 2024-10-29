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
      !||====================================================================
      !||    damping_vref_rby_mod   ../engine/source/assembly/damping_vref_rby.F90
      !||--- called by ------------------------------------------------------
      !||    rbyfor                 ../engine/source/constraints/general/rbody/rbyfor.F
      !||====================================================================
      module retractor_table_inv_mod
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
      !||    damping_vref_rby            ../engine/source/assembly/damping_vref_rby.F90
      !||--- called by ------------------------------------------------------
      !||    rbyfor                      ../engine/source/constraints/general/rbody/rbyfor.F
      !||--- calls      -----------------------------------------------------
      !||    damping_vref_sum6_rby       ../engine/source/assembly/damping_vref_sum6_rby.F90
      !||    get_u_func                  ../engine/source/user_interface/ufunc.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod                ../common_source/modules/constant_mod.F
      !||    damping_vref_sum6_rby_mod   ../engine/source/assembly/damping_vref_sum6_rby.F90
      !||    groupdef_mod                ../common_source/modules/groupdef_mod.F
      !||====================================================================
        subroutine retractor_table_inv(table,xx,yy)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use table_mod , only: TTABLE
          use constant_mod , only: one,zero
          use message_mod
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(TTABLE),                              intent(in) :: TABLE                       !< local table of retractor
          my_real,                                  intent(out) :: xx                          !< abcissa
          my_real,                                   intent(in) :: yy                          !< ordinate
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ndim, ipos, nxk, i
          my_real :: r, dy2, unr
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!          
!         works only for monotonic increasing tables - null slope ine table treated in starter for retractors        
!         
          ndim = table%ndim
          if (ndim > 1) then
            call ancmsg(msgid=36, anmode=aninfo, c1='table interpolation')
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
              r = (table%y%values(i) - yy) / (table%y%values(i) - table%y%values(i - 1))
              exit
            endif
          end do
          unr = one - r
!
          xx = r * table%x(1)%values(ipos) + unr * table%x(1)%values(ipos + 1)
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine retractor_table_inv
      end module retractor_table_inv_mod
