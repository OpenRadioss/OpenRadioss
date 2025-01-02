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
      !||    retractor_table_inv2_mod   ../engine/source/tools/seatbelts/retractor_table_inv2.F90
      !||--- called by ------------------------------------------------------
      !||    material_flow              ../engine/source/tools/seatbelts/material_flow.F
      !||====================================================================
      module retractor_table_inv2_mod
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
      !||    retractor_table_inv2   ../engine/source/tools/seatbelts/retractor_table_inv2.F90
      !||--- called by ------------------------------------------------------
      !||    material_flow          ../engine/source/tools/seatbelts/material_flow.F
      !||--- calls      -----------------------------------------------------
      !||    ancmsg                 ../engine/source/output/message/message.F
      !||    arret                  ../engine/source/system/arret.F
      !||--- uses       -----------------------------------------------------
      !||    constant_mod           ../common_source/modules/constant_mod.F
      !||    message_mod            ../engine/share/message_module/message_mod.F
      !||    table_mod              ../engine/share/modules/table_mod.F
      !||====================================================================
        subroutine retractor_table_inv2(table,xx,yy,xx_prev)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
          use table_mod , only: TTABLE
          use constant_mod , only: one,zero,ep20
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
          my_real,                                   intent(in) :: xx_prev                     !< last abcissa
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: ndim, ipos, nxk, i
          my_real :: r, dy2, unr, xx_temp, error
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
!          
!         works only for monotonic increasing tables - null slope ine table treated in starter for retractors  
!         similar as retractor_table_inv but for non monotonic table
!         will return solution that is the closest to last known solution xx_prev                
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
          error = ep20 
          do i = 2, nxk
!            dy2 = table%y%values(i) - yy
!            if (dy2 >= zero .or. i == nxk) then
            if (((yy >= table%y%values(i-1)).and.(yy <= table%y%values(i))).or.  &
                ((yy >= table%y%values(i)).and.(yy <= table%y%values(i-1)))) then
              ipos = i - 1
              r = (table%y%values(i) - yy) / (table%y%values(i) - table%y%values(i - 1))
              unr = one - r
              xx_temp = r * table%x(1)%values(ipos) + unr * table%x(1)%values(ipos + 1)
              if (abs(xx_temp - xx_prev) < error) then
                xx = xx_temp
                error = abs(xx_temp - xx_prev)
              end if
            endif
          end do
!
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine retractor_table_inv2
      end module retractor_table_inv2_mod
