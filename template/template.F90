!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
      module my_subroutine_mod
      contains
! ======================================================================================================================
!                                                   PROCEDURES
! ======================================================================================================================
!! \brief Here is a small description of the routine, [after the header]
!! \details if needed, more details can be added here
        subroutine subroutine_example(intbuf_tab, buffer,buffer_size,acceleration,acceleration_size)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Modules
! ----------------------------------------------------------------------------------------------------------------------
!  [ the module names in use must be in uppercase for now, it will change latter]
!  [ ONLY is mandatory, note the space before the ,]
          use intbuf_def_mod, only: intbuf_struct
          use constant_mod, only : PI
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
! [ no comment on the same line as #include #define #ifdef, #endif ]
! [ my_real.inc must be includeed, it was included in "implicit_f.inc"]
#include "my_real.inc"
! [ generally spealing, #include is forbidden, there are only few exceptions: ]
#include "nchar_c.inc"
#include "task_c.inc"
#include "units_c.inc"
#include "mvsiz_p.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(intbuf_struct),                       intent(in) :: intbuf_tab          !< intent(in) and intent(inout) are madatory
          integer,                                   intent(in) :: buffer_size         !< the size of the buffer
          integer,                                intent(inout) :: buffer(buffer_size) !< it is possible to allocate arrays within the routine
          integer,                                   intent(in) :: acceleration_size !< the size of array must appear before the array
          my_real,                                   intent(in) :: acceleration(3,acceleration_size) !< assumed size arrays are not allowed
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: i,l,k,m ! small integers can be declared in the same line
          integer :: pos ! more complex variable should be declared in separate lines, and described
! ----------------------------------------------------------------------------------------------------------------------
!                                                   External functions
! ----------------------------------------------------------------------------------------------------------------------
! [ external functions must be kept to mimimum ]
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Body
! ----------------------------------------------------------------------------------------------------------------------
! [ the code must be indented with 2 spaces]
! [ the code must be commented ]
! [ routines must be short, and should not exceed 200 lines for leaf routines, 1000 lines for main routines]

! [ separators can be used between blocks of code]
! ----------------------------------------------------------------------------------------------------------------------
        end subroutine subroutine_example
      end module my_subroutine_mod
