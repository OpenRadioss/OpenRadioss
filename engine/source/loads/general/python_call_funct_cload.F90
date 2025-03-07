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

       module python_call_funct_cload_mod
         contains

      !||====================================================================
      !||    python_call_funct1d_dp   ../common_source/modules/python_mod.F90
      !||--- called by ------------------------------------------------------
      !||    python_solve             ../common_source/modules/python_mod.F90
      !||--- calls      -----------------------------------------------------
      !||====================================================================
        subroutine python_call_funct_cload(py, funct_id, x, y,n,nodes)
          use python_funct_mod
          use nodal_arrays_mod
          implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_),                      intent(in) :: py !< the Fortran structure that holds the python function
          integer,                            intent(in) :: funct_id !< the id of the python function
          my_real,                               intent(in) :: x !< the input value
          my_real,                               intent(out) :: y !< the output value
          integer,                                        intent(in) :: n
          type(nodal_arrays_),                            intent(in) :: nodes
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(1) :: argin, argout
          my_real, dimension(3) :: zeros
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          argin(1) = dble(x)
          zeros(1:3) = 0.0d0
          ! the double precision function is called, then the result is converted to single
!      subroutine python_set_active_node_values(name_len, name, val) bind(c, name="cpp_python_update_active_node")
          if(n <= size(nodes%x,2) .and. n > 0) then
            call python_set_active_node_values(1,"C",nodes%X(:,n))
          else
            call python_set_active_node_values(1,"C",zeros)
          endif
          if(n <= size(nodes%A,2) .and. n > 0) then
            call python_set_active_node_values(1,"A",nodes%A(:,n))
          else
            call python_set_active_node_values(1,"A",zeros)
          endif
          if(n <= size(nodes%D,2) .and. n > 0) then
            call python_set_active_node_values(1,"D",nodes%D(:,n))
          else
            call python_set_active_node_values(1,"D",zeros)
          endif
          if(n <= size(nodes%DR,2) .and. n > 0) then
            call python_set_active_node_values(2,"DR",nodes%DR(:,n))
          else
            call python_set_active_node_values(2,"DR",zeros)
          endif
          if(n <= size(nodes%V,2) .and. n > 0) then
            call python_set_active_node_values(1,"V",nodes%V(:,n))
          else
            call python_set_active_node_values(1,"V",zeros)
          endif
          if(n <= size(nodes%VR,2) .and. n > 0) then
            call python_set_active_node_values(2,"VR",nodes%VR(:,n))
          else
            call python_set_active_node_values(2,"VR",zeros)
          endif
          if(n <= size(nodes%AR,2) .and. n > 0) then
            call python_set_active_node_values(2,"AR",nodes%AR(:,n))
          else
            call python_set_active_node_values(2,"AR",zeros)
          endif
!$OMP CRITICAL
          call python_call_function(py%functs(funct_id)%name, 1, argin, 1, argout)
!$OMP END CRITICAL
#ifdef MYREAL8
          y = argout(1)
#else
          y = real(argout(1),kind(1.0))
#endif
        end subroutine
        end module





