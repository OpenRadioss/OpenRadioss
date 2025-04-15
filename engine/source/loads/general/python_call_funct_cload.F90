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
      !||    python_call_funct_cload_mod   ../engine/source/loads/general/python_call_funct_cload.F90
      !||--- called by ------------------------------------------------------
      !||    fixvel                        ../engine/source/constraints/general/impvel/fixvel.F
      !||    force                         ../engine/source/loads/general/force.F90
      !||====================================================================
       module python_call_funct_cload_mod

         interface python_call_funct_cload
           module procedure python_call_funct_cload_sp
           module procedure python_call_funct_cload_dp
         end interface

         contains

      !||====================================================================
      !||    python_call_funct_cload_sp      ../engine/source/loads/general/python_call_funct_cload.F90
      !||--- calls      -----------------------------------------------------
      !||    python_set_active_node_values   ../common_source/modules/python_mod.F90
      !||--- uses       -----------------------------------------------------
      !||    nodal_arrays_mod                ../engine/source/engine/node_spliting/nodal_arrays.F90
      !||    python_funct_mod                ../common_source/modules/python_mod.F90
      !||====================================================================
        subroutine python_call_funct_cload_sp(py, funct_id, x, y,n,nodes)
          use python_funct_mod
          use nodal_arrays_mod
          implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_),                      intent(in) :: py !< the Fortran structure that holds the python function
          integer,                            intent(in) :: funct_id !< the id of the python function
          real,                               intent(in) :: x !< the input value
          real,                               intent(out) :: y !< the output value
          integer,                                        intent(in) :: n
          type(nodal_arrays_),                            intent(in) :: nodes
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(1) :: argin, argout
          double precision, dimension(3) :: tmp
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          argin(1) = dble(x)
          ! the double precision function is called, then the result is converted to single
!      subroutine python_set_active_node_values(name_len, name, val) bind(c, name="cpp_python_update_active_node")
          if(n <= size(nodes%x,2) .and. n > 0) then
            tmp = nodes%X(:,n)
            call python_set_active_node_values(1,"C",tmp)
          else
            tmp(1:3) = 0.0d0
            call python_set_active_node_values(1,"C",tmp)
          endif
          if(n <= size(nodes%A,2) .and. n > 0) then
            tmp = nodes%A(:,n)
            call python_set_active_node_values(1,"A",tmp)
          else
            tmp(1:3) = 0.0d0
            call python_set_active_node_values(1,"A",tmp)
          endif
          if(n <= size(nodes%D,2) .and. n > 0) then
            tmp = nodes%D(:,n)
            call python_set_active_node_values(1,"D",tmp)
          else
            tmp(1:3) = 0.0d0
            call python_set_active_node_values(1,"D",tmp)
          endif
          if(n <= size(nodes%DR,2) .and. n > 0) then
            tmp = nodes%DR(:,n)
            call python_set_active_node_values(2,"DR",tmp)
          else
            tmp(1:3) = 0.0d0
            call python_set_active_node_values(2,"DR",tmp)
          endif
          if(n <= size(nodes%V,2) .and. n > 0) then
            tmp = nodes%V(:,n)
            call python_set_active_node_values(1,"V",tmp)
          else
            tmp = 0.0d0
            call python_set_active_node_values(1,"V",tmp)
          endif
          if(n <= size(nodes%VR,2) .and. n > 0) then
            tmp = nodes%VR(:,n)
            call python_set_active_node_values(2,"VR",tmp)
          else
            tmp = 0.0d0
            call python_set_active_node_values(2,"VR",tmp)
          endif
          if(n <= size(nodes%AR,2) .and. n > 0) then
            tmp = nodes%AR(:,n)
            call python_set_active_node_values(2,"AR",tmp)
          else
            tmp = 0.0d0
            call python_set_active_node_values(2,"AR",tmp)
          endif
!$OMP CRITICAL
          call python_call_function(py%functs(funct_id)%name, 1, argin, 1, argout)
!$OMP END CRITICAL
          y = real(argout(1),kind(1.0))
        end subroutine

      !||====================================================================
      !||    python_call_funct_cload_dp      ../engine/source/loads/general/python_call_funct_cload.F90
      !||--- calls      -----------------------------------------------------
      !||    python_set_active_node_values   ../common_source/modules/python_mod.F90
      !||--- uses       -----------------------------------------------------
      !||    nodal_arrays_mod                ../engine/source/engine/node_spliting/nodal_arrays.F90
      !||    python_funct_mod                ../common_source/modules/python_mod.F90
      !||====================================================================
        subroutine python_call_funct_cload_dp(py, funct_id, x, y,n,nodes)
          use python_funct_mod
          use nodal_arrays_mod
          implicit none
#include "my_real.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_),                      intent(in) :: py !< the Fortran structure that holds the python function
          integer,                            intent(in) :: funct_id !< the id of the python function
          double precision,                               intent(in) :: x !< the input value
          double precision,                               intent(out) :: y !< the output value
          integer,                                        intent(in) :: n
          type(nodal_arrays_),                            intent(in) :: nodes
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(1) :: argin, argout
          double precision, dimension(3) :: zeros
          double precision, dimension(3) :: tmp
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          argin(1) = dble(x)
          zeros(1:3) = 0.0d0
          ! the double precision function is called, then the result is converted to single
!      subroutine python_set_active_node_values(name_len, name, val) bind(c, name="cpp_python_update_active_node")
          if(n <= size(nodes%x,2) .and. n > 0) then
            tmp = nodes%X(:,n)
            call python_set_active_node_values(1,"C",tmp)
          else
            call python_set_active_node_values(1,"C",zeros)
          endif
          if(n <= size(nodes%A,2) .and. n > 0) then
            tmp = nodes%A(:,n)
            call python_set_active_node_values(1,"A",tmp)
          else
            call python_set_active_node_values(1,"A",zeros)
          endif
          if(n <= size(nodes%D,2) .and. n > 0) then
            tmp = nodes%D(:,n)
            call python_set_active_node_values(1,"D",tmp)
          else
            call python_set_active_node_values(1,"D",zeros)
          endif
          if(n <= size(nodes%DR,2) .and. n > 0) then
            tmp = nodes%DR(:,n)
            call python_set_active_node_values(2,"DR",tmp)
          else
            call python_set_active_node_values(2,"DR",zeros)
          endif
          if(n <= size(nodes%V,2) .and. n > 0) then
            tmp = nodes%V(:,n)
            call python_set_active_node_values(1,"V",tmp)
          else
            call python_set_active_node_values(1,"V",zeros)
          endif
          if(n <= size(nodes%VR,2) .and. n > 0) then
            tmp = nodes%VR(:,n)
            call python_set_active_node_values(2,"VR",tmp)
          else
            call python_set_active_node_values(2,"VR",zeros)
          endif
          if(n <= size(nodes%AR,2) .and. n > 0) then
            tmp = nodes%AR(:,n)
            call python_set_active_node_values(2,"AR",tmp)
          else
            call python_set_active_node_values(2,"AR",zeros)
          endif
!$OMP CRITICAL
          call python_call_function(py%functs(funct_id)%name, 1, argin, 1, argout)
!$OMP END CRITICAL
          y = argout(1)
        end subroutine
        end module

