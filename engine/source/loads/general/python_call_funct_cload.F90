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
!||    fixfingeo                     ../engine/source/constraints/general/impvel/fixfingeo.F
!||    fixvel                        ../engine/source/constraints/general/impvel/fixvel.F
!||    force                         ../engine/source/loads/general/force.F90
!||    forcefingeo                   ../engine/source/loads/general/forcefingeo.F
!||    lag_fxv                       ../engine/source/tools/lagmul/lag_fxv.F
!||    lag_fxvp                      ../engine/source/tools/lagmul/lag_fxv.F
!||    resol                         ../engine/source/engine/resol.F
!||====================================================================
      module python_call_funct_cload_mod

        implicit none

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
!||    nodal_arrays_mod                ../common_source/modules/nodal_arrays.F90
!||    python_funct_mod                ../common_source/modules/python_mod.F90
!||====================================================================
        subroutine python_call_funct_cload_sp(py, funct_id, x, y,n,nodes)
          use python_funct_mod
          use nodal_arrays_mod
          implicit none
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
!$OMP CRITICAL
          argin(1) = dble(x)
          ! the double precision function is called, then the result is converted to single
!      subroutine python_set_active_node_values(name_len, name, val) bind(c, name="cpp_python_update_active_node")
          if(n <= size(nodes%x,2) .and. n > 0) then
            tmp = nodes%X(:,n)
            call python_set_active_node_values(1,"C",tmp)
            call python_set_active_node_ids(n,nodes%itab(n))
          else
            tmp(1:3) = 0.0d0
            call python_set_active_node_values(1,"C",tmp)
          end if
          if(n <= size(nodes%A,2) .and. n > 0) then
            tmp = nodes%A(:,n)
            call python_set_active_node_values(1,"A",tmp)
          else
            tmp(1:3) = 0.0d0
            call python_set_active_node_values(1,"A",tmp)
          end if
          if(n <= size(nodes%D,2) .and. n > 0) then
            tmp = nodes%D(:,n)
            call python_set_active_node_values(1,"D",tmp)
          else
            tmp(1:3) = 0.0d0
            call python_set_active_node_values(1,"D",tmp)
          end if
          if(n <= size(nodes%DR,2) .and. n > 0) then
            tmp = nodes%DR(:,n)
            call python_set_active_node_values(2,"DR",tmp)
          else
            tmp(1:3) = 0.0d0
            call python_set_active_node_values(2,"DR",tmp)
          end if
          if(n <= size(nodes%V,2) .and. n > 0) then
            tmp = nodes%V(:,n)
            call python_set_active_node_values(1,"V",tmp)
          else
            tmp = 0.0d0
            call python_set_active_node_values(1,"V",tmp)
          end if
          if(n <= size(nodes%VR,2) .and. n > 0) then
            tmp = nodes%VR(:,n)
            call python_set_active_node_values(2,"VR",tmp)
          else
            tmp = 0.0d0
            call python_set_active_node_values(2,"VR",tmp)
          end if
          if(n <= size(nodes%AR,2) .and. n > 0) then
            tmp = nodes%AR(:,n)
            call python_set_active_node_values(2,"AR",tmp)
          else
            tmp = 0.0d0
            call python_set_active_node_values(2,"AR",tmp)
          end if
          call python_call_function(py%functs(funct_id)%name, 1, argin, 1, argout)
          y = real(argout(1),kind(1.0))
!$OMP END CRITICAL
        end subroutine python_call_funct_cload_sp

!||====================================================================
!||    python_call_funct_cload_dp      ../engine/source/loads/general/python_call_funct_cload.F90
!||--- calls      -----------------------------------------------------
!||    python_set_active_node_values   ../common_source/modules/python_mod.F90
!||--- uses       -----------------------------------------------------
!||    nodal_arrays_mod                ../common_source/modules/nodal_arrays.F90
!||    python_funct_mod                ../common_source/modules/python_mod.F90
!||====================================================================
        subroutine python_call_funct_cload_dp(py, funct_id, x, y,n,nodes)
          use python_funct_mod
          use nodal_arrays_mod
          implicit none
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
          end if
          if(n <= size(nodes%A,2) .and. n > 0) then
            tmp = nodes%A(:,n)
            call python_set_active_node_values(1,"A",tmp)
          else
            call python_set_active_node_values(1,"A",zeros)
          end if
          if(n <= size(nodes%D,2) .and. n > 0) then
            tmp = nodes%D(:,n)
            call python_set_active_node_values(1,"D",tmp)
          else
            call python_set_active_node_values(1,"D",zeros)
          end if
          if(n <= size(nodes%DR,2) .and. n > 0) then
            tmp = nodes%DR(:,n)
            call python_set_active_node_values(2,"DR",tmp)
          else
            call python_set_active_node_values(2,"DR",zeros)
          end if
          if(n <= size(nodes%V,2) .and. n > 0) then
            tmp = nodes%V(:,n)
            call python_set_active_node_values(1,"V",tmp)
          else
            call python_set_active_node_values(1,"V",zeros)
          end if
          if(n <= size(nodes%VR,2) .and. n > 0) then
            tmp = nodes%VR(:,n)
            call python_set_active_node_values(2,"VR",tmp)
          else
            call python_set_active_node_values(2,"VR",zeros)
          end if
          if(n <= size(nodes%AR,2) .and. n > 0) then
            tmp = nodes%AR(:,n)
            call python_set_active_node_values(2,"AR",tmp)
          else
            call python_set_active_node_values(2,"AR",zeros)
          end if
!$OMP CRITICAL
          call python_call_function(py%functs(funct_id)%name, 1, argin, 1, argout)
!$OMP END CRITICAL
          y = argout(1)
        end subroutine python_call_funct_cload_dp

!||====================================================================
!||    python_dummy_active_node        ../engine/source/loads/general/python_call_funct_cload.F90
!||--- called by ------------------------------------------------------
!||    resol                           ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||    python_set_active_node_values   ../common_source/modules/python_mod.F90
!||--- uses       -----------------------------------------------------
!||    python_funct_mod                ../common_source/modules/python_mod.F90
!||====================================================================
        subroutine python_dummy_active_node(py)
          use python_funct_mod
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                     Arguments
! ----------------------------------------------------------------------------------------------------------------------
          type(python_),                      intent(in) :: py !< the Fortran structure that holds the python function
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          double precision, dimension(3) :: zeros
! ----------------------------------------------------------------------------------------------------------------------
!                                                      Body
! ----------------------------------------------------------------------------------------------------------------------
          if(py%nb_functs < 1) return
          zeros(1:3) = 0.0d0
          call python_set_active_node_values(1,"C",zeros)
          call python_set_active_node_values(1,"A",zeros)
          call python_set_active_node_values(1,"D",zeros)
          call python_set_active_node_values(2,"DR",zeros)
          call python_set_active_node_values(1,"V",zeros)
          call python_set_active_node_values(2,"VR",zeros)
          call python_set_active_node_values(2,"AR",zeros)

        end subroutine python_dummy_active_node
      end module python_call_funct_cload_mod

