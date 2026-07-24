!Copyright>        OpenRadioss
!Copyright>        Copyright (C) 2026 Siemens
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
!Copyright>        Commercial Alternative: Simcenter Radioss Software
!Copyright>
!Copyright>        As an alternative to this open-source version, Siemens also offers Simcenter(TM) Radioss(R)
!Copyright>        software under a commercial license.  Contact Siemens to discuss further if the
!Copyright>        commercial version may interest you: 
!Copyright>        https://www.siemens.com/en-us/products/simcenter/mechanical-simulation/radioss/.
      module get_mpi_operator_mod
        use spmd_operator_mod, only: SPMD_MAX, SPMD_MIN, SPMD_SUM, SPMD_PROD
      contains
!! \brief Get the MPI operator for a given SPMD operator
!||====================================================================
!||    get_mpi_operator         ../engine/source/mpi/spmd_mod.F90
!||--- called by ------------------------------------------------------
!||    spmd_allreduce_double    ../engine/source/mpi/spmd_mod.F90
!||    spmd_allreduce_doubles   ../engine/source/mpi/spmd_mod.F90
!||    spmd_allreduce_int       ../engine/source/mpi/spmd_mod.F90
!||    spmd_allreduce_ints      ../engine/source/mpi/spmd_mod.F90
!||    spmd_allreduce_real      ../engine/source/mpi/spmd_mod.F90
!||    spmd_allreduce_reals     ../engine/source/mpi/spmd_mod.F90
!||    spmd_reduce_double       ../engine/source/mpi/spmd_mod.F90
!||    spmd_reduce_doubles      ../engine/source/mpi/spmd_mod.F90
!||    spmd_reduce_int          ../engine/source/mpi/spmd_mod.F90
!||    spmd_reduce_ints         ../engine/source/mpi/spmd_mod.F90
!||    spmd_reduce_real         ../engine/source/mpi/spmd_mod.F90
!||    spmd_reduce_reals        ../engine/source/mpi/spmd_mod.F90
!||====================================================================
        function get_mpi_operator(spmd_op) result(mpi_operator)
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Implicit none
! ----------------------------------------------------------------------------------------------------------------------
          implicit none
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Included files
! ----------------------------------------------------------------------------------------------------------------------
#include "spmd.inc"
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Arguments
! ----------------------------------------------------------------------------------------------------------------------
          integer, intent(in) :: spmd_op
! ----------------------------------------------------------------------------------------------------------------------
!                                                   Local variables
! ----------------------------------------------------------------------------------------------------------------------
          integer :: mpi_operator
#ifdef MPI
          select case(spmd_op)
           case(SPMD_MAX)
            mpi_operator = MPI_MAX
           case(SPMD_MIN)
            mpi_operator = MPI_MIN
           case(SPMD_SUM)
            mpi_operator = MPI_SUM
           case(SPMD_PROD)
            mpi_operator = MPI_PROD
           case default
            mpi_operator = MPI_OP_NULL
          end select
#else
          mpi_operator = 0
#endif
        end function get_mpi_operator
      end module
