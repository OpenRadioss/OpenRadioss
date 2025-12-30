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
      module spmd_constants_mod
        implicit none
        integer, parameter, public :: TAG_BARRIER = -1
        integer, parameter, public :: TAG_REDUCE = -5
        integer, parameter, public :: TAG_ALLREDUCE = -6

#ifndef MPI
        integer, parameter, public :: MPI_STATUS_IGNORE = 0
        integer, parameter, public :: MPI_STATUS_SIZE = 1
        integer, parameter, public :: MPI_REQUEST_NULL = 0
        integer, parameter, public :: MPI_COMM_WORLD = 0
#endif




      end module spmd_constants_mod
