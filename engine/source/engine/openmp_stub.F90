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
#ifndef _OPENMP
        ! Provide a stub for omp_get_wtime, returning a simple system time
        real(8) function omp_get_wtime()
            implicit none
            call cpu_time(omp_get_wtime)
        end function omp_get_wtime
    
        ! Provide a stub for omp_get_max_threads, returning 1 (single-threaded)
        integer function omp_get_max_threads()
            implicit none
            omp_get_max_threads = 1
        end function omp_get_max_threads
    
        ! Provide a stub for omp_get_num_threads, returning 1 (single-threaded)
        integer function omp_get_num_threads()
            implicit none
            omp_get_num_threads = 1
        end function omp_get_num_threads
    
        ! Provide a stub for omp_get_thread_num, returning 0 (only one thread)
        integer function omp_get_thread_num()
            implicit none
            omp_get_thread_num = 0
        end function omp_get_thread_num
    
        ! Provide a stub for omp_set_num_threads, does nothing
        subroutine omp_set_num_threads(num_threads)
            implicit none
            integer, intent(inout) :: num_threads
            num_threads = 1
        end subroutine omp_set_num_threads
    
#endif
    