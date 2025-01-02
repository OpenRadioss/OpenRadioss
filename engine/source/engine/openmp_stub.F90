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
#ifndef _OPENMP
        ! Provide a stub for omp_get_wtime, returning a simple system time
        real(8) function omp_get_wtime()
            implicit none
            call cpu_time(omp_get_wtime)
        end function omp_get_wtime
    
        ! Provide a stub for omp_get_max_threads, returning 1 (single-threaded)
      !||====================================================================
      !||    omp_get_max_threads   ../engine/source/engine/openmp_stub.F90
      !||--- called by ------------------------------------------------------
      !||    inipar                ../engine/source/mpi/init/inipar.F
      !||====================================================================
        integer function omp_get_max_threads()
            implicit none
            omp_get_max_threads = 1
        end function omp_get_max_threads
    
        ! Provide a stub for omp_get_num_threads, returning 1 (single-threaded)
      !||====================================================================
      !||    omp_get_num_threads   ../engine/source/engine/openmp_stub.F90
      !||--- called by ------------------------------------------------------
      !||    get_volume_area       ../engine/source/airbag/get_volume_area.F90
      !||    imp_pcgh              ../engine/source/implicit/imp_pcg.F
      !||====================================================================
        integer function omp_get_num_threads()
            implicit none
            omp_get_num_threads = 1
        end function omp_get_num_threads
    
        ! Provide a stub for omp_get_thread_num, returning 0 (only one thread)
      !||====================================================================
      !||    omp_get_thread_num      ../engine/source/engine/openmp_stub.F90
      !||--- called by ------------------------------------------------------
      !||    convec                  ../engine/source/constraints/thermic/convec.F
      !||    forintc                 ../engine/source/elements/forintc.F
      !||    get_u_task              ../engine/source/user_interface/upidmid.F
      !||    get_volume_area         ../engine/source/airbag/get_volume_area.F90
      !||    imp_fsa_inv2hp          ../engine/source/implicit/imp_fsa_inv.F
      !||    imp_fsa_invhp           ../engine/source/implicit/imp_fsa_inv.F
      !||    imp_glob_khp            ../engine/source/implicit/imp_glob_k.F
      !||    imp_smpini              ../engine/source/implicit/imp_solv.F
      !||    inixfem                 ../engine/source/elements/xfem/inixfem.F
      !||    lin_solv                ../engine/source/implicit/lin_solv.F
      !||    lin_solvh0              ../engine/source/implicit/lin_solv.F
      !||    lin_solvh1              ../engine/source/implicit/lin_solv.F
      !||    lin_solvhm              ../engine/source/implicit/lin_solv.F
      !||    lin_solvih2             ../engine/source/implicit/lin_solv.F
      !||    pblast_1                ../engine/source/loads/pblast/pblast_1.F
      !||    r2r_init                ../engine/source/coupling/rad2rad/r2r_init.F
      !||    radiation               ../engine/source/constraints/thermic/radiation.F
      !||    radioss2                ../engine/source/engine/radioss2.F
      !||    resol                   ../engine/source/engine/resol.F
      !||    smp_init                ../engine/source/engine/resol_init.F
      !||    write_cut_cell_buffer   ../engine/source/interfaces/int22/write_cut_cell_buffer.F
      !||====================================================================
        integer function omp_get_thread_num()
            implicit none
            omp_get_thread_num = 0
        end function omp_get_thread_num
    
        ! Provide a stub for omp_set_num_threads, does nothing
      !||====================================================================
      !||    omp_set_num_threads   ../engine/source/engine/openmp_stub.F90
      !||--- called by ------------------------------------------------------
      !||    inipar                ../engine/source/mpi/init/inipar.F
      !||    resol                 ../engine/source/engine/resol.F
      !||====================================================================
        subroutine omp_set_num_threads(num_threads)
            implicit none
            integer, intent(inout) :: num_threads
            num_threads = 1
        end subroutine omp_set_num_threads
    
#endif
    