Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
Copyright>
Copyright>        This program is free software: you can redistribute it and/or modify
Copyright>        it under the terms of the GNU Affero General Public License as published by
Copyright>        the Free Software Foundation, either version 3 of the License, or
Copyright>        (at your option) any later version.
Copyright>
Copyright>        This program is distributed in the hope that it will be useful,
Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Copyright>        GNU Affero General Public License for more details.
Copyright>
Copyright>        You should have received a copy of the GNU Affero General Public License
Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
Copyright>
Copyright>
Copyright>        Commercial Alternative: Altair Radioss Software
Copyright>
Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss
Copyright>        software under a commercial license.  Contact Altair to discuss further if the
Copyright>        commercial version may interest you: https://www.altair.com/radioss/.
        !   -----------------------------------
        subroutine get_env_variable_zombi(zombi_activation)
#include "implicit_f.inc"
        character(len=100) env
        logical, intent(inout) :: zombi_activation

        env(1:len(env)) = ''
        call getenv ( 'ENABLE_PID_PRINT', env ) 
        zombi_activation = .false.           
        if ( env(1:2)=='ON' ) then
            zombi_activation = .true.
        endif

        return
        end subroutine get_env_variable_zombi
        !   -----------------------------------
        subroutine get_process_pid_zombi(current_process_pid)
#include "implicit_f.inc"
        integer, intent(inout) :: current_process_pid
        integer :: GETPID

        current_process_pid = GETPID()

        return
        end subroutine get_process_pid_zombi
        !   -----------------------------------
        subroutine write_process_pid_zombi( current_process_pid,
     .              radflex_process_pid ,nspmd,list_process)
#include "implicit_f.inc"
        integer, intent(in) :: radflex_process_pid
        integer, intent(in) :: current_process_pid
        integer, intent(in) :: nspmd
        integer, dimension(nspmd+1), intent(in) :: list_process        
        integer :: i

        open(file="running_pids",unit=666,access="append",
     .       status="unknown",action="readwrite")
        do i=1,nspmd+1
            if(list_process(i)/=0) write(666,*) list_process(i) 
        enddo
        close(666)
        return
        end subroutine write_process_pid_zombi
        !   -----------------------------------
        subroutine init_list_process_pid( radflex_process_pid,
     .               current_process_pid,nspmd,list_process )
#include "implicit_f.inc"

#ifdef MPI
#include "mpif.h"
#endif
        integer, intent(in) :: radflex_process_pid,nspmd
        integer, dimension(nspmd+1), intent(inout) :: list_process
        integer, intent(in) :: current_process_pid
        
        integer :: i
        integer :: ierror
        
        list_process(1:nspmd+1 ) = 0
#ifdef MPI
        call MPI_GATHER(current_process_pid,1,MPI_INTEGER,
     .      list_process,1,MPI_INTEGER,0,MPI_COMM_WORLD,ierror)
        list_process(nspmd+1) = radflex_process_pid
#endif
        if(nspmd==1) then
            list_process(1) = current_process_pid
            list_process(2) = radflex_process_pid
        endif

        return
        end subroutine init_list_process_pid
        !   -----------------------------------
        subroutine walking_dead_mode(radflex_process_pid,ispmd,nspmd)
#include "implicit_f.inc"

#ifdef MPI
#include "mpif.h"
#endif
        integer, intent(in) :: radflex_process_pid,nspmd,ispmd
        logical :: zombi_activation
        integer :: current_process_pid
        integer, dimension(nspmd+1) :: list_process
        

        call get_env_variable_zombi(zombi_activation)

        if(zombi_activation) then

            call get_process_pid_zombi(current_process_pid)
            call init_list_process_pid( radflex_process_pid,
     .            current_process_pid,nspmd,list_process )
            if(ispmd==0) call write_process_pid_zombi( 
     .    current_process_pid,radflex_process_pid,nspmd,list_process )
        endif
        
        return
        end subroutine walking_dead_mode
        !   -----------------------------------
