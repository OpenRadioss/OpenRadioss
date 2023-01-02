//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2023 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.
#include "hardware.inc"
#include <stdio.h>
#include <stdlib.h>

#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32

#define _FCALL


#elif 1

#include <unistd.h>
#define _FCALL

#endif



#if CPP_mach == CPP_w95 || CPP_mach == CPP_win64_spmd || CPP_mach == CPP_p4win64_spmd || CPP_mach == CPP_wnt || CPP_mach == CPP_wmr || CPP_mach == CPP_p4win64 || CPP_mach == CPP_p4win32

void my_fork_c(int * pid){
 *pid=-1;   
}
void my_waitpid_c(int * pid, int * istat, int * pidp, int * pidret){
 *pid=-1;   
}



#elif 1

void my_fork_c(int * pid){
*pid = fork();
}
void my_waitpid_c(int * pid, int * istat, int * pidp, int * pidret){
waitpid(*pid,*istat,*pidp);
}


#endif


void my_fork_( int * pid) {
 my_fork_c(pid);
}

void my_fork__( int * pid) {
 my_fork_c(pid);
}

void my_fork( int * pid) {
 my_fork_c(pid);
}

void _FCALL MY_FORK( int * pid) {
 my_fork_c(pid);
}

void my_waitpid_(int * pid, int * istat, int * pidp, int * pidret) {
 my_waitpid_c(pid,istat,pidp,pidret);
}

void my_waitpid__(int * pid, int * istat, int * pidp, int * pidret) {
 my_waitpid_c(pid,istat,pidp,pidret);
}

void my_waitpid(int * pid, int * istat, int * pidp, int * pidret) {
 my_waitpid_c(pid,istat,pidp,pidret);
}

void _FCALL MY_WAITPID(int * pid, int * istat, int * pidp, int * pidret) {
 my_waitpid_c(pid,istat,pidp,pidret);
}

