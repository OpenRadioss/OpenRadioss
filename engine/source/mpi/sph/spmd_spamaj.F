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
Chd|====================================================================
Chd|  SPMD_SPAMAJ                   source/mpi/sph/spmd_spamaj.F  
Chd|-- called by -----------
Chd|        SPHPREP                       source/elements/sph/sphprep.F 
Chd|-- calls ---------------
Chd|        SPHBOX                        share/modules/sphbox.F        
Chd|====================================================================
      SUBROUTINE SPMD_SPAMAJ(IREDUCE, SPAGLOB)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE SPHBOX
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IREDUCE
       my_real
     .        SPAGLOB
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER P, I, MSGTYP, LOC_PROC, IERROR,
     .        STATUS(MPI_STATUS_SIZE),MSGOFF
      INTEGER :: type_reduc,myop,I_LEN

       my_real, DIMENSION(2) :: SBUF,RBUF
       DATA MSGOFF/2007/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
       LOC_PROC = ISPMD+1

       I_LEN = 2
!      -----------------------
       SBUF(1) = IREDUCE
       SBUF(2) = -SPAGLOB

!      -----------------------
!      allreduce with MIN and MAX operation
       call MPI_AllReduce(SBUF, RBUF, 2, REAL , MPI_MAX,
     &                    MPI_COMM_WORLD, ierror)
!      -----------------------

       IREDUCE = nint(RBUF(1))
       SPAGLOB = -RBUF(2)

#endif
      RETURN
      END
