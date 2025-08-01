Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2025 Altair Engineering Inc.
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

!||====================================================================
!||    rad_spmd_recv         ../engine/source/mpi/generic/rad_spmd_recv.F
!||--- called by ------------------------------------------------------
!||    aniskewf              ../engine/source/output/anim/generate/aniskewf.F
!||    printime              ../engine/source/system/timer.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
      SUBROUTINE RAD_SPMD_RECV(A,SIZ,ISPMD,IT_SPMD,MSGTAG,INTSIZE)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
      use SPMD_COMM_WORLD_MOD, only: SPMD_COMM_WORLD
       implicit none
C#include      "implicit_f.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER SIZ,ISPMD,IT_SPMD(*),MSGTAG,INTSIZE
      INTEGER, INTENT(INOUT) :: A(*)
C-----------------------------------------------
C   L o c a l  V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER INFO,BUFSIZMAX,ADD,LEN,SIZ_I,
     .        ATID,ATAG,ALEN
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR
C segment de memoire partagee doit etre superieur a 10 MB
      DATA BUFSIZMAX/10000000/
C-----------------------------------------------
C   S o u r c e  L i n e s
C-----------------------------------------------
      ADD = 1
      SIZ_I = SIZ
 1    LEN = MIN(SIZ_I,BUFSIZMAX)

      CALL MPI_RECV(A(ADD),LEN,MPI_BYTE,IT_SPMD(ISPMD+1),MSGTAG,
     .              SPMD_COMM_WORLD,STATUS,ierror)
      CALL MPI_SEND(A,0,MPI_BYTE,IT_SPMD(ISPMD+1),MSGTAG,
     .              SPMD_COMM_WORLD,ierror)

      ADD = ADD + LEN/INTSIZE
      SIZ_I = SIZ_I - LEN
      IF (SIZ_I>0) GO TO 1
#endif
      RETURN
      END
