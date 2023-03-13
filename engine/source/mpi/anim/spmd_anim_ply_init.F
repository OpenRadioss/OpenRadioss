Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2023 Altair Engineering Inc.
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
Chd|  SPMD_ANIM_PLY_INIT            source/mpi/anim/spmd_anim_ply_init.F
Chd|-- called by -----------
Chd|        RESOL_INIT                    source/engine/resol_init.F    
Chd|-- calls ---------------
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE SPMD_ANIM_PLY_INIT()
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE PLYXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#ifdef MPI
#include "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "param_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
#ifdef MPI
      INTEGER, DIMENSION(:),ALLOCATABLE :: PLYNODTAG,SENDBUF,RECBUF
      INTEGER MAXSENDSZ,RECSZ,PLYSZ
      INTEGER I,J,P,ND, ITAG, IDCMAXP(NSPMD-1), REQ(NSPMD-1),NODCOUNT
      INTEGER MSGOFF,MSGOFF2,STAT(MPI_STATUS_SIZE,NSPMD-1), IERR
C-----------------------------------------------
      DATA MSGOFF/7054/
C-----------------------------------------------
C
C Allocate Global PLY Structure
C
      IF (ISPMD==0)THEN
        ALLOCATE (PLYIADNOD(NPLYMAX,NSPMD))
        PLYIADNOD = 0
      ELSE
         ALLOCATE (PLYIADNOD(1,1))
      ENDIF
      PLYIADNOD = 0


C Need to rebuild a global PLYNOD Structure on processor 0 to have
C a conversion array from Global Node ID to Internal PLYNOD ID

      DO I=1,NPLYMAX
        IF (ISPMD==0)THEN
C PROC 0, RECEIVE NODES NUM for each ply
           DO P=2,NSPMD
              ITAG=MSGOFF
              CALL MPI_RECV(RECSZ,1, MPI_INTEGER, IT_SPMD(P),
     .                      ITAG, MPI_COMM_WORLD, STAT, IERR)
              PLYIADNOD(I,P) = RECSZ
           ENDDO
        ELSE
           ITAG=MSGOFF
           CALL MPI_SEND(PLYNOD(I)%PLYNUMNODS,1,MPI_INTEGER,IT_SPMD(1),
     .                   ITAG,MPI_COMM_WORLD,IERR)
        ENDIF
      ENDDO

C
#endif
      RETURN
      END
