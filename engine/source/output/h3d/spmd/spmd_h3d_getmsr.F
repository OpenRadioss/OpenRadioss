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
!||    spmd_h3d_getmsr       ../engine/source/output/h3d/spmd/spmd_h3d_getmsr.F
!||--- called by ------------------------------------------------------
!||    h3d_dxyz_rwall        ../engine/source/output/h3d/h3d_build_fortran/h3d_dxyz_rwall.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    spmd_comm_world_mod   ../engine/source/mpi/spmd_comm_world.F90
!||====================================================================
        SUBROUTINE SPMD_H3D_GETMSR(FR_WALL,X,MSR,XWL,YWL,ZWL,RWL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
        USE SPMD_COMM_WORLD_MOD, ONLY : SPMD_COMM_WORLD
#include      "implicit_f.inc"
#include "spmd.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "task_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
       my_real
     .   X(3,*),XWL,YWL,ZWL,RWL(*)
       INTEGER MSR,FR_WALL(NSPMD+2)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
#ifdef MPI
       INTEGER P
       INTEGER STATUS(MPI_STATUS_SIZE),IERROR,MSGOFF,MSGTYP
       my_real
     .         XYZWL(3) 
       DATA MSGOFF/7032/

       P = FR_WALL(NSPMD+2)
       IF (P==1.OR.P==0) THEN
         IF (ISPMD==0) THEN
           IF (MSR==0) THEN
              XWL = RWL(4) 
              YWL = RWL(5)
              ZWL = RWL(6)
           ELSE
             XWL = X(1,MSR)
             YWL = X(2,MSR)
             ZWL = X(3,MSR)
           END IF
         END IF
       ELSE
         IF (P==ISPMD+1) THEN
           IF (MSR==0) THEN
             XWL = RWL(4) 
             YWL = RWL(5)
             ZWL = RWL(6)
           ELSE
             XYZWL(1)= X(1,MSR)
             XYZWL(2)= X(2,MSR)
             XYZWL(3)= X(3,MSR)
           END IF

           MSGTYP = MSGOFF 
           CALL MPI_SEND(XYZWL,3,REAL,IT_SPMD(1),MSGTYP,
     .     SPMD_COMM_WORLD,ierror)
         END IF
         IF (ISPMD==0) THEN
            MSGTYP = MSGOFF 
            CALL MPI_RECV(XYZWL,3,REAL,IT_SPMD(P),MSGTYP,
     .              SPMD_COMM_WORLD,STATUS,ierror)
            XWL = XYZWL(1)
            YWL = XYZWL(2) 
            ZWL = XYZWL(3) 
         END IF
       END IF
#endif
      RETURN
      END      

