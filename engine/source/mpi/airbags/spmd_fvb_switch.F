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
!||    spmd_fvb_switch   ../engine/source/mpi/airbags/spmd_fvb_switch.F
!||--- called by ------------------------------------------------------
!||    resol             ../engine/source/engine/resol.F
!||--- calls      -----------------------------------------------------
!||--- uses       -----------------------------------------------------
!||    elbufdef_mod      ../common_source/modules/mat_elem/elbufdef_mod.F90
!||    fvbag_mod         ../engine/share/modules/fvbag_mod.F
!||    spmd_mod          ../engine/source/mpi/spmd_mod.F90
!||====================================================================
      SUBROUTINE SPMD_FVB_SWITCH(MONVOL)
C  Broadcast the information that a type 8 fvmbag has switched to type 7 UP monvol
C  To avoid deadlock, all processor must know which FVMBAGs have switched
C  to UP
C-----------------------------------------------
C   M o d u l e s 
C-----------------------------------------------
      USE FVBAG_MOD
      USE ELBUFDEF_MOD
      USE SPMD_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   M e s s a g e   P a s s i n g
C-----------------------------------------------
#include "spmd.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MONVOL(*)                                                
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER :: K1,N,ITYP(NVOLU),ITYPR(NVOLU),IERR
C-----------------------------------------------
#ifdef MPI

      K1=1
      DO N=1,NVOLU
         ITYP(N)=MONVOL(K1-1+2)
         K1=K1+NIMV
      ENDDO

      CALL MPI_ALLREDUCE(ITYP,ITYPR,NVOLU,MPI_INTEGER,MPI_MIN,SPMD_COMM_WORLD,IERR)
      K1=1

      DO N=1,NVOLU
         IF(MONVOL(K1-1+2) == 8 .AND. ITYPR(N) == 7) THEN 
           MONVOL(K1-1+2) = 7 
           MONVOL(K1-1+15) = -1 
         ENDIF
         K1=K1+NIMV
      ENDDO
#endif 
      RETURN
      END      

