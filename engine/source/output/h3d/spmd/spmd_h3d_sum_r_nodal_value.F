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
Chd|  SPMD_H3D_SUM_R_NODAL_VALUE    source/output/h3d/spmd/spmd_h3d_sum_r_nodal_value.F
Chd|-- called by -----------
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE SPMD_H3D_SUM_R_NODAL_VALUE(NODGLOB,V,LEN,VP0,LENP0)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "r4r8_p.inc"
#ifdef MPI
#include      "mpif.h"
#endif
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  , INTENT(IN) :: LEN,LENP0, NODGLOB(LEN)
      my_real  , INTENT(INOUT) :: 
     .   V(LEN),VP0(LENP0)
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER J,K 
      REAL
     .       , DIMENSION(:), ALLOCATABLE ::  V_TMP,VP0_TMP
#ifdef MPI
      INTEGER STATUS(MPI_STATUS_SIZE),IERROR,I

        ALLOCATE(V_TMP(LENP0))
        ALLOCATE(VP0_TMP(LENP0))

        DO I=1,LENP0
           VP0_TMP(I)=ZERO
           V_TMP(I) = ZERO
        ENDDO
        DO K=1,LEN
           I=NODGLOB(K)
           VP0_TMP(I)=V(K)
        ENDDO

       IF (LENP0 > 0) THEN
        CALL MPI_REDUCE(VP0_TMP,V_TMP,LENP0,
     .                  MPI_REAL,MPI_SUM,IT_SPMD(1),
     .                  MPI_COMM_WORLD,IERROR)
       ENDIF
       IF (ISPMD==0) THEN
         DO I=1,LENP0
          VP0(I) = V_TMP(I)
         END DO
       ENDIF

       DEALLOCATE(V_TMP,VP0_TMP)
#endif
       RETURN
       END


