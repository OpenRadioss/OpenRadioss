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
!||    drbycnt           ../engine/source/output/anim/generate/drbycnt.F
!||--- called by ------------------------------------------------------
!||    genani            ../engine/source/output/anim/generate/genani.F
!||    genh3d            ../engine/source/output/h3d/h3d_results/genh3d.F
!||--- calls      -----------------------------------------------------
!||    spmd_glob_isum9   ../engine/source/mpi/interfaces/spmd_th.F
!||    spmd_ibcast       ../engine/source/mpi/generic/spmd_ibcast.F
!||====================================================================
      SUBROUTINE DRBYCNT(NERBY,NPBY,FR_RBY2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "task_c.inc"
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NERBY,NERWL,NPBY(NNPBY,*),FR_RBY2(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,LOC_PROC
C-----------------------------------------------
      LOC_PROC = ISPMD+1
      IF (NSPMD == 1) THEN
        DO I=1,NRBODY
          NERBY = NERBY + NPBY(2,I)
        ENDDO
      ELSE
        DO I=1,NRBODY
         IF (ABS(FR_RBY2(3,I))==LOC_PROC)
     .    NERBY = NERBY + FR_RBY2(2,I)
        ENDDO
        CALL SPMD_GLOB_ISUM9(NERBY,1)
        CALL SPMD_IBCAST(NERBY,NERBY,1,1,0,2)
      ENDIF

      RETURN
      END
