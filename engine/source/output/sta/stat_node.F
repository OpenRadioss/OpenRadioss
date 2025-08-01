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
!||    stat_node          ../engine/source/output/sta/stat_node.F
!||--- called by ------------------------------------------------------
!||    genstat            ../engine/source/output/sta/genstat.F
!||--- calls      -----------------------------------------------------
!||    spmd_dstat_vgath   ../engine/source/mpi/output/spmd_stat.F
!||--- uses       -----------------------------------------------------
!||    my_alloc_mod       ../common_source/tools/memory/my_alloc.F90
!||====================================================================
      SUBROUTINE STAT_NODE(X,NUMNOD,ITAB,ITABG,LENG,
     .                    NODGLOB,WEIGHT,NODTAG)
C-----------------------------------------------
C   M o d u l e s
C-----------------------------------------------
      USE MY_ALLOC_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER ITAB(*), NUMNOD,WEIGHT(*),ITABG(*),LENG,
     .        NODGLOB(*),NODTAG(*)
      my_real
     .   X(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
      INTEGER,DIMENSION(:),ALLOCATABLE :: NODTAGLOB
      my_real,DIMENSION(:,:),ALLOCATABLE :: VGLOB
C-----------------------------------------------
      CALL MY_ALLOC(NODTAGLOB,LENG)
      CALL MY_ALLOC(VGLOB,3,LENG)
C-----------------------------------------------

      IF (NSPMD > 1) THEN
        CALL SPMD_DSTAT_VGATH(X,NODGLOB,WEIGHT,VGLOB,NODTAG,
     .                        NODTAGLOB)
        IF (ISPMD/=0) RETURN
      ENDIF

      WRITE(IUGEO,'(A)')'/NODE'
      WRITE(IUGEO,'(2A)')'#    NODID               XCOOR',
     .         '               YCOOR               ZCOOR'

      IF (NSPMD == 1) THEN
        DO I=1,NUMNOD
           IF(NODTAG(I) /= 0) THEN
            WRITE(IUGEO,'(I10,1P3E20.13)')
     .       ITAB(I),X(1,I),X(2,I),X(3,I)
           END IF
        END DO
      ELSE
        DO I=1,NUMNODG
          IF(NODTAGLOB(I) /= 0)
     .      WRITE(IUGEO,'(I10,1P3E20.13)')
     .       ITABG(I),VGLOB(1,I),VGLOB(2,I),VGLOB(3,I)
        END DO
      ENDIF
C-----------------------------------------------
      DEALLOCATE(NODTAGLOB)
      DEALLOCATE(VGLOB)
C-----------------------------------------------
      RETURN
      END
