Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
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
Chd|  DYNAIN_NODE                   source/output/dynain/dynain_node.F
Chd|-- called by -----------
Chd|        GENDYNAIN                     source/output/dynain/gendynain.F
Chd|-- calls ---------------
Chd|        SPMD_DSTAT_VGATH              source/mpi/output/spmd_stat.F 
Chd|====================================================================
      SUBROUTINE DYNAIN_NODE(X   ,NUMNOD  ,ITAB  ,ITABG  ,LENG  ,
     .                    NODGLOB,WEIGHT  ,NODTAG)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "units_c.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "scr16_c.inc"
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
      INTEGER I , IERR
      INTEGER ,  DIMENSION(:),ALLOCATABLE :: NODTAGLOB
      my_real ,  DIMENSION(:,:),ALLOCATABLE :: VGLOB
C-----------------------------------------------

      IF (NSPMD > 1) THEN
C-----------------------
C    Allocation Tabs
C-----------------------
        ALLOCATE(NODTAGLOB(LENG),STAT=IERR)
        ALLOCATE(VGLOB(3,LENG),STAT=IERR)
        CALL SPMD_DSTAT_VGATH(X,NODGLOB,WEIGHT,VGLOB,NODTAG,
     .                        NODTAGLOB)
        IF (ISPMD/=0) RETURN
      ENDIF

      WRITE(IUDYNAIN,'(A)')'*NODE'
      WRITE(IUDYNAIN,'(2A)')'$    NID               X',
     .         '               Y               Z'

      IF (NSPMD == 1) THEN
        DO I=1,NUMNOD
           IF(NODTAG(I) /= 0) THEN
            WRITE(IUDYNAIN,'(I8,1P3G16.9)')
     .       ITAB(I),X(1,I),X(2,I),X(3,I)
           END IF
        END DO
      ELSE
        DO I=1,NUMNODG
          IF(NODTAGLOB(I) /= 0)
     .      WRITE(IUDYNAIN,'(I8,1P3G16.9)')
     .       ITABG(I),VGLOB(1,I),VGLOB(2,I),VGLOB(3,I)
        END DO
      ENDIF
      IF (NSPMD > 1) DEALLOCATE(VGLOB,NODTAGLOB)
C-----------------------------------------------
      RETURN
      END
