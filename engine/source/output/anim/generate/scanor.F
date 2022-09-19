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
Chd|  SCANOR                        source/output/anim/generate/scanor.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|        GENH3D                        source/output/h3d/h3d_results/genh3d.F
Chd|-- calls ---------------
Chd|        SPMD_MIN_MAX                  source/mpi/anim/spmd_min_max.F
Chd|====================================================================
      SUBROUTINE SCANOR(X,D,CDG,XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,SCALE,
     .                  WEIGHT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "task_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER WEIGHT(*)
C     REAL
      my_real
     .   X(3,*),D(3,*),CDG(*),XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX, SCALE
C     REAL
      my_real
     .   DX,DY,DZ
      INTEGER  N
C-----------------------------------------------
      XMIN = EP30
      YMIN = EP30
      ZMIN = EP30
      XMAX = -EP30
      YMAX = -EP30
      ZMAX = -EP30
C
c      IF (IMACH/=3) THEN

c       DO N=1,NUMNOD
c          XMIN = MIN(XMIN,(X(1,N)-D(1,N)))
c          YMIN = MIN(YMIN,(X(2,N)-D(2,N)))
c          ZMIN = MIN(ZMIN,(X(3,N)-D(3,N)))
c          XMAX = MAX(XMAX,(X(1,N)-D(1,N)))
c          YMAX = MAX(YMAX,(X(2,N)-D(2,N)))
c          ZMAX = MAX(ZMAX,(X(3,N)-D(3,N)))
c       END DO

c      ELSE

       DO N=1,NUMNOD
         IF (WEIGHT(N)==1) THEN
          XMIN = MIN(XMIN,(X(1,N)-D(1,N)))
          YMIN = MIN(YMIN,(X(2,N)-D(2,N)))
          ZMIN = MIN(ZMIN,(X(3,N)-D(3,N)))
          XMAX = MAX(XMAX,(X(1,N)-D(1,N)))
          YMAX = MAX(YMAX,(X(2,N)-D(2,N)))
          ZMAX = MAX(ZMAX,(X(3,N)-D(3,N)))
         END IF
       END DO
       IF(NSPMD > 1)
     .   CALL SPMD_MIN_MAX(XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX)
c      END IF

      CDG(1) = HALF * (XMAX + XMIN)
      CDG(2) = HALF * (YMAX + YMIN)
      CDG(3) = HALF * (ZMAX + ZMIN)
C
C  
      DX = XMAX - XMIN
      DY = YMAX - YMIN
      DZ = ZMAX - ZMIN
C
      SCALE = MAX(DX,DY,DZ)

      SCALE = ONE/SCALE

      RETURN
      END
