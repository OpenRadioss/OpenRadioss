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
Chd|  XYZNOR                        source/output/anim/generate/xyznor.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_WRT_XYZNOR               source/mpi/anim/spmd_wrt_xyznor.F
Chd|        WRITE_S_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE XYZNOR(XNORM,NODGLOB,WEIGHT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODGLOB(*),WEIGHT(NUMNOD)
C     REAL ou REAL*8
      my_real
     .   XNORM(3,*) 
C     REAL ou REAL*8
      my_real
     .   S3000,S
      INTEGER I,INORM(3),K,P,IXNN
C-----------------------------------------------
      S3000 = THREE1000
      IXNN = S3000
      IF(FMT_ANI.EQ.4)IXNN=0
C

      IF(NSPMD == 1) THEN
        DO I=1,NUMNOD
         S = SQRT(XNORM(1,I)**2 + XNORM(2,I)**2 + XNORM(3,I)**2)
         IF(S.NE.ZERO)THEN
           S = S3000 / S
           INORM(1) = XNORM(1,I) * S
           INORM(2) = XNORM(2,I) * S 
           INORM(3) = XNORM(3,I) * S
         ELSE
           INORM(1) = 0
           INORM(2) = 0
           INORM(3) = IXNN
         END IF
           CALL WRITE_S_C(INORM,3)
        END DO

      ELSE
         IF(ISPMD.EQ.0) THEN
           CALL SPMD_WRT_XYZNOR(XNORM,WEIGHT,NODGLOB,NUMNODG)
         ELSE
           CALL SPMD_WRT_XYZNOR(XNORM,WEIGHT,NODGLOB,1)
         ENDIF
      END IF

      IF(NSPMD == 1) THEN
        DO I=1,64*NUMELIG3D
           INORM(1) = 0
           INORM(2) = 0
           INORM(3) = IXNN
           CALL WRITE_S_C(INORM,3)
        END DO

      ENDIF
C-----------------------------------------------
      RETURN
      END
