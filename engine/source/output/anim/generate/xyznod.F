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
Chd|  XYZNOD                        source/output/anim/generate/xyznod.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_WRT_XYZNOD               source/mpi/anim/spmd_wrt_xyznod.F
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE XYZNOD(X,X_TEMP,NODGLOB,WEIGHT)
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
#include      "task_c.inc"
#include      "spmd_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODGLOB(*), WEIGHT(NUMNOD)
      my_real
     .   X(3,*)
      my_real
     .   X_TEMP(3,*) 
      REAL R4
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I

      IF (NSPMD == 1) THEN
        DO I=1,NUMNOD
           R4 = X(1,I)
           CALL WRITE_R_C(R4,1)
           R4 = X(2,I)
           CALL WRITE_R_C(R4,1)
           R4 = X(3,I)
           CALL WRITE_R_C(R4,1)
        END DO
      ELSE
C   le proc 0 a besoin d un tableau de taille numnodg pour reconstruire 
C   le tableau global des X. LEs autres non. Cela evite une allocation dynamique

         IF (ISPMD==0) THEN
           CALL SPMD_WRT_XYZNOD(X,WEIGHT,NODGLOB,NUMNODG)
         ELSE
           CALL SPMD_WRT_XYZNOD(X,WEIGHT,NODGLOB,1)
         ENDIF
      ENDIF
      IF (NSPMD == 1 .AND. NUMELIG3D /= 0)THEN
        DO I=1,64*NUMELIG3D
           R4 = X_TEMP(1,I)
           CALL WRITE_R_C(R4,1)
           R4 = X_TEMP(2,I)
           CALL WRITE_R_C(R4,1)
           R4 = X_TEMP(3,I)
           CALL WRITE_R_C(R4,1)
        END DO
      ENDIF

C-----------------------------------------------
      RETURN
      END
