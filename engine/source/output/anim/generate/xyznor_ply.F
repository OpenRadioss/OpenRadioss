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
Chd|  XYZNOR_PLY                    source/output/anim/generate/xyznor_ply.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_ANIM_PLY_XYZNOR          source/mpi/anim/spmd_anim_ply_xyznor.F
Chd|        WRITE_S_C                     ../common_source/tools/input_output/write_routtines.c
Chd|        PLYXFEM_MOD                   share/modules/plyxfem_mod.F   
Chd|====================================================================
      SUBROUTINE XYZNOR_PLY(IPLY,XNORM,NODGLOB,WEIGHT,EMPSIZPL)
C-----------------------------------------------
      USE PLYXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "scr14_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODGLOB(*),WEIGHT(NUMNOD),EMPSIZPL
      INTEGER  IPLY
C     REAL
      my_real
     .   XNORM(3,*) 
C     REAL
      my_real
     .   S3000,S
      INTEGER I,INORM(3),K,P,IXNN,N,ARSZ,EMPL
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: INORMAR
C-----------------------------------------------
      S3000 = THREE1000
      IXNN = S3000
      IF(FMT_ANI==4)IXNN=0
C
      IF(NSPMD == 1 ) THEN
        ARSZ = PLYNOD(IPLY)%PLYNUMNODS
        ALLOCATE(INORMAR(3,ARSZ))

        DO N=1,PLYNOD(IPLY)%PLYNUMNODS
          I = PLYNOD(IPLY)%NODES(N)
          EMPL = PLYNOD(IPLY)%PLYNODID(N)-EMPSIZPL
          S = SQRT(XNORM(1,I)**2 + XNORM(2,I)**2 + XNORM(3,I)**2)
          IF(S/=ZERO)THEN
           S = S3000 / S
           INORMAR(1,EMPL) = XNORM(1,I) * S
           INORMAR(2,EMPL) = XNORM(2,I) * S
           INORMAR(3,EMPL) = XNORM(3,I) * S
          ELSE
           INORMAR(1,EMPL) = 0
           INORMAR(2,EMPL) = 0
           INORMAR(3,EMPL) = IXNN
          END IF
        END DO
        CALL WRITE_S_C(INORMAR,3*ARSZ)
        DEALLOCATE(INORMAR)
        EMPSIZPL = EMPSIZPL + PLYNOD(IPLY)%PLYNUMNODS
      ELSE
        CALL SPMD_ANIM_PLY_XYZNOR( NODGLOB,IPLY,XNORM,EMPSIZPL)
      END IF
C-----------------------------------------------
      RETURN
      END
