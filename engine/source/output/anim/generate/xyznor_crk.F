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
Chd|  XYZNOR_CRK                    source/output/anim/generate/xyznor_crk.F
Chd|-- called by -----------
Chd|        GENANI                        source/output/anim/generate/genani.F
Chd|-- calls ---------------
Chd|        SPMD_WRT_CRK_XYZNOR           source/mpi/anim/spmd_wrt_crk_xyznor.F
Chd|        WRITE_S_C                     source/output/tools/sortie_c.c
Chd|        CRACKXFEM_MOD                 share/modules/crackxfem_mod.F 
Chd|====================================================================
      SUBROUTINE XYZNOR_CRK(ICRK,XNORM,NFNOD_CRKXFEMG)
C-----------------------------------------------
      USE CRACKXFEM_MOD
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
#include      "com04_c.inc"
#include      "com_xfem1.inc"
#include      "scr05_c.inc"
#include      "scr14_c.inc"
#include      "task_c.inc"
#include      "spmd_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  ICRK
C     REAL ou REAL*8
      my_real
     .   XNORM(3,*) 
C-----------------------------------------------
C   L O C A L   V A R I A B L E S
C-----------------------------------------------
      INTEGER I,INORM(3),K,P,IXNN,ND,ARSZ,EMPL,NFNOD_CRKXFEMG
      INTEGER, DIMENSION(:,:), ALLOCATABLE :: INORMAR
C     REAL ou REAL*8
      my_real
     .   S3000,S
c=======================================================================
      S3000 = TROIS1000
      IXNN = S3000
      IF (FMT_ANI == 4) IXNN=0
C
      IF (NSPMD == 1) THEN
        ARSZ = CRKNOD(ICRK)%CRKNUMNODS
        ALLOCATE(INORMAR(3,ARSZ))
        DO ND=1,ARSZ
C          EMPL = ND
C          S = SQRT(XNORM(1,ND)**2 + XNORM(2,ND)**2 + XNORM(3,ND)**2)
          S = ZERO
          IF(S /= ZERO)THEN
C           S = S3000 / S
c           INORMAR(1,EMPL) = XNORM(1,I) * S
c           INORMAR(2,EMPL) = XNORM(2,I) * S
c           INORMAR(3,EMPL) = XNORM(3,I) * S
          ELSE
           INORMAR(1,ND) = 0
           INORMAR(2,ND) = 0
           INORMAR(3,ND) = IXNN
          END IF
        END DO
        CALL WRITE_S_C(INORMAR,3*ARSZ)
        DEALLOCATE(INORMAR)
      ELSE
         IF (ISPMD.EQ.0) THEN
           CALL SPMD_WRT_CRK_XYZNOR(ICRK,XNORM,NFNOD_CRKXFEMG)
         ELSE
           CALL SPMD_WRT_CRK_XYZNOR(ICRK,XNORM,1)
         ENDIF
      END IF
C-----------------------------------------------
      RETURN
      END
