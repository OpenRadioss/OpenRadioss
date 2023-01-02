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
C            SPMD_AGETMSR                    /spe/mach_spmd.F
Chd|====================================================================
Chd|  DXWALP                        source/output/anim/generate/dxwalp.F
Chd|-- called by -----------
Chd|        DXYZSECT                      source/output/anim/generate/dxyzsect.F
Chd|-- calls ---------------
Chd|        SPMD_AGETMSR                  source/mpi/anim/spmd_agetmsr.F
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE DXWALP(X ,RWL ,MSR ,FR_WALL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "scr05_c.inc"
#include      "task_c.inc"
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MSR,FR_WALL(NSPMD+2)
C     REAL
      my_real
     .   X(3,*), RWL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   XWL, YWL, ZWL,XN,YN,ZN,D,DX,DY,DZ,XL1,YL1,ZL1,XL2,YL2,ZL2
      REAL R4
C-----------------------------------------------
C
       XN =RWL(1)
       YN =RWL(2)
       ZN =RWL(3)
      IF (NSPMD == 1) THEN
        IF(MSR==0)THEN
         XWL=RWL(4)
         YWL=RWL(5)
         ZWL=RWL(6)
        ELSE
         XWL=X(1,MSR)
         YWL=X(2,MSR)
         ZWL=X(3,MSR)
        ENDIF
      ELSE
         CALL SPMD_AGETMSR(FR_WALL,X,MSR,XWL,YWL,ZWL,RWL)
      END IF 
      IF (ISPMD==0) THEN
C
      XL1=RWL(7)
      YL1=RWL(8)
      ZL1=RWL(9)
      XL2=RWL(10)
      YL2=RWL(11)
      ZL2=RWL(12)
C
      R4 = XWL
      CALL WRITE_R_C(R4,1)
      R4 = YWL
      CALL WRITE_R_C(R4,1)
      R4 = ZWL
      CALL WRITE_R_C(R4,1)
      R4 = XWL + XL1
      CALL WRITE_R_C(R4,1)
      R4 = YWL + YL1
      CALL WRITE_R_C(R4,1)
      R4 = ZWL + ZL1
      CALL WRITE_R_C(R4,1)
      R4 = XWL + XL1 + XL2
      CALL WRITE_R_C(R4,1)
      R4 = YWL + YL1 + YL2
      CALL WRITE_R_C(R4,1)
      R4 = ZWL + ZL1 + ZL2
      CALL WRITE_R_C(R4,1)
      R4 = XWL + XL2
      CALL WRITE_R_C(R4,1)
      R4 = YWL + YL2
      CALL WRITE_R_C(R4,1)
      R4 = ZWL + ZL2
      CALL WRITE_R_C(R4,1)
      ENDIF
C
      RETURN
      END
