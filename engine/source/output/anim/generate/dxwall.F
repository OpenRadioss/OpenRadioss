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
Cgw          SPMD_AGETMSR                    /spe/mach_spmd.F
Chd|====================================================================
Chd|  DXWALL                        source/output/anim/generate/dxwall.F
Chd|-- called by -----------
Chd|        DXYZSECT                      source/output/anim/generate/dxyzsect.F
Chd|-- calls ---------------
Chd|        SPMD_AGETMSR                  source/mpi/anim/spmd_agetmsr.F
Chd|        WRITE_R_C                     source/output/tools/sortie_c.c
Chd|====================================================================
      SUBROUTINE DXWALL(X ,RWL ,MSR ,
     3            XMIN ,YMIN ,ZMIN  ,XMAX ,YMAX  ,
     4            ZMAX,FR_WALL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
#include      "com01_c.inc"
#include      "task_c.inc"
#include      "scr05_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MSR,FR_WALL(NSPMD+2)
C     REAL
      my_real
     .   X(3,*), RWL(*),XMIN ,YMIN ,ZMIN  ,XMAX ,YMAX  , ZMAX
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   XWL, YWL, ZWL,XN,YN,ZN,D,DX,DY,DZ, R, V1, V2, V3, VV1, VV2,
     .   VV3, VV
      REAL R4
C-----------------------------------------------
C
       XN =RWL(1)
       YN =RWL(2)
       ZN =RWL(3)
      IF (NSPMD == 1) THEN
        IF(MSR.EQ.0)THEN
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
C
      END IF 
      IF (ISPMD.EQ.0) THEN
      DX = XMAX - XMIN
      DY = YMAX - YMIN
      DZ = ZMAX - ZMIN
C
      R = ZEP707*MAX(DX,DY,DZ)
      V1 = ZERO
      V2 = ZEP707
      V3 = ZEP707
      VV1 = V2 * ZN - V3 * YN
      VV2 = V3 * XN - V1 * ZN
      VV3 = V1 * YN - V2 * XN
      VV = SQRT(VV1*VV1 + VV2*VV2 + VV3*VV3)
      IF(VV.LE.HALF)THEN
        V1 = ZERO
        V2 = -ZEP707
        V3 = ZEP707        
        VV1 = V2 * ZN - V3 * YN
        VV2 = V3 * XN - V1 * ZN
        VV3 = V1 * YN - V2 * XN
        VV = SQRT(VV1*VV1 + VV2*VV2 + VV3*VV3)
      ENDIF
      VV1 = R*VV1/VV
      VV2 = R*VV2/VV
      VV3 = R*VV3/VV
C
      V1 = VV2 * ZN - VV3 * YN
      V2 = VV3 * XN - VV1 * ZN
      V3 = VV1 * YN - VV2 * XN
C
      R4 = XWL + V1
      CALL WRITE_R_C(R4,1)
      R4 = YWL + V2
      CALL WRITE_R_C(R4,1)
      R4 = ZWL + V3
      CALL WRITE_R_C(R4,1)
      R4 = XWL + VV1
      CALL WRITE_R_C(R4,1)
      R4 = YWL + VV2
      CALL WRITE_R_C(R4,1)
      R4 = ZWL + VV3
      CALL WRITE_R_C(R4,1)
      R4 = XWL - V1
      CALL WRITE_R_C(R4,1)
      R4 = YWL - V2
      CALL WRITE_R_C(R4,1)
      R4 = ZWL - V3
      CALL WRITE_R_C(R4,1)
      R4 = XWL - VV1
      CALL WRITE_R_C(R4,1)
      R4 = YWL - VV2
      CALL WRITE_R_C(R4,1)
      R4 = ZWL - VV3
      CALL WRITE_R_C(R4,1)
      ENDIF
C
      RETURN
      END
