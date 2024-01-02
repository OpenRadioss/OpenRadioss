Copyright>        OpenRadioss
Copyright>        Copyright (C) 1986-2024 Altair Engineering Inc.
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
Chd|  DNWALC                        source/output/anim/generate/dnwalc.F
Chd|-- called by -----------
Chd|        DSECNOR                       source/output/anim/generate/dsecnor.F
Chd|-- calls ---------------
Chd|        WRITE_S_C                     ../common_source/tools/input_output/write_routtines.c
Chd|====================================================================
      SUBROUTINE DNWALC(X ,RWL ,MSR )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MSR
      my_real
     .   X(3,*), RWL(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real
     .   XWL, YWL, ZWL,XN,YN,ZN,D,DX,DY,DZ,XL, V1, V2, V3, VV1,
     .   VV2, VV3, VV,S,C,XX,YY,ZZ,ALPHA,X0,Y0,Z0
      REAL R4
C-----------------------------------------------
C
       XN =RWL(1)
       YN =RWL(2)
       ZN =RWL(3)
      IF(MSR==0)THEN
       XWL=RWL(4)
       YWL=RWL(5)
       ZWL=RWL(6)
      ELSE
       XWL=X(1,MSR)
       YWL=X(2,MSR)
       ZWL=X(3,MSR)
      ENDIF
C
      V1 = ZERO
      V2 = ZEP707
      V3 = ZEP707

      VV1 = V2 * ZN - V3 * YN
      VV2 = V3 * XN - V1 * ZN
      VV3 = V1 * YN - V2 * XN
      VV = SQRT(VV1*VV1 + VV2*VV2 + VV3*VV3)
      IF(VV<=HALF)THEN
        V1 = ZERO
        V2 = -ZEP707
        V3 = ZEP707
        VV1 = V2 * ZN - V3 * YN
        VV2 = V3 * XN - V1 * ZN
        VV3 = V1 * YN - V2 * XN
        VV = SQRT(VV1*VV1 + VV2*VV2 + VV3*VV3)
      ENDIF
      VV1 = VV1/VV
      VV2 = VV2/VV
      VV3 = VV3/VV
      V1 = VV2 * ZN - VV3 * YN
      V2 = VV3 * XN - VV1 * ZN
      V3 = VV1 * YN - VV2 * XN
C
      ALPHA = ZERO
      DO I=1,24
        S = SIN(ALPHA)
        C = COS(ALPHA)
        XX = VV1 * S + V1 * C
        YY = VV2 * S + V2 * C
        ZZ = VV3 * S + V3 * C
        D = THREE1000
        DO J=1,2
          CALL WRITE_S_C(NINT(XX*D),1)
          CALL WRITE_S_C(NINT(YY*D),1)
          CALL WRITE_S_C(NINT(ZZ*D),1)
        ENDDO
        ALPHA = ALPHA + PI*ONE_OVER_12
      ENDDO
C
      RETURN
      END
