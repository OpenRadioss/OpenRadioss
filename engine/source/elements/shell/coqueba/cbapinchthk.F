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
!||    cbapinchthk     ../engine/source/elements/shell/coqueba/cbapinchthk.F
!||--- called by ------------------------------------------------------
!||    cbaforc3        ../engine/source/elements/shell/coqueba/cbaforc3.F
!||--- uses       -----------------------------------------------------
!||    pinchtype_mod   ../common_source/modules/pinchtype_mod.F
!||====================================================================
      SUBROUTINE CBAPINCHTHK(
     1                       JFT   ,JLT   ,NPLAT  ,IPLAT  ,
     2                       DT1C  ,THK   ,THK0   ,EZZPG)
C-------------------------------------------------------------------------------
C   M o d u l e s
C-------------------------------------------------------------------------------
      USE PINCHTYPE_MOD
C-------------------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-------------------------------------------------------------------------------
#include      "implicit_f.inc"
C-------------------------------------------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-------------------------------------------------------------------------------
#include      "mvsiz_p.inc"
C-------------------------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-------------------------------------------------------------------------------
      INTEGER JFT, JLT, NPLAT,IPLAT(*)
C     REAL
      my_real
     .   DT1C(*), THK(*), THK0(*), EZZPG(MVSIZ,4)
C-------------------------------------------------------------------------------
C   L o c a l   V a r i a b l e s
C-------------------------------------------------------------------------------
      INTEGER I,J,EP
      my_real THKN(MVSIZ,4), THKAVG
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8

#include "vectorize.inc"
      DO I=JFT,JLT 
        EP=IPLAT(I)
        THKN(EP,1) = THK(EP) * (ONE + EZZPG(EP,1))
        THKN(EP,2) = THK(EP) * (ONE + EZZPG(EP,2))
        THKN(EP,3) = THK(EP) * (ONE + EZZPG(EP,3))
        THKN(EP,4) = THK(EP) * (ONE + EZZPG(EP,4))
        THKAVG = FOURTH*(THKN(EP,1)+THKN(EP,2)+THKN(EP,3)+THKN(EP,4))
        THK(EP) = THKAVG
        THK0(EP) = THKAVG      
      ENDDO       
      RETURN
      END
