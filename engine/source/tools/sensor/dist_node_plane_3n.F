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
Chd|  DIST_NODE_PLANE_3N            source/tools/sensor/dist_node_plane_3n.F
Chd|-- called by -----------
Chd|        SENSOR_DIST_SURF              source/tools/sensor/sensor_dist_surf.F
Chd|-- calls ---------------
Chd|====================================================================
        SUBROUTINE DIST_NODE_PLANE_3N (DIST,
     .             XNOD,YNOD,ZNOD,XP1,YP1,ZP1,XP2,YP2,ZP2,XP3,YP3,ZP3)
c-----------------------------------------------------------------------
c Calculates distance between NODE(XNOD,YNOD,ZNOD) 
c and a flat surface defined by three points P1, P2, P3
c-----------------------------------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real , INTENT(IN)  :: XNOD,YNOD,ZNOD,
     .                         XP1,YP1,ZP1,XP2,YP2,ZP2,XP3,YP3,ZP3
      my_real , INTENT(OUT) :: DIST
C----------------------------------------------------------
C Local Variables
C----------------------------------------------------------
      my_real :: AX,AY,AZ,BX,BY,BZ,VECX,VECY,VECZ,NX,NY,NZ,NORM
C=======================================================================
      AX = XP2 - XP1
      AY = YP2 - YP1
      AZ = ZP2 - ZP1
      BX = XP3 - XP1
      BY = YP3 - YP1
      BZ = ZP3 - ZP1
      VECX = XNOD - XP1 
      VECY = YNOD - YP1 
      VECZ = ZNOD - ZP1 
c
      NX = AY*BZ - AZ*BY
      NY = AZ*BX - AX*BZ
      NZ = AX*BY - AY*BX
      NORM = SQRT(NX*NX + NY*NY + NZ*NZ)
c     distance to plane
      DIST = ABS(NX*VECX + NY*VECY + NZ*VECZ) / NORM
c-----------
      RETURN
      END SUBROUTINE

