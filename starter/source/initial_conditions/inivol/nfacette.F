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
Chd|  NFACETTE                      source/initial_conditions/inivol/nfacette.F
Chd|-- called by -----------
Chd|        GETPHASE                      source/initial_conditions/inivol/getphase.F
Chd|        IN_OUT_SIDE                   source/initial_conditions/inivol/in_out_side.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NFACETTE(XFAS,NX,NY,NZ)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   XFAS(3,4),NX,NY,NZ
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   RX,RY,RZ,SX,SY,SZ,SUM
C-----------------------------------------------
c       RX=XFAS(1,2)+XFAS(1,3)-XFAS(1,4)-XFAS(1,1)
c       SX=XFAS(1,3)+XFAS(1,4)-XFAS(1,1)-XFAS(1,2)
c       RY=XFAS(2,2)+XFAS(2,3)-XFAS(2,4)-XFAS(2,1)
c       SY=XFAS(2,3)+XFAS(2,4)-XFAS(2,1)-XFAS(2,2)
c       RZ=XFAS(3,2)+XFAS(3,3)-XFAS(3,4)-XFAS(3,1)
c       SZ=XFAS(3,3)+XFAS(3,4)-XFAS(3,1)-XFAS(3,2)
       RX=XFAS(1,3) - XFAS(1,1)
       SX=XFAS(1,4) - XFAS(1,2)
       RY=XFAS(2,3) - XFAS(2,1)
       SY=XFAS(2,4) - XFAS(2,2)
       RZ=XFAS(3,3) - XFAS(3,1)
       SZ=XFAS(3,4) - XFAS(3,2)
C---------NORMAL------------
       NX = RY * SZ - RZ * SY 
       NY = RZ * SX - RX * SZ 
       NZ = RX * SY - RY * SX 
       SUM = SQRT(NX*NX + NY*NY + NZ*NZ)
C
       SUM = ONE/MAX(SUM,EM30)
       NX = NX * SUM 
       NY = NY * SUM 
       NZ = NZ * SUM 
C---
      RETURN
      END
