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
C   This function uses a 3d rotation matrix to turn a point x around a
C   center X0.
Chd|====================================================================
Chd|  EULER_VROT                    source/model/submodel/euler_vrot.F
Chd|-- called by -----------
Chd|        HM_READ_SKW                   source/tools/skew/hm_read_skw.F
Chd|        LECSUBMOD                     source/model/submodel/lecsubmod.F
Chd|        LECTRANS                      source/model/transformation/lectrans.F
Chd|        SUBROTPOINT                   source/model/submodel/subrot.F
Chd|        SUBROTVECT                    source/model/submodel/subrot.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE EULER_VROT (X0,X,ROT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      my_real
     .   X0(3),X(3), ROT(9)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
C     REAL
      my_real
     .   RX, RY, RZ
C=======================================================================
      RX = X(1) - X0(1)
      RY = X(2) - X0(2)
      RZ = X(3) - X0(3)
C                                              
      X(1) = X0(1) + ROT(1)*RX + ROT(4)*RY + ROT(7)*RZ
      X(2) = X0(2) + ROT(2)*RX + ROT(5)*RY + ROT(8)*RZ
      X(3) = X0(3) + ROT(3)*RX + ROT(6)*RY + ROT(9)*RZ
C---
      RETURN
      END
