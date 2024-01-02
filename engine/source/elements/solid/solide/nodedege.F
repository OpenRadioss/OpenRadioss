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
Chd|  NODEDEGE                      source/elements/solid/solide/nodedege.F
Chd|-- called by -----------
Chd|        DEGES4V                       source/elements/solid/solide/deges4v.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE NODEDEGE(X,Y,Z,XYZ,NN)
C----6---------------------------------------------------------------7---------8
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NN
      my_real
     .   XYZ(3,*),X,Y,Z
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J
      my_real
     .   XIJ,YIJ,ZIJ
C----6---------------------------------------------------------------7---------8
       DO J=1,NN
              XIJ=XYZ(1,J)-X
              YIJ=XYZ(2,J)-Y
              ZIJ=XYZ(3,J)-Z
        IF (XIJ==ZERO.AND.YIJ==ZERO.AND.ZIJ==ZERO) RETURN
       END DO !J=1,NN
               NN = NN + 1
               XYZ(1,NN)=X
               XYZ(2,NN)=Y
               XYZ(3,NN)=Z
C
      RETURN
      END
