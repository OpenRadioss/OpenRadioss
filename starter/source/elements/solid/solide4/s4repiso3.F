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
Chd|  S4REPISO3                     source/elements/solid/solide4/s4repiso3.F
Chd|-- called by -----------
Chd|        S4REFSTA3                     source/elements/solid/solide4/s4refsta3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S4REPISO3(X1   ,X2   ,X3   ,X4   ,
     .                     Y1   ,Y2   ,Y3   ,Y4   ,
     .                     Z1   ,Z2   ,Z3   ,Z4   ,
     .                     RX   ,RY   ,RZ   ,
     .                     SX   ,SY   ,SZ   ,
     .                     TX   ,TY   ,TZ   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
      DOUBLE PRECISION
     .   X1(*), X2(*), X3(*), X4(*),Y1(*), Y2(*), Y3(*), Y4(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*)
       my_real
     .   RX(*) ,RY(*) ,RZ(*) ,SX(*) ,SY(*) ,SZ(*) ,TX(*) ,TY(*) ,TZ(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43
C=======================================================================
C     Repere isoparametrique element tetra 4 nodes
C
      DO I = LFT,LLT
        X43 = X4(I) - X3(I)
        Y43 = Y4(I) - Y3(I)
        Z43 = Z4(I) - Z3(I)
        X41 = X4(I) - X1(I)
        Y41 = Y4(I) - Y1(I)
        Z41 = Z4(I) - Z1(I)
        X42 = X4(I) - X2(I)
        Y42 = Y4(I) - Y2(I)
        Z42 = Z4(I) - Z2(I)
        RX(I) = -X41
        RY(I) = -Y41
        RZ(I) = -Z41
        SX(I) = -X42
        SY(I) = -Y42
        SZ(I) = -Z42
        TX(I) = -X43
        TY(I) = -Y43
        TZ(I) = -Z43
      ENDDO
C-------------
      RETURN
      END SUBROUTINE S4REPISO3

Chd|====================================================================
Chd|  S4REPISOT3                    source/elements/solid/solide4/s4repiso3.F
Chd|-- called by -----------
Chd|        S4REFSTA3                     source/elements/solid/solide4/s4refsta3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE S4REPISOT3(X1   ,X2   ,X3   ,X4   ,
     .                      Y1   ,Y2   ,Y3   ,Y4   ,
     .                      Z1   ,Z2   ,Z3   ,Z4   ,
     .                      RX   ,RY   ,RZ   ,
     .                      SX   ,SY   ,SZ   ,
     .                      TX   ,TY   ,TZ   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL
       my_real
     .   X1(*), X2(*), X3(*), X4(*),Y1(*), Y2(*), Y3(*), Y4(*),
     .   Z1(*), Z2(*), Z3(*), Z4(*),
     .   RX(*) ,RY(*) ,RZ(*) ,SX(*) ,SY(*) ,SZ(*) ,TX(*) ,TY(*) ,TZ(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .   X41, Y41, Z41, X42, Y42, Z42, X43, Y43, Z43
C=======================================================================
C     Repere isoparametrique element tetra 4 nodes
C
      DO I = LFT,LLT
        X43 = X4(I) - X3(I)
        Y43 = Y4(I) - Y3(I)
        Z43 = Z4(I) - Z3(I)
        X41 = X4(I) - X1(I)
        Y41 = Y4(I) - Y1(I)
        Z41 = Z4(I) - Z1(I)
        X42 = X4(I) - X2(I)
        Y42 = Y4(I) - Y2(I)
        Z42 = Z4(I) - Z2(I)
        RX(I) = -X41
        RY(I) = -Y41
        RZ(I) = -Z41
        SX(I) = -X42
        SY(I) = -Y42
        SZ(I) = -Z42
        TX(I) = -X43
        TY(I) = -Y43
        TZ(I) = -Z43
      ENDDO
C-------------
      RETURN
      END SUBROUTINE S4REPISOT3
