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
Chd|  SHAPEH                        source/ale/shapeh.F           
Chd|-- called by -----------
Chd|        I9AVEL                        source/interfaces/int09/i9avel.F
Chd|        I9FRMS                        source/interfaces/int09/i9frms.F
Chd|        I9WAL3                        source/interfaces/int09/i9wal3.F
Chd|        IQELA1                        source/ale/iqela1.F           
Chd|        IQELA2                        source/ale/iqela2.F           
Chd|        IQELA3                        source/ale/iqela3.F           
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SHAPEH(H,S,T)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real,INTENT(IN)    :: S, T
      my_real,INTENT(INOUT) :: H(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real :: SP, SM, TP, TM
C-----------------------------------------------
      SP   = UN +S
      SM   = UN -S
      TP   = QUART*(UN + T)
      TM   = QUART*(UN - T)
      H(1) = TM*SM
      H(2) = TM*SP
      H(3) = TP*SP
      H(4) = TP*SM
C-----------------------------------------------

      RETURN
      END
