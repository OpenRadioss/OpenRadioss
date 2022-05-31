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
C-------------------------------------------------------------------------
Chd|====================================================================
Chd|  M24IN3                        source/materials/mat/mat024/m24in3.F
Chd|-- called by -----------
Chd|        MATINI                        source/materials/mat_share/matini.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M24IN3(PM,IX,ANG,SF,VK0,ROB,NEL)
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER IX(NIXS,*),NEL
      my_real PM(NPROPM,*),ANG(NEL,6),SF(NEL,3),VK0(NEL),ROB(NEL)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX
C=======================================================================
      DO I=LFT,LLT
        ANG(I,1)  = ONE
        ANG(I,2)  = ZERO
        ANG(I,3)  = ZERO
        ANG(I,4)  = ZERO
        ANG(I,5)  = ONE
        ANG(I,6)  = ZERO
      ENDDO
      MX = IX(1,1+NFT)
      SF (1:NEL,1:3) = -ONE
      VK0(1:NEL) = PM(46,MX)
      ROB(1:NEL) = PM(30,MX)
c-----------
      RETURN
      END
