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
Chd|  VISC_ET                       source/elements/solid/solidez/szforc3.F
Chd|-- called by -----------
Chd|        SZFORC3                       source/elements/solid/solidez/szforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VISC_ET(
     .           NEL     ,MXT     ,IPM     ,UPARAM  ,CXX     ,PM      ,
     .           ET      )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,MXT(*)
      INTEGER IPM(NPROPMI,*)
      my_real
     .   UPARAM(*),CXX(*),ET(*),PM(NPROPM,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,J ,IADBUF, NPRONY,IMAT
      my_real
     .   GG,GV,RHO0,NU,GV_MAX,NU1
C=======================================================================        
      IMAT = MXT(1)
      IADBUF = IPM(223,IMAT)  
      NPRONY  = NINT(UPARAM(IADBUF + 1))
C
      GV  = ZERO
      DO J=1,NPRONY                                  
       GV   = GV  +  UPARAM(IADBUF + 1 + J)            
      ENDDO
C
      RHO0=PM(1,IMAT)
      NU=PM(21,IMAT)
      GV_MAX = THIRD*(ONE+NU)/(ONE -TWO*NU)
      NU1= RHO0*(ONE -TWO*NU)/(ONE-NU)
C
      DO I=1,NEL
       GG = CXX(I)*CXX(I)*NU1    
       ET(I)= ET(I) + MIN(GV_MAX,GV/GG)
      ENDDO
C------------
      RETURN
      END
