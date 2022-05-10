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
Cqz| nouvelle routine base sur SRROTA3-->par vector
Chd|====================================================================
Chd|  VRROTA3                       source/elements/thickshell/solide6c/vrrota3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|        S6RCOOR3                      source/elements/thickshell/solide6c/s6rcoor3.F
Chd|        S6RCOORK                      source/elements/thickshell/solide6c/s6rcoork.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE VRROTA3(
     .   R11, R12, R13, R21, R22, R23, R31, R32, R33,
     .   X1, Y1, Z1)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
C     REAL ou REAL*8
      my_real
     .   R11(*), R12(*), R13(*),
     .   R21(*), R22(*), R23(*),
     .   R31(*), R32(*), R33(*),
     .   X1(*),  Y1(*), Z1(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com06_c.inc"
#include      "units_c.inc"
#include      "vect01_c.inc"
#include      "scr18_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL ou REAL*8
      my_real
     .   X,Y,Z
C-----------------------------------------------
Clm   X=RX' <=> X'=t(R)X chgt de base.
      DO 10 I=LFT,LLT
       X=R11(I)*X1(I)+R21(I)*Y1(I)+R31(I)*Z1(I)
       Y=R12(I)*X1(I)+R22(I)*Y1(I)+R32(I)*Z1(I)
       Z=R13(I)*X1(I)+R23(I)*Y1(I)+R33(I)*Z1(I)
       X1(I)=X
       Y1(I)=Y
       Z1(I)=Z
   10 CONTINUE
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      RETURN
      END
