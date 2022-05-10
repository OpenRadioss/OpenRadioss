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
Chd|  SEG_INTERSECT                 source/materials/fail/failwave/seg_intersect.F
Chd|-- called by -----------
Chd|        SET_FAILWAVE_NOD3             source/materials/fail/failwave/set_failwave_nod3.F
Chd|        SET_FAILWAVE_NOD4             source/materials/fail/failwave/set_failwave_nod4.F
Chd|-- calls ---------------
Chd|====================================================================
       INTEGER FUNCTION SEG_INTERSECT(X1    ,Y1    ,X2    ,Y2    ,X3    ,Y3    ,
     .                                X4    ,Y4    ,XINT  ,YINT  ,IDEBUG       )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real  :: X1,Y1,X2,Y2,X3,Y3,X4,Y4,XINT,YINT,RATIO
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER  :: OK,IDEBUG
      my_real  :: AX,BX,AY,BY,CX,CY,SY,SX,DM,ALPHA,BETA,TOL
c----------------------------------------------------------
c     calcule intersection de deux segments en 2D : N1-N2 et N3-N4
c     intersection trouve => OK=1, sinon OK=0
c     alpha, beta = ratio du point intersection de chaque segment
c=======================================================================
      OK = 0
      AX = X2 - X1
      AY = Y2 - Y1
      BX = X4 - X3
      BY = Y4 - Y3
c
      DM = AY*BX - AX*BY
      IF (DM /= ZERO) THEN  ! les lignes ne sont pas paralleles
        CX = X3 - X1
        CY = Y3 - Y1
c
         ALPHA = (BX * CY - BY * CX) / DM
         BETA  = (AX * CY - AY * CX) / DM
c
         IF (ALPHA >= ZERO .and. ALPHA <= UN .and.
     .       BETA  >= ZERO .and. BETA  <= UN) THEN
           OK = 1
           XINT = X1 + ALPHA * AX
           YINT = Y1 + ALPHA * AY
         ENDIF
c         IF (IDEBUG == 1) print*,'ALPHA,BETA=',ALPHA,BETA
       ENDIF
       SEG_INTERSECT = OK
c-----------
      RETURN
      END
