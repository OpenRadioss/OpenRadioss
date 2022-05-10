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
Chd|  C3PXPY3                       source/elements/sh3n/coque3n/c3pxpy3.F
Chd|-- called by -----------
Chd|        C3FORC3                       source/elements/sh3n/coque3n/c3forc3.F
Chd|        C3FORC3_CRK                   source/elements/xfem/c3forc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE C3PXPY3(JFT   ,JLT ,PM  ,STI  ,STIR,
     2                   SMSTR ,PX1 ,PY1 ,PY2  ,MAT ,
     3                   SSP   ,NEL )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(*),JFT,JLT,MX,NEL
C     REAL ou REAL*8
      my_real
     .   SSP(MVSIZ),PM(NPROPM,*), STI(*), STIR(*), 
     .   PX1(*), PY1(*), PY2(*)
      DOUBLE PRECISION 
     .   SMSTR(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II(3)
c=======================================================================
      MX = MAT(JFT)
      DO I=JFT,JLT
        SSP(I) = PM(27,MX)
      ENDDO
C
      DO I=1,3
        II(I) = NEL*(I-1)
      ENDDO
C
      DO I=JFT,JLT
        PX1(I) = SMSTR(II(1)+I)
        PY1(I) = SMSTR(II(2)+I)
        PY2(I) = SMSTR(II(3)+I)
      ENDDO
C
      DO  I=JFT,JLT
        STI(I) = EP30
        STIR(I)= ZERO
      ENDDO
C-----------
      RETURN
      END
