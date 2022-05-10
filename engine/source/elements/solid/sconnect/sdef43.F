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
Chd|  SDEF43                        source/elements/solid/sconnect/sdef43.F
Chd|-- called by -----------
Chd|        SUSER43                       source/elements/solid/sconnect/suser43.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SDEF43(NEL    ,NPG    ,HH     ,
     .                  DZX    ,DYZ    ,DZZ    ,
     .                  VXLOC  ,VYLOC  ,VZLOC  )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
CC-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include "mvsiz_p.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NEL,NPG
C     REAL ou REAL*8
      my_real 
     .   VXLOC(MVSIZ,8),VYLOC(MVSIZ,8),VZLOC(MVSIZ,8),HH(4,NPG),
     .   DZX(MVSIZ,NPG),DYZ(MVSIZ,NPG),DZZ(MVSIZ,NPG)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER IEL,IPG
C     REAL ou REAL*8
      my_real
     .   PG,P1,P2,P3,DXINF,DYINF,DZINF,DXSUP,DYSUP,DZSUP
      DATA   PG/0.5773502691896257645091488/
C=======================================================================
      P1 = (UN + PG)*(UN - PG) * QUART
      P2 = (UN + PG)*(UN + PG) * QUART
      P3 = (UN - PG)*(UN - PG) * QUART
C
      HH(1,1) = P2
      HH(2,1) = P1
      HH(3,1) = P3
      HH(4,1) = P1
C
      HH(1,2) = P1
      HH(2,2) = P2
      HH(3,2) = P1
      HH(4,2) = P3
C
      HH(1,3) = P3
      HH(2,3) = P1
      HH(3,3) = P2
      HH(4,3) = P1
C
      HH(1,4) = P1
      HH(2,4) = P3
      HH(3,4) = P1
      HH(4,4) = P2
C
      DO IPG = 1,NPG
        DO IEL = 1,NEL
          DXSUP   = HH(1,IPG)*VXLOC(IEL,5)+HH(2,IPG)*VXLOC(IEL,6)
     .            + HH(3,IPG)*VXLOC(IEL,7)+HH(4,IPG)*VXLOC(IEL,8)
          DXINF   = HH(1,IPG)*VXLOC(IEL,1)+HH(2,IPG)*VXLOC(IEL,2)
     .            + HH(3,IPG)*VXLOC(IEL,3)+HH(4,IPG)*VXLOC(IEL,4)
          DYSUP   = HH(1,IPG)*VYLOC(IEL,5)+HH(2,IPG)*VYLOC(IEL,6)
     .            + HH(3,IPG)*VYLOC(IEL,7)+HH(4,IPG)*VYLOC(IEL,8)
          DYINF   = HH(1,IPG)*VYLOC(IEL,1)+HH(2,IPG)*VYLOC(IEL,2)
     .            + HH(3,IPG)*VYLOC(IEL,3)+HH(4,IPG)*VYLOC(IEL,4)
          DZSUP   = HH(1,IPG)*VZLOC(IEL,5)+HH(2,IPG)*VZLOC(IEL,6)
     .            + HH(3,IPG)*VZLOC(IEL,7)+HH(4,IPG)*VZLOC(IEL,8)
          DZINF   = HH(1,IPG)*VZLOC(IEL,1)+HH(2,IPG)*VZLOC(IEL,2)
     .            + HH(3,IPG)*VZLOC(IEL,3)+HH(4,IPG)*VZLOC(IEL,4)
C
          DZZ(IEL,IPG) = DZSUP - DZINF
          DYZ(IEL,IPG) = DYSUP - DYINF
          DZX(IEL,IPG) = DXSUP - DXINF
        ENDDO
      ENDDO
C-----------
      RETURN
      END


