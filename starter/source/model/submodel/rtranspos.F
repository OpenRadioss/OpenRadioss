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
Chd|  RTRANSPOS                     source/model/submodel/rtranspos.F
Chd|-- called by -----------
Chd|        LECTRANSSUB                   source/model/submodel/lectranssub.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE RTRANSPOS(POINT     ,MYTRANS, RTRANS )
C-----------------------------------------------
C     TRANSFORMATION OF COORDS OF POINT ACCORDING TO RTRANS(MYTRANS)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MYTRANS
      my_real
     .   POINT(3),RTRANS(NTRANSF,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K
      my_real
     .   XP, YP, ZP, ROT(9), XCOLD(3), XCNEW(3)
C-----------------------------------------------
      DO K=1,3
        XCOLD(K)=RTRANS(MYTRANS,K+11)
      ENDDO
      DO K=1,3
        XCNEW(K)=RTRANS(MYTRANS,K+14)
      ENDDO
      DO K=1,9
        ROT(K)=RTRANS(MYTRANS,K+2)
      ENDDO

      XP = POINT(1) - XCOLD(1)
      YP = POINT(2) - XCOLD(2)
      ZP = POINT(3) - XCOLD(3)
      POINT(1) = XCNEW(1) + ROT(1)*XP + ROT(4)*YP + ROT(7)*ZP                        
      POINT(2) = XCNEW(2) + ROT(2)*XP + ROT(5)*YP + ROT(8)*ZP                         
      POINT(3) = XCNEW(3) + ROT(3)*XP + ROT(6)*YP + ROT(9)*ZP                         
C-----------------------------------------------
      RETURN
      END 
