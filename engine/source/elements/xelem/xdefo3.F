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
Chd|  XDEFO3                        source/elements/xelem/xdefo3.F
Chd|-- called by -----------
Chd|        XFORC3                        source/elements/xelem/xforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE XDEFO3(V  ,VR   ,KXX   ,IXX   ,NX   ,
     2                 VUSR ,VRUSR )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "scr23_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER KXX(NIXX),IXX(*),NX
C     REAL
      my_real
     .   V(3,*),VR(3,*),VUSR(3,NX),VRUSR(3,NX)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER K,I1,IADNOD
C     REAL
C--------------------------------------------
C     VITESSES REPERE GLOBAL.
C--------------------------------------------
      IADNOD=KXX(4)
      DO K=1,NX
         I1=IXX(IADNOD+K-1)
         VUSR(1,K) =V(1,I1)
         VUSR(2,K) =V(2,I1)
         VUSR(3,K) =V(3,I1)
         VRUSR(1,K)=VR(1,I1)
         VRUSR(2,K)=VR(2,I1)
         VRUSR(3,K)=VR(3,I1)
      ENDDO
      RETURN
      END
