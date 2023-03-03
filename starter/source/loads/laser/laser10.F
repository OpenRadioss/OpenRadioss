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
Chd|  LASER10                       source/loads/laser/laser10.F  
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|        LASER20                       source/loads/laser/laser20.F  
Chd|====================================================================
      SUBROUTINE LASER10(LAS ,XLAS ,X   ,IXQ,IPARG )
C
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "param_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   XLAS(*),X(3,*)
      INTEGER IPARG(NPARG,*),LAS(*),IXQ(7,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, K1, K2, NL, NS1, NS2
C
      K1 = 1
      K2 = 1
      NS1 = 0
      NS2 = 0
      DO I=1,NLASER
        NL = LAS(K1)
        IF(NL == 0)CYCLE
        CALL LASER20(NL ,NS1 ,NS2 ,LAS(K1+6) ,XLAS(K2) ,X,IXQ,IPARG)
        LAS(K1+1) = NS1
        LAS(K1+2) = NS2
        K1 = K1 + 6 + 2*(NL+1)
        K2 = K2 + 6
      END DO !I=1,NLASER
C
      RETURN
      END
