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
C-------------------------------------------------------------------------
Chd|====================================================================
Chd|  SPORTH3                       source/elements/sph/sporth3.F 
Chd|-- called by -----------
Chd|        SPINIT3                       source/elements/sph/spinit3.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPORTH3(IPART ,IPARTSP ,IGEO ,GAMA ,SKEW,
     .                   NEL   )
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
#include      "param_c.inc"
#include      "scr17_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER  IPART(LIPART1,*),IPARTSP(*),IGEO(NPROPGI,*),NEL
      my_real
     .  GAMA(NEL,6),SKEW(LSKEW,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, IPRT, IG, ISK
C=======================================================================
      DO I=LFT,LLT
        IPRT=IPARTSP(I)
        IG  =IPART(2,IPRT)
        ISK = IGEO(2,IG)
        IF (ISK/=0) THEN
C        ISK = GEO(2,IG)
          GAMA(I,1)= SKEW(1,ISK)
          GAMA(I,2)= SKEW(2,ISK)
          GAMA(I,3)= SKEW(3,ISK)
          GAMA(I,4)= SKEW(4,ISK)
          GAMA(I,5)= SKEW(5,ISK)
          GAMA(I,6)= SKEW(6,ISK)
        ENDIF
      ENDDO
C
      RETURN
      END
