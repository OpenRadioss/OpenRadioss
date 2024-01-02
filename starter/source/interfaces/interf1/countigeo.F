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
Chd|  COUNTIGEO                     source/interfaces/interf1/countigeo.F
Chd|-- called by -----------
Chd|        INSURFIGEO                    source/interfaces/interf1/insurfigeo.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE COUNTIGEO(IRECT,MNN,N,NRT_IGE,NRT_FE)
C-----------------------------------------------------------------------
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER N, NRT_IGE, NRT_FE
      INTEGER IRECT(*), MNN(*)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NTAG(0:2*NUMNOD+2*NUMFAKENODIGEO)
      INTEGER I, NN
C-----------------------------------------------
C
      NTAG(0:2*NUMNOD+2*NUMFAKENODIGEO) = 0
      NTAG(0) = 1      ! special case IRECT=0
      N = 0
      DO I=NRT_FE*4+1,NRT_FE*4+NRT_IGE*4     ! 4 : size of IRECT
        NN = IRECT(I)
        IF(NTAG(NN) == 0) THEN
          N=N+1
          MNN(N)=NN
          NTAG(NN)=1
        END IF            
      ENDDO
C
      RETURN
      END
