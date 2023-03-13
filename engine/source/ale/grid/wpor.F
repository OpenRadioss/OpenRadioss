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
Chd|  WPOR                          source/ale/grid/wpor.F        
Chd|-- called by -----------
Chd|        ALEWDX                        source/ale/grid/alewdx.F      
Chd|-- calls ---------------
Chd|====================================================================
       SUBROUTINE WPOR(
     1    GEO,NODPOR,X   ,V      ,VR     ,
     2    W  ,RBY   ,NALE,NPORGEO        )
C-----------------------------------------------
C   D e s c r i p t i o n
C-----------------------------------------------
C     GRID VELOCITY POROUS MEDIA
C     AUGUST 1994
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "param_c.inc"
#include      "com04_c.inc"
#include      "com08_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NODPOR(*), NALE(*), NPORGEO(*)
      my_real GEO(NPROPG,NUMGEO),X(3,NUMNOD),V(3,NUMNOD),VR(3,NUMNOD),W(3,NUMNOD),RBY(NRBY,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NAD,IG,N,IR,M,I,JP
      my_real VG(3),V1X2,V2X1,V2X3,V3X2,V3X1,V1X3
C-----------------------------------------------
C   S o u r c e   L i n e s
C-----------------------------------------------
      NAD=0
      DO IG=1,NUMGEO
        IF(INT(GEO(12,IG)) /= 15)CYCLE
        N = NPORGEO(IG)        ! num of local nodes stored in a specific spmd array
        IF(N == 0)CYCLE
        IR=INT(GEO(33,IG))
        IF(IR > 0)THEN
          M=INT(GEO(29,IG))
          VG(1)=VR(1,M)
          VG(2)=VR(2,M)
          VG(3)=VR(3,M)
          DO I=NAD+1,NAD+N
            JP=NODPOR(I)
              IF(IABS(NALE(JP)) == 1)THEN
        	V1X2=VG(1)*(X(2,JP)-X(2,M))
        	V2X1=VG(2)*(X(1,JP)-X(1,M))
        	V2X3=VG(2)*(X(3,JP)-X(3,M))
        	V3X2=VG(3)*(X(2,JP)-X(2,M))
        	V3X1=VG(3)*(X(1,JP)-X(1,M))
        	V1X3=VG(1)*(X(3,JP)-X(3,M))
        	W(1,JP)= V(1,M) + V2X3-V3X2-HALF*DT12*(VG(2)*V2X1+VG(3)*V3X1)
        	W(2,JP)= V(2,M) + V3X1-V1X3-HALF*DT12*(VG(3)*V3X2+VG(1)*V1X2)
        	W(3,JP)= V(3,M) + V1X2-V2X1-HALF*DT12*(VG(1)*V1X3+VG(2)*V2X3)
              ENDIF
          ENDDO 
        ENDIF
        NAD=NAD+N
      ENDDO!next IG
C
      RETURN
      END
