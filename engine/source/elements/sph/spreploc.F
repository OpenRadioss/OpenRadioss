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
Chd|  SPREPLOC                      source/elements/sph/spreploc.F
Chd|-- called by -----------
Chd|        SPSTRES                       source/elements/sph/spstres.F 
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPREPLOC(REPLOC,WXX,WYY,WZZ,GAMA,
     .                    NEL   )
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
      INTEGER NEL
C     REAL
      my_real
     .   REPLOC(NEL,6),WXX(*), WYY(*), WZZ(*), GAMA(MVSIZ,6)
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "vect01_c.inc"
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J
C     REAL
      my_real
     .   RX(MVSIZ),RY(MVSIZ),RZ(MVSIZ),CR(MVSIZ),SR(MVSIZ),
     .   NR,PS,UX,UY,UZ,VX,VY,VZ,WX,WY,WZ,TX,TY,TZ
C-----------------------------------------------
      IF (ISORTH == 0) THEN
        DO I=LFT,LLT
          GAMA(I,1)=ONE
          GAMA(I,2)=ZERO 
          GAMA(I,3)=ZERO 
          GAMA(I,4)=ZERO
          GAMA(I,5)=ONE 
          GAMA(I,6)=ZERO 
        ENDDO
      ELSE
C
        DO I=LFT,LLT
          NR=SQRT(WXX(I)*WXX(I)+WYY(I)*WYY(I)+WZZ(I)*WZZ(I))
          CR(I)=ONE-HALF*NR*NR
          SR(I)=NR
          NR=ONE/MAX(EM20,NR)
          RX(I)=WXX(I)*NR
          RY(I)=WYY(I)*NR
          RZ(I)=WZZ(I)*NR
        ENDDO
C
        DO I=LFT,LLT
          UX=REPLOC(I,1)
          UY=REPLOC(I,2)
          UZ=REPLOC(I,3)
          PS=UX*RX(I)+UY*RY(I)+UZ*RZ(I)
          VX=PS*RX(I)
          VY=PS*RY(I)
          VZ=PS*RZ(I)
          WX=UX-VX
          WY=UY-VY
          WZ=UZ-VZ
          TX=RY(I)*WZ-RZ(I)*WY
          TY=RZ(I)*WX-RX(I)*WZ
          TZ=RX(I)*WY-RY(I)*WX
          UX=VX+CR(I)*WX+SR(I)*TX
          UY=VY+CR(I)*WY+SR(I)*TY
          UZ=VZ+CR(I)*WZ+SR(I)*TZ
          NR=ONE/MAX(EM20,SQRT(UX*UX+UY*UY+UZ*UZ))
          REPLOC(I,1)=UX*NR
          REPLOC(I,2)=UY*NR
          REPLOC(I,3)=UZ*NR
        ENDDO
C
        DO I=LFT,LLT
          UX=REPLOC(I,4)
          UY=REPLOC(I,5)
          UZ=REPLOC(I,6)
          PS=UX*RX(I)+UY*RY(I)+UZ*RZ(I)
          VX=PS*RX(I)
          VY=PS*RY(I)
          VZ=PS*RZ(I)
          WX=UX-VX
          WY=UY-VY
          WZ=UZ-VZ
          TX=RY(I)*WZ-RZ(I)*WY
          TY=RZ(I)*WX-RX(I)*WZ
          TZ=RX(I)*WY-RY(I)*WX
          UX=VX+CR(I)*WX+SR(I)*TX
          UY=VY+CR(I)*WY+SR(I)*TY
          UZ=VZ+CR(I)*WZ+SR(I)*TZ
          NR=ONE/MAX(EM20,SQRT(UX*UX+UY*UY+UZ*UZ))
          REPLOC(I,4)=UX*NR
          REPLOC(I,5)=UY*NR
          REPLOC(I,6)=UZ*NR
        ENDDO
C
        DO I=LFT,LLT
          GAMA(I,1)=REPLOC(I,1)
          GAMA(I,2)=REPLOC(I,2)
          GAMA(I,3)=REPLOC(I,3)
          GAMA(I,4)=REPLOC(I,4)
          GAMA(I,5)=REPLOC(I,5)
          GAMA(I,6)=REPLOC(I,6)
        ENDDO
C
      ENDIF
C
      RETURN
      END
