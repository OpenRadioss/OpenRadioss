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
Chd|  SORTHDIR17                    source/elements/solid/solide8z/srcoork.F
Chd|-- called by -----------
Chd|        SRCOORK                       source/elements/solid/solide8z/srcoork.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SORTHDIR17(
     1   RX,      RY,      RZ,      SX,
     2   SY,      SZ,      TX,      TY,
     3   TZ,      E1X,     E2X,     E3X,
     4   E1Y,     E2Y,     E3Y,     E1Z,
     5   E2Z,     E3Z,     GAMA,    NEL,
     6   JCVT)
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
      INTEGER, INTENT(IN) :: NEL
      INTEGER, INTENT(IN) :: JCVT
      my_real, DIMENSION(NEL), INTENT(IN)    ::
     .   RX, RY, RZ, SX, SY, SZ, TX, TY, TZ,
     .   E1X, E1Y, E1Z, E2X, E2Y, E2Z, E3X, E3Y, E3Z
      my_real, 
     .  DIMENSION(MVSIZ,6), INTENT(INOUT) :: GAMA
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C     REAL
      my_real
     .  UX,UY,UZ,VX,VY,VZ,WX,WY,WZ,D1,D2,D3,GX,GY,GZ,SUMA,S2,S3
      my_real, 
     .  DIMENSION(6)  :: GAMA0
C=======================================================================
        IF (JCVT== 0) THEN
C------U,V,DW ->R,S,T orthogalized        
         DO I=1,NEL                                            
          SUMA = ONE/SQRT(RX(I)**2+RY(I)**2+RZ(I)**2)
          UX = RX(I)*SUMA
          UY = RY(I)*SUMA
          UZ = RZ(I)*SUMA             
          WX = UY*SZ(I)-UZ*SY(I)                 
          WY = UZ*SX(I)-UX*SZ(I)                 
          WZ = UX*SY(I)-UY*SX(I)                 
          SUMA = ONE/SQRT(WX*WX + WY*WY + WZ*WZ)
          WX = WX*SUMA                 
          WY = WY*SUMA                 
          WZ = WZ*SUMA                 
          VX = WY*UZ-WZ*UY                 
          VY = WZ*UX-WX*UZ                 
          VZ = WX*UY-WY*UX                 
          SUMA = ONE/SQRT(VX*VX + VY*VY + VZ*VZ)
          VX = VX*SUMA                 
          VY = VY*SUMA                 
          VZ = VZ*SUMA
C---------refind --_global ortho          
          GAMA0(1)= GAMA(I,1)*UX+GAMA(I,2)*VX+GAMA(I,3)*WX
          GAMA0(2)= GAMA(I,1)*UY+GAMA(I,2)*VY+GAMA(I,3)*WY
          GAMA0(3)= GAMA(I,1)*UZ+GAMA(I,2)*VZ+GAMA(I,3)*WZ
C
          GAMA0(4)= GAMA(I,4)*UX+GAMA(I,5)*VX+GAMA(I,6)*WX
          GAMA0(5)= GAMA(I,4)*UY+GAMA(I,5)*VY+GAMA(I,6)*WY
          GAMA0(6)= GAMA(I,4)*UZ+GAMA(I,5)*VZ+GAMA(I,6)*WZ
C         
          GAMA(I,1)=
     .      GAMA0(1)*E1X(I)+GAMA0(2)*E1Y(I)+GAMA0(3)*E1Z(I)
          GAMA(I,2)=
     .      GAMA0(1)*E2X(I)+GAMA0(2)*E2Y(I)+GAMA0(3)*E2Z(I)
          GAMA(I,3)=
     .      GAMA0(1)*E3X(I)+GAMA0(2)*E3Y(I)+GAMA0(3)*E3Z(I)
          GAMA(I,4)=
     .      GAMA0(4)*E1X(I)+GAMA0(5)*E1Y(I)+GAMA0(6)*E1Z(I)
          GAMA(I,5)=
     .      GAMA0(4)*E2X(I)+GAMA0(5)*E2Y(I)+GAMA0(6)*E2Z(I)
          GAMA(I,6)=
     .      GAMA0(4)*E3X(I)+GAMA0(5)*E3Y(I)+GAMA0(6)*E3Z(I)
         ENDDO                                     
C
        ELSE
          DO I=1,NEL
            GAMA0(1)  = GAMA(I,1)   
            GAMA0(2)  = GAMA(I,2)   
            GAMA0(3)  = GAMA(I,3)   
            GAMA0(4)  = GAMA(I,4)   
            GAMA0(5)  = GAMA(I,5)   
            GAMA0(6)  = GAMA(I,6)   
!                                       
            GAMA(I,1) = GAMA0(3)                             
            GAMA(I,2) = GAMA0(1)                             
            GAMA(I,3) = GAMA0(2)
            GAMA(I,4) = GAMA0(6)                             
            GAMA(I,5) = GAMA0(4)                           
            GAMA(I,6) = GAMA0(5)
          ENDDO      
        ENDIF                                      
C-------------
      RETURN
      END
