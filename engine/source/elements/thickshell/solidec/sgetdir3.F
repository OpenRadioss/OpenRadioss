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
Chd|  SGETDIR3                      source/elements/thickshell/solidec/sgetdir3.F
Chd|-- called by -----------
Chd|        S6CFORC3                      source/elements/thickshell/solide6c/s6cforc3.F
Chd|        SCFORC3                       source/elements/thickshell/solidec/scforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SGETDIR3(NEL,RX, RY, RZ, TX, TY, TZ,
     .                    E1X,E1Y,E1Z,E2X,E2Y,E2Z,DIR,DIRB,IREP)
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
      INTEGER :: NEL,IREP
C
      my_real
     .   RX(*),RY(*),RZ(*),TX(*),TY(*),TZ(*),DIR(NEL,6),DIRB(MVSIZ,2),
     .   E1X(*),E1Y(*),E1Z(*),E2X(*),E2Y(*),E2Z(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I
C
      my_real 
     .   V1,V2,V3,VR,VS,AA,BB,SUMA
C-----------------------------------------------
      IF (IREP == 0) THEN                   
        DO I=1,NEL                        
          DIRB(I,1) = DIR(I,1)              
          DIRB(I,2) = DIR(I,2)              
        ENDDO                               
      ELSE                                  
        DO I=1,NEL                        
          AA = DIR(I,1)                     
          BB = DIR(I,2)                     
          V1 = AA*TX(I) + BB*RX(I)          
          V2 = AA*TY(I) + BB*RY(I)          
          V3 = AA*TZ(I) + BB*RZ(I)          
          VR=V1*E1X(I)+V2*E1Y(I)+V3*E1Z(I)  
          VS=V1*E2X(I)+V2*E2Y(I)+V3*E2Z(I)  
          SUMA=SQRT(VR*VR + VS*VS)          
          SUMA=ONE/MAX(EM20,SUMA)            
          DIRB(I,1) = VR*SUMA               
          DIRB(I,2) = VS*SUMA               
        ENDDO                               
      ENDIF                                 
C-----------
      RETURN
      END
