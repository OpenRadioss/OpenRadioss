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
Chd|  FXBVINI                       source/constraints/fxbody/fxbvini.F
Chd|-- called by -----------
Chd|        INITIA                        source/elements/initia/initia.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FXBVINI(FXBIPM, FXBVIT, FXBRPM, V, VR)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com04_c.inc"
#include      "fxbcom.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER FXBIPM(NBIPM,*)
      my_real
     .   FXBVIT(*), FXBRPM(*), V(3,*), VR(3,*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER NFX,I,J,AVAR,ARPM,IDMAST
      my_real
     .   P11,P12,P13,P21,P22,P23,P31,P32,P33,W1,W2,W3,
     .   WL1,WL2,WL3
C
      DO NFX=1,NFXBODY
         IDMAST=FXBIPM(2,NFX)
         AVAR=FXBIPM(13,NFX)
         ARPM=FXBIPM(14,NFX)
         DO I=1,3
            FXBVIT(AVAR+9+I-1)=V(I,IDMAST)
         END DO
         P11=FXBRPM(ARPM+1)   
         P12=FXBRPM(ARPM+2)   
         P13=FXBRPM(ARPM+3)   
         P21=FXBRPM(ARPM+4)   
         P22=FXBRPM(ARPM+5)   
         P23=FXBRPM(ARPM+6)   
         P31=FXBRPM(ARPM+7)   
         P32=FXBRPM(ARPM+8)   
         P33=FXBRPM(ARPM+9)
         W1=VR(1,IDMAST)
         W2=VR(2,IDMAST)
         W3=VR(3,IDMAST)
         WL1=P11*W1+P21*W2+P31*W3
         WL2=P12*W1+P22*W2+P32*W3
         WL3=P13*W1+P23*W2+P33*W3
         FXBVIT(AVAR)  =V(1,IDMAST)+P12*WL3-P13*WL2
         FXBVIT(AVAR+1)=V(2,IDMAST)+P22*WL3-P23*WL2
         FXBVIT(AVAR+2)=V(3,IDMAST)+P32*WL3-P33*WL2
         FXBVIT(AVAR+3)=V(1,IDMAST)-P11*WL3+P13*WL1
         FXBVIT(AVAR+4)=V(2,IDMAST)-P21*WL3+P23*WL1
         FXBVIT(AVAR+5)=V(3,IDMAST)-P31*WL3+P33*WL1
         FXBVIT(AVAR+6)=V(1,IDMAST)+P11*WL2-P12*WL1
         FXBVIT(AVAR+7)=V(2,IDMAST)+P21*WL2-P22*WL1
         FXBVIT(AVAR+8)=V(3,IDMAST)+P31*WL2-P32*WL1
      ENDDO
C      
      RETURN
      END  
         
