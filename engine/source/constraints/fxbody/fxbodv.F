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
Chd|  FXLINK                        source/constraints/fxbody/fxbodv.F
Chd|-- called by -----------
Chd|        FXBODVP1                      source/constraints/fxbody/fxbodvp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FXLINK(CR    , SR,  DT1, DT2, FXBRPM,
     .                  FXBVIT, NME)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER NME
      my_real
     .   CR(6,*), SR(*), DT1, DT2, FXBRPM(*), FXBVIT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II
      my_real
     .   V1(3),V2(3),V3(3),V4(3),V5(3),V6(3)
      my_real
     .   D1,D2,D3,D4,D5,D6
C-----------------------------------------------
C   VECTEURS
C-----------------------------------------------
      V1(1)=FXBRPM(2)
      V1(2)=FXBRPM(5)    
      V1(3)=FXBRPM(8)    
      V2(1)=FXBRPM(3)
      V2(2)=FXBRPM(6)
      V2(3)=FXBRPM(9)
      V3(1)=FXBRPM(4)
      V3(2)=FXBRPM(7)
      V3(3)=FXBRPM(10)
      V4(1)=V2(1)-V1(1)
      V4(2)=V2(2)-V1(2)  
      V4(3)=V2(3)-V1(3)  
      V5(1)=V3(1)-V1(1)
      V5(2)=V3(2)-V1(2)  
      V5(3)=V3(3)-V1(3)  
      V6(1)=V3(1)-V2(1)
      V6(2)=V3(2)-V2(2)  
      V6(3)=V3(3)-V2(3)  
C
      D1=ZERO
      D2=ZERO
      D3=ZERO
      D4=ZERO
      D5=ZERO
      D6=ZERO
      DO I=1,3
         D1=D1+V1(I)**2
         D2=D2+V2(I)**2
         D3=D3+V3(I)**2
         D4=D4+V4(I)**2
         D5=D5+V5(I)**2
         D6=D6+V6(I)**2
      END DO
C-----------------------------------------------
C   MATRICE
C-----------------------------------------------
      DO I=1,6
         DO II=1,NME
            CR(I,II)=ZERO
         ENDDO
      ENDDO
C
      CR(1,10)=-V1(1)-DT2*(FXBVIT(1)-FXBVIT(10))
      CR(1,11)=-V1(2)-DT2*(FXBVIT(2)-FXBVIT(11))
      CR(1,12)=-V1(3)-DT2*(FXBVIT(3)-FXBVIT(12))
      CR(1,1)=-CR(1,10)
      CR(1,2)=-CR(1,11)
      CR(1,3)=-CR(1,12)
C
      CR(2,10)=-V2(1)-DT2*(FXBVIT(4)-FXBVIT(10))
      CR(2,11)=-V2(2)-DT2*(FXBVIT(5)-FXBVIT(11))
      CR(2,12)=-V2(3)-DT2*(FXBVIT(6)-FXBVIT(12))
      CR(2,4)=-CR(2,10)
      CR(2,5)=-CR(2,11)
      CR(2,6)=-CR(2,12)
C 
      CR(3,10)=-V3(1)-DT2*(FXBVIT(7)-FXBVIT(10))
      CR(3,11)=-V3(2)-DT2*(FXBVIT(8)-FXBVIT(11))
      CR(3,12)=-V3(3)-DT2*(FXBVIT(9)-FXBVIT(12))
      CR(3,7)=-CR(3,10)
      CR(3,8)=-CR(3,11)
      CR(3,9)=-CR(3,12)
C      
      CR(4,1)=-V4(1)-DT2*(FXBVIT(4)-FXBVIT(1))
      CR(4,2)=-V4(2)-DT2*(FXBVIT(5)-FXBVIT(2))
      CR(4,3)=-V4(3)-DT2*(FXBVIT(6)-FXBVIT(3))
      CR(4,4)=-CR(4,1)
      CR(4,5)=-CR(4,2)
      CR(4,6)=-CR(4,3)
C
      CR(5,1)=-V5(1)-DT2*(FXBVIT(7)-FXBVIT(1))
      CR(5,2)=-V5(2)-DT2*(FXBVIT(8)-FXBVIT(2))
      CR(5,3)=-V5(3)-DT2*(FXBVIT(9)-FXBVIT(3))
      CR(5,7)=-CR(5,1)
      CR(5,8)=-CR(5,2)
      CR(5,9)=-CR(5,3)
C 
      CR(6,4)=-V6(1)-DT2*(FXBVIT(7)-FXBVIT(4))
      CR(6,5)=-V6(2)-DT2*(FXBVIT(8)-FXBVIT(5))
      CR(6,6)=-V6(3)-DT2*(FXBVIT(9)-FXBVIT(6))
      CR(6,7)=-CR(6,4)
      CR(6,8)=-CR(6,5)
      CR(6,9)=-CR(6,6)
C-----------------------------------------------
C   SECOND MEMBRE
C-----------------------------------------------
      DO I=1,6
         SR(I)=ZERO
      ENDDO
      DO I=1,3
         SR(1)=SR(1)-ONE/(DT1+DT2)*(TWO*V1(I)+
     .               DT2*(FXBVIT(I)-FXBVIT(9+I)))
     .                  *(FXBVIT(I)-FXBVIT(9+I))
         SR(2)=SR(2)-ONE/(DT1+DT2)*(TWO*V2(I)+
     .               DT2*(FXBVIT(3+I)-FXBVIT(9+I)))
     .                  *(FXBVIT(3+I)-FXBVIT(9+I))
         SR(3)=SR(3)-ONE/(DT1+DT2)*(TWO*V3(I)+
     .               DT2*(FXBVIT(6+I)-FXBVIT(9+I)))
     .                  *(FXBVIT(6+I)-FXBVIT(9+I))
         SR(4)=SR(4)-ONE/(DT1+DT2)*(TWO*V4(I)+
     .               DT2*(FXBVIT(3+I)-FXBVIT(I)))
     .                  *(FXBVIT(3+I)-FXBVIT(I))
         SR(5)=SR(5)-ONE/(DT1+DT2)*(TWO*V5(I)+
     .               DT2*(FXBVIT(6+I)-FXBVIT(I)))
     .                  *(FXBVIT(6+I)-FXBVIT(I))
         SR(6)=SR(6)-ONE/(DT1+DT2)*(TWO*V6(I)+
     .               DT2*(FXBVIT(6+I)-FXBVIT(3+I)))
     .                  *(FXBVIT(6+I)-FXBVIT(3+I))
      END DO
      SR(1)=SR(1)+ONE/(DT2*(DT1+DT2))*(ONE-D1)
      SR(2)=SR(2)+ONE/(DT2*(DT1+DT2))*(ONE-D2)
      SR(3)=SR(3)+ONE/(DT2*(DT1+DT2))*(ONE-D3)
      SR(4)=SR(4)+ONE/(DT2*(DT1+DT2))*(TWO-D4)
      SR(5)=SR(5)+ONE/(DT2*(DT1+DT2))*(TWO-D5)
      SR(6)=SR(6)+ONE/(DT2*(DT1+DT2))*(TWO-D6)
C      
      RETURN
      END   
Chd|====================================================================
Chd|  SPLINK                        source/constraints/fxbody/fxbodv.F
Chd|-- called by -----------
Chd|        FXBODVP1                      source/constraints/fxbody/fxbodvp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE SPLINK(MT, ST, DT1, FXBRPM, FXBVIT,
     .                  DMT)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER DMT
      my_real
     .   MT(DMT,*), ST(*), DT1, FXBRPM(*), FXBVIT(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I,II
      my_real
     .   P11,P12,P13,P21,P22,P23,P31,P32,P33,DT05
C      
      DT05=HALF*DT1
C
      DO I=1,3
         DO II=1,DMT
            MT(21+I,II)=ZERO
         ENDDO
         DO II=16,21
            MT(II,21+I)=ZERO
         ENDDO
      ENDDO
C      
      P11=FXBRPM(2)
      P12=FXBRPM(3)
      P13=FXBRPM(4)
      P21=FXBRPM(5)
      P22=FXBRPM(6)
      P23=FXBRPM(7)
      P31=FXBRPM(8)
      P32=FXBRPM(9)
      P33=FXBRPM(10)
C
      IF (NCYCLE==0) THEN
         MT(22,13)=ONE
         MT(23,14)=ONE
         MT(24,15)=ONE
      ELSE
         MT(22,4)=-P13*P11
         MT(22,5)=-P13*P21
         MT(22,6)=-P13*P31
         MT(22,8)=-P11*P22+P12*P21
         MT(22,9)=-P11*P32+P12*P31
         MT(22,10)=P13*P11
         MT(22,11)=P11*P22-P12*P21+P13*P21
         MT(22,12)=P11*P32-P12*P31+P13*P31
         MT(22,13)=-ONE
         MT(23,4)=-P23*P11
         MT(23,5)=-P23*P21
         MT(23,6)=-P23*P31
         MT(23,7)=-P21*P12+P22*P11
         MT(23,9)=-P21*P32+P22*P31
         MT(23,10)=P21*P12-P22*P11+P23*P11
         MT(23,11)=P23*P21
         MT(23,12)=P21*P32-P22*P31+P23*P31
         MT(23,14)=-ONE
         MT(24,4)=-P33*P11
         MT(24,5)=-P33*P21
         MT(24,6)=-P33*P31
         MT(24,7)=-P31*P12+P32*P11
         MT(24,8)=-P31*P22+P32*P21
         MT(24,10)=P31*P12-P32*P11+P33*P11
         MT(24,11)=P31*P22-P32*P21+P33*P21
         MT(24,12)=P33*P31
         MT(24,15)=-ONE
      ENDIF
C
      IF (NCYCLE==0) THEN
         ST(22)=ZERO
         ST(23)=ZERO
         ST(24)=ZERO
      ELSE
         ST(22)=FXBVIT(13)+FXBVIT(4)*P13*P11+FXBVIT(5)*P13*P21
     .         +FXBVIT(6)*P13*P31+FXBVIT(8)*(P11*P22-P12*P21)
     .         +FXBVIT(9)*(P11*P32-P12*P31)-FXBVIT(10)*P13*P11
     .         -FXBVIT(11)*(P11*P22-P12*P21+P13*P21)
     .         -FXBVIT(12)*(P11*P32-P12*P31+P13*P31)
C
         ST(23)=FXBVIT(14)+FXBVIT(4)*P23*P11+FXBVIT(5)*P23*P21
     .         +FXBVIT(6)*P23*P31+FXBVIT(7)*(P21*P12-P22*P11)
     .         +FXBVIT(9)*(P21*P32-P22*P31)
     .         -FXBVIT(10)*(P21*P12-P22*P11+P23*P11)-FXBVIT(11)*P23*P21
     .         -FXBVIT(12)*(P21*P32-P22*P31+P23*P31)
C
         ST(24)=FXBVIT(15)+FXBVIT(4)*P33*P11+FXBVIT(5)*P33*P21
     .         +FXBVIT(6)*P33*P31+FXBVIT(7)*(P31*P12-P32*P11)
     .         +FXBVIT(8)*(P31*P22-P32*P21)
     .         -FXBVIT(10)*(P31*P12-P32*P11+P33*P11)
     .         -FXBVIT(11)*(P31*P22-P32*P21+P33*P21)-FXBVIT(12)*P33*P31
C
         ST(22)=ST(22)/DT05
         ST(23)=ST(23)/DT05
         ST(24)=ST(24)/DT05
      ENDIF
C    
      DO I=1,3
         DO II=1,15
            MT(II,21+I)=MT(21+I,II)
         ENDDO
      ENDDO
C 
      RETURN
      END   
Chd|====================================================================
Chd|  FXSPIN                        source/constraints/fxbody/fxbodv.F
Chd|-- called by -----------
Chd|        FXBODVP2                      source/constraints/fxbody/fxbodvp.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE FXSPIN(FXBRPM, FXBVIT, S, R12, DT2)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      my_real
     .   FXBRPM(*), FXBVIT(*), S(*), R12(*), DT2
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      my_real
     .   SL(3),DDEP(12),DT05
      my_real
     .   RT(3,3),RP(3,3),OM(3,3)
      INTEGER I,J,K
C
      DT05=HALF*DT2
      DO I=1,12
         DDEP(I)=DT05*FXBVIT(I)
      ENDDO
      DO I=1,3
         DO J=1,3
            R12(3*(I-1)+J)=FXBRPM(1+3*(I-1)+J)
     .                     +DDEP(3*(J-1)+I)-DDEP(9+I)
         ENDDO
      ENDDO
C
      SL(1)=-R12(2)*(FXBVIT(7)-FXBVIT(10))
     .      -R12(5)*(FXBVIT(8)-FXBVIT(11))
     .      -R12(8)*(FXBVIT(9)-FXBVIT(12))
C
      SL(2)=R12(1)*(FXBVIT(7)-FXBVIT(10))
     .     +R12(4)*(FXBVIT(8)-FXBVIT(11))
     .     +R12(7)*(FXBVIT(9)-FXBVIT(12))
C
      SL(3)=-R12(1)*(FXBVIT(4)-FXBVIT(10))
     .      -R12(4)*(FXBVIT(5)-FXBVIT(11))
     .      -R12(7)*(FXBVIT(6)-FXBVIT(12))
C
      S(1)=R12(1)*SL(1)+R12(2)*SL(2)+R12(3)*SL(3)
      S(2)=R12(4)*SL(1)+R12(5)*SL(2)+R12(6)*SL(3)
      S(3)=R12(7)*SL(1)+R12(8)*SL(2)+R12(9)*SL(3)
C      
      RETURN
      END
