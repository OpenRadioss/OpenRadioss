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
Chd|  CBASTRA3                      source/elements/shell/coqueba/cbastra3.F
Chd|-- called by -----------
Chd|        CBAFORC3                      source/elements/shell/coqueba/cbaforc3.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CBASTRA3(GSTR,GSTRPG,
     1                   JFT, JLT, NFT, NPG,VDEF,
     2                   EXX, EYY, EXY, EXZ, EYZ,
     3                   KXX, KYY, KXY, DT1C,EPSDOT,
     4                   IEPSDOT, ISTRAIN,UX1 ,UX2 ,UX3 ,
     5                   UX4    ,UY1    ,UY2 ,UY3 ,UY4 ,
     6                   X13, X24, Y13, Y24, BM  ,
     7                   ISMSTR ,MTN ,NPLAT,IPLAT,ISROT,
     8                   WXY   ,F_DEF,GSTRWPG,NEL)
C-----------------------------------------------
C   I m p l i c i t   T y p e s
C-----------------------------------------------
#include      "implicit_f.inc"
C-----------------------------------------------
C   G l o b a l   P a r a m e t e r s
C-----------------------------------------------
#include      "mvsiz_p.inc"
C-----------------------------------------------
C   C o m m o n   B l o c k s
C-----------------------------------------------
#include      "com01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, NFT, IEPSDOT, ISTRAIN, NPG,II,NEL
      INTEGER ISMSTR ,MTN ,NPLAT,IPLAT(*),ISROT
C     REAL
      my_real
     .   VDEF(MVSIZ,8),GSTR(NEL,8),GSTRPG(NEL,8),DT1C(*),EPSDOT(6,*),
     .   EXX(*),EYY(*),EXY(*),EXZ(*),EYZ(*),KXX(*),KYY(*),KXY(*),
     .   UX1(*),UX2(*),UX3(*),UX4(*),UY1(*),UY2(*),UY3(*),UY4(*),
     .   X13(*), X24(*), Y13(*), Y24(*), BM(MVSIZ,36),F_DEF(MVSIZ,8),
     .   WXY(*),GSTRWPG(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, J,EP
      my_real
     .        UX13,UX24,UY13,UY24,UXHI,UYHI,QUATRE1,
     .        EXXT(MVSIZ),EYYT(MVSIZ),EXYT(MVSIZ)
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C
      QUATRE1 = ONE/NPG
      IF (IEPSDOT /= 0) THEN
       DO I=JFT,JLT
        J = I + NFT
        EPSDOT(1,J) = VDEF(I,1)
        EPSDOT(2,J) = VDEF(I,2)
        EPSDOT(3,J) = VDEF(I,3)
        EPSDOT(4,J) = VDEF(I,6)
        EPSDOT(5,J) = VDEF(I,7)
        EPSDOT(6,J) = VDEF(I,8)
       ENDDO
      ENDIF
C
      DO I=JFT,JLT
        EXX(I) = VDEF(I,1) * DT1C(I)
        EYY(I) = VDEF(I,2) * DT1C(I)
        EXY(I) = VDEF(I,3) * DT1C(I)
        EYZ(I) = VDEF(I,5) * DT1C(I)
        EXZ(I) = VDEF(I,4) * DT1C(I)
        KXX(I) = VDEF(I,6) * DT1C(I)
        KYY(I) = VDEF(I,7) * DT1C(I)
        KXY(I) = VDEF(I,8) * DT1C(I)
      ENDDO
C
      IF (ISTRAIN /= 0.OR.ISMSTR == 10) THEN
       IF(ISMSTR == 10)THEN
        DO I=JFT,JLT
         GSTR(I,1)=GSTR(I,1)+EXX(I)*QUATRE1
         GSTR(I,2)=GSTR(I,2)+EYY(I)*QUATRE1
         GSTR(I,3)=GSTR(I,3)+EXY(I)*QUATRE1
         GSTR(I,4)=GSTR(I,4)+EYZ(I)*QUATRE1
         GSTR(I,5)=GSTR(I,5)+EXZ(I)*QUATRE1
         GSTR(I,6)=GSTR(I,6)+KXX(I)*QUATRE1
         GSTR(I,7)=GSTR(I,7)+KYY(I)*QUATRE1
         GSTR(I,8)=GSTR(I,8)+KXY(I)*QUATRE1
C         
         GSTRPG(I,1)=GSTRPG(I,1)+EXX(I)
         GSTRPG(I,2)=GSTRPG(I,2)+EYY(I)
         GSTRPG(I,3)=GSTRPG(I,3)+EXY(I)
         GSTRPG(I,4)=GSTRPG(I,4)+EYZ(I)
         GSTRPG(I,5)=GSTRPG(I,5)+EXZ(I)
         GSTRPG(I,6)=GSTRPG(I,6)+KXX(I)
         GSTRPG(I,7)=GSTRPG(I,7)+KYY(I)
         GSTRPG(I,8)=GSTRPG(I,8)+KXY(I)
C		 
         GSTRWPG(I)=GSTRWPG(I)+WXY(I)* DT1C(I)
        ENDDO
C----- use ISTRAIN temporaly for KXX.. cumulated       
        DO I=JFT,JLT
         F_DEF(I,6) = GSTRPG(I,6)
         F_DEF(I,7) = GSTRPG(I,7)
         F_DEF(I,8) = (GSTRPG(I,8)+GSTRWPG(I))*HALF
         F_DEF(I,5) = (GSTRPG(I,8)-GSTRWPG(I))*HALF
        ENDDO
       ELSEIF(ISMSTR /= 11)THEN
        DO I=JFT,JLT
         GSTR(I,1)=GSTR(I,1)+EXX(I)*QUATRE1
         GSTR(I,2)=GSTR(I,2)+EYY(I)*QUATRE1
         GSTR(I,3)=GSTR(I,3)+EXY(I)*QUATRE1
         GSTR(I,4)=GSTR(I,4)+EYZ(I)*QUATRE1
         GSTR(I,5)=GSTR(I,5)+EXZ(I)*QUATRE1
         GSTR(I,6)=GSTR(I,6)+KXX(I)*QUATRE1
         GSTR(I,7)=GSTR(I,7)+KYY(I)*QUATRE1
         GSTR(I,8)=GSTR(I,8)+KXY(I)*QUATRE1
C
         GSTRPG(I,1)=GSTRPG(I,1)+EXX(I)
         GSTRPG(I,2)=GSTRPG(I,2)+EYY(I)
         GSTRPG(I,3)=GSTRPG(I,3)+EXY(I)
         GSTRPG(I,4)=GSTRPG(I,4)+EYZ(I)
         GSTRPG(I,5)=GSTRPG(I,5)+EXZ(I)
         GSTRPG(I,6)=GSTRPG(I,6)+KXX(I)
         GSTRPG(I,7)=GSTRPG(I,7)+KYY(I)
         GSTRPG(I,8)=GSTRPG(I,8)+KXY(I)
        ENDDO
       ELSE
       IF (ISROT > 0) THEN
#include "vectorize.inc"
        DO EP=JFT,NPLAT
         I=IPLAT(EP)
         UX13=UX1(I)-UX3(I)
         UX24=UX2(I)-UX4(I)
         UY13=UY1(I)-UY3(I)
         UY24=UY2(I)-UY4(I)
         UXHI=UX1(I)-UX2(I)+UX3(I)-UX4(I)
         UYHI=UY1(I)-UY2(I)+UY3(I)-UY4(I)
         EXXT(I)=BM(I,1)*UX13+BM(I,2)*UX24+BM(I,3)*UXHI
         EYYT(I)=BM(I,5)*UY13+BM(I,6)*UY24+BM(I,7)*UYHI
         EXYT(I)=BM(I,1)*UY13+BM(I,2)*UY24+BM(I,3)*UYHI
     .          +BM(I,5)*UX13+BM(I,6)*UX24+BM(I,7)*UXHI
        END DO
#include "vectorize.inc"
        DO EP=NPLAT+1,JLT
         I=IPLAT(EP)
         EXXT(I)=BM(I,1)*UX1(I)+BM(I,4)*UY1(I)
     .          +BM(I,10)*UX2(I)+BM(I,13)*UY2(I)
     .          +BM(I,19)*UX3(I)+BM(I,22)*UY3(I)
     .          +BM(I,28)*UX4(I)+BM(I,31)*UY4(I)
         EYYT(I)=BM(I,2)*UX1(I)+BM(I,5)*UY1(I)
     .          +BM(I,11)*UX2(I)+BM(I,14)*UY2(I)
     .          +BM(I,20)*UX3(I)+BM(I,23)*UY3(I)
     .          +BM(I,29)*UX4(I)+BM(I,32)*UY4(I)
         EXYT(I)=BM(I,3)*UX1(I)+BM(I,6)*UY1(I)
     .          +BM(I,12)*UX2(I)+BM(I,15)*UY2(I)
     .          +BM(I,21)*UX3(I)+BM(I,24)*UY3(I)
     .          +BM(I,30)*UX4(I)+BM(I,33)*UY4(I)
        END DO
       ELSE
#include "vectorize.inc"
        DO EP=JFT,NPLAT
         I=IPLAT(EP)
         UX13=UX1(I)-UX3(I)
         UX24=UX2(I)-UX4(I)
         UY13=UY1(I)-UY3(I)
         UY24=UY2(I)-UY4(I)
         UXHI=UX1(I)-UX2(I)+UX3(I)-UX4(I)
         UYHI=UY1(I)-UY2(I)+UY3(I)-UY4(I)
         EXXT(I)=BM(I,1)*UX13+BM(I,2)*UX24+BM(I,3)*UXHI
         EYYT(I)=BM(I,5)*UY13+BM(I,6)*UY24+BM(I,7)*UYHI
         EXYT(I)=Y24(I)*UY13-Y13(I)*UY24
     .          -X24(I)*UX13+X13(I)*UX24
        END DO
#include "vectorize.inc"
        DO EP=NPLAT+1,JLT
         I=IPLAT(EP)
         UX13=UX1(I)-UX3(I)
         UX24=UX2(I)-UX4(I)
         UY13=UY1(I)-UY3(I)
         UY24=UY2(I)-UY4(I)
         EXXT(I)=BM(I,1)*UX1(I)+BM(I,4)*UY1(I)
     .          +BM(I,10)*UX2(I)+BM(I,13)*UY2(I)
     .          +BM(I,19)*UX3(I)+BM(I,22)*UY3(I)
     .          +BM(I,28)*UX4(I)+BM(I,31)*UY4(I)
         EYYT(I)=BM(I,2)*UX1(I)+BM(I,5)*UY1(I)
     .          +BM(I,11)*UX2(I)+BM(I,14)*UY2(I)
     .          +BM(I,20)*UX3(I)+BM(I,23)*UY3(I)
     .          +BM(I,29)*UX4(I)+BM(I,32)*UY4(I)
         EXYT(I)=Y24(I)*UY13-Y13(I)*UY24
     .          -X24(I)*UX13+X13(I)*UX24
        END DO
       END IF !(ISROT > 0) THEN
        DO I=JFT,JLT
         GSTR(I,1)=GSTR(I,1)+EXXT(I)*QUATRE1
         GSTR(I,2)=GSTR(I,2)+EYYT(I)*QUATRE1
         GSTR(I,3)=GSTR(I,3)+EXYT(I)*QUATRE1
         GSTR(I,4)=GSTR(I,4)+EYZ(I)*QUATRE1
         GSTR(I,5)=GSTR(I,5)+EXZ(I)*QUATRE1
         GSTR(I,6)=GSTR(I,6)+KXX(I)*QUATRE1
         GSTR(I,7)=GSTR(I,7)+KYY(I)*QUATRE1
         GSTR(I,8)=GSTR(I,8)+KXY(I)*QUATRE1
C
         GSTRPG(I,1)=EXXT(I)
         GSTRPG(I,2)=EYYT(I)
         GSTRPG(I,3)=EXYT(I)
         GSTRPG(I,4)=GSTRPG(I,4)+EYZ(I)
         GSTRPG(I,5)=GSTRPG(I,5)+EXZ(I)
         GSTRPG(I,6)=GSTRPG(I,6)+KXX(I)
         GSTRPG(I,7)=GSTRPG(I,7)+KYY(I)
         GSTRPG(I,8)=GSTRPG(I,8)+KXY(I)
        ENDDO
       ENDIF !IF(ISMSTR /= 11)
      ENDIF !IF(ISTRAIN /= 0)
c-----------
      RETURN
      END
