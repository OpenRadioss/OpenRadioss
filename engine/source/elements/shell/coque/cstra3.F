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
Chd|  CSTRA3                        source/elements/shell/coque/cstra3.F
Chd|-- called by -----------
Chd|        CFORC3                        source/elements/shell/coque/cforc3.F
Chd|        CFORC3_CRK                    source/elements/xfem/cforc3_crk.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE CSTRA3(JFT    ,JLT  ,GSTR   ,SHF     ,AREA  , 
     2                  EXX    ,EYY  ,EXY    ,EXZ     ,EYZ   ,  
     3                  KXX    ,KYY  ,KXY    ,DT1C    ,EPSDOT, 
     4                  FOR    ,MOM  ,ISMSTR ,MTN   ,
     6                  IHBE   ,NFT  ,ISTRAIN,UX1     ,UX2   ,
     7                  UX3    ,UX4  ,UY1    ,UY2     ,UY3   ,
     8                  UY4    ,PX1  ,PX2    ,PY1     ,PY2   ,
     9                  WXY    ,GSTRW,F_DEF  ,NEL     )
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
#include      "units_c.inc"
#include      "scr06_c.inc"
#include      "scr14_c.inc"
#include      "param_c.inc"
#include      "com01_c.inc"
#include      "com08_c.inc"
#include      "com20_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER JFT, JLT, IHBE,NFT ,ISTRAIN,ISMSTR,MTN,
     .        NEL
C     REAL
      my_real
     .   GSTR(NEL,8),
     .   AREA(MVSIZ),SHF(MVSIZ),DT1C(MVSIZ),
     .   EXX(MVSIZ), EYY(MVSIZ), EXY(MVSIZ), EXZ(MVSIZ), EYZ(MVSIZ),
     .   KXX(MVSIZ), KYY(MVSIZ), KXY(MVSIZ),
     .   EPSDOT(6,*),FOR(NEL,5),MOM(NEL,3)
      my_real
     .  UX1(*),UX2(*),UX3(*),UX4(*),UY1(*),UY2(*),UY3(*),UY4(*),
     .  PX1(*),PX2(*),PY1(*),PY2(*),WXY(*),F_DEF(MVSIZ,8),GSTRW(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER 
     .        I, MX, J, K
C     REAL
      my_real
     .   FAC1(MVSIZ), FAC2, Q1(MVSIZ), R1(MVSIZ), R2(MVSIZ),FC1P,
     .   FTMP1(MVSIZ),FTMP2(MVSIZ),FTMP5(MVSIZ),MTMP1(MVSIZ),
     .   MTMP2(MVSIZ),FTMP4(MVSIZ),MTMP3(MVSIZ),FTMP3(MVSIZ),
     .   GTMP1(MVSIZ),GTMP2(MVSIZ),GTMP3(MVSIZ),GTMP4(MVSIZ),
     .   GTMP5(MVSIZ),GTMP6(MVSIZ),GTMP7(MVSIZ),GTMP8(MVSIZ)
      my_real
     .   UX13,UX24,UY13,UY24,EXXT,EYYT,EXYT
C---+----1----+----2----+----3----+----4----+----5----+----6----+----7--
C
      IF(IEPSDOT.NE.0)THEN
       DO I=JFT,JLT
        FAC1(I)  =ONE/AREA(I)
        K = I + NFT
        EPSDOT(1,K)=EXX(I)*FAC1(I)
        EPSDOT(2,K)=EYY(I)*FAC1(I)
        EPSDOT(3,K)=EXY(I)*FAC1(I)
        EPSDOT(4,K)=KXX(I)*FAC1(I)
        EPSDOT(5,K)=KYY(I)*FAC1(I)
        EPSDOT(6,K)=KXY(I)*FAC1(I)
       ENDDO
      ENDIF

      DO I=JFT,JLT
        FAC1(I)  =DT1C(I)/AREA(I)
        EXX(I)=EXX(I)*FAC1(I)
        EYY(I)=EYY(I)*FAC1(I)
        EXY(I)=EXY(I)*FAC1(I)
        EYZ(I)=EYZ(I)*FAC1(I)
        EXZ(I)=EXZ(I)*FAC1(I)
        KXX(I)=KXX(I)*FAC1(I)
        KYY(I)=KYY(I)*FAC1(I)
        KXY(I)=KXY(I)*FAC1(I)
      ENDDO
C
      IF(ISHFRAM.EQ.1.AND.IHBE.GE.2)THEN
#include "vectorize.inc" 
       DO I=JFT,JLT
         FTMP1(I) = FOR(I,1)
         FTMP2(I) = FOR(I,2)
         FTMP3(I) = FOR(I,3)
         FTMP4(I) = FOR(I,4)
         FTMP5(I) = FOR(I,5)
         MTMP1(I) = MOM(I,1)
         MTMP2(I) = MOM(I,2)
         MTMP3(I) = MOM(I,3)
         R1(I)= EXY(I)
         Q1(I)=-FOR(I,3)*R1(I)
         FOR(I,3)=FOR(I,3)-R1(I)*(FOR(I,1)-FOR(I,2))
         FOR(I,1)=FOR(I,1)-Q1(I)
         FOR(I,2)=FOR(I,2)+Q1(I)
         Q1(I)= R1(I)*FOR(I,4)
         FOR(I,4)=FOR(I,4)-R1(I)*FOR(I,5)
         FOR(I,5)=FOR(I,5)+Q1(I)
         Q1(I)=-MOM(I,3)*R1(I)
         MOM(I,3)=MOM(I,3)-R1(I)*(MOM(I,1)-MOM(I,2))
         MOM(I,1)=MOM(I,1)-Q1(I)
         MOM(I,2)=MOM(I,2)+Q1(I)
       ENDDO
       IF(ISTRAIN.NE.0)THEN
        DO I=JFT,JLT
         GTMP1(I)=GSTR(I,1)
         GTMP2(I)=GSTR(I,2)
         GTMP3(I)=GSTR(I,3)
         GTMP4(I)=GSTR(I,4)
         GTMP5(I)=GSTR(I,5)
         GTMP6(I)=GSTR(I,6)
         GTMP7(I)=GSTR(I,7)
         GTMP8(I)=GSTR(I,8)
         R1(I)= EXY(I)
         R2(I)=TWO*EXY(I)
         Q1(I)=-HALF*GSTR(I,3)*R1(I)
         GSTR(I,3)=GSTR(I,3)-R2(I)*(GSTR(I,1)-GSTR(I,2))
         GSTR(I,1)=GSTR(I,1)-Q1(I)
         GSTR(I,2)=GSTR(I,2)+Q1(I)
         Q1(I)= R1(I)*GSTR(I,4)
         GSTR(I,4)=GSTR(I,4)-R1(I)*GSTR(I,5)
         GSTR(I,5)=GSTR(I,5)+Q1(I)
         Q1(I)=-HALF*GSTR(I,8)*R1(I)
         GSTR(I,8)=GSTR(I,8)-R2(I)*(GSTR(I,6)-GSTR(I,7))
         GSTR(I,6)=GSTR(I,6)-Q1(I)
         GSTR(I,7)=GSTR(I,7)+Q1(I)
        ENDDO
       ENDIF
      ENDIF
C
      IF (ISTRAIN /= 0.OR.ISMSTR == 10) THEN
       IF(ISMSTR == 10)THEN
        DO I=JFT,JLT
         GSTR(I,1)=GSTR(I,1)+EXX(I)
         GSTR(I,2)=GSTR(I,2)+EYY(I)
         GSTR(I,3)=GSTR(I,3)+EXY(I)
         GSTR(I,4)=GSTR(I,4)+EYZ(I)
         GSTR(I,5)=GSTR(I,5)+EXZ(I)
         GSTR(I,6)=GSTR(I,6)+KXX(I)
         GSTR(I,7)=GSTR(I,7)+KYY(I)
         GSTR(I,8)=GSTR(I,8)+KXY(I)
C-------- WXY         
         GSTRW(I)=GSTRW(I)+WXY(I)*FAC1(I)
        ENDDO
C-----    
        DO I=JFT,JLT 
          F_DEF(I,6) = GSTR(I,6)
          F_DEF(I,7) = GSTR(I,7)
          F_DEF(I,8) = (GSTR(I,8)+GSTRW(I))*HALF
          F_DEF(I,5) = (GSTR(I,8)-GSTRW(I))*HALF
        ENDDO
       ELSEIF(ISMSTR /= 11 )THEN
        DO I=JFT,JLT
         GSTR(I,1)=GSTR(I,1)+EXX(I)
         GSTR(I,2)=GSTR(I,2)+EYY(I)
         GSTR(I,3)=GSTR(I,3)+EXY(I)
         GSTR(I,4)=GSTR(I,4)+EYZ(I)
         GSTR(I,5)=GSTR(I,5)+EXZ(I)
         GSTR(I,6)=GSTR(I,6)+KXX(I)
         GSTR(I,7)=GSTR(I,7)+KYY(I)
         GSTR(I,8)=GSTR(I,8)+KXY(I)
        ENDDO
       ELSE
        DO I=JFT,JLT
         FAC1(I)  =ONE/AREA(I)
         UX13=UX1(I)-UX3(I)
         UX24=UX2(I)-UX4(I)
         UY13=UY1(I)-UY3(I)
         UY24=UY2(I)-UY4(I)
         EXXT=(PX1(I)*UX13+PX2(I)*UX24)*FAC1(I)
         EYYT=(PY1(I)*UY13+PY2(I)*UY24)*FAC1(I)
         EXYT=(PY1(I)*UX13+PY2(I)*UX24
     .        +PX1(I)*UY13+PX2(I)*UY24)*FAC1(I)
         GSTR(I,1)=EXXT
         GSTR(I,2)=EYYT
         GSTR(I,3)=EXYT
         GSTR(I,4)=GSTR(I,4)+EYZ(I)
         GSTR(I,5)=GSTR(I,5)+EXZ(I)
         GSTR(I,6)=GSTR(I,6)+KXX(I)
         GSTR(I,7)=GSTR(I,7)+KYY(I)
         GSTR(I,8)=GSTR(I,8)+KXY(I)
        ENDDO
       ENDIF
      ENDIF
C
      RETURN
      END
