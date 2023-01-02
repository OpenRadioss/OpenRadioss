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
Chd|  M26LAW                        source/materials/mat/mat026/m26law.F
Chd|-- called by -----------
Chd|        MMAIN                         source/materials/mat_share/mmain.F
Chd|-- calls ---------------
Chd|====================================================================
      SUBROUTINE M26LAW(PM   ,OFF   ,SIG  ,RHO,
     2                  EPXE ,THETA ,EPD  ,Z  ,
     3                  MAT  ,VOLN  ,DVOL ,D1 ,
     4                  D2   ,D3    ,D4   ,D5 ,
     5                  D6   ,NEL, P, RHO0, DF)
C
C     LOI DE COMPORTEMENT TYPE JOHNSON - COOK - SESAME
C
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
#include      "com08_c.inc"
#include      "param_c.inc"
#include      "vect01_c.inc"
C-----------------------------------------------
C   D u m m y   A r g u m e n t s
C-----------------------------------------------
      INTEGER MAT(*),NEL
      my_real
     .   PM(NPROPM,*), OFF(*), SIG(NEL,6), RHO(*), EPXE(*), THETA(*),
     .   EPD(*), Z(*), VOLN(MVSIZ), DVOL(*), D1(*), D2(*), D3(*), D4(*),
     .   D5(*), D6(*), P(*), RHO0(*), DF(*)
C-----------------------------------------------
C   L o c a l   V a r i a b l e s
C-----------------------------------------------
      INTEGER I, MX
      my_real
     .   G(MVSIZ), G1(MVSIZ), G2(MVSIZ), AK(MVSIZ), QH(MVSIZ), TMELT(MVSIZ),
     .   AJ2(MVSIZ), DMU(MVSIZ), DAV(MVSIZ), EPMX(MVSIZ),
     .   THETL(MVSIZ), CA(MVSIZ), CB(MVSIZ), CC(MVSIZ), CN(MVSIZ), EPDR(MVSIZ), CMX(MVSIZ),
     .   SIGMX(MVSIZ),  TSTAR, CT, CE, CH, SCALE,
     .   RHO0_1, CA_1, CB_1, CN_1, CC_1,
     .   CMX_1, TMELT_1, EPDR_1, THETL_1,EPMX_1,
     .   SIGMX_1
C-----------------------------------------------
C
      MX      =MAT(LFT)
C
      RHO0_1 =PM( 1,MX)
      CA_1   =PM(38,MX)
      CB_1   =PM(39,MX)
      CN_1   =PM(40,MX)
      CC_1   =PM(43,MX)
      CMX_1  =PM(45,MX)
      TMELT_1=PM(46,MX)
      EPDR_1 =PM(44,MX)
      THETL_1=PM(47,MX)
      EPMX_1 =PM(41,MX)
      SIGMX_1=PM(42,MX)
C
      DO 10 I=LFT,LLT
      RHO0(I) =RHO0_1
      G(I)    =PM(22,MX)*OFF(I)
      CA(I)   =CA_1
      CB(I)   =CB_1
      CN(I)   =CN_1
      CC(I)   =CC_1
      CMX(I)  =CMX_1
      TMELT(I)=TMELT_1
      EPDR(I) =EPDR_1
C      SPH(I)  =PM(48,MX)
C      PC(I)   =PM(37,MX)
      THETL(I)=THETL_1
      EPMX(I) =EPMX_1
      SIGMX(I)=SIGMX_1
   10 CONTINUE
C
      DO 15 I=LFT,LLT
   15 DF(I)=RHO0(I)/RHO(I)
C
      DO 30 I=LFT,LLT
      P(I)  =-THIRD*(SIG(I,1)+SIG(I,2)+SIG(I,3))
      DAV(I)=-THIRD*(D1(I)+D2(I)+D3(I))
      G1(I)=DT1*G(I)
      G2(I)=TWO*G1(I)
      DMU(I)=-DVOL(I)/VOLN(I)
   30 CONTINUE
C-------------------------------
C     CONTRAINTES DEVIATORIQUES
C-------------------------------
      DO 40 I=LFT,LLT
      SIG(I,1)=SIG(I,1)+P(I)+G2(I)*(D1(I)+DAV(I))
      SIG(I,2)=SIG(I,2)+P(I)+G2(I)*(D2(I)+DAV(I))
      SIG(I,3)=SIG(I,3)+P(I)+G2(I)*(D3(I)+DAV(I))
      SIG(I,4)=SIG(I,4)+G1(I)*D4(I)
      SIG(I,5)=SIG(I,5)+G1(I)*D5(I)
   40 SIG(I,6)=SIG(I,6)+G1(I)*D6(I)
C
      DO 50 I=LFT,LLT
      AJ2(I)=HALF*(SIG(I,1)**2+SIG(I,2)**2+SIG(I,3)**2)
     1              +SIG(I,4)**2+SIG(I,5)**2+SIG(I,6)**2
   50 AJ2(I)=SQRT(3.*AJ2(I))
C
      DO 90 I=LFT,LLT
      IF(THETA(I)>=TMELT(I)) GOTO 90
C
      TSTAR=0.
      CT=1.
      IF(THETA(I)<=THREE100) GOTO 60
      TSTAR=(THETA(I)-THREE100)/(TMELT(I)-THREE100)
      IF(THETA(I)>THETL(I)) CMX(I)=ONE
      CT=ONE -TSTAR**CMX(I)
C
   60 CE=ONE
C
      EPD(I)=OFF(I)* MAX(   ABS(D1(I)),   ABS(D2(I)),   ABS(D3(I)),
     .             HALF*ABS(D4(I)),.5*ABS(D5(I)),.5*ABS(D6(I)))
C----------------------------------------------------------
      IF(EPD(I)<=EPDR(I)) GOTO 70
      CE=ONE + CC(I) * LOG(EPD(I)/EPDR(I))
C
   70 CH=CA(I)
      IF(EPXE(I)<=ZERO) GOTO 80
      CH=CA(I)+CB(I)*EPXE(I)**CN(I)
      IF(EPXE(I)>EPMX(I)) CH=CA(I)+CB(I)*EPMX(I)**CN(I)
C
   80 AK(I)= MIN(SIGMX(I),CH)*CE*CT
C-----------------------
C     MODULE ECROUISSAGE
C-----------------------
      IF(CN(I)>=1) THEN
       QH(I)= (CB(I)*CN(I)*EPXE(I)**(CN(I)-ONE))*CE*CT
      ELSE
       IF(EPXE(I)/=ZERO)THEN
        QH(I)= (CB(I)*CN(I)/EPXE(I)**(ONE-CN(I)))*CE*CT
       ELSE
       QH(I)=ZERO
       ENDIF
      ENDIF
   90 CONTINUE
C
      DO 110 I=LFT,LLT
      IF(THETA(I)>=TMELT(I)) GOTO 100
      IF(AJ2(I)<=AK(I))      GOTO 110
C
      SCALE=ZERO
      IF(AJ2(I)/=ZERO) SCALE=AK(I)/AJ2(I)
      SIG(I,1)=SCALE*SIG(I,1)
      SIG(I,2)=SCALE*SIG(I,2)
      SIG(I,3)=SCALE*SIG(I,3)
      SIG(I,4)=SCALE*SIG(I,4)
      SIG(I,5)=SCALE*SIG(I,5)
      SIG(I,6)=SCALE*SIG(I,6)
      EPXE(I)=EPXE(I)+(ONE-SCALE)*AJ2(I)/(THREE*G(I)+QH(I))
      GOTO 110
C
  100 AK(I)=ZERO
      EPXE(I)=ZERO
      SIG(I,1)=ZERO
      SIG(I,2)=ZERO
      SIG(I,3)=ZERO
      SIG(I,4)=ZERO
      SIG(I,5)=ZERO
      SIG(I,6)=ZERO	 
C
  110 CONTINUE
      RETURN
      END
